{ ****************************************************************************** }
{ * memory Rasterization with AGG support                                      * }
{ * by QQ 600585@qq.com                                                        * }
{ ****************************************************************************** }
{ * https://github.com/PassByYou888/CoreCipher                                 * }
{ * https://github.com/PassByYou888/ZServer4D                                  * }
{ * https://github.com/PassByYou888/zExpression                                * }
{ * https://github.com/PassByYou888/zTranslate                                 * }
{ * https://github.com/PassByYou888/zSound                                     * }
{ * https://github.com/PassByYou888/zAnalysis                                  * }
{ * https://github.com/PassByYou888/zGameWare                                  * }
{ * https://github.com/PassByYou888/zRasterization                             * }
{ ****************************************************************************** }

(*
  ////////////////////////////////////////////////////////////////////////////////
  //                                                                            //
  //  Anti-Grain Geometry (modernized Pascal fork, aka 'AggPasMod')             //
  //    Maintained by Christian-W. Budde (Christian@pcjv.de)                    //
  //    Copyright (c) 2012-2017                                                 //
  //                                                                            //
  //  Based on:                                                                 //
  //    Pascal port by Milan Marusinec alias Milano (milan@marusinec.sk)        //
  //    Copyright (c) 2005-2006, see http://www.aggpas.org                      //
  //                                                                            //
  //  Original License:                                                         //
  //    Anti-Grain Geometry - Version 2.4 (Public License)                      //
  //    Copyright (C) 2002-2005 Maxim Shemanarev (http://www.antigrain.com)     //
  //    Contact: McSeem@antigrain.com / McSeemAgg@yahoo.com                     //
  //                                                                            //
  //  Permission to copy, use, modify, sell and distribute this software        //
  //  is granted provided this copyright notice appears in all copies.          //
  //  This software is provided "as is" without express or implied              //
  //  warranty, and with no claim as to its suitability for any purpose.        //
  //                                                                            //
  ////////////////////////////////////////////////////////////////////////////////
*)
unit AggBSpline;

{$DEFINE FPC_DELPHI_MODE}
{$INCLUDE zDefine.inc}
interface

uses
  AggBasics;

type
  // ----------------------------------------------------------------
  // A very simple class of Bi-cubic Spline interpolation.
  // First call init(num, x[], y[]) where num - number of source points,
  // x, y - arrays of X and Y values respectively. Here Y must be a function
  // of X. It means that all the X-coordinates must be arranged in the ascending
  // order.
  // Then call get(x) that calculates a value Y for the respective X.
  // The class supports extrapolation, i.e. you can call get(x) where x is
  // outside the given with init() X-range. Extrapolation is a simple linear
  // function.

  TAggBSpline = class
  private
    FMax, FNum: Integer;
    fx, fy, FSplineBuffer: PDouble;
    FLastIdx: Integer;
  protected
    procedure BSearch(n: Integer; x: PDouble; x0: Double; i: PInteger);

    function ExtrapolationLeft(x: Double): Double;
    function ExtrapolationRight(x: Double): Double;

    function Interpolation(x: Double; i: Integer): Double;
  public
    constructor Create; overload;
    constructor Create(Num: Integer); overload;
    constructor Create(Num: Integer; x, y: PDouble); overload;
    destructor Destroy; override;

    procedure Init(Max: Integer); overload;
    procedure Init(Num: Integer; x, y: PDouble); overload;
    procedure Init(Num: Integer; Points: PPointDouble); overload;

    procedure AddPoint(x, y: Double);
    procedure Prepare;

    function Get(x: Double): Double;
    function GetStateful(x: Double): Double;
  end;

implementation


{ TAggBSpline }

constructor TAggBSpline.Create;
begin
  FMax := 0;
  FNum := 0;

  fx := nil;
  fy := nil;
  FSplineBuffer := nil;

  FLastIdx := -1;
end;

constructor TAggBSpline.Create(Num: Integer);
begin
  Create;

  Init(Num);
end;

constructor TAggBSpline.Create(Num: Integer; x, y: PDouble);
begin
  Create;

  Init(Num, x, y);
end;

destructor TAggBSpline.Destroy;
begin
  AggFreeMem(Pointer(FSplineBuffer), FMax * 3 * SizeOf(Double));

  inherited;
end;

procedure TAggBSpline.Init(Max: Integer);
begin
  if (Max > 2) and (Max > FMax) then
  begin
    AggFreeMem(Pointer(FSplineBuffer), FMax * 3 * SizeOf(Double));
    AggGetMem(Pointer(FSplineBuffer), Max * 3 * SizeOf(Double));

    FMax := Max;

    fx := PDouble(PtrComp(FSplineBuffer) + FMax * SizeOf(Double));
    fy := PDouble(PtrComp(FSplineBuffer) + FMax * 2 * SizeOf(Double));
  end;

  FNum := 0;
  FLastIdx := -1;
end;

procedure TAggBSpline.Init(Num: Integer; x, y: PDouble);
var
  i: Integer;
begin
  if Num > 2 then
  begin
    Init(Num);

    for i := 0 to Num - 1 do
    begin
      AddPoint(x^, y^);

      inc(x);
      inc(y);
    end;

    Prepare;
  end;

  FLastIdx := -1;
end;

procedure TAggBSpline.Init(Num: Integer; Points: PPointDouble);
var
  i: Integer;
begin
  if Num > 2 then
  begin
    Init(Num);

    for i := 0 to Num - 1 do
    begin
      AddPoint(Points^.x, Points^.y);

      inc(Points);
    end;

    Prepare;
  end;

  FLastIdx := -1;
end;

procedure TAggBSpline.AddPoint(x, y: Double);
begin
  if FNum < FMax then
  begin
    PDouble(PtrComp(fx) + FNum * SizeOf(Double))^ := x;
    PDouble(PtrComp(fy) + FNum * SizeOf(Double))^ := y;

    inc(FNum);
  end;
end;

procedure TAggBSpline.Prepare;
var
  i, k, n1, SZ  : Integer;
  Temp, r, s, al: PDouble;
  h, p, d, f, E : Double;
begin
  if FNum > 2 then
  begin
    for k := 0 to FNum - 1 do
      PDouble(PtrComp(FSplineBuffer) + k * SizeOf(Double))^ := 0;

    n1 := 3 * FNum;
    SZ := n1;

    AggGetMem(Pointer(al), n1 * SizeOf(Double));

    Temp := al;

    for k := 0 to n1 - 1 do
      PDouble(PtrComp(Temp) + k * SizeOf(Double))^ := 0;

    r := PDouble(PtrComp(Temp) + FNum * SizeOf(Double));
    s := PDouble(PtrComp(Temp) + FNum * 2 * SizeOf(Double));

    n1 := FNum - 1;
    d := PDouble(PtrComp(fx) + SizeOf(Double))^ - fx^;
    E := (PDouble(PtrComp(fy) + SizeOf(Double))^ - fy^) / d;

    k := 1;

    while k < n1 do
    begin
      h := d;
      d := PDouble(PtrComp(fx) + (k + 1) * SizeOf(Double))^ -
        PDouble(PtrComp(fx) + k * SizeOf(Double))^;
      f := E;
      E := (PDouble(PtrComp(fy) + (k + 1) * SizeOf(Double))^ -
        PDouble(PtrComp(fy) + k * SizeOf(Double))^) / d;

      PDouble(PtrComp(al) + k * SizeOf(Double))^ := d / (d + h);
      PDouble(PtrComp(r) + k * SizeOf(Double))^ :=
        1.0 - PDouble(PtrComp(al) + k * SizeOf(Double))^;
      PDouble(PtrComp(s) + k * SizeOf(Double))^ := 6.0 * (E - f) / (h + d);

      inc(k);
    end;

    k := 1;

    while k < n1 do
    begin
      p := 1.0 / (PDouble(PtrComp(r) + k * SizeOf(Double))^ *
        PDouble(PtrComp(al) + (k - 1) * SizeOf(Double))^ + 2.0);

      PDouble(PtrComp(al) + k * SizeOf(Double))^ :=
        PDouble(PtrComp(al) + k * SizeOf(Double))^ * -p;

      PDouble(PtrComp(s) + k * SizeOf(Double))^ :=
        (PDouble(PtrComp(s) + k * SizeOf(Double))^ -
        PDouble(PtrComp(r) + k * SizeOf(Double))^ *
        PDouble(PtrComp(s) + (k - 1) * SizeOf(Double))^) * p;

      inc(k);
    end;

    PDouble(PtrComp(FSplineBuffer) + n1 * SizeOf(Double))^ := 0.0;

    PDouble(PtrComp(al) + (n1 - 1) * SizeOf(Double))^ :=
      PDouble(PtrComp(s) + (n1 - 1) * SizeOf(Double))^;

    PDouble(PtrComp(FSplineBuffer) + (n1 - 1) * SizeOf(Double))^ :=
      PDouble(PtrComp(al) + (n1 - 1) * SizeOf(Double))^;

    k := n1 - 2;
    i := 0;

    while i < FNum - 2 do
    begin
      PDouble(PtrComp(al) + k * SizeOf(Double))^ :=
        PDouble(PtrComp(al) + k * SizeOf(Double))^ *
        PDouble(PtrComp(al) + (k + 1) * SizeOf(Double))^ +
        PDouble(PtrComp(s) + k * SizeOf(Double))^;

      PDouble(PtrComp(FSplineBuffer) + k * SizeOf(Double))^ :=
        PDouble(PtrComp(al) + k * SizeOf(Double))^;

      inc(i);
      dec(k);
    end;

    AggFreeMem(Pointer(al), SZ * SizeOf(Double));
  end;

  FLastIdx := -1;
end;

function TAggBSpline.Get(x: Double): Double;
var
  i: Integer;
begin
  if FNum > 2 then
  begin
    // Extrapolation on the left
    if x < fx^ then
    begin
      Result := ExtrapolationLeft(x);

      Exit;
    end;

    // Extrapolation on the right
    if x >= PDouble(PtrComp(fx) + (FNum - 1) * SizeOf(Double))^ then
    begin
      Result := ExtrapolationRight(x);

      Exit;
    end;

    // Interpolation
    BSearch(FNum, fx, x, @i);

    Result := Interpolation(x, i);

    Exit;
  end;

  Result := 0.0;
end;

function TAggBSpline.GetStateful(x: Double): Double;
begin
  if FNum > 2 then
  begin
    // Extrapolation on the left
    if x < fx^ then
    begin
      Result := ExtrapolationLeft(x);

      Exit;
    end;

    // Extrapolation on the right
    if x >= PDouble(PtrComp(fx) + (FNum - 1) * SizeOf(Double))^ then
    begin
      Result := ExtrapolationRight(x);

      Exit;
    end;

    if FLastIdx >= 0 then
    begin
      // Check if x is not in current range
      if (x < PDouble(PtrComp(fx) + FLastIdx * SizeOf(Double))^) or
        (x > PDouble(PtrComp(fx) + (FLastIdx + 1) * SizeOf(Double))^) then
        // Check if x between next points (most probably)
        if (FLastIdx < FNum - 2) and
          (x >= PDouble(PtrComp(fx) + (FLastIdx + 1) * SizeOf(Double))^)
          and (x <= PDouble(PtrComp(fx) + (FLastIdx + 2) *
          SizeOf(Double))^) then
          inc(FLastIdx)
        else if (FLastIdx > 0) and
          (x >= PDouble(PtrComp(fx) + (FLastIdx - 1) * SizeOf(Double))^)
          and (x <= PDouble(PtrComp(fx) + FLastIdx * SizeOf(Double))^)
        then
          // x is between pevious points
          dec(FLastIdx)
        else
          // Else perform full search
          BSearch(FNum, fx, x, @FLastIdx);

      Result := Interpolation(x, FLastIdx);

      Exit;
    end
    else
    begin
      // Interpolation
      BSearch(FNum, fx, x, @FLastIdx);

      Result := Interpolation(x, FLastIdx);

      Exit;
    end;
  end;

  Result := 0.0;
end;

procedure TAggBSpline.BSearch(n: Integer; x: PDouble; x0: Double; i: PInteger);
var
  j, k: Integer;
begin
  j := n - 1;
  i^ := 0;

  while j - i^ > 1 do
  begin
    k := ShrInt32(i^ + j, 1);

    if x0 < PDouble(PtrComp(x) + k * SizeOf(Double))^ then
      j := k
    else
      i^ := k;
  end;
end;

function TAggBSpline.ExtrapolationLeft(x: Double): Double;
var
  d: Double;
begin
  d := PDouble(PtrComp(fx) + SizeOf(Double))^ - fx^;

  Result := (-d * PDouble(PtrComp(FSplineBuffer) + SizeOf(Double))^ / 6 +
    (PDouble(PtrComp(fy) + SizeOf(Double))^ - fy^) / d) *
    (x - fx^) + fy^;
end;

function TAggBSpline.ExtrapolationRight(x: Double): Double;
var
  d: Double;
begin
  d := PDouble(PtrComp(fx) + (FNum - 1) * SizeOf(Double))^ -
    PDouble(PtrComp(fx) + (FNum - 2) * SizeOf(Double))^;

  Result := (d * PDouble(PtrComp(FSplineBuffer) + (FNum - 2) * SizeOf(Double))^ / 6 +
    (PDouble(PtrComp(fy) + (FNum - 1) * SizeOf(Double))^ -
    PDouble(PtrComp(fy) + (FNum - 2) * SizeOf(Double))^) / d) *
    (x - PDouble(PtrComp(fx) + (FNum - 1) * SizeOf(Double))^) +
    PDouble(PtrComp(fy) + (FNum - 1) * SizeOf(Double))^;
end;

function TAggBSpline.Interpolation(x: Double; i: Integer): Double;
var
  j: Integer;
  d, h, r, p: Double;
begin
  j := i + 1;
  d := PDouble(PtrComp(fx) + i * SizeOf(Double))^ -
    PDouble(PtrComp(fx) + j * SizeOf(Double))^;
  h := x - PDouble(PtrComp(fx) + j * SizeOf(Double))^;
  r := PDouble(PtrComp(fx) + i * SizeOf(Double))^ - x;
  p := d * d / 6.0;

  Result := (PDouble(PtrComp(FSplineBuffer) + j * SizeOf(Double))^ * r * r * r +
    PDouble(PtrComp(FSplineBuffer) + i * SizeOf(Double))^ * h * h * h) / 6.0 / d +
    ((PDouble(PtrComp(fy) + j * SizeOf(Double))^ - PDouble(PtrComp(FSplineBuffer)
    + j * SizeOf(Double))^ * p) * r +
    (PDouble(PtrComp(fy) + i * SizeOf(Double))^ - PDouble(PtrComp(FSplineBuffer) +
    i * SizeOf(Double))^ * p) * h) / d;
end;

end. 
 
 
