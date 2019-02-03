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
unit AggImageFilters;

(*
  ////////////////////////////////////////////////////////////////////////////////
  //                                                                            //
  //  Image transformation filters,                                             //
  //  Filtering classes (TAggImageFilterLUT, TAggImageFilter),                  //
  //  Basic filter shape classes                                                //
  //                                                                            //
  ////////////////////////////////////////////////////////////////////////////////
*)

{$DEFINE FPC_DELPHI_MODE}
{$INCLUDE zDefine.inc}
interface
uses
  Math,
  AggBasics,
  AggMath;

const
  CAggImageFilterShift = 14;
  CAggImageFilterSize  = 1 shl CAggImageFilterShift;
  CAggImageFilterMask  = CAggImageFilterSize - 1;

  CAggImageSubpixelShift = 8;
  CAggImageSubpixelSize  = 1 shl CAggImageSubpixelShift;
  CAggImageSubpixelMask  = CAggImageSubpixelSize - 1;

type
  TAggCustomImageFilter = class
  protected
    function GetRadius: Double; virtual; abstract;
    procedure SetRadius(r: Double); virtual;
  public
    constructor Create; virtual;

    function CalculateWeight(x: Double): Double; virtual; abstract;

    property radius: Double read GetRadius write SetRadius;
  end;

  TAggImageFilterLUT = class
  private
    FRadius: Double;
    FDiameter: Cardinal;
    FStart: Integer;

    FWeightArray: PInt16;
    FMaxSize: Cardinal;
    procedure ReallocLut(ARadius: Double);

    function GetRadius: Double;
    function GetDiameter: Cardinal;
    function GetStart: Integer;
  protected
    procedure Normalize;
  public
    constructor Create; overload; virtual;
    constructor Create(Filter: TAggCustomImageFilter; Normalization: Boolean = True); overload; virtual;
    destructor Destroy; override;

    procedure Calculate(Filter: TAggCustomImageFilter; Normalization: Boolean = True);

    function WeightArray: PInt16;

    property radius: Double read GetRadius;
    property Diameter: Cardinal read GetDiameter;
    property Start: Integer read GetStart;
  end;

  TAggImageFilter = class(TAggImageFilterLUT)
  private
    FFilterFunction: TAggCustomImageFilter;
  public
    constructor Create(Filter: TAggCustomImageFilter; Normalization: Boolean = True); override;
  end;

  TAggImageFilterBilinear = class(TAggCustomImageFilter)
  public
    function GetRadius: Double; override;
    function CalculateWeight(x: Double): Double; override;
  end;

  TAggImageFilterHanning = class(TAggCustomImageFilter)
  public
    function GetRadius: Double; override;
    function CalculateWeight(x: Double): Double; override;
  end;

  TAggImageFilterHamming = class(TAggCustomImageFilter)
  public
    function GetRadius: Double; override;
    function CalculateWeight(x: Double): Double; override;
  end;

  TAggImageFilterHermite = class(TAggCustomImageFilter)
  public
    function GetRadius: Double; override;
    function CalculateWeight(x: Double): Double; override;
  end;

  TAggImageFilterQuadric = class(TAggCustomImageFilter)
  public
    function GetRadius: Double; override;
    function CalculateWeight(x: Double): Double; override;
  end;

  TAggImageFilterBicubic = class(TAggCustomImageFilter)
  public
    function Pow3(x: Double): Double;

    function GetRadius: Double; override;
    function CalculateWeight(x: Double): Double; override;
  end;

  TAggImageFilterKaiser = class(TAggCustomImageFilter)
  private
    a, I0a, Epsilon: Double;
  public
    constructor Create(b: Double = 6.33);

    function GetRadius: Double; override;
    function CalculateWeight(x: Double): Double; override;

    function Bessel_i0(x: Double): Double;
  end;

  TAggImageFilterCatrom = class(TAggCustomImageFilter)
  public
    function GetRadius: Double; override;
    function CalculateWeight(x: Double): Double; override;
  end;

  TAggImageFilterMitchell = class(TAggCustomImageFilter)
  private
    P0, p2, p3, Q0, q1, q2, Q3: Double;
  public
    constructor Create(b: Double = 1 / 3; c: Double = 1 / 3);

    function GetRadius: Double; override;
    function CalculateWeight(x: Double): Double; override;
  end;

  TAggImageFilterSpline16 = class(TAggCustomImageFilter)
  public
    function GetRadius: Double; override;
    function CalculateWeight(x: Double): Double; override;
  end;

  TAggImageFilterSpline36 = class(TAggCustomImageFilter)
  public
    function GetRadius: Double; override;
    function CalculateWeight(x: Double): Double; override;
  end;

  TAggImageFilterGaussian = class(TAggCustomImageFilter)
  public
    function GetRadius: Double; override;
    function CalculateWeight(x: Double): Double; override;
  end;

  TAggImageFilterBessel = class(TAggCustomImageFilter)
  public
    function GetRadius: Double; override;
    function CalculateWeight(x: Double): Double; override;
  end;

  TAggImageFilterSinc = class(TAggCustomImageFilter)
  private
    FRadius: Double;
  public
    constructor Create(r: Double); overload; virtual;

    function GetRadius: Double; override;
    function CalculateWeight(x: Double): Double; override;

    procedure SetRadius(r: Double); override;
  end;

  TAggImageFilterLanczos = class(TAggCustomImageFilter)
  private
    FRadius: Double;
  public
    constructor Create(r: Double); overload; virtual;

    function GetRadius: Double; override;
    function CalculateWeight(x: Double): Double; override;

    procedure SetRadius(r: Double); override;
  end;

  TAggImageFilterBlackman = class(TAggCustomImageFilter)
  private
    FRadius: Double;
  public
    constructor Create(r: Double); overload; virtual;

    function GetRadius: Double; override;
    function CalculateWeight(x: Double): Double; override;

    procedure SetRadius(r: Double); override;
  end;

  TAggImageFilterSinc36 = class(TAggImageFilterSinc)
  public
    constructor Create; override;
  end;

  TAggImageFilterSinc64 = class(TAggImageFilterSinc)
  public
    constructor Create; override;
  end;

  TAggImageFilterSinc100 = class(TAggImageFilterSinc)
  public
    constructor Create; override;
  end;

  TAggImageFilterSinc144 = class(TAggImageFilterSinc)
  public
    constructor Create; override;
  end;

  TAggImageFilterSinc196 = class(TAggImageFilterSinc)
  public
    constructor Create; override;
  end;

  TAggImageFilterSinc256 = class(TAggImageFilterSinc)
  public
    constructor Create; override;
  end;

  TAggImageFilterLanczos36 = class(TAggImageFilterLanczos)
  public
    constructor Create; override;
  end;

  TAggImageFilterLanczos64 = class(TAggImageFilterLanczos)
  public
    constructor Create; override;
  end;

  TAggImageFilterLanczos100 = class(TAggImageFilterLanczos)
  public
    constructor Create; override;
  end;

  TAggImageFilterLanczos144 = class(TAggImageFilterLanczos)
  public
    constructor Create; override;
  end;

  TAggImageFilterLanczos196 = class(TAggImageFilterLanczos)
  public
    constructor Create; override;
  end;

  TAggImageFilterLanczos256 = class(TAggImageFilterLanczos)
  public
    constructor Create; override;
  end;

  TAggImageFilterBlackman36 = class(TAggImageFilterBlackman)
  public
    constructor Create; override;
  end;

  TAggImageFilterBlackman64 = class(TAggImageFilterBlackman)
  public
    constructor Create; override;
  end;

  TAggImageFilterBlackman100 = class(TAggImageFilterBlackman)
  public
    constructor Create; override;
  end;

  TAggImageFilterBlackman144 = class(TAggImageFilterBlackman)
  public
    constructor Create; override;
  end;

  TAggImageFilterBlackman196 = class(TAggImageFilterBlackman)
  public
    constructor Create; override;
  end;

  TAggImageFilterBlackman256 = class(TAggImageFilterBlackman)
  public
    constructor Create; override;
  end;

implementation


{ TAggCustomImageFilter }

constructor TAggCustomImageFilter.Create;
begin
end;

procedure TAggCustomImageFilter.SetRadius(r: Double);
begin
end;

{ TAggImageFilterLUT }

constructor TAggImageFilterLUT.Create;
begin
  FWeightArray := nil;
  FMaxSize := 0;
end;

constructor TAggImageFilterLUT.Create(Filter: TAggCustomImageFilter;
  Normalization: Boolean = True);
begin
  FWeightArray := nil;
  FMaxSize := 0;

  Calculate(Filter, Normalization);
end;

destructor TAggImageFilterLUT.Destroy;
begin
  AggFreeMem(Pointer(FWeightArray), FMaxSize * SizeOf(Int16));
  inherited;
end;

procedure TAggImageFilterLUT.Calculate(Filter: TAggCustomImageFilter;
  Normalization: Boolean = True);
var
  r, x, y: Double;
  i, Pivot, stop: Cardinal;
begin
  r := Filter.radius;

  ReallocLut(r);

  Pivot := Diameter shl (CAggImageSubpixelShift - 1);

  i := 0;

  while i < Pivot do
    begin
      x := i / CAggImageSubpixelSize;
      y := Filter.CalculateWeight(x);

      PInt16(PtrComp(FWeightArray) + (Pivot + i) * SizeOf(Int16))^ :=
        Int16(Trunc(y * CAggImageFilterSize + 0.5));

      PInt16(PtrComp(FWeightArray) + (Pivot - i) * SizeOf(Int16))^ :=
        PInt16(PtrComp(FWeightArray) + (Pivot + i) * SizeOf(Int16))^;

      inc(i);
    end;

  stop := (Diameter shl CAggImageSubpixelShift) - 1;

  PInt16(FWeightArray)^ := PInt16(PtrComp(FWeightArray) +
    stop * SizeOf(Int16))^;

  if Normalization then
      Normalize;
end;

function TAggImageFilterLUT.GetRadius: Double;
begin
  Result := FRadius;
end;

function TAggImageFilterLUT.GetDiameter: Cardinal;
begin
  Result := FDiameter;
end;

function TAggImageFilterLUT.GetStart: Integer;
begin
  Result := FStart;
end;

function TAggImageFilterLUT.WeightArray: PInt16;
begin
  Result := FWeightArray;
end;

// This function normalizes integer values and corrects the rounding
// errors. It doesn't do anything with the source floating point values
// (FWeightArrayDouble), it corrects only integers according to the rule
// of 1.0 which means that any sum of pixel weights must be equal to 1.0.
// So, the filter function must produce a graph of the proper shape.
procedure TAggImageFilterLUT.Normalize;
var
  k: Double;

  i, j, idx, Pivot, stop: Cardinal;

  Flip, Sum, IncValue, v: Integer;
begin
  Flip := 1;
  i := 0;

  while i < CAggImageSubpixelSize do
    begin
      repeat
        Sum := 0;

        for j := 0 to FDiameter - 1 do
            inc(Sum, PInt16(PtrComp(FWeightArray) + (j * CAggImageSubpixelSize +
            i) * SizeOf(Int16))^);

        if Sum = CAggImageFilterSize then
            Break;

        k := CAggImageFilterSize / Sum;
        Sum := 0;

        for j := 0 to FDiameter - 1 do
          begin
            PInt16(PtrComp(FWeightArray) + (j * CAggImageSubpixelSize + i) *
              SizeOf(Int16))^ :=
              Int16(Trunc(PInt16(PtrComp(FWeightArray) +
              (j * CAggImageSubpixelSize + i) * SizeOf(Int16))^ * k));

            inc(Sum, PInt16(PtrComp(FWeightArray) + (j * CAggImageSubpixelSize +
              i) * SizeOf(Int16))^);
          end;

        dec(Sum, CAggImageFilterSize);

        if Sum > 0 then
            IncValue := -1
        else
            IncValue := 1;

        j := 0;

        while (j < FDiameter) and (Sum <> 0) do
          begin
            Flip := Flip xor 1;

            if Flip <> 0 then
                idx := FDiameter div 2 + j div 2
            else
                idx := FDiameter div 2 - j div 2;

            v := PInt16(PtrComp(FWeightArray) + (idx * CAggImageSubpixelSize + i)
              * SizeOf(Int16))^;

            if v < CAggImageFilterSize then
              begin
                inc(PInt16(PtrComp(FWeightArray) + (idx * CAggImageSubpixelSize +
                  i) * SizeOf(Int16))^, IncValue);

                inc(Sum, IncValue);
              end;

            inc(j);
          end;

      until False;

      inc(i);
    end;

  Pivot := FDiameter shl (CAggImageSubpixelShift - 1);

  for i := 0 to Pivot - 1 do
      PInt16(PtrComp(FWeightArray) + (Pivot + i) * SizeOf(Int16))^ :=
      PInt16(PtrComp(FWeightArray) + (Pivot - i) * SizeOf(Int16))^;

  stop := (Diameter shl CAggImageSubpixelShift) - 1;

  PInt16(FWeightArray)^ := PInt16(PtrComp(FWeightArray) +
    stop * SizeOf(Int16))^;
end;

procedure TAggImageFilterLUT.ReallocLut(ARadius: Double);
var
  Size: Cardinal;
begin
  FRadius := ARadius;
  FDiameter := Cardinal(Trunc(Ceil(ARadius))) * 2;
  FStart := -Integer(FDiameter div 2 - 1);

  Size := FDiameter shl CAggImageSubpixelShift;

  if Size > FMaxSize then
    begin
      AggFreeMem(Pointer(FWeightArray), FMaxSize * SizeOf(Int16));
      AggGetMem(Pointer(FWeightArray), Size * SizeOf(Int16));

      FMaxSize := Size;
    end;
end;

{ TAggImageFilter }

constructor TAggImageFilter.Create(Filter: TAggCustomImageFilter;
  Normalization: Boolean = True);
begin
  inherited Create;
  FFilterFunction := Filter;
  Calculate(FFilterFunction, Normalization);
end;

{ TAggImageFilterBilinear }

function TAggImageFilterBilinear.GetRadius: Double;
begin
  Result := 1.0;
end;

function TAggImageFilterBilinear.CalculateWeight;
begin
  Result := 1.0 - x;
end;

{ TAggImageFilterHanning }

function TAggImageFilterHanning.GetRadius: Double;
begin
  Result := 1.0;
end;

function TAggImageFilterHanning.CalculateWeight;
begin
  Result := 0.5 + 0.5 * Cos(pi * x);
end;

{ TAggImageFilterHamming }

function TAggImageFilterHamming.GetRadius: Double;
begin
  Result := 1.0;
end;

function TAggImageFilterHamming.CalculateWeight;
begin
  Result := 0.54 + 0.46 * Cos(pi * x);
end;

{ TAggImageFilterHermite }

function TAggImageFilterHermite.GetRadius: Double;
begin
  Result := 1.0;
end;

function TAggImageFilterHermite.CalculateWeight;
begin
  Result := (2.0 * x - 3.0) * x * x + 1.0;
end;

{ TAggImageFilterQuadric }

function TAggImageFilterQuadric.GetRadius: Double;
begin
  Result := 1.5;
end;

function TAggImageFilterQuadric.CalculateWeight;
var
  t: Double;

begin
  if x < 0.5 then
      Result := 0.75 - x * x
  else if x < 1.5 then
    begin
      t := x - 1.5;

      Result := 0.5 * t * t;

    end
  else
      Result := 0.0;
end;

{ TAggImageFilterBicubic }

function TAggImageFilterBicubic.Pow3;
begin
  if x <= 0.0 then
      Result := 0.0
  else
      Result := x * x * x;
end;

function TAggImageFilterBicubic.GetRadius: Double;
begin
  Result := 2.0;
end;

function TAggImageFilterBicubic.CalculateWeight;
begin
  Result := (1.0 / 6.0) * (Pow3(x + 2) - 4 * Pow3(x + 1) + 6 * Pow3(x) - 4 *
    Pow3(x - 1));
end;

{ TAggImageFilterKaiser }

constructor TAggImageFilterKaiser.Create;
begin
  a := b;

  Epsilon := 1E-12;

  I0a := 1.0 / Bessel_i0(b);
end;

function TAggImageFilterKaiser.GetRadius: Double;
begin
  Result := 1.0;
end;

function TAggImageFilterKaiser.CalculateWeight;
begin
  Result := Bessel_i0(a * Sqrt(1.0 - x * x)) * I0a;
end;

function TAggImageFilterKaiser.Bessel_i0(x: Double): Double;
var
  i: Integer;
  Sum, y, t: Double;
begin
  Sum := 1;

  y := Sqr(0.5 * x);
  t := y;
  i := 2;

  while t > Epsilon do
    begin
      Sum := Sum + t;

      t := t * (y / (i * i));

      inc(i);
    end;

  Result := Sum;
end;

{ TAggImageFilterCatrom }

function TAggImageFilterCatrom.GetRadius: Double;
begin
  Result := 2.0;
end;

function TAggImageFilterCatrom.CalculateWeight;
begin
  if x < 1.0 then
      Result := 0.5 * (2.0 + x * x * (-5.0 + x * 3.0))
  else if x < 2.0 then
      Result := 0.5 * (4.0 + x * (-8.0 + x * (5.0 - x)))
  else
      Result := 0.0;
end;

{ TAggImageFilterMitchell }

constructor TAggImageFilterMitchell.Create;
begin
  P0 := (6.0 - 2.0 * b) / 6.0;
  p2 := (-18.0 + 12.0 * b + 6.0 * c) / 6.0;
  p3 := (12.0 - 9.0 * b - 6.0 * c) / 6.0;
  Q0 := (8.0 * b + 24.0 * c) / 6.0;
  q1 := (-12.0 * b - 48.0 * c) / 6.0;
  q2 := (6.0 * b + 30.0 * c) / 6.0;
  Q3 := (-b - 6.0 * c) / 6.0;
end;

function TAggImageFilterMitchell.GetRadius: Double;
begin
  Result := 2.0;
end;

function TAggImageFilterMitchell.CalculateWeight;
begin
  if x < 1.0 then
      Result := P0 + x * x * (p2 + x * p3)
  else if x < 2.0 then
      Result := Q0 + x * (q1 + x * (q2 + x * Q3))
  else
      Result := 0.0;
end;

{ TAggImageFilterSpline16 }

function TAggImageFilterSpline16.GetRadius: Double;
begin
  Result := 2.0;
end;

function TAggImageFilterSpline16.CalculateWeight;
begin
  if x < 1.0 then
      Result := ((x - 9.0 / 5.0) * x - 1.0 / 5.0) * x + 1.0
  else
      Result := ((-1.0 / 3.0 * (x - 1) + 4.0 / 5.0) * (x - 1) - 7.0 / 15.0)
      * (x - 1);
end;

{ TAggImageFilterSpline36 }

function TAggImageFilterSpline36.GetRadius: Double;
begin
  Result := 3.0;
end;

function TAggImageFilterSpline36.CalculateWeight;
begin
  if x < 1.0 then
      Result := ((13.0 / 11.0 * x - 453.0 / 209.0) * x - 3.0 / 209.0) * x + 1.0
  else if x < 2.0 then
      Result := ((-6.0 / 11.0 * (x - 1) + 270.0 / 209.0) * (x - 1) - 156.0 /
      209.0) * (x - 1)
  else
      Result := ((1.0 / 11.0 * (x - 2) - 45.0 / 209.0) * (x - 2) + 26.0 / 209.0)
      * (x - 2);
end;

{ TAggImageFilterGaussian }

function TAggImageFilterGaussian.GetRadius: Double;
begin
  Result := 2.0;
end;

function TAggImageFilterGaussian.CalculateWeight;
begin
  Result := Exp(-2.0 * x * x) * Sqrt(2.0 / pi);
end;

{ TAggImageFilterBessel }

function TAggImageFilterBessel.GetRadius: Double;
begin
  Result := 3.2383;
end;

function TAggImageFilterBessel.CalculateWeight;
begin
  if x = 0.0 then
      Result := pi / 4.0
  else
      Result := Besj(pi * x, 1) / (2.0 * x);
end;

{ TAggImageFilterSinc }

constructor TAggImageFilterSinc.Create(r: Double);
begin
  if r < 2.0 then
      FRadius := 2.0
  else
      FRadius := r;
end;

function TAggImageFilterSinc.GetRadius: Double;
begin
  Result := FRadius;
end;

function TAggImageFilterSinc.CalculateWeight;
begin
  if x = 0.0 then
      Result := 1.0
  else
    begin
      x := x * pi;

      Result := Sin(x) / x;
    end;
end;

procedure TAggImageFilterSinc.SetRadius;
begin
  if r < 2.0 then
      FRadius := 2.0
  else
      FRadius := r;
end;

{ TAggImageFilterLanczos }

constructor TAggImageFilterLanczos.Create(r: Double);
begin
  if r < 2.0 then
      FRadius := 2.0
  else
      FRadius := r;
end;

function TAggImageFilterLanczos.GetRadius: Double;
begin
  Result := FRadius;
end;

function TAggImageFilterLanczos.CalculateWeight;
var
  Xr: Double;

begin
  if x = 0.0 then
      Result := 1.0
  else if x > FRadius then
      Result := 0.0
  else
    begin
      x := x * pi;
      Xr := x / FRadius;

      Result := (Sin(x) / x) * (Sin(Xr) / Xr);
    end;
end;

procedure TAggImageFilterLanczos.SetRadius;
begin
  if r < 2.0 then
      FRadius := 2.0
  else
      FRadius := r;
end;

{ TAggImageFilterBlackman }

constructor TAggImageFilterBlackman.Create(r: Double);
begin
  if r < 2.0 then
      FRadius := 2.0
  else
      FRadius := r;
end;

function TAggImageFilterBlackman.GetRadius: Double;
begin
  Result := FRadius;
end;

function TAggImageFilterBlackman.CalculateWeight;
var
  Xr: Double;

begin
  if x = 0.0 then
      Result := 1.0
  else if x > FRadius then
      Result := 0.0
  else
    begin
      x := x * pi;
      Xr := x / FRadius;

      Result := (Sin(x) / x) * (0.42 + 0.5 * Cos(Xr) + 0.08 * Cos(2 * Xr));
    end;
end;

procedure TAggImageFilterBlackman.SetRadius;
begin
  if r < 2.0 then
      FRadius := 2.0
  else
      FRadius := r;
end;

{ TAggImageFilterSinc36 }

constructor TAggImageFilterSinc36.Create;
begin
  inherited Create(3.0);
end;

{ TAggImageFilterSinc64 }

constructor TAggImageFilterSinc64.Create;
begin
  inherited Create(4.0);
end;

{ TAggImageFilterSinc100 }

constructor TAggImageFilterSinc100.Create;
begin
  inherited Create(5.0);
end;

{ TAggImageFilterSinc144 }

constructor TAggImageFilterSinc144.Create;
begin
  inherited Create(6.0);
end;

{ TAggImageFilterSinc196 }

constructor TAggImageFilterSinc196.Create;
begin
  inherited Create(7.0);
end;

{ TAggImageFilterSinc256 }

constructor TAggImageFilterSinc256.Create;
begin
  inherited Create(8.0);
end;

{ TAggImageFilterLanczos36 }

constructor TAggImageFilterLanczos36.Create;
begin
  inherited Create(3.0);
end;

{ TAggImageFilterLanczos64 }

constructor TAggImageFilterLanczos64.Create;
begin
  inherited Create(4.0);
end;

{ TAggImageFilterLanczos100 }

constructor TAggImageFilterLanczos100.Create;
begin
  inherited Create(5.0);
end;

{ TAggImageFilterLanczos144 }

constructor TAggImageFilterLanczos144.Create;
begin
  inherited Create(6.0);
end;

{ TAggImageFilterLanczos196 }

constructor TAggImageFilterLanczos196.Create;
begin
  inherited Create(7.0);
end;

{ TAggImageFilterLanczos256 }

constructor TAggImageFilterLanczos256.Create;
begin
  inherited Create(8.0);
end;

{ TAggImageFilterBlackman36 }

constructor TAggImageFilterBlackman36.Create;
begin
  inherited Create(3.0);
end;

{ TAggImageFilterBlackman64 }

constructor TAggImageFilterBlackman64.Create;
begin
  inherited Create(4.0);
end;

{ TAggImageFilterBlackman100 }

constructor TAggImageFilterBlackman100.Create;
begin
  inherited Create(5.0);
end;

{ TAggImageFilterBlackman144 }

constructor TAggImageFilterBlackman144.Create;
begin
  inherited Create(6.0);
end;

{ TAggImageFilterBlackman196 }

constructor TAggImageFilterBlackman196.Create;
begin
  inherited Create(7.0);
end;

{ TAggImageFilterBlackman256 }

constructor TAggImageFilterBlackman256.Create;
begin
  inherited Create(8.0);
end;

end. 
 
 
