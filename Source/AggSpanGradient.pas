{ ****************************************************************************** }
{ * memory Rasterization with AGG support                                      * }
{ * by QQ 600585@qq.com                                                        * }
{ ****************************************************************************** }
{ * https://zpascal.net                                                        * }
{ * https://github.com/PassByYou888/zAI                                        * }
{ * https://github.com/PassByYou888/ZServer4D                                  * }
{ * https://github.com/PassByYou888/PascalString                               * }
{ * https://github.com/PassByYou888/zRasterization                             * }
{ * https://github.com/PassByYou888/CoreCipher                                 * }
{ * https://github.com/PassByYou888/zSound                                     * }
{ * https://github.com/PassByYou888/zChinese                                   * }
{ * https://github.com/PassByYou888/zExpression                                * }
{ * https://github.com/PassByYou888/zGameWare                                  * }
{ * https://github.com/PassByYou888/zAnalysis                                  * }
{ * https://github.com/PassByYou888/FFMPEG-Header                              * }
{ * https://github.com/PassByYou888/zTranslate                                 * }
{ * https://github.com/PassByYou888/InfiniteIoT                                * }
{ * https://github.com/PassByYou888/FastMD5                                    * }
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
unit AggSpanGradient;

{$DEFINE FPC_DELPHI_MODE}
{$INCLUDE zDefine.inc}
interface
uses
  Math,
  AggBasics,
  AggSpanAllocator,
  AggSpanGenerator,
  AggMath,
  AggArray,
  AggSpanInterpolatorLinear,
  AggColor32;

const
  CAggGradientSubpixelShift = 4;
  CAggGradientSubpixelSize  = 1 shl CAggGradientSubpixelShift;
  CAggGradientSubpixelMask  = CAggGradientSubpixelSize - 1;

type
  TAggCustomGradient = class;

  TAggSpanGradient = class(TAggSpanGenerator)
  private
    FDownscaleShift: Integer;

    FInterpolator: TAggSpanInterpolator;
    FGradientFunction: TAggCustomGradient;
    FColorFunction: TAggCustomArray;

    FD1, FD2: Integer;
    function GetD1: Double;
    function GetD2: Double;
    procedure SetD1(Value: Double);
    procedure SetD2(Value: Double);
  public
    constructor Create(Alloc: TAggSpanAllocator); overload;
    constructor Create(Alloc: TAggSpanAllocator;
      inter: TAggSpanInterpolator; GradientFunction: TAggCustomGradient;
      ColorFunction: TAggCustomArray; AD1, AD2: Double); overload;

    function Generate(x, y: Integer; Len: Cardinal): PAggColor; override;

    property d1: Double read GetD1 write SetD1;
    property d2: Double read GetD2 write SetD2;

    property Interpolator: TAggSpanInterpolator read FInterpolator write FInterpolator;
    property GradientFunction: TAggCustomGradient read FGradientFunction write FGradientFunction;
    property ColorFunction: TAggCustomArray read FColorFunction write FColorFunction;
  end;

  TAggGradientLinearColor = class(TAggPodAutoArray)
  private
    FC1, FC2, FRes: TAggColor;
  protected
    function GetSize: Cardinal; override;
    function ArrayOperator(i: Cardinal): Pointer; override;
  public
    constructor Create(c1, c2: PAggColor; ASize: Cardinal = 256);

    procedure SetColors(c1, c2: PAggColor; ASize: Cardinal = 256);
  end;

  TAggCustomGradient = class
  public
    constructor Create; virtual;
    function Calculate(x, y, d: Integer): Integer; virtual; abstract;
  end;

  TAggCustomGradientClass = class of TAggCustomGradient;

  TAggGradientRadial = class(TAggCustomGradient)
  public
    function Calculate(x, y, d: Integer): Integer; override;
  end;

  // Actually the same as radial. Just for compatibility
  TAggGradientCircle = TAggGradientRadial;

  TAggGradientRadialDouble = class(TAggCustomGradient)
  public
    function Calculate(x, y, d: Integer): Integer; override;
  end;

  TAggGradientRadialFocus = class(TAggCustomGradient)
  private
    FRadius: Integer;
    FFocus: TPointInteger;

    FRadius2, FTrivial: Double;
  public
    constructor Create; overload; override;
    constructor Create(r, fx, fy: Double); overload;

    procedure Init(r, fx, fy: Double);

    function radius: Double;
    function GetFocusX: Double;
    function GetFocusY: Double;

    function Calculate(x, y, d: Integer): Integer; override;

    procedure UpdateValues;
  end;

  TAggGradientRadialFocusExtended = class(TAggCustomGradient)
  private
    FRadius: Integer;
    FFocus: TPointInteger;
    FRadius2, FMul: Double;
    FFocusSquared: TPointDouble;
  public
    constructor Create; overload; override;
    constructor Create(r, fx, fy: Double); overload;

    procedure Init(r, fx, fy: Double);

    function radius: Double;
    function GetFocusX: Double;
    function GetFocusY: Double;

    function Calculate(x, y, d: Integer): Integer; override;

    procedure UpdateValues;
  end;

  TAggGradientX = class(TAggCustomGradient)
  public
    function Calculate(x, y, d: Integer): Integer; override;
  end;

  TAggGradientY = class(TAggCustomGradient)
  public
    function Calculate(x, y, d: Integer): Integer; override;
  end;

  TAggGradientDiamond = class(TAggCustomGradient)
  public
    function Calculate(x, y, d: Integer): Integer; override;
  end;

  TAggGradientXY = class(TAggCustomGradient)
  public
    function Calculate(x, y, d: Integer): Integer; override;
  end;

  TAggGradientSqrtXY = class(TAggCustomGradient)
  public
    function Calculate(x, y, d: Integer): Integer; override;
  end;

  TAggGradientConic = class(TAggCustomGradient)
  public
    function Calculate(x, y, d: Integer): Integer; override;
  end;

  TAggGradientRepeatAdaptor = class(TAggCustomGradient)
  private
    FGradient: TAggCustomGradient;
  public
    constructor Create(Gradient: TAggCustomGradient);

    function Calculate(x, y, d: Integer): Integer; override;
  end;

  TAggGradientReflectAdaptor = class(TAggCustomGradient)
  private
    FGradient: TAggCustomGradient;
  public
    constructor Create(Gradient: TAggCustomGradient);

    function Calculate(x, y, d: Integer): Integer; override;
  end;

implementation


{ TAggSpanGradient }

constructor TAggSpanGradient.Create(Alloc: TAggSpanAllocator);
begin
  inherited Create(Alloc);

  FInterpolator := nil;
  FGradientFunction := nil;
  FColorFunction := nil;
end;

constructor TAggSpanGradient.Create(Alloc: TAggSpanAllocator;
  inter: TAggSpanInterpolator; GradientFunction: TAggCustomGradient;
  ColorFunction: TAggCustomArray; AD1, AD2: Double);
begin
  inherited Create(Alloc);

  FInterpolator := inter;
  FGradientFunction := GradientFunction;
  FColorFunction := ColorFunction;

  FDownscaleShift := FInterpolator.SubpixelShift - CAggGradientSubpixelShift;

  FD1 := Trunc(AD1 * CAggGradientSubpixelSize);
  FD2 := Trunc(AD2 * CAggGradientSubpixelSize);
end;

function TAggSpanGradient.GetD1: Double;
begin
  Result := FD1 / CAggGradientSubpixelSize;
end;

function TAggSpanGradient.GetD2: Double;
begin
  Result := FD2 / CAggGradientSubpixelSize;
end;

procedure TAggSpanGradient.SetD1(Value: Double);
begin
  FD1 := Trunc(Value * CAggGradientSubpixelSize);
end;

procedure TAggSpanGradient.SetD2(Value: Double);
begin
  FD2 := Trunc(Value * CAggGradientSubpixelSize);
end;

function TAggSpanGradient.Generate(x, y: Integer; Len: Cardinal): PAggColor;
var
  Span: PAggColor;
  DD, d: Integer;
begin
  Span := Allocator.Span;

  DD := FD2 - FD1;

  if DD < 1 then
      DD := 1;

  FInterpolator.SetBegin(x + 0.5, y + 0.5, Len);

  repeat
    FInterpolator.Coordinates(@x, @y);

    d := FGradientFunction.Calculate(ShrInt32(x, FDownscaleShift),
      ShrInt32(y, FDownscaleShift), FD2);

    d := ((d - FD1) * FColorFunction.Size) div DD;

    if d < 0 then
        d := 0;

    if d >= FColorFunction.Size then
        d := FColorFunction.Size - 1;

    Span^ := PAggColor(FColorFunction[d])^;

    inc(PtrComp(Span), SizeOf(TAggColor));

    FInterpolator.IncOperator;

    dec(Len);
  until Len = 0;

  Result := Allocator.Span;
end;

{ TAggGradientLinearColor }

constructor TAggGradientLinearColor.Create(c1, c2: PAggColor;
  ASize: Cardinal = 256);
begin
  FC1 := c1^;
  FC2 := c2^;

  FSize := ASize;
end;

function TAggGradientLinearColor.GetSize: Cardinal;
begin
  Result := FSize;
end;

function TAggGradientLinearColor.ArrayOperator(i: Cardinal): Pointer;
begin
  FRes := Gradient(FC1, FC2, i / (FSize - 1));
  Result := @FRes;
end;

procedure TAggGradientLinearColor.SetColors(c1, c2: PAggColor;
  ASize: Cardinal = 256);
begin
  FC1 := c1^;
  FC2 := c2^;

  FSize := Size;
end;

{ TAggCustomGradient }

constructor TAggCustomGradient.Create;
begin
end;

{ TAggGradientRadial }

function TAggGradientRadial.Calculate(x, y, d: Integer): Integer;
begin
  Result := FastSqrt(x * x + y * y);
end;

{ TAggGradientRadialDouble }

function TAggGradientRadialDouble.Calculate(x, y, d: Integer): Integer;
begin
  Result := Trunc(Hypot(x, y));
end;

{ TAggGradientRadialFocus }

constructor TAggGradientRadialFocus.Create;
begin
  FRadius := 100 * CAggGradientSubpixelSize;
  FFocus := PointInteger(0);

  UpdateValues;
end;

constructor TAggGradientRadialFocus.Create(r, fx, fy: Double);
begin
  FRadius := Trunc(r * CAggGradientSubpixelSize);
  FFocus.x := Trunc(fx * CAggGradientSubpixelSize);
  FFocus.y := Trunc(fy * CAggGradientSubpixelSize);

  UpdateValues;
end;

procedure TAggGradientRadialFocus.Init;
begin
  FRadius := Trunc(r * CAggGradientSubpixelSize);
  FFocus.x := Trunc(fx * CAggGradientSubpixelSize);
  FFocus.y := Trunc(fy * CAggGradientSubpixelSize);

  UpdateValues;
end;

function TAggGradientRadialFocus.radius;
begin
  Result := FRadius / CAggGradientSubpixelSize;
end;

function TAggGradientRadialFocus.GetFocusX;
begin
  Result := FFocus.x / CAggGradientSubpixelSize;
end;

function TAggGradientRadialFocus.GetFocusY;
begin
  Result := FFocus.y / CAggGradientSubpixelSize;
end;

function TAggGradientRadialFocus.Calculate(x, y, d: Integer): Integer;
var
  Solution: TPointDouble;
  Slope, Yint, a, b, c, det, IntToFocus, CurToFocus: Double;
begin
  // Special case to avoid divide by zero or very near zero
  if x = FFocus.x then
    begin
      Solution := PointDouble(FFocus.x, 0.0);

      if y > FFocus.y then
          Solution.y := Solution.y + FTrivial
      else
          Solution.y := Solution.y - FTrivial;
    end
  else
    begin
      // Slope of the focus-current line
      Slope := (y - FFocus.y) / (x - FFocus.x);

      // y-intercept of that same line
      Yint := y - (Slope * x);

      // Use the classical quadratic formula to calculate
      // the intersection point
      a := Sqr(Slope) + 1;
      b := 2 * Slope * Yint;
      c := Sqr(Yint) - FRadius2;

      det := Sqrt(Sqr(b) - (4 * a * c));

      Solution.x := -b;

      // Choose the positive or negative root depending
      // on where the X coord lies with respect to the focus.
      if x < FFocus.x then
          Solution.x := Solution.x - det
      else
          Solution.x := Solution.x + det;

      Solution.x := Solution.x / (2 * a);

      // Calculating of Y is trivial
      Solution.y := (Slope * Solution.x) + Yint;
    end;

  // Calculate the percentage (0..1) of the current point along the
  // focus-circumference line and return the normalized (0..d) value
  Solution.x := Solution.x - FFocus.x;
  Solution.y := Solution.y - FFocus.y;

  IntToFocus := Sqr(Solution.x) + Sqr(Solution.y);
  CurToFocus := Sqr(x - FFocus.x) + Sqr(y - FFocus.y);

  Result := Trunc(Sqrt(CurToFocus / IntToFocus) * FRadius);
end;

procedure TAggGradientRadialFocus.UpdateValues;
var
  Dist, r: Double;
  sn, CN: Double;
begin
  // For use in the quadratic equation
  FRadius2 := Sqr(FRadius);

  Dist := Hypot(FFocus.x, FFocus.y);

  // Test if distance from focus to center is greater than the radius
  // For the sake of assurance factor restrict the point to be
  // no further than 99% of the radius.
  r := FRadius * 0.99;

  if Dist > r then
    begin
      // clamp focus to radius
      // x = r cos theta, y = r sin theta
      SinCos(ArcTan2(FFocus.y, FFocus.x), sn, CN);
      FFocus.x := Trunc(r * CN);
      FFocus.y := Trunc(r * sn);
    end;

  // Calculate the solution to be used in the case where x == GetFocusX
  FTrivial := Sqrt(FRadius2 - Sqr(FFocus.x));
end;

{ TAggGradientRadialFocusExtended }

constructor TAggGradientRadialFocusExtended.Create;
begin
  FRadius := 100 * CAggGradientSubpixelSize;
  FFocus := PointInteger(0);

  UpdateValues;
end;

constructor TAggGradientRadialFocusExtended.Create(r, fx, fy: Double);
begin
  FRadius := IntegerRound(r * CAggGradientSubpixelSize);
  FFocus.x := IntegerRound(fx * CAggGradientSubpixelSize);
  FFocus.y := IntegerRound(fy * CAggGradientSubpixelSize);

  UpdateValues;
end;

procedure TAggGradientRadialFocusExtended.Init(r, fx, fy: Double);
begin
  FRadius := IntegerRound(r * CAggGradientSubpixelSize);
  FFocus.x := IntegerRound(fx * CAggGradientSubpixelSize);
  FFocus.y := IntegerRound(fy * CAggGradientSubpixelSize);

  UpdateValues;
end;

function TAggGradientRadialFocusExtended.radius: Double;
begin
  Result := FRadius / CAggGradientSubpixelSize;
end;

function TAggGradientRadialFocusExtended.GetFocusX: Double;
begin
  Result := FFocus.x / CAggGradientSubpixelSize;
end;

function TAggGradientRadialFocusExtended.GetFocusY: Double;
begin
  Result := FFocus.y / CAggGradientSubpixelSize;
end;

function TAggGradientRadialFocusExtended.Calculate(x, y, d: Integer): Integer;
var
  dx, dy, d2, d3: Double;

begin
  dx := x - FFocus.x;
  dy := y - FFocus.y;
  d2 := dx * FFocus.y - dy * FFocus.x;
  d3 := FRadius2 * (Sqr(dx) + Sqr(dy)) - Sqr(d2);

  Result := IntegerRound((dx * FFocus.x + dy * FFocus.y + Sqrt(Abs(d3))) * FMul);
end;

// Calculate the invariant values. In case the focal center
// lies exactly on the Gradient circle the divisor degenerates
// into zero. In this case we just move the focal center by
// one subpixel unit possibly in the direction to the origin (0,0)
// and calculate the values again.
procedure TAggGradientRadialFocusExtended.UpdateValues;
var
  d: Double;
begin
  FRadius2 := Sqr(FRadius);
  FFocusSquared := PointDouble(Sqr(FFocus.x), Sqr(FFocus.y));

  d := (FRadius2 - (FFocusSquared.x + FFocusSquared.y));

  if d = 0 then
    begin
      if FFocus.x <> 0 then
        if FFocus.x < 0 then
            inc(FFocus.x)
        else
            dec(FFocus.x);

      if FFocus.y <> 0 then
        if FFocus.y < 0 then
            inc(FFocus.y)
        else
            dec(FFocus.y);

      FFocusSquared := PointDouble(Sqr(FFocus.x), Sqr(FFocus.y));

      d := (FRadius2 - (FFocusSquared.x + FFocusSquared.y));
    end;

  FMul := FRadius / d;
end;

{ TAggGradientX }

function TAggGradientX.Calculate(x, y, d: Integer): Integer;
begin
  Result := x;
end;

{ TAggGradientY }

function TAggGradientY.Calculate(x, y, d: Integer): Integer;
begin
  Result := y;
end;

{ TAggGradientDiamond }

function TAggGradientDiamond.Calculate(x, y, d: Integer): Integer;
var
  Ax, Ay: Integer;
begin
  Ax := Abs(x);
  Ay := Abs(y);

  if Ax > Ay then
      Result := Ax
  else
      Result := Ay;
end;

{ TAggGradientXY }

function TAggGradientXY.Calculate(x, y, d: Integer): Integer;
begin
  if d = 0 then
      Result := 0
  else
      Result := Abs(x) * Abs(y) div d;
end;

{ TAggGradientSqrtXY }

function TAggGradientSqrtXY.Calculate(x, y, d: Integer): Integer;
begin
  Result := FastSqrt(Abs(x) * Abs(y));
end;

{ TAggGradientConic }

function TAggGradientConic.Calculate(x, y, d: Integer): Integer;
begin
  Result := Trunc(Abs(ArcTan2(y, x)) * d / pi);
end;

{ TAggGradientRepeatAdaptor }

constructor TAggGradientRepeatAdaptor.Create(Gradient: TAggCustomGradient);
begin
  inherited Create;
  FGradient := Gradient;
end;

function TAggGradientRepeatAdaptor.Calculate(x, y, d: Integer): Integer;
begin
  if d = 0 then
      Result := 0
  else
      Result := FGradient.Calculate(x, y, d) mod d;

  if Result < 0 then
      inc(Result, d);
end;

{ TAggGradientReflectAdaptor }

constructor TAggGradientReflectAdaptor.Create(Gradient: TAggCustomGradient);
begin
  inherited Create;
  FGradient := Gradient;
end;

function TAggGradientReflectAdaptor.Calculate(x, y, d: Integer): Integer;
var
  d2: Integer;
begin
  d2 := d shl 1;

  if d2 = 0 then
      Result := 0
  else
      Result := FGradient.Calculate(x, y, d) mod d2;

  if Result < 0 then
      inc(Result, d2);

  if Result >= d then
      Result := d2 - Result;
end;

end. 
 
 
