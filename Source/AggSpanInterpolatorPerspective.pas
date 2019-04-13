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
unit AggSpanInterpolatorPerspective;

{$DEFINE FPC_DELPHI_MODE}
{$INCLUDE zDefine.inc}
interface
uses
  Math,
  AggBasics,
  AggTransPerspective,
  AggSpanInterpolatorLinear,
  AggDdaLine;

type
  TAggSpanInterpolatorPerspectiveExact = class(TAggSpanInterpolator)
  private
    FTransDir, FTransInv: TAggTransPerspective23;
    FIterator: AggTransPerspective.TAggIteratorX23;
    FScaleX, FScaleY: TAggDda2LineInterpolator;
  public
    constructor Create(SS: Cardinal = 8); overload;

    // Arbitrary quadrangle transformations
    constructor Create(Src, Dst: PQuadDouble; SS: Cardinal = 8); overload;

    // Direct transformations
    constructor Create(x1, y1, x2, y2: Double; Quad: PQuadDouble; SS: Cardinal = 8); overload;
    constructor Create(Rect: TRectDouble; Quad: PQuadDouble; SS: Cardinal = 8); overload;

    // Reverse transformations
    constructor Create(Quad: PQuadDouble; x1, y1, x2, y2: Double; SS: Cardinal = 8); overload;
    constructor Create(Quad: PQuadDouble; Rect: TRectDouble; SS: Cardinal = 8); overload;

    destructor Destroy; override;

    // Set the transformations using two arbitrary quadrangles.
    procedure QuadToQuad(Src, Dst: PQuadDouble);

    // Set the direct transformations, i.e., rectangle -> quadrangle
    procedure RectToQuad(x1, y1, x2, y2: Double; Quad: PQuadDouble); overload;
    procedure RectToQuad(Rect: TRectDouble; Quad: PQuadDouble); overload;

    // Set the reverse transformations, i.e., quadrangle -> rectangle
    procedure QuadToRect(Quad: PQuadDouble; x1, y1, x2, y2: Double); overload;
    procedure QuadToRect(Quad: PQuadDouble; Rect: TRectDouble); overload;

    // Check if the equations were solved successfully
    function IsValid: Boolean;

    // Span interpolator interface
    procedure SetBegin(x, y: Double; Len: Cardinal); override;

    procedure Resynchronize(XE, Ye: Double; Len: Cardinal); override;

    procedure IncOperator; override;
    procedure Coordinates(x, y: PInteger); override;
    procedure Coordinates(var x, y: Integer); override;

    procedure LocalScale(x, y: PInteger); override;

    procedure Transform(x, y: PDouble);
  end;

  TAggSpanInterpolatorPerspectiveLerp = class(TAggSpanInterpolator)
  private
    FTransDir, FTransInv: TAggTransPerspective23;

    FCoordX, FCoordY, FScaleX, FScaleY: TAggDda2LineInterpolator;
    function GetIsValid: Boolean;
  public
    constructor Create(SS: Cardinal = 8); overload;

    // Arbitrary quadrangle transformations
    constructor Create(Src, Dst: PQuadDouble; SS: Cardinal = 8); overload;

    // Direct transformations
    constructor Create(x1, y1, x2, y2: Double; Quad: PQuadDouble; SS: Cardinal = 8); overload;

    // Reverse transformations
    constructor Create(Quad: PQuadDouble; x1, y1, x2, y2: Double; SS: Cardinal = 8); overload;
    constructor Create(Quad: PQuadDouble; Rect: TRectDouble; SS: Cardinal = 8); overload;

    destructor Destroy; override;

    // Set the transformations using two arbitrary quadrangles.
    procedure QuadToQuad(Src, Dst: PQuadDouble);

    // Set the direct transformations, i.e., rectangle -> quadrangle
    procedure RectToQuad(x1, y1, x2, y2: Double; Quad: PQuadDouble); overload;
    procedure RectToQuad(Rect: TRectDouble; Quad: PQuadDouble); overload;

    // Set the reverse transformations, i.e., quadrangle -> rectangle
    procedure QuadToRect(Quad: PQuadDouble; x1, y1, x2, y2: Double); overload;
    procedure QuadToRect(Quad: PQuadDouble; Rect: TRectDouble); overload;

    // Span interpolator interface
    procedure SetBegin(x, y: Double; Len: Cardinal); override;

    procedure Resynchronize(XE, Ye: Double; Len: Cardinal); override;

    procedure IncOperator; override;
    procedure Coordinates(x, y: PInteger); override;
    procedure Coordinates(var x, y: Integer); override;

    procedure LocalScale(x, y: PInteger); override;

    procedure Transform(x, y: PDouble);

    // Check if the equations were solved successfully
    property IsValid: Boolean read GetIsValid;
  end;

implementation


{ TAggSpanInterpolatorPerspectiveExact }

constructor TAggSpanInterpolatorPerspectiveExact.Create(SS: Cardinal = 8);
begin
  inherited Create(SS);

  FTransDir := TAggTransPerspective23.Create;
  FTransInv := TAggTransPerspective23.Create;
end;

constructor TAggSpanInterpolatorPerspectiveExact.Create(Src, Dst: PQuadDouble;
  SS: Cardinal = 8);
begin
  Create(SS);
  QuadToQuad(Src, Dst);
end;

constructor TAggSpanInterpolatorPerspectiveExact.Create(x1, y1, x2, y2: Double;
  Quad: PQuadDouble; SS: Cardinal = 8);
begin
  Create(SS);
  RectToQuad(x1, y1, x2, y2, Quad);
end;

constructor TAggSpanInterpolatorPerspectiveExact.Create(Rect: TRectDouble;
  Quad: PQuadDouble; SS: Cardinal = 8);
begin
  Create(SS);
  RectToQuad(Rect, Quad);
end;

constructor TAggSpanInterpolatorPerspectiveExact.Create(Quad: PQuadDouble;
  x1, y1, x2, y2: Double; SS: Cardinal = 8);
begin
  Create(SS);
  QuadToRect(Quad, x1, y1, x2, y2);
end;

constructor TAggSpanInterpolatorPerspectiveExact.Create(Quad: PQuadDouble;
  Rect: TRectDouble; SS: Cardinal = 8);
begin
  Create(SS);
  QuadToRect(Quad, Rect);
end;

destructor TAggSpanInterpolatorPerspectiveExact.Destroy;
begin
  FTransDir.Free;
  FTransInv.Free;

  inherited;
end;

procedure TAggSpanInterpolatorPerspectiveExact.QuadToQuad(Src,
  Dst: PQuadDouble);
begin
  FTransDir.QuadToQuad(Src, Dst);
  FTransInv.QuadToQuad(Dst, Src);
end;

procedure TAggSpanInterpolatorPerspectiveExact.RectToQuad(x1, y1, x2, y2: Double;
  Quad: PQuadDouble);
var
  Src: TQuadDouble;
begin
  Src := QuadDouble(RectDouble(x1, y1, x2, y2));
  QuadToQuad(@Src, Quad);
end;

procedure TAggSpanInterpolatorPerspectiveExact.RectToQuad(Rect: TRectDouble;
  Quad: PQuadDouble);
var
  Src: TQuadDouble;
begin
  Src := QuadDouble(Rect);
  QuadToQuad(@Src, Quad);
end;

procedure TAggSpanInterpolatorPerspectiveExact.QuadToRect(Quad: PQuadDouble;
  x1, y1, x2, y2: Double);
var
  Dst: TQuadDouble;
begin
  Dst := QuadDouble(RectDouble(x1, y1, x2, y2));
  QuadToQuad(Quad, @Dst);
end;

procedure TAggSpanInterpolatorPerspectiveExact.QuadToRect(Quad: PQuadDouble;
  Rect: TRectDouble);
var
  Dst: TQuadDouble;
begin
  Dst := QuadDouble(Rect);
  QuadToQuad(Quad, @Dst);
end;

function TAggSpanInterpolatorPerspectiveExact.IsValid;
begin
  Result := FTransDir.IsValid;
end;

procedure TAggSpanInterpolatorPerspectiveExact.SetBegin;
var
  Delta, Temp: TPointDouble;
  TempDelta: Double;
  Sx1, Sy1, Sx2, Sy2: Integer;
begin
  FIterator := FTransDir.GetBegin(x, y, 1.0);

  Temp.x := FIterator.x;
  Temp.y := FIterator.y;

  TempDelta := 1 / CAggSubpixelSize;

  Delta.x := Temp.x + TempDelta;
  Delta.y := Temp.y;

  FTransInv.Transform(FTransInv, @Delta.x, @Delta.y);

  Delta.x := Delta.x - x;
  Delta.y := Delta.y - y;
  Sx1 := ShrInt32(Trunc(CAggSubpixelSize / Hypot(Delta.x, Delta.y)),
    CAggSubpixelShift);
  Delta.x := Temp.x;
  Delta.y := Temp.y + TempDelta;

  FTransInv.Transform(FTransInv, @Delta.x, @Delta.y);

  Delta.x := Delta.x - x;
  Delta.y := Delta.y - y;
  Sy1 := ShrInt32(Trunc(CAggSubpixelSize / Hypot(Delta.x, Delta.y)),
    CAggSubpixelShift);

  x := x + Len;
  Temp.x := x;
  Temp.y := y;

  FTransDir.Transform(FTransDir, @Temp.x, @Temp.y);

  Delta.x := Temp.x + TempDelta;
  Delta.y := Temp.y;

  FTransInv.Transform(FTransInv, @Delta.x, @Delta.y);

  Delta.x := Delta.x - x;
  Delta.y := Delta.y - y;
  Sx2 := ShrInt32(Trunc(CAggSubpixelSize / Hypot(Delta.x, Delta.y)),
    CAggSubpixelShift);
  Delta.x := Temp.x;
  Delta.y := Temp.y + TempDelta;

  FTransInv.Transform(FTransInv, @Delta.x, @Delta.y);

  Delta.x := Delta.x - x;
  Delta.y := Delta.y - y;
  Sy2 := ShrInt32(Trunc(CAggSubpixelSize / Hypot(Delta.x, Delta.y)),
    CAggSubpixelShift);

  FScaleX.Initialize(Sx1, Sx2, Len);
  FScaleY.Initialize(Sy1, Sy2, Len);
end;

procedure TAggSpanInterpolatorPerspectiveExact.Resynchronize;
var
  Sx1, Sy1, Sx2, Sy2: Integer;
  TempDelta: Double;
  Temp, Delta: TPointDouble;
begin
  // Assume x1,y1 are equal to the ones at the previous end point
  Sx1 := FScaleX.y;
  Sy1 := FScaleY.y;

  // Calculate transformed coordinates at x2,y2
  Temp.x := XE;
  Temp.y := Ye;

  FTransDir.Transform(FTransDir, @Temp.x, @Temp.y);

  TempDelta := 1 / CAggSubpixelSize;

  // Calculate scale by X at x2,y2
  Delta.x := Temp.x + TempDelta;
  Delta.y := Temp.y;

  FTransInv.Transform(FTransInv, @Delta.x, @Delta.y);

  Delta.x := Delta.x - XE;
  Delta.y := Delta.y - Ye;
  Sx2 := ShrInt32(Trunc(CAggSubpixelSize / Hypot(Delta.x, Delta.y)),
    CAggSubpixelShift);

  // Calculate scale by Y at x2,y2
  Delta.x := Temp.x;
  Delta.y := Temp.y + TempDelta;

  FTransInv.Transform(FTransInv, @Delta.x, @Delta.y);

  Delta.x := Delta.x - XE;
  Delta.y := Delta.y - Ye;
  Sy2 := ShrInt32(Trunc(CAggSubpixelSize / Hypot(Delta.x, Delta.y)),
    CAggSubpixelShift);

  // Initialize the interpolators
  FScaleX.Initialize(Sx1, Sx2, Len);
  FScaleY.Initialize(Sy1, Sy2, Len);
end;

procedure TAggSpanInterpolatorPerspectiveExact.IncOperator;
begin
  FIterator.IncOperator;
  FScaleX.PlusOperator;
  FScaleY.PlusOperator;
end;

procedure TAggSpanInterpolatorPerspectiveExact.Coordinates(x, y: PInteger);
begin
  x^ := Trunc(FIterator.x * CAggSubpixelSize + 0.5);
  y^ := Trunc(FIterator.y * CAggSubpixelSize + 0.5);
end;

procedure TAggSpanInterpolatorPerspectiveExact.Coordinates(var x, y: Integer);
begin
  x := Trunc(FIterator.x * CAggSubpixelSize + 0.5);
  y := Trunc(FIterator.y * CAggSubpixelSize + 0.5);
end;

procedure TAggSpanInterpolatorPerspectiveExact.LocalScale(x, y: PInteger);
begin
  x^ := FScaleX.y;
  y^ := FScaleY.y;
end;

procedure TAggSpanInterpolatorPerspectiveExact.Transform(x, y: PDouble);
begin
  FTransDir.Transform(FTransDir, x, y);
end;

{ TAggSpanInterpolatorPerspectiveLerp }

constructor TAggSpanInterpolatorPerspectiveLerp.Create(SS: Cardinal = 8);
begin
  inherited Create(SS);

  FTransDir := TAggTransPerspective23.Create;
  FTransInv := TAggTransPerspective23.Create;
end;

constructor TAggSpanInterpolatorPerspectiveLerp.Create(Src, Dst: PQuadDouble;
  SS: Cardinal = 8);
begin
  inherited Create(SS);

  FTransDir := TAggTransPerspective23.Create;
  FTransInv := TAggTransPerspective23.Create;

  QuadToQuad(Src, Dst);
end;

constructor TAggSpanInterpolatorPerspectiveLerp.Create(x1, y1, x2, y2: Double;
  Quad: PQuadDouble; SS: Cardinal = 8);
begin
  inherited Create(SS);

  FTransDir := TAggTransPerspective23.Create;
  FTransInv := TAggTransPerspective23.Create;

  RectToQuad(x1, y1, x2, y2, Quad);
end;

constructor TAggSpanInterpolatorPerspectiveLerp.Create(Quad: PQuadDouble;
  x1, y1, x2, y2: Double; SS: Cardinal = 8);
begin
  inherited Create(SS);

  FTransDir := TAggTransPerspective23.Create;
  FTransInv := TAggTransPerspective23.Create;

  QuadToRect(Quad, x1, y1, x2, y2);
end;

constructor TAggSpanInterpolatorPerspectiveLerp.Create(Quad: PQuadDouble;
  Rect: TRectDouble; SS: Cardinal);
begin
  inherited Create(SS);

  FTransDir := TAggTransPerspective23.Create;
  FTransInv := TAggTransPerspective23.Create;

  QuadToRect(Quad, Rect.x1, Rect.y1, Rect.x2, Rect.y2);
end;

destructor TAggSpanInterpolatorPerspectiveLerp.Destroy;
begin
  FTransDir.Free;
  FTransInv.Free;
  inherited;
end;

procedure TAggSpanInterpolatorPerspectiveLerp.QuadToQuad(Src, Dst: PQuadDouble);
begin
  FTransDir.QuadToQuad(Src, Dst);
  FTransInv.QuadToQuad(Dst, Src);
end;

procedure TAggSpanInterpolatorPerspectiveLerp.RectToQuad(x1, y1, x2, y2: Double;
  Quad: PQuadDouble);
var
  Src: TQuadDouble;
begin
  Src := QuadDouble(RectDouble(x1, y1, x2, y2));
  QuadToQuad(@Src, Quad);
end;

procedure TAggSpanInterpolatorPerspectiveLerp.RectToQuad(Rect: TRectDouble;
  Quad: PQuadDouble);
var
  Src: TQuadDouble;
begin
  Src := QuadDouble(Rect);
  QuadToQuad(@Src, Quad);
end;

procedure TAggSpanInterpolatorPerspectiveLerp.QuadToRect(Quad: PQuadDouble;
  x1, y1, x2, y2: Double);
var
  Dst: TQuadDouble;
begin
  Dst := QuadDouble(RectDouble(x1, y1, x2, y2));
  QuadToQuad(Quad, @Dst);
end;

procedure TAggSpanInterpolatorPerspectiveLerp.QuadToRect(Quad: PQuadDouble;
  Rect: TRectDouble);
var
  Dst: TQuadDouble;
begin
  Dst := QuadDouble(Rect);
  QuadToQuad(Quad, @Dst);
end;

function TAggSpanInterpolatorPerspectiveLerp.GetIsValid;
begin
  Result := FTransDir.IsValid;
end;

procedure TAggSpanInterpolatorPerspectiveLerp.SetBegin(x, y: Double;
  Len: Cardinal);
var
  xt, Yt, dx, dy, Delta: Double;
  x1, y1, Sx1, Sy1, x2, y2, Sx2, Sy2: Integer;
begin
  // Calculate transformed coordinates at x1,y1
  xt := x;
  Yt := y;

  FTransDir.Transform(FTransDir, @xt, @Yt);

  x1 := Trunc(xt * CAggSubpixelSize);
  y1 := Trunc(Yt * CAggSubpixelSize);

  Delta := 1 / CAggSubpixelSize;

  // Calculate scale by X at x1,y1
  dx := xt + Delta;
  dy := Yt;

  FTransInv.Transform(FTransInv, @dx, @dy);

  dx := dx - x;
  dy := dy - y;
  Sx1 := ShrInt32(Trunc(CAggSubpixelSize / Sqrt(dx * dx + dy * dy)),
    CAggSubpixelShift);

  // Calculate scale by Y at x1,y1
  dx := xt;
  dy := Yt + Delta;

  FTransInv.Transform(FTransInv, @dx, @dy);

  dx := dx - x;
  dy := dy - y;
  Sy1 := ShrInt32(Trunc(CAggSubpixelSize / Sqrt(dx * dx + dy * dy)),
    CAggSubpixelShift);

  // Calculate transformed coordinates at x2,y2
  x := x + Len;
  xt := x;
  Yt := y;

  FTransDir.Transform(FTransDir, @xt, @Yt);

  x2 := Trunc(xt * CAggSubpixelSize);
  y2 := Trunc(Yt * CAggSubpixelSize);

  // Calculate scale by X at x2,y2
  dx := xt + Delta;
  dy := Yt;

  FTransInv.Transform(FTransInv, @dx, @dy);

  dx := dx - x;
  dy := dy - y;
  Sx2 := ShrInt32(Trunc(CAggSubpixelSize / Sqrt(dx * dx + dy * dy)),
    CAggSubpixelShift);

  // Calculate scale by Y at x2,y2
  dx := xt;
  dy := Yt + Delta;

  FTransInv.Transform(FTransInv, @dx, @dy);

  dx := dx - x;
  dy := dy - y;
  Sy2 := ShrInt32(Trunc(CAggSubpixelSize / Sqrt(dx * dx + dy * dy)),
    CAggSubpixelShift);

  // Initialize the interpolators
  FCoordX.Initialize(x1, x2, Len);
  FCoordY.Initialize(y1, y2, Len);
  FScaleX.Initialize(Sx1, Sx2, Len);
  FScaleY.Initialize(Sy1, Sy2, Len);
end;

procedure TAggSpanInterpolatorPerspectiveLerp.Resynchronize;
var
  x1, y1, Sx1, Sy1, x2, y2, Sx2, Sy2: Integer;

  xt, Yt, Delta, dx, dy: Double;

begin
  // Assume x1,y1 are equal to the ones at the previous end point
  x1 := FCoordX.y;
  y1 := FCoordY.y;
  Sx1 := FScaleX.y;
  Sy1 := FScaleY.y;

  // Calculate transformed coordinates at x2,y2
  xt := XE;
  Yt := Ye;

  FTransDir.Transform(FTransDir, @xt, @Yt);

  x2 := Trunc(xt * CAggSubpixelSize);
  y2 := Trunc(Yt * CAggSubpixelSize);

  Delta := 1 / CAggSubpixelSize;

  // Calculate scale by X at x2,y2
  dx := xt + Delta;
  dy := Yt;

  FTransInv.Transform(FTransInv, @dx, @dy);

  dx := dx - XE;
  dy := dy - Ye;
  Sx2 := ShrInt32(Trunc(CAggSubpixelSize / Sqrt(dx * dx + dy * dy)),
    CAggSubpixelShift);

  // Calculate scale by Y at x2,y2
  dx := xt;
  dy := Yt + Delta;

  FTransInv.Transform(FTransInv, @dx, @dy);

  dx := dx - XE;
  dy := dy - Ye;
  Sy2 := ShrInt32(Trunc(CAggSubpixelSize / Sqrt(dx * dx + dy * dy)),
    CAggSubpixelShift);

  // Initialize the interpolators
  FCoordX.Initialize(x1, x2, Len);
  FCoordY.Initialize(y1, y2, Len);
  FScaleX.Initialize(Sx1, Sx2, Len);
  FScaleY.Initialize(Sy1, Sy2, Len);
end;

procedure TAggSpanInterpolatorPerspectiveLerp.IncOperator;
begin
  FCoordX.PlusOperator;
  FCoordY.PlusOperator;
  FScaleX.PlusOperator;
  FScaleY.PlusOperator;
end;

procedure TAggSpanInterpolatorPerspectiveLerp.Coordinates(x, y: PInteger);
begin
  x^ := FCoordX.y;
  y^ := FCoordY.y;
end;

procedure TAggSpanInterpolatorPerspectiveLerp.Coordinates(var x, y: Integer);
begin
  x := FCoordX.y;
  y := FCoordY.y;
end;

procedure TAggSpanInterpolatorPerspectiveLerp.LocalScale;
begin
  x^ := FScaleX.y;
  y^ := FScaleY.y;
end;

procedure TAggSpanInterpolatorPerspectiveLerp.Transform;
begin
  FTransDir.Transform(FTransDir, x, y);
end;

end. 
 
 
