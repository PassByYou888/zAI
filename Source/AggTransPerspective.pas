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
unit AggTransPerspective;

(*
  ////////////////////////////////////////////////////////////////////////////////
  //                                                                            //
  // Perspective 2D transformations                                             //
  //                                                                            //
  ////////////////////////////////////////////////////////////////////////////////
*)

{$DEFINE FPC_DELPHI_MODE}
{$INCLUDE zDefine.inc}

interface

uses
  Math,
  AggBasics,
  AggMath,
  AggSimulEq,
  AggTransAffine;

type
  PAggIteratorX23 = ^TAggIteratorX23;

  TAggIteratorX23 = record
  public
    Den, DenStep: Double;
    NomStep, Nom: TPointDouble;

    x, y: Double;
  public
    procedure Initialize(TX, TY, Step: Double; M: PDoubleArray8); overload;

    procedure IncOperator;
  end;

  TAggTransPerspective23 = class(TAggTransAffine)
  private
    FValid: Boolean;
    FMatrix: TDoubleArray8;
  public
    constructor Create; override;

    // Arbitrary quadrangle transformations
    constructor Create(Src, Dst: PQuadDouble); overload;

    // Direct transformations
    constructor Create(x1, y1, x2, y2: Double; Quad: PQuadDouble); overload;
    constructor Create(Rect: TRectDouble; Quad: PQuadDouble); overload;

    // Reverse transformations
    constructor Create(Quad: PQuadDouble; x1, y1, x2, y2: Double); overload;
    constructor Create(Quad: PQuadDouble; Rect: TRectDouble); overload;

    // Set the transformations using two arbitrary quadrangles.
    procedure QuadToQuad(Src, Dst: PQuadDouble);

    // Set the direct transformations, i.e., rectangle -> quadrangle
    procedure RectToQuad(x1, y1, x2, y2: Double; Quad: PQuadDouble); overload;
    procedure RectToQuad(Rect: TRectDouble; Quad: PQuadDouble); overload;

    // Set the reverse transformations, i.e., quadrangle -> rectangle
    procedure QuadToRect(Quad: PQuadDouble; x1, y1, x2, y2: Double); overload;
    procedure QuadToRect(Quad: PQuadDouble; Rect: TRectDouble); overload;

    function GetBegin(x, y, Step: Double): TAggIteratorX23;

    // Check if the equations were solved successfully
    property IsValid: Boolean read FValid;
  end;

  PAggQuadrilateral = ^TAggQuadrilateral;
  TAggQuadrilateral = array [0 .. 8] of Double;

  TAggTransPerspective = class;

  TAggIteratorXRecord = record
  public
    Den, DenStep: Double;
    Nom, NomStep: TPointDouble;

    x, y: Double;
  public
    procedure Initialize(x, y, Step: Double; M: TAggTransPerspective); overload;

    procedure OperatorInc;
  end;

  TAggTransPerspective = class(TAggTransAffine)
  private
    SX, Shy, W0, Shx, SY, W1, TX, TY, W2: Double;

    TransformAffine: TAggProcTransform;

    // private
    function GetBegin(x, y, Step: Double): TAggIteratorXRecord;
    procedure InitializeTransforms;
  public
    // ------------------------------------------------------- Construction
    // Identity matrix
    constructor Create; overload;

    // Custom matrix
    constructor Create(v0, v1, v2, v3, v4, V5, V6, V7, V8: Double); overload;

    // Custom matrix from m[9]
    constructor Create(M: PAggQuadrilateral); overload;

    // From affine
    constructor CreateAffine(a: TAggTransAffine);

    // From affine
    constructor Create(p: TAggTransPerspective); overload;

    // Rectangle to quadrilateral
    constructor Create(x1, y1, x2, y2: Double; Quad: PAggQuadrilateral); overload;
    constructor Create(Rect: TRectDouble; Quad: PAggQuadrilateral); overload;

    // Quadrilateral to rectangle
    constructor Create(Quad: PAggQuadrilateral; x1, y1, x2, y2: Double); overload;
    constructor Create(Quad: PAggQuadrilateral; Rect: TRectDouble); overload;

    // Arbitrary quadrilateral transformations
    constructor Create(Src, Dst: PAggQuadrilateral); overload;

    // -------------------------------------- Quadrilateral transformations
    // The arguments are double[8] that are mapped to quadrilaterals:
    // x1,y1, x2,y2, x3,y3, x4,y4
    function QuadToQuad(Qs, Qd: PAggQuadrilateral): Boolean;

    function RectToQuad(x1, y1, x2, y2: Double; q: PAggQuadrilateral): Boolean; overload;
    function RectToQuad(Rect: TRectDouble; q: PAggQuadrilateral): Boolean; overload;

    function QuadToRect(q: PAggQuadrilateral; x1, y1, x2, y2: Double): Boolean; overload;
    function QuadToRect(q: PAggQuadrilateral; Rect: TRectDouble): Boolean; overload;
    function QuadToRect(var q: TAggQuadrilateral; Rect: TRectDouble): Boolean; overload;

    // Map square (0,0,1,1) to the quadrilateral and vice versa
    function SquareToQuad(q: PAggQuadrilateral): Boolean;
    function QuadToSquare(q: PAggQuadrilateral): Boolean;

    // --------------------------------------------------------- Operations
    // Reset - load an identity matrix
    procedure Reset; override;

    // Invert matrix. Returns false in degenerate case
    function Invert: Boolean;

    // Direct transformations operations
    function Translate(x, y: Double): TAggTransPerspective;
    function Rotate(a: Double): TAggTransPerspective;
    function Scale(s: Double): TAggTransPerspective; overload;
    function Scale(x, y: Double): TAggTransPerspective; overload;

    // Multiply the matrix by another one
    function Multiply(a: TAggTransPerspective): TAggTransPerspective;

    // Multiply "m" by "this" and assign the result to "this"
    function PreMultiply(b: TAggTransPerspective): TAggTransPerspective;

    // Multiply matrix to inverse of another one
    function MultiplyInv(M: TAggTransPerspective): TAggTransPerspective;

    // Multiply inverse of "m" by "this" and assign the result to "this"
    function PreMultiplyInv(M: TAggTransPerspective): TAggTransPerspective;

    // Multiply the matrix by another one
    function MultiplyAffine(a: TAggTransAffine): TAggTransPerspective;

    // Multiply "m" by "this" and assign the result to "this"
    function PreMultiplyAffine(b: TAggTransAffine): TAggTransPerspective;

    // Multiply the matrix by inverse of another one
    function MultiplyInvAffine(M: TAggTransAffine): TAggTransPerspective;

    // Multiply inverse of "m" by "this" and assign the result to "this"
    function PreMultiplyInvAffine(M: TAggTransAffine): TAggTransPerspective;

    // --------------------------------------------------------- Load/Store
    procedure StoreTo(M: PAggQuadrilateral);
    function LoadFrom(M: PAggQuadrilateral): TAggTransPerspective;

    // ---------------------------------------------------------- Auxiliary
    function FromAffine(a: TAggTransAffine): TAggTransPerspective;

    function Determinant: Double;
    function DeterminantReciprocal: Double;

    function IsValid(Epsilon: Double = CAggAffineEpsilon): Boolean;
    function IsIdentity(Epsilon: Double = CAggAffineEpsilon): Boolean;
    function IsEqual(M: TAggTransPerspective; Epsilon: Double = CAggAffineEpsilon): Boolean;

    // Determine the major affine parameters. Use with caution
    // considering possible degenerate cases.
    function Scale: Double; overload;
    function Rotation: Double;

    procedure Translation(dx, dy: PDouble);
    procedure Scaling(x, y: PDouble);
    procedure ScalingAbs(x, y: PDouble);
  end;

implementation


{ TAggIteratorX23 }

procedure TAggIteratorX23.Initialize(TX, TY, Step: Double; M: PDoubleArray8);
var
  d: Double;
begin
  Den := M[6] * TX + M[7] * TY + 1;
  DenStep := M[6] * Step;

  Nom.x := M[0] + M[1] * TX + M[2] * TY;
  Nom.y := M[3] + M[4] * TX + M[5] * TY;
  NomStep := PointDouble(M[1] * Step, M[4] * Step);

  d := 1 / Den;
  x := Nom.x * d;
  y := Nom.y * d;
end;

procedure TAggIteratorX23.IncOperator;
var
  d: Double;
begin
  Den := Den + DenStep;
  Nom.x := Nom.x + NomStep.x;
  Nom.y := Nom.y + NomStep.y;

  d := 1 / Den;
  x := Nom.x * d;
  y := Nom.y * d;
end;

procedure PerspectiveTransform23(This: TAggTransPerspective23; x, y: PDouble);
var
  TX, TY, d: Double;
begin
  TX := x^;
  TY := y^;
  d := 1 / (This.FMatrix[6] * TX + This.FMatrix[7] * TY + 1);

  x^ := (This.FMatrix[0] + This.FMatrix[1] * TX + This.FMatrix[2] * TY) * d;
  y^ := (This.FMatrix[3] + This.FMatrix[4] * TX + This.FMatrix[5] * TY) * d;
end;

{ TAggTransPerspective23 }

constructor TAggTransPerspective23.Create;
begin
  inherited Create;

  FValid := False;

  Transform := @PerspectiveTransform23;
end;

constructor TAggTransPerspective23.Create(Src, Dst: PQuadDouble);
begin
  inherited Create;

  QuadToQuad(Src, Dst);

  Transform := @PerspectiveTransform23;
end;

constructor TAggTransPerspective23.Create(x1, y1, x2, y2: Double;
  Quad: PQuadDouble);
begin
  inherited Create;

  RectToQuad(x1, y1, x2, y2, Quad);

  Transform := @PerspectiveTransform23;
end;

constructor TAggTransPerspective23.Create(Rect: TRectDouble; Quad: PQuadDouble);
begin
  inherited Create;

  RectToQuad(Rect, Quad);

  Transform := @PerspectiveTransform23;
end;

constructor TAggTransPerspective23.Create(Quad: PQuadDouble;
  x1, y1, x2, y2: Double);
begin
  inherited Create;

  QuadToRect(Quad, x1, y1, x2, y2);

  Transform := @PerspectiveTransform23;
end;

constructor TAggTransPerspective23.Create(Quad: PQuadDouble; Rect: TRectDouble);
begin
  inherited Create;

  QuadToRect(Quad, Rect);

  Transform := @PerspectiveTransform23;
end;

procedure TAggTransPerspective23.QuadToQuad(Src, Dst: PQuadDouble);
var
  Left: TDoubleMatrix8x8;
  Right: TDoubleMatrix8x1;

  i, ix, iy: Cardinal;
begin
  for i := 0 to 3 do
    begin
      ix := i * 2;
      iy := ix + 1;

      Left[ix, 0] := 1.0;
      Left[ix, 1] := PDouble(PtrComp(Src) + ix * SizeOf(Double))^;
      Left[ix, 2] := PDouble(PtrComp(Src) + iy * SizeOf(Double))^;
      Left[ix, 3] := 0.0;
      Left[ix, 4] := 0.0;
      Left[ix, 5] := 0.0;
      Left[ix, 6] := -PDouble(PtrComp(Src) + ix * SizeOf(Double))^ * PDouble(PtrComp(Dst) + ix * SizeOf(Double))^;
      Left[ix, 7] := -PDouble(PtrComp(Src) + iy * SizeOf(Double))^ * PDouble(PtrComp(Dst) + ix * SizeOf(Double))^;
      Right[ix, 0] := PDouble(PtrComp(Dst) + ix * SizeOf(Double))^;

      Left[iy, 0] := 0.0;
      Left[iy, 1] := 0.0;
      Left[iy, 2] := 0.0;
      Left[iy, 3] := 1.0;
      Left[iy, 4] := PDouble(PtrComp(Src) + ix * SizeOf(Double))^;
      Left[iy, 5] := PDouble(PtrComp(Src) + iy * SizeOf(Double))^;
      Left[iy, 6] := -PDouble(PtrComp(Src) + ix * SizeOf(Double))^ * PDouble(PtrComp(Dst) + iy * SizeOf(Double))^;
      Left[iy, 7] := -PDouble(PtrComp(Src) + iy * SizeOf(Double))^ * PDouble(PtrComp(Dst) + iy * SizeOf(Double))^;
      Right[iy, 0] := PDouble(PtrComp(Dst) + iy * SizeOf(Double))^;
    end;

  FValid := SimulEqSolve(@Left, @Right, @FMatrix, 8, 1);
end;

procedure TAggTransPerspective23.RectToQuad(x1, y1, x2, y2: Double; Quad: PQuadDouble);
var
  Src: TQuadDouble;
begin
  Src := QuadDouble(RectDouble(x1, y1, x2, y2));
  QuadToQuad(@Src, Quad);
end;

procedure TAggTransPerspective23.RectToQuad(Rect: TRectDouble; Quad: PQuadDouble);
var
  Src: TQuadDouble;
begin
  Src := QuadDouble(Rect);
  QuadToQuad(@Src, Quad);
end;

procedure TAggTransPerspective23.QuadToRect(Quad: PQuadDouble; x1, y1, x2, y2: Double);
var
  Dst: TQuadDouble;
begin
  Dst := QuadDouble(RectDouble(x1, y1, x2, y2));
  QuadToQuad(Quad, @Dst);
end;

procedure TAggTransPerspective23.QuadToRect(Quad: PQuadDouble; Rect: TRectDouble);
var
  Dst: TQuadDouble;
begin
  Dst := QuadDouble(Rect);
  QuadToQuad(Quad, @Dst);
end;

function TAggTransPerspective23.GetBegin(x, y, Step: Double): TAggIteratorX23;
begin
  Result.Initialize(x, y, Step, @FMatrix);
end;

{ TAggIteratorXRecord }

procedure TAggIteratorXRecord.Initialize(x, y, Step: Double; M: TAggTransPerspective);
var
  d: Double;
begin
  Den := x * M.W0 + y * M.W1 + M.W2;
  DenStep := M.W0 * Step;

  Nom.x := x * M.SX + y * M.Shx + M.TX;
  Nom.y := x * M.Shy + y * M.SY + M.TY;
  NomStep := PointDouble(Step * M.SX, Step * M.Shy);

  d := 1 / Den;
  Self.x := Nom.x * d;
  Self.y := Nom.y * d;
end;

procedure TAggIteratorXRecord.OperatorInc;
var
  d: Double;
begin
  Den := Den + DenStep;
  Nom := PointDouble(Nom.x + NomStep.x, Nom.y + NomStep.y);

  d := 1 / Den;
  x := Nom.x * d;
  y := Nom.y * d;
end;

// Direct transformation of x and y
procedure TransPerspectiveTransform(This: TAggTransPerspective; Px, Py: PDouble);
var
  x, y, M: Double;
begin
  x := Px^;
  y := Py^;

  try
      M := 1 / (x * This.W0 + y * This.W1 + This.W2);
  except
      M := 0;
  end;

  Px^ := M * (x * This.SX + y * This.Shx + This.TX);
  Py^ := M * (x * This.Shy + y * This.SY + This.TY);
end;

// Direct transformation of x and y, affine part only
procedure TransPerspectiveTransformAffine(This: TAggTransPerspective; x, y: PDouble);
var
  tmp: Double;
begin
  tmp := x^;

  x^ := tmp * This.SX + y^ * This.Shx + This.TX;
  y^ := tmp * This.Shy + y^ * This.SY + This.TY;
end;

// Direct transformation of x and y, 2x2 matrix only, no translation
procedure TransPerspectiveTransform2x2(This: TAggTransPerspective; x, y: PDouble);
var
  tmp: Double;
begin
  tmp := x^;

  x^ := tmp * This.SX + y^ * This.Shx;
  y^ := tmp * This.Shy + y^ * This.SY;
end;

// Inverse transformation of x and y. It works sLow because
// it explicitly inverts the matrix on every call. For massive
// operations it's better to invert() the matrix and then use
// direct transformations.
procedure TransPerspectiveInverseTransform(This: TAggTransPerspective; x, y: PDouble);
var
  t: TAggTransPerspective;
begin
  t := TAggTransPerspective.Create(This);
  try
    if t.Invert then
        t.Transform(t, x, y);
  finally
      t.Free;
  end;
end;

{ TAggTransPerspective }

constructor TAggTransPerspective.Create;
begin
  inherited Create;
  InitializeTransforms;

  SX := 1;
  Shy := 0;
  W0 := 0;
  Shx := 0;
  SY := 1;
  W1 := 0;
  TX := 0;
  TY := 0;
  W2 := 1;
end;

constructor TAggTransPerspective.Create(v0, v1, v2, v3, v4, V5, V6, V7, V8: Double);
begin
  inherited Create;
  InitializeTransforms;

  SX := v0;
  Shy := v1;
  W0 := v2;
  Shx := v3;
  SY := v4;
  W1 := V5;
  TX := V6;
  TY := V7;
  W2 := V8;
end;

constructor TAggTransPerspective.Create(M: PAggQuadrilateral);
begin
  inherited Create;
  InitializeTransforms;

  SX := M[0];
  Shy := M[1];
  W0 := M[2];
  Shx := M[3];
  SY := M[4];
  W1 := M[5];
  TX := M[6];
  TY := M[7];
  W2 := M[8];
end;

constructor TAggTransPerspective.CreateAffine(a: TAggTransAffine);
begin
  inherited Create;
  InitializeTransforms;

  SX := a.M0;
  Shy := a.m1;
  W0 := 0;
  Shx := a.m2;
  SY := a.M3;
  W1 := 0;
  TX := a.M4;
  TY := a.M5;
  W2 := 1;
end;

constructor TAggTransPerspective.Create(p: TAggTransPerspective);
begin
  inherited Create;
  InitializeTransforms;

  SX := p.SX;
  Shy := p.Shy;
  W0 := p.W0;
  Shx := p.Shx;
  SY := p.SY;
  W1 := p.W1;
  TX := p.TX;
  TY := p.TY;
  W2 := p.W2;
end;

constructor TAggTransPerspective.Create(x1, y1, x2, y2: Double; Quad: PAggQuadrilateral);
begin
  inherited Create;
  InitializeTransforms;

  RectToQuad(x1, y1, x2, y2, Quad);
end;

constructor TAggTransPerspective.Create(Quad: PAggQuadrilateral; x1, y1, x2, y2: Double);
begin
  inherited Create;
  InitializeTransforms;

  QuadToRect(Quad, x1, y1, x2, y2);
end;

constructor TAggTransPerspective.Create(Src, Dst: PAggQuadrilateral);
begin
  inherited Create;
  InitializeTransforms;

  QuadToQuad(Src, Dst);
end;

constructor TAggTransPerspective.Create(Rect: TRectDouble; Quad: PAggQuadrilateral);
begin
  inherited Create;
  InitializeTransforms;

  RectToQuad(Rect, Quad);
end;

constructor TAggTransPerspective.Create(Quad: PAggQuadrilateral; Rect: TRectDouble);
begin
  inherited Create;
  InitializeTransforms;

  QuadToRect(Quad, Rect);
end;

procedure TAggTransPerspective.InitializeTransforms;
begin
  Transform := @TransPerspectiveTransform;
  Transform2x2 := @TransPerspectiveTransform2x2;
  InverseTransform := @TransPerspectiveInverseTransform;
  TransformAffine := @TransPerspectiveTransformAffine;
end;

function TAggTransPerspective.QuadToQuad(Qs, Qd: PAggQuadrilateral): Boolean;
var
  p: TAggTransPerspective;
begin
  Result := False;

  if not QuadToSquare(Qs) then
      Exit;

  p := TAggTransPerspective.Create;
  try
    if not p.SquareToQuad(Qd) then
        Exit;

    Multiply(p);
  finally
      p.Free;
  end;

  Result := True;
end;

function TAggTransPerspective.RectToQuad(x1, y1, x2, y2: Double; q: PAggQuadrilateral): Boolean;
var
  Src: TQuadDouble;
begin
  Src := QuadDouble(RectDouble(x1, y1, x2, y2));
  Result := QuadToQuad(@Src, q);
end;

function TAggTransPerspective.RectToQuad(Rect: TRectDouble; q: PAggQuadrilateral): Boolean;
var
  Src: TQuadDouble;
begin
  Src := QuadDouble(Rect);
  Result := QuadToQuad(@Src, q);
end;

function TAggTransPerspective.QuadToRect(q: PAggQuadrilateral; x1, y1, x2, y2: Double): Boolean;
var
  Dst: TQuadDouble;
begin
  Dst := QuadDouble(RectDouble(x1, y1, x2, y2));
  Result := QuadToQuad(q, @Dst);
end;

function TAggTransPerspective.QuadToRect(var q: TAggQuadrilateral; Rect: TRectDouble): Boolean;
var
  Dst: TQuadDouble;
begin
  Dst := QuadDouble(Rect);
  Result := QuadToQuad(@q, @Dst);
end;

function TAggTransPerspective.QuadToRect(q: PAggQuadrilateral; Rect: TRectDouble): Boolean;
var
  Dst: TQuadDouble;
begin
  Dst := QuadDouble(Rect);
  Result := QuadToQuad(q, @Dst);
end;

function TAggTransPerspective.SquareToQuad(q: PAggQuadrilateral): Boolean;
var
  Delta: TPointDouble;
  Dx1, Dy1, Dx2, Dy2, Den, u, v: Double;
begin
  Delta.x := q[0] - q[2] + q[4] - q[6];
  Delta.y := q[1] - q[3] + q[5] - q[7];

  if (Delta.x = 0.0) and (Delta.y = 0.0) then
    begin
      // Affine case (parallelogram)
      SX := q[2] - q[0];
      Shy := q[3] - q[1];
      W0 := 0.0;
      Shx := q[4] - q[2];
      SY := q[5] - q[3];
      W1 := 0.0;
      TX := q[0];
      TY := q[1];
      W2 := 1.0;
    end
  else
    begin
      Dx1 := q[2] - q[4];
      Dy1 := q[3] - q[5];
      Dx2 := q[6] - q[4];
      Dy2 := q[7] - q[5];
      Den := Dx1 * Dy2 - Dx2 * Dy1;

      if Den = 0.0 then
        begin
          // Singular case
          SX := 0.0;
          Shy := 0.0;
          W0 := 0.0;
          Shx := 0.0;
          SY := 0.0;
          W1 := 0.0;
          TX := 0.0;
          TY := 0.0;
          W2 := 0.0;

          Result := False;

          Exit;
        end;

      // General case
      Den := 1 / Den;
      u := (Delta.x * Dy2 - Delta.y * Dx2) * Den;
      v := (Delta.y * Dx1 - Delta.x * Dy1) * Den;

      SX := q[2] - q[0] + u * q[2];
      Shy := q[3] - q[1] + u * q[3];
      W0 := u;
      Shx := q[6] - q[0] + v * q[6];
      SY := q[7] - q[1] + v * q[7];
      W1 := v;
      TX := q[0];
      TY := q[1];
      W2 := 1.0;
    end;

  Result := True;
end;

function TAggTransPerspective.QuadToSquare(q: PAggQuadrilateral): Boolean;
begin
  Result := False;
  if SquareToQuad(q) then
    begin
      Invert;
      Result := True;
    end;
end;

procedure TAggTransPerspective.Reset;
begin
  SX := 1;
  Shy := 0;
  W0 := 0;
  Shx := 0;
  SY := 1;
  W1 := 0;
  TX := 0;
  TY := 0;
  W2 := 1;
end;

function TAggTransPerspective.Invert: Boolean;
var
  D0, d1, d2, d: Double;

  a: TAggTransPerspective;
begin
  D0 := SY * W2 - W1 * TY;
  d1 := W0 * TY - Shy * W2;
  d2 := Shy * W1 - W0 * SY;
  d := SX * D0 + Shx * d1 + TX * d2;

  if d = 0.0 then
    begin
      SX := 0.0;
      Shy := 0.0;
      W0 := 0.0;
      Shx := 0.0;
      SY := 0.0;
      W1 := 0.0;
      TX := 0.0;
      TY := 0.0;
      W2 := 0.0;

      Result := False;

      Exit;
    end;

  d := 1.0 / d;

  a := TAggTransPerspective.Create(TAggTransPerspective(Self));
  try
    SX := d * D0;
    Shy := d * d1;
    W0 := d * d2;
    Shx := d * (a.W1 * a.TX - a.Shx * a.W2);
    SY := d * (a.SX * a.W2 - a.W0 * a.TX);
    W1 := d * (a.W0 * a.Shx - a.SX * a.W1);
    TX := d * (a.Shx * a.TY - a.SY * a.TX);
    TY := d * (a.Shy * a.TX - a.SX * a.TY);
    W2 := d * (a.SX * a.SY - a.Shy * a.Shx);
  finally
      a.Free;
  end;

  Result := True;
end;

function TAggTransPerspective.Translate(x, y: Double): TAggTransPerspective;
begin
  TX := TX + x;
  TY := TY + y;

  Result := Self;
end;

function TAggTransPerspective.Rotate(a: Double): TAggTransPerspective;
var
  Tar: TAggTransAffineRotation;
begin
  Tar := TAggTransAffineRotation.Create(a);
  try
      MultiplyAffine(Tar);
  finally
      Tar.Free;
  end;

  Result := Self;
end;

function TAggTransPerspective.Scale(s: Double): TAggTransPerspective;
var
  Tas: TAggTransAffineScaling;
begin
  Tas := TAggTransAffineScaling.Create(s);
  try
      MultiplyAffine(Tas);
  finally
      Tas.Free;
  end;

  Result := Self;
end;

function TAggTransPerspective.Scale(x, y: Double): TAggTransPerspective;
var
  Tas: TAggTransAffineScaling;
begin
  Tas := TAggTransAffineScaling.Create(x, y);
  try
      MultiplyAffine(Tas);
  finally
      Tas.Free;
  end;

  Result := Self;
end;

function TAggTransPerspective.Multiply(a: TAggTransPerspective): TAggTransPerspective;
var
  b: TAggTransPerspective;
begin
  b := TAggTransPerspective.Create(TAggTransPerspective(Self));
  try
    SX := a.SX * b.SX + a.Shx * b.Shy + a.TX * b.W0;
    Shx := a.SX * b.Shx + a.Shx * b.SY + a.TX * b.W1;
    TX := a.SX * b.TX + a.Shx * b.TY + a.TX * b.W2;
    Shy := a.Shy * b.SX + a.SY * b.Shy + a.TY * b.W0;
    SY := a.Shy * b.Shx + a.SY * b.SY + a.TY * b.W1;
    TY := a.Shy * b.TX + a.SY * b.TY + a.TY * b.W2;
    W0 := a.W0 * b.SX + a.W1 * b.Shy + a.W2 * b.W0;
    W1 := a.W0 * b.Shx + a.W1 * b.SY + a.W2 * b.W1;
    W2 := a.W0 * b.TX + a.W1 * b.TY + a.W2 * b.W2;
  finally
      b.Free;
  end;

  Result := Self;
end;

function TAggTransPerspective.PreMultiply(b: TAggTransPerspective): TAggTransPerspective;
var
  a: TAggTransPerspective;
begin
  a := TAggTransPerspective.Create(TAggTransPerspective(Self));
  try
    SX := a.SX * b.SX + a.Shx * b.Shy + a.TX * b.W0;
    Shx := a.SX * b.Shx + a.Shx * b.SY + a.TX * b.W1;
    TX := a.SX * b.TX + a.Shx * b.TY + a.TX * b.W2;
    Shy := a.Shy * b.SX + a.SY * b.Shy + a.TY * b.W0;
    SY := a.Shy * b.Shx + a.SY * b.SY + a.TY * b.W1;
    TY := a.Shy * b.TX + a.SY * b.TY + a.TY * b.W2;
    W0 := a.W0 * b.SX + a.W1 * b.Shy + a.W2 * b.W0;
    W1 := a.W0 * b.Shx + a.W1 * b.SY + a.W2 * b.W1;
    W2 := a.W0 * b.TX + a.W1 * b.TY + a.W2 * b.W2;
  finally
      a.Free;
  end;

  Result := Self;
end;

function TAggTransPerspective.MultiplyInv(M: TAggTransPerspective): TAggTransPerspective;
var
  t: TAggTransPerspective;
begin
  t := TAggTransPerspective.Create(M);
  try
    t.Invert;
    Result := Multiply(t);
  finally
      t.Free;
  end;

  Result := Self;
end;

function TAggTransPerspective.PreMultiplyInv(M: TAggTransPerspective): TAggTransPerspective;
var
  t: TAggTransPerspective;
begin
  t := TAggTransPerspective.Create(M);
  try
    t.Invert;
    t.Multiply(Self);
  finally
      t.Free;
  end;

  Result := Self;
end;

function TAggTransPerspective.MultiplyAffine(a: TAggTransAffine): TAggTransPerspective;
var
  b: TAggTransPerspective;
begin
  b := TAggTransPerspective.Create(TAggTransPerspective(Self));
  try
    SX := a.M0 * b.SX + a.m2 * b.Shy + a.M4 * b.W0;
    Shx := a.M0 * b.Shx + a.m2 * b.SY + a.M4 * b.W1;
    TX := a.M0 * b.TX + a.m2 * b.TY + a.M4 * b.W2;
    Shy := a.m1 * b.SX + a.M3 * b.Shy + a.M5 * b.W0;
    SY := a.m1 * b.Shx + a.M3 * b.SY + a.M5 * b.W1;
    TY := a.m1 * b.TX + a.M3 * b.TY + a.M5 * b.W2;
  finally
      b.Free;
  end;

  Result := Self;
end;

function TAggTransPerspective.PreMultiplyAffine(b: TAggTransAffine): TAggTransPerspective;
var
  a: TAggTransPerspective;
begin
  a := TAggTransPerspective.Create(TAggTransPerspective(Self));
  try
    SX := a.SX * b.M0 + a.Shx * b.m1;
    Shx := a.SX * b.m2 + a.Shx * b.M3;
    TX := a.SX * b.M4 + a.Shx * b.M5 + a.TX;
    Shy := a.Shy * b.M0 + a.SY * b.m1;
    SY := a.Shy * b.m2 + a.SY * b.M3;
    TY := a.Shy * b.M4 + a.SY * b.M5 + a.TY;
    W0 := a.W0 * b.M0 + a.W1 * b.m1;
    W1 := a.W0 * b.m2 + a.W1 * b.M3;
    W2 := a.W0 * b.M4 + a.W1 * b.M5 + a.W2;
  finally
      a.Free;
  end;

  Result := Self;
end;

function TAggTransPerspective.MultiplyInvAffine(M: TAggTransAffine): TAggTransPerspective;
var
  t: TAggTransAffine;
begin
  t := TAggTransAffine.Create(M.M0, M.m1, M.m2, M.M3, M.M4, M.M5);
  try
    t.Invert;
    Result := MultiplyAffine(t);
  finally
      t.Free;
  end;

  Result := Self;
end;

function TAggTransPerspective.PreMultiplyInvAffine(M: TAggTransAffine): TAggTransPerspective;
var
  t: TAggTransPerspective;
begin
  t := TAggTransPerspective.CreateAffine(M);
  try
    t.Invert;

    t.Multiply(Self);

    // Create(TAggTransPerspective(T)); // ??????
  finally
      t.Free;
  end;

  Result := Self;
end;

procedure TAggTransPerspective.StoreTo(M: PAggQuadrilateral);
begin
  M[0] := SX;
  M[1] := Shy;
  M[2] := W0;
  M[3] := Shx;
  M[4] := SY;
  M[5] := W1;
  M[6] := TX;
  M[7] := TY;
  M[8] := W2;
end;

function TAggTransPerspective.LoadFrom(M: PAggQuadrilateral): TAggTransPerspective;
begin
  SX := M[0];
  Shy := M[1];
  W0 := M[2];
  Shx := M[3];
  SY := M[4];
  W1 := M[5];
  TX := M[6];
  TY := M[7];
  W2 := M[8];

  Result := Self;
end;

function TAggTransPerspective.FromAffine(a: TAggTransAffine): TAggTransPerspective;
begin
  SX := a.M0;
  Shy := a.m1;
  W0 := 0;
  Shx := a.m2;
  SY := a.M3;
  W1 := 0;
  TX := a.M4;
  TY := a.M5;
  W2 := 1;

  Result := Self;
end;

function TAggTransPerspective.Determinant: Double;
begin
  Result := SX * (SY * W2 - TY * W1) + Shx * (TY * W0 - Shy * W2) + TX *
    (Shy * W1 - SY * W0);
end;

function TAggTransPerspective.DeterminantReciprocal: Double;
begin
  Result := 1.0 / Determinant;
end;

function TAggTransPerspective.IsValid(Epsilon: Double = CAggAffineEpsilon): Boolean;
begin
  Result := (Abs(SX) > Epsilon) and (Abs(SY) > Epsilon) and (Abs(W2) > Epsilon);
end;

function TAggTransPerspective.IsIdentity(Epsilon: Double = CAggAffineEpsilon): Boolean;
begin
  Result := IsEqualEpsilon(SX, 1.0, Epsilon) and
    IsEqualEpsilon(Shy, 0.0, Epsilon) and
    IsEqualEpsilon(W0, 0.0, Epsilon) and
    IsEqualEpsilon(Shx, 0.0, Epsilon) and
    IsEqualEpsilon(SY, 1.0, Epsilon) and
    IsEqualEpsilon(W1, 0.0, Epsilon) and
    IsEqualEpsilon(TX, 0.0, Epsilon) and
    IsEqualEpsilon(TY, 0.0, Epsilon) and
    IsEqualEpsilon(W2, 1.0, Epsilon);
end;

function TAggTransPerspective.IsEqual(M: TAggTransPerspective; Epsilon: Double = CAggAffineEpsilon): Boolean;
begin
  Result := IsEqualEpsilon(SX, M.SX, Epsilon) and
    IsEqualEpsilon(Shy, M.Shy, Epsilon) and
    IsEqualEpsilon(W0, M.W0, Epsilon) and
    IsEqualEpsilon(Shx, M.Shx, Epsilon) and
    IsEqualEpsilon(SY, M.SY, Epsilon) and
    IsEqualEpsilon(W1, M.W1, Epsilon) and
    IsEqualEpsilon(TX, M.TX, Epsilon) and
    IsEqualEpsilon(TY, M.TY, Epsilon) and
    IsEqualEpsilon(W2, M.W2, Epsilon);
end;

function TAggTransPerspective.Scale: Double;
var
  x, y: Double;
const
  CSqrt2Half: Double = 0.70710678118654752440084436210485;
begin
  x := CSqrt2Half * SX + CSqrt2Half * Shx;
  y := CSqrt2Half * Shy + CSqrt2Half * SY;

  Result := Hypot(x, y);
end;

function TAggTransPerspective.Rotation: Double;
var
  x1, y1, x2, y2: Double;
begin
  x1 := 0.0;
  y1 := 0.0;
  x2 := 1.0;
  y2 := 0.0;

  TransPerspectiveTransform(Self, @x1, @y1);
  TransPerspectiveTransform(Self, @x2, @y2);

  Result := ArcTan2(y2 - y1, x2 - x1);
end;

procedure TAggTransPerspective.Translation(dx, dy: PDouble);
begin
  dx^ := TX;
  dy^ := TY;
end;

procedure TAggTransPerspective.Scaling(x, y: PDouble);
var
  x1, y1, x2, y2: Double;
  t: TAggTransPerspective;
  Tar: TAggTransAffineRotation;
begin
  x1 := 0.0;
  y1 := 0.0;
  x2 := 1.0;
  y2 := 1.0;

  t := TAggTransPerspective.Create(TAggTransPerspective(Self));
  try
    Tar := TAggTransAffineRotation.Create(-Rotation);
    try
        t.MultiplyAffine(Tar);
    finally
        Tar.Free;
    end;

    t.Transform(t, @x1, @y1);
    t.Transform(t, @x2, @y2);
  finally
      t.Free;
  end;

  x^ := x2 - x1;
  y^ := y2 - y1;
end;

procedure TAggTransPerspective.ScalingAbs(x, y: PDouble);
begin
  x^ := Hypot(SX, Shx);
  y^ := Hypot(Shy, SY);
end;

function TAggTransPerspective.GetBegin(x, y, Step: Double): TAggIteratorXRecord;
begin
  Result.Initialize(x, y, Step, Self);
end;

end.
