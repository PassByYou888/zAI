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
unit AggTransAffine;

(*
  ////////////////////////////////////////////////////////////////////////////////
  //                                                                            //
  // Affine transformation classes.                                             //
  //                                                                            //
  ////////////////////////////////////////////////////////////////////////////////
  //                                                                            //
  //  Affine transformation are linear transformations in Cartesian coordinates //
  //  (strictly speaking not only in Cartesian, but for the beginning we will   //
  //  think so). They are rotation, scaling, translation and skewing.           //
  //  After any affine transformation a line segment remains a line segment     //
  //  and it will never become a curve.                                         //
  //                                                                            //
  //  There will be no math about matrix calculations, since it has been        //
  //  described many times. Ask yourself a very simple question:                //
  //  "why do we need to understand and use some matrix stuff instead of just   //
  //  rotating, scaling and so on". The answers are:                            //
  //                                                                            //
  //  1. Any combination of transformations can be done by only 4               //
  //  multiplications and 4 additions in floating point.                        //
  //  2. One matrix transformation is equivalent to the number of consecutive   //
  //  discrete transformations, i.e. the matrix "accumulates" all               //
  //  transformations in the order of their settings. Suppose we have 4         //
  //  transformations:                                                          //
  //    * rotate by 30 degrees,                                                 //
  //    * scale X to 2,                                                         //
  //    * scale Y to 1.5,                                                       //
  //    * move to (100, 100).                                                   //
  //  The result will depend on the order of these transformations,             //
  //  and the advantage of matrix is that the sequence of discret calls:        //
  //  rotate(30), scaleX(2.0), scaleY(1.5), move(100,100)                       //
  //  will have exactly the same result as the following matrix                 //
  //  transformations:                                                          //
  //                                                                            //
  //  m : TAffineMatrix;                                                        //
  //  m := m * RotateMatrix(30);                                                //
  //  m := m * ScaleXMatrix(2.0);                                               //
  //  m := m * ScaleYMatrix(1.5);                                               //
  //  m := m * MoveMatrix(100,100);                                             //
  //                                                                            //
  //  m.TransformMyPointAtLast(x, y);                                           //
  //                                                                            //
  //  What is the good of it? In real life we will set-up the matrix only once  //
  //  and then transform many points, let alone the convenience to set any      //
  //  combination of transformations.                                           //
  //                                                                            //
  //  So, how to use it? Very easy - literally as it's shown above. Not quite,  //
  //  let us write a correct example:                                           //
  //                                                                            //
  //  m : TAggTransAffine;                                                      //
  //  m := m * TAggTransAffineRotation(30.0 * Pi / 180.0);                      //
  //  m := m * TAggTransAffineScaling(2.0, 1.5);                                //
  //  m := m * TAggTransAffineTranslation(100.0, 100.0);                        //
  //  m.Transform(@X, @Y);                                                      //
  //                                                                            //
  //  The affine matrix is all you need to perform any linear transformation,   //
  //  but all transformations have origin point (0, 0). It means that we need   //
  //  to use 2 translations if we want to rotate someting around (100, 100):    //
  //                                                                            //
  //  m := m * TAggTransAffineTranslation(-100, -100); // move to (0,0)         //
  //  m := m * TAggTransAffineRotation(30 * Pi / 180); // rotate                //
  //  m := m * TAggTransAffineTranslation(100, 100);   // move back to origin   //
  //                                                                            //
  ////////////////////////////////////////////////////////////////////////////////
*)

{$DEFINE FPC_DELPHI_MODE}
{$INCLUDE zDefine.inc}
interface
uses
  Math,
  AggMath,
  AggBasics;

const
  CAggAffineEpsilon = 1E-14; // About of precision of doubles

type
  TAggTransAffine = class;

  TAggProcTransform = procedure(This: TAggTransAffine; x, y: PDouble);

  PAggParallelogram = ^TAggParallelogram;
  TAggParallelogram = array [0 .. 5] of Double;

  TAggTransAffine = class
  protected
    FData: TAggParallelogram;
  public
    Transform, Transform2x2, InverseTransform: TAggProcTransform;

    // Construct an identity matrix - it does not transform anything
    constructor Create; overload; virtual;

    // Construct a custom matrix. Usually used in derived classes
    constructor Create(v0, v1, v2, v3, v4, V5: Double); overload;

    // Construct a matrix to transform a parallelogram to another one
    constructor Create(Rect, Parl: PAggParallelogram); overload;

    // Construct a matrix to transform a rectangle to a parallelogram
    constructor Create(x1, y1, x2, y2: Double; Parl: PAggParallelogram); overload;
    constructor Create(Rect: TRectDouble; Parl: PAggParallelogram); overload;

    // Construct a matrix to transform a parallelogram to a rectangle
    constructor Create(Parl: PAggParallelogram; x1, y1, x2, y2: Double); overload;
    constructor Create(Parl: PAggParallelogram; Rect: TRectDouble); overload;

    // Construct a matrix with different transform function
    constructor Create(tr: TAggProcTransform); overload;

    // Calculate a matrix to transform a parallelogram to another one.
    // src and dst are pointers to arrays of three points
    // (double[6], x,y,...) that identify three corners of the
    // parallelograms assuming implicit fourth points.
    // There are also transformations rectangtle to parallelogram and
    // parellelogram to rectangle
    procedure ParlToParl(Src, Dst: PAggParallelogram);
    procedure RectToParl(Rect: TRectDouble; Parl: PAggParallelogram); overload;
    procedure RectToParl(x1, y1, x2, y2: Double; Parl: PAggParallelogram); overload;
    procedure ParlToRect(Parl: PAggParallelogram; Rect: TRectDouble); overload;
    procedure ParlToRect(Parl: PAggParallelogram; x1, y1, x2, y2: Double); overload;

    // Reset - actually load an identity matrix
    procedure Reset; virtual;

    // Initialize Transforms
    procedure InitializeTransforms;

    // Multiply matrix to another one
    procedure Multiply(M: TAggTransAffine);

    // Multiply "m" to "this" and assign the result to "this"
    procedure PreMultiply(M: TAggTransAffine);

    // Multiply matrix to inverse of another one
    procedure MultiplyInv(M: TAggTransAffine);

    // Multiply inverse of "m" to "this" and assign the result to "this"
    procedure PreMultiplyInv(M: TAggTransAffine);

    // Invert matrix. Do not try to invert degenerate matrices,
    // there's no check for validity. If you set scale to 0 and
    // then try to invert matrix, expect unpredictable result.
    procedure Invert;

    // Mirroring around X
    procedure FlipX;

    // Mirroring around Y
    procedure FlipY;

    // ------------------------------------------- Load/Store
    // Store matrix to an array [6] of double
    procedure StoreTo(M: PAggParallelogram); overload;
    procedure StoreTo(out M: TAggParallelogram); overload;

    // Load matrix from an array [6] of double
    procedure LoadFrom(M: PAggParallelogram); overload;
    procedure LoadFrom(var M: TAggParallelogram); overload;

    // -------------------------------------------- Transformations
    // Direct transformation x and y
    // see: transform : TAggProcTransform; above

    // Direct transformation x and y, 2x2 matrix only, no translation
    // procedure Transform2x2(x ,y : PDouble );

    // Inverse transformation x and y. It works sLower than the
    // direct transformation, so if the performance is critical
    // it's better to invert() the matrix and then use transform()
    // procedure InverseTransform(x ,y : PDouble );

    // -------------------------------------------- Auxiliary
    // Calculate the determinant of matrix
    function Determinant: Double;

    // Get the average scale (by X and Y).
    // Basically used to calculate the ApproximationScale when
    // decomposinting curves into line segments.
    function GetScale: Double; overload;

    // Check to see if it's an identity matrix
    function IsIdentity(Epsilon: Double = CAggAffineEpsilon): Boolean;

    // Check to see if two matrices are equal
    function IsEqual(M: TAggTransAffine; Epsilon: Double = CAggAffineEpsilon): Boolean;

    // Determine the major parameters. Use carefully considering degenerate matrices
    function GetRotation: Double;
    procedure GetTranslation(out dx, dy: Double);
    procedure GetScaling(out SX, SY: Double);
    procedure GetScalingAbs(out SX, SY: Double);

    // Trans Affine Assignations
    procedure Assign(From: TAggTransAffine);
    procedure AssignAll(From: TAggTransAffine);

    // Direct transformations operations
    function Translate(x, y: Double): TAggTransAffine; overload;
    function Translate(Value: TPointDouble): TAggTransAffine; overload;
    function Rotate(a: Double): TAggTransAffine;
    function Scale(s: Double): TAggTransAffine; overload;
    function Scale(x, y: Double): TAggTransAffine; overload;
    function Scale(Value: TPointDouble): TAggTransAffine; overload;
    function Skew(x, y: Double): TAggTransAffine; overload;
    function Skew(Value: TPointDouble): TAggTransAffine; overload;

    property M0: Double read FData[0];
    property m1: Double read FData[1];
    property m2: Double read FData[2];
    property M3: Double read FData[3];
    property M4: Double read FData[4];
    property M5: Double read FData[5];
  end;

  // Rotation matrix.
  TAggTransAffineRotation = class(TAggTransAffine)
  public
    constructor Create(angle: Double);
  end;

  // Scaling matrix. ScaleX, ScaleY - scale coefficients by X and Y respectively
  TAggTransAffineScaling = class(TAggTransAffine)
  public
    constructor Create(Scale: Double); overload;
    constructor Create(ScaleX, ScaleY: Double); overload;
  end;

  // Translation matrix
  TAggTransAffineTranslation = class(TAggTransAffine)
  public
    constructor Create(TX, TY: Double);
  end;

  // Skewing (shear) matrix
  TAggTransAffineSkewing = class(TAggTransAffine)
  public
    constructor Create(SX, SY: Double);
  end;

  // Rotate, Scale and Translate, associating 0...dist with line segment
  // x1,y1,x2,y2
  TAggTransAffineLineSegment = class(TAggTransAffine)
  public
    constructor Create(x1, y1, x2, y2, Dist: Double);
  end;

  // Reflection matrix. Reflect coordinates across the line through
  // the origin containing the unit vector (ux, uy).
  // Contributed by John Horigan
  TAggTransAffineReflectionUnit = class(TAggTransAffine)
  public
    constructor Create(Ux, Uy: Double);
  end;

  // Reflection matrix. Reflect coordinates across the line through
  // the origin at the angle a or containing the non-unit vector (x, y).
  // Contributed by John Horigan
  TAggTransAffineReflection = class(TAggTransAffineReflectionUnit)
  public
    constructor Create(a: Double); overload;
    constructor Create(x, y: Double); overload;
  end;

function IsEqualEpsilon(v1, v2, Epsilon: Double): Boolean;

procedure TransAffineRotation(Matrix: TAggTransAffine; Rotation: Double);
procedure TransAffineScaling(Matrix: TAggTransAffine; Scaling: Double); overload;
procedure TransAffineScaling(Matrix: TAggTransAffine; ScalingX, ScalingY: Double); overload;
procedure TransAffineSkewing(Matrix: TAggTransAffine; SkewingX, SkewingY: Double);
procedure TransAffineTranslation(Matrix: TAggTransAffine; TranslationX, TranslationY: Double);

implementation


function IsEqualEpsilon;
begin
  Result := Abs(v1 - v2) < Epsilon;
end;

procedure TransAffineRotation(Matrix: TAggTransAffine; Rotation: Double);
var
  TransAffine: TAggTransAffineRotation;
begin
  TransAffine := TAggTransAffineRotation.Create(Rotation);
  try
      Matrix.Multiply(TransAffine);
  finally
      TransAffine.Free;
  end;
end;

procedure TransAffineScaling(Matrix: TAggTransAffine; Scaling: Double);
var
  TransAffine: TAggTransAffineScaling;
begin
  TransAffine := TAggTransAffineScaling.Create(Scaling);
  try
      Matrix.Multiply(TransAffine);
  finally
      TransAffine.Free;
  end;
end;

procedure TransAffineScaling(Matrix: TAggTransAffine; ScalingX,
  ScalingY: Double);
var
  TransAffine: TAggTransAffineScaling;
begin
  TransAffine := TAggTransAffineScaling.Create(ScalingX, ScalingY);
  try
      Matrix.Multiply(TransAffine);
  finally
      TransAffine.Free;
  end;
end;

procedure TransAffineSkewing(Matrix: TAggTransAffine; SkewingX,
  SkewingY: Double);
var
  TransAffine: TAggTransAffineSkewing;
begin
  TransAffine := TAggTransAffineSkewing.Create(SkewingX, SkewingY);
  try
      Matrix.Multiply(TransAffine);
  finally
      TransAffine.Free;
  end;
end;

procedure TransAffineTranslation(Matrix: TAggTransAffine; TranslationX,
  TranslationY: Double);
var
  TransAffine: TAggTransAffineTranslation;
begin
  TransAffine := TAggTransAffineTranslation.Create(TranslationX, TranslationY);
  try
      Matrix.Multiply(TransAffine);
  finally
      TransAffine.Free;
  end;
end;

procedure TransAffineTransform(This: TAggTransAffine; x, y: PDouble);
var
  TX: Double;
begin
  TX := x^;
  x^ := TX * This.FData[0] + y^ * This.FData[2] + This.FData[4];
  y^ := TX * This.FData[1] + y^ * This.FData[3] + This.FData[5];
end;

procedure TransAffineTransform2x2(This: TAggTransAffine; x, y: PDouble);
var
  TX: Double;
begin
  TX := x^;
  x^ := TX * This.FData[0] + y^ * This.FData[2];
  y^ := TX * This.FData[1] + y^ * This.FData[3];
end;

procedure TransAffineInverseTransform(This: TAggTransAffine;
  x, y: PDouble);
var
  d, a, b: Double;
begin
  d := This.Determinant;
  a := (x^ - This.FData[4]) * d;
  b := (y^ - This.FData[5]) * d;

  x^ := a * This.FData[3] - b * This.FData[2];
  y^ := b * This.FData[0] - a * This.FData[1];
end;

{ TAggTransAffine }

constructor TAggTransAffine.Create;
begin
  FData[0] := 1;
  FData[1] := 0;
  FData[2] := 0;
  FData[3] := 1;
  FData[4] := 0;
  FData[5] := 0;

  InitializeTransforms;
end;

constructor TAggTransAffine.Create(v0, v1, v2, v3, v4, V5: Double);
begin
  FData[0] := v0;
  FData[1] := v1;
  FData[2] := v2;
  FData[3] := v3;
  FData[4] := v4;
  FData[5] := V5;

  InitializeTransforms;
end;

constructor TAggTransAffine.Create(Rect, Parl: PAggParallelogram);
begin
  ParlToParl(Rect, Parl);
  InitializeTransforms;
end;

constructor TAggTransAffine.Create(x1, y1, x2, y2: Double; Parl: PAggParallelogram);
begin
  RectToParl(x1, y1, x2, y2, Parl);
  InitializeTransforms;
end;

constructor TAggTransAffine.Create(Rect: TRectDouble; Parl: PAggParallelogram);
begin
  RectToParl(Rect, Parl);
  InitializeTransforms;
end;

constructor TAggTransAffine.Create(Parl: PAggParallelogram; x1, y1, x2, y2: Double);
begin
  ParlToRect(Parl, x1, y1, x2, y2);
  InitializeTransforms;
end;

constructor TAggTransAffine.Create(Parl: PAggParallelogram; Rect: TRectDouble);
begin
  ParlToRect(Parl, Rect);
  InitializeTransforms;
end;

constructor TAggTransAffine.Create(tr: TAggProcTransform);
begin
  FData[0] := 1;
  FData[1] := 0;
  FData[2] := 0;
  FData[3] := 1;
  FData[4] := 0;
  FData[5] := 0;

  Transform := tr;
  Transform2x2 := @TransAffineTransform2x2;
  InverseTransform := @TransAffineInverseTransform;
end;

procedure TAggTransAffine.InitializeTransforms;
begin
  Transform := @TransAffineTransform;
  Transform2x2 := @TransAffineTransform2x2;
  InverseTransform := @TransAffineInverseTransform;
end;

procedure TAggTransAffine.ParlToParl(Src, Dst: PAggParallelogram);
var
  M: TAggTransAffine;
begin
  FData[0] := Src[2] - Src[0];
  FData[1] := Src[3] - Src[1];
  FData[2] := Src[4] - Src[0];
  FData[3] := Src[5] - Src[1];
  FData[4] := Src[0];
  FData[5] := Src[1];

  Invert;

  M := TAggTransAffine.Create(Dst[2] - Dst[0], Dst[3] - Dst[1], Dst[4] - Dst[0],
    Dst[5] - Dst[1], Dst[0], Dst[1]);
  try
      Multiply(M);
  finally
      M.Free;
  end;
end;

procedure TAggTransAffine.RectToParl(x1, y1, x2, y2: Double;
  Parl: PAggParallelogram);
var
  Src: TAggParallelogram;
begin
  Src[0] := x1;
  Src[1] := y1;
  Src[2] := x2;
  Src[3] := y1;
  Src[4] := x2;
  Src[5] := y2;

  ParlToParl(@Src, Parl);
end;

procedure TAggTransAffine.ParlToRect(Parl: PAggParallelogram;
  Rect: TRectDouble);
var
  Src: TAggParallelogram;
begin
  Src[0] := Rect.x1;
  Src[1] := Rect.y1;
  Src[2] := Rect.x2;
  Src[3] := Rect.y1;
  Src[4] := Rect.x2;
  Src[5] := Rect.y2;

  ParlToParl(@Src, Parl);
end;

procedure TAggTransAffine.ParlToRect(Parl: PAggParallelogram; x1, y1, x2,
  y2: Double);
var
  Dst: TAggParallelogram;
begin
  Dst[0] := x1;
  Dst[1] := y1;
  Dst[2] := x2;
  Dst[3] := y1;
  Dst[4] := x2;
  Dst[5] := y2;

  ParlToParl(Parl, @Dst);
end;

procedure TAggTransAffine.RectToParl(Rect: TRectDouble;
  Parl: PAggParallelogram);
var
  Dst: TAggParallelogram;
begin
  Dst[0] := Rect.x1;
  Dst[1] := Rect.y1;
  Dst[2] := Rect.x2;
  Dst[3] := Rect.y1;
  Dst[4] := Rect.x2;
  Dst[5] := Rect.y2;

  ParlToParl(Parl, @Dst);
end;

procedure TAggTransAffine.Reset;
begin
  FData[0] := 1;
  FData[1] := 0;
  FData[2] := 0;
  FData[3] := 1;
  FData[4] := 0;
  FData[5] := 0;
end;

procedure TAggTransAffine.Multiply(M: TAggTransAffine);
var
  t0, t2, T4: Double;
begin
  t0 := FData[0] * M.FData[0] + FData[1] * M.FData[2];
  t2 := FData[2] * M.FData[0] + FData[3] * M.FData[2];
  T4 := FData[4] * M.FData[0] + FData[5] * M.FData[2] + M.FData[4];
  FData[1] := FData[0] * M.FData[1] + FData[1] * M.FData[3];
  FData[3] := FData[2] * M.FData[1] + FData[3] * M.FData[3];
  FData[5] := FData[4] * M.FData[1] + FData[5] * M.FData[3] + M.FData[5];
  FData[0] := t0;
  FData[2] := t2;
  FData[4] := T4;
end;

procedure TAggTransAffine.PreMultiply(M: TAggTransAffine);
begin
  Transform := @M.Transform;
  Transform2x2 := @M.Transform2x2;
  InverseTransform := @M.InverseTransform;
  Multiply(M);
end;

procedure TAggTransAffine.MultiplyInv(M: TAggTransAffine);
var
  t: TAggTransAffine;
begin
  t.AssignAll(M);
  t.Invert;

  Multiply(@t);
end;

procedure TAggTransAffine.PreMultiplyInv(M: TAggTransAffine);
var
  t: TAggTransAffine;
begin
  t.AssignAll(M);

  t.Invert;
  t.Multiply(Self);

  Assign(@t);
end;

procedure TAggTransAffine.Invert;
var
  d, t0, T4: Double;
begin
  d := Determinant;

  t0 := FData[3] * d;
  FData[3] := FData[0] * d;
  FData[1] := -FData[1] * d;
  FData[2] := -FData[2] * d;

  T4 := -FData[4] * t0 - FData[5] * FData[2];
  FData[5] := -FData[4] * FData[1] - FData[5] * FData[3];

  FData[0] := t0;
  FData[4] := T4;
end;

procedure TAggTransAffine.FlipX;
begin
  FData[0] := -FData[0];
  FData[1] := -FData[1];
  FData[4] := -FData[4];
end;

procedure TAggTransAffine.FlipY;
begin
  FData[2] := -FData[2];
  FData[3] := -FData[3];
  FData[5] := -FData[5];
end;

procedure TAggTransAffine.StoreTo(M: PAggParallelogram);
begin
  M^ := FData;
end;

procedure TAggTransAffine.StoreTo(out M: TAggParallelogram);
begin
  M := FData;
end;

procedure TAggTransAffine.LoadFrom(M: PAggParallelogram);
begin
  FData := M^;
end;

procedure TAggTransAffine.LoadFrom(var M: TAggParallelogram);
begin
  FData := M;
end;

function TAggTransAffine.Determinant: Double;
begin
  try
      Result := 1 / (FData[0] * FData[3] - FData[1] * FData[2]);
  except
      Result := 0;
  end;
end;

function TAggTransAffine.GetScale: Double;
var
  x, y: Double;
const
  CSqrt2Half: Double = 0.70710678118654752440084436210485;
begin
  x := CSqrt2Half * FData[0] + CSqrt2Half * FData[2];
  y := CSqrt2Half * FData[1] + CSqrt2Half * FData[3];

  Result := Sqrt(x * x + y * y);
end;

function TAggTransAffine.IsIdentity(Epsilon: Double = CAggAffineEpsilon): Boolean;
begin
  Result := IsEqualEpsilon(FData[0], 1, Epsilon) and
    IsEqualEpsilon(FData[1], 0, Epsilon) and
    IsEqualEpsilon(FData[2], 0, Epsilon) and
    IsEqualEpsilon(FData[3], 1, Epsilon) and
    IsEqualEpsilon(FData[4], 0, Epsilon) and
    IsEqualEpsilon(FData[5], 0, Epsilon);
end;

function TAggTransAffine.IsEqual(M: TAggTransAffine;
  Epsilon: Double = CAggAffineEpsilon): Boolean;
begin
  Result := IsEqualEpsilon(FData[0], M.FData[0], Epsilon) and
    IsEqualEpsilon(FData[1], M.FData[1], Epsilon) and
    IsEqualEpsilon(FData[2], M.FData[2], Epsilon) and
    IsEqualEpsilon(FData[3], M.FData[3], Epsilon) and
    IsEqualEpsilon(FData[4], M.FData[4], Epsilon) and
    IsEqualEpsilon(FData[5], M.FData[5], Epsilon);
end;

function TAggTransAffine.GetRotation: Double;
var
  x1, y1, x2, y2: Double;
begin
  x1 := 0;
  y1 := 0;
  x2 := 1;
  y2 := 0;

  Transform(Self, @x1, @y1);
  Transform(Self, @x2, @y2);

  Result := ArcTan2(y2 - y1, x2 - x1);
end;

procedure TAggTransAffine.GetTranslation(out dx, dy: Double);
begin
  dx := 0;
  dy := 0;

  Transform(Self, @dx, @dy);
end;

procedure TAggTransAffine.GetScaling(out SX, SY: Double);
var
  t: TAggTransAffineRotation;
  x1, y1, x2, y2: Double;
begin
  x1 := 0;
  y1 := 0;
  x2 := 1;
  y2 := 1;

  TAggTransAffine(t) := Self;

  t := TAggTransAffineRotation.Create(-GetRotation);
  try
    t.Transform(Self, @x1, @y1);
    t.Transform(Self, @x2, @y2);
  finally
      t.Free;
  end;

  SX := x2 - x1;
  SY := y2 - y1;
end;

procedure TAggTransAffine.GetScalingAbs(out SX, SY: Double);
begin
  SX := Hypot(FData[0], FData[2]);
  SY := Hypot(FData[1], FData[3]);
end;

procedure TAggTransAffine.Assign(From: TAggTransAffine);
begin
  FData := From.FData;
end;

procedure TAggTransAffine.AssignAll(From: TAggTransAffine);
begin
  FData := From.FData;

  Transform := @From.Transform;
  Transform2x2 := @From.Transform2x2;
  InverseTransform := @From.InverseTransform;
end;

function TAggTransAffine.Translate(x, y: Double): TAggTransAffine;
begin
  FData[4] := FData[4] + x;
  FData[5] := FData[5] + y;

  Result := Self;
end;

function TAggTransAffine.Translate(Value: TPointDouble): TAggTransAffine;
begin
  FData[4] := FData[4] + Value.x;
  FData[5] := FData[5] + Value.y;

  Result := Self;
end;

function TAggTransAffine.Rotate(a: Double): TAggTransAffine;
var
  ca, SA, Temp: Double;
begin
  SinCos(a, SA, ca);

  Temp := FData[0] * ca - FData[1] * SA;
  FData[1] := FData[0] * SA + FData[1] * ca;
  FData[0] := Temp;

  Temp := FData[2] * ca - FData[3] * SA;
  FData[3] := FData[2] * SA + FData[3] * ca;
  FData[2] := Temp;

  Temp := FData[4] * ca - FData[5] * SA;
  FData[5] := FData[4] * SA + FData[5] * ca;
  FData[4] := Temp;

  Result := Self;
end;

function TAggTransAffine.Scale(s: Double): TAggTransAffine;
begin
  FData[0] := FData[0] * s;
  FData[1] := FData[1] * s;
  FData[2] := FData[2] * s;
  FData[3] := FData[3] * s;
  FData[4] := FData[4] * s;
  FData[5] := FData[5] * s;

  Result := Self;
end;

function TAggTransAffine.Scale(x, y: Double): TAggTransAffine;
begin
  FData[0] := FData[0] * x;
  FData[2] := FData[2] * x;
  FData[4] := FData[4] * x;
  FData[1] := FData[1] * y;
  FData[3] := FData[3] * y;
  FData[5] := FData[5] * y;

  Result := Self;
end;

function TAggTransAffine.Scale(Value: TPointDouble): TAggTransAffine;
begin
  FData[0] := FData[0] * Value.x;
  FData[2] := FData[2] * Value.x;
  FData[4] := FData[4] * Value.x;
  FData[1] := FData[1] * Value.y;
  FData[3] := FData[3] * Value.y;
  FData[5] := FData[5] * Value.y;

  Result := Self;
end;

function TAggTransAffine.Skew(x, y: Double): TAggTransAffine;
var
  TY, TX, Temp: Double;
begin
  TY := Tan(y);
  TX := Tan(x);

  Temp := FData[0] + FData[1] * TX;
  FData[1] := FData[0] * TY + FData[1];
  FData[0] := Temp;

  Temp := FData[2] + FData[3] * TX;
  FData[3] := FData[2] * TY + FData[3];
  FData[2] := Temp;

  Temp := FData[4] + FData[5] * TX;
  FData[5] := FData[4] * TY + FData[5];
  FData[4] := Temp;

  Result := Self;
end;

function TAggTransAffine.Skew(Value: TPointDouble): TAggTransAffine;
begin
  Result := Skew(Value.x, Value.y);
end;

{ TAggTransAffineRotation }

constructor TAggTransAffineRotation.Create(angle: Double);
var
  sn, CN: Double;
begin
  SinCos(angle, sn, CN);
  inherited Create(CN, sn, -sn, CN, 0, 0);
end;

{ TAggTransAffineScaling }

constructor TAggTransAffineScaling.Create(ScaleX, ScaleY: Double);
begin
  inherited Create(ScaleX, 0, 0, ScaleY, 0, 0);
end;

constructor TAggTransAffineScaling.Create(Scale: Double);
begin
  inherited Create(Scale, 0, 0, Scale, 0, 0);
end;

{ TAggTransAffineTranslation }

constructor TAggTransAffineTranslation.Create(TX, TY: Double);
begin
  inherited Create(1, 0, 0, 1, TX, TY);
end;

{ TAggTransAffineSkewing }

constructor TAggTransAffineSkewing.Create(SX, SY: Double);
begin
  inherited Create(1, Tan(SY), Tan(SX), 1, 0, 0);
end;

{ TAggTransAffineLineSegment }

constructor TAggTransAffineLineSegment.Create(x1, y1, x2, y2, Dist: Double);
var
  Delta: TPointDouble;
begin
  Delta := PointDouble(x2 - x1, y2 - y1);

  if Dist > 0 then
      Scale(Hypot(Delta.x, Delta.y) / Dist);

  Rotate(ArcTan2(Delta.y, Delta.x));
  Translate(x1, y1);
end;

{ TAggTransAffineReflectionUnit }

constructor TAggTransAffineReflectionUnit.Create(Ux, Uy: Double);
begin
  inherited Create(2 * Sqr(Ux) - 1, 2 * Ux * Uy, 2 * Ux * Uy, 2 * Sqr(Uy) - 1,
    0, 0);
end;

{ TAggTransAffineReflection }

constructor TAggTransAffineReflection.Create(a: Double);
var
  sn, CN: Double;
begin
  SinCos(a, sn, CN);
  inherited Create(CN, sn);
end;

constructor TAggTransAffineReflection.Create(x, y: Double);
var
  Nx, Ny: Double;
  tmp: Double;
begin
  if (x = 0) and (y = 0) then
    begin
      x := 0;
      y := 0;
    end
  else
    begin
      tmp := 1 / Hypot(x, y);
      Nx := x * tmp;
      Ny := y * tmp;
    end;

  inherited Create(Nx, Ny);
end;

end. 
 
 
