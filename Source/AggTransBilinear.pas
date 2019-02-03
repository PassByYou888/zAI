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
unit AggTransBilinear;

{$DEFINE FPC_DELPHI_MODE}
{$INCLUDE zDefine.inc}
interface
uses
  AggBasics,
  AggTransAffine,
  AggSimulEq;

type
  PAggIteratorX = ^TAggIteratorX;

  TAggIteratorX = record
  private
    IncX, IncY, x, y: Double;
  public
    procedure Initialize(TX, TY, Step: Double; M: PDoubleArray42); overload;

    procedure IncOperator;
  end;

  TAggTransBilinear = class(TAggTransAffine)
  private
    FValid: Boolean;
    FMatrix: array [0 .. 3, 0 .. 1] of Double;
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

    function GetBegin(x, y, Step: Double): TAggIteratorX;

    // Check if the equations were solved successfully
    property IsValid: Boolean read FValid;
  end;

implementation


{ TAggIteratorX }

procedure TAggIteratorX.Initialize(TX, TY, Step: Double; M: PDoubleArray42);
begin
  IncX := M^[1, 0] * Step * TY + M^[2, 0] * Step;
  IncY := M^[1, 1] * Step * TY + M^[2, 1] * Step;

  x := M^[0, 0] + M^[1, 0] * TX * TY + M^[2, 0] * TX + M^[3, 0] * TY;
  y := M^[0, 1] + M^[1, 1] * TX * TY + M^[2, 1] * TX + M^[3, 1] * TY;
end;

procedure TAggIteratorX.IncOperator;
begin
  x := x + IncX;
  y := y + IncY;
end;

procedure BilinearTransform(This: TAggTransBilinear; x, y: PDouble);
var
  Temp: TPointDouble;
  xy: Double;
begin
  Temp.x := x^;
  Temp.y := y^;
  xy := Temp.x * Temp.y;

  x^ := This.FMatrix[0, 0] + This.FMatrix[1, 0] * xy +
    This.FMatrix[2, 0] * Temp.x + This.FMatrix[3, 0] * Temp.y;
  y^ := This.FMatrix[0, 1] + This.FMatrix[1, 1] * xy +
    This.FMatrix[2, 1] * Temp.x + This.FMatrix[3, 1] * Temp.y;
end;

{ TAggTransBilinear }

constructor TAggTransBilinear.Create;
begin
  inherited Create;

  FValid := False;

  Transform := @BilinearTransform;
end;

constructor TAggTransBilinear.Create(Src, Dst: PQuadDouble);
begin
  inherited Create;

  QuadToQuad(Src, Dst);

  Transform := @BilinearTransform;
end;

constructor TAggTransBilinear.Create(x1, y1, x2, y2: Double; Quad: PQuadDouble);
begin
  inherited Create;

  RectToQuad(x1, y1, x2, y2, Quad);

  Transform := @BilinearTransform;
end;

constructor TAggTransBilinear.Create(Rect: TRectDouble; Quad: PQuadDouble);
begin
  inherited Create;

  RectToQuad(Rect, Quad);

  Transform := @BilinearTransform;
end;

constructor TAggTransBilinear.Create(Quad: PQuadDouble; x1, y1, x2, y2: Double);
begin
  inherited Create;

  QuadToRect(Quad, x1, y1, x2, y2);

  Transform := @BilinearTransform;
end;

constructor TAggTransBilinear.Create(Quad: PQuadDouble; Rect: TRectDouble);
begin
  inherited Create;

  QuadToRect(Quad, Rect);

  Transform := @BilinearTransform;
end;

procedure TAggTransBilinear.QuadToQuad(Src, Dst: PQuadDouble);
var
  Left: TDoubleMatrix4x4;
  Right: TDoubleArray42;

  i, ix, iy: Cardinal;
begin
  for i := 0 to 3 do
    begin
      ix := i * 2;
      iy := ix + 1;

      Left[i, 0] := 1.0;
      Left[i, 1] := PDouble(PtrComp(Src) + ix * SizeOf(Double))^ *
        PDouble(PtrComp(Src) + iy * SizeOf(Double))^;
      Left[i, 2] := PDouble(PtrComp(Src) + ix * SizeOf(Double))^;
      Left[i, 3] := PDouble(PtrComp(Src) + iy * SizeOf(Double))^;

      Right[i, 0] := PDouble(PtrComp(Dst) + ix * SizeOf(Double))^;
      Right[i, 1] := PDouble(PtrComp(Dst) + iy * SizeOf(Double))^;
    end;

  FValid := SimulEqSolve(@Left, @Right, @FMatrix, 4, 2);
end;

procedure TAggTransBilinear.RectToQuad(x1, y1, x2, y2: Double; Quad: PQuadDouble);
var
  Src: TQuadDouble;
begin
  Src.values[0] := x1;
  Src.values[1] := y1;
  Src.values[2] := x2;
  Src.values[3] := y1;
  Src.values[4] := x2;
  Src.values[5] := y2;
  Src.values[6] := x1;
  Src.values[7] := y2;

  QuadToQuad(@Src, Quad);
end;

procedure TAggTransBilinear.RectToQuad(Rect: TRectDouble; Quad: PQuadDouble);
var
  Src: TQuadDouble;
begin
  Src := QuadDouble(Rect);
  QuadToQuad(@Src, Quad);
end;

procedure TAggTransBilinear.QuadToRect(Quad: PQuadDouble; x1, y1, x2, y2: Double);
var
  Dst: TQuadDouble;
begin
  Dst.values[0] := x1;
  Dst.values[1] := y1;
  Dst.values[2] := x2;
  Dst.values[3] := y1;
  Dst.values[4] := x2;
  Dst.values[5] := y2;
  Dst.values[6] := x1;
  Dst.values[7] := y2;

  QuadToQuad(Quad, @Dst);
end;

procedure TAggTransBilinear.QuadToRect(Quad: PQuadDouble; Rect: TRectDouble);
var
  Dst: TQuadDouble;
begin
  Dst := QuadDouble(Rect);
  QuadToQuad(Quad, @Dst);
end;

function TAggTransBilinear.GetBegin(x, y, Step: Double): TAggIteratorX;
begin
  Result.Initialize(x, y, Step, @FMatrix);
end;

end. 
 
 
