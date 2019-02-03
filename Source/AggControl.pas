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
unit AggControl;

{$DEFINE FPC_DELPHI_MODE}
{$INCLUDE zDefine.inc}
interface
uses
  AggBasics,
  AggTransAffine,
  AggRasterizerScanLine,
  AggScanline,
  AggRendererScanLine,
  AggRenderScanlines,
  AggVertexSource,
  AggColor32;

type
  TAggCustomAggControl = class(TAggCustomVertexSource)
  private
    FFlipY: Boolean;
    FMatrix: TAggTransAffine;
    function GetScale: Double;
  protected
    FRect: TRectDouble;
    function GetColorPointer(index: Cardinal): PAggColor; virtual; abstract;
  public
    constructor Create(x1, y1, x2, y2: Double; FlipY: Boolean); virtual;
    destructor Destroy; override;

    procedure SetClipBox(x1, y1, x2, y2: Double); overload; virtual;
    procedure SetClipBox(ClipBox: TRectDouble); overload; virtual;

    function InRect(x, y: Double): Boolean; virtual;

    function OnMouseButtonDown(x, y: Double): Boolean; virtual;
    function OnMouseButtonUp(x, y: Double): Boolean; virtual;

    function OnMouseMove(x, y: Double; ButtonFlag: Boolean): Boolean; virtual;
    function OnArrowKeys(Left, Right, Down, up: Boolean): Boolean; virtual;

    procedure Transform(Matrix: TAggTransAffine);
    procedure TransformXY(x, y: PDouble);
    procedure InverseTransformXY(x, y: PDouble); overload;
    procedure InverseTransformXY(var x, y: Double); overload;
    procedure NoTransform;

    property ColorPointer[index: Cardinal]: PAggColor read GetColorPointer;
    property Scale: Double read GetScale;
  end;

procedure RenderControl(Ras: TAggRasterizerScanLine; SL: TAggCustomScanLine; r: TAggCustomRendererScanLineSolid; c: TAggCustomAggControl);

implementation


{ TAggCustomAggControl }

constructor TAggCustomAggControl.Create;
begin
  inherited Create;

  FRect.x1 := x1;
  FRect.y1 := y1;
  FRect.x2 := x2;
  FRect.y2 := y2;

  FFlipY := FlipY;

  FMatrix := nil;
end;

destructor TAggCustomAggControl.Destroy;
begin
  inherited;
end;

function TAggCustomAggControl.InRect(x, y: Double): Boolean;
begin
  Result := False;
end;

function TAggCustomAggControl.OnMouseButtonDown(x, y: Double): Boolean;
begin
  Result := False;
end;

function TAggCustomAggControl.OnMouseButtonUp(x, y: Double): Boolean;
begin
  Result := False;
end;

function TAggCustomAggControl.OnMouseMove(x, y: Double; ButtonFlag: Boolean): Boolean;
begin
  Result := False;
end;

procedure TAggCustomAggControl.SetClipBox(x1, y1, x2, y2: Double);
begin
  SetClipBox(RectDouble(x1, y1, x2, y2));
end;

procedure TAggCustomAggControl.SetClipBox(ClipBox: TRectDouble);
begin
  FRect := ClipBox;
end;

function TAggCustomAggControl.OnArrowKeys(Left, Right, Down, up: Boolean): Boolean;
begin
  Result := False;
end;

procedure TAggCustomAggControl.Transform(Matrix: TAggTransAffine);
begin
  FMatrix := Matrix;
end;

procedure TAggCustomAggControl.NoTransform;
begin
  FMatrix := nil;
end;

procedure TAggCustomAggControl.TransformXY(x, y: PDouble);
begin
  if FFlipY then
      y^ := FRect.y1 + FRect.y2 - y^;

  if FMatrix <> nil then
      FMatrix.Transform(FMatrix, x, y);
end;

procedure TAggCustomAggControl.InverseTransformXY(x, y: PDouble);
begin
  if FMatrix <> nil then
      FMatrix.InverseTransform(FMatrix, x, y);

  if FFlipY then
      y^ := FRect.y1 + FRect.y2 - y^;
end;

procedure TAggCustomAggControl.InverseTransformXY(var x, y: Double);
begin
  InverseTransformXY(@x, @y);
end;

function TAggCustomAggControl.GetScale: Double;
begin
  if FMatrix <> nil then
      Result := FMatrix.GetScale
  else
      Result := 1.0;
end;

procedure RenderControl(Ras: TAggRasterizerScanLine; SL: TAggCustomScanLine;
  r: TAggCustomRendererScanLineSolid; c: TAggCustomAggControl);
var
  i: Cardinal;
begin
  if c.PathCount > 0 then
    for i := 0 to c.PathCount - 1 do
      begin
        Ras.Reset;
        Ras.AddPath(c, i);

        r.SetColor(c.ColorPointer[i]);

        RenderScanLines(Ras, SL, r);
      end;
end;

end. 
 
 
 
