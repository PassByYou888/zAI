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
unit AggVpGenClipPolygon;

{$DEFINE FPC_DELPHI_MODE}
{$INCLUDE zDefine.inc}
interface
uses
  AggBasics,
  AggVertexSource,
  AggVpGen,
  AggClipLiangBarsky;

type
  TAggVpgenClipPolygon = class(TAggCustomVpgen)
  private
    FClipBox: TRectDouble;
    FX1, FY1: Double;
    FClipFlags: Cardinal;
    fx, fy: array [0 .. 3] of Double;
    FNumVertices, FVertex, FCmd: Cardinal;
    function GetX1: Double;
    function GetY1: Double;
    function GetX2: Double;
    function GetY2: Double;

    function GetAutoClose: Boolean;
    function GetAutoUnclose: Boolean;
  protected
    function ClippingFlags(x, y: Double): Cardinal;
  public
    constructor Create; override;

    function Vertex(x, y: PDouble): Cardinal; override;

    procedure SetClipBox(x1, y1, x2, y2: Double); overload;
    procedure SetClipBox(Bounds: TRectDouble); overload;

    procedure Reset; override;
    procedure MoveTo(x, y: Double); override;
    procedure LineTo(x, y: Double); override;

    property x1: Double read GetX1;
    property y1: Double read GetY1;
    property x2: Double read GetX2;
    property y2: Double read GetY2;

    property AutoClose: Boolean read GetAutoClose;
    property AutoUnclose: Boolean read GetAutoUnclose;
  end;

implementation


{ TAggVpgenClipPolygon }

constructor TAggVpgenClipPolygon.Create;
begin
  FClipBox := RectDouble(0, 0, 1, 1);

  FX1 := 0;
  FY1 := 0;

  FClipFlags := 0;
  FNumVertices := 0;

  FVertex := 0;
  FCmd := CAggPathCmdMoveTo;
end;

function TAggVpgenClipPolygon.GetX1;
begin
  Result := FClipBox.x1;
end;

function TAggVpgenClipPolygon.GetY1;
begin
  Result := FClipBox.y1;
end;

function TAggVpgenClipPolygon.GetX2;
begin
  Result := FClipBox.x2;
end;

function TAggVpgenClipPolygon.GetY2;
begin
  Result := FClipBox.y2;
end;

function TAggVpgenClipPolygon.GetAutoClose;
begin
  Result := True;
end;

function TAggVpgenClipPolygon.GetAutoUnclose;
begin
  Result := False;
end;

procedure TAggVpgenClipPolygon.Reset;
begin
  FVertex := 0;
  FNumVertices := 0;
end;

procedure TAggVpgenClipPolygon.SetClipBox(x1, y1, x2, y2: Double);
begin
  FClipBox.x1 := x1;
  FClipBox.y1 := y1;
  FClipBox.x2 := x2;
  FClipBox.y2 := y2;

  FClipBox.Normalize;
end;

procedure TAggVpgenClipPolygon.SetClipBox(Bounds: TRectDouble);
begin
  FClipBox := Bounds;
  FClipBox.Normalize;
end;

procedure TAggVpgenClipPolygon.MoveTo(x, y: Double);
begin
  FVertex := 0;
  FNumVertices := 0;
  FClipFlags := ClippingFlags(x, y);

  if FClipFlags = 0 then
    begin
      fx[0] := x;
      fy[0] := y;

      FNumVertices := 1;
    end;

  FX1 := x;
  FY1 := y;
  FCmd := CAggPathCmdMoveTo;
end;

procedure TAggVpgenClipPolygon.LineTo(x, y: Double);
var
  Flags: Cardinal;
begin
  FVertex := 0;
  FNumVertices := 0;

  Flags := ClippingFlags(x, y);

  if FClipFlags = Flags then
    if Flags = 0 then
      begin
        fx[0] := x;
        fy[0] := y;

        FNumVertices := 1;
      end
    else
  else
      FNumVertices := ClipLiangBarskyDouble(FX1, FY1, x, y, @FClipBox,
      @fx, @fy);

  FClipFlags := Flags;

  FX1 := x;
  FY1 := y;
end;

function TAggVpgenClipPolygon.Vertex(x, y: PDouble): Cardinal;
var
  Cmd: Cardinal;

begin
  if FVertex < FNumVertices then
    begin
      x^ := fx[FVertex];
      y^ := fy[FVertex];

      inc(FVertex);

      Cmd := FCmd;
      FCmd := CAggPathCmdLineTo;

      Result := Cmd;

    end
  else
      Result := CAggPathCmdStop;
end;

// Determine the clipping code of the vertex according to the
// Cyrus-Beck line clipping algorithm
//
// |        |
// 0110  |  0010  | 0011
// |        |
// -------+--------+-------- ClipBox.y2
// |        |
// 0100  |  0000  | 0001
// |        |
// -------+--------+-------- ClipBox.y1
// |        |
// 1100  |  1000  | 1001
// |        |
// ClipBox.x1  ClipBox.x2
function TAggVpgenClipPolygon.ClippingFlags;
begin
  if x < FClipBox.x1 then
    begin
      if y > FClipBox.y2 then
        begin
          Result := 6;

          Exit;
        end;

      if y < FClipBox.y1 then
        begin
          Result := 12;

          Exit;
        end;

      Result := 4;

      Exit;
    end;

  if x > FClipBox.x2 then
    begin
      if y > FClipBox.y2 then
        begin
          Result := 3;

          Exit;
        end;

      if y < FClipBox.y1 then
        begin
          Result := 9;

          Exit;
        end;

      Result := 1;

      Exit;
    end;

  if y > FClipBox.y2 then
    begin
      Result := 2;

      Exit;
    end;

  if y < FClipBox.y1 then
    begin
      Result := 8;

      Exit;
    end;

  Result := 0;
end;

end. 
 
 
