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
unit AggVcgenSmoothPoly1;

{$DEFINE FPC_DELPHI_MODE}
{$INCLUDE zDefine.inc}
interface
uses
  AggBasics,
  AggVertexSource,
  AggVertexSequence;

type
  TAggStatusEnum = (seInitial, seReady, sePolygon, seCtrlB, seCtrlE, seCtrl1, seCtrl2, seEndPoly, seStop);

  TAggVcgenSmoothPoly1 = class(TAggVertexSource)
  private
    FSourceVertices: TAggVertexSequence;
    FSmoothValue: Double;

    FClosed: Cardinal;
    FStatus: TAggStatusEnum;

    FSourceVertex: Cardinal;

    FControl: array [0 .. 1] of TPointDouble;
    procedure SetSmoothValue(v: Double);
    function GetSmoothValue: Double;
  protected
    procedure Calculate(v0, v1, v2, v3: PAggVertexDistance);
  public
    constructor Create;
    destructor Destroy; override;

    // Vertex Generator Interface
    procedure RemoveAll; override;
    procedure AddVertex(x, y: Double; Cmd: Cardinal); override;

    // Vertex Source Interface
    procedure Rewind(PathID: Cardinal); override;
    function Vertex(x, y: PDouble): Cardinal; override;

    property SmoothValue: Double read GetSmoothValue write SetSmoothValue;
  end;

implementation

{ TAggVcgenSmoothPoly1 }

constructor TAggVcgenSmoothPoly1.Create;
begin
  FSourceVertices := TAggVertexSequence.Create(SizeOf(TAggVertexDistance), 6);

  FSmoothValue := 0.5;

  FClosed := 0;
  FStatus := seInitial;

  FSourceVertex := 0;
end;

destructor TAggVcgenSmoothPoly1.Destroy;
begin
  FSourceVertices.Free;

  inherited;
end;

procedure TAggVcgenSmoothPoly1.SetSmoothValue;
begin
  FSmoothValue := v * 0.5;
end;

function TAggVcgenSmoothPoly1.GetSmoothValue;
begin
  Result := 2 * FSmoothValue;
end;

procedure TAggVcgenSmoothPoly1.RemoveAll;
begin
  FSourceVertices.RemoveAll;

  FClosed := 0;
  FStatus := seInitial;
end;

procedure TAggVcgenSmoothPoly1.AddVertex;
var
  VD: TAggVertexDistance;
begin
  FStatus := seInitial;

  if IsMoveTo(Cmd) then
    begin
      VD.Pos := PointDouble(x, y);
      VD.Dist := 0;

      FSourceVertices.ModifyLast(@VD);
    end
  else if IsVertex(Cmd) then
    begin
      VD.Pos := PointDouble(x, y);
      VD.Dist := 0;

      FSourceVertices.Add(@VD);
    end
  else
      FClosed := GetCloseFlag(Cmd);
end;

procedure TAggVcgenSmoothPoly1.Rewind(PathID: Cardinal);
begin
  if FStatus = seInitial then
      FSourceVertices.Close(Boolean(FClosed <> 0));

  FStatus := seReady;
  FSourceVertex := 0;
end;

function TAggVcgenSmoothPoly1.Vertex(x, y: PDouble): Cardinal;
label
  _ready, _polygon;
begin
  Result := CAggPathCmdLineTo;

  while not IsStop(Result) do
    case FStatus of
      seInitial:
        begin
          Rewind(0);

          goto _ready;
        end;

      seReady:
      _ready:
        begin
          if FSourceVertices.Size < 2 then
            begin
              Result := CAggPathCmdStop;
              Continue;
            end;

          if FSourceVertices.Size = 2 then
            begin
              x^ := PAggVertexDistance
                (FSourceVertices[FSourceVertex]).Pos.x;
              y^ := PAggVertexDistance
                (FSourceVertices[FSourceVertex]).Pos.y;

              inc(FSourceVertex);

              if FSourceVertex = 1 then
                begin
                  Result := CAggPathCmdMoveTo;

                  Exit;
                end;

              if FSourceVertex = 2 then
                begin
                  Result := CAggPathCmdLineTo;

                  Exit;
                end;

              Result := CAggPathCmdStop;
              Continue;
            end;

          Result := CAggPathCmdMoveTo;

          FStatus := sePolygon;

          FSourceVertex := 0;

          goto _polygon;
        end;

      sePolygon:
      _polygon:
        begin
          if FClosed <> 0 then
            if FSourceVertex >= FSourceVertices.Size then
              begin
                x^ := PAggVertexDistance(FSourceVertices[0]).Pos.x;
                y^ := PAggVertexDistance(FSourceVertices[0]).Pos.y;

                FStatus := seEndPoly;
                Result := CAggPathCmdCurve4;

                Exit;
              end
            else
          else if FSourceVertex >= FSourceVertices.Size - 1 then
            begin
              x^ := PAggVertexDistance
                (FSourceVertices[FSourceVertices.Size - 1]).Pos.x;
              y^ := PAggVertexDistance
                (FSourceVertices[FSourceVertices.Size - 1]).Pos.y;

              FStatus := seEndPoly;
              Result := CAggPathCmdCurve3;

              Exit;
            end;

          Calculate(FSourceVertices.Prev(FSourceVertex),
            FSourceVertices.Curr(FSourceVertex),
            FSourceVertices.Next(FSourceVertex),
            FSourceVertices.Next(FSourceVertex + 1));

          x^ := PAggVertexDistance(FSourceVertices[FSourceVertex]).Pos.x;
          y^ := PAggVertexDistance(FSourceVertices[FSourceVertex]).Pos.y;

          inc(FSourceVertex);

          if FClosed <> 0 then
            begin
              FStatus := seCtrl1;

              if FSourceVertex = 1 then
                  Result := CAggPathCmdMoveTo
              else
                  Result := CAggPathCmdCurve4;
              Exit;
            end
          else
            begin
              if FSourceVertex = 1 then
                begin
                  FStatus := seCtrlB;
                  Result := CAggPathCmdMoveTo;
                  Exit;
                end;

              if FSourceVertex >= FSourceVertices.Size - 1 then
                begin
                  FStatus := seCtrlE;
                  Result := CAggPathCmdCurve3;
                  Exit;
                end;

              FStatus := seCtrl1;
              Result := CAggPathCmdCurve4;
              Exit;
            end;
        end;

      seCtrlB:
        begin
          x^ := FControl[1].x;
          y^ := FControl[1].y;

          FStatus := sePolygon;
          Result := CAggPathCmdCurve3;
          Exit;
        end;

      seCtrlE:
        begin
          x^ := FControl[0].x;
          y^ := FControl[0].y;

          FStatus := sePolygon;
          Result := CAggPathCmdCurve3;
          Exit;
        end;

      seCtrl1:
        begin
          x^ := FControl[0].x;
          y^ := FControl[0].y;

          FStatus := seCtrl2;
          Result := CAggPathCmdCurve4;
          Exit;
        end;

      seCtrl2:
        begin
          x^ := FControl[1].x;
          y^ := FControl[1].y;

          FStatus := sePolygon;
          Result := CAggPathCmdCurve4;
          Exit;
        end;

      seEndPoly:
        begin
          FStatus := seStop;
          Result := CAggPathCmdEndPoly or FClosed;
          Exit;
        end;

      seStop:
        begin
          Result := CAggPathCmdStop;
          Exit;
        end;
    end;
end;

procedure TAggVcgenSmoothPoly1.Calculate;
var
  k1, k2, Xm1, Ym1, Xm2, Ym2: Double;
begin
  k1 := v0.Dist / (v0.Dist + v1.Dist);
  k2 := v1.Dist / (v1.Dist + v2.Dist);

  Xm1 := v0.Pos.x + (v2.Pos.x - v0.Pos.x) * k1;
  Ym1 := v0.Pos.y + (v2.Pos.y - v0.Pos.y) * k1;
  Xm2 := v1.Pos.x + (v3.Pos.x - v1.Pos.x) * k2;
  Ym2 := v1.Pos.y + (v3.Pos.y - v1.Pos.y) * k2;

  FControl[0].x := v1.Pos.x + FSmoothValue * (v2.Pos.x - Xm1);
  FControl[0].y := v1.Pos.y + FSmoothValue * (v2.Pos.y - Ym1);
  FControl[1].x := v2.Pos.x + FSmoothValue * (v1.Pos.x - Xm2);
  FControl[1].y := v2.Pos.y + FSmoothValue * (v1.Pos.y - Ym2);
end;

end. 
 
 
 
