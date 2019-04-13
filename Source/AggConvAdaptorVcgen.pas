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
unit AggConvAdaptorVcgen;

{$DEFINE FPC_DELPHI_MODE}
{$INCLUDE zDefine.inc}
interface
uses
  AggBasics,
  AggVertexSource;

type
  TAggNullMarkers = class(TAggVertexSource)
  private
    FMarkers: TAggVertexSource;
    procedure SetMarkers(Value: TAggVertexSource);
  public
    constructor Create;

    procedure RemoveAll; override;
    procedure AddVertex(x, y: Double; Cmd: Cardinal); override;
    procedure PrepareSource;

    procedure Rewind(PathID: Cardinal); override;
    function Vertex(x, y: PDouble): Cardinal; override;

    property Markers: TAggVertexSource read FMarkers write SetMarkers;
  end;

  TAggInternalStatus = (siInitial, siAccumulate, siGenerate);

  TAggConvAdaptorVcgen = class(TAggVertexSource)
  private
    FSource: TAggCustomVertexSource;
    FGenerator: TAggVertexSource;
    FMarkers: TAggNullMarkers;
    FStatus: TAggInternalStatus;
    FLastCmd: Cardinal;
    FStart: TPointDouble;
    function GetMarkers: TAggVertexSource;
    procedure SetMarkers(Value: TAggVertexSource);
    procedure SetSource(Source: TAggCustomVertexSource);
  public
    constructor Create(Source: TAggCustomVertexSource; Generator: TAggVertexSource);
    destructor Destroy; override;

    procedure Rewind(PathID: Cardinal); override;
    function Vertex(x, y: PDouble): Cardinal; override;

    property Markers: TAggVertexSource read GetMarkers write SetMarkers;

    property Source: TAggCustomVertexSource read FSource write SetSource;
    property Generator: TAggVertexSource read FGenerator;
  end;

implementation


{ TAggNullMarkers }

constructor TAggNullMarkers.Create;
begin
  inherited Create;

  FMarkers := nil;
end;

procedure TAggNullMarkers.RemoveAll;
begin
  if FMarkers <> nil then
      FMarkers.RemoveAll;
end;

procedure TAggNullMarkers.AddVertex(x, y: Double; Cmd: Cardinal);
begin
  if FMarkers <> nil then
      FMarkers.AddVertex(x, y, Cmd);
end;

procedure TAggNullMarkers.PrepareSource;
begin
end;

procedure TAggNullMarkers.Rewind(PathID: Cardinal);
begin
  if FMarkers <> nil then
      FMarkers.Rewind(PathID);
end;

function TAggNullMarkers.Vertex(x, y: PDouble): Cardinal;
begin
  if FMarkers <> nil then
      Result := FMarkers.Vertex(x, y)
  else
      Result := CAggPathCmdStop;
end;

procedure TAggNullMarkers.SetMarkers(Value: TAggVertexSource);
begin
  FMarkers := Value;
end;

{ TAggConvAdaptorVcgen }

constructor TAggConvAdaptorVcgen.Create(Source: TAggCustomVertexSource;
  Generator: TAggVertexSource);
begin
  inherited Create;

  FSource := Source;
  FStatus := siInitial;

  FGenerator := Generator;

  FMarkers := TAggNullMarkers.Create;

  FLastCmd := 0;
  FStart.x := 0;
  FStart.y := 0;
end;

destructor TAggConvAdaptorVcgen.Destroy;
begin
  FMarkers.Free;
  inherited;
end;

procedure TAggConvAdaptorVcgen.SetSource;
begin
  FSource := Source;
end;

procedure TAggConvAdaptorVcgen.SetMarkers(Value: TAggVertexSource);
begin
  FMarkers.SetMarkers(Value);
end;

function TAggConvAdaptorVcgen.GetMarkers: TAggVertexSource;
begin
  if FMarkers.FMarkers <> nil then
      Result := FMarkers.FMarkers
  else
      Result := FMarkers;
end;

procedure TAggConvAdaptorVcgen.Rewind(PathID: Cardinal);
begin
  FSource.Rewind(PathID);

  FStatus := siInitial;
end;

function TAggConvAdaptorVcgen.Vertex(x, y: PDouble): Cardinal;
var
  Cmd: Cardinal;
label
  _acc, _gen;

begin
  Cmd := CAggPathCmdStop;

  repeat
    case FStatus of
      siInitial:
        begin
          FMarkers.RemoveAll;

          FLastCmd := FSource.Vertex(@FStart.x, @FStart.y);
          FStatus := siAccumulate;

          goto _acc;
        end;

      siAccumulate:
        begin
        _acc:
          if IsStop(FLastCmd) then
            begin
              Result := CAggPathCmdStop;
              Exit;
            end;

          FGenerator.RemoveAll;
          FGenerator.AddVertex(FStart.x, FStart.y, CAggPathCmdMoveTo);
          FMarkers.AddVertex(FStart.x, FStart.y, CAggPathCmdMoveTo);

          repeat
            Cmd := FSource.Vertex(x, y);

            if IsVertex(Cmd) then
              begin
                FLastCmd := Cmd;

                if IsMoveTo(Cmd) then
                  begin
                    FStart := PointDouble(x^, y^);
                    Break;
                  end;

                FGenerator.AddVertex(x^, y^, Cmd);
                FMarkers.AddVertex(x^, y^, CAggPathCmdLineTo);
              end
            else
              begin
                if IsStop(Cmd) then
                  begin
                    FLastCmd := CAggPathCmdStop;
                    Break;
                  end;

                if IsEndPoly(Cmd) then
                  begin
                    FGenerator.AddVertex(x^, y^, Cmd);
                    Break;
                  end;
              end;
          until False;

          FGenerator.Rewind(0);
          FStatus := siGenerate;

          goto _gen;
        end;

      siGenerate:
        begin
        _gen:
          Cmd := FGenerator.Vertex(x, y);

          if IsStop(Cmd) then
            begin
              FStatus := siAccumulate;
              Continue;
            end;

          Break;
        end;
    end;
  until False;

  Result := Cmd;
end;

end.
 
 
 
