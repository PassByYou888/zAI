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
unit AggVcgenDash;

{$DEFINE FPC_DELPHI_MODE}
{$INCLUDE zDefine.inc}
interface
uses
  AggBasics,
  AggVertexSource,
  AggVertexSequence,
  AggShortenPath;

const
  CMaxDashes = 32;

type
  TAggInternalStatus = (siInitial, siReady, siPolyline, siStop);

  TAggVcgenDash = class(TAggVertexSource)
  private
    FDashes: array [0 .. CMaxDashes - 1] of Double;

    FTotalDashLen: Double;
    FNumDashes: Cardinal;
    FDashStart, FShorten, FCurrDashStart: Double;

    FCurrentDash: Cardinal;
    FCurrentRest: Double;

    FVertex1, FVertex2: PAggVertexDistance;

    FSourceVertices: TAggVertexSequence;

    FClosed: Cardinal;
    FStatus: TAggInternalStatus;

    FSourceVertex: Cardinal;

    function GetShorten: Double;
    procedure SetShorten(Value: Double);
    procedure SetDashStart(Value: Double);
  protected
    procedure CalculateDashStart(DS: Double);
  public
    constructor Create;
    destructor Destroy; override;

    procedure RemoveAllDashes;
    procedure AddDash(DashLength, GapLength: Double);

    // Vertex Generator Interface
    procedure RemoveAll; override;
    procedure AddVertex(x, y: Double; Cmd: Cardinal); override;

    // Vertex Source Interface
    procedure Rewind(PathID: Cardinal); override;
    function Vertex(x, y: PDouble): Cardinal; override;

    property Shorten: Double read GetShorten write SetShorten;
    property DashStart: Double read FDashStart write SetDashStart;
  end;

implementation


{ TAggVcgenDash }

constructor TAggVcgenDash.Create;
begin
  FSourceVertices := TAggVertexSequence.Create(SizeOf(TAggVertexDistance));

  FTotalDashLen := 0.0;
  FNumDashes := 0;
  FDashStart := 0.0;
  FShorten := 0.0;
  FCurrDashStart := 0.0;
  FCurrentDash := 0;

  FClosed := 0;
  FStatus := siInitial;
  FSourceVertex := 0;
end;

destructor TAggVcgenDash.Destroy;
begin
  FSourceVertices.Free;
  inherited;
end;

procedure TAggVcgenDash.RemoveAllDashes;
begin
  FTotalDashLen := 0.0;
  FNumDashes := 0;
  FCurrDashStart := 0.0;
  FCurrentDash := 0;
end;

procedure TAggVcgenDash.AddDash(DashLength, GapLength: Double);
begin
  if FNumDashes < CMaxDashes then
    begin
      FTotalDashLen := FTotalDashLen + DashLength + GapLength;

      FDashes[FNumDashes] := DashLength;

      inc(FNumDashes);

      FDashes[FNumDashes] := GapLength;

      inc(FNumDashes);
    end;
end;

procedure TAggVcgenDash.SetDashStart(Value: Double);
begin
  FDashStart := Value;

  CalculateDashStart(Abs(Value));
end;

procedure TAggVcgenDash.SetShorten(Value: Double);
begin
  FShorten := Value;
end;

function TAggVcgenDash.GetShorten: Double;
begin
  Result := FShorten;
end;

procedure TAggVcgenDash.RemoveAll;
begin
  FStatus := siInitial;

  FSourceVertices.RemoveAll;

  FClosed := 0;
end;

procedure TAggVcgenDash.AddVertex(x, y: Double; Cmd: Cardinal);
var
  VD: TAggVertexDistance;
begin
  FStatus := siInitial;

  VD.Pos := PointDouble(x, y);
  VD.Dist := 0;

  if IsMoveTo(Cmd) then
      FSourceVertices.ModifyLast(@VD)
  else if IsVertex(Cmd) then
      FSourceVertices.Add(@VD)
  else
      FClosed := GetCloseFlag(Cmd);
end;

procedure TAggVcgenDash.Rewind(PathID: Cardinal);
begin
  if FStatus = siInitial then
    begin
      FSourceVertices.Close(Boolean(FClosed <> 0));

      ShortenPath(FSourceVertices, FShorten, FClosed);
    end;

  FStatus := siReady;
  FSourceVertex := 0;
end;

function TAggVcgenDash.Vertex(x, y: PDouble): Cardinal;
var
  DashRest, Temp: Double;
label
  _ready;

begin
  Result := CAggPathCmdMoveTo;

  while not IsStop(Result) do
    case FStatus of
      siInitial:
        begin
          Rewind(0);

          goto _ready;
        end;

      siReady:
      _ready:
        begin
          if (FNumDashes < 2) or (FSourceVertices.Size < 2) then
            begin
              Result := CAggPathCmdStop;
              Continue;
            end;

          FStatus := siPolyline;
          FSourceVertex := 1;

          FVertex1 := FSourceVertices[0];
          FVertex2 := FSourceVertices[1];

          FCurrentRest := FVertex1.Dist;

          x^ := FVertex1.Pos.x;
          y^ := FVertex1.Pos.y;

          if FDashStart >= 0.0 then
              CalculateDashStart(FDashStart);

          Result := CAggPathCmdMoveTo;

          Exit;
        end;

      siPolyline:
        begin
          DashRest := FDashes[FCurrentDash] - FCurrDashStart;

          if FCurrentDash and 1 <> 0 then
              Result := CAggPathCmdMoveTo
          else
              Result := CAggPathCmdLineTo;

          if FCurrentRest > DashRest then
            begin
              FCurrentRest := FCurrentRest - DashRest;

              inc(FCurrentDash);

              if FCurrentDash >= FNumDashes then
                  FCurrentDash := 0;

              FCurrDashStart := 0.0;

              Temp := FCurrentRest / FVertex1.Dist;
              x^ := FVertex2.Pos.x - (FVertex2.Pos.x - FVertex1.Pos.x) * Temp;
              y^ := FVertex2.Pos.y - (FVertex2.Pos.y - FVertex1.Pos.y) * Temp;
            end
          else
            begin
              FCurrDashStart := FCurrDashStart + FCurrentRest;

              x^ := FVertex2.Pos.x;
              y^ := FVertex2.Pos.y;

              inc(FSourceVertex);

              FVertex1 := FVertex2;

              FCurrentRest := FVertex1.Dist;

              if FClosed <> 0 then
                if FSourceVertex > FSourceVertices.Size then
                    FStatus := siStop
                else if FSourceVertex >= FSourceVertices.Size then
                    FVertex2 := FSourceVertices[0]
                else
                    FVertex2 := FSourceVertices[FSourceVertex]
              else if FSourceVertex >= FSourceVertices.Size then
                  FStatus := siStop
              else
                  FVertex2 := FSourceVertices[FSourceVertex];
            end;

          Exit;
        end;

      siStop:
        Result := CAggPathCmdStop;
    end;
end;

procedure TAggVcgenDash.CalculateDashStart;
begin
  FCurrentDash := 0;
  FCurrDashStart := 0.0;

  while DS > 0.0 do
    if DS > FDashes[FCurrentDash] then
      begin
        DS := DS - FDashes[FCurrentDash];

        inc(FCurrentDash);

        FCurrDashStart := 0.0;

        if FCurrentDash >= FNumDashes then
            FCurrentDash := 0;

      end
    else
      begin
        FCurrDashStart := DS;

        DS := 0.0;
      end;
end;

end. 
 
 
 
