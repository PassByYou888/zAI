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
unit AggVcgenVertexSequence;

{$DEFINE FPC_DELPHI_MODE}
{$INCLUDE zDefine.inc}
interface
uses
  AggBasics,
  AggVertexSequence,
  AggShortenPath,
  AggVertexSource;

type
  TAggVcgenVertexSequence = class(TAggVertexSource)
  private
    FSourceVertices: TAggVertexSequence;
    FFlags, FCurrentVertex: Cardinal;

    FShorten: Double;
    FReady: Boolean;

    procedure SetShorten(s: Double);
    function GetShorten: Double;
  public
    constructor Create;
    destructor Destroy; override;

    // Vertex Generator Interface
    procedure RemoveAll; override;
    procedure AddVertex(x, y: Double; Cmd: Cardinal); override;

    // Vertex Source Interface
    procedure Rewind(PathID: Cardinal); override;
    function Vertex(x, y: PDouble): Cardinal; override;

    property Shorten: Double read GetShorten write SetShorten;
  end;

implementation


{ TAggVcgenVertexSequence }

constructor TAggVcgenVertexSequence.Create;
begin
  FSourceVertices := TAggVertexSequence.Create(SizeOf(TAggVertexDistCmd), 6);

  FFlags := 0;
  FCurrentVertex := 0;
  FShorten := 0.0;
  FReady := False;
end;

destructor TAggVcgenVertexSequence.Destroy;
begin
  FSourceVertices.Free;

  inherited;
end;

procedure TAggVcgenVertexSequence.SetShorten(s: Double);
begin
  FShorten := s;
end;

function TAggVcgenVertexSequence.GetShorten: Double;
begin
  Result := FShorten;
end;

procedure TAggVcgenVertexSequence.RemoveAll;
begin
  FReady := False;

  FSourceVertices.RemoveAll;

  FCurrentVertex := 0;
  FFlags := 0;
end;

procedure TAggVcgenVertexSequence.AddVertex;
var
  VC: TAggVertexDistCmd;
begin
  FReady := False;

  VC.Pos := PointDouble(x, y);
  VC.Dist := 0;
  VC.Cmd := Cmd;

  if IsMoveTo(Cmd) then
      FSourceVertices.ModifyLast(@VC)
  else if IsVertex(Cmd) then
      FSourceVertices.Add(@VC)
  else
      FFlags := Cmd and CAggPathFlagsMask;
end;

procedure TAggVcgenVertexSequence.Rewind(PathID: Cardinal);
begin
  if not FReady then
    begin
      FSourceVertices.Close(IsClosed(FFlags));

      ShortenPath(FSourceVertices, FShorten, GetCloseFlag(FFlags));
    end;

  FReady := True;
  FCurrentVertex := 0;
end;

function TAggVcgenVertexSequence.Vertex(x, y: PDouble): Cardinal;
var
  v: PAggVertexDistCmd;
begin
  if not FReady then
      Rewind(0);

  if FCurrentVertex = FSourceVertices.Size then
    begin
      inc(FCurrentVertex);

      Result := CAggPathCmdEndPoly or FFlags;

      Exit;
    end;

  if FCurrentVertex > FSourceVertices.Size then
    begin
      Result := CAggPathCmdStop;

      Exit;
    end;

  v := FSourceVertices[FCurrentVertex];

  inc(FCurrentVertex);

  x^ := v.Pos.x;
  y^ := v.Pos.y;

  Result := v.Cmd;
end;

end. 
 
 
 
