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
unit AggVcgenMarkersTerm;

{$DEFINE FPC_DELPHI_MODE}
{$INCLUDE zDefine.inc}
interface
uses
  AggBasics,
  AggArray,
  AggVertexSource,
  AggVertexSequence;

type
  TAggVcgenMarkersTerm = class(TAggVertexSource)
  private
    FMarkers: TAggPodDeque;
    FCurrentID, FCurrentIndex: Cardinal;
  public
    constructor Create;
    destructor Destroy; override;

    // Vertex Generator Interface
    procedure RemoveAll; override;
    procedure AddVertex(x, y: Double; Cmd: Cardinal); override;

    // Vertex Source Interface
    procedure Rewind(PathID: Cardinal); override;
    function Vertex(x, y: PDouble): Cardinal; override;
  end;

implementation


{ TAggVcgenMarkersTerm }

constructor TAggVcgenMarkersTerm.Create;
begin
  FMarkers := TAggPodDeque.Create(SizeOf(TPointDouble), 6);

  FCurrentID := 0;
  FCurrentIndex := 0;
end;

destructor TAggVcgenMarkersTerm.Destroy;
begin
  FMarkers.Free;

  inherited
end;

procedure TAggVcgenMarkersTerm.RemoveAll;
begin
  FMarkers.RemoveAll;
end;

procedure TAggVcgenMarkersTerm.AddVertex(x, y: Double; Cmd: Cardinal);
var
  ct: TPointDouble;
begin
  if IsMoveTo(Cmd) then
    if FMarkers.Size and 1 <> 0 then
      begin
        // Initial state, the first coordinate was added.
        // If two of more calls of StartVertex() occures
        // we just modify the last one.
        ct := PointDouble(x, y);
        FMarkers.ModifyLast(@ct);
      end
    else
      begin
        ct := PointDouble(x, y);
        FMarkers.Add(@ct);
      end
  else if IsVertex(Cmd) then
    if FMarkers.Size and 1 <> 0 then
      begin
        // Initial state, the first coordinate was added.
        // Add three more points, 0,1,1,0
        ct := PointDouble(x, y);
        FMarkers.Add(@ct);
        FMarkers.Add(FMarkers[FMarkers.Size - 1]);
        FMarkers.Add(FMarkers[FMarkers.Size - 3]);
      end
    else if FMarkers.Size <> 0 then
      begin
        // Replace two last points: 0,1,1,0 -> 0,1,2,1
        ct := PointDouble(x, y);

        Move(FMarkers[FMarkers.Size - 2]^,
          FMarkers[FMarkers.Size - 1]^, SizeOf(TPointDouble));

        Move(ct, FMarkers[FMarkers.Size - 2]^,
          SizeOf(TPointDouble));
      end;
end;

procedure TAggVcgenMarkersTerm.Rewind(PathID: Cardinal);
begin
  FCurrentID := PathID * 2;
  FCurrentIndex := FCurrentID;
end;

function TAggVcgenMarkersTerm.Vertex(x, y: PDouble): Cardinal;
var
  c: PPointDouble;
begin
  if (FCurrentID > 2) or (FCurrentIndex >= FMarkers.Size) then
    begin
      Result := CAggPathCmdStop;

      Exit;
    end;

  c := FMarkers[FCurrentIndex];

  x^ := c.x;
  y^ := c.y;

  if FCurrentIndex and 1 <> 0 then
    begin
      inc(FCurrentIndex, 3);

      Result := CAggPathCmdLineTo;

      Exit;
    end;

  inc(FCurrentIndex);

  Result := CAggPathCmdMoveTo;
end;

end. 
 
 
