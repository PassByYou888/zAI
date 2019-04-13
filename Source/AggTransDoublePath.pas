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
unit AggTransDoublePath;

{$DEFINE FPC_DELPHI_MODE}
{$INCLUDE zDefine.inc}
interface
uses
  AggBasics,
  AggVertexSource,
  AggVertexSequence,
  AggTransAffine;

type
  TAggInternalStatus = (siInitial, siMakingPath, siReady);

  TAggTransDoublePath = class(TAggTransAffine)
  private
    FSourceVertices: array [0 .. 1] of TAggVertexSequence;

    FBaseLength, FBaseHeight: Double;
    FKIndex: array [0 .. 1] of Double;

    FStatus: array [0 .. 1] of TAggInternalStatus;

    FPreserveXScale: Boolean;

    procedure SetBaseLength(v: Double);
    function GetBaseLength: Double;

    procedure SetBaseHeight(v: Double);
    function GetBaseHeight: Double;

    procedure SetPreserveXScale(f: Boolean);
    function GetPreserveXScale: Boolean;
  public
    constructor Create; override;
    destructor Destroy; override;

    procedure Reset; virtual;

    procedure MoveTo1(x, y: Double);
    procedure LineTo1(x, y: Double);
    procedure MoveTo2(x, y: Double);
    procedure LineTo2(x, y: Double);
    procedure FinalizePaths;

    procedure AddPaths(Vs1, Vs2: TAggVertexSource; Path1ID: Cardinal = 0; Path2ID: Cardinal = 0);

    function TotalLength1: Double;
    function TotalLength2: Double;

    function FinalizePath(Vertices: TAggVertexSequence): Double;

    procedure Transform1(Vertices: TAggVertexSequence; kIndex, KX: Double; x, y: PDouble);

    property BaseLength: Double read GetBaseLength write SetBaseLength;
    property BaseHeight: Double read GetBaseHeight write SetBaseHeight;
    property PreserveXScale: Boolean read GetPreserveXScale write SetPreserveXScale;
  end;

implementation


procedure DoublePathTransform(This: TAggTransDoublePath; x, y: PDouble);
var
  Rect: TRectDouble;
  DD: Double;
begin
  if (This.FStatus[0] = siReady) and (This.FStatus[1] = siReady) then
    begin
      if This.FBaseLength > 1E-10 then
          x^ := x^ * (PAggVertexDistance(This.FSourceVertices[0][
          This.FSourceVertices[0].Size - 1]).Dist / This.FBaseLength);

      Rect.x1 := x^;
      Rect.y1 := y^;
      Rect.x2 := x^;
      Rect.y2 := y^;
      DD := PAggVertexDistance(This.FSourceVertices[1][
        This.FSourceVertices[1].Size - 1]).Dist /
        PAggVertexDistance(This.FSourceVertices[0][
        This.FSourceVertices[0].Size - 1]).Dist;

      This.Transform1(This.FSourceVertices[0], This.FKIndex[0], 1.0, @Rect.x1,
        @Rect.y1);
      This.Transform1(This.FSourceVertices[1], This.FKIndex[1], DD, @Rect.x2,
        @Rect.y2);

      x^ := Rect.x1 + y^ * (Rect.x2 - Rect.x1) / This.FBaseHeight;
      y^ := Rect.y1 + y^ * (Rect.y2 - Rect.y1) / This.FBaseHeight;
    end;
end;

{ TAggTransDoublePath }

constructor TAggTransDoublePath.Create;
begin
  inherited Create;

  Transform := @DoublePathTransform;

  FSourceVertices[0] := TAggVertexSequence.Create(SizeOf(TAggVertexDistance));
  FSourceVertices[1] := TAggVertexSequence.Create(SizeOf(TAggVertexDistance));

  FKIndex[0] := 0;
  FKIndex[1] := 0;

  FBaseLength := 0;
  FBaseHeight := 1;

  FStatus[0] := siInitial;
  FStatus[1] := siInitial;

  FPreserveXScale := True;
end;

destructor TAggTransDoublePath.Destroy;
begin
  FSourceVertices[0].Free;
  FSourceVertices[1].Free;
  inherited
end;

procedure TAggTransDoublePath.SetBaseLength;
begin
  FBaseLength := v;
end;

function TAggTransDoublePath.GetBaseLength;
begin
  Result := FBaseLength;
end;

procedure TAggTransDoublePath.SetBaseHeight;
begin
  FBaseHeight := v;
end;

function TAggTransDoublePath.GetBaseHeight;
begin
  Result := FBaseHeight;
end;

procedure TAggTransDoublePath.SetPreserveXScale;
begin
  FPreserveXScale := f;
end;

function TAggTransDoublePath.GetPreserveXScale;
begin
  Result := FPreserveXScale;
end;

procedure TAggTransDoublePath.Reset;
begin
  FSourceVertices[0].RemoveAll;
  FSourceVertices[1].RemoveAll;

  FKIndex[0] := 0.0;
  FKIndex[0] := 0.0;
  FStatus[0] := siInitial;
  FStatus[1] := siInitial;
end;

procedure TAggTransDoublePath.MoveTo1;
var
  VD: TAggVertexDistance;
begin
  if FStatus[0] = siInitial then
    begin
      VD.Pos := PointDouble(x, y);

      VD.Dist := 0;

      FSourceVertices[0].ModifyLast(@VD);

      FStatus[0] := siMakingPath;
    end
  else
      LineTo1(x, y);
end;

procedure TAggTransDoublePath.LineTo1;
var
  VD: TAggVertexDistance;
begin
  if FStatus[0] = siMakingPath then
    begin
      VD.Pos := PointDouble(x, y);

      VD.Dist := 0;

      FSourceVertices[0].Add(@VD);
    end;
end;

procedure TAggTransDoublePath.MoveTo2;
var
  VD: TAggVertexDistance;
begin
  if FStatus[1] = siInitial then
    begin
      VD.Pos := PointDouble(x, y);

      VD.Dist := 0;

      FSourceVertices[1].ModifyLast(@VD);

      FStatus[1] := siMakingPath;
    end
  else
      LineTo2(x, y);
end;

procedure TAggTransDoublePath.LineTo2;
var
  VD: TAggVertexDistance;
begin
  if FStatus[1] = siMakingPath then
    begin
      VD.Pos := PointDouble(x, y);

      VD.Dist := 0;

      FSourceVertices[1].Add(@VD);
    end;
end;

procedure TAggTransDoublePath.FinalizePaths;
begin
  if (FStatus[0] = siMakingPath) and (FSourceVertices[0].Size > 1) and
    (FStatus[1] = siMakingPath) and (FSourceVertices[1].Size > 1) then
    begin
      FKIndex[0] := FinalizePath(FSourceVertices[0]);
      FKIndex[1] := FinalizePath(FSourceVertices[1]);
      FStatus[0] := siReady;
      FStatus[1] := siReady;
    end;
end;

procedure TAggTransDoublePath.AddPaths;
var
  x, y: Double;
  Cmd: Cardinal;
begin
  Vs1.Rewind(Path1ID);

  Cmd := Vs1.Vertex(@x, @y);

  while not IsStop(Cmd) do
    begin
      if IsMoveTo(Cmd) then
          MoveTo1(x, y)
      else if IsVertex(Cmd) then
          LineTo1(x, y);

      Cmd := Vs1.Vertex(@x, @y);
    end;

  Vs2.Rewind(Path2ID);

  Cmd := Vs2.Vertex(@x, @y);

  while not IsStop(Cmd) do
    begin
      if IsMoveTo(Cmd) then
          MoveTo2(x, y)
      else if IsVertex(Cmd) then
          LineTo2(x, y);

      Cmd := Vs2.Vertex(@x, @y);
    end;

  FinalizePaths;
end;

function TAggTransDoublePath.TotalLength1;
begin
  if FBaseLength >= 1E-10 then
      Result := FBaseLength
  else if FStatus[0] = siReady then
      Result := PAggVertexDistance(FSourceVertices[0][
      FSourceVertices[0].Size - 1]).Dist
  else
      Result := 0.0;
end;

function TAggTransDoublePath.TotalLength2;
begin
  if FBaseLength >= 1E-10 then
      Result := FBaseLength
  else if FStatus[1] = siReady then
      Result := PAggVertexDistance(FSourceVertices[1][
      FSourceVertices[1].Size - 1]).Dist
  else
      Result := 0.0;
end;

function TAggTransDoublePath.FinalizePath;
var
  i: Cardinal;
  v: PAggVertexDistance;

  d, Dist: Double;

begin
  Vertices.Close(False);

  if Vertices.Size > 2 then
    if PAggVertexDistance(Vertices[Vertices.Size - 2]).Dist * 10 <
      PAggVertexDistance(Vertices[Vertices.Size - 3]).Dist then
      begin
        d := PAggVertexDistance(Vertices[Vertices.Size - 3]).Dist +
          PAggVertexDistance(Vertices[Vertices.Size - 2]).Dist;

        Move(Vertices[Vertices.Size - 1]^, Vertices[Vertices.Size - 2]^,
          SizeOf(TAggVertexDistance));

        Vertices.RemoveLast;

        PAggVertexDistance(Vertices[Vertices.Size - 2]).Dist := d;
      end;

  Dist := 0;

  for i := 0 to Vertices.Size - 1 do
    begin
      v := Vertices[i];
      d := v.Dist;

      v.Dist := Dist;
      Dist := Dist + d;
    end;

  Result := (Vertices.Size - 1) / Dist;
end;

procedure TAggTransDoublePath.Transform1(Vertices: TAggVertexSequence;
  kIndex, KX: Double; x, y: PDouble);
var
  Delta: TPointDouble;
  x1, y1, d, DD: Double;
  i, j, k: Cardinal;
begin
  x1 := 0;
  y1 := 0;
  Delta.x := 1;
  Delta.y := 1;
  d := 0;
  DD := 1;

  x^ := x^ * KX;

  if x^ < 0.0 then
    begin
      // Extrapolation on the left
      x1 := PAggVertexDistance(Vertices[0]).Pos.x;
      y1 := PAggVertexDistance(Vertices[0]).Pos.y;
      Delta.x := PAggVertexDistance(Vertices[1]).Pos.x - x1;
      Delta.y := PAggVertexDistance(Vertices[1]).Pos.y - y1;
      DD := PAggVertexDistance(Vertices[1]).Dist -
        PAggVertexDistance(Vertices[0]).Dist;
      d := x^;
    end
  else
    if x^ > PAggVertexDistance(Vertices[Vertices.Size - 1]).Dist then
    begin
      i := Vertices.Size - 2;
      j := Vertices.Size - 1;

      x1 := PAggVertexDistance(Vertices[j]).Pos.x;
      y1 := PAggVertexDistance(Vertices[j]).Pos.y;
      Delta.x := x1 - PAggVertexDistance(Vertices[i]).Pos.x;
      Delta.y := y1 - PAggVertexDistance(Vertices[i]).Pos.y;
      DD := PAggVertexDistance(Vertices[j]).Dist -
        PAggVertexDistance(Vertices[i]).Dist;
      d := x^ - PAggVertexDistance(Vertices[j]).Dist;
    end
  else
    begin
      // Interpolation
      i := 0;
      j := Vertices.Size - 1;

      if FPreserveXScale then
        begin
          i := 0;

          while j - i > 1 do
            begin
              k := (i + j) shr 1;

              if x^ < PAggVertexDistance(Vertices[k]).Dist then
                  j := k
              else
                  i := k;
            end;

          d := PAggVertexDistance(Vertices[i]).Dist;
          DD := PAggVertexDistance(Vertices[j]).Dist - d;
          d := x^ - d;
        end
      else
        begin
          i := Trunc(x^ * kIndex);
          j := i + 1;
          DD := PAggVertexDistance(Vertices[j]).Dist -
            PAggVertexDistance(Vertices[i]).Dist;
          d := ((x^ * kIndex) - i) * DD;
        end;

      x1 := PAggVertexDistance(Vertices[i]).Pos.x;
      y1 := PAggVertexDistance(Vertices[i]).Pos.y;
      Delta.x := PAggVertexDistance(Vertices[j]).Pos.x - x1;
      Delta.y := PAggVertexDistance(Vertices[j]).Pos.y - y1;
    end;

  DD := d / DD;
  x^ := x1 + Delta.x * DD;
  y^ := y1 + Delta.y * DD;
end;

end. 
 
 
 
