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
unit AggRasterizerOutlineAA;

{$DEFINE FPC_DELPHI_MODE}
{$INCLUDE zDefine.inc}
interface
uses
  AggBasics,
  AggColor32,
  AggLineAABasics,
  AggVertexSource,
  AggVertexSequence,
  AggRendererOutlineAA,
  AggControl;

type
  // Vertex (x, y) with the distance to the next one. The last vertex has
  // the distance between the last and the first points
  PAggLineAAVertex = ^TAggLineAAVertex;

  TAggLineAAVertex = record
    x, y, Len: Integer;
    procedure Initialize(x, y: Integer);
  end;

  PAggDrawVars = ^TAggDrawVars;

  TAggDrawVars = record
    idx: Cardinal;
    x1, y1, x2, y2: Integer;
    Curr, Next: TAggLineParameters;
    Lcurr, Lnext, Xb1, Yb1, Xb2, Yb2: Integer;
    Flags: Cardinal;
  end;

  TAggRasterizerOutlineAA = class
  private
    FRen: TAggRendererOutline;
    FSourceVertices: TAggVertexSequence;
    FAccurateJoin, FRoundCap: Boolean;
    FStart: TPointInteger;

    function GetAccurateJoin: Boolean;
    function GetRoundCap: Boolean;
    procedure SetAccurateJoin(v: Boolean);
    procedure SetRoundCap(v: Boolean);
  public
    constructor Create(Ren: TAggRendererOutline);
    destructor Destroy; override;

    procedure Renderer(Ren: TAggRendererOutline);

    procedure Draw(DV: PAggDrawVars; Start, stop: Cardinal);

    procedure MoveTo(x, y: Integer);
    procedure LineTo(x, y: Integer);

    procedure MoveToDouble(x, y: Double);
    procedure LineToDouble(x, y: Double);

    procedure Render(ClosePolygon: Boolean);

    procedure AddVertex(x, y: Double; Cmd: Cardinal);
    procedure AddPath(VertexSource: TAggCustomVertexSource; PathID: Cardinal = 0);

    procedure RenderAllPaths(VertexSource: TAggVertexSource; Colors: PAggColor; PathID: PCardinal; PathCount: Cardinal);

    procedure RenderControl(c: TAggCustomAggControl);

    property RoundCap: Boolean read GetRoundCap write SetRoundCap;
    property AccurateJoin: Boolean read GetAccurateJoin write SetAccurateJoin;
  end;

function CompareDistStart(d: Integer): Boolean;
function CompareDistEnd(d: Integer): Boolean;

function LineAAVertexFuncOperator(This, val: PAggLineAAVertex): Boolean;

implementation


function CompareDistStart;
begin
  Result := d > 0;
end;

function CompareDistEnd;
begin
  Result := d <= 0;
end;

{ TAggLineAAVertex }

procedure TAggLineAAVertex.Initialize(x, y: Integer);
begin
  Self.x := x;
  Self.y := y;
  Len := 0;
end;

function LineAAVertexFuncOperator;
var
  dx, dy: Double;

begin
  dx := val.x - This.x;
  dy := val.y - This.y;

  This.Len := Trunc(Sqrt(dx * dx + dy * dy));

  Result := This.Len > (CAggLineSubpixelSize + CAggLineSubpixelSize div 2);
end;

{ TAggRasterizerOutlineAA }

constructor TAggRasterizerOutlineAA.Create(Ren: TAggRendererOutline);
begin
  FSourceVertices := TAggVertexSequence.Create(SizeOf(TAggLineAAVertex), 6,
    @LineAAVertexFuncOperator);

  FRen := Ren;

  FAccurateJoin := FRen.AccurateJoinOnly;
  FRoundCap := False;

  FStart := PointInteger(0);
end;

destructor TAggRasterizerOutlineAA.Destroy;
begin
  FSourceVertices.Free;
  inherited;
end;

procedure TAggRasterizerOutlineAA.Renderer(Ren: TAggRendererOutline);
begin
  FRen := Ren;
end;

procedure TAggRasterizerOutlineAA.Draw(DV: PAggDrawVars; Start, stop: Cardinal);
var
  i: Cardinal;
  v: PAggLineAAVertex;
begin
  i := Start;

  while i < stop do
    begin
      case DV.Flags of
        0:
          FRen.Line3(@DV.Curr, DV.Xb1, DV.Yb1, DV.Xb2, DV.Yb2);
        1:
          FRen.Line2(@DV.Curr, DV.Xb2, DV.Yb2);
        2:
          FRen.Line1(@DV.Curr, DV.Xb1, DV.Yb1);
        3:
          FRen.Line0(@DV.Curr);
      end;

      DV.x1 := DV.x2;
      DV.y1 := DV.y2;

      DV.Lcurr := DV.Lnext;
      DV.Lnext := PAggLineAAVertex(FSourceVertices[DV.idx]).Len;

      inc(DV.idx);

      if DV.idx >= FSourceVertices.Size then
          DV.idx := 0;

      v := FSourceVertices[DV.idx];

      DV.x2 := v.x;
      DV.y2 := v.y;

      DV.Curr := DV.Next;

      DV.Next.Initialize(DV.x1, DV.y1, DV.x2, DV.y2, DV.Lnext);

      DV.Xb1 := DV.Xb2;
      DV.Yb1 := DV.Yb2;

      if FAccurateJoin then
          DV.Flags := 0
      else
        begin
          DV.Flags := DV.Flags shr 1;

          DV.Flags := DV.Flags or
            (Cardinal(DV.Curr.DiagonalQuadrant = DV.Next.DiagonalQuadrant) shl 1);
        end;

      if DV.Flags and 2 = 0 then
          Bisectrix(@DV.Curr, @DV.Next, @DV.Xb2, @DV.Yb2);

      inc(i)
    end;
end;

procedure TAggRasterizerOutlineAA.SetAccurateJoin;
begin
  if FRen.AccurateJoinOnly then
      FAccurateJoin := True
  else
      FAccurateJoin := v;
end;

function TAggRasterizerOutlineAA.GetAccurateJoin;
begin
  Result := FAccurateJoin;
end;

procedure TAggRasterizerOutlineAA.SetRoundCap;
begin
  FRoundCap := v;
end;

function TAggRasterizerOutlineAA.GetRoundCap;
begin
  Result := FRoundCap;
end;

procedure TAggRasterizerOutlineAA.MoveTo(x, y: Integer);
var
  VT: TAggLineAAVertex;
begin
  FStart := PointInteger(x, y);

  VT.Initialize(x, y);

  FSourceVertices.ModifyLast(@VT);
end;

procedure TAggRasterizerOutlineAA.LineTo(x, y: Integer);
var
  VT: TAggLineAAVertex;
begin
  VT.Initialize(x, y);

  FSourceVertices.Add(@VT);
end;

procedure TAggRasterizerOutlineAA.MoveToDouble(x, y: Double);
begin
  MoveTo(LineCoord(x), LineCoord(y));
end;

procedure TAggRasterizerOutlineAA.LineToDouble(x, y: Double);
begin
  LineTo(LineCoord(x), LineCoord(y));
end;

procedure TAggRasterizerOutlineAA.Render(ClosePolygon: Boolean);
var
  DV: TAggDrawVars;
  v: PAggLineAAVertex;
  x1, y1, x2, y2, Lprev, x3, y3, Lnext: Integer;
  Prev, LP, Lp1, Lp2: TAggLineParameters;
begin
  FSourceVertices.Close(ClosePolygon);

  if ClosePolygon then
    if FSourceVertices.Size >= 3 then
      begin
        DV.idx := 2;

        v := FSourceVertices[FSourceVertices.Size - 1];
        x1 := v.x;
        y1 := v.y;
        Lprev := v.Len;

        v := FSourceVertices[0];
        x2 := v.x;
        y2 := v.y;

        DV.Lcurr := v.Len;

        Prev.Initialize(x1, y1, x2, y2, Lprev);

        v := FSourceVertices[1];
        DV.x1 := v.x;
        DV.y1 := v.y;

        DV.Lnext := v.Len;

        DV.Curr.Initialize(x2, y2, DV.x1, DV.y1, DV.Lcurr);

        v := FSourceVertices[DV.idx];
        DV.x2 := v.x;
        DV.y2 := v.y;

        DV.Next.Initialize(DV.x1, DV.y1, DV.x2, DV.y2, DV.Lnext);

        DV.Xb1 := 0;
        DV.Yb1 := 0;
        DV.Xb2 := 0;
        DV.Yb2 := 0;

        if FAccurateJoin then
            DV.Flags := 0
        else
            DV.Flags := Cardinal(Prev.DiagonalQuadrant = DV.Curr.DiagonalQuadrant)
            or (Cardinal(DV.Curr.DiagonalQuadrant = DV.Next.DiagonalQuadrant) shl 1);

        if DV.Flags and 1 = 0 then
            Bisectrix(@Prev, @DV.Curr, @DV.Xb1, @DV.Yb1);

        if DV.Flags and 2 = 0 then
            Bisectrix(@DV.Curr, @DV.Next, @DV.Xb2, @DV.Yb2);

        Draw(@DV, 0, FSourceVertices.Size);
      end
    else
  else
    case FSourceVertices.Size of
      2:
        begin
          v := FSourceVertices[0];
          x1 := v.x;
          y1 := v.y;
          Lprev := v.Len;
          v := FSourceVertices[1];
          x2 := v.x;
          y2 := v.y;

          LP.Initialize(x1, y1, x2, y2, Lprev);

          if FRoundCap then
              FRen.Semidot(@CompareDistStart, x1, y1, x1 + (y2 - y1),
              y1 - (x2 - x1));

          FRen.Line3(@LP, x1 + (y2 - y1), y1 - (x2 - x1), x2 + (y2 - y1),
            y2 - (x2 - x1));

          if FRoundCap then
              FRen.Semidot(@CompareDistEnd, x2, y2, x2 + (y2 - y1),
              y2 - (x2 - x1));
        end;

      3:
        begin
          v := FSourceVertices[0];
          x1 := v.x;
          y1 := v.y;
          Lprev := v.Len;
          v := FSourceVertices[1];
          x2 := v.x;
          y2 := v.y;
          Lnext := v.Len;
          v := FSourceVertices[2];
          x3 := v.x;
          y3 := v.y;

          Lp1.Initialize(x1, y1, x2, y2, Lprev);
          Lp2.Initialize(x2, y2, x3, y3, Lnext);

          Bisectrix(@Lp1, @Lp2, @DV.Xb1, @DV.Yb1);

          if FRoundCap then
              FRen.Semidot(@CompareDistStart, x1, y1, x1 + (y2 - y1),
              y1 - (x2 - x1));

          FRen.Line3(@Lp1, x1 + (y2 - y1), y1 - (x2 - x1), DV.Xb1, DV.Yb1);

          FRen.Line3(@Lp2, DV.Xb1, DV.Yb1, x3 + (y3 - y2), y3 - (x3 - x2));

          if FRoundCap then
              FRen.Semidot(@CompareDistEnd, x3, y3, x3 + (y3 - y2),
              y3 - (x3 - x2));
        end;

      0, 1:
      else
        begin
          DV.idx := 3;

          v := FSourceVertices[0];
          x1 := v.x;
          y1 := v.y;
          Lprev := v.Len;

          v := FSourceVertices[1];
          x2 := v.x;
          y2 := v.y;

          DV.Lcurr := v.Len;

          Prev.Initialize(x1, y1, x2, y2, Lprev);

          v := FSourceVertices[2];
          DV.x1 := v.x;
          DV.y1 := v.y;

          DV.Lnext := v.Len;

          DV.Curr.Initialize(x2, y2, DV.x1, DV.y1, DV.Lcurr);

          v := FSourceVertices[DV.idx];
          DV.x2 := v.x;
          DV.y2 := v.y;

          DV.Next.Initialize(DV.x1, DV.y1, DV.x2, DV.y2, DV.Lnext);

          DV.Xb1 := 0;
          DV.Yb1 := 0;
          DV.Xb2 := 0;
          DV.Yb2 := 0;

          if FAccurateJoin then
              DV.Flags := 0
          else
              DV.Flags :=
              Cardinal(Prev.DiagonalQuadrant = DV.Curr.DiagonalQuadrant) or
              (Cardinal(DV.Curr.DiagonalQuadrant = DV.Next.
              DiagonalQuadrant) shl 1);

          if DV.Flags and 1 = 0 then
            begin
              Bisectrix(@Prev, @DV.Curr, @DV.Xb1, @DV.Yb1);
              FRen.Line3(@Prev, x1 + (y2 - y1), y1 - (x2 - x1), DV.Xb1, DV.Yb1);

            end
          else
              FRen.Line1(@Prev, x1 + (y2 - y1), y1 - (x2 - x1));

          if FRoundCap then
              FRen.Semidot(@CompareDistStart, x1, y1, x1 + (y2 - y1),
              y1 - (x2 - x1));

          if DV.Flags and 2 = 0 then
              Bisectrix(@DV.Curr, @DV.Next, @DV.Xb2, @DV.Yb2);

          Draw(@DV, 1, FSourceVertices.Size - 2);

          if DV.Flags and 1 = 0 then
              FRen.Line3(@DV.Curr, DV.Xb1, DV.Yb1,
              DV.Curr.x2 + (DV.Curr.y2 - DV.Curr.y1),
              DV.Curr.y2 - (DV.Curr.x2 - DV.Curr.x1))
          else
              FRen.Line2(@DV.Curr, DV.Curr.x2 + (DV.Curr.y2 - DV.Curr.y1),
              DV.Curr.y2 - (DV.Curr.x2 - DV.Curr.x1));

          if FRoundCap then
              FRen.Semidot(@CompareDistEnd, DV.Curr.x2, DV.Curr.y2,
              DV.Curr.x2 + (DV.Curr.y2 - DV.Curr.y1),
              DV.Curr.y2 - (DV.Curr.x2 - DV.Curr.x1));
        end;
    end;

  FSourceVertices.RemoveAll;
end;

procedure TAggRasterizerOutlineAA.AddVertex;
begin
  if IsMoveTo(Cmd) then
    begin
      Render(False);
      MoveToDouble(x, y);
    end
  else if IsEndPoly(Cmd) then
    begin
      Render(IsClosed(Cmd));

      if IsClosed(Cmd) then
          MoveTo(FStart.x, FStart.y);
    end
  else
      LineToDouble(x, y);
end;

procedure TAggRasterizerOutlineAA.AddPath(VertexSource: TAggCustomVertexSource; PathID: Cardinal = 0);
var
  x, y: Double;
  Cmd: Cardinal;
begin
  VertexSource.Rewind(PathID);

  Cmd := VertexSource.Vertex(@x, @y);

  while not IsStop(Cmd) do
    begin
      AddVertex(x, y, Cmd);

      Cmd := VertexSource.Vertex(@x, @y);
    end;

  Render(False);
end;

procedure TAggRasterizerOutlineAA.RenderAllPaths;
var
  i: Cardinal;
begin
  for i := 0 to PathCount - 1 do
    begin
      FRen.SetColor(PAggColor(PtrComp(Colors) + i * SizeOf(TAggColor)));
      AddPath(VertexSource, PCardinal(PtrComp(PathID) + i * SizeOf(Cardinal))^);
    end;
end;

procedure TAggRasterizerOutlineAA.RenderControl;
var
  i: Cardinal;
begin
  for i := 0 to c.PathCount - 1 do
    begin
      FRen.SetColor(c.ColorPointer[i]);
      AddPath(c, i);
    end;
end;

end. 
 
 
 
