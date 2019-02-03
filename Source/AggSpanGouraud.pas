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
unit AggSpanGouraud;

{$DEFINE FPC_DELPHI_MODE}
{$INCLUDE zDefine.inc}
interface
uses
  AggBasics,
  AggMath,
  AggSpanAllocator,
  AggSpanGenerator,
  AggColor32,
  AggVertexSource;

type
  PAggCoordType = ^TAggCoordType;

  TAggCoordType = record
    x, y: Double;
    COLOR: TAggColor;
  end;

  TAggSpanGouraud = class(TAggSpanGenerator)
  private
    FCoord: array [0 .. 2] of TAggCoordType;
    fx, fy: array [0 .. 7] of Double;
    FCmd: array [0 .. 7] of Cardinal;
    FVertex: Cardinal;
  protected
    procedure ArrangeVertices(Coord: PAggCoordType);
  public
    constructor Create(Alloc: TAggSpanAllocator); overload;
    constructor Create(Alloc: TAggSpanAllocator; c1, c2, c3: PAggColor; x1, y1, x2, y2, x3, y3, d: Double); overload;

    procedure SetColors(c1, c2, c3: PAggColor); overload;
    procedure SetColors(c1, c2, c3: TAggRgba8); overload;
    procedure Triangle(x1, y1, x2, y2, x3, y3, d: Double);

    // Vertex Source Interface to feed the coordinates to the Rasterizer
    procedure Rewind(PathID: Cardinal); override;
    function Vertex(x, y: PDouble): Cardinal; override;
  end;

implementation


{ TAggSpanGouraud }

constructor TAggSpanGouraud.Create(Alloc: TAggSpanAllocator);
begin
  inherited Create(Alloc);

  FCmd[0] := CAggPathCmdStop;
end;

constructor TAggSpanGouraud.Create(Alloc: TAggSpanAllocator;
  c1, c2, c3: PAggColor; x1, y1, x2, y2, x3, y3, d: Double);
begin
  inherited Create(Alloc);

  SetColors(c1, c2, c3);
  Triangle(x1, y1, x2, y2, x3, y3, d);
end;

// Sets the triangle and dilates it if needed.
// The trick here is to calculate beveled joins in the vertices of the
// triangle and render it as a 6-vertex polygon.
// It's necessary to achieve numerical stability.
// However, the coordinates to interpolate colors are calculated
// as miter joins (CalculateIntersection).
procedure TAggSpanGouraud.Triangle(x1, y1, x2, y2, x3, y3, d: Double);
begin
  FCoord[0].x := x1;
  fx[0] := x1;
  FCoord[0].y := y1;
  fy[0] := y1;
  FCoord[1].x := x2;
  fx[1] := x2;
  FCoord[1].y := y2;
  fy[1] := y2;
  FCoord[2].x := x3;
  fx[2] := x3;
  FCoord[2].y := y3;
  fy[2] := y3;

  FCmd[0] := CAggPathCmdMoveTo;
  FCmd[1] := CAggPathCmdLineTo;
  FCmd[2] := CAggPathCmdLineTo;
  FCmd[3] := CAggPathCmdStop;

  if d <> 0.0 then
    begin
      DilateTriangle(FCoord[0].x, FCoord[0].y, FCoord[1].x, FCoord[1].y,
        FCoord[2].x, FCoord[2].y, @fx, @fy, d);

      CalculateIntersection(fx[4], fy[4], fx[5], fy[5], fx[0], fy[0], fx[1],
        fy[1], @FCoord[0].x, @FCoord[0].y);

      CalculateIntersection(fx[0], fy[0], fx[1], fy[1], fx[2], fy[2], fx[3],
        fy[3], @FCoord[1].x, @FCoord[1].y);

      CalculateIntersection(fx[2], fy[2], fx[3], fy[3], fx[4], fy[4], fx[5],
        fy[5], @FCoord[2].x, @FCoord[2].y);

      FCmd[3] := CAggPathCmdLineTo;
      FCmd[4] := CAggPathCmdLineTo;
      FCmd[5] := CAggPathCmdLineTo;
      FCmd[6] := CAggPathCmdStop;
    end;
end;

procedure TAggSpanGouraud.Rewind(PathID: Cardinal);
begin
  FVertex := 0;
end;

procedure TAggSpanGouraud.SetColors(c1, c2, c3: PAggColor);
begin
  FCoord[0].COLOR := c1^;
  FCoord[1].COLOR := c2^;
  FCoord[2].COLOR := c3^;
end;

procedure TAggSpanGouraud.SetColors(c1, c2, c3: TAggRgba8);
var
  COLOR: TAggColor;
begin
  COLOR.Rgba8 := c1;
  FCoord[0].COLOR := COLOR;
  COLOR.Rgba8 := c2;
  FCoord[1].COLOR := COLOR;
  COLOR.Rgba8 := c3;
  FCoord[2].COLOR := COLOR;
end;

function TAggSpanGouraud.Vertex(x, y: PDouble): Cardinal;
begin
  x^ := fx[FVertex];
  y^ := fy[FVertex];

  Result := FCmd[FVertex];

  inc(FVertex);
end;

procedure TAggSpanGouraud.ArrangeVertices(Coord: PAggCoordType);
var
  tmp: TAggCoordType;
begin
  PAggCoordType(PtrComp(Coord))^ := FCoord[0];
  PAggCoordType(PtrComp(Coord) + SizeOf(TAggCoordType))^ := FCoord[1];
  PAggCoordType(PtrComp(Coord) + 2 * SizeOf(TAggCoordType))^ := FCoord[2];

  if FCoord[0].y > FCoord[2].y then
    begin
      PAggCoordType(Coord)^ := FCoord[2];
      PAggCoordType(PtrComp(Coord) + 2 * SizeOf(TAggCoordType))^ := FCoord[0];
    end;

  if PAggCoordType(Coord).y >
    PAggCoordType(PtrComp(Coord) + SizeOf(TAggCoordType)).y then
    begin
      tmp := PAggCoordType(PtrComp(Coord) + SizeOf(TAggCoordType))^;

      PAggCoordType(PtrComp(Coord) + SizeOf(TAggCoordType))^ :=
        PAggCoordType(Coord)^;

      PAggCoordType(Coord)^ := tmp;
    end;

  if PAggCoordType(PtrComp(Coord) + SizeOf(TAggCoordType)).y >
    PAggCoordType(PtrComp(Coord) + 2 * SizeOf(TAggCoordType)).y then
    begin
      tmp := PAggCoordType(PtrComp(Coord) + 2 * SizeOf(TAggCoordType))^;

      PAggCoordType(PtrComp(Coord) + 2 * SizeOf(TAggCoordType))^ :=
        PAggCoordType(PtrComp(Coord) + SizeOf(TAggCoordType))^;

      PAggCoordType(PtrComp(Coord) + SizeOf(TAggCoordType))^ := tmp;
    end;
end;

end. 
 
 
