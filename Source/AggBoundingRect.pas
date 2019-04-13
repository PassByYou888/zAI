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

unit AggBoundingRect;

{$DEFINE FPC_DELPHI_MODE}
{$INCLUDE zDefine.inc}

interface

uses
  AggBasics,
  AggVertexSource;

function BoundingRect(Vs: TAggVertexSource; Gi: PCardinal; Start, Num: Cardinal; x1, y1, x2, y2: PDouble): Boolean; overload;
function BoundingRect(Vs: TAggVertexSource; Gi: PCardinal; Start, Num: Cardinal; var Rect: TRectDouble): Boolean; overload;

function BoundingRectVertexSource(Vs, Gi: TAggVertexSource; Start, Num: Cardinal; x1, y1, x2, y2: PDouble): Boolean; overload;
function BoundingRectVertexSource(Vs, Gi: TAggVertexSource; Start, Num: Cardinal; var Rect: TRectDouble): Boolean; overload;

function BoundingRectInteger(Vs: TAggVertexSource; Ul: TCardinalList; Start, Num: Cardinal; x1, y1, x2, y2: PDouble): Boolean;

function BoundingRectSingle(Vs: TAggVertexSource; PathID: Cardinal; x1, y1, x2, y2: PDouble): Boolean;

function BoundingRectAllPaths(Vs: TAggVertexSource; x1, y1, x2, y2: PDouble): Boolean; overload;
function BoundingRectAllPaths(Vs: TAggVertexSource; Rect: TRectDouble): Boolean; overload;

implementation

function BoundingRect(Vs: TAggVertexSource; Gi: PCardinal;
  Start, Num: Cardinal; x1, y1, x2, y2: PDouble): Boolean;
var
  i, Cmd: Cardinal;
  x, y: Double;
  First: Boolean;
begin
  First := True;

  x1^ := 1;
  y1^ := 1;
  x2^ := 0;
  y2^ := 0;

  i := 0;

  while i < Num do
    begin
      Vs.Rewind(PCardinal(PtrComp(Gi) + (Start + i) * SizeOf(Cardinal))^);

      Cmd := Vs.Vertex(@x, @y);

      while not IsStop(Cmd) do
        begin
          if IsVertex(Cmd) then
            if First then
              begin
                x1^ := x;
                y1^ := y;
                x2^ := x;
                y2^ := y;

                First := False;
              end
            else
              begin
                if x < x1^ then
                    x1^ := x;

                if y < y1^ then
                    y1^ := y;

                if x > x2^ then
                    x2^ := x;

                if y > y2^ then
                    y2^ := y;
              end;

          Cmd := Vs.Vertex(@x, @y);
        end;

      inc(i);
    end;

  Result := (x1^ <= x2^) and (y1^ <= y2^);
end;

function BoundingRect(Vs: TAggVertexSource; Gi: PCardinal; Start, Num: Cardinal;
  var Rect: TRectDouble): Boolean;
begin
  Result := BoundingRect(Vs, Gi, Start, Num, @Rect.x1, @Rect.y1, @Rect.x2, @Rect.y2);
end;

function BoundingRectVertexSource(Vs, Gi: TAggVertexSource; Start, Num: Cardinal;
  x1, y1, x2, y2: PDouble): Boolean;
var
  i, Cmd: Cardinal;
  x, y: Double;
  First: Boolean;
begin
  First := True;

  x1^ := 1;
  y1^ := 1;
  x2^ := 0;
  y2^ := 0;

  i := 0;

  while i < Num do
    begin
      Vs.Rewind(Gi.PathID[Start + i]);

      Cmd := Vs.Vertex(@x, @y);

      while not IsStop(Cmd) do
        begin
          if IsVertex(Cmd) then
            if First then
              begin
                x1^ := x;
                y1^ := y;
                x2^ := x;
                y2^ := y;

                First := False;

              end
            else
              begin
                if x < x1^ then
                    x1^ := x;

                if y < y1^ then
                    y1^ := y;

                if x > x2^ then
                    x2^ := x;

                if y > y2^ then
                    y2^ := y;
              end;

          Cmd := Vs.Vertex(@x, @y);
        end;

      inc(i);
    end;

  Result := (x1^ <= x2^) and (y1^ <= y2^);
end;

function BoundingRectVertexSource(Vs, Gi: TAggVertexSource; Start, Num: Cardinal;
  var Rect: TRectDouble): Boolean;
begin
  Result := BoundingRectVertexSource(Vs, Gi, Start, Num, @Rect.x1, @Rect.y1, @Rect.x2, @Rect.y2)
end;

function BoundingRectInteger(Vs: TAggVertexSource; Ul: TCardinalList;
  Start, Num: Cardinal; x1, y1, x2, y2: PDouble): Boolean;
var
  i, Cmd: Cardinal;
  x, y: Double;
  First: Boolean;
begin
  First := True;

  x1^ := 1;
  y1^ := 1;
  x2^ := 0;
  y2^ := 0;

  i := 0;

  while i < Num do
    begin
      Vs.Rewind(Ul[Start + i]);

      Cmd := Vs.Vertex(@x, @y);

      while not IsStop(Cmd) do
        begin
          if IsVertex(Cmd) then
            if First then
              begin
                x1^ := x;
                y1^ := y;
                x2^ := x;
                y2^ := y;

                First := False;
              end
            else
              begin
                if x < x1^ then
                    x1^ := x;

                if y < y1^ then
                    y1^ := y;

                if x > x2^ then
                    x2^ := x;

                if y > y2^ then
                    y2^ := y;
              end;

          Cmd := Vs.Vertex(@x, @y);
        end;

      inc(i);
    end;

  Result := (x1^ <= x2^) and (y1^ <= y2^);
end;

function BoundingRectSingle(Vs: TAggVertexSource; PathID: Cardinal;
  x1, y1, x2, y2: PDouble): Boolean;
var
  Cmd: Cardinal;
  x, y: Double;
  First: Boolean;
begin
  First := True;

  x1^ := 1;
  y1^ := 1;
  x2^ := 0;
  y2^ := 0;

  Vs.Rewind(PathID);

  Cmd := Vs.Vertex(@x, @y);

  while not IsStop(Cmd) do
    begin
      if IsVertex(Cmd) then
        if First then
          begin
            x1^ := x;
            y1^ := y;
            x2^ := x;
            y2^ := y;

            First := False;
          end
        else
          begin
            if x < x1^ then
                x1^ := x;

            if y < y1^ then
                y1^ := y;

            if x > x2^ then
                x2^ := x;

            if y > y2^ then
                y2^ := y;
          end;

      Cmd := Vs.Vertex(@x, @y);
    end;

  Result := (x1^ <= x2^) and (y1^ <= y2^);
end;

function BoundingRectAllPaths(Vs: TAggVertexSource; x1, y1, x2, y2: PDouble): Boolean;
var
  i, Paths: Cardinal;
  Sx1, Sy1, Sx2, Sy2: Double;
  First: Boolean;
begin
  First := True;
  Paths := Vs.PathCount;

  x1^ := 1;
  y1^ := 1;
  x2^ := 0;
  y2^ := 0;

  i := 0;

  while i < Paths do
    begin
      if BoundingRectSingle(Vs, i, @Sx1, @Sy1, @Sx2, @Sy2) then
        begin
          if First then
            begin
              x1^ := Sx1;
              y1^ := Sy1;
              x2^ := Sx2;
              y2^ := Sy2;
            end
          else
            begin
              if Sx1 < x1^ then
                  x1^ := Sx1;

              if Sy1 < y1^ then
                  y1^ := Sy1;

              if Sx2 > x2^ then
                  x2^ := Sx2;

              if Sy2 > y2^ then
                  y2^ := Sy2;
            end;

          First := False;
        end;

      inc(i);
    end;

  Result := (x1^ <= x2^) and (y1^ <= y2^);
end;

function BoundingRectAllPaths(Vs: TAggVertexSource; Rect: TRectDouble): Boolean;
begin
  Result := BoundingRectAllPaths(Vs, @Rect.x1, @Rect.y1, @Rect.x2, @Rect.y2);
end;

end.
