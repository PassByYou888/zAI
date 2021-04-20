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
unit AggRasterizerOutline;

{$DEFINE FPC_DELPHI_MODE}
{$INCLUDE zDefine.inc}
interface
uses
  AggBasics,
  AggRendererPrimitives,
  AggVertexSource;

type
  TAggRasterizerOutline = class
  private
    FRendererPrimitives: TAggRendererPrimitives;
    FStartX, FStartY: Integer;
    FVertices: Cardinal;
  public
    constructor Create(Ren: TAggRendererPrimitives);

    procedure MoveTo(x, y: Integer);
    procedure LineTo(x, y: Integer);

    procedure MoveToDouble(x, y: Double);
    procedure LineToDouble(x, y: Double);
    procedure Close;

    procedure AddVertex(x, y: Double; Cmd: Cardinal);
    procedure AddPath(Vs: TAggVertexSource; PathID: Cardinal = 0);
  end;

implementation


{ TAggRasterizerOutline }

constructor TAggRasterizerOutline.Create(Ren: TAggRendererPrimitives);
begin
  FRendererPrimitives := Ren;

  FStartX := 0;
  FStartY := 0;

  FVertices := 0;
end;

procedure TAggRasterizerOutline.MoveTo(x, y: Integer);
begin
  FVertices := 1;

  FStartX := x;
  FStartY := y;

  FRendererPrimitives.MoveTo(x, y);
end;

procedure TAggRasterizerOutline.LineTo(x, y: Integer);
begin
  inc(FVertices);

  FRendererPrimitives.LineTo(x, y);
end;

procedure TAggRasterizerOutline.MoveToDouble(x, y: Double);
begin
  MoveTo(FRendererPrimitives.Coord(x), FRendererPrimitives.Coord(y));
end;

procedure TAggRasterizerOutline.LineToDouble(x, y: Double);
begin
  LineTo(FRendererPrimitives.Coord(x), FRendererPrimitives.Coord(y));
end;

procedure TAggRasterizerOutline.Close;
begin
  if FVertices > 2 then
      LineTo(FStartX, FStartY);

  FVertices := 0;
end;

procedure TAggRasterizerOutline.AddVertex(x, y: Double; Cmd: Cardinal);
begin
  if IsMoveTo(Cmd) then
      MoveToDouble(x, y)
  else if IsEndPoly(Cmd) then
    if IsClosed(Cmd) then
        Close
    else
  else
      LineToDouble(x, y);
end;

procedure TAggRasterizerOutline.AddPath(Vs: TAggVertexSource;
  PathID: Cardinal = 0);
var
  Cmd: Cardinal;
  x, y: Double;
begin
  Vs.Rewind(PathID);

  Cmd := Vs.Vertex(@x, @y);

  while not IsStop(Cmd) do
    begin
      AddVertex(x, y, Cmd);

      Cmd := Vs.Vertex(@x, @y);
    end;
end;

end.
 
 
 
