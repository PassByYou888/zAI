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
unit AggColorConversion;

{$DEFINE FPC_DELPHI_MODE}
{$INCLUDE zDefine.inc}
interface

uses
  AggBasics,
  AggColor32,
  AggRenderingBuffer;

type
  CopyRow = procedure(Dst, Src: PInt8u; width: Cardinal);

procedure ColorConversion(Dst, Src: TAggRenderingBuffer; CopyRowFunctor: CopyRow);

procedure ColorConversionGray8ToBgr24(Dst, Src: PInt8u; width: Cardinal);
procedure ColorConversionGray8ToRgb24(Dst, Src: PInt8u; width: Cardinal);

procedure ColorConversionRgb565ToRgb555(Dst, Src: PInt8u; width: Cardinal);

procedure ColorConversionBgr24ToRgb24(Dst, Src: PInt8u; width: Cardinal);
procedure ColorConversionBgr24ToBgr24(Dst, Src: PInt8u; width: Cardinal);
procedure ColorConversionBgr24ToBgra32(Dst, Src: PInt8u; width: Cardinal);

procedure ColorConversionRgb24ToBgr24(Dst, Src: PInt8u; width: Cardinal);
procedure ColorConversionRgb24ToBgra32(Dst, Src: PInt8u; width: Cardinal);

procedure ColorConversionBgra32ToArgb32(Dst, Src: PInt8u; width: Cardinal);

procedure ColorConversionAbgr32ToArgb32(Dst, Src: PInt8u; width: Cardinal);
procedure ColorConversionAbgr32ToBgra32(Dst, Src: PInt8u; width: Cardinal);

procedure ColorConversionRgba32ToArgb32(Dst, Src: PInt8u; width: Cardinal);
procedure ColorConversionRgba32ToBgra32(Dst, Src: PInt8u; width: Cardinal);

procedure ColorConversionArgb32ToBgra32(Dst, Src: PInt8u; width: Cardinal);

implementation

procedure ColorConversion(Dst, Src: TAggRenderingBuffer;
  CopyRowFunctor: CopyRow);
var
  y, width, height: Cardinal;
begin
  width := Src.width;
  height := Src.height;

  if Dst.width < width then
    width := Dst.width;

  if Dst.height < height then
    height := Dst.height;

  if width > 0 then
    for y := 0 to height - 1 do
      CopyRowFunctor(Dst.Row(y), Src.Row(y), width);
end;

procedure ColorConversionGray8ToBgr24(Dst, Src: PInt8u; width: Cardinal);
begin
  repeat
    PInt8u(PtrComp(Dst) + CAggOrderBgr.r)^ := Src^;
    PInt8u(PtrComp(Dst) + CAggOrderBgr.g)^ := Src^;
    PInt8u(PtrComp(Dst) + CAggOrderBgr.b)^ := Src^;

    inc(PtrComp(Dst), 3);
    inc(PtrComp(Src));
    dec(width);
  until width = 0;
end;

procedure ColorConversionGray8ToRgb24(Dst, Src: PInt8u; width: Cardinal);
begin
  repeat
    PInt8u(PtrComp(Dst) + CAggOrderRgb.r)^ := Src^;
    PInt8u(PtrComp(Dst) + CAggOrderRgb.g)^ := Src^;
    PInt8u(PtrComp(Dst) + CAggOrderRgb.b)^ := Src^;

    inc(PtrComp(Dst), 3);
    inc(PtrComp(Src));
    dec(width);
  until width = 0;
end;

procedure ColorConversionBgr24ToRgb24(Dst, Src: PInt8u; width: Cardinal);
begin
  repeat
    PInt8u(PtrComp(Dst) + CAggOrderBgr.r)^ :=
      PInt8u(PtrComp(Src) + CAggOrderRgb.r)^;
    PInt8u(PtrComp(Dst) + CAggOrderBgr.g)^ :=
      PInt8u(PtrComp(Src) + CAggOrderRgb.g)^;
    PInt8u(PtrComp(Dst) + CAggOrderBgr.b)^ :=
      PInt8u(PtrComp(Src) + CAggOrderRgb.b)^;

    inc(PtrComp(Dst), 3);
    inc(PtrComp(Src), 3);
    dec(width);
  until width = 0;
end;

procedure ColorConversionBgr24ToBgr24(Dst, Src: PInt8u; width: Cardinal);
begin
  Move(Src^, Dst^, width * 3);
end;

procedure ColorConversionBgra32ToArgb32(Dst, Src: PInt8u; width: Cardinal);
begin
  repeat
    PInt8u(PtrComp(Dst) + CAggOrderArgb.r)^ :=
      PInt8u(PtrComp(Src) + CAggOrderBgra.r)^;
    PInt8u(PtrComp(Dst) + CAggOrderArgb.g)^ :=
      PInt8u(PtrComp(Src) + CAggOrderBgra.g)^;
    PInt8u(PtrComp(Dst) + CAggOrderArgb.b)^ :=
      PInt8u(PtrComp(Src) + CAggOrderBgra.b)^;
    PInt8u(PtrComp(Dst) + CAggOrderArgb.a)^ :=
      PInt8u(PtrComp(Src) + CAggOrderBgra.a)^;

    inc(PtrComp(Dst), 4);
    inc(PtrComp(Src), 4);
    dec(width);
  until width = 0;
end;

procedure ColorConversionAbgr32ToArgb32(Dst, Src: PInt8u; width: Cardinal);
begin
  repeat
    PInt8u(PtrComp(Dst) + CAggOrderArgb.r)^ :=
      PInt8u(PtrComp(Src) + CAggOrderAbgr.r)^;
    PInt8u(PtrComp(Dst) + CAggOrderArgb.g)^ :=
      PInt8u(PtrComp(Src) + CAggOrderAbgr.g)^;
    PInt8u(PtrComp(Dst) + CAggOrderArgb.b)^ :=
      PInt8u(PtrComp(Src) + CAggOrderAbgr.b)^;
    PInt8u(PtrComp(Dst) + CAggOrderArgb.a)^ :=
      PInt8u(PtrComp(Src) + CAggOrderAbgr.a)^;

    inc(PtrComp(Dst), 4);
    inc(PtrComp(Src), 4);
    dec(width);
  until width = 0;
end;

procedure ColorConversionRgba32ToArgb32(Dst, Src: PInt8u; width: Cardinal);
begin
  repeat
    PInt8u(PtrComp(Dst) + CAggOrderArgb.r)^ :=
      PInt8u(PtrComp(Src) + CAggOrderRgba.r)^;
    PInt8u(PtrComp(Dst) + CAggOrderArgb.g)^ :=
      PInt8u(PtrComp(Src) + CAggOrderRgba.g)^;
    PInt8u(PtrComp(Dst) + CAggOrderArgb.b)^ :=
      PInt8u(PtrComp(Src) + CAggOrderRgba.b)^;
    PInt8u(PtrComp(Dst) + CAggOrderArgb.a)^ :=
      PInt8u(PtrComp(Src) + CAggOrderRgba.a)^;

    inc(PtrComp(Dst), 4);
    inc(PtrComp(Src), 4);
    dec(width);
  until width = 0;
end;

procedure ColorConversionBgr24ToBgra32(Dst, Src: PInt8u; width: Cardinal);
begin
  repeat
    PAggOrderBgra(Dst)^.b := PAggOrderBgr(Src)^.b;
    PAggOrderBgra(Dst)^.g := PAggOrderBgr(Src)^.g;
    PAggOrderBgra(Dst)^.r := PAggOrderBgr(Src)^.r;
    PAggOrderBgra(Dst)^.a := $FF;

    inc(Dst, 4);
    inc(Src, 3);
    dec(width);
  until width = 0;
end;

procedure ColorConversionRgb565ToRgb555(Dst, Src: PInt8u; width: Cardinal);
var
  RGB: Integer;
begin
  repeat
    RGB := PInt16u(Src)^;

    PInt16u(Dst)^ := ((RGB shr 1) and $7FE0) or (RGB and $1F);

    inc(PtrComp(Src), 2);
    inc(PtrComp(Dst), 2);
    dec(width);
  until width = 0;
end;

procedure ColorConversionRgb24ToBgr24(Dst, Src: PInt8u; width: Cardinal);
begin
  repeat
    PAggOrderBgr(Dst)^.r := PAggOrderRgb(Src)^.r;
    PAggOrderBgr(Dst)^.g := PAggOrderRgb(Src)^.g;
    PAggOrderBgr(Dst)^.b := PAggOrderRgb(Src)^.b;

    inc(Src, 3);
    inc(Dst, 3);
    dec(width);
  until width = 0;
end;

procedure ColorConversionAbgr32ToBgra32(Dst, Src: PInt8u; width: Cardinal);
begin
  repeat
    PAggOrderBgra(Dst)^.b := PAggOrderAbgr(Src)^.b;
    PAggOrderBgra(Dst)^.g := PAggOrderAbgr(Src)^.g;
    PAggOrderBgra(Dst)^.r := PAggOrderAbgr(Src)^.r;
    PAggOrderBgra(Dst)^.a := PAggOrderAbgr(Src)^.a;

    inc(Src, 4);
    inc(Dst, 4);
    dec(width);
  until width = 0;
end;

procedure ColorConversionArgb32ToBgra32(Dst, Src: PInt8u; width: Cardinal);
begin
  repeat
    PAggOrderBgra(Dst)^.b := PAggOrderArgb(Src)^.b;
    PAggOrderBgra(Dst)^.g := PAggOrderArgb(Src)^.g;
    PAggOrderBgra(Dst)^.r := PAggOrderArgb(Src)^.r;
    PAggOrderBgra(Dst)^.a := PAggOrderArgb(Src)^.a;

    inc(Src, 4);
    inc(Dst, 4);
    dec(width);
  until width = 0;
end;

procedure ColorConversionRgba32ToBgra32(Dst, Src: PInt8u; width: Cardinal);
begin
  repeat
    PAggOrderBgra(Dst)^.b := PAggOrderRgba(Src)^.b;
    PAggOrderBgra(Dst)^.g := PAggOrderRgba(Src)^.g;
    PAggOrderBgra(Dst)^.r := PAggOrderRgba(Src)^.r;
    PAggOrderBgra(Dst)^.a := PAggOrderRgba(Src)^.a;

    inc(Src, 4);
    inc(Dst, 4);
    dec(width);
  until width = 0;
end;

procedure ColorConversionRgb24ToBgra32(Dst, Src: PInt8u; width: Cardinal);
begin
  repeat
    PAggOrderBgra(Dst)^.b := PAggOrderRgb(Src)^.b;
    PAggOrderBgra(Dst)^.g := PAggOrderRgb(Src)^.g;
    PAggOrderBgra(Dst)^.r := PAggOrderRgb(Src)^.r;
    PAggOrderBgra(Dst)^.a := $FF;

    inc(Src, 4);
    inc(Dst, 4);
    dec(width);
  until width = 0;
end;

end. 
 
 
