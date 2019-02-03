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
unit AggClipLiangBarsky;

{$DEFINE FPC_DELPHI_MODE}
{$INCLUDE zDefine.inc}
interface
uses
  AggBasics;

function ClippingFlagsInteger(x, y: Integer; const ClipBox: TRectInteger): Cardinal;
function ClippingFlagsDouble(x, y: Double; ClipBox: PRectDouble): Cardinal;

function ClippingFlagsXInteger(x: Integer; const ClipBox: TRectInteger): Cardinal;
function ClippingFlagsXDouble(x: Double; ClipBox: PRectDouble): Cardinal;

function ClippingFlagsYInteger(y: Integer; const ClipBox: TRectInteger): Cardinal;
function ClippingFlagsYDouble(y: Double; ClipBox: PRectDouble): Cardinal;

function ClipLiangBarskyInteger(x1, y1, x2, y2: Integer; const ClipBox: TRectInteger; x, y: PInteger): Cardinal; overload;
function ClipLiangBarskyInteger(x1, y1, x2, y2: Integer; const ClipBox: TRectInteger; Point: PPointInteger): Cardinal; overload;

function ClipLiangBarskyDouble(x1, y1, x2, y2: Double; ClipBox: PRectDouble; x, y: PDouble): Cardinal;

implementation

// Determine the clipping code of the vertex according to the
// Cyrus-Beck line clipping algorithm
//
// |        |
// 0110  |  0010  | 0011
// |        |
// -------+--------+-------- ClipBox.y2
// |        |
// 0100  |  0000  | 0001
// |        |
// -------+--------+-------- ClipBox.y1
// |        |
// 1100  |  1000  | 1001
// |        |
// ClipBox.x1  ClipBox.x2
//

function ClippingFlagsInteger(x, y: Integer; const ClipBox: TRectInteger): Cardinal;
begin
  Result := Cardinal(x > ClipBox.x2) or (Cardinal(y > ClipBox.y2) shl 1) or
    (Cardinal(x < ClipBox.x1) shl 2) or (Cardinal(y < ClipBox.y1) shl 3);
end;

function ClippingFlagsDouble(x, y: Double; ClipBox: PRectDouble): Cardinal;
begin
  Result := Cardinal(x > ClipBox.x2) or (Cardinal(y > ClipBox.y2) shl 1) or
    (Cardinal(x < ClipBox.x1) shl 2) or (Cardinal(y < ClipBox.y1) shl 3);
end;

function ClippingFlagsXInteger(x: Integer; const ClipBox: TRectInteger): Cardinal;
begin
  Result := Cardinal(x > ClipBox.x2) or (Cardinal(x < ClipBox.x1) shl 2);
end;

function ClippingFlagsXDouble(x: Double; ClipBox: PRectDouble): Cardinal;
begin
  Result := Cardinal(x > ClipBox.x2) or (Cardinal(x < ClipBox.x1) shl 2);
end;

function ClippingFlagsYInteger(y: Integer; const ClipBox: TRectInteger): Cardinal;
begin
  Result := (Cardinal(y > ClipBox.y2) shl 1) or
    (Cardinal(y < ClipBox.y1) shl 3);
end;

function ClippingFlagsYDouble(y: Double; ClipBox: PRectDouble): Cardinal;
begin
  Result := (Cardinal(y > ClipBox.y2) shl 1) or
    (Cardinal(y < ClipBox.y1) shl 3);
end;

function ClipLiangBarskyInteger(x1, y1, x2, y2: Integer;
  const ClipBox: TRectInteger; x, y: PInteger): Cardinal;
const
  CNearZero = 1E-30;
var
  Inside, Outside, TempIn, TempOut, Delta: TPointDouble;
  TIn1, TIn2, TOut1: Double;
  np: Cardinal;
begin
  Delta.x := x2 - x1;
  Delta.y := y2 - y1;

  np := 0;

  // bump off of the vertical
  if Delta.x = 0.0 then
    if x1 > ClipBox.x1 then
        Delta.x := -CNearZero
    else
        Delta.x := CNearZero;

  // bump off of the horizontal
  if Delta.y = 0.0 then
    if y1 > ClipBox.y1 then
        Delta.y := -CNearZero
    else
        Delta.y := CNearZero;

  if Delta.x > 0.0 then
    begin
      // points to right
      Inside.x := ClipBox.x1;
      Outside.x := ClipBox.x2;
    end
  else
    begin
      Inside.x := ClipBox.x2;
      Outside.x := ClipBox.x1;
    end;

  if Delta.y > 0.0 then
    begin
      // points up
      Inside.y := ClipBox.y1;
      Outside.y := ClipBox.y2;
    end
  else
    begin
      Inside.y := ClipBox.y2;
      Outside.y := ClipBox.y1;
    end;

  TempIn.x := (Inside.x - x1) / Delta.x;
  TempIn.y := (Inside.y - y1) / Delta.y;

  if TempIn.x < TempIn.y then
    begin
      // hits x first
      TIn1 := TempIn.x;
      TIn2 := TempIn.y;
    end
  else
    begin
      // hits y first
      TIn1 := TempIn.y;
      TIn2 := TempIn.x;
    end;

  if TIn1 <= 1.0 then
    begin
      if 0.0 < TIn1 then
        begin
          x^ := Trunc(Inside.x);
          y^ := Trunc(Inside.y);

          inc(PtrComp(x), SizeOf(Integer));
          inc(PtrComp(y), SizeOf(Integer));
          inc(np);
        end;

      if TIn2 <= 1.0 then
        begin
          TempOut.x := (Outside.x - x1) / Delta.x;
          TempOut.y := (Outside.y - y1) / Delta.y;

          if TempOut.x < TempOut.y then
              TOut1 := TempOut.x
          else
              TOut1 := TempOut.y;

          if (TIn2 > 0.0) or (TOut1 > 0.0) then
            if TIn2 <= TOut1 then
              begin
                if TIn2 > 0.0 then
                  begin
                    if TempIn.x > TempIn.y then
                      begin
                        x^ := Trunc(Inside.x);
                        y^ := Trunc(y1 + TempIn.x * Delta.y);
                      end
                    else
                      begin
                        x^ := Trunc(x1 + TempIn.y * Delta.x);
                        y^ := Trunc(Inside.y);
                      end;

                    inc(PtrComp(x), SizeOf(Integer));
                    inc(PtrComp(y), SizeOf(Integer));
                    inc(np);
                  end;

                if TOut1 < 1.0 then
                  if TempOut.x < TempOut.y then
                    begin
                      x^ := Trunc(Outside.x);
                      y^ := Trunc(y1 + TempOut.x * Delta.y);
                    end
                  else
                    begin
                      x^ := Trunc(x1 + TempOut.y * Delta.x);
                      y^ := Trunc(Outside.y);
                    end
                else
                  begin
                    x^ := x2;
                    y^ := y2;
                  end;

                inc(PtrComp(x), SizeOf(Integer));
                inc(PtrComp(y), SizeOf(Integer));
                inc(np);
              end
            else
              begin
                if TempIn.x > TempIn.y then
                  begin
                    x^ := Trunc(Inside.x);
                    y^ := Trunc(Outside.y);
                  end
                else
                  begin
                    x^ := Trunc(Outside.x);
                    y^ := Trunc(Inside.y);
                  end;

                inc(PtrComp(x), SizeOf(Integer));
                inc(PtrComp(y), SizeOf(Integer));
                inc(np);
              end;
        end;
    end;

  Result := np;
end;

function ClipLiangBarskyInteger(x1, y1, x2, y2: Integer;
  const ClipBox: TRectInteger; Point: PPointInteger): Cardinal;
const
  CNearZero = 1E-30;
var
  Delta, Inside, Outside, TempIn, TempOut: TPointDouble;
  TempIn1, TempIn2, TempOut1: Double;
  np: Cardinal;
begin
  Delta.x := x2 - x1;
  Delta.y := y2 - y1;

  np := 0;

  // bump off of the vertical
  if Delta.x = 0.0 then
    if x1 > ClipBox.x1 then
        Delta.x := -CNearZero
    else
        Delta.x := CNearZero;

  // bump off of the horizontal
  if Delta.y = 0.0 then
    if y1 > ClipBox.y1 then
        Delta.y := -CNearZero
    else
        Delta.y := CNearZero;

  if Delta.x > 0.0 then
    begin
      // points to right
      Inside.x := ClipBox.x1;
      Outside.x := ClipBox.x2;
    end
  else
    begin
      Inside.x := ClipBox.x2;
      Outside.x := ClipBox.x1;
    end;

  if Delta.y > 0.0 then
    begin
      // points up
      Inside.y := ClipBox.y1;
      Outside.y := ClipBox.y2;
    end
  else
    begin
      Inside.y := ClipBox.y2;
      Outside.y := ClipBox.y1;
    end;

  TempIn.x := (Inside.x - x1) / Delta.x;
  TempIn.y := (Inside.y - y1) / Delta.y;

  if TempIn.x < TempIn.y then
    begin
      // hits x first
      TempIn1 := TempIn.x;
      TempIn2 := TempIn.y;
    end
  else
    begin
      // hits y first
      TempIn1 := TempIn.y;
      TempIn2 := TempIn.x;
    end;

  if TempIn1 <= 1.0 then
    begin
      if 0.0 < TempIn1 then
        begin
          Point^.x := Trunc(Inside.x);
          Point^.y := Trunc(Inside.y);

          inc(Point);
          inc(np);
        end;

      if TempIn2 <= 1.0 then
        begin
          TempOut.x := (Outside.x - x1) / Delta.x;
          TempOut.y := (Outside.y - y1) / Delta.y;

          if TempOut.x < TempOut.y then
              TempOut1 := TempOut.x
          else
              TempOut1 := TempOut.y;

          if (TempIn2 > 0.0) or (TempOut1 > 0.0) then
            if TempIn2 <= TempOut1 then
              begin
                if TempIn2 > 0.0 then
                  begin
                    if TempIn.x > TempIn.y then
                      begin
                        Point^.x := Trunc(Inside.x);
                        Point^.y := Trunc(y1 + TempIn.x * Delta.y);
                      end
                    else
                      begin
                        Point^.x := Trunc(x1 + TempIn.y * Delta.x);
                        Point^.y := Trunc(Inside.y);
                      end;

                    inc(Point);
                    inc(np);
                  end;

                if TempOut1 < 1.0 then
                  if TempOut.x < TempOut.y then
                    begin
                      Point^.x := Trunc(Outside.x);
                      Point^.y := Trunc(y1 + TempOut.x * Delta.y);
                    end
                  else
                    begin
                      Point^.x := Trunc(x1 + TempOut.y * Delta.x);
                      Point^.y := Trunc(Outside.y);
                    end
                else
                  begin
                    Point^.x := x2;
                    Point^.y := y2;
                  end;

                inc(Point);
                inc(np);
              end
            else
              begin
                if TempIn.x > TempIn.y then
                  begin
                    Point^.x := Trunc(Inside.x);
                    Point^.y := Trunc(Outside.y);
                  end
                else
                  begin
                    Point^.x := Trunc(Outside.x);
                    Point^.y := Trunc(Inside.y);
                  end;

                inc(Point);
                inc(np);
              end;
        end;
    end;

  Result := np;
end;

function ClipLiangBarskyDouble(x1, y1, x2, y2: Double; ClipBox: PRectDouble;
  x, y: PDouble): Cardinal;
const
  CNearZero = 1E-30;
var
  Delta, Inside, Outside, TempIn, TempOut: TPointDouble;
  TIn1, TIn2, TOut1: Double;
  np: Cardinal;
begin
  Delta.x := x2 - x1;
  Delta.y := y2 - y1;

  np := 0;

  // bump off of the vertical
  if Delta.x = 0.0 then
    if x1 > ClipBox.x1 then
        Delta.x := -CNearZero
    else
        Delta.x := CNearZero;

  // bump off of the horizontal
  if Delta.y = 0.0 then
    if y1 > ClipBox.y1 then
        Delta.y := -CNearZero
    else
        Delta.y := CNearZero;

  if Delta.x > 0.0 then
    begin
      // points to right
      Inside.x := ClipBox.x1;
      Outside.x := ClipBox.x2;
    end
  else
    begin
      Inside.x := ClipBox.x2;
      Outside.x := ClipBox.x1;
    end;

  if Delta.y > 0.0 then
    begin
      // points up
      Inside.y := ClipBox.y1;
      Outside.y := ClipBox.y2;
    end
  else
    begin
      Inside.y := ClipBox.y2;
      Outside.y := ClipBox.y1;
    end;

  TempIn.x := (Inside.x - x1) / Delta.x;
  TempIn.y := (Inside.y - y1) / Delta.y;

  if TempIn.x < TempIn.y then
    begin
      // hits x first
      TIn1 := TempIn.x;
      TIn2 := TempIn.y;
    end
  else
    begin
      // hits y first
      TIn1 := TempIn.y;
      TIn2 := TempIn.x;
    end;

  if TIn1 <= 1.0 then
    begin
      if 0.0 < TIn1 then
        begin
          x^ := Inside.x;
          y^ := Inside.y;

          inc(PtrComp(x), SizeOf(Integer));
          inc(PtrComp(y), SizeOf(Integer));
          inc(np);
        end;

      if TIn2 <= 1.0 then
        begin
          TempOut.x := (Outside.x - x1) / Delta.x;
          TempOut.y := (Outside.y - y1) / Delta.y;

          if TempOut.x < TempOut.y then
              TOut1 := TempOut.x
          else
              TOut1 := TempOut.y;

          if (TIn2 > 0.0) or (TOut1 > 0.0) then
            if TIn2 <= TOut1 then
              begin
                if TIn2 > 0.0 then
                  begin
                    if TempIn.x > TempIn.y then
                      begin
                        x^ := Inside.x;
                        y^ := y1 + TempIn.x * Delta.y;

                      end
                    else
                      begin
                        x^ := x1 + TempIn.y * Delta.x;
                        y^ := Inside.y;
                      end;

                    inc(PtrComp(x), SizeOf(Integer));
                    inc(PtrComp(y), SizeOf(Integer));
                    inc(np);
                  end;

                if TOut1 < 1.0 then
                  if TempOut.x < TempOut.y then
                    begin
                      x^ := Outside.x;
                      y^ := y1 + TempOut.x * Delta.y;
                    end
                  else
                    begin
                      x^ := x1 + TempOut.y * Delta.x;
                      y^ := Outside.y;
                    end
                else
                  begin
                    x^ := x2;
                    y^ := y2;
                  end;

                inc(PtrComp(x), SizeOf(Integer));
                inc(PtrComp(y), SizeOf(Integer));
                inc(np);

              end
            else
              begin
                if TempIn.x > TempIn.y then
                  begin
                    x^ := Inside.x;
                    y^ := Outside.y;
                  end
                else
                  begin
                    x^ := Outside.x;
                    y^ := Inside.y;
                  end;

                inc(PtrComp(x), SizeOf(Integer));
                inc(PtrComp(y), SizeOf(Integer));
                inc(np);
              end;
        end;
    end;

  Result := np;
end;

end. 
 
 
