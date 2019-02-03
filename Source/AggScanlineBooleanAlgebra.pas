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
unit AggScanlineBooleanAlgebra;

{$DEFINE FPC_DELPHI_MODE}
{$INCLUDE zDefine.inc}
interface
uses
  AggBasics,
  AggRasterizerScanLine,
  AggScanline,
  AggRendererScanLine;

type
  TAggBoolScanLineOp = (bsoOr, bsoAnd, bsoXor, bsoXorSaddle, bsoXorAbsDiff, bsoAMinusB, bsoBMinusA);

  TAggBoolScanLineFunctor = class;

  TAggBoolScanLineFunctor1 = procedure(This: TAggBoolScanLineFunctor; Span: TAggCustomSpan; x: Integer; Len: Cardinal; SL: TAggCustomScanLine);
  TAggBoolScanLineFunctor2 = procedure(This: TAggBoolScanLineFunctor; Span1, Span2: TAggCustomSpan; x: Integer; Len: Cardinal; SL: TAggCustomScanLine);
  TAggBoolScanLineFormula  = function(This: TAggBoolScanLineFunctor; a, b: Cardinal): Cardinal;

  TAggBoolScanLineFunctor = class
  private
    FCoverShift, FCoverSize, FCoverMask, FCoverFull: Cardinal;
  public
    Functor1: TAggBoolScanLineFunctor1;
    Functor2: TAggBoolScanLineFunctor2;
    Formula: TAggBoolScanLineFormula;

    constructor Create1(f1: TAggBoolScanLineFunctor1; CoverShift: Cardinal = AggBasics.CAggCoverShift);
    constructor Create2(f2: TAggBoolScanLineFunctor2; CoverShift: Cardinal = AggBasics.CAggCoverShift);
  end;

procedure BoolScanLineSubtractShapesAA(Sg1, Sg2: TAggRasterizerScanLine; Sl1, Sl2, SL: TAggCustomScanLine; Ren: TAggCustomRendererScanLine);

procedure BoolScanLineXORShapesAbsDiffAA(Sg1, Sg2: TAggRasterizerScanLine; Sl1, Sl2, SL: TAggCustomScanLine; Ren: TAggCustomRendererScanLine);

procedure BoolScanLineXORShapesSaddleAA(Sg1, Sg2: TAggRasterizerScanLine; Sl1, Sl2, SL: TAggCustomScanLine; Ren: TAggCustomRendererScanLine);

procedure BoolScanLineXORShapesAA(Sg1, Sg2: TAggRasterizerScanLine; Sl1, Sl2, SL: TAggCustomScanLine; Ren: TAggCustomRendererScanLine);

procedure BoolScanLineIntersectShapesAA(Sg1, Sg2: TAggRasterizerScanLine; Sl1, Sl2, SL: TAggCustomScanLine; Ren: TAggCustomRendererScanLine);

procedure BoolScanLineUniteShapesAA(Sg1, Sg2: TAggRasterizerScanLine; Sl1, Sl2, SL: TAggCustomScanLine; Ren: TAggCustomRendererScanLine);

procedure BoolScanLineCombineShapesAA(Op: TAggBoolScanLineOp; Sg1, Sg2: TAggRasterizerScanLine; Sl1, Sl2, SL: TAggCustomScanLine; Ren: TAggCustomRendererScanLine);

procedure BoolScanLineSubtractShapesBin(Sg1, Sg2: TAggRasterizerScanLine; Sl1, Sl2, SL: TAggCustomScanLine; Ren: TAggCustomRendererScanLine);

procedure BoolScanLineXORShapesBin(Sg1, Sg2: TAggRasterizerScanLine; Sl1, Sl2, SL: TAggCustomScanLine; Ren: TAggCustomRendererScanLine);

procedure BoolScanLineIntersectShapesBin(Sg1, Sg2: TAggRasterizerScanLine; Sl1, Sl2, SL: TAggCustomScanLine; Ren: TAggCustomRendererScanLine);

procedure BoolScanLineUniteShapesBin(Sg1, Sg2: TAggRasterizerScanLine; Sl1, Sl2, SL: TAggCustomScanLine; Ren: TAggCustomRendererScanLine);

procedure BoolScanLineCombineShapesBin(Op: TAggBoolScanLineOp; Sg1, Sg2: TAggRasterizerScanLine; Sl1, Sl2, SL: TAggCustomScanLine; Ren: TAggCustomRendererScanLine);

implementation


{ TAggBoolScanLineFunctor }

constructor TAggBoolScanLineFunctor.Create1(f1: TAggBoolScanLineFunctor1;
  CoverShift: Cardinal = AggBasics.CAggCoverShift);
begin
  FCoverShift := CoverShift;
  FCoverSize := 1 shl FCoverShift;
  FCoverMask := FCoverSize - 1;
  FCoverFull := FCoverMask;

  Functor1 := f1;
  Functor2 := nil;
  Formula := nil;
end;

constructor TAggBoolScanLineFunctor.Create2(f2: TAggBoolScanLineFunctor2;
  CoverShift: Cardinal = AggBasics.CAggCoverShift);
begin
  FCoverShift := CoverShift;
  FCoverSize := 1 shl FCoverShift;
  FCoverMask := FCoverSize - 1;
  FCoverFull := FCoverMask;

  Functor1 := nil;
  Functor2 := f2;
  Formula := nil;
end;

// Functor.
// Add nothing. Used in conbine_shapes_sub
procedure BoolScanLineAddSpanEmpty(This: TAggBoolScanLineFunctor; Span: PAggSpanRecord;
  x: Integer; Len: Cardinal; SL: TAggCustomScanLine);
begin
end;

// Functor.
// Combine two Spans as empty ones. The functor does nothing
// and is used to XOR binary Spans.
procedure BoolScanLineCombineSpansEmpty(This: TAggBoolScanLineFunctor;
  Span1, Span2: PAggSpanRecord; x: Integer; Len: Cardinal; SL: TAggCustomScanLine);
begin
end;

// Functor.
// Add an anti-aliased Span
// anti-aliasing information, but only X and Length. The function
// is compatible with any type of ScanLines.
procedure BoolScanLineAddSpanAA(This: TAggBoolScanLineFunctor; Span: PAggSpanRecord;
  x: Integer; Len: Cardinal; SL: TAggCustomScanLine);
var
  Covers: PInt8u;
begin
  if Span.Len < 0 then
      SL.AddSpan(x, Len, Span.Covers^)
  else if Span.Len > 0 then
    begin
      Covers := Span.Covers;

      if Span.x < x then
          inc(PtrComp(Covers), x - Span.x);

      SL.AddCells(x, Len, Covers);
    end;
end;

// Functor.
// Unite two Spans preserving the anti-aliasing information.
// The result is added to the "sl" ScanLine.
procedure BoolScanLineUniteSpansAA(This: TAggBoolScanLineFunctor;
  Span1, Span2: PAggSpanRecord; x: Integer; Len: Cardinal; SL: TAggCustomScanLine);
var
  Cover: Cardinal;
  Covers1, Covers2: PInt8u;
begin
  // Calculate the operation code and choose the
  // proper combination algorithm.
  // 0 = Both Spans are of AA type
  // 1 = Span1 is solid, Span2 is AA
  // 2 = Span1 is AA, Span2 is solid
  // 3 = Both Spans are of solid type
  case Cardinal(Span1.Len < 0) or (Cardinal(Span2.Len < 0) shl 1) of
    0: // Both are AA Spans
      begin
        Covers1 := Span1.Covers;
        Covers2 := Span2.Covers;

        if Span1.x < x then
            inc(PtrComp(Covers1), (x - Span1.x) * SizeOf(Int8u));

        if Span2.x < x then
            inc(PtrComp(Covers2), (x - Span2.x) * SizeOf(Int8u));

        repeat
          Cover := This.FCoverMask * This.FCoverMask -
            (This.FCoverMask - Covers1^) * (This.FCoverMask - Covers2^);

          inc(PtrComp(Covers1), SizeOf(Int8u));
          inc(PtrComp(Covers2), SizeOf(Int8u));

          if Cover = This.FCoverFull * This.FCoverFull then
              SL.AddCell(x, This.FCoverFull)
          else
              SL.AddCell(x, Cover shr This.FCoverShift);

          inc(x);
          dec(Len);

        until Len = 0;
      end;

    1: // Span1 is solid, Span2 is AA
      begin
        Covers2 := Span2.Covers;

        if Span2.x < x then
            inc(PtrComp(Covers2), (x - Span2.x) * SizeOf(Int8u));

        if Span1.Covers^ = This.FCoverFull then
            SL.AddSpan(x, Len, This.FCoverFull)
        else
          repeat
            Cover := This.FCoverMask * This.FCoverMask -
              (This.FCoverMask - Span1.Covers^) * (This.FCoverMask - Covers2^);

            inc(PtrComp(Covers2), SizeOf(Int8u));

            if Cover = This.FCoverFull * This.FCoverFull then
                SL.AddCell(x, This.FCoverFull)
            else
                SL.AddCell(x, Cover shr This.FCoverShift);

            inc(x);
            dec(Len);

          until Len = 0;
      end;

    2: // Span1 is AA, Span2 is solid
      begin
        Covers1 := Span1.Covers;

        if Span1.x < x then
            inc(PtrComp(Covers1), (x - Span1.x) * SizeOf(Int8u));

        if Span2.Covers^ = This.FCoverFull then
            SL.AddSpan(x, Len, This.FCoverFull)
        else
          repeat
            Cover := This.FCoverMask * This.FCoverMask -
              (This.FCoverMask - Covers1^) * (This.FCoverMask - Span2.Covers^);

            inc(PtrComp(Covers1), SizeOf(Int8u));

            if Cover = This.FCoverFull * This.FCoverFull then
                SL.AddCell(x, This.FCoverFull)
            else
                SL.AddCell(x, Cover shr This.FCoverShift);

            inc(x);
            dec(Len);

          until Len = 0;
      end;

    3: // Both are solid Spans
      begin
        Cover := This.FCoverMask * This.FCoverMask -
          (This.FCoverMask - Span1.Covers^) * (This.FCoverMask - Span2.Covers^);

        if Cover = This.FCoverFull * This.FCoverFull then
            SL.AddSpan(x, Len, This.FCoverFull)
        else
            SL.AddSpan(x, Len, Cover shr This.FCoverShift);
      end;
  end;
end;

// Functor.
// Combine two binary encoded Spans, i.e., when we don't have any
// anti-aliasing information, but only X and Length. The function
// is compatible with any type of ScanLines.
procedure BoolScanLineCombineSpansBin(This: TAggBoolScanLineFunctor;
  Span1, Span2: PAggSpanRecord; x: Integer; Len: Cardinal; SL: TAggCustomScanLine);
begin
  SL.AddSpan(x, Len, This.FCoverFull);
end;

// Functor.
// Add a binary Span
procedure BoolScanLineAddSpanBin(This: TAggBoolScanLineFunctor; Span: PAggSpanRecord;
  x: Integer; Len: Cardinal; SL: TAggCustomScanLine);
begin
  SL.AddSpan(x, Len, This.FCoverFull);
end;

// Functor.
// Intersect two Spans preserving the anti-aliasing information.
// The result is added to the "sl" ScanLine.
procedure SboolIntersecTAggSpansAA(This: TAggBoolScanLineFunctor;
  Span1, Span2: PAggSpanRecord; x: Integer; Len: Cardinal; SL: TAggCustomScanLine);
var
  Cover: Cardinal;
  Covers1, Covers2: PInt8u;
begin
  // Calculate the operation code and choose the
  // proper combination algorithm.
  // 0 = Both Spans are of AA type
  // 1 = Span1 is solid, Span2 is AA
  // 2 = Span1 is AA, Span2 is solid
  // 3 = Both Spans are of solid type
  case Cardinal(Span1.Len < 0) or (Cardinal(Span2.Len < 0) shl 1) of
    0: // Both are AA Spans
      begin
        Covers1 := Span1.Covers;
        Covers2 := Span2.Covers;

        if Span1.x < x then
            inc(PtrComp(Covers1), (x - Span1.x) * SizeOf(Int8u));

        if Span2.x < x then
            inc(PtrComp(Covers2), (x - Span2.x) * SizeOf(Int8u));

        repeat
          Cover := Covers1^ * Covers2^;

          inc(PtrComp(Covers1), SizeOf(Int8u));
          inc(PtrComp(Covers2), SizeOf(Int8u));

          if Cover = This.FCoverFull * This.FCoverFull then
              SL.AddCell(x, This.FCoverFull)
          else
              SL.AddCell(x, Cover shr This.FCoverShift);

          inc(x);
          dec(Len);

        until Len = 0;
      end;

    1: // Span1 is solid, Span2 is AA
      begin
        Covers2 := Span2.Covers;

        if Span2.x < x then
            inc(PtrComp(Covers2), (x - Span2.x));

        if Span1.Covers^ = This.FCoverFull then
            SL.AddCells(x, Len, Covers2)
        else
          repeat
            Cover := Span1.Covers^ * Covers2^;

            inc(PtrComp(Covers2), SizeOf(Int8u));

            if Cover = This.FCoverFull * This.FCoverFull then
                SL.AddCell(x, This.FCoverFull)
            else
                SL.AddCell(x, Cover shr This.FCoverShift);

            inc(x);
            dec(Len);

          until Len = 0;
      end;

    2: // Span1 is AA, Span2 is solid
      begin
        Covers1 := Span1.Covers;

        if Span1.x < x then
            inc(PtrComp(Covers1), (x - Span1.x) * SizeOf(Int8u));

        if Span2.Covers^ = This.FCoverFull then
            SL.AddCells(x, Len, Covers1)
        else
          repeat
            Cover := Covers1^ * Span2.Covers^;

            inc(PtrComp(Covers1), SizeOf(Int8u));

            if Cover = This.FCoverFull * This.FCoverFull then
                SL.AddCell(x, This.FCoverFull)
            else
                SL.AddCell(x, Cover shr This.FCoverShift);

            inc(x);
            dec(Len);

          until Len = 0;
      end;

    3: // Both are solid Spans
      begin
        Cover := Span1.Covers^ * Span2.Covers^;

        if Cover = This.FCoverFull * This.FCoverFull then
            SL.AddSpan(x, Len, This.FCoverFull)
        else
            SL.AddSpan(x, Len, Cover shr This.FCoverShift);
      end;
  end;
end;

// Functor.
// XOR two Spans preserving the anti-aliasing information.
// The result is added to the "sl" ScanLine.
procedure BoolScanLineXORSpansAA(This: TAggBoolScanLineFunctor; Span1,
  Span2: PAggSpanRecord; x: Integer; Len: Cardinal; SL: TAggCustomScanLine);
var
  Cover: Cardinal;
  Covers1, Covers2: PInt8u;
begin
  // Calculate the operation code and choose the
  // proper combination algorithm.
  // 0 = Both Spans are of AA type
  // 1 = Span1 is solid, Span2 is AA
  // 2 = Span1 is AA, Span2 is solid
  // 3 = Both Spans are of solid type
  case Cardinal(Span1.Len < 0) or (Cardinal(Span2.Len < 0) shl 1) of
    0: // Both are AA Spans
      begin
        Covers1 := Span1.Covers;
        Covers2 := Span2.Covers;

        if Span1.x < x then
            inc(PtrComp(Covers1), (x - Span1.x) * SizeOf(Int8u));

        if Span2.x < x then
            inc(PtrComp(Covers2), (x - Span2.x) * SizeOf(Int8u));

        repeat
          Cover := This.Formula(This, Covers1^, Covers2^);

          inc(PtrComp(Covers1), SizeOf(Int8u));
          inc(PtrComp(Covers2), SizeOf(Int8u));

          if Cover <> 0 then
              SL.AddCell(x, Cover);

          inc(x);
          dec(Len);

        until Len = 0;
      end;

    1: // Span1 is solid, Span2 is AA
      begin
        Covers2 := Span2.Covers;

        if Span2.x < x then
            inc(PtrComp(Covers2), (x - Span2.x) * SizeOf(Int8u));

        repeat
          Cover := This.Formula(This, Span1.Covers^, Covers2^);

          inc(PtrComp(Covers2), SizeOf(Int8u));

          if Cover <> 0 then
              SL.AddCell(x, Cover);

          inc(x);
          dec(Len);

        until Len = 0;
      end;

    2: // Span1 is AA, Span2 is solid
      begin
        Covers1 := Span1.Covers;

        if Span1.x < x then
            inc(PtrComp(Covers1), (x - Span1.x) * SizeOf(Int8u));

        repeat
          Cover := This.Formula(This, Covers1^, Span2.Covers^);

          inc(PtrComp(Covers1), SizeOf(Int8u));

          if Cover <> 0 then
              SL.AddCell(x, Cover);

          inc(x);
          dec(Len);

        until Len = 0;
      end;

    3: // Both are solid Spans
      begin
        Cover := This.Formula(This, Span1.Covers^, Span2.Covers^);

        if Cover <> 0 then
            SL.AddSpan(x, Len, Cover);
      end;
  end;
end;

// Functor.
// Unite two Spans preserving the anti-aliasing information.
// The result is added to the "sl" ScanLine.
procedure BoolScanLineSubtracTAggSpansAA(This: TAggBoolScanLineFunctor;
  Span1, Span2: PAggSpanRecord; x: Integer; Len: Cardinal; SL: TAggCustomScanLine);
var
  Cover: Cardinal;
  Covers1, Covers2: PInt8u;
begin
  // Calculate the operation code and choose the
  // proper combination algorithm.
  // 0 = Both Spans are of AA type
  // 1 = Span1 is solid, Span2 is AA
  // 2 = Span1 is AA, Span2 is solid
  // 3 = Both Spans are of solid type
  case Cardinal(Span1.Len < 0) or (Cardinal(Span2.Len < 0) shl 1) of
    0: // Both are AA Spans
      begin
        Covers1 := Span1.Covers;
        Covers2 := Span2.Covers;

        if Span1.x < x then
            inc(PtrComp(Covers1), (x - Span1.x) * SizeOf(Int8u));

        if Span2.x < x then
            inc(PtrComp(Covers2), (x - Span2.x) * SizeOf(Int8u));

        repeat
          Cover := Covers1^ * (This.FCoverMask - Covers2^);

          inc(PtrComp(Covers1), SizeOf(Int8u));
          inc(PtrComp(Covers2), SizeOf(Int8u));

          if Cover <> 0 then
            if Cover = This.FCoverFull * This.FCoverFull then
                SL.AddCell(x, This.FCoverFull)
            else
                SL.AddCell(x, Cover shr This.FCoverShift);

          inc(x);
          dec(Len);

        until Len = 0;
      end;

    1: // Span1 is solid, Span2 is AA
      begin
        Covers2 := Span2.Covers;

        if Span2.x < x then
            inc(PtrComp(Covers2), (x - Span2.x) * SizeOf(Int8u));

        repeat
          Cover := Span1.Covers^ * (This.FCoverMask - Covers2^);

          inc(PtrComp(Covers2), SizeOf(Int8u));

          if Cover <> 0 then
            if Cover = This.FCoverFull * This.FCoverFull then
                SL.AddCell(x, This.FCoverFull)
            else
                SL.AddCell(x, Cover shr This.FCoverShift);

          inc(x);
          dec(Len);

        until Len = 0;
      end;

    2: // Span1 is AA, Span2 is solid
      begin
        Covers1 := Span1.Covers;

        if Span1.x < x then
            inc(PtrComp(Covers1), (x - Span1.x) * SizeOf(Int8u));

        if Span2.Covers^ <> This.FCoverFull then
          repeat
            Cover := Covers1^ * (This.FCoverMask - Span2.Covers^);

            inc(PtrComp(Covers1), SizeOf(Int8u));

            if Cover <> 0 then
              if Cover = This.FCoverFull * This.FCoverFull then
                  SL.AddCell(x, This.FCoverFull)
              else
                  SL.AddCell(x, Cover shr This.FCoverShift);

            inc(x);
            dec(Len);

          until Len = 0;
      end;

    3: // Both are solid Spans
      begin
        Cover := Span1.Covers^ * (This.FCoverMask - Span2.Covers^);

        if Cover <> 0 then
          if Cover = This.FCoverFull * This.FCoverFull then
              SL.AddSpan(x, Len, This.FCoverFull)
          else
              SL.AddSpan(x, Len, Cover shr This.FCoverShift);
      end;
  end;
end;

function BoolScanLineXORFormulaLinear(This: TAggBoolScanLineFunctor; a, b: Cardinal)
  : Cardinal;
var
  Cover: Cardinal;
begin
  Cover := a + b;

  if Cover > This.FCoverMask then
      Cover := This.FCoverMask + This.FCoverMask - Cover;

  Result := Cover;
end;

function BoolScanLineXORFormulaSaddle(This: TAggBoolScanLineFunctor; a, b: Integer): Cardinal;
var
  k: Cardinal;
begin
  k := a * b;

  if k = This.FCoverMask * This.FCoverMask then
      Result := 0
  else
    begin
      a := (This.FCoverMask * This.FCoverMask - (a shl This.FCoverShift) + k)
        shr This.FCoverShift;
      b := (This.FCoverMask * This.FCoverMask - (b shl This.FCoverShift) + k)
        shr This.FCoverShift;

      Result := This.FCoverMask - ((a * b) shr This.FCoverShift);
    end;
end;

function BoolScanLineXORFormulaAbsDiff(This: TAggBoolScanLineFunctor; a, b: Integer)
  : Cardinal;
begin
  Result := Abs(a - b);
end;

procedure BoolScanLineAddSpansAndRender(Sl1, SL: TAggCustomScanLine;
  Ren: TAggCustomRendererScanLine; AddSpan: TAggBoolScanLineFunctor);
var
  // Ss: Cardinal;
  // Span: PAggSpanRecord;
  NumSpans: Cardinal;
  Span: TAggCustomSpan;
begin
  SL.ResetSpans;

  // Ss := Sl1.SizeOfSpan;
  Span := Sl1.GetBegin;
  NumSpans := Sl1.NumSpans;

  repeat
    AddSpan.Functor1(AddSpan, Span, Span.x, Abs(Span.Len), SL);

    dec(NumSpans);

    if NumSpans = 0 then
        Break;

    // Inc(PtrComp(Span), Ss);
    Span.IncOperator;
  until False;

  Span.Free;

  SL.Finalize(Sl1.y);
  Ren.Render(SL);
end;

// Unite two ScanLines, "sl1" and "sl2" and generate a new "sl" one.
// The CombineSpans functor can be of type BoolScanLineCombineSpansBin or
// SboolIntersecTAggSpansAA. First is a general functor to combine
// two Spans without Anti-Aliasing, the second preserves the AA
// information, but works sLower
procedure BoolScanLineUniteScanLines(Sl1, Sl2, SL: TAggCustomScanLine;
  AddSpan1, AddSpan2, CombineSpans: TAggBoolScanLineFunctor);
const
  CInvalidB = $FFFFFFF;
  CInvalidE = CInvalidB - 1;
var
  Num1, Num2: Cardinal;
  Xb1, Xb2, Xe1, Xe2, XB, XE, Len: Integer;
  // Ss1, Ss2: Cardinal;
  // Span1, Span2: PAggSpanRecord;
  Span1, Span2: TAggCustomSpan;
begin
  SL.ResetSpans;

  Num1 := Sl1.NumSpans;
  Num2 := Sl2.NumSpans;

  // Initialize the Spans as invalid
  Xb1 := CInvalidB;
  Xb2 := CInvalidB;
  Xe1 := CInvalidE;
  Xe2 := CInvalidE;

  Span1 := nil;
  Span2 := nil;

  // Initialize Span1 if there are Spans
  if Num1 <> 0 then
    begin
      Span1 := Sl1.GetBegin;
      // Ss1 := Sl1.SizeOfSpan;
      Xb1 := Span1.x;
      Xe1 := Xb1 + Abs(Span1.Len) - 1;

      dec(Num1);
    end;

  // Initialize Span2 if there are Spans
  if Num2 <> 0 then
    begin
      Span2 := Sl2.GetBegin;
      // Ss2 := Sl2.SizeOfSpan;
      Xb2 := Span2.x;
      Xe2 := Xb2 + Abs(Span2.Len) - 1;

      dec(Num2);
    end;

  repeat
    // Retrieve a new Span1 if it's invalid
    if (Num1 <> 0) and (Xb1 > Xe1) then
      begin
        dec(Num1);
        // Inc(PtrComp(Span1), Ss1);
        Span1.IncOperator;

        Xb1 := Span1.x;
        Xe1 := Xb1 + Abs(Span1.Len) - 1;
      end;

    // Retrieve a new Span2 if it's invalid
    if (Num2 <> 0) and (Xb2 > Xe2) then
      begin
        dec(Num2);
        // Inc(PtrComp(Span2), Ss2);
        Span2.IncOperator;

        Xb2 := Span2.x;
        Xe2 := Xb2 + Abs(Span2.Len) - 1;
      end;

    if (Xb1 > Xe1) and (Xb2 > Xe2) then
        Break;

    // Calculate the intersection
    XB := Xb1;
    XE := Xe1;

    if XB < Xb2 then
        XB := Xb2;

    if XE > Xe2 then
        XE := Xe2;

    Len := XE - XB + 1; // The length of the intersection

    if Len > 0 then
      begin
        // The Spans intersect,
        // add the beginning of the Span
        if Xb1 < Xb2 then
          begin
            AddSpan1.Functor1(AddSpan1, Span1, Xb1, Xb2 - Xb1, SL);

            Xb1 := Xb2;
          end
        else if Xb2 < Xb1 then
          begin
            AddSpan2.Functor1(AddSpan2, Span2, Xb2, Xb1 - Xb2, SL);

            Xb2 := Xb1;
          end;

        // Add the combination part of the Spans
        CombineSpans.Functor2(CombineSpans, Span1, Span2, XB, Len, SL);

        // Invalidate the fully processed Span or both
        if Xe1 < Xe2 then
          begin
            // Invalidate Span1 and eat
            // the processed part of Span2
            Xb1 := CInvalidB;
            Xe1 := CInvalidE;

            inc(Xb2, Len);
          end
        else if Xe2 < Xe1 then
          begin
            // Invalidate Span2 and eat
            // the processed part of Span1
            Xb2 := CInvalidB;
            Xe2 := CInvalidE;

            inc(Xb1, Len);
          end
        else
          begin
            Xb1 := CInvalidB; // Invalidate both
            Xb2 := CInvalidB;
            Xe1 := CInvalidE;
            Xe2 := CInvalidE;
          end;
      end
    else
      // The Spans do not intersect
      if Xb1 < Xb2 then
        begin
          // Advance Span1
          if Xb1 <= Xe1 then
              AddSpan1.Functor1(AddSpan1, Span1, Xb1, Xe1 - Xb1 + 1, SL);

          Xb1 := CInvalidB; // Invalidate
          Xe1 := CInvalidE;
        end
      else
        begin
          // Advance Span2
          if Xb2 <= Xe2 then
              AddSpan2.Functor1(AddSpan2, Span2, Xb2, Xe2 - Xb2 + 1, SL);

          Xb2 := CInvalidB; // Invalidate
          Xe2 := CInvalidE;
        end;

  until False;

  if Assigned(Span1) then
      Span1.Free;

  if Assigned(Span2) then
      Span2.Free;
end;

// Unite the ScanLine shapes. Here the "ScanLine Generator"
// abstraction is used. ScanLineGen1 and ScanLineGen2 are
// the generators, and can be of type TAggRasterizerScanLineAA<>.
// There function requires three ScanLine containers that can be
// of different type.
// "sl1" and "sl2" are used to retrieve ScanLines from the generators,
// "sl" is ised as the resulting ScanLine to render it.
// The external "sl1" and "sl2" are used only for the sake of
// optimization and reusing of the scanline objects.
// the function calls BoolScanLineUniteScanLines with CombineSpansFunctor
// as the last argument. See BoolScanLineUniteScanLines for details.
procedure BoolScanLineUniteShapes(Sg1, Sg2: TAggRasterizerScanLine;
  Sl1, Sl2, SL: TAggCustomScanLine; Ren: TAggCustomRendererScanLine;
  AddSpan1, AddSpan2, CombineSpans: TAggBoolScanLineFunctor);
var
  Flag1, Flag2: Boolean;
  r1, r2, Ur: TRectInteger;
begin
  // Prepare the ScanLine generators.
  // If anyone of them doesn't contain
  // any ScanLines, then return.
  Flag1 := Sg1.RewindScanLines;
  Flag2 := Sg2.RewindScanLines;

  if not Flag1 and not Flag2 then
      Exit;

  // Get the bounding boxes
  r1 := RectInteger(Sg1.MinimumX, Sg1.MinimumY, Sg1.MaximumX, Sg1.MaximumY);
  r2 := RectInteger(Sg2.MinimumX, Sg2.MinimumY, Sg2.MaximumX, Sg2.MaximumY);

  // Calculate the union of the bounding boxes
  Ur := UniteRectangles(r1, r2);

  if not Ur.IsValid then
      Exit;

  Ren.Prepare(Cardinal(Ur.x2 - Ur.x2 + 2));

  // Reset the ScanLines and get two first ones
  SL.Reset(Ur.x1, Ur.x2);

  if Flag1 then
    begin
      Sl1.Reset(Sg1.MinimumX, Sg1.MaximumX);

      Flag1 := Sg1.SweepScanLine(Sl1);
    end;

  if Flag2 then
    begin
      Sl2.Reset(Sg2.MinimumX, Sg2.MaximumX);

      Flag2 := Sg2.SweepScanLine(Sl2);
    end;

  // The main loop
  // Here we synchronize the ScanLines with
  // the same Y coordinate.
  while Flag1 or Flag2 do
    if Flag1 and Flag2 then
      if Sl1.y = Sl2.y then
        begin
          // The Y coordinates are the same.
          // Combine the ScanLines, render if they contain any Spans,
          // and advance both generators to the next ScanLines
          BoolScanLineUniteScanLines(Sl1, Sl2, SL, AddSpan1, AddSpan2,
            CombineSpans);

          if SL.NumSpans <> 0 then
            begin
              SL.Finalize(Sl1.y);
              Ren.Render(SL);
            end;

          Flag1 := Sg1.SweepScanLine(Sl1);
          Flag2 := Sg2.SweepScanLine(Sl2);
        end
      else if Sl1.y < Sl2.y then
        begin
          BoolScanLineAddSpansAndRender(Sl1, SL, Ren, AddSpan1);

          Flag1 := Sg1.SweepScanLine(Sl1);
        end
      else
        begin
          BoolScanLineAddSpansAndRender(Sl2, SL, Ren, AddSpan2);

          Flag2 := Sg2.SweepScanLine(Sl2);
        end
    else
      begin
        if Flag1 then
          begin
            BoolScanLineAddSpansAndRender(Sl1, SL, Ren, AddSpan1);

            Flag1 := Sg1.SweepScanLine(Sl1);
          end;

        if Flag2 then
          begin
            BoolScanLineAddSpansAndRender(Sl2, SL, Ren, AddSpan2);

            Flag2 := Sg2.SweepScanLine(Sl2);
          end;
      end;
end;

// Intersect two ScanLines, "sl1" and "sl2" and generate a new "sl" one.
// The CombineSpans functor can be of type BoolScanLineCombineSpansBin or
// SboolIntersecTAggSpansAA. First is a general functor to combine
// two Spans without Anti-Aliasing, the second preserves the AA
// information, but works sLower
procedure BoolScanLineIntersecTAggScanLines(Sl1, Sl2, SL: TAggCustomScanLine;
  CombineSpans: TAggBoolScanLineFunctor);
var
  Num1, Num2: Cardinal;
  // Span1, Span2: PAggSpanRecord;
  Span1, Span2: TAggCustomSpan;

  Xb1, Xb2, Xe1, Xe2: Cardinal;
  // Ss1, Ss2: Integer;

  Advance_Span1, Advance_both: Boolean;

begin
  SL.ResetSpans;

  Num1 := Sl1.NumSpans;

  if Num1 = 0 then
      Exit;

  Num2 := Sl2.NumSpans;

  if Num2 = 0 then
      Exit;

  Span1 := Sl1.GetBegin;
  // Ss1 := Sl1.SizeOfSpan;
  Span2 := Sl2.GetBegin;
  // Ss2 := Sl2.SizeOfSpan;

  while (Num1 <> 0) and (Num2 <> 0) do
    begin
      Xb1 := Span1.x;
      Xb2 := Span2.x;
      Xe1 := Xb1 + Abs(Span1.Len) - 1;
      Xe2 := Xb2 + Abs(Span2.Len) - 1;

      // Determine what Spans we should advance in the next step
      // The Span with the least ending X should be advanced
      // advance_both is just an optimization when we ending
      // coordinates are the same and we can advance both
      Advance_Span1 := Xe1 < Xe2;
      Advance_both := Xe1 = Xe2;

      // Find the intersection of the Spans
      // and check if they intersect
      if Xb1 < Xb2 then
          Xb1 := Xb2;

      if Xe1 > Xe2 then
          Xe1 := Xe2;

      if Xb1 <= Xe1 then
          CombineSpans.Functor2(CombineSpans, Span1, Span2, Xb1,
          Xe1 - Xb1 + 1, SL);

      // Advance the Spans
      if Advance_both then
        begin
          dec(Num1);
          dec(Num2);

          if Num1 <> 0 then
            // Inc(PtrComp(Span1), Ss1);
              Span1.IncOperator;

          if Num2 <> 0 then
            // Inc(PtrComp(Span2), Ss2);
              Span2.IncOperator;
        end
      else if Advance_Span1 then
        begin
          dec(Num1);

          if Num1 <> 0 then
            // Inc(PtrComp(Span1), Ss1);
              Span1.IncOperator;
        end
      else
        begin
          dec(Num2);

          if Num2 <> 0 then
            // Inc(PtrComp(Span2), Ss2);
              Span2.IncOperator;
        end;
    end;

  Span1.Free;
  Span2.Free;
end;

// Intersect the ScanLine shapes. Here the "ScanLine Generator"
// abstraction is used. ScanLineGen1 and ScanLineGen2 are
// the generators, and can be of type TAggRasterizerScanLineAA<>.
// There function requires three ScanLine containers that can be of
// different types.
// "sl1" and "sl2" are used to retrieve ScanLines from the generators,
// "sl" is ised as the resulting ScanLine to render it.
// The external "sl1" and "sl2" are used only for the sake of
// optimization and reusing of the scanline objects.
// the function calls BoolScanLineIntersecTAggScanLines with CombineSpansFunctor
// as the last argument. See BoolScanLineIntersecTAggScanLines for details.
procedure BoolScanLineIntersectShapes(Sg1, Sg2: TAggRasterizerScanLine;
  Sl1, Sl2, SL: TAggCustomScanLine; Ren: TAggCustomRendererScanLine;
  CombineSpans: TAggBoolScanLineFunctor);
var
  r1, r2, IR: TRectInteger;
begin
  // Prepare the ScanLine generators.
  // If anyone of them doesn't contain
  // any ScanLines, then return.
  if not Sg1.RewindScanLines then
      Exit;

  if not Sg2.RewindScanLines then
      Exit;

  // Get the bounding boxes
  r1 := RectInteger(Sg1.MinimumX, Sg1.MinimumY, Sg1.MaximumX, Sg1.MaximumY);
  r2 := RectInteger(Sg2.MinimumX, Sg2.MinimumY, Sg2.MaximumX, Sg2.MaximumY);

  // Calculate the intersection of the bounding
  // boxes and return if they don't intersect.
  IR := IntersectRectangles(r1, r2);

  if not IR.IsValid then
      Exit;

  // Reset the ScanLines and get two first ones
  SL.Reset(IR.x1, IR.x2);
  Sl1.Reset(Sg1.MinimumX, Sg1.MaximumX);
  Sl2.Reset(Sg2.MinimumX, Sg2.MaximumX);

  if not Sg1.SweepScanLine(Sl1) then
      Exit;

  if not Sg2.SweepScanLine(Sl2) then
      Exit;

  Ren.Prepare(Cardinal(IR.x2 - IR.x1 + 2));

  // The main loop
  // Here we synchronize the ScanLines with
  // the same Y coordinate, ignoring all other ones.
  // Only ScanLines having the same Y-coordinate
  // are to be combined.
  repeat
    while Sl1.y < Sl2.y do
      if not Sg1.SweepScanLine(Sl1) then
          Exit;

    while Sl2.y < Sl1.y do
      if not Sg2.SweepScanLine(Sl2) then
          Exit;

    if Sl1.y = Sl2.y then
      begin
        // The Y coordinates are the same.
        // Combine the ScanLines, render if they contain any Spans,
        // and advance both generators to the next ScanLines
        BoolScanLineIntersecTAggScanLines(Sl1, Sl2, SL, CombineSpans);

        if SL.NumSpans <> 0 then
          begin
            SL.Finalize(Sl1.y);
            Ren.Render(SL);
          end;

        if not Sg1.SweepScanLine(Sl1) then
            Exit;

        if not Sg2.SweepScanLine(Sl2) then
            Exit;
      end;
  until False;
end;

// Subtract the ScanLine shapes, "sg1-sg2". Here the "ScanLine Generator"
// abstraction is used. ScanLineGen1 and ScanLineGen2 are
// the generators, and can be of type TAggRasterizerScanLineAA<>.
// There function requires three ScanLine containers that can be of
// different types.
// "sl1" and "sl2" are used to retrieve ScanLines from the generators,
// "sl" is ised as the resulting ScanLine to render it.
// The external "sl1" and "sl2" are used only for the sake of
// optimization and reusing of the scanline objects.
// the function calls BoolScanLineIntersecTAggScanLines with CombineSpansFunctor
// as the last argument. See combine_ScanLines_sub for details.
procedure BoolScanLineSubtractShapes(Sg1, Sg2: TAggRasterizerScanLine;
  Sl1, Sl2, SL: TAggCustomScanLine; Ren: TAggCustomRendererScanLine;
  AddSpan1, CombineSpans: TAggBoolScanLineFunctor);
var
  r1: TRectInteger;
  Flag1, Flag2: Boolean;
  AddSpan2: TAggBoolScanLineFunctor;
begin
  // Prepare the ScanLine generators.
  // Here "sg1" is master, "sg2" is slave.
  if not Sg1.RewindScanLines then
      Exit;

  Flag2 := Sg2.RewindScanLines;

  // Get the bounding box
  r1 := RectInteger(Sg1.MinimumX, Sg1.MinimumY, Sg1.MaximumX, Sg1.MaximumY);

  // Reset the ScanLines and get two first ones
  SL.Reset(Sg1.MinimumX, Sg1.MaximumX);
  Sl1.Reset(Sg1.MinimumX, Sg1.MaximumX);
  Sl2.Reset(Sg2.MinimumX, Sg2.MaximumX);

  if not Sg1.SweepScanLine(Sl1) then
      Exit;

  if Flag2 then
      Flag2 := Sg2.SweepScanLine(Sl2);

  Ren.Prepare(Cardinal(Sg1.MaximumX - Sg1.MinimumX + 2));

  // A fake Span2 processor
  AddSpan2 := TAggBoolScanLineFunctor.Create1(@BoolScanLineAddSpanEmpty);
  try
    // The main loop
    // Here we synchronize the ScanLines with
    // the same Y coordinate, ignoring all other ones.
    // Only ScanLines having the same Y-coordinate
    // are to be combined.
    Flag1 := True;

    repeat
      // Synchronize "slave" with "master"
      while Flag2 and (Sl2.y < Sl1.y) do
          Flag2 := Sg2.SweepScanLine(Sl2);

      if Flag2 and (Sl2.y = Sl1.y) then
        begin
          // The Y coordinates are the same.
          // Combine the ScanLines and render if they contain any Spans.
          BoolScanLineUniteScanLines(Sl1, Sl2, SL, AddSpan1, AddSpan2, CombineSpans);

          if SL.NumSpans <> 0 then
            begin
              SL.Finalize(Sl1.y);
              Ren.Render(SL);
            end;
        end
      else
          BoolScanLineAddSpansAndRender(Sl1, SL, Ren, AddSpan1);

      // Advance the "master"
      Flag1 := Sg1.SweepScanLine(Sl1);
    until not Flag1;
  finally
      AddSpan2.Free;
  end;
end;

// Subtract shapes "sg1-sg2" with anti-aliasing
// See IntersectShapes_aa for more comments
procedure BoolScanLineSubtractShapesAA;
var
  AddFunctor, CombineFunctor: TAggBoolScanLineFunctor;
begin
  AddFunctor := TAggBoolScanLineFunctor.Create1(@BoolScanLineAddSpanAA);
  CombineFunctor := TAggBoolScanLineFunctor.Create2(@BoolScanLineSubtracTAggSpansAA);
  try
      BoolScanLineSubtractShapes(Sg1, Sg2, Sl1, Sl2, SL, Ren, AddFunctor,
      CombineFunctor);
  finally
    AddFunctor.Free;
    CombineFunctor.Free;
  end;
end;

// Apply eXclusive OR to two anti-aliased ScanLine shapes.
// There's the absolute difference used to calculate
// Anti-Aliasing values, that is:
// a XOR b : abs(a-b)
// See IntersectShapes_aa for more comments
procedure BoolScanLineXORShapesAbsDiffAA;
var
  AddFunctor, CombineFunctor: TAggBoolScanLineFunctor;
begin
  AddFunctor := TAggBoolScanLineFunctor.Create1(@BoolScanLineAddSpanAA);
  CombineFunctor := TAggBoolScanLineFunctor.Create2(@BoolScanLineXORSpansAA);
  try
    CombineFunctor.Formula := @BoolScanLineXORFormulaAbsDiff;

    BoolScanLineUniteShapes(Sg1, Sg2, Sl1, Sl2, SL, Ren, AddFunctor, AddFunctor,
      CombineFunctor);
  finally
    AddFunctor.Free;
    CombineFunctor.Free;
  end;
end;

// Apply eXclusive OR to two anti-aliased ScanLine shapes.
// There's the classical "Saddle" used to calculate the
// Anti-Aliasing values, that is:
// a XOR b : 1-((1-a+a*b)*(1-b+a*b))
// See IntersectShapes_aa for more comments
procedure BoolScanLineXORShapesSaddleAA;
var
  AddFunctor, CombineFunctor: TAggBoolScanLineFunctor;
begin
  AddFunctor := TAggBoolScanLineFunctor.Create1(@BoolScanLineAddSpanAA);
  CombineFunctor := TAggBoolScanLineFunctor.Create2(@BoolScanLineXORSpansAA);

  try
    CombineFunctor.Formula := @BoolScanLineXORFormulaSaddle;

    BoolScanLineUniteShapes(Sg1, Sg2, Sl1, Sl2, SL, Ren, AddFunctor, AddFunctor,
      CombineFunctor);
  finally
    AddFunctor.Free;
    CombineFunctor.Free;
  end;
end;

// Apply eXclusive OR to two anti-aliased ScanLine shapes. There's
// a modified "Linear" XOR used instead of classical "Saddle" one.
// The reason is to have the result absolutely conststent with what
// the ScanLine Rasterizer produces.
// See IntersectShapes_aa for more comments
procedure BoolScanLineXORShapesAA;
var
  AddFunctor, CombineFunctor: TAggBoolScanLineFunctor;
begin
  AddFunctor := TAggBoolScanLineFunctor.Create1(@BoolScanLineAddSpanAA);
  CombineFunctor := TAggBoolScanLineFunctor.Create2(@BoolScanLineXORSpansAA);

  try
    CombineFunctor.Formula := @BoolScanLineXORFormulaLinear;

    BoolScanLineUniteShapes(Sg1, Sg2, Sl1, Sl2, SL, Ren, AddFunctor,
      AddFunctor, CombineFunctor);
  finally
    AddFunctor.Free;
    CombineFunctor.Free;
  end;
end;

// Intersect two anti-aliased ScanLine shapes.
// Here the "ScanLine Generator" abstraction is used.
// ScanLineGen1 and ScanLineGen2 are the generators, and can be of
// type TAggRasterizerScanLineAA<>. There function requires three
// ScanLine containers that can be of different types.
// "sl1" and "sl2" are used to retrieve ScanLines from the generators,
// "sl" is ised as the resulting ScanLine to render it.
// The external "sl1" and "sl2" are used only for the sake of
// optimization and reusing of the scanline objects.
procedure BoolScanLineIntersectShapesAA(Sg1, Sg2: TAggRasterizerScanLine;
  Sl1, Sl2, SL: TAggCustomScanLine; Ren: TAggCustomRendererScanLine);
var
  CombineFunctor: TAggBoolScanLineFunctor;
begin
  CombineFunctor := TAggBoolScanLineFunctor.Create2(@SboolIntersecTAggSpansAA);
  try
      BoolScanLineIntersectShapes(Sg1, Sg2, Sl1, Sl2, SL, Ren, CombineFunctor);
  finally
      CombineFunctor.Free;
  end;
end;

// Unite two anti-aliased ScanLine shapes
// See IntersectShapes_aa for more comments
procedure BoolScanLineUniteShapesAA;
var
  AddFunctor, CombineFunctor: TAggBoolScanLineFunctor;
begin
  AddFunctor := TAggBoolScanLineFunctor.Create1(@BoolScanLineAddSpanAA);
  CombineFunctor := TAggBoolScanLineFunctor.Create2(@BoolScanLineUniteSpansAA);
  try
      BoolScanLineUniteShapes(Sg1, Sg2, Sl1, Sl2, SL, Ren, AddFunctor,
      AddFunctor, CombineFunctor);
  finally
    AddFunctor.Free;
    CombineFunctor.Free;
  end;
end;

procedure BoolScanLineCombineShapesAA(Op: TAggBoolScanLineOp;
  Sg1, Sg2: TAggRasterizerScanLine; Sl1, Sl2, SL: TAggCustomScanLine;
  Ren: TAggCustomRendererScanLine);
begin
  case Op of
    bsoOr:
      BoolScanLineUniteShapesAA(Sg1, Sg2, Sl1, Sl2, SL, Ren);

    bsoAnd:
      BoolScanLineIntersectShapesAA(Sg1, Sg2, Sl1, Sl2, SL, Ren);

    bsoXor:
      BoolScanLineXORShapesAA(Sg1, Sg2, Sl1, Sl2, SL, Ren);

    bsoXorSaddle:
      BoolScanLineXORShapesSaddleAA(Sg1, Sg2, Sl1, Sl2, SL, Ren);

    bsoXorAbsDiff:
      BoolScanLineXORShapesAbsDiffAA(Sg1, Sg2, Sl1, Sl2, SL, Ren);

    bsoAMinusB:
      BoolScanLineSubtractShapesAA(Sg1, Sg2, Sl1, Sl2, SL, Ren);

    bsoBMinusA:
      BoolScanLineSubtractShapesAA(Sg2, Sg1, Sl2, Sl1, SL, Ren);
  end;
end;

procedure BoolScanLineSubtractShapesBin;
var
  AddFunctor, CombineFunctor: TAggBoolScanLineFunctor;
begin
  AddFunctor := TAggBoolScanLineFunctor.Create1(@BoolScanLineAddSpanBin);
  CombineFunctor := TAggBoolScanLineFunctor.Create2(@BoolScanLineCombineSpansEmpty);
  try
      BoolScanLineSubtractShapes(Sg1, Sg2, Sl1, Sl2, SL, Ren, AddFunctor,
      CombineFunctor);
  finally
    AddFunctor.Free;
    CombineFunctor.Free;
  end;
end;

procedure BoolScanLineXORShapesBin;
var
  AddFunctor, CombineFunctor: TAggBoolScanLineFunctor;

begin
  AddFunctor := TAggBoolScanLineFunctor.Create1(@BoolScanLineAddSpanBin);
  CombineFunctor := TAggBoolScanLineFunctor.Create2(@BoolScanLineCombineSpansEmpty);

  try
      BoolScanLineUniteShapes(Sg1, Sg2, Sl1, Sl2, SL, Ren, AddFunctor,
      AddFunctor, CombineFunctor);
  finally
    AddFunctor.Free;
    CombineFunctor.Free;
  end;
end;

procedure BoolScanLineIntersectShapesBin;
var
  CombineFunctor: TAggBoolScanLineFunctor;
begin
  CombineFunctor := TAggBoolScanLineFunctor.Create2(@BoolScanLineCombineSpansBin);

  try
      BoolScanLineIntersectShapes(Sg1, Sg2, Sl1, Sl2, SL, Ren, CombineFunctor);
  finally
      CombineFunctor.Free;
  end;
end;

procedure BoolScanLineUniteShapesBin;
var
  AddFunctor, CombineFunctor: TAggBoolScanLineFunctor;
begin
  AddFunctor := TAggBoolScanLineFunctor.Create1(@BoolScanLineAddSpanBin);
  CombineFunctor := TAggBoolScanLineFunctor.Create2(@BoolScanLineCombineSpansBin);

  try
      BoolScanLineUniteShapes(Sg1, Sg2, Sl1, Sl2, SL, Ren, AddFunctor,
      AddFunctor, CombineFunctor);
  finally
    AddFunctor.Free;
    CombineFunctor.Free;
  end;
end;

procedure BoolScanLineCombineShapesBin(Op: TAggBoolScanLineOp;
  Sg1, Sg2: TAggRasterizerScanLine; Sl1, Sl2, SL: TAggCustomScanLine;
  Ren: TAggCustomRendererScanLine);
begin
  case Op of
    bsoOr:
      BoolScanLineUniteShapesBin(Sg1, Sg2, Sl1, Sl2, SL, Ren);

    bsoAnd:
      BoolScanLineIntersectShapesBin(Sg1, Sg2, Sl1, Sl2, SL, Ren);

    bsoXor, bsoXorSaddle, bsoXorAbsDiff:
      BoolScanLineXORShapesBin(Sg1, Sg2, Sl1, Sl2, SL, Ren);

    bsoAMinusB:
      BoolScanLineSubtractShapesBin(Sg1, Sg2, Sl1, Sl2, SL, Ren);

    bsoBMinusA:
      BoolScanLineSubtractShapesBin(Sg2, Sg1, Sl2, Sl1, SL, Ren);
  end;
end;

end. 
 
 
