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
unit AggRendererScanLine;

{$DEFINE FPC_DELPHI_MODE}
{$INCLUDE zDefine.inc}
interface
uses
  AggBasics,
  AggRendererBase,
  AggColor32,
  AggScanline,
  AggScanlinePacked,
  AggScanlineBin,
  AggSpanGenerator,
  AggSpanAllocator,
  AggRasterizerScanLine,
  AggRasterizerCompoundAA;

type
  TAggCustomRendererScanLine = class // class(TAggRasterizerScanLine)
  public
    procedure Prepare(u: Cardinal); virtual; abstract;
    procedure Render(SL: TAggCustomScanLine); virtual; abstract;
  end;

  TAggRendererScanLineAA = class(TAggCustomRendererScanLine)
  private
    FRen: TAggRendererBase;
    FSpanGen: TAggSpanGenerator;
  public
    constructor Create; overload;
    constructor Create(Ren: TAggRendererBase; SpanGen: TAggSpanGenerator); overload;

    procedure Attach(Ren: TAggRendererBase; SpanGen: TAggSpanGenerator);

    procedure Prepare(u: Cardinal); override;
    procedure Render(SL: TAggCustomScanLine); override;
  end;

  TAggCustomRendererScanLineSolid = class(TAggCustomRendererScanLine)
  public
    procedure SetColor(c: PAggColor); overload; virtual; abstract;
    procedure SetColor(c: TAggRgba8); overload; virtual; abstract;
  end;

  TAggRendererScanLineAASolid = class(TAggCustomRendererScanLineSolid)
  private
    FRen: TAggRendererBase;
    FColor: TAggColor;
  public
    constructor Create(Ren: TAggRendererBase);

    procedure SetColor(c: PAggColor); override;
    procedure SetColor(c: TAggRgba8); override;
    procedure Prepare(u: Cardinal); override;
    procedure Render(SL: TAggCustomScanLine); override;
  end;

  TAggRendererScanLineBinSolid = class(TAggCustomRendererScanLineSolid)
  private
    FRen: TAggRendererBase;
    FColor: TAggColor;
  public
    constructor Create(Ren: TAggRendererBase);

    procedure SetColor(c: PAggColor); override;
    procedure SetColor(c: TAggRgba8); override;
    procedure Prepare(u: Cardinal); override;
    procedure Render(SL: TAggCustomScanLine); override;
  end;

  TAggCustomStyleHandler = class
  public
    function IsSolid(Style: Cardinal): Boolean; virtual; abstract;
    function GetColor(Style: Cardinal): PAggColor; virtual; abstract;

    procedure GenerateSpan(Span: PAggColor; x, y: Integer; Len, Style: Cardinal); virtual; abstract;
  end;

procedure RenderScanLineAASolid(SL: TAggCustomScanLine; Ren: TAggRendererBase; COLOR: PAggColor);

procedure RenderScanLinesAASolid(Ras: TAggRasterizerScanLine; SL: TAggCustomScanLine; Ren: TAggRendererBase; COLOR: PAggColor);

procedure RenderScanLinesCompound(Ras: TAggRasterizerCompoundAA; ScanLineAA, SlBin: TAggCustomScanLine; Ren: TAggRendererBase; Alloc: TAggSpanAllocator; StyleHandler: TAggCustomStyleHandler);

procedure RenderScanLinesCompoundLayered(Ras: TAggRasterizerCompoundAA; ScanLineAA: TAggCustomScanLine; Ren: TAggRendererBase; Alloc: TAggSpanAllocator; StyleHandler: TAggCustomStyleHandler);

implementation


{ TAggRendererScanLineAA }

constructor TAggRendererScanLineAA.Create;
begin
  FRen := nil;
  FSpanGen := nil;
end;

constructor TAggRendererScanLineAA.Create(Ren: TAggRendererBase;
  SpanGen: TAggSpanGenerator);
begin
  Assert(Ren is TAggRendererBase);
  FRen := Ren;
  FSpanGen := SpanGen;
end;

procedure TAggRendererScanLineAA.Attach(Ren: TAggRendererBase;
  SpanGen: TAggSpanGenerator);
begin
  Assert(Ren is TAggRendererBase);
  FRen := Ren;
  FSpanGen := SpanGen;
end;

procedure TAggRendererScanLineAA.Prepare(u: Cardinal);
begin
  FSpanGen.Prepare(u);
end;

procedure TAggRendererScanLineAA.Render(SL: TAggCustomScanLine);
var
  y, XMin, XMax, x, Len: Integer;

  NumSpans: Cardinal;
  // SS: Cardinal;

  // Span  : PAggSpanRecord;
  Span: TAggCustomSpan;
  Solid: Boolean;
  Covers: PInt8u;
begin
  y := SL.y;

  FRen.FirstClipBox;

  repeat
    XMin := FRen.XMin;
    XMax := FRen.XMax;

    if (y >= FRen.YMin) and (y <= FRen.YMax) then
      begin
        NumSpans := SL.NumSpans;

        Span := SL.GetBegin;
        // Ss := Sl.SizeOfSpan;

        repeat
          x := Span.x;
          Len := Span.Len;

          Solid := False;
          Covers := Span.Covers;

          if Len < 0 then
            begin
              Solid := True;
              Len := -Len;
            end;

          if x < XMin then
            begin
              dec(Len, XMin - x);

              if not Solid then
                  inc(PtrComp(Covers), XMin - x);

              x := XMin;
            end;

          if Len > 0 then
            begin
              if x + Len > XMax then
                  Len := XMax - x + 1;

              if Len > 0 then
                if Solid then
                    FRen.BlendColorHSpanNoClip(x, y, Len,
                    FSpanGen.Generate(x, y, Len), nil, Covers^)
                else
                    FRen.BlendColorHSpanNoClip(x, y, Len,
                    FSpanGen.Generate(x, y, Len), Covers, Covers^);
            end;

          dec(NumSpans);

          if NumSpans = 0 then
              Break;

          // Inc(PtrComp(Span), Ss);
          Span.IncOperator;
        until False;

        Span.Free;
      end;

  until not FRen.NextClipBox;
end;

{ TAggRendererScanLineAASolid }

constructor TAggRendererScanLineAASolid.Create(Ren: TAggRendererBase);
begin
  Assert(Ren is TAggRendererBase);
  FRen := Ren;
end;

procedure TAggRendererScanLineAASolid.SetColor(c: PAggColor);
begin
  Assert(Assigned(c));
  FColor := c^;
end;

procedure TAggRendererScanLineAASolid.SetColor(c: TAggRgba8);
begin
  FColor.FromRgba8(c);
end;

procedure TAggRendererScanLineAASolid.Prepare(u: Cardinal);
begin
end;

procedure TAggRendererScanLineAASolid.Render(SL: TAggCustomScanLine);
var
  x, y: Integer;
  // Ss: Cardinal;
  // SpanRecord : PAggSpanRecord;
  NumSpans: Cardinal;
  Span: TAggCustomSpan;
begin
  y := SL.y;

  NumSpans := SL.NumSpans;

  // SpanRecord := nil;
  // Span := nil;

  // if Sl.IsPlainSpan then
  // begin
  // SpanRecord := Sl.GetBegin;
  // Ss := Sl.SizeOfSpan;
  // end
  // else
  // Span := Sl.GetBegin;
  Span := SL.GetBegin;

  { if SpanRecord <> nil then
    repeat
    X := SpanRecord.X;

    if SpanRecord.Len > 0 then
    FRen.BlendSolidHSpan(X, Y, Cardinal(SpanRecord.Len), @FColor,
    SpanRecord.Covers)
    else
    FRen.BlendHorizontalLine(X, Y, Cardinal(X - SpanRecord.Len - 1), @FColor,
    SpanRecord.Covers^);

    Dec(NumSpans);

    if NumSpans = 0 then
    Break;

    Inc(PtrComp(SpanRecord), Ss);
    until False
    else
    begin }
  repeat
    x := Span.x;

    if Span.Len > 0 then
        FRen.BlendSolidHSpan(x, y, Cardinal(Span.Len), @FColor,
        Span.Covers)
    else
        FRen.BlendHorizontalLine(x, y, Cardinal(x - Span.Len - 1), @FColor,
        Span.Covers^);

    dec(NumSpans);

    if NumSpans = 0 then
        Break;

    Span.IncOperator;
  until False;

  Span.Free;
  { end; }
end;

{ TAggRendererScanLineBinSolid }

constructor TAggRendererScanLineBinSolid.Create(Ren: TAggRendererBase);
begin
  Assert(Ren is TAggRendererBase);
  FRen := Ren;
end;

procedure TAggRendererScanLineBinSolid.SetColor(c: PAggColor);
begin
  FColor := c^;
end;

procedure TAggRendererScanLineBinSolid.SetColor(c: TAggRgba8);
begin
  FColor.FromRgba8(c);
end;

procedure TAggRendererScanLineBinSolid.Prepare(u: Cardinal);
begin
end;

procedure TAggRendererScanLineBinSolid.Render(SL: TAggCustomScanLine);
var
  // SpanPl: PAggSpanRecord;
  // Ss: Cardinal;
  Span: TAggCustomSpan;
  NumSpans: Cardinal;
begin
  NumSpans := SL.NumSpans;

  { SpanPl := nil;
    Span := nil;

    if Sl.IsPlainSpan then
    begin
    SpanPl := Sl.GetBegin;
    Ss := Sl.SizeOfSpan;
    end
    else
    Span := Sl.GetBegin; }
  Span := SL.GetBegin;

  { if SpanPl <> nil then
    repeat
    if SpanPl.Len < 0 then
    FRen.BlendHorizontalLine(SpanPl.X, Sl.Y, SpanPl.X - 1 - SpanPl.Len,
    @FColor, CAggCoverFull)
    else
    FRen.BlendHorizontalLine(SpanPl.X, Sl.Y, SpanPl.X - 1 + SpanPl.Len,
    @FColor, CAggCoverFull);

    Dec(NumSpans);

    if NumSpans = 0 then
    Break;

    Inc(PtrComp(SpanPl), Ss);
    until False
    else }
  repeat
    if Span.Len < 0 then
        FRen.BlendHorizontalLine(Span.x, SL.y, Span.x - 1 - Span.Len,
        @FColor, CAggCoverFull)
    else
        FRen.BlendHorizontalLine(Span.x, SL.y, Span.x - 1 + Span.Len,
        @FColor, CAggCoverFull);

    dec(NumSpans);

    if NumSpans = 0 then
        Break;

    Span.IncOperator;
  until False;

  Span.Free;
end;

procedure RenderScanLineAASolid(SL: TAggCustomScanLine; Ren: TAggRendererBase;
  COLOR: PAggColor);
var
  y, x: Integer;
  NumSpans: Cardinal;
  // Ss: Cardinal;
  // Span: PAggSpanRecord;
  Span: TAggCustomSpan;
begin
  Assert(Ren is TAggRendererBase);

  y := SL.y;
  NumSpans := SL.NumSpans;
  Span := SL.GetBegin;
  // Ss := Sl.SizeOfSpan;

  repeat
    x := Span.x;

    if Span.Len > 0 then
        Ren.BlendSolidHSpan(x, y, Cardinal(Span.Len), COLOR, Span.Covers)
    else
        Ren.BlendHorizontalLine(x, y, Cardinal(x - Span.Len - 1), COLOR, Span.Covers^);

    dec(NumSpans);

    if NumSpans = 0 then
        Break;

    // Inc(PtrComp(Span), Ss);
    Span.IncOperator;
  until False;

  Span.Free;
end;

procedure RenderScanLinesAASolid(Ras: TAggRasterizerScanLine;
  SL: TAggCustomScanLine; Ren: TAggRendererBase; COLOR: PAggColor);
var
  y, x: Integer;
  NumSpans: Cardinal;
  // Ss: Cardinal;
  // Span: PAggSpanRecord;
  Span: TAggCustomSpan;
begin
  Assert(Ren is TAggRendererBase);

  if Ras.RewindScanLines then
    begin
      SL.Reset(Ras.MinimumX, Ras.MaximumX);

      while Ras.SweepScanLine(SL) do
        begin
          y := SL.y;
          NumSpans := SL.NumSpans;
          // Ss := Sl.SizeOfSpan;
          Span := SL.GetBegin;

          repeat
            x := Span.x;

            if Span.Len > 0 then
                Ren.BlendSolidHSpan(x, y, Cardinal(Span.Len), COLOR, Span.Covers)
            else
                Ren.BlendHorizontalLine(x, y, Cardinal(x - Span.Len - 1), COLOR,
                Span.Covers^);

            dec(NumSpans);

            if NumSpans = 0 then
                Break;

            // Inc(PtrComp(Span), Ss);
            Span.IncOperator;
          until False;

          Span.Free;
        end;
    end;
end;

procedure RenderScanLinesCompound(Ras: TAggRasterizerCompoundAA;
  ScanLineAA, SlBin: TAggCustomScanLine; Ren: TAggRendererBase;
  Alloc: TAggSpanAllocator; StyleHandler: TAggCustomStyleHandler);
var
  MinX, Len: Integer;
  NumSpans, NumStyles, Style, i: Cardinal;
  // SsAntiAlias, SsBin: Cardinal;
  ColorSpan, MixBuffer, Colors, ClrSpan: PAggColor;
  c: TAggColor;
  Solid: Boolean;
  // SpanAA: PAggSpanRecord;
  // SpanBin: PAggSpanBin;
  SpanAA: TAggCustomSpan;
  SpanBin: TAggCustomSpan;
  Covers: PInt8u;
begin
  Assert(Ren is TAggRendererBase);

  if Ras.RewindScanLines then
    begin
      MinX := Ras.MinimumX;
      Len := Ras.MaximumX - MinX + 2;

      ScanLineAA.Reset(MinX, Ras.MaximumX);
      SlBin.Reset(MinX, Ras.MaximumX);

      ColorSpan := Alloc.Allocate(Len * 2);
      MixBuffer := PAggColor(PtrComp(ColorSpan) + Len * SizeOf(TAggColor));

      NumStyles := Ras.SweepStyles;

      while NumStyles > 0 do
        begin
          if NumStyles = 1 then
            // Optimization for a single style. Happens often
            if Ras.SweepScanLine(ScanLineAA, 0) then
              begin
                Style := Ras.Style(0);

                if StyleHandler.IsSolid(Style) then
                  // Just solid fill
                    RenderScanLineAASolid(ScanLineAA, Ren, StyleHandler.GetColor(Style))
                else
                  begin
                    // Arbitrary Span generator
                    SpanAA := ScanLineAA.GetBegin;
                    // SsAntiAlias := ScanLineAA.SizeOfSpan;
                    NumSpans := ScanLineAA.NumSpans;

                    repeat
                      Len := SpanAA.Len;

                      StyleHandler.GenerateSpan(ColorSpan, SpanAA.x, ScanLineAA.y, Len, Style);
                      Ren.BlendColorHSpan(SpanAA.x, ScanLineAA.y, SpanAA.Len, ColorSpan,
                        SpanAA.Covers);

                      dec(NumSpans);

                      if NumSpans = 0 then
                          Break;

                      // Inc(PtrComp(SpanAA), SsAntiAlias);
                      SpanAA.IncOperator;
                    until False;

                    SpanAA.Free;
                  end;
              end
            else
          else // if NumStyles = 1 ... else
            if Ras.SweepScanLine(SlBin, -1) then
              begin
                // Clear the Spans of the MixBuffer
                SpanBin := SlBin.GetBegin;
                // SsBin := SlBin.SizeOfSpan;
                NumSpans := SlBin.NumSpans;

                repeat
                  FillChar(PAggColor(PtrComp(MixBuffer) + (SpanBin.x - MinX) *
                    SizeOf(TAggColor))^, SpanBin.Len * SizeOf(TAggColor), 0);

                  dec(NumSpans);

                  if NumSpans = 0 then
                      Break;

                  // Inc(PtrComp(SpanBin), SsBin);
                  SpanBin.IncOperator;
                until False;

                SpanBin.Free;

                i := 0;

                while i < NumStyles do
                  begin
                    Style := Ras.Style(i);
                    Solid := StyleHandler.IsSolid(Style);

                    if Ras.SweepScanLine(ScanLineAA, i) then
                      begin
                        SpanAA := ScanLineAA.GetBegin;
                        // SsAntiAlias := ScanLineAA.SizeOfSpan;
                        NumSpans := ScanLineAA.NumSpans;

                        if Solid then
                          // Just solid fill
                          repeat
                            c := StyleHandler.GetColor(Style)^;
                            Len := SpanAA.Len;

                            Colors := PAggColor(PtrComp(MixBuffer) + (SpanAA.x - MinX)
                              * SizeOf(TAggColor));
                            Covers := SpanAA.Covers;

                            repeat
                              if Covers^ = CAggCoverFull then
                                  Colors^ := c
                              else
                                  Colors.Add(@c, Covers^);

                              inc(PtrComp(Colors), SizeOf(TAggColor));
                              inc(PtrComp(Covers), SizeOf(Int8u));
                              dec(Len);

                            until Len = 0;

                            dec(NumSpans);

                            if NumSpans = 0 then
                                Break;

                            // Inc(PtrComp(SpanAA), SsAntiAlias);
                            SpanAA.IncOperator;

                          until False

                        else
                          // Arbitrary Span generator
                          repeat
                            Len := SpanAA.Len;
                            Colors := PAggColor(PtrComp(MixBuffer) + (SpanAA.x - MinX)
                              * SizeOf(TAggColor));
                            ClrSpan := ColorSpan;

                            StyleHandler.GenerateSpan(ClrSpan, SpanAA.x, ScanLineAA.y,
                              Len, Style);

                            Covers := SpanAA.Covers;

                            repeat
                              if Covers^ = CAggCoverFull then
                                  Colors^ := ClrSpan^
                              else
                                  Colors.Add(ClrSpan, Covers^);

                              inc(PtrComp(ClrSpan), SizeOf(TAggColor));
                              inc(PtrComp(Colors), SizeOf(TAggColor));
                              inc(PtrComp(Covers), SizeOf(Int8u));
                              dec(Len);

                            until Len = 0;

                            dec(NumSpans);

                            if NumSpans = 0 then
                                Break;

                            // Inc(PtrComp(SpanAA), SsAntiAlias);
                            SpanAA.IncOperator;

                          until False;

                        SpanAA.Free;
                      end;

                    inc(i);
                  end;

                // Emit the blended result as a color hSpan
                SpanBin := SlBin.GetBegin;
                // SsBin := SlBin.SizeOfSpan;
                NumSpans := SlBin.NumSpans;

                repeat
                  Ren.BlendColorHSpan(SpanBin.x, SlBin.y, SpanBin.Len,
                    PAggColor(PtrComp(MixBuffer) + (SpanBin.x - MinX) *
                    SizeOf(TAggColor)), 0, CAggCoverFull);

                  dec(NumSpans);

                  if NumSpans = 0 then
                      Break;

                  // Inc(PtrComp(SpanBin), SsBin);
                  SpanBin.IncOperator;
                until False;

                SpanBin.Free;
              end; // if ras.SweepScanLine(SlBin ,-1 )

          NumStyles := Ras.SweepStyles;
        end; // while NumStyles > 0
    end;     // if ras.RewindScanLines
end;

procedure RenderScanLinesCompoundLayered(Ras: TAggRasterizerCompoundAA;
  ScanLineAA: TAggCustomScanLine; Ren: TAggRendererBase; Alloc: TAggSpanAllocator;
  StyleHandler: TAggCustomStyleHandler);
var
  MinX, Len, ScanLineStart, Sl_y: Integer;
  NumSpans, NumStyles, Style, ScanLineLen, i, Cover: Cardinal;
  // SsAntiAlias: Cardinal;
  ColorSpan, MixBuffer, Colors, ClrSpan: PAggColor;
  Solid: Boolean;
  c: TAggColor;
  CoverBuffer, SourceCovers, DestCovers: PCover;
  // SpanAA: PAggSpanRecord;
  SpanAA: TAggCustomSpan;
begin
  Assert(Ren is TAggRendererBase);

  if Ras.RewindScanLines then
    begin
      MinX := Ras.MinimumX;
      Len := Ras.MaximumX - MinX + 2;

      ScanLineAA.Reset(MinX, Ras.MaximumX);

      ColorSpan := Alloc.Allocate(Len * 2);
      MixBuffer := PAggColor(PtrComp(ColorSpan) + Len * SizeOf(TAggColor));
      CoverBuffer := Ras.AllocateCoverBuffer(Len);

      NumStyles := Ras.SweepStyles;

      while NumStyles > 0 do
        begin
          if NumStyles = 1 then
            // Optimization for a single style. Happens often
            if Ras.SweepScanLine(ScanLineAA, 0) then
              begin
                Style := Ras.Style(0);

                if StyleHandler.IsSolid(Style) then
                  // Just solid fill
                    RenderScanLineAASolid(ScanLineAA, Ren, StyleHandler.GetColor(Style))

                else
                  begin
                    // Arbitrary Span generator
                    SpanAA := ScanLineAA.GetBegin;
                    NumSpans := ScanLineAA.NumSpans;
                    // SsAntiAlias := ScanLineAA.SizeOfSpan;

                    repeat
                      Len := SpanAA.Len;

                      StyleHandler.GenerateSpan(ColorSpan, SpanAA.x, ScanLineAA.y, Len, Style);
                      Ren.BlendColorHSpan(SpanAA.x, ScanLineAA.y, SpanAA.Len, ColorSpan,
                        SpanAA.Covers);

                      dec(NumSpans);

                      if NumSpans = 0 then
                          Break;

                      // Inc(PtrComp(SpanAA), SsAntiAlias);
                      SpanAA.IncOperator;

                    until False;

                    SpanAA.Free;
                  end;

              end
            else
          else
            begin
              ScanLineStart := Ras.ScanLineStart;
              ScanLineLen := Ras.ScanLineLength;

              if ScanLineLen <> 0 then
                begin
                  FillChar(PAggColor(PtrComp(MixBuffer) + (ScanLineStart - MinX) *
                    SizeOf(TAggColor))^, ScanLineLen * SizeOf(TAggColor), 0);

                  FillChar(PCover(PtrComp(CoverBuffer) + (ScanLineStart - MinX) *
                    SizeOf(TCover))^, ScanLineLen * SizeOf(TCover), 0);

                  Sl_y := $7FFFFFFF;

                  i := 0;

                  while i < NumStyles do
                    begin
                      Style := Ras.Style(i);
                      Solid := StyleHandler.IsSolid(Style);

                      if Ras.SweepScanLine(ScanLineAA, i) then
                        begin
                          SpanAA := ScanLineAA.GetBegin;
                          NumSpans := ScanLineAA.NumSpans;
                          Sl_y := ScanLineAA.y;
                          // SsAntiAlias := ScanLineAA.SizeOfSpan;

                          if Solid then
                            // Just solid fill
                            repeat
                              c := StyleHandler.GetColor(Style)^;

                              Len := SpanAA.Len;
                              Colors := PAggColor(PtrComp(MixBuffer) + (SpanAA.x - MinX)
                                * SizeOf(TAggColor));

                              SourceCovers := PCover(SpanAA.Covers);
                              DestCovers :=
                                PCover(PtrComp(CoverBuffer) + (SpanAA.x - MinX) *
                                SizeOf(TCover));

                              repeat
                                Cover := SourceCovers^;

                                if DestCovers^ + Cover > CAggCoverFull then
                                    Cover := CAggCoverFull - DestCovers^;

                                if Cover <> 0 then
                                  begin
                                    Colors.Add(@c, Cover);

                                    DestCovers^ := DestCovers^ + Cover;
                                  end;

                                inc(PtrComp(Colors), SizeOf(TAggColor));
                                inc(PtrComp(SourceCovers), SizeOf(TCover));
                                inc(PtrComp(DestCovers), SizeOf(TCover));
                                dec(Len);

                              until Len = 0;

                              dec(NumSpans);

                              if NumSpans = 0 then
                                  Break;

                              // Inc(PtrComp(SpanAA), SsAntiAlias);
                              SpanAA.IncOperator;

                            until False
                          else
                            // Arbitrary Span generator
                            repeat
                              Len := SpanAA.Len;
                              Colors := PAggColor(PtrComp(MixBuffer) + (SpanAA.x - MinX)
                                * SizeOf(TAggColor));
                              ClrSpan := ColorSpan;

                              StyleHandler.GenerateSpan(ClrSpan, SpanAA.x, ScanLineAA.y, Len, Style);

                              SourceCovers := PCover(SpanAA.Covers);
                              DestCovers :=
                                PCover(PtrComp(CoverBuffer) + (SpanAA.x - MinX) *
                                SizeOf(TCover));

                              repeat
                                Cover := SourceCovers^;

                                if DestCovers^ + Cover > CAggCoverFull then
                                    Cover := CAggCoverFull - DestCovers^;

                                if Cover <> 0 then
                                  begin
                                    Colors.Add(ClrSpan, Cover);

                                    DestCovers^ := DestCovers^ + Cover;
                                  end;

                                inc(PtrComp(ClrSpan), SizeOf(TAggColor));
                                inc(PtrComp(Colors), SizeOf(TAggColor));
                                inc(PtrComp(SourceCovers), SizeOf(TCover));
                                inc(PtrComp(DestCovers), SizeOf(TCover));
                                dec(Len);

                              until Len = 0;

                              dec(NumSpans);

                              if NumSpans = 0 then
                                  Break;

                              // Inc(PtrComp(SpanAA), SsAntiAlias);
                              SpanAA.IncOperator;

                            until False;

                          SpanAA.Free;
                        end;

                      inc(i);
                    end;

                  Ren.BlendColorHSpan(ScanLineStart, Sl_y, ScanLineLen,
                    PAggColor(PtrComp(MixBuffer) + (ScanLineStart - MinX) * SizeOf(TAggColor)
                    ), 0, CAggCoverFull);

                end; // if ScanLineLen <> 0

            end; // if NumStyles = 1 ... else

          NumStyles := Ras.SweepStyles;

        end; // while NumStyles > 0

    end; // if ras.RewindScanLines
end;

end. 
 
 
 
