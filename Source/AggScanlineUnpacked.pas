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
unit AggScanlineUnpacked;

{$DEFINE FPC_DELPHI_MODE}
{$INCLUDE zDefine.inc}
interface
uses
  AggBasics,
  AggScanline,
  AggAlphaMaskUnpacked8;

type
  // Unpacked ScanLine container class
  //
  // This class is used to transfer data from a ScanLine Rasterizer
  // to the rendering buffer. It's organized very simple. The class stores
  // information of horizontal Spans to render it into a pixel-map buffer.
  // Each Span has staring X, length, and an array of bytes that determine the
  // cover-values for each pixel.
  // Before using this class you should know the minimal and maximal pixel
  // coordinates of your ScanLine. The protocol of using is:
  // 1. reset(MinX, MaxX)
  // 2. AddCell() / AddSpan() - accumulate ScanLine.
  // When forming one ScanLine the next X coordinate must be always greater
  // than the last stored one, i.e. it works only with ordered coordinates.
  // 3. Call finalize(y) and render the ScanLine.
  // 3. Call ResetSpans() to prepare for the new ScanLine.
  //
  // 4. Rendering:
  //
  // ScanLine provides an iterator class that allows you to extract
  // the Spans and the cover values for each pixel. Be aware that clipping
  // has not been done yet, so you should perform it yourself.
  // Use ScanLineU8::iterator to render Spans:
  // -------------------------------------------------------------------------
  //
  // int y = sl.y();                    // Y-coordinate of the ScanLine
  //
  // ************************************
  // ...Perform vertical clipping here...
  // ************************************
  //
  // ScanLineU8::const_iterator Span = sl.begin();
  //
  // Cardinal char* row = m_rbuf->row(y); // The the address of the beginning
  // // of the current row
  //
  // Cardinal NumSpans = sl.NumSpans(); // Number of Spans. It's guaranteed that
  // // NumSpans is always greater than 0.
  //
  // do
  // {
  // const ScanLineU8::TCover* covers =
  // Span->covers;                     // The array of the cover values
  //
  // int num_pix = Span->len;              // Number of pixels of the Span.
  // // Always greater than 0, still it's
  // // better to use "int" instead of
  // // "Cardinal" because it's more
  // // convenient for clipping
  // int x = Span->x;
  //
  // **************************************
  // ...Perform horizontal clipping here...
  // ...you have x, covers, and pix_count..
  // **************************************
  //
  // Cardinal char* dst = row + x;  // Calculate the start address of the row.
  // // In this case we assume a simple
  // // grayscale image 1-byte per pixel.
  // do
  // {
  // *dst++ = *covers++;        // Hypotetical rendering.
  // }
  // while(--num_pix);
  //
  // ++Span;
  // }
  // while(--NumSpans);  // NumSpans cannot be 0, so this loop is quite safe
  // ------------------------------------------------------------------------
  //
  // The question is: why should we accumulate the whole ScanLine when we
  // could render just separate Spans when they're ready?
  // That's because using the ScanLine is generally faster. When is consists
  // of more than one Span the conditions for the processor cash system
  // are better, because switching between two different areas of memory
  // (that can be very large) occurs less frequently.
  // ------------------------------------------------------------------------

  PAggSpanUnpacked8 = ^TAggSpanUnpacked8;

  TAggSpanUnpacked8 = record
    x, Len: Int16;
    Covers: PInt8u;
  end;

  TAggScanLineUnpacked8 = class(TAggCustomScanLine)
  private type
    TConstIterator = class(TAggCustomSpan)
    private
      FSpan: PAggSpanUnpacked8;
    protected
      function GetX: Integer; override;
      function GetLength: Integer; override;
    public
      constructor Create(aScanline: TAggScanLineUnpacked8);
      function Covers: PInt8u; override;
      procedure IncOperator; override;
    end;
  private
    FMinX: Integer;
    FMaxLength: Cardinal;
    FLastX, fy: Integer;

    FCovers: PInt8u;
    FSpans, FCurrentSpan: PAggSpanUnpacked8;
  protected
    function GetY: Integer; override;
    function GetNumSpans: Cardinal; override;
  public
    constructor Create; virtual;
    destructor Destroy; override;

    procedure Reset(MinX, MaxX: Integer); override;
    procedure ResetSpans; override;
    function GetBegin: TAggCustomSpan; override;

    procedure Finalize(y: Integer); override;
    procedure AddCell(x: Integer; Cover: Cardinal); override;
    procedure AddCells(x: Integer; Len: Cardinal; Covers: PInt8u); override;
    procedure AddSpan(x: Integer; Len, Cover: Cardinal); override;
  end;

  TAggScanLineUnpacked8AlphaMask = class(TAggScanLineUnpacked8)
  private
    FAlphaMask: TAggCustomAlphaMask;
  public
    constructor Create; overload; override;
    constructor Create(AlphaMask: TAggCustomAlphaMask); overload;

    procedure Finalize(y: Integer); override;
  end;

implementation

{ TAggScanLineUnpacked8.TConstIterator }

function TAggScanLineUnpacked8.TConstIterator.Covers: PInt8u;
begin
  Result := FSpan.Covers;
end;

constructor TAggScanLineUnpacked8.TConstIterator.Create(
  aScanline: TAggScanLineUnpacked8);
begin
  inherited Create;
  FSpan := PAggSpanUnpacked8(PtrComp(aScanline.FSpans) + SizeOf(TAggSpanUnpacked8));
end;

function TAggScanLineUnpacked8.TConstIterator.GetLength: Integer;
begin
  Result := FSpan.Len;
end;

function TAggScanLineUnpacked8.TConstIterator.GetX: Integer;
begin
  Result := FSpan.x;
end;

procedure TAggScanLineUnpacked8.TConstIterator.IncOperator;
begin
  inc(PtrComp(FSpan), SizeOf(TAggSpanUnpacked8));
end;

{ TAggScanLineUnpacked8 }

constructor TAggScanLineUnpacked8.Create;
begin
  FMinX := 0;
  FMaxLength := 0;
  FLastX := $7FFFFFF0;

  FCovers := nil;
  FSpans := nil;
  FCurrentSpan := nil;
  inherited;
end;

destructor TAggScanLineUnpacked8.Destroy;
begin
  AggFreeMem(Pointer(FSpans), FMaxLength * SizeOf(TAggSpanUnpacked8));
  AggFreeMem(Pointer(FCovers), FMaxLength * SizeOf(Int8u));
  inherited;
end;

procedure TAggScanLineUnpacked8.Reset(MinX, MaxX: Integer);
var
  MaxLength: Cardinal;
begin
  MaxLength := MaxX - MinX + 2;

  if MaxLength > FMaxLength then
    begin
      AggFreeMem(Pointer(FSpans), FMaxLength * SizeOf(TAggSpanUnpacked8));
      AggFreeMem(Pointer(FCovers), FMaxLength * SizeOf(Int8u));

      AggGetMem(Pointer(FCovers), MaxLength * SizeOf(Int8u));
      AggGetMem(Pointer(FSpans), MaxLength * SizeOf(TAggSpanUnpacked8));

      FMaxLength := MaxLength;
    end;

  FLastX := $7FFFFFF0;
  FMinX := MinX;
  FCurrentSpan := FSpans;
end;

procedure TAggScanLineUnpacked8.ResetSpans;
begin
  FLastX := $7FFFFFF0;
  FCurrentSpan := FSpans;
end;

procedure TAggScanLineUnpacked8.Finalize(y: Integer);
begin
  fy := y;
end;

procedure TAggScanLineUnpacked8.AddCell(x: Integer; Cover: Cardinal);
begin
  dec(x, FMinX);

  PInt8u(PtrComp(FCovers) + x * SizeOf(Int8u))^ := Int8u(Cover);

  if x = FLastX + 1 then
      inc(FCurrentSpan.Len)
  else
    begin
      inc(PtrComp(FCurrentSpan), SizeOf(TAggSpanUnpacked8));

      FCurrentSpan.x := Int16(x + FMinX);
      FCurrentSpan.Len := 1;

      FCurrentSpan.Covers := PInt8u(PtrComp(FCovers) + x * SizeOf(Int8u));
    end;

  FLastX := x;
end;

procedure TAggScanLineUnpacked8.AddCells(x: Integer; Len: Cardinal;
  Covers: PInt8u);
begin
  dec(x, FMinX);
  Move(Covers^, PInt8u(PtrComp(FCovers) + x)^, Len * SizeOf(Int8u));

  if x = FLastX + 1 then
      inc(FCurrentSpan.Len, Int16(Len))
  else
    begin
      inc(PtrComp(FCurrentSpan), SizeOf(TAggSpanUnpacked8));

      FCurrentSpan.x := Int16(x + FMinX);
      FCurrentSpan.Len := Int16(Len);
      FCurrentSpan.Covers := PInt8u(PtrComp(FCovers) + x * SizeOf(Int8u));
    end;

  FLastX := x + Len - 1;
end;

procedure TAggScanLineUnpacked8.AddSpan(x: Integer; Len, Cover: Cardinal);
begin
  dec(x, FMinX);

  FillChar(PInt8u(PtrComp(FCovers) + x * SizeOf(Int8u))^, Len, Cover);

  if x = FLastX + 1 then
      inc(FCurrentSpan.Len, Int16(Len))
  else
    begin
      inc(PtrComp(FCurrentSpan), SizeOf(TAggSpanUnpacked8));

      FCurrentSpan.x := Int16(x + FMinX);
      FCurrentSpan.Len := Int16(Len);
      FCurrentSpan.Covers := PInt8u(PtrComp(FCovers) + x * SizeOf(Int8u));
    end;

  FLastX := x + Len - 1;
end;

function TAggScanLineUnpacked8.GetY: Integer;
begin
  Result := fy;
end;

function TAggScanLineUnpacked8.GetNumSpans: Cardinal;
begin
  Result := (PtrComp(FCurrentSpan) - PtrComp(FSpans)) div SizeOf(TAggSpanUnpacked8);
end;

function TAggScanLineUnpacked8.GetBegin: TAggCustomSpan;
begin
  Result := TConstIterator.Create(Self);
end;

constructor TAggScanLineUnpacked8AlphaMask.Create;
begin
  inherited Create;

  FAlphaMask := nil;
end;

constructor TAggScanLineUnpacked8AlphaMask.Create(AlphaMask: TAggCustomAlphaMask);
begin
  inherited Create;

  FAlphaMask := AlphaMask;
end;

procedure TAggScanLineUnpacked8AlphaMask.Finalize(y: Integer);
var
  Span: TAggCustomSpan;
  Count: Cardinal;
begin
  inherited Finalize(y);

  if FAlphaMask <> nil then
    begin
      Span := GetBegin;
      Count := NumSpans;

      repeat
        FAlphaMask.CombineHSpan(Span.x, y, Span.Covers, Span.Len);

        Span.IncOperator;
        dec(Count);

      until Count = 0;

      Span.Free;
    end;
end;

end. 
 
 
