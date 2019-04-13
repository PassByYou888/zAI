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
unit AggScanlineStorageBin;

{$DEFINE FPC_DELPHI_MODE}
{$INCLUDE zDefine.inc}
interface
uses
  AggBasics,
  AggArray,
  AggScanline,
  AggRendererScanLine,
  AggRenderScanlines,
  AggRasterizerScanLine;

type
  PAggSpanData = ^TAggSpanData;

  TAggSpanData = record
    x, Len: Int32;
  end;

  PAggScanLineData = ^TAggScanLineData;

  TAggScanLineData = record
    y: Integer;
    NumSpans, Start_Span: Cardinal;
  end;

  TAggScanLineStorageBin = class;

  TAggEmbeddedScanLineBin = class(TAggEmbeddedScanLine)
  private
    type
    TConstIterator = class(TAggCustomSpan)
    private
      FStorage: TAggScanLineStorageBin;
      FSpanIndex: Cardinal;
      FSpan: TAggSpanData;
    protected
      function GetX: Integer; override;
      function GetLength: Integer; override;
    public
      constructor Create(ScanLine: TAggEmbeddedScanLineBin);
      procedure IncOperator; override;
    end;
  private
    FStorage: TAggScanLineStorageBin;
    FScanLine: TAggScanLineData;

    FScanLineIndex: Cardinal;

  protected
    function GetY: Integer; override;
    function GetNumSpans: Cardinal; override;
  public
    constructor Create(Storage: TAggScanLineStorageBin);
    destructor Destroy; override;

    procedure Reset(MinX, MaxX: Integer); override;

    function GetBegin: TAggCustomSpan; override;

    procedure Setup(ScanLineIndex: Cardinal); override;
  end;

  TAggScanLineStorageBin = class(TAggCustomRendererScanLine)
  private
    FSpans, FScanLines: TAggPodDeque;

    FFakeSpan: TAggSpanData;
    FFakeScanLine: TAggScanLineData;

    FMin, FMax: TPointInteger;

    FCurrentScanLine: Cardinal;
  protected
    // Iterate ScanLines interface
    function GetMinX: Integer; virtual; // override;
    function GetMinY: Integer; virtual; // override;
    function GetMaxX: Integer; virtual; // override;
    function GetMaxY: Integer; virtual; // override;
  public
    constructor Create;
    destructor Destroy; override;

    // Renderer Interface
    procedure Prepare(u: Cardinal); override;
    procedure Render(ScanLine: TAggCustomScanLine); override;

    function RewindScanLines: Boolean; virtual;                                       // override;
    function SweepScanLine(ScanLine: TAggCustomScanLine): Boolean; overload; virtual; // override;

    // Specialization for embedded_ScanLine
    function SweepScanLine(ScanLine: TAggEmbeddedScanLine): Boolean; overload; virtual; // override;

    function ByteSize: Cardinal;
    procedure WriteInt32(Dst: PInt8u; val: Int32);
    procedure Serialize(Data: PInt8u);

    function ScanLineByIndex(i: Cardinal): PAggScanLineData;
    function SpanByIndex(i: Cardinal): PAggSpanData;
  public
    property MinimumX: Integer read GetMinX;
    property MinimumY: Integer read GetMinY;
    property MaximumX: Integer read GetMaxX;
    property MaximumY: Integer read GetMaxY;
  end;

  TAggEmbeddedScanLineA = class(TAggEmbeddedScanLine)
  private
    type
    TConstIterator = class(TAggCustomSpan)
    private
      FInternalData: PInt8u;
      FSpan: TAggSpanData;
      FDeltaX: Integer;
      function ReadInt32: Integer;
    protected
      function GetX: Integer; override;
      function GetLength: Integer; override;
    public
      constructor Create(ScanLine: TAggEmbeddedScanLineA);
      procedure IncOperator; override;
    end;
  private
    FInternalData: PInt8u;
    fy: Integer;

    FNumSpans: Cardinal;

    FDeltaX: Integer;
  protected
    function GetY: Integer; override;
    function GetNumSpans: Cardinal; override;
  public
    constructor Create;
    destructor Destroy; override;

    procedure Reset(MinX, MaxX: Integer); override;

    function GetBegin: TAggCustomSpan; override;

    function ReadInt32: Integer;
    procedure Init(PTR: PInt8u; dx, dy: Integer); override;
  end;

  TAggSerializedScanLinesAdaptorBin = class(TAggRasterizerScanLine)
  private
    FData, FEnd, FInternalData: PInt8u;

    FDelta, FMin, FMax: TPointInteger;
  protected
    function GetMinX: Integer; override;
    function GetMinY: Integer; override;
    function GetMaxX: Integer; override;
    function GetMaxY: Integer; override;
  public
    constructor Create; overload;
    constructor Create(Data: PInt8u; Size: Cardinal;
      dx, dy: Double); overload;

    procedure Init(Data: PInt8u; Size: Cardinal; dx, dy: Double);
    function ReadInt32: Integer;

    // Iterate ScanLines interface
    function RewindScanLines: Boolean; override;

    function SweepScanLine(ScanLine: TAggCustomScanLine): Boolean; override;

    // Specialization for embedded_ScanLine
    function SweepScanLine(ScanLine: TAggEmbeddedScanLine): Boolean; override;
  end;

implementation


{ TAggEmbeddedScanLineBin.TConstIterator }

constructor TAggEmbeddedScanLineBin.TConstIterator.Create(
  ScanLine: TAggEmbeddedScanLineBin);
begin
  inherited Create;
  FStorage := ScanLine.FStorage;
  FSpanIndex := ScanLine.FScanLine.Start_Span;

  FSpan := FStorage.SpanByIndex(FSpanIndex)^;
end;

function TAggEmbeddedScanLineBin.TConstIterator.GetLength: Integer;
begin
  Result := FSpan.Len;
end;

function TAggEmbeddedScanLineBin.TConstIterator.GetX: Integer;
begin
  Result := FSpan.x;
end;

procedure TAggEmbeddedScanLineBin.TConstIterator.IncOperator;
begin
  inc(FSpanIndex);

  FSpan := FStorage.SpanByIndex(FSpanIndex)^;
end;

{ TAggEmbeddedScanLineBin }

constructor TAggEmbeddedScanLineBin.Create;
begin
  FStorage := Storage;

  Setup(0);
end;

procedure TAggEmbeddedScanLineBin.Reset;
begin
end;

function TAggEmbeddedScanLineBin.GetY;
begin
  Result := FScanLine.y;
end;

function TAggEmbeddedScanLineBin.GetNumSpans;
begin
  Result := FScanLine.NumSpans;
end;

destructor TAggEmbeddedScanLineBin.Destroy;
begin
  inherited;
end;

function TAggEmbeddedScanLineBin.GetBegin;
begin
  // FResult := TAggConstIteratorBin.Create(@Self);
  // Result := FResult;
  Result := TConstIterator.Create(Self);
end;

procedure TAggEmbeddedScanLineBin.Setup;
begin
  FScanLineIndex := ScanLineIndex;
  FScanLine := FStorage.ScanLineByIndex(FScanLineIndex)^;
end;

{ TAggScanLineStorageBin }

constructor TAggScanLineStorageBin.Create;
begin
  FSpans := TAggPodDeque.Create(256 - 2, SizeOf(TAggSpanData), 10); // Block increment size
  FScanLines := TAggPodDeque.Create(SizeOf(TAggScanLineData), 8);

  FMin.x := $7FFFFFFF;
  FMin.y := $7FFFFFFF;
  FMax.x := -$7FFFFFFF;
  FMax.y := -$7FFFFFFF;

  FCurrentScanLine := 0;
  FFakeScanLine.y := 0;

  FFakeScanLine.NumSpans := 0;
  FFakeScanLine.Start_Span := 0;

  FFakeSpan.x := 0;
  FFakeSpan.Len := 0;
end;

destructor TAggScanLineStorageBin.Destroy;
begin
  FSpans.Free;
  FScanLines.Free;
  inherited;
end;

procedure TAggScanLineStorageBin.Prepare(u: Cardinal);
begin
  FScanLines.RemoveAll;
  FSpans.RemoveAll;

  FMin.x := $7FFFFFFF;
  FMin.y := $7FFFFFFF;
  FMax.x := -$7FFFFFFF;
  FMax.y := -$7FFFFFFF;

  FCurrentScanLine := 0;
end;

procedure TAggScanLineStorageBin.Render(ScanLine: TAggCustomScanLine);
var
  y, x1, x2: Integer;

  ScanLineData: TAggScanLineData;
  NumSpans: Cardinal;

  Span: TAggCustomSpan;
  sp: TAggSpanData;
begin
  y := ScanLine.y;

  if y < FMin.y then
      FMin.y := y;

  if y > FMax.y then
      FMax.y := y;

  ScanLineData.y := y;
  ScanLineData.NumSpans := ScanLine.NumSpans;
  ScanLineData.Start_Span := FSpans.Size;

  NumSpans := ScanLineData.NumSpans;

  Span := ScanLine.GetBegin;

  repeat
    sp.x := Span.x;
    sp.Len := Span.Len;

    FSpans.Add(@sp);

    x1 := sp.x;
    x2 := sp.x + sp.Len - 1;

    if x1 < FMin.x then
        FMin.x := x1;

    if x2 > FMax.x then
        FMax.x := x2;

    dec(NumSpans);

    if NumSpans = 0 then
        Break;

    Span.IncOperator;
  until False;

  Span.Free;

  FScanLines.Add(@ScanLineData);
end;

function TAggScanLineStorageBin.GetMinX: Integer;
begin
  Result := FMin.x;
end;

function TAggScanLineStorageBin.GetMinY: Integer;
begin
  Result := FMin.y;
end;

function TAggScanLineStorageBin.GetMaxX: Integer;
begin
  Result := FMax.x;
end;

function TAggScanLineStorageBin.GetMaxY: Integer;
begin
  Result := FMax.y;
end;

function TAggScanLineStorageBin.RewindScanLines: Boolean;
begin
  FCurrentScanLine := 0;

  Result := FScanLines.Size > 0;
end;

function TAggScanLineStorageBin.SweepScanLine(ScanLine: TAggCustomScanLine): Boolean;
var
  ScanLineData: PAggScanLineData;
  NumSpans, SpanIndex: Cardinal;
  sp: PAggSpanData;
begin
  ScanLine.ResetSpans;

  repeat
    if FCurrentScanLine >= FScanLines.Size then
      begin
        Result := False;

        Exit;
      end;

    ScanLineData := FScanLines[FCurrentScanLine];

    NumSpans := ScanLineData.NumSpans;
    SpanIndex := ScanLineData.Start_Span;

    repeat
      sp := FSpans[SpanIndex];

      inc(SpanIndex);

      ScanLine.AddSpan(sp.x, sp.Len, CAggCoverFull);

      dec(NumSpans);

    until NumSpans = 0;

    inc(FCurrentScanLine);

    if ScanLine.NumSpans <> 0 then
      begin
        ScanLine.Finalize(ScanLineData.y);

        Break;
      end;

  until False;

  Result := True;
end;

function TAggScanLineStorageBin.SweepScanLine(ScanLine: TAggEmbeddedScanLine): Boolean;
begin
  repeat
    if FCurrentScanLine >= FScanLines.Size then
      begin
        Result := False;

        Exit;
      end;

    ScanLine.Setup(FCurrentScanLine);

    inc(FCurrentScanLine);

  until ScanLine.NumSpans <> 0;

  Result := True;
end;

function TAggScanLineStorageBin.ByteSize: Cardinal;
var
  i, Size: Cardinal;
begin
  Size := SizeOf(Int32) * 4; // MinX, min_y, MaxX, max_y

  i := 0;

  while i < FScanLines.Size do
    begin
      Size := Size + SizeOf(Int32) * 2 + // Y, NumSpans
        Cardinal(PAggScanLineData(FScanLines[i]).NumSpans) *
        SizeOf(Int32) * 2; // X, Span_len

      inc(i);
    end;

  Result := Size;
end;

procedure TAggScanLineStorageBin.WriteInt32(Dst: PInt8u; val: Int32);
begin
  PInt8u(Dst)^ := TInt32Int8uAccess(val).values[0];
  PInt8u(PtrComp(Dst) + SizeOf(Int8u))^ := TInt32Int8uAccess(val).values[1];
  PInt8u(PtrComp(Dst) + 2 * SizeOf(Int8u))^ := TInt32Int8uAccess(val).values[2];
  PInt8u(PtrComp(Dst) + 3 * SizeOf(Int8u))^ := TInt32Int8uAccess(val).values[3];
end;

procedure TAggScanLineStorageBin.Serialize(Data: PInt8u);
var
  i, NumSpans, SpanIndex: Cardinal;
  ScanLineData: PAggScanLineData;
  sp: PAggSpanData;
begin
  WriteInt32(Data, GetMinX); // MinX
  inc(PtrComp(Data), SizeOf(Int32));

  WriteInt32(Data, GetMinY); // min_y
  inc(PtrComp(Data), SizeOf(Int32));

  WriteInt32(Data, GetMaxX); // MaxX
  inc(PtrComp(Data), SizeOf(Int32));

  WriteInt32(Data, GetMaxY); // max_y
  inc(PtrComp(Data), SizeOf(Int32));

  i := 0;

  while i < FScanLines.Size do
    begin
      ScanLineData := FScanLines[i];

      WriteInt32(Data, ScanLineData.y); // Y
      inc(PtrComp(Data), SizeOf(Int32));

      WriteInt32(Data, ScanLineData.NumSpans); // NumSpans
      inc(PtrComp(Data), SizeOf(Int32));

      NumSpans := ScanLineData.NumSpans;
      SpanIndex := ScanLineData.Start_Span;

      repeat
        sp := FSpans[SpanIndex];

        inc(SpanIndex);

        WriteInt32(Data, sp.x); // X
        inc(PtrComp(Data), SizeOf(Int32));

        WriteInt32(Data, sp.Len); // len
        inc(PtrComp(Data), SizeOf(Int32));

        dec(NumSpans);

      until NumSpans = 0;

      inc(i);
    end;
end;

function TAggScanLineStorageBin.ScanLineByIndex(i: Cardinal): PAggScanLineData;
begin
  if i < FScanLines.Size then
      Result := FScanLines[i]
  else
      Result := @FFakeScanLine;
end;

function TAggScanLineStorageBin.SpanByIndex(i: Cardinal): PAggSpanData;
begin
  if i < FSpans.Size then
      Result := FSpans[i]
  else
      Result := @FFakeSpan;
end;

{ TAggEmbeddedScanLineA.TConstIterator }

constructor TAggEmbeddedScanLineA.TConstIterator.Create(
  ScanLine: TAggEmbeddedScanLineA);
begin
  inherited Create;
  FInternalData := ScanLine.FInternalData;
  FDeltaX := ScanLine.FDeltaX;

  FSpan.x := ReadInt32 + FDeltaX;
  FSpan.Len := ReadInt32;
end;

function TAggEmbeddedScanLineA.TConstIterator.GetLength: Integer;
begin
  Result := FSpan.Len;
end;

function TAggEmbeddedScanLineA.TConstIterator.GetX: Integer;
begin
  Result := FSpan.x;
end;

procedure TAggEmbeddedScanLineA.TConstIterator.IncOperator;
begin
  FSpan.x := ReadInt32 + FDeltaX;
  FSpan.Len := ReadInt32;
end;

function TAggEmbeddedScanLineA.TConstIterator.ReadInt32: Integer;
begin
  TInt32Int8uAccess(Result).values[0] := FInternalData^;
  inc(PtrComp(FInternalData), SizeOf(Int8u));
  TInt32Int8uAccess(Result).values[1] := FInternalData^;
  inc(PtrComp(FInternalData), SizeOf(Int8u));
  TInt32Int8uAccess(Result).values[2] := FInternalData^;
  inc(PtrComp(FInternalData), SizeOf(Int8u));
  TInt32Int8uAccess(Result).values[3] := FInternalData^;
  inc(PtrComp(FInternalData), SizeOf(Int8u));
end;

{ TAggEmbeddedScanLineA }

constructor TAggEmbeddedScanLineA.Create;
begin
  FInternalData := nil;
  fy := 0;

  FNumSpans := 0;
end;

procedure TAggEmbeddedScanLineA.Reset(MinX, MaxX: Integer);
begin
end;

function TAggEmbeddedScanLineA.GetY: Integer;
begin
  Result := fy;
end;

function TAggEmbeddedScanLineA.GetNumSpans: Cardinal;
begin
  Result := FNumSpans;
end;

destructor TAggEmbeddedScanLineA.Destroy;
begin
  inherited;
end;

function TAggEmbeddedScanLineA.GetBegin;
begin
  Result := TConstIterator.Create(Self);
end;

function TAggEmbeddedScanLineA.ReadInt32: Integer;
begin
  TInt32Int8uAccess(Result).values[0] := FInternalData^;
  inc(PtrComp(FInternalData), SizeOf(Int8u));
  TInt32Int8uAccess(Result).values[1] := FInternalData^;
  inc(PtrComp(FInternalData), SizeOf(Int8u));
  TInt32Int8uAccess(Result).values[2] := FInternalData^;
  inc(PtrComp(FInternalData), SizeOf(Int8u));
  TInt32Int8uAccess(Result).values[3] := FInternalData^;
  inc(PtrComp(FInternalData), SizeOf(Int8u));
end;

procedure TAggEmbeddedScanLineA.Init(PTR: PInt8u; dx, dy: Integer);
begin
  FInternalData := PTR;
  fy := ReadInt32 + dy;
  FNumSpans := Cardinal(ReadInt32);
  FDeltaX := dx;
end;

{ TAggSerializedScanLinesAdaptorBin }

constructor TAggSerializedScanLinesAdaptorBin.Create;
begin
  FData := nil;
  FEnd := nil;
  FInternalData := nil;

  FDelta.x := 0;
  FDelta.y := 0;

  FMin.x := $7FFFFFFF;
  FMin.y := $7FFFFFFF;
  FMax.x := -$7FFFFFFF;
  FMax.y := -$7FFFFFFF;
end;

constructor TAggSerializedScanLinesAdaptorBin.Create(Data: PInt8u; Size: Cardinal;
  dx, dy: Double);
begin
  FData := Data;
  FEnd := PInt8u(PtrComp(Data) + Size);
  FInternalData := Data;

  FDelta.x := Trunc(dx + 0.5);
  FDelta.y := Trunc(dy + 0.5);

  FMin.x := $7FFFFFFF;
  FMin.y := $7FFFFFFF;
  FMax.x := -$7FFFFFFF;
  FMax.y := -$7FFFFFFF;
end;

procedure TAggSerializedScanLinesAdaptorBin.Init(Data: PInt8u; Size: Cardinal;
  dx, dy: Double);
begin
  FData := Data;
  FEnd := PInt8u(PtrComp(Data) + Size);
  FInternalData := Data;

  FDelta.x := Trunc(dx + 0.5);
  FDelta.y := Trunc(dy + 0.5);

  FMin.x := $7FFFFFFF;
  FMin.y := $7FFFFFFF;
  FMax.x := -$7FFFFFFF;
  FMax.y := -$7FFFFFFF;
end;

function TAggSerializedScanLinesAdaptorBin.ReadInt32: Integer;
begin
  Result := 0;

  TInt32Int8uAccess(Result).values[0] := FInternalData^;
  inc(PtrComp(FInternalData), SizeOf(Int8u));
  TInt32Int8uAccess(Result).values[1] := FInternalData^;
  inc(PtrComp(FInternalData), SizeOf(Int8u));
  TInt32Int8uAccess(Result).values[2] := FInternalData^;
  inc(PtrComp(FInternalData), SizeOf(Int8u));
  TInt32Int8uAccess(Result).values[3] := FInternalData^;
  inc(PtrComp(FInternalData), SizeOf(Int8u));
end;

function TAggSerializedScanLinesAdaptorBin.RewindScanLines: Boolean;
begin
  FInternalData := FData;

  if PtrComp(FInternalData) < PtrComp(FEnd) then
    begin
      FMin.x := ReadInt32 + FDelta.x;
      FMin.y := ReadInt32 + FDelta.y;
      FMax.x := ReadInt32 + FDelta.x;
      FMax.y := ReadInt32 + FDelta.y;

      Result := True;
    end
  else
      Result := False;
end;

function TAggSerializedScanLinesAdaptorBin.GetMinX: Integer;
begin
  Result := FMin.x;
end;

function TAggSerializedScanLinesAdaptorBin.GetMinY: Integer;
begin
  Result := FMin.y;
end;

function TAggSerializedScanLinesAdaptorBin.GetMaxX: Integer;
begin
  Result := FMax.x;
end;

function TAggSerializedScanLinesAdaptorBin.GetMaxY: Integer;
begin
  Result := FMax.y;
end;

function TAggSerializedScanLinesAdaptorBin.SweepScanLine(
  ScanLine: TAggCustomScanLine): Boolean;
var
  y, x, Len: Integer;
  NumSpans: Cardinal;
begin
  ScanLine.ResetSpans;

  repeat
    if PtrComp(FInternalData) >= PtrComp(FEnd) then
      begin
        Result := False;

        Exit;
      end;

    y := ReadInt32 + FDelta.y;
    NumSpans := ReadInt32;

    repeat
      x := ReadInt32 + FDelta.x;
      Len := ReadInt32;

      if Len < 0 then
          Len := -Len;

      ScanLine.AddSpan(x, Cardinal(Len), CAggCoverFull);

      inc(NumSpans);

    until NumSpans = 0;

    if ScanLine.NumSpans <> 0 then
      begin
        ScanLine.Finalize(y);

        Break;
      end;
  until False;

  Result := True;
end;

function TAggSerializedScanLinesAdaptorBin.SweepScanLine(
  ScanLine: TAggEmbeddedScanLine): Boolean;
var
  NumSpans: Integer;
begin
  repeat
    if PtrComp(FInternalData) >= PtrComp(FEnd) then
      begin
        Result := False;

        Exit;
      end;

    ScanLine.Init(FInternalData, FDelta.x, FDelta.y);

    // Jump to the next ScanLine
    ReadInt32; // Y

    NumSpans := ReadInt32; // NumSpans

    inc(PtrComp(FInternalData), NumSpans * SizeOf(Int32) * 2);
  until ScanLine.NumSpans <> 0;

  Result := True;
end;

end. 
 
 
