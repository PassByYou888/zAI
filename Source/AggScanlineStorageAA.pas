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
unit AggScanlineStorageAA;

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
  PAggExtraSpan = ^TAggExtraSpan;

  TAggExtraSpan = record
    Len: Cardinal;
    PTR: Pointer;
  end;

  TAggScanLineCellStorage = class
  private
    FCells, FExtraStorage: TAggPodDeque;
    function ArrayOperator(index: Integer): Pointer;
  public
    constructor Create(EntrySize: Cardinal); overload;
    constructor Create(v: TAggScanLineCellStorage); overload;
    destructor Destroy; override;

    procedure RemoveAll;
    function AddCells(Cells: Pointer; NumCells: Cardinal): Integer;

    function AssignOperator(v: TAggScanLineCellStorage): TAggScanLineCellStorage;

    procedure CopyExtraStorage(v: TAggScanLineCellStorage);

    property CellPointer[index: Integer]: Pointer read ArrayOperator; default;
  end;

  PAggSpanDataSS = ^TAggSpanDataSS;

  TAggSpanDataSS = record
    x, Len: Int32;     // If negative, it's a solid Span, covers is valid
    CoversID: Integer; // The index of the cells in the TAggScanLineCellStorage
  end;

  PAggScanLineDataSS = ^TAggScanLineDataSS;

  TAggScanLineDataSS = record
    y: Integer;
    NumSpans, StartSpan: Cardinal;
  end;

  PAggSpanSS = ^TAggSpanSS;

  TAggSpanSS = record
    x, Len: Int32; // If negative, it's a solid Span, covers is valid

    Covers: Pointer;
  end;

  TAggCustomScanLineStorageAA = class;

  TAggEmbeddedScanLineSS = class(TAggEmbeddedScanLine)
  private
    type
    TConstIterator = class(TAggCustomSpan)
    private
      FStorage: TAggCustomScanLineStorageAA;
      FSpanIdx: Cardinal;
      FSpan: TAggSpanSS;
      procedure Init;
    protected
      function GetX: Integer; override;
      function GetLength: Integer; override;
    public
      constructor Create(ScanLine: TAggEmbeddedScanLineSS);
      function Covers: PInt8u; override;
      procedure IncOperator; override;
    end;
  private
    FStorage: TAggCustomScanLineStorageAA;
    FScanLine: TAggScanLineDataSS;
    FScanLineIndex: Cardinal;
  public
    constructor Create(Storage: TAggCustomScanLineStorageAA);
    destructor Destroy; override;

    procedure Reset(MinX, MaxX: Integer); override;

    function GetY: Integer; override;
    function GetNumSpans: Cardinal; override;
    function GetBegin: TAggCustomSpan; override;

    procedure Setup(ScanLineIndex: Cardinal); override;
  end;

  TAggCustomScanLineStorageAA = class(TAggCustomRendererScanLine)
  private
    FCovers: TAggScanLineCellStorage;
    FSpans, FScanLines: TAggPodDeque;

    FFakeSpan: TAggSpanDataSS;
    FFakeScanLine: TAggScanLineDataSS;

    FMin, FMax: TPointInteger;

    FCurrentScanLine: Cardinal;
  public
    constructor Create; virtual;
    destructor Destroy; override;

    // Renderer Interface
    procedure Prepare(u: Cardinal); override;
    procedure Render(SL: TAggCustomScanLine); override;

    // Iterate ScanLines interface
    function GetMinX: Integer; virtual;
    function GetMinY: Integer; virtual;
    function GetMaxX: Integer; virtual;
    function GetMaxY: Integer; virtual;

    function RewindScanLines: Boolean; virtual;
    function SweepScanLine(SL: TAggCustomScanLine): Boolean; overload; virtual;

    // Specialization for embedded_ScanLine
    function SweepScanLine(SL: TAggEmbeddedScanLine): Boolean; overload; virtual;

    function ByteSize: Cardinal; virtual;
    procedure WriteInt32(Dst: PInt8u; val: Int32);
    procedure Serialize(Data: PInt8u); virtual;

    function ScanLineByIndex(i: Cardinal): PAggScanLineDataSS;
    function SpanByIndex(i: Cardinal): PAggSpanDataSS;
    function CoversByIndex(i: Integer): Pointer;
  public
    property MinimumX: Integer read GetMinX;
    property MinimumY: Integer read GetMinY;
    property MaximumX: Integer read GetMaxX;
    property MaximumY: Integer read GetMaxY;
  end;

  TAggScanLineStorageAA8 = class(TAggCustomScanLineStorageAA)
  public
    constructor Create; override;
  end;

  TAggScanLineStorageAA16 = class(TAggCustomScanLineStorageAA)
  public
    constructor Create; override;

    function SweepScanLine(SL: TAggCustomScanLine): Boolean; override;

    function ByteSize: Cardinal; override;
    procedure Serialize(Data: PInt8u); override;
  end;

  TAggScanLineStorageAA32 = class(TAggCustomScanLineStorageAA)
  public
    constructor Create; override;

    function SweepScanLine(SL: TAggCustomScanLine): Boolean; override;

    function ByteSize: Cardinal; override;
    procedure Serialize(Data: PInt8u); override;
  end;

  // TAggEmbeddedScanLineSA = class(TAggCustomScanLine)
  TAggEmbeddedScanLineSA = class(TAggEmbeddedScanLine)
  private type
    TConstIterator = class(TAggCustomSpan)
    private
      FPtr: PInt8u;
      FSpan: TAggSpanSS;

      FDeltaX: Integer;
      FSize: Cardinal;
      function GetSize: Cardinal;
      procedure Init;
    protected
      function GetX: Integer; override;
      function GetLength: Integer; override;
      function ReadInt32: Integer;
    public
      constructor Create(aScanline: TAggEmbeddedScanLineSA; ASize: Cardinal);
      function Covers: PInt8u; override;
      procedure IncOperator; override;

      property Size: Cardinal read GetSize;
    end;
  private
    FPtr: PInt8u;
    fy: Integer;

    FNumSpans: Cardinal;

    FDeltaX: Integer;
    FSize: Cardinal;

    function GetSize: Cardinal;
  protected
    function GetY: Integer; override;
    function GetNumSpans: Cardinal; override;
  public
    constructor Create(Size: Cardinal);
    destructor Destroy; override;

    procedure Reset(MinX, MaxX: Integer); override;

    function GetBegin: TAggCustomSpan; override;
    procedure Init(PTR: PInt8u; dx, dy: Integer); override;

    function ReadInt32: Integer;

    property Size: Cardinal read GetSize;
  end;

  TAggSerializedScanLinesAdaptorAA = class(TAggRasterizerScanLine)
  private
    FData, FEnd, FPtr: PInt8u;

    FDelta, FMin, FMax: TPointInteger;

    FSize: Cardinal;

    function GetSize: Cardinal;
  protected
    function GetMinX: Integer; override;
    function GetMinY: Integer; override;
    function GetMaxX: Integer; override;
    function GetMaxY: Integer; override;
  public
    constructor Create(SZ: Cardinal); overload;
    constructor Create(SZ: Cardinal; Data: PInt8u; ASize: Cardinal;
      dx, dy: Double); overload;

    procedure Init(Data: PInt8u; ASize: Cardinal; dx, dy: Double);

    function ReadInt32: Integer;
    function ReadInt32u: Cardinal;

    // Iterate ScanLines interface
    function RewindScanLines: Boolean; override;

    function SweepScanLine(SL: TAggCustomScanLine): Boolean; override;

    // Specialization for embedded_ScanLine
    function SweepScanLine(SL: TAggEmbeddedScanLine): Boolean; override;

    property Size: Cardinal read GetSize;
  end;

  TAggSerializedScanLinesAdaptorAA8 = class(TAggSerializedScanLinesAdaptorAA)
  public
    constructor Create; overload;
    constructor Create(Data: PInt8u; ASize: Cardinal; dx, dy: Double); overload;
  end;

  TAggSerializedScanLinesAdaptorAA16 = class(TAggSerializedScanLinesAdaptorAA)
  public
    constructor Create; overload;
    constructor Create(Data: PInt8u; ASize: Cardinal; dx, dy: Double); overload;
  end;

  TAggSerializedScanLinesAdaptorAA32 = class(TAggSerializedScanLinesAdaptorAA)
  public
    constructor Create; overload;
    constructor Create(Data: PInt8u; ASize: Cardinal; dx, dy: Double); overload;
  end;

implementation

{ TAggScanLineCellStorage }

constructor TAggScanLineCellStorage.Create(EntrySize: Cardinal);
begin
  FCells := TAggPodDeque.Create(128 - 2, EntrySize, 12);
  FExtraStorage := TAggPodDeque.Create(SizeOf(TAggExtraSpan), 6);
end;

constructor TAggScanLineCellStorage.Create(v: TAggScanLineCellStorage);
begin
  FCells := TAggPodDeque.Create(v.FCells.EntrySize);
  FExtraStorage := TAggPodDeque.Create(SizeOf(TAggExtraSpan), 6);

  AssignOperator(v);
  CopyExtraStorage(v);
end;

destructor TAggScanLineCellStorage.Destroy;
begin
  RemoveAll;

  FCells.Free;
  FExtraStorage.Free;

  inherited;
end;

procedure TAggScanLineCellStorage.RemoveAll;
var
  i: Integer;
  s: PAggExtraSpan;
begin
  i := FExtraStorage.Size;
  dec(i);

  while i >= 0 do
    begin
      s := FExtraStorage[i];

      AggFreeMem(s.PTR, s.Len * FCells.EntrySize);

      dec(i);
    end;

  FExtraStorage.RemoveAll;
  FCells.RemoveAll;
end;

function TAggScanLineCellStorage.AddCells;
var
  index: Integer;
  PTR: Pointer;

  s: TAggExtraSpan;

begin
  index := FCells.AllocateContinuousBlock(NumCells);

  if index >= 0 then
    begin
      PTR := FCells[index];

      Move(Cells^, PTR^, FCells.EntrySize * NumCells);

      Result := index;

      Exit;
    end;

  s.Len := NumCells;

  AggGetMem(s.PTR, s.Len * FCells.EntrySize);

  Move(Cells^, s.PTR^, s.Len * FCells.EntrySize);

  FExtraStorage.Add(@s);

  Result := -Integer(FExtraStorage.Size);
end;

function TAggScanLineCellStorage.AssignOperator;
begin
  RemoveAll;

  FCells.AssignOperator(@v.FCells);
  CopyExtraStorage(v);

  Result := @Self;
end;

function TAggScanLineCellStorage.ArrayOperator;
var
  i: Cardinal;

begin
  if index >= 0 then
    begin
      if index >= FCells.Size then
        begin
          Result := nil;

          Exit;
        end;

      Result := FCells[index];

      Exit;
    end;

  i := Cardinal(-index - 1);

  if i >= FExtraStorage.Size then
    begin
      Result := 0;

      Exit;
    end;

  Result := PAggExtraSpan(FExtraStorage[i]).PTR;
end;

procedure TAggScanLineCellStorage.CopyExtraStorage;
var
  i: Cardinal;

  Src: PAggExtraSpan;
  Dst: TAggExtraSpan;
begin
  i := 0;

  while i < v.FExtraStorage.Size do
    begin
      Src := v.FExtraStorage[i];

      Dst.Len := Src.Len;

      AggGetMem(Dst.PTR, Dst.Len * v.FCells.EntrySize);

      Move(Src.PTR^, Dst.PTR^, Dst.Len * v.FCells.EntrySize);

      FExtraStorage.Add(@Dst);

      inc(i);
    end;
end;

{ TAggEmbeddedScanLineSS.TConstIterator }

function TAggEmbeddedScanLineSS.TConstIterator.Covers: PInt8u;
begin
  Result := FSpan.Covers;
end;

constructor TAggEmbeddedScanLineSS.TConstIterator.Create;
begin
  inherited Create;

  FStorage := ScanLine.FStorage;
  FSpanIdx := ScanLine.FScanLine.StartSpan;
  Init;
end;

function TAggEmbeddedScanLineSS.TConstIterator.GetLength: Integer;
begin
  Result := FSpan.Len;
end;

function TAggEmbeddedScanLineSS.TConstIterator.GetX: Integer;
begin
  Result := FSpan.x;
end;

procedure TAggEmbeddedScanLineSS.TConstIterator.IncOperator;
begin
  inc(FSpanIdx);
  Init;
end;

procedure TAggEmbeddedScanLineSS.TConstIterator.Init;
var
  s: PAggSpanDataSS;
begin
  s := FStorage.SpanByIndex(FSpanIdx);

  FSpan.x := s.x;
  FSpan.Len := s.Len;
  FSpan.Covers := FStorage.CoversByIndex(s.CoversID);
end;

{ TAggEmbeddedScanLineSS }

constructor TAggEmbeddedScanLineSS.Create;
begin
  FStorage := Storage;

  Setup(0);
end;

procedure TAggEmbeddedScanLineSS.Reset;
begin
end;

function TAggEmbeddedScanLineSS.GetY;
begin
  Result := FScanLine.y;
end;

function TAggEmbeddedScanLineSS.GetNumSpans;
begin
  Result := FScanLine.NumSpans;
end;

destructor TAggEmbeddedScanLineSS.Destroy;
begin
  inherited;
end;

function TAggEmbeddedScanLineSS.GetBegin;
begin
  Result := TConstIterator.Create(Self);
end;

procedure TAggEmbeddedScanLineSS.Setup;
begin
  FScanLineIndex := ScanLineIndex;
  FScanLine := FStorage.ScanLineByIndex(FScanLineIndex)^;
end;

{ TAggCustomScanLineStorageAA }

constructor TAggCustomScanLineStorageAA.Create;
begin
  FSpans := TAggPodDeque.Create(256 - 2, SizeOf(TAggSpanDataSS), 10); // Block increment size
  FScanLines := TAggPodDeque.Create(SizeOf(TAggScanLineDataSS), 8);

  FMin.x := $7FFFFFFF;
  FMin.y := $7FFFFFFF;
  FMax.x := -$7FFFFFFF;
  FMax.y := -$7FFFFFFF;

  FCurrentScanLine := 0;

  FFakeScanLine.y := 0;
  FFakeScanLine.NumSpans := 0;
  FFakeScanLine.StartSpan := 0;

  FFakeSpan.x := 0;
  FFakeSpan.Len := 0;
  FFakeSpan.CoversID := 0;
end;

destructor TAggCustomScanLineStorageAA.Destroy;
begin
  FCovers.Free;
  FSpans.Free;
  FScanLines.Free;
  inherited;
end;

procedure TAggCustomScanLineStorageAA.Prepare;
begin
  FCovers.RemoveAll;
  FScanLines.RemoveAll;
  FSpans.RemoveAll;

  FMin.x := $7FFFFFFF;
  FMin.y := $7FFFFFFF;
  FMax.x := -$7FFFFFFF;
  FMax.y := -$7FFFFFFF;

  FCurrentScanLine := 0;
end;

procedure TAggCustomScanLineStorageAA.Render;
var
  ScanLineData: TAggScanLineDataSS;

  y, x1, x2, Len: Integer;

  NumSpans: Cardinal;
  Span: TAggCustomSpan;

  sp: TAggSpanDataSS;
begin
  y := SL.y;

  if y < FMin.y then
      FMin.y := y;

  if y > FMax.y then
      FMax.y := y;

  ScanLineData.y := y;
  ScanLineData.NumSpans := SL.NumSpans;
  ScanLineData.StartSpan := FSpans.Size;

  NumSpans := ScanLineData.NumSpans;

  Span := SL.GetBegin;

  repeat
    sp.x := Span.x;
    sp.Len := Span.Len;

    Len := Abs(sp.Len);

    sp.CoversID := FCovers.AddCells(Span.Covers, Cardinal(Len));

    FSpans.Add(@sp);

    x1 := sp.x;
    x2 := sp.x + Len - 1;

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

function TAggCustomScanLineStorageAA.GetMinX;
begin
  Result := FMin.x;
end;

function TAggCustomScanLineStorageAA.GetMinY;
begin
  Result := FMin.y;
end;

function TAggCustomScanLineStorageAA.GetMaxX;
begin
  Result := FMax.x;
end;

function TAggCustomScanLineStorageAA.GetMaxY;
begin
  Result := FMax.y;
end;

function TAggCustomScanLineStorageAA.RewindScanLines;
begin
  FCurrentScanLine := 0;

  Result := FScanLines.Size > 0;
end;

function TAggCustomScanLineStorageAA.SweepScanLine(
  SL: TAggCustomScanLine): Boolean;
var
  ScanLineData: PAggScanLineDataSS;
  NumSpans, SpanIndex: Cardinal;
  sp: PAggSpanDataSS;
  Covers: PInt8u;
begin
  SL.ResetSpans;

  repeat
    if FCurrentScanLine >= FScanLines.Size then
      begin
        Result := False;

        Exit;
      end;

    ScanLineData := FScanLines[FCurrentScanLine];

    NumSpans := ScanLineData.NumSpans;
    SpanIndex := ScanLineData.StartSpan;

    repeat
      sp := FSpans[SpanIndex];

      inc(SpanIndex);

      Covers := CoversByIndex(sp.CoversID);

      if sp.Len < 0 then
          SL.AddSpan(sp.x, Cardinal(-sp.Len), Covers^)
      else
          SL.AddCells(sp.x, sp.Len, Covers);

      dec(NumSpans);
    until NumSpans = 0;

    inc(FCurrentScanLine);

    if SL.NumSpans <> 0 then
      begin
        SL.Finalize(ScanLineData.y);

        Break;
      end;

  until False;

  Result := True;
end;

// function TAggCustomScanLineStorageAA.SweepScanLineEm;
function TAggCustomScanLineStorageAA.SweepScanLine(
  SL: TAggEmbeddedScanLine): Boolean;
begin
  repeat
    if FCurrentScanLine >= FScanLines.Size then
      begin
        Result := False;

        Exit;
      end;

    SL.Setup(FCurrentScanLine);

    inc(FCurrentScanLine);

  until SL.NumSpans <> 0;

  Result := True;
end;

function TAggCustomScanLineStorageAA.ByteSize;
var
  i, Size, NumSpans, SpanIndex: Cardinal;

  ScanLineData: PAggScanLineDataSS;

  sp: PAggSpanDataSS;

begin
  Size := SizeOf(Int32) * 4; // MinX, MinY, MaxX, MaxY

  i := 0;

  while i < FScanLines.Size do
    begin
      inc(Size, SizeOf(Int32) * 3); // ScanLine size in bytes, Y, NumSpans

      ScanLineData := FScanLines[i];

      NumSpans := ScanLineData.NumSpans;
      SpanIndex := ScanLineData.StartSpan;

      repeat
        sp := FSpans[SpanIndex];

        inc(SpanIndex);
        inc(Size, SizeOf(Int32) * 2); // X, Span Length

        if sp.Len < 0 then
            inc(Size, SizeOf(Int8u)) // cover
        else
            inc(Size, SizeOf(Int8u) * Cardinal(sp.Len)); // covers

        dec(NumSpans);
      until NumSpans = 0;

      inc(i);
    end;

  Result := Size;
end;

procedure TAggCustomScanLineStorageAA.WriteInt32;
begin
  PInt8u(Dst)^ := TInt32Int8uAccess(val).values[0];
  PInt8u(PtrComp(Dst) + SizeOf(Int8u))^ := TInt32Int8uAccess(val).values[1];
  PInt8u(PtrComp(Dst) + 2 * SizeOf(Int8u))^ := TInt32Int8uAccess(val).values[2];
  PInt8u(PtrComp(Dst) + 3 * SizeOf(Int8u))^ := TInt32Int8uAccess(val).values[3];
end;

procedure TAggCustomScanLineStorageAA.Serialize;
var
  i, NumSpans, SpanIndex: Cardinal;

  ScanLineThis: PAggScanLineDataSS;

  sp: PAggSpanDataSS;

  Covers: PInt8u;

  SizePointer: PInt8u;

begin
  WriteInt32(Data, GetMinX); // MinX
  inc(PtrComp(Data), SizeOf(Int32));

  WriteInt32(Data, GetMinY); // MinY
  inc(PtrComp(Data), SizeOf(Int32));

  WriteInt32(Data, GetMaxX); // MaxX
  inc(PtrComp(Data), SizeOf(Int32));

  WriteInt32(Data, GetMaxY); // MaxY
  inc(PtrComp(Data), SizeOf(Int32));

  i := 0;

  while i < FScanLines.Size do
    begin
      ScanLineThis := FScanLines[i];
      SizePointer := Data;

      inc(PtrComp(Data), SizeOf(Int32));
      // Reserve space for ScanLine size in bytes

      WriteInt32(Data, ScanLineThis.y); // Y
      inc(PtrComp(Data), SizeOf(Int32));

      WriteInt32(Data, ScanLineThis.NumSpans); // NumSpans
      inc(PtrComp(Data), SizeOf(Int32));

      NumSpans := ScanLineThis.NumSpans;
      SpanIndex := ScanLineThis.StartSpan;

      repeat
        sp := FSpans[SpanIndex];

        inc(SpanIndex);

        Covers := CoversByIndex(sp.CoversID);

        WriteInt32(Data, sp.x); // X
        inc(PtrComp(Data), SizeOf(Int32));

        WriteInt32(Data, sp.Len); // Span Length
        inc(PtrComp(Data), SizeOf(Int32));

        if sp.Len < 0 then
          begin
            Move(Covers^, Data^, SizeOf(Int8u));
            inc(PtrComp(Data), SizeOf(Int8u));
          end
        else
          begin
            Move(Covers^, Data^, Cardinal(sp.Len) * SizeOf(Int8u));
            inc(PtrComp(Data), SizeOf(Int8u) * Cardinal(sp.Len));
          end;

        dec(NumSpans);
      until NumSpans = 0;

      WriteInt32(SizePointer, PtrComp(Data) - PtrComp(SizePointer));

      inc(i);
    end;
end;

function TAggCustomScanLineStorageAA.ScanLineByIndex;
begin
  if i < FScanLines.Size then
      Result := FScanLines[i]
  else
      Result := @FFakeScanLine;
end;

function TAggCustomScanLineStorageAA.SpanByIndex;
begin
  if i < FSpans.Size then
      Result := FSpans[i]
  else
      Result := @FFakeSpan;
end;

function TAggCustomScanLineStorageAA.CoversByIndex;
begin
  Result := FCovers[i];
end;

{ TAggScanLineStorageAA8 }

constructor TAggScanLineStorageAA8.Create;
begin
  inherited;
  FCovers := TAggScanLineCellStorage.Create(SizeOf(Int8u));
end;

{ TAggScanLineStorageAA16 }

constructor TAggScanLineStorageAA16.Create;
begin
  inherited;
  FCovers := TAggScanLineCellStorage.Create(SizeOf(Int16u));
end;

function TAggScanLineStorageAA16.SweepScanLine(SL: TAggCustomScanLine): Boolean;
var
  ScanLineData: PAggScanLineDataSS;
  NumSpans, SpanIndex: Cardinal;
  sp: PAggSpanDataSS;
  Covers: PInt16u;
begin
  SL.ResetSpans;

  repeat
    if FCurrentScanLine >= FScanLines.Size then
      begin
        Result := False;

        Exit;
      end;

    ScanLineData := FScanLines[FCurrentScanLine];

    NumSpans := ScanLineData.NumSpans;
    SpanIndex := ScanLineData.StartSpan;

    repeat
      sp := FSpans[SpanIndex];

      inc(SpanIndex);

      Covers := CoversByIndex(sp.CoversID);

      if sp.Len < 0 then
          SL.AddSpan(sp.x, Cardinal(-sp.Len), Covers^)
      else
          SL.AddCells(sp.x, sp.Len, PInt8u(Covers));

      dec(NumSpans);
    until NumSpans = 0;

    inc(FCurrentScanLine);

    if SL.NumSpans <> 0 then
      begin
        SL.Finalize(ScanLineData.y);

        Break;
      end;

  until False;

  Result := True;
end;

function TAggScanLineStorageAA16.ByteSize;
var
  i, Size, NumSpans, SpanIndex: Cardinal;

  ScanLineData: PAggScanLineDataSS;

  sp: PAggSpanDataSS;

begin
  Size := SizeOf(Int32) * 4; // MinX, min_y, MaxX, max_y

  i := 0;

  while i < FScanLines.Size do
    begin
      inc(Size, SizeOf(Int32) * 3); // ScanLine size in bytes, Y, NumSpans

      ScanLineData := FScanLines[i];

      NumSpans := ScanLineData.NumSpans;
      SpanIndex := ScanLineData.StartSpan;

      repeat
        sp := FSpans[SpanIndex];

        inc(SpanIndex);
        inc(Size, SizeOf(Int32) * 2); // X, Span Length

        if sp.Len < 0 then
            inc(Size, SizeOf(Int16u)) // cover
        else
            inc(Size, SizeOf(Int16u) * Cardinal(sp.Len)); // covers

        dec(NumSpans);
      until NumSpans = 0;

      inc(i);
    end;

  Result := Size;
end;

procedure TAggScanLineStorageAA16.Serialize;
var
  i, NumSpans, SpanIndex: Cardinal;
  ScanLineThis: PAggScanLineDataSS;
  sp: PAggSpanDataSS;
  Covers: PInt16u;
  SizePointer: PInt8u;
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
      ScanLineThis := FScanLines[i];
      SizePointer := Data;

      inc(PtrComp(Data), SizeOf(Int32));
      // Reserve space for ScanLine size in bytes

      WriteInt32(Data, ScanLineThis.y); // Y
      inc(PtrComp(Data), SizeOf(Int32));

      WriteInt32(Data, ScanLineThis.NumSpans); // NumSpans
      inc(PtrComp(Data), SizeOf(Int32));

      NumSpans := ScanLineThis.NumSpans;
      SpanIndex := ScanLineThis.StartSpan;

      repeat
        sp := FSpans[SpanIndex];

        inc(SpanIndex);

        Covers := CoversByIndex(sp.CoversID);

        WriteInt32(Data, sp.x); // X
        inc(PtrComp(Data), SizeOf(Int32));

        WriteInt32(Data, sp.Len); // Span Length
        inc(PtrComp(Data), SizeOf(Int32));

        if sp.Len < 0 then
          begin
            Move(Covers^, Data^, SizeOf(Int16u));
            inc(PtrComp(Data), SizeOf(Int16u));
          end
        else
          begin
            Move(Covers^, Data^, Cardinal(sp.Len) * SizeOf(Int16u));
            inc(PtrComp(Data), SizeOf(Int16u) * Cardinal(sp.Len));
          end;

        dec(NumSpans);
      until NumSpans = 0;

      WriteInt32(SizePointer, PtrComp(Data) - PtrComp(SizePointer));

      inc(i);
    end;
end;

{ TAggScanLineStorageAA32 }

constructor TAggScanLineStorageAA32.Create;
begin
  inherited;
  FCovers := TAggScanLineCellStorage.Create(SizeOf(Int32u));
end;

function TAggScanLineStorageAA32.SweepScanLine(SL: TAggCustomScanLine): Boolean;
var
  ScanLineThis: PAggScanLineDataSS;

  NumSpans, SpanIndex: Cardinal;

  sp: PAggSpanDataSS;

  Covers: PInt32u;

begin
  SL.ResetSpans;

  repeat
    if FCurrentScanLine >= FScanLines.Size then
      begin
        Result := False;

        Exit;
      end;

    ScanLineThis := FScanLines[FCurrentScanLine];

    NumSpans := ScanLineThis.NumSpans;
    SpanIndex := ScanLineThis.StartSpan;

    repeat
      sp := FSpans[SpanIndex];

      inc(SpanIndex);

      Covers := CoversByIndex(sp.CoversID);

      if sp.Len < 0 then
          SL.AddSpan(sp.x, Cardinal(-sp.Len), Covers^)
      else
          SL.AddCells(sp.x, sp.Len, PInt8u(Covers));

      dec(NumSpans);
    until NumSpans = 0;

    inc(FCurrentScanLine);

    if SL.NumSpans <> 0 then
      begin
        SL.Finalize(ScanLineThis.y);

        Break;
      end;

  until False;

  Result := True;
end;

function TAggScanLineStorageAA32.ByteSize;
var
  i, Size, NumSpans, SpanIndex: Cardinal;
  ScanLineThis: PAggScanLineDataSS;
  sp: PAggSpanDataSS;
begin
  Size := SizeOf(Int32) * 4; // MinX, min_y, MaxX, max_y

  i := 0;

  while i < FScanLines.Size do
    begin
      inc(Size, SizeOf(Int32) * 3); // ScanLine size in bytes, Y, NumSpans

      ScanLineThis := FScanLines[i];

      NumSpans := ScanLineThis.NumSpans;
      SpanIndex := ScanLineThis.StartSpan;

      repeat
        sp := FSpans[SpanIndex];

        inc(SpanIndex);
        inc(Size, SizeOf(Int32) * 2); // X, Span Length

        if sp.Len < 0 then
            inc(Size, SizeOf(Int32u)) // cover
        else
            inc(Size, SizeOf(Int32u) * Cardinal(sp.Len)); // covers

        dec(NumSpans);
      until NumSpans = 0;

      inc(i);
    end;

  Result := Size;
end;

procedure TAggScanLineStorageAA32.Serialize;
var
  i, NumSpans, SpanIndex: Cardinal;

  ScanLineThis: PAggScanLineDataSS;

  sp: PAggSpanDataSS;

  Covers: PInt32u;

  SizePointer: PInt8u;

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
      ScanLineThis := FScanLines[i];
      SizePointer := Data;

      inc(PtrComp(Data), SizeOf(Int32));
      // Reserve space for ScanLine size in bytes

      WriteInt32(Data, ScanLineThis.y); // Y
      inc(PtrComp(Data), SizeOf(Int32));

      WriteInt32(Data, ScanLineThis.NumSpans); // NumSpans
      inc(PtrComp(Data), SizeOf(Int32));

      NumSpans := ScanLineThis.NumSpans;
      SpanIndex := ScanLineThis.StartSpan;

      repeat
        sp := FSpans[SpanIndex];

        inc(SpanIndex);

        Covers := CoversByIndex(sp.CoversID);

        WriteInt32(Data, sp.x); // X
        inc(PtrComp(Data), SizeOf(Int32));

        WriteInt32(Data, sp.Len); // Span Length
        inc(PtrComp(Data), SizeOf(Int32));

        if sp.Len < 0 then
          begin
            Move(Covers^, Data^, SizeOf(Int32u));
            inc(PtrComp(Data), SizeOf(Int32u));

          end
        else
          begin
            Move(Covers^, Data^, Cardinal(sp.Len) * SizeOf(Int32u));
            inc(PtrComp(Data), SizeOf(Int32u) * Cardinal(sp.Len));
          end;

        dec(NumSpans);

      until NumSpans = 0;

      WriteInt32(SizePointer, PtrComp(Data) - PtrComp(SizePointer));

      inc(i);
    end;
end;

{ TAggEmbeddedScanLineSA.TConstIterator }

function TAggEmbeddedScanLineSA.TConstIterator.Covers: PInt8u;
begin
  Result := FSpan.Covers;
end;

constructor TAggEmbeddedScanLineSA.TConstIterator.Create(
  aScanline: TAggEmbeddedScanLineSA; ASize: Cardinal);
begin
  inherited Create;
  FPtr := aScanline.FPtr;
  FDeltaX := aScanline.FDeltaX;
  FSize := ASize;

  Init;
end;

function TAggEmbeddedScanLineSA.TConstIterator.GetLength: Integer;
begin
  Result := FSpan.Len;
end;

function TAggEmbeddedScanLineSA.TConstIterator.GetSize: Cardinal;
begin
  Result := FSize;
end;

function TAggEmbeddedScanLineSA.TConstIterator.GetX: Integer;
begin
  Result := FSpan.x;
end;

procedure TAggEmbeddedScanLineSA.TConstIterator.IncOperator;
begin
  if FSpan.Len < 0 then
      inc(PtrComp(FPtr), FSize)
  else
      inc(PtrComp(FPtr), FSpan.Len * FSize);

  Init;
end;

procedure TAggEmbeddedScanLineSA.TConstIterator.Init;
begin
  FSpan.x := ReadInt32 + FDeltaX;
  FSpan.Len := ReadInt32;
  FSpan.Covers := FPtr;
end;

function TAggEmbeddedScanLineSA.TConstIterator.ReadInt32: Integer;
begin
  TInt32Int8uAccess(Result).values[0] := FPtr^;
  inc(PtrComp(FPtr), SizeOf(Int8u));
  TInt32Int8uAccess(Result).values[1] := FPtr^;
  inc(PtrComp(FPtr), SizeOf(Int8u));
  TInt32Int8uAccess(Result).values[2] := FPtr^;
  inc(PtrComp(FPtr), SizeOf(Int8u));
  TInt32Int8uAccess(Result).values[3] := FPtr^;
  inc(PtrComp(FPtr), SizeOf(Int8u));
end;

{ TAggEmbeddedScanLineSA }

constructor TAggEmbeddedScanLineSA.Create(Size: Cardinal);
begin
  FPtr := nil;
  fy := 0;
  FSize := Size;

  FNumSpans := 0;
end;

function TAggEmbeddedScanLineSA.GetSize: Cardinal;
begin
  Result := FSize;
end;

procedure TAggEmbeddedScanLineSA.Reset(MinX, MaxX: Integer);
begin
end;

function TAggEmbeddedScanLineSA.GetY: Integer;
begin
  Result := fy;
end;

function TAggEmbeddedScanLineSA.GetNumSpans: Cardinal;
begin
  Result := FNumSpans;
end;

destructor TAggEmbeddedScanLineSA.Destroy;
begin
  inherited;
end;

function TAggEmbeddedScanLineSA.GetBegin: TAggCustomSpan;
begin
  Result := TConstIterator.Create(Self, FSize);
end;

procedure TAggEmbeddedScanLineSA.Init(PTR: PInt8u; dx, dy: Integer);
begin
  FPtr := PTR;
  fy := ReadInt32 + dy;
  FNumSpans := Cardinal(ReadInt32);
  FDeltaX := dx;
end;

function TAggEmbeddedScanLineSA.ReadInt32: Integer;
begin
  TInt32Int8uAccess(Result).values[0] := FPtr^;
  inc(PtrComp(FPtr), SizeOf(Int8u));
  TInt32Int8uAccess(Result).values[1] := FPtr^;
  inc(PtrComp(FPtr), SizeOf(Int8u));
  TInt32Int8uAccess(Result).values[2] := FPtr^;
  inc(PtrComp(FPtr), SizeOf(Int8u));
  TInt32Int8uAccess(Result).values[3] := FPtr^;
  inc(PtrComp(FPtr), SizeOf(Int8u));
end;

{ TAggSerializedScanLinesAdaptorAA }

constructor TAggSerializedScanLinesAdaptorAA.Create(SZ: Cardinal);
begin
  FData := nil;
  FEnd := nil;
  FPtr := nil;

  FDelta.x := 0;
  FDelta.y := 0;

  FMin.x := $7FFFFFFF;
  FMin.y := $7FFFFFFF;
  FMax.x := -$7FFFFFFF;
  FMax.y := -$7FFFFFFF;

  FSize := SZ;
end;

constructor TAggSerializedScanLinesAdaptorAA.Create(SZ: Cardinal;
  Data: PInt8u; ASize: Cardinal; dx, dy: Double);
begin
  FData := Data;
  FEnd := PInt8u(PtrComp(Data) + ASize);
  FPtr := Data;

  FDelta.x := Trunc(dx + 0.5);
  FDelta.y := Trunc(dy + 0.5);

  FMin.x := $7FFFFFFF;
  FMin.y := $7FFFFFFF;
  FMax.x := -$7FFFFFFF;
  FMax.y := -$7FFFFFFF;

  FSize := SZ;
end;

procedure TAggSerializedScanLinesAdaptorAA.Init(Data: PInt8u; ASize: Cardinal; dx, dy: Double);
begin
  FData := Data;
  FEnd := PInt8u(PtrComp(Data) + ASize);
  FPtr := Data;

  FDelta.x := Trunc(dx + 0.5);
  FDelta.y := Trunc(dy + 0.5);

  FMin.x := $7FFFFFFF;
  FMin.y := $7FFFFFFF;
  FMax.x := -$7FFFFFFF;
  FMax.y := -$7FFFFFFF;
end;

function TAggSerializedScanLinesAdaptorAA.ReadInt32: Integer;
begin
  TInt32Int8uAccess(Result).values[0] := FPtr^;
  inc(PtrComp(FPtr), SizeOf(Int8u));
  TInt32Int8uAccess(Result).values[1] := FPtr^;
  inc(PtrComp(FPtr), SizeOf(Int8u));
  TInt32Int8uAccess(Result).values[2] := FPtr^;
  inc(PtrComp(FPtr), SizeOf(Int8u));
  TInt32Int8uAccess(Result).values[3] := FPtr^;
  inc(PtrComp(FPtr), SizeOf(Int8u));
end;

function TAggSerializedScanLinesAdaptorAA.ReadInt32u: Cardinal;
begin
  TInt32Int8uAccess(Result).values[0] := FPtr^;
  inc(PtrComp(FPtr), SizeOf(Int8u));
  TInt32Int8uAccess(Result).values[1] := FPtr^;
  inc(PtrComp(FPtr), SizeOf(Int8u));
  TInt32Int8uAccess(Result).values[2] := FPtr^;
  inc(PtrComp(FPtr), SizeOf(Int8u));
  TInt32Int8uAccess(Result).values[3] := FPtr^;
  inc(PtrComp(FPtr), SizeOf(Int8u));
end;

function TAggSerializedScanLinesAdaptorAA.RewindScanLines;
begin
  Result := False;
  FPtr := FData;

  if PtrComp(FPtr) < PtrComp(FEnd) then
    begin
      FMin.x := ReadInt32 + FDelta.x;
      FMin.y := ReadInt32 + FDelta.y;
      FMax.x := ReadInt32 + FDelta.x;
      FMax.y := ReadInt32 + FDelta.y;

      Result := True;
    end;
end;

function TAggSerializedScanLinesAdaptorAA.GetMinX;
begin
  Result := FMin.x;
end;

function TAggSerializedScanLinesAdaptorAA.GetMinY;
begin
  Result := FMin.y;
end;

function TAggSerializedScanLinesAdaptorAA.GetSize: Cardinal;
begin
  Result := FSize;
end;

function TAggSerializedScanLinesAdaptorAA.GetMaxX;
begin
  Result := FMax.x;
end;

function TAggSerializedScanLinesAdaptorAA.GetMaxY;
begin
  Result := FMax.y;
end;

function TAggSerializedScanLinesAdaptorAA.SweepScanLine(
  SL: TAggCustomScanLine): Boolean;
var
  y, x, Len: Integer;
  NumSpans: Cardinal;
begin
  SL.ResetSpans;

  repeat
    if PtrComp(FPtr) >= PtrComp(FEnd) then
      begin
        Result := False;
        Exit;
      end;

    ReadInt32; // Skip ScanLine size in bytes
    y := ReadInt32 + FDelta.y;
    NumSpans := ReadInt32;

    repeat
      x := ReadInt32 + FDelta.x;
      Len := ReadInt32;

      if Len < 0 then
        begin
          SL.AddSpan(x, Cardinal(-Len), FPtr^);
          inc(PtrComp(FPtr), FSize);
        end
      else
        begin
          SL.AddCells(x, Len, FPtr);
          inc(PtrComp(FPtr), Len * FSize);
        end;
      dec(NumSpans);
    until NumSpans = 0;

    if SL.NumSpans <> 0 then
      begin
        SL.Finalize(y);
        Break;
      end;
  until False;
  Result := True;
end;

// function TAggSerializedScanLinesAdaptorAA.SweepScanLineEm;
function TAggSerializedScanLinesAdaptorAA.SweepScanLine(
  SL: TAggEmbeddedScanLine): Boolean;
var
  ByteSize: Cardinal;
begin
  repeat
    if PtrComp(FPtr) >= PtrComp(FEnd) then
      begin
        Result := False;
        Exit;
      end;
    ByteSize := ReadInt32u;
    SL.Init(FPtr, FDelta.x, FDelta.y);
    inc(PtrComp(FPtr), ByteSize - SizeOf(Int32));
  until SL.NumSpans <> 0;
  Result := True;
end;

{ TAggSerializedScanLinesAdaptorAA8 }

constructor TAggSerializedScanLinesAdaptorAA8.Create;
begin
  inherited Create(SizeOf(Int8u));
end;

constructor TAggSerializedScanLinesAdaptorAA8.Create(Data: PInt8u;
  ASize: Cardinal; dx, dy: Double);
begin
  inherited Create(SizeOf(Int8u), Data, ASize, dx, dy);
end;

{ TAggSerializedScanLinesAdaptorAA16 }

constructor TAggSerializedScanLinesAdaptorAA16.Create;
begin
  inherited Create(SizeOf(Int16u));
end;

constructor TAggSerializedScanLinesAdaptorAA16.Create(Data: PInt8u;
  ASize: Cardinal; dx, dy: Double);
begin
  inherited Create(SizeOf(Int8u), Data, ASize, dx, dy);
end;

{ TAggSerializedScanLinesAdaptorAA32 }

constructor TAggSerializedScanLinesAdaptorAA32.Create;
begin
  inherited Create(SizeOf(Int32u));
end;

constructor TAggSerializedScanLinesAdaptorAA32.Create(Data: PInt8u;
  ASize: Cardinal; dx, dy: Double);
begin
  inherited Create(SizeOf(Int8u), Data, ASize, dx, dy);
end;

end. 
 
 
 
