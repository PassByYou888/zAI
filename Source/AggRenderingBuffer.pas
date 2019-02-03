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
unit AggRenderingBuffer;

{$DEFINE FPC_DELPHI_MODE}
{$INCLUDE zDefine.inc}
interface
uses
  AggBasics;

type
  PAggRowDataType = ^TAggRowDataType;

  TAggRowDataType = record
    x1, x2: Integer;
    PTR: PInt8u;
  public
    procedure Initialize(x1, x2: Integer; PTR: PInt8u); overload;
  end;

  TAggSpanData = record
    x: Integer;
    Len: Byte;
    PTR: PInt8u;
  public
    procedure Initialize(x: Integer; Len: Byte; PTR: PInt8u); overload;
  end;

  TAggRenderingBuffer = class
  private
    FBuffer: PInt8u;      // Pointer to rendering buffer
    FRows: PPInt8u;       // Pointers to each row of the buffer
    FStride: Integer;     // Number of bytes per row. Can be < 0
    FMaxHeight: Cardinal; // The maximal height (currently allocated)
    function GetPixelPointer(x, y: Cardinal): PInt8u;
  protected
    FWidth: Cardinal;  // Width in pixels
    FHeight: Cardinal; // Height in pixels
    function GetStrideAbs: Cardinal;
  public
    constructor Create; overload;
    constructor Create(ABuffer: PInt8u; AWidth, AHeight: Cardinal; AStride: Integer); overload;
    destructor Destroy; override;

    procedure Attach(ABuffer: PInt8u; AWidth, AHeight: Cardinal; AStride: Integer);

    function RowXY(x, y: Integer; Len: Cardinal): PInt8u; virtual;
    function Row(y: Cardinal): PInt8u; virtual;
    function NextRow(p: PInt8u): PInt8u; virtual;
    function Rows: PInt8u;

    procedure CopyFrom(RenderingBuffer: TAggRenderingBuffer);
    procedure Clear(Value: Int8u);

    property buffer: PInt8u read FBuffer;
    property height: Cardinal read FHeight;
    property width: Cardinal read FWidth;
    property stride: Integer read FStride;
    property StrideAbs: Cardinal read GetStrideAbs;

    property ScanLine[index: Cardinal]: PInt8u read Row;
    property PixelPointer[x, y: Cardinal]: PInt8u read GetPixelPointer;
  end;

implementation


{ TAggRowDataType }

procedure TAggRowDataType.Initialize(x1, x2: Integer; PTR: PInt8u);
begin
  Self.x1 := x1;
  Self.x2 := x2;
  Self.PTR := PTR;
end;

{ TAggSpanData }

procedure TAggSpanData.Initialize(x: Integer; Len: Byte; PTR: PInt8u);
begin
  Self.x := x;
  Self.Len := Len;
  Self.PTR := PTR;
end;

{ TAggRenderingBuffer }

constructor TAggRenderingBuffer.Create;
begin
  FBuffer := nil;
  FRows := nil;
  FWidth := 0;
  FHeight := 0;
  FStride := 0;

  FMaxHeight := 0;
  inherited;
end;

constructor TAggRenderingBuffer.Create(ABuffer: PInt8u;
  AWidth, AHeight: Cardinal; AStride: Integer);
begin
  Create;
  Attach(ABuffer, AWidth, AHeight, AStride);
end;

destructor TAggRenderingBuffer.Destroy;
begin
  AggFreeMem(Pointer(FRows), FMaxHeight * SizeOf(PInt8u));
  inherited;
end;

procedure TAggRenderingBuffer.Attach(ABuffer: PInt8u; AWidth, AHeight: Cardinal;
  AStride: Integer);
var
  RowsPointer: PPInt8u;
  RowPointer: PInt8u;
begin
  FBuffer := ABuffer;
  FWidth := AWidth;
  FHeight := AHeight;
  FStride := AStride;

  if AHeight > FMaxHeight then
    begin
      AggFreeMem(Pointer(FRows), FMaxHeight * SizeOf(PInt8u));
      AggGetMem(Pointer(FRows), AHeight * SizeOf(PInt8u));

      FMaxHeight := AHeight;
    end;

  if AStride < 0 then
    if AHeight > 0 then
      begin
        RowPointer := FBuffer;
        dec(RowPointer, (AHeight - 1) * AStride);
      end
    else
        RowPointer := nil
  else
      RowPointer := FBuffer;

  RowsPointer := Pointer(FRows);

  while AHeight > 0 do
    begin
      RowsPointer^ := RowPointer;

      inc(PtrComp(RowPointer), AStride);
      inc(PtrComp(RowsPointer), SizeOf(PInt8u));

      dec(AHeight);
    end;
end;

function TAggRenderingBuffer.GetPixelPointer(x, y: Cardinal): PInt8u;
begin
  Result := RowXY(x, y, Abs(FStride) div FWidth);
end;

function TAggRenderingBuffer.GetStrideAbs;
begin
  if FStride < 0 then
      Result := -FStride
  else
      Result := FStride;
end;

function TAggRenderingBuffer.RowXY(x, y: Integer; Len: Cardinal): PInt8u;
var
  RowPointer: PPInt8u;
begin
  RowPointer := FRows;
  inc(RowPointer, y);
  Result := RowPointer^;
end;

function TAggRenderingBuffer.Row(y: Cardinal): PInt8u;
var
  RowPointer: PPInt8u;
begin
  RowPointer := FRows;
  inc(RowPointer, y);
  Result := RowPointer^;
end;

function TAggRenderingBuffer.NextRow(p: PInt8u): PInt8u;
begin
  Result := p;
  inc(Result, FStride);
end;

function TAggRenderingBuffer.Rows;
begin
  Result := Pointer(FRows);
end;

procedure TAggRenderingBuffer.CopyFrom(RenderingBuffer: TAggRenderingBuffer);
var
  h, L, y: Cardinal;
begin
  h := height;

  if RenderingBuffer.height < h then
      h := RenderingBuffer.height;

  L := StrideAbs;

  if RenderingBuffer.StrideAbs < L then
      L := RenderingBuffer.StrideAbs;

  L := L * SizeOf(Int8u);

  if h > 0 then
    for y := 0 to h - 1 do
        Move(RenderingBuffer.Row(y)^, Row(y)^, L);
end;

procedure TAggRenderingBuffer.Clear(Value: Int8u);
var
  y, x: Cardinal;
  p: PInt8u;
begin
  if height > 0 then
    for y := 0 to height - 1 do
      begin
        p := Row(y);

        if StrideAbs > 0 then
          for x := 0 to StrideAbs - 1 do
            begin
              p^ := Value;

              inc(PtrComp(p), SizeOf(Int8u));
            end;
      end;
end;

end. 
 
 
 
