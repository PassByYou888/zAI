{ ****************************************************************************** }
{ * h264BitStream.pas        by qq600585                                       * }
{ * https://github.com/PassByYou888/CoreCipher                                 * }
{ * https://github.com/PassByYou888/ZServer4D                                  * }
{ * https://github.com/PassByYou888/zExpression                                * }
{ * https://github.com/PassByYou888/zTranslate                                 * }
{ * https://github.com/PassByYou888/zSound                                     * }
{ * https://github.com/PassByYou888/zAnalysis                                  * }
{ * https://github.com/PassByYou888/zGameWare                                  * }
{ * https://github.com/PassByYou888/zRasterization                             * }
{ ****************************************************************************** }

unit h264BitStream;

{$INCLUDE zDefine.inc}
{$POINTERMATH ON}

interface

uses h264Types;

type
  TBitStreamWriter = class
  private
    buffer: uint32_p;
    Cur: uint32_p;
    Mask: uint32_t;
    closed: Boolean;
    function GetBitSize: uint32_t;
    function GetByteSize: uint32_t;
    function GetDataStart: uint8_p;
  public
    property BitSize: uint32_t read GetBitSize;
    property ByteSize: uint32_t read GetByteSize;
    property DataStart: uint8_p read GetDataStart;
    constructor Create(const memory_buffer: uint8_p);
    destructor Destroy; override;
    procedure Close;
    function IsByteAligned: Boolean;
    procedure ByteAlign;
    procedure write(const Bit: int32_t); overload;
    procedure write(Bits, bit_count: uint32_t); overload;
  end;

implementation

function bswap(n: uint32_t): uint32_t; inline;
begin
  Result := (n shr 24) or (n shl 24) or ((n shr 8) and $FF00) or ((n shl 8) and $FF0000);
end;

{ TBitstreamWriter }

function TBitStreamWriter.GetBitSize: uint32_t;
begin
  Result := 32 * (Cur - buffer) + (32 - int32_t(Mask));
end;

function TBitStreamWriter.GetByteSize: uint32_t;
begin
  Result := (Cur - buffer) * 4;
  inc(Result, (32 - Mask + 7) div 8); // + buffer
end;

function TBitStreamWriter.GetDataStart: uint8_p;
begin
  Result := uint8_p(buffer);
end;

constructor TBitStreamWriter.Create(const memory_buffer: uint8_p);
begin
  inherited Create;
  buffer := uint32_p(memory_buffer);
  Cur := buffer;
  Cur^ := 0;
  Mask := 32;
end;

destructor TBitStreamWriter.Destroy;
begin
  if not closed then
      Close;
  inherited Destroy;
end;

procedure TBitStreamWriter.Close;
begin
  if not closed then
    begin
      if Mask < 32 then
          Cur^ := bswap(Cur^);
      closed := True;
    end;
end;

function TBitStreamWriter.IsByteAligned: Boolean;
begin
  Result := Mask mod 8 = 0;
end;

procedure TBitStreamWriter.ByteAlign;
begin
  while not IsByteAligned do
      write(0);
end;

procedure TBitStreamWriter.write(const Bit: int32_t);
begin
  dec(Mask);
  Cur^ := Cur^ or uint32_t((Bit and 1) shl Mask);

  if Mask = 0 then
    begin
      Cur^ := bswap(Cur^);
      inc(Cur);
      Cur^ := 0;
      Mask := 32;
    end;
end;

procedure TBitStreamWriter.write(Bits, bit_count: uint32_t);
var
  bits_left: uint32_t;
begin
  Bits := Bits and ($FFFFFFFF shr (32 - bit_count)); // safety check
  if Mask > bit_count then
    begin
      dec(Mask, bit_count);
      Cur^ := Cur^ or (Bits shl Mask);
    end
  else
    begin
      bits_left := bit_count - Mask;
      Mask := 32 - bits_left;
      Cur^ := Cur^ or (Bits shr bits_left);
      Cur^ := bswap(Cur^);
      inc(Cur);
      Cur^ := 0;
      if bits_left > 0 then
          Cur^ := Bits shl Mask;
    end;
end;

end.  
 
 
