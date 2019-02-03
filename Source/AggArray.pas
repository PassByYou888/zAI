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
unit AggArray;

{$DEFINE FPC_DELPHI_MODE}
{$INCLUDE zDefine.inc}

interface

uses
  AggBasics;

type
  TAggFuncLess = function(e1, e2: Pointer): Boolean;
  TAggFuncEqual = function(e1, e2: Pointer): Boolean;

  TAggCustomArray = class
  protected
    function GetSize: Cardinal; virtual; abstract;
    function GetEntry: Cardinal; virtual; abstract;
    function ArrayOperator(index: Cardinal): Pointer; virtual; abstract;
  public
    function at(index: Cardinal): Pointer; virtual;

    property Size: Cardinal read GetSize;
    property Entry: Cardinal read GetEntry;
    property ItemPointer[index: Cardinal]: Pointer read ArrayOperator; default;
  end;

  TAggRangeAdaptor = class(TAggCustomArray)
  private
    FStart: Cardinal;
  protected
    FSize: Cardinal;
    FArray: TAggCustomArray;
    function GetSize: Cardinal; override;
    function GetEntry: Cardinal; override;
    function ArrayOperator(index: Cardinal): Pointer; override;
  public
    constructor Create(AArray: TAggCustomArray; Start, ASize: Cardinal);
  end;

  TAggPodArrayAdaptor = class(TAggCustomArray)
  private
    FEntry: Cardinal;
  protected
    FSize: Cardinal;
    FArray: Pointer;
    function GetSize: Cardinal; override;
    function GetEntry: Cardinal; override;
    function ArrayOperator(index: Cardinal): Pointer; override;
  public
    constructor Create(AArray: Pointer; ASize, Entry: Cardinal);

    function at(index: Cardinal): Pointer; override;
  end;

  TAggPodAutoArray = class(TAggCustomArray)
  protected
    FSize: Cardinal;
    FArray: Pointer;
    FEntrySize: Cardinal;
    function GetSize: Cardinal; override;
    function GetEntry: Cardinal; override;
    function ArrayOperator(index: Cardinal): Pointer; override;
  public
    constructor Create(ASize, EntrySize: Cardinal);
    destructor Destroy; override;

    property EntrySize: Cardinal read FEntrySize;
  end;

  // ------------------------------------------------------------------------
  // A simple class template to store Plain Old Data, a vector
  // of a fixed size. The data is continous in memory
  // ------------------------------------------------------------------------
  TAggPodArray = class(TAggCustomArray)
  private
    FCapacity: Cardinal;
  protected
    FSize: Cardinal;
    FArray: Pointer;
    FEntrySize: Cardinal;
    function GetSize: Cardinal; override;
    function GetEntry: Cardinal; override;
    function ArrayOperator(index: Cardinal): Pointer; override;
  public
    constructor Create(EntrySize: Cardinal); overload;
    constructor Create(EntrySize, ASize: Cardinal); overload;
    destructor Destroy; override;

    procedure Allocate(ASize: Cardinal; ExtraTail: Cardinal = 0);
    procedure Resize(NewSize: Cardinal);
    procedure Capacity(Cap, ExtraTail: Cardinal);

    procedure Zero;
    procedure Add(Value: Pointer);
    function Data: Pointer;

    property ArrayPointer: Pointer read FArray;
    property EntrySize: Cardinal read FEntrySize;
  end;

  TAggPodVector = TAggPodArray;

  // ------------------------------------------------------------------------
  // A simple class template to store Plain Old Data, similar to std::deque
  // It doesn't reallocate memory but instead, uses blocks of data of size
  // of (1 << S), that is, power of two. The data is NOT contiguous in memory,
  // so the only valid access method is operator [] or curr(), prev(), next()
  //
  // There reallocs occure only when the pool of pointers to blocks needs
  // to be extended (it happens very rarely). You can control the value
  // of increment to reallocate the pointer buffer. See the second constructor.
  // By default, the incremeent value equals (1 << S), i.e., the block size.
  // ------------------------------------------------------------------------
  TAggPodDeque = class(TAggCustomArray)
  private
    FBlockShift, FBlockSize, FBlockMask: Cardinal;

    FNumBlocks, FMaxBlocks, FBlockPtrInc: Cardinal;

    FBlocks: PPointer;
  protected
    FSize: Cardinal;
    FEntrySize: Cardinal;
    function GetDataPointer: Pointer;
    procedure FreeBlocks;
    function GetSize: Cardinal; override;
    function GetEntry: Cardinal; override;
    function ArrayOperator(index: Cardinal): Pointer; override;
  public
    constructor Create(EntrySize: Cardinal; AShift: Cardinal = 6); overload;
    constructor Create(BlockPointerInc, EntrySize: Cardinal;
      AShift: Cardinal); overload;
    destructor Destroy; override;

    procedure Clear;
    procedure RemoveAll;
    procedure RemoveLast;

    procedure Add(val: Pointer); virtual;
    procedure ModifyLast(val: Pointer); virtual;

    procedure CutAt(ASize: Cardinal);
    procedure AssignOperator(v: TAggPodDeque);

    function Curr(idx: Cardinal): Pointer;
    function Prev(idx: Cardinal): Pointer;
    function Next(idx: Cardinal): Pointer;
    function Last: Pointer;

    function AllocateContinuousBlock(NumElements: Cardinal): Integer;
    procedure AllocateBlock(nb: Cardinal);

    property EntrySize: Cardinal read FEntrySize;
  end;

  TAggPodBVector = TAggPodDeque;

  // ------------------------------------------------------------------------
  // Allocator for arbitrary POD data. Most usable in different cache
  // systems for efficient memory allocations.
  // Memory is allocated with blocks of fixed size ("block_size" in
  // the constructor). If required size exceeds the block size the allocator
  // creates a new block of the required size. However, the most efficient
  // use is when the average reqired size is much less than the block size.
  // ------------------------------------------------------------------------
  PAggPodAlloc = ^TAggPodAlloc;

  TAggPodAlloc = record
    PTR: PInt8u;
    Size: Cardinal;
  end;

  TAggPodAllocator = class
  private
    FBlockSize, FBlockPtrInc, FNumBlocks, FMaxBlocks: Cardinal;

    FBlocks: PAggPodAlloc;
    FBufPtr: PInt8u;

    FRest: Cardinal;
  public
    constructor Create(FBlockSize: Cardinal; BlockPointerInc: Cardinal = 256 - 8);
    destructor Destroy; override;

    procedure RemoveAll;

    function Allocate(Size: Cardinal; alignment: Cardinal = 1): PInt8u;

    procedure AllocateBlock(Size: Cardinal);
  end;

procedure QuickSort(Arr: TAggCustomArray; Less: TAggFuncLess);
function RemoveDuplicates(Arr: TAggCustomArray; Equal: TAggFuncEqual): Cardinal;

function IntLess(a, b: Pointer): Boolean; {$IFDEF INLINE_ASM} inline; {$ENDIF}
function IntGreater(a, b: Pointer): Boolean; {$IFDEF INLINE_ASM} inline; {$ENDIF}

function CardinalLess(a, b: Pointer): Boolean; {$IFDEF INLINE_ASM} inline; {$ENDIF}
function CardinalGreater(a, b: Pointer): Boolean; {$IFDEF INLINE_ASM} inline; {$ENDIF}


implementation


procedure QuickSort(Arr: TAggCustomArray; Less: TAggFuncLess);
const
  QuickSortThreshold = 9;
type
  Int80_ptr = ^Int80;
  Int80 = array [0 .. 79] of Integer;

var
  Temp, e1, e2: Pointer;
  Swap: Cardinal;
  Stack: Int80;
  Top: Int80_ptr;
  Limit, Base, Len, i, j, Pivot: Integer;
begin
  if Arr.Size < 2 then
      Exit;

  AggGetMem(Temp, Arr.Entry);

  Swap := Arr.Entry;
  Top := @Stack;
  Limit := Arr.Size;
  Base := 0;

  repeat
    Len := Limit - Base;

    if Len > QuickSortThreshold then
      begin
        // we use base + len/2 as the pivot
        Pivot := Base + Len div 2;

        // SwapElements(arr[base], arr[pivot]);
        Move(Arr[Base]^, Temp^, Swap);
        Move(Arr[Pivot]^, Arr[Base]^, Swap);
        Move(Temp^, Arr[Pivot]^, Swap);

        i := Base + 1;
        j := Limit - 1;

        // now ensure that *i <= *base <= *j
        e1 := Arr[j];
        e2 := Arr[i];

        if Less(e1, e2) then
          begin
            // SwapElements(*e1, *e2);
            Move(e1^, Temp^, Swap);
            Move(e2^, e1^, Swap);
            Move(Temp^, e2^, Swap);
          end;

        e1 := Arr[Base];
        e2 := Arr[i];

        if Less(e1, e2) then
          begin
            // SwapElements(*e1, *e2);
            Move(e1^, Temp^, Swap);
            Move(e2^, e1^, Swap);
            Move(Temp^, e2^, Swap);
          end;

        e1 := Arr[j];
        e2 := Arr[Base];

        if Less(e1, e2) then
          begin
            // SwapElements(*e1, *e2);
            Move(e1^, Temp^, Swap);
            Move(e2^, e1^, Swap);
            Move(Temp^, e2^, Swap);
          end;

        repeat
          repeat
              inc(i)
          until not Less(Arr[i], Arr[Base]);

          repeat
              dec(j);
          until not Less(Arr[Base], Arr[j]);

          if i > j then
              Break;

          // SwapElements(arr[i], arr[j]);
          Move(Arr[i]^, Temp^, Swap);
          Move(Arr[j]^, Arr[i]^, Swap);
          Move(Temp^, Arr[j]^, Swap);
        until False;

        // SwapElements(arr[base], arr[j]);
        Move(Arr[Base]^, Temp^, Swap);
        Move(Arr[j]^, Arr[Base]^, Swap);
        Move(Temp^, Arr[j]^, Swap);

        // now, push the largest sub-array
        if j - Base > Limit - i then
          begin
            Top^[0] := Base;
            Top^[1] := j;
            Base := i;
          end
        else
          begin
            Top^[0] := i;
            Top^[1] := Limit;
            Limit := j;
          end;

        inc(PtrComp(Top), 2 * SizeOf(Integer));
      end
    else
      begin
        // the sub-array is small, perform insertion sort
        j := Base;
        i := j + 1;

        while i < Limit do
          begin
            e1 := Arr[j + 1];
            e2 := Arr[j];

            while Less(e1, e2) do
              begin
                // SwapElements(*e1, *e2);
                Move(e1^, Temp^, Swap);
                Move(e2^, e1^, Swap);
                Move(Temp^, e2^, Swap);

                if j = Base then
                    Break;

                dec(j);

                e1 := Arr[j + 1];
                e2 := Arr[j];
              end;

            j := i;

            inc(i);
          end;

        if PtrComp(Top) > PtrComp(@Stack) then
          begin
            dec(PtrComp(Top), 2 * SizeOf(Integer));

            Base := Top^[0];
            Limit := Top^[1];
          end
        else
            Break;
      end;
  until False;

  AggFreeMem(Temp, Arr.Entry);
end;

// Remove duplicates from a sorted array. It doesn't cut the
// tail of the array, it just returns the number of remaining elements.
function RemoveDuplicates(Arr: TAggCustomArray; Equal: TAggFuncEqual): Cardinal;
var
  i, j: Cardinal;
  E: Pointer;
begin
  if Arr.Size < 2 then
    begin
      Result := Arr.Size;

      Exit;
    end;

  i := 1;
  j := 1;

  while i < Arr.Size do
    begin
      E := Arr[i];

      if not Equal(E, Arr.ArrayOperator(i - 1)) then
        begin
          Move(E^, Arr.ArrayOperator(j)^, Arr.Entry);
          inc(j);
        end;

      inc(i);
    end;

  Result := j;
end;

function IntLess(a, b: Pointer): Boolean;
begin
  Result := PInteger(a)^ < PInteger(b)^;
end;

function IntGreater(a, b: Pointer): Boolean;
begin
  Result := PInteger(a)^ > PInteger(b)^;
end;

function CardinalLess(a, b: Pointer): Boolean;
begin
  Result := PCardinal(a)^ < PCardinal(b)^;
end;

function CardinalGreater(a, b: Pointer): Boolean;
begin
  Result := PCardinal(a)^ > PCardinal(b)^;
end;

{ TAggCustomArray }

function TAggCustomArray.at(index: Cardinal): Pointer;
begin
  at := ArrayOperator(index);
end;

{ TAggRangeAdaptor }

constructor TAggRangeAdaptor.Create(AArray: TAggCustomArray;
  Start, ASize: Cardinal);
begin
  FArray := AArray;
  FStart := Start;
  FSize := ASize;
end;

function TAggRangeAdaptor.GetSize: Cardinal;
begin
  Result := FSize;
end;

function TAggRangeAdaptor.GetEntry: Cardinal;
begin
  Result := FArray.Entry;
end;

function TAggRangeAdaptor.ArrayOperator(index: Cardinal): Pointer;
begin
  Result := FArray.ArrayOperator(FStart + index);
end;

{ TAggPodArrayAdaptor }

constructor TAggPodArrayAdaptor.Create(AArray: Pointer; ASize, Entry: Cardinal);
begin
  FArray := AArray;
  FSize := ASize;
  FEntry := Entry;
end;

function TAggPodArrayAdaptor.GetSize;
begin
  Result := FSize;
end;

function TAggPodArrayAdaptor.GetEntry;
begin
  Result := FEntry;
end;

function TAggPodArrayAdaptor.ArrayOperator(index: Cardinal): Pointer;
begin
  Result := Pointer(PtrComp(FArray) + index * SizeOf(FEntry));
end;

function TAggPodArrayAdaptor.at(index: Cardinal): Pointer;
begin
  Result := Pointer(PtrComp(FArray) + index * FEntry);
end;

{ TAggPodAutoArray }

constructor TAggPodAutoArray.Create;
begin
  FSize := ASize;
  FEntrySize := EntrySize;

  AggGetMem(FArray, FSize * FEntrySize);
end;

destructor TAggPodAutoArray.Destroy;
begin
  AggFreeMem(FArray, FSize * FEntrySize);
  inherited
end;

function TAggPodAutoArray.GetSize;
begin
  Result := FSize;
end;

function TAggPodAutoArray.GetEntry;
begin
  Result := FEntrySize;
end;

function TAggPodAutoArray.ArrayOperator(index: Cardinal): Pointer;
begin
  Result := Pointer(PtrComp(FArray) + index * FEntrySize);
end;

{ TAggPodArray }

constructor TAggPodArray.Create(EntrySize: Cardinal);
begin
  FEntrySize := EntrySize;
  FSize := 0;
  FCapacity := 0;

  FArray := nil;
end;

constructor TAggPodArray.Create(EntrySize, ASize: Cardinal);
begin
  Create(EntrySize);
  Allocate(ASize);
  FSize := 0;
end;

destructor TAggPodArray.Destroy;
begin
  if FArray <> nil then
      AggFreeMem(FArray, FCapacity * FEntrySize);

  inherited;
end;

// Allocate n elements. All data is lost,
// but elements can be accessed in range 0...size-1.
procedure TAggPodArray.Allocate(ASize: Cardinal; ExtraTail: Cardinal = 0);
begin
  Capacity(ASize, ExtraTail);

  FSize := ASize;
end;

// Resize keeping the content.
procedure TAggPodArray.Resize;
var
  buff: Pointer;
begin
  if NewSize > FSize then
    if NewSize > FCapacity then
      begin
        AggGetMem(buff, NewSize * FEntrySize);

        if FArray <> nil then
          begin
            Move(FArray^, buff^, FSize * FEntrySize);

            AggFreeMem(FArray, FCapacity * FEntrySize);
          end;

        FArray := buff;
        FCapacity := NewSize;

      end
    else
  else
      FSize := NewSize;
end;

procedure TAggPodArray.Capacity(Cap, ExtraTail: Cardinal);
begin
  FSize := 0;

  if Cap > FCapacity then
    begin
      AggFreeMem(FArray, FCapacity * FEntrySize);

      FCapacity := Cap + ExtraTail;

      if FCapacity > 0 then
          AggGetMem(FArray, FCapacity * FEntrySize)
      else
          FArray := 0;
    end;
end;

procedure TAggPodArray.Zero;
begin
  FillChar(FArray^, FEntrySize * FSize, 0);
end;

procedure TAggPodArray.Add(Value: Pointer);
begin
  Move(Value^, Pointer(PtrComp(FArray) + FSize * FEntrySize)^, FEntrySize);
  inc(FSize);
end;

function TAggPodArray.Data;
begin
  Result := FArray;
end;

function TAggPodArray.GetSize;
begin
  Result := FSize;
end;

function TAggPodArray.GetEntry;
begin
  Result := FEntrySize;
end;

function TAggPodArray.ArrayOperator(index: Cardinal): Pointer;
begin
  Result := Pointer(PtrComp(FArray) + index * FEntrySize);
end;

{ TAggPodDeque }

constructor TAggPodDeque.Create(EntrySize: Cardinal; AShift: Cardinal = 6);
begin
  FBlockShift := AShift;
  FBlockSize := 1 shl FBlockShift;
  FBlockMask := FBlockSize - 1;

  FSize := 0;
  FNumBlocks := 0;
  FMaxBlocks := 0;
  FBlocks := 0;
  FBlockPtrInc := FBlockSize;

  FEntrySize := EntrySize;
end;

constructor TAggPodDeque.Create(BlockPointerInc, EntrySize: Cardinal;
  AShift: Cardinal);
begin
  Create(EntrySize, AShift);

  FBlockPtrInc := BlockPointerInc;
end;

destructor TAggPodDeque.Destroy;
begin
  FreeBlocks;

  inherited;
end;

procedure TAggPodDeque.FreeBlocks;
var
  blk: Pointer;
begin
  if FNumBlocks <> 0 then
    begin
      blk := Pointer(PtrComp(FBlocks) + (FNumBlocks - 1) * SizeOf(Pointer));

      while FNumBlocks <> 0 do
        begin
          AggFreeMem(PPointer(blk)^, FBlockSize * FEntrySize);

          dec(PtrComp(blk), SizeOf(Pointer));
          dec(FNumBlocks);
        end;

      AggFreeMem(Pointer(FBlocks), FMaxBlocks * SizeOf(Pointer));
    end;
end;

procedure TAggPodDeque.Clear;
begin
  FSize := 0;
end;

procedure TAggPodDeque.RemoveAll;
begin
  FSize := 0;
end;

procedure TAggPodDeque.RemoveLast;
begin
  if FSize <> 0 then
      dec(FSize);
end;

procedure TAggPodDeque.Add(val: Pointer);
var
  p: Pointer;
begin
  p := GetDataPointer;

  Move(val^, p^, FEntrySize);
  inc(FSize);
end;

procedure TAggPodDeque.ModifyLast(val: Pointer);
begin
  RemoveLast;
  Add(val);
end;

procedure TAggPodDeque.CutAt(ASize: Cardinal);
begin
  if ASize < FSize then
      FSize := ASize;
end;

function TAggPodDeque.GetSize;
begin
  Result := FSize;
end;

function TAggPodDeque.GetEntry;
begin
  Result := FEntrySize;
end;

function TAggPodDeque.ArrayOperator(index: Cardinal): Pointer;
var
  p: PPointer;
begin
  p := FBlocks;
  inc(p, (index shr FBlockShift));
  Result := p^;
  inc(PByte(Result), (index and FBlockMask) * FEntrySize);
end;

procedure TAggPodDeque.AssignOperator;
var
  i: Cardinal;
  Src, Dst: Pointer;
begin
  FreeBlocks;

  FBlockShift := v.FBlockShift;
  FBlockSize := v.FBlockSize;
  FBlockMask := v.FBlockMask;

  FSize := v.FSize;
  FEntrySize := v.FEntrySize;

  FNumBlocks := v.FNumBlocks;
  FMaxBlocks := v.FMaxBlocks;

  FBlockPtrInc := v.FBlockPtrInc;

  if FMaxBlocks <> 0 then
      AggGetMem(Pointer(FBlocks), FMaxBlocks * SizeOf(Pointer))
  else
      FBlocks := nil;

  Src := v.FBlocks;
  Dst := FBlocks;
  i := 0;

  while i < FNumBlocks do
    begin
      AggGetMem(PPointer(Dst)^, FBlockSize * FEntrySize);

      Move(PPointer(Src)^^, PPointer(Dst)^^, FBlockSize * FEntrySize);

      inc(PtrComp(Src), SizeOf(Pointer));
      inc(PtrComp(Dst), SizeOf(Pointer));
      inc(i);
    end;
end;

function TAggPodDeque.Curr;
begin
  Result := ArrayOperator(idx);
end;

function TAggPodDeque.Prev;
begin
  Result := ArrayOperator((idx + FSize - 1) mod FSize);
end;

function TAggPodDeque.Next;
begin
  Result := ArrayOperator((idx + 1) mod FSize);
end;

function TAggPodDeque.Last: Pointer;
begin
  Result := ArrayOperator(FSize - 1);
end;

function TAggPodDeque.AllocateContinuousBlock;
var
  Rest, index: Cardinal;

begin
  if NumElements < FBlockSize then
    begin
      GetDataPointer; // Allocate initial block if necessary

      Rest := FBlockSize - (FSize and FBlockMask);

      if NumElements <= Rest then
        begin
          // The rest of the block is good, we can use it
          index := FSize;

          inc(FSize, NumElements);

          Result := index;

          Exit;
        end;

      // New block
      inc(FSize, Rest);

      GetDataPointer;

      index := FSize;

      inc(FSize, NumElements);

      Result := index;

      Exit;
    end;

  Result := -1; // Impossible to allocate
end;

procedure TAggPodDeque.AllocateBlock(nb: Cardinal);
var
  NewBlocks: Pointer;
  Blocks: PPointer;
begin
  if nb >= FMaxBlocks then
    begin
      AggGetMem(NewBlocks, (FMaxBlocks + FBlockPtrInc) * SizeOf(Pointer));

      if FBlocks <> nil then
        begin
          Move(FBlocks^, NewBlocks^, FNumBlocks * SizeOf(Pointer));

          AggFreeMem(Pointer(FBlocks), FMaxBlocks * SizeOf(Pointer));
        end;

      FBlocks := NewBlocks;

      inc(FMaxBlocks, FBlockPtrInc);
    end;

  Blocks := FBlocks;
  inc(Blocks, nb);
  AggGetMem(Blocks^, FBlockSize * FEntrySize);

  inc(FNumBlocks);
end;

function TAggPodDeque.GetDataPointer: Pointer;
var
  nb: Cardinal;
  Block: PPointer;
begin
  nb := FSize shr FBlockShift;

  if nb >= FNumBlocks then
      AllocateBlock(nb);

  Block := FBlocks;
  inc(Block, nb);

  Result := Block^;
  inc(PInt8u(Result), (FSize and FBlockMask) * FEntrySize);
end;

{ TAggPodAllocator }

constructor TAggPodAllocator.Create;
begin
  FBlockSize := FBlockSize;
  FBlockPtrInc := BlockPointerInc;

  FNumBlocks := 0;
  FMaxBlocks := 0;

  FBlocks := nil;
  FBufPtr := nil;
  FRest := 0;
end;

destructor TAggPodAllocator.Destroy;
begin
  RemoveAll;
  inherited;
end;

procedure TAggPodAllocator.RemoveAll;
var
  blk: PAggPodAlloc;
begin
  if FNumBlocks <> 0 then
    begin
      blk := PAggPodAlloc(PtrComp(FBlocks) + (FNumBlocks - 1) *
        SizeOf(TAggPodAlloc));

      while FNumBlocks <> 0 do
        begin
          AggFreeMem(Pointer(blk.PTR), blk.Size);

          dec(PtrComp(blk), SizeOf(TAggPodAlloc));
          dec(FNumBlocks);
        end;

      AggFreeMem(Pointer(FBlocks), FMaxBlocks * SizeOf(TAggPodAlloc));
    end;

  FNumBlocks := 0;
  FMaxBlocks := 0;

  FBlocks := nil;
  FBufPtr := nil;
  FRest := 0;
end;

function TAggPodAllocator.Allocate;
var
  PTR: PInt8u;
  Align: Cardinal;
begin
  if Size = 0 then
    begin
      Result := 0;

      Exit;
    end;

  if Size <= FRest then
    begin
      PTR := FBufPtr;

      if alignment > 1 then
        begin
          Align := (alignment - Cardinal(Int32u(PTR)) mod alignment) mod alignment;

          inc(Size, Align);
          inc(PtrComp(PTR), Align);

          if Size <= FRest then
            begin
              dec(FRest, Size);
              inc(PtrComp(FBufPtr), Size);

              Result := PTR;

              Exit;
            end;

          AllocateBlock(Size);

          Result := Allocate(Size - Align, alignment);

          Exit;
        end;

      dec(FRest, Size);
      inc(PtrComp(FBufPtr), Size);

      Result := PTR;

      Exit;
    end;

  AllocateBlock(Size + alignment - 1);

  Result := Allocate(Size, alignment);
end;

procedure TAggPodAllocator.AllocateBlock(Size: Cardinal);
var
  NewBlocks: PAggPodAlloc;
begin
  if Size < FBlockSize then
      Size := FBlockSize;

  if FNumBlocks >= FMaxBlocks then
    begin
      AggGetMem(Pointer(NewBlocks), (FMaxBlocks + FBlockPtrInc) *
        SizeOf(TAggPodAlloc));

      if FBlocks <> nil then
        begin
          Move(FBlocks^, NewBlocks^, FNumBlocks * SizeOf(TAggPodAlloc));

          AggFreeMem(Pointer(FBlocks), FMaxBlocks * SizeOf(TAggPodAlloc));
        end;

      FBlocks := NewBlocks;

      inc(FMaxBlocks, FBlockPtrInc);
    end;

  AggGetMem(Pointer(FBufPtr), Size * SizeOf(Int8u));

  PAggPodAlloc(PtrComp(FBlocks) + FNumBlocks * SizeOf(TAggPodAlloc)).PTR := FBufPtr;
  PAggPodAlloc(PtrComp(FBlocks) + FNumBlocks * SizeOf(TAggPodAlloc)).Size := Size;

  inc(FNumBlocks);

  FRest := Size;
end;

end.
