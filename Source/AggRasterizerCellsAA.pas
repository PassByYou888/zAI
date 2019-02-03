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
unit AggRasterizerCellsAA;

{$DEFINE FPC_DELPHI_MODE}
{$INCLUDE zDefine.inc}
interface
uses
  AggBasics,
  AggMath,
  AggArray,
  AggScanline;

const
  CAggCellBlockShift = 12;
  CAggCellBlockSize  = 1 shl CAggCellBlockShift;
  CAggCellBlockMask  = CAggCellBlockSize - 1;
  CAggCellBlockPool  = 256;
  CAggCellBlockLimit = 1024;

type
  // A pixel cell. There're no constructors defined and it was done
  // intentionally in order to avoid extra overhead when allocating an
  // array of cells.
  PPPAggCellStyleAA = ^PPAggCellStyleAA;
  PPAggCellStyleAA  = ^PAggCellStyleAA;
  PAggCellStyleAA   = ^TAggCellStyleAA;

  TAggCellStyleAA = record
    x, y, Cover, Area: Integer;
    Left, Right: Int16;
  public
    procedure Initial;
    procedure Style(c: PAggCellStyleAA);
    function NotEqual(EX, EY: Integer; c: PAggCellStyleAA): Integer;
  end;

  PAggSortedY = ^TAggSortedY;

  TAggSortedY = record
    Start, Num: Cardinal;
  end;

  // An internal class that implements the main rasterization algorithm.
  // Used in the Rasterizer. Should not be used direcly.
  TAggRasterizerCellsAA = class
  private
    FNumBlocks, FMaxBlocks, FCurrVlock, FNumCells: Cardinal;

    FCells: PPAggCellStyleAA;

    FCurrentCellPtr: PAggCellStyleAA;
    FSortedCells, FSortedY: TAggPodVector;
    FCurrentCell, FStyleCell: TAggCellStyleAA;

    FMin, FMax: TPointInteger;

    FSorted: Boolean;

    procedure SetCurrentCell(x, y: Integer);
    procedure AddCurrentCell;
    procedure RenderHorizontalLine(EY, x1, y1, x2, y2: Integer);
    procedure AllocateBlock;

    function GetMinX: Integer;
    function GetMinY: Integer;
    function GetMaxX: Integer;
    function GetMaxY: Integer;
  public
    constructor Create;
    destructor Destroy; override;

    procedure Reset;
    procedure Style(StyleCell: PAggCellStyleAA);
    procedure Line(x1, y1, x2, y2: Integer);

    procedure SortCells;

    function ScanLineNumCells(y: Cardinal): Cardinal;
    function ScanLineCells(y: Cardinal): PPAggCellStyleAA;

    property TotalCells: Cardinal read FNumCells;
    property Sorted: Boolean read FSorted;

    property MinX: Integer read GetMinX;
    property MinY: Integer read GetMinY;
    property MaxX: Integer read GetMaxX;
    property MaxY: Integer read GetMaxY;
  end;

  TAggScanLineHitTest = class(TAggCustomScanLine)
  private
    fx: Integer;
    FHit: Boolean;
  protected
    function GetNumSpans: Cardinal; override;
  public
    constructor Create(x: Integer);

    procedure ResetSpans; override;

    procedure Finalize(y: Integer); override;
    procedure AddCell(x: Integer; Cover: Cardinal); override;
    procedure AddSpan(x: Integer; Len, Cover: Cardinal); override;

    property Hit: Boolean read FHit;
  end;

implementation


{ TAggCellStyleAA }

procedure TAggCellStyleAA.Initial;
begin
  x := $7FFFFFFF;
  y := $7FFFFFFF;
  Cover := 0;
  Area := 0;
  Left := -1;
  Right := -1;
end;

procedure TAggCellStyleAA.Style(c: PAggCellStyleAA);
begin
  Left := c.Left;
  Right := c.Right;
end;

function TAggCellStyleAA.NotEqual(EX, EY: Integer; c: PAggCellStyleAA): Integer;
begin
  Result := (EX - x) or (EY - y) or (Left - c.Left) or (Right - c.Right);
end;

{ TAggRasterizerCellsAA }

constructor TAggRasterizerCellsAA.Create;
begin
  FNumBlocks := 0;
  FMaxBlocks := 0;
  FCurrVlock := 0;
  FNumCells := 0;

  FCells := nil;
  FCurrentCellPtr := nil;

  FSortedCells := TAggPodVector.Create(SizeOf(PAggCellStyleAA));
  FSortedY := TAggPodVector.Create(SizeOf(TAggSortedY));

  FMin.x := $7FFFFFFF;
  FMin.y := $7FFFFFFF;
  FMax.x := -$7FFFFFFF;
  FMax.y := -$7FFFFFFF;
  FSorted := False;

  FStyleCell.Initial;
  FCurrentCell.Initial;
end;

destructor TAggRasterizerCellsAA.Destroy;
var
  PTR: PPAggCellStyleAA;
begin
  FSortedCells.Free;
  FSortedY.Free;

  if FNumBlocks <> 0 then
    begin
      PTR := PPAggCellStyleAA(PtrComp(FCells) + (FNumBlocks - 1) *
        SizeOf(PAggCellStyleAA));

      while FNumBlocks <> 0 do
        begin
          dec(FNumBlocks);

          AggFreeMem(Pointer(PTR^), CAggCellBlockSize * SizeOf(TAggCellStyleAA));

          dec(PtrComp(PTR), SizeOf(PAggCellStyleAA));
        end;

      AggFreeMem(Pointer(FCells), FMaxBlocks * SizeOf(PAggCellStyleAA));
    end;

  inherited;
end;

procedure TAggRasterizerCellsAA.Reset;
begin
  FNumCells := 0;
  FCurrVlock := 0;

  FCurrentCell.Initial;
  FStyleCell.Initial;

  FSorted := False;
  FMin.x := $7FFFFFFF;
  FMin.y := $7FFFFFFF;
  FMax.x := -$7FFFFFFF;
  FMax.y := -$7FFFFFFF;
end;

procedure TAggRasterizerCellsAA.Style(StyleCell: PAggCellStyleAA);
begin
  FStyleCell.Style(StyleCell);
end;

procedure TAggRasterizerCellsAA.Line(x1, y1, x2, y2: Integer);
const
  CDxLimit = 16384 shl CAggPolySubpixelShift;
var
  dx, dy: Integer;
  center: TPointInteger;
  Ex1, Ex2, Ey1, Ey2, FY1, FY2, EX, TwoFX, Area: Integer;
  FromX, tox, p, Rem, ModValue, Lift, Delta, First, Incr: Integer;
begin
  dx := x2 - x1;

  if (dx >= CDxLimit) or (dx <= -CDxLimit) then
    begin
      center := PointInteger(ShrInt32(x1 + x2, 1), ShrInt32(y1 + y2, 1));

      Line(x1, y1, center.x, center.y);
      Line(center.x, center.y, x2, y2);
    end;

  dy := y2 - y1;
  Ex1 := ShrInt32(x1, CAggPolySubpixelShift);
  Ex2 := ShrInt32(x2, CAggPolySubpixelShift);
  Ey1 := ShrInt32(y1, CAggPolySubpixelShift);
  Ey2 := ShrInt32(y2, CAggPolySubpixelShift);
  FY1 := y1 and CAggPolySubpixelMask;
  FY2 := y2 and CAggPolySubpixelMask;

  if Ex1 < FMin.x then
      FMin.x := Ex1;

  if Ex1 > FMax.x then
      FMax.x := Ex1;

  if Ey1 < FMin.y then
      FMin.y := Ey1;

  if Ey1 > FMax.y then
      FMax.y := Ey1;

  if Ex2 < FMin.x then
      FMin.x := Ex2;

  if Ex2 > FMax.x then
      FMax.x := Ex2;

  if Ey2 < FMin.y then
      FMin.y := Ey2;

  if Ey2 > FMax.y then
      FMax.y := Ey2;

  SetCurrentCell(Ex1, Ey1);

  // everything is on a single HorizontalLine
  if Ey1 = Ey2 then
    begin
      RenderHorizontalLine(Ey1, x1, FY1, x2, FY2);
      Exit;
    end;

  // Vertical line - we have to calculate start and end cells,
  // and then - the common values of the area and coverage for
  // all cells of the line. We know exactly there's only one
  // cell, so, we don't have to call render_HorizontalLine().
  Incr := 1;

  if dx = 0 then
    begin
      EX := ShrInt32(x1, CAggPolySubpixelShift);
      TwoFX := (x1 - (EX shl CAggPolySubpixelShift)) shl 1;
      First := CAggPolySubpixelScale;

      if dy < 0 then
        begin
          First := 0;
          Incr := -1;
        end;

      FromX := x1;

      // RenderHorizontalLine(ey1 ,FromX ,fy1 ,FromX ,first );
      Delta := First - FY1;

      inc(FCurrentCell.Cover, Delta);
      inc(FCurrentCell.Area, TwoFX * Delta);
      inc(Ey1, Incr);

      SetCurrentCell(EX, Ey1);

      Delta := First + First - CAggPolySubpixelScale;
      Area := TwoFX * Delta;

      while Ey1 <> Ey2 do
        begin
          // RenderHorizontalLine(ey1 ,FromX ,CAggPolySubpixelScale - first ,FromX ,first );
          FCurrentCell.Cover := Delta;
          FCurrentCell.Area := Area;

          inc(Ey1, Incr);

          SetCurrentCell(EX, Ey1);
        end;

      // RenderHorizontalLine(ey1 ,FromX ,CAggPolySubpixelScale - first ,FromX ,fy2 );
      Delta := FY2 - CAggPolySubpixelScale + First;

      inc(FCurrentCell.Cover, Delta);
      inc(FCurrentCell.Area, TwoFX * Delta);

      Exit;
    end;

  // ok, we have to render several HorizontalLines
  p := (CAggPolySubpixelScale - FY1) * dx;
  First := CAggPolySubpixelScale;

  if dy < 0 then
    begin
      p := FY1 * dx;
      First := 0;
      Incr := -1;
      dy := -dy;
    end;

  Delta := p div dy;
  ModValue := p mod dy;

  if ModValue < 0 then
    begin
      dec(Delta);
      inc(ModValue, dy);
    end;

  FromX := x1 + Delta;

  RenderHorizontalLine(Ey1, x1, FY1, FromX, First);

  inc(Ey1, Incr);

  SetCurrentCell(ShrInt32(FromX, CAggPolySubpixelShift), Ey1);

  if Ey1 <> Ey2 then
    begin
      p := CAggPolySubpixelScale * dx;
      Lift := p div dy;
      Rem := p mod dy;

      if Rem < 0 then
        begin
          dec(Lift);
          inc(Rem, dy);
        end;

      dec(ModValue, dy);

      while Ey1 <> Ey2 do
        begin
          Delta := Lift;

          inc(ModValue, Rem);

          if ModValue >= 0 then
            begin
              dec(ModValue, dy);
              inc(Delta);
            end;

          tox := FromX + Delta;

          RenderHorizontalLine(Ey1, FromX, CAggPolySubpixelScale - First, tox, First);

          FromX := tox;

          inc(Ey1, Incr);

          SetCurrentCell(ShrInt32(FromX, CAggPolySubpixelShift), Ey1);
        end;
    end;

  RenderHorizontalLine(Ey1, FromX, CAggPolySubpixelScale - First, x2, FY2);
end;

function TAggRasterizerCellsAA.GetMinX: Integer;
begin
  Result := FMin.x;
end;

function TAggRasterizerCellsAA.GetMinY: Integer;
begin
  Result := FMin.y;
end;

function TAggRasterizerCellsAA.GetMaxX: Integer;
begin
  Result := FMax.x;
end;

function TAggRasterizerCellsAA.GetMaxY: Integer;
begin
  Result := FMax.y;
end;

procedure SwapCells(a, b: Pointer);
var
  Temp: Pointer;

begin
  Temp := Pointer(a^);
  Pointer(a^) := Pointer(b^);
  Pointer(b^) := Temp;
end;

const
  CQSortThreshold = 9;

procedure QuickSortCells(Start: PPAggCellStyleAA; Num: Cardinal);
var
  Stack: array [0 .. 79] of PPAggCellStyleAA;
  Top: PPPAggCellStyleAA;
  Limit, Base: PPAggCellStyleAA;
  Len, x: Integer;
  i, j, Pivot: PPAggCellStyleAA;
begin
  Limit := PPAggCellStyleAA(PtrComp(Start) + Num *
    SizeOf(PAggCellStyleAA));
  Base := Start;
  Top := @Stack[0];

  repeat
    Len := (PtrComp(Limit) - PtrComp(Base)) div SizeOf(PAggCellStyleAA);

    if Len > CQSortThreshold then
      begin
        // we use base + len/2 as the pivot
        Pivot := PPAggCellStyleAA(PtrComp(Base) + (Len div 2) *
          SizeOf(PAggCellStyleAA));

        SwapCells(Base, Pivot);

        i := PPAggCellStyleAA(PtrComp(Base) + SizeOf(PAggCellStyleAA));
        j := PPAggCellStyleAA(PtrComp(Limit) - SizeOf(PAggCellStyleAA));

        // now ensure that *i <= *base <= *j
        if j^^.x < i^^.x then
            SwapCells(i, j);

        if Base^^.x < i^^.x then
            SwapCells(Base, i);

        if j^^.x < Base^^.x then
            SwapCells(Base, j);

        repeat
          x := Base^^.x;

          repeat
              inc(PtrComp(i), SizeOf(PAggCellStyleAA));
          until i^^.x >= x;

          repeat
              dec(PtrComp(j), SizeOf(PAggCellStyleAA));
          until x >= j^^.x;

          if PtrComp(i) > PtrComp(j) then
              Break;

          SwapCells(i, j);
        until False;

        SwapCells(Base, j);

        // now, push the largest sub-array
        if PtrComp(j) - PtrComp(Base) > PtrComp(Limit) - PtrComp(i) then
          begin
            Top^ := Base;

            PPPAggCellStyleAA(PtrComp(Top) +
              SizeOf(PPAggCellStyleAA))^ := j;

            Base := i;
          end
        else
          begin
            Top^ := i;

            PPPAggCellStyleAA(PtrComp(Top) + SizeOf(PPAggCellStyleAA))
              ^ := Limit;

            Limit := j;
          end;

        inc(PtrComp(Top), 2 * SizeOf(PPAggCellStyleAA));
      end
    else
      begin
        // the sub-array is small, perform insertion sort
        j := Base;
        i := PPAggCellStyleAA(PtrComp(j) + SizeOf(PAggCellStyleAA));

        while PtrComp(i) < PtrComp(Limit) do
          begin
            while PPAggCellStyleAA(PtrComp(j) + SizeOf(PAggCellStyleAA))^^.x
              < j^^.x do
              begin
                SwapCells(PPAggCellStyleAA(PtrComp(j) +
                  SizeOf(PAggCellStyleAA)), j);

                if PtrComp(j) = PtrComp(Base) then
                    Break;

                dec(PtrComp(j), SizeOf(PAggCellStyleAA));
              end;

            j := i;

            inc(PtrComp(i), SizeOf(PAggCellStyleAA));
          end;

        if PtrComp(Top) > PtrComp(@Stack[0]) then
          begin
            dec(PtrComp(Top), 2 * SizeOf(PPAggCellStyleAA));

            Base := Top^;
            Limit := PPPAggCellStyleAA
              (PtrComp(Top) + SizeOf(PPAggCellStyleAA))^;
          end
        else
            Break;
      end;
  until False;
end;

procedure TAggRasterizerCellsAA.SortCells;
var
  BlockPointer: PPAggCellStyleAA;
  CellPointer: PAggCellStyleAA;

  nb, i, Start, v: Cardinal;

  CurrentY: PAggSortedY;
begin
  // Perform sort only the first time.
  if FSorted then
      Exit;

  AddCurrentCell;

  FCurrentCell.x := $7FFFFFFF;
  FCurrentCell.y := $7FFFFFFF;
  FCurrentCell.Cover := 0;
  FCurrentCell.Area := 0;

  if FNumCells = 0 then
      Exit;

  // Allocate the array of cell pointers
  FSortedCells.Allocate(FNumCells, 16);

  // Allocate and zero the Y array
  FSortedY.Allocate(FMax.y - FMin.y + 1, 16);
  FSortedY.Zero;

  // Create the Y-histogram (count the numbers of cells for each Y)
  BlockPointer := FCells;

  nb := FNumCells shr CAggCellBlockShift;

  while nb <> 0 do
    begin
      dec(nb);

      CellPointer := BlockPointer^;

      inc(PtrComp(BlockPointer), SizeOf(PAggCellStyleAA));

      i := CAggCellBlockSize;

      while i <> 0 do
        begin
          dec(i);
          inc(PAggSortedY(FSortedY[CellPointer.y - FMin.y]).Start);
          inc(PtrComp(CellPointer), SizeOf(TAggCellStyleAA));
        end;
    end;

  CellPointer := BlockPointer^;

  inc(PtrComp(BlockPointer), SizeOf(PAggCellStyleAA));

  i := FNumCells and CAggCellBlockMask;

  while i <> 0 do
    begin
      dec(i);
      inc(PAggSortedY(FSortedY[CellPointer.y - FMin.y]).Start);
      inc(PtrComp(CellPointer), SizeOf(TAggCellStyleAA));
    end;

  // Convert the Y-histogram into the array of starting indexes
  Start := 0;
  i := 0;

  while i < FSortedY.Size do
    begin
      v := PAggSortedY(FSortedY[i]).Start;

      PAggSortedY(FSortedY[i]).Start := Start;

      inc(Start, v);
      inc(i);
    end;

  // Fill the cell pointer array sorted by Y
  BlockPointer := FCells;

  nb := FNumCells shr CAggCellBlockShift;

  while nb <> 0 do
    begin
      dec(nb);

      CellPointer := BlockPointer^;

      inc(PtrComp(BlockPointer), SizeOf(PAggCellStyleAA));

      i := CAggCellBlockSize;

      while i <> 0 do
        begin
          dec(i);

          CurrentY := PAggSortedY(FSortedY[CellPointer.y - FMin.y]);

          PPAggCellStyleAA(FSortedCells[CurrentY.Start +
            CurrentY.Num])^ := CellPointer;

          inc(CurrentY.Num);
          inc(PtrComp(CellPointer), SizeOf(TAggCellStyleAA));
        end;
    end;

  CellPointer := BlockPointer^;

  inc(PtrComp(BlockPointer), SizeOf(PAggCellStyleAA));

  i := FNumCells and CAggCellBlockMask;

  while i <> 0 do
    begin
      dec(i);

      CurrentY := PAggSortedY(FSortedY[CellPointer.y - FMin.y]);

      PPAggCellStyleAA(FSortedCells[CurrentY.Start +
        CurrentY.Num])^ := CellPointer;

      inc(CurrentY.Num);
      inc(PtrComp(CellPointer), SizeOf(TAggCellStyleAA));
    end;

  // Finally arrange the X-arrays
  i := 0;

  while i < FSortedY.Size do
    begin
      CurrentY := PAggSortedY(FSortedY[i]);

      if CurrentY.Num <> 0 then
          QuickSortCells(PPAggCellStyleAA(PtrComp(FSortedCells.Data) +
          CurrentY.Start * SizeOf(PAggCellStyleAA)), CurrentY.Num);

      inc(i);
    end;

  FSorted := True;
end;

function TAggRasterizerCellsAA.ScanLineNumCells(y: Cardinal): Cardinal;
begin
  Result := PAggSortedY(FSortedY[y - FMin.y]).Num;
end;

function TAggRasterizerCellsAA.ScanLineCells(y: Cardinal): PPAggCellStyleAA;
begin
  Result := PPAggCellStyleAA(PtrComp(FSortedCells.Data) +
    PAggSortedY(FSortedY[y - FMin.y]).Start * SizeOf(PAggCellStyleAA));
end;

procedure TAggRasterizerCellsAA.SetCurrentCell(x, y: Integer);
begin
  if FCurrentCell.NotEqual(x, y, @FStyleCell) <> 0 then
    begin
      AddCurrentCell;

      FCurrentCell.Style(@FStyleCell);

      FCurrentCell.x := x;
      FCurrentCell.y := y;
      FCurrentCell.Cover := 0;
      FCurrentCell.Area := 0;
    end;
end;

procedure TAggRasterizerCellsAA.AddCurrentCell;
begin
  if FCurrentCell.Area or FCurrentCell.Cover <> 0 then
    begin
      if FNumCells and CAggCellBlockMask = 0 then
        begin
          if FNumBlocks >= CAggCellBlockLimit then
              Exit;

          AllocateBlock;
        end;

      FCurrentCellPtr^ := FCurrentCell;

      inc(PtrComp(FCurrentCellPtr), SizeOf(TAggCellStyleAA));
      inc(FNumCells);
    end;
end;

procedure TAggRasterizerCellsAA.RenderHorizontalLine(EY, x1, y1, x2, y2: Integer);
var
  Ex1, Ex2, FX1, FX2, Delta, p, First, dx, Incr, Lift, ModValue, Rem: Integer;
begin
  Ex1 := ShrInt32(x1, CAggPolySubpixelShift);
  Ex2 := ShrInt32(x2, CAggPolySubpixelShift);
  FX1 := x1 and CAggPolySubpixelMask;
  FX2 := x2 and CAggPolySubpixelMask;

  // trivial case. Happens often
  if y1 = y2 then
    begin
      SetCurrentCell(Ex2, EY);
      Exit;
    end;

  // everything is located in a single cell.  That is easy!
  if Ex1 = Ex2 then
    begin
      Delta := y2 - y1;

      inc(FCurrentCell.Cover, Delta);
      inc(FCurrentCell.Area, (FX1 + FX2) * Delta);

      Exit;
    end;

  // ok, we'll have to render a run of adjacent cells on the same
  // HorizontalLine...
  p := (CAggPolySubpixelScale - FX1) * (y2 - y1);
  First := CAggPolySubpixelScale;
  Incr := 1;

  dx := x2 - x1;

  if dx < 0 then
    begin
      p := FX1 * (y2 - y1);
      First := 0;
      Incr := -1;
      dx := -dx;
    end;

  Delta := p div dx;
  ModValue := p mod dx;

  if ModValue < 0 then
    begin
      dec(Delta);
      inc(ModValue, dx);
    end;

  inc(FCurrentCell.Cover, Delta);
  inc(FCurrentCell.Area, (FX1 + First) * Delta);
  inc(Ex1, Incr);

  SetCurrentCell(Ex1, EY);

  inc(y1, Delta);

  if Ex1 <> Ex2 then
    begin
      p := CAggPolySubpixelScale * (y2 - y1 + Delta);
      Lift := p div dx;
      Rem := p mod dx;

      if Rem < 0 then
        begin
          dec(Lift);
          inc(Rem, dx);
        end;

      dec(ModValue, dx);

      while Ex1 <> Ex2 do
        begin
          Delta := Lift;

          inc(ModValue, Rem);

          if ModValue >= 0 then
            begin
              dec(ModValue, dx);
              inc(Delta);
            end;

          inc(FCurrentCell.Cover, Delta);
          inc(FCurrentCell.Area, CAggPolySubpixelScale * Delta);

          inc(y1, Delta);
          inc(Ex1, Incr);

          SetCurrentCell(Ex1, EY);
        end;
    end;

  Delta := y2 - y1;

  inc(FCurrentCell.Cover, Delta);
  inc(FCurrentCell.Area, (FX2 + CAggPolySubpixelScale - First) * Delta);
end;

procedure TAggRasterizerCellsAA.AllocateBlock;
var
  NewCells: PPAggCellStyleAA;
begin
  if FCurrVlock >= FNumBlocks then
    begin
      if FNumBlocks >= FMaxBlocks then
        begin
          AggGetMem(Pointer(NewCells), (FMaxBlocks + CAggCellBlockPool) *
            SizeOf(PAggCellStyleAA));

          if FCells <> nil then
            begin
              Move(FCells^, NewCells^, FMaxBlocks * SizeOf(PAggCellStyleAA));

              AggFreeMem(Pointer(FCells), FMaxBlocks * SizeOf(PAggCellStyleAA));
            end;

          FCells := NewCells;

          inc(FMaxBlocks, CAggCellBlockPool);
        end;

      AggGetMem(Pointer(PPAggCellStyleAA(PtrComp(FCells) + FNumBlocks *
        SizeOf(PAggCellStyleAA))^), CAggCellBlockSize * SizeOf(TAggCellStyleAA));

      inc(FNumBlocks);
    end;

  FCurrentCellPtr := PPAggCellStyleAA(PtrComp(FCells) + FCurrVlock *
    SizeOf(PAggCellStyleAA))^;

  inc(FCurrVlock);
end;

{ TAggScanLineHitTest }

constructor TAggScanLineHitTest.Create(x: Integer);
begin
  fx := x;
  FHit := False;
end;

procedure TAggScanLineHitTest.ResetSpans;
begin
end;

procedure TAggScanLineHitTest.Finalize(y: Integer);
begin
end;

procedure TAggScanLineHitTest.AddCell(x: Integer; Cover: Cardinal);
begin
  if fx = x then
      FHit := True;
end;

procedure TAggScanLineHitTest.AddSpan(x: Integer; Len, Cover: Cardinal);
begin
  if (fx >= x) and (fx < x + Len) then
      FHit := True;
end;

function TAggScanLineHitTest.GetNumSpans: Cardinal;
begin
  Result := 1;
end;

end. 
 
 
