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
unit AggRasterizerScanLineAA;
(*
  ////////////////////////////////////////////////////////////////////////////////
  //                                                                            //
  // Class TAggOutlineAA - implementation.                                      //
  //                                                                            //
  // Initially the rendering algorithm was designed by David Turner and the     //
  // other authors of the FreeType library - see the above notice. I nearly     //
  // created a similar Renderer, but still I was far from David's work.         //
  // I completely redesigned the original code and adapted it for Anti-Grain    //
  // ideas. Two functions - RenderLine and RenderHorizontalLine are the core    //
  // of the algorithm - they calculate the exact coverage of each pixel cell    //
  // of the polygon. I left these functions almost as is, because there's       //
  // no way to improve the perfection - hats off to David and his group!        //
  //                                                                            //
  // All other code is very different from the original.                        //
  //                                                                            //
  ////////////////////////////////////////////////////////////////////////////////
*)

{$DEFINE FPC_DELPHI_MODE}
{$INCLUDE zDefine.inc}
interface
uses
  AggBasics,
  AggArray,
  AggScanline,
  AggRasterizerScanLine,
  AggVertexSource,
  AggGammaFunctions,
  AggClipLiangBarsky;

const
  CAggAntiAliasingShift = 8;
  CAggAntiAliasingNum   = 1 shl CAggAntiAliasingShift;
  CAggAntiAliasingMask  = CAggAntiAliasingNum - 1;
  CAggAntiAliasing2Num  = CAggAntiAliasingNum * 2;
  CAggAntiAliasing2Mask = CAggAntiAliasing2Num - 1;

  CAggCellBlockShift = 12;
  CAggCellBlockSize  = 1 shl CAggCellBlockShift;
  CAggCellBlockMask  = CAggCellBlockSize - 1;
  CAggCellBlockPool  = 256;
  CAggCellBlockLimit = 1024;

  // These constants determine the subpixel accuracy, to be more precise,
  // the number of bits of the fractional part of the coordinates.
  // The possible coordinate capacity in bits can be calculated by formula:
  // SizeOf(Integer) * 8 - CAggPolyBaseShift * 2, i.e, for 32-bit integers and
  // 8-bits fractional part the capacity is 16 bits or [-32768...32767].
  CAggPolyBaseShift = 8;                       // ----CAggPolyBaseShift
  CAggPolyBaseSize  = 1 shl CAggPolyBaseShift; // ----CAggPolyBaseSize
  CAggPolyBaseMask  = CAggPolyBaseSize - 1;    // ----CAggPolyBaseMask

type
  // A pixel cell. There're no constructors defined and it was done
  // intentionally in order to avoid extra overhead when allocating an
  // array of cells.

  PPAggCellAA = ^PAggCellAA;
  PAggCellAA  = ^TAggCellAA;

  TAggCellAA = record
    x, y, Cover, Area: Integer;
  end;

  // An internal class that implements the main rasterization algorithm.
  // Used in the Rasterizer. Should not be used direcly.

  PAggSortedY = ^TAggSortedY;

  TAggSortedY = record
    Start, Num: Cardinal;
  end;

  TAggOutlineAA = class
  private
    FNumBlocks, FMaxBlocks, FCurBlock, FNumCells: Cardinal;

    FCur, FMin, FMax: TPointInteger;

    FSorted: Boolean;

    FCells: PPAggCellAA;
    FCurCellPointer: PAggCellAA;
    FCurCell: TAggCellAA;

    FSortedCells: TAggPodArray;
    FSortedY: TAggPodArray;
  public
    constructor Create;
    destructor Destroy; override;

    procedure MoveTo(x, y: Integer);
    procedure LineTo(x, y: Integer);

    procedure Reset;

    procedure AddCurrentCell;
    procedure SetCurrentCell(x, y: Integer);

    procedure SortCells;
    function ScanLineNumCells(y: Cardinal): Cardinal;
    function ScanLineCells(y: Cardinal): PPAggCellAA;

    procedure RenderLine(x1, y1, x2, y2: Integer);
    procedure RenderHorizontalLine(EY, x1, y1, x2, y2: Integer);

    procedure AllocateBlock;

    property TotalCells: Cardinal read FNumCells;
    property Sorted: Boolean read FSorted;
    property MinX: Integer read FMin.x;
    property MinY: Integer read FMin.y;
    property MaxX: Integer read FMax.x;
    property MaxY: Integer read FMax.y;
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

  // Polygon Rasterizer that is used to render filled polygons with
  // High-quality Anti-Aliasing. Internally, by default, the class uses
  // integer coordinates in format 24.8, i.e. 24 bits for integer part
  // and 8 bits for fractional - see CAggPolyBaseShift. This class can be
  // used in the following  way:
  //
  // 1. SetFillingRule(TAggFillingRule ft) - optional.
  //
  // 2. Gamma() - optional.
  //
  // 3. reset()
  //
  // 4. MoveTo(x, y) / LineTo(x, y) - make the polygon. One can create
  // more than one contour, but each contour must consist of at least 3
  // vertices, i.e. MoveTo(x1, y1); LineTo(x2, y2); LineTo(x3, y3);
  // is the absolute minimum of vertices that define a triangle.
  // The algorithm does not check either the number of vertices nor
  // coincidence of their coordinates, but in the worst case it just
  // won't draw anything.
  // The orger of the vertices (clockwise or counterclockwise)
  // is important when using the non-zero filling rule (frNonZero).
  // In this case the vertex order of all the contours must be the same
  // if you want your intersecting polygons to be without "holes".
  // You actually can use different vertices order. If the contours do not
  // intersect each other the order is not important anyway. If they do,
  // contours with the same vertex order will be rendered without "holes"
  // while the intersecting contours with different orders will have "holes".
  //
  // SetFillingRule() and Gamma() can be called anytime before "sweeping".
  // ------------------------------------------------------------------------
  // TAggFillingRule = (frNonZero ,frEvenOdd );

  TInitialStatus = (siStatusInitial, siStatusLineTo, siStatusClosed);

  TAggRasterizerScanLineAA = class(TAggRasterizerScanLine)
  private
    FOutline: TAggOutlineAA;
    FGamma: array [0 .. CAggAntiAliasingNum - 1] of Integer;

    FFillingRule: TAggFillingRule;
    FClippedStart: TPointInteger;
    FStart, FPrev: TPointInteger;

    FPrevFlags: Cardinal;
    FStatus: TInitialStatus;

    FClipBox: TRectInteger;
    FClipping: Boolean;

    FCurY, FXScale: Integer;

    FAutoClose: Boolean;

    procedure ClosePolygon;
    procedure ClosePolygonNoClip;
  protected
    function GetMinX: Integer; override;
    function GetMinY: Integer; override;
    function GetMaxX: Integer; override;
    function GetMaxY: Integer; override;

    function GetFillingRule: TAggFillingRule; override;
    procedure SetFillingRule(Value: TAggFillingRule); override;
  public
    constructor Create;
    destructor Destroy; override;

    procedure Reset; override;
    procedure AutoClose(flag: Boolean);
    procedure SetClipBox(x1, y1, x2, y2: Double); override;
    procedure SetClipBox(Rect: TRectDouble); override;

    procedure Gamma(AGammaFunction: TAggCustomVertexSource); override;
    function ApplyGamma(Cover: Cardinal): Cardinal;

    procedure MoveToNoClip(x, y: Integer); overload;
    procedure MoveToNoClip(Point: TPointInteger); overload;
    procedure LineToNoClip(x, y: Integer); overload;
    procedure LineToNoClip(Point: TPointInteger); overload;

    procedure ClipSegment(x, y: Integer); overload;
    procedure ClipSegment(Point: TPointInteger); overload;

    procedure MoveToDouble(x, y: Double); overload;
    procedure MoveToDouble(Point: TPointDouble); overload;
    procedure LineToDouble(x, y: Double); overload;
    procedure LineToDouble(Point: TPointDouble); overload;

    procedure MoveTo(x, y: Integer); overload;
    procedure MoveTo(Point: TPointInteger); overload;
    procedure LineTo(x, y: Integer); overload;
    procedure LineTo(Point: TPointInteger); overload;

    procedure Sort; override;
    function RewindScanLines: Boolean; override;
    function SweepScanLine(SL: TAggCustomScanLine): Boolean; override;

    function NavigateScanLine(y: Integer): Boolean;

    function HitTest(TX, TY: Integer): Boolean; override;

    function CalculateAlpha(Area: Integer): Cardinal;

    procedure AddPath(Vs: TAggCustomVertexSource; PathID: Cardinal = 0); override;
    procedure AddVertex(x, y: Double; Cmd: Cardinal); override;
  end;

function PolyCoord(Value: Double): Integer; overload;
function PolyCoord(Value: TPointDouble): TPointInteger; overload;
function PolyCoord(Value: TRectDouble): TRectInteger; overload;

implementation


function PolyCoord(Value: Double): Integer;
begin
  Result := Trunc(Value * CAggPolyBaseSize);
end;

function PolyCoord(Value: TPointDouble): TPointInteger;
begin
  Result.x := Trunc(Value.x * CAggPolyBaseSize);
  Result.y := Trunc(Value.y * CAggPolyBaseSize);
end;

function PolyCoord(Value: TRectDouble): TRectInteger; overload;
begin
  Result.x1 := Trunc(Value.x1 * CAggPolyBaseSize);
  Result.y1 := Trunc(Value.y1 * CAggPolyBaseSize);
  Result.x2 := Trunc(Value.x2 * CAggPolyBaseSize);
  Result.y2 := Trunc(Value.y2 * CAggPolyBaseSize);
end;

{ TAggOutlineAA }

constructor TAggOutlineAA.Create;
begin
  FSortedCells := TAggPodArray.Create(SizeOf(PAggCellAA));
  FSortedY := TAggPodArray.Create(SizeOf(TAggSortedY));

  FNumBlocks := 0;
  FMaxBlocks := 0;
  FCurBlock := 0;
  FNumCells := 0;

  FCur.x := 0;
  FCur.y := 0;
  FMin.x := $7FFFFFFF;
  FMin.y := $7FFFFFFF;
  FMax.x := -$7FFFFFFF;
  FMax.y := -$7FFFFFFF;

  FSorted := False;

  FCells := nil;
  FCurCellPointer := nil;

  with FCurCell do
    begin
      x := $7FFF;
      y := $7FFF;

      Cover := 0;
      Area := 0;
    end;
end;

destructor TAggOutlineAA.Destroy;
begin
  FSortedCells.Free;
  FSortedY.Free;

  if FNumBlocks > 0 then
    begin
      repeat
        dec(FNumBlocks);

        AggFreeMem(Pointer(Pointer(PtrComp(FCells) + FNumBlocks *
          SizeOf(PAggCellAA))^), CAggCellBlockSize * SizeOf(TAggCellAA));
      until FNumBlocks = 0;

      AggFreeMem(Pointer(FCells), SizeOf(PAggCellAA) * FMaxBlocks);
    end;

  inherited;
end;

procedure TAggOutlineAA.MoveTo(x, y: Integer);
begin
  if FSorted then
      Reset;

  // SetCurrentCell(x shr CAggPolyBaseShift ,y shr CAggPolyBaseShift );
  SetCurrentCell(ShrInt32(x, CAggPolyBaseShift), ShrInt32(y, CAggPolyBaseShift));

  FCur.x := x;
  FCur.y := y;
end;

procedure TAggOutlineAA.LineTo(x, y: Integer);
begin
  RenderLine(FCur.x, FCur.y, x, y);

  FCur.x := x;
  FCur.y := y;

  FSorted := False;
end;

procedure TAggOutlineAA.Reset;
begin
  FNumCells := 0;
  FCurBlock := 0;

  FCurCell.x := $7FFF;
  FCurCell.y := $7FFF;
  FCurCell.Cover := 0;
  FCurCell.Area := 0;

  FSorted := False;

  FMin.x := $7FFFFFFF;
  FMin.y := $7FFFFFFF;
  FMax.x := -$7FFFFFFF;
  FMax.y := -$7FFFFFFF;
end;

procedure TAggOutlineAA.AddCurrentCell;
begin
  if (FCurCell.Area or FCurCell.Cover) <> 0 then
    begin
      if (FNumCells and CAggCellBlockMask) = 0 then
        begin
          if FNumBlocks >= CAggCellBlockLimit then
              Exit;

          AllocateBlock;
        end;

      FCurCellPointer^ := FCurCell;

      inc(PtrComp(FCurCellPointer), SizeOf(TAggCellAA));
      inc(FNumCells);

      if FCurCell.x < FMin.x then
          FMin.x := FCurCell.x;

      if FCurCell.x > FMax.x then
          FMax.x := FCurCell.x;

      if FCurCell.y < FMin.y then
          FMin.y := FCurCell.y;

      if FCurCell.y > FMax.y then
          FMax.y := FCurCell.y;
    end;
end;

procedure TAggOutlineAA.SetCurrentCell(x, y: Integer);
begin
  if (FCurCell.x <> x) or (FCurCell.y <> y) then
    begin
      AddCurrentCell;

      FCurCell.x := x;
      FCurCell.y := y;

      FCurCell.Cover := 0;
      FCurCell.Area := 0;
    end;
end;

procedure QuickSortCells(Start: PPAggCellAA; Num: Cardinal);
var
  Len, x: Integer;

  Stack: array [0 .. 79] of PPAggCellAA;
  Limit, Base: PPAggCellAA;
  i, j, Pivot: PPAggCellAA;
  Top: ^PPAggCellAA;
const
  CQSortThreshold = 9;
begin
  Limit := PPAggCellAA(PtrComp(Start) + Num * SizeOf(Pointer));
  Base := Start;
  Top := @Stack[0];

  repeat
    Len := (PtrComp(Limit) - PtrComp(Base)) div SizeOf(Pointer);

    if Len > CQSortThreshold then
      begin
        // we use base + len/2 as the pivot
        Pivot := PPAggCellAA(PtrComp(Base) + (Len div 2) * SizeOf(Pointer));

        SwapPointers(Base, Pivot);

        i := PPAggCellAA(PtrComp(Base) + SizeOf(Pointer));
        j := PPAggCellAA(PtrComp(Limit) - SizeOf(Pointer));

        // now ensure that *i <= *base <= *j
        if j^.x < i^.x then
            SwapPointers(j, i);

        if Base^.x < i^.x then
            SwapPointers(Base, i);

        if j^.x < Base^.x then
            SwapPointers(Base, j);

        repeat
          x := Base^.x;

          inc(PtrComp(i), SizeOf(PAggCellAA));

          while i^.x < x do
              inc(PtrComp(i), SizeOf(PAggCellAA));

          dec(PtrComp(j), SizeOf(PAggCellAA));

          while x < j^.x do
              dec(PtrComp(j), SizeOf(PAggCellAA));

          if PtrComp(i) > PtrComp(j) then
              Break;

          SwapPointers(i, j);
        until False;

        SwapPointers(Base, j);

        // now, push the largest sub-array
        if (PtrComp(j) - PtrComp(Base)) div SizeOf(Pointer) >
          (PtrComp(Limit) - PtrComp(i)) div SizeOf(Pointer)
        then
          begin
            Top^ := Base;

            inc(PtrComp(Top), SizeOf(PPAggCellAA));

            Top^ := j;
            Base := i;
          end
        else
          begin
            Top^ := i;

            inc(PtrComp(Top), SizeOf(PPAggCellAA));

            Top^ := Limit;
            Limit := j;
          end;

        inc(PtrComp(Top), SizeOf(PPAggCellAA));
      end
    else
      begin
        // the sub-array is small, perform insertion sort
        j := Base;
        i := PPAggCellAA(PtrComp(j) + SizeOf(Pointer));

        while PtrComp(i) < PtrComp(Limit) do
          begin
            while PPAggCellAA(PtrComp(j) + SizeOf(Pointer))^^.x < j^.x do
              begin
                SwapPointers(PPAggCellAA(PtrComp(j) + SizeOf(Pointer)), j);
                if j = Base then
                    Break;

                dec(j);
              end;

            j := i;

            inc(PtrComp(i), SizeOf(PAggCellAA));
          end;

        if PtrComp(Top) > PtrComp(@Stack[0]) then
          begin
            dec(PtrComp(Top), SizeOf(PPAggCellAA));

            Limit := Top^;

            dec(PtrComp(Top), SizeOf(PPAggCellAA));

            Base := Top^;
          end
        else
            Break;
      end;

  until False;
end;

procedure TAggOutlineAA.SortCells;
var
  nb, i, v, Start: Cardinal;
  CurY, CurMinY: PAggSortedY;

  BlockPtr: PPAggCellAA;
  CellPtr: PAggCellAA;
begin
  // Perform sort only the first time
  if FSorted then
      Exit;

  AddCurrentCell;

  if FNumCells = 0 then
      Exit;

  // Allocate the array of cell pointers
  FSortedCells.Allocate(FNumCells, 16);

  // Allocate and zero the Y array
  FSortedY.Allocate(FMax.y - FMin.y + 1, 16);
  FSortedY.Zero;

  // Create the Y-histogram (count the numbers of cells for each Y)
  BlockPtr := FCells;

  nb := FNumCells shr CAggCellBlockShift;

  CurMinY := PAggSortedY(FSortedY.ArrayPointer);
  dec(CurMinY, FMin.y);

  while nb > 0 do
    begin
      dec(nb);

      CellPtr := BlockPtr^;
      inc(BlockPtr);
      i := CAggCellBlockSize;

      while i > 0 do
        begin
          dec(i);
          CurY := CurMinY;
          inc(CurY, CellPtr^.y);
          inc(CurY^.Start);
          inc(CellPtr);
        end;
    end;

  CellPtr := BlockPtr^;

  inc(BlockPtr);

  i := FNumCells and CAggCellBlockMask;

  while i > 0 do
    begin
      dec(i);
      CurY := CurMinY;
      inc(CurY, CellPtr^.y);
      inc(CurY^.Start);
      inc(CellPtr);
    end;

  // Convert the Y-histogram into the array of starting indexes
  Start := 0;

  CurY := PAggSortedY(FSortedY.ArrayPointer);
  for i := 0 to FSortedY.Size - 1 do
    begin
      v := CurY^.Start;

      CurY^.Start := Start;
      inc(CurY);

      inc(Start, v);
    end;

  // Fill the cell pointer array sorted by Y
  BlockPtr := FCells;

  nb := FNumCells shr CAggCellBlockShift;

  while nb > 0 do
    begin
      dec(nb);

      CellPtr := BlockPtr^;

      inc(BlockPtr);

      i := CAggCellBlockSize;

      while i > 0 do
        begin
          dec(i);

          CurY := CurMinY;
          inc(CurY, CellPtr.y);

          PPointer(PtrComp(FSortedCells.ArrayPointer) +
            Cardinal(CurY.Start + CurY.Num) * FSortedCells.EntrySize)^ := CellPtr;

          inc(CurY.Num);
          inc(CellPtr);
        end;
    end;

  CellPtr := BlockPtr^;
  inc(BlockPtr);
  i := FNumCells and CAggCellBlockMask;

  while i > 0 do
    begin
      dec(i);

      CurY := CurMinY;
      inc(CurY, CellPtr.y);

      PPointer(PtrComp(FSortedCells.ArrayPointer) +
        Cardinal(CurY.Start + CurY.Num) * FSortedCells.EntrySize)^ := CellPtr;

      inc(CurY.Num);
      inc(CellPtr);
    end;

  // Finally arrange the X-arrays
  CurY := PAggSortedY(FSortedY.ArrayPointer);
  for i := 0 to FSortedY.Size - 1 do
    begin
      if CurY.Num > 0 then
          QuickSortCells(PPAggCellAA(PtrComp(FSortedCells.ArrayPointer) + CurY.Start
          * FSortedCells.EntrySize), CurY.Num);
      inc(CurY);
    end;

  FSorted := True;
end;

function TAggOutlineAA.ScanLineNumCells(y: Cardinal): Cardinal;
begin
  Result := PAggSortedY(PtrComp(FSortedY.ArrayPointer) + Cardinal(y - FMin.y) *
    FSortedY.EntrySize).Num;
end;

function TAggOutlineAA.ScanLineCells(y: Cardinal): PPAggCellAA;
begin
  Result := PPAggCellAA(PtrComp(FSortedCells.ArrayPointer) +
    PAggSortedY(PtrComp(FSortedY.ArrayPointer) + Cardinal(y - FMin.y) *
    FSortedY.EntrySize).Start * FSortedCells.EntrySize);
end;

procedure TAggOutlineAA.RenderLine(x1, y1, x2, y2: Integer);
var
  center, Delta: TPointInteger;
  p, EX, Ey1, Ey2, FY1, FY2, Rem, DeltaMod,
    FromX, tox, Lift, DeltaVal, First, Incr, TwoFX, Area: Integer;
const
  CDxLimit = 16384 shl CAggPolyBaseShift;
begin
  Delta.x := x2 - x1;

  if (Delta.x >= CDxLimit) or (Delta.x <= -CDxLimit) then
    begin
      center.x := (x1 + x2) shr 1;
      center.y := (y1 + y2) shr 1;

      RenderLine(x1, y1, center.x, center.y);
      RenderLine(center.x, center.y, x2, y2);
    end;

  Delta.y := y2 - y1;

  // ey1:=y1 shr CAggPolyBaseShift;
  // ey2:=y2 shr CAggPolyBaseShift;
  Ey1 := ShrInt32(y1, CAggPolyBaseShift);
  Ey2 := ShrInt32(y2, CAggPolyBaseShift);

  FY1 := y1 and CAggPolyBaseMask;
  FY2 := y2 and CAggPolyBaseMask;

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

  if Delta.x = 0 then
    begin
      // Ex := x1 shr CAggPolyBaseShift;
      EX := ShrInt32(x1, CAggPolyBaseShift);

      TwoFX := (x1 - (EX shl CAggPolyBaseShift)) shl 1;
      First := CAggPolyBaseSize;

      if Delta.y < 0 then
        begin
          First := 0;
          Incr := -1;
        end;

      FromX := x1;

      // RenderHorizontalLine(ey1 ,FromX ,fy1 ,FromX ,first );
      DeltaVal := First - FY1;

      inc(FCurCell.Cover, DeltaVal);
      inc(FCurCell.Area, TwoFX * DeltaVal);
      inc(Ey1, Incr);

      SetCurrentCell(EX, Ey1);

      DeltaVal := First + First - CAggPolyBaseSize;
      Area := TwoFX * DeltaVal;

      while Ey1 <> Ey2 do
        begin
          // RenderHorizontalLine(ey1 ,FromX ,CAggPolyBaseSize - first ,FromX ,first );
          FCurCell.Cover := DeltaVal;
          FCurCell.Area := Area;

          inc(Ey1, Incr);

          SetCurrentCell(EX, Ey1);
        end;

      // RenderHorizontalLine(ey1, FromX, CAggPolyBaseSize - first, FromX, fy2);
      DeltaVal := FY2 - CAggPolyBaseSize + First;

      inc(FCurCell.Cover, DeltaVal);
      inc(FCurCell.Area, TwoFX * DeltaVal);

      Exit;
    end;

  // ok, we have to render several HorizontalLines
  p := (CAggPolyBaseSize - FY1) * Delta.x;
  First := CAggPolyBaseSize;

  if Delta.y < 0 then
    begin
      p := FY1 * Delta.x;
      First := 0;
      Incr := -1;
      Delta.y := -Delta.y;
    end;

  DeltaVal := p div Delta.y;
  DeltaMod := p mod Delta.y;

  if DeltaMod < 0 then
    begin
      dec(DeltaVal);
      inc(DeltaMod, Delta.y);
    end;

  FromX := x1 + DeltaVal;

  RenderHorizontalLine(Ey1, x1, FY1, FromX, First);

  inc(Ey1, Incr);

  // SetCurrentCell(FromX shr CAggPolyBaseShift ,ey1 );
  SetCurrentCell(ShrInt32(FromX, CAggPolyBaseShift), Ey1);

  if Ey1 <> Ey2 then
    begin
      p := CAggPolyBaseSize * Delta.x;
      Lift := p div Delta.y;
      Rem := p mod Delta.y;

      if Rem < 0 then
        begin
          dec(Lift);
          inc(Rem, Delta.y);
        end;

      dec(DeltaMod, Delta.y);

      while Ey1 <> Ey2 do
        begin
          DeltaVal := Lift;

          inc(DeltaMod, Rem);

          if DeltaMod >= 0 then
            begin
              dec(DeltaMod, Delta.y);
              inc(DeltaVal);
            end;

          tox := FromX + DeltaVal;

          RenderHorizontalLine(Ey1, FromX, CAggPolyBaseSize - First, tox, First);

          FromX := tox;

          inc(Ey1, Incr);

          // SetCurrentCell(FromX shr CAggPolyBaseShift ,ey1 );
          SetCurrentCell(ShrInt32(FromX, CAggPolyBaseShift), Ey1);
        end;
    end;

  RenderHorizontalLine(Ey1, FromX, CAggPolyBaseSize - First, x2, FY2);
end;

procedure TAggOutlineAA.RenderHorizontalLine(EY, x1, y1, x2, y2: Integer);
var
  p, deltax, Ex1, Ex2, FX1, FX2: Integer;
  Delta, First, Incr, Lift, DeltaMod, Rem: Integer;
begin
  Ex1 := ShrInt32(x1, CAggPolyBaseShift);
  Ex2 := ShrInt32(x2, CAggPolyBaseShift);

  // trivial case. Happens often
  if y1 = y2 then
    begin
      SetCurrentCell(Ex2, EY);

      Exit;
    end;

  FX1 := x1 and CAggPolyBaseMask;
  FX2 := x2 and CAggPolyBaseMask;

  // everything is located in a single cell.  That is easy!
  if Ex1 = Ex2 then
    begin
      Delta := y2 - y1;

      inc(FCurCell.Cover, Delta);
      inc(FCurCell.Area, (FX1 + FX2) * Delta);

      Exit;
    end;

  // ok, we'll have to render a run of adjacent cells on the same
  // HorizontalLine...
  p := (CAggPolyBaseSize - FX1) * (y2 - y1);
  First := CAggPolyBaseSize;
  Incr := 1;
  deltax := x2 - x1;

  if deltax < 0 then
    begin
      p := FX1 * (y2 - y1);
      First := 0;
      Incr := -1;
      deltax := -deltax;
    end;

  Delta := p div deltax;
  DeltaMod := p mod deltax;

  if DeltaMod < 0 then
    begin
      dec(Delta);
      inc(DeltaMod, deltax);
    end;

  inc(FCurCell.Cover, Delta);
  inc(FCurCell.Area, (FX1 + First) * Delta);

  inc(Ex1, Incr);

  SetCurrentCell(Ex1, EY);

  inc(y1, Delta);

  if Ex1 <> Ex2 then
    begin
      p := CAggPolyBaseSize * (y2 - y1 + Delta);
      Lift := p div deltax;
      Rem := p mod deltax;

      if Rem < 0 then
        begin
          dec(Lift);
          inc(Rem, deltax);
        end;

      dec(DeltaMod, deltax);

      while Ex1 <> Ex2 do
        begin
          Delta := Lift;

          inc(DeltaMod, Rem);

          if DeltaMod >= 0 then
            begin
              dec(DeltaMod, deltax);
              inc(Delta);
            end;

          inc(FCurCell.Cover, Delta);
          inc(FCurCell.Area, (CAggPolyBaseSize) * Delta);
          inc(y1, Delta);
          inc(Ex1, Incr);

          SetCurrentCell(Ex1, EY);
        end;
    end;

  Delta := y2 - y1;

  inc(FCurCell.Cover, Delta);
  inc(FCurCell.Area, (FX2 + CAggPolyBaseSize - First) * Delta);
end;

procedure TAggOutlineAA.AllocateBlock;
var
  NewCells: PPAggCellAA;
begin
  if FCurBlock >= FNumBlocks then
    begin
      if FNumBlocks >= FMaxBlocks then
        begin
          AggGetMem(Pointer(NewCells), SizeOf(PAggCellAA) *
            (FMaxBlocks + CAggCellBlockPool));

          if FCells <> nil then
            begin
              Move(FCells^, NewCells^, SizeOf(PAggCellAA) * FMaxBlocks);

              AggFreeMem(Pointer(FCells), SizeOf(PAggCellAA) * FMaxBlocks);
            end;

          FCells := NewCells;

          inc(FMaxBlocks, CAggCellBlockPool);
        end;

      AggGetMem(Pointer(Pointer(PtrComp(FCells) + FNumBlocks *
        SizeOf(PAggCellAA))^), CAggCellBlockSize * SizeOf(TAggCellAA));

      inc(FNumBlocks);
    end;

  FCurCellPointer := PPAggCellAA(PtrComp(FCells) + FCurBlock *
    SizeOf(PAggCellAA))^;

  inc(FCurBlock);
end;

{ TAggScanLineHitTest }

constructor TAggScanLineHitTest.Create;
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

{ TAggRasterizerScanLineAA }

constructor TAggRasterizerScanLineAA.Create;
var
  i: Integer;
begin
  FOutline := TAggOutlineAA.Create;

  FFillingRule := frNonZero;
  FAutoClose := True;

  FClippedStart := PointInteger(0);
  FStart := PointInteger(0);
  FPrev := PointInteger(0);

  FPrevFlags := 0;
  FStatus := siStatusInitial;
  FClipping := False;

  for i := 0 to CAggAntiAliasingNum - 1 do
      FGamma[i] := i;

  FXScale := 1;
end;

destructor TAggRasterizerScanLineAA.Destroy;
begin
  FOutline.Free;
  inherited;
end;

procedure TAggRasterizerScanLineAA.Reset;
begin
  FOutline.Reset;

  FStatus := siStatusInitial;
end;

procedure TAggRasterizerScanLineAA.SetClipBox(x1, y1, x2, y2: Double);
begin
  Reset;

  FClipBox := PolyCoord(RectDouble(x1, y1, x2, y2));
  FClipBox.Normalize;

  FClipping := True;
end;

procedure TAggRasterizerScanLineAA.SetClipBox(Rect: TRectDouble);
begin
  Reset;

  FClipBox := PolyCoord(Rect);
  FClipBox.Normalize;

  FClipping := True;
end;

procedure TAggRasterizerScanLineAA.SetFillingRule(Value: TAggFillingRule);
begin
  FFillingRule := Value;
end;

procedure TAggRasterizerScanLineAA.AutoClose(flag: Boolean);
begin
  FAutoClose := flag;
end;

procedure TAggRasterizerScanLineAA.Gamma(AGammaFunction: TAggCustomVertexSource);
var
  i: Integer;
begin
  for i := 0 to CAggAntiAliasingNum - 1 do
      FGamma[i] := Trunc(AGammaFunction.FuncOperatorGamma(
      i / CAggAntiAliasingMask) * CAggAntiAliasingMask + 0.5);
end;

function TAggRasterizerScanLineAA.ApplyGamma(Cover: Cardinal): Cardinal;
begin
  Result := FGamma[Cover];
end;

procedure TAggRasterizerScanLineAA.MoveToNoClip(x, y: Integer);
begin
  if (FStatus = siStatusLineTo) and FAutoClose then
      ClosePolygonNoClip;

  FOutline.MoveTo(x * FXScale, y);

  FClippedStart := PointInteger(x, y);

  FStatus := siStatusLineTo;
end;

procedure TAggRasterizerScanLineAA.MoveToNoClip(Point: TPointInteger);
begin
  if (FStatus = siStatusLineTo) and FAutoClose then
      ClosePolygonNoClip;

  FOutline.MoveTo(Point.x * FXScale, Point.y);

  FClippedStart := Point;

  FStatus := siStatusLineTo;
end;

procedure TAggRasterizerScanLineAA.LineToNoClip(x, y: Integer);
begin
  if FStatus <> siStatusInitial then
    begin
      FOutline.LineTo(x * FXScale, y);

      FStatus := siStatusLineTo;
    end;
end;

procedure TAggRasterizerScanLineAA.LineToNoClip(Point: TPointInteger);
begin
  if FStatus <> siStatusInitial then
    begin
      FOutline.LineTo(Point.x * FXScale, Point.y);

      FStatus := siStatusLineTo;
    end;
end;

procedure TAggRasterizerScanLineAA.ClosePolygon;
begin
  if FClipping then
      ClipSegment(FStart);

  if FAutoClose then
      ClosePolygonNoClip;
end;

procedure TAggRasterizerScanLineAA.ClosePolygonNoClip;
begin
  if FStatus = siStatusLineTo then
    begin
      FOutline.LineTo(FClippedStart.x * FXScale, FClippedStart.y);

      FStatus := siStatusClosed;
    end;
end;

procedure TAggRasterizerScanLineAA.ClipSegment(x, y: Integer);
var
  Flags, n: Cardinal;

  center: array [0 .. 3] of TPointInteger;
  Pnt: PPointInteger;
begin
  Flags := ClippingFlagsInteger(x, y, FClipBox);

  if FPrevFlags = Flags then
    if Flags = 0 then
      if FStatus = siStatusInitial then
          MoveToNoClip(x, y)
      else
          LineToNoClip(x, y)
    else
  else
    begin
      n := ClipLiangBarskyInteger(FPrev.x, FPrev.y, x, y, FClipBox, @center[0]);

      Pnt := @center[0];

      while n > 0 do
        begin
          if FStatus = siStatusInitial then
              MoveToNoClip(Pnt^)
          else
              LineToNoClip(Pnt^);

          inc(Pnt);
          dec(n);
        end;
    end;

  FPrevFlags := Flags;
  FPrev := PointInteger(x, y);
end;

procedure TAggRasterizerScanLineAA.ClipSegment(Point: TPointInteger);
var
  Flags, n: Cardinal;

  center: array [0 .. 3] of TPointInteger;
  Pnt: PPointInteger;
begin
  Flags := ClippingFlagsInteger(Point.x, Point.y, FClipBox);

  if FPrevFlags = Flags then
    if Flags = 0 then
      if FStatus = siStatusInitial then
          MoveToNoClip(Point)
      else
          LineToNoClip(Point)
    else
  else
    begin
      n := ClipLiangBarskyInteger(FPrev.x, FPrev.y, Point.x, Point.y, FClipBox,
        @center[0]);

      Pnt := @center[0].x;

      while n > 0 do
        begin
          if FStatus = siStatusInitial then
              MoveToNoClip(Pnt^)
          else
              LineToNoClip(Pnt^);

          inc(Pnt);
          dec(n);
        end;
    end;

  FPrevFlags := Flags;
  FPrev := Point;
end;

procedure TAggRasterizerScanLineAA.MoveToDouble(x, y: Double);
begin
  MoveTo(PolyCoord(x), PolyCoord(y));
end;

procedure TAggRasterizerScanLineAA.LineToDouble(x, y: Double);
begin
  LineTo(PolyCoord(x), PolyCoord(y));
end;

procedure TAggRasterizerScanLineAA.MoveToDouble(Point: TPointDouble);
begin
  MoveTo(PolyCoord(Point));
end;

procedure TAggRasterizerScanLineAA.LineToDouble(Point: TPointDouble);
begin
  LineTo(PolyCoord(Point));
end;

procedure TAggRasterizerScanLineAA.MoveTo(Point: TPointInteger);
begin
  if FClipping then
    begin
      if FOutline.Sorted then
          Reset;

      if (FStatus = siStatusLineTo) and FAutoClose then
          ClosePolygon;

      FPrev := Point;
      FStart := Point;
      FStatus := siStatusInitial;

      FPrevFlags := ClippingFlagsInteger(Point.x, Point.y, FClipBox);

      if FPrevFlags = 0 then
          MoveToNoClip(Point);
    end
  else
      MoveToNoClip(Point);
end;

procedure TAggRasterizerScanLineAA.MoveTo(x, y: Integer);
begin
  if FClipping then
    begin
      if FOutline.Sorted then
          Reset;

      if (FStatus = siStatusLineTo) and FAutoClose then
          ClosePolygon;

      FPrev := PointInteger(x, y);
      FStart := PointInteger(x, y);
      FStatus := siStatusInitial;

      FPrevFlags := ClippingFlagsInteger(x, y, FClipBox);

      if FPrevFlags = 0 then
          MoveToNoClip(x, y);
    end
  else
      MoveToNoClip(x, y);
end;

procedure TAggRasterizerScanLineAA.LineTo(Point: TPointInteger);
begin
  if FClipping then
      ClipSegment(Point)
  else
      LineToNoClip(Point);
end;

procedure TAggRasterizerScanLineAA.LineTo(x, y: Integer);
begin
  if FClipping then
      ClipSegment(x, y)
  else
      LineToNoClip(x, y);
end;

procedure TAggRasterizerScanLineAA.Sort;
begin
  FOutline.SortCells;
end;

function TAggRasterizerScanLineAA.RewindScanLines: Boolean;
begin
  if FAutoClose then
      ClosePolygon;

  FOutline.SortCells;

  if FOutline.TotalCells = 0 then
    begin
      Result := False;

      Exit;
    end;

  FCurY := FOutline.MinY;
  Result := True;
end;

function TAggRasterizerScanLineAA.SweepScanLine(SL: TAggCustomScanLine): Boolean;
var
  x, Area: Integer;
  Cover: Integer;
  alpha: Cardinal;
  Cells: PPAggCellAA;

  CurCell: PAggCellAA;
  NumCells: Cardinal;
begin
  repeat
    if FCurY > FOutline.MaxY then
      begin
        Result := False;

        Exit;
      end;

    SL.ResetSpans;

    NumCells := FOutline.ScanLineNumCells(FCurY);
    Cells := FOutline.ScanLineCells(FCurY);

    Cover := 0;

    while NumCells > 0 do
      begin
        CurCell := Cells^;

        x := CurCell.x;
        Area := CurCell.Area;

        inc(Cover, CurCell.Cover);

        // accumulate all cells with the same X
        dec(NumCells);

        while NumCells > 0 do
          begin
            inc(Cells);

            CurCell := Cells^;

            if CurCell.x <> x then
                Break;

            inc(Area, CurCell.Area);
            inc(Cover, CurCell.Cover);

            dec(NumCells);
          end;

        if Area <> 0 then
          begin
            alpha := CalculateAlpha((Cover shl (CAggPolyBaseShift + 1)) - Area);

            if alpha <> 0 then
                SL.AddCell(x, alpha);

            inc(x);
          end;

        if (NumCells <> 0) and (CurCell.x > x) then
          begin
            alpha := CalculateAlpha(Cover shl (CAggPolyBaseShift + 1));

            if alpha <> 0 then
                SL.AddSpan(x, CurCell.x - x, alpha);
          end;
      end;

    if Boolean(SL.NumSpans) then
        Break;

    inc(FCurY);
  until False;

  SL.Finalize(FCurY);

  inc(FCurY);

  Result := True;
end;

function TAggRasterizerScanLineAA.NavigateScanLine(y: Integer): Boolean;
begin
  if FAutoClose then
      ClosePolygon;

  FOutline.SortCells;

  if (FOutline.TotalCells = 0) or (y < FOutline.MinY) or
    (y > FOutline.MaxY) then
    begin
      Result := False;

      Exit;
    end;

  FCurY := y;
  Result := True;
end;

function TAggRasterizerScanLineAA.HitTest(TX, TY: Integer): Boolean;
var
  SL: TAggScanLineHitTest;
begin
  if not NavigateScanLine(TY) then
    begin
      Result := False;

      Exit;
    end;

  SL := TAggScanLineHitTest.Create(TX);
  try
    SweepScanLine(SL);

    Result := SL.Hit;
  finally
      SL.Free
  end;
end;

function TAggRasterizerScanLineAA.GetMinX;
begin
  Result := FOutline.MinX;
end;

function TAggRasterizerScanLineAA.GetMinY;
begin
  Result := FOutline.MinY;
end;

function TAggRasterizerScanLineAA.GetFillingRule: TAggFillingRule;
begin
  Result := FFillingRule
end;

function TAggRasterizerScanLineAA.GetMaxX;
begin
  Result := FOutline.MaxX;
end;

function TAggRasterizerScanLineAA.GetMaxY;
begin
  Result := FOutline.MaxY;
end;

function TAggRasterizerScanLineAA.CalculateAlpha(Area: Integer): Cardinal;
var
  Cover: System.Integer;
begin
  // 1: cover:=area shr (CAggPolyBaseShift * 2 + 1 - CAggAntiAliasingShift );
  // 2: cover:=round(area / (1 shl (CAggPolyBaseShift * 2 + 1 - CAggAntiAliasingShift ) ) );
  Cover := ShrInt32(Area, CAggPolyBaseShift shl 1 + 1 - CAggAntiAliasingShift);

  if Cover < 0 then
      Cover := -Cover;

  if FFillingRule = frEvenOdd then
    begin
      Cover := Cover and CAggAntiAliasing2Mask;

      if Cover > CAggAntiAliasingNum then
          Cover := CAggAntiAliasing2Num - Cover;
    end;

  if Cover > CAggAntiAliasingMask then
      Cover := CAggAntiAliasingMask;

  Result := FGamma[Cover];
end;

procedure TAggRasterizerScanLineAA.AddPath;
var
  Cmd: Cardinal;
  x, y: Double;
begin
  Vs.Rewind(PathID);

  Cmd := Vs.Vertex(@x, @y);

  while not IsStop(Cmd) do
    begin
      AddVertex(x, y, Cmd);

      Cmd := Vs.Vertex(@x, @y);
    end;
end;

procedure TAggRasterizerScanLineAA.AddVertex;
begin
  if IsClose(Cmd) then
      ClosePolygon
  else if IsMoveTo(Cmd) then
      MoveTo(PolyCoord(x), PolyCoord(y))
  else if IsVertex(Cmd) then
      LineTo(PolyCoord(x), PolyCoord(y));
end;

end. 
 
 
