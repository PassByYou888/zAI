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
unit AggRasterizerCompoundAA;

{$DEFINE FPC_DELPHI_MODE}
{$INCLUDE zDefine.inc}
interface
uses
  AggBasics,
  AggArray,
  AggRasterizerCellsAA,
  AggRasterizerScanLine,
  AggRasterizerScanlineClip,
  AggVertexSource,
  AggScanline;

const
  CAggAntiAliasingShift  = 8;
  CAggAntiAliasingScale  = 1 shl CAggAntiAliasingShift;
  CAggAntiAliasingMask   = CAggAntiAliasingScale - 1;
  CAggAntiAliasingScale2 = CAggAntiAliasingScale * 2;
  CAggAntiAliasingMask2  = CAggAntiAliasingScale2 - 1;

type
  TAggLayerOrder = (loUnsorted, loDirect, loInverse);

  PAggStyleInfo = ^TAggStyleInfo;

  TAggStyleInfo = record
    StartCell, NumCells: Cardinal;
    LastX: Integer;
  end;

  PAggCellInfo = ^TAggCellInfo;

  TAggCellInfo = record
    x, Area, Cover: Integer;
  end;

  TAggRasterizerCompoundAA = class(TAggCustomBoundsRasterizerScanLine)
  private
    FRasterizerConverter: TAggRasterizerConv;

    FOutline: TAggRasterizerCellsAA;
    FClipper: TAggRasterizerScanLineClip;

    FFillingRule: TAggFillingRule;
    FLayerOrder: TAggLayerOrder;

    FStyles,           // Active Styles
    FActiveStyleTable, // Active Style Table (unique values)
    FActiveStyleMask,  // Active Style Mask
    FCells, FCoverBuffer: TAggPodVector;
    FMasterAlpha: TAggPodBVector;

    FMinStyle, FMaxStyle, FScanY, FScanLineStart: Integer;
    FScanLineLength: Cardinal;
    procedure AddStyle(StyleID: Integer);
    procedure AllocateMasterAlpha;
  protected
    function GetMinX: Integer; override;
    function GetMinY: Integer; override;
    function GetMaxX: Integer; override;
    function GetMaxY: Integer; override;
    function GetFillingRule: TAggFillingRule; override;
    procedure SetFillingRule(Value: TAggFillingRule); override;
  public
    constructor Create(Clip: TAggRasterizerScanLineClip);
    destructor Destroy; override;

    procedure Reset; override;
    procedure ResetClipping;
    procedure SetClipBox(x1, y1, x2, y2: Double); override;
    procedure SetClipBox(Rect: TRectDouble); override;

    procedure LayerOrder(Order: TAggLayerOrder);
    procedure MasterAlpha(AStyle: Integer; alpha: Double);

    procedure Styles(Left, Right: Integer);
    procedure MoveTo(x, y: Integer); virtual; abstract;
    procedure LineTo(x, y: Integer);

    procedure MoveToDouble(x, y: Double); virtual; abstract;
    procedure LineToDouble(x, y: Double);

    procedure EdgeInteger(x1, y1, x2, y2: Integer);
    procedure EdgeDouble(x1, y1, x2, y2: Double);

    procedure AddPath(Vs: TAggCustomVertexSource; PathID: Cardinal = 0); override;

    function MinStyle: Integer;
    function MaxStyle: Integer;

    procedure Sort; override;
    function RewindScanLines: Boolean; override;
    function SweepStyles: Cardinal;
    function ScanLineStart: Integer;
    function ScanLineLength: Cardinal;
    function Style(StyleIndex: Cardinal): Cardinal;

    function AllocateCoverBuffer(Len: Cardinal): PCover;

    function NavigateScanLine(y: Integer): Boolean;

    function HitTest(TX, TY: Integer): Boolean; override;

    function CalculateAlpha(Area: Integer; MasterAlpha: Cardinal): Cardinal;
    function SweepScanLine(SL: TAggCustomScanLine; StyleIndex: Integer): Boolean;
  end;

  TAggRasterizerCompoundAAInteger = class(TAggRasterizerCompoundAA)
  private
    FRasterizerScanLineClip: TAggRasterizerScanLineClipInt;
    FStart: TPointInteger;
  public
    constructor Create(Clip: TAggRasterizerScanLineClip = nil);
    destructor Destroy; override;

    procedure MoveTo(x, y: Integer); override;
    procedure MoveToDouble(x, y: Double); override;

    procedure AddVertex(x, y: Double; Cmd: Cardinal); override;
  end;

  TAggRasterizerCompoundAADouble = class(TAggRasterizerCompoundAA)
  private
    FRasterizerScanLineClip: TAggRasterizerScanLineDoubleClip;
    FOwnsRasterizer: Boolean;
    FStart: TPointDouble;
  public
    constructor Create(Clip: TAggRasterizerScanLineClip = nil);
    destructor Destroy; override;

    procedure MoveTo(x, y: Integer); override;
    procedure MoveToDouble(x, y: Double); override;

    procedure AddVertex(x, y: Double; Cmd: Cardinal); override;
  end;

implementation


{ TAggRasterizerCompoundAA }

constructor TAggRasterizerCompoundAA.Create(Clip: TAggRasterizerScanLineClip);
begin
  FClipper := Clip;
  FRasterizerConverter := FClipper.ConverterType;

  FOutline := TAggRasterizerCellsAA.Create;

  FFillingRule := frNonZero;
  FLayerOrder := loDirect;

  FStyles := TAggPodVector.Create(SizeOf(TAggStyleInfo));      // Active Styles
  FActiveStyleTable := TAggPodVector.Create(SizeOf(Cardinal)); // Active Style Table (unique values)
  FActiveStyleMask := TAggPodVector.Create(SizeOf(Int8u));     // Active Style Mask
  FCells := TAggPodVector.Create(SizeOf(TAggCellInfo));
  FCoverBuffer := TAggPodVector.Create(SizeOf(TCover));
  FMasterAlpha := TAggPodBVector.Create(SizeOf(Cardinal));

  FMinStyle := $7FFFFFFF;
  FMaxStyle := -$7FFFFFFF;
  FScanY := $7FFFFFFF;
  FScanLineStart := 0;
  FScanLineLength := 0;
end;

destructor TAggRasterizerCompoundAA.Destroy;
begin
  FOutline.Free;
  FStyles.Free;
  FActiveStyleTable.Free;
  FActiveStyleMask.Free;
  FCells.Free;
  FCoverBuffer.Free;
  FMasterAlpha.Free;

  inherited;
end;

procedure TAggRasterizerCompoundAA.Reset;
begin
  FOutline.Reset;

  FMinStyle := $7FFFFFFF;
  FMaxStyle := -$7FFFFFFF;
  FScanY := $7FFFFFFF;
  FScanLineStart := 0;
  FScanLineLength := 0;
end;

procedure TAggRasterizerCompoundAA.ResetClipping;
begin
  Reset;
  FClipper.ResetClipping;
end;

procedure TAggRasterizerCompoundAA.SetClipBox(x1, y1, x2, y2: Double);
begin
  Reset;
  FClipper.SetClipBox(FRasterizerConverter.Upscale(x1),
    FRasterizerConverter.Upscale(y1), FRasterizerConverter.Upscale(x2),
    FRasterizerConverter.Upscale(y2));
end;

procedure TAggRasterizerCompoundAA.SetClipBox(Rect: TRectDouble);
begin
  Reset;
  FClipper.SetClipBox(FRasterizerConverter.Upscale(Rect.x1),
    FRasterizerConverter.Upscale(Rect.y1),
    FRasterizerConverter.Upscale(Rect.x2),
    FRasterizerConverter.Upscale(Rect.y2));
end;

procedure TAggRasterizerCompoundAA.SetFillingRule(Value: TAggFillingRule);
begin
  FFillingRule := Value;
end;

function TAggRasterizerCompoundAA.GetFillingRule: TAggFillingRule;
begin
  Result := FFillingRule;
end;

procedure TAggRasterizerCompoundAA.LayerOrder(Order: TAggLayerOrder);
begin
  FLayerOrder := Order;
end;

procedure TAggRasterizerCompoundAA.MasterAlpha(AStyle: Integer; alpha: Double);
var
  Uv: Cardinal;
begin
  if AStyle >= 0 then
    begin
      Uv := CAggAntiAliasingMask;

      while Integer(FMasterAlpha.Size) <= AStyle do
          FMasterAlpha.Add(@Uv);

      PCardinal(FMasterAlpha[AStyle])^ :=
        UnsignedRound(alpha * CAggAntiAliasingMask);
    end;
end;

procedure TAggRasterizerCompoundAA.Styles(Left, Right: Integer);
var
  Cell: TAggCellStyleAA;
begin
  Cell.Initial;

  Cell.Left := Int16(Left);
  Cell.Right := Int16(Right);

  FOutline.Style(@Cell);

  if (Left >= 0) and (Left < FMinStyle) then
      FMinStyle := Left;

  if (Left >= 0) and (Left > FMaxStyle) then
      FMaxStyle := Left;

  if (Right >= 0) and (Right < FMinStyle) then
      FMinStyle := Right;

  if (Right >= 0) and (Right > FMaxStyle) then
      FMaxStyle := Right;
end;

procedure TAggRasterizerCompoundAA.LineTo(x, y: Integer);
begin
  FClipper.LineTo(FOutline, FRasterizerConverter.Downscale(x),
    FRasterizerConverter.Downscale(y));
end;

procedure TAggRasterizerCompoundAA.LineToDouble(x, y: Double);
begin
  FClipper.LineTo(FOutline, FRasterizerConverter.Upscale(x),
    FRasterizerConverter.Upscale(y));
end;

procedure TAggRasterizerCompoundAA.EdgeInteger(x1, y1, x2, y2: Integer);
begin
  if FOutline.Sorted then
      Reset;

  FClipper.MoveTo(FRasterizerConverter.Downscale(x1),
    FRasterizerConverter.Downscale(y1));
  FClipper.LineTo(FOutline, FRasterizerConverter.Downscale(x2),
    FRasterizerConverter.Downscale(y2));
end;

procedure TAggRasterizerCompoundAA.EdgeDouble(x1, y1, x2, y2: Double);
begin
  if FOutline.Sorted then
      Reset;

  FClipper.MoveTo(FRasterizerConverter.Upscale(x1),
    FRasterizerConverter.Upscale(y1));
  FClipper.LineTo(FOutline, FRasterizerConverter.Upscale(x2),
    FRasterizerConverter.Upscale(y2));
end;

procedure TAggRasterizerCompoundAA.AddPath(Vs: TAggCustomVertexSource;
  PathID: Cardinal = 0);
var
  x, y: Double;
  Cmd: Cardinal;
begin
  Vs.Rewind(PathID);

  if FOutline.Sorted then
      Reset;

  Cmd := Vs.Vertex(@x, @y);

  while not IsStop(Cmd) do
    begin
      AddVertex(x, y, Cmd);

      Cmd := Vs.Vertex(@x, @y);
    end;
end;

function TAggRasterizerCompoundAA.GetMinX: Integer;
begin
  Result := FOutline.MinX;
end;

function TAggRasterizerCompoundAA.GetMinY: Integer;
begin
  Result := FOutline.MinY;
end;

function TAggRasterizerCompoundAA.GetMaxX: Integer;
begin
  Result := FOutline.MaxX;
end;

function TAggRasterizerCompoundAA.GetMaxY: Integer;
begin
  Result := FOutline.MaxY;
end;

function TAggRasterizerCompoundAA.MinStyle: Integer;
begin
  Result := FMinStyle
end;

function TAggRasterizerCompoundAA.MaxStyle: Integer;
begin
  Result := FMaxStyle
end;

procedure TAggRasterizerCompoundAA.Sort;
begin
  FOutline.SortCells;
end;

function TAggRasterizerCompoundAA.RewindScanLines: Boolean;
begin
  FOutline.SortCells;

  if FOutline.TotalCells = 0 then
    begin
      Result := False;

      Exit;
    end;

  if FMaxStyle < FMinStyle then
    begin
      Result := False;

      Exit;
    end;

  FScanY := FOutline.MinY;

  FStyles.Allocate(FMaxStyle - FMinStyle + 2, 128);

  AllocateMasterAlpha;

  Result := True;
end;

function TAggRasterizerCompoundAA.SweepStyles: Cardinal;
var
  NumCells, NumStyles, i, StartCell, Uv, v, StyleID: Cardinal;

  CurrCell: PAggCellStyleAA;
  TempStyle, st: PAggStyleInfo;

  Cells: PPAggCellStyleAA;
  Cell: PAggCellInfo;

  RA: TAggRangeAdaptor;

begin
  repeat
    if FScanY > FOutline.MaxY then
      begin
        Result := 0;

        Exit;
      end;

    NumCells := FOutline.ScanLineNumCells(FScanY);
    Cells := FOutline.ScanLineCells(FScanY);
    NumStyles := FMaxStyle - FMinStyle + 2;

    FCells.Allocate(NumCells * 2, 256); // Each cell can have two styles
    FActiveStyleTable.Capacity(NumStyles, 64);
    FActiveStyleMask.Allocate(ShrInt32(NumStyles + 7, 3), 8);
    FActiveStyleMask.Zero;

    if NumCells <> 0 then
      begin
        // Pre-add zero (for no-fill style, that is, -1).
        // We need that to ensure that the "-1 style" would go first.
        PInt8u(FActiveStyleMask[0])^ :=
          PInt8u(FActiveStyleMask[0])^ or 1;

        Uv := 0;

        FActiveStyleTable.Add(@Uv);

        TempStyle := FStyles[0];

        TempStyle.StartCell := 0;
        TempStyle.NumCells := 0;
        TempStyle.LastX := -$7FFFFFFF;

        FScanLineStart := Cells^^.x;
        FScanLineLength := PPAggCellStyleAA(PtrComp(Cells) + (NumCells - 1) *
          SizeOf(PAggCellStyleAA))^^.x - FScanLineStart + 1;

        while NumCells <> 0 do
          begin
            dec(NumCells);

            CurrCell := Cells^;

            inc(PtrComp(Cells), SizeOf(PAggCellStyleAA));

            AddStyle(CurrCell.Left);
            AddStyle(CurrCell.Right);
          end;

        // Convert the Y-histogram into the array of starting indexes
        StartCell := 0;

        i := 0;

        while i < FActiveStyleTable.Size do
          begin
            st := PAggStyleInfo
              (FStyles[PCardinal(FActiveStyleTable[i])^]);

            v := st.StartCell;

            st.StartCell := StartCell;

            inc(StartCell, v);
            inc(i);
          end;

        Cells := FOutline.ScanLineCells(FScanY);
        NumCells := FOutline.ScanLineNumCells(FScanY);

        while NumCells <> 0 do
          begin
            dec(NumCells);

            CurrCell := Cells^;

            inc(PtrComp(Cells), SizeOf(PAggCellStyleAA));

            if CurrCell.Left < 0 then
                StyleID := 0
            else
                StyleID := CurrCell.Left - FMinStyle + 1;

            TempStyle := FStyles[StyleID];

            if CurrCell.x = TempStyle.LastX then
              begin
                Cell := FCells[TempStyle.StartCell +
                  TempStyle.NumCells - 1];

                inc(Cell.Area, CurrCell.Area);
                inc(Cell.Cover, CurrCell.Cover);

              end
            else
              begin
                Cell := FCells[TempStyle.StartCell + TempStyle.NumCells];

                Cell.x := CurrCell.x;
                Cell.Area := CurrCell.Area;
                Cell.Cover := CurrCell.Cover;
                TempStyle.LastX := CurrCell.x;

                inc(TempStyle.NumCells);
              end;

            if CurrCell.Right < 0 then
                StyleID := 0
            else
                StyleID := CurrCell.Right - FMinStyle + 1;

            TempStyle := FStyles[StyleID];

            if CurrCell.x = TempStyle.LastX then
              begin
                Cell := FCells[TempStyle.StartCell +
                  TempStyle.NumCells - 1];

                dec(Cell.Area, CurrCell.Area);
                dec(Cell.Cover, CurrCell.Cover);
              end
            else
              begin
                Cell := FCells[TempStyle.StartCell + TempStyle.NumCells];

                Cell.x := CurrCell.x;
                Cell.Area := -CurrCell.Area;
                Cell.Cover := -CurrCell.Cover;
                TempStyle.LastX := CurrCell.x;

                inc(TempStyle.NumCells);
              end;
          end;
      end;

    if FActiveStyleTable.Size > 1 then
        Break;

    inc(FScanY);
  until False;

  inc(FScanY);

  if FLayerOrder <> loUnsorted then
    begin
      RA := TAggRangeAdaptor.Create(FActiveStyleTable, 1, FActiveStyleTable.Size - 1);
      try
        if FLayerOrder = loDirect then
            QuickSort(RA, @CardinalGreater)
        else
            QuickSort(RA, @CardinalLess);
      finally
          RA.Free;
      end;
    end;

  Result := FActiveStyleTable.Size - 1;
end;

function TAggRasterizerCompoundAA.ScanLineStart: Integer;
begin
  Result := FScanLineStart;
end;

function TAggRasterizerCompoundAA.ScanLineLength: Cardinal;
begin
  Result := FScanLineLength;
end;

function TAggRasterizerCompoundAA.Style(StyleIndex: Cardinal): Cardinal;
begin
  Result := PCardinal(FActiveStyleTable[StyleIndex + 1])^ +
    FMinStyle - 1;
end;

function TAggRasterizerCompoundAA.AllocateCoverBuffer(Len: Cardinal)
  : PCover;
begin
  FCoverBuffer.Allocate(Len, 256);

  Result := FCoverBuffer[0];
end;

function TAggRasterizerCompoundAA.NavigateScanLine(y: Integer): Boolean;
begin
  FOutline.SortCells;

  if FOutline.TotalCells = 0 then
    begin
      Result := False;

      Exit;
    end;

  if FMaxStyle < FMinStyle then
    begin
      Result := False;

      Exit;
    end;

  if (y < FOutline.MinY) or (y > FOutline.MaxY) then
    begin
      Result := False;

      Exit;
    end;

  FScanY := y;

  FStyles.Allocate(FMaxStyle - FMinStyle + 2, 128);
  AllocateMasterAlpha;

  Result := True;
end;

function TAggRasterizerCompoundAA.HitTest(TX, TY: Integer): Boolean;
var
  NumStyles: Cardinal;
  SL: TAggScanLineHitTest;
begin
  if not NavigateScanLine(TY) then
    begin
      Result := False;

      Exit;
    end;

  NumStyles := SweepStyles;

  if NumStyles <= 0 then
    begin
      Result := False;

      Exit;
    end;

  SL := TAggScanLineHitTest.Create(TX);
  try
    SweepScanLine(@SL, -1);

    Result := SL.Hit;
  finally
      SL.Free;
  end;
end;

function TAggRasterizerCompoundAA.CalculateAlpha(Area: Integer;
  MasterAlpha: Cardinal): Cardinal;
var
  Cover: Integer;

begin
  Cover := ShrInt32(Area, CAggPolySubpixelShift * 2 + 1 - CAggAntiAliasingShift);

  if Cover < 0 then
      Cover := -Cover;

  if FFillingRule = frEvenOdd then
    begin
      Cover := Cover and CAggAntiAliasingMask2;

      if Cover > CAggAntiAliasingScale then
          Cover := CAggAntiAliasingScale2 - Cover;
    end;

  if Cover > CAggAntiAliasingMask then
      Cover := CAggAntiAliasingMask;

  Result := ShrInt32(Cover * MasterAlpha + CAggAntiAliasingMask, CAggAntiAliasingShift);
end;

function TAggRasterizerCompoundAA.SweepScanLine(SL: TAggCustomScanLine;
  StyleIndex: Integer): Boolean;
var
  Scan_y, Cover, x, Area: Integer;

  MasterAlpha, NumCells, alpha: Cardinal;

  st: PAggStyleInfo;

  Cell: PAggCellInfo;

begin
  Scan_y := FScanY - 1;

  if Scan_y > FOutline.MaxY then
    begin
      Result := False;

      Exit;
    end;

  SL.ResetSpans;

  MasterAlpha := CAggAntiAliasingMask;

  if StyleIndex < 0 then
      StyleIndex := 0
  else
    begin
      inc(StyleIndex);

      MasterAlpha := PCardinal
        (FMasterAlpha[PCardinal(FActiveStyleTable[StyleIndex])^ + FMinStyle - 1])^;
    end;

  st := FStyles[PCardinal(FActiveStyleTable[StyleIndex])^];
  NumCells := st.NumCells;
  Cell := FCells[st.StartCell];
  Cover := 0;

  while NumCells <> 0 do
    begin
      dec(NumCells);

      x := Cell.x;
      Area := Cell.Area;

      inc(Cover, Cell.Cover);
      inc(PtrComp(Cell), SizeOf(TAggCellInfo));

      if Area <> 0 then
        begin
          alpha := CalculateAlpha((Cover shl (CAggPolySubpixelShift + 1)) - Area,
            MasterAlpha);

          SL.AddCell(x, alpha);

          inc(x);
        end;

      if (NumCells <> 0) and (Cell.x > x) then
        begin
          alpha := CalculateAlpha(Cover shl (CAggPolySubpixelShift + 1),
            MasterAlpha);

          if alpha <> 0 then
              SL.AddSpan(x, Cell.x - x, alpha);
        end;
    end;

  if SL.NumSpans = 0 then
    begin
      Result := False;

      Exit;
    end;

  SL.Finalize(Scan_y);

  Result := True;
end;

procedure TAggRasterizerCompoundAA.AddStyle(StyleID: Integer);
var
  Nbyte, Mask: Cardinal;

  TempStyle: PAggStyleInfo;

  Uv: Cardinal;

begin
  if StyleID < 0 then
      StyleID := 0
  else
      dec(StyleID, FMinStyle - 1);

  Nbyte := ShrInt32(StyleID, 3);
  Mask := 1 shl (StyleID and 7);

  TempStyle := FStyles[StyleID];

  if PInt8u(FActiveStyleMask[Nbyte])^ and Mask = 0 then
    begin
      Uv := StyleID;

      FActiveStyleTable.Add(@Uv);

      PInt8u(FActiveStyleMask[Nbyte])^ :=
        PInt8u(FActiveStyleMask[Nbyte])^ or Mask;

      TempStyle.StartCell := 0;
      TempStyle.NumCells := 0;
      TempStyle.LastX := -$7FFFFFFF;
    end;

  inc(TempStyle.StartCell);
end;

procedure TAggRasterizerCompoundAA.AllocateMasterAlpha;
var
  Uv: Cardinal;

begin
  Uv := CAggAntiAliasingMask;

  while Integer(FMasterAlpha.Size) <= FMaxStyle do
      FMasterAlpha.Add(@Uv);
end;

{ TAggRasterizerCompoundAAInteger }

constructor TAggRasterizerCompoundAAInteger.Create(Clip: TAggRasterizerScanLineClip = nil);
begin
  FRasterizerScanLineClip := nil;

  if Clip <> nil then
      inherited Create(Clip)
  else
    begin
      FRasterizerScanLineClip := TAggRasterizerScanLineClipInt.Create;

      inherited Create(FRasterizerScanLineClip);
    end;

  FStart.x := 0;
  FStart.y := 0;
end;

destructor TAggRasterizerCompoundAAInteger.Destroy;
begin
  if Assigned(FRasterizerScanLineClip) then
      FRasterizerScanLineClip.Free;

  inherited;
end;

procedure TAggRasterizerCompoundAAInteger.MoveTo(x, y: Integer);
begin
  if FOutline.Sorted then
      Reset;

  FStart.x := PInteger(FRasterizerConverter.Downscale(x))^;
  FStart.y := PInteger(FRasterizerConverter.Downscale(y))^;

  FClipper.MoveTo(@FStart.x, @FStart.y);
end;

procedure TAggRasterizerCompoundAAInteger.MoveToDouble(x, y: Double);
begin
  if FOutline.Sorted then
      Reset;

  FStart.x := PInteger(FRasterizerConverter.Upscale(x))^;
  FStart.y := PInteger(FRasterizerConverter.Upscale(y))^;

  FClipper.MoveTo(@FStart.x, @FStart.y);
end;

procedure TAggRasterizerCompoundAAInteger.AddVertex(x, y: Double; Cmd: Cardinal);
begin
  if IsMoveTo(Cmd) then
      MoveToDouble(x, y)
  else if IsVertex(Cmd) then
      LineToDouble(x, y)
  else if IsClose(Cmd) then
      FClipper.LineTo(FOutline, @FStart.x, @FStart.y);
end;

{ TAggRasterizerCompoundAADouble }

constructor TAggRasterizerCompoundAADouble.Create(Clip: TAggRasterizerScanLineClip = nil);
begin
  FOwnsRasterizer := False;

  if Clip <> nil then
      inherited Create(Clip)
  else
    begin
      FRasterizerScanLineClip := TAggRasterizerScanLineDoubleClip.Create;
      FOwnsRasterizer := True;

      inherited Create(FRasterizerScanLineClip);
    end;

  FStart.x := 0;
  FStart.y := 0;
end;

destructor TAggRasterizerCompoundAADouble.Destroy;
begin
  if FOwnsRasterizer then
      FRasterizerScanLineClip.Free;
  inherited;
end;

procedure TAggRasterizerCompoundAADouble.MoveTo(x, y: Integer);
begin
  if FOutline.Sorted then
      Reset;

  FStart.x := PDouble(FRasterizerConverter.Downscale(x))^;
  FStart.y := PDouble(FRasterizerConverter.Downscale(y))^;

  FClipper.MoveTo(@FStart.x, @FStart.y);
end;

procedure TAggRasterizerCompoundAADouble.MoveToDouble(x, y: Double);
begin
  if FOutline.Sorted then
      Reset;

  FStart.x := PDouble(FRasterizerConverter.Upscale(x))^;
  FStart.y := PDouble(FRasterizerConverter.Upscale(y))^;

  FClipper.MoveTo(@FStart.x, @FStart.y);
end;

procedure TAggRasterizerCompoundAADouble.AddVertex(x, y: Double; Cmd: Cardinal);
begin
  if IsMoveTo(Cmd) then
      MoveToDouble(x, y)
  else if IsVertex(Cmd) then
      LineToDouble(x, y)
  else if IsClose(Cmd) then
      FClipper.LineTo(FOutline, @FStart.x, @FStart.y);
end;

end. 
 
 
