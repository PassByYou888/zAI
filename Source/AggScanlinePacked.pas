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
unit AggScanlinePacked;

{$DEFINE FPC_DELPHI_MODE}
{$INCLUDE zDefine.inc}
interface
uses
  AggBasics,
  AggScanline;

type
  PAggSpanPacked8 = ^TAggSpanPacked8;

  TAggSpanPacked8 = record
    x, Len: Int16; // If negative, it's a solid Span, covers is valid

    Covers: PInt8u;
  end;

  TAggScanLinePacked8 = class(TAggCustomScanLine)
  private type
    TConstIterator = class(TAggCustomSpan)
    private
      FSpan: PAggSpanPacked8;
    protected
      function GetX: Integer; override;
      function GetLength: Integer; override;
    public
      constructor Create(aScanline: TAggScanLinePacked8);
      function Covers: PInt8u; override;
      procedure IncOperator; override;
    end;
  private
    FMaxLength: Cardinal;
    FLastX, fy: Integer;
    FCovers, FCoverPtr: PInt8u;
    FSpans, FCurrentSpan: PAggSpanPacked8;
  protected
    function GetY: Integer; override;
    function GetNumSpans: Cardinal; override;
  public
    constructor Create;
    destructor Destroy; override;

    procedure Reset(MinX, MaxX: Integer); override;
    procedure ResetSpans; override;

    procedure Finalize(y: Integer); override;
    procedure AddCell(x: Integer; Cover: Cardinal); override;
    procedure AddCells(x: Integer; Len: Cardinal; Covers: PInt8u); override;
    procedure AddSpan(x: Integer; Len, Cover: Cardinal); override;

    function GetBegin: TAggCustomSpan; override;
  end;

implementation


{ TAggScanLinePacked8.TConstIterator }

function TAggScanLinePacked8.TConstIterator.Covers: PInt8u;
begin
  Result := FSpan.Covers;
end;

constructor TAggScanLinePacked8.TConstIterator.Create(
  aScanline: TAggScanLinePacked8);
begin
  inherited Create;

  FSpan := PAggSpanPacked8(PtrComp(aScanline.FSpans) + SizeOf(TAggSpanPacked8));
end;

function TAggScanLinePacked8.TConstIterator.GetLength: Integer;
begin
  Result := FSpan.Len;
end;

function TAggScanLinePacked8.TConstIterator.GetX: Integer;
begin
  Result := FSpan.x;
end;

procedure TAggScanLinePacked8.TConstIterator.IncOperator;
begin
  inc(PtrComp(FSpan), SizeOf(TAggSpanPacked8));
end;

{ TAggScanLinePacked8 }

constructor TAggScanLinePacked8.Create;
begin
  FMaxLength := 0;
  FLastX := $7FFFFFF0;

  fy := 0;

  FCovers := nil;
  FCoverPtr := nil;

  FSpans := nil;
  FCurrentSpan := nil;
end;

destructor TAggScanLinePacked8.Destroy;
begin
  AggFreeMem(Pointer(FSpans), FMaxLength * SizeOf(TAggSpanPacked8));
  AggFreeMem(Pointer(FCovers), FMaxLength * SizeOf(Int8u));
  inherited;
end;

procedure TAggScanLinePacked8.Reset(MinX, MaxX: Integer);
var
  MaxLen: Cardinal;
begin
  MaxLen := MaxX - MinX + 3;

  if MaxLen > FMaxLength then
    begin
      AggFreeMem(Pointer(FCovers), FMaxLength);
      AggFreeMem(Pointer(FSpans), FMaxLength * SizeOf(TAggSpanPacked8));

      AggGetMem(Pointer(FCovers), MaxLen * SizeOf(Int8u));
      AggGetMem(Pointer(FSpans), MaxLen * SizeOf(TAggSpanPacked8));

      FMaxLength := MaxLen;
    end;

  FLastX := $7FFFFFF0;

  FCoverPtr := FCovers;
  FCurrentSpan := FSpans;

  FCurrentSpan.Len := 0;
end;

procedure TAggScanLinePacked8.ResetSpans;
begin
  FLastX := $7FFFFFF0;

  FCoverPtr := FCovers;
  FCurrentSpan := FSpans;

  FCurrentSpan.Len := 0;
end;

procedure TAggScanLinePacked8.Finalize(y: Integer);
begin
  fy := y;
end;

procedure TAggScanLinePacked8.AddCell(x: Integer; Cover: Cardinal);
begin
  FCoverPtr^ := Int8u(Cover);

  if (x = FLastX + 1) and (FCurrentSpan.Len > 0) then
      inc(FCurrentSpan.Len)
  else
    begin
      inc(PtrComp(FCurrentSpan), SizeOf(TAggSpanPacked8));

      FCurrentSpan.Covers := FCoverPtr;

      FCurrentSpan.x := Int16(x);
      FCurrentSpan.Len := 1;
    end;

  FLastX := x;

  inc(PtrComp(FCoverPtr), SizeOf(Int8u));
end;

procedure TAggScanLinePacked8.AddCells(x: Integer; Len: Cardinal; Covers: PInt8u);
begin
  Move(Covers^, FCoverPtr^, Len * SizeOf(Int8u));

  if (x = FLastX + 1) and (FCurrentSpan.Len > 0) then
      inc(FCurrentSpan.Len, Int16(Len))
  else
    begin
      inc(PtrComp(FCurrentSpan), SizeOf(TAggSpanPacked8));

      FCurrentSpan.Covers := FCoverPtr;
      FCurrentSpan.x := Int16(x);
      FCurrentSpan.Len := Int16(Len);
    end;

  inc(PtrComp(FCoverPtr), Len * SizeOf(Int8u));

  FLastX := x + Len - 1;
end;

procedure TAggScanLinePacked8.AddSpan(x: Integer; Len, Cover: Cardinal);
begin
  if (x = FLastX + 1) and (FCurrentSpan.Len < 0) and (Cover = FCurrentSpan.Covers^)
  then
      dec(FCurrentSpan.Len, Int16(Len))
  else
    begin
      FCoverPtr^ := Int8u(Cover);

      inc(PtrComp(FCurrentSpan), SizeOf(TAggSpanPacked8));

      FCurrentSpan.Covers := FCoverPtr;
      FCurrentSpan.x := Int16(x);
      FCurrentSpan.Len := Int16(Len);
      FCurrentSpan.Len := -FCurrentSpan.Len;

      inc(PtrComp(FCoverPtr), SizeOf(Int8u));
    end;

  FLastX := x + Len - 1;
end;

function TAggScanLinePacked8.GetY;
begin
  Result := fy;
end;

function TAggScanLinePacked8.GetNumSpans: Cardinal;
begin
  Result := (PtrComp(FCurrentSpan) - PtrComp(FSpans)) div SizeOf(TAggSpanPacked8);
end;

function TAggScanLinePacked8.GetBegin: TAggCustomSpan;
begin
  Result := TConstIterator.Create(Self);
end;

end. 
 
 
