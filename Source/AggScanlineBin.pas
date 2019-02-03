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
unit AggScanlineBin;

{$DEFINE FPC_DELPHI_MODE}
{$INCLUDE zDefine.inc}
interface
uses
  AggBasics,
  AggScanline;

type
  PAggSpanBin = ^TAggSpanBin;

  TAggSpanBin = record
    x, Len: Int16;
  end;

  // This is binary scaline container which supports the interface
  // used in the Rasterizer::render(). See description of AggScanlineUnpacked8
  // for details.

  TAggScanLineBin = class(TAggCustomScanLine)
  private type
    TConstIterator = class(TAggCustomSpan)
    private
      FSpan: PAggSpanBin;
    protected
      function GetX: Integer; override;
      function GetLength: Integer; override;
    public
      constructor Create(aScanline: TAggScanLineBin);
      procedure IncOperator; override;
    end;
  private
    FMaxLength: Cardinal;
    FLastX, fy: Integer;

    FSpans, FCurrentSpan: PAggSpanBin;
  protected
    function GetY: Integer; override;
    function GetNumSpans: Cardinal; override;
    // function GetSizeOfSpan: Cardinal; override;
    // function GetIsPlainSpan: Boolean; override;
  public
    constructor Create;
    destructor Destroy; override;

    procedure Reset(MinX, MaxX: Integer); override;
    procedure ResetSpans; override;

    procedure Finalize(y: Integer); override;
    procedure AddCell(x: Integer; Cover: Cardinal); override;
    procedure AddSpan(x: Integer; Len, Cover: Cardinal); override;

    function GetBegin: TAggCustomSpan; override;
  end;

implementation

{ TAggScanLineBin.TConstIterator }

constructor TAggScanLineBin.TConstIterator.Create(aScanline: TAggScanLineBin);
begin
  inherited Create;

  FSpan := PAggSpanBin(PtrComp(aScanline.FSpans) + SizeOf(TAggSpanBin));
end;

function TAggScanLineBin.TConstIterator.GetLength: Integer;
begin
  Result := FSpan.Len;
end;

function TAggScanLineBin.TConstIterator.GetX: Integer;
begin
  Result := FSpan.x;
end;

procedure TAggScanLineBin.TConstIterator.IncOperator;
begin
  inc(PtrComp(FSpan), SizeOf(TAggSpanBin));
end;

{ TAggScanLineBin }

constructor TAggScanLineBin.Create;
begin
  FMaxLength := 0;
  FLastX := $7FFFFFF0;

  FSpans := nil;
  FCurrentSpan := nil;
end;

destructor TAggScanLineBin.Destroy;
begin
  AggFreeMem(Pointer(FSpans), FMaxLength * SizeOf(TAggSpanBin));
  inherited;
end;

procedure TAggScanLineBin.Reset(MinX, MaxX: Integer);
var
  MaxLength: Cardinal;
begin
  MaxLength := MaxX - MinX + 3;

  if MaxLength > FMaxLength then
    begin
      AggFreeMem(Pointer(FSpans), FMaxLength * SizeOf(TAggSpanBin));
      AggGetMem(Pointer(FSpans), MaxLength * SizeOf(TAggSpanBin));

      FMaxLength := MaxLength;
    end;

  FLastX := $7FFFFFF0;
  FCurrentSpan := FSpans;
end;

procedure TAggScanLineBin.ResetSpans;
begin
  FLastX := $7FFFFFF0;
  FCurrentSpan := FSpans;
end;

procedure TAggScanLineBin.Finalize(y: Integer);
begin
  fy := y;
end;

procedure TAggScanLineBin.AddCell(x: Integer; Cover: Cardinal);
begin
  if x = FLastX + 1 then
      inc(FCurrentSpan.Len)
  else
    begin
      inc(PtrComp(FCurrentSpan), SizeOf(TAggSpanBin));

      FCurrentSpan.x := Int16(x);
      FCurrentSpan.Len := 1;
    end;

  FLastX := x;
end;

procedure TAggScanLineBin.AddSpan(x: Integer; Len, Cover: Cardinal);
begin
  if x = FLastX + 1 then
      FCurrentSpan.Len := Int16(FCurrentSpan.Len + Len)
  else
    begin
      inc(PtrComp(FCurrentSpan), SizeOf(TAggSpanBin));

      FCurrentSpan.x := Int16(x);
      FCurrentSpan.Len := Int16(Len);
    end;

  FLastX := x + Len - 1;
end;

function TAggScanLineBin.GetY: Integer;
begin
  Result := fy
end;

function TAggScanLineBin.GetNumSpans: Cardinal;
begin
  Result := (PtrComp(FCurrentSpan) - PtrComp(FSpans)) div SizeOf(TAggSpanBin);
end;

function TAggScanLineBin.GetBegin: TAggCustomSpan;
begin
  Result := TConstIterator.Create(Self);
end;

{ function TAggScanLineBin.GetIsPlainSpan: Boolean;
  begin
  Result := False;
  end; }

{ function TAggScanLineBin.GetSizeOfSpan: Cardinal;
  begin
  Result := SizeOf(TAggSpanBin);
  end; }

end. 
 
 
