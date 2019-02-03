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
unit AggScanline;

{$DEFINE FPC_DELPHI_MODE}
{$INCLUDE zDefine.inc}
interface
uses
  AggBasics;

type
  PAggSpanRecord = ^TAggSpanRecord;

  TAggSpanRecord = record
    x, Len: Int16;
    Covers: PInt8u;
  end;

  TAggCustomSpan = class
  protected
    function GetX: Integer; virtual;
    function GetLength: Integer; virtual;
  public
    function Covers: PInt8u; virtual;

    procedure IncOperator; virtual;

    property x: Integer read GetX;
    property Len: Integer read GetLength;
  end;

  TAggCustomScanLine = class
  protected
    // function GetSizeOfSpan: Cardinal; virtual; abstract;
    // function GetIsPlainSpan: Boolean; virtual;
    // function GetIsEmbedded: Boolean; virtual;
    function GetNumSpans: Cardinal; virtual; abstract;
    function GetY: Integer; virtual; abstract;
  public
    procedure Reset(MinX, MaxX: Integer); virtual; abstract;
    procedure ResetSpans; virtual; abstract;

    procedure Finalize(y: Integer); virtual; abstract;
    procedure AddCell(x: Integer; Cover: Cardinal); virtual; abstract;
    procedure AddCells(x: Integer; Len: Cardinal; Covers: PInt8u); virtual; abstract;
    procedure AddSpan(x: Integer; Len, Cover: Cardinal); virtual; abstract;

    function GetBegin: TAggCustomSpan; virtual; abstract;

    // procedure Init(Ptr: PInt8u; Dx, Dy: Integer); virtual; abstract;
    // procedure Setup(ScanLineIndex: Cardinal); virtual; abstract;

    // property IsPlainSpan: Boolean read GetIsPlainSpan;
    // property IsEmbedded: Boolean read GetIsEmbedded;
    property NumSpans: Cardinal read GetNumSpans;
    // property SizeOfSpan: Cardinal read GetSizeOfSpan;
    property y: Integer read GetY;
  end;

  TAggEmbeddedScanLine = class(TAggCustomScanLine)
  public
    procedure Init(PTR: PInt8u; dx, dy: Integer); virtual; abstract;
    procedure Setup(ScanLineIndex: Cardinal); virtual; abstract;
  end;

implementation

{ TAggCustomSpan }

function TAggCustomSpan.GetX: Integer;
begin
  Result := 0;
end;

function TAggCustomSpan.GetLength: Integer;
begin
  Result := 0;
end;

function TAggCustomSpan.Covers: PInt8u;
begin
  Result := nil;
end;

procedure TAggCustomSpan.IncOperator;
begin
end;

{ TAggCustomScanLine }

{ function TAggCustomScanLine.GetIsPlainSpan: Boolean;
  begin
  Result := True;
  end; }

{ function TAggCustomScanLine.GetIsEmbedded: Boolean;
  begin
  Result := False;
  end; }

end. 
 
 
