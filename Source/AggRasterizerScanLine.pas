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
unit AggRasterizerScanLine;

{$DEFINE FPC_DELPHI_MODE}
{$INCLUDE zDefine.inc}
interface

uses
  AggBasics,
  AggScanline,
  AggVertexSource;

type
  TAggCustomRasterizerScanLine = class
  protected
    function GetFillingRule: TAggFillingRule; virtual; abstract;
    procedure SetFillingRule(FillingRule: TAggFillingRule); virtual; abstract;
  public
    procedure Reset; virtual; abstract;
    procedure SetClipBox(x1, y1, x2, y2: Double); overload; virtual; abstract;
    procedure SetClipBox(Rect: TRectDouble); overload; virtual; abstract;

    procedure AddPath(Vs: TAggCustomVertexSource; PathID: Cardinal = 0); virtual; abstract;
    procedure AddVertex(x, y: Double; Cmd: Cardinal); virtual; abstract;

    procedure Sort; virtual; abstract;
    function RewindScanLines: Boolean; virtual; abstract;

    function HitTest(TX, TY: Integer): Boolean; virtual; abstract;

    property FillingRule: TAggFillingRule read GetFillingRule write SetFillingRule;
  end;

  TAggCustomBoundsRasterizerScanLine = class(TAggCustomRasterizerScanLine)
  protected
    function GetMinX: Integer; virtual; abstract;
    function GetMinY: Integer; virtual; abstract;
    function GetMaxX: Integer; virtual; abstract;
    function GetMaxY: Integer; virtual; abstract;
  public
    property MinimumX: Integer read GetMinX;
    property MinimumY: Integer read GetMinY;
    property MaximumX: Integer read GetMaxX;
    property MaximumY: Integer read GetMaxY;
  end;

  IAggRasterizerScanline = interface
    procedure Gamma(AGammaFunction: TAggCustomVertexSource);
    function SweepScanLine(SL: TAggCustomScanLine): Boolean;
    function SweepScanLineEm(SL: TAggCustomScanLine): Boolean;
  end;

  TAggRasterizerScanLine = class(TAggCustomBoundsRasterizerScanLine)
  public
    procedure Gamma(AGammaFunction: TAggCustomVertexSource); virtual; abstract;
    function SweepScanLine(SL: TAggCustomScanLine): Boolean; overload; virtual; abstract;
    // function SweepScanLineEm(Sl: TAggCustomScanLine): Boolean; virtual; abstract;
    function SweepScanLine(SL: TAggEmbeddedScanLine): Boolean; overload; virtual; abstract;
  end;

implementation

end. 
 
 
