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
unit AggSpanInterpolatorTrans;

(*
  ////////////////////////////////////////////////////////////////////////////////
  //                                                                            //
  //  Horizontal Span interpolator for use with an arbitrary transformer.       //
  //  The efficiency highly depends on the operations done in the transformer   //
  //                                                                            //
  ////////////////////////////////////////////////////////////////////////////////
*)

{$DEFINE FPC_DELPHI_MODE}
{$INCLUDE zDefine.inc}
interface
uses
  AggBasics,
  AggTransAffine,
  AggSpanInterpolatorLinear;

type
  TAggSpanInterpolatorTrans = class(TAggSpanInterpolator)
  private
    FTrans: TAggTransAffine;

    fx, fy: Double;
    FIntX, FIntY: Integer;
  public
    constructor Create(SS: Cardinal = 8); overload;
    constructor Create(Trans: TAggTransAffine; SS: Cardinal = 8); overload;
    constructor Create(Trans: TAggTransAffine; x, y, z: Cardinal; SS: Cardinal = 8); overload;

    function GetTransformer: TAggTransAffine; override;
    procedure SetTransformer(Trans: TAggTransAffine); override;

    procedure SetBegin(x, y: Double; Len: Cardinal); override;

    procedure IncOperator; override;
    procedure Coordinates(x, y: PInteger); override;
    procedure Coordinates(var x, y: Integer); override;
  end;

implementation


{ TAggSpanInterpolatorTrans }

constructor TAggSpanInterpolatorTrans.Create(SS: Cardinal = 8);
begin
  inherited Create(SS);

  FTrans := nil;
end;

constructor TAggSpanInterpolatorTrans.Create(Trans: TAggTransAffine;
  SS: Cardinal = 8);
begin
  inherited Create(SS);

  FTrans := Trans;
end;

constructor TAggSpanInterpolatorTrans.Create(Trans: TAggTransAffine;
  x, y, z: Cardinal; SS: Cardinal = 8);
begin
  inherited Create(SS);

  FTrans := Trans;

  SetBegin(x, y, 0);
end;

function TAggSpanInterpolatorTrans.GetTransformer;
begin
  Result := FTrans;
end;

procedure TAggSpanInterpolatorTrans.SetTransformer;
begin
  FTrans := Trans;
end;

procedure TAggSpanInterpolatorTrans.SetBegin(x, y: Double; Len: Cardinal);
begin
  fx := x;
  fy := y;

  FTrans.Transform(FTrans, @x, @y);

  FIntX := IntegerRound(x * FSubpixelSize);
  FIntY := IntegerRound(y * FSubpixelSize);
end;

procedure TAggSpanInterpolatorTrans.IncOperator;
var
  x, y: Double;

begin
  fx := fx + 1.0;

  x := fx;
  y := fy;

  FTrans.Transform(FTrans, @x, @y);

  FIntX := IntegerRound(x * FSubpixelSize);
  FIntY := IntegerRound(y * FSubpixelSize);
end;

procedure TAggSpanInterpolatorTrans.Coordinates(x, y: PInteger);
begin
  x^ := FIntX;
  y^ := FIntY;
end;

procedure TAggSpanInterpolatorTrans.Coordinates(var x, y: Integer);
begin
  x := FIntX;
  y := FIntY;
end;

end. 
 
 
