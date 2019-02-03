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
unit AggSpanInterpolatorAdaptor;

{$DEFINE FPC_DELPHI_MODE}
{$INCLUDE zDefine.inc}
interface
uses
  AggBasics,
  AggSpanInterpolatorLinear,
  AggTransAffine;

type
  TAggDistortion = class
  public
    procedure Calculate(var x, y: Integer); virtual; abstract;
  end;

  TAggSpanInterpolatorAdaptor = class(TAggSpanInterpolatorLinear)
  private
    FDistortion: TAggDistortion;
    procedure SetDistortion(Dist: TAggDistortion);
  public
    constructor Create; overload;
    constructor Create(Trans: TAggTransAffine; Dist: TAggDistortion); overload;
    constructor Create(Trans: TAggTransAffine; Dist: TAggDistortion; x, y: Double; Len: Cardinal); overload;

    procedure Coordinates(x, y: PInteger); override;
    procedure Coordinates(var x, y: Integer); override;

    property Distortion: TAggDistortion read FDistortion write SetDistortion;
  end;

implementation


{ TAggSpanInterpolatorAdaptor }

constructor TAggSpanInterpolatorAdaptor.Create;
begin
  inherited Create;

  FDistortion := nil;
end;

constructor TAggSpanInterpolatorAdaptor.Create(Trans: TAggTransAffine;
  Dist: TAggDistortion);
begin
  inherited Create(Trans);

  FDistortion := Dist;
end;

constructor TAggSpanInterpolatorAdaptor.Create(Trans: TAggTransAffine;
  Dist: TAggDistortion; x, y: Double; Len: Cardinal);
begin
  inherited Create(Trans, x, y, Len);

  FDistortion := Dist;
end;

procedure TAggSpanInterpolatorAdaptor.SetDistortion;
begin
  FDistortion := Dist;
end;

procedure TAggSpanInterpolatorAdaptor.Coordinates(x, y: PInteger);
begin
  inherited Coordinates(x, y);

  FDistortion.Calculate(x^, y^);
end;

procedure TAggSpanInterpolatorAdaptor.Coordinates(var x, y: Integer);
begin
  inherited Coordinates(x, y);

  FDistortion.Calculate(x, y);
end;

end.
 
 
 
