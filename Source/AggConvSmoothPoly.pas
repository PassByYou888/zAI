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
unit AggConvSmoothPoly;

{$DEFINE FPC_DELPHI_MODE}
{$INCLUDE zDefine.inc}
interface
uses
  AggBasics,
  AggVcgenSmoothPoly1,
  AggConvAdaptorVcgen,
  AggConvCurve,
  AggVertexSource;

type
  TAggConvSmoothPoly = class(TAggConvAdaptorVcgen)
  private
    FGenerator: TAggVcgenSmoothPoly1;

    procedure SetSmoothValue(Value: Double);
    function GetSmoothValue: Double;
  public
    constructor Create(VertexSource: TAggVertexSource);
    destructor Destroy; override;

    property SmoothValue: Double read GetSmoothValue write SetSmoothValue;
  end;

  TAggConvSmoothPolyCurve = class(TAggConvCurve)
  private
    FSmooth: TAggConvSmoothPoly;

    procedure SetSmoothValue(Value: Double);
    function GetSmoothValue: Double;
  public
    constructor Create(VertexSource: TAggVertexSource);
    destructor Destroy; override;

    property SmoothValue: Double read GetSmoothValue write SetSmoothValue;
  end;

implementation


{ TAggConvSmoothPoly }

constructor TAggConvSmoothPoly.Create(VertexSource: TAggVertexSource);
begin
  FGenerator := TAggVcgenSmoothPoly1.Create;

  inherited Create(VertexSource, FGenerator);
end;

destructor TAggConvSmoothPoly.Destroy;
begin
  FGenerator.Free;

  inherited;
end;

procedure TAggConvSmoothPoly.SetSmoothValue(Value: Double);
begin
  FGenerator.SmoothValue := Value;
end;

function TAggConvSmoothPoly.GetSmoothValue: Double;
begin
  Result := FGenerator.SmoothValue;
end;

{ TAggConvSmoothPolyCurve }

constructor TAggConvSmoothPolyCurve.Create(VertexSource: TAggVertexSource);
begin
  FSmooth := TAggConvSmoothPoly.Create(VertexSource);

  inherited Create(FSmooth);
end;

destructor TAggConvSmoothPolyCurve.Destroy;
begin
  FSmooth.Free;

  inherited;
end;

procedure TAggConvSmoothPolyCurve.SetSmoothValue(Value: Double);
begin
  TAggVcgenSmoothPoly1(FSmooth.Generator).SmoothValue := Value;
end;

function TAggConvSmoothPolyCurve.GetSmoothValue: Double;
begin
  Result := TAggVcgenSmoothPoly1(FSmooth.Generator).SmoothValue;
end;

end.
 
 
