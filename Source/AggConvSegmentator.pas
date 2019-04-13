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
unit AggConvSegmentator;

{$DEFINE FPC_DELPHI_MODE}
{$INCLUDE zDefine.inc}
interface
uses
  AggBasics,
  AggVertexSource,
  AggConvAdaptorVpgen,
  AggVpGenSegmentator;

type
  TAggConvSegmentator = class(TAggConvAdaptorVpgenSegmentator)
  private
    FGenerator: TAggVpgenSegmentator;

    procedure SetApproximationScale(Value: Double);
    function GetApproximationScale: Double;
  public
    constructor Create(Vs: TAggCustomVertexSource);
    destructor Destroy; override;

    property ApproximationScale: Double read GetApproximationScale write SetApproximationScale;
  end;

implementation


{ TAggConvSegmentator }

constructor TAggConvSegmentator.Create(Vs: TAggCustomVertexSource);
begin
  FGenerator := TAggVpgenSegmentator.Create;

  inherited Create(Vs, FGenerator);
end;

destructor TAggConvSegmentator.Destroy;
begin
  FGenerator.Free;
  inherited;
end;

procedure TAggConvSegmentator.SetApproximationScale(Value: Double);
begin
  VpGenSegmentator.ApproximationScale := Value;
end;

function TAggConvSegmentator.GetApproximationScale: Double;
begin
  Result := VpGenSegmentator.ApproximationScale;
end;

end. 
 
