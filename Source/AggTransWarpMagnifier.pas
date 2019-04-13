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
unit AggTransWarpMagnifier;

{$DEFINE FPC_DELPHI_MODE}
{$INCLUDE zDefine.inc}
interface
uses
  AggBasics,
  AggTransAffine;

type
  TAggTransWarpMagnifier = class(TAggTransAffine)
  private
    FCenter: TPointDouble;
    FMagnification, FRadius: Double;
    procedure SetMagnification(M: Double);
    procedure SetRadius(r: Double);
  public
    constructor Create;

    procedure SetCenter(x, y: Double);

    property Magnification: Double read FMagnification write SetMagnification;
    property radius: Double read FRadius write SetRadius;
  end;

implementation

procedure WarpMagnifierTransform(This: TAggTransWarpMagnifier; x, y: PDouble);
var
  dx, dy, r, M: Double;

begin
  dx := x^ - This.FCenter.x;
  dy := y^ - This.FCenter.y;
  r := Sqrt(dx * dx + dy * dy);

  if r < This.FRadius then
    begin
      x^ := This.FCenter.x + dx * This.FMagnification;
      y^ := This.FCenter.y + dy * This.FMagnification;

      Exit;
    end;

  M := (r + This.FRadius * (This.FMagnification - 1.0)) / r;

  x^ := This.FCenter.x + dx * M;
  y^ := This.FCenter.y + dy * M;
end;

procedure WarpMagnifierTransformInverseTransform(This: TAggTransWarpMagnifier;
  x, y: PDouble);
var
  t: TAggTransWarpMagnifier;
begin
  t := TAggTransWarpMagnifier.Create;
  try
    t := This;

    t.SetMagnification(1.0 / This.FMagnification);
    t.SetRadius(This.FRadius * This.FMagnification);
    t.Transform(@t, x, y);
  finally
      t.Free;
  end;
end;

{ TAggTransWarpMagnifier }

constructor TAggTransWarpMagnifier.Create;
begin
  inherited Create;

  FCenter.x := 0.0;
  FCenter.y := 0.0;

  FMagnification := 1.0;
  FRadius := 1.0;

  Transform := @WarpMagnifierTransform;
  InverseTransform := @WarpMagnifierTransformInverseTransform;
end;

procedure TAggTransWarpMagnifier.SetCenter(x, y: Double);
begin
  FCenter.x := x;
  FCenter.y := y;
end;

procedure TAggTransWarpMagnifier.SetMagnification(M: Double);
begin
  FMagnification := M;
end;

procedure TAggTransWarpMagnifier.SetRadius(r: Double);
begin
  FRadius := r;
end;

end. 
 
 
