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
unit AggSpanSubdivAdaptor;

{$DEFINE FPC_DELPHI_MODE}
{$INCLUDE zDefine.inc}
interface
uses
  AggBasics,
  AggSpanInterpolatorLinear,
  AggTransAffine;

type
  TAggSpanSubdivAdaptor = class(TAggSpanInterpolator)
  private
    FSubdivShift, FSubdivSize, FSubdivMask: Cardinal;

    FInterpolator: TAggSpanInterpolator;

    FSourceX: Integer;
    FSourceY: Double;
    fPos, FLength: Cardinal;

    procedure SetSubdivShift(Shift: Cardinal);
    procedure SetInterpolator(Value: TAggSpanInterpolator);
  protected
    function GetTransformer: TAggTransAffine; override;
    procedure SetTransformer(Trans: TAggTransAffine); override;
  public
    constructor Create(SS: Cardinal = 8); overload;
    constructor Create(Interpolator: TAggSpanInterpolator; ASubdivShift: Cardinal = 4; SS: Cardinal = 8); overload;
    constructor Create(Interpolator: TAggSpanInterpolator; x, y: Double; Len: Cardinal; ASubdivShift: Cardinal = 4; SS: Cardinal = 8); overload;

    procedure SetBegin(x, y: Double; Len: Cardinal); override;

    procedure IncOperator; override;
    procedure Coordinates(x, y: PInteger); override;
    procedure Coordinates(var x, y: Integer); override;

    procedure LocalScale(x, y: PInteger); override;

    property Interpolator: TAggSpanInterpolator read FInterpolator write SetInterpolator;
    property SubdivShift: Cardinal read FSubdivShift write SetSubdivShift;
    property Transformer: TAggTransAffine read GetTransformer write SetTransformer;
  end;

implementation


{ TAggSpanSubdivAdaptor }

constructor TAggSpanSubdivAdaptor.Create(SS: Cardinal = 8);
begin
  FSubpixelShift := SS;
  FSubpixelSize := 1 shl FSubpixelShift;

  FSubdivShift := 4;
  FSubdivSize := 1 shl FSubdivShift;
  FSubdivMask := FSubdivSize - 1;

  FInterpolator := nil;
end;

constructor TAggSpanSubdivAdaptor.Create(Interpolator: TAggSpanInterpolator;
  ASubdivShift: Cardinal = 4; SS: Cardinal = 8);
begin
  FSubpixelShift := SS;
  FSubpixelSize := 1 shl FSubpixelShift;

  FSubdivShift := ASubdivShift;
  FSubdivSize := 1 shl FSubdivShift;
  FSubdivMask := FSubdivSize - 1;

  FInterpolator := Interpolator;
end;

constructor TAggSpanSubdivAdaptor.Create(Interpolator: TAggSpanInterpolator;
  x, y: Double; Len: Cardinal; ASubdivShift: Cardinal = 4; SS: Cardinal = 8);
begin
  FSubpixelShift := SS;
  FSubpixelSize := 1 shl FSubpixelShift;

  FSubdivShift := ASubdivShift;
  FSubdivSize := 1 shl FSubdivShift;
  FSubdivMask := FSubdivSize - 1;

  FInterpolator := Interpolator;

  SetBegin(x, y, Len);
end;

procedure TAggSpanSubdivAdaptor.SetInterpolator(Value: TAggSpanInterpolator);
begin
  FInterpolator := Value;
end;

function TAggSpanSubdivAdaptor.GetTransformer;
begin
  Result := FInterpolator.Transformer;
end;

procedure TAggSpanSubdivAdaptor.SetTransformer;
begin
  FInterpolator.Transformer := Trans;
end;

procedure TAggSpanSubdivAdaptor.SetSubdivShift;
begin
  FSubdivShift := Shift;
  FSubdivSize := 1 shl FSubdivShift;
  FSubdivMask := FSubdivSize - 1;
end;

procedure TAggSpanSubdivAdaptor.SetBegin(x, y: Double; Len: Cardinal);
begin
  fPos := 1;
  FSourceX := Trunc(x * FSubpixelSize) + FSubpixelSize;
  FSourceY := y;
  FLength := Len;

  if Len > FSubdivSize then
      Len := FSubdivSize;

  FInterpolator.SetBegin(x, y, Len);
end;

procedure TAggSpanSubdivAdaptor.IncOperator;
var
  Len: Cardinal;
begin
  FInterpolator.IncOperator;

  if fPos >= FSubdivSize then
    begin
      Len := FLength;

      if Len > FSubdivSize then
          Len := FSubdivSize;

      FInterpolator.Resynchronize(FSourceX / FSubpixelSize + Len, FSourceY, Len);

      fPos := 0;
    end;

  inc(FSourceX, FSubpixelSize);
  inc(fPos);
  dec(FLength);
end;

procedure TAggSpanSubdivAdaptor.Coordinates(x, y: PInteger);
begin
  FInterpolator.Coordinates(x, y);
end;

procedure TAggSpanSubdivAdaptor.Coordinates(var x, y: Integer);
begin
  FInterpolator.Coordinates(x, y);
end;

procedure TAggSpanSubdivAdaptor.LocalScale;
begin
  FInterpolator.LocalScale(x, y);
end;

end. 
 
 
