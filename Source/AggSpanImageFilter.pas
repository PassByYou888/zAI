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
unit AggSpanImageFilter;

{$DEFINE FPC_DELPHI_MODE}
{$INCLUDE zDefine.inc}
interface
uses
  AggBasics,
  AggColor32,
  AggImageFilters,
  AggRenderingBuffer,
  AggSpanGenerator,
  AggSpanAllocator,
  AggSpanInterpolatorLinear;

type
  TAggSpanImageFilter = class(TAggSpanGenerator)
  private
    FSource: TAggRenderingBuffer;
    FBackColor: TAggColor;
    FInterpolator: TAggSpanInterpolator;
    FFilter: TAggImageFilterLUT;

    FDeltaDouble: TPointDouble;
    FDeltaXInt, FDeltaYInt: Cardinal;
    procedure SetInterpolator(Value: TAggSpanInterpolator);
    procedure SetFilter(Value: TAggImageFilterLUT);
  protected
    procedure SetSourceImage(Value: TAggRenderingBuffer); virtual;
  public
    constructor Create(Alloc: TAggSpanAllocator); overload; virtual;
    constructor Create(Alloc: TAggSpanAllocator; Src: TAggRenderingBuffer;
      BackColor: PAggColor; Interpolator: TAggSpanInterpolator;
      Filter: TAggImageFilterLUT); overload; virtual;

    function GetBackgroundColor: PAggColor;

    procedure SetBackgroundColor(v: PAggColor);
    procedure SetFilterOffset(deltax, deltay: Double); overload;
    procedure SetFilterOffset(Delta: Double); overload;

    property Interpolator: TAggSpanInterpolator read FInterpolator write SetInterpolator;
    property SourceImage: TAggRenderingBuffer read FSource write SetSourceImage;
    property Filter: TAggImageFilterLUT read FFilter write SetFilter;
    property FilterDeltaXInteger: Cardinal read FDeltaXInt;
    property FilterDeltaYInteger: Cardinal read FDeltaYInt;
    property FilterDeltaXDouble: Double read FDeltaDouble.x;
    property FilterDeltaYDouble: Double read FDeltaDouble.y;
  end;

implementation


{ TAggSpanImageFilter }

constructor TAggSpanImageFilter.Create(Alloc: TAggSpanAllocator);
begin
  inherited Create(Alloc);

  FSource := nil;
  FInterpolator := nil;
  FFilter := nil;

  FDeltaDouble := PointDouble(0);
  FDeltaXInt := 0;
  FDeltaYInt := 0;
end;

constructor TAggSpanImageFilter.Create(Alloc: TAggSpanAllocator;
  Src: TAggRenderingBuffer; BackColor: PAggColor;
  Interpolator: TAggSpanInterpolator; Filter: TAggImageFilterLUT);
begin
  inherited Create(Alloc);

  FSource := Src;
  FBackColor := BackColor^;
  FInterpolator := Interpolator;
  FFilter := Filter;

  FDeltaDouble := PointDouble(0.5);
  FDeltaXInt := CAggImageSubpixelSize div 2;
  FDeltaYInt := CAggImageSubpixelSize div 2;
end;

function TAggSpanImageFilter.GetBackgroundColor;
begin
  Result := @FBackColor;
end;

procedure TAggSpanImageFilter.SetSourceImage(Value: TAggRenderingBuffer);
begin
  FSource := Value;
end;

procedure TAggSpanImageFilter.SetBackgroundColor;
begin
  FBackColor := v^;
end;

procedure TAggSpanImageFilter.SetInterpolator(Value: TAggSpanInterpolator);
begin
  FInterpolator := Value;
end;

procedure TAggSpanImageFilter.SetFilter(Value: TAggImageFilterLUT);
begin
  FFilter := Value;
end;

procedure TAggSpanImageFilter.SetFilterOffset(deltax, deltay: Double);
begin
  FDeltaDouble := PointDouble(deltax, deltay);
  FDeltaXInt := Trunc(deltax * CAggImageSubpixelSize);
  FDeltaYInt := Trunc(deltay * CAggImageSubpixelSize);
end;

procedure TAggSpanImageFilter.SetFilterOffset(Delta: Double);
begin
  SetFilterOffset(Delta, Delta);
end;

end. 
 
 
