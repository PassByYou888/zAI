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
unit AggSpanPattern;

{$DEFINE FPC_DELPHI_MODE}
{$INCLUDE zDefine.inc}
interface
uses
  AggBasics,
  AggColor32,
  AggRenderingBuffer,
  AggSpanGenerator,
  AggSpanAllocator;

const
  CAggBaseMask = AggColor32.CAggBaseMask;

type
  TAggSpanPatternBase = class(TAggSpanGenerator)
  private
    FSource: TAggRenderingBuffer;
    FOffsetX, FOffsetY: Cardinal;
    FAlpha: Int8u;
    function GetAlpha: Double;
    procedure SetAlpha(v: Double);
    procedure SetOffsetX(v: Cardinal);
    procedure SetOffsetY(v: Cardinal);
  protected
    function GetSourceImage: TAggRenderingBuffer;
    procedure SetSourceImage(v: TAggRenderingBuffer); virtual;
  public
    constructor Create(Alloc: TAggSpanAllocator); overload;
    constructor Create(Alloc: TAggSpanAllocator; Src: TAggRenderingBuffer; OffsetX, OffsetY: Cardinal; alpha: Double); overload;

    function GetAlphaInt: Int8u;

    property OffsetX: Cardinal read FOffsetX write SetOffsetX;
    property OffsetY: Cardinal read FOffsetY write SetOffsetY;
    property alpha: Double read GetAlpha write SetAlpha;
    property SourceImage: TAggRenderingBuffer read GetSourceImage write SetSourceImage;
  end;

  TAggWrapMode = class
  protected
    FValue: Cardinal;
  public
    procedure Init(Size: Cardinal); virtual;
    procedure Reset; virtual;

    function FuncOperator(v: Integer): Cardinal; virtual; abstract;
    function IncOperator: Cardinal; virtual; abstract;
  end;

  TAggWrapModeRepeat = class(TAggWrapMode)
  private
    FSize, FAdd: Cardinal;
  public
    procedure Init(Size: Cardinal); override;

    function FuncOperator(v: Integer): Cardinal; override;
    function IncOperator: Cardinal; override;
  end;

  TAggWrapModeRepeatPow2 = class(TAggWrapMode)
  private
    FMask: Cardinal;
  public
    procedure Init(Size: Cardinal); override;

    function FuncOperator(v: Integer): Cardinal; override;
    function IncOperator: Cardinal; override;
  end;

  TAggWrapModeRepeatAutoPow2 = class(TAggWrapMode)
  private
    FSize, FAdd, FMask: Cardinal;
  public
    procedure Init(Size: Cardinal); override;

    function FuncOperator(v: Integer): Cardinal; override;
    function IncOperator: Cardinal; override;
  end;

  TAggWrapModeReflect = class(TAggWrapMode)
  private
    FSize, FSize2, FAdd: Cardinal;
  public
    procedure Init(Size: Cardinal); override;

    function FuncOperator(v: Integer): Cardinal; override;
    function IncOperator: Cardinal; override;
  end;

  TAggWrapModeReflectPow2 = class(TAggWrapMode)
  private
    FSize, FMask: Cardinal;
  public
    procedure Init(Size: Cardinal); override;

    function FuncOperator(v: Integer): Cardinal; override;
    function IncOperator: Cardinal; override;
  end;

  TAggWrapModeReflectAutoPow2 = class(TAggWrapMode)
  private
    FSize, FSize2, FAdd, FMask: Cardinal;
  public
    procedure Init(Size: Cardinal); override;

    function FuncOperator(v: Integer): Cardinal; override;
    function IncOperator: Cardinal; override;
  end;

implementation


{ TAggSpanPatternBase }

constructor TAggSpanPatternBase.Create(Alloc: TAggSpanAllocator);
begin
  inherited Create(Alloc);

  FSource := nil;
  FOffsetX := 0;
  FOffsetY := 0;
  FAlpha := 0;
end;

constructor TAggSpanPatternBase.Create(Alloc: TAggSpanAllocator;
  Src: TAggRenderingBuffer; OffsetX, OffsetY: Cardinal; alpha: Double);
begin
  inherited Create(Alloc);

  FSource := Src;
  FOffsetX := OffsetX;
  FOffsetY := OffsetY;
  FAlpha := Int8u(Trunc(alpha * CAggBaseMask));
end;

function TAggSpanPatternBase.GetSourceImage;
begin
  Result := FSource;
end;

function TAggSpanPatternBase.GetAlpha;
begin
  Result := FAlpha / CAggBaseMask;
end;

function TAggSpanPatternBase.GetAlphaInt;
begin
  Result := FAlpha;
end;

procedure TAggSpanPatternBase.SetSourceImage;
begin
  FSource := v;
end;

procedure TAggSpanPatternBase.SetOffsetX;
begin
  FOffsetX := v;
end;

procedure TAggSpanPatternBase.SetOffsetY;
begin
  FOffsetY := v;
end;

procedure TAggSpanPatternBase.SetAlpha;
begin
  FAlpha := Int8u(Trunc(v * CAggBaseMask));
end;

{ TAggWrapMode }

procedure TAggWrapMode.Init(Size: Cardinal);
begin
  Reset;
end;

procedure TAggWrapMode.Reset;
begin
  FValue := 0;
end;

{ TAggWrapModeRepeat }

procedure TAggWrapModeRepeat.Init(Size: Cardinal);
begin
  inherited;
  FSize := Size;
  FAdd := Size * ($3FFFFFFF div Size);
end;

function TAggWrapModeRepeat.FuncOperator(v: Integer): Cardinal;
begin
  FValue := (Cardinal(v) + FAdd) mod FSize;
  Result := FValue;
end;

function TAggWrapModeRepeat.IncOperator: Cardinal;
begin
  inc(FValue);

  if FValue >= FSize then
      FValue := 0;

  Result := FValue;
end;

{ TAggWrapModeRepeatPow2 }

procedure TAggWrapModeRepeatPow2.Init(Size: Cardinal);
begin
  inherited;
  FMask := 1;

  while FMask < Size do
      FMask := (FMask shl 1) or 1;

  FMask := FMask shr 1;
end;

function TAggWrapModeRepeatPow2.FuncOperator(v: Integer): Cardinal;
begin
  FValue := Cardinal(v) and FMask;
  Result := FValue;
end;

function TAggWrapModeRepeatPow2.IncOperator: Cardinal;
begin
  inc(FValue);

  if FValue > FMask then
      FValue := 0;

  Result := FValue;
end;

{ TAggWrapModeRepeatAutoPow2 }

procedure TAggWrapModeRepeatAutoPow2.Init(Size: Cardinal);
begin
  inherited;

  FSize := Size;
  FAdd := Size * ($3FFFFFFF div Size);

  if FSize and (FSize - 1) <> 0 then
      FMask := 0
  else
      FMask := FSize - 1;
end;

function TAggWrapModeRepeatAutoPow2.FuncOperator(v: Integer): Cardinal;
begin
  if FMask <> 0 then
    begin
      FValue := Cardinal(Cardinal(v) and FMask);
      Result := FValue;
    end
  else
    begin
      FValue := Cardinal((Cardinal(v) + FAdd) mod FSize);
      Result := FValue;
    end;
end;

function TAggWrapModeRepeatAutoPow2.IncOperator: Cardinal;
begin
  inc(FValue);

  if FValue >= FSize then
      FValue := 0;

  Result := FValue;
end;

{ TAggWrapModeReflect }

procedure TAggWrapModeReflect.Init(Size: Cardinal);
begin
  inherited;

  FSize := Size;
  FSize2 := Size * 2;
  FAdd := FSize2 * ($3FFFFFFF div FSize2);
end;

function TAggWrapModeReflect.FuncOperator(v: Integer): Cardinal;
begin
  FValue := (Cardinal(v) + FAdd) mod FSize2;

  if FValue >= FSize then
      Result := FSize2 - FValue - 1
  else
      Result := FValue;
end;

function TAggWrapModeReflect.IncOperator: Cardinal;
begin
  inc(FValue);

  if FValue >= FSize2 then
      FValue := 0;

  if FValue >= FSize then
      Result := FSize2 - FValue - 1
  else
      Result := FValue;
end;

{ TAggWrapModeReflectPow2 }

procedure TAggWrapModeReflectPow2.Init(Size: Cardinal);
begin
  inherited;

  FMask := 1;
  FSize := 1;

  while FMask < Size do
    begin
      FMask := (FMask shl 1) or 1;
      FSize := FSize shl 1;
    end;
end;

function TAggWrapModeReflectPow2.FuncOperator(v: Integer): Cardinal;
begin
  FValue := Cardinal(v) and FMask;

  if FValue >= FSize then
      Result := FMask - FValue
  else
      Result := FValue;
end;

function TAggWrapModeReflectPow2.IncOperator: Cardinal;
begin
  inc(FValue);

  FValue := FValue and FMask;

  if FValue >= FSize then
      Result := FMask - FValue
  else
      Result := FValue;
end;

{ TAggWrapModeReflectAutoPow2 }

procedure TAggWrapModeReflectAutoPow2.Init(Size: Cardinal);
begin
  inherited;

  FSize := Size;
  FSize2 := Size * 2;
  FAdd := FSize2 * ($3FFFFFFF div FSize2);

  if FSize2 and (FSize2 - 1) <> 0 then
      FMask := 0
  else
      FMask := FSize2 - 1;
end;

function TAggWrapModeReflectAutoPow2.FuncOperator(v: Integer): Cardinal;
begin
  if FMask <> 0 then
      FValue := Cardinal(v) and FMask
  else
      FValue := (Cardinal(v) + FAdd) mod FSize2;

  if FValue >= FSize then
      Result := FSize2 - FValue - 1
  else
      Result := FValue;
end;

function TAggWrapModeReflectAutoPow2.IncOperator: Cardinal;
begin
  inc(FValue);

  if FValue >= FSize2 then
      FValue := 0;

  if FValue >= FSize then
      Result := FSize2 - FValue - 1
  else
      Result := FValue;
end;

end. 
 
 
