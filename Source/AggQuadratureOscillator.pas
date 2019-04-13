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
unit AggQuadratureOscillator;

{$DEFINE FPC_DELPHI_MODE}
{$INCLUDE zDefine.inc}
interface

uses
  AggBasics;

type
  TAggQuadratureOscillator = class
  private
    function GetPhase: Double;
    procedure SetPhase(const Value: Double);
    procedure SetFrequency(const Value: Double);
    procedure SetAmplitude(const Value: Double);
  protected
    FAmplitude: Double;
    FFrequency: Double;
    FAngle: TPointDouble;
    FPosition: TPointDouble;
    procedure FrequencyChanged; virtual;
  public
    constructor Create(Frequency: Double; Amplitude: Double = 1); virtual;

    procedure Next; virtual;
    procedure Reset; virtual;

    property Amplitude: Double read FAmplitude write SetAmplitude;
    property Frequency: Double read FFrequency write SetFrequency;
    property sine: Double read FPosition.y;
    property cosine: Double read FPosition.x;
    property Phase: Double read GetPhase write SetPhase;
  end;

implementation

uses
  Math, AggMath;

{ TAggQuadratureOscillator }

constructor TAggQuadratureOscillator.Create(Frequency: Double;
  Amplitude: Double = 1);
begin
  inherited Create;
  FFrequency := Frequency;
  FAmplitude := Amplitude;
  SinCos(FFrequency, FAngle.x, FAngle.y);
  FPosition.y := 0;
  FPosition.x := FAmplitude;
end;

procedure TAggQuadratureOscillator.FrequencyChanged;
begin
  SinCos(FFrequency, FAngle.x, FAngle.y);
end;

function TAggQuadratureOscillator.GetPhase: Double;
begin
  Result := -ArcTan2(FPosition.y, -FPosition.x);
end;

procedure TAggQuadratureOscillator.Next;
var
  Temp: Double;
begin
  Temp := FPosition.y * FAngle.y - FPosition.x * FAngle.x;
  FPosition.x := FPosition.x * FAngle.y + FPosition.y * FAngle.x;
  FPosition.y := Temp;
end;

procedure TAggQuadratureOscillator.Reset;
begin
  Phase := 0;
end;

procedure TAggQuadratureOscillator.SetAmplitude(const Value: Double);
begin
  if FAmplitude <> Value then
    begin
      if FAmplitude = 0 then
        begin
          FPosition.y := 0;
          FPosition.x := Value;
        end
      else
        begin
          FPosition.y := FPosition.y / FAmplitude * Value;
          FPosition.x := FPosition.x / FAmplitude * Value;
        end;
      FAmplitude := Value;
    end;
end;

procedure TAggQuadratureOscillator.SetFrequency(const Value: Double);
begin
  if FFrequency <> Value then
    begin
      FFrequency := Value;
      FrequencyChanged;
    end;
end;

procedure TAggQuadratureOscillator.SetPhase(const Value: Double);
begin
  SinCos(Value, FPosition.y, FPosition.x);
  FPosition.y := FPosition.y * -FAmplitude;
  FPosition.x := FPosition.x * -FAmplitude;
end;

end. 
 
 
