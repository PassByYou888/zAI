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
unit AggEllipse;

{$DEFINE FPC_DELPHI_MODE}
{$INCLUDE zDefine.inc}
interface
uses
  Math,
  AggMath,
  AggBasics,
  AggVertexSource;

type
  TAggCustomEllipse = class(TAggVertexSource)
  private
    FCenter: TPointDouble;
    FApproximationScale: Double;
    FNum, FStep: Cardinal;
    FClockWise: Boolean;

    procedure SetApproximationScale(Value: Double);
  protected
    procedure CalculateNumSteps; virtual; abstract;
    function GetPathID(index: Cardinal): Cardinal; override;
  public
    constructor Create; virtual;

    procedure Rewind(PathID: Cardinal); override;

    property ApproximationScale: Double read FApproximationScale write SetApproximationScale;
  end;

  TAggCircle = class(TAggCustomEllipse)
  private
    FRadius: Double;
  protected
    procedure CalculateNumSteps; override;
  public
    constructor Create; overload; override;
    constructor Create(x, y, radius: Double; NumSteps: Cardinal = 0; CW: Boolean = False); overload;
    constructor Create(center: TPointDouble; radius: Double; NumSteps: Cardinal = 0; CW: Boolean = False); overload;

    procedure Initialize(x, y, radius: Double; NumSteps: Cardinal = 0; CW: Boolean = False); overload;
    procedure Initialize(center: TPointDouble; radius: Double; NumSteps: Cardinal = 0; CW: Boolean = False); overload;

    function Vertex(x, y: PDouble): Cardinal; override;
  end;

  TAggEllipse = class(TAggCustomEllipse)
  private
    FRadius: TPointDouble;
  protected
    procedure CalculateNumSteps; override;
  public
    constructor Create; overload; override;
    constructor Create(x, y, RX, RY: Double; NumSteps: Cardinal = 0; CW: Boolean = False); overload;
    constructor Create(center, radius: TPointDouble; NumSteps: Cardinal = 0; CW: Boolean = False); overload;

    procedure Initialize(x, y, RX, RY: Double; NumSteps: Cardinal = 0; CW: Boolean = False); overload;
    procedure Initialize(center, radius: TPointDouble; NumSteps: Cardinal = 0; CW: Boolean = False); overload;

    function Vertex(x, y: PDouble): Cardinal; override;
  end;

implementation


{ TAggCustomEllipse }

constructor TAggCustomEllipse.Create;
begin
  inherited Create;
  FApproximationScale := 1.0;
end;

function TAggCustomEllipse.GetPathID(index: Cardinal): Cardinal;
begin
  Result := 0;
end;

procedure TAggCustomEllipse.Rewind(PathID: Cardinal);
begin
  FStep := 0;
end;

procedure TAggCustomEllipse.SetApproximationScale(Value: Double);
begin
  if FApproximationScale <> Value then
    begin
      FApproximationScale := Value;
      CalculateNumSteps;
    end;
end;

{ TAggCircle }

constructor TAggCircle.Create;
begin
  inherited Create;

  Initialize(PointDouble(0), 1, 4, False);
end;

constructor TAggCircle.Create(x, y, radius: Double; NumSteps: Cardinal;
  CW: Boolean);
begin
  inherited Create;

  Initialize(PointDouble(x, y), radius, NumSteps, CW);
end;

constructor TAggCircle.Create(center: TPointDouble; radius: Double;
  NumSteps: Cardinal; CW: Boolean);
begin
  inherited Create;

  Initialize(center, radius, NumSteps, CW);
end;

procedure TAggCircle.Initialize(x, y, radius: Double; NumSteps: Cardinal;
  CW: Boolean);
begin
  Initialize(PointDouble(x, y), radius, NumSteps, CW);
end;

procedure TAggCircle.Initialize(center: TPointDouble; radius: Double;
  NumSteps: Cardinal; CW: Boolean);
begin
  FCenter := center;
  FRadius := radius;

  FNum := NumSteps;
  FStep := 0;
  FClockWise := CW;

  if FNum = 0 then
      CalculateNumSteps;
end;

function TAggCircle.Vertex(x, y: PDouble): Cardinal;
var
  angle: Double;
  sn, CN: Double;
begin
  if FStep = FNum then
    begin
      inc(FStep);

      Result := CAggPathCmdEndPoly or CAggPathFlagsClose or CAggPathFlagsCcw;

      Exit;
    end;

  if FStep > FNum then
    begin
      Result := CAggPathCmdStop;

      Exit;
    end;

  angle := 2 * pi * FStep / FNum;

  if FClockWise then
      angle := 2 * pi - angle;

  SinCosScale(angle, sn, CN, FRadius);
  x^ := FCenter.x + CN;
  y^ := FCenter.y + sn;

  inc(FStep);

  if FStep = 1 then
      Result := CAggPathCmdMoveTo
  else
      Result := CAggPathCmdLineTo;
end;

procedure TAggCircle.CalculateNumSteps;
var
  RA, DA: Double;
begin
  RA := Abs(FRadius);
  DA := ArcCos(RA / (RA + 0.125 / FApproximationScale)) * 2;

  FNum := Trunc(2 * pi / DA);
end;

{ TAggEllipse }

constructor TAggEllipse.Create;
begin
  inherited Create;

  Initialize(PointDouble(0), PointDouble(1), 4, False);
end;

constructor TAggEllipse.Create(x, y, RX, RY: Double; NumSteps: Cardinal = 0;
  CW: Boolean = False);
begin
  inherited Create;

  Initialize(PointDouble(x, y), PointDouble(RX, RY), NumSteps, CW);
end;

constructor TAggEllipse.Create(center, radius: TPointDouble; NumSteps: Cardinal;
  CW: Boolean);
begin
  inherited Create;

  Initialize(center, radius, NumSteps, CW);
end;

procedure TAggEllipse.Initialize(x, y, RX, RY: Double; NumSteps: Cardinal = 0;
  CW: Boolean = False);
begin
  Initialize(PointDouble(x, y), PointDouble(RX, RY), NumSteps, CW);
end;

procedure TAggEllipse.Initialize(center, radius: TPointDouble; NumSteps: Cardinal = 0;
  CW: Boolean = False);
begin
  FCenter := center;
  FRadius := radius;

  FNum := NumSteps;
  FStep := 0;
  FClockWise := CW;

  if FNum = 0 then
      CalculateNumSteps;
end;

function TAggEllipse.Vertex(x, y: PDouble): Cardinal;
var
  angle: Double;
  sn, CN: Double;
begin
  if FStep = FNum then
    begin
      inc(FStep);

      Result := CAggPathCmdEndPoly or CAggPathFlagsClose or CAggPathFlagsCcw;

      Exit;
    end;

  if FStep > FNum then
    begin
      Result := CAggPathCmdStop;

      Exit;
    end;

  angle := 2 * pi * FStep / FNum;

  if FClockWise then
      angle := 2 * pi - angle;

  SinCosScale(angle, sn, CN, FRadius.y, FRadius.x);
  x^ := FCenter.x + CN;
  y^ := FCenter.y + sn;

  inc(FStep);

  if FStep = 1 then
      Result := CAggPathCmdMoveTo
  else
      Result := CAggPathCmdLineTo;
end;

procedure TAggEllipse.CalculateNumSteps;
var
  RA, DA: Double;
begin
  RA := (Abs(FRadius.x) + Abs(FRadius.y)) * 0.5;
  DA := ArcCos(RA / (RA + 0.125 / FApproximationScale)) * 2;

  FNum := Trunc(2 * pi / DA);
end;

end. 
 
 
 
