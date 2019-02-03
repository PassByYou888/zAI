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
unit AggTransViewport;

(*
  ////////////////////////////////////////////////////////////////////////////////
  //                                                                            //
  //  Viewport transformer - simple orthogonal conversions from world           //
  //  coordinates to screen (device) ones                                       //
  //                                                                            //
  ////////////////////////////////////////////////////////////////////////////////
*)

{$DEFINE FPC_DELPHI_MODE}
{$INCLUDE zDefine.inc}
interface
uses
  AggBasics,
  AggTransAffine;

type
  TAggAspectRatio = (arStretch, arMeet, arSlice);

  TAggTransViewport = class(TAggTransAffine)
  private
    FAspect: TAggAspectRatio;
    FIsValid: Boolean;
    FAlign, FDelta: TPointDouble;
    FActualWorld, FWorld, FDevice: TRectDouble;
    FK: TPointDouble;
    function GetDeviceDeltaX: Double;
    function GetDeviceDeltaY: Double;
    function GetScale: Double;
    function GetByteSize: Cardinal;
  public
    constructor Create;

    procedure PreserveAspectRatio(AlignX, AlignY: Double; Aspect: TAggAspectRatio); overload;
    procedure PreserveAspectRatio(Align: TPointDouble; Aspect: TAggAspectRatio); overload;

    procedure DeviceViewport(x1, y1, x2, y2: Double); overload;
    procedure DeviceViewport(Rect: TRectDouble); overload;
    procedure DeviceViewportNoUpdate(var x1, y1, x2, y2: Double);

    procedure WorldViewport(x1, y1, x2, y2: Double); overload;
    procedure WorldViewport(Rect: TRectDouble); overload;

    procedure WorldViewportNoUpdate(var x1, y1, x2, y2: Double); overload;
    procedure WorldViewportNoUpdate(var Rect: TRectDouble); overload;

    procedure WorldViewportActual(var x1, y1, x2, y2: Double);

    procedure InverseTransformScaleOnly(var x, y: Double);

    procedure ToAffine(Mtx: TAggTransAffine);
    procedure ToAffineScaleOnly(Mtx: TAggTransAffine);

    procedure Update;

    property AspectRatio: TAggAspectRatio read FAspect;
    property IsValid: Boolean read FIsValid;
    property AlignX: Double read FAlign.x;
    property AlignY: Double read FAlign.y;

    property ByteSize: Cardinal read GetByteSize;
    property DeviceDeltaX: Double read GetDeviceDeltaX;
    property DeviceDeltaY: Double read GetDeviceDeltaY;
    property ScaleX: Double read FK.x;
    property ScaleY: Double read FK.y;
    property Scale: Double read GetScale;
  end;

implementation

procedure Transform(This: TAggTransViewport; x, y: PDouble);
begin
  x^ := x^ * This.FK.x;
  y^ := y^ * This.FK.y;
end;

procedure InverseTransform(This: TAggTransViewport; x, y: PDouble);
begin
  x^ := (x^ - This.FDelta.x) / This.FK.x + This.FActualWorld.x1;
  y^ := (y^ - This.FDelta.y) / This.FK.y + This.FActualWorld.y1;
end;

{ TAggTransViewport }

constructor TAggTransViewport.Create;
begin
  inherited Create;

  Transform := @Transform;
  InverseTransform := @InverseTransform;

  FWorld.x1 := 0;
  FWorld.y1 := 0;
  FWorld.x2 := 1;
  FWorld.y2 := 1;

  FDevice.x1 := 0;
  FDevice.y1 := 0;
  FDevice.x2 := 1;
  FDevice.y2 := 1;

  FAspect := arStretch;

  FAlign.x := 0.5;
  FAlign.y := 0.5;

  FActualWorld.x1 := 0;
  FActualWorld.y1 := 0;
  FActualWorld.x2 := 1;
  FActualWorld.y2 := 1;
  FDelta.x := 0;
  FDelta.y := 0;

  FK.x := 1;
  FK.y := 1;
end;

procedure TAggTransViewport.PreserveAspectRatio(AlignX, AlignY: Double;
  Aspect: TAggAspectRatio);
begin
  FAlign.x := AlignX;
  FAlign.y := AlignY;
  FAspect := Aspect;

  Update;
end;

procedure TAggTransViewport.PreserveAspectRatio(Align: TPointDouble;
  Aspect: TAggAspectRatio);
begin
  FAlign := Align;
  FAspect := Aspect;

  Update;
end;

procedure TAggTransViewport.DeviceViewport(x1, y1, x2, y2: Double);
begin
  FDevice.x1 := x1;
  FDevice.y1 := y1;
  FDevice.x2 := x2;
  FDevice.y2 := y2;

  Update;
end;

procedure TAggTransViewport.DeviceViewport(Rect: TRectDouble);
begin
  FDevice.x1 := Rect.x1;
  FDevice.y1 := Rect.y1;
  FDevice.x2 := Rect.x2;
  FDevice.y2 := Rect.y2;

  Update;
end;

procedure TAggTransViewport.DeviceViewportNoUpdate(var x1, y1, x2, y2: Double);
begin
  x1 := FDevice.x1;
  y1 := FDevice.y1;
  x2 := FDevice.x2;
  y2 := FDevice.y2;
end;

procedure TAggTransViewport.WorldViewport(x1, y1, x2, y2: Double);
begin
  FWorld.x1 := x1;
  FWorld.y1 := y1;
  FWorld.x2 := x2;
  FWorld.y2 := y2;

  Update;
end;

procedure TAggTransViewport.WorldViewport(Rect: TRectDouble);
begin
  FWorld.x1 := Rect.x1;
  FWorld.y1 := Rect.y1;
  FWorld.x2 := Rect.x2;
  FWorld.y2 := Rect.y2;

  Update;
end;

procedure TAggTransViewport.WorldViewportNoUpdate(var x1, y1, x2, y2: Double);
begin
  x1 := FWorld.x1;
  y1 := FWorld.y1;
  x2 := FWorld.x2;
  y2 := FWorld.y2;
end;

procedure TAggTransViewport.WorldViewportNoUpdate(var Rect: TRectDouble);
begin
  Rect.x1 := FWorld.x1;
  Rect.y1 := FWorld.y1;
  Rect.x2 := FWorld.x2;
  Rect.y2 := FWorld.y2;
end;

procedure TAggTransViewport.WorldViewportActual(var x1, y1, x2, y2: Double);
begin
  x1 := FActualWorld.x1;
  y1 := FActualWorld.y1;
  x2 := FActualWorld.x2;
  y2 := FActualWorld.y2;
end;

procedure TAggTransViewport.InverseTransformScaleOnly(var x, y: Double);
begin
  x := x / FK.x;
  y := y / FK.y;
end;

function TAggTransViewport.GetDeviceDeltaX: Double;
begin
  Result := FDelta.x - FActualWorld.x1 * FK.x;
end;

function TAggTransViewport.GetDeviceDeltaY: Double;
begin
  Result := FDelta.y - FActualWorld.y1 * FK.y;
end;

function TAggTransViewport.GetScale: Double;
begin
  Result := (FK.x + FK.y) * 0.5;
end;

procedure TAggTransViewport.ToAffine(Mtx: TAggTransAffine);
var
  M: TAggTransAffineTranslation;
begin
  M := TAggTransAffineTranslation.Create(-FActualWorld.x1, -FActualWorld.y1);
  try
    M.Scale(FK);
    M.Translate(FDelta);
    Mtx.Assign(M);
  finally
      M.Free;
  end;
end;

procedure TAggTransViewport.ToAffineScaleOnly(Mtx: TAggTransAffine);
begin
  Mtx.Scale(FK)
end;

function TAggTransViewport.GetByteSize: Cardinal;
begin
  Result := SizeOf(Self);
end;

procedure TAggTransViewport.Update;
const
  cEpsilon: Double = 1E-30;
var
  d: Double;
  World: TRectDouble;
  Device: TRectDouble;
begin
  if (Abs(FWorld.x1 - FWorld.x2) < cEpsilon) or
    (Abs(FWorld.y1 - FWorld.y2) < cEpsilon) or
    (Abs(FDevice.x1 - FDevice.x2) < cEpsilon) or
    (Abs(FDevice.y1 - FDevice.y2) < cEpsilon) then
    begin
      FActualWorld.x1 := FWorld.x1;
      FActualWorld.y1 := FWorld.y1;
      FActualWorld.x2 := FWorld.x1 + 1; // possibly wrong???
      FActualWorld.y2 := FWorld.y2 + 1;
      FDelta.x := FDevice.x1;
      FDelta.y := FDevice.y1;
      FK.x := 1;
      FK.y := 1;

      FIsValid := False;
    end
  else
    begin
      World.x1 := FWorld.x1;
      World.y1 := FWorld.y1;
      World.x2 := FWorld.x2;
      World.y2 := FWorld.y2;
      Device.x1 := FDevice.x1;
      Device.y1 := FDevice.y1;
      Device.x2 := FDevice.x2;
      Device.y2 := FDevice.y2;

      if not(FAspect = arStretch) then
        begin
          FK.x := (Device.x2 - Device.x1) / (World.x2 - World.x1);
          FK.y := (Device.y2 - Device.y1) / (World.y2 - World.y1);

          if (FAspect = arMeet) = (FK.x < FK.y) then
            begin
              d := (World.y2 - World.y1) * FK.y / FK.x;

              World.y1 := World.y1 + ((World.y2 - World.y1 - d) * FAlign.y);
              World.y2 := World.y1 + d;
            end
          else
            begin
              d := (World.x2 - World.x1) * FK.x / FK.y;

              World.x1 := World.x1 + ((World.x2 - World.x1 - d) * FAlign.x);
              World.x2 := World.x1 + d;
            end;
        end;

      FActualWorld.x1 := World.x1;
      FActualWorld.y1 := World.y1;
      FActualWorld.x2 := World.x2;
      FActualWorld.y2 := World.y2;
      FDelta.x := Device.x1;
      FDelta.y := Device.y1;
      FK.x := (Device.x2 - Device.x1) / (World.x2 - World.x1);
      FK.y := (Device.y2 - Device.y1) / (World.y2 - World.y1);

      FIsValid := True;
    end;
end;

end. 
 
 
