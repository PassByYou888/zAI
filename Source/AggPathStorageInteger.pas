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
unit AggPathStorageInteger;

{$DEFINE FPC_DELPHI_MODE}
{$INCLUDE zDefine.inc}
interface
uses
  AggBasics,
  AggArray,
  AggVertexSource;

const
  CAggCmdMoveTo = 0;
  CAggCmdLineTo = 1;
  CAggCmdCurve3 = 2;
  CAggCmdCurve4 = 3;

  CAggCoordShift = 6;
  CAggCoordScale = 1 shl CAggCoordShift;

type
  PAggVertexInt16 = ^TAggVertexInt16;

  TAggVertexInt16 = record
  private
    x, y: Int16;
  public
    procedure Initialize(x, y: Int16; flag: Cardinal);

    function Vertex(x, y: PDouble; dx: Double = 0; dy: Double = 0; Scale: Double = 1.0): Cardinal;
  end;

  PAggVertexInt32 = ^TAggVertexInt32;

  TAggVertexInt32 = record
  private
    x, y: Int32;
  public
    procedure Initialize(x, y: Int32; flag: Cardinal);

    function Vertex(x, y: PDouble; dx: Double = 0; dy: Double = 0; Scale: Double = 1.0): Cardinal;
  end;

  TAggCustomPathStorageInteger = class(TAggVertexSource)
  public
    procedure MoveTo(x, y: Int32); virtual; abstract;
    procedure LineTo(x, y: Int32); virtual; abstract;
    procedure Curve3To(ControlX, ControlY, tox, ToY: Int32); virtual; abstract;
    procedure Curve4To(Control1X, Control1Y, Control2X, Control2Y, tox, ToY: Int32); virtual; abstract;
    function SetVertex(index: Cardinal; x, y: PInt32): Cardinal; virtual; abstract;
    procedure ClosePolygon; virtual; abstract;
  end;

  TAggPathStorageInt16 = class(TAggCustomPathStorageInteger)
  private
    FStorage: TAggPodDeque;
    FVertexIndex: Cardinal;
    FClosed: Boolean;
  public
    constructor Create;
    destructor Destroy; override;

    procedure RemoveAll; override;

    procedure MoveTo(x, y: Int32); override;
    procedure LineTo(x, y: Int32); override;
    procedure Curve3To(ControlX, ControlY, tox, ToY: Int32); override;
    procedure Curve4To(Control1X, Control1Y, Control2X, Control2Y, tox, ToY: Int32); override;

    procedure ClosePolygon; override;

    function Size: Cardinal;
    function SetVertex(index: Cardinal; x, y: PInt32): Cardinal; override;

    function ByteSize: Cardinal;
    procedure Serialize(PTR: PInt8u);

    procedure Rewind(PathID: Cardinal); override;
    function Vertex(x, y: PDouble): Cardinal; override;

    function GetBoundingRect: TRectDouble;
  end;

  TAggPathStorageInt32 = class(TAggCustomPathStorageInteger)
  private
    FStorage: TAggPodDeque;
    FVertexIndex: Cardinal;
    FClosed: Boolean;
  public
    constructor Create;
    destructor Destroy; override;

    procedure RemoveAll; override;

    procedure MoveTo(x, y: Int32); override;
    procedure LineTo(x, y: Int32); override;
    procedure Curve3To(ControlX, ControlY, tox, ToY: Int32); override;
    procedure Curve4To(Control1X, Control1Y, Control2X, Control2Y, tox, ToY: Int32); override;

    procedure ClosePolygon; override;

    function Size: Cardinal;
    function SetVertex(index: Cardinal; x, y: PInt32): Cardinal; override;

    function ByteSize: Cardinal;
    procedure Serialize(PTR: PInt8u);

    procedure Rewind(PathID: Cardinal); override;
    function Vertex(x, y: PDouble): Cardinal; override;

    function GetBoundingRect: TRectDouble;
  end;

  TAggCustomSerializedIntegerPathAdaptor = class(TAggVertexSource)
  public
    procedure Init(Data: PInt8u; Size: Cardinal; dx, dy: Double;
      Scale: Double = 1.0); virtual; abstract;
  end;

  TAggSerializedInt16PathAdaptor = class(TAggCustomSerializedIntegerPathAdaptor)
  private
    FData, FEnd: PInt8u;
    FInternalData: PInt8u;
    FDelta: TPointDouble;
    FScale: Double;
    FVertices: Cardinal;
  protected
    function GetPathCount: Cardinal; override;
  public
    constructor Create; overload;
    constructor Create(Data: PInt8u; Size: Cardinal; dx, dy: Double); overload;

    procedure Init(Data: PInt8u; Size: Cardinal; dx, dy: Double; Scale: Double = 1.0); override;

    procedure Rewind(PathID: Cardinal); override;
    function Vertex(x, y: PDouble): Cardinal; override;
  end;

  TAggSerializedInt32PathAdaptor = class(TAggCustomSerializedIntegerPathAdaptor)
  private
    FData, FEnd: PInt8u;
    FInternalData: PInt8u;
    FDelta: TPointDouble;
    FScale: Double;
    FVertices: Cardinal;
  protected
    function GetPathCount: Cardinal; override;
  public
    constructor Create; overload;
    constructor Create(Data: PInt8u; Size: Cardinal; dx, dy: Double); overload;

    procedure Init(Data: PInt8u; Size: Cardinal; dx, dy: Double; Scale: Double = 1.0); override;

    procedure Rewind(PathID: Cardinal); override;
    function Vertex(x, y: PDouble): Cardinal; override;
  end;

implementation


{ TAggVertexInt16 }

procedure TAggVertexInt16.Initialize(x, y: Int16; flag: Cardinal);
begin
  Self.x := ((x shl 1) and not 1) or (flag and 1);
  Self.y := ((y shl 1) and not 1) or (flag shr 1);
end;

function TAggVertexInt16.Vertex(x, y: PDouble; dx: Double = 0; dy: Double = 0;
  Scale: Double = 1.0): Cardinal;
begin
  x^ := dx + (ShrInt16(Self.x, 1) / CAggCoordScale) * Scale;
  y^ := dy + (ShrInt16(Self.y, 1) / CAggCoordScale) * Scale;

  case ((Self.y and 1) shl 1) or (Self.x and 1) of
    CAggCmdMoveTo:
      Result := CAggPathCmdMoveTo;

    CAggCmdLineTo:
      Result := CAggPathCmdLineTo;

    CAggCmdCurve3:
      Result := CAggPathCmdCurve3;

    CAggCmdCurve4:
      Result := CAggPathCmdCurve4;

    else
      Result := CAggPathCmdStop;
  end;
end;

{ TAggVertexInt32 }

procedure TAggVertexInt32.Initialize(x, y: Int32; flag: Cardinal);
begin
  Self.x := ((x shl 1) and not 1) or (flag and 1);
  Self.y := ((y shl 1) and not 1) or (flag shr 1);
end;

function TAggVertexInt32.Vertex(x, y: PDouble; dx: Double = 0; dy: Double = 0;
  Scale: Double = 1.0): Cardinal;
begin
  x^ := dx + (ShrInt32(Self.x, 1) / CAggCoordScale) * Scale;
  y^ := dy + (ShrInt32(Self.y, 1) / CAggCoordScale) * Scale;

  case ((Self.y and 1) shl 1) or (Self.x and 1) of
    CAggCmdMoveTo:
      Result := CAggPathCmdMoveTo;

    CAggCmdLineTo:
      Result := CAggPathCmdLineTo;

    CAggCmdCurve3:
      Result := CAggPathCmdCurve3;

    CAggCmdCurve4:
      Result := CAggPathCmdCurve4;

    else
      Result := CAggPathCmdStop;
  end;
end;

{ TAggPathStorageInt16 }

constructor TAggPathStorageInt16.Create;
begin
  FStorage := TAggPodDeque.Create(SizeOf(TAggVertexInt16));

  FVertexIndex := 0;
  FClosed := True;
end;

destructor TAggPathStorageInt16.Destroy;
begin
  FStorage.Free;
  inherited
end;

procedure TAggPathStorageInt16.RemoveAll;
begin
  FStorage.RemoveAll;
end;

procedure TAggPathStorageInt16.MoveTo;
var
  v: TAggVertexInt16;
begin
  v.Initialize(Int16(x), Int16(y), CAggCmdMoveTo);
  FStorage.Add(@v);
end;

procedure TAggPathStorageInt16.LineTo(x, y: Int32);
var
  v: TAggVertexInt16;
begin
  v.Initialize(Int16(x), Int16(y), CAggCmdLineTo);
  FStorage.Add(@v);
end;

procedure TAggPathStorageInt16.Curve3To(ControlX, ControlY, tox, ToY: Int32);
var
  v: TAggVertexInt16;
begin
  v.Initialize(Int16(ControlX), Int16(ControlY), CAggCmdCurve3);
  FStorage.Add(@v);

  v.Initialize(Int16(tox), Int16(ToY), CAggCmdCurve3);
  FStorage.Add(@v);
end;

procedure TAggPathStorageInt16.Curve4To(Control1X, Control1Y, Control2X, Control2Y, tox,
  ToY: Int32);
var
  v: TAggVertexInt16;
begin
  v.Initialize(Int16(Control1X), Int16(Control1Y), CAggCmdCurve4);
  FStorage.Add(@v);

  v.Initialize(Int16(Control2X), Int16(Control2Y), Int16(CAggCmdCurve4));
  FStorage.Add(@v);

  v.Initialize(Int16(tox), Int16(ToY), CAggCmdCurve4);
  FStorage.Add(@v);
end;

procedure TAggPathStorageInt16.ClosePolygon;
begin
end;

function TAggPathStorageInt16.Size;
begin
  Result := FStorage.Size;
end;

function TAggPathStorageInt16.SetVertex;
var
  v: PAggVertexInt16;
begin
  v := FStorage[index];

  PInt16(x)^ := ShrInt16(v.x, 1);
  PInt16(y)^ := ShrInt16(v.y, 1);

  Result := ((v.y and 1) shl 1) or (v.x and 1);
end;

function TAggPathStorageInt16.ByteSize;
begin
  Result := FStorage.Size * SizeOf(TAggVertexInt16);
end;

procedure TAggPathStorageInt16.Serialize;
var
  i: Cardinal;
begin
  i := 0;

  while i < FStorage.Size do
    begin
      Move(FStorage[i]^, PTR^, SizeOf(TAggVertexInt16));

      inc(PtrComp(PTR), SizeOf(TAggVertexInt16));
      inc(i);
    end;
end;

procedure TAggPathStorageInt16.Rewind(PathID: Cardinal);
begin
  FVertexIndex := 0;
  FClosed := True;
end;

function TAggPathStorageInt16.Vertex(x, y: PDouble): Cardinal;
var
  Cmd: Cardinal;
begin
  if (FStorage.Size < 2) or (FVertexIndex > FStorage.Size) then
    begin
      x^ := 0;
      y^ := 0;

      Result := CAggPathCmdStop;

      Exit;
    end;

  if FVertexIndex = FStorage.Size then
    begin
      x^ := 0;
      y^ := 0;

      inc(FVertexIndex);

      Result := CAggPathCmdEndPoly or CAggPathFlagsClose;

      Exit;
    end;

  Cmd := PAggVertexInt16(FStorage[FVertexIndex]).Vertex(x, y);

  if IsMoveTo(Cmd) and not FClosed then
    begin
      x^ := 0;
      y^ := 0;

      FClosed := True;

      Result := CAggPathCmdEndPoly or CAggPathFlagsClose;

      Exit;
    end;

  FClosed := False;

  inc(FVertexIndex);

  Result := Cmd;
end;

function TAggPathStorageInt16.GetBoundingRect: TRectDouble;
var
  i: Cardinal;
  x, y: Double;
begin
  Result := RectDouble(1E100, 1E100, -1E100, -1E100);

  if FStorage.Size = 0 then
      Result := RectDouble(0.0, 0.0, 0.0, 0.0)
  else
    for i := 0 to FStorage.Size - 1 do
      begin
        PAggVertexInt16(FStorage[i]).Vertex(@x, @y);

        if x < Result.x1 then
            Result.x1 := x;

        if y < Result.y1 then
            Result.y1 := y;

        if x > Result.x2 then
            Result.x2 := x;

        if y > Result.y2 then
            Result.y2 := y;
      end;
end;

{ TAggPathStorageInt32 }

constructor TAggPathStorageInt32.Create;
begin
  FStorage := TAggPodDeque.Create(SizeOf(TAggVertexInt32));

  FVertexIndex := 0;
  FClosed := True;
end;

destructor TAggPathStorageInt32.Destroy;
begin
  FStorage.Free;
  inherited;
end;

procedure TAggPathStorageInt32.RemoveAll;
begin
  FStorage.RemoveAll;
end;

procedure TAggPathStorageInt32.MoveTo;
var
  v: TAggVertexInt32;
begin
  v.Initialize(x, y, CAggCmdMoveTo);
  FStorage.Add(@v);
end;

procedure TAggPathStorageInt32.LineTo;
var
  v: TAggVertexInt32;
begin
  v.Initialize(x, y, CAggCmdLineTo);
  FStorage.Add(@v);
end;

procedure TAggPathStorageInt32.Curve3To;
var
  v: TAggVertexInt32;
begin
  v.Initialize(ControlX, ControlY, CAggCmdCurve3);
  FStorage.Add(@v);

  v.Initialize(tox, ToY, CAggCmdCurve3);
  FStorage.Add(@v);
end;

procedure TAggPathStorageInt32.Curve4To;
var
  v: TAggVertexInt32;
begin
  v.Initialize(Control1X, Control1Y, CAggCmdCurve4);
  FStorage.Add(@v);

  v.Initialize(Control2X, Control2Y, CAggCmdCurve4);
  FStorage.Add(@v);

  v.Initialize(tox, ToY, CAggCmdCurve4);
  FStorage.Add(@v);
end;

procedure TAggPathStorageInt32.ClosePolygon;
begin
end;

function TAggPathStorageInt32.Size;
begin
  Result := FStorage.Size;
end;

function TAggPathStorageInt32.SetVertex;
var
  v: PAggVertexInt32;
begin
  v := FStorage[index];

  x^ := ShrInt32(v.x, 1);
  y^ := ShrInt32(v.y, 1);

  Result := ((v.y and 1) shl 1) or (v.x and 1);
end;

function TAggPathStorageInt32.ByteSize;
begin
  Result := FStorage.Size * SizeOf(TAggVertexInt32);
end;

procedure TAggPathStorageInt32.Serialize;
var
  i: Cardinal;
begin
  i := 0;

  while i < FStorage.Size do
    begin
      Move(FStorage[i]^, PTR^, SizeOf(TAggVertexInt32));

      inc(PtrComp(PTR), SizeOf(TAggVertexInt32));
      inc(i);
    end;
end;

procedure TAggPathStorageInt32.Rewind(PathID: Cardinal);
begin
  FVertexIndex := 0;
  FClosed := True;
end;

function TAggPathStorageInt32.Vertex(x, y: PDouble): Cardinal;
var
  Cmd: Cardinal;
begin
  if (FStorage.Size < 2) or (FVertexIndex > FStorage.Size) then
    begin
      x^ := 0;
      y^ := 0;

      Result := CAggPathCmdStop;

      Exit;
    end;

  if FVertexIndex = FStorage.Size then
    begin
      x^ := 0;
      y^ := 0;

      inc(FVertexIndex);

      Result := CAggPathCmdEndPoly or CAggPathFlagsClose;

      Exit;
    end;

  Cmd := PAggVertexInt32(FStorage[FVertexIndex]).Vertex(x, y);

  if IsMoveTo(Cmd) and not FClosed then
    begin
      x^ := 0;
      y^ := 0;

      FClosed := True;

      Result := CAggPathCmdEndPoly or CAggPathFlagsClose;

      Exit;
    end;

  FClosed := False;

  inc(FVertexIndex);

  Result := Cmd;
end;

function TAggPathStorageInt32.GetBoundingRect;
var
  i: Cardinal;
  x, y: Double;
begin
  Result := RectDouble(1E100, 1E100, -1E100, -1E100);

  if FStorage.Size = 0 then
      Result := RectDouble(0.0, 0.0, 0.0, 0.0)
  else
    for i := 0 to FStorage.Size - 1 do
      begin
        PAggVertexInt32(FStorage[i]).Vertex(@x, @y);

        if x < Result.x1 then
            Result.x1 := x;

        if y < Result.y1 then
            Result.y1 := y;

        if x > Result.x2 then
            Result.x2 := x;

        if y > Result.y2 then
            Result.y2 := y;
      end;
end;

{ TAggSerializedInt16PathAdaptor }

constructor TAggSerializedInt16PathAdaptor.Create;
begin
  FData := nil;
  FEnd := nil;
  FInternalData := nil;

  FDelta.x := 0.0;
  FDelta.y := 0.0;

  FScale := 1.0;
  FVertices := 0;
end;

constructor TAggSerializedInt16PathAdaptor.Create(Data: PInt8u;
  Size: Cardinal; dx, dy: Double);
begin
  FData := Data;
  FEnd := PInt8u(PtrComp(Data) + Size);
  FInternalData := Data;

  FDelta.x := dx;
  FDelta.y := dy;

  FScale := 0.0;
  FVertices := 0;
end;

procedure TAggSerializedInt16PathAdaptor.Init;
begin
  FData := Data;
  FEnd := PInt8u(PtrComp(Data) + Size);
  FInternalData := Data;

  FDelta.x := dx;
  FDelta.y := dy;

  FScale := Scale;
  FVertices := 0;
end;

function TAggSerializedInt16PathAdaptor.GetPathCount: Cardinal;
begin
  Result := FVertices;
end;

procedure TAggSerializedInt16PathAdaptor.Rewind(PathID: Cardinal);
begin
  FInternalData := FData;

  FVertices := 0;
end;

function TAggSerializedInt16PathAdaptor.Vertex(x, y: PDouble): Cardinal;
var
  v: TAggVertexInt16;
  Cmd: Cardinal;
begin
  if (FData = nil) or (PtrComp(FInternalData) > PtrComp(FEnd)) then
    begin
      x^ := 0;
      y^ := 0;

      Result := CAggPathCmdStop;

      Exit;
    end;

  if PtrComp(FInternalData) = PtrComp(FEnd) then
    begin
      x^ := 0;
      y^ := 0;

      inc(PtrComp(FInternalData), SizeOf(TAggVertexInt16));

      Result := CAggPathCmdEndPoly or CAggPathFlagsClose;

      Exit;
    end;

  Move(FInternalData^, v, SizeOf(TAggVertexInt16));

  Cmd := v.Vertex(x, y, FDelta.x, FDelta.y, FScale);

  if IsMoveTo(Cmd) and (FVertices > 2) then
    begin
      x^ := 0;
      y^ := 0;

      FVertices := 0;

      Result := CAggPathCmdEndPoly or CAggPathFlagsClose;

      Exit;
    end;

  inc(FVertices);
  inc(PtrComp(FInternalData), SizeOf(TAggVertexInt16));

  Result := Cmd;
end;

{ TAggSerializedInt32PathAdaptor }

constructor TAggSerializedInt32PathAdaptor.Create;
begin
  FData := nil;
  FEnd := nil;
  FInternalData := nil;

  FDelta.x := 0.0;
  FDelta.y := 0.0;

  FScale := 1.0;
  FVertices := 0;
end;

constructor TAggSerializedInt32PathAdaptor.Create(Data: PInt8u;
  Size: Cardinal; dx, dy: Double);
begin
  FData := Data;
  FEnd := PInt8u(PtrComp(Data) + Size);
  FInternalData := Data;

  FDelta.x := dx;
  FDelta.y := dy;

  FScale := 0.0;
  FVertices := 0;
end;

procedure TAggSerializedInt32PathAdaptor.Init;
begin
  FData := Data;
  FEnd := PInt8u(PtrComp(Data) + Size);
  FInternalData := Data;

  FDelta.x := dx;
  FDelta.y := dy;

  FScale := Scale;
  FVertices := 0;
end;

function TAggSerializedInt32PathAdaptor.GetPathCount: Cardinal;
begin
  Result := FVertices;
end;

procedure TAggSerializedInt32PathAdaptor.Rewind(PathID: Cardinal);
begin
  FInternalData := FData;

  FVertices := 0;
end;

function TAggSerializedInt32PathAdaptor.Vertex(x, y: PDouble): Cardinal;
var
  v: TAggVertexInt32;
  Cmd: Cardinal;
begin
  if (FData = nil) or (PtrComp(FInternalData) > PtrComp(FEnd)) then
    begin
      x^ := 0;
      y^ := 0;

      Result := CAggPathCmdStop;
      Exit;
    end;

  if PtrComp(FInternalData) = PtrComp(FEnd) then
    begin
      x^ := 0;
      y^ := 0;

      inc(PtrComp(FInternalData), SizeOf(TAggVertexInt32));

      Result := CAggPathCmdEndPoly or CAggPathFlagsClose;
      Exit;
    end;

  Move(FInternalData^, v, SizeOf(TAggVertexInt32));

  Cmd := v.Vertex(x, y, FDelta.x, FDelta.y, FScale);

  if IsMoveTo(Cmd) and (FVertices > 2) then
    begin
      x^ := 0;
      y^ := 0;

      FVertices := 0;

      Result := CAggPathCmdEndPoly or CAggPathFlagsClose;
      Exit;
    end;

  inc(FVertices);
  inc(PtrComp(FInternalData), SizeOf(TAggVertexInt32));

  Result := Cmd;
end;

end. 
 
 
 
