{ ****************************************************************************** }
{ * poly manager create by qq600585                                            * }
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
unit zNavigationPoly;

{$INCLUDE zDefine.inc}

interface

uses CoreClasses, Math, Geometry2DUnit;

type
  TPolyManager = class;

  TPolyManagerChildren = class(TDeflectionPolygon)
  protected
    FOwner: TPolyManager;
    FIndex: Integer;
  public
    constructor Create(AOwner: TPolyManager);
    destructor Destroy; override;

    property Owner: TPolyManager read FOwner;

    // cache in connect use
    property index: Integer read FIndex;
  end;

  TPolyManager = class(TCoreClassPersistent)
  protected
    FPolyList: TCoreClassListForObj;
    FScene: TPolyManagerChildren;
  public
    constructor Create;
    destructor Destroy; override;

    procedure Clear;
    procedure Add(Poly: TPolyManagerChildren);
    function AddPointList(pl: TVec2List): TPolyManagerChildren;
    procedure AddConvexHullPointList(pl: TVec2List);
    function Count: Integer;
    procedure Delete(idx: Integer);
    procedure DeletePoly(p: TPolyManagerChildren);
    function GetPoly(index: Integer): TPolyManagerChildren;
    property Poly[index: Integer]: TPolyManagerChildren read GetPoly; default;
    function Last: TPolyManagerChildren;
    procedure AssignFrom(APoly: TPolyManager);

    function PointOk(AExpandDist: TGeoFloat; pt: TVec2): Boolean;
    function LineIntersect(AExpandDist: TGeoFloat; lb, le: TVec2): Boolean;
    function GetNearLine(AExtandDistance: TGeoFloat; const pt: TVec2): TVec2;
    function Collision2Circle(cp: TVec2; r: TGeoFloat; OutputList: TDeflectionPolygonLines): Boolean;

    procedure Rebuild;
    procedure Reverse;
    procedure SetScale(v: TGeoFloat);
    procedure SetAngle(v: TGeoFloat);
    procedure ReverseY;
    procedure ReverseX;

    property Scene: TPolyManagerChildren read FScene;
  end;

implementation

constructor TPolyManagerChildren.Create(AOwner: TPolyManager);
begin
  inherited Create;
  FOwner := AOwner;
end;

destructor TPolyManagerChildren.Destroy;
begin
  inherited Destroy;
end;

constructor TPolyManager.Create;
begin
  inherited Create;
  FPolyList := TCoreClassListForObj.Create;
  FScene := TPolyManagerChildren.Create(Self);
  FScene.ExpandMode := emConcave;
  FScene.FIndex := 0;
end;

destructor TPolyManager.Destroy;
begin
  Clear;
  DisposeObject(FPolyList);
  DisposeObject(FScene);
  inherited Destroy;
end;

procedure TPolyManager.Clear;
var
  i: Integer;
begin
  for i := 0 to FPolyList.Count - 1 do
      DisposeObject(TPolyManagerChildren(FPolyList[i]));
  FPolyList.Clear;
  FScene.Clear;
end;

procedure TPolyManager.Add(Poly: TPolyManagerChildren);
begin
  Poly.FIndex := FPolyList.Add(Poly) + 1;
end;

function TPolyManager.AddPointList(pl: TVec2List): TPolyManagerChildren;
var
  APoly: TPolyManagerChildren;
begin
  APoly := TPolyManagerChildren.Create(Self);
  APoly.ExpandMode := emConvex;
  APoly.Rebuild(pl, True);
  Add(APoly);
  Result := APoly;
end;

procedure TPolyManager.AddConvexHullPointList(pl: TVec2List);
var
  APoly: TPolyManagerChildren;
begin
  APoly := TPolyManagerChildren.Create(Self);
  APoly.ExpandMode := emConvex;
  APoly.ConvexHullFrom(pl);
  Add(APoly);
end;

function TPolyManager.Count: Integer;
begin
  Result := FPolyList.Count;
end;

procedure TPolyManager.Delete(idx: Integer);
var
  i: Integer;
begin
  DisposeObject(TPolyManagerChildren(FPolyList[idx]));
  FPolyList.Delete(idx);
  for i := 0 to Count - 1 do
      Poly[i].FIndex := i + 1;
end;

procedure TPolyManager.DeletePoly(p: TPolyManagerChildren);
var
  i: Integer;
begin
  i := 0;
  while i < Count do
    if Poly[i] = p then
        Delete(i)
    else
        inc(i);
end;

function TPolyManager.GetPoly(index: Integer): TPolyManagerChildren;
begin
  Result := FPolyList[index] as TPolyManagerChildren;
end;

function TPolyManager.Last: TPolyManagerChildren;
begin
  Result := Poly[Count - 1];
end;

procedure TPolyManager.AssignFrom(APoly: TPolyManager);
var
  i: Integer;
  np: TPolyManagerChildren;
begin
  Clear;
  for i := 0 to APoly.Count - 1 do
    begin
      np := TPolyManagerChildren.Create(Self);
      np.CopyPoly(APoly[i], False);
      Add(np);
    end;
  FScene.CopyPoly(APoly.Scene, False);
end;

function TPolyManager.PointOk(AExpandDist: TGeoFloat; pt: TVec2): Boolean;
var
  i: Integer;
begin
  Result := False;
  if not FScene.InHere(AExpandDist, pt) then
      Exit;
  for i := 0 to Count - 1 do
    if GetPoly(i).InHere(AExpandDist, pt) then
        Exit;
  Result := True;
end;

function TPolyManager.LineIntersect(AExpandDist: TGeoFloat; lb, le: TVec2): Boolean;
var
  i: Integer;
begin
  Result := False;
  for i := 0 to Count - 1 do
    begin
      if Poly[i].LineIntersect(AExpandDist, lb, le, True) then
        begin
          Result := True;
          Exit;
        end;
    end;

  if FScene.LineIntersect(AExpandDist, lb, le, True) then
      Result := True;
end;

function TPolyManager.GetNearLine(AExtandDistance: TGeoFloat; const pt: TVec2): TVec2;
var
  i: Integer;
  d, d2: TGeoFloat;
  lb, le: Integer;
  r: TVec2;
begin
  Result := FScene.GetNearLine(AExtandDistance, pt, True, lb, le);
  d := PointDistance(pt, Result);

  for i := 0 to Count - 1 do
    begin
      r := Poly[i].GetNearLine(AExtandDistance, pt, True, lb, le);
      d2 := PointDistance(pt, r);
      if (d2 < d) then
        begin
          Result := r;
          d := d2;
        end;
    end;
end;

function TPolyManager.Collision2Circle(cp: TVec2; r: TGeoFloat; OutputList: TDeflectionPolygonLines): Boolean;
var
  i: Integer;
begin
  Result := False;
  for i := 0 to Count - 1 do
    begin
      if Poly[i].Collision2Circle(cp, r, True, OutputList) then
          Result := True;
    end;

  if FScene.Collision2Circle(cp, r, True, OutputList) then
      Result := True;
end;

procedure TPolyManager.Rebuild;
var
  i: Integer;
begin
  for i := 0 to Count - 1 do
      Poly[i].Rebuild;
  FScene.Rebuild;
end;

procedure TPolyManager.Reverse;
var
  i: Integer;
begin
  for i := 0 to Count - 1 do
      Poly[i].Reverse;
  FScene.Reverse;
end;

procedure TPolyManager.SetScale(v: TGeoFloat);
var
  i: Integer;
begin
  for i := 0 to Count - 1 do
      Poly[i].Scale := v;
  FScene.Scale := v;
end;

procedure TPolyManager.SetAngle(v: TGeoFloat);
var
  i: Integer;
begin
  for i := 0 to Count - 1 do
      Poly[i].angle := v;
  FScene.angle := v;
end;

procedure TPolyManager.ReverseY;
  procedure RY(p: TPolyManagerChildren);
  var
    i: Integer;
    pt: TVec2;
  begin
    for i := 0 to p.Count - 1 do
      begin
        pt := p.Points[i];
        pt[1] := -pt[1];
        p.Points[i] := pt;
      end;
  end;

var
  i: Integer;
begin
  for i := 0 to Count - 1 do
      RY(Poly[i]);
  RY(FScene);
end;

procedure TPolyManager.ReverseX;
  procedure RY(p: TPolyManagerChildren);
  var
    i: Integer;
    pt: TVec2;
  begin
    for i := 0 to p.Count - 1 do
      begin
        pt := p.Points[i];
        pt[0] := -pt[0];
        p.Points[i] := pt;
      end;
  end;

var
  i: Integer;
begin
  for i := 0 to Count - 1 do
      RY(Poly[i]);
  RY(FScene);
end;

end.
