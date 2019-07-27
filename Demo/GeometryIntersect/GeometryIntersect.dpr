program GeometryIntersect;

uses
  System.StartUpCopy,
  FMX.Forms,
  GeometryIntersectFrm in 'GeometryIntersectFrm.pas' {GeometryIntersectForm};

{$R *.res}

begin
  Application.Initialize;
  Application.CreateForm(TGeometryIntersectForm, GeometryIntersectForm);
  Application.Run;
end.
