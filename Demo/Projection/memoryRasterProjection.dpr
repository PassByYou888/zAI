program memoryRasterProjection;

uses
  System.StartUpCopy,
  FMX.Forms,
  ProjectionFrm in 'ProjectionFrm.pas' {ProjectionForm};

{$R *.res}

begin
  Application.Initialize;
  Application.CreateForm(TProjectionForm, ProjectionForm);
  Application.Run;
end.
