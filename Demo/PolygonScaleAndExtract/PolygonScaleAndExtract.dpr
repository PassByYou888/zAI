program PolygonScaleAndExtract;

uses
  System.StartUpCopy,
  FMX.Forms,
  PolygonScaleAndExtractMainFrm in 'PolygonScaleAndExtractMainFrm.pas' {PolygonScaleAndExtractMainForm};

{$R *.res}

begin
  Application.Initialize;
  Application.CreateForm(TPolygonScaleAndExtractMainForm, PolygonScaleAndExtractMainForm);
  Application.Run;
end.
