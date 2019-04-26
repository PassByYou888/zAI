program GaussPyramidRegionDemo;

uses
  System.StartUpCopy,
  FMX.Forms,
  GaussPyramidsRegionFrm in 'GaussPyramidsRegionFrm.pas' {GaussPyramidsForm};

{$R *.res}

begin
  Application.Initialize;
  Application.CreateForm(TGaussPyramidsForm, GaussPyramidsForm);
  Application.Run;
end.
