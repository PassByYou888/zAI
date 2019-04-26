program GaussPyramidDemo;

uses
  System.StartUpCopy,
  FMX.Forms,
  GaussPyramidsFrm in 'GaussPyramidsFrm.pas' {GaussPyramidsForm};

{$R *.res}

begin
  Application.Initialize;
  Application.CreateForm(TGaussPyramidsForm, GaussPyramidsForm);
  Application.Run;
end.
