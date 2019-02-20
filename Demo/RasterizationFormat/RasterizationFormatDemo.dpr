program RasterizationFormatDemo;

uses
  System.StartUpCopy,
  FMX.Forms,
  RasterizationFormatFrm in 'RasterizationFormatFrm.pas' {RasterizationFormatForm};

{$R *.res}

begin
  Application.Initialize;
  Application.CreateForm(TRasterizationFormatForm, RasterizationFormatForm);
  Application.Run;
end.
