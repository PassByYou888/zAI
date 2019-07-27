program ColorSegmentation;

uses
  System.StartUpCopy,
  FMX.Forms,
  ColorSegmentationMainFrm in 'ColorSegmentationMainFrm.pas' {ColorSegmentationMainForm};

{$R *.res}

begin
  Application.Initialize;
  Application.CreateForm(TColorSegmentationMainForm, ColorSegmentationMainForm);
  Application.Run;
end.
