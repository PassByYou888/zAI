program MemoryRasterMoreFont;

uses
  System.StartUpCopy,
  FMX.Forms,
  MemoryRasterMoreFontMainFrm in 'MemoryRasterMoreFontMainFrm.pas' {MemoryRasterMoreFontMainForm};

{$R *.res}

begin
  Application.Initialize;
  Application.CreateForm(TMemoryRasterMoreFontMainForm, MemoryRasterMoreFontMainForm);
  Application.Run;
end.
