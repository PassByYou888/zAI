program TriangleOptimization;

uses
  System.StartUpCopy,
  FMX.Forms,
  TriangleOptimizationMainFrm in 'TriangleOptimizationMainFrm.pas' {TriangleOptimizationMainForm};

{$R *.res}

begin
  Application.Initialize;
  Application.CreateForm(TTriangleOptimizationMainForm, TriangleOptimizationMainForm);
  Application.Run;
end.
