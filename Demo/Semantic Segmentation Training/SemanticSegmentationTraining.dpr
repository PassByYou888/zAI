program SemanticSegmentationTraining;

uses
  System.StartUpCopy,
  FMX.Forms,
  SSMainFrm in 'SSMainFrm.pas' {SSMainForm},
  ShowImageFrm in '..\Reponse_FaceServer\ShowImageFrm.pas' {ShowImageForm};

{$R *.res}

begin
  Application.Initialize;
  Application.CreateForm(TSSMainForm, SSMainForm);
  Application.Run;
end.
