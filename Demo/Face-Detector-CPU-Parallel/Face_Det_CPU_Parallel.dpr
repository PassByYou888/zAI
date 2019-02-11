program Face_Det_CPU_Parallel;

uses
  System.StartUpCopy,
  FMX.Forms,
  Face_DetFrm_CPU_Parallel in 'Face_DetFrm_CPU_Parallel.pas' {Face_DetForm};

{$R *.res}

begin
  Application.Initialize;
  Application.CreateForm(TFace_DetForm, Face_DetForm);
  Application.Run;
end.
