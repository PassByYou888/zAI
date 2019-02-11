program Face_Alignment_Parallel;

uses
  System.StartUpCopy,
  FMX.Forms,
  Face_AlignmentFrm_Parallel in 'Face_AlignmentFrm_Parallel.pas' {Face_AlignmentForm_Parallel};

{$R *.res}

begin
  Application.Initialize;
  Application.CreateForm(TFace_DetForm, Face_DetForm);
  Application.Run;
end.
