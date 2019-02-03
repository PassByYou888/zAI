program Face_Alignment;

uses
  System.StartUpCopy,
  FMX.Forms,
  Face_AlignmentFrm in 'Face_AlignmentFrm.pas' {Face_DetForm};

{$R *.res}

begin
  Application.Initialize;
  Application.CreateForm(TFace_DetForm, Face_DetForm);
  Application.Run;
end.
