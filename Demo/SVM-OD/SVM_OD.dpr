program SVM_OD;

uses
  System.StartUpCopy,
  FMX.Forms,
  ODDemoFrm in 'ODDemoFrm.pas' {ODDemoForm};

{$R *.res}

begin
  Application.Initialize;
  Application.CreateForm(TODDemoForm, ODDemoForm);
  Application.Run;
end.
