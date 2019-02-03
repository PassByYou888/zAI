program SVM_OD;

uses
  System.StartUpCopy,
  FMX.Forms,
  ODDemoFrm in 'ODDemoFrm.pas' {Form1};

{$R *.res}

begin
  Application.Initialize;
  Application.CreateForm(TForm1, Form1);
  Application.Run;
end.
