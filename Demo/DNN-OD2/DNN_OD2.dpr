program DNN_OD2;

uses
  System.StartUpCopy,
  FMX.Forms,
  DNN_OD2_Frm in 'DNN_OD2_Frm.pas' {Form1};

{$R *.res}

begin
  Application.Initialize;
  Application.CreateForm(TForm1, Form1);
  Application.Run;
end.
