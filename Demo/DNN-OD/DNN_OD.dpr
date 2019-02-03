program DNN_OD;

uses
  System.StartUpCopy,
  FMX.Forms,
  DNN_OD_DemoFrm in 'DNN_OD_DemoFrm.pas' {DNN_OD_Form};

{$R *.res}

begin
  Application.Initialize;
  Application.CreateForm(TDNN_OD_Form, DNN_OD_Form);
  Application.Run;
end.
