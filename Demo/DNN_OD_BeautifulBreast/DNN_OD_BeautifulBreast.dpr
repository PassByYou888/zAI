program DNN_OD_BeautifulBreast;

uses
  System.StartUpCopy,
  FMX.Forms,
  DNN_OD_BeautifulBreast_Frm in 'DNN_OD_BeautifulBreast_Frm.pas' {Form1};

{$R *.res}

begin
  Application.Initialize;
  Application.CreateForm(TForm1, Form1);
  Application.Run;
end.
