object Form2: TForm2
  Left = 0
  Top = 0
  Caption = 'Training .ImgMat'
  ClientHeight = 577
  ClientWidth = 1059
  Color = clBtnFace
  Font.Charset = DEFAULT_CHARSET
  Font.Color = clWindowText
  Font.Height = -11
  Font.Name = 'Tahoma'
  Font.Style = []
  OldCreateOrder = False
  OnCreate = FormCreate
  PixelsPerInch = 96
  TextHeight = 13
  object Memo1: TMemo
    Left = 24
    Top = 16
    Width = 1009
    Height = 425
    Lines.Strings = (
      'SP'#30340#25216#26415#25351#26631
      #25805#20316#31995#32479#65306'ios,android,windows,linux'
      #22788#29702#22120#26550#26500#65306'intel x86,x64,arm32,arm64'
      'IOT'#65306#25903#25345
      #20869#23384#38656#27714#65306#20302
      'GPU'#38656#27714#65306#26080
      #24182#34892#65306#25903#25345
      #25968#25454#20860#23481#65306#20840#20860#23481
      #23454#26102#24615#65306#20013#39640#65292#23454#26102#35745#31639#33021#28385#36275'100fps'#30340#38656#27714'('#19981#21253#21547'OD'#24320#38144')'
      ''
      'SP'#30340#35757#32451#21644'OD'#31867#20284#65292
      #35813'Demo'#21482#28436#31034#23545'.ImgDataset'#20570#35757#32451
      #22522#20110'XML,ImgMat'#31561#31561#35757#32451#26041#27861#65292#21487#21442#32771'OD'#30340#35757#32451
      ''
      'by.qq600585')
    TabOrder = 0
  end
  object FileEdit: TLabeledEdit
    Left = 24
    Top = 480
    Width = 193
    Height = 21
    EditLabel.Width = 60
    EditLabel.Height = 13
    EditLabel.Caption = #26679#26412#25968#25454#38598
    TabOrder = 1
    Text = 'bear_sp.imgDataset'
  end
  object trainingButton: TButton
    Left = 24
    Top = 507
    Width = 121
    Height = 25
    Caption = #35757#32451
    TabOrder = 2
    OnClick = trainingButtonClick
  end
  object SaveDialog: TSaveDialog
    Left = 464
    Top = 432
  end
  object Timer1: TTimer
    Interval = 100
    OnTimer = Timer1Timer
    Left = 464
    Top = 368
  end
end
