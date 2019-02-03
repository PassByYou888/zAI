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
  DesignSize = (
    1059
    577)
  PixelsPerInch = 96
  TextHeight = 13
  object Memo1: TMemo
    Left = 24
    Top = 16
    Width = 1009
    Height = 425
    Anchors = [akLeft, akTop, akRight, akBottom]
    Lines.Strings = (
      #28145#24230#23398#20064#35757#32451':'#38754#37096#24230#37327#21270#32593#32476#25216#26415#25351#26631
      #25805#20316#31995#32479#25903#25345#65306'windows,linux'
      #22788#29702#22120#26550#26500#65306'intel x86,x64'
      'IOT'#65306#19981
      'CPU'#20869#23384#38656#27714#65306#39640#65292#20570'1000'#20154#30340#38754#37096#24230#37327#21270#38656#35201'160G'#20197#19978#20869#23384
      'GPU'#38656#27714#65306#38656#35201#65292'GPU'#20869#23384#38656#27714#27604#36739#39640#65292'8G'#36215#28857
      #24182#34892#65306#25903#25345
      #25968#25454#20860#23481#65306#20840#20860#23481
      ''
      #26412'Demo'#28436#31034#20102#23545'.imgMat'#25968#25454#65292#36827#34892#22823#35268#27169#22270#29255#30697#38453#36827#34892#20154#33080#35757#32451#30340#26041#27861
      #26368#21518#20250#36755#20986#19968#20010'5000'#20154#30340#38754#37096#24230#37327#21270#32593#32476#65292#20197#21450#36755#20986#19982'learn'#23545#24212#30340#21521#37327#24211
      ''
      'by.qq600585'
      ''
      '')
    ScrollBars = ssBoth
    TabOrder = 0
  end
  object FileEdit: TLabeledEdit
    Left = 24
    Top = 480
    Width = 193
    Height = 21
    Anchors = [akLeft, akBottom]
    EditLabel.Width = 60
    EditLabel.Height = 13
    EditLabel.Caption = #26679#26412#25968#25454#38598
    TabOrder = 1
    Text = 'face_db.imgMat'
  end
  object trainingButton: TButton
    Left = 24
    Top = 507
    Width = 121
    Height = 25
    Anchors = [akLeft, akBottom]
    Caption = #35757#32451
    TabOrder = 2
    OnClick = trainingButtonClick
  end
  object Timer1: TTimer
    Interval = 100
    OnTimer = Timer1Timer
    Left = 464
    Top = 368
  end
end
