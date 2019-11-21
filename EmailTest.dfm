object Form1: TForm1
  Left = 505
  Top = 146
  Caption = 'memo'
  ClientHeight = 464
  ClientWidth = 908
  Color = clBtnFace
  Font.Charset = DEFAULT_CHARSET
  Font.Color = clWindowText
  Font.Height = -11
  Font.Name = 'Tahoma'
  Font.Style = []
  OldCreateOrder = False
  OnCreate = FormCreate
  OnDestroy = FormDestroy
  DesignSize = (
    908
    464)
  PixelsPerInch = 96
  TextHeight = 13
  object lContent: TLabel
    Left = 466
    Top = 206
    Width = 39
    Height = 13
    Anchors = [akTop, akRight]
    Caption = 'Content'
    ExplicitLeft = 662
  end
  object lAuthType: TLabel
    Left = 455
    Top = 59
    Width = 50
    Height = 13
    Anchors = [akTop, akRight]
    Caption = 'Auth Type'
    ExplicitLeft = 651
  end
  object lTlsMode: TLabel
    Left = 463
    Top = 83
    Width = 42
    Height = 13
    Anchors = [akTop, akRight]
    Caption = 'Tls Mode'
    ExplicitLeft = 659
  end
  object Label1: TLabel
    Left = 443
    Top = 286
    Width = 62
    Height = 13
    Anchors = [akTop, akRight]
    Caption = 'Substitutions'
  end
  object mHTML: TMemo
    Left = 8
    Top = 8
    Width = 421
    Height = 450
    Anchors = [akLeft, akTop, akRight, akBottom]
    Ctl3D = False
    ParentCtl3D = False
    ScrollBars = ssBoth
    TabOrder = 0
    WordWrap = False
  end
  object eHost: TLabeledEdit
    Left = 508
    Top = 8
    Width = 205
    Height = 19
    Anchors = [akTop, akRight]
    Ctl3D = False
    EditLabel.Width = 22
    EditLabel.Height = 13
    EditLabel.Caption = 'Host'
    LabelPosition = lpLeft
    ParentCtl3D = False
    TabOrder = 1
  end
  object ePort: TLabeledEdit
    Left = 508
    Top = 32
    Width = 53
    Height = 19
    Anchors = [akTop, akRight]
    Ctl3D = False
    EditLabel.Width = 20
    EditLabel.Height = 13
    EditLabel.Caption = 'Port'
    LabelPosition = lpLeft
    ParentCtl3D = False
    TabOrder = 2
  end
  object eUserName: TLabeledEdit
    Left = 508
    Top = 104
    Width = 205
    Height = 19
    Anchors = [akTop, akRight]
    Ctl3D = False
    EditLabel.Width = 52
    EditLabel.Height = 13
    EditLabel.Caption = 'User Name'
    LabelPosition = lpLeft
    ParentCtl3D = False
    TabOrder = 3
  end
  object ePassword: TLabeledEdit
    Left = 508
    Top = 128
    Width = 205
    Height = 19
    Anchors = [akTop, akRight]
    Ctl3D = False
    EditLabel.Width = 46
    EditLabel.Height = 13
    EditLabel.Caption = 'Password'
    LabelPosition = lpLeft
    ParentCtl3D = False
    TabOrder = 4
  end
  object eFromName: TLabeledEdit
    Left = 508
    Top = 152
    Width = 205
    Height = 19
    Anchors = [akTop, akRight]
    Ctl3D = False
    EditLabel.Width = 54
    EditLabel.Height = 13
    EditLabel.Caption = 'From Name'
    LabelPosition = lpLeft
    ParentCtl3D = False
    TabOrder = 5
  end
  object eFromAddress: TLabeledEdit
    Left = 508
    Top = 176
    Width = 205
    Height = 19
    Anchors = [akTop, akRight]
    Ctl3D = False
    EditLabel.Width = 66
    EditLabel.Height = 13
    EditLabel.Caption = 'From Address'
    LabelPosition = lpLeft
    ParentCtl3D = False
    TabOrder = 6
  end
  object lbContent: TListBox
    Left = 507
    Top = 203
    Width = 393
    Height = 78
    Anchors = [akTop, akRight, akBottom]
    Ctl3D = False
    ItemHeight = 13
    ParentCtl3D = False
    TabOrder = 7
  end
  object cbAuthType: TComboBox
    Left = 508
    Top = 56
    Width = 121
    Height = 21
    Anchors = [akTop, akRight]
    Ctl3D = False
    ItemHeight = 13
    ParentCtl3D = False
    TabOrder = 8
    ExplicitLeft = 704
  end
  object cbTlsMode: TComboBox
    Left = 508
    Top = 80
    Width = 121
    Height = 21
    Anchors = [akTop, akRight]
    Ctl3D = False
    ItemHeight = 13
    ParentCtl3D = False
    TabOrder = 9
    ExplicitLeft = 704
  end
  object bLoadIni: TButton
    Left = 508
    Top = 433
    Width = 75
    Height = 25
    Anchors = [akRight, akBottom]
    Caption = 'Load Ini'
    TabOrder = 10
    OnClick = bLoadIniClick
    ExplicitLeft = 704
  end
  object bSend: TButton
    Left = 589
    Top = 433
    Width = 75
    Height = 25
    Caption = 'Send'
    TabOrder = 11
    OnClick = bSendClick
  end
  object eEmailTo: TLabeledEdit
    Left = 508
    Top = 383
    Width = 205
    Height = 19
    Anchors = [akTop, akRight]
    Ctl3D = False
    EditLabel.Width = 39
    EditLabel.Height = 13
    EditLabel.Caption = 'Email To'
    LabelPosition = lpLeft
    ParentCtl3D = False
    TabOrder = 12
    Text = 'mdspf@microdata.systems'
  end
  object eVoucher: TLabeledEdit
    Left = 508
    Top = 359
    Width = 93
    Height = 19
    Anchors = [akTop, akRight]
    Ctl3D = False
    EditLabel.Width = 39
    EditLabel.Height = 13
    EditLabel.Caption = 'Voucher'
    LabelPosition = lpLeft
    ParentCtl3D = False
    TabOrder = 13
  end
  object eEmailSubject: TLabeledEdit
    Left = 508
    Top = 407
    Width = 392
    Height = 19
    Anchors = [akTop, akRight]
    Ctl3D = False
    EditLabel.Width = 63
    EditLabel.Height = 13
    EditLabel.Caption = 'Email Subject'
    LabelPosition = lpLeft
    ParentCtl3D = False
    TabOrder = 14
    Text = 'A test email'
  end
  object lbSubstitutions: TListBox
    Left = 507
    Top = 283
    Width = 393
    Height = 70
    Anchors = [akTop, akRight, akBottom]
    Ctl3D = False
    ItemHeight = 13
    ParentCtl3D = False
    TabOrder = 15
  end
  object odIni: TFileOpenDialog
    DefaultExtension = '.ini'
    FavoriteLinks = <>
    FileTypes = <
      item
        DisplayName = 'Ini File'
        FileMask = '*.ini'
      end>
    Options = [fdoFileMustExist]
    Left = 872
    Top = 8
  end
  object IdIOHandlerStream1: TIdIOHandlerStream
    MaxLineAction = maException
    Port = 0
    FreeStreams = False
    Left = 768
    Top = 128
  end
end
