object fTestDLL: TfTestDLL
  Left = 0
  Top = 0
  Caption = 'Test DLL'
  ClientHeight = 311
  ClientWidth = 631
  Color = clBtnFace
  Font.Charset = DEFAULT_CHARSET
  Font.Color = clWindowText
  Font.Height = -11
  Font.Name = 'Tahoma'
  Font.Style = []
  OldCreateOrder = False
  OnCreate = FormCreate
  OnShow = FormShow
  PixelsPerInch = 96
  TextHeight = 13
  object lFormHandle: TLabel
    Left = 8
    Top = 96
    Width = 57
    Height = 13
    Caption = 'FormHandle'
  end
  object lAppHandle: TLabel
    Left = 8
    Top = 120
    Width = 52
    Height = 13
    Caption = 'AppHandle'
  end
  object bTestHandle: TButton
    Left = 8
    Top = 8
    Width = 75
    Height = 25
    Caption = 'Test Handle'
    TabOrder = 0
    OnClick = bTestHandleClick
  end
  object bSimCM: TButton
    Left = 320
    Top = 176
    Width = 75
    Height = 25
    Caption = 'Sim CM'
    TabOrder = 1
    OnClick = bSimCMClick
  end
  object eDDI: TJvEdit
    Left = 320
    Top = 104
    Width = 121
    Height = 21
    EmptyValue = 'DDI'
    TabOrder = 2
    Text = ''
    OnChange = eDDIChange
  end
  object eCLI: TJvEdit
    Left = 320
    Top = 128
    Width = 121
    Height = 21
    EmptyValue = 'CLI'
    TabOrder = 3
    Text = ''
    OnChange = eDDIChange
  end
  object eOperator: TJvEdit
    Left = 320
    Top = 152
    Width = 121
    Height = 21
    EmptyValue = 'Operator'
    TabOrder = 4
    Text = ''
    OnChange = eDDIChange
  end
  object rgMessageType: TRadioGroup
    Left = 112
    Top = 3
    Width = 185
    Height = 166
    Caption = 'Message Type'
    Items.Strings = (
      'cmtNone'
      'cmtConnect'
      'cmtConnectAccept'
      'cmtDisconnect'
      'cmtDisconnectAccept'
      'cmtCallReceived'
      'cmtCallResult')
    TabOrder = 5
    OnClick = rgMessageTypeClick
  end
  object bSetRoute: TButton
    Left = 320
    Top = 64
    Width = 75
    Height = 25
    Caption = 'Set Route'
    TabOrder = 6
    OnClick = bSetRouteClick
  end
  object eRouteName: TJvEdit
    Left = 320
    Top = 8
    Width = 121
    Height = 21
    EmptyValue = 'Route Name'
    TabOrder = 7
    Text = ''
    OnChange = eDDIChange
  end
  object bTestReceive: TButton
    Left = 8
    Top = 40
    Width = 75
    Height = 25
    Caption = 'Test Receive'
    TabOrder = 8
    OnClick = bTestReceiveClick
  end
  object eCallId: TJvEdit
    Left = 464
    Top = 104
    Width = 121
    Height = 21
    EmptyValue = 'DDI'
    TabOrder = 9
    Text = ''
    OnChange = eDDIChange
  end
  object eChannelName: TJvEdit
    Left = 320
    Top = 32
    Width = 75
    Height = 21
    EmptyValue = 'Route Name'
    TabOrder = 10
    Text = ''
    OnChange = eDDIChange
  end
end
