object fTestServer: TfTestServer
  Left = 0
  Top = 0
  Caption = 'Test Server'
  ClientHeight = 310
  ClientWidth = 587
  Color = clBtnFace
  Font.Charset = DEFAULT_CHARSET
  Font.Color = clWindowText
  Font.Height = -11
  Font.Name = 'Tahoma'
  Font.Style = []
  OldCreateOrder = False
  OnCreate = FormCreate
  DesignSize = (
    587
    310)
  PixelsPerInch = 96
  TextHeight = 13
  object bCreate: TButton
    Left = 8
    Top = 8
    Width = 75
    Height = 25
    Caption = 'Create'
    TabOrder = 0
    OnClick = bCreateClick
  end
  object bSimCM: TButton
    Left = 320
    Top = 144
    Width = 75
    Height = 25
    Caption = 'Sim CM'
    TabOrder = 1
    OnClick = bSimCMClick
  end
  object eOperator: TJvEdit
    Left = 320
    Top = 120
    Width = 121
    Height = 21
    EmptyValue = 'Operator'
    TabOrder = 2
    Text = ''
    OnChange = eOperatorChange
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
    TabOrder = 3
  end
  object bSetRoute: TButton
    Left = 320
    Top = 32
    Width = 75
    Height = 25
    Caption = 'Set Route'
    TabOrder = 4
    OnClick = bSetRouteClick
  end
  object eRouteName: TJvEdit
    Left = 320
    Top = 8
    Width = 121
    Height = 21
    EmptyValue = 'Route Name'
    TabOrder = 5
    Text = ''
    OnChange = eOperatorChange
  end
  object bFree: TButton
    Left = 8
    Top = 40
    Width = 75
    Height = 25
    Caption = 'Free'
    TabOrder = 6
    OnClick = bFreeClick
  end
  object mCallLog: TMemo
    Left = 8
    Top = 175
    Width = 571
    Height = 127
    Anchors = [akLeft, akTop, akRight, akBottom]
    TabOrder = 7
  end
  object eCallId: TJvEdit
    Left = 320
    Top = 88
    Width = 121
    Height = 21
    EmptyValue = 'Operator'
    TabOrder = 8
    Text = ''
    OnChange = eOperatorChange
  end
end
