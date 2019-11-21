object fMain: TfMain
  Left = 905
  Top = 493
  Caption = 'Call Router Test'
  ClientHeight = 199
  ClientWidth = 562
  Color = clBtnFace
  Font.Charset = DEFAULT_CHARSET
  Font.Color = clWindowText
  Font.Height = -11
  Font.Name = 'Tahoma'
  Font.Style = []
  OldCreateOrder = False
  Position = poScreenCenter
  OnClose = FormClose
  PixelsPerInch = 96
  TextHeight = 13
  object Label1: TLabel
    Left = 38
    Top = 11
    Width = 78
    Height = 13
    Alignment = taRightJustify
    Caption = 'Call Router Host'
  end
  object Label2: TLabel
    Left = 40
    Top = 35
    Width = 76
    Height = 13
    Alignment = taRightJustify
    Caption = 'Call Router Port'
  end
  object Label3: TLabel
    Left = 18
    Top = 59
    Width = 98
    Height = 13
    Alignment = taRightJustify
    Caption = 'Calling Number (CLI)'
  end
  object Label4: TLabel
    Left = 18
    Top = 83
    Width = 98
    Height = 13
    Alignment = taRightJustify
    Caption = 'Called Number (DDI)'
  end
  object Label5: TLabel
    Left = 42
    Top = 107
    Width = 74
    Height = 13
    Alignment = taRightJustify
    Caption = 'Operator Name'
  end
  object Label6: TLabel
    Left = 58
    Top = 131
    Width = 58
    Height = 13
    Alignment = taRightJustify
    Caption = 'Operator ID'
  end
  object eHost: TEdit
    Left = 120
    Top = 8
    Width = 121
    Height = 21
    TabOrder = 0
    Text = 'localhost'
  end
  object ePort: TEdit
    Left = 120
    Top = 32
    Width = 121
    Height = 21
    TabOrder = 1
    Text = '7777'
  end
  object eCLI: TEdit
    Left = 120
    Top = 56
    Width = 121
    Height = 21
    TabOrder = 2
  end
  object eDDI: TEdit
    Left = 120
    Top = 80
    Width = 121
    Height = 21
    TabOrder = 3
  end
  object eOperName: TEdit
    Left = 120
    Top = 104
    Width = 121
    Height = 21
    TabOrder = 4
  end
  object eOperId: TEdit
    Left = 120
    Top = 128
    Width = 121
    Height = 21
    TabOrder = 5
  end
  object bClose: TButton
    Left = 8
    Top = 168
    Width = 75
    Height = 25
    Caption = '&Close'
    TabOrder = 7
    OnClick = bCloseClick
  end
  object bTest: TButton
    Left = 248
    Top = 6
    Width = 75
    Height = 25
    Caption = '&Test'
    TabOrder = 6
    OnClick = bTestClick
  end
  object mResponse: TMemo
    Left = 329
    Top = 32
    Width = 225
    Height = 93
    TabOrder = 8
  end
  object eURL: TEdit
    Left = 329
    Top = 8
    Width = 225
    Height = 21
    TabOrder = 9
  end
  object eError: TEdit
    Left = 329
    Top = 128
    Width = 225
    Height = 21
    CharCase = ecLowerCase
    TabOrder = 10
  end
  object idHTTP: TIdHTTP
    AllowCookies = True
    ProxyParams.BasicAuthentication = False
    ProxyParams.ProxyPort = 0
    Request.ContentLength = -1
    Request.ContentRangeEnd = -1
    Request.ContentRangeStart = -1
    Request.ContentRangeInstanceLength = -1
    Request.Accept = 'text/html,application/xhtml+xml,application/xml;q=0.9,*/*;q=0.8'
    Request.BasicAuthentication = False
    Request.UserAgent = 'Mozilla/3.0 (compatible; Indy Library)'
    Request.Ranges.Units = 'bytes'
    Request.Ranges = <>
    HTTPOptions = [hoForceEncodeParams]
    Left = 120
    Top = 152
  end
end
