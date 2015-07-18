object Form1: TForm1
  Left = 0
  Top = 0
  Caption = 'Form1'
  ClientHeight = 409
  ClientWidth = 639
  Color = clBtnFace
  Font.Charset = DEFAULT_CHARSET
  Font.Color = clWindowText
  Font.Height = -11
  Font.Name = 'Tahoma'
  Font.Style = []
  OldCreateOrder = False
  PixelsPerInch = 96
  TextHeight = 13
  object Label1: TLabel
    Left = 8
    Top = 8
    Width = 45
    Height = 13
    Caption = '&RegExpr:'
    FocusControl = Edit1
  end
  object Label2: TLabel
    Left = 8
    Top = 54
    Width = 26
    Height = 13
    Caption = '&Text:'
    FocusControl = Edit2
  end
  object Button1: TButton
    Left = 280
    Top = 108
    Width = 75
    Height = 25
    Caption = '&Match'
    TabOrder = 0
    OnClick = Button1Click
  end
  object Memo1: TMemo
    Left = 8
    Top = 151
    Width = 623
    Height = 250
    ScrollBars = ssVertical
    TabOrder = 1
  end
  object Edit1: TEdit
    Left = 8
    Top = 27
    Width = 623
    Height = 21
    TabOrder = 2
    Text = '(\d{2,5})\s*[(-]?\s*(\d{1,4})\s*[\s)-]?\s*(\d{4,5})'
  end
  object Edit2: TEdit
    Left = 8
    Top = 73
    Width = 623
    Height = 21
    TabOrder = 3
    Text = #38651#35441#30058#21495#12399' 03 ( 1234 ) 5678 '#12391#12377#12290
  end
end
