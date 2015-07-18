object Form2: TForm2
  Left = 0
  Top = 0
  Caption = 'Form2'
  ClientHeight = 332
  ClientWidth = 660
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
    Left = 12
    Top = 11
    Width = 78
    Height = 13
    Caption = #26908#32034#25991#23383#21015'(&T):'
    FocusControl = edtSearch
  end
  object Label2: TLabel
    Left = 13
    Top = 39
    Width = 77
    Height = 13
    Caption = #23550#35937#12501#12457#12523#12480'(&F):'
    FocusControl = edtFolder
  end
  object Label3: TLabel
    Left = 13
    Top = 67
    Width = 78
    Height = 13
    Caption = #12501#12449#12452#12523#12510#12473#12463'(&K):'
    FocusControl = edtFolder
  end
  object edtSearch: TEdit
    Left = 104
    Top = 8
    Width = 425
    Height = 21
    TabOrder = 0
    Text = '[\w\d_]+\s*=\s*\d+'
  end
  object Button1: TButton
    Left = 564
    Top = 6
    Width = 75
    Height = 25
    Caption = #26908#32034
    Default = True
    TabOrder = 1
  end
  object Button2: TButton
    Left = 533
    Top = 35
    Width = 25
    Height = 22
    Caption = '...'
    TabOrder = 2
  end
  object edtFolder: TEdit
    Left = 104
    Top = 36
    Width = 423
    Height = 21
    TabOrder = 3
    Text = 'C:\Users\shu\Documents\RAD Studio\Lib\SkRegExpLib'
  end
  object chkIgnoreCase: TCheckBox
    Left = 14
    Top = 104
    Width = 201
    Height = 17
    Caption = #22823#25991#23383#23567#25991#23383#12434#21306#21029#12375#12394#12356'(&C)'
    TabOrder = 4
  end
  object CheckBox4: TCheckBox
    Left = 8
    Top = 240
    Width = 201
    Height = 17
    Caption = #12471#12531#12464#12523#12521#12452#12531#12514#12540#12489'(&S)'
    TabOrder = 5
  end
  object CheckBox5: TCheckBox
    Left = 8
    Top = 263
    Width = 201
    Height = 17
    Caption = #12510#12523#12481#12521#12452#12531#12514#12540#12489'(&M)'
    TabOrder = 6
  end
  object CheckBox2: TCheckBox
    Left = 15
    Top = 173
    Width = 200
    Height = 17
    Caption = #12402#12425#12364#12394'/'#12459#12479#12459#12490#12434#21306#21029#12375#12394#12356'(&K)'
    TabOrder = 7
  end
  object CheckBox3: TCheckBox
    Left = 14
    Top = 150
    Width = 201
    Height = 17
    Caption = #20840#35282#21322#35282#12434#21306#21029#12375#12394#12356'(&W)'
    TabOrder = 8
  end
  object edtMask: TEdit
    Left = 104
    Top = 63
    Width = 49
    Height = 21
    TabOrder = 9
    Text = '*.*'
  end
  object chkASCIIOnly: TCheckBox
    Left = 30
    Top = 127
    Width = 179
    Height = 17
    Caption = '&ASCII'#12398#31684#22258#12398#12415#21306#21029#12375#12394#12356
    TabOrder = 10
  end
end
