object Form1: TForm1
  Left = 0
  Top = 0
  Align = alClient
  Caption = #1050#1072#1083#1100#1082#1091#1083#1103#1090#1086#1088
  ClientHeight = 588
  ClientWidth = 1114
  Color = clWhite
  Font.Charset = DEFAULT_CHARSET
  Font.Color = clWindowText
  Font.Height = -12
  Font.Name = 'Segoe UI'
  Font.Style = []
  Menu = mm1
  TextHeight = 15
  object lbl1: TLabel
    Left = 552
    Top = 144
    Width = 70
    Height = 15
    Caption = #1050#1072#1083#1100#1082#1091#1083#1103#1090#1086#1088
    Color = clWhite
    ParentColor = False
  end
  object edit1: TEdit
    Left = 439
    Top = 224
    Width = 318
    Height = 23
    TabOrder = 0
  end
  object btn1: TBitBtn
    Left = 520
    Top = 400
    Width = 75
    Height = 25
    Caption = '0'
    TabOrder = 1
    OnClick = btn1Click
  end
  object btn2: TBitBtn
    Left = 440
    Top = 369
    Width = 75
    Height = 25
    Caption = '1'
    TabOrder = 2
    OnClick = btn2Click
  end
  object btn3: TBitBtn
    Left = 521
    Top = 369
    Width = 75
    Height = 25
    Caption = '2'
    TabOrder = 3
    OnClick = btn3Click
  end
  object btn4: TBitBtn
    Left = 602
    Top = 369
    Width = 75
    Height = 25
    Caption = '3'
    TabOrder = 4
    OnClick = btn4Click
  end
  object btn5: TBitBtn
    Left = 440
    Top = 338
    Width = 75
    Height = 25
    Caption = '4'
    TabOrder = 5
    OnClick = btn5Click
  end
  object btn6: TBitBtn
    Left = 521
    Top = 338
    Width = 75
    Height = 25
    Caption = '5'
    TabOrder = 6
    OnClick = btn6Click
  end
  object btn7: TBitBtn
    Left = 602
    Top = 338
    Width = 75
    Height = 25
    Caption = '6'
    TabOrder = 7
    OnClick = btn7Click
  end
  object btn8: TBitBtn
    Left = 440
    Top = 307
    Width = 75
    Height = 25
    Caption = '7'
    TabOrder = 8
    OnClick = btn8Click
  end
  object btn9: TBitBtn
    Left = 521
    Top = 307
    Width = 75
    Height = 25
    Caption = '8'
    TabOrder = 9
    OnClick = btn9Click
  end
  object btn10: TBitBtn
    Left = 601
    Top = 307
    Width = 75
    Height = 25
    Caption = '9'
    TabOrder = 10
    OnClick = btn10Click
  end
  object btn15: TBitBtn
    Left = 683
    Top = 338
    Width = 75
    Height = 25
    Caption = '-'
    TabOrder = 11
    OnClick = btn15Click
  end
  object btn16: TBitBtn
    Left = 683
    Top = 369
    Width = 75
    Height = 25
    Caption = '+'
    TabOrder = 12
    OnClick = btn16Click
  end
  object btnEqual: TBitBtn
    Left = 682
    Top = 400
    Width = 75
    Height = 25
    Caption = '='
    TabOrder = 13
    OnClick = btnEqualClick
  end
  object btn18: TBitBtn
    Left = 682
    Top = 307
    Width = 75
    Height = 25
    Caption = '*'
    TabOrder = 14
    OnClick = btn18Click
  end
  object btn19: TBitBtn
    Left = 682
    Top = 276
    Width = 75
    Height = 25
    Caption = '/'
    TabOrder = 15
    OnClick = btn19Click
  end
  object btn20: TBitBtn
    Left = 520
    Top = 276
    Width = 75
    Height = 25
    Caption = 'sqrt'
    TabOrder = 16
    OnClick = btn20Click
  end
  object btn22: TBitBtn
    Left = 601
    Top = 276
    Width = 75
    Height = 25
    Caption = 'C'
    TabOrder = 17
    OnClick = btn22Click
  end
  object btn24: TBitBtn
    Left = 440
    Top = 276
    Width = 75
    Height = 25
    Caption = '.'
    TabOrder = 18
    OnClick = btn24Click
  end
  object StatusBar1: TStatusBar
    Left = 0
    Top = 569
    Width = 1114
    Height = 19
    Panels = <>
  end
  object mm1: TMainMenu
    Left = 712
    Top = 120
    object N1: TMenuItem
      Caption = #1060#1072#1081#1083
      object N4: TMenuItem
        Caption = #1042#1099#1093#1086#1076
        OnClick = N4Click
      end
    end
    object N2: TMenuItem
      Caption = #1055#1088#1072#1074#1082#1072
      object N5: TMenuItem
        Caption = #1050#1086#1087#1080#1088#1086#1074#1072#1090#1100
        OnClick = N5Click
      end
      object N6: TMenuItem
        Caption = #1042#1089#1090#1072#1074#1080#1090#1100
        OnClick = N6Click
      end
    end
    object N3: TMenuItem
      Caption = #1057#1087#1088#1072#1074#1082#1072
    end
  end
end
