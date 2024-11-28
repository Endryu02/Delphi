object Form3: TForm3
  Left = 0
  Top = 0
  Caption = 'Vector Editor'
  ClientHeight = 400
  ClientWidth = 600
  Color = clBtnFace
  Font.Charset = DEFAULT_CHARSET
  Font.Color = clWindowText
  Font.Height = -11
  Font.Name = 'Tahoma'
  Font.Style = []
  OnCreate = FormCreate
  OnDestroy = FormDestroy
  TextHeight = 13
  object PaintBox: TPaintBox
    Left = 0
    Top = 41
    Width = 600
    Height = 359
    Align = alClient
    OnMouseDown = PaintBoxMouseDown
    OnMouseMove = PaintBoxMouseMove
    OnMouseUp = PaintBoxMouseUp
    OnPaint = PaintBoxPaint
    ExplicitLeft = 8
    ExplicitTop = 47
    ExplicitWidth = 584
    ExplicitHeight = 345
  end
  object pnl1: TPanel
    Left = 0
    Top = 0
    Width = 600
    Height = 41
    Align = alTop
    TabOrder = 0
    object btnLine: TButton
      Left = 8
      Top = 8
      Width = 75
      Height = 25
      Caption = 'Line'
      TabOrder = 0
      OnClick = btnLineClick
    end
    object btnRectangle: TButton
      Tag = 1
      Left = 89
      Top = 8
      Width = 75
      Height = 25
      Caption = 'Rectangle'
      TabOrder = 1
      OnClick = btnRectangleClick
    end
    object btnEllipse: TButton
      Tag = 2
      Left = 170
      Top = 8
      Width = 75
      Height = 25
      Caption = 'Ellipse'
      TabOrder = 2
      OnClick = btnEllipseClick
    end
    object cbColor: TColorBox
      Left = 251
      Top = 10
      Width = 145
      Height = 22
      TabOrder = 3
      OnChange = cbColorChange
    end
    object cbLineWidth: TComboBox
      Left = 402
      Top = 10
      Width = 145
      Height = 21
      ItemIndex = 0
      TabOrder = 4
      Text = '1'
      OnChange = cbLineWidthChange
      Items.Strings = (
        '1'
        '2'
        '3'
        '4'
        '5')
    end
  end
end
