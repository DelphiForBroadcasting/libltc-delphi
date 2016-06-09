object Form1: TForm1
  Left = 0
  Top = 0
  Caption = 'Form1'
  ClientHeight = 276
  ClientWidth = 516
  Color = clBtnFace
  DoubleBuffered = True
  Font.Charset = DEFAULT_CHARSET
  Font.Color = clWindowText
  Font.Height = -10
  Font.Name = 'Tahoma'
  Font.Style = []
  Menu = MainMenu1
  OldCreateOrder = False
  Position = poScreenCenter
  OnClose = FormClose
  OnCreate = FormCreate
  OnDestroy = FormDestroy
  PixelsPerInch = 96
  TextHeight = 12
  object Memo1: TMemo
    Left = 198
    Top = 6
    Width = 304
    Height = 110
    Margins.Left = 2
    Margins.Top = 2
    Margins.Right = 2
    Margins.Bottom = 2
    Ctl3D = True
    ParentCtl3D = False
    ScrollBars = ssVertical
    TabOrder = 0
  end
  object GroupBox1: TGroupBox
    Left = 6
    Top = 6
    Width = 180
    Height = 110
    Margins.Left = 2
    Margins.Top = 2
    Margins.Right = 2
    Margins.Bottom = 2
    Caption = 'Timecode Source'
    TabOrder = 1
    object ComboBox1: TComboBox
      Left = 13
      Top = 20
      Width = 148
      Height = 20
      Margins.Left = 2
      Margins.Top = 2
      Margins.Right = 2
      Margins.Bottom = 2
      Style = csDropDownList
      TabOrder = 0
      OnChange = ComboBox1Change
    end
    object ComboBox2: TComboBox
      Left = 13
      Top = 44
      Width = 148
      Height = 20
      Margins.Left = 2
      Margins.Top = 2
      Margins.Right = 2
      Margins.Bottom = 2
      Style = csDropDownList
      TabOrder = 1
      OnChange = ComboBox2Change
    end
    object Button1: TButton
      Left = 13
      Top = 79
      Width = 60
      Height = 20
      Margins.Left = 2
      Margins.Top = 2
      Margins.Right = 2
      Margins.Bottom = 2
      Caption = 'start'
      TabOrder = 2
      OnClick = Button1Click
    end
    object Button2: TButton
      Left = 78
      Top = 79
      Width = 60
      Height = 20
      Margins.Left = 2
      Margins.Top = 2
      Margins.Right = 2
      Margins.Bottom = 2
      Caption = 'stop'
      TabOrder = 3
      OnClick = Button2Click
    end
  end
  object GroupBox2: TGroupBox
    Left = 6
    Top = 134
    Width = 240
    Height = 110
    Margins.Left = 2
    Margins.Top = 2
    Margins.Right = 2
    Margins.Bottom = 2
    Caption = 'Decode signal'
    TabOrder = 2
    object Label1: TLabel
      Left = 14
      Top = 61
      Width = 220
      Height = 36
      Margins.Left = 2
      Margins.Top = 2
      Margins.Right = 2
      Margins.Bottom = 2
      AutoSize = False
      Caption = '00:00:00.00'
      Font.Charset = RUSSIAN_CHARSET
      Font.Color = clWindowText
      Font.Height = -34
      Font.Name = 'Consolas'
      Font.Style = []
      ParentFont = False
    end
  end
  object StatusBar1: TStatusBar
    Left = 0
    Top = 257
    Width = 516
    Height = 19
    Margins.Left = 2
    Margins.Top = 2
    Margins.Right = 2
    Margins.Bottom = 2
    Panels = <>
  end
  object GroupBox3: TGroupBox
    Left = 262
    Top = 134
    Width = 238
    Height = 110
    Margins.Left = 2
    Margins.Top = 2
    Margins.Right = 2
    Margins.Bottom = 2
    Caption = 'FreeRun timer'
    TabOrder = 4
    object Label2: TLabel
      Left = 20
      Top = 61
      Width = 221
      Height = 36
      Margins.Left = 2
      Margins.Top = 2
      Margins.Right = 2
      Margins.Bottom = 2
      AutoSize = False
      Caption = '00:00:00.00'
      Font.Charset = RUSSIAN_CHARSET
      Font.Color = clWindowText
      Font.Height = -34
      Font.Name = 'Consolas'
      Font.Style = []
      ParentFont = False
    end
    object Button3: TButton
      Left = 19
      Top = 26
      Width = 116
      Height = 20
      Margins.Left = 2
      Margins.Top = 2
      Margins.Right = 2
      Margins.Bottom = 2
      Caption = 'compare LTC & FreeRun'
      TabOrder = 0
      OnClick = Button3Click
    end
  end
  object OpenDialog1: TOpenDialog
    Left = 192
    Top = 160
  end
  object MainMenu1: TMainMenu
    Left = 152
    Top = 152
    object exit1: TMenuItem
      Caption = 'File'
      object exit2: TMenuItem
        Caption = 'Exit'
        OnClick = exit2Click
      end
    end
    object N1: TMenuItem
      Caption = '?'
    end
  end
end
