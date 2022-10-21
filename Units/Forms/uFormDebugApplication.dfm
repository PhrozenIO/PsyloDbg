object FormDebugApplication: TFormDebugApplication
  Left = 0
  Top = 0
  BorderIcons = [biSystemMenu]
  BorderStyle = bsSingle
  Caption = 'Debug Application'
  ClientHeight = 265
  ClientWidth = 435
  Color = clWhite
  DoubleBuffered = True
  Font.Charset = DEFAULT_CHARSET
  Font.Color = clWindowText
  Font.Height = -12
  Font.Name = 'Segoe UI'
  Font.Style = []
  KeyPreview = True
  Position = poOwnerFormCenter
  OnCreate = FormCreate
  OnKeyUp = FormKeyUp
  TextHeight = 15
  object PanelBottom: TPanel
    Left = 0
    Top = 224
    Width = 435
    Height = 41
    Align = alBottom
    BevelOuter = bvNone
    Color = 15790320
    ParentBackground = False
    TabOrder = 0
    ExplicitTop = 191
    ExplicitWidth = 421
    object ButtonStart: TSpeedButton
      AlignWithMargins = True
      Left = 341
      Top = 8
      Width = 90
      Height = 25
      Margins.Left = 2
      Margins.Top = 8
      Margins.Right = 4
      Margins.Bottom = 8
      Align = alRight
      Caption = 'Start'
      ImageIndex = 15
      ImageName = 'bug-filled-new'
      Images = FormMain.VirtualImageList
      OnClick = ButtonStartClick
      ExplicitLeft = 321
    end
    object ButtonCancel: TSpeedButton
      AlignWithMargins = True
      Left = 247
      Top = 8
      Width = 90
      Height = 25
      Margins.Top = 8
      Margins.Right = 2
      Margins.Bottom = 8
      Align = alRight
      Caption = 'Cancel'
      OnClick = ButtonCancelClick
      ExplicitLeft = 207
    end
  end
  object PanelBackground: TPanel
    Left = 0
    Top = 0
    Width = 435
    Height = 224
    Align = alClient
    BevelOuter = bvNone
    Color = clWhite
    ParentBackground = False
    TabOrder = 1
    ExplicitLeft = 32
    ExplicitTop = 40
    ExplicitWidth = 185
    ExplicitHeight = 41
    object PanelCore: TPanel
      AlignWithMargins = True
      Left = 8
      Top = 8
      Width = 419
      Height = 208
      Margins.Left = 8
      Margins.Top = 8
      Margins.Right = 8
      Margins.Bottom = 8
      Align = alClient
      BevelOuter = bvNone
      Color = clWhite
      ParentBackground = False
      TabOrder = 0
      ExplicitWidth = 405
      ExplicitHeight = 175
      object Shape1: TShape
        Left = 78
        Top = 0
        Width = 2
        Height = 208
        Align = alRight
        Pen.Color = 15790320
        ExplicitLeft = 32
        ExplicitTop = 112
        ExplicitHeight = 65
      end
      object PanelForm: TPanel
        AlignWithMargins = True
        Left = 88
        Top = 0
        Width = 331
        Height = 208
        Margins.Left = 8
        Margins.Top = 0
        Margins.Right = 0
        Margins.Bottom = 0
        Align = alRight
        BevelOuter = bvNone
        Color = clWhite
        TabOrder = 0
        object Label1: TLabel
          AlignWithMargins = True
          Left = 0
          Top = 8
          Width = 331
          Height = 15
          Margins.Left = 0
          Margins.Top = 8
          Margins.Right = 0
          Align = alTop
          Caption = 'Application Path:'
          Font.Charset = DEFAULT_CHARSET
          Font.Color = clWindowText
          Font.Height = -12
          Font.Name = 'Segoe UI'
          Font.Style = [fsBold]
          ParentFont = False
          ExplicitWidth = 93
        end
        object Label2: TLabel
          AlignWithMargins = True
          Left = 0
          Top = 57
          Width = 331
          Height = 15
          Margins.Left = 0
          Margins.Top = 8
          Margins.Right = 0
          Align = alTop
          Caption = 'Optional Arguments:'
          Font.Charset = DEFAULT_CHARSET
          Font.Color = clWindowText
          Font.Height = -12
          Font.Name = 'Segoe UI'
          Font.Style = [fsBold]
          ParentFont = False
          ExplicitWidth = 115
        end
        object EditApplication: TButtonedEdit
          Left = 0
          Top = 26
          Width = 331
          Height = 23
          Align = alTop
          Images = FormMain.VirtualImageList
          LeftButton.ImageIndex = 16
          LeftButton.ImageName = 'folder-filled-new'
          RightButton.ImageIndex = 16
          RightButton.ImageName = 'folder-filled-new'
          RightButton.Visible = True
          TabOrder = 0
          OnRightButtonClick = EditApplicationRightButtonClick
        end
        object CheckBoxShowProcess: TCheckBox
          AlignWithMargins = True
          Left = 0
          Top = 108
          Width = 331
          Height = 17
          Margins.Left = 0
          Margins.Top = 10
          Margins.Right = 0
          Margins.Bottom = 0
          Align = alTop
          Caption = 'Show Process.'
          Checked = True
          State = cbChecked
          TabOrder = 1
        end
        object EditArguments: TEdit
          Left = 0
          Top = 75
          Width = 331
          Height = 23
          Align = alTop
          TabOrder = 2
        end
        object CheckBoxDebugChild: TCheckBox
          AlignWithMargins = True
          Left = 0
          Top = 135
          Width = 331
          Height = 17
          Margins.Left = 0
          Margins.Top = 10
          Margins.Right = 0
          Margins.Bottom = 0
          Align = alTop
          Caption = 'Debug Child Process.'
          Checked = True
          State = cbChecked
          TabOrder = 3
        end
        object PanelWarning: TPanel
          Left = 0
          Top = 156
          Width = 331
          Height = 52
          Align = alBottom
          AutoSize = True
          BevelOuter = bvNone
          ParentColor = True
          TabOrder = 4
          object Label4: TLabel
            AlignWithMargins = True
            Left = 20
            Top = 7
            Width = 311
            Height = 45
            Margins.Left = 20
            Margins.Top = 0
            Margins.Right = 0
            Margins.Bottom = 0
            Align = alBottom
            Caption = 
              'This action will execute target program. If you are planning to ' +
              'debug an unknown program, be sure to be in a safe, controlled an' +
              'd isolated environement.'
            Font.Charset = DEFAULT_CHARSET
            Font.Color = 6908265
            Font.Height = -12
            Font.Name = 'Segoe UI'
            Font.Style = []
            ParentFont = False
            WordWrap = True
            ExplicitWidth = 310
          end
          object Shape2: TShape
            AlignWithMargins = True
            Left = 3
            Top = 3
            Width = 325
            Height = 1
            Align = alBottom
            Pen.Color = 15658734
          end
          object IconWarning: TVirtualImage
            Left = 0
            Top = 7
            Width = 16
            Height = 16
            ImageCollection = FormMain.ImageCollection
            ImageWidth = 0
            ImageHeight = 0
            ImageIndex = 21
            ImageName = 'warning'
          end
        end
      end
      object PanelIcon: TPanel
        Left = 0
        Top = 0
        Width = 78
        Height = 208
        Align = alClient
        BevelOuter = bvNone
        ParentColor = True
        TabOrder = 1
        object ImageIcon: TVirtualImage
          Left = 0
          Top = 0
          Width = 78
          Height = 64
          Margins.Left = 4
          Margins.Top = 8
          Margins.Right = 4
          Margins.Bottom = 8
          Align = alTop
          Center = True
          ImageCollection = FormMain.ImageCollection
          ImageWidth = 0
          ImageHeight = 0
          ImageIndex = 13
          ImageName = 'bug-filled-gear-filled'
          ExplicitWidth = 83
        end
      end
    end
  end
  object OpenDialog: TOpenDialog
    Left = 256
    Top = 8
  end
end
