object FormDumpMemoryImage: TFormDumpMemoryImage
  Left = 0
  Top = 0
  BorderIcons = [biSystemMenu]
  BorderStyle = bsSingle
  Caption = 'Dump and Reconstruct Memory Module'
  ClientHeight = 258
  ClientWidth = 369
  Color = clWhite
  DoubleBuffered = True
  Font.Charset = DEFAULT_CHARSET
  Font.Color = clWindowText
  Font.Height = -12
  Font.Name = 'Segoe UI'
  Font.Style = []
  KeyPreview = True
  Position = poOwnerFormCenter
  OnHide = FormHide
  OnKeyUp = FormKeyUp
  OnShow = FormShow
  TextHeight = 15
  object PanelBackground: TPanel
    Left = 0
    Top = 0
    Width = 369
    Height = 217
    Align = alClient
    BevelOuter = bvNone
    Color = clWhite
    ParentBackground = False
    TabOrder = 0
    ExplicitWidth = 332
    ExplicitHeight = 133
    object PanelCore: TPanel
      AlignWithMargins = True
      Left = 8
      Top = 8
      Width = 353
      Height = 201
      Margins.Left = 8
      Margins.Top = 8
      Margins.Right = 8
      Margins.Bottom = 8
      Align = alClient
      BevelOuter = bvNone
      Color = clWhite
      ParentBackground = False
      TabOrder = 0
      ExplicitWidth = 316
      ExplicitHeight = 117
      object Shape1: TShape
        Left = 48
        Top = 0
        Width = 2
        Height = 201
        Align = alLeft
        Pen.Color = 15790320
        ExplicitLeft = 32
        ExplicitTop = 112
        ExplicitHeight = 65
      end
      object PanelForm: TPanel
        AlignWithMargins = True
        Left = 58
        Top = 0
        Width = 295
        Height = 201
        Margins.Left = 8
        Margins.Top = 0
        Margins.Right = 0
        Margins.Bottom = 0
        Align = alClient
        BevelOuter = bvNone
        Color = clWhite
        TabOrder = 0
        ExplicitWidth = 258
        ExplicitHeight = 117
        object Label1: TLabel
          AlignWithMargins = True
          Left = 0
          Top = 64
          Width = 295
          Height = 15
          Margins.Left = 0
          Margins.Top = 8
          Margins.Right = 0
          Align = alTop
          Caption = 'Destination Path'
          Font.Charset = DEFAULT_CHARSET
          Font.Color = clWindowText
          Font.Height = -12
          Font.Name = 'Segoe UI'
          Font.Style = [fsBold]
          ParentFont = False
          ExplicitTop = 8
          ExplicitWidth = 92
        end
        object LabelTargetProcess: TLabel
          AlignWithMargins = True
          Left = 0
          Top = 8
          Width = 295
          Height = 15
          Margins.Left = 0
          Margins.Top = 8
          Margins.Right = 0
          Align = alTop
          Caption = 'Target Debugged Process'
          Font.Charset = DEFAULT_CHARSET
          Font.Color = clWindowText
          Font.Height = -12
          Font.Name = 'Segoe UI'
          Font.Style = [fsBold]
          ParentFont = False
          ExplicitWidth = 142
        end
        object EditPath: TButtonedEdit
          Left = 0
          Top = 82
          Width = 295
          Height = 23
          Align = alTop
          Images = FormMain.VirtualImageList
          LeftButton.ImageIndex = 16
          LeftButton.ImageName = 'folder-filled-new'
          RightButton.ImageIndex = 16
          RightButton.ImageName = 'folder-filled-new'
          RightButton.Visible = True
          TabOrder = 0
          OnChange = EditPathChange
          OnRightButtonClick = EditPathRightButtonClick
          ExplicitTop = 26
          ExplicitWidth = 258
        end
        object ModuleDumpOptions: TRadioGroup
          AlignWithMargins = True
          Left = 0
          Top = 113
          Width = 295
          Height = 88
          Margins.Left = 0
          Margins.Top = 8
          Margins.Right = 0
          Margins.Bottom = 0
          Align = alClient
          Caption = 'Module Dump Options'
          Font.Charset = DEFAULT_CHARSET
          Font.Color = clWindowText
          Font.Height = -12
          Font.Name = 'Segoe UI'
          Font.Style = []
          ItemIndex = 2
          Items.Strings = (
            'Dump main module only.'
            'Dump all modules except main module.'
            'Dump all modules.')
          ParentFont = False
          TabOrder = 1
          ExplicitTop = 57
          ExplicitWidth = 258
          ExplicitHeight = 60
        end
        object PanelComboProcess: TPanel
          Left = 0
          Top = 26
          Width = 295
          Height = 30
          Align = alTop
          BevelOuter = bvNone
          Color = clWhite
          ParentBackground = False
          TabOrder = 2
        end
      end
      object PanelIcon: TPanel
        Left = 0
        Top = 0
        Width = 48
        Height = 201
        Align = alLeft
        BevelOuter = bvNone
        ParentColor = True
        TabOrder = 1
        ExplicitLeft = -1
        ExplicitTop = 2
        ExplicitHeight = 184
        object ImageIcon: TVirtualImage
          AlignWithMargins = True
          Left = 4
          Top = 4
          Width = 40
          Height = 64
          Margins.Left = 4
          Margins.Top = 4
          Margins.Right = 4
          Margins.Bottom = 4
          Align = alTop
          Center = True
          ImageCollection = FormMain.ImageCollection
          ImageWidth = 0
          ImageHeight = 0
          ImageIndex = 43
          ImageName = 'application-windows-filled-arrow-down-filled'
          ExplicitLeft = 26
          ExplicitTop = 8
        end
      end
    end
  end
  object PanelBottom: TPanel
    Left = 0
    Top = 217
    Width = 369
    Height = 41
    Align = alBottom
    BevelOuter = bvNone
    Color = 15790320
    ParentBackground = False
    TabOrder = 1
    ExplicitTop = 133
    ExplicitWidth = 332
    object ButtonAction: TSpeedButton
      AlignWithMargins = True
      Left = 275
      Top = 8
      Width = 90
      Height = 25
      Margins.Left = 2
      Margins.Top = 8
      Margins.Right = 4
      Margins.Bottom = 8
      Align = alRight
      Caption = 'Dump'
      ImageIndex = 42
      ImageName = 'application-windows-filled-arrow-down-filled'
      Images = FormMain.VirtualImageList
      Enabled = False
      OnClick = ButtonActionClick
      ExplicitLeft = 321
    end
    object ButtonCancel: TSpeedButton
      AlignWithMargins = True
      Left = 181
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
  object Timer: TTimer
    Enabled = False
    OnTimer = TimerTimer
    Left = 65511
    Top = 160
  end
end
