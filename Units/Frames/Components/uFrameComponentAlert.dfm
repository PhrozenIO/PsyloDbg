object FrameComponentAlert: TFrameComponentAlert
  Left = 0
  Top = 0
  Width = 805
  Height = 128
  Align = alClient
  Color = clWhite
  ParentBackground = False
  ParentColor = False
  TabOrder = 0
  object BorderShape: TShape
    Left = 0
    Top = 0
    Width = 805
    Height = 128
    Align = alClient
    ExplicitHeight = 145
  end
  object LabelMessage: TLabel
    AlignWithMargins = True
    Left = 8
    Top = 8
    Width = 789
    Height = 112
    Margins.Left = 8
    Margins.Top = 8
    Margins.Right = 8
    Margins.Bottom = 8
    Align = alClient
    Alignment = taCenter
    Color = clWhite
    ParentColor = False
    Transparent = False
    Layout = tlCenter
    WordWrap = True
    ExplicitWidth = 7
    ExplicitHeight = 32
  end
end
