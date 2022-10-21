object FrameEventDetail: TFrameEventDetail
  Left = 0
  Top = 0
  Width = 1137
  Height = 737
  Align = alClient
  DoubleBuffered = True
  Color = 15790320
  ParentBackground = False
  ParentColor = False
  ParentDoubleBuffered = False
  TabOrder = 0
  object VST: TVirtualStringTree
    AlignWithMargins = True
    Left = 1
    Top = 1
    Width = 1135
    Height = 714
    Margins.Left = 1
    Margins.Top = 1
    Margins.Right = 1
    Margins.Bottom = 1
    Align = alClient
    BorderStyle = bsNone
    Colors.BorderColor = 15987699
    Colors.DisabledColor = clGray
    Colors.DropMarkColor = 15385233
    Colors.DropTargetColor = 15385233
    Colors.DropTargetBorderColor = 15385233
    Colors.FocusedSelectionColor = 15385233
    Colors.FocusedSelectionBorderColor = 15385233
    Colors.GridLineColor = 15987699
    Colors.HeaderHotColor = clBlack
    Colors.HotColor = clBlack
    Colors.SelectionRectangleBlendColor = 15385233
    Colors.SelectionRectangleBorderColor = 15385233
    Colors.SelectionTextColor = clBlack
    Colors.TreeLineColor = 9471874
    Colors.UnfocusedColor = clGray
    Colors.UnfocusedSelectionColor = clWhite
    Colors.UnfocusedSelectionBorderColor = clWhite
    Header.AutoSizeIndex = 2
    Header.Options = [hoAutoResize, hoColumnResize, hoDrag, hoShowSortGlyphs, hoVisible]
    Images = FormMain.ImageSystem
    StateImages = FormMain.VirtualImageList
    TabOrder = 0
    TreeOptions.AutoOptions = [toAutoDropExpand, toAutoScrollOnExpand, toAutoSort, toAutoSpanColumns, toAutoTristateTracking, toAutoDeleteMovedNodes, toAutoChangeScale]
    TreeOptions.PaintOptions = [toShowButtons, toShowDropmark, toShowRoot, toShowTreeLines, toShowVertGridLines, toThemeAware, toUseBlendedImages, toFullVertGridLines, toUseExplorerTheme]
    TreeOptions.SelectionOptions = [toExtendedFocus, toFullRowSelect, toMiddleClickSelect, toRestoreSelection]
    OnBeforeCellPaint = VSTBeforeCellPaint
    OnChange = VSTChange
    OnFocusChanged = VSTFocusChanged
    OnGetText = VSTGetText
    OnPaintText = VSTPaintText
    OnGetImageIndex = VSTGetImageIndex
    OnGetNodeDataSize = VSTGetNodeDataSize
    Touch.InteractiveGestures = [igPan, igPressAndTap]
    Touch.InteractiveGestureOptions = [igoPanSingleFingerHorizontal, igoPanSingleFingerVertical, igoPanInertia, igoPanGutter, igoParentPassthrough]
    Columns = <
      item
        Position = 0
        Text = 'Key'
        Width = 260
      end
      item
        Position = 1
        Text = 'Value'
        Width = 220
      end
      item
        Position = 2
        Text = 'Hex'
        Width = 655
      end>
  end
  object PanelBottom: TPanel
    AlignWithMargins = True
    Left = 1
    Top = 717
    Width = 1135
    Height = 19
    Margins.Left = 1
    Margins.Top = 1
    Margins.Right = 1
    Margins.Bottom = 1
    Align = alBottom
    BevelOuter = bvNone
    Color = 15790320
    ParentBackground = False
    TabOrder = 1
    object ButtonCopy: TSpeedButton
      Left = 1095
      Top = 0
      Width = 40
      Height = 19
      Margins.Left = 1
      Margins.Top = 1
      Margins.Right = 1
      Margins.Bottom = 1
      Align = alRight
      ImageIndex = 12
      ImageName = 'clipboard'
      Images = FormMain.VirtualImageList
      Enabled = False
      OnClick = ButtonCopyClick
      ExplicitLeft = 1087
      ExplicitTop = 1
      ExplicitHeight = 17
    end
    object EditClipboard: TEdit
      AlignWithMargins = True
      Left = 1
      Top = 1
      Width = 1093
      Height = 17
      Margins.Left = 1
      Margins.Top = 1
      Margins.Right = 1
      Margins.Bottom = 1
      Align = alClient
      BorderStyle = bsNone
      ReadOnly = True
      TabOrder = 0
      OnChange = EditClipboardChange
    end
  end
end
