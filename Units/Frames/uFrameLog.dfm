object FrameLog: TFrameLog
  Left = 0
  Top = 0
  Width = 1136
  Height = 771
  Align = alClient
  DoubleBuffered = True
  Color = clWhite
  ParentBackground = False
  ParentColor = False
  ParentDoubleBuffered = False
  TabOrder = 0
  object VST: TVirtualStringTree
    AlignWithMargins = True
    Left = 1
    Top = 1
    Width = 1134
    Height = 769
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
    Colors.SelectionTextColor = clWhite
    Colors.TreeLineColor = 9471874
    Colors.UnfocusedColor = clGray
    Colors.UnfocusedSelectionColor = clWhite
    Colors.UnfocusedSelectionBorderColor = clWhite
    Header.AutoSizeIndex = -1
    Header.Options = [hoAutoResize, hoColumnResize, hoDrag, hoShowSortGlyphs, hoVisible]
    Images = FormMain.VirtualImageList
    TabOrder = 0
    TreeOptions.AutoOptions = [toAutoDropExpand, toAutoScrollOnExpand, toAutoSort, toAutoSpanColumns, toAutoTristateTracking, toAutoDeleteMovedNodes, toAutoChangeScale]
    TreeOptions.PaintOptions = [toShowButtons, toShowDropmark, toShowTreeLines, toShowVertGridLines, toThemeAware, toUseBlendedImages, toFullVertGridLines, toUseExplorerTheme]
    TreeOptions.SelectionOptions = [toExtendedFocus, toFullRowSelect, toMiddleClickSelect, toRestoreSelection]
    OnBeforeCellPaint = VSTBeforeCellPaint
    OnChange = VSTChange
    OnFocusChanged = VSTFocusChanged
    OnGetText = VSTGetText
    OnGetImageIndex = VSTGetImageIndex
    OnGetNodeDataSize = VSTGetNodeDataSize
    Touch.InteractiveGestures = [igPan, igPressAndTap]
    Touch.InteractiveGestureOptions = [igoPanSingleFingerHorizontal, igoPanSingleFingerVertical, igoPanInertia, igoPanGutter, igoParentPassthrough]
    Columns = <
      item
        Position = 0
        Text = 'Date Time'
        Width = 150
      end
      item
        Position = 1
        Text = 'Owner'
        Width = 100
      end
      item
        Position = 2
        Text = 'Message'
        Width = 884
      end>
  end
  object PopupMenu: TPopupMenu
    Images = FormMain.VirtualImageList
    Left = 304
    Top = 368
    object Clear1: TMenuItem
      Caption = 'Clear'
      ImageIndex = 33
      ImageName = 'cleanup'
      OnClick = Clear1Click
    end
  end
end
