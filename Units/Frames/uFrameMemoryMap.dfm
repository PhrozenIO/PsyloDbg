inherited FrameMemoryMap: TFrameMemoryMap
  Width = 870
  Height = 532
  Align = alClient
  Color = 15790320
  ExplicitWidth = 870
  ExplicitHeight = 532
  object VST: TVirtualStringTree
    AlignWithMargins = True
    Left = 1
    Top = 50
    Width = 868
    Height = 481
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
    Header.AutoSizeIndex = 4
    Header.Options = [hoAutoResize, hoColumnResize, hoDrag, hoShowSortGlyphs, hoVisible, hoHeaderClickAutoSort]
    Header.SortColumn = 0
    Images = FormMain.ImageSystem
    PopupMenu = PopupMenu
    StateImages = FormMain.VirtualImageList
    TabOrder = 0
    TreeOptions.AutoOptions = [toAutoDropExpand, toAutoScrollOnExpand, toAutoSort, toAutoSpanColumns, toAutoTristateTracking, toAutoDeleteMovedNodes, toAutoChangeScale]
    TreeOptions.PaintOptions = [toShowButtons, toShowDropmark, toShowRoot, toShowTreeLines, toShowVertGridLines, toThemeAware, toUseBlendedImages, toFullVertGridLines, toUseExplorerTheme]
    TreeOptions.SelectionOptions = [toExtendedFocus, toFullRowSelect, toMiddleClickSelect, toMultiSelect, toRightClickSelect, toRestoreSelection]
    OnBeforeCellPaint = VSTBeforeCellPaint
    OnChange = VSTChange
    OnCompareNodes = VSTCompareNodes
    OnFocusChanged = VSTFocusChanged
    OnFreeNode = VSTFreeNode
    OnGetText = VSTGetText
    OnGetImageIndex = VSTGetImageIndex
    OnGetNodeDataSize = VSTGetNodeDataSize
    Touch.InteractiveGestures = [igPan, igPressAndTap]
    Touch.InteractiveGestureOptions = [igoPanSingleFingerHorizontal, igoPanSingleFingerVertical, igoPanInertia, igoPanGutter, igoParentPassthrough]
    Columns = <
      item
        Position = 0
        Text = 'Address'
        Width = 230
      end
      item
        Position = 1
        Text = 'Type'
        Width = 80
      end
      item
        Position = 2
        Text = 'State'
        Width = 80
      end
      item
        Position = 3
        Text = 'Protect'
        Width = 200
      end
      item
        Position = 4
        Text = 'Page Size'
        Width = 278
      end>
  end
  inline FrameComponentComboProcess1: TFrameComponentComboProcess
    Left = 0
    Top = 0
    Width = 870
    Height = 49
    Align = alTop
    AutoSize = True
    DoubleBuffered = True
    Color = clWhite
    ParentBackground = False
    ParentColor = False
    ParentDoubleBuffered = False
    TabOrder = 1
    ExplicitWidth = 870
    ExplicitHeight = 49
    inherited ComboBox: TComboBoxEx
      Width = 862
      ExplicitWidth = 862
    end
  end
  object PopupMenu: TPopupMenu
    OnPopup = PopupMenuPopup
    Left = 296
    Top = 176
    object Refresh1: TMenuItem
      Caption = 'Refresh'
      ImageIndex = 22
      ImageName = 'symbol-refresh'
      OnClick = Refresh1Click
    end
    object N1: TMenuItem
      Caption = '-'
    end
    object SelectAll1: TMenuItem
      Caption = 'Select All'
      OnClick = SelectAll1Click
    end
    object N3: TMenuItem
      Caption = '-'
    end
    object FullExpand1: TMenuItem
      Caption = 'Full Expand'
      OnClick = FullExpand1Click
    end
    object FullCollapse1: TMenuItem
      Caption = 'Full Collapse'
      OnClick = FullCollapse1Click
    end
    object N2: TMenuItem
      Caption = '-'
    end
    object DumpSelectedMemory1: TMenuItem
      Caption = 'Dump Selected Memory'
      ImageIndex = 29
      ImageName = 'memory-filled-arrow-down'
      OnClick = DumpSelectedMemory1Click
    end
  end
end
