inherited FrameModules: TFrameModules
  Width = 870
  Height = 532
  Align = alClient
  Color = 15790320
  ExplicitWidth = 870
  ExplicitHeight = 532
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
    TabOrder = 0
    ExplicitWidth = 870
    ExplicitHeight = 49
    inherited ComboBox: TComboBoxEx
      Width = 862
      ExplicitWidth = 862
    end
  end
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
    AccessibleName = 'Module Base'
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
    Header.AutoSizeIndex = 3
    Header.Options = [hoAutoResize, hoColumnResize, hoDrag, hoShowSortGlyphs, hoVisible, hoHeaderClickAutoSort]
    Images = FormMain.ImageSystem
    PopupMenu = PopupMenu
    StateImages = FormMain.VirtualImageList
    TabOrder = 1
    TreeOptions.AutoOptions = [toAutoDropExpand, toAutoScrollOnExpand, toAutoSort, toAutoSpanColumns, toAutoTristateTracking, toAutoDeleteMovedNodes, toAutoChangeScale]
    TreeOptions.PaintOptions = [toShowButtons, toShowDropmark, toShowTreeLines, toShowVertGridLines, toThemeAware, toUseBlendedImages, toFullVertGridLines, toUseExplorerTheme]
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
        Text = 'Name'
        Width = 150
      end
      item
        Position = 1
        Text = 'Base Address'
        Width = 170
      end
      item
        Position = 2
        Text = 'Size'
        Width = 120
      end
      item
        Position = 3
        Text = 'Image Path'
        Width = 428
      end>
  end
  object PopupMenu: TPopupMenu
    OnPopup = PopupMenuPopup
    Left = 248
    Top = 208
    object Refresh1: TMenuItem
      Caption = 'Refresh'
      OnClick = Refresh1Click
    end
    object N4: TMenuItem
      Caption = '-'
    end
    object SelectAll1: TMenuItem
      Caption = 'Select All'
      OnClick = SelectAll1Click
    end
    object N3: TMenuItem
      Caption = '-'
    end
    object DumpMemoryImage1: TMenuItem
      Caption = 'Dump and Reconstruct selected Modules'
      OnClick = DumpMemoryImage1Click
    end
    object DumpandReconstructallModulesPEImage1: TMenuItem
      Caption = 'Dump and Reconstruct all Modules'
      OnClick = DumpandReconstructallModulesPEImage1Click
    end
    object N2: TMenuItem
      Caption = '-'
    end
    object ShowinExplorer1: TMenuItem
      Caption = 'Show in Explorer'
      OnClick = ShowinExplorer1Click
    end
    object N1: TMenuItem
      Caption = '-'
    end
    object FileProperties1: TMenuItem
      Caption = 'File Properties'
      OnClick = FileProperties1Click
    end
  end
end
