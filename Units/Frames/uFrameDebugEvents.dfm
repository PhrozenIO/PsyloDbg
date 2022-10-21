object FrameDebugEvents: TFrameDebugEvents
  Left = 0
  Top = 0
  Width = 961
  Height = 615
  Align = alClient
  DoubleBuffered = True
  Color = clWhite
  ParentBackground = False
  ParentColor = False
  ParentDoubleBuffered = False
  TabOrder = 0
  object MultiPanel: TOMultiPanel
    Left = 0
    Top = 0
    Width = 961
    Height = 615
    PanelType = ptVertical
    PanelCollection = <
      item
        Control = PanelList
        Position = 0.500000000000000000
        Visible = True
        Index = 0
      end
      item
        Control = PanelDetail
        Position = 1.000000000000000000
        Visible = False
        Index = 1
      end>
    MinPosition = 0.020000000000000000
    Align = alClient
    TabOrder = 0
    DesignSize = (
      961
      615)
    object PanelDetail: TPanel
      Left = 0
      Top = 311
      Width = 961
      Height = 304
      Margins.Left = 1
      Anchors = []
      BevelOuter = bvNone
      Color = clWhite
      ParentBackground = False
      ShowCaption = False
      TabOrder = 1
    end
    object PanelList: TPanel
      Left = 0
      Top = 0
      Width = 961
      Height = 308
      Anchors = []
      BevelOuter = bvNone
      Color = 15790320
      ParentBackground = False
      TabOrder = 0
      object VST: TVirtualStringTree
        AlignWithMargins = True
        Left = 1
        Top = 1
        Width = 959
        Height = 306
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
        Header.AutoSizeIndex = 3
        Header.Options = [hoColumnResize, hoDrag, hoShowSortGlyphs, hoVisible]
        Images = FormMain.ImageSystem
        StateImages = FormMain.VirtualImageList
        TabOrder = 0
        TreeOptions.PaintOptions = [toShowButtons, toShowDropmark, toShowTreeLines, toShowVertGridLines, toThemeAware, toUseBlendedImages, toFullVertGridLines, toUseExplorerTheme]
        TreeOptions.SelectionOptions = [toExtendedFocus, toFullRowSelect]
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
            Text = 'Process'
            Width = 150
          end
          item
            Position = 1
            Text = 'Thread Id'
            Width = 120
          end
          item
            Position = 2
            Text = 'Event Kind'
            Width = 140
          end
          item
            Options = [coAllowClick, coDraggable, coEnabled, coParentBidiMode, coParentColor, coResizable, coShowDropMark, coAllowFocus, coEditable, coStyleColor]
            Position = 3
            Text = 'Data'
            Width = 250
          end
          item
            Position = 4
            Text = 'PProcess'
            Width = 110
          end
          item
            Position = 5
            Text = 'PPID'
            Width = 80
          end
          item
            Position = 6
            Text = 'Image Path'
            Width = 200
          end
          item
            Position = 7
            Text = 'Event Date Time'
            Width = 150
          end>
      end
    end
  end
end
