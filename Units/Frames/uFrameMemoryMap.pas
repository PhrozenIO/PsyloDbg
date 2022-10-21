{******************************************************************************}
{                                                                              }
{            __________.__                                                     }
{            \______   \  |_________  ____________ ____   ____                 }
{             |     ___/  |  \_  __ \/  _ \___   // __ \ /    \                }
{             |    |   |   Y  \  | \(  <_> )    /\  ___/|   |  \               }
{             |____|   |___|  /__|   \____/_____ \\___  >___|  /               }
{             \/                  \/    \/     \/                              }
{                                                                              }
{                                                                              }
{                   Author: DarkCoderSc (Jean-Pierre LESUEUR)                  }
{                   https://www.twitter.com/                                   }
{                   https://www.phrozen.io/                                    }
{                   https://github.com/darkcodersc                             }
{                   License: Apache License 2.0                                }
{                                                                              }
{                                                                              }
{******************************************************************************}

unit uFrameMemoryMap;

interface

uses
  Winapi.Windows, Winapi.Messages, System.SysUtils, System.Variants, System.Classes,
  Vcl.Graphics, Vcl.Controls, Vcl.Forms, Vcl.Dialogs, VirtualTrees, uMemoryMap,
  Vcl.Menus;

type
  TTreeData = record
    RegionName   : String;
    BaseAddress  : Pointer;
    ImageIndex   : Integer;
    FormatedSize : String;
    Size         : UInt64;
    Page         : TMemoryPage;
  end;
  PTreeData = ^TTreeData;

  TFrameMemoryMap = class(TFrame)
    VST: TVirtualStringTree;
    PopupMenu: TPopupMenu;
    Refresh1: TMenuItem;
    N1: TMenuItem;
    FullExpand1: TMenuItem;
    FullCollapse1: TMenuItem;
    N2: TMenuItem;
    DumpSelectedMemory1: TMenuItem;
    N3: TMenuItem;
    SelectAll1: TMenuItem;
    procedure VSTGetNodeDataSize(Sender: TBaseVirtualTree;
      var NodeDataSize: Integer);
    procedure VSTGetText(Sender: TBaseVirtualTree; Node: PVirtualNode;
      Column: TColumnIndex; TextType: TVSTTextType; var CellText: string);
    procedure VSTGetImageIndex(Sender: TBaseVirtualTree; Node: PVirtualNode;
      Kind: TVTImageKind; Column: TColumnIndex; var Ghosted: Boolean;
      var ImageIndex: TImageIndex);
    procedure VSTFreeNode(Sender: TBaseVirtualTree; Node: PVirtualNode);
    procedure VSTBeforeCellPaint(Sender: TBaseVirtualTree;
      TargetCanvas: TCanvas; Node: PVirtualNode; Column: TColumnIndex;
      CellPaintMode: TVTCellPaintMode; CellRect: TRect; var ContentRect: TRect);
    procedure Refresh1Click(Sender: TObject);
    procedure FullExpand1Click(Sender: TObject);
    procedure FullCollapse1Click(Sender: TObject);
    procedure VSTCompareNodes(Sender: TBaseVirtualTree; Node1,
      Node2: PVirtualNode; Column: TColumnIndex; var Result: Integer);
    procedure VSTChange(Sender: TBaseVirtualTree; Node: PVirtualNode);
    procedure VSTFocusChanged(Sender: TBaseVirtualTree; Node: PVirtualNode;
      Column: TColumnIndex);
    procedure PopupMenuPopup(Sender: TObject);
    procedure DumpSelectedMemory1Click(Sender: TObject);
    procedure SelectAll1Click(Sender: TObject);
  private
    FMemorySize : Int64;
  public
    {@M}
    procedure Refresh();
    procedure Reset();

    {@S}
    property MemorySize : Int64 write FMemorySize;
  end;

implementation

uses uFormMain, uGraphicUtils, uConstants, uMemoryUtils, FileCtrl,
     System.Math, uMemoryMapThread, uFormDumpMemoryOperation, uMemoryDumpThread;

{$R *.dfm}

procedure TFrameMemoryMap.Reset();
begin
  VST.Clear();
end;

procedure TFrameMemoryMap.SelectAll1Click(Sender: TObject);
begin
  VST.SelectAll(True);
end;

procedure TFrameMemoryMap.DumpSelectedMemory1Click(Sender: TObject);
var AFormOperation  : TFormDumpMemoryOperation;
    ADumpThread     : TMemoryDumpThread;

    pNode           : PVirtualNode;
    pData           : PTreeData;
    ABaseOffset     : NativeUInt;
    APrefix         : String;
    AFileName       : String;
    ADirectory      : String;
begin
  if not FormMain.Debugging then
    Exit();
  ///

  if VST.SelectedCount <= 0 then
    Exit();

  if not SelectDirectory('Select output directory', '', ADirectory, [sdNewFolder, sdShowShares]) then
    Exit();

  AFormOperation := TFormDumpMemoryOperation.Create(self);
  try
    ADumpThread := TMemoryDumpThread.Create(FormMain.DebugSessionInformation.ProcessId);

    ADumpThread.OnThreadStart  := AFormOperation.OnThreadStart;
    ADumpThread.OnThreadStop   := AFormOperation.OnThreadStop;
    ADumpThread.OnTaskBegin    := AFormOperation.OnTaskBegin;
    ADumpThread.OnTaskEnd      := AFormOperation.OnTaskEnd;
    ADumpThread.OnTaskProgress := AFormOperation.OnTaskProgress;

    for pNode in VST.SelectedNodes do begin
      pData := pNode.GetData;
      if not Assigned(pData) then
        continue;
      ///

      if Assigned(pData^.Page) then begin
        ABaseOffset := NativeUInt(pData^.Page.PageAddress);
        APrefix := 'page';
      end else begin
        ABaseOffset := NativeUInt(pData^.BaseAddress);
        APrefix := 'region';
      end;

      AFileName := Format('%s\%s_%p_to_%p.bin', [
        ADirectory,
        APrefix,
        Pointer(ABaseOffset),
        Pointer(ABaseOffset + pData^.Size)
      ]);

      ///
      ADumpThread.Tasks.Add(TMemoryDumpTask.Create(AFileName, ABaseOffset, pData^.Size));
    end;

    ADumpThread.Start();

    AFormOperation.ShowModal();

    if Assigned(ADumpThread) then
      FreeAndNil(ADumpThread);
  finally
    if Assigned(AFormOperation) then
      FreeAndNil(AFormOperation);
  end;
end;

procedure TFrameMemoryMap.FullCollapse1Click(Sender: TObject);
begin
  VST.FullCollapse();
end;

procedure TFrameMemoryMap.FullExpand1Click(Sender: TObject);
begin
  VST.FullExpand();
end;

procedure TFrameMemoryMap.PopupMenuPopup(Sender: TObject);
begin
  self.DumpSelectedMemory1.Enabled := VST.FocusedNode <> nil;
end;

procedure TFrameMemoryMap.Refresh();
begin
  VST.Clear();
  FMemorySize := 0;
  ///

  if not FormMain.Debugging then
    Exit();
  ///

  TMemoryMapThread.Create(FormMain.DebugSessionInformation.ProcessId, self);
end;

procedure TFrameMemoryMap.Refresh1Click(Sender: TObject);
begin
  self.Refresh();
end;

procedure TFrameMemoryMap.VSTBeforeCellPaint(Sender: TBaseVirtualTree;
  TargetCanvas: TCanvas; Node: PVirtualNode; Column: TColumnIndex;
  CellPaintMode: TVTCellPaintMode; CellRect: TRect; var ContentRect: TRect);
var pData         : PTreeData;
    pParentData   : PTreeData;
    AProgress     : TRect;
    ADrawGradient : Boolean;
    AColor        : TColor;
    AColorA       : TColor;
    AColorB       : TColor;
begin
  ADrawGradient := False;
  ///

  pData := Node.GetData;

  if not Assigned(pData) then
    Exit();

  AProgress := CellRect;

  AColor  := clNone;
  AColorA := clNone;
  AColorB := clNone;

  case VST.GetNodeLevel(Node) of
    // Memory Region
    0 : begin
      AColor := _ODD_LIST_BG_COLOR;

      if Column = 4 then begin
        //AProgress.Width := muldiv(AProgress.Width, pData^.Size, FMemorySize);
        if FMemorySize > 0 then
          AProgress.Width := (AProgress.Width * pData^.Size) div FMemorySize
        else
          AProgress.Width := 0;

        AColorA := _COLOR_GRAD2_BEG;
        AColorB := _COLOR_GRAD2_END;

        ADrawGradient := True;
      end;
    end;

    // Memory Region page
    1 : begin
      case pData^.Page.Kind of
        mpkPEHeader      : AColor := _COLOR_10;
        mpkPESectionData : AColor := _COLOR_11;
      end;

      if Column = 4 then begin
        pParentData := Node.Parent.GetData;
        if not Assigned(pParentData) then
          Exit();

        AColorA := _COLOR_GRAD1_BEG;
        AColorB := _COLOR_GRAD1_END;

        //AProgress.Width := muldiv(AProgress.Width, pData^.Size, pParentData^.Size);
        if pParentData^.Size > 0 then
          AProgress.Width := (AProgress.Width * pData^.Size) div pParentData^.Size
        else
          AProgress.Width := 0;

        ADrawGradient := True;
      end;
    end;
  end;

  // Draw Optional Background Color
  if AColor <> clNone then begin
    TargetCanvas.Brush.Color := AColor;

    TargetCanvas.FillRect(CellRect);
  end;

  // Draw Optional Grandiant
  if ADrawGradient and (AColorA <> clNone) and (AColorB <> clNone) then
    DrawGradient(
      TargetCanvas,
      AColorA,
      AColorB,
      AProgress,
      False
    );
end;

procedure TFrameMemoryMap.VSTChange(Sender: TBaseVirtualTree;
  Node: PVirtualNode);
begin
  VST.Refresh();
end;

procedure TFrameMemoryMap.VSTCompareNodes(Sender: TBaseVirtualTree; Node1,
  Node2: PVirtualNode; Column: TColumnIndex; var Result: Integer);
var pData1  : PTreeData;
    pData2  : PTreeData;
    ALevel1 : Byte;
    ALevel2 : Byte;
begin
  pData1 := Node1.GetData;
  pData2 := Node2.GetData;
  ///

  ALevel1 := VST.GetNodeLevel(Node1);
  ALevel2 := VST.GetNodeLevel(Node2);

  if ALevel1 <> ALevel2 then
    Exit();

  case ALevel1 of
    // Memory Region
    0 : begin
      case Column of
        // Base Address
        0 : Result := CompareValue(
              NativeUInt(pData1^.BaseAddress),
              NativeUInt(pData2^.BaseAddress)
            );

        // Size
        4 : Result := CompareValue(
              pData1^.Size,
              pData2^.Size
            );

        else
          Exit();
      end;
    end;

    // Memory Region Page
    1 : begin
      if not Assigned(pData1^.Page) or
         not Assigned(pData2^.Page) then
          Exit();

      case Column of
        // Base Address
        0 : Result := CompareValue(
              NativeUInt(pData1^.Page.PageAddress),
              NativeUInt(pData2^.Page.PageAddress)
            );

        // Page Type
        1 : Result := CompareValue(
              pData1^.Page.PageType,
              pData2^.Page.PageType
            );

        // Page State
        2 : Result := CompareValue(
              pData1^.Page.State,
              pData2^.Page.State
            );

        // Page Protect
        3 : Result := CompareValue(
              pData1^.Page.Protect,
              pData2^.Page.Protect
            );

        // Page Size
        4 : Result := CompareValue(
              pData1^.Page.Size,
              pData2^.Page.Size
            );
      end;
    end;
  end;
end;

procedure TFrameMemoryMap.VSTFocusChanged(Sender: TBaseVirtualTree;
  Node: PVirtualNode; Column: TColumnIndex);
begin
  VST.Refresh();
end;

procedure TFrameMemoryMap.VSTFreeNode(Sender: TBaseVirtualTree;
  Node: PVirtualNode);
var pData : PTreeData;
begin
  pData := Node.GetData;
  ///

  if Assigned(pData) then begin
    if Assigned(pData^.Page) then
      FreeAndNil(pData^.Page);
  end;
end;

procedure TFrameMemoryMap.VSTGetImageIndex(Sender: TBaseVirtualTree;
  Node: PVirtualNode; Kind: TVTImageKind; Column: TColumnIndex;
  var Ghosted: Boolean; var ImageIndex: TImageIndex);
var pData  : PTreeData;
    ALevel : Cardinal;
begin
  pData := Node.GetData;

  if not Assigned(pData) then
    Exit();

  ALevel := VST.GetNodeLevel(Node);

  case Kind of
    ikNormal, ikSelected: begin
      if column = 0 then begin
        ImageIndex := pData^.ImageIndex;
      end;
    end;

    ikState: begin
      if column = 0 then begin
        if ALevel = 0 then begin
          if pData^.ImageIndex = -1 then
            ImageIndex := 10;
        end else begin
          if IsMemoryExecutable(pData^.Page.Protect) then
            ImageIndex := 26
          else
            ImageIndex := 25;
        end;
      end;
    end;
  end;
end;

procedure TFrameMemoryMap.VSTGetNodeDataSize(Sender: TBaseVirtualTree;
  var NodeDataSize: Integer);
begin
  NodeDataSize := SizeOf(TTreeData);
end;

procedure TFrameMemoryMap.VSTGetText(Sender: TBaseVirtualTree;
  Node: PVirtualNode; Column: TColumnIndex; TextType: TVSTTextType;
  var CellText: string);
var pData : PTreeData;
begin
  pData := Node.GetData;

  if not Assigned(pData) then
    Exit();

  CellText := '';

  if VST.GetNodeLevel(Node) = 0 then begin
    // Memory Region
    case Column of
      0 : begin
        if pData^.RegionName.IsEmpty then
          CellText := Format('0x%p', [pData^.BaseAddress])
        else
          CellText := Format('0x%p (%s)', [
            pData^.BaseAddress,
            pData^.RegionName
          ]);
      end;

      4 : CellText := pData^.FormatedSize;
    end;
  end else begin
    // Memory Page
    if not Assigned(pData^.Page) then
      Exit();

    case Column of
      0 : begin
        if pData^.Page.Name.IsEmpty then
          CellText := Format('0x%p', [pData^.Page.PageAddress])
        else
          CellText := Format('0x%p (%s)', [
            pData^.Page.PageAddress,
            pData^.Page.Name
          ]);
      end;

      1 : CellText := pData^.Page.Type_STR;
      2 : CellText := pData^.Page.State_STR;
      3 : begin
        if pData^.Page.Protect = 0 then
          CellText := Format('0x%.8x', [pData^.Page.Protect])
        else
          CellText := Format('0x%.8x (%s)', [
            pData^.Page.Protect,
            pData^.Page.Protect_STR
          ]);
      end;
      4 : CellText := pData^.FormatedSize;
    end;
  end;

end;

end.
