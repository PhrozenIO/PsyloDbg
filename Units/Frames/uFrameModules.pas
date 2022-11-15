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

unit uFrameModules;

interface

uses
  Winapi.Windows, Winapi.Messages, System.SysUtils, System.Variants, System.Classes,
  Vcl.Graphics, Vcl.Controls, Vcl.Forms, Vcl.Dialogs, VirtualTrees, uMemoryModules,
  uPsyloThread, uFrameComponentComboProcess, uPsyloFrame, Vcl.Menus;

type
  TTreeData = record
    Module     : TMemoryModule;
    ImageIndex : Cardinal;
  end;
  PTreeData = ^TTreeData;

  TFrameModules = class(TPsyloFrame)
    FrameComponentComboProcess1: TFrameComponentComboProcess;
    VST: TVirtualStringTree;
    PopupMenu: TPopupMenu;
    FileProperties1: TMenuItem;
    N1: TMenuItem;
    ShowinExplorer1: TMenuItem;
    DumpMemoryImage1: TMenuItem;
    N2: TMenuItem;
    DumpandReconstructallModulesPEImage1: TMenuItem;
    Refresh1: TMenuItem;
    N3: TMenuItem;
    SelectAll1: TMenuItem;
    N4: TMenuItem;
    procedure VSTChange(Sender: TBaseVirtualTree; Node: PVirtualNode);
    procedure VSTFreeNode(Sender: TBaseVirtualTree; Node: PVirtualNode);
    procedure VSTFocusChanged(Sender: TBaseVirtualTree; Node: PVirtualNode;
      Column: TColumnIndex);
    procedure VSTGetNodeDataSize(Sender: TBaseVirtualTree;
      var NodeDataSize: Integer);
    procedure VSTGetText(Sender: TBaseVirtualTree; Node: PVirtualNode;
      Column: TColumnIndex; TextType: TVSTTextType; var CellText: string);
    procedure VSTBeforeCellPaint(Sender: TBaseVirtualTree;
      TargetCanvas: TCanvas; Node: PVirtualNode; Column: TColumnIndex;
      CellPaintMode: TVTCellPaintMode; CellRect: TRect; var ContentRect: TRect);
    procedure VSTGetImageIndex(Sender: TBaseVirtualTree; Node: PVirtualNode;
      Kind: TVTImageKind; Column: TColumnIndex; var Ghosted: Boolean;
      var ImageIndex: TImageIndex);
    procedure VSTCompareNodes(Sender: TBaseVirtualTree; Node1,
      Node2: PVirtualNode; Column: TColumnIndex; var Result: Integer);
    procedure Refresh1Click(Sender: TObject);
    procedure PopupMenuPopup(Sender: TObject);
    procedure FileProperties1Click(Sender: TObject);
    procedure ShowinExplorer1Click(Sender: TObject);
    procedure SelectAll1Click(Sender: TObject);
    procedure DumpandReconstructallModulesPEImage1Click(Sender: TObject);
    procedure DumpMemoryImage1Click(Sender: TObject);
  private
    FTotalModuleSize  : UInt64;
    FCurrentProcessId : Cardinal;

    {@M}
    procedure OnEnumModulesBegins(Sender : TObject);
    procedure OnEnumModulesEnds(Sender : TObject; const AOnError : Boolean; const AErrorDetail : String);
    procedure OnSelectProcess(Sender: TObject; const AProcessId : Cardinal);
    function GetNodeFilePath(const pNode : PVirtualNode) : String;
    procedure DumpMemoryModules(const AOnlySelected : Boolean = True);
    procedure OnDumpMemoryImageEnds(Sender : TObject; const AOnError : Boolean; const AErrorDetail : String);
  protected
    {@M}
    procedure OnDebugNewProcess(const AProcessId : Cardinal); override;
    procedure OnDebugExitProcess(const AProcessId : Cardinal); override;
  public
    {@M}
    procedure Refresh(AProcessId : Integer = -1);
    procedure ResetData();

    {@G/S}
    property TotalModuleSize : UInt64 read FTotalModuleSize write FTotalModuleSize;

    {@C}
    constructor Create(AOwner : TComponent); override;
  end;

implementation

uses uFormMain, uMemoryModulesThread, uFunctions, uGraphicUtils, uConstants,
     uFormPsyloThreadManager, System.Math, uDumpMemoryImageThread, VCL.FileCtrl,
     uStrings, uDialogs;

{$R *.dfm}

procedure TFrameModules.OnDumpMemoryImageEnds(Sender : TObject; const AOnError : Boolean; const AErrorDetail : String);
begin
  if AOnError then
    Dialog_ActionError(AErrorDetail)
  else
    Dialog_SuccessStoredData(TDumpMemoryImageThread(Sender).DestPath);
end;

procedure TFrameModules.DumpMemoryImage1Click(Sender: TObject);
begin
  self.DumpMemoryModules(True);
end;

procedure TFrameModules.DumpMemoryModules(const AOnlySelected : Boolean = True);
var AWorker    : TDumpMemoryImageThread;
    ADirectory : String;
    pNode      : PVirtualNode;
    pData      : PTreeData;
begin
  if not FormMain.Debugging then
    Exit();
  ///

  if not SelectDirectory('Select output directory', '', ADirectory, [sdNewFolder, sdShowShares]) then
    Exit();

  AWorker := TDumpMemoryImageThread.Create(FCurrentProcessId, ADirectory);

  if AOnlySelected then begin
    for pNode in VST.Nodes do begin
      if not (vsSelected in pNode.States) then
        continue;
      ///

      pData := pNode.GetData;
      if not Assigned(pData) then
        continue;
      ///

      AWorker.AddTargetModule(pData^.Module.Handle);

      AWorker.Mode := dmimListedOnly;
    end;
  end else
    AWorker.Mode := dmimAll;

  AWorker.OnThreadExecutionEnds := self.OnDumpMemoryImageEnds;

  AWorker.Start();
end;

procedure TFrameModules.OnDebugNewProcess(const AProcessId : Cardinal);
begin
  ///
end;

procedure TFrameModules.DumpandReconstructallModulesPEImage1Click(
  Sender: TObject);
begin
  self.DumpMemoryModules(False);
end;

procedure TFrameModules.FileProperties1Click(Sender: TObject);
var AFilePath : String;
begin
  AFilePath := GetNodeFilePath(VST.FocusedNode);
  ///

  if FileExists(AFilePath) then
    FileProperties(AFilePath);
end;

function TFrameModules.GetNodeFilePath(const pNode : PVirtualNode) : String;
var pData : PTreeData;
begin
  result := '';
  ///

  if not Assigned(pNode) then
    Exit();

  pData := pNode.GetData;
  if not Assigned(pData) then
    Exit();
  ///

  result := pData^.Module.ImagePath;
end;

procedure TFrameModules.OnDebugExitProcess(const AProcessId : Cardinal);
var ANode : PVirtualNode;
begin
  if AProcessId = FCurrentProcessId then begin
    VST.BeginUpdate();
    try
      for ANode in VST.Nodes do
        ANode.States := ANode.States + [vsDisabled];
    finally
      VST.EndUpdate();
    end;
  end;
end;

procedure TFrameModules.OnSelectProcess(Sender: TObject; const AProcessId : Cardinal);
begin
  Refresh(AProcessId);
end;

procedure TFrameModules.PopupMenuPopup(Sender: TObject);
begin
  self.DumpandReconstructallModulesPEImage1.Enabled := VST.TotalCount > 0;
  self.DumpMemoryImage1.Enabled := VST.FocusedNode <> nil;
  self.Refresh1.Enabled         := FCurrentProcessId > 0;
  self.FileProperties1.Enabled  := VST.FocusedNode <> nil;
  self.ShowinExplorer1.Enabled  := VST.FocusedNode <> nil;
end;

constructor TFrameModules.Create(AOwner : TComponent);
begin
  inherited Create(AOwner);
  ///

  self.FrameComponentComboProcess1.OnSelectProcess := OnSelectProcess;

  FCurrentProcessId := 0;
end;

procedure TFrameModules.OnEnumModulesBegins(Sender : TObject);
begin
  self.ResetData();

  VST.BeginUpdate();
end;

procedure TFrameModules.OnEnumModulesEnds(Sender : TObject; const AOnError : Boolean; const AErrorDetail : String);
begin
  VST.EndUpdate();
end;

procedure TFrameModules.Refresh(AProcessId : Integer = -1);
var AWorker : TMemoryModulesThread;
begin
  if not FormMain.Debugging then
    Exit();

  self.ResetData();
  ///

  if AProcessId = -1 then
    AProcessId := FormMain.DebugSessionInformation.ProcessId;

  FCurrentProcessId := AProcessId;

  self.FrameComponentComboProcess1.SelectProcess(AProcessId);

  AWorker := TMemoryModulesThread.Create(self, AProcessId);
  AWorker.OnThreadExecutionBegins := OnEnumModulesBegins;
  AWorker.OnThreadExecutionEnds := OnEnumModulesEnds;
  AWorker.Start;

  FormPsyloThreadManager.AddWorker(
    AWorker
  );
end;

procedure TFrameModules.Refresh1Click(Sender: TObject);
begin
  self.Refresh(FCurrentProcessId);
end;

procedure TFrameModules.ResetData();
begin
  FTotalModuleSize := 0;
  VST.Clear();
end;

procedure TFrameModules.SelectAll1Click(Sender: TObject);
begin
  VST.SelectAll(False);
end;

procedure TFrameModules.ShowinExplorer1Click(Sender: TObject);
var AFilePath : String;
begin
  AFilePath := GetNodeFilePath(VST.FocusedNode);
  ///

  if FileExists(AFilePath) then
    ShowFileOnExplorer(AFilePath);
end;

procedure TFrameModules.VSTBeforeCellPaint(Sender: TBaseVirtualTree;
  TargetCanvas: TCanvas; Node: PVirtualNode; Column: TColumnIndex;
  CellPaintMode: TVTCellPaintMode; CellRect: TRect; var ContentRect: TRect);
var pData         : PTreeData;
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

  if pData^.Module.MainModule then
    AColor := _COLOR_10;

  if (column = 2) and Assigned(pData^.Module) then begin
    if FTotalModuleSize > 0 then
      AProgress.Width := (AProgress.Width * pData^.Module.BaseSize) div FTotalModuleSize
    else
      AProgress.Width := 0;

    AColorA := _COLOR_GRAD1_BEG;
    AColorB := _COLOR_GRAD1_END;

    ADrawGradient := True;
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

procedure TFrameModules.VSTChange(Sender: TBaseVirtualTree; Node: PVirtualNode);
begin
  VST.Refresh();
end;

procedure TFrameModules.VSTCompareNodes(Sender: TBaseVirtualTree; Node1,
  Node2: PVirtualNode; Column: TColumnIndex; var Result: Integer);
var pData1 : PTreeData;
    pData2 : PTreeData;
begin
  pData1 := Node1.GetData;
  pData2 := Node2.GetData;
  ///

  if not Assigned(pData1) or not Assigned(pData2) then
    Exit();

  case Column of
    0 : Result := CompareText(
      ExtractFileName(pData1^.Module.ImagePath),
      ExtractFileName(pData2^.Module.ImagePath)
    );
    1 : Result := CompareValue(pData1^.Module.Base, pData2^.Module.Base);
    2 : Result := CompareValue(pData1^.Module.BaseSize, pData2^.Module.BaseSize);
    3 : Result := CompareText(pData1^.Module.ImagePath, pData2^.Module.ImagePath);
  end;
end;

procedure TFrameModules.VSTFocusChanged(Sender: TBaseVirtualTree;
  Node: PVirtualNode; Column: TColumnIndex);
begin
  VST.Refresh();
end;

procedure TFrameModules.VSTFreeNode(Sender: TBaseVirtualTree;
  Node: PVirtualNode);
var pData : PTreeData;
begin
  pData := Node.GetData;
  if not Assigned(pData) then
    Exit();
  ///

  if Assigned(pData^.Module) then
    FreeAndNil(pData^.Module);
end;

procedure TFrameModules.VSTGetImageIndex(Sender: TBaseVirtualTree;
  Node: PVirtualNode; Kind: TVTImageKind; Column: TColumnIndex;
  var Ghosted: Boolean; var ImageIndex: TImageIndex);
var pData : PTreeData;
begin
  pData := Node.GetData;
  if not Assigned(pData) then
    Exit();
  ///

  if column <> 0 then
    Exit();

  case Kind of
    ikNormal, ikSelected:
      ImageIndex := pData^.ImageIndex;
    ikState: ;
    ikOverlay: ;
  end;
end;

procedure TFrameModules.VSTGetNodeDataSize(Sender: TBaseVirtualTree;
  var NodeDataSize: Integer);
begin
  NodeDataSize := SizeOf(TTreeData);
end;

procedure TFrameModules.VSTGetText(Sender: TBaseVirtualTree; Node: PVirtualNode;
  Column: TColumnIndex; TextType: TVSTTextType; var CellText: string);
var pData : PTreeData;
begin
  pData := Node.GetData;
  if not Assigned(pData) then
    Exit();
  ///

  if not Assigned(pData^.Module) then
    Exit();

  case Column of
    0 : CellText := ExtractFileName(pData^.Module.ImagePath);
    1 : CellText := Format('0x%p', [Pointer(pData^.Module.Base)]);
    2 : CellText := FormatSize(pData^.Module.BaseSize);
    3 : CellText := pData^.Module.ImagePath;
  end;
end;

end.
