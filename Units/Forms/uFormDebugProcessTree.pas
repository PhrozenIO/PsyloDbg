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

unit uFormDebugProcessTree;

interface

uses
  Winapi.Windows, Winapi.Messages, System.SysUtils, System.Variants, System.Classes, Vcl.Graphics,
  Vcl.Controls, Vcl.Forms, Vcl.Dialogs, VirtualTrees, uDebugSession,
  System.Generics.Collections;

type
  TTreeData = record
    ImagePath  : String;
    ProcessId  : Cardinal;
    ImageIndex : Integer;
  end;
  PTreeData = ^TTreeData;

  TFormDebugProcessTree = class(TForm)
    VST: TVirtualStringTree;
    procedure VSTChange(Sender: TBaseVirtualTree; Node: PVirtualNode);
    procedure VSTFocusChanged(Sender: TBaseVirtualTree; Node: PVirtualNode;
      Column: TColumnIndex);
    procedure VSTGetText(Sender: TBaseVirtualTree; Node: PVirtualNode;
      Column: TColumnIndex; TextType: TVSTTextType; var CellText: string);
    procedure VSTGetImageIndex(Sender: TBaseVirtualTree; Node: PVirtualNode;
      Kind: TVTImageKind; Column: TColumnIndex; var Ghosted: Boolean;
      var ImageIndex: TImageIndex);
    procedure VSTGetNodeDataSize(Sender: TBaseVirtualTree;
      var NodeDataSize: Integer);
    procedure FormShow(Sender: TObject);
    procedure VSTBeforeCellPaint(Sender: TBaseVirtualTree;
      TargetCanvas: TCanvas; Node: PVirtualNode; Column: TColumnIndex;
      CellPaintMode: TVTCellPaintMode; CellRect: TRect; var ContentRect: TRect);
  private
    {@M}
    procedure OnEnumDebuggedProcessBegins(Sender : TObject);
    procedure OnEnumDebuggedProcessEnds(Sender : TObject);
    function GetNode(const AProcessId : Cardinal) : PVirtualNode;
    procedure GetRecursiveChilds(const pNode : PVirtualNode; var AChildNodes : TList<PVirtualNode>);
  public
    {@M}
    procedure ResetData();
    procedure AddProcess(const ADebugProcess : TDebugProcess);
    procedure RemoveProcess(const ADebugProcess : TDebugProcess);
  end;

var
  FormDebugProcessTree: TFormDebugProcessTree;

implementation

uses uFormMain, uFormPsyloThreadManager, uFunctions,
     uConstants;

{$R *.dfm}

function TFormDebugProcessTree.GetNode(const AProcessId : Cardinal) : PVirtualNode;
var pNode : PVirtualNode;
    pData : PTreeData;
begin
  result := nil;
  ///

  for pNode in VST.Nodes do begin
    pData := pNode.GetData;
    if not Assigned(pData) then
      continue;
    ///

    if pData^.ProcessId = AProcessId then begin
      result := pNode;

      break;
    end;
  end;
end;

// TODO: Use new event system to add process
procedure TFormDebugProcessTree.AddProcess(const ADebugProcess : TDebugProcess);
var pParentNode      : PVirtualNode;
    pNode            : PVirtualNode;
    pData            : PTreeData;
    AParentProcessId : Cardinal;
begin
  VST.BeginUpdate();
  try
    AParentProcessId := GetParentProcessId(ADebugProcess.ProcessId);

    pParentNode := GetNode(AParentProcessId);
    ///

    pNode := VST.AddChild(pParentNode);
    pData := pNode.GetData;

    pData^.ProcessId  := ADebugProcess.ProcessId;
    pData^.ImagePath  := ADebugProcess.ImagePath;
    pData^.ImageIndex := SystemFileIcon(pData^.ImagePath);

    ///
    VST.FullExpand(nil);
  finally
    VST.EndUpdate();
  end;
end;

procedure TFormDebugProcessTree.GetRecursiveChilds(const pNode : PVirtualNode; var AChildNodes : TList<PVirtualNode>);
var pChildNode : PVirtualNode;
begin
  if not Assigned(AChildNodes) then
    Exit();
  ///

  for pChildNode in VST.ChildNodes(pNode) do begin
    AChildNodes.Add(pChildNode);

    ///
    GetRecursiveChilds(pChildNode, AChildNodes);
  end;
end;

// TODO: Use new event system to remove process
procedure TFormDebugProcessTree.RemoveProcess(const ADebugProcess : TDebugProcess);
var pNode       : PVirtualNode;
    pChildNode  : PVirtualNode;
    AChildNodes : TList<PVirtualNode>;
    pData       : PTreeData;
    pParentNode : PVirtualNode;
    pNewNode    : PVirtualNode;
    pNewData    : PTreeData;
begin
  if not Assigned(ADebugProcess) then
    Exit();
  ///

  pNode := GetNode(ADebugProcess.ProcessId);
  if not Assigned(pNode) then
    Exit();
  ///

  VST.BeginUpdate();
  try
    AChildNodes := TList<PVirtualNode>.Create();
    try
      GetRecursiveChilds(pNode, AChildNodes);
      ///

      for pChildNode in AChildNodes do begin
        pData := pChildNode.GetData;
        if not Assigned(pData) then
          continue;
        ///

        // Maybe a bug, changing Node.Parent does not work as expected and may cause
        // a bug. The workaround is to create a new node on root then copy old
        // pData to new pData.
        // TODO: Understand why VirtualStringTree Parent Update does not work as exptected.
        pNewNode := VST.AddChild(nil);

        pNewData := pNewNode.GetData;
        pNewData^.ImagePath  := pData^.ImagePath;
        pNewData^.ProcessId  := pData^.ProcessId;
        pNewData^.ImageIndex := pData^.ImageIndex;
      end;
    finally
      if Assigned(AChildNodes) then
        FreeAndNil(AChildNodes);
    end;

    VST.DeleteNode(pNode);
    //pNode.States := pNode.States + [vsDisabled];
  finally
    VST.EndUpdate();
  end;
end;

procedure TFormDebugProcessTree.ResetData();
begin
  VST.Clear();
end;

procedure TFormDebugProcessTree.FormShow(Sender: TObject);
begin
  self.Refresh();
end;

procedure TFormDebugProcessTree.OnEnumDebuggedProcessBegins(Sender : TObject);
begin
  self.ResetData();

  VST.BeginUpdate();
end;

procedure TFormDebugProcessTree.OnEnumDebuggedProcessEnds(Sender : TObject);
begin
  VST.FullExpand(nil);

  VST.EndUpdate();
end;


procedure TFormDebugProcessTree.VSTBeforeCellPaint(Sender: TBaseVirtualTree;
  TargetCanvas: TCanvas; Node: PVirtualNode; Column: TColumnIndex;
  CellPaintMode: TVTCellPaintMode; CellRect: TRect; var ContentRect: TRect);
var AColor : TColor;
    pData  : PTreeData;
begin
  AColor := clNone;
  ///

  pData := Node.GetData;
  if not Assigned(pData) then
    Exit();
  ///


  if AColor <> clNone then begin
    TargetCanvas.Brush.Color := AColor;

    TargetCanvas.FillRect(CellRect);
  end;
end;

procedure TFormDebugProcessTree.VSTChange(Sender: TBaseVirtualTree;
  Node: PVirtualNode);
begin
  VST.Refresh();
end;

procedure TFormDebugProcessTree.VSTFocusChanged(Sender: TBaseVirtualTree;
  Node: PVirtualNode; Column: TColumnIndex);
begin
  VST.Refresh();
end;

procedure TFormDebugProcessTree.VSTGetImageIndex(Sender: TBaseVirtualTree;
  Node: PVirtualNode; Kind: TVTImageKind; Column: TColumnIndex;
  var Ghosted: Boolean; var ImageIndex: TImageIndex);
var pData : PTreeData;
begin
  pData := Node.GetData;
  if not Assigned(pData) then
    Exit();
  ///

  case Kind of
    ikNormal, ikSelected:
      ImageIndex := pData^.ImageIndex;
  end;
end;

procedure TFormDebugProcessTree.VSTGetNodeDataSize(Sender: TBaseVirtualTree;
  var NodeDataSize: Integer);
begin
  NodeDataSize := SizeOf(TTreeData);
end;

procedure TFormDebugProcessTree.VSTGetText(Sender: TBaseVirtualTree;
  Node: PVirtualNode; Column: TColumnIndex; TextType: TVSTTextType;
  var CellText: string);
var pData : PTreeData;
begin
  pData := Node.GetData;
  if not Assigned(pData) then
    Exit();
  ///

  case Column of
    0 : CellText := Format('%s (%d)', [
      ExtractFileName(pData^.ImagePath),
      pData^.ProcessId
    ]);
  end;
end;

end.
