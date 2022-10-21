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

unit uFrameEventDetail;

interface

uses
  Winapi.Windows, Winapi.Messages, System.SysUtils, System.Variants, System.Classes,
  Vcl.Graphics, Vcl.Controls, Vcl.Forms, Vcl.Dialogs, VirtualTrees, Vcl.StdCtrls,
  XSuperObject, Vcl.ExtCtrls, Vcl.Buttons;

type
  TTreeData = record
    Key             : String;
    Value           : String;
    Hex             : String;
    ImageIndex      : Integer;
    StateImageIndex : Integer;
  end;
  PTreeData = ^TTreeData;

  TFrameEventDetail = class(TFrame)
    VST: TVirtualStringTree;
    PanelBottom: TPanel;
    ButtonCopy: TSpeedButton;
    EditClipboard: TEdit;
    procedure VSTGetNodeDataSize(Sender: TBaseVirtualTree;
      var NodeDataSize: Integer);
    procedure VSTChange(Sender: TBaseVirtualTree; Node: PVirtualNode);
    procedure VSTFocusChanged(Sender: TBaseVirtualTree; Node: PVirtualNode;
      Column: TColumnIndex);
    procedure VSTGetText(Sender: TBaseVirtualTree; Node: PVirtualNode;
      Column: TColumnIndex; TextType: TVSTTextType; var CellText: string);
    procedure VSTPaintText(Sender: TBaseVirtualTree;
      const TargetCanvas: TCanvas; Node: PVirtualNode; Column: TColumnIndex;
      TextType: TVSTTextType);
    procedure VSTBeforeCellPaint(Sender: TBaseVirtualTree;
      TargetCanvas: TCanvas; Node: PVirtualNode; Column: TColumnIndex;
      CellPaintMode: TVTCellPaintMode; CellRect: TRect; var ContentRect: TRect);
    procedure VSTGetImageIndex(Sender: TBaseVirtualTree; Node: PVirtualNode;
      Kind: TVTImageKind; Column: TColumnIndex; var Ghosted: Boolean;
      var ImageIndex: TImageIndex);
    procedure EditClipboardChange(Sender: TObject);
    procedure ButtonCopyClick(Sender: TObject);
  private
    { Private declarations }
  public
    {@M}
    procedure LoadJsonNode(const AJsonNode : ISuperObject; const AParent : PVirtualNode = nil);
    procedure Reset();
  end;

implementation

uses uFormMain, System.StrUtils, uFunctions, uConstants, uEventUtils;

{$R *.dfm}

procedure TFrameEventDetail.VSTBeforeCellPaint(Sender: TBaseVirtualTree;
  TargetCanvas: TCanvas; Node: PVirtualNode; Column: TColumnIndex;
  CellPaintMode: TVTCellPaintMode; CellRect: TRect; var ContentRect: TRect);
begin
  if odd(Node.Index) then begin
    TargetCanvas.Brush.Color := _ODD_LIST_BG_COLOR;

    TargetCanvas.FillRect(CellRect);
  end;
end;

procedure TFrameEventDetail.VSTChange(Sender: TBaseVirtualTree;
  Node: PVirtualNode);
begin
  VST.Refresh();
end;

procedure TFrameEventDetail.VSTFocusChanged(Sender: TBaseVirtualTree;
  Node: PVirtualNode; Column: TColumnIndex);
var pData : PTreeData;
begin
  VST.Refresh();

  pData := Node.GetData;
  if Assigned(pData) then
    self.EditClipboard.Text := pData^.Value
  else
    self.EditClipboard.Clear;
end;

procedure TFrameEventDetail.VSTGetImageIndex(Sender: TBaseVirtualTree;
  Node: PVirtualNode; Kind: TVTImageKind; Column: TColumnIndex;
  var Ghosted: Boolean; var ImageIndex: TImageIndex);
var pData : PTreeData;
begin
  pData := Node.GetData;
  if not Assigned(pData) then
    Exit();
  ///

  case Kind of
    ikNormal, ikSelected: begin
      if Column = 0 then
        ImageIndex := pData^.ImageIndex;
    end;

    ikState: begin
      if Column = 0 then
        ImageIndex := pData^.StateImageIndex;
    end;
  end;
end;

procedure TFrameEventDetail.VSTGetNodeDataSize(Sender: TBaseVirtualTree;
  var NodeDataSize: Integer);
begin
  NodeDataSize := SizeOf(TTreeData);
end;

procedure TFrameEventDetail.VSTGetText(Sender: TBaseVirtualTree;
  Node: PVirtualNode; Column: TColumnIndex; TextType: TVSTTextType;
  var CellText: string);
var pData : PTreeData;
begin
  pData := Node.GetData;
  if not Assigned(pData) then
    Exit();
  ///

  case Column of
    0 : CellText := pData^.Key;
    1 : CellText := pData^.Value;
    2 : CellText := pData^.Hex;
  end;
end;

procedure TFrameEventDetail.VSTPaintText(Sender: TBaseVirtualTree;
  const TargetCanvas: TCanvas; Node: PVirtualNode; Column: TColumnIndex;
  TextType: TVSTTextType);
begin
  case Column of
    2 : begin
      TargetCanvas.Font.Color := RGB(0, 174, 174);
    end;
  end;
end;

procedure TFrameEventDetail.ButtonCopyClick(Sender: TObject);
begin
  self.EditClipboard.CopyToClipboard;
end;

procedure TFrameEventDetail.EditClipboardChange(Sender: TObject);
begin
  self.ButtonCopy.Enabled := (Length(self.EditClipboard.Text) > 0);
end;

procedure TFrameEventDetail.Reset();
begin
  self.EditClipboard.Clear();

  VST.Clear();
end;

procedure TFrameEventDetail.LoadJsonNode(const AJsonNode : ISuperObject; const AParent : PVirtualNode = nil);
var pNode      : PVirtualNode;
    pData      : PTreeData;
    I          : Cardinal;
    pChildNode : PVirtualNode;
    pChildData : PTreeData;
begin
  if AParent = nil then
    self.Reset();
  ///

  VST.BeginUpdate();
  try
    AJsonNode.First;
    repeat
      try
        pNode := VST.AddChild(AParent);
        pData := pNode.GetData;
        ///

        pData^.Key             := AJsonNode.CurrentKey.Replace('_', ' ', [rfReplaceAll]);
        pData^.ImageIndex      := -1;
        pData^.StateImageIndex := _STATE_IMAGE_EVENT_ITEM;
        pData^.Hex             := '-';

        case AJsonNode.GetType(AJsonNode.CurrentKey) of
          varSmallInt, varWord, varInteger, varUInt32, varSingle, varByte,
          varShortInt, varInt64, varDouble, varUInt64 : begin
            pData^.Value := Format('%d', [AJsonNode.I[AJsonNode.CurrentKey]]);
            pData^.Hex := Format('0x%.16x', [AJsonNode.I[AJsonNode.CurrentKey]]);

            // Special Lookup
            case AnsiIndexStr(AJsonNode.CurrentKey, [
              _EVENT_KIND
            ]) of
              0 : begin
                pData^.StateImageIndex := EventKindToImageIndex(AJsonNode.I[AJsonNode.CurrentKey]);
              end;
            end;
          end;

          varDate:
            pData^.Value := DateTimeToStr(AJsonNode.D[AJsonNode.CurrentKey]);

          varBoolean : begin
            if AJsonNode.B[AJsonNode.CurrentKey] then
              pData^.Value := 'True'
            else
              pData^.Value := 'False';
          end;

          varString, varUString : begin
            pData^.Value := AJsonNode.S[AJsonNode.CurrentKey];
            ///pData^.Hex := StringToHex(pData^.Value);

            // Special Lookup
            case AnsiIndexStr(AJsonNode.CurrentKey, [
              _IMAGE_PATH,
              _PARENT_IMAGE_PATH,
              _RESOLVED_IMAGE_NAME
            ]) of
              0, 1, 2 : begin
                if FileExists(pData^.Value) then
                  pData^.ImageIndex := SystemFileIcon(pData^.Value)
                else
                  pData^.ImageIndex := FormMain.DefaultExeIconIndex;
              end;
            end;
          end;

          varArray : begin
            pData^.Key := AJsonNode.CurrentKey.Replace('_', ' ', [rfReplaceAll]);
            pData^.StateImageIndex := _STATE_IMAGE_EVENT_ARRAY;

            for I := 0 to AJsonNode.A[AJsonNode.CurrentKey].Length -1 do begin
              pChildNode := VST.AddChild(pNode);
              pChildData := pChildNode.GetData;
              ///

              pChildData^.ImageIndex := -1;

              // Special Lookup
              case AnsiIndexStr(AJsonNode.CurrentKey, [
                _EXCEPTION_CHAIN
              ]) of
                0 : begin
                  pChildData^.StateImageIndex := _STATE_IMAGE_EVENT_EXCEPTION;
                  pChildData^.Key := Format('Exception: %d', [I + 1]);
                end
              else
                pChildData^.StateImageIndex := -1;
                pChildData^.Key := Format('Row: %d', [I + 1]);
              end;

              self.LoadJsonNode(AJsonNode.A[AJsonNode.CurrentKey].O[I], pChildNode);
            end;
          end;
        end;

        ///
        if pData^.ImageIndex <> -1 then
          pData^.StateImageIndex := -1;
      finally
        AJsonNode.Next;
      end;
    until AJsonNode.EoF;
  finally
    VST.EndUpdate();
  end;

  if not Assigned(AParent) then
    VST.FullExpand(nil);
end;

end.
