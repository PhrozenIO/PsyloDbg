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

unit uFrameComponentComboProcess;

interface

uses
  Winapi.Windows, Winapi.Messages, System.SysUtils, System.Variants, System.Classes,
  Vcl.Graphics, Vcl.Controls, Vcl.Forms, Vcl.Dialogs, Vcl.StdCtrls, Vcl.ComCtrls,
  uMessageListener, uPsyloFrame;

type
  TOnSelectProcess = procedure(Sender: TObject; const AProcessId : Cardinal) of object;

  TFrameComponentComboProcess = class(TPsyloFrame)
    ComboBox: TComboBoxEx;
    procedure ComboBoxSelect(Sender: TObject);
  private
    FOnSelectProcess    : TOnSelectProcess;
    FOnVisibilityChange : TNotifyEvent;
    FAutoHide           : Boolean;

    {@M}
    procedure ResetData();
    procedure SetAutoHide(const AValue : Boolean);
  protected
    {@M}
    procedure OnDebugNewProcess(const AProcessId : Cardinal); override;
    procedure OnDebugExitProcess(const AProcessId : Cardinal); override;
    procedure OnDebugStart(); override;
    procedure OnDebugStop(); override;
    procedure OnShow(); override;
    procedure OnHide(); override;
    function GetSelectedProcess() : Integer;
    procedure UpdateVisibility();
    function GetItemIndexByProcess(const AProcessId : Cardinal) : Integer;

    procedure AddProcess(const AProcessId : Cardinal; AImagePath : String = '');
    procedure RemoveProcess(const AProcessId : Cardinal);
  public
    {@M}
    procedure Refresh();
    procedure SelectProcess(const AProcessId : Cardinal);

    {@C}
    constructor Create(AOwner : TComponent); override;
    destructor Destroy(); override;

    {@G/S}
    property OnSelectProcess    : TOnSelectProcess read FOnSelectProcess    write FOnSelectProcess;
    property OnVisibilityChange : TNotifyEvent     read FOnVisibilityChange write FOnVisibilityChange;
    property AutoHide           : Boolean          read FAutoHide           write SetAutoHide;

    {@G}
    property SelectedProcess : Integer read GetSelectedProcess;
  end;

implementation

uses uFormMain, uDebugSession, uFunctions, uConstants, uMessages;

{$R *.dfm}

procedure TFrameComponentComboProcess.OnShow();
begin
  if Assigned(FOnVisibilityChange) then
    FOnVisibilityChange(self);
end;

procedure TFrameComponentComboProcess.OnHide();
begin
  if Assigned(FOnVisibilityChange) then
    FOnVisibilityChange(self);
end;

procedure TFrameComponentComboProcess.UpdateVisibility();
var AShow : Boolean;
begin
  if not FAutoHide then
    Exit();
  ///

  AShow := FormMain.DebugSessionInformation.Processes.Count > 1;

  self.Visible := AShow;
end;

function TFrameComponentComboProcess.GetSelectedProcess() : Integer;
begin
  result := -1;
  ///

  if ComboBox.ItemIndex >= 0 then
    result := PCardinal(ComboBox.ItemsEx.ComboItems[ComboBox.ItemIndex].Data)^;
end;

function TFrameComponentComboProcess.GetItemIndexByProcess(const AProcessId : Cardinal) : Integer;
var I     : Integer;
    AItem : TComboExItem;
begin
  result := -1;
  ///

  for I := 0 to ComboBox.ItemsEx.Count -1 do begin
    AItem := ComboBox.ItemsEx.ComboItems[I];
    ///

    if PCardinal(AItem.Data)^ = AProcessId then begin
      result := I;

      break;
    end;
  end;
end;

procedure TFrameComponentComboProcess.SelectProcess(const AProcessId : Cardinal);
begin
  if self.GetSelectedProcess = AProcessId then
    Exit;
  ///  

  self.ComboBox.ItemIndex := GetItemIndexByProcess(AProcessId);
end;

procedure TFrameComponentComboProcess.OnDebugNewProcess(const AProcessId : Cardinal);
begin
  self.AddProcess(AProcessId);

  ///
  self.UpdateVisibility();
end;

procedure TFrameComponentComboProcess.OnDebugExitProcess(const AProcessId : Cardinal);
begin
  self.RemoveProcess(AProcessId);

  ///
  self.UpdateVisibility();
end;

procedure TFrameComponentComboProcess.OnDebugStart();
begin
  self.ResetData();
end;

procedure TFrameComponentComboProcess.OnDebugStop();
begin
  self.ResetData();
end;

constructor TFrameComponentComboProcess.Create(AOwner : TComponent);
begin
  inherited Create(AOwner);
  ///

  FOnSelectProcess    := nil;
  FAutoHide           := True;
  FOnVisibilityChange := nil;
  self.Visible        := False;
end;


destructor TFrameComponentComboProcess.Destroy();
begin

  ///
  inherited Destroy();
end;

procedure TFrameComponentComboProcess.SetAutoHide(const AValue : Boolean);
begin
  if AValue = FAutoHide then
    Exit();
  ///

  FAutoHide := AValue;

  if not FAutoHide then
    self.Visible := True
  else
    self.UpdateVisibility();
end;

procedure TFrameComponentComboProcess.ResetData();
var I     : Integer;
    AItem : TComboExItem;
begin
  for I := 0 to ComboBox.ItemsEx.Count -1 do begin
    AItem := ComboBox.ItemsEx.ComboItems[I];
    ///

    Dispose(PCardinal(AItem.Data));
  end;

  ///
  self.ComboBox.Clear();
end;

procedure TFrameComponentComboProcess.ComboBoxSelect(Sender: TObject);
begin
  if Assigned(FOnSelectProcess) then
    FOnSelectProcess(
      self,
      self.GetSelectedProcess()
    );
end;

procedure TFrameComponentComboProcess.AddProcess(const AProcessId : Cardinal; AImagePath : String = '');
var pInt     : PCardinal;
    AProcess : TDebugProcess;
begin
  if AImagePath = '' then begin
    AProcess := FormMain.DebugSessionInformation.GetProcess(AProcessId);
    if not Assigned(AProcess) then
      Exit();

    ///
    AImagePath := AProcess.ImagePath;
  end;
  ///

  with ComboBox.ItemsEx.Add do begin
    Caption := Format('%s (%d)', [
    ExtractFileName(AImagePath),
      AProcessId
    ]);

    ImageIndex := SystemFileIcon(AImagePath);

    New(pInt);

    pInt^ := AProcessId;

    Data := pInt;
  end;
end;

procedure TFrameComponentComboProcess.RemoveProcess(const AProcessId : Cardinal);
var AItem  : TComboExItem;
    AIndex : Integer;
begin
  AIndex := self.GetItemIndexByProcess(AProcessId);

  AItem := nil;
  if (AIndex >= 0) and (AIndex <= self.ComboBox.Items.Count) then
    AItem := self.ComboBox.ItemsEx.ComboItems[AIndex];

  if Assigned(AItem) then begin
    Dispose(PCardinal(AItem.Data));

    if AIndex = self.ComboBox.ItemIndex then
      self.ComboBox.ItemIndex := -1;

    self.ComboBox.ItemsEx.Delete(AItem.Index);
  end;
end;

procedure TFrameComponentComboProcess.Refresh();
var AProcess : TDebugProcess;    
begin
  if not FormMain.Debugging then
    Exit();
  ///

  self.ResetData();

  for AProcess in FormMain.DebugSessionInformation.Processes do
    AddProcess(AProcess.ProcessId, AProcess.ImagePath);  
end;

end.
