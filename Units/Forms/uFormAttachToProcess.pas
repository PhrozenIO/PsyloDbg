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

unit uFormAttachToProcess;

interface

uses
  Winapi.Windows, Winapi.Messages, System.SysUtils, System.Variants, System.Classes, Vcl.Graphics,
  Vcl.Controls, Vcl.Forms, Vcl.Dialogs, Vcl.ComCtrls, Vcl.Buttons, Vcl.ExtCtrls;

type
  TFormAttachToProcess = class(TForm)
    ProcessList: TListView;
    PanelBottom: TPanel;
    ButtonStart: TSpeedButton;
    ButtonCancel: TSpeedButton;
    ButtonRefresh: TSpeedButton;
    procedure ButtonCancelClick(Sender: TObject);
    procedure FormShow(Sender: TObject);
    procedure ButtonRefreshClick(Sender: TObject);
    procedure ProcessListCustomDrawItem(Sender: TCustomListView;
      Item: TListItem; State: TCustomDrawState; var DefaultDraw: Boolean);
    procedure FormKeyUp(Sender: TObject; var Key: Word; Shift: TShiftState);
    procedure ProcessListSelectItem(Sender: TObject; Item: TListItem;
      Selected: Boolean);
    procedure ProcessListDblClick(Sender: TObject);
    procedure ButtonStartClick(Sender: TObject);
    procedure ProcessListDeletion(Sender: TObject; Item: TListItem);
  private
    {@M}
    procedure Refresh();
  public
  end;

var
  FormAttachToProcess: TFormAttachToProcess;

implementation

uses uFormMain, Winapi.TlHelp32, uFunctions, uExceptions;

{$R *.dfm}

procedure TFormAttachToProcess.ButtonRefreshClick(Sender: TObject);
begin
  self.Refresh();
end;

procedure TFormAttachToProcess.ButtonStartClick(Sender: TObject);
begin
  if self.ProcessList.Selected = nil then
    Exit();
  ///

  FormMain.AttachProcess(PCardinal(self.ProcessList.Selected.Data)^);

  self.Close();
end;

procedure TFormAttachToProcess.FormKeyUp(Sender: TObject; var Key: Word;
  Shift: TShiftState);
begin
  case Key of
    13    : ButtonStart.Click();
    27    : ButtonCancel.Click();
    VK_F5 : ButtonRefresh.Click();
  end;
end;

procedure TFormAttachToProcess.FormShow(Sender: TObject);
begin
  self.Refresh();
end;

procedure TFormAttachToProcess.ProcessListCustomDrawItem(
  Sender: TCustomListView; Item: TListItem; State: TCustomDrawState;
  var DefaultDraw: Boolean);
begin
  if odd(Item.Index) then
    TListView(Sender).Canvas.Brush.Color := RGB(250, 250, 250);

  ///
  DefaultDraw := True;
end;

procedure TFormAttachToProcess.ProcessListDblClick(Sender: TObject);
begin
  self.ButtonStart.Click();
end;

procedure TFormAttachToProcess.ProcessListDeletion(Sender: TObject;
  Item: TListItem);
begin
  if Assigned(Item.Data) then
    FreeMem(Item.Data, SizeOf(Cardinal));
end;

procedure TFormAttachToProcess.ProcessListSelectItem(Sender: TObject;
  Item: TListItem; Selected: Boolean);
begin
  ButtonStart.Enabled := Selected;
end;

procedure TFormAttachToProcess.Refresh();
var hSnap         : THandle;
    AProcessEntry : TProcessEntry32;

    procedure AddItem();
    var AImagePath : String;
        pProcessId : PCardinal;
    begin
      try
        GetMem(pProcessId, SizeOf(Cardinal));

        pProcessId^ := AProcessEntry.th32ProcessID;

        // Filter different process arch
        if not IsProcessRunningSameArchitecture(pProcessId^) then
          Exit();
        ///

        if (not IsCurrentProcessElevated()) then begin
          if IsProcessElevatedById(pProcessId^) then
            Exit();
        end;

        AImagePath := GetImagePathFromProcessId(pProcessId^)
      except
        Exit();
      end;
      ///

      with ProcessList.Items.Add() do begin
        Caption := Format('%s (%d)', [
          ExtractFileName(AImagePath),
          pProcessId^
        ]);

        Data := pProcessId;

        ///
        ImageIndex := SystemFileIcon(AImagePath);
      end;
    end;

begin
  self.ProcessList.Clear();
  ButtonStart.Enabled := False;
  ///

  hSnap := CreateToolHelp32Snapshot(TH32CS_SNAPPROCESS, 0);
  if hSnap = INVALID_HANDLE_VALUE then
    raise EWindowsException.Create('CreateToolHelp32Snapshot');

  ZeroMemory(@AProcessEntry, SizeOf(TProcessEntry32));
  AProcessEntry.dwSize := SizeOf(TProcessEntry32);

  if not Process32First(hSnap, AProcessEntry) then
    raise EWindowsException.Create('Process32First');

  repeat
    AddItem();

    ///
    ZeroMemory(@AProcessEntry, SizeOf(TProcessEntry32));
    AProcessEntry.dwSize := SizeOf(TProcessEntry32);
  until (not Process32Next(hSnap, AProcessEntry));
end;

procedure TFormAttachToProcess.ButtonCancelClick(Sender: TObject);
begin
  self.Close();
end;

end.
