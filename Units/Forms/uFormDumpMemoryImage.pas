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

unit uFormDumpMemoryImage;

interface

uses
  Winapi.Windows, Winapi.Messages, System.SysUtils, System.Variants, System.Classes, Vcl.Graphics,
  Vcl.Controls, Vcl.Forms, Vcl.Dialogs, uPsyloForm, Vcl.Buttons,
  Vcl.VirtualImage, Vcl.StdCtrls, Vcl.ExtCtrls, Vcl.ComCtrls, uFrameComponentComboProcess;

type
  TFormDumpMemoryImage = class(TFormPsylo)
    PanelBackground: TPanel;
    PanelCore: TPanel;
    Shape1: TShape;
    PanelForm: TPanel;
    Label1: TLabel;
    EditPath: TButtonedEdit;
    PanelIcon: TPanel;
    ImageIcon: TVirtualImage;
    PanelBottom: TPanel;
    ButtonAction: TSpeedButton;
    ButtonCancel: TSpeedButton;
    Timer: TTimer;
    ModuleDumpOptions: TRadioGroup;
    LabelTargetProcess: TLabel;
    PanelComboProcess: TPanel;
    procedure EditPathRightButtonClick(Sender: TObject);
    procedure ButtonCancelClick(Sender: TObject);
    procedure FormKeyUp(Sender: TObject; var Key: Word; Shift: TShiftState);
    procedure ButtonActionClick(Sender: TObject);
    procedure EditPathChange(Sender: TObject);
    procedure TimerTimer(Sender: TObject);
    procedure FormShow(Sender: TObject);
    procedure FormHide(Sender: TObject);
  private
    FFrameComboProcess : TFrameComponentComboProcess;

    {@M}
    procedure OnDumpMemoryImageEnds(Sender : TObject; const AOnError : Boolean; const AErrorDetail : String);
    procedure OnSelectProcess(Sender: TObject; const AProcessId : Cardinal);
    procedure RefreshFormRequirements();
  protected
    {@M}
    procedure OnDebugStart(); override;
    procedure OnDebugStop(); override;
  public
    {@C}
    constructor Create(AOwner : TComponent); override;
    destructor Destroy(); override;
  end;

var
  FormDumpMemoryImage: TFormDumpMemoryImage;

implementation

uses VCL.FileCtrl, uDumpMemoryImageThread, uFormMain, uDialogs;

{$R *.dfm}

procedure TFormDumpMemoryImage.RefreshFormRequirements();
begin
  self.ButtonAction.Enabled := (self.FFrameComboProcess.SelectedProcess <> -1) and
                                (DirectoryExists(EditPath.Text));
end;

procedure TFormDumpMemoryImage.OnSelectProcess(Sender: TObject; const AProcessId : Cardinal);
begin
  self.RefreshFormRequirements();
end;

constructor TFormDumpMemoryImage.Create(AOwner: TComponent);
begin
  inherited Create(AOwner);
  ///

  FFrameComboProcess                 := TFrameComponentComboProcess.Create(self.PanelComboProcess);
  FFrameComboProcess.Parent          := self.PanelComboProcess;
  FFrameComboProcess.AutoHide        := False;
  FFrameComboProcess.Align           := alClient;
  FFrameComboProcess.OnSelectProcess := OnSelectProcess;

  FFrameComboProcess.ComboBox.AlignWithMargins := False;
end;

destructor TFormDumpMemoryImage.Destroy();
begin
  if Assigned(FFrameComboProcess) then
    FreeAndNil(FFrameComboProcess);

  ///
  inherited Destroy();
end;

procedure TFormDumpMemoryImage.OnDumpMemoryImageEnds(Sender : TObject; const AOnError : Boolean; const AErrorDetail : String);
begin
  if AOnError then
    Dialog_ActionError(AErrorDetail)
  else
    Dialog_SuccessStoredData(TDumpMemoryImageThread(Sender).DestPath);
end;

procedure TFormDumpMemoryImage.ButtonActionClick(Sender: TObject);
var AWorker : TDumpMemoryImageThread;
    AMode   : TDumpMemoryImageMode;
begin
  if not FormMain.Debugging then
    Exit();
  ///

  case self.ModuleDumpOptions.ItemIndex of
    0 : AMode := dmimMainModule;
    1 : AMode := dmimLoadedModules;
    2 : AMode := dmimAll;
  end;

  AWorker := TDumpMemoryImageThread.Create(self.FFrameComboProcess.SelectedProcess, EditPath.Text, AMode);

  AWorker.OnThreadExecutionEnds  := self.OnDumpMemoryImageEnds;

  AWorker.Start();
end;

procedure TFormDumpMemoryImage.ButtonCancelClick(Sender: TObject);
begin
  self.Close();
end;

procedure TFormDumpMemoryImage.EditPathChange(Sender: TObject);
begin
  self.RefreshFormRequirements();
end;

procedure TFormDumpMemoryImage.EditPathRightButtonClick(Sender: TObject);
var ADirectory : String;
begin
  if not SelectDirectory('Select output directory', '', ADirectory, [sdNewFolder, sdShowShares]) then
    Exit();

  ///
  TEdit(Sender).Text := ADirectory;
end;

procedure TFormDumpMemoryImage.FormHide(Sender: TObject);
begin
  self.Timer.Enabled := False;
end;

procedure TFormDumpMemoryImage.FormKeyUp(Sender: TObject; var Key: Word;
  Shift: TShiftState);
begin
  case Key of
    13 : ButtonAction.Click();
    27 : ButtonCancel.Click();
  end;
end;

procedure TFormDumpMemoryImage.FormShow(Sender: TObject);
begin
  self.Timer.Enabled := True;
end;

procedure TFormDumpMemoryImage.OnDebugStart();
begin
  ///
end;

procedure TFormDumpMemoryImage.OnDebugStop();
begin
  self.Close();
end;

procedure TFormDumpMemoryImage.TimerTimer(Sender: TObject);
begin
  self.RefreshFormRequirements();
end;

end.
