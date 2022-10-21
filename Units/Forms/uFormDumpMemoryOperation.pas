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

unit uFormDumpMemoryOperation;

interface

uses
  Winapi.Windows, Winapi.Messages, System.SysUtils, System.Variants, System.Classes, Vcl.Graphics,
  Vcl.Controls, Vcl.Forms, Vcl.Dialogs, Vcl.ComCtrls, Vcl.ExtCtrls, Vcl.StdCtrls,
  Vcl.Buttons, Vcl.VirtualImage, Vcl.Samples.Gauges;

type
  TFormDumpMemoryOperation = class(TForm)
    PanelBottom: TPanel;
    ButtonCancel: TSpeedButton;
    PanelBackground: TPanel;
    PanelCore: TPanel;
    PanelIcon: TPanel;
    ImageIcon: TVirtualImage;
    PanelForm: TPanel;
    LabelTotal: TLabel;
    LabelCurrent: TLabel;
    ProgressCurrent: TProgressBar;
    ProgressTotal: TProgressBar;
    procedure FormKeyUp(Sender: TObject; var Key: Word; Shift: TShiftState);
    procedure ButtonCancelClick(Sender: TObject);
  private
  public
    {@M}
    procedure OnThreadStart(Sender: TObject; const ATotalTasks: Cardinal);
    procedure OnThreadStop(Sender: TObject);
    procedure OnTaskBegin(Sender: TObject; const ALabel : String; const AProgressMax: UInt64);
    procedure OnTaskEnd(Sender: TObject);
    procedure OnTaskProgress(Sender: TObject; const AProgress: UInt64);
  end;

var
  FormDumpMemoryOperation: TFormDumpMemoryOperation;

implementation

uses uFormMain;

{$R *.dfm}

procedure TFormDumpMemoryOperation.OnThreadStart(Sender: TObject; const ATotalTasks: Cardinal);
begin
  self.ProgressCurrent.Position := 0;
  self.ProgressTotal.Position := 0;

  self.ProgressTotal.Max := ATotalTasks;
  self.LabelTotal.Caption := Format('Operation 0/%d', [ATotalTasks]);
end;

procedure TFormDumpMemoryOperation.OnThreadStop(Sender: TObject);
begin
  self.ProgressCurrent.Position := 0;
  self.ProgressTotal.Position := 0;

  self.Close();
end;

procedure TFormDumpMemoryOperation.OnTaskBegin(Sender: TObject; const ALabel : String; const AProgressMax: UInt64);
begin
  self.LabelCurrent.Caption := ALabel;
  self.ProgressCurrent.Max := AProgressMax;
  self.ProgressCurrent.Position := 0;
end;

procedure TFormDumpMemoryOperation.OnTaskEnd(Sender: TObject);
begin
  self.ProgressCurrent.Position := 0;

  self.ProgressTotal.Position := self.ProgressTotal.Position + 1;
  self.LabelTotal.Caption := Format('Operation %d/%d', [self.ProgressTotal.Position, self.ProgressTotal.Max]);
end;

procedure TFormDumpMemoryOperation.OnTaskProgress(Sender: TObject; const AProgress: UInt64 );
begin
  self.ProgressCurrent.Position := AProgress;
end;

procedure TFormDumpMemoryOperation.ButtonCancelClick(Sender: TObject);
begin
  self.Close();
end;

procedure TFormDumpMemoryOperation.FormKeyUp(Sender: TObject; var Key: Word;
  Shift: TShiftState);
begin
  case Key of
    27: ButtonCancel.Click();
  end;
end;

end.
