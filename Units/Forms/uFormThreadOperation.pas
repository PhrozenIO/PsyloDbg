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

unit uFormThreadOperation;

interface

uses
  Winapi.Windows, Winapi.Messages, System.SysUtils, System.Variants, System.Classes, Vcl.Graphics,
  Vcl.Controls, Vcl.Forms, Vcl.Dialogs, Vcl.ComCtrls, Vcl.ExtCtrls, Vcl.StdCtrls,
  Vcl.Buttons, Vcl.VirtualImage, Vcl.Samples.Gauges, uPsyloOperationThread;

type
  TFormThreadOperation = class(TForm)
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
    procedure FormClose(Sender: TObject; var Action: TCloseAction);
  private
    FOperationThread : TPsyloOperationThread;
  public
    {@C}
    constructor Create(AOwner : TComponent; const AOperationThread : TPsyloOperationThread; const ATitle : String; const AIconIndex : Integer = -1); overload;
  end;

var
  FormThreadOperation: TFormThreadOperation;

implementation

uses uFormMain, uFormPsyloThreadManager;

{$R *.dfm}

constructor TFormThreadOperation.Create(AOwner : TComponent; const AOperationThread : TPsyloOperationThread; const ATitle : String; const AIconIndex : Integer = -1);
begin
  inherited Create(AOwner);
  ///

  self.Caption                  := ATitle;
  self.ProgressCurrent.Position := 0;
  self.ProgressTotal.Position   := 0;
  self.ProgressTotal.Max        := 0;
  self.LabelTotal.Caption       := 'Operation 0/0';
  FOperationThread              := AOperationThread;

  self.ImageIcon.ImageIndex := AIconIndex;
  PanelIcon.Visible         := AIconIndex <> -1;

  ///
  FormPsyloThreadManager.AddWorker(
    AOperationThread
  );
end;

procedure TFormThreadOperation.ButtonCancelClick(Sender: TObject);
begin
  if Assigned(FOperationThread) then
    FOperationThread.Terminate;
end;

procedure TFormThreadOperation.FormClose(Sender: TObject;
  var Action: TCloseAction);
begin
  Action := TCloseAction.caFree;
end;

procedure TFormThreadOperation.FormKeyUp(Sender: TObject; var Key: Word;
  Shift: TShiftState);
begin
  case Key of
    27: ButtonCancel.Click();
  end;
end;

end.
