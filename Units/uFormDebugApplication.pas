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

unit uFormDebugApplication;

interface

uses
  Winapi.Windows, Winapi.Messages, System.SysUtils, System.Variants, System.Classes, Vcl.Graphics,
  Vcl.Controls, Vcl.Forms, Vcl.Dialogs, Vcl.ExtCtrls, Vcl.VirtualImage,
  Vcl.StdCtrls, Vcl.Buttons;

type
  TFormDebugApplication = class(TForm)
    PanelCore: TPanel;
    PanelForm: TPanel;
    PanelIcon: TPanel;
    ImageIcon: TVirtualImage;
    Label1: TLabel;
    EditApplication: TButtonedEdit;
    Shape1: TShape;
    PanelBottom: TPanel;
    ButtonStart: TSpeedButton;
    ButtonCancel: TSpeedButton;
    CheckBoxShowProcess: TCheckBox;
    Label2: TLabel;
    EditArguments: TEdit;
    CheckBoxDebugChild: TCheckBox;
    OpenDialog: TOpenDialog;
    PanelWarning: TPanel;
    Label4: TLabel;
    Shape2: TShape;
    IconWarning: TVirtualImage;
    procedure FormKeyUp(Sender: TObject; var Key: Word; Shift: TShiftState);
    procedure ButtonCancelClick(Sender: TObject);
    procedure ButtonStartClick(Sender: TObject);
    procedure EditApplicationRightButtonClick(Sender: TObject);
  private
    { Private declarations }
  public
    { Public declarations }
  end;

var
  FormDebugApplication: TFormDebugApplication;

implementation

uses uFormMain;

{$R *.dfm}

procedure TFormDebugApplication.FormKeyUp(Sender: TObject; var Key: Word;
  Shift: TShiftState);
begin
  case Key of
    13 : ButtonStartClick(ButtonStart);
    27 : ButtonCancelClick(ButtonCancel);
  end;
end;

procedure TFormDebugApplication.ButtonStartClick(Sender: TObject);
begin
  FormMain.DebugApplication(
      self.EditApplication.Text,
      self.EditArguments.Text,
      self.CheckBoxDebugChild.Checked,
      self.CheckBoxShowProcess.Checked
  );

  ///
  self.Close();
end;

procedure TFormDebugApplication.EditApplicationRightButtonClick(
  Sender: TObject);
begin
  if not OpenDialog.Execute() then
    Exit();
  ///

  TButtonedEdit(Sender).Text := OpenDialog.FileName;
end;

procedure TFormDebugApplication.ButtonCancelClick(Sender: TObject);
begin
  self.Close();
end;

end.
