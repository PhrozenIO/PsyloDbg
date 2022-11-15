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

unit uFrameComponentAlert;

interface

uses
  Winapi.Windows, Winapi.Messages, System.SysUtils, System.Variants, System.Classes,
  Vcl.Graphics, Vcl.Controls, Vcl.Forms, Vcl.Dialogs, Vcl.ExtCtrls, Vcl.StdCtrls;

type
  TAlertKind = (
    akInformation
  );

  TFrameComponentAlert = class(TFrame)
    BorderShape: TShape;
    LabelMessage: TLabel;
  private
    {@M}
    procedure UpdateKind(const AKind : TAlertKind);
  public
    {@M}
    procedure ResetData();
    procedure SetMessage(const AMessage : String; const AKind : TAlertKind = akInformation; const AShowFrame : Boolean = False);
    procedure DoResize();

    {@C}
    constructor Create(AOwner : TComponent); override;
  end;

implementation

{$R *.dfm}

procedure TFrameComponentAlert.DoResize();
var ARect       : TRect;
    ACaption    : String;
    AClientRect : TRect;
begin
  LabelMessage.Canvas.Font := LabelMessage.Font;

  ACaption := LabelMessage.Caption;

  ARect := LabelMessage.ClientRect;

  LabelMessage.Canvas.TextRect(ARect, ACaption, [tfCalcRect, tfWordBreak]);

  self.ClientHeight := ARect.Height + LabelMessage.Margins.Top + LabelMessage.Margins.Bottom;
end;

constructor TFrameComponentAlert.Create(AOwner : TComponent);
begin
  inherited Create(AOwner);
  ///

end;

procedure TFrameComponentAlert.ResetData();
begin
  self.LabelMessage.Caption := '';
  self.Visible := False;
end;

procedure TFrameComponentAlert.UpdateKind(const AKind : TAlertKind);
var AColor      : TColor;
    ABackground : TColor;
    ABorder     : TColor;
begin
  AColor := clNone;
  ///

  case AKind of
    akInformation : begin
      AColor      := RGB(8, 66, 152);
      ABackground := RGB(207, 226, 255);
      ABorder     := RGB(182, 212, 254);
    end;
  end;

  if AColor <> clNone then begin
    self.BorderShape.Pen.Color   := ABorder;
    self.BorderShape.Brush.Color := ABackground;
    self.LabelMessage.Font.Color := AColor;
    self.LabelMessage.Color      := ABackground; // We don't want to use transparent option to avoid issues in some systems.
  end;
end;

procedure TFrameComponentAlert.SetMessage(const AMessage : String; const AKind : TAlertKind = akInformation; const AShowFrame : Boolean = False);
begin
  self.LabelMessage.Caption := AMessage;

  UpdateKind(AKind);

  if AShowFrame then
    self.Visible := True;
end;

end.
