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

unit uPsyloFrame;

interface

uses
  Winapi.Windows, Winapi.Messages, System.SysUtils, System.Variants, System.Classes,
  Vcl.Graphics, Vcl.Controls, Vcl.Forms, Vcl.Dialogs, uMessageListener;

type
  TPsyloFrame = class(TFrame)
  private
    FMessageListener : TMessageListener;

    {@M}
    procedure OnReadMessage(Sender : TObject; const AMessage : Cardinal; const AParameter : NativeUInt);
  protected
    {@M}
    procedure Resize; override;
    ///
    procedure CMEnabledChanged(var AMessage: TMessage); message CM_ENABLEDCHANGED;
    procedure CMVisibleChanged(var AMessage: TMessage); message CM_VISIBLECHANGED;
    procedure DoResize(); virtual;
    procedure OnShow(); virtual;
    procedure OnHide(); virtual;
    procedure OnEnabled(); virtual;
    procedure OnDisabled(); virtual;
    procedure OnDebugNewProcess(const AProcessId : Cardinal); virtual;
    procedure OnDebugExitProcess(const AProcessId : Cardinal); virtual;
    procedure OnDebugStart(); virtual;
    procedure OnDebugStop(); virtual;
  public
    {@C}
    constructor Create(AOwner : TComponent); override;
    destructor Destroy(); override;
  end;
implementation

uses uMessages;

{$R *.dfm}

procedure TPsyloFrame.Resize();
begin
  inherited;
  ///
  self.DoResize();
end;

constructor TPsyloFrame.Create(AOwner : TComponent);
begin
  inherited Create(AOwner);
  ///
  self.DoubleBuffered := True;

  FMessageListener := TMessageListener.Create();
  FMessageListener.OnMessage := OnReadMessage;
  FMessageListener.Add(PSYLO_DEBUG_NEW_PROCESS);
  FMessageListener.Add(PSYLO_DEBUG_EXIT_PROCESS);
  FMessageListener.Add(PSYLO_DEBUG_START);
  FMessageListener.Add(PSYLO_DEBUG_STOP);
end;

destructor TPsyloFrame.Destroy();
begin
  if Assigned(FMessageListener) then
    FreeAndNil(FMessageListener);
  ///
  inherited Destroy();
end;

procedure TPsyloFrame.OnReadMessage(Sender : TObject; const AMessage : Cardinal; const AParameter : NativeUInt);
begin
  if AMessage = PSYLO_DEBUG_NEW_PROCESS then begin
    self.OnDebugNewProcess(AParameter);
  end
  else if AMessage = PSYLO_DEBUG_EXIT_PROCESS then begin
    self.OnDebugExitProcess(AParameter);
  end
  else if AMessage = PSYLO_DEBUG_START then begin
    self.OnDebugStart();
  end
  else if AMessage = PSYLO_DEBUG_STOP then begin
    self.OnDebugStop();
  end;
end;

procedure TPsyloFrame.CMEnabledChanged(var AMessage: TMessage);
begin
  inherited;
  ///
  if self.Enabled then
    self.OnEnabled()
  else
    self.OnDisabled();
end;

procedure TPsyloFrame.CMVisibleChanged(var AMessage: TMessage);
begin
  inherited;
  ///
  if self.Visible then begin
    self.OnShow();
    self.DoResize();
  end else
    self.OnHide();
end;

procedure TPsyloFrame.DoResize();
begin
  ///
end;

procedure TPsyloFrame.OnShow();
begin
  ///
end;

procedure TPsyloFrame.OnHide();
begin
  ///
end;

procedure TPsyloFrame.OnEnabled();
begin
  ///
end;

procedure TPsyloFrame.OnDisabled();
begin
  ///
end;

procedure TPsyloFrame.OnDebugNewProcess(const AProcessId : Cardinal);
begin
  ///
end;

procedure TPsyloFrame.OnDebugExitProcess(const AProcessId : Cardinal);
begin
  ///
end;

procedure TPsyloFrame.OnDebugStart();
begin
  ///
end;

procedure TPsyloFrame.OnDebugStop();
begin
  ///
end;

end.
