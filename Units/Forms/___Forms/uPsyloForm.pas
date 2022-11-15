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

unit uPsyloForm;

interface

uses
  Winapi.Windows, Winapi.Messages, System.SysUtils, System.Variants, System.Classes, Vcl.Graphics,
  Vcl.Controls, Vcl.Forms, Vcl.Dialogs, uMessageListener;

type
  TFormPsylo = class(TForm)
  private
    FMessageListener : TMessageListener;

    {@M}
    procedure OnReadMessage(Sender : TObject; const AMessage : Cardinal; const AParameter : NativeUInt);
  protected
    {@M}
    procedure OnDebugNewProcess(const AProcessId : Cardinal); virtual;
    procedure OnDebugExitProcess(const AProcessId : Cardinal); virtual;
    procedure OnDebugStart(); virtual;
    procedure OnDebugStop(); virtual;
  public
    {@C}
    constructor Create(AOwner : TComponent); override;
    destructor Destroy(); override;
  end;

var
  FormPsylo: TFormPsylo;

implementation

uses uMessages;

{$R *.dfm}

constructor TFormPsylo.Create(AOwner : TComponent);
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

destructor TFormPsylo.Destroy();
begin
  if Assigned(FMessageListener) then
    FreeAndNil(FMessageListener);
  ///
  inherited Destroy();
end;

procedure TFormPsylo.OnReadMessage(Sender : TObject; const AMessage : Cardinal; const AParameter : NativeUInt);
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

procedure TFormPsylo.OnDebugNewProcess(const AProcessId : Cardinal);
begin
  ///
end;

procedure TFormPsylo.OnDebugExitProcess(const AProcessId : Cardinal);
begin
  ///
end;

procedure TFormPsylo.OnDebugStart();
begin
  ///
end;

procedure TFormPsylo.OnDebugStop();
begin
  ///
end;

end.
