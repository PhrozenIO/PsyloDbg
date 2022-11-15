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

unit uMessageListener;

interface

uses Winapi.Windows, System.Classes, Winapi.Messages, Generics.Collections;

type
  TOnMessageEvent = procedure(Sender : TObject; const AMessage : Cardinal; const AParameter : NativeUInt) of object;

  TMessageListener = class(TObject)
  private
    FMsgHandler         : THandle;
    FRegisteredMessages : TList<Cardinal>;
    FOnMessage          : TOnMessageEvent;

    {@M}
    procedure WndMethod(var AMessage : TMessage);
  public
    {@M}
    procedure Add(const AMessage : Cardinal);
    procedure Remove(const AMessage : Cardinal);
    procedure Clear();

    {@C}
    constructor Create();
    destructor Destroy(); override;

    {@G/S}
    property OnMessage : TOnMessageEvent read FOnMessage write FOnMessage;
  end;

  procedure BroadcastMessage(const AMessage : UINT; const AParam : NativeUInt = 0; const AIsolate : Boolean = True);

implementation

uses System.SysUtils;

{ TMessageListener.Add }
procedure TMessageListener.Add(const AMessage : Cardinal);
begin
  if not Assigned(FRegisteredMessages) or (AMessage = 0) then
    Exit();

  if FRegisteredMessages.Contains(AMessage) then
    Exit();

  FRegisteredMessages.Add(AMessage);
end;

{ TMessageListener.Remove }
procedure TMessageListener.Remove(const AMessage : Cardinal);
begin
  if not Assigned(FRegisteredMessages) then
    Exit();

  if not FRegisteredMessages.Contains(AMessage) then
    Exit();

  FRegisteredMessages.Remove(AMessage);
end;

{ TMessageListener.Clear }
procedure TMessageListener.Clear();
begin
  if Assigned(FRegisteredMessages) then
    FRegisteredMessages.Clear();
end;

{ TMessageListener.Create }
constructor TMessageListener.Create();
begin
  inherited Create();
  ///

  FMsgHandler := AllocateHWnd(WndMethod);
  FOnMessage  := nil;

  FRegisteredMessages := TList<Cardinal>.Create();
end;

{ TMessageListener.Destroy }
destructor TMessageListener.Destroy();
begin
  DeallocateHWnd(FMsgHandler);

  if Assigned(FRegisteredMessages) then
    FreeAndNil(FRegisteredMessages);

  ///
  inherited Destroy();
end;

{ TMessageListener.WndMethod }
procedure TMessageListener.WndMethod(var AMessage : TMessage);
var ADefault : Boolean;
begin
  ADefault := True;
  try
    if Assigned(FOnMessage) and Assigned(FRegisteredMessages) then begin
      if (FRegisteredMessages.Contains(AMessage.Msg)) then begin
        if (AMessage.wParam <> 0) and (AMessage.wParam <> GetCurrentProcessId) then
          Exit();
        ///

        FOnMessage(self, AMessage.Msg, AMessage.LParam);
      end;
    end;
  finally
    if ADefault then
      AMessage.Result := DefWindowProc(
                                        FMsgHandler,
                                        AMessage.Msg,
                                        AMessage.wParam,
                                        AMessage.lParam
      );
  end;
end;

{ _.BroadcastMessage }
procedure BroadcastMessage(const AMessage : UINT; const AParam : NativeUInt = 0; const AIsolate : Boolean = True);
var wParam : NativeUInt;
begin
  if AMessage = 0 then
    Exit();
  ///

  wParam := 0;
  if AIsolate then
    wParam := GetCurrentProcessId();

  ///
  PostMessage(HWND_BROADCAST, AMessage, wParam, AParam);
end;

end.
