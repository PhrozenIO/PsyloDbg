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

unit uDialogs;

interface

procedure Dialog_SuccessStoredData(const APath : String);
procedure Dialog_ActionError(const AError : String);

implementation

uses uStrings, uFunctions, VCL.Dialogs, System.SysUtils, Winapi.Windows;

{ _.Dialog_SuccessStoredData }
procedure Dialog_SuccessStoredData(const APath : String);
begin
  if MessageDlg(Format(STR_ACTION_SUCCESS_N_FSTORED, [APath]), mtConfirmation, [mbYes, mbNo], 0) = ID_NO then
    Exit();
  ///

  Open(APath);
end;

{ _.Dialog_ActionError }
procedure Dialog_ActionError(const AError : String);
begin
  MessageDlg(Format(STR_ACTION_ERROR, [AError]), mtError, [mbOK], 0)
end;

end.
