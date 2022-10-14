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

unit uExceptions;

interface

uses System.SysUtils;

type
  EWindowsException = class(Exception)
  private
    FLastError : Integer;
  public
    {@C}
    constructor Create(const WinAPI : String); overload;

    {@G}
    property LastError : Integer read FLastError;
  end;

  TPEExceptionKind = (
    peekInvalidDosHeader,
    peekInvalidSignature
  );

  EPortableExecutableException = class(Exception)
  public
    {@C}
    constructor Create(const AExceptionKind : TPEExceptionKind); overload;
  end;

implementation

(* EWindowsException *)

{ EWindowsException.Create }
constructor EWindowsException.Create(const WinAPI : String);
var AFormatedMessage : String;
begin
  FLastError := GetLastError();

  AFormatedMessage := Format('___%s: last_err=%d, last_err_msg="%s".', [
      WinAPI,
      FLastError,
      SysErrorMessage(FLastError)
  ]);

  ///
  inherited Create(AFormatedMessage);
end;

(* EPortableExecutableException *)

{ EPortableExecutableException.Create }
constructor EPortableExecutableException.Create(const AExceptionKind : TPEExceptionKind);
var AFormatedMessage : String;
    AReason          : String;
begin
  case AExceptionKind of
    peekInvalidDosHeader: AReason := 'Invalid Image Dos Header';
    peekInvalidSignature: AReason := 'Invalid NT Header Signature';

    else
      AReason := 'Unknown';
  end;

  AFormatedMessage := Format('Invalid PE File: "%s"', [
    AReason
  ]);

  ///
  inherited Create(AFormatedMessage);
end;

end.
