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

unit uMessages;

interface

uses Winapi.Messages,
     Winapi.Windows;

const PSYLO_GLOBAL_NAME_PREFIX = 'PsyloDbgMsg';

var
  PSYLO_DEBUG_NEW_PROCESS  : UINT = 0;
  PSYLO_DEBUG_EXIT_PROCESS : UINT = 0;
  PSYLO_DEBUG_START        : UINT = 0;
  PSYLO_DEBUG_STOP         : UINT = 0;

implementation

{ _.RegisterGloablMessage }
function RegisterGlobalMessage(const AName : String) : UINT;
var AMessageName : String;
begin
  AMessageName := PSYLO_GLOBAL_NAME_PREFIX + AName;

  result := RegisterWindowMessage(PWideChar(AMessageName));
end;

initialization
  PSYLO_DEBUG_NEW_PROCESS  := RegisterGlobalMessage('DebugNewProcess');
  PSYLO_DEBUG_EXIT_PROCESS := RegisterGlobalMessage('DebugExitProcess');
  PSYLO_DEBUG_START        := RegisterGlobalMessage('DebugStart');
  PSYLO_DEBUG_STOP         := RegisterGlobalMessage('DebugStop');

end.
