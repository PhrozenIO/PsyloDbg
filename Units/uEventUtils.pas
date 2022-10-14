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

unit uEventUtils;

interface

uses WinApi.Windows, VCL.Graphics;

function EventKindToString(const AEventKind : Cardinal) : String;
function EventKindToColor(const AEventKind : Cardinal) : TColor;
function EventKindToImageIndex(const AEventKind : Cardinal) : Integer;

implementation

uses uConstants;

{ _.EventKindToString }
function EventKindToString(const AEventKind : Cardinal) : String;
begin
  case AEventKind of
    CREATE_PROCESS_DEBUG_EVENT : result := 'Create Process';
    CREATE_THREAD_DEBUG_EVENT  : result := 'Create Thread';
    EXCEPTION_DEBUG_EVENT      : result := 'Exception';
    EXIT_PROCESS_DEBUG_EVENT   : result := 'Exit Process';
    EXIT_THREAD_DEBUG_EVENT    : result := 'Exit Thread';
    LOAD_DLL_DEBUG_EVENT       : result := 'Load DLL';
    OUTPUT_DEBUG_STRING_EVENT  : result := 'Debug String';
    RIP_EVENT                  : result := 'RIP Event';
    UNLOAD_DLL_DEBUG_EVENT     : result := 'Unload DLL';

    else
      result := 'Unknown';
  end;
end;

{ _.EventKindToColor }
function EventKindToColor(const AEventKind : Cardinal) : TColor;
begin
  case AEventKind of
    CREATE_PROCESS_DEBUG_EVENT : result := _COLOR_1;
    CREATE_THREAD_DEBUG_EVENT  : result := _COLOR_2;
    EXCEPTION_DEBUG_EVENT      : result := _COLOR_3;
    EXIT_PROCESS_DEBUG_EVENT   : result := _COLOR_4;
    EXIT_THREAD_DEBUG_EVENT    : result := _COLOR_5;
    LOAD_DLL_DEBUG_EVENT       : result := _COLOR_6;
    OUTPUT_DEBUG_STRING_EVENT  : result := _COLOR_7;
    RIP_EVENT                  : result := _COLOR_8;
    UNLOAD_DLL_DEBUG_EVENT     : result := _COLOR_9;

    else
      result := clNone;
  end;
end;

{ _.EventKindToImageIndex }
function EventKindToImageIndex(const AEventKind : Cardinal) : Integer;
begin
  case AEventKind of
    CREATE_PROCESS_DEBUG_EVENT : result := _STATE_IMAGE_EVENT_KIND_PROCESS_CREATE;
    CREATE_THREAD_DEBUG_EVENT  : result := _STATE_IMAGE_EVENT_KIND_CREATE_THREAD;
    EXCEPTION_DEBUG_EVENT      : result := _STATE_IMAGE_EVENT_KIND_EXCEPTION;
    EXIT_PROCESS_DEBUG_EVENT   : result := _STATE_IMAGE_EVENT_KIND_EXIT_PROCESS;
    EXIT_THREAD_DEBUG_EVENT    : result := _STATE_IMAGE_EVENT_KIND_EXIT_THREAD;
    LOAD_DLL_DEBUG_EVENT       : result := _STATE_IMAGE_EVENT_KIND_LOAD_DLL;
    OUTPUT_DEBUG_STRING_EVENT  : result := _STATE_IMAGE_EVENT_KIND_OUTPUT_DEBUG_STRING;
    RIP_EVENT                  : result := _STATE_IMAGE_EVENT_KIND_RIP;
    UNLOAD_DLL_DEBUG_EVENT     : result := _STATE_IMAGE_EVENT_KIND_UNLOAD_DLL;

    else
      result := -1;
  end;
end;

end.
