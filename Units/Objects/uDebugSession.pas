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

unit uDebugSession;

interface

uses uTypes,
     System.SysUtils,
     System.Classes,
     System.Generics.Collections;

type
  TDebugProcess = class
  private
    FProcessId : TProcessId;
    FActive    : Boolean;
    FImagePath : String;
  public
    {@C}
    constructor Create(const AProcessId : Cardinal; const AImagePath : String);

    {@G}
    property ProcessId : TProcessId read FProcessId;
    property ImagePath : String     read FImagePath;

    {@G/S}
    property Active : Boolean read FActive write FActive;
  end;

  TDebugProcessEvent = procedure(Sender : TObject; const ADebugProcess : TDebugProcess) of object;

  TDebugSessionInformation = class
  private
    FProcessId : Cardinal;

    // Debug Process Tree: TObjectDictionary would seems more suitable but in reality no!
    // We must keep track of terminated process id's that could be the same as new
    // spawned process. Even if it is really unlikely to happen, it could happend.
    FProcesses            : TObjectList<TDebugProcess>;
    FOnCreateProcess      : TDebugProcessEvent;
    FOnExitProcess        : TDebugProcessEvent;
  public
    {@C}
    constructor Create(const AProcessId: Cardinal);
    destructor Destroy(); override;

    {@M}
    procedure AddProcess(const AProcessId : TProcessId; const AImagePath : String);
    function GetProcess(const AProcessId : TProcessId) : TDebugProcess;
    procedure RemoveProcess(const AProcessId : TProcessId);

    {@G}
    property ProcessId : Cardinal                   read FProcessId;
    property Processes : TObjectList<TDebugProcess> read FProcesses;

    {@G/S}
    property OnCreateProcess : TDebugProcessEvent read FOnCreateProcess write FOnCreateProcess;
    property OnExitProcess   : TDebugProcessEvent read FOnExitProcess   write FOnExitProcess;
  end;

implementation

uses uFunctions, uMessageListener, uConstants, uMessages;

(* TDebugProcess *)

{ TDebugProcess.Create }
constructor TDebugProcess.Create(const AProcessId : Cardinal; const AImagePath : String);
begin
  inherited Create();
  ///

  FProcessId := AProcessId;
  FActive    := True;
  FImagePath := AImagePath;
end;

(* TDebugSessionInformation *)

{ TDebugSessionInformation.Create }
constructor TDebugSessionInformation.Create(const AProcessId: Cardinal);
begin
  inherited Create();
  ///

  FProcessId := AProcessId;

  FProcesses := TObjectList<TDebugProcess>.Create(True);

  FOnCreateProcess := nil;
  FOnExitProcess   := nil;

  ///
  BroadcastMessage(PSYLO_DEBUG_START);
end;

{ TDebugSessionInformation.Destroy }
destructor TDebugSessionInformation.Destroy();
var AList    : TList<TDebugProcess>;
    AProcess : TDebugProcess;
begin
  if Assigned(FProcesses) then
    FreeAndNil(FProcesses);

  BroadcastMessage(PSYLO_DEBUG_STOP);

  ///
  inherited Destroy();
end;

{ TDebugSessionInformation.Destroy }
procedure TDebugSessionInformation.AddProcess(const AProcessId : TProcessId; const AImagePath : String);
var AProcess : TDebugProcess;
begin
  if Assigned(GetProcess(AProcessId)) then
    Exit();
  ///

  AProcess := TDebugProcess.Create(AProcessId, AImagePath);

  FProcesses.Add(AProcess);

  if Assigned(FOnCreateProcess) then
    FOnCreateProcess(self, AProcess);

  ///
  BroadcastMessage(PSYLO_DEBUG_NEW_PROCESS, AProcessId);
end;

{ TDebugSessionInformation.GetProcess }
function TDebugSessionInformation.GetProcess(const AProcessId : TProcessId) : TDebugProcess;
var AProcess : TDebugProcess;
begin
  result := nil;
  ///

  for AProcess in FProcesses do begin
    if AProcess.ProcessId = AProcessId then begin
      result := AProcess;

      ///
      break;
    end;
  end;
end;

{ TDebugSessionInformation.RemoveProcess }
procedure TDebugSessionInformation.RemoveProcess(const AProcessId : TProcessId);
var AProcess : TDebugProcess;
begin
  AProcess := GetProcess(AProcessId);
  ///

  if not Assigned(AProcess) then
    Exit();

  ///
  if Assigned(FOnExitProcess) then
    FOnExitProcess(self, AProcess);

  ///
  FProcesses.Remove(AProcess);

  ///
  BroadcastMessage(PSYLO_DEBUG_EXIT_PROCESS, AProcessId);
end;

end.
