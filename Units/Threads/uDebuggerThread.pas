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

unit uDebuggerThread;

interface

uses WinApi.Windows,
     WinApi.PsAPI,
     System.SysUtils,
     System.Classes;


type
  TDebuggerThread = class;

  TDebuggerStatus = (
    dbgStart,
    dbgProcessAttached,
    dbgStop
  );

  TOnDebuggerStatusChange = procedure(Sender: TDebuggerThread; const ADebuggerStatus : TDebuggerStatus) of object;

  TDebuggerThread = class(TThread)
  private
    FEventTimeout : Integer;

    FOnDebuggerStatusChange  : TOnDebuggerStatusChange;
  protected
    FProcessId : Cardinal;

    {@M}
    procedure Prologue();
    procedure MonitorEvents();
    procedure Epilogue();

    procedure Execute(); override;
    procedure Proc(); virtual; abstract;
  public
    {@C}
    constructor Create(); overload;
    destructor Destroy(); override;

    {@S}
    property OnDebuggerStatusChange : TOnDebuggerStatusChange write FOnDebuggerStatusChange;

    {@G}
    property ProcessId : Cardinal read FProcessId;
  end;

  TCreateProcessAndDebugThread = class(TDebuggerThread)
  private
    FFileName         : String;
    FProcessArguments : String;
    FDebugChild       : Boolean;
    FShowProcess      : Boolean;
  protected
    {@M}
    procedure Proc(); override;
  public
    {@C}
    constructor Create(const AFileName, AProcessArguments : String; const ADebugChild, AShowProcess : Boolean); overload;
  end;

  TAttachProcessAndDebugThread = class(TDebuggerThread)
  protected
    {@M}
    procedure Proc(); override;
  public
    {@C}
    constructor Create(const AProcessId : Cardinal); overload;
  end;

  function WaitForDebugEventEx(var lpDebugEvent: TDebugEvent; dwMilliseconds: DWORD) : BOOL; stdcall; external 'Kernel32.Dll';

implementation

uses uExceptions, uFormMain, uFunctions;

(* TDebuggerThread *)

procedure TDebuggerThread.Prologue();
begin
  if Assigned(FOnDebuggerStatusChange) then begin
    Synchronize(procedure begin
      FOnDebuggerStatusChange(self, dbgStart);
    end);
  end;
end;

procedure TDebuggerThread.Epilogue();
begin
  if Assigned(FOnDebuggerStatusChange) then begin
    Synchronize(procedure begin
      FOnDebuggerStatusChange(self, dbgStop);
    end);
  end;
end;

procedure TDebuggerThread.MonitorEvents();
var ADebugEvent : TDebugEvent;
    AResult     : Bool;
    ABreakPoint : Boolean;
begin
  if Assigned(FOnDebuggerStatusChange) then begin
    Synchronize(procedure begin
      FOnDebuggerStatusChange(self, dbgProcessAttached);
    end);
  end;
  ///

  while not Terminated do begin
    SetLastError(0);
    ///

    if TOSVersion.Major >= 10 then
      AResult := WaitForDebugEventEx(ADebugEvent, FEventTimeout) // For Unicode Support
    else
      AResult := WaitForDebugEvent(ADebugEvent, FEventTimeout);

    // Timeout Check
    if GetLastError() = 121 then
      continue;

    if not AResult then
      break;

    // Check if exception is a breakpoint.
    ABreakPoint := False;
    if ADebugEvent.dwDebugEventCode = EXCEPTION_DEBUG_EVENT then
      ABreakPoint := ADebugEvent.Exception.ExceptionRecord.ExceptionCode = EXCEPTION_BREAKPOINT;

    // We do not log breakpoint in Debug Event List to avoid event flood.
    if not ABreakPoint then
      Synchronize(procedure begin
        FormMain.DebugEvents.DisplayDebugEvent(ADebugEvent);
      end);

    if not ContinueDebugEvent(ADebugEvent.dwProcessId, ADebugEvent.dwThreadId, DBG_CONTINUE) then
      break;

    // Detect debugged process exit to interrupt debugger.
    if FProcessId = ADebugEvent.dwProcessId then begin
      case ADebugEvent.dwDebugEventCode of
        EXIT_PROCESS_DEBUG_EVENT: begin
          DebugActiveProcessStop(FProcessId); // Can be removed ?

          break;
        end;
      end;
    end;
  end;
end;

{ TDebuggerThread.Execute }
procedure TDebuggerThread.Execute();
begin
  self.Prologue();
  try
    try
      Proc();
    except
      on E : Exception do
        Synchronize(procedure begin
          FormMain.OnException(self, E);
        end);
    end;
  finally
    self.Epilogue();

    ///
    ExitThread(0); // !important
  end;
end;

{ TDebuggerThread.Create }
constructor TDebuggerThread.Create();
begin
  inherited Create(True);
  ///

  self.FreeOnTerminate := False;

  FOnDebuggerStatusChange := nil;
  FEventTimeout := 100;
  FProcessId := 0;
end;

{ TDebuggerThread.Destroy }
destructor TDebuggerThread.Destroy();
begin
  self.Terminate;

  self.WaitFor();

  ///
  inherited Destroy();
end;

(* TCreateProcessAndDebugThread *)

{ TCreateProcessAndDebugThread.Proc }
procedure TCreateProcessAndDebugThread.Proc();
var AStartupInfo   : TStartupInfo;
    AProcessInfo   : TProcessInformation;
    ACreationFlags : Cardinal;
begin
  try
    CheckIfValidApplicationForDebugging(FFileName);
  except
    on E : Exception do begin
      raise EPsyloException.Create(E.Message, True);
    end;
  end;
  ///

  ZeroMemory(@AProcessInfo, SizeOf(TProcessInformation));
  ZeroMemory(@AStartupInfo, Sizeof(TStartupInfo));

  AStartupInfo.cb          := SizeOf(TStartupInfo);
  AStartupInfo.wShowWindow := Ternary(self.FShowProcess, SW_SHOW, SW_HIDE);
  AStartupInfo.dwFlags     := (STARTF_USESHOWWINDOW);

  UniqueString(FFileName);
  UniqueString(FProcessArguments);

  ACreationFlags := CREATE_NEW_CONSOLE;
  if FDebugChild then
    ACreationFlags := ACreationFlags or DEBUG_PROCESS
  else
    ACreationFlags := ACreationFlags or DEBUG_ONLY_THIS_PROCESS;

  if not CreateProcessW(
                  nil,
                  PWideChar(FFileName),
                  nil,
                  nil,
                  False,
                  ACreationFlags,
                  nil,
                  nil,
                  AStartupInfo,
                  AProcessInfo
  ) then
    raise EWindowsException.Create('CreateProcessW', True);
  try
    FProcessId := AProcessInfo.dwProcessId;

    self.MonitorEvents();

    ///
    DebugActiveProcessStop(AProcessInfo.dwProcessId);
  finally
    TerminateProcess(AProcessInfo.hProcess, 0);
  end;
end;

{ TCreateProcessAndDebugThread.Create }
constructor TCreateProcessAndDebugThread.Create(const AFileName, AProcessArguments : String; const ADebugChild, AShowProcess : Boolean);
begin
  inherited Create();
  ///

  FFileName         := AFileName;
  FProcessArguments := AProcessArguments;
  FDebugChild       := ADebugChild;
  FShowProcess      := AShowProcess;
end;

(* TAttachProcessAndDebugThread *)

{ TAttachProcessAndDebugThread.Proc }
procedure TAttachProcessAndDebugThread.Proc();
begin
  DebugActiveProcess(FProcessId);
  try
    self.MonitorEvents();
  finally
    DebugActiveProcessStop(FProcessId);
  end;
end;

{ TAttachProcessAndDebugThread.Create }
constructor TAttachProcessAndDebugThread.Create(const AProcessId : Cardinal);
begin
  inherited Create();
  ///

  FProcessId := AProcessId;
end;

end.
