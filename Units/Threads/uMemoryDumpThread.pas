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

unit uMemoryDumpThread;

interface

uses
  Winapi.Windows,
  System.SysUtils,
  System.Classes,
  System.Generics.Collections,
  uPsyloOperationThread;

type
  TMemoryDumpTask = class
  private
    FStartOffset    : NativeUInt;
    FContentSize    : UInt64;
    FDestFileHandle : THandle;

    {@M}
    function IsValidHandle() : Boolean;
    function GetEndOffset() : NativeUInt;
  public
    {@C}
    constructor Create(const ADestFile : String; const AStartOffset : NativeUInt; const AContentSize : UInt64);
    destructor Destroy(); override;

    {@M}
    procedure CloseFile();

    {@G}
    property StartOffset    : NativeUInt read FStartOffset;
    property ContentSize    : UInt64     read FContentSize;
    property DestFileHandle : THandle    read FDestFileHandle;
    property ValidHandle    : Boolean    read IsValidHandle;
    property EndOffset      : NativeUInt read GetEndOffset;
  end;

  TMemoryDumpThread = class(TPsyloOperationThread)
  private
    FProcessId      : Cardinal;
    FProcessHandle  : THandle;
    FChunkSize      : Cardinal;
    FTasks          : TObjectList<TMemoryDumpTask>;

    {@M}
    function DumpMemory(const AMemoryDumpTask: TMemoryDumpTask) : Boolean;
  protected
    {@M}
    procedure ThreadExecute; override;
  public
    {@C}
    constructor Create(const AProcessId : Cardinal); overload;
    destructor Destroy(); override;

    {@G/S}
    property Tasks : TObjectList<TMemoryDumpTask> read FTasks write FTasks;
  end;

implementation

uses uFormMain, uExceptions, uConstants;

(* TMemoryDumpTask Class *)

{ TMemoryDumpTask.Create }
constructor TMemoryDumpTask.Create(const ADestFile : String; const AStartOffset : NativeUInt; const AContentSize : UInt64);
begin
  inherited Create();
  ///

  FStartOffset    := AStartOffset;
  FContentSize    := AContentSize;

  FDestFileHandle := CreateFileW(
      PWideChar(ADestFile),
      GENERIC_WRITE,
      FILE_SHARE_READ,
      nil,
      CREATE_ALWAYS,
      FILE_ATTRIBUTE_NORMAL,
      0
  );
  if FDestFileHandle = INVALID_HANDLE_VALUE then
    raise EWindowsException.Create(Format('CreateFileW(%s)', [ADestFile]));
end;

{ TMemoryDumpTask.Destroy }
destructor TMemoryDumpTask.Destroy();
begin
  self.CloseFile();

  ///
  inherited Destroy();
end;

{ TMemoryDumpTask.CloseFile }
procedure TMemoryDumpTask.CloseFile();
begin
  if self.IsValidHandle() then
    CloseHandle(FDestFileHandle);

  ///
  FDestFileHandle := INVALID_HANDLE_VALUE;
end;

{ TMemoryDumpTask.IsValidHandle }
function TMemoryDumpTask.IsValidHandle() : Boolean;
begin
  result := FDestFileHandle <> INVALID_HANDLE_VALUE;
end;

{ TMemoryDumpTask.GetEndOffset }
function TMemoryDumpTask.GetEndOffset() : NativeUInt;
begin
  result := FStartOffset + FContentSize;
end;

(* TMemoryDumpThread Class *)

{ TMemoryDumpThread.DumpMemory }
function TMemoryDumpThread.DumpMemory(const AMemoryDumpTask: TMemoryDumpTask) : Boolean;
var pBuffer       : Pointer;
    ATotalRead    : UInt64;
    ACursor       : UInt64;
    AReadSize     : Cardinal;
    ABytesWritten : Cardinal;
    ABytesRead    : SIZE_T;
    b             : Boolean;
    AException    : Exception;
begin
  result := False;
  ///

  if not Assigned(AMemoryDumpTask) or
     not AMemoryDumpTask.ValidHandle
   then
    Exit();

  GetMem(pBuffer, FChunkSize);
  try
    ATotalRead := 0;
    ACursor    := 0;
    repeat
      ZeroMemory(pBuffer, FChunkSize);
      ///

      if AMemoryDumpTask.ContentSize - ACursor < FChunkSize  then
        AReadSize := AMemoryDumpTask.ContentSize - ACursor
      else
        AReadSize := FChunkSize;

      // Read Memory
      if not ReadProcessMemory(
          FProcessHandle,
          Pointer(AMemoryDumpTask.StartOffset + ACursor),
          pBuffer,
          AReadSize,
          ABytesRead
      )
      then
        ABytesRead := 0; // Just to be sure

      // Write to File
      if not WriteFile(AMemoryDumpTask.DestFileHandle, PByte(pBuffer)^, AReadSize, ABytesWritten, nil) then
        Exit();

      NotifyTaskProgress(AMemoryDumpTask.ContentSize, ACursor);

      ///
      Inc(ATotalRead, ABytesRead);
      Inc(ACursor, AReadSize);
    until (ACursor >= AMemoryDumpTask.ContentSize) or Terminated;

    ///
    result := ATotalRead = AMemoryDumpTask.FContentSize;
  finally
    FreeMem(pBuffer, FChunkSize);

    ///
    AMemoryDumpTask.CloseFile();
  end;
end;

{ TMemoryDumpThread.ThreadExecute }
procedure TMemoryDumpThread.ThreadExecute;
var ATask : TMemoryDumpTask;
begin
  NotifyOperationStart();
  try
    NotifyTaskCount(FTasks.Count);
    ///

    for ATask in FTasks do begin
      if Terminated then
        break;
      ///

      NotifyTaskStart(
        Format('%p -> %p', [
          Pointer(ATask.StartOffset),
          Pointer(ATask.EndOffset)
        ])
      );

      try
        if not self.DumpMemory(ATask) then
          Synchronize(procedure begin
            FormMain.OnException(self, EPsyloException.Create(
              Format('Error encoutered during %p -> %p region dump for process %d.', [
                Pointer(ATask.StartOffset),
                Pointer(ATask.EndOffset),
                FProcessId
              ]),
            False));
          end);
      finally
        NotifyTaskEnd();
      end;
    end;
  finally
    NotifyOperationEnd();
  end;
end;

{ TMemoryDumpThread.Create }
constructor TMemoryDumpThread.Create(const AProcessId : Cardinal);
begin
  inherited Create(FormMain, 'Dump Memory', _ICON_MEMORY_DUMP);
  ///

  FProcessId      := AProcessId;
  FChunkSize      := 1024 * 8;

  FTasks := TObjectList<TMemoryDumpTask>.Create(True);

  FProcessHandle := OpenProcess(PROCESS_VM_READ, False, FProcessId);
  if FProcessHandle = INVALID_HANDLE_VALUE then
    raise EWindowsException.Create(Format('OpenProcess(%d)', [FProcessId]));
end;

{ TMemoryDumpThread.Destroy }
destructor TMemoryDumpThread.Destroy();
begin
  if FProcessHandle = INVALID_HANDLE_VALUE then
    CloseHandle(FProcessHandle);

  if Assigned(FTasks) then
    FreeAndNil(FTasks);

  ///
  inherited Destroy();
end;

end.
