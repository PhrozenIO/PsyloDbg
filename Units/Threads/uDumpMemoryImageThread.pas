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

unit uDumpMemoryImageThread;

interface

uses System.Classes,
     System.SysUtils,
     Winapi.Windows,
     Generics.Collections,
     uPsyloOperationThread;

type
  TDumpMemoryImageMode = (
    dmimMainModule,
    dmimLoadedModules,
    dmimAll,
    dmimListedOnly
  );

  TDumpMemoryImageThread = class(TPsyloOperationThread)
  private
    FProcessId     : Cardinal;
    FProcessHandle : THandle;
    FMode          : TDumpMemoryImageMode;
    FDestPath      : String;
    FTargetModules : TList<HMODULE>;

    {@M}
    procedure DumpModule(const AModule : HMODULE);
  protected
    {@M}
    procedure ThreadExecute; override;
  public
    {@C}
    constructor Create(const AProcessId : Cardinal; const ADestPath : String; const AMode : TDumpMemoryImageMode = dmimAll); overload;
    destructor Destroy(); override;

    {@M}
    procedure AddTargetModule(const AModule : HMODULE);

    {@G}
    property DestPath : String read FDestPath;

    {@S}
    property Mode : TDumpMemoryImageMode write FMode;
  end;

implementation

uses uFormMain, uExceptions, uModuleDump, Winapi.PsAPI, uFunctions, uConstants;

procedure TDumpMemoryImageThread.DumpModule(const AModule : HMODULE);
var AModuleDump   : TModuleDump;
    ADumpDestFile : String;
    ArchStr       : String;
    AModuleName   : String;
begin
  AModuleDump := nil;
  try
    NotifyTaskStart(Format('Resolving "%d" module', [AModule]));
    try
     if not DirectoryExists(FDestPath) then
        Exit();
      ///

      AModuleDump := TModuleDump.Create(FProcessHandle, AModule);
      ///

      ArchStr := 'x86-';
      if AModuleDump.Is64 then
        ArchStr := ArchStr + '64'
      else
        ArchStr := ArchStr + '32';

      try
        AModuleName := DELF_GetModuleFileNameEx(FProcessHandle, AModule);
      except
        try
          AModuleName := DELF_GetMappedFileName(FProcessHandle, AModuleDump.BaseOfAddress);
        except
          AModuleName := Format('pid_%d-hmod_%d.bin', [
            FProcessId,
            AModule
          ]);
        end;
      end;

      ADumpDestFile := Format('%s%s%s', [
        IncludeTrailingPathDelimiter(FDestPath),
        IncludeTrailingPathDelimiter(ArchStr),
        ExtractFileName(AModuleName)
      ]);

      NotifyUpdateTaskLabel(ExtractFileName(AModuleName));

      ///
      AModuleDump.SaveToFile(ADumpDestFile);

      ///
      NotifyTaskProgress(100, 100);
    except
      if Assigned(AModuleDump) then
        FreeAndNil(AModuleDump);
    end;
  finally
    NotifyTaskEnd();
  end;
end;

{ TDumpMemoryImageThread.ThreadExecute }
procedure TDumpMemoryImageThread.ThreadExecute();
var AModuleHandle : HMODULE;
    ACBNeeded     : Cardinal;
    AModules      : array of HMODULE;
    AModulesCount : Cardinal;
    I             : Cardinal;
    ADumpCount    : Cardinal;
begin
  NotifyOperationStart();
  try
    case FMode of
      dmimAll,
      dmimMainModule,
      dmimLoadedModules: begin
        if not EnumProcessModules(FProcessHandle, nil, 0, ACBNeeded) then
          raise EWindowsException.Create(Format('EnumProcessModules(call_1)[pid: %d]', [FProcessId]));

        AModulesCount := ACBNeeded div SizeOf(HMODULE);

        SetLength(AModules, AModulesCount);

        if not EnumProcessModules(FProcessHandle, @AModules[0], ACBNeeded, ACBNeeded) then
          raise EWindowsException.Create(Format('EnumProcessModules(call_2)[pid: %d]', [FProcessId]));

        ADumpCount := 0;
        I := 0;
        case FMode of
          dmimAll           : ADumpCount := AModulesCount;
          dmimMainModule    : ADumpCount := 1;

          dmimLoadedModules : begin
            ADumpCount := AModulesCount -1;
            I := 1;
          end;
        end;

        NotifyTaskCount(ADumpCount);

        for I := I to ADumpCount -1 do begin
        if self.Terminated then
          break;

          ///
          self.DumpModule(AModules[I]);
        end;
      end;

      dmimListedOnly : begin
        NotifyTaskCount(FTargetModules.Count);
        ///

        for AModuleHandle in FTargetModules do begin
          if self.Terminated then
            break;

          ///
          self.DumpModule(AModuleHandle);
        end;
      end;
    end;

    ///
    SetLength(AModules, 0);
  finally
    NotifyOperationEnd();
  end;
end;

{ TDumpMemoryImageThread.AddTargetModule }
procedure TDumpMemoryImageThread.AddTargetModule(const AModule : HMODULE);
begin
  if not Assigned(FTargetModules) then
    Exit();
  ///

  FTargetModules.Add(AModule);
end;

constructor TDumpMemoryImageThread.Create(const AProcessId : Cardinal; const ADestPath : String; const AMode : TDumpMemoryImageMode = dmimAll);
begin
  inherited Create(FormMain, 'Dump Memory Module(s)', _ICON_MODULE_DUMP);
  ///

  FTargetModules := TList<HMODULE>.Create();

  FMode              := AMode;
  FDestPath          := ADestPath;
  FProcessId         := AProcessId;

  if not DirectoryExists(FDestPath) then
    raise Exception.Create(Format('Directory "%s" does not exists.', [FDestPath]));

  FProcessHandle := OpenProcess(PROCESS_QUERY_INFORMATION or PROCESS_VM_READ, False, FProcessId);
  if FProcessHandle = INVALID_HANDLE_VALUE then
    raise EWindowsException.Create(Format('OpenProcess(%d)', [FProcessId]));
end;

{ TDumpMemoryImageThread.Destroy }
destructor TDumpMemoryImageThread.Destroy();
begin
  if Assigned(FTargetModules) then
    FreeAndNil(FTargetModules);

  if FProcessHandle = INVALID_HANDLE_VALUE then
    CloseHandle(FProcessHandle);

  ///
  inherited Destroy();
end;

end.
