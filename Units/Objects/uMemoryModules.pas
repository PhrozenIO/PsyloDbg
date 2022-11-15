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

unit uMemoryModules;

interface

uses Winapi.Windows,
     System.SysUtils,
     System.Generics.Collections,
     System.Classes,
     Winapi.TlHelp32,
     uPsyloThread;

type
  TEnumModulesMethod = (
    emmToolHelp32
  );

  TMemoryModule = class(TPersistent)
  private
    FProcessId  : Cardinal;
    FHandle     : THandle;
    FBase       : NativeUInt;
    FBaseSize   : Cardinal;
    FImagePath  : String;
    FMainModule : Boolean;
  public
    {@C}
    constructor Create(const AModuleEntry : TModuleEntry32); overload;
    constructor Create(const AModule : TMemoryModule); overload;

    {@M}
    procedure Assign(ASource : TPersistent); override;

    {@G}
    property Handle     : THandle    read FHandle;
    property Base       : NativeUInt read FBase;
    property BaseSize   : Cardinal   read FBaseSize;
    property ImagePath  : String     read FImagePath;
    property ProcessId  : Cardinal   read FProcessid;
    property MainModule : Boolean    read FMainModule;
  end;

  TEnumMemoryModules = class
  private
    FOwnerThread : TPsyloThread;
    FProcessId   : Cardinal;
    FModules     : TObjectList<TMemoryModule>;

    {@M}
    procedure EnumModules_ToolHelp32();
  public
    {@C}
    constructor Create(const AProcessId : Cardinal; const AOwnerThread : TPsyloThread = nil);
    destructor Destroy(); override;

    {@M}
    procedure Refresh(const AMode : TEnumModulesMethod = emmToolHelp32);

    {@G}
    property Modules : TObjectList<TMemoryModule> read FModules;
  end;

implementation

uses uExceptions, uFunctions;

(* TMemoryModule *)

{ TMemoryModule.Create }
constructor TMemoryModule.Create(const AModuleEntry : TModuleEntry32);
begin
  inherited Create();
  ///

  FHandle     := AModuleEntry.hModule;
  FBase       := NativeUInt(AModuleEntry.modBaseAddr);
  FBaseSize   := AModuleEntry.modBaseSize;
  FProcessId  := AModuleEntry.th32ProcessID;
  FImagePath  := AModuleEntry.szExePath;

  if FImagePath.Trim.IsEmpty then begin
    FImagePath := GetModuleName(FHandle);
    if FImagePath.IsEmpty then begin
      try
        FImagePath := DELF_GetMappedFileName(FProcessId, Pointer(FBase));
      except
        FImagePath := 'Unknown';
      end;
    end;
  end;

  try
    FMainModule := GetProcessMainModule(FProcessId) = FBase;    
  except
    FMainModule := False;
  end;
end;

constructor TMemoryModule.Create(const AModule : TMemoryModule);
begin
  inherited Create();
  ///

  self.Assign(AModule);
end;

{ TMemoryModule.Assign }
procedure TMemoryModule.Assign(ASource : TPersistent);
begin
  if ASource is TMemoryModule then begin
    FHandle     := TMemoryModule(ASource).Handle;
    FBase       := TMemoryModule(ASource).Base;
    FBaseSize   := TMemoryModule(ASource).BaseSize;
    FImagePath  := TMemoryModule(ASource).ImagePath;
    FProcessId  := TMemoryModule(ASource).ProcessId;
    FMainModule := TMemoryModule(ASource).MainModule;
  end else
    inherited Assign(ASource);
end;

(* TEnumMemoryModules *)

{ TEnumMemoryModules.Create }
constructor TEnumMemoryModules.Create(const AProcessId : Cardinal; const AOwnerThread : TPsyloThread = nil);
begin
  inherited Create();
  ///

  FProcessId   := AProcessId;
  FOwnerThread := AOwnerThread;
  FModules     := TObjectList<TMemoryModule>.Create(True);
end;

{ TEnumMemoryModules.Destroy }
destructor TEnumMemoryModules.Destroy();
begin
  if Assigned(FModules) then
    FreeAndNil(FModules);

  ///
  inherited Destroy();
end;

{ TEnumMemoryModules.EnumModules_ToolHelp32 }
procedure TEnumMemoryModules.EnumModules_ToolHelp32();
var hSnapshot    : THandle;
    AModuleEntry : TModuleEntry32;
    AFirst       : Boolean;

    function Next() : Boolean;
    begin
      result := False;
      ///

      ZeroMemory(@AModuleEntry, SizeOf(TModuleEntry32));
      AModuleEntry.dwSize := SizeOf(TModuleEntry32);
      ///

      if AFirst then begin
        if not Module32First(hSnapshot, AModuleEntry) then
          raise EWindowsException.Create('Module32First');
        ///

        result := True;

        ///
        AFirst := False;
      end else
        result := Module32Next(hSnapshot, AModuleEntry);

      if result then
        FModules.Add(TMemoryModule.Create(AModuleEntry));
    end;

const TH32CS_SNAPMODULE32 = $00000010;
begin
  hSnapshot := CreateToolHelp32Snapshot(TH32CS_SNAPMODULE or TH32CS_SNAPMODULE32, FProcessId);
  if hSnapshot = INVALID_HANDLE_VALUE then
    raise EWindowsException.Create('CreateToolHelp32Snapshot');
  try
    AFirst := True;
    repeat
      if Assigned(FOwnerThread) then
        if FOwnerThread.IsTerminated then
          break;
    until (not Next());
  finally
    CloseHandle(hSnapshot);
  end;
end;

{ TEnumMemoryModules.Refresh }
procedure TEnumMemoryModules.Refresh(const AMode : TEnumModulesMethod = emmToolHelp32);
begin
  FModules.Clear();
  ///

  case AMode of
    emmToolHelp32: EnumModules_ToolHelp32();
  end;
end;

end.
