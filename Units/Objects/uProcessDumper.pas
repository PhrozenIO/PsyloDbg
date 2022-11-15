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

unit uProcessDumper;

interface

uses System.Classes,
     System.SysUtils,
     Generics.Collections,
     WinApi.Windows,
     WinApi.PsAPI,
     uModuleDump;

type
  TProcessDumper = class
  private
    FProcessId     : Cardinal;
    FProcessHandle : THandle;

    {@M}
    function GetMainModule() : HMODULE;
  public
    {@C}
    constructor Create(const AProcessId : Cardinal);
    destructor Destroy(); override;

    {@M}
    function DumpMainModule() : TModuleDump;
    function DumpModule(const AModuleHandle : THandle) : TModuleDump;
    procedure DumpLoadedModules(var ADumpedModules : TObjectList<TModuleDump>);
  end;

implementation

uses uExceptions;

{ TProcessDumper.GetMainModule }
function TProcessDumper.GetMainModule() : HMODULE;
var ACBNeeded   : Cardinal;
begin
  if not EnumProcessModules(FProcessHandle, @result, SizeOf(HMODULE), ACBNeeded) then
    raise EWindowsException.Create(Format('EnumProcessModules[pid: %d]', [FProcessId]));
end;

{ TProcessDumper.DumpModule }
function TProcessDumper.DumpModule(const AModuleHandle : THandle) : TModuleDump;
begin
  result := TModuleDump.Create(FProcessHandle, AModuleHandle);
end;

{ TProcessDumper.DumpLoadedModules }
procedure TProcessDumper.DumpLoadedModules(var ADumpedModules : TObjectList<TModuleDump>);
var AModuleHandle : HMODULE;
    ACBNeeded     : Cardinal;
    AModules      : array of HMODULE;
    AModulesCount : Cardinal;
    I             : Integer;
    AModuleDump   : TModuleDump;
    hMainModule   : HMODULE;
begin
  if not Assigned(ADumpedModules) then
    ADumpedModules := TObjectList<TModuleDump>.Create(True)
  else begin
    ADumpedModules.OwnsObjects := True;

    ADumpedModules.Clear();
  end;

  hMainModule := self.GetMainModule();

  if not EnumProcessModules(FProcessHandle, nil, 0, ACBNeeded) then
    raise EWindowsException.Create(Format('EnumProcessModules(call_1)[pid: %d]', [FProcessId]));

  AModulesCount := ACBNeeded div SizeOf(HMODULE);

  SetLength(AModules, AModulesCount);

  if not EnumProcessModules(FProcessHandle, @AModules[0], ACBNeeded, ACBNeeded) then
    raise EWindowsException.Create(Format('EnumProcessModules(call_2)[pid: %d]', [FProcessId]));

  for I := 0 to AModulesCount -1 do begin
    if AModules[I] = hMainModule then
      continue;
    try
      AModuleDump := TModuleDump.Create(FProcessHandle, AModules[I]);

      ///
      ADumpedModules.Add(AModuleDump);
    except
      if Assigned(AModuleDump) then
        FreeAndNil(AModuleDump);
    end;
  end;
end;

{ TProcessDumper.DumpMainModule }
function TProcessDumper.DumpMainModule() : TModuleDump;
var hMainModule : HMODULE;
begin
  result := nil;
  ///

  hMainModule := self.GetMainModule();

  result := TModuleDump.Create(FProcessHandle, hMainModule);
end;

{ TProcessDumper.Create }
constructor TProcessDumper.Create(const AProcessId : Cardinal);
begin
  inherited Create();
  ///

  FProcessId := AProcessId;

  FProcessHandle := OpenProcess(PROCESS_QUERY_INFORMATION or PROCESS_VM_READ, False, FProcessId);
  if FProcessHandle = 0 then
    raise EWindowsException.Create(Format('OpenProcess[pid: %d]', [FProcessId]));
end;

{ TProcessDumper.Destroy }
destructor TProcessDumper.Destroy();
begin
  if FProcessHandle > 0 then
    CloseHandle(FProcessHandle);

  ///
  inherited Destroy();
end;

end.

