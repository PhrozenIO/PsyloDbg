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

unit uFunctions;

interface

uses Winapi.Windows,
     Winapi.ShellAPI,
     Winapi.PsAPI,
     Winapi.TlHelp32,
     System.Classes,
     VCL.Controls,
     System.Math,
     System.IOUtils,
     System.SysUtils;

type
  TArchitecture = (
      archUnknown,
      arch32,
      arch64
  );

function BufferToHexView(ABuffer : PVOID; ABufferSize : Int64; pLastOffset : PNativeUINT = nil; AStartOffset : NativeUINT = 0) : String;
function DELF_GetModuleFileNameEx(const hProcess, hModule : THandle) : String;
function GetImagePathFromProcessId(const AProcessID : Cardinal) : String;
procedure InitializeSystemIcons(var AImages : TImageList; var AFileInfo : TSHFileInfo; ALargeIcon : Boolean = False);
function SystemFileIcon(const AFileName : string; const AExtensionMode : Boolean = False) : Integer;
function GetWindowsDirectory() : string;
function GetParentProcessIdByProcessId(AProcessId : Integer) : Cardinal;
function StringToHex(const AString : String) : String;
procedure NTSetPrivilege(const APrivilegeName: string; const AEnabled: Boolean);
function Ternary(const ACondition : Boolean; const APositiveResult, ANegativeResult : Integer) : Integer;
procedure CheckIfValidApplicationForDebugging(const AFilePath : String);
function GetPEFileArchitecture(AFilePath : String) : TArchitecture;
function SearchPath_DELF(const AFilePath : String) : String;
function GetProcessArchitecture(const AProcessId : Cardinal) : TArchitecture;
function IsProcessRunningSameArchitecture(const AProcessId : Cardinal) : Boolean;
function IsProcessElevatedById(const AProcessId : Cardinal) : Boolean;
function IsProcessElevated(const hProcess : THandle) : Boolean;
function IsCurrentProcessElevated() : Boolean;
function DELF_GetMappedFileName(const hProcess : THandle; pOffset : Pointer) : String;
function PhysicalToVirtualPath(APath : String) : String;
function FormatSize(const ASize : Int64) : string;

function SearchPathW(
    lpPath,
    lpFileName,
    lpExtension: LPCWSTR;
    nBufferLength: DWORD;
    lpBuffer: LPWSTR;
    lpFilePart: LPWSTR
): DWORD; stdcall; external 'Kernel32.dll';

const PROCESS_QUERY_LIMITED_INFORMATION = $1000;

implementation

uses uExceptions;

{ _.FormatSize }
function FormatSize(const ASize : Int64) : string;
const AByteDescription : Array[0..8] of string = ('Bytes', 'KiB', 'MB', 'GiB', 'TB', 'PB', 'EB', 'ZB', 'YB');

var ACount : Integer;
begin
  ACount := 0;

  while ASize > Power(1024, ACount +1) do
    Inc(ACount);

  result := Format('%s %s', [FormatFloat('###0.00', ASize / Power(1024, ACount)), AByteDescription[ACount]]);
end;


{ _.PhysicalToVirtualPath }
function PhysicalToVirtualPath(APath : String) : String;
var i          : integer;
    ADrive     : String;
    ABuffer    : array[0..MAX_PATH-1] of Char;
    ACandidate : String;
begin
  result := '';
  {$I-}
  try
    for I := 0 to 25 do begin
      ADrive := Format('%s:', [Chr(Ord('A') + i)]);
      ///

      if (QueryDosDevice(PWideChar(ADrive), ABuffer, MAX_PATH) = 0) then
        continue;

      ACandidate := String(ABuffer).ToLower();

      if String(Copy(APath, 1, Length(ACandidate))).ToLower() = ACandidate then begin
        Delete(APath, 1, Length(ACandidate));

        result := Format('%s%s', [ADrive, APath]);
      end;
    end;
  except
    result := '';
  end;
  {$I+}
end;

{ _.Ternary }
function Ternary(const ACondition : Boolean; const APositiveResult, ANegativeResult : Integer) : Integer;
begin
  if ACondition then
    result := APositiveResult
  else
    result := ANegativeResult;
end;

{ _.NTSetPrivilege }
procedure NTSetPrivilege(const APrivilegeName: string; const AEnabled: Boolean);
var AProcessToken   : THandle;
    ATokenPrivilege : TOKEN_PRIVILEGES;
begin
  if not OpenProcessToken(GetCurrentProcess(), TOKEN_ADJUST_PRIVILEGES or TOKEN_QUERY, AProcessToken) then
    raise EWindowsException.Create('OpenProcessToken');

  try
    if not LookupPrivilegeValue(nil, PChar(APrivilegeName), ATokenPrivilege.Privileges[0].Luid) then
      raise EWindowsException.Create('LookupPrivilegeValue');

    ATokenPrivilege.PrivilegeCount := 1;

    case AEnabled of
      True  : ATokenPrivilege.Privileges[0].Attributes  := SE_PRIVILEGE_ENABLED;
      False : ATokenPrivilege.Privileges[0].Attributes  := 0;
    end;

    if not AdjustTokenPrivileges(
                                  AProcessToken,
                                  False,
                                  ATokenPrivilege,
                                  SizeOf(TOKEN_PRIVILEGES),
                                  PTokenPrivileges(nil)^,
                                  PDWORD(nil)^
    ) then
      raise EWindowsException.Create('AdjustTokenPrivileges');
  finally
    CloseHandle(AProcessToken);
  end;
end;

{ _.StringToHex }
function StringToHex(const AString : String) : String;
var AEncodedBytes : array of byte;
begin
  SetLength(AEncodedBytes, Length(AString) * 2 * SizeOf(WideChar));
  try
    BinToHex(PWideChar(AString), PWideChar(AEncodedBytes), Length(AEncodedBytes));

    ///
    SetString(result, PWideChar(AEncodedBytes), Length(AEncodedBytes));
  finally
    SetLength(AEncodedBytes, 0);
  end;
end;

{ _.GetImagePathFromProcessId }
function GetImagePathFromProcessId(const AProcessID : Cardinal) : String;
var hProcess    : THandle;
    ACBNeeded   : Cardinal;
    hMainModule : THandle;
begin
  result := '';

  hProcess := OpenProcess(PROCESS_QUERY_INFORMATION or PROCESS_VM_READ, False, AProcessId);
  if hProcess = 0 then
    raise EWindowsException.Create(Format('OpenProcess[pid: %d]', [AProcessId]));
  try
    if not EnumProcessModules(hProcess, @hMainModule, SizeOf(HMODULE), ACBNeeded) then
      raise EWindowsException.Create(Format('EnumProcessModules[pid: %d]', [AProcessId]));
    ///

    result := DELF_GetModuleFileNameEx(hProcess, hMainModule);
  finally
    CloseHandle(hProcess);
  end;
end;

{ _.DELF_GetModuleFileNameEx }
function DELF_GetModuleFileNameEx(const hProcess, hModule : THandle) : String;
var ALength         : Cardinal;
    AReturnedLength : Cardinal;
begin
  result := '';
  ///

  ALength := MAX_PATH * 2;

  SetLength(result, ALength);
  try
    AReturnedLength := GetModuleFileNameExW(hProcess, hModule, PWideChar(result), ALength);
    if AReturnedLength = 0 then
      raise EWindowsException.Create('GetModuleFileNameExW');
  finally
    SetLength(result, AReturnedLength);
  end;
end;

{ _.GetWindowsDirectory }
function GetWindowsDirectory() : string;
var ALen  : Cardinal;
begin
  SetLength(result, MAX_PATH);

  ALen := WinAPI.Windows.GetWindowsDirectory(@result[1], MAX_PATH);

  SetLength(result, ALen);
  if ALen > MAX_PATH then
    WinAPI.Windows.GetWindowsDirectory(@result[1], ALen);

  ///
  result := IncludeTrailingPathDelimiter(result);
end;

{ _.InitializeSystemIcons }
procedure InitializeSystemIcons(var AImages : TImageList; var AFileInfo : TSHFileInfo; ALargeIcon : Boolean = False);
var AFlags : Integer;
begin
  ZeroMemory(@AFileInfo, SizeOf(TSHFileInfo));
  ///

  if ALargeIcon then
    AFlags := SHGFI_LARGEICON
  else
    AFlags := SHGFI_SMALLICON;

  AImages.Handle := SHGetFileInfo(
                                    PChar(TPath.GetPathRoot(GetWindowsDirectory())),
                                    0,
                                    AFileInfo,
                                    SizeOf(AFileInfo),
                                    AFlags or (SHGFI_SYSICONINDEX)
  );
end;

{ _.SystemFileIcon }
function SystemFileIcon(const AFileName : string; const AExtensionMode : Boolean = False) : Integer;
var AFileInfo : TSHFileInfo;
    AFlags    : Integer;
begin
  ZeroMemory(@AFileInfo, sizeof(AFileInfo));
  ///

  AFlags := SHGFI_SMALLICON or SHGFI_SYSICONINDEX;
  if AExtensionMode then
    AFlags := AFlags or SHGFI_USEFILEATTRIBUTES;

  SHGetFileInfo(PWideChar(AFileName), 0, AFileInfo, SizeOf(AFileInfo), AFlags);

  Result := AFileInfo.iIcon;
end;

{ _.BufferToHexView }
function BufferToHexView(ABuffer : PVOID; ABufferSize : Int64; pLastOffset : PNativeUINT = nil; AStartOffset : NativeUINT = 0) : String;
var ARows     : DWORD;
    i, n      : integer;
    AVal      : Byte;
    sBuilder  : TStringBuilder;
    HexVal    : array[0..16-1] of TVarRec;
    AsciiVal  : array[0..16-1] of TVarRec;
    HexMask   : String; {%x}
    AsciiMask : String; {%s}

begin
  result := '';

  ///
  ARows := ceil(ABufferSize / 16);

  sBuilder := TStringBuilder.Create();
  try
    {
      Row
    }
    for I := 0 to ARows -1 do begin
      {
        Col
      }
      for n := 0 to 16-1 do begin
        AVal := PByte(NativeUInt(ABuffer) + (I * 16) + n)^;

        HexVal[n].VType    := vtInteger;
        HexVal[n].VInteger := AVal;

        AsciiVal[n].VType := vtChar;
        if AVal in [32..255] then begin
          AsciiVal[n].VChar := AnsiChar(AVal);
        end else begin
          AsciiVal[n].VChar := '.';
        end;
      end;

      HexMask   := '';
      AsciiMask := '';
      for n := 0 to 16-1 do begin
        if ((I * 16) + n) > ABufferSize then begin
          HexMask   := HexMask   + #32#32#32;
          AsciiMask := AsciiMask + #32#32;

          continue;
        end;

        HexMask   := HexMask + '%.2x' + #32;
        AsciiMask := AsciiMask + '%s';
      end;
      Delete(HexMask, length(HexMask), 1);

      {
        Draw
      }
      sBuilder.AppendLine(
          Format('%.8x', [AStartOffset + (I * 16)]) + '|' +
          Format(HexMask, HexVal) + '|' +
          Format(AsciiMask, AsciiVal)
      );
    end;
  finally
    result := sBuilder.ToString();

    if Assigned(pLastOffset) then begin
      pLastOffset^ := (ARows * 16);
    end;

    sBuilder.Free;
  end;
end;

{ _.GetParentProcessIdByProcessId }
function GetParentProcessIdByProcessId(AProcessId : Integer) : Cardinal;
var AProcHandle   : THandle;
    AProcessEntry : TProcessEntry32;
begin
  result := 0;
  ///

  ZeroMemory(@AProcessEntry, sizeOf(TProcessEntry32));
  AProcessEntry.dwSize := SizeOf(AProcessEntry);
  AProcHandle := CreateToolhelp32Snapshot(TH32CS_SNAPPROCESS, 0);
  try
    if NOT Process32First(AProcHandle, AProcessEntry) then
      Exit();

    if (AProcessEntry.th32ProcessID = AProcessId) then begin
      result := AProcessEntry.th32ParentProcessID;

      Exit();
    end;

    while Process32Next(AProcHandle, AProcessEntry) do begin
      if (AProcessEntry.th32ProcessID = AProcessId) then begin
        result := AProcessEntry.th32ParentProcessID;

        Break;
      end;
    end;
  finally
    CloseHandle(AProcHandle);
  end;
end;

{ _.GetPEFileArchitecture }
function GetPEFileArchitecture(AFilePath : String) : TArchitecture;
var hFile                   : THandle;
    AImageDosHeader         : TImageDosHeader;
    dwBytesRead             : DWORD;
    AImageFileHeader        : TImageFileHeader;
    AImageNtHeaderSignature : DWORD;
begin
  result := archUnknown;
  ///

  hFile := CreateFileW(
                        PWideChar(AFilePath),
                        GENERIC_READ,
                        FILE_SHARE_READ,
                        nil,
                        OPEN_EXISTING,
                        0,
                        0
  );
  if hFile = INVALID_HANDLE_VALUE then
    raise EWindowsException.Create('CreateFile');

  try
    SetFilePointer(hFile, 0, nil, FILE_BEGIN);

    // Read the Image Dos Header
    if NOT ReadFile(
                      hFile,
                      AImageDosHeader,
                      SizeOf(TImageDosHeader),
                      dwBytesRead,
                      nil
    ) then
      raise EWindowsException.Create('ReadFile');

    // To be considered as a valid PE file, e_magic must be $5A4D (MZ)
    if (AImageDosHeader.e_magic <> IMAGE_DOS_SIGNATURE) then
      raise EPortableExecutableException.Create(peekInvalidDosHeader);

    // Move the cursor to Image NT Signature
    SetFilePointer(hFile, AImageDosHeader._lfanew, nil, FILE_BEGIN);

    // Read the Image NT Signature
    if NOT ReadFile(
                      hFile,
                      AImageNtHeaderSignature,
                      SizeOf(DWORD),
                      dwBytesRead,
                      nil
    ) then
      raise EWindowsException.Create('ReadFile');

    // To be considered as a valid PE file, Image NT Signature must be $00004550 (PE00)
    if (AImageNtHeaderSignature <> IMAGE_NT_SIGNATURE) then
      raise EPortableExecutableException.Create(peekInvalidSignature);

    // Read the Image File Header
    if NOT ReadFile(
                      hFile,
                      AImageFileHeader,
                      sizeOf(TImageFileHeader),
                      dwBytesRead,
                      0
    ) then
      raise EWindowsException.Create('ReadFile');

    // TImageDosHeader.Machine contains the architecture of the file
    case AImageFileHeader.Machine of
      IMAGE_FILE_MACHINE_AMD64 : begin
        result := arch64;
      end;

      IMAGE_FILE_MACHINE_I386 : begin
        result := arch32;
      end;
    end;
  finally
    CloseHandle(hFile);
  end;
end;

{ _.SearchPath_DELF }
function SearchPath_DELF(const AFilePath : String) : String;
var ABufferLen : Cardinal;
    pBuffer    : PWideChar;
begin
  result := AFilePath;
  ///

  if FileExists(AFilePath) then
    Exit();

  ABufferLen := SearchPathW(nil, PWideChar(AFilePath), nil, 0, nil, nil);
  if ABufferLen = 0 then
    raise EWindowsException.Create('SearchPathW');
  ///

  SetLength(result, ABufferLen);

  if SearchPathW(nil, PWideChar(AFilePath), nil, ABufferLen, PWideChar(result), nil) = 0 then
    raise EWindowsException.Create('SearchPathW');
end;

{ _.CheckIfValidApplicationForDebugging }
procedure CheckIfValidApplicationForDebugging(const AFilePath : String);
var AArchitecture : TArchitecture;
    ACurrent64    : Boolean;
begin
  AArchitecture := GetPEFileArchitecture(SearchPath_DELF(AFilePath));
  ///

  if AArchitecture = archUnknown then
    raise EPsyloException.Create('Target file architecture is not compatible.');

  {$IFDEF WIN64}
    ACurrent64 := True;
  {$ELSE IF WIN32}
    ACurrent64 := False;
  {$ENDIF}

  if ACurrent64 and (AArchitecture = arch32) then
    raise Exception.Create('A x86-32 bit version of the debugger can only open x86-32 bit images.');

  if (not ACurrent64) and (AArchitecture = arch64) then
    raise Exception.Create('A x86-64 bit version of the debugger can only open x86-64 bit images.');
end;

function GetProcessArchitecture(const AProcessId : Cardinal) : TArchitecture;
var hProcess : THandle;
    AWow64Process : bool;
begin
  result := archUnknown;
  ///

  if (TOSVersion.Architecture = arIntelX86) then
    Exit(arch32);
  ///

  hProcess := OpenProcess(PROCESS_QUERY_LIMITED_INFORMATION, False, AProcessId);
  if hProcess = 0 then
    raise EWindowsException.Create('OpenProcess');
  try
    IsWow64Process(hProcess, AWow64Process);
    ///

    if AWow64Process then
      result := arch32
    else
      result := arch64;
  finally
    CloseHandle(hProcess);
  end;
end;

function IsProcessRunningSameArchitecture(const AProcessId : Cardinal) : Boolean;
var ACurrent64    : Boolean;
    AProcess64    : Boolean;
    AArchitecture : TArchitecture;
begin
  {$IFDEF WIN64}
    ACurrent64 := True;
  {$ELSE IF WIN32}
    ACurrent64 := False;
  {$ENDIF}

  AArchitecture := GetProcessArchitecture(AProcessId);
  if AArchitecture = archUnknown then
    raise Exception.Create('Could not resolve process architecture');

  AProcess64 := (AArchitecture = arch64);

  ///
  result := (ACurrent64 = AProcess64);
end;

{ _.IsProcessElevated }
function IsProcessElevated(const hProcess : THandle) : Boolean;
var AToken     : THandle;
    AElevation : DWORD;
    ALength    : Cardinal;

const ATokenForElevation = 20;
begin
  result := True;
  ///

  if NOT OpenProcessToken(hProcess, TOKEN_QUERY, AToken) then
    raise EWindowsException.Create('OpenProcessToken');
  try
    if NOT GetTokenInformation(AToken, TTokenInformationClass(ATokenForElevation), @AElevation, SizeOf(DWORD), ALength) then
      raise EWindowsException.Create('GetTokenInformation');
    
    result := (AElevation <> 0);
  finally
    CloseHandle(AToken);
  end;
end;

{ _.IsProcessElevatedById }
function IsProcessElevatedById(const AProcessId : Cardinal) : Boolean;
var hProcess : THandle;
begin
  result := True;
  ///

  if (Win32MajorVersion < 6) then
    Exit();

  hProcess := OpenProcess(PROCESS_QUERY_LIMITED_INFORMATION, false, AProcessId);
  if hProcess = 0 then
    raise EWindowsException.Create('OpenProcess');
  try
    result := IsProcessElevated(hProcess);
  finally
    CloseHandle(hProcess);
  end;
end;

{ _.IsCurrentProcessElevated }
function IsCurrentProcessElevated() : Boolean;
begin
  result := IsProcessElevated(GetCurrentProcess());
end;

{ _.DELF_GetMappedFileName }
function DELF_GetMappedFileName(const hProcess : THandle; pOffset : Pointer) : String;
var ALength         : Cardinal;
    AReturnedLength : Cardinal;
begin
  result := '';
  ///

  ALength := MAX_PATH * 2;

  SetLength(result, ALength);
  try
    AReturnedLength := GetMappedFileNameW(hProcess, pOffset, PWideChar(result), ALength);
    if AReturnedLength = 0 then
      raise EWindowsException.Create('GetMappedFileNameW');
  finally
    SetLength(result, AReturnedLength);
  end;
end;

end.
