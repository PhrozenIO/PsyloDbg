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

// TODO: Integrate with uModuleDump.pas

unit uPortableExecutable;

interface

uses Winapi.Windows,
     System.Classes,
     Generics.Collections;

type
  TPEHeaderSectionNature = (
    pesnNone,
    pesnDosHeader,
    pesnDosStub,
    pesnNtSignature,
    pesnFileHeader,
    pesnOptionalHeader,
    pesnSectionHeader,
    pesnSectionData
  );

  TImageOptionalHeader =
    {$IFDEF WIN64}
      TImageOptionalHeader64
    {$ELSE}
      TImageOptionalHeader32
    {$ENDIF};
  PImageOptionalHeader = ^TImageOptionalHeader;

  TSection = class
  private
    FName               : String;
    FOffset             : NativeUInt;
    FImageSectionHeader : TImageSectionHeader;
  public
    {@C}
    constructor Create(const AImageSectionHeader : TImageSectionHeader; const AOffset : NativeUInt);

    {@G}
    property Name               : String              read FName;
    property Offset             : NativeUInt          read FOffset;
    property ImageSectionHeader : TImageSectionHeader read FImageSectionHeader;
  end;

  TParseFrom = (
    pfNone,
    pfFile,
    pfMemory
  );

  TPortableExecutable = class
  private
    FSuccess                   : Boolean;
    FParseFrom                 : TParseFrom;

    FHandle                    : THandle;
    FCloseHandle               : Boolean;
    FBaseOffset                : NativeUInt;

    FImageDosHeader            : TImageDosHeader;

    FDosStubOffset             : NativeUInt;
    FDosStub                   : array of byte;

    FImageNtSignatureOffset    : NativeUInt;
    FImageNtSignature          : DWORD;

    FImageFileHeaderOffset     : NativeUInt;
    FImageFileHeader           : TImageFileHeader;

    FImageOptionalHeaderOffset : NativeUInt;
    FImageOptionalHeader       : TImageOptionalHeader;

    FSections                  : TObjectList<TSection>;

    {@C}
    constructor Create();

    {@M}
    function GetImageDosHeader() : TImageDosHeader;
    function GetImageNtSignature() : DWORD;
    function GetImageFileHeader() : TImageFileHeader;
    function GetImageOptionalHeader() : TImageOptionalHeader;
    function GetSections() : TList<TSection>;

    procedure RaiseUnparsedHeader();

    procedure Parse();
  public
    {@C}
    constructor CreateFromFile(const AFileName : String);
    constructor CreateFromMemory(const AProcessId : Cardinal; const ABaseAddress : Pointer); overload;
    constructor CreateFromMemory(const AProcessHandle : THandle; const ABaseAddress : Pointer); overload;

    destructor Destroy(); override;

    {@M}
    function GetHeaderSectionNature(const AOffset : NativeUInt; var AExtraInfo : String) : TPEHeaderSectionNature; overload;
    function GetHeaderSectionNature(const AOffset : NativeUInt) : TPEHeaderSectionNature; overload;

    function GetHeaderSectionName(const AOffset : NativeUInt; const AShortName : Boolean; var APEHeaderSectionNature : TPEHeaderSectionNature) : String; overload;
    function GetHeaderSectionName(const AOffset : NativeUInt; const AShortName : Boolean) : String; overload;

    {@G}
    property ImageDosHeader      : TImageDosHeader      read GetImageDosHeader;
    property ImageNtSignature    : DWORD                read GetImageNtSignature;
    property ImageFileHeader     : TImageFileHeader     read GetImageFileHeader;
    property ImageOptionalHeader : TImageOptionalHeader read GetImageOptionalHeader;
    property Sections            : TList<TSection>      read GetSections;
  end;

implementation

uses uExceptions, System.SysUtils;

(* TPortableExecutable Class *)

{ TPortableExecutable.Create }
constructor TPortableExecutable.Create();
begin
  inherited Create();
  ///

  FSuccess     := False;
  FParseFrom   := pfNone;
  FHandle      := INVALID_HANDLE_VALUE;
  FCloseHandle := True;
  FBaseOffset  := 0;

  FDosStubOffset             := 0;
  FImageNtSignatureOffset    := 0;
  FImageOptionalHeaderOffset := 0;
  FImageFileHeaderOffset     := 0;

  ZeroMemory(@FImageDosHeader, SizeOf(TImageDosHeader));
  ZeroMemory(@FImageFileHeader, SizeOf(TImageFileHeader));
  ZeroMemory(@FImageOptionalHeader, SizeOf(TImageOptionalHeader));

  SetLength(FDosStub, 0);

  FImageNtSignature := 0;

  FSections := TObjectList<TSection>.Create(True);
end;

{ TPortableExecutable.Parse }
procedure TPortableExecutable.Parse();
var AOffset              : NativeUInt;
    I                    : Cardinal;
    ASectionHeader       : TImageSectionHeader;
    ASectionHeaderOffset : NativeUInt;

  procedure Read(pBuffer : Pointer; const ABufferSize : UInt64; var ASavedOffset : NativeUInt; const AForwardOffset : Boolean = False); overload;
  var ABytesRead  : Cardinal;
      stBytesRead : SIZE_T;
  begin
    case FParseFrom of
      pfFile: begin
        if not SetFilePointerEx(FHandle, AOffset, nil, FILE_BEGIN) then
          raise EWindowsException.Create('SetFilePointerEx');
        ///

        if not ReadFile(FHandle, PByte(pBuffer)^, ABufferSize, ABytesRead, nil) then
          raise EWindowsException.Create('ReadFile');
      end;

      pfMemory: begin
        if not ReadProcessMemory(FHandle, Pointer(AOffset), pBuffer, ABufferSize, stBytesRead) then
          raise EWindowsException.Create('ReadProcessMemory');
      end;
    end;

    ASavedOffset := AOffset;

    ///
    if AForwardOffset then
      Inc(AOffset, ABufferSize);
  end;

  procedure Read(pBuffer : Pointer; const ABufferSize : UInt64; const AForwardOffset : Boolean = False); overload;
  var ADummy : NativeUInt;
  begin
    Read(pBuffer, ABufferSize, ADummy, AForwardOffset);
  end;

begin
  FSuccess := False;
  ///

  if FHandle = INVALID_HANDLE_VALUE then
    raise Exception.Create('Invalid handle, can''t parse "PE Header".');
  ///

  AOffset := FBaseOffset;

  // Read Dos and NT Header
  Read(@FImageDosHeader, SizeOf(FImageDosHeader), True);

  if FImageDosHeader.e_magic <> IMAGE_DOS_SIGNATURE then
    raise EPortableExecutableException.Create(peekInvalidDosHeader);

  //SetLength(FDosStub, (FBaseOffset + FImageDosHeader._lfanew) - SizeOf(TImageDosHeader));

  //Read(@FDosStub, Length(FDosStub), FDosStubOffset, True);

  AOffset := FBaseOffset + FImageDosHeader._lfanew;

  Read(@FImageNtSignature, SizeOf(DWORD), FImageNtSignatureOffset, True);

  if FImageNtSignature <> IMAGE_NT_SIGNATURE then
    raise EPortableExecutableException.Create(peekInvalidSignature);

  Read(@FImageFileHeader, SizeOf(TImageFileHeader), FImageFileHeaderOffset, True);

  case FImageFileHeader.Machine of
    IMAGE_FILE_MACHINE_AMD64,
    IMAGE_FILE_MACHINE_R3000,
    IMAGE_FILE_MACHINE_R4000,
    IMAGE_FILE_MACHINE_R10000,
    IMAGE_FILE_MACHINE_ALPHA,
    IMAGE_FILE_MACHINE_POWERPC,
    IMAGE_FILE_MACHINE_IA64,
    IMAGE_FILE_MACHINE_ALPHA64,
    IMAGE_FILE_MACHINE_I386: ;
    else
      raise EPortableExecutableException.Create(peekInvalidArchitecture);
  end;

  {$IFDEF WIN64}
    if FImageFileHeader.Machine <> IMAGE_FILE_MACHINE_AMD64 then
  {$ELSE}
    if FImageFileHeader.Machine <> IMAGE_FILE_MACHINE_I386 then
  {$ENDIF}
    raise EPortableExecutableException.Create(peekIncompatibleArchitecture);

  Read(@FImageOptionalHeader, SizeOf(TImageOptionalHeader), FImageOptionalHeaderOffset, True);

  FSections.Clear();

  // Read Sections Headers
  for I := 1 to FImageFileHeader.NumberOfSections do begin
    Read(@ASectionHeader, SizeOf(TImageSectionHeader), ASectionHeaderOffset, True);

    FSections.Add(TSection.Create(ASectionHeader, ASectionHeaderOffset));
  end;

  ///
  FSuccess := True;
end;

{ TPortableExecutable.CreateFromFile }
constructor TPortableExecutable.CreateFromFile(const AFileName : String);
begin
  Create();
  ///

  FParseFrom := pfFile;

  FHandle := CreateFileW(
      PWideChar(AFileName),
      GENERIC_READ,
      FILE_SHARE_READ,
      nil,
      OPEN_EXISTING,
      0,
      0
  );
  if FHandle = INVALID_HANDLE_VALUE then
    raise EWindowsException.Create('CreateFileW');

  ///
  self.Parse();
end;

{ TPortableExecutable.CreateFromMemory }
constructor TPortableExecutable.CreateFromMemory(const AProcessId : Cardinal; const ABaseAddress : Pointer);
var AProcessHandle : THandle;
begin
  AProcessHandle := OpenProcess(
      PROCESS_VM_READ,
      False,
      AProcessId
  );
  if AProcessHandle = 0 then
    raise EWindowsException.Create('OpenProcess');
  ///

  self.CreateFromMemory(AProcessHandle, ABaseAddress);
end;

constructor TPortableExecutable.CreateFromMemory(const AProcessHandle : THandle; const ABaseAddress : Pointer);
begin
  Create();
  ///

  FParseFrom   := pfMemory;

  FHandle      := AProcessHandle;
  FCloseHandle := False;

  FBaseOffset  := NativeUInt(ABaseAddress);

  ///
  self.Parse();
end;

{ TPortableExecutable.Destroy }
destructor TPortableExecutable.Destroy();
begin
  SetLength(FDosStub, 0);
  ///

  if Assigned(FSections) then
    FreeAndNil(FSections);

  if (FHandle <> INVALID_HANDLE_VALUE) and FCloseHandle then
    CloseHandle(FHandle);

  ///
  inherited Destroy();
end;

{ TPortableExecutable.GetHeaderSectionNature }
function TPortableExecutable.GetHeaderSectionNature(const AOffset : NativeUInt; var AExtraInfo : String) : TPEHeaderSectionNature;
var ASection       : TSection;
    ANextSection   : TSection;
    ASectionSize   : UInt64;
    ASectionOffset : UInt64;
begin
  result := pesnNone;
  ///

  AExtraInfo := '';

  self.RaiseUnparsedHeader();

  try
    if (AOffset >= FBaseOffset) and (AOffset < FDosStubOffset) then
      result := pesnDosHeader
    else if (AOffset >= FDosStubOffset) and (AOffset < FImageDosHeader._lfanew) then
      result := pesnDosStub
    else if (AOffset >= FImageDosHeader._lfanew) and (AOffset < FImageNtSignatureOffset) then
      result := pesnNtSignature
    else if (AOffset >= FImageNtSignatureOffset) and (AOffset < FImageFileHeaderOffset) then
      result := pesnFileHeader
    else if (AOffset >= FImageFileHeaderOffset) and (AOffset < FImageOptionalHeaderOffset) then
      result := pesnOptionalHeader
    else if (AOffset >= FImageOptionalHeaderOffset) and (
      AOffset < (FImageOptionalHeaderOffset + (FSections.Count * SizeOf(TImageSectionHeader)))
    ) then
      result := pesnSectionHeader
    else begin
      // Section Headers
      for ASection in FSections do begin
        case FParseFrom of
          pfFile : begin
            ASectionSize   := ASection.ImageSectionHeader.SizeOfRawData;
            ASectionOffset := FBaseOffset + ASection.ImageSectionHeader.PointerToRawData;
          end;

          pfMemory : begin
            ASectionSize   := ASection.ImageSectionHeader.Misc.VirtualSize;
            ASectionOffset := FBaseOffset + ASection.ImageSectionHeader.VirtualAddress;
          end;
        end;

        if (AOffset >= ASectionOffset) and (AOffset < (ASectionOffset + ASectionSize)) then begin
          result := pesnSectionData;

          AExtraInfo := ASection.Name;

          break;
        end;
      end;
    end;
  except
    // Ignore (Possible Integer Overflow)
  end;
end;

function TPortableExecutable.GetHeaderSectionNature(const AOffset : NativeUInt) : TPEHeaderSectionNature;
var ADummy : String;
begin
  GetHeaderSectionNature(AOffset, ADummy);
end;

{ TPortableExecutable.GetHeaderSectionName }
function TPortableExecutable.GetHeaderSectionName(const AOffset : NativeUInt; const AShortName : Boolean; var APEHeaderSectionNature : TPEHeaderSectionNature) : String;
var AExtraInfo : String;
begin
  result := '';
  ///

  APEHeaderSectionNature := GetHeaderSectionNature(AOffset, AExtraInfo);

  if AShortName then begin
    case APEHeaderSectionNature of
      pesnDosHeader,
      pesnDosStub,
      pesnNtSignature,
      pesnFileHeader,
      pesnOptionalHeader,
      pesnSectionHeader:
        result := 'PE Header';
    end;
  end else begin
    case APEHeaderSectionNature of
      pesnDosHeader      : result := 'Dos Header';
      pesnDosStub        : result := 'Dos Stub';
      pesnNtSignature    : result := 'NT Signature';
      pesnFileHeader     : result := 'File Header';
      pesnOptionalHeader : result := 'Optional Header';
      pesnSectionHeader  : result := 'Section Header';
    end;
  end;

  ///
  if (APEHeaderSectionNature = pesnSectionData) then begin
    if AShortName then
      result := AExtraInfo
    else
      result := Format('%s (Section Data)', [AExtraInfo]);
  end;
end;

function TPortableExecutable.GetHeaderSectionName(const AOffset : NativeUInt; const AShortName : Boolean) : String;
var ANature : TPEHeaderSectionNature;
begin
  self.GetHeaderSectionName(AOffset, AShortName, ANature);
end;

{ TPortableExecutable.RaiseUnparsedHeader }
procedure TPortableExecutable.RaiseUnparsedHeader();
begin
  if not FSuccess then
    raise Exception.Create('PE Header not parsed.');
end;

{ TPortableExecutable.GetImageDosHeader }
function TPortableExecutable.GetImageDosHeader() : TImageDosHeader;
begin
  self.RaiseUnparsedHeader();
  ///

  result := FImageDosHeader;
end;

{ TPortableExecutable.GetImageNtSignature }
function TPortableExecutable.GetImageNtSignature() : DWORD;
begin
  self.RaiseUnparsedHeader();
  ///

  result := FImageNtSignature;
end;

{ TPortableExecutable.GetImageFileHeader }
function TPortableExecutable.GetImageFileHeader() : TImageFileHeader;
begin
  self.RaiseUnparsedHeader();
  ///

  result := FImageFileHeader;
end;

{ TPortableExecutable.GetImageOptionalHeader }
function TPortableExecutable.GetImageOptionalHeader() : TImageOptionalHeader;
begin
  self.RaiseUnparsedHeader();
  ///

  result := FImageOptionalHeader;
end;

{ TPortableExecutable.GetSections }
function TPortableExecutable.GetSections() : TList<TSection>;
begin
  self.RaiseUnparsedHeader();
  ///

  result := FSections;
end;

(* TSection Class *)

{ TSection.Create }
constructor TSection.Create(const AImageSectionHeader : TImageSectionHeader; const AOffset : NativeUInt);
begin
  inherited Create();
  ///

  FOffset             := AOffset;
  FImageSectionHeader := AImageSectionHeader;

  SetString(FName, PAnsiChar(@AImageSectionHeader.Name), Length(AImageSectionHeader.Name));

  FName := FName.Trim();
end;

end.

