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

// TODO: Integrate with uPortableExecutable.pas

unit uModuleDump;

interface

uses
  WinAPI.Windows,
  WinAPI.PsAPI,
  WinAPI.ImageHlp,
  WinAPI.TlHelp32,
  System.Math,
  System.Classes,
  Generics.Collections,
  System.SysUtils;

type
  TModuleDump = class
  private
    FProcessId              : Cardinal;
    FProcessHandle          : THandle;
    FModuleHandle           : THandle;
    FModuleInformation      : TModuleInfo;

    FHeaderExtracted        : Boolean;
    FImageDosHeader         : TImageDosHeader;
    FDosStub                : array of byte;
    FImageNtHeaderSignature : DWORD;
    FImageFileHeader        : TImageFileHeader;
    FOptionalHeader         : TImageOptionalHeader;
    FSectionHeaders         : TList<TImageSectionHeader>;
    FAlignedSectionHeaders  : TList<TImageSectionHeader>;

    {@M}
    function GetBaseOfAddress() : Pointer;
    function GetSizeOfImage() : DWORD;
    function GetEntryPoint() : Pointer;

    procedure ResetHeaderInformation();
    procedure AlignSectionHeaders();

    function GetIs64() : Boolean;

    function GetExtendedExceptionInformation() : String;
  public
    {@C}
    constructor Create(const AProcessHandle, AModuleHandle : Thandle);
    destructor Destroy(); override;

    {@M}
    procedure ExtractHeaders();
    procedure SaveToFile(const AOutputFileName : String);

    {@G}
    property BaseOfAddress : Pointer read GetBaseOfAddress;
    property SizeOfImage   : DWORD   read GetSizeOfImage;
    property EntryPoint    : Pointer read GetEntryPoint;
    property Is64          : Boolean read GetIs64;
  end;

implementation

uses uExceptions, uFunctions, uPortableExecutable;

{ _.ComputePECheckSum }
procedure ComputePECheckSum(const APEFile : String);
var hFile        : THandle;
    hFileMap     : THandle;
    pFileMapView : Pointer;
    AFileSize    : Int64;
    AHeaderSum   : DWORD;
    ACheckSum    : DWORD;
    AOffset      : DWORD;
begin
  hFile := CreateFileW(
      PWideChar(APEFile),
      GENERIC_READ or GENERIC_WRITE,
      FILE_SHARE_READ,
      nil,
      OPEN_EXISTING,
      FILE_ATTRIBUTE_NORMAL,
      0
  );
  if hFile = INVALID_HANDLE_VALUE then
    raise EWindowsException.Create(Format('CreateFileW[%s]', [APEFile]));
  try
    if not GetFileSizeEx(hFile, AFileSize) then
      raise EWindowsException.Create(Format('GetFileSizeEx[%s]', [APEFile]));
    ///

    hFileMap := CreateFileMapping(hFile, nil, PAGE_READWRITE, 0, AFileSize, nil);
    if hFileMap = 0 then
      raise EWindowsException.Create(Format('CreateFileMapping[%s]', [APEFile]));
    try
      pFileMapView := MapViewOfFile(hFileMap, FILE_MAP_READ or FILE_MAP_WRITE, 0, 0, AFileSize);
      if pFileMapView = nil then
        raise EWindowsException.Create(Format('MapViewOfFile[%s]', [APEFile]));
      try
        if CheckSumMappedFile(pFileMapView, AFileSize, @AHeaderSum, @ACheckSum) = nil then
          raise EWindowsException.Create(Format('CheckSumMappedFile[%s]', [APEFile]));
        ///

        if (PImageDosHeader(pFileMapView)^.e_magic <> IMAGE_DOS_SIGNATURE) or
           (PDWORD(NativeUInt(pFileMapView) + PImageDosHeader(pFileMapView)^._lfanew)^ <> IMAGE_NT_SIGNATURE) then
          raise Exception.Create(Format('"%s" is not a valid PE file.', [APEFile]));
        ///

        // Hot-Patch Checksum in Optional Header
        AOffset := PImageDosHeader(pFileMapView)^._lfanew;
        Inc(AOffset, SizeOf(DWORD)); // NT Signature
        Inc(AOffset, SizeOf(TImageFileHeader));

        PImageOptionalHeader(Pointer(NativeUInt(pFileMapView) + AOffset))^.CheckSum := ACheckSum;
      finally
        FlushViewOfFile(pFileMapView, AFileSize);
        UnmapViewOfFile(pFileMapView);
      end;
    finally
      CloseHandle(hFileMap);
    end;
  finally
    CloseHandle(hFile);
  end;
end;

{ TModuleDump.GetExtendedExceptionInformation }
function TModuleDump.GetExtendedExceptionInformation() : String;
begin
  result := Format('pid: %d, mhandle: %d, offset: %p', [
      FProcessId,
      FModuleHandle,
      FModuleInformation.lpBaseOfDll
  ]);
end;

{ TModuleDump.ResetHeaderInformation }
procedure TModuleDump.ResetHeaderInformation();
begin
  FHeaderExtracted := False;
  ///

  ZeroMemory(@FImageDosHeader, SizeOf(TImageDosHeader));
  ZeroMemory(@FImageFileHeader, SizeOf(TImageFileHeader));
  ZeroMemory(@FOptionalHeader, SizeOf(TImageOptionalHeader));

  SetLength(FDosStub, 0);

  if Assigned(FSectionHeaders) then
    FSectionHeaders.Clear();

  if Assigned(FAlignedSectionHeaders) then
    FAlignedSectionHeaders.Clear();

  FImageNtHeaderSignature := 0;
end;

{ TModuleDump.ExtractHeaders }
procedure TModuleDump.ExtractHeaders();
var pModuleOffset       : Pointer;
    AImageSectionHeader : TImageSectionHeader;
    ABytesRead          : SIZE_T;
    I                   : Integer;
begin
  self.ResetHeaderInformation();
  ///

  pModuleOffset := FModuleInformation.lpBaseOfDll;
  try
    // Read Image Dos Header
    if not ReadProcessMemory(FProcessHandle, pModuleOffset, @FImageDosHeader, SizeOf(TImageDosHeader), ABytesRead) then
      raise EWindowsException.Create('ReadProcessMemory[IMAGE_DOS_HEADER]');
    ///

    // Check Image Dos Signature
    if FImageDosHeader.e_magic <> IMAGE_DOS_SIGNATURE then
      raise Exception.Create('Invalid Dos Header Signature.');

    // Extract Dos Stub
    pModuleOffset := Pointer(NativeUInt(pModuleOffset) + SizeOf(TImageDosHeader));

    SetLength(FDosStub, FImageDosHeader._lfanew - SizeOf(TImageDosHeader));
    if not ReadProcessMemory(FProcessHandle, pModuleOffset, @FDosStub[0], Length(FDosStub), ABytesRead) then
      raise EWindowsException.Create('ReadProcessMemory[DOS_STUB]');

    // Read NT Signature
    pModuleOffset := Pointer(NativeUInt(pModuleOffset) + Length(FDosStub));

    if not ReadProcessMemory(FProcessHandle, pModuleOffset, @FImageNtHeaderSignature, SizeOf(DWORD), ABytesRead) then
      raise Exception.Create('ReadProcessMemory[IMAGE_NT_HEADER_SIGNATURE]');

    if (FImageNtHeaderSignature <> IMAGE_NT_SIGNATURE) then
      raise Exception.Create('Invalid NT Signature.');

    // Read Image File Header
    pModuleOffset := Pointer(NativeUInt(pModuleOffset) + SizeOf(DWORD));

    if not ReadProcessMemory(FProcessHandle, pModuleOffset, @FImageFileHeader, SizeOf(TImageFileHeader), ABytesRead) then
      raise Exception.Create('ReadProcessMemory[IMAGE_FILE_HEADER]');

    // Read Image Optional Header
    pModuleOffset := Pointer(NativeUInt(pModuleOffset) + SizeOf(TImageFileHeader));

    if not ReadProcessMemory(FProcessHandle, pModuleOffset, @FOptionalHeader, SizeOf(TImageOptionalHeader), ABytesRead) then
      raise Exception.Create('ReadProcessMemory[IMAGE_OPTIONAL_HEADER]');

    // Iterate through available section information to calculate raw image size
    pModuleOffset := Pointer(NativeUInt(pModuleOffset) + SizeOf(TImageOptionalHeader));

    for I := 0 to FImageFileHeader.NumberOfSections -1 do begin
      if not ReadProcessMemory(FProcessHandle, pModuleOffset, @AImageSectionHeader, SizeOf(TImageSectionHeader), ABytesRead) then
        raise Exception.Create(Format('ReadProcessMemory[IMAGE_SECTION_HEADER, section_id:%d]', [I+1]));
      ///

      pModuleOffset := Pointer(NativeUInt(pModuleOffset) + SizeOf(TImageSectionHeader));

      FSectionHeaders.Add(AImageSectionHeader);
    end;

    ///
    FHeaderExtracted := True;
  except
    on E : Exception do begin
      E.Message := Format('%s, %s', [
        E.Message,
        self.GetExtendedExceptionInformation()
      ]);

      ///
      raise E;
    end;
  end;
end;

{ TModuleDump.AlignSectionHeaders }
procedure TModuleDump.AlignSectionHeaders();
var AImageSectionHeader : TImageSectionHeader;
    I                   : Integer;
begin
  FAlignedSectionHeaders.Clear();
  ///

  for I := 0 to FSectionHeaders.Count -1 do begin
    AImageSectionHeader := FSectionHeaders.Items[i];

    AImageSectionHeader.PointerToRawData := AImageSectionHeader.VirtualAddress;

    if (I + 1 < FSectionHeaders.Count) then
      AImageSectionHeader.SizeOfRawData := (
        FSectionHeaders.Items[I + 1].VirtualAddress - AImageSectionHeader.VirtualAddress
      );

    ///
    FAlignedSectionHeaders.Add(AImageSectionHeader);
  end;
end;

{ TModuleDump.SaveToFile }
procedure TModuleDump.SaveToFile(const AOutputFileName : String);
var AFileStream         : TFileStream;
    AImageSectionHeader : TImageSectionHeader;
    ABufferSize         : Int64;
    pBuffer             : Pointer;
    pOffset             : Pointer;
    ABytesRead          : SIZE_T;
begin
  if not FHeaderExtracted then
    self.ExtractHeaders();
  ///
  try
    ForceDirectories(ExtractFilePath(AOutputFileName));

    AFileStream := TFileStream.Create(AOutputFilename, fmOpenWrite or fmCreate or fmShareExclusive);
    try
      // Write DOS Header
      AFileStream.Write(FImageDosHeader, SizeOf(TImageDosHeader));

      // Write DOS Stub
      AFileStream.Write(FDosStub[0], Length(FDosStub));

      // Write NT Header Signature
      AFileStream.Write(FImageNtHeaderSignature, SizeOf(DWORD));

      // Write Image File Header
      AFileStream.Write(FImageFileHeader, SizeOf(TImageFileHeader));

      // Write The Optional Header
      AFileStream.Write(FOptionalHeader, SizeOf(TImageOptionalHeader));

      // Align Sections
      self.AlignSectionHeaders();

      // Write Aligned Section Headers
      for AImageSectionHeader in FAlignedSectionHeaders do
        AFileStream.Write(AImageSectionHeader, SizeOf(TImageSectionHeader));

      // Write Section Data (JIT)
      for AImageSectionHeader in FAlignedSectionHeaders do begin
        ABufferSize := AImageSectionHeader.SizeOfRawData;
        if ABufferSize = 0 then
          continue;

        AFileStream.Position := AImageSectionHeader.PointerToRawData;

        GetMem(pBuffer, ABufferSize);
        try
          pOffset := Pointer(NativeUInt(FModuleInformation.lpBaseOfDll) + AImageSectionHeader.VirtualAddress);

          if not ReadProcessMemory(FProcessHandle, pOffset, pBuffer, ABufferSize, ABytesRead) then
            raise EWindowsException.Create(
              Format('ReadProcessMemory[SECTION_DATA, section_id:%d]', [
                FAlignedSectionHeaders.IndexOf(AImageSectionHeader)
              ])
            );
          ///

          AFileStream.Write(PByte(pBuffer)^, ABufferSize);
        finally
          FreeMem(pBuffer, ABufferSize);
        end;
      end;
    finally
      if Assigned(AFileStream) then
        FreeAndNil(AFileStream);
    end;

    // Update Optional Header Checksum with Image Dump
    ComputePECheckSum(AOutputFileName);
  except
    on E : Exception do begin
      E.Message := Format('%s, %s', [
        E.Message,
        self.GetExtendedExceptionInformation()
      ]);

      ///
      raise E;
    end;
  end;
end;

{ TModuleDump.Create }
constructor TModuleDump.Create(const AProcessHandle, AModuleHandle : THandle);
begin
  inherited Create();
  ///

  FSectionHeaders := TList<TImageSectionHeader>.Create();
  FAlignedSectionHeaders := TList<TImageSectionHeader>.Create();

  ZeroMemory(@FModuleInformation, SizeOf(TModuleInfo));

  self.ResetHeaderInformation();

  FProcessHandle := AProcessHandle;
  FModuleHandle  := AModuleHandle;

  if (FProcessHandle <= 0) or (FModuleHandle <= 0) then
    raise Exception.Create('You must pass a valid process and module handle.');

  FProcessId := GetProcessId(FProcessHandle);
  if FProcessId = 0 then
    raise EWindowsException.Create(
    Format('GetProcessId(phandle: %d, mhandle: %d)', [
      FProcessHandle,
      FModuleHandle
    ])
  );

  if not GetModuleInformation(FProcessHandle, FModuleHandle, @FModuleInformation, sizeOf(TModuleInfo)) then
    raise EWindowsException.Create(
      Format('GetModuleInformation(mhandle: %d, pid: %d)', [
        FModuleHandle,
        FProcessId
      ])
    );
end;

{ TModuleDump.Destroy }
destructor TModuleDump.Destroy();
begin
  self.ResetHeaderInformation();

  if Assigned(FSectionHeaders) then
    FreeAndNil(FSectionHeaders);

  if Assigned(FAlignedSectionHeaders) then
    FreeAndNil(FAlignedSectionHeaders);

  ///
  inherited Destroy();
end;

{ TModuleDump.GetBaseOfAddress }
function TModuleDump.GetBaseOfAddress() : Pointer;
begin
  if not FHeaderExtracted then
    self.ExtractHeaders();
  ///

  result := FModuleInformation.lpBaseOfDll;
end;

{ TModuleDump.GetSizeOfImage }
function TModuleDump.GetSizeOfImage() : DWORD;
begin
  if not FHeaderExtracted then
    self.ExtractHeaders();
  ///

  result := FModuleInformation.SizeOfImage;
end;

{ TModuleDump.GetEntryPoint }
function TModuleDump.GetEntryPoint() : Pointer;
begin
  if not FHeaderExtracted then
    self.ExtractHeaders();
  ///

  result := FModuleInformation.EntryPoint;
end;

{ TModuleDump.GetIs64 }
function TModuleDump.GetIs64() : Boolean;
begin
  if not FHeaderExtracted then
    self.ExtractHeaders();
  ///

  result := (self.FImageFileHeader.Machine = IMAGE_FILE_MACHINE_AMD64);
end;

end.
