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

unit uMemoryMap;

interface

uses System.Classes,
     System.SysUtils,
     Generics.Collections,
     Winapi.Windows,
     uPortableExecutable;

type
  TMemoryPageKind = (
    mpkNone,
    mpkPEHeader,
    mpkPESectionData
  );

  TMemoryPage = class(TPersistent)
  private
    FName         : String;
    FBaseAddress  : Pointer;
    FPageAddress  : Pointer;
    FAllocProtect : DWORD;
    FProtect      : DWORD;
    FState        : DWORD;
    FSize         : SIZE_T;
    FType         : DWORD;
    FKind         : TMemoryPageKind;

    {@M}
    function GetProtectString() : String;
    function GetAllocProtectString() : String;
    function GetTypeString() : String;
    function GetFormatedSize() : String;
    function GetStateString() : String;
  public
    {@C}
    constructor Create(const AMemoryBasicInformation : TMemoryBasicInformation); overload;

    {@M}
    procedure Assign(ASource : TPersistent); override;

    {@G}
    property BaseAddress  : Pointer         read FBaseAddress;
    property PageAddress  : Pointer         read FPageAddress;
    property Protect      : DWORD           read FProtect;
    property AllocProtect : DWORD           read FAllocProtect;
    property State        : DWORD           read FState;
    property Size         : SIZE_T          read FSize;
    property PageType     : DWORD           read FType;

    {@G/S}
    property Name : String          read FName write FName;
    property Kind : TMemoryPageKind read FKind write FKind;

    {@G}
    property Protect_STR : String read GetProtectString;
    property AllocProtect_STR : String read GetAllocProtectString;
    property State_STR : String read GetStateString;
    property Type_STR : String read GetTypeString;
    property FormatedSize : String read GetFormatedSize;
  end;

  TMemoryRegion = class
  private
    FName           : String;
    FBaseAddress    : Pointer;
    FPages          : TObjectList<TMemoryPage>;
    FPEHeaderRegion : TPortableExecutable;

    {@M}
    function GetRegionSize() : Int64;
    function GetRegionFormatedSize() : String;
  public
    {@C}
    constructor Create(const AName : String; const pBaseAddress : Pointer);
    destructor Destroy(); override;

    {@M}
    procedure AddPage(const APage : TMemoryPage);
    procedure ProbeRegion(const hProcess : THandle);

    {@G}
    property Pages        : TObjectList<TMemoryPage> read FPages;
    property Name         : String                   read FName;
    property BaseAddress  : Pointer                  read FBaseAddress;
    property Size         : Int64                    read GetRegionSize;
    property FormatedSize : String                   read GetRegionFormatedSize;
  end;

  TMemoryMap = class
  private
    FRegions : TObjectDictionary<Pointer, TMemoryRegion>;

    {@M}
    function GetMemorySize() : Int64;
  public
    {@C}
    constructor Create();
    destructor Destroy(); override;

    {@M}
    function AddRegion(const ARegionName : String; const pBaseAddress : Pointer) : TMemoryRegion;
    procedure AddPage(const pBaseAddress : Pointer; const APage : TMemoryPage);

    {@G}
    property Regions : TObjectDictionary<Pointer, TMemoryRegion> read FRegions;
    property Size    : Int64 read GetMemorySize;
  end;

implementation

uses uEventUtils, uMemoryUtils, uFunctions;

(* TMemoryPage *)

{ TMemoryPage.Create }
constructor TMemoryPage.Create(const AMemoryBasicInformation : TMemoryBasicInformation);
begin
  inherited Create();
  ///

  FName := '';
  FKind := mpkNone;
  ///

  FBaseAddress  := AMemoryBasicInformation.AllocationBase;
  FPageAddress  := AMemoryBasicInformation.BaseAddress;
  FAllocProtect := AMemoryBasicInformation.AllocationProtect;
  FProtect      := AMemoryBasicInformation.Protect;
  FSize         := AMemoryBasicInformation.RegionSize;
  FState        := AMemoryBasicInformation.State;
  FType         := AMemoryBasicInformation.Type_9;
end;

{ TMemoryPage.Assign }
procedure TMemoryPage.Assign(ASource : TPersistent);
begin
  if ASource is TMemoryPage then begin
    FName         := TMemoryPage(ASource).Name;
    FBaseAddress  := TMemoryPage(ASource).BaseAddress;
    FPageAddress  := TMemoryPage(ASource).PageAddress;
    FProtect      := TMemoryPage(ASource).Protect;
    FAllocProtect := TMemoryPage(ASource).AllocProtect;
    FSize         := TMemoryPage(ASource).Size;
    FType         := TMemoryPage(ASource).PageType;
    FState        := TMemoryPage(ASource).State;
    FKind         := TMemoryPage(ASource).Kind;
  end else
    inherited Assign(ASource);
end;

{ TMemoryPage.GetProtectString }
function TMemoryPage.GetProtectString() : String;
begin
  result := ProtectToString(FProtect);
end;

{ TMemoryPage.GetAllocProtectString }
function TMemoryPage.GetAllocProtectString() : String;
begin
  result := ProtectToString(FAllocProtect);
end;

{ TMemoryPage.GetTypeString }
function TMemoryPage.GetTypeString() : String;
begin
  result := MemoryTypeToString(FType);
end;

{ TMemoryPage.GetStateString }
function TMemoryPage.GetStateString() : String;
begin
  result := MemoryStateToString(FState);
end;

{ TMemoryPage.GetFormatedSize }
function TMemoryPage.GetFormatedSize() : String;
begin
  result := FormatSize(FSize);
end;

(* TMemoryRegion *)

{ TMemoryRegion.Create }
constructor TMemoryRegion.Create(const AName : String; const pBaseAddress : Pointer);
begin
  inherited Create();
  ///

  FName := AName;
  FBaseAddress := pBaseAddress;

  FPages := TObjectList<TMemoryPage>.Create(True);

  FPEHeaderRegion := nil;
end;

{ TMemoryRegion.ProbeRegion }
procedure TMemoryRegion.ProbeRegion(const hProcess : THandle);
begin
  try
    FPEHeaderRegion := TPortableExecutable.CreateFromMemory(hProcess, FBaseAddress);
  except
    if Assigned(FPEHeaderRegion) then
      FreeAndNil(FPEHeaderRegion);
  end;
end;

{ TMemoryRegion.Destroy }
destructor TMemoryRegion.Destroy();
begin
  if Assigned(FPages) then
    FreeAndNil(FPages);

  ///
  inherited Destroy();
end;

{ TMemoryRegion.AddPage }
procedure TMemoryRegion.AddPage(const APage : TMemoryPage);
var ANature : TPEHeaderSectionNature;
begin
  if not Assigned(APage) then
    Exit();
  ///

  // Extend Page Information
  if Assigned(FPEHeaderRegion) then begin
    APage.Name := FPEHeaderRegion.GetHeaderSectionName(NativeUInt(APage.PageAddress), True, ANature);
    
    case ANature of
        pesnDosHeader,
        pesnDosStub,
        pesnNtSignature,
        pesnFileHeader,
        pesnOptionalHeader,
        pesnSectionHeader:
          APage.Kind := mpkPEHeader;

        pesnSectionData: 
          APage.Kind := mpkPESectionData;
    end;
  end;

  ///
  FPages.Add(APage);
end;

{ TMemoryRegion.GetRegionSize }
function TMemoryRegion.GetRegionSize() : Int64;
var APage  : TMemoryPage;
    ATotal : Int64;
begin
  result := 0;

  for APage in FPages do
    Inc(result, APage.Size);
end;

{ TMemoryRegion.GetRegionFormatedSize }
function TMemoryRegion.GetRegionFormatedSize() : String;
begin
  result := FormatSize(GetRegionSize());
end;

(* TMemoryMap *)

{ TMemoryMap.Create }
constructor TMemoryMap.Create();
begin
  FRegions := TObjectDictionary<Pointer, TMemoryRegion>.Create([doOwnsValues]);
end;

{ TMemoryMap.Destroy }
destructor TMemoryMap.Destroy();
begin
  if Assigned(FRegions) then
    FreeAndNil(FRegions);

  ///
  inherited Destroy();
end;

{ TMemoryMap.AddRegion }
function TMemoryMap.AddRegion(const ARegionName : String; const pBaseAddress : Pointer) : TMemoryRegion;
begin
  result := nil;
  ///

  if pBaseAddress = nil then
    Exit();
  ///

  result := TMemoryRegion.Create(ARegionName, pBaseAddress);
  
  if not FRegions.ContainsKey(pBaseAddress) then
    FRegions.AddOrSetValue(pBaseAddress, result);
end;

{ TMemoryMap.AddPage }
procedure TMemoryMap.AddPage(const pBaseAddress : Pointer; const APage : TMemoryPage);
var ARegion : TMemoryRegion;
begin
  if (pBaseAddress = nil) or (not Assigned(APage)) then
    Exit();
  ///

  if not FRegions.TryGetValue(pBaseAddress, ARegion) then
    Exit();

  if Assigned(ARegion) then
    ARegion.AddPage(APage);
end;

{ TMemoryMap.GetMemorySize }
function TMemoryMap.GetMemorySize() : Int64;
var ARegion : TMemoryRegion;
begin
  result := 0;
  ///

  for ARegion in FRegions.Values do
    Inc(result, ARegion.Size);
end;

end.
