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

unit uMemoryMapThread;

interface

uses
  System.Classes,
  System.SysUtils,
  Winapi.Windows,
  Winapi.PsAPI,
  VirtualTrees,
  uFrameMemoryMap,
  uMemoryMap;

type
  TMemoryMapThread = class(TThread)
  private
    FProcessId : Cardinal;
    FMemoryMap : TMemoryMap;
    FView      : TFrameMemoryMap;

    {@M}
    procedure Enum();
    procedure Display();
  protected
    procedure Execute; override;
  public
    {@C}
    constructor Create(const AProcessId : Cardinal; const AView : TFrameMemoryMap) overload;
  end;

implementation

uses uExceptions, uFunctions, uFormMain;


{ TMemoryMapThread.Enum }
procedure TmemoryMapThread.Enum();
var hProcess                : THandle;
    AMemoryBasicInformation : TMemoryBasicInformation;
    AMemoryRegion           : TMemoryRegion;
    ASize                   : Cardinal;
    AOldMemoryBase          : Pointer;
    AMappedName             : String;
    ALength                 : Cardinal;
begin
  if not Assigned(FMemoryMap) then
    Exit();
  ///

  hProcess := OpenProcess(PROCESS_QUERY_LIMITED_INFORMATION or PROCESS_VM_READ, False, FProcessId);
  if hProcess = 0 then
    raise EWindowsException.Create('OpenProcess');
  try
    ASize := SizeOf(TMemoryBasicInformation);
    ///

    AOldMemoryBase := nil;

    ZeroMemory(@AMemoryBasicInformation, ASize);
    while VirtualQueryEx(hProcess, AMemoryBasicInformation.BaseAddress, AMemoryBasicInformation, ASize) = ASize do begin
      //if AMemoryBasicInformation.RegionSize <= 0 then
      //   break;
      try
        // New Memory Region
        if AOldMemoryBase <> AMemoryBasicInformation.AllocationBase then begin
          try
            AMappedName := DELF_GetMappedFileName(hProcess, AMemoryBasicInformation.AllocationBase);
          except
            AMappedName := '';
          end;

          AMemoryRegion := FMemoryMap.AddRegion(AMappedName, AMemoryBasicInformation.AllocationBase);
          if Assigned(AMemoryRegion) then
            AMemoryRegion.ProbeRegion(hProcess);

          ///
          AOldMemoryBase := AMemoryBasicInformation.AllocationBase;
        end;

        // New Memory Region Page
        FMemoryMap.AddPage(AMemoryBasicInformation.AllocationBase, TMemoryPage.Create(AMemoryBasicInformation));
      finally
        AMemoryBasicInformation.BaseAddress := Pointer(
          NativeUInt(AMemoryBasicInformation.BaseAddress) + AMemoryBasicInformation.RegionSize
        );
      end;
    end;
  finally
    if hProcess <> 0 then
      CloseHandle(hProcess);
  end;
end;

{ TMemoryMapThread.Display }
procedure TMemoryMapThread.Display();
var ARegionNode  : PVirtualNode;
    APageNode    : PVirtualNode;
    pData        : PTreeData;
    ARegion      : TMemoryRegion;
    pBaseAddress : Pointer;
    AVPath       : String;
    APage        : TMemoryPage;
begin
  if not Assigned(FMemoryMap) then
    Exit();
  ///

  if FMemoryMap.Regions.Count = 0 then
    Exit();
  ///

  Synchronize(procedure begin
    FView.VST.Clear();

    FView.VST.BeginUpdate();

    FView.MemorySize := FMemoryMap.Size;
  end);
  try
    for pBaseAddress in FMemoryMap.Regions.Keys do begin
      if not FMemoryMap.Regions.TryGetValue(pBaseAddress, ARegion) then
        continue;
      ///

      { Draw Memory Region }
      ARegionNode := FView.VST.AddChild(nil);

      pData := ARegionNode.GetData;

      pData^.RegionName   := ARegion.Name;
      pData^.BaseAddress  := ARegion.BaseAddress;
      pData^.FormatedSize := ARegion.FormatedSize;
      pData^.Size         := ARegion.Size;
      pData^.Page         := nil;

      AVPath := PhysicalToVirtualPath(pData^.RegionName);

      if FileExists(AVPath) and (not AVPath.IsEmpty) then
        pData^.ImageIndex := SystemFileIcon(AVPath)
      else
        pData^.ImageIndex := -1;

      { Draw Memory Region Page }
      for APage in ARegion.Pages do begin
        APageNode := FView.VST.AddChild(ARegionNode);

        pData := APageNode.GetData;

        pData^.RegionName   := APage.Name;
        pData^.BaseAddress  := ARegion.BaseAddress;
        pData^.FormatedSize := APage.FormatedSize;
        pData^.Size         := APage.Size;

        pData^.Page := TMemoryPage.Create();
        pData^.Page.Assign(APage);

        pData^.ImageIndex := -1;
      end;
    end;
  finally
    Synchronize(procedure begin
      FView.VST.FullExpand();

      FView.VST.EndUpdate();

      FreeAndNil(FMemoryMap);
    end);
  end;
end;

{ TMemoryMapThread.Execute }
procedure TMemoryMapThread.Execute;
begin
  try
    if not Assigned(FView) then
      Exit();
    ///
    FMemoryMap := TMemoryMap.Create();
    try
      self.Enum();

      self.Display();
    finally
      if Assigned(FMemoryMap) then
        FreeAndNil(FMemoryMap);
    end;
  except
    on E : Exception do begin
      FormMain.OnException(self, E);
    end;
  end;
end;

constructor TMemoryMapThread.Create(const AProcessId : Cardinal; const AView : TFrameMemoryMap);
begin
  inherited Create(False);
  ///

  FProcessId := AProcessId;
  FView      := AView;

  FMemoryMap := nil;
end;

end.
