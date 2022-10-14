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

unit uFrameDebugEvents;

interface

uses
  Winapi.Windows, Winapi.Messages, System.SysUtils, System.Variants, System.Classes,
  Vcl.Graphics, Vcl.Controls, Vcl.Forms, Vcl.Dialogs, VirtualTrees, XSuperObject,
  Vcl.ExtCtrls, OMultiPanel, uFrameEventDetail, Generics.Collections;

type
  TTreeData = record
    Resolved            : Boolean;
    Data                : ISuperObject;
    ProcessImgIdx       : Integer;
    EventKindImgIdx     : Integer;
    ParentProcessImgIdx : Integer;
  end;
  PTreeData = ^TTreeData;

  TFrameDebugEvents = class(TFrame)
    MultiPanel: TOMultiPanel;
    PanelDetail: TPanel;
    PanelList: TPanel;
    VST: TVirtualStringTree;
    procedure VSTChange(Sender: TBaseVirtualTree; Node: PVirtualNode);
    procedure VSTFocusChanged(Sender: TBaseVirtualTree; Node: PVirtualNode;
      Column: TColumnIndex);
    procedure VSTGetImageIndex(Sender: TBaseVirtualTree; Node: PVirtualNode;
      Kind: TVTImageKind; Column: TColumnIndex; var Ghosted: Boolean;
      var ImageIndex: TImageIndex);
    procedure VSTBeforeCellPaint(Sender: TBaseVirtualTree;
      TargetCanvas: TCanvas; Node: PVirtualNode; Column: TColumnIndex;
      CellPaintMode: TVTCellPaintMode; CellRect: TRect; var ContentRect: TRect);
    procedure VSTGetText(Sender: TBaseVirtualTree; Node: PVirtualNode;
      Column: TColumnIndex; TextType: TVSTTextType; var CellText: string);
    procedure VSTGetNodeDataSize(Sender: TBaseVirtualTree;
      var NodeDataSize: Integer);
  private
    FEventDetailFrame         : TFrameEventDetail;
    FUnresolvedProcessTracker : TList<PTreeData>;
    FDLLTracker               : TDictionary<Pointer, String>; // DllBase, DllImagePath
  public
    {@M}
    procedure DisplayDebugEvent(const ADebugEvent : TDebugEvent);
    procedure Reset();

    {@C}
    constructor Create(AOwner : TComponent); override;
    destructor Destroy(); override;
  end;

implementation

uses uFormMain, uFunctions, uExceptions, uEventUtils, uConstants;

{$R *.dfm}

procedure TFrameDebugEvents.Reset();
begin
  VST.Clear();

  self.MultiPanel.PanelCollection.Items[1].Visible := False;

  self.FEventDetailFrame.Reset();
end;

constructor TFrameDebugEvents.Create(AOwner : TComponent);
begin
  inherited Create(AOwner);
  ///

  FEventDetailFrame := TFrameEventDetail.Create(PanelDetail);
  FEventDetailFrame.Parent := PanelDetail;

  FUnresolvedProcessTracker := TList<PTreeData>.Create();
  FDLLTracker := TDictionary<Pointer, String>.Create();
end;

destructor TFrameDebugEvents.Destroy();
begin
  if Assigned(FEventDetailFrame) then
    FreeAndNil(FEventDetailFrame);

  if Assigned(FUnresolvedProcessTracker) then
    FreeAndNil(FUnresolvedProcessTracker);

  if Assigned(FDLLTracker) then
    FreeAndNil(FDLLTracker);

  ///
  inherited Destroy();
end;

procedure TFrameDebugEvents.VSTBeforeCellPaint(Sender: TBaseVirtualTree;
  TargetCanvas: TCanvas; Node: PVirtualNode; Column: TColumnIndex;
  CellPaintMode: TVTCellPaintMode; CellRect: TRect; var ContentRect: TRect);
var pData  : PTreeData;
begin
  if FormMain.DisplayEventColors1.Checked then begin
    pData := Node.GetData;
    if not Assigned(pData) then
      Exit();

    TargetCanvas.Brush.Style := bsSolid;
    TargetCanvas.Brush.Color := EventKindToColor(pData^.Data.I[_EVENT_KIND]);

    TargetCanvas.FillRect(CellRect);
  end;
end;

procedure TFrameDebugEvents.VSTChange(Sender: TBaseVirtualTree;
  Node: PVirtualNode);
begin
  VST.Refresh();
end;

procedure TFrameDebugEvents.VSTFocusChanged(Sender: TBaseVirtualTree;
  Node: PVirtualNode; Column: TColumnIndex);
var pData : PTreeData;
begin
  VST.Refresh();

  MultiPanel.PanelCollection.Items[1].Visible := Assigned(Node);
  ///

  if Assigned(Node) then begin
    pData := Node.GetData;

    if Assigned(pData) then
      self.FEventDetailFrame.LoadJsonNode(pData^.Data);
  end;
end;

procedure TFrameDebugEvents.VSTGetImageIndex(Sender: TBaseVirtualTree;
  Node: PVirtualNode; Kind: TVTImageKind; Column: TColumnIndex;
  var Ghosted: Boolean; var ImageIndex: TImageIndex);
  var pData : PTreeData;
begin
  pData := Node.GetData;
  if not Assigned(pData) then
    Exit();

  case Kind of
    ikNormal,
    ikSelected: begin
      case Column of
        0: ImageIndex := pData^.ProcessImgIdx;
        4: ImageIndex := pData^.ParentProcessImgIdx;
      end;
    end;

    ikState: begin
      case Column of
        2: ImageIndex := pData^.EventKindImgIdx;
      end;
    end;
  end;
end;

procedure TFrameDebugEvents.VSTGetNodeDataSize(Sender: TBaseVirtualTree;
  var NodeDataSize: Integer);
begin
  NodeDataSize := SizeOf(TTreeData);
end;

procedure TFrameDebugEvents.VSTGetText(Sender: TBaseVirtualTree;
  Node: PVirtualNode; Column: TColumnIndex; TextType: TVSTTextType;
  var CellText: string);
var pData : PTreeData;
begin
  pData := Node.GetData;
  if not Assigned(pData) then
    Exit();

  case Column of
    0 : CellText := Format('%s (%d)', [
      ExtractFileName(pData^.Data.S[_IMAGE_PATH]),
      pData^.Data.I[_PROCESS_ID]]
    );

    1 : CellText := Format('%d (0x%.8x)', [
      pData^.Data.I[_THREAD_ID],
      pData^.Data.I[_THREAD_ID]
    ]);

    2 : CellText := EventKindToString(pData^.Data.I[_EVENT_KIND]);
    3 : CellText := pData^.Data.AsJSON();
    4 : CellText := ExtractFileName(pData^.Data.S[_PARENT_IMAGE_PATH]);
    5 : CellText := IntToStr(pData^.Data.I[_PARENT_PROCESS_ID]);
    6 : CellText := pData^.Data.S[_IMAGE_PATH];
    7 : CellText := DateTimeToStr(pData^.Data.D[_EVENT_DATE_TIME]);
  end;
end;

procedure TFrameDebugEvents.DisplayDebugEvent(const ADebugEvent : TDebugEvent);
var pNode              : PVirtualNode;
    pData              : PTreeData;
    AImagePath         : String;
    AParentImagePath   : String;
    AString            : String;
    hProcess           : THandle;
    pCurrentException  : PExceptionRecord;
    AJsonArray         : ISuperArray;
    AJsonObject        : ISuperObject;
    AResolvedProcess   : TList<PTreeData>;
    pResolveData       : PTreeData;
    AHandle            : THandle;

    function ReadRemoteString(const pOffset : Pointer; const ALength : Cardinal; const AUnicode : Boolean) : String; overload;
    var ABytesRead : SIZE_T;
        pBuffer    : Pointer;
    begin
      try
        GetMem(pBuffer, ALength);
        try
          if not ReadProcessMemory(hProcess, pOffset, pBuffer, ALength, ABytesRead) then
            raise EWindowsException.Create('ReadProcessMemory');

          if not AUnicode then
            SetString(result, PAnsiChar(pBuffer), ALength)
          else
            SetString(result, PWideChar(pBuffer), ALength);
        finally
          FreeMem(pBuffer, ALength);
        end;
      except
        result := 'n/a';
      end;
    end;

    function IsMemoryNull(const pOffset : Pointer; const ARegionSize : Cardinal) : Boolean;
    var I : Integer;
    begin
      if (pOffset = nil) or (ARegionSize = 0) then
        raise Exception.Create('Memory offset must not be null and size above zero.');

      result := True;

      for I := 0 to ARegionSize -1 do begin
        if PByte(NativeUInt(pOffset) + I)^ <> 0 then begin
          result := False;

          break;
        end;
      end;
    end;

    function ReadRemoteString(const pBaseOffset : Pointer; const AUnicode : Boolean) : String; overload;
    var pBuffer    : Pointer;
        ALength    : Cardinal;
        I          : Cardinal;
        pOffset    : Pointer;
        ABytesRead : SIZE_T;
        ACharacter : array of byte;
    begin
      result := 'n/a';
      if pBaseOffset = nil then
        Exit();
      try
        ALength := MAX_PATH * 2;
        ///

        if AUnicode then
          SetLength(ACharacter, SizeOf(WideChar))
        else
          SetLength(ACharacter, SizeOf(AnsiChar));

        GetMem(pBuffer, ALength); // Might be sufficient
        try
          ZeroMemory(pBuffer, ALength);
          ///

          I := 0;
          while True do begin
            pOffset := Pointer(NativeUInt(pBaseOffset) + I);
            ///

            if not ReadProcessMemory(hProcess, pOffset, PByte(ACharacter), Length(ACharacter), ABytesRead) then
              raise EWindowsException.Create('ReadProcessMemory');

            // Null-Byte Exit
            if IsMemoryNull(ACharacter, Length(ACharacter)) then
              break;

            CopyMemory(Pointer(NativeUInt(pBuffer) + I), PByte(ACharacter), Length(ACharacter));

            Inc(I, Length(ACharacter));

            // Emergency Exit
            if I >= ALength -1 then
              break;
          end;

          ///
          if not AUnicode then
            SetString(result, PAnsiChar(pBuffer), ALength)
          else
            SetString(result, PWideChar(pBuffer), ALength);
        finally
          FreeMem(pBuffer, ALength);
        end;
      except
      end;
    end;

    function ReadRemoteString_PTRPTR(const pBaseOffset : Pointer; const AUnicode : Boolean) : String;
    var AStringOffset : NativeUInt;
        ABytesRead    : SIZE_T;
    begin
      result := 'n/a';
      if pBaseOffset = nil then
        Exit();
      try
        // Get String Pointer
        if not ReadProcessMemory(hProcess, pBaseOffset, @AStringOffset, SizeOf(Pointer), ABytesRead) then
          raise EWindowsException.Create('ReadProcessMemory');
        ///

        // Read String Content
        if AStringOffset > 0 then
          result := ReadRemoteString(Pointer(AStringOffset), AUnicode);
      except
      end;
    end;

begin
  hProcess := 0;

  VST.BeginUpdate();
  try
    hProcess := OpenProcess(PROCESS_VM_READ, False, ADebugEvent.dwProcessId);
    if hProcess = 0 then
      raise EWindowsException.Create('OpenProcess');
    ///

    pNode := VST.AddChild(nil);

    pData := pNode.GetData;

    // Resolve Image Path
    try
      AImagePath := GetImagePathFromProcessId(ADebugEvent.dwProcessId);

      pData^.ProcessImgIdx := SystemFileIcon(AImagePath);

      pData^.Resolved := True;
    except
      pData^.ProcessImgIdx := FormMain.DefaultExeIconIndex;

      pData^.Resolved := False;

      ///
      FUnresolvedProcessTracker.Add(pData);
    end;

    pData^.Data := SO();

    // Generic block of information
    pData^.Data.I[_PROCESS_ID] := ADebugEvent.dwProcessId;
    pData^.Data.S[_IMAGE_PATH] := AImagePath;
    pData^.Data.I[_THREAD_ID] := ADebugEvent.dwThreadId;
    pData^.Data.I[_EVENT_KIND] := ADebugEvent.dwDebugEventCode;
    pData^.Data.D[_EVENT_DATE_TIME] := Now();
    pData^.Data.I[_PARENT_PROCESS_ID] := GetParentProcessIdByProcessId(ADebugEvent.dwProcessId);
    pData^.EventKindImgIdx := EventKindToImageIndex(ADebugEvent.dwDebugEventCode);

    // Resolve Parent Image Path
    pData^.ParentProcessImgIdx := -1;
    try
      AParentImagePath := GetImagePathFromProcessId(pData^.Data.I[_PARENT_PROCESS_ID]);

      pData^.ParentProcessImgIdx := SystemFileIcon(AParentImagePath)
    except
      AParentImagePath := '-';
    end;

    pData^.Data.S[_PARENT_IMAGE_PATH] := AParentImagePath;

    // Event extended information block
    case ADebugEvent.dwDebugEventCode of
      CREATE_PROCESS_DEBUG_EVENT : begin
        pData^.Data.I[_FILE_HANDLE] := ADebugEvent.CreateProcessInfo.hFile;
        pData^.Data.I[_PROCESS_HANDLE] := ADebugEvent.CreateProcessInfo.hProcess;
        pData^.Data.I[_THREAD_HANDLE] := ADebugEvent.CreateProcessInfo.hThread;
        pData^.Data.I[_BASE_OF_IMAGE] := NativeUInt(ADebugEvent.CreateProcessInfo.lpBaseOfImage);
        pData^.Data.I[_DEBUG_INFO_FILE_OFFSET] := ADebugEvent.CreateProcessInfo.dwDebugInfoFileOffset;
        pData^.Data.I[_DEBUG_INFO_SIZE] := ADebugEvent.CreateProcessInfo.nDebugInfoSize;
        pData^.Data.I[_THREAD_LOCAL_BASE] := NativeUInt(ADebugEvent.CreateProcessInfo.lpThreadLocalBase);
        pData^.Data.I[_START_ADDRESS] := NativeUInt(ADebugEvent.CreateProcessInfo.lpStartAddress);

        pData^.Data.I[_IMAGE_NAME] := NativeUInt(ADebugEvent.CreateProcessInfo.lpImageName);
        pData^.Data.B[_UNICODE] := (ADebugEvent.CreateProcessInfo.fUnicode > 0);

        pData^.Data.S[_RESOLVED_IMAGE_NAME] := ReadRemoteString_PTRPTR(
                                                                    ADebugEvent.CreateProcessInfo.lpImageName,
                                                                    pData^.Data.B[_UNICODE]
        );
      end;

      CREATE_THREAD_DEBUG_EVENT : begin
        pData^.Data.I[_THREAD_HANDLE] := ADebugEvent.CreateThread.hThread;
        pData^.Data.I[_THREAD_LOCAL_BASE] := NativeUInt(ADebugEvent.CreateThread.lpThreadLocalBase);
        pData^.Data.I[_START_ADDRESS] := NativeUInt(ADebugEvent.CreateThread.lpStartAddress);
      end;

      EXCEPTION_DEBUG_EVENT : begin
        pData^.Data.I[_FIRST_CHANCE] := ADebugEvent.Exception.dwFirstChance;

        AJsonArray := SA();

        pCurrentException := PExceptionRecord(@ADebugEvent.Exception.ExceptionRecord);
        repeat
          AJsonObject := SO();

          AJsonObject.I[_EXCEPTION_CODE] := pCurrentException^.ExceptionCode;
          case pCurrentException^.ExceptionCode of
            EXCEPTION_ACCESS_VIOLATION         : AString := 'Access Violation';
            EXCEPTION_ARRAY_BOUNDS_EXCEEDED    : AString := 'Array Bounds Exceeded';
            EXCEPTION_BREAKPOINT               : AString := 'Breakpoint';
            EXCEPTION_DATATYPE_MISALIGNMENT    : AString := 'Data Type Misalignment';
            EXCEPTION_FLT_DENORMAL_OPERAND     : AString := 'Floating Point Denormal Operand';
            EXCEPTION_FLT_DIVIDE_BY_ZERO       : AString := 'Floating Point Division By Zero';
            EXCEPTION_FLT_INEXACT_RESULT       : AString := 'Floating Point Inexact Result';
            EXCEPTION_FLT_INVALID_OPERATION    : AString := 'Floating Point Invalid Operation';
            EXCEPTION_FLT_OVERFLOW             : AString := 'Floating Point Overflow';
            EXCEPTION_FLT_STACK_CHECK          : AString := 'Floating Point Stack Overflow';
            EXCEPTION_FLT_UNDERFLOW            : AString := 'Floating Point Underflow';
            EXCEPTION_ILLEGAL_INSTRUCTION      : AString := 'Illegal Instruction';
            EXCEPTION_IN_PAGE_ERROR            : AString := 'Page Error';
            EXCEPTION_INT_DIVIDE_BY_ZERO       : AString := 'Division By Zero';
            EXCEPTION_INT_OVERFLOW             : AString := 'Integer Overflow';
            EXCEPTION_INVALID_DISPOSITION      : AString := 'Invalid Disposition';
            EXCEPTION_NONCONTINUABLE_EXCEPTION : AString := 'Non Continuable Exception';
            EXCEPTION_PRIV_INSTRUCTION         : AString := 'Invalid Instruction';
            EXCEPTION_SINGLE_STEP              : AString := 'Single Step';
            EXCEPTION_STACK_OVERFLOW           : AString := 'Stack Overflow';
            else
              AString := 'Unknown';
          end;

          AJsonObject.S[_EXCEPTION_CODE_MEANING] := AString;

          AJsonObject.I[_EXCEPTION_FLAGS] := pCurrentException^.ExceptionFlags;
          case pCurrentException^.ExceptionFlags of
            EXCEPTION_NONCONTINUABLE     : AString := 'Non Continuable';
            else
              AString := 'Reserved';
          end;

          AJsonObject.I[_EXCEPTION_ADDRESS] := NativeUInt(pCurrentException^.ExceptionAddress);
          AJsonObject.I[_NUMBER_PARAMETERS] := pCurrentException^.NumberParameters;
          AJsonObject.I[_EXCEPT_ADDR] := NativeUInt(pCurrentException^.ExceptAddr);
          AJsonObject.I[_EXCEPT_OBJECT] := NativeUInt(pCurrentException^.ExceptObject);
          AJsonObject.I[_NEXT_EXCEPTION] := NativeUInt(pCurrentException^.ExceptionRecord);

          AJsonObject.S[_EXCEPTION_FLAGS_MEANING] := AString;

          AJsonArray.Add(AJsonObject);

          ///

          // TODO: Find a way to test chained-exceptions. Do we need to ReadMemory for next
          // Exception record ???
          pCurrentException := pCurrentException^.ExceptionRecord;
        until pCurrentException = nil;

        pData^.Data.A[_EXCEPTION_CHAIN] := AJsonArray;
      end;

      EXIT_PROCESS_DEBUG_EVENT : begin
        pData^.Data.I[_EXIT_CODE] := ADebugEvent.ExitProcess.dwExitCode;
      end;

      EXIT_THREAD_DEBUG_EVENT : begin
        pData^.Data.I[_EXIT_CODE] := NativeUInt(ADebugEvent.ExitThread.dwExitCode);
      end;

      LOAD_DLL_DEBUG_EVENT : begin
        pData^.Data.I[_FILE_HANDLE] := ADebugEvent.LoadDll.hFile;
        pData^.Data.I[_BASE_OF_DLL] := NativeUInt(ADebugEvent.LoadDll.lpBaseOfDll);
        pData^.Data.I[_DEBUG_INFO_FILE_OFFSET] := ADebugEvent.LoadDll.dwDebugInfoFileOffset;
        pData^.Data.I[_DEBUG_INFO_SIZE] := ADebugEvent.LoadDll.nDebugInfoSize;
        pData^.Data.I[_IMAGE_NAME] := NativeUInt(ADebugEvent.LoadDll.lpImageName);
        pData^.Data.B[_UNICODE] := (ADebugEvent.LoadDll.fUnicode > 0);

        pData^.Data.S[_RESOLVED_IMAGE_NAME] := ReadRemoteString_PTRPTR(
                                                                    ADebugEvent.LoadDll.lpImageName,
                                                                    pData^.Data.B[_UNICODE]
        );

        // TODO For tracing API calls
        //if not VirtualProtectEx(hProcess, ADebugEvent.LoadDll.lpBaseOfDll, 100, PAGE_EXECUTE_READWRITE or PAGE_NOACCESS, AOldProtect) then
        //  Exit();

        ///
        if FDLLTracker.ContainsKey(ADebugEvent.LoadDll.lpBaseOfDll) then
          FDLLTracker.Items[ADebugEvent.LoadDll.lpBaseOfDll] := pData^.Data.S[_RESOLVED_IMAGE_NAME]
        else
          FDLLTracker.Add(ADebugEvent.LoadDll.lpBaseOfDll, pData^.Data.S[_RESOLVED_IMAGE_NAME]);
      end;

      OUTPUT_DEBUG_STRING_EVENT : begin
        pData^.Data.S[_MESSAGE] := ReadRemoteString(
                                                      ADebugEvent.DebugString.lpDebugStringData,
                                                      ADebugEvent.DebugString.nDebugStringLength,
                                                      (ADebugEvent.DebugString.fUnicode > 0)
        );
      end;

      RIP_EVENT : begin
        pData^.Data.I[_ERROR] := ADebugEvent.RipInfo.dwError;
        pData^.Data.I[_TYPE] := ADebugEvent.RipInfo.dwType;
      end;

      UNLOAD_DLL_DEBUG_EVENT : begin
        pData^.Data.I[_BASE_OF_DLL] := NativeUInt(ADebugEvent.UnloadDll.lpBaseOfDll);

        if FDLLTracker.ContainsKey(ADebugEvent.UnloadDll.lpBaseOfDll) then begin
          if FDLLTracker.TryGetValue(ADebugEvent.UnloadDll.lpBaseOfDll, AString) then
            pData^.Data.S[_RESOLVED_IMAGE_NAME] := AString;

          ///
          FDLLTracker.Remove(ADebugEvent.UnloadDll.lpBaseOfDll);
        end;
      end;
    end;

    /// Fix unresolved process
    if FUnresolvedProcessTracker.Count > 0 then begin
      AResolvedProcess := TList<PTreeData>.Create();
      try
        for pResolveData in FUnresolvedProcessTracker do begin
          if not Assigned(pResolveData) then
            continue;

          if (pResolveData^.Data.I[_PROCESS_ID] = pData^.Data.I[_PROCESS_ID]) and (pData^.Resolved) then begin
            pResolveData^.ProcessImgIdx := pData^.ProcessImgIdx;

            pResolveData^.Data.S[_IMAGE_PATH] := pData^.Data.S[_IMAGE_PATH];

            ///
            AResolvedProcess.Add(pResolveData);
          end;
        end;

        // Purge List
        for pResolveData in AResolvedProcess do begin
          FUnresolvedProcessTracker.Remove(pResolveData);
        end;
      finally
        if Assigned(AResolvedProcess) then
          FreeAndNil(AResolvedProcess);
      end;
    end;

  finally
    if hProcess <> 0 then
      CloseHandle(hProcess);

    VST.EndUpdate();
  end;
end;

end.
