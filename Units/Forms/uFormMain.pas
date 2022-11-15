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

// Improve Exception Notification System (Overall Exception System)
// Use TPsyloThread instead of TThread for previous features and implement them in Thread Watcher System.
// Code terminate debugging on parent process end.
// TThreadList is badly designed for Thread Worker Manager. Fix that!

unit uFormMain;

interface

uses
  Winapi.Windows, Winapi.Messages, System.SysUtils, System.Variants, System.Classes, Vcl.Graphics,
  Vcl.Controls, Vcl.Forms, Vcl.Dialogs, Vcl.Menus, VirtualTrees, Vcl.ExtCtrls,
  OMultiPanel, System.ImageList, Vcl.ImgList, Winapi.ShellAPI, System.Generics.Collections,
  Vcl.VirtualImageList, Vcl.BaseImageCollection, Vcl.ImageCollection, XSuperObject,
  Winapi.ShLwApi, Winapi.GDIPAPI, uFrameDebugEvents, Vcl.ComCtrls, uDebuggerThread,
  Vcl.StdCtrls, Vcl.ToolWin, Vcl.WinXCtrls, uFrameMemoryMap, uFrameLog,
  uFrameComponentAlert, uFrameModules, uTypes, uDebugSession;

type
  TFormMain = class(TForm)
    MainMenu: TMainMenu;
    File1: TMenuItem;
    ImageSystem: TImageList;
    ImageCollection: TImageCollection;
    VirtualImageList: TVirtualImageList;
    View1: TMenuItem;
    DisplayEventColors1: TMenuItem;
    Pages: TPageControl;
    TabDebugEvents: TTabSheet;
    DebugFile1: TMenuItem;
    DebugApplicationAdvanced1: TMenuItem;
    N1: TMenuItem;
    Exit1: TMenuItem;
    Debug1: TMenuItem;
    Pause1: TMenuItem;
    Stop1: TMenuItem;
    N2: TMenuItem;
    Restart1: TMenuItem;
    ToolBar1: TToolBar;
    ToolDebugApplication: TToolButton;
    ToolAttachProcess: TToolButton;
    ToolButton3: TToolButton;
    ToolPauseResumeDebug: TToolButton;
    ToolStopDebug: TToolButton;
    ToolRestartDebug: TToolButton;
    ToolButton7: TToolButton;
    About1: TMenuItem;
    TabMemoryMap: TTabSheet;
    TabLog: TTabSheet;
    N3: TMenuItem;
    theme1: TMenuItem;
    appearance1: TMenuItem;
    General1: TMenuItem;
    OnSessionCloseClearAll1: TMenuItem;
    FrameComponentAlert1: TFrameComponentAlert;
    ToolButton1: TToolButton;
    ToolClear: TToolButton;
    TabModules: TTabSheet;
    ToolButton2: TToolButton;
    ToolButton4: TToolButton;
    ToolButtonRepo: TToolButton;
    ToolButton5: TToolButton;
    ToolProcessTree: TToolButton;
    ToolButton6: TToolButton;
    ToolButton8: TToolButton;
    ToolDumpPE: TToolButton;
    procedure FormCreate(Sender: TObject);
    procedure DisplayEventColors1Click(Sender: TObject);
    procedure Exit1Click(Sender: TObject);
    procedure DebugApplicationAdvanced1Click(Sender: TObject);
    procedure DebugFile1Click(Sender: TObject);
    procedure ToolDebugApplicationClick(Sender: TObject);
    procedure ToolAttachProcessClick(Sender: TObject);
    procedure Stop1Click(Sender: TObject);
    procedure ToolStopDebugClick(Sender: TObject);
    procedure Restart1Click(Sender: TObject);
    procedure ToolRestartDebugClick(Sender: TObject);
    procedure About1Click(Sender: TObject);
    procedure PagesChange(Sender: TObject);
    procedure FormShow(Sender: TObject);
    procedure FormResize(Sender: TObject);
    procedure ToolClearClick(Sender: TObject);
    procedure ToolButton4Click(Sender: TObject);
    procedure ToolButtonRepoClick(Sender: TObject);
    procedure ToolProcessTreeClick(Sender: TObject);
    procedure FormClose(Sender: TObject; var Action: TCloseAction);
    procedure ToolButton6Click(Sender: TObject);
    procedure ToolDumpPEClick(Sender: TObject);
  private
    FFileInfo                  : TSHFileInfo;
    FDefaultExeIconIndex       : Integer;
    FDebugger                  : TDebuggerThread;
    FDebugMode                 : TDebugMode;
    FFrameDebugEvents          : TFrameDebugEvents;
    FFrameMemoryMap            : TFrameMemoryMap;
    FFrameModules              : TFrameModules;
    FFrameLog                  : TFrameLog;
    FLastDebuggedProcessId     : Cardinal;
    FDebugSessionInformation   : TDebugSessionInformation;

    {@M}
    procedure OnDebuggerStatusChange(Sender: TDebuggerThread; const ADebuggerStatus : TDebuggerStatus);
    procedure RestartDebugger();
    procedure CloseSession(const AResetData : Boolean = True);
    procedure ResetData();
    function GetDebugging() : Boolean;
    procedure RefreshVCLThemes();
    procedure OnThemeChange(Sender: TObject);
    procedure OnSessionCreateProcess(Sender : TObject; const ADebugProcess : TDebugProcess);
    procedure OnSessionExitProcess(Sender : TObject; const ADebugProcess : TDebugProcess);
  public
    {@M}
    procedure OnException(Sender : TObject; E : Exception);
    procedure DebugApplication(const AFileName, AProcessArguments : String; const ADebugChild, AShowProcess : Boolean);
    procedure AttachProcess(const AProcessId : Cardinal);
    procedure Log(const AMessage : String; const Sender: TObject; const ALevel : TLogLevel);
    procedure SetFormCaption(ACaption : String = '');

    {@C}
    constructor Create(AOwner : TComponent); override;
    destructor Destroy(); override;

    {@G}
    property DefaultExeIconIndex       : Integer                  read FDefaultExeIconIndex;
    property DebugEvents               : TFrameDebugEvents        read FFrameDebugEvents;
    property DebugSessionInformation   : TDebugSessionInformation read FDebugSessionInformation;
    property Debugging                 : Boolean                  read GetDebugging;
  end;

var
  FormMain: TFormMain;

implementation

{$R *.dfm}

uses uFunctions, uExceptions, uEventUtils, uConstants, uFormAttachToProcess,
     uFormDebugApplication, uFormThreadOperation, VCL.Styles, VCL.Themes,
     uFormPsyloThreadManager, uFormAbout, uFormDebugProcessTree,
     uFormDumpMemoryImage;

procedure TFormMain.OnSessionCreateProcess(Sender : TObject; const ADebugProcess : TDebugProcess);
begin
  FormDebugProcessTree.AddProcess(ADebugProcess);
end;

procedure TFormMain.OnSessionExitProcess(Sender : TObject; const ADebugProcess : TDebugProcess);
begin
  FormDebugProcessTree.RemoveProcess(ADebugProcess);
end;

procedure TFormMain.SetFormCaption(ACaption : String = '');
begin
  if not ACaption.IsEmpty then
    ACaption := ' ' + ACaption;
  ///

  self.Caption := Format('%s v%s%s - %s', [
    APP_NAME,
    APP_VERSION,
    ACaption,
    GetElevationLabel
  ]);
end;

function TFormMain.GetDebugging() : Boolean;
begin
  result := Assigned(FDebugSessionInformation);
end;

procedure TFormMain.ResetData();
begin
  FrameComponentAlert1.ResetData();
  FFrameDebugEvents.ResetData();
  FFrameMemoryMap.ResetData();
  FFrameModules.ResetData();
end;

procedure TFormMain.CloseSession(const AResetData : Boolean = True);
begin
  if Assigned(FDebugSessionInformation) then begin
    self.FrameComponentAlert1.SetMessage(
      Format('Current displayed data comes from a terminated debugging session (pid: %d).', [
        FDebugSessionInformation.ProcessId
      ])
    , akInformation, True);

    FreeAndNil(FDebugSessionInformation);

    FormDebugProcessTree.ResetData();

    ///
    self.Resize();
  end;
  ///

  if AResetData then
    self.ResetData();
end;

procedure TFormMain.Log(const AMessage : String; const Sender: TObject; const ALevel : TLogLevel);
begin
  if Assigned(FFrameLog) then
    FFrameLog.Log(AMessage, Sender, ALevel);
end;

procedure TFormMain.OnException(Sender : TObject; E : Exception);
begin
  Log(E.Message, Sender, llException);

  if E is EPsyloException then
    if EPsyloException(E).Display then
      Application.MessageBox(PWideChar(E.Message), 'Exception', MB_ICONHAND);
end;

procedure TFormMain.PagesChange(Sender: TObject);
var AIndex : Cardinal;
begin
  AIndex := TPageControl(Sender).ActivePageIndex;
  ///

  if AIndex = TabMemoryMap.TabIndex then
    FFrameMemoryMap.Refresh()
  else if AIndex = TabModules.TabIndex then
    FFrameModules.Refresh();
end;

procedure TFormMain.OnDebuggerStatusChange(Sender: TDebuggerThread; const ADebuggerStatus : TDebuggerStatus);
begin
  self.Debug1.Enabled                    := (ADebuggerStatus <> dbgStop);
  self.DebugFile1.Enabled                := not self.Debug1.Enabled;
  self.DebugApplicationAdvanced1.Enabled := not self.Debug1.Enabled;

  self.ToolDebugApplication.Enabled      := not self.Debug1.Enabled;
  self.ToolAttachProcess.Enabled         := not self.Debug1.Enabled;
  self.ToolPauseResumeDebug.Enabled      := self.Debug1.Enabled;
  self.ToolStopDebug.Enabled             := self.Debug1.Enabled;
  self.ToolRestartDebug.Enabled          := self.Debug1.Enabled;
  self.ToolDumpPE.Enabled                := self.Debug1.Enabled;

  case ADebuggerStatus of
    dbgStart: begin
      self.CloseSession();
    end;

    dbgStop: begin
      Log(Format('Debugger has stopped debugging process: %d', [Sender.ProcessId]), self, llInteruption);

      ///
      self.CloseSession(OnSessionCloseClearAll1.Checked);
    end;

    dbgProcessAttached: begin
      Log(Format('Debugger has attached to process: %d', [Sender.ProcessId]), self, llSuccess);

      FDebugSessionInformation := TDebugSessionInformation.Create(Sender.ProcessId);
      FDebugSessionInformation.OnCreateProcess := OnSessionCreateProcess;
      FDebugSessionInformation.OnExitProcess   := OnSessionExitProcess;

      self.SetFormCaption(Format('(Debugging: %d)', [
        FDebugSessionInformation.ProcessId
      ]));

      ///
      self.Pages.ActivePage := self.TabDebugEvents;
    end;
  end;
end;

procedure TFormMain.Restart1Click(Sender: TObject);
begin
  self.RestartDebugger();
end;

procedure TFormMain.Stop1Click(Sender: TObject);
begin
  if Assigned(FDebugger) then
    FreeAndNil(FDebugger);
end;

procedure TFormMain.ToolAttachProcessClick(Sender: TObject);
begin
  self.DebugApplicationAdvanced1.Click();
end;

procedure TFormMain.ToolButton4Click(Sender: TObject);
begin
  FormPsyloThreadManager.Show();
end;

procedure TFormMain.ToolButton6Click(Sender: TObject);
begin
  self.About1.Click();
end;

procedure TFormMain.ToolButtonRepoClick(Sender: TObject);
begin
  Open('https://github.com/DarkCoderSc/PsyloDbg');
end;

procedure TFormMain.ToolClearClick(Sender: TObject);
begin
  if MessageDlg('This action will clear data from all panes. Are you sure?', mtConfirmation, [mbYes, mbNo], 0) = ID_YES then
    self.ResetData();
end;

procedure TFormMain.ToolDebugApplicationClick(Sender: TObject);
begin
  self.DebugFile1.Click();
end;

procedure TFormMain.ToolDumpPEClick(Sender: TObject);
begin
  FormDumpMemoryImage.Show();
end;

procedure TFormMain.ToolProcessTreeClick(Sender: TObject);
begin
  FormDebugProcessTree.Show();
end;

procedure TFormMain.ToolRestartDebugClick(Sender: TObject);
begin
  self.Restart1.Click();
end;

procedure TFormMain.ToolStopDebugClick(Sender: TObject);
begin
  self.Stop1.Click();
end;

procedure TFormMain.RestartDebugger();
begin
  case FDebugMode of
    dmCreate: FormDebugApplication.ButtonStart.Click();
    dmAttach: self.AttachProcess(FLastDebuggedProcessId);
  end;
end;

procedure TFormMain.About1Click(Sender: TObject);
begin
  FormAbout.Show();
end;

procedure TFormMain.AttachProcess(const AProcessId : Cardinal);
begin
  FDEbugMode := dmAttach;
  ///

  if Assigned(FDebugger) then
    FreeAndNil(FDebugger);

  FDebugger := TAttachProcessAndDebugThread.Create(AProcessId);

  FDebugger.OnDebuggerStatusChange := OnDebuggerStatusChange;

  FDebugger.Start();

  FLastDebuggedProcessId := AProcessId;
end;

procedure TFormMain.DebugApplication(const AFileName, AProcessArguments : String; const ADebugChild, AShowProcess : Boolean);
begin
  FDebugMode := dmCreate;

  if Assigned(FDebugger) then
    FreeAndNil(FDebugger);

  // Terminate previous thread
  FDebugger := TCreateProcessAndDebugThread.Create(
      AFileName,
      AProcessArguments,
      ADebugChild,
      AShowProcess
  );

  FDebugger.OnDebuggerStatusChange := OnDebuggerStatusChange;

  FDebugger.Start();
end;

constructor TFormMain.Create(AOwner : TComponent);
begin
  inherited Create(AOwner);
  ///

  self.SetFormCaption();

  FDefaultExeIconIndex := SystemFileIcon('.exe', True);

  FDebugger := nil;

  FFrameDebugEvents := TFrameDebugEvents.Create(TabDebugEvents);
  FFrameDebugEvents.Parent := TabDebugEvents;

  FFrameMemoryMap := TFrameMemoryMap.Create(TabMemoryMap);
  FFrameMemoryMap.Parent := TabMemoryMap;

  FFrameLog := TFrameLog.Create(TabLog);
  FFrameLog.Parent := TabLog;

  FFrameModules := TFrameModules.Create(TabModules);
  FFrameModules.Parent := TabModules;

  FLastDebuggedProcessId := 0;

  FDebugSessionInformation := nil;
end;

procedure TFormMain.DebugApplicationAdvanced1Click(Sender: TObject);
begin
  FormAttachToProcess.ShowModal();
end;

procedure TFormMain.DebugFile1Click(Sender: TObject);
begin
  FormDebugApplication.ShowModal();
end;

destructor TFormMain.Destroy();
begin
  if Assigned(FFrameDebugEvents) then
    FreeAndNil(FFrameDebugEvents);

  if Assigned(FFrameMemoryMap) then
    FreeAndNil(FFrameMemoryMap);

  if Assigned(FFrameLog) then
    FreeAndNil(FFrameLog);

  if Assigned(FFrameModules) then
    FreeAndNil(FFrameModules);

  ///
  inherited Destroy();
end;

procedure TFormMain.DisplayEventColors1Click(Sender: TObject);
begin
  self.FFrameDebugEvents.VST.Refresh();
end;

procedure TFormMain.Exit1Click(Sender: TObject);
begin
  self.Close();
end;

procedure TFormMain.FormClose(Sender: TObject; var Action: TCloseAction);
begin
  if Assigned(FDebugger) then
    FreeAndNil(FDebugger);
end;

procedure TFormMain.FormCreate(Sender: TObject);
begin
  InitializeSystemIcons(ImageSystem, FFileInfo);

  Application.OnException := OnException;

  Pages.ActivePageIndex := 0;
end;

procedure TFormMain.FormResize(Sender: TObject);
begin
  if self.FrameComponentAlert1.Visible then
    self.FrameComponentAlert1.DoResize();
end;

procedure TFormMain.FormShow(Sender: TObject);
begin
  self.RefreshVCLThemes();
end;

procedure TFormMain.OnThemeChange(Sender: TObject);
begin
  TStyleManager.SetStyle(TMenuItem(Sender).Caption.Replace('&', ''));
end;

procedure TFormMain.RefreshVCLThemes();
var AStyleName : String;
    AMenu      : TMenuItem;
begin
  self.theme1.Clear;
  ///

  for AStyleName in TStyleManager.StyleNames do begin
    AMenu := TMenuItem.Create(self.theme1);
    AMenu.Caption := AStyleName;
    AMenu.OnClick := OnThemeChange;

    ///
    self.theme1.Add(AMenu);
  end;
end;

end.
