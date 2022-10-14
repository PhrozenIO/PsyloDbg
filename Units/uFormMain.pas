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

unit uFormMain;

interface

uses
  Winapi.Windows, Winapi.Messages, System.SysUtils, System.Variants, System.Classes, Vcl.Graphics,
  Vcl.Controls, Vcl.Forms, Vcl.Dialogs, Vcl.Menus, VirtualTrees, Vcl.ExtCtrls,
  OMultiPanel, System.ImageList, Vcl.ImgList, Winapi.ShellAPI, System.Generics.Collections,
  Vcl.VirtualImageList, Vcl.BaseImageCollection, Vcl.ImageCollection, XSuperObject,
  Winapi.ShLwApi, Winapi.GDIPAPI, uFrameDebugEvents, Vcl.ComCtrls, uDebuggerThread,
  Vcl.StdCtrls, Vcl.ToolWin, Vcl.WinXCtrls;

type
  TDebugMode = (
    dmCreate,
    dmAttach
  );

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
  private
    FFileInfo              : TSHFileInfo;
    FDefaultExeIconIndex   : Integer;
    FDebugger              : TDebuggerThread;
    FDebugMode             : TDebugMode;
    FFrameDebugEvents      : TFrameDebugEvents;
    FLastDebuggedProcessId : Cardinal;

    {@M}
    procedure OnDebuggerStatusChange(Sender: TDebuggerThread; const ADebuggerStatus : TDebuggerStatus);
    procedure RestartDebugger();

  public
    {@M}
    procedure DebugApplication(const AFileName, AProcessArguments : String; const ADebugChild, AShowProcess : Boolean);
    procedure AttachProcess(const AProcessId : Cardinal);
    procedure OnException(Sender : TObject; E : Exception);

    {@C}
    constructor Create(AOwner : TComponent); override;
    destructor Destroy(); override;

    {@G}
    property DefaultExeIconIndex : Integer           read FDefaultExeIconIndex;
    property DebugEvents         : TFrameDebugEvents read FFrameDebugEvents;
  end;

var
  FormMain: TFormMain;

implementation

{$R *.dfm}

uses uFunctions, uExceptions, uEventUtils, uConstants, uFormAttachToProcess,
     uFormDebugApplication;

procedure TFormMain.OnException(Sender : TObject; E : Exception);
begin
  // TODO
  // allocconsole();
  // writeln(E.Message);
end;

procedure TFormMain.OnDebuggerStatusChange(Sender: TDebuggerThread; const ADebuggerStatus : TDebuggerStatus);
begin
  self.Debug1.Enabled                    := (ADebuggerStatus = dbgStart);
  self.DebugFile1.Enabled                := not self.Debug1.Enabled;
  self.DebugApplicationAdvanced1.Enabled := not self.Debug1.Enabled;

  self.ToolDebugApplication.Enabled      := not self.Debug1.Enabled;
  self.ToolAttachProcess.Enabled         := not self.Debug1.Enabled;
  self.ToolPauseResumeDebug.Enabled      := self.Debug1.Enabled;
  self.ToolStopDebug.Enabled             := self.Debug1.Enabled;
  self.ToolRestartDebug.Enabled          := self.Debug1.Enabled;

  case ADebuggerStatus of
    dbgStart, dbgStop: begin
      FFrameDebugEvents.Reset();
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

procedure TFormMain.ToolDebugApplicationClick(Sender: TObject);
begin
  self.DebugFile1.Click();
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
  MessageDlg('Coded by Jean-Pierre LESUEUR (@DarkCoderSc).' + #13#10 + 'https://www.github.com/darkcodersc', mtInformation, [mbOk], 0);
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
  CheckIfValidApplicationForDebugging(AFileName);

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

  FDefaultExeIconIndex := SystemFileIcon('.exe', True);

  FDebugger := nil;

  FFrameDebugEvents := TFrameDebugEvents.Create(self.Pages.Pages[0]);
  FFrameDebugEvents.Parent := self.Pages.Pages[0];

  FLastDebuggedProcessId := 0;
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
  if Assigned(FDebugger) then
    FreeAndNil(FDebugger);

  if Assigned(FFrameDebugEvents) then
    FreeAndNil(FFrameDebugEvents);

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

procedure TFormMain.FormCreate(Sender: TObject);
begin
  InitializeSystemIcons(ImageSystem, FFileInfo);

  Application.OnException := OnException;
end;

end.
