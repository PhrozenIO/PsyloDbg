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

program PsyloDbg;

uses
  Vcl.Forms,
  XSuperObject in 'Libs\XSuperObject\XSuperObject.pas',
  XSuperJSON in 'Libs\XSuperObject\XSuperJSON.pas',
  uFormMain in 'Units\Forms\uFormMain.pas' {FormMain},
  uExceptions in 'Units\Objects\uExceptions.pas',
  uDebuggerThread in 'Units\Threads\uDebuggerThread.pas',
  uFunctions in 'Units\uFunctions.pas',
  uFrameEventDetail in 'Units\Frames\uFrameEventDetail.pas' {FrameEventDetail: TFrame},
  uEventUtils in 'Units\uEventUtils.pas',
  uConstants in 'Units\uConstants.pas',
  uFormAttachToProcess in 'Units\Forms\uFormAttachToProcess.pas' {FormAttachToProcess},
  uFormDebugApplication in 'Units\Forms\uFormDebugApplication.pas' {FormDebugApplication},
  uFrameDebugEvents in 'Units\Frames\uFrameDebugEvents.pas' {FrameDebugEvents: TFrame},
  uFrameMemoryMap in 'Units\Frames\uFrameMemoryMap.pas' {FrameMemoryMap: TFrame},
  uMemoryMapThread in 'Units\Threads\uMemoryMapThread.pas',
  uMemoryMap in 'Units\Objects\uMemoryMap.pas',
  uMemoryUtils in 'Units\uMemoryUtils.pas',
  uGraphicUtils in 'Units\uGraphicUtils.pas',
  uPortableExecutable in 'Units\Objects\uPortableExecutable.pas',
  uFrameLog in 'Units\Frames\uFrameLog.pas' {FrameLog: TFrame},
  uMemoryDumpThread in 'Units\Threads\uMemoryDumpThread.pas',
  uFormDumpMemoryOperation in 'Units\Forms\uFormDumpMemoryOperation.pas' {FormDumpMemoryOperation},
  Vcl.Themes,
  Vcl.Styles;

{$R *.res}

begin
  IsMultiThread := True;

  NTSetPrivilege('SeDebugPrivilege', True);
  ///

  Application.Initialize;
  Application.MainFormOnTaskbar := True;
  Application.CreateForm(TFormMain, FormMain);
  Application.CreateForm(TFormAttachToProcess, FormAttachToProcess);
  Application.CreateForm(TFormDebugApplication, FormDebugApplication);
  Application.CreateForm(TFormDumpMemoryOperation, FormDumpMemoryOperation);
  Application.Run;
end.
