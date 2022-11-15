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
  Vcl.Themes,
  Vcl.Styles,
  uFormMain in 'Units\Forms\uFormMain.pas' {FormMain},
  uFrameEventDetail in 'Units\Frames\uFrameEventDetail.pas' {FrameEventDetail: TFrame},
  uFormAttachToProcess in 'Units\Forms\uFormAttachToProcess.pas' {FormAttachToProcess},
  uFormDebugApplication in 'Units\Forms\uFormDebugApplication.pas' {FormDebugApplication},
  uFrameDebugEvents in 'Units\Frames\uFrameDebugEvents.pas' {FrameDebugEvents: TFrame},
  uFrameMemoryMap in 'Units\Frames\uFrameMemoryMap.pas' {FrameMemoryMap: TFrame},
  uFrameLog in 'Units\Frames\uFrameLog.pas' {FrameLog: TFrame},
  uFormThreadOperation in 'Units\Forms\uFormThreadOperation.pas' {FormThreadOperation},
  uFrameComponentAlert in 'Units\Frames\Components\uFrameComponentAlert.pas' {FrameComponentAlert: TFrame},
  uFrameModules in 'Units\Frames\uFrameModules.pas' {FrameModules: TFrame},
  uFormPsyloThreadManager in 'Units\Forms\uFormPsyloThreadManager.pas' {FormPsyloThreadManager},
  uFormAbout in 'Units\Forms\uFormAbout.pas' {FormAbout},
  uFormDebugProcessTree in 'Units\Forms\uFormDebugProcessTree.pas' {FormDebugProcessTree},
  uFrameComponentComboProcess in 'Units\Frames\Components\uFrameComponentComboProcess.pas' {FrameComponentComboProcess: TFrame},
  uPsyloFrame in 'Units\Frames\___Frames\uPsyloFrame.pas' {PsyloFrame: TFrame},
  uFormDumpMemoryImage in 'Units\Forms\uFormDumpMemoryImage.pas' {FormDumpMemoryImage},
  uPsyloForm in 'Units\Forms\___Forms\uPsyloForm.pas' {FormPsylo},
  XSuperObject in 'Libs\XSuperObject\XSuperObject.pas',
  XSuperJSON in 'Libs\XSuperObject\XSuperJSON.pas',
  uExceptions in 'Units\Objects\uExceptions.pas',
  uDebuggerThread in 'Units\Threads\uDebuggerThread.pas',
  uFunctions in 'Units\uFunctions.pas',
  uEventUtils in 'Units\uEventUtils.pas',
  uConstants in 'Units\uConstants.pas',
  uMemoryMapThread in 'Units\Threads\uMemoryMapThread.pas',
  uMemoryMap in 'Units\Objects\uMemoryMap.pas',
  uMemoryUtils in 'Units\uMemoryUtils.pas',
  uGraphicUtils in 'Units\uGraphicUtils.pas',
  uPortableExecutable in 'Units\Objects\uPortableExecutable.pas',
  uMemoryDumpThread in 'Units\Threads\uMemoryDumpThread.pas',
  uMemoryModules in 'Units\Objects\uMemoryModules.pas',
  uMemoryModulesThread in 'Units\Threads\uMemoryModulesThread.pas',
  uPsyloThread in 'Units\Threads\Components\uPsyloThread.pas',
  uTypes in 'Units\uTypes.pas',
  uDebugSession in 'Units\Objects\uDebugSession.pas',
  uMessageListener in 'Units\Objects\uMessageListener.pas',
  uMessages in 'Units\uMessages.pas',
  uModuleDump in 'Units\Objects\uModuleDump.pas',
  uProcessDumper in 'Units\Objects\uProcessDumper.pas',
  uPsyloOperationThread in 'Units\Threads\Components\uPsyloOperationThread.pas',
  uDumpMemoryImageThread in 'Units\Threads\uDumpMemoryImageThread.pas',
  uStrings in 'Units\uStrings.pas',
  uDialogs in 'Units\uDialogs.pas';

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
  Application.CreateForm(TFormThreadOperation, FormThreadOperation);
  Application.CreateForm(TFormPsyloThreadManager, FormPsyloThreadManager);
  Application.CreateForm(TFormAbout, FormAbout);
  Application.CreateForm(TFormDebugProcessTree, FormDebugProcessTree);
  Application.CreateForm(TFormDumpMemoryImage, FormDumpMemoryImage);
  Application.CreateForm(TFormPsylo, FormPsylo);
  Application.Run;
end.
