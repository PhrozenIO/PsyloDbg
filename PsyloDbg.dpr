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
  uFormMain in 'Units\uFormMain.pas' {FormMain},
  uExceptions in 'Units\uExceptions.pas',
  uDebuggerThread in 'Units\uDebuggerThread.pas',
  uFunctions in 'Units\uFunctions.pas',
  uFrameEventDetail in 'Units\uFrameEventDetail.pas' {FrameEventDetail: TFrame},
  uEventUtils in 'Units\uEventUtils.pas',
  uConstants in 'Units\uConstants.pas',
  uFormAttachToProcess in 'Units\uFormAttachToProcess.pas' {FormAttachToProcess},
  uFormDebugApplication in 'Units\uFormDebugApplication.pas' {FormDebugApplication},
  uFrameDebugEvents in 'Units\uFrameDebugEvents.pas' {FrameDebugEvents: TFrame};

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
  Application.Run;
end.
