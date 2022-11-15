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

unit uPsyloThread;

interface

uses System.Classes,
     System.SyncObjs;

type
  TOnThreadExecutionEnds = procedure(Sender : TObject; const AOnError : Boolean; const AErrorDetail : String) of object;

  TPsyloThread = class(TThread)
  private
    FOnThreadExecutionBegins : TNotifyEvent;
    FOnThreadExecutionEnds   : TOnThreadExecutionEnds;
  strict private
    {@G}
    function GetIsTerminated() : Boolean;
  protected
    {@M}
    procedure TerminatedSet(); override;
    procedure ThreadExecute(); virtual; abstract;
    procedure Execute(); override;
  public
    {@C}
    constructor Create(); overload; virtual;
    destructor Destroy(); override;

    {@S}
    property OnThreadExecutionBegins : TNotifyEvent           write FOnThreadExecutionBegins;
    property OnThreadExecutionEnds   : TOnThreadExecutionEnds write FOnThreadExecutionEnds;

    {@G}
    property IsTerminated : Boolean read GetIsTerminated;
  end;

implementation

uses Winapi.Windows, uFormMain, System.SysUtils;

{ TPsyloThread.Execute }
procedure TPsyloThread.Execute();
var AOnError     : Boolean;
    AErrorDetail : String;
begin
  AOnError     := False;
  AErrorDetail := '';
  ///

  if Assigned(FOnThreadExecutionBegins) then
    Synchronize(procedure begin
      FOnThreadExecutionBegins(self);
    end);
  try
    try
      ThreadExecute();
    except
      on E : Exception do begin
        AOnError := True;
        AErrorDetail := E.Message;
        ///

        Synchronize(procedure begin
          FormMain.OnException(self, E);
        end);
      end;
    end;
  finally
    if Assigned(FOnThreadExecutionEnds) then
      Synchronize(procedure begin
        FOnThreadExecutionEnds(self, AOnError, AErrorDetail);
      end);

    ///
    ExitThread(0);
  end;
end;

{ TPsyloThread.Create}
constructor TPsyloThread.Create();
begin
  inherited Create(True);
  ///

  FOnThreadExecutionBegins := nil;
  FOnThreadExecutionEnds   := nil;

  self.FreeOnTerminate := False;
end;

{ TPsyloThread.Destroy }
destructor TPsyloThread.Destroy();
begin
  self.Terminate();

  self.WaitFor();

  ///
  inherited Destroy();
end;

{ TPsyloThread.TerminatedSet }
procedure TPsyloThread.TerminatedSet();
begin
  inherited TerminatedSet();
  ///

end;

{ TPsyloThread.GetIsTerminated }
function TPsyloThread.GetIsTerminated() : Boolean;
var AExitCode : Cardinal;
begin
  result := self.Terminated;
  ///

  if not result then
    if GetExitCodeThread(self.Handle, AExitCode) then
      result := (AExitCode = 0);
end;

end.
