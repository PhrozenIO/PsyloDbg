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

// TODO: Option to not automatically close / destroy form on thread execution end.

unit uPsyloOperationThread;

interface

uses System.Classes,
     System.SysUtils,
     Winapi.Windows,
     VCL.Forms,
     uPsyloThread;

type
  TPsyloOperationThread = class(TPsyloThread)
  private
    FForm : TForm;

    {@M}
    procedure RefreshLabel();
  protected
    {@M}
    procedure NotifyOperationStart();
    procedure NotifyOperationEnd();
    procedure NotifyTaskCount(const ATaskCount : Cardinal);
    procedure NotifyTaskStart(const ALabel : String);
    procedure NotifyUpdateTaskLabel(const ALabel : String);
    procedure NotifyTaskEnd();
    procedure NotifyTaskProgress(const AProgressMax, AProgress: UInt64);
  public
    {@C}
    constructor Create(const AOwner : TComponent; const AOperationTitle : String; const AIconIndex : Integer = -1); overload;
    destructor Destroy(); override;
  end;

implementation

uses uFormThreadOperation;

{ TPsyloOperationThread.RefreshLabel }
procedure TPsyloOperationThread.RefreshLabel();
begin
  Synchronize(procedure begin
    TFormThreadOperation(FForm).LabelTotal.Caption := Format('Operation %d/%d', [
      TFormThreadOperation(FForm).ProgressTotal.Position,
      TFormThreadOperation(FForm).ProgressTotal.Max]
    );
  end);
end;

{ TPsyloOperationThread.NotifyTaskCount }
procedure TPsyloOperationThread.NotifyTaskCount(const ATaskCount : Cardinal);
begin
  TFormThreadOperation(FForm).ProgressTotal.Max := ATaskCount;

  ///
  self.RefreshLabel();
end;

{ TPsyloOperationThread.NotifyOperationStart }
procedure TPsyloOperationThread.NotifyOperationStart();
begin
  if Assigned(FForm) then
    Synchronize(procedure begin
      FForm.Show();
    end);
end;

{ TPsyloOperationThread.NotifyOperationEnd }
procedure TPsyloOperationThread.NotifyOperationEnd();
begin
  if Assigned(FForm) then
    Synchronize(procedure begin
      FForm.Hide();
    end);
end;

{ TPsyloOperationThread.NotifyTaskStart }
procedure TPsyloOperationThread.NotifyTaskStart(const ALabel : String);
begin
  if not Assigned(FForm) then
    Exit();
  ///


  Synchronize(procedure begin
    TFormThreadOperation(FForm).LabelCurrent.Caption     := ALabel;
    TFormThreadOperation(FForm).ProgressCurrent.Position := 0;
  end);

  ///
  self.RefreshLabel();
end;

{ TPsyloOperationThread.NotifyUpdateTaskLabel }
procedure TPsyloOperationThread.NotifyUpdateTaskLabel(const ALabel : String);
begin
  Synchronize(procedure begin
    TFormThreadOperation(FForm).LabelCurrent.Caption := ALabel;
  end);
end;

{ TPsyloOperationThread.NotifyTaskEnds }
procedure TPsyloOperationThread.NotifyTaskEnd();
begin
  if not Assigned(FForm) then
    Exit();
  ///

  Synchronize(procedure begin
    TFormThreadOperation(FForm).ProgressCurrent.Position := 0;
    TFormThreadOperation(FForm).ProgressTotal.Position   := TFormThreadOperation(FForm).ProgressTotal.Position + 1;

    ///
    self.RefreshLabel();
  end);
end;

{ TPsyloOperationThread.NotifyTaskProgress }
procedure TPsyloOperationThread.NotifyTaskProgress(const AProgressMax, AProgress: UInt64);
begin
  Synchronize(procedure begin
    TFormThreadOperation(FForm).ProgressCurrent.Position := (AProgress * 100) div AProgressMax;
  end);
end;

{ TPsyloOperationThread.Create }
constructor TPsyloOperationThread.Create(const AOwner : TComponent; const AOperationTitle : String; const AIconIndex : Integer = -1);
begin
  inherited Create();
  ///

  FForm := TFormThreadOperation.Create(AOwner, self, AOperationTitle, AIconIndex);
end;

{ TPsyloOperationThread.Destroy }
destructor TPsyloOperationThread.Destroy();
begin

  ///
  inherited Destroy();
end;

end.
