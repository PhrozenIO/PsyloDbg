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

unit uMemoryModulesThread;

interface

uses
  System.Classes,
  Winapi.Windows,
  System.SysUtils,
  uPsyloThread,
  uFrameModules;

type
  TMemoryModulesThread = class(TPsyloThread)
  private
    FProcessId : Cardinal;
    FView      : TFrameModules;
  protected
    {@M}
    procedure ThreadExecute(); override;
  public
    {@C}
    constructor Create(const AView : TFrameModules; const AProcessId : Cardinal); overload;
  end;

implementation

uses uMemoryModules, VirtualTrees, uFunctions;

{ TMemoryModulesThread.ThreadExecute }
procedure TMemoryModulesThread.ThreadExecute;
var AModules   : TEnumMemoryModules;
    AModule    : TMemoryModule;
    pNode      : PVirtualNode;
    pData      : PTreeData;
    ATotalSize : UInt64;
begin
  try
    ATotalSize := 0;
    ///

    if not Assigned(FView) then
      Exit();
    ///

    AModules := TEnumMemoryModules.Create(FProcessId, self);
    ///

    AModules.Refresh();

    for AModule in AModules.Modules do begin
      if self.Terminated then
        break;
      ///

      Synchronize(procedure begin
        pNode := FView.VST.AddChild(nil);

        pData := pNode.GetData;
      end);
      ///

      pData^.Module     := TMemoryModule.Create(AModule);
      pData^.ImageIndex := SystemFileIcon(pData^.Module.ImagePath);

      ///
      Inc(ATotalSize, pData^.Module.BaseSize);
    end;
  finally
    if Assigned(AModules) then
      FreeAndNil(AModules);

    Synchronize(procedure begin
      FView.TotalModuleSize := ATotalSize;
    end);
  end;
end;

{ TMemoryModulesThread.Create }
constructor TMemoryModulesThread.Create(const AView : TFrameModules; const AProcessId: Cardinal);
begin
  inherited Create();
  ///

  FView      := AView;
  FProcessId := AProcessId;
end;

end.
