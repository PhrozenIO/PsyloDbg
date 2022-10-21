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

unit uMemoryUtils;

interface

uses Winapi.Windows,
     System.SysUtils,
     System.Classes;

function ProtectToString(const AValue : DWORD) : String;
function MemoryStateToString(const AState : DWORD) : String;
function MemoryTypeToString(const AType : DWORD) : String;
function IsMemoryExecutable(const AProtect : DWORD) : Boolean;

implementation

{ _.ProtectToStr }
function ProtectToString(const AValue : DWORD) : String;
var AResult : TStringList;
begin
  AResult := TStringList.Create();
  try
    if (AValue and PAGE_EXECUTE) = PAGE_EXECUTE then
      AResult.Add('Execute');

    if (AValue and PAGE_EXECUTE_READ) = PAGE_EXECUTE_READ then
      AResult.Add('Execute Read');

    if (AValue and PAGE_EXECUTE_READWRITE) = PAGE_EXECUTE_READWRITE then
      AResult.Add('Execute Read Write');

    if (AValue and PAGE_EXECUTE_WRITECOPY) = PAGE_EXECUTE_WRITECOPY then
      AResult.Add('Execute Read Write Copy');

    if (AValue and PAGE_NOACCESS) = PAGE_NOACCESS  then
      AResult.Add('No Access');

    if (AValue and PAGE_READONLY) = PAGE_READONLY then
      AResult.Add('Read Only');

    if (AValue and PAGE_READWRITE) = PAGE_READWRITE then
      AResult.Add('Read Write');

    if (AValue and PAGE_WRITECOPY) = PAGE_WRITECOPY then
      AResult.Add('Write Copy');

    if (AValue and PAGE_GUARD) = PAGE_GUARD then
      AResult.Add('Guard');

    if (AValue and PAGE_NOCACHE) = PAGE_NOCACHE then
      AResult.Add('No Cache');
  finally

    result := AResult.Text.Trim.Replace(#13, ', ', [rfReplaceAll]);
    ///

    if Assigned(AResult) then
      FreeAndNil(AResult);
  end;
end;

{ _.MemoryStateToString }
function MemoryStateToString(const AState : DWORD) : String;
begin
  case AState of
    MEM_COMMIT  : result := 'Commit';
    MEM_FREE    : result := 'Free';
    MEM_RESERVE : result := 'Reserve';

    else
      result := '-';
  end;
end;

{ _.MemoryTypeToString }
function MemoryTypeToString(const AType : DWORD) : String;
begin
  case AType of
    MEM_IMAGE   : result := 'Image';
    MEM_MAPPED  : result := 'Mapped';
    MEM_PRIVATE : result := 'Private';

    else
      result := '-';
  end;
end;

{ _.IsMemoryExecutable }
function IsMemoryExecutable(const AProtect : DWORD) : Boolean;
begin
  result := false;
  ///

  if ((AProtect and PAGE_EXECUTE_READWRITE) = PAGE_EXECUTE_READWRITE) or
     ((AProtect and PAGE_EXECUTE) = PAGE_EXECUTE) or
     ((AProtect and PAGE_EXECUTE_READ) = PAGE_EXECUTE_READ) or
     ((AProtect and PAGE_EXECUTE_WRITECOPY) = PAGE_EXECUTE_WRITECOPY) then
    result := true;
end;


end.
