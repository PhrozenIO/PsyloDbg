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


unit uConstants;

interface

uses Winapi.Windows, VCL.Graphics;

const
      // JSON KEYS
      _PROCESS_ID              = 'Process_Id';
      _IMAGE_PATH              = 'Image_Path';
      _THREAD_ID               = 'Thread_Id';
      _EVENT_KIND              = 'Event_Kind';
      _EVENT_DATE_TIME         = 'Event_Date_Time';
      _PARENT_PROCESS_ID       = 'Parent_Process_Id';
      _PARENT_IMAGE_PATH       = 'Parent_Image_Path';
      _FILE_HANDLE             = 'File_Handle';
      _PROCESS_HANDLE          = 'Process_Handle';
      _THREAD_HANDLE           = 'Thread_Handle';
      _BASE_OF_IMAGE           = 'Base_Of_Image';
      _DEBUG_INFO_FILE_OFFSET  = 'Debug_Info_File_Offset';
      _DEBUG_INFO_SIZE         = 'Debug_Info_Size';
      _THREAD_LOCAL_BASE       = 'Thread_Local_Base';
      _START_ADDRESS           = 'Start_Address';
      _IMAGE_NAME              = 'Image_Name';
      _UNICODE                 = 'Unicode';
      _RESOLVED_IMAGE_NAME     = 'Resolved_Image_Name';
      _FIRST_CHANCE            = 'First_Chance';
      _EXCEPTION_CODE          = 'Exception_Code';
      _EXCEPTION_FLAGS         = 'Exception_Flags';
      _EXCEPTION_FLAGS_MEANING = 'Exception_Flags_Meaning';
      _EXCEPTION_CODE_MEANING  = 'Exception_Code_Meaning';
      _EXCEPTION_ADDRESS       = 'Exception_Address';
      _NUMBER_PARAMETERS       = 'Number_Parameters';
      _EXCEPT_ADDR             = 'Except_Addr';
      _EXCEPT_OBJECT           = 'Except_Object';
      _NEXT_EXCEPTION          = 'Next_Exception';
      _EXCEPTION_CHAIN         = 'Exception_Chain';
      _EXIT_CODE               = 'Exit_Code';
      _BASE_OF_DLL             = 'Base_Of_Dll';
      _MESSAGE                 = 'Message';
      _ERROR                   = 'Error';
      _TYPE                    = 'Type';

      // IMAGE_INDEX (STATE)
      _STATE_IMAGE_EVENT_KIND_PROCESS_CREATE      = 0;
      _STATE_IMAGE_EVENT_KIND_EXIT_PROCESS        = 1;
      _STATE_IMAGE_EVENT_KIND_EXCEPTION           = 2;
      _STATE_IMAGE_EVENT_KIND_OUTPUT_DEBUG_STRING = 3;
      _STATE_IMAGE_EVENT_KIND_LOAD_DLL            = 4;
      _STATE_IMAGE_EVENT_KIND_UNLOAD_DLL          = 5;
      _STATE_IMAGE_EVENT_KIND_RIP                 = 6;
      _STATE_IMAGE_EVENT_KIND_CREATE_THREAD       = 7;
      _STATE_IMAGE_EVENT_KIND_EXIT_THREAD         = 8;
      _STATE_IMAGE_EVENT_ITEM                     = 9;
      _STATE_IMAGE_EVENT_ARRAY                    = 10;
      _STATE_IMAGE_EVENT_EXCEPTION                = 11;
      _STATE_IMAGE_EXCEPTION                      = 28;
      _STATE_IMAGE_SUCCESS                        = 31;
      _STATE_IMAGE_INTERUPTION                    = 32;
      _STATE_IMAGE_INFORMATION                    = 30;

var
  // COLORS
  _COLOR_1  : TColor;
  _COLOR_2  : TColor;
  _COLOR_3  : TColor;
  _COLOR_4  : TColor;
  _COLOR_5  : TColor;
  _COLOR_6  : TColor;
  _COLOR_7  : TColor;
  _COLOR_8  : TColor;
  _COLOR_9  : TColor;

  // Light Colors
  _COLOR_10 : TColor;
  _COLOR_11 : TColor;
  _COLOR_12 : TColor;  // Light Red

  _COLOR_GRAD1_BEG : TColor;
  _COLOR_GRAD1_END : TColor;

  _COLOR_GRAD2_BEG : TColor;
  _COLOR_GRAD2_END : TColor;

  _ODD_LIST_BG_COLOR : TColor;

implementation

initialization
  _COLOR_1  := rgb(233, 233, 255);
  _COLOR_2  := rgb(231, 238, 254);
  _COLOR_3  := rgb(243, 246, 250);
  _COLOR_4  := rgb(255, 225, 250);
  _COLOR_5  := rgb(255, 242, 225);
  _COLOR_6  := rgb(231, 244, 249);
  _COLOR_7  := rgb(234, 247, 254);
  _COLOR_8  := rgb(248, 248, 229);
  _COLOR_9  := rgb(255, 247, 246);
  _COLOR_10 := rgb(242, 242, 255);
  _COLOR_11 := rgb(249, 242, 255);
  _COLOR_12 := rgb(254, 236, 231);

  _COLOR_GRAD1_BEG := $00CAF3FF;
  _COLOR_GRAD1_END := $008CEBFF;

  _COLOR_GRAD2_BEG := rgb(202, 224, 255);
  _COLOR_GRAD2_END := rgb(140, 207, 255);

  _ODD_LIST_BG_COLOR := rgb(250, 250, 250);

end.
