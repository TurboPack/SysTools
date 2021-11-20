unit Delphi.Windows;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils
  , windows
  , ShellAPI
  ;

function GetEnvironmentVariable(lpName:LPCSTR; lpBuffer:LPSTR; nSize:DWORD):DWORD; overload;inline;
function GetEnvironmentVariable(lpName:LPCWSTR; lpBuffer:LPWSTR; nSize:DWORD):DWORD; overload;inline;

function GetNumberOfEventLogRecords(hEventLog: THandle; var NumberOfRecords: DWORD): BOOL; overload;inline;
function GetNumberOfEventLogRecords(hEventLog: THandle; NumberOfRecords: PDWORD): BOOL; overload;inline;


const
  {$EXTERNALSYM CONNDLG_RO_PATH}
  CONNDLG_RO_PATH = 1;    { Resource path should be read-only     }
  {$EXTERNALSYM CONNDLG_CONN_POINT}
  CONNDLG_CONN_POINT = 2; { Netware -style movable connection point enabled  }
  {$EXTERNALSYM CONNDLG_USE_MRU}
  CONNDLG_USE_MRU = 4;    { Use MRU combobox   }
  {$EXTERNALSYM CONNDLG_HIDE_BOX}
  CONNDLG_HIDE_BOX = 8;   { Hide persistent connect checkbox   }

  { NOTE:  Set at most ONE of the below flags.  If neither flag is set,
           then the persistence is set to whatever the user chose during
           a previous connection }

  {$EXTERNALSYM CONNDLG_PERSIST}
  CONNDLG_PERSIST = $10;       { Force persistent connection  }
  {$EXTERNALSYM CONNDLG_NOT_PERSIST}
  CONNDLG_NOT_PERSIST = $20;   { Force connection NOT persistent  }

const
{ Network Connections. }

  {$EXTERNALSYM NETPROPERTY_PERSISTENT}
  NETPROPERTY_PERSISTENT = 1;

  {$EXTERNALSYM CONNECT_UPDATE_PROFILE}
  CONNECT_UPDATE_PROFILE          = $00000001;
  {$EXTERNALSYM CONNECT_UPDATE_RECENT}
  CONNECT_UPDATE_RECENT           = $00000002;
  {$EXTERNALSYM CONNECT_TEMPORARY}
  CONNECT_TEMPORARY               = $00000004;
  {$EXTERNALSYM CONNECT_INTERACTIVE}
  CONNECT_INTERACTIVE             = $00000008;
  {$EXTERNALSYM CONNECT_PROMPT}
  CONNECT_PROMPT                  = $00000010;
  {$EXTERNALSYM CONNECT_NEED_DRIVE}
  CONNECT_NEED_DRIVE              = $00000020;
  {$EXTERNALSYM CONNECT_REFCOUNT}
  CONNECT_REFCOUNT                = $00000040;
  {$EXTERNALSYM CONNECT_REDIRECT}
  CONNECT_REDIRECT                = $00000080;
  {$EXTERNALSYM CONNECT_LOCALDRIVE}
  CONNECT_LOCALDRIVE              = $00000100;
  {$EXTERNALSYM CONNECT_CURRENT_MEDIA}
  CONNECT_CURRENT_MEDIA           = $00000200;
  {$EXTERNALSYM CONNECT_DEFERRED}
  CONNECT_DEFERRED                = $00000400;
  {$EXTERNALSYM CONNECT_RESERVED}
  CONNECT_RESERVED                = DWORD($FF000000);


const
  {$EXTERNALSYM DISC_UPDATE_PROFILE}
  DISC_UPDATE_PROFILE = 1;
  {$EXTERNALSYM DISC_NO_FORCE}
  DISC_NO_FORCE = $40;

const
  {$EXTERNALSYM WNCON_FORNETCARD}
  WNCON_FORNETCARD = 1;
  {$EXTERNALSYM WNCON_NOTROUTED}
  WNCON_NOTROUTED = 2;
  {$EXTERNALSYM WNCON_SLOWLINK}
  WNCON_SLOWLINK = 4;
  {$EXTERNALSYM WNCON_DYNAMIC}
  WNCON_DYNAMIC = 8;

const
  { Registry Key restore flags }

  REG_WHOLE_HIVE_VOLATILE = ($00000001);    { Restore whole hive volatile }
  {$EXTERNALSYM REG_WHOLE_HIVE_VOLATILE}
  REG_REFRESH_HIVE        = ($00000002);    { Unwind changes to last flush }
  {$EXTERNALSYM REG_REFRESH_HIVE}

const //var
  HeapAllocFlags: Word = 2;   { Heap allocation flags, gmem_Moveable }


implementation

function GetEnvironmentVariable(lpName:LPCSTR; lpBuffer:LPSTR; nSize:DWORD):DWORD;
begin
  result := windows.GetEnvironmentVariableA(lpName, lpBuffer, nSize);
end;

function GetEnvironmentVariable(lpName:LPCWSTR; lpBuffer:LPWSTR; nSize:DWORD):DWORD;
begin
  result := windows.GetEnvironmentVariableW(lpName, lpBuffer, nSize);
end;

function GetNumberOfEventLogRecords(hEventLog: THandle; var NumberOfRecords: DWORD): BOOL;
begin
  result := windows.GetNumberOfEventLogRecords(hEventLog, @NumberOfRecords);
end;

function GetNumberOfEventLogRecords(hEventLog: THandle; NumberOfRecords: PDWORD): BOOL;
begin
  result := windows.GetNumberOfEventLogRecords(hEventLog, NumberOfRecords);
end;

end.

