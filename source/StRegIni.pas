// Upgraded to Delphi 2009: Sebastian Zierer

(* ***** BEGIN LICENSE BLOCK *****
 * Version: MPL 1.1
 *
 * The contents of this file are subject to the Mozilla Public License Version
 * 1.1 (the "License"); you may not use this file except in compliance with
 * the License. You may obtain a copy of the License at
 * http://www.mozilla.org/MPL/
 *
 * Software distributed under the License is distributed on an "AS IS" basis,
 * WITHOUT WARRANTY OF ANY KIND, either express or implied. See the License
 * for the specific language governing rights and limitations under the
 * License.
 *
 * The Original Code is TurboPower SysTools
 *
 * The Initial Developer of the Original Code is
 * TurboPower Software
 *
 * Portions created by the Initial Developer are Copyright (C) 1996-2002
 * the Initial Developer. All Rights Reserved.
 *
 * Contributor(s):
 *
 * ***** END LICENSE BLOCK ***** *)

{*********************************************************}
{* SysTools: StRegIni.pas 4.04                           *}
{*********************************************************}
{* SysTools: Registry and INI file access                *}
{*********************************************************}

{$I StDefine.inc}

unit StRegIni;

interface

uses
  Windows,
  Graphics, Classes, SysUtils,
  STStrL, StDate, STConst, STBase;

type
{.Z+}
  TRegIniType = (riIniType, riRegType);
  TRegIniMode = (riSet, riGet);
  TWinVerType = (riWin31,riWin32s,riWin95,riWinNT);
{.Z-}

  TQueryKeyInfo = record
    QIKey       : HKey;         {Value of key being queried}
    QIClassName : string;       {Class Name associated with key}
    QINumSubKeys: DWORD;        {Number of Subkeys under queried key}
    QIMaxSKNLen : DWORD;        {Length of longest subkey name}
    QIMaxCNLen  : DWORD;        {Length of longest class name found}
    QINumValues : DWORD;        {Number of values found in queried key ONLY, i.e., values in subkeys not included}
    QIMaxVNLen  : DWORD;        {Length of longest value name}
    QIMaxDataLen: DWORD;        {Largest size (in bytes) of values in queried key}
    QISDescLen  : DWORD;        {Length of Security Descriptor}
    QIFileTime  : TFileTime;    {Time/date file/key was last modified}
  end;

const
{.Z+}
  RI_INVALID_VALUE = -1;
  RIVOLATILE = REG_WHOLE_HIVE_VOLATILE;
  ShortBufSize = 255;
  MaxBufSize = 8192;
  MaxByteArraySize = 127;
{.Z-}

  RIMachine = 'MACHINE';
  RIUsers   = 'USERS';
  RIRoot    = 'ROOT';
  RICUser   = 'C_USERS';


type
  TStRegIni = class(TObject)
{.Z+}
  protected {private}
    riMode           : TRegIniMode;

    riWinVer         : TWinVerType;
    riType           : TRegIniType;
    riHoldPrimary,
    riPrimaryKey     : HKey;
    riRemoteKey      : HKey;

    riCurSubKey,
    riTrueString,
    riFalseString    : PChar;

{$IFDEF ThreadSafe}
    riThreadSafe     : TRTLCriticalSection;
{$ENDIF}

    function GetAttributes : TSecurityAttributes;
      {-get security attributes record or value}
    procedure SetAttributes(Value : TSecurityAttributes);
      {-get security attributes record or value}

    function GetCurSubKey : string;
      {-get current subkey/section}
    procedure SetCurSubKey(Value : string);
      {-set current subkey/section}

    function GetIsIniFile : Boolean;
      {-get whether current instance in IniFile or no}

    procedure ParseIniFile(SList : TStrings);
      {-adds section names in an INI file to a string list}

  protected
    FCurSubKey       : string;
    FriSecAttr       : TSecurityAttributes;
    FIsIniFile       : Boolean;

    riRootName       : PChar;

    BmpText,
    BmpBinary        : TBitMap;

    {protected procedures to manage open/closing}
    function OpenRegKey : HKey;
      {-opens/creates key or ini file}
    procedure CloseRegKey(const Key : HKey);
      {-closes open key or ini file}

    procedure EnterCS;
      {- call EnterCriticalSection procedure}
    procedure LeaveCS;
      {- call LeaveCriticalSection procedure}

    function WriteIniData(const ValueName : string; Data : string) : Boolean;
      {-write data to an Ini file}

    function ReadIniData(const ValueName : string; var Value : string;
                         Default : string) : Integer;
      {-read data from an Ini file}

    function WriteRegData(Key : HKey; const ValueName : string; Data : Pointer;
                          DType : DWORD; Size : Integer) : LongInt;
      {-write data to the registry}

    function ReadRegData(Key : HKey; const ValueName : string; Data : Pointer;
                         Size : LongInt; DType : DWORD) : LongInt;
      {-read data from the registry}

{.Z-}
  public
    constructor Create(RootName : String; IsIniFile : Boolean); virtual;
    destructor Destroy; override;

    procedure SetPrimary(Value : string);
      {-change INI filename or primary key of registry}
    function GetPrimary : string;
      {-return current INI filename or primary key of registry}

    function GetDataInfo(Key : HKey; const ValueName : string;
                         var Size : LongInt; var DType : DWORD) : LongInt;
      {-get size and type of data for entry in registry}

    function BytesToString(Value : PByte; Size : Cardinal) : AnsiString;
      {-converts byte array to string with no spaces}
    function StringToBytes(const IString : AnsiString; var Value; Size : Cardinal) : Boolean;
      {-converts string (by groups of 2 char) to byte values}


    function GetFullKeyPath : string;

    procedure WriteBoolean(const ValueName : string; Value : Boolean);
      {-set boolean data in the ini file or registry}
    function  ReadBoolean(const ValueName : string; Default : Boolean) : Boolean;
      {-get boolean data in the ini file or registry}
    procedure WriteInteger(const ValueName : string; Value : DWORD);
      {-set integer data in the ini file or registry}
    function  ReadInteger(const ValueName : string; Default : DWORD) : DWORD;
      {-get integer data in the ini file or registry}
    procedure WriteString(const ValueName : string; const Value : string);
      {-set string data in the ini file or registry}
    function  ReadString(const ValueName : string; const Default : string) : string;
      {-get string data in the ini file or registry}
    procedure WriteBinaryData(const ValueName : string; const Value; Size : Integer);
      {-set byte array in the ini file or registry}
    procedure ReadBinaryData(const ValueName : string; const Default; var Value; var Size : Integer);
      {-get byte array from the ini file or registry}
    procedure WriteFloat(const ValueName : string; const Value : Double);
      {-set float value in the ini file or registry}
    function ReadFloat(const ValueName : string; const Default : TStFloat) : TStFloat;
      {-get float from the ini file or registry}
    procedure WriteDate(const ValueName : string; const Value : TStDate);
      {-set date value in the ini file or registry}
    function ReadDate(const ValueName : string; const Default : TStDate) : TStDate;
      {-get date value from the ini file or registry}
    procedure WriteDateTime(const ValueName : string; const Value : TDateTime);
      {-set datetime value in the ini file or registry}
    function ReadDateTime(const ValueName : string; const Default : TDateTime) : TDateTime;
      {-get datetime value from the ini file or registry}
    procedure WriteTime(const ValueName : string; const Value : TStTime);
      {-set time value in the ini file or registry}
    function ReadTime(const ValueName : string; const Default : TStTime) : TStTime;
      {-get time value from the ini file or registry}


    procedure CreateKey(const KeyName : string);
      {-creates Section in INI file or Key in Registry}
    procedure GetSubKeys(SK : TStrings);
      {-lists sections in INI file or subkeys of SubKey in Registry}
    procedure GetValues(SKV : TStrings);
      {-lists values in INI section or in Registry SubKey}
    procedure DeleteKey(const KeyName : string; DeleteSubKeys : Boolean);
      {-Deletes section in INI file or key in Registry file}
    procedure DeleteValue(const ValueName : string);
      {-Deletes a value from an INI section or Registry key}
    procedure QueryKey(var KeyInfo : TQueryKeyInfo);
      {-lists information about an INI section or Registry SubKey}
    function KeyExists(KeyName : string) : Boolean;
      {-checks if exists in INI file/Registry}
    function IsKeyEmpty(Primary, SubKey : string) : Boolean;
      {-checks if key has values and/or subkeys}

    procedure SaveKey(const SubKey : string; FileName : string);
      {-saves an INI Section with values or Registry Subkey with all values and
        subkeys to specified file}
    procedure LoadKey(const SubKey, FileName : string);
      {-loads an INI file section or Registry key with all subkeys/values}
    procedure UnLoadKey(const SubKey : string);
      {-same as DeleteKey for INI file; removes key/subkeys loaded with LoadKey}
    procedure ReplaceKey(const SubKey, InputFile, SaveFile : string);
      {-replaces an INI file section or Registry key/subkeys
        from InputFile, saves old data in SaveFile}
    procedure RestoreKey(const SubKey, KeyFile : string; Options : DWORD);
      {-restores an INI section or Registry key/subkeys from KeyFile}

    procedure RegOpenRemoteKey(CompName : string);
      {-connects to Registry on another computer on network}
    procedure RegCloseRemoteKey;
      {-closes connection made with RegConnectRegistry}

    property Attributes : TSecurityAttributes
      read GetAttributes
      write SetAttributes;

    property CurSubKey : string
      read GetCurSubKey
      write SetCurSubKey;

    property IsIniFile : Boolean
      read GetIsIniFile;
    procedure RegGetKeySecurity(const SubKey : string; var SD : TSecurityDescriptor);
      {-gets KeySecurity information on WinNT machines}
    procedure RegSetKeySecurity(const SubKey : string; SD : TSecurityDescriptor);
      {-sets KeySecurity information on WinNT machines}
  end;


implementation

uses
  AnsiStrings;

procedure RaiseRegIniError(Code : LongInt);
var
  E : ESTRegIniError;
begin
  E := ESTRegIniError.CreateResTP(Code, 0);
  E.ErrorCode := Code;
  raise E;
end;

{==========================================================================}

procedure RaiseRegIniErrorFmt(Code : LongInt; A : array of const);
var
  E : ESTRegIniError;
begin
  E := ESTRegIniError.CreateResFmtTP(Code, A, 0);
  E.ErrorCode := Code;
  raise E;
end;

{==========================================================================}

constructor TStRegIni.Create(RootName : String; IsIniFile : Boolean);
var
  S   : string;
  OSI : TOSVERSIONINFO;
begin
{$IFDEF ThreadSafe}
  Windows.InitializeCriticalSection(riThreadSafe);
{$ENDIF}

  {check if a primary key or ini file is specified}
  if (Length(RootName) = 0) then
    RaiseRegIniError(stscNoFileKey);
  RootName := ANSIUpperCase(RootName);

  {get False string from resource}
  S := SysToolsStr(stscFalseString);
  riFalseString := StrAlloc(Length(S)); // GetMem(riFalseString,Length(S)+1);
  StrPCopy(riFalseString,S);

  {get True string from resource}
  S := SysToolsStr(stscTrueString);
  riTrueString := StrAlloc(Length(S)); // GetMem(riTrueString,Length(S)+1);
  StrPCopy(riTrueString,S);

  riCurSubKey := StrAlloc(1); // GetMem(riCurSubKey,1);
  riCurSubKey[0] := #0;

  BmpText   := TBitMap.Create;
  BmpBinary := TBitMap.Create;

  BmpText.Handle := LoadBitmap(HInstance, 'STBMPTEXT');
  BmpBinary.Handle := LoadBitmap(HInstance, 'STBMPBINARY');

  {setup ini file/primary key via riRootName}
  if (IsIniFile) then begin
    riType := riIniType;
    riRootName := StrAlloc(Length(RootName)); // GetMem(riRootName,Length(RootName)+1);
    StrPCopy(riRootName,RootName);
  end else begin
    riType := riRegType;

    riPrimaryKey := 0;
    riHoldPrimary := 0;
    if (RootName = RIMachine) then
       riPrimaryKey := HKEY_LOCAL_MACHINE
    else if (RootName = RIUsers) then
       riPrimaryKey := HKEY_USERS
    else if (RootName = RIRoot) then
       riPrimaryKey := HKEY_CLASSES_ROOT
    else if (RootName = RICUser) then
       riPrimaryKey := HKEY_CURRENT_USER
    else
       riPrimaryKey := HKEY_CURRENT_USER;

    OSI.dwOSVersionInfoSize := SizeOf(OSI);
    if (GetVersionEX(OSI)) then begin
      case OSI.dwPlatformID of
         VER_PLATFORM_WIN32S : RaiseRegIniError(stscNoWin32S);
         VER_PLATFORM_WIN32_WINDOWS : riWinVer := riWin95;
         VER_PLATFORM_WIN32_NT : riWinVer := riWinNT;
      end;
    end;

    if (FriSecAttr.nLength <> sizeOf(TSecurityAttributes)) then begin
      FriSecAttr.nLength := sizeof(TSecurityAttributes);
      FriSecAttr.lpSecurityDescriptor := nil;
      FriSecAttr.bInheritHandle := TRUE;
    end;

  end;
end;

{==========================================================================}

destructor TStRegIni.Destroy;
begin
  {no need to check for local key since none are kept open}
  {longer than needed for a specific method}
  if (riRemoteKey <> 0) then
    RegCloseRemoteKey;

  if (riRootName <> nil) then
    FreeMem(riRootName,StrLen(riRootName)+1);
  if (riFalseString <> nil) then
    FreeMem(riFalseString,StrLen(riFalseString)+1);
  if (riTrueString <> nil) then
    FreeMem(riTrueString,StrLen(riTrueString)+1);
  if (riCurSubKey <> nil) then
    FreeMem(riCurSubKey,StrLen(riCurSubKey)+1);

  BmpText.Free;
  BmpBinary.Free;

{$IFDEF ThreadSafe}
  Windows.DeleteCriticalSection(riThreadSafe);
{$ENDIF}
  inherited Destroy;
end;

{==========================================================================}


procedure TStRegIni.SetPrimary(Value : string);
  {-change working Ini file or top level key in registry}
begin
  if riType = riIniType then begin
    if CompareText(Value,StrPas(riRootName)) = 0 then Exit;

    if (riRootName <> nil) then
      StrDispose(riRootName); // FreeMem(riRootName,StrLen(riRootName)+1);
    riRootName := StrAlloc(Length(Value)); //GetMem(riRootName,Length(Value)+1);
    StrPCopy(riRootName,Value);
  end else begin
    if (riRemoteKey <> 0) then
      RegCloseRemoteKey;

    if (Value = RIMachine) then
      riPrimaryKey := HKEY_LOCAL_MACHINE
    else if (Value = RIUsers) then
       riPrimaryKey := HKEY_USERS
    else if (Value = RIRoot) then
       riPrimaryKey := HKEY_CLASSES_ROOT
    else if (Value = RICUser) then
       riPrimaryKey := HKEY_CURRENT_USER
    else
       riPrimaryKey := HKEY_CURRENT_USER;
  end;
end;

{==========================================================================}

function TStRegIni.GetPrimary : string;
  {-return working Ini file or top level registry key}
begin
  if (riType = riIniType) then
    Result := StrPas(riRootName)
  else begin
    case riPrimaryKey of
      HKEY_LOCAL_MACHINE : Result := RIMachine;
      HKEY_USERS         : Result := RIUsers;
      HKEY_CLASSES_ROOT  : Result := RIRoot;
      HKEY_CURRENT_USER  : Result := RICUser;
    else
      Result := 'Invalid primary key'
    end;
  end;
end;

{==========================================================================}

procedure TStRegIni.EnterCS;
begin
{$IFDEF ThreadSafe}
  EnterCriticalSection(riThreadSafe);
{$ENDIF}
end;

{==========================================================================}

procedure TStRegIni.LeaveCS;
begin
{$IFDEF ThreadSafe}
  LeaveCriticalSection(riThreadSafe);
{$ENDIF}
end;

{==========================================================================}

function TStRegIni.GetIsIniFile : Boolean;
  {-get whether instance is IniFile or no}
begin
  Result := riType = riIniType;
end;

{==========================================================================}

function TStRegIni.GetAttributes : TSecurityAttributes;
  {-Get current security attributes (NT Only) }
begin
  with Result do begin
    nLength := sizeof(TSecurityAttributes);
    lpSecurityDescriptor := FriSecAttr.lpSecurityDescriptor;
    bInheritHandle := FriSecAttr.bInheritHandle;
  end;
end;

{==========================================================================}

procedure TStRegIni.SetAttributes(Value : TSecurityAttributes);
  {-set security attributes (NT only) }
begin
  FriSecAttr.nLength := sizeof(TSecurityAttributes);
  FriSecAttr.lpSecurityDescriptor := Value.lpSecurityDescriptor;
  FriSecAttr.bInheritHandle := Value.bInheritHandle;
end;

{==========================================================================}

function TStRegIni.GetCurSubKey : string;
  {-retrn name of working Ini file section or registry subkey}
begin
  Result := FCurSubKey;
end;

{==========================================================================}

procedure TStRegIni.SetCurSubKey(Value : string);
  {-set name of working Ini file section or registry subkey}
begin
  if (riCurSubKey <> nil) then
    StrDispose(riCurSubKey); // FreeMem(riCurSubKey,StrLen(riCurSubKey)+1);
  FCurSubKey := Value;
  riCurSubKey := StrAlloc(Length(Value)); // GetMem(riCurSubKey,Length(Value)+1);
  StrPCopy(riCurSubKey,Value);
end;

{==========================================================================}

function TStRegIni.OpenRegKey : HKey;
  {-open a registry key}
var
  Disposition   : DWORD;
  ECode         : LongInt;
begin
  Disposition := 0;
  if (riMode = riSet) then begin
    {Keys are created with all key access privilages and as non-volatile}
    ECode := RegCreateKeyEx(riPrimaryKey, riCurSubKey,0,nil,
        REG_OPTION_NON_VOLATILE,KEY_ALL_ACCESS,@FriSecAttr,
        Result,@Disposition);
    if (ECode <> ERROR_SUCCESS) then
      RaiseRegIniErrorFmt(stscCreateKeyFail, [ECode]);
  end else begin
    {Read operations limit key access to read only}
    ECode := RegOpenKeyEx(riPrimaryKey,riCurSubKey, 0, KEY_READ,Result);
    if (ECode <> ERROR_SUCCESS) then
      RaiseRegIniErrorFmt(stscOpenKeyFail, [ECode]);
  end;
end;

{==========================================================================}

procedure TStRegIni.CloseRegKey(const Key : HKey);
  {-close registry key}
begin
  RegCloseKey(Key);
end;

{==========================================================================}

function TStRegIni.WriteIniData(const ValueName : string;
                                      Data      : String) : Boolean;
  {-write data to the Ini file in the working section}
var
  PData,
  PValueName : PChar;
  VNLen,
  DLen       : integer;
begin
  if (ValueName = '') then
    RaiseRegIniError(stscNoValueNameSpecified);

  PData := nil;
  PValueName := nil;
  VNLen := Length(ValueName) + 1;
  DLen  := Length(Data) + 1;

  try
    PValueName := StrAlloc(VNLen); // GetMem(PValueName, VNLen);
    PData := StrAlloc(DLen); // GetMem(PData, DLen);

    strPCopy(PValueName, ValueName);
    strPCopy(PData, Data);

    Result := WritePrivateProfileString(riCurSubKey, PValueName,
                                        PData, riRootName)
  finally
    if PValueName <> nil then
      StrDispose(PValueName); // FreeMem(PValueName, VNLen);
    if PData <> nil then
      StrDispose(PData); // FreeMem(PData, DLen);
  end;
end;

{==========================================================================}

function TStRegIni.ReadIniData(const ValueName : string; var Value : String;
                               Default : String) : Integer;
  {-read a value from the working section of the Ini file}
var
  PValue   : array[0..1024] of char;
  PVName,
  PDefault : PChar;
begin
  PDefault := nil;
  PVName := nil;

  try
    PVName := StrAlloc(Length(ValueName)); // GetMem(PVName,Length(ValueName)+1);
    PDefault := StrAlloc(Length(Default)); // GetMem(PDefault,Length(Default)+1);

    StrPCopy(PVName,ValueName);
    StrPCopy(PDefault,Default);

    GetPrivateProfileString(riCurSubKey,PVName,PDefault,
        PValue,Length(PValue)-1,riRootName);

    Value := StrPas(PValue);
    Result := Length(Value);
  finally
    if PVName <> nil then
      StrDispose(PVName); // FreeMem(PVName,strlen(PVName)+1);
    if PDefault <> nil then
      StrDispose(PDefault); // FreeMem(PDefault,strlen(PDefault)+1);
  end;
end;

{==========================================================================}

function TStRegIni.WriteRegData(Key : HKey; const ValueName : string; Data : Pointer;
                                DType : DWORD; Size : Integer) : LongInt;
  {-write a value into the registry}
begin
  Result := RegSetValueEx(Key, PChar(ValueName), 0, DType, Data, Size);
end;

{==========================================================================}

function TStRegIni.GetDataInfo(Key : HKey; const ValueName : string;
                               var Size : LongInt; var DType : DWORD) : LongInt;
  {-get the size and type of a specific value in the registry}
var
  PVName : PChar;
  Opened : Boolean;
  TS     : string;
begin
  Opened := False;
  riMode := riGet;
  if (riType = riIniType) then begin
    TS := ReadString(ValueName,'');
    Size := Length(TS);
    DType := REG_SZ;
    Result := ERROR_SUCCESS;
    Exit;
  end;

  PVName := StrAlloc(Length(ValueName)); //GetMem(PVName,Length(ValueName)+1);
  try
    StrPCopy(PVName,ValueName);
    if Key = 0 then begin
      Key := OpenRegKey;
      Opened := True;
    end;
    Result := RegQueryValueEx(Key,PVName,nil,@DType,nil,LPDWORD(@Size));
  finally
    StrDispose(PVName); // FreeMem(PVName,strlen(PVName)+1);
  end;
  if Opened then
    RegCloseKey(Key);
end;

{==========================================================================}

function TStRegIni.ReadRegData(Key : HKey; const ValueName : string; Data : Pointer;
                               Size : LongInt; DType : DWORD) : LongInt;
  {-read a value from the registry}
var
  PVName : PChar;
begin
  PVName := StrAlloc(Length(ValueName)); // GetMem(PVName,(Length(ValueName)+1) * SizeOf(Char));
  try
    StrPCopy(PVName,ValueName);
    DType := REG_NONE;
    Result := RegQueryValueEx(Key, PVName, nil,@DType,PByte(Data),LPDWORD(@Size));
  finally
    StrDispose(PVName); // FreeMem(PVName,strlen(PVName)+1);
  end;
end;

{==========================================================================}

function TStRegIni.GetFullKeyPath : string;
begin
{$IFDEF ThreadSafe}
  EnterCS;
  try
{$ENDIF}
    if (riType = riIniType) then begin
      Result := StrPas(riRootName) + '\' + StrPas(riCurSubKey);
    end else begin
      case riPrimaryKey of

        HKEY_LOCAL_MACHINE : Result := 'HKEY_LOCAL_MACHINE\';
        HKEY_USERS         : Result := 'HKEY_USERS\';
        HKEY_CLASSES_ROOT  : Result := 'HKEY_CLASSES_ROOT\';
        HKEY_CURRENT_USER  : Result := 'HKEY_CURRENT_USER\';
      end;
      Result := Result + StrPas(riCurSubKey);
    end;
{$IFDEF ThreadSafe}
  finally
    LeaveCS;
  end;
{$ENDIF}
end;

{==========================================================================}

procedure TStRegIni.WriteBoolean(const ValueName : string; Value : Boolean);
  {-write Boolean value to the Ini file or registry}
var
  ECode    : LongInt;
  IValue   : DWORD;
  Key      : HKey;
  wResult  : Boolean;

begin
  riMode := riSet;
{$IFDEF ThreadSafe}
  EnterCS;
  try
{$ENDIF}
    if (riType = riIniType) then begin
      if (Value) then
        wResult := WriteIniData(ValueName, StrPas(riTrueString))
      else
        wResult := WriteIniData(ValueName, StrPas(riFalseString));
      if (NOT wResult) then
        RaiseRegIniError(stscIniWriteFail);
    end else begin
      Key := OpenRegKey;
      try
        IValue := Ord(Value);
        ECode := WriteRegData(Key,ValueName,@IValue,REG_DWORD,SizeOf(DWORD));
        if (ECode <> ERROR_SUCCESS) then
          RaiseRegIniErrorFmt(stscRegWriteFail,[ECode]);
      finally
        if (riRemoteKey = 0) then
          CloseRegKey(Key);
      end;
    end;
{$IFDEF ThreadSafe}
  finally
    LeaveCS;
  end;
{$ENDIF}
end;

{==========================================================================}

function TStRegIni.ReadBoolean(const ValueName : string; Default : Boolean) : Boolean;
  {-read a Boolean value from the Ini file or registry}
var
  Value      : string;
  IVal       : Double;
  Key        : HKey;
  ECode,

  ValSize    : LongInt;
  ValType    : DWORD;
  LResult    : Pointer;
  Code       : Integer;

begin
  riMode := riGet;
{$IFDEF ThreadSafe}
  EnterCS;
  try
{$ENDIF}
    if (riType = riIniType) then begin
      if Default then
        ReadIniData(ValueName,Value,StrPas(riTrueString))
      else
        ReadIniData(ValueName,Value,StrPas(riFalseString));

      if (CompareText(Value,StrPas(riFalseString)) = 0) then
        Result := False
      else begin
        if (CompareText(Value,StrPas(riTrueString)) = 0) then
          Result := True
        else begin
          Val(Value,IVal,Code);
          if (Code = 0) then
            Result := IVal <> 0
          else
            Result := Default;
        end;
      end;

    end else begin
      try
        Key := OpenRegKey;
      except
        Result := Default;
        Exit;
      end;
      try
        {get info on requested value}
        ECode := GetDataInfo(Key,ValueName,ValSize,ValType);
        if (ECode <> ERROR_SUCCESS) then begin
          Result := Default;
          Exit;
        end;

        {Size does not include null terminator for strings}
        if (ValType = REG_SZ) OR (ValType = REG_EXPAND_SZ) then
        begin
          Inc(ValSize);
          {$IFDEF UNICODE}
          ValSize := ValSize * 2;
          {$ENDIF}
        end;
        GetMem(LResult,ValSize);
        try
          ECode := ReadRegData(Key,ValueName,LResult,ValSize,ValType);
          if (ECode <> ERROR_SUCCESS) then
            Result := Default
          else begin
            {convert data, if possible, to Boolean}
            case (ValType) of
              REG_SZ,
              REG_EXPAND_SZ : Result := StrIComp(PChar(LResult),riFalseString) <> 0;
              REG_BINARY,
              REG_DWORD     : Result := (LongInt(LResult^) <> 0);
            else
              Result := Default;
            end;
          end;
        finally
          FreeMem(LResult,ValSize);
        end;
      finally
        if (riRemoteKey = 0) then
          CloseRegKey(Key);
      end;
    end;
{$IFDEF ThreadSafe}
  finally
    LeaveCS;
  end;
{$ENDIF}
end;

{==========================================================================}

procedure TStRegIni.WriteInteger(const ValueName : string; Value : DWORD);
  {-write an integer to the Ini file or the registry}
var
  ECode   : LongInt;
  Key     : HKey;

begin
  riMode := riSet;
{$IFDEF ThreadSafe}
  EnterCS;
  try
{$ENDIF}
    if (riType = riIniType) then begin
      if (NOT WriteIniData(ValueName,IntToStr(Value))) then
        RaiseRegIniError(stscIniWriteFail);
    end else begin
      Key := OpenRegKey;
      try
        ECode := WriteRegData(Key,ValueName,@Value,REG_DWORD,SizeOf(DWORD));
        if (ECode <> ERROR_SUCCESS) then
          RaiseRegIniErrorFmt(stscRegWriteFail,[ECode]);
      finally
        if (riRemoteKey = 0) then
          CloseRegKey(Key);
      end;
    end;
{$IFDEF ThreadSafe}
  finally
    LeaveCS;
  end;
{$ENDIF}
end;

{==========================================================================}

function TStRegIni.ReadInteger(const ValueName : string; Default : DWORD) : DWORD;
  {-read an integer from the Ini file or registry}
var
  Value      : string;

  ECode,
  Key        : HKey;
  Len        : LongInt;
  ValSize    : LongInt;
  ValType    : DWORD;

  LResult    : Pointer;
  Code       : Integer;
begin
  riMode := riGet;
{$IFDEF ThreadSafe}
  EnterCS;
  try
{$ENDIF}
    if (riType = riIniType) then begin
      Len := ReadIniData(ValueName,Value,IntToStr(Default));
      if (Len > 0) then begin
        Val(Value,Result,Code);
        if (Code <> 0) then
          Result := Default;
      end else
        Result := Default;
    end else begin
      try
        Key := OpenRegKey;
      except
        Result := Default;
        Exit;
      end;
      try
        {get info on requested value}
        ECode := GetDataInfo(Key,ValueName,ValSize,ValType);
        if (ECode <> ERROR_SUCCESS) then begin
          Result := Default;
          Exit;
        end;

        {Size does not include null terminator for strings}
        if (ValType = REG_SZ) OR (ValType = REG_EXPAND_SZ) then
        begin
          Inc(ValSize);
          {$IFDEF UNICODE}
          ValSize := ValSize * 2;
          {$ENDIF}
        end;
        GetMem(LResult,ValSize);
        try
          ECode := ReadRegData(Key,ValueName,LResult,ValSize,ValType);
          if (ECode <> ERROR_SUCCESS) then
            Result := Default
          else begin
            {convert data, if possible, to an integer value}
            case (ValType) of
              REG_SZ,
              REG_EXPAND_SZ : begin
                                Value := StrPas(PChar(LResult));
                                Val(Value,Result,Code);
                                if (Code <> 0) then
                                  Result := Default;
                              end;
              REG_BINARY,
              REG_DWORD     : Result := DWORD(LResult^);
            else
              Result := Default;
            end;
          end;
        finally
          FreeMem(LResult,ValSize);
        end;
      finally
        if (riRemoteKey = 0) then
          CloseRegKey(Key);
      end;
    end;
{$IFDEF ThreadSafe}
  finally
    LeaveCS;
  end;
{$ENDIF}
end;

{==========================================================================}

function TStRegIni.BytesToString(Value : PByte; Size : Cardinal) : AnsiString;
  {-convert byte array to string, no spaces or hex enunciators, e.g., '$'}
var
  I,
  Index  : Cardinal;
  S      : String[3];

begin
  SetLength(Result,2*Size);

  for I := 1 to Size do begin
    Index := I*2;
    S := HexBL(Byte(PAnsiChar(Value)[I-1]));
    Result[(Index)-1] := S[1];
    Result[Index] := S[2];
  end;
end;

{==========================================================================}

function TStRegIni.StringToBytes(const IString : AnsiString; var Value; Size : Cardinal) : Boolean;
  {-convert string (by groups of 2 char) to byte values}
var
  Code,
  Index,
  I     : Integer;
  Q     : array[1..MaxByteArraySize] of byte;
  S     : array[1..3] of AnsiChar;
begin
  if ((Length(IString) div 2) <> LongInt(Size)) then begin
    Result := False;
    Exit;
  end;

  Result := True;
  for I := 1 to Size do begin
    Index := (2*(I-1))+1;
    S[1] := '$';
    S[2] := IString[Index];
    S[3] := IString[Index+1];
    Val(string(S),Q[I],Code);
    if (Code <> 0) then begin
      Result := False;
      Exit;
    end;
 end;
  Move(Q, Value, Size);
end;

{==========================================================================}

procedure TStRegIni.WriteBinaryData(const ValueName : string; const Value; Size : Integer);
  {-write binary data of any form to Ini file or registry}
var
  SValue : string;
  ECode  : LongInt;
  Key    : HKey;
begin
  riMode := riSet;
{$IFDEF ThreadSafe}
  EnterCS;
  try
{$ENDIF}
    if (riType = riIniType) then begin
      if (Size > MaxByteArraySize) then
        RaiseRegIniError(stscByteArrayTooLarge);
      SValue := string(BytesToString(PByte(@Value),Size));
      if (NOT WriteIniData(ValueName,SValue)) then
        RaiseRegIniError(stscIniWriteFail);
    end else begin
      Key := OpenRegKey;
      try
        ECode := WriteRegData(Key,ValueName,@Value,REG_BINARY,Size);
        if (ECode <> ERROR_SUCCESS) then
          RaiseRegIniErrorFmt(stscRegWriteFail,[ECode]);
      finally
        if (riRemoteKey = 0) then
          CloseRegKey(Key);
      end;
    end;
{$IFDEF ThreadSafe}
  finally
    LeaveCS;
  end;
{$ENDIF}
end;

{==========================================================================}

procedure TStRegIni.ReadBinaryData(const ValueName : string; const Default;
                                     var Value; var Size : Integer);
  {-read binary data of any form from Ini file or regsitry}
var
  ECode     : LongInt;
  Key       : HKey;
  Len       : Cardinal;

  ValSize   : LongInt;
  ValType   : DWORD;

  DefVals,
  Values    : String;

begin
  riMode := riGet;
{$IFDEF ThreadSafe}
  EnterCS;
  try
{$ENDIF}
    if (riType = riIniType) then begin
      DefVals := string(BytesToString(PByte(@Default), Size));
      Len := ReadIniData(ValueName, Values, DefVals);
      if (Len mod 2 = 0) then begin
        {covert string, if possible, to series of bytes}
        if not (StringToBytes(AnsiString(Values), PByte(Value), Size)) then
          Move(Default, PByte(Value), Size);
      end else
        Move(Default, PByte(Value), Size);
    end else begin
      try
        Key := OpenRegKey;
      except
        Move(Default, Value, Size);
        Exit;
      end;
      try
        {get info on requested value}
        ECode := GetDataInfo(Key, ValueName, ValSize, ValType);
        if (ECode <> ERROR_SUCCESS) then begin
          Move(Default, Value, Size);
          Exit;
        end;

        if (ValSize <> Size) then
          RaiseRegIniErrorFmt(stscBufferDataSizesDif, [Size,ValSize])
        else
          Size := ValSize;

        if (ValType <> REG_BINARY) then
          Move(Default, Value, Size)
        else begin
          ECode := ReadRegData(Key, ValueName, PByte(@Value), ValSize, ValType);
          if (ECode <> ERROR_SUCCESS) then
            Move(Default, Value, Size)
        end;
      finally
        if (riRemoteKey = 0) then
          CloseRegKey(Key);
      end;
    end;
{$IFDEF ThreadSafe}
  finally
    LeaveCS;
  end;
{$ENDIF}
end;

{==========================================================================}

procedure TStRegIni.WriteString(const ValueName : string; const Value : string);
  {-write a string to the Ini file or registry}
var
  ECode  : LongInt;
  Key    : HKey;
  PValue : PChar;
begin
  riMode := riSet;
{$IFDEF ThreadSafe}
  EnterCS;
  try
{$ENDIF}
    if (riType = riIniType) then begin
      if NOT WriteIniData(ValueName, Value) then
        RaiseRegIniError(stscIniWriteFail);
    end else begin
      PValue := StrAlloc(Length(Value)); // GetMem(PValue, Length(Value)+1);
      try
        StrPCopy(PValue, Value);
        Key := OpenRegKey;
        try
          {same call for 16/32 since we're using a PChar}
          ECode := WriteRegData(Key,ValueName, PValue,REG_SZ, (strlen(PValue)+1) * SizeOf(Char));
          if (ECode <> ERROR_SUCCESS) then
            RaiseRegIniErrorFmt(stscRegWriteFail,[ECode]);
        finally
          if (riRemoteKey = 0) then
            CloseRegKey(Key);
        end;
      finally
        StrDispose(PValue); // FreeMem(PValue,strlen(PValue)+1);
      end;
    end;
{$IFDEF ThreadSafe}
  finally
    LeaveCS;
  end;
{$ENDIF}
end;

{==========================================================================}

function TStRegIni.ReadString(const ValueName : string; const Default : string) : string;
  {-read a string from an Ini file or the registry}
var
  ECode     : LongInt;
  Len       : LongInt;
  ValSize   : LongInt;
  Key       : HKey;
  ValType   : DWORD;
  TmpVal    : DWORD;
  LResult   : Pointer;
  sBuffer: ShortString;

begin
  riMode := riGet;
{$IFDEF ThreadSafe}
  EnterCS;
  try
{$ENDIF}
    if (riType = riIniType) then begin
      Len := ReadIniData(ValueName,Result,Default);
      if (Len < 1) then
        Result := Default;
    end else begin
      try
        Key := OpenRegKey;
      except
        Result := Default;
        Exit;
      end;
      try
        {get info on requested value}
        ECode := GetDataInfo(Key,ValueName,ValSize,ValType);
        if (ECode <> ERROR_SUCCESS) then begin
          Result := Default;
          Exit;
        end;

        if (ValType = REG_SZ) OR (ValType = REG_EXPAND_SZ)then
        begin
          Inc(ValSize);
          {$IFDEF UNICODE}
          ValSize := ValSize * 2;
          {$ENDIF}
        end;
        GetMem(LResult,ValSize);
        try
          ECode := ReadRegData(Key,ValueName,LResult,ValSize,ValType);
          if (ECode <> ERROR_SUCCESS) AND (ECode <> ERROR_MORE_DATA) then
            Result := Default
          else begin
            {convert data, if possible, to string}
            case (ValType) of
              REG_SZ,
              REG_EXPAND_SZ : Result := StrPas(PChar(LResult));
              REG_BINARY   : begin
                               if (ValSize > MaxByteArraySize) then
                                 RaiseRegIniError(stscByteArrayTooLarge);
                               Result := string(BytesToString(PByte(@LResult),ValSize));
                             end;
              REG_DWORD    : begin
                               TmpVal := DWORD(LResult^);
                               Str(TmpVal,sBuffer);
                               Result := string(sBuffer);
                             end;
            else
              Result := Default;
              end;
            end;
        finally
          FreeMem(LResult,ValSize);
        end;
      finally
        if (riRemoteKey = 0) then
          CloseRegKey(Key);
      end;
    end;
{$IFDEF ThreadSafe}
  finally
    LeaveCS;
  end;
{$ENDIF}
end;

{==========================================================================}

procedure TStRegIni.WriteFloat(const ValueName : string; const Value : Double);
  {-write floating point number to Ini file or registry}
var
  ECode   : LongInt;
  Key     : HKey;
  SValue  : string;
  sBuffer: ShortString;
begin
  riMode := riSet;
{$IFDEF ThreadSafe}
  EnterCS;
  try
{$ENDIF}
    Str(Value, sBuffer);
    SValue := string(sBuffer);
    while (SValue[1] = ' ') do
      System.Delete(SValue, 1, 1);
    if (riType = riIniType) then begin
      if (NOT WriteIniData(ValueName, SValue)) then
        RaiseRegIniError(stscIniWriteFail);
    end else begin
      Key := OpenRegKey;
      try
        ECode := WriteRegData(Key,ValueName,@Value,REG_BINARY,SizeOf(Double));
        if (ECode <> ERROR_SUCCESS) then
          RaiseRegIniErrorFmt(stscRegWriteFail,[ECode]);
      finally
        if (riRemoteKey = 0) then
          CloseRegKey(Key);
      end;
    end;
{$IFDEF ThreadSafe}
  finally
    LeaveCS;
  end;
{$ENDIF}
end;

{==========================================================================}

function TStRegIni.ReadFloat(const ValueName : string; const Default : TStFloat) : TStFloat;
  {-read floating point value from Ini file or registry}
var
  SDefault,
  Value      : string;
  sBuffer: ShortString;
  ECode,
  Key        : HKey;
  Len        : LongInt;
  ValSize    : LongInt;
  ValType    : DWORD;

  LResult    : Pointer;
  Code       : integer;

begin
  riMode := riGet;
{$IFDEF ThreadSafe}
  EnterCS;
  try
{$ENDIF}
    if (riType = riIniType) then begin
      Str(Default,sBuffer);
      SDefault := string(sBuffer);
      Len := ReadIniData(ValueName,Value,SDefault);
      if (Len > 0) then begin
        Val(Value,Result,Code);
        if (Code <> 0) then
          Result := Default;
      end else
        Result := Default;
    end else begin
      try
        Key := OpenRegKey;
      except
        Result := Default;
        Exit;
      end;
      try
        ECode := GetDataInfo(Key,ValueName,ValSize,ValType);

        if (ECode <> ERROR_SUCCESS) then begin
          Result := Default;
          Exit;
        end;

        {Size does not include null terminator for strings}
        if (ValType = REG_SZ) OR (ValType = REG_EXPAND_SZ) then
        begin
          Inc(ValSize);
          {$IFDEF UNICODE}
          ValSize := ValSize * 2;
          {$ENDIF}
        end;

        GetMem(LResult,ValSize);
        try
          ECode := ReadRegData(Key,ValueName,LResult,ValSize,ValType);
          if (ECode <> ERROR_SUCCESS) then
            Result := Default
          else begin
            {convert data, if possible, to floating point number}
            case (ValType) of
              REG_SZ,
              REG_EXPAND_SZ : begin
                                Value := StrPas(PChar(LResult));
                                Val(Value,Result,Code);
                                if (Code <> 0) then
                                  Result := Default;
                              end;
              REG_BINARY,
              REG_DWORD     : Result := Double(LResult^);
            else
              Result := Default;
            end;
          end;
        finally
          FreeMem(LResult,ValSize);
        end;
      finally
        if (riRemoteKey = 0) then
          CloseRegKey(Key);
      end;
    end;
{$IFDEF ThreadSafe}
  finally
    LeaveCS;
  end;
{$ENDIF}
end;

{==========================================================================}

procedure TStRegIni.WriteDateTime(const ValueName : string; const Value : TDateTime);
  {-write a Delphi DateTime to Ini file or registry}
var
  ECode   : LongInt;
  Key     : HKey;
  SValue  : string;
  sBuffer: ShortString;
begin
  riMode := riSet;
{$IFDEF ThreadSafe}
  EnterCS;
  try
{$ENDIF}
    Str(Value,sBuffer);
    SValue := string(sBuffer);
    if (riType = riIniType) then begin
      if (NOT WriteIniData(ValueName,SValue)) then
        RaiseRegIniError(stscIniWriteFail);
    end else begin
      Key := OpenRegKey;
      try
        ECode := WriteRegData(Key,ValueName,@Value,REG_BINARY,SizeOf(TDateTime));
        if (ECode <> ERROR_SUCCESS) then
          RaiseRegIniErrorFmt(stscRegWriteFail,[ECode]);
      finally
        if (riRemoteKey = 0) then
          CloseRegKey(Key);
      end;
    end;
{$IFDEF ThreadSafe}
  finally
    LeaveCS;
  end;
{$ENDIF}
end;

{==========================================================================}

function TStRegIni.ReadDateTime(const ValueName : string; const Default : TDateTime) : TDateTime;
  {-read a Delphi DateTime from the Ini file or registry}
var
  SDefault,
  Value      : string;

  ECode,
  Key        : HKey;
  Len        : LongInt;
  ValSize    : LongInt;
  ValType    : DWORD;

  LResult    : Pointer;
  Code       : integer;
  sBuffer: ShortString;
begin
  riMode := riGet;
{$IFDEF ThreadSafe}
  EnterCS;
  try
{$ENDIF}
    if (riType = riIniType) then begin
      Str(Default,sBuffer);
      SDefault := string(sBuffer);
      Len := ReadIniData(ValueName,Value,SDefault);
      if (Len > 0) then begin
        Val(Value,Result,Code);
        if (Code <> 0) then
          Result := Default;
      end else
        Result := Default;
    end else begin
      try
        Key := OpenRegKey;
      except
        Result := Default;
        Exit;
      end;
      try
        ECode := GetDataInfo(Key,ValueName,ValSize,ValType);

        if (ECode <> ERROR_SUCCESS) then begin
          Result := Default;
          Exit;
        end;

        {Size does not include null terminator for strings}
        if (ValType = REG_SZ) OR (ValType = REG_EXPAND_SZ) then
        begin
          Inc(ValSize);
          {$IFDEF UNICODE}
          ValSize := ValSize * 2;
          {$ENDIF}
        end;
        GetMem(LResult,ValSize);
        try
          ECode := ReadRegData(Key,ValueName,LResult,ValSize,ValType);
          if (ECode <> ERROR_SUCCESS) then
            Result := Default
          else begin
            {covert data, if possible, to DateTime value}
            case (ValType) of
              REG_SZ,
              REG_EXPAND_SZ : begin
                                Value := string(AnsiStrings.StrPas(PAnsiChar(LResult)));
                                Val(Value,Result,Code);
                                if (Code <> 0) then
                                  Result := Default;
                              end;
              REG_BINARY,
              REG_DWORD     : Result := TDateTime(LResult^);
            else
              Result := Default;
            end;
          end;
        finally
          FreeMem(LResult,ValSize);
        end;
      finally
        if (riRemoteKey = 0) then
          CloseRegKey(Key);
      end;
    end;
{$IFDEF ThreadSafe}
  finally
    LeaveCS;
  end;
{$ENDIF}
end;

{==========================================================================}

procedure TStRegIni.WriteDate(const ValueName : string; const Value : TStDate);
  {-write a SysTools Date to Ini file or registry}
begin
  WriteInteger(ValueName,DWORD(Value));
end;

{==========================================================================}

function TStRegIni.ReadDate(const ValueName : string; const Default : TStDate) : TStDate;
  {-read a SysTools Date from Ini file or registry}
begin
  Result := TStDate(ReadInteger(ValueName,DWORD(Default)));
end;

{==========================================================================}

procedure TStRegIni.WriteTime(const ValueName : string; const Value : TStTime);
  {-write SysTools Time to Ini file or registry}
begin
  WriteInteger(ValueName,DWORD(Value));
end;

{==========================================================================}

function TStRegIni.ReadTime(const ValueName : string; const Default : TStTime) : TStTime;
  {-read SysTools Time from Ini file or registry}
begin
  Result := TStTime(ReadInteger(ValueName,DWORD(Default)));
end;

{==========================================================================}

procedure TStRegIni.CreateKey(const KeyName : string);
  {-create a new section in Ini file or subkey in registry}
const
  TempValueName = '$ABC123098FED';
var
  Disposition   : DWORD;
  ECode         : LongInt;
  newKey        : HKey;
  PCSKey,
  PSKey         : PChar;
  HoldKey       : HKey;
begin
{$IFDEF ThreadSafe}
  EnterCS;
  try
{$ENDIF}
    if (Length(KeyName) = 0) then
      RaiseRegIniError(stscNoKeyName);

    if (riType = riIniType) then begin
      PSKey := StrAlloc(Length(KeyName)); // GetMem(PSKey,Length(KeyName)+1);
      try
        StrPCopy(PSKey,KeyName);
        {Create Section with temporary value}
        if (NOT WritePrivateProfileString(PSKey,TempValueName,' ',riRootName)) then
          RaiseRegIniError(stscCreateKeyFail);
        {Delete temporary value but leave section intact}
        if (NOT WritePrivateProfileString(PSKey,TempValueName,nil,riRootName)) then
          RaiseRegIniError(stscIniWriteFail);
      finally
        StrDispose(PSKey); // FreeMem(PSKey,Length(KeyName)+1);
      end;
    end else begin
      HoldKey := 0;
      PCSKey := StrAlloc(Length(KeyName) + Integer(StrLen(riCurSubKey)) + 2); // GetMem(PCSKey, Length(KeyName)+1 + LongInt(strlen(riCurSubkey))+2);
      PSKey := StrAlloc(Length(KeyName)); // GetMem(PSKey, Length(KeyName)+1);
      try
        PCSKey[0] := #0;
        StrPCopy(PSKey,KeyName);
        if riCurSubKey[0] <> #0 then
          strcat(Strcopy(PCSKey, riCurSubKey), '\');
        strcat(PCSKey, PSKey);
        if (riRemoteKey <> 0) then begin
          HoldKey := riPrimaryKey;
          riPrimaryKey := riRemoteKey;
        end;
        Disposition := 0;
        {creates a new key or opens an existing key}
        ECode := RegCreateKeyEx(riPrimaryKey,PCSKey,0,nil,
                 REG_OPTION_NON_VOLATILE,KEY_ALL_ACCESS,@FriSecAttr,
                 newKey,@Disposition);
        if (ECode <> ERROR_SUCCESS) then
          RaiseRegIniErrorFmt(stscCreateKeyFail,[ECode]);

        {don't leave a key open longer than it's needed}
        RegCloseKey(newKey);
      finally
        if (HoldKey <> 0) then
          riPrimaryKey := HoldKey;
        StrDispose(PSKey); // FreeMem(PSKey,Length(KeyName)+1);
        StrDispose(PCSKey); // FreeMem(PCSKey, Length(KeyName)+1 + LongInt(strlen(riCurSubkey))+2);
      end;
    end;
{$IFDEF ThreadSafe}
  finally
    LeaveCS;
  end;
{$ENDIF}
end;

{==========================================================================}

procedure TStRegIni.ParseIniFile(SList : TStrings);
{-procedure to read through an INI text file}
var
   F : TextFile;
   L : string;
begin
  AssignFile(F, riRootName);
  Reset(F);
  try
    Readln(F,L);
    while NOT EOF(F) do begin
      if (L[1] = '[') AND (L[Length(L)] = ']') then begin
        Delete(L, Length(L), 1);
        Delete(L, 1, 1);
        SList.Add(L);
      end;
      Readln(F,L);
    end;
  finally
    CloseFile(F);
  end;
end;

{==========================================================================}

procedure TStRegIni.GetSubKeys(SK : TStrings);
  {-get list of section names (or values) from Ini file or subkeys in registry}
  {For Ini files only: if riCurSubKey =  '', list is of section names}
  {                    if riCurSubKey <> '', list is of value names in section}
var
  ValueName     : PChar;

  Sections,
  valuePos,
  NumSubKeys,
  LongSKName,
  LongVName,
  NumVals,
  MaxSize,
  VSize         : DWORD;
  Buffer        : array[0..MaxBufSize] of Char;
  S             : string;
  ECode         : LongInt;
  Key           : HKey;

begin
  riMode := riGet;
{$IFDEF ThreadSafe}
  EnterCS;
  try
{$ENDIF}
    SK.Clear;

    if (riType = riIniType) then begin
      Buffer[0] := #0;
      if (riCurSubKey[0] = #0) then begin
        {Get section names in ini file}
        Sections := GetPrivateProfileSectionNames(Buffer,MaxBufSize,riRootName);
      end else
        {get value names in specified section}
        Sections := GetPrivateProfileString(riCurSubKey,nil,#0,
                    Buffer,MaxBufSize,riRootName);

      {parse Section Names from Buffer string}
      if (Sections > 0) then begin
        valuePos := 0;
        repeat
          S := StrPas(Buffer+valuePos);
          if (Length(S) > 0) then begin
            SK.Add(S);
            Inc(valuePos,StrEnd(Buffer+valuePos)-(Buffer+valuePos)+1);
          end else
            break;
        until Length(S) = 0;
      end;
    end else begin
      Key := OpenRegKey;
      try
        ECode := RegQueryInfoKey(Key,nil,nil,nil,@NumSubKeys,
                   @LongSKName,nil,@NumVals,@LongVName,@MaxSize,nil,nil);
        if (ECode <> ERROR_SUCCESS) then
          RaiseRegIniErrorFmt(stscQueryKeyFail,[ECode]);
        Inc(LongSKName);
        valuePos := 0;
        ValueName := StrAlloc(LongSKName); // GetMem(ValueName,LongSKName);
        try
          while valuePos < NumSubKeys do begin
            ValueName[0] := #0;
            VSize := LongSKName;
            ECode := RegEnumKeyEx(Key,valuePos,ValueName,VSize,
                       nil,nil,nil,nil);
            if (ECode <> ERROR_SUCCESS) AND
               (ECode <> ERROR_MORE_DATA) then
              RaiseRegIniErrorFmt(stscEnumKeyFail,[ECode]);
            SK.Add(StrPas(ValueName));
            Inc(valuePos);
          end;
        finally
          StrDispose(ValueName); // FreeMem(ValueName,LongSKName);
        end;
      finally
        if (riRemoteKey = 0) then
          CloseRegKey(Key);
      end;
    end;
{$IFDEF ThreadSafe}
  finally
    LeaveCS;
  end;
{$ENDIF}
end;

{==========================================================================}

procedure TStRegIni.GetValues(SKV : TStrings);
  {-return value names and string representation of data in}
  {Ini file section or registry subkey}
var
  ValueName    : PChar;

  valuePos,
  NumSubKeys,
  LongSKName,
  LongVName,
  NumVals,
  MaxSize,
  VSize,
  DSize        : DWORD;

  S, TS        : string;
  KeyList      : TStringList;
  ECode        : LongInt;
  Key          : HKey;

  ValType      : DWORD;
  LResult      : Pointer;

  sBuffer: ShortString;
begin
  riMode := riGet;
{$IFDEF ThreadSafe}
  EnterCS;
  try
{$ENDIF}
    SKV.Clear;

    if (riType = riIniType) then begin
      KeyList := TStringList.Create;
      try
        {get list of value names in section}
        GetSubKeys(KeyList);
        if (KeyList.Count > 0) then begin
          for valuePos := 0 to KeyList.Count-1 do begin
            S := KeyList[valuePos] + '='
               + ReadString(KeyList[valuePos],'');
            SKV.AddObject(S,BmpText);
          end;
        end;
      finally
        KeyList.Free;
      end;
    end else begin
      Key := OpenRegKey;
      try
        {get data on specified keys}
        ECode := RegQueryInfoKey(Key,nil,nil,nil,
                   @NumSubKeys,@LongSKName,nil,@NumVals,
                   @LongVName,@MaxSize,nil,nil);
        if (ECode <> ERROR_SUCCESS) then
          RaiseRegIniErrorFmt(stscQueryKeyFail,[ECode]);
        Inc(MaxSize);
        Inc(LongVName);
        GetMem(LResult,MaxSize);
        try
          valuePos := 0;
          ValueName := StrAlloc(LongVName); // GetMem(ValueName,LongVName);
          try
            {step through values in subkey and get data from each}
            while valuePos < NumVals do begin
              ValueName[0] := #0;
              VSize := LongVName;
              DSize := MaxSize;
              ECode := RegEnumValue(Key,valuePos,ValueName,
                         VSize,nil,@ValType,LResult,@DSize);
              if (ECode <> ERROR_SUCCESS) AND
                 (ECode <> ERROR_MORE_DATA) then
                RaiseRegIniErrorFmt(stscEnumValueFail,[ECode]);
              if (Length(ValueName) > 0) then
                S := StrPas(ValueName) + '='
              else
                S := 'Default=';
              case ValType of
                {convert data to string representation}
                REG_SZ,
                REG_EXPAND_SZ : begin
                                  TS := StrPas(PChar(LResult));
                                  S := S + TS;
                                  SKV.AddObject(S,BmpText);
                                end;

                REG_DWORD,
                REG_BINARY     : begin
                                  if ValType = REG_DWORD then
                                  begin
                                    Str(LongInt(LResult^),sBuffer);
                                    TS := string(sBuffer);
                                  end
                                  else
                                    TS := string(BytesToString(PByte(LResult),DSize));
                                  S := S + TS;
                                  SKV.AddObject(S,BmpBinary);
                                end;
              end;
              Inc(valuePos);
            end;
          finally
            StrDispose(ValueName); // FreeMem(ValueName,LongVName);
          end;
        finally
          FreeMem(LResult,MaxSize);
        end;
      finally
        if (riRemoteKey = 0) then
          CloseRegKey(Key);
      end;
    end;
{$IFDEF ThreadSafe}
  finally
    LeaveCS;
  end;
{$ENDIF}
end;

{==========================================================================}

procedure TStRegIni.DeleteKey(const KeyName : string; DeleteSubKeys : Boolean);
  {-delete a section from Ini file or subkey from registry}
  {if DeleteSubKeys = True  : specified section (key) and values (subkeys),}
  {                           if any, are deleted                          }
  {                 = False : specified section (key) can not be deleted   }
  {                           if there are any values (subkeys)            }
var
  PSKey      : PChar;
  NumSubKeys,
  NumValues  : DWORD;
  Key        : HKey;
  ECode      : LongInt;
  TS,
  HldKey     : String;
  ASL        : TStringList;


     procedure ClearKey(StartKey : HKey);
     var
       SL   : TStringList;
       NK   : HKey;
       NSK,
       NV   : DWORD;
       J    : LongInt;
       TS,
       HK   : String;
       PSK  : array[0..255] of char;
     begin
       ECode := RegQueryInfoKey(StartKey, nil, nil, nil, @NSK,
                  nil, nil, @NV, nil, nil, nil, nil);
       if (NV > 0) then begin
         SL := TStringList.Create;
         try
           GetValues(SL);
           for J := 0 to SL.Count-1 do begin
             TS := SL.Names[J];
             if (AnsiCompareText('Default', TS) <> 0) then
               DeleteValue(TS);
           end;
         finally
           SL.Free;
         end;
       end;

       if NSK > 0 then begin
         SL := TStringList.Create;
         try
           GetSubKeys(SL);
           for J := 0 to SL.Count-1 do begin
             HK := GetCurSubKey;
             SetCurSubKey(HK + '\' + SL[J]);
             NK := OpenRegKey;
             ClearKey(NK);
             RegCloseKey(NK);
             SetCurSubKey(HK);
             StrPCopy(PSK, SL[J]);
             RegDeleteKey(StartKey, PSK);
           end;
         finally
           SL.Free;
         end;
       end;
     end;

begin
  riMode := riSet;
  {$IFDEF ThreadSafe}
  EnterCS;
  try
  {$ENDIF}
    PSKey := StrAlloc(Length(KeyName)); // GetMem(PSKey,Length(KeyName)+1);
    try
      StrPCopy(PSKey,KeyName);
      if (riType = riIniType) then begin
        ASL := TStringList.Create;
        try
          {check for values in section}
          HldKey := GetCurSubkey;
          SetCurSubKey(KeyName);
          GetSubKeys(ASL);
          SetCurSubKey(HldKey);
          NumSubKeys := ASL.Count;

          {remove section KeyName from INI file}
          if (NumSubKeys > 0) AND (NOT DeleteSubKeys) then
            RaiseRegIniErrorFmt(stscKeyHasSubKeys,[NumSubKeys]);
          if (NOT WritePrivateProfileString(PSKey,nil,nil,riRootName)) then
            RaiseRegIniError(stscIniDeleteFail);
        finally
          ASL.Free;
        end;
      end else begin
        HldKey := GetCurSubkey;
        TS := HldKey + '\' + KeyName;
        if TS[1] = '\' then
          Delete(TS, 1, 1);
        SetCurSubKey(TS);
        Key := OpenRegKey;
        try
          {check for subkeys under key to be deleted}
          ECode := RegQueryInfoKey(Key, nil, nil, nil, @NumSubKeys,
                     nil, nil, @NumValues, nil, nil, nil, nil);

          if (ECode <> ERROR_SUCCESS) then
            RaiseRegIniErrorFmt(stscQueryKeyFail,[ECode]);

          if (NumSubKeys > 0) OR (NumValues > 0) then begin
            if (NOT DeleteSubKeys) then
              RaiseRegIniErrorFmt(stscKeyHasSubKeys,[NumSubKeys])
            else
              if (riWinVer = riWinNT) then
                ClearKey(Key);
          end;
        finally
          RegCloseKey(Key);
          SetCurSubKey(HldKey);
        end;

        Key := OpenRegKey;
        try
          ECode := RegDeleteKey(Key, PSKey);
          if (ECode <> ERROR_SUCCESS) then
            RaiseRegIniErrorFmt(stscDeleteKeyFail,[ECode]);
        finally
          if (riRemoteKey = 0) then
            RegCloseKey(Key);
        end;
      end;
    finally
      StrDispose(PSKey); // FreeMem(PSKey,Length(KeyName)+1);
    end;
{$IFDEF ThreadSafe}
  finally
    LeaveCS;
  end;
{$ENDIF}
end;

{==========================================================================}

procedure TStRegIni.DeleteValue(const ValueName : string);
  {-delete value from Ini file section or registry subkey}
var
  PVName : PChar;
  ECode  : LongInt;
  Key    : HKey;
begin
  riMode := riSet;
{$IFDEF ThreadSafe}
  EnterCS;
  try
{$ENDIF}
    PVName := StrAlloc(Length(valueName)); // GetMem(PVName,Length(valueName)+1);
    try
      StrPCopy(PVName,valueName);
      if (riType = riIniType) then begin
        if (NOT WritePrivateProfileString(riCurSubKey,PVName,nil,riRootName)) then
          RaiseRegIniError(stscIniDelValueFail);
      end else begin
        Key := OpenRegKey;
        try
          ECode := RegDeleteValue(Key,PVName);
          if (ECode <> ERROR_SUCCESS) then
            RaiseRegIniErrorFmt(stscRegDelValueFail,[ECode]);
        finally
          if (riRemoteKey = 0) then
            CloseRegKey(Key);
        end;
      end;
    finally
      StrDispose(PVName); // FreeMem(PVName,Length(valueName)+1);
    end;
{$IFDEF ThreadSafe}
  finally
    LeaveCS;
  end;
{$ENDIF}
end;

{==========================================================================}

procedure TStRegIni.QueryKey(var KeyInfo : TQueryKeyInfo);
  {-get informatino about Ini file seciton or registry subkey}
const
  BufSize = 2048;
var
  PVName,
  PCName       : PChar;

  P,
  step         : integer;

  CNSize       : DWORD;
  Key          : HKey;
  ECode        : LongInt;
  SL           : TStringList;

begin
  riMode := riGet;
{$IFDEF ThreadSafe}
  EnterCS;
  try
{$ENDIF}
    if (riType = riIniType) then begin
    {data for the specified section in the INI file}
      SL := TStringList.Create;
      try
        FillChar(KeyInfo,sizeof(KeyInfo),#0);
        {get value names/values}
        GetValues(SL);
        with KeyInfo do begin
          QIMaxVNLen   := 0;
          QIMaxDataLen := 0;
          QINumValues := SL.Count;
          if (SL.Count > 0) then begin
            for step := 0 to SL.Count-1 do begin
              {find maximum length of value names and values}
              P := pos('=',SL[step])-1;
              if (P > LongInt(QIMaxVNLen)) then
                QIMaxVNLen := P;

              P := Length(SL[step]) - P;
              if (P > LongInt(QIMaxDataLen)) then
                QIMaxDataLen := P;
            end;
          end;
        end;
      finally
        SL.Free;
      end;
    end else begin
      PVName := nil;
      PCName := nil;
      try
        PVName := StrAlloc(BufSize); // GetMem(PVName,BufSize);
        PCName := StrAlloc(BufSize); //GetMem(PCName,BufSize);

        Key := OpenRegKey;
        try
          PCName[0] := #0;
          CNSize := BufSize;
          with KeyInfo do begin
            ECode := RegQueryInfoKey(Key,PCName,@CNSize,
                       nil,@QINumSubKeys,@QIMaxSKNLen,
                       @QIMaxCNLen, @QINumValues,
                       @QIMaxVNLen, @QIMaxDataLen,
                       @QISDescLen, @QIFileTime);
            if (ECode <> ERROR_SUCCESS) then
              RaiseRegIniErrorFmt(stscQueryKeyFail,[ECode]);
            QIKey := Key;
            QIClassName := StrPas(PCName);
          end;
        finally
          if (riRemoteKey = 0) then
            CloseRegKey(Key);
        end;
      finally
        if (PVName <> nil) then
          StrDispose(PVName); // FreeMem(PVName,BufSize);
        if (PCName <> nil) then
          StrDispose(PCName); // FreeMem(PCName,BufSize);
      end;
    end;
{$IFDEF ThreadSafe}
  finally
    LeaveCS;
  end;
{$ENDIF}
end;

{==========================================================================}

function TStRegIni.KeyExists(KeyName : string) : Boolean;
  {-checks if exists in INI file/Registry}
var
  KN : PChar;
  PV : array[0..9] of char;
  HK : HKey;
begin
  riMode := riGet;
{$IFDEF ThreadSafe}
  EnterCS;
  try
{$ENDIF}
    KN := StrAlloc(Length(KeyName)); // GetMem(KN, Length(KeyName)+1);
    try
      StrPCopy(KN, KeyName);
      if (riType = riIniType) then begin
        GetPrivateProfileString(KN, nil, '$KDNE1234', PV, 10, riRootName);
        Result := StrIComp(PV, '$KDNE1234') <> 0;
      end else begin
         Result := RegOpenKeyEx(riPrimaryKey,KN,0,KEY_READ,HK) = ERROR_SUCCESS;
         if Result then
           RegCloseKey(HK);
      end;
    finally
      StrDispose(KN); // FreeMem(KN, Length(KeyName)+1);
    end;
{$IFDEF ThreadSafe}
  finally
    LeaveCS;
  end;
{$ENDIF}
end;

{==========================================================================}

function TStRegIni.IsKeyEmpty(Primary, SubKey : string) : Boolean;
var
  FindPos    : Integer;
  Key        : HKey;
  NumSubKeys,
  NumValues  : DWORD;
  ECode      : LongInt;
  HPrime,
  HSubKy     : String;
  ASL        : TStringList;

begin
  riMode := riGet;
{$IFDEF ThreadSafe}
  EnterCS;
  try
{$ENDIF}
    HPrime := GetPrimary;
    HSubKy := CurSubKey;

    SetPrimary(Primary);
    CurSubKey := SubKey;
    Result := True;

    if (riType = riIniType) then begin
        {check for values in section}
      ASL := TStringList.Create;
      try
        ParseIniFile(ASL);
        if not (ASL.Find( '[' + SubKey + ']', FindPos)) then
          Result := False;
      finally
        ASL.Free;
      end;
    end else begin
      try
        Key := OpenRegKey;
        try
          ECode := RegQueryInfoKey(Key, nil, nil, nil, @NumSubKeys,
                     nil, nil, @NumValues, nil, nil, nil, nil);
          if (ECode <> ERROR_SUCCESS) or
             (NumSubKeys > 0) or (NumValues > 0) then
            Result := False;
        except
          Result := False;
        end;
        RegCloseKey(Key);
      finally
        SetPrimary(HPrime);
        SetCurSubKey(HSubKy);
      end;
    end;
{$IFDEF ThreadSafe}
  finally
    LeaveCS;
  end;
{$ENDIF}
end;

{==========================================================================}

procedure TStRegIni.SaveKey(const SubKey : string; FileName : string);
  {-save contents of registry key to a file}
var
  SKey    : string;
  I,
  DotPos  : Cardinal;
  TSL     : TStringList;
  F       : TextFile;
begin
  riMode := riSet;
{$IFDEF ThreadSafe}
  EnterCS;
  try
{$ENDIF}
    if (SubKey <> FCurSubKey) then begin
      SKey := FCurSubKey;
      SetCurSubKey(SubKey);
    end;

    if (riType = riIniType) then begin
      if (FileExists(FileName)) then
        RaiseRegIniError(stscOutputFileExists);
      TSL := TStringList.Create;
      try
        {get valuenames and values from specified section}
        GetValues(TSL);
        if (TSL.Count < 1) then
          RaiseRegIniError(stscKeyIsEmptyNotExists);
        AssignFile(F,FileName);
        ReWrite(F);
        try
          writeln(F,'[' + SubKey + ']');
          for I := 0 to TSL.Count-1 do
            writeln(F,TSL[I]);
        finally
          CloseFile(F);
        end;
      finally
        TSL.Free;
      end;
    end else begin
      if (FileExists(FileName)) then
        RaiseRegIniError(stscOutputFileExists);
      if (HasExtensionL(FileName,DotPos)) then
        RaiseRegIniError(stscFileHasExtension);
(* TODO: this was only executed if $H+   why?
      GetMem(PFName,Length(FileName)+1);
      try
        StrPCopy(PFName,FileName);
        Key := OpenRegKey;
        try
          if (riWinVer = riWinNT) then begin
            OpenProcessToken(GetCurrentProcess(),
                TOKEN_ADJUST_PRIVILEGES OR TOKEN_QUERY, hToken);
            LookupPrivilegeValue(nil,'SeBackupPrivilege',luid);
            tp.PrivilegeCount     := 1;
            tp.Privileges[0].Luid := luid;
            tp.Privileges[0].Attributes := SE_PRIVILEGE_ENABLED;

            AdjustTokenPrivileges(hToken, FALSE, tp,
                                  sizeOf(TTokenPrivileges),ptp,retval);
          end;

          ECode := RegSaveKey(Key,PFName,@FriSecAttr);

          if (riWinVer = riWinNT) then
            AdjustTokenPrivileges(hToken,TRUE,tp,
                                  sizeOf(TTokenPrivileges),ptp,retval);

          if (ECode <> ERROR_SUCCESS) then
            RaiseRegIniErrorFmt(stscSaveKeyFail,[ECode]);
        finally
          if (riRemoteKey = 0) then
            CloseRegKey(Key);
        end;
      finally
        FreeMem(PFName,Length(FileName)+1);
      end;
*)
    end;

    if (SKey <> '') then
      SetCurSubKey(SKey);
{$IFDEF ThreadSafe}
  finally
    LeaveCS;
  end;
{$ENDIF}
end;

{==========================================================================}

procedure TStRegIni.LoadKey(const SubKey, FileName : string);
  {-load a registry key from a file created with SaveKey}
const
  BufSize = 2048;
var
  I,
  DotPos  : Cardinal;

  F       : TextFile;
  TSL     : TStringList;
  S,
  SKey    : string;
  ECode   : LongInt;
  P       : LongInt;

  hToken    : THandle;
  ptp,
  tp        : TTokenPrivileges;
  luid      : TLargeInteger;
  retval    : DWORD;

begin
{$IFDEF ThreadSafe}
  EnterCS;
{$ENDIF}
  riMode := riSet;
  try
    if (riType = riIniType) then begin
      if (NOT FileExists(FileName)) then
        RaiseRegIniError(stscCantFindInputFile);

      {read contents of file into a string list}
      TSL := TStringList.Create;
      try
        AssignFile(F,FileName);
        try
          ReSet(F);
          while NOT EOF(F) do begin
            Readln(F,S);
            TSL.Add(S);
          end;
        finally
          CloseFile(F);
        end;

        if (TSL.Count < 1) then
          RaiseRegIniError(stscKeyIsEmptyNotExists);

        {if section exists - delete it and all values}
        DeleteKey(SubKey,True);

        {write contents of string list to ini file}
        for I := 1 to TSL.Count-1 do begin
          S := TSL[I];
          P := pos('=',S);
          Delete(S,P,Length(S)-P+1);
          WritePrivateProfileString(PChar(SubKey),PChar(S), PChar(TSL.Values[S]),riRootName);
        end;
      finally
        TSL.Free;
      end;
    end else begin
      if (NOT FileExists(FileName)) then
        RaiseRegIniError(stscCantFindInputFile);
      if (HasExtensionL(FileName,DotPos)) then
        RaiseRegIniError(stscFileHasExtension);

      {save current subkey if saving another}
      if (SubKey <> FCurSubKey) then begin
        SKey := FCurSubKey;
        SetCurSubKey(SubKey);
      end;

      {get security token for NT}
      if (riWinVer = riWinNT) then begin
        OpenProcessToken(GetCurrentProcess(),
                         TOKEN_ADJUST_PRIVILEGES OR TOKEN_QUERY, hToken);
        LookupPrivilegeValue(nil,'SeRestorePrivilege',luid);
        tp.PrivilegeCount     := 1;
        tp.Privileges[0].Luid := luid;
        tp.Privileges[0].Attributes := SE_PRIVILEGE_ENABLED;

        AdjustTokenPrivileges(hToken, FALSE, tp,
                                    sizeOf(TTokenPrivileges),ptp,retval);
      end;

      {can load only at top of registry}
      if (riPrimaryKey = HKEY_LOCAL_MACHINE) OR
         (riPrimaryKey = HKEY_USERS) then begin
        ECode := RegLoadKey(riPrimaryKey,PChar(SubKey),PChar(FileName));
        if (riWinVer = riWinNT) then
           AdjustTokenPrivileges(hToken,TRUE,tp,
                                 sizeOf(TTokenPrivileges),ptp,retval);
        if (ECode <> ERROR_SUCCESS) then
          RaiseRegIniErrorFmt(stscLoadKeyFail,[ECode]);
      end else begin
        if (riRemoteKey <> 0) then begin
          ECode := RegLoadKey(riRemoteKey,PChar(SubKey),PChar(FileName));
          if (riWinVer = riWinNT) then
             AdjustTokenPrivileges(hToken,TRUE,tp,
                                   sizeOf(TTokenPrivileges),ptp,retval);
          if (ECode <> ERROR_SUCCESS) then
            RaiseRegIniErrorFmt(stscLoadKeyFail,[ECode]);
        end else
          RaiseRegIniError(stscInvalidPKey);
      end;

      {restore current subkey if necessary}
      if (SKey <> '') then
        SetCurSubKey(SKey);
    end;
  finally
{$IFDEF ThreadSafe}
    LeaveCS;
{$ENDIF}
  end;
end;

{==========================================================================}

procedure TStRegIni.UnLoadKey(const SubKey : string);
  {-remove a section from Ini file or subkey from registry}
  {Registry only: SubKey must have been loaded with LoadKey}
var
  ECode      : LongInt;
  HoldKey    : HKey;

  hToken    : THandle;
  ptp,
  tp        : TTokenPrivileges;
  luid      : TLargeInteger;
  retval    : DWORD;

begin
  riMode := riSet;
{$IFDEF ThreadSafe}
  EnterCS;
  try
{$ENDIF}
    if (riType = riIniType) then
      DeleteKey(SubKey,TRUE)
    else
    begin
      HoldKey := 0;

      {store primary key if working on remote computer}
      if (riRemoteKey <> 0) then begin
        HoldKey := riPrimaryKey;
        riPrimaryKey := riRemoteKey;
      end;
      try
        if (riWinVer = riWinNT) then begin
          OpenProcessToken(GetCurrentProcess(),
                           TOKEN_ADJUST_PRIVILEGES OR TOKEN_QUERY, hToken);
          LookupPrivilegeValue(nil,'SeRestorePrivilege',luid);
          tp.PrivilegeCount     := 1;
          tp.Privileges[0].Luid := luid;
          tp.Privileges[0].Attributes := SE_PRIVILEGE_ENABLED;

          AdjustTokenPrivileges(hToken, FALSE, tp,
                                sizeOf(TTokenPrivileges),ptp,retval);
        end;

        ECode := RegUnLoadKey(riPrimaryKey,PChar(SubKey));

        if (riWinVer = riWinNT) then
           AdjustTokenPrivileges(hToken,TRUE,tp,
                                 sizeOf(TTokenPrivileges),ptp,retval);

        if (ECode <> ERROR_SUCCESS) then
          RaiseRegIniErrorFmt(stscUnloadKeyFail,[ECode]);
      finally
        {restore primary key if function used on remote computer}
        if (riRemoteKey <> 0) then
          riPrimaryKey := HoldKey;
      end;
    end;
{$IFDEF ThreadSafe}
  finally
    LeaveCS;
  end;
{$ENDIF}
end;

{==========================================================================}

procedure TStRegIni.RestoreKey(const SubKey, KeyFile : string; Options : DWORD);
  {-restore a section of Ini file or subkey of registry}
  {Registry only: key being loaded must have been stored using SaveKey}
var
  ECode   : LongInt;
  Key     : HKey;
  hToken    : THandle;
  ptp,
  tp        : TTokenPrivileges;
  luid      : TLargeInteger;
  retval    : DWORD;

begin
  riMode := riSet;
{$IFDEF ThreadSafe}
  EnterCS;
  try
{$ENDIF}
    if (riType = riIniType) then
      LoadKey(SubKey, KeyFile)
    else begin
      if (riWinVer <> riWinNT) then
        RaiseRegIniError(stscNotWinNTPlatform);

      Key := OpenRegKey;
      try
        if (Options = REG_WHOLE_HIVE_VOLATILE) AND
           (Key <> HKEY_USERS) AND
           (Key <> HKEY_LOCAL_MACHINE) then
          RaiseRegIniError(stscBadOptionsKeyCombo);

        {get process token for WinNT}
        if (riWinVer = riWinNT) then begin
          OpenProcessToken(GetCurrentProcess(),
                           TOKEN_ADJUST_PRIVILEGES OR TOKEN_QUERY, hToken);
          LookupPrivilegeValue(nil,'SeRestorePrivilege',luid);
          tp.PrivilegeCount     := 1;
          tp.Privileges[0].Luid := luid;
          tp.Privileges[0].Attributes := SE_PRIVILEGE_ENABLED;

          AdjustTokenPrivileges(hToken, FALSE, tp,
                                sizeOf(TTokenPrivileges),ptp,retval);
        end;

        ECode := RegRestoreKey(Key,PChar(KeyFile),Options);

        if (riWinVer = riWinNT) then
           AdjustTokenPrivileges(hToken,TRUE,tp,
                                 sizeOf(TTokenPrivileges),ptp,retval);

        if (ECode <> ERROR_SUCCESS) then
          RaiseRegIniErrorFmt(stscRestoreKeyFail,[ECode]);
      finally
        CloseRegKey(Key);
      end;
    end;
{$IFDEF ThreadSafe}
  finally
    LeaveCS;
  end;
{$ENDIF}
end;

{==========================================================================}

procedure TStRegIni.ReplaceKey(const SubKey, InputFile, SaveFile : string);
  {-replace existing section or registry subkey}
  {Registry only: key being loaded must have been stored with SaveKey}
  {               "new" key does not take affect unti re-boot}
var
  DotPos    : Cardinal;
  ECode     : LongInt;
  hToken    : THandle;
  ptp,
  tp        : TTokenPrivileges;
  luid      : TLargeInteger;
  retval    : DWORD;

begin
  riMode := riSet;
{$IFDEF ThreadSafe}
  EnterCS;
  try
{$ENDIF}
    if (riType = riIniType) then begin
      if (FileExists(SaveFile)) then
        RaiseRegIniError(stscOutputFileExists);
      SaveKey(SubKey,SaveFile);
      LoadKey(SubKey,InputFile);
    end else begin
      if (FileExists(SaveFile)) then
        RaiseRegIniError(stscOutputFileExists);
      if (HasExtensionL(SaveFile,DotPos)) OR
         (HasExtensionL(InputFile,DotPos)) then
        RaiseRegIniError(stscFileHasExtension);

      if (riWinVer = riWinNT) then begin
        OpenProcessToken(GetCurrentProcess(),
                         TOKEN_ADJUST_PRIVILEGES OR TOKEN_QUERY,
                         {$IFNDEF VERSION3}
                         @hToken);
                         {$ELSE}
                         hToken);
                         {$ENDIF}
        LookupPrivilegeValue(nil,'SeRestorePrivilege',luid);
        tp.PrivilegeCount     := 1;
        tp.Privileges[0].Luid := luid;
        tp.Privileges[0].Attributes := SE_PRIVILEGE_ENABLED;

        AdjustTokenPrivileges(hToken, FALSE, tp,
                              sizeOf(TTokenPrivileges),ptp,retval);
      end;

      if (riRemoteKey <> 0) then begin
        ECode := RegReplaceKey(riRemoteKey,PChar(SubKey),PChar(InputFile),PChar(SaveFile));

        if (riWinVer = riWinNT) then
           AdjustTokenPrivileges(hToken,TRUE,tp,
                                 sizeOf(TTokenPrivileges),ptp,retval);
        if (ECode <> ERROR_SUCCESS) then
          RaiseRegIniErrorFmt(stscReplaceKeyFail,[ECode]);
      end else begin
        ECode := RegReplaceKey(riPrimaryKey,PChar(SubKey),PChar(InputFile),PChar(SaveFile));
        if (riWinVer = riWinNT) then
           AdjustTokenPrivileges(hToken,TRUE,tp,
                                 sizeOf(TTokenPrivileges),ptp,retval);
        if (ECode <> ERROR_SUCCESS) then
          RaiseRegIniErrorFmt(stscReplaceKeyFail,[ECode]);
      end;
    end;
{$IFDEF ThreadSafe}
  finally
    LeaveCS;
  end;
{$ENDIF}
end;

{==========================================================================}

procedure TStRegIni.RegOpenRemoteKey(CompName : string);
  {-open a registry subkey on a remote computer}
var
  ECode    : LongInt;
begin
  riMode := riSet;
{$IFDEF ThreadSafe}
  EnterCS;
  try
{$ENDIF}
    if (riType = riIniType) then
      RaiseRegIniError(stscNoIniFileSupport)
    else begin
      if (riRemoteKey <> 0) then
        RaiseRegIniError(stscRemoteKeyIsOpen);

      if (riPrimaryKey <> HKEY_LOCAL_MACHINE) AND
         (riPrimaryKey <> HKEY_USERS) then
        RaiseRegIniError(stscInvalidPKey);

      ECode := Windows.RegConnectRegistry(PChar(CompName),riPrimaryKey,riRemoteKey);
      if (ECode <> ERROR_SUCCESS) then
        RaiseRegIniErrorFmt(stscConnectRemoteKeyFail,[ECode]);

      {store current primary key while remote key is open}
      if (riPrimaryKey <> riRemoteKey) then
        riHoldPrimary := riPrimaryKey;
      riPrimaryKey := riRemoteKey;
    end;
{$IFDEF ThreadSafe}
  finally
    LeaveCS;
  end;
{$ENDIF}
end;

{==========================================================================}

procedure TStRegIni.RegCloseRemoteKey;
  {-close a registry key on a remote computer}
var
  ECode   : LongInt;
begin
  riMode := riSet;
{$IFDEF ThreadSafe}
  EnterCS;
  try
{$ENDIF}
    if (riType = riIniType) then
      RaiseRegIniError(stscNoIniFileSupport)
    else begin
      if (riRemoteKey <> 0) then begin
        ECode := RegCloseKey(riRemoteKey);
        if (ECode <> ERROR_SUCCESS) then
          RaiseRegIniErrorFmt(stscCloseRemoteKeyFail,[ECode]);
        riRemoteKey := 0;

        {reset primary key if opening remote key changed it}
        if riHoldPrimary <> 0 then begin
          riPrimaryKey := riHoldPrimary;
          riHoldPrimary := 0;
        end;
      end;
    end;
{$IFDEF ThreadSafe}
  finally
    LeaveCS;
  end;
{$ENDIF}
end;

{==========================================================================}

procedure TStRegIni.RegGetKeySecurity(const SubKey : string; var SD : TSecurityDescriptor);
  {-get security attributes for key (WinNT only) }
  //SZ: todo Subkey never used
var
  Key       : HKey;
  ECode     : LongInt;
  SDSize    : DWORD;
  SI        : SECURITY_INFORMATION;
  QI        : TQueryKeyInfo;

  hToken    : THandle;
  ptp,
  tp        : TTokenPrivileges;
  luid      : TLargeInteger;
  retval    : DWORD;

begin
  riMode := riSet;
{$IFDEF ThreadSafe}
  EnterCS;
  try
{$ENDIF}
    if (riType = riIniType) then
      RaiseRegIniError(stscNoIniFileSupport)
    else begin
      if (riWinVer <> riWinNT) then
        RaiseRegIniError(stscNotWinNTPlatform);

      QueryKey(QI);

      Key := OpenRegKey;
      try
        SDSize := QI.QISDescLen;
        SI := OWNER_SECURITY_INFORMATION or
              GROUP_SECURITY_INFORMATION or
              DACL_SECURITY_INFORMATION  or
              SACL_SECURITY_INFORMATION;

        OpenProcessToken(GetCurrentProcess(),
                         TOKEN_ADJUST_PRIVILEGES OR TOKEN_QUERY, hToken);
        LookupPrivilegeValue(nil,'SeSecurityPrivilege',luid);
        tp.PrivilegeCount     := 1;
        tp.Privileges[0].Luid := luid;
        tp.Privileges[0].Attributes := SE_PRIVILEGE_ENABLED;

        AdjustTokenPrivileges(hToken, FALSE, tp,
                              sizeOf(TTokenPrivileges),ptp,retval);
        ECode := Windows.RegGetKeySecurity(Key,SI,@SD,SDSize);

        AdjustTokenPrivileges(hToken,TRUE,tp,
                              sizeOf(TTokenPrivileges),ptp,retval);

        if (ECode <> ERROR_SUCCESS) then
          RaiseRegIniErrorFmt(stscGetSecurityFail,[ECode]);
      finally
        CloseRegKey(Key);
      end;
    end;
{$IFDEF ThreadSafe}
  finally
    LeaveCS;
  end;
{$ENDIF}
end;

{==========================================================================}

procedure TStRegIni.RegSetKeySecurity(const SubKey : string; SD : TSecurityDescriptor);
  {-set security attributes for a registry key (WinNT only) }
var
  Key       : HKey;
  ECode     : LongInt;
  SI        : SECURITY_INFORMATION;

  hToken    : THandle;
  ptp,
  tp        : TTokenPrivileges;
  luid      : TLargeInteger;
  retval    : DWORD;

begin
  riMode := riSet;
{$IFDEF ThreadSafe}
  EnterCS;
  try
{$ENDIF}
    if (riType = riIniType) then
      RaiseRegIniError(stscNoIniFileSupport)
    else begin
      if (riWinVer <> riWinNT) then
        RaiseRegIniError(stscNotWinNTPlatform);

      Key := OpenRegKey;
      try
        SI := OWNER_SECURITY_INFORMATION or
              GROUP_SECURITY_INFORMATION or
              DACL_SECURITY_INFORMATION  or
              SACL_SECURITY_INFORMATION;

        OpenProcessToken(GetCurrentProcess(),
                         TOKEN_ADJUST_PRIVILEGES OR TOKEN_QUERY, hToken);
        LookupPrivilegeValue(nil,'SeSecurityName',luid);
        tp.PrivilegeCount     := 1;
        tp.Privileges[0].Luid := luid;
        tp.Privileges[0].Attributes := SE_PRIVILEGE_ENABLED;

        AdjustTokenPrivileges(hToken, FALSE, tp,
                              sizeOf(TTokenPrivileges),ptp,retval);

        ECode := Windows.RegSetKeySecurity(Key,SI,@SD);

        AdjustTokenPrivileges(hToken,TRUE,tp,
                              sizeOf(TTokenPrivileges),ptp,retval);

        if (ECode <> ERROR_SUCCESS) then
          RaiseRegIniErrorFmt(stscSetSecurityFail,[ECode]);
      finally
        if (riRemoteKey = 0) then
          CloseRegKey(Key);
      end;
    end;
{$IFDEF ThreadSafe}
  finally
    LeaveCS;
  end;
{$ENDIF}
end;

end.
