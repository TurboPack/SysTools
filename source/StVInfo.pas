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
{* SysTools: StVInfo.pas 4.04                            *}
{*********************************************************}
{* SysTools: Version Information Extraction Component    *}
{*********************************************************}

{$I StDefine.inc}

{$I+} {I/O Checking On}

unit StVInfo;

interface

uses
  Windows, SysUtils, Classes,
  StBase, StConst;

{!!.02 - added }
const
  STVERMAJOR   = 0;
  STVERMINOR   = 1;
  STVERBUILD   = 2;
  STVERRELEASE = 3;
{!!.02 - added end }

type
  PVerTranslation = ^TVerTranslation;
  TVerTranslation = record
    Language : Word;
    CharSet  : Word;
  end;

  TStCustomVersionInfo = class(TStComponent)
  protected {private}
{$Z+}
    FComments            : string;
    FCompanyName         : string;
    FFileDescription     : string;
    FFileDate            : TDateTime;
    FFileFlags           : DWORD;                                      {!!.02}
    FFileFlagsMask       : DWORD;                                      {!!.02}
    FFileMajorVersion    : DWORD;                                      {!!.02}
    FFileMinorVersion    : DWORD;                                      {!!.02}
    FFileName            : string;
    FFileOS              : DWORD;                                      {!!.02}
    FFileType            : DWORD;                                      {!!.02}
    FFileSubtype         : DWORD;                                      {!!.02}
    FFileVersion         : string;
    FFileVersionFloat    : Double;
    FInternalName        : string;
    FLanguageCount       : Integer;
    FLanguageName        : string;
    FLegalCopyright      : string;
    FLegalTrademark      : string;
    FOriginalFilename    : string;
    FProductMajorVersion : DWORD;                                      {!!.02}
    FProductMinorVersion : DWORD;                                      {!!.02}
    FProductName         : string;
    FProductVersion      : string;
    FProductVersionFloat : Double;
    FTranslationValue    : Integer;
    VInfoLoaded          : Boolean;

    function GetComments : string;
    function GetCompanyName : string;
    function GetFileDate: TDateTime;
    function GetFileDescription : string;
    function GetFileFlags: DWORD;                                      {!!.02}
    function GetFileFlagsMask: DWORD;                                  {!!.02}
    function GetFileMajorVersion: DWORD;                               {!!.02}
    function GetFileMinorVersion: DWORD;                               {!!.02}
    function GetFileOS: DWORD;                                         {!!.02}
    function GetFileSubtype: DWORD;                                    {!!.02}
    function GetFileType: DWORD;                                       {!!.02}
    function GetFileVersion : string;
    function GetFileVersionFloat : Double;
    function GetInternalName : string;
    function GetLanguageCount: Integer;
    function GetLanguageName: string;
    function GetLegalCopyright : string;
    function GetLegalTrademark : string;
    function GetOriginalFilename : string;
    function GetProductMajorVersion: DWORD;                            {!!.02}
    function GetProductMinorVersion: DWORD;                            {!!.02}
    function GetProductName : string;
    function GetProductVersion : string;
    function GetProductVersionFloat : Double;
    function GetTranslationValue: Integer;
    procedure SetFileName(const Value : string);

    function LoadVersionInfo(const Key : string) : string;
    procedure Loaded; override;

{!!.02 - added }
    function GetFileVerSubPart(Index : Integer) : Word;
    function GetProdVerSubPart(Index : Integer) : Word;
{!!.02 - added end }

  protected

{$Z-}
    {properties}
    property Comments : string
      read GetComments;

    property CompanyName : string
      read GetCompanyName;

    property FileDate : TDateTime
      read GetFileDate;

    property FileDescription : string
      read GetFileDescription;

    property FileFlags : DWORD                                         {!!.02}
      read GetFileFlags;

    property FileFlagsMask : DWORD                                     {!!.02}
      read GetFileFlagsMask;

    property FileMajorVersion : DWORD                                  {!!.02}
      read GetFileMajorVersion;

    property FileMinorVersion : DWORD                                  {!!.02}
      read GetFileMinorVersion;

    property FileName : string
      read FFileName write SetFileName;

    property FileOS : DWORD                                            {!!.02}
      read GetFileOS;

    property FileType : DWORD                                          {!!.02}
      read GetFileType;

    property FileSubtype : DWORD                                       {!!.02}
      read GetFileSubtype;

    property FileVersion : string
      read GetFileVersion;

    property FileVersionFloat : Double
      read GetFileVersionFloat;

    property InternalName : string
      read GetInternalName;

    property LanguageCount : Integer
      read GetLanguageCount;

    property LanguageName : string
      read GetLanguageName;

    property LegalCopyright : string
      read GetLegalCopyright;

    property LegalTrademark : string
      read GetLegalTrademark;

    property OriginalFilename : string
      read GetOriginalFilename;

    property ProductName : string
      read GetProductName;

    property ProductMajorVersion : DWORD                               {!!.02}
      read GetProductMajorVersion;

    property ProductMinorVersion : DWORD                               {!!.02}
      read GetProductMinorVersion;

    property ProductVersion : string
      read GetProductVersion;

    property ProductVersionFloat : Double
      read GetProductVersionFloat;

    property TranslationValue : Integer
      read GetTranslationValue;

{!!.02 - added }
    property FileVerMajor : Word
      index STVERMAJOR read GetFileVerSubPart;
    property FileVerMinor : Word
      index STVERMINOR read GetFileVerSubPart;
    property FileVerBuild : Word
      index STVERBUILD read GetFileVerSubPart;
    property FileVerRelease : Word
      index STVERRELEASE read GetFileVerSubPart;
    property ProductVerMajor : Word
      index STVERMAJOR read GetProdVerSubPart;
    property ProductVerMinor : Word
      index STVERMINOR read GetProdVerSubPart;
    property ProductVerBuild : Word
      index STVERBUILD read GetProdVerSubPart;
    property ProductVerRelease : Word
      index STVERRELEASE read GetProdVerSubPart;
{!!.02 - added end }


  public
    { Public declarations }
{$Z+}
    constructor Create(AOwner : TComponent);
      override;
    destructor Destroy;
      override;
{$Z-}
    function GetKeyValue(const Key : string) : string;

  published
    { Published declarations }
  end;

  TStVersionInfo = class(TStCustomVersionInfo)
  public
    {properties}
    property Comments;
    property CompanyName;
    property FileDescription;
    property FileDate;
    property FileFlags;
    property FileFlagsMask;
    property FileMajorVersion;
    property FileMinorVersion;
    property FileOS;
    property FileType;
    property FileSubtype;
    property FileVersion;
    property FileVersionFloat;
    property InternalName;
    property LanguageCount;
    property LanguageName;
    property LegalCopyright;
    property LegalTrademark;
    property OriginalFilename;
    property ProductMajorVersion;
    property ProductMinorVersion;
    property ProductName;
    property ProductVersion;
    property ProductVersionFloat;
    property TranslationValue;

{!!.02 - added }
    property FileVerMajor;
    property FileVerMinor;
    property FileVerBuild;
    property FileVerRelease;
    property ProductVerMajor;
    property ProductVerMinor;
    property ProductVerBuild;
    property ProductVerRelease;
{!!.02 - added end }


  published
    {properties}
    property FileName;
  end;

implementation

constructor TStCustomVersionInfo.Create(AOwner : TComponent);
begin
  inherited Create(AOwner);
  VInfoLoaded := False;
  SetFileName('');
end;

destructor TStCustomVersionInfo.Destroy;
begin
  inherited Destroy;
end;

function TStCustomVersionInfo.LoadVersionInfo(const Key : string) : string;
var
  Handle : DWORD;
  Res     : Boolean;
  Size    : Integer;
  Error   : Integer;
  Data    : Pointer;
  Buffer  : Pointer;
  ErrCode : Integer;
  Bytes   : Cardinal;
  TempStr : array [0..259] of Char;
  LangBuff: array [0..259] of Char;
  BaseStr : string;
  InfoStr : string;
  Trans   : PVerTranslation;
  TrSize  : Integer;
  FixedInfo : TVSFixedFileInfo;
  FT : TFileTime;                                                      {!!.02}
  ST : TSystemTime;                                                    {!!.02}

  function MakeFloat(S : string) : Double;
  var
    Buff  : array [0..5] of Char;
    I     : Integer;
    Count : Integer;
  begin
    Count := 0;
    FillChar(Buff, SizeOf(Buff), 0);
    Buff[0] := '0';
    { The file version string might be specified like }
    { 4.72.3105.0. Parse it down to just one decimal  }
    { place and create the floating point version #.  }
    for I := 1 to Pred(Length(S)) do begin
      if S[I] = '.' then begin
        { Found the first period. Replace it with the DecimalSeparator }
        { constant so that StrToFloat works properly. }
        S[I] := FormatSettings.DecimalSeparator;
        Inc(Count);
        if (Count = 2) and (I <= Length(Buff)) then begin
          Move(S[1], Buff, (I - 1) * SizeOf(Char));
          Break;
        end;
      end;
    end;
    Result := StrToFloat(Buff);
  end;

begin
  TrSize := 0;
  Size := GetFileVersionInfoSize(StrPCopy(TempStr, FFileName), Handle);
  if Size = 0 then begin
    { GetFileVersionInfoSize might fail because the }
    { file is a 16-bit file or because the file does not     }
    { contain version info. }
    Error := GetLastError;
    if Error = ERROR_RESOURCE_TYPE_NOT_FOUND then
      RaiseStError(EStVersionInfoError, stscNoVerInfo);
    if Error = 0 then
      RaiseStError(EStVersionInfoError, stscVerInfoFail);
  end;

  { Allocate some memory and get version info block. }
  GetMem(Data, Size);
  Res := GetFileVersionInfo(TempStr, Handle, Size, Data);
  Trans  := nil;
  try
    if not Res then
      { Error. Raise an exception. }
      RaiseStError(EStVersionInfoError, stscVerInfoFail);

    { Get the translation value. We need it to get the version info. }
    Res := VerQueryValue(Data, '\VarFileInfo\Translation', Buffer, Bytes);
    if not Res then
      RaiseStError(EStVersionInfoError, stscVerInfoFail);
    TrSize := Bytes;
    GetMem(Trans, TrSize);
    Move(Buffer^, Trans^, TrSize);
    FTranslationValue := Integer(Trans^);
    FLanguageCount := Bytes div SizeOf(TVerTranslation);
    VerLanguageName(Trans^.Language, LangBuff, Length(LangBuff));
    FLanguageName := StrPas(LangBuff);
    VInfoLoaded := True;

    { Build a base string including the translation value. }
    BaseStr := Format('StringFileInfo\%.4x%.4x\', [Trans^.Language, Trans^.CharSet]);

    { User-defined string. Get the string and exit. }
    if Key <> '' then begin
      InfoStr := BaseStr + Key;
      Res := VerQueryValue(Data, StrPCopy(TempStr, InfoStr), Buffer, Bytes);

      if Res then begin
        Result := StrPas(PChar(Buffer));
//        Exit;                                                        {!!.02}
      end else begin
        Result := '';
        RaiseStError(EStVersionInfoError, stscBadVerInfoKey);
      end;
    end                                                                {!!.02}
    else begin                                                         {!!.02}

    { Get the fixed version info. }
    Bytes := SizeOf(FixedInfo);
    FillChar(FixedInfo, Bytes, 0);
    { '\' is used to get the root block. }
    Res := VerQueryValue(Data, '\', Buffer, Bytes);
    if not Res then
      RaiseStError(EStVersionInfoError, stscVerInfoFail);

    Move(Buffer^, FixedInfo, Bytes);
    with FixedInfo do begin
      FFileMajorVersion := dwFileVersionMS;
      FFileMinorVersion := dwFileVersionLS;
      FProductMajorVersion := dwProductVersionMS;
      FProductMinorVersion := dwProductVersionLS;
      FFileFlagsMask := dwFileFlagsMask;
      FFileFlags := dwFileFlags;

{!!.02 - rewritten }
      { Note: Most files don't set the binary date. }
//      FFileDate := MakeLong(dwFileDateMS, dwFileDateLS);
      FT.dwHighDateTime := dwFileDateMS;
      FT.dwLowDateTime  := dwFileDateLS;
      FileTimeToSystemTime(FT, ST);
      FFileDate := SystemTimeToDateTime(ST);
{!!.02 - rewritten end}

      FFileOS := dwFileOS;
      FFileType := dwFileType;
      FFileSubtype := dwFileSubtype;
    end;

    { Comments }
    InfoStr := BaseStr + 'Comments';
    Res := VerQueryValue(Data, StrPCopy(TempStr, InfoStr), Buffer, Bytes);
    if Res and (Bytes <> 0) then
      FComments := StrPas(PChar(Buffer))
    else
      FComments := '';

    { CompanyName }
    InfoStr := BaseStr + 'CompanyName';
    Res := VerQueryValue(Data, StrPCopy(TempStr, InfoStr), Buffer, Bytes);
    if Res and (Bytes <> 0) then
      FCompanyName := StrPas(PChar(Buffer))
    else
      FCompanyName := '';

    { FileDescription }
    InfoStr := BaseStr + 'FileDescription';
    Res := VerQueryValue(Data, StrPCopy(TempStr, InfoStr), Buffer, Bytes);
    if Res and (Bytes <> 0) then
      FFileDescription := StrPas(PChar(Buffer))
    else
      FFileDescription := '';

    { FileVersion }
    InfoStr := BaseStr + 'FileVersion';
    Res := VerQueryValue(Data, StrPCopy(TempStr, InfoStr), Buffer, Bytes);
    if Res and (Bytes <> 0) then begin
      FFileVersion := StrPas(PChar(Buffer));
      { First try to convert the version number to a float as-is. }
      Val(FFileVersion, FFileVersionFloat, ErrCode);
      if ErrCode <> 0 then
        { Failed. Create the float with the local MakeFloat function. }
        try
          FFileVersionFloat := MakeFloat(FFileVersion);
        except
          FFileVersionFloat := 0;
        end;
    end else begin
      FFileVersion := '';
      FFileVersionFloat := 0;
    end;

    { InternalName }
    InfoStr := BaseStr + 'InternalName';
    Res := VerQueryValue(Data, StrPCopy(TempStr, InfoStr), Buffer, Bytes);
    if Res and (Bytes <> 0) then
      FInternalName := StrPas(PChar(Buffer))
    else
      FInternalName := '';

    { LegalCopyright }
    InfoStr := BaseStr + 'LegalCopyright';
    Res := VerQueryValue(Data, StrPCopy(TempStr, InfoStr), Buffer, Bytes);
    if Res and (Bytes <> 0) then
      FLegalCopyright := StrPas(PChar(Buffer))
    else
      FLegalCopyright := '';

    { LegalTrademarks }
    InfoStr := BaseStr + 'LegalTrademarks';
    Res := VerQueryValue(Data, StrPCopy(TempStr, InfoStr), Buffer, Bytes);
    if Res and (Bytes <> 0) then
      FLegalTrademark := StrPas(PChar(Buffer))
    else
      FLegalTrademark := '';

    { OriginalFilename }
    InfoStr := BaseStr + 'OriginalFilename';
    Res := VerQueryValue(Data, StrPCopy(TempStr, InfoStr), Buffer, Bytes);
    if Res and (Bytes <> 0) then
      FOriginalFilename := StrPas(PChar(Buffer))
    else
      FOriginalFilename := '';

    { ProductName }
    InfoStr := BaseStr + 'ProductName';
    Res := VerQueryValue(Data, StrPCopy(TempStr, InfoStr), Buffer, Bytes);
    if Res and (Bytes <> 0) then
      FProductName := StrPas(PChar(Buffer))
    else
      FProductName := '';

    { ProductVersion }
    InfoStr := BaseStr + 'ProductVersion';
    Res := VerQueryValue(Data, StrPCopy(TempStr, InfoStr), Buffer, Bytes);
    if Res and (Bytes <> 0) then begin
      FProductVersion := StrPas(PChar(Buffer));
      { First try to convert the product number to a float as-is. }
      Val(FProductVersion, FProductVersionFloat, ErrCode);
      if ErrCode <> 0 then
        { Failed. Create the float with the local MakeFloat function. }
        try
          FProductVersionFloat := MakeFloat(FProductVersion);
        except
          FProductVersionFloat := 0;
        end;
    end else begin
      FProductVersion := '';
      FProductVersionFloat := 0;
    end;

    end;                                                               {!!.02}

  finally
    FreeMem(Data, Size);
    FreeMem(Trans, TrSize);
  end;
end;

function TStCustomVersionInfo.GetComments : string;
begin
  if not VInfoLoaded then
    LoadVersionInfo('');
  Result := FComments;
end;

function TStCustomVersionInfo.GetCompanyName : string;
begin
  if not VInfoLoaded then
    LoadVersionInfo('');
  Result := FCompanyName;
end;

function TStCustomVersionInfo.GetFileDescription : string;
begin
  if not VInfoLoaded then
    LoadVersionInfo('');
  Result := FFileDescription;
end;

function TStCustomVersionInfo.GetFileVersion : string;
begin
  if not VInfoLoaded then
    LoadVersionInfo('');
  Result := FFileVersion;
end;

function TStCustomVersionInfo.GetInternalName : string;
begin
  if not VInfoLoaded then
    LoadVersionInfo('');
  Result := FInternalName;
end;

function TStCustomVersionInfo.GetLegalCopyright : string;
begin
  if not VInfoLoaded then
    LoadVersionInfo('');
  Result := FLegalCopyright;
end;

function TStCustomVersionInfo.GetLegalTrademark : string;
begin
  if not VInfoLoaded then
    LoadVersionInfo('');
  Result := FLegalTrademark;
end;

function TStCustomVersionInfo.GetOriginalFilename : string;
begin
  if not VInfoLoaded then
    LoadVersionInfo('');
  Result := FOriginalFilename;
end;

function TStCustomVersionInfo.GetProductName : string;
begin
  if not VInfoLoaded then
    LoadVersionInfo('');
  Result := FProductName;
end;

function TStCustomVersionInfo.GetProductVersion : string;
begin
  if not VInfoLoaded then
    LoadVersionInfo('');
  Result := FProductVersion;
end;

function TStCustomVersionInfo.GetProductVersionFloat : Double;
begin
  if not VInfoLoaded then
    LoadVersionInfo('');
  Result := FProductVersionFloat;
end;

function TStCustomVersionInfo.GetFileVersionFloat : Double;
begin
  if not VInfoLoaded then
    LoadVersionInfo('');
  Result := FFileVersionFloat;
end;

procedure TStCustomVersionInfo.SetFileName(const Value : string);
var
  Buff : array [0..255] of Char;
begin
  if (Value <> '') and not (csDesigning in ComponentState) then
    if not FileExists(Value) then
      RaiseStError(EStVersionInfoError, stscFileOpen);
  if FFileName <> Value then
    VInfoLoaded := False;
  FFileName := Value;
  { If FileName is an emtpy string then load the }
  { version info for the current process.        }
  if (FFileName = '') and not (csDesigning in ComponentState) then
    if GetModuleFileName(0, Buff, Length(Buff)) = 0 then
      FFileName := ''
    else
      FFileName := StrPas(Buff);
end;

function TStCustomVersionInfo.GetFileDate: TDateTime;
begin
  if not VInfoLoaded then
    LoadVersionInfo('');
  Result := FFileDate;
end;

function TStCustomVersionInfo.GetFileFlags: DWORD;                     {!!.02}
begin
  if not VInfoLoaded then
    LoadVersionInfo('');
  Result := FFileFlags;
end;

function TStCustomVersionInfo.GetFileFlagsMask: DWORD;                 {!!.02}
begin
  if not VInfoLoaded then
    LoadVersionInfo('');
  Result := FFileFlagsMask;
end;

function TStCustomVersionInfo.GetFileOS: DWORD;                        {!!.02}
begin
  if not VInfoLoaded then
    LoadVersionInfo('');
  Result := FFileOS;
end;

function TStCustomVersionInfo.GetFileSubtype: DWORD;                   {!!.02}
begin
  if not VInfoLoaded then
    LoadVersionInfo('');
  Result := FFileSubtype;
end;

function TStCustomVersionInfo.GetFileType: DWORD;                      {!!.02}
begin
  if not VInfoLoaded then
    LoadVersionInfo('');
  Result := FFileType;
end;

function TStCustomVersionInfo.GetFileMajorVersion: DWORD;              {!!.02}
begin
  if not VInfoLoaded then
    LoadVersionInfo('');
  Result := FFileMajorVersion;
end;

function TStCustomVersionInfo.GetFileMinorVersion: DWORD;              {!!.02}
begin
  if not VInfoLoaded then
    LoadVersionInfo('');
  Result := FFileMinorVersion;
end;

function TStCustomVersionInfo.GetProductMajorVersion: DWORD;           {!!.02}
begin
  if not VInfoLoaded then
    LoadVersionInfo('');
  Result := FProductMajorVersion;
end;

function TStCustomVersionInfo.GetProductMinorVersion: DWORD;           {!!.02}
begin
  if not VInfoLoaded then
    LoadVersionInfo('');
  Result := FProductMinorVersion;
end;

function TStCustomVersionInfo.GetLanguageCount: Integer;
begin
  if not VInfoLoaded then
    LoadVersionInfo('');
  Result := FLanguageCount;
end;

function TStCustomVersionInfo.GetLanguageName: string;
begin
  if not VInfoLoaded then
    LoadVersionInfo('');
  Result := FLanguageName;
end;

function TStCustomVersionInfo.GetTranslationValue: Integer;
begin
  if not VInfoLoaded then
    LoadVersionInfo('');
  Result := FTranslationValue;
end;

function TStCustomVersionInfo.GetKeyValue(const Key: string): string;
begin
  Result := LoadVersionInfo(Key);
end;

procedure TStCustomVersionInfo.Loaded;
begin
  inherited Loaded;
  if FFileName = '' then
    SetFileName('');
end;

{!!.02 - added }
function TStCustomVersionInfo.GetFileVerSubPart(Index: Integer): Word;
begin
  Result := 0;
  if not VInfoLoaded then
    LoadVersionInfo('');
  case Index of
    STVERMAJOR:   Result := HIWORD(FFileMajorVersion);
    STVERMINOR:   Result := LOWORD(FFileMajorVersion);
    STVERBUILD:   Result := HIWORD(FFileMinorVersion);
    STVERRELEASE: Result := LOWORD(FFileMinorVersion);
  end; { case }
end;

function TStCustomVersionInfo.GetProdVerSubPart(Index: Integer): Word;
begin
  Result := 0;
  if not VInfoLoaded then
    LoadVersionInfo('');
  case Index of
    STVERMAJOR:   Result := HIWORD(FProductMajorVersion);
    STVERMINOR:   Result := LOWORD(FProductMajorVersion);
    STVERBUILD:   Result := HIWORD(FProductMinorVersion);
    STVERRELEASE: Result := LOWORD(FProductMinorVersion);
  end; { case }
end;
{!!.02 - added end }

end.
