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
{*                  _STREGINI.PAS 3.00                   *}
{*********************************************************}

{$I STDEFINE.INC}
{$I STCOMDEF.INC}
unit _StRegINI;

interface

uses
  Windows, ComObj, ActiveX, SysTools_TLB, StRegIni, Classes, StdVcl;

type
  IEnumVariant = interface(IUnknown)
    ['{00020404-0000-0000-C000-000000000046}']
    function Next(celt: Longint; out elt; pceltFetched: PLongint): HResult; stdcall;
    function Skip(celt: Longint): HResult; stdcall;
    function Reset: HResult; stdcall;
    function Clone(out Enum: IEnumVariant): HResult; stdcall;
  end;

  TStRegINIQueryKeyInfo = class(TAutoIntfObject, IStRegINIQueryKeyInfo)
   private
     FOwner : StRegIni.TQueryKeyInfo;
   public
    constructor Create(AOwner: StRegIni.TQueryKeyInfo);
   protected
    function Get_QIClassName: WideString; safecall;
    function Get_QIFileTime: TDateTime; safecall;
    function Get_QIKey: Integer; safecall;
    function Get_QIMaxCNLen: Integer; safecall;
    function Get_QIMaxDataLen: Integer; safecall;
    function Get_QIMaxSKNLen: Integer; safecall;
    function Get_QIMaxVNLen: Integer; safecall;
    function Get_QINumSubKeys: Integer; safecall;
    function Get_QINumValues: Integer; safecall;
    function Get_QISDescLen: Integer; safecall;
    { Protected declarations }
  end;

  TStRegINISubKey = class(TAutoIntfObject, IStRegINISubKey)
   private
     FOwner : WideString;
   public
    constructor Create(AOwner: WideString);
   protected
    function Get_Value: WideString; safecall;
    { Protected declarations }
  end;

  TStRegINISubKeys = class(TAutoIntfObject, IStRegINISubKeys, IEnumVariant)
   private
    FOwner   : TStrings;
    FEnumPos : Integer;
   public
    constructor Create(AOwner: TStrings);
    destructor Destroy; override;
   protected
    { IEnumVariant }
    function Next(celt: LongInt; out elt; pceltFetched: PLongInt): HResult; stdcall;
    function Skip(celt: LongInt): HResult; stdcall;
    function Reset: HResult; stdcall;
    function Clone(out Enum: IEnumVariant): HResult; stdcall;

    function Get__NewEnum: IUnknown; safecall;
    function Get_Count: Integer; safecall;
    function Get_Item(Index: Integer): IStRegINISubKey; safecall;
  end;

  TStRegIniValue = class(TAutoIntfObject, IStRegIniValue)
   private
     FOwner : WideString;
   public
    constructor Create(AOwner: WideString);
   protected
    function Get_Value: WideString; safecall;
  end;

  TStRegINIValues = class(TAutoIntfObject, IStRegINIValues, IEnumVariant)
   private
    FOwner   : TStrings;
    FEnumPos : Integer;
   public
    constructor Create(AOwner: TStrings);
    destructor Destroy; override;
   protected
    { IEnumVariant }
    function Next(celt: LongInt; out elt; pceltFetched: PLongInt): HResult; stdcall;
    function Skip(celt: LongInt): HResult; stdcall;
    function Reset: HResult; stdcall;
    function Clone(out Enum: IEnumVariant): HResult; stdcall;

    function Get__NewEnum: IUnknown; safecall;
    function Get_Count: Integer; safecall;
    function Get_Item(Index: Integer): IStRegINIValue; safecall;
  end;

  TStRegINI = class(TAutoObject, IStRegINI)
   private
    FOwner : StRegIni.TStRegIni;
    FIsLicensed : Boolean;
   public
    procedure Initialize; override;
    destructor Destroy; override;
   protected
    function Get_CurrentSubKey: WideString; safecall;
    function Get_IsIniFile: WordBool; safecall;
    function Get_Primary: WideString; safecall;
    function Get_SubKeys: IStRegINISubKeys; safecall;
    function Get_Values: IStRegINIValues; safecall;
    function KeyExists(const KeyName: WideString): WordBool; safecall;
    function QueryKey: IStRegINIQueryKeyInfo; safecall;
    function ReadBoolean(const ValueName: WideString;
      Default: WordBool): WordBool; safecall;
    function ReadDate(const ValueName: WideString;
      Default: TDateTime): TDateTime; safecall;
    function ReadDateTime(const ValueName: WideString;
      Default: TDateTime): TDateTime; safecall;
    function ReadInteger(const ValueName: WideString;
      Default: Integer): Integer; safecall;
    function ReadString(const ValueName, Default: WideString): WideString;
      safecall;
    function ReadTime(const ValueName: WideString;
      Default: TDateTime): TDateTime; safecall;
    procedure CreateKey(const KeyName: WideString); safecall;
    procedure DeleteKey(const KeyName: WideString; DeleteSubKeys: WordBool);
      safecall;
    procedure DeleteValue(const ValueName: WideString); safecall;
    procedure Open(const RootName: WideString; IsIniFile: WordBool); safecall;
    procedure Set_CurrentSubKey(const Value: WideString); safecall;
    procedure Set_Primary(const Value: WideString); safecall;
    procedure WriteBoolean(const ValueName: WideString; Value: WordBool);
      safecall;
    procedure WriteDate(const ValueName: WideString; Value: TDateTime);
      safecall;
    procedure WriteDateTime(const ValueName: WideString; Value: TDateTime);
      safecall;
    procedure WriteInteger(const ValueName: WideString; Value: Integer);
      safecall;
    procedure WriteString(const ValueName, Value: WideString); safecall;
    procedure WriteTime(const ValueName: WideString; Value: TDateTime);
      safecall;
    function License(const Key: WideString): WordBool; safecall;
    { Protected declarations }
  end;

implementation

uses ComServ, StBase, StConst, StDate {$IFDEF LICENSE}, StComLic {$ENDIF};

procedure RaiseRegIniError(Code : LongInt);
var
  E : ESTRegIniError;
begin
  E := ESTRegIniError.CreateResTP(Code, 0);
  E.ErrorCode := Code;
  raise E;
end;

{ ******** IRegIniQueryKeyInfo *** }
constructor TStRegINIQueryKeyInfo.Create(AOwner: StRegIni.TQueryKeyInfo);
begin
  inherited  Create(ComServer.TypeLib, IStRegIniQueryKeyInfo);
  FOwner := AOwner;
end;

function TStRegINIQueryKeyInfo.Get_QIClassName: WideString;
begin
  Result := FOwner.QIClassName;
end;

function TStRegINIQueryKeyInfo.Get_QIFileTime: TDateTime;
begin
//  Result := FOwner.QIFileTime;
end;

function TStRegINIQueryKeyInfo.Get_QIKey: Integer;
begin
  Result := FOwner.QIKey;
end;

function TStRegINIQueryKeyInfo.Get_QIMaxCNLen: Integer;
begin
  Result := FOwner.QIMaxCNLen;
end;

function TStRegINIQueryKeyInfo.Get_QIMaxDataLen: Integer;
begin
  Result := FOwner.QIMaxDataLen;
end;

function TStRegINIQueryKeyInfo.Get_QIMaxSKNLen: Integer;
begin
  Result := FOwner.QIMaxSKNLen;
end;

function TStRegINIQueryKeyInfo.Get_QIMaxVNLen: Integer;
begin
  Result := FOwner.QIMaxVNLen;
end;

function TStRegINIQueryKeyInfo.Get_QINumSubKeys: Integer;
begin
  Result := FOwner.QINumSubKeys;
end;

function TStRegINIQueryKeyInfo.Get_QINumValues: Integer;
begin
  Result := FOwner.QINumValues;
end;

function TStRegINIQueryKeyInfo.Get_QISDescLen: Integer;
begin
  Result := FOwner.QISDescLen;
end;
{ ******************************** }

{ ******** IRegIniSubKey ********* }
constructor TStRegIniSubKey.Create(AOwner: WideString);
begin
  inherited  Create(ComServer.TypeLib, IStRegIniSubKey);
  FOwner := AOwner;
end;

function TStRegINISubKey.Get_Value: WideString;
begin
  Result := FOwner;
end;
{ ******************************** }

{ ******** IRegIniSubKeys ******** }
constructor TStRegIniSubKeys.Create(AOwner: TStrings);
begin
  inherited Create(ComServer.TypeLib, IStRegIniSubKeys);
  FOwner := AOwner;
  FEnumPos := 0;
end;

destructor TStRegIniSubKeys.Destroy;
begin
  FOwner.Free;
  inherited Destroy;
end;

function TStRegIniSubKeys.Next(celt: LongInt; out elt; pceltFetched: PLongInt): HRESULT;
var
  V : OleVariant;
  I : Integer;
begin
  Result := S_FALSE;
  try
    if pceltFetched <> nil then
      pceltFetched^ := 0;
    for I := 0 to celt - 1 do begin
      if FEnumPos >= FOwner.Count then begin
        FEnumPos := 0;
        Exit;
      end;
      V := Get_Item(FEnumPos);
      TVariantArgList(elt)[I] := TVariantArg(V);

      // Prevent COM garbage collection
      TVarData(V).VType := varEmpty;
      TVarData(V).VInteger := 0;

      Inc(FEnumPos);
      if pceltFetched <> nil then
        Inc(pceltFetched^);
    end;
  except
  end;
  if (pceltFetched = nil) or ((pceltFetched <> nil) and (pceltFetched^ = celt)) then
   Result := S_OK;
end;

function TStRegIniSubKeys.Skip(celt: LongInt): HResult;
begin
  Inc(FEnumPos, celt);
  Result := S_OK;
end;

function TStRegIniSubKeys.Reset: HResult;
begin
  FEnumPos := 0;
  Result := S_OK;
end;

function TStRegIniSubKeys.Clone(out Enum: IEnumVariant): HResult;
begin
  Enum := nil;
  Result := S_OK;
  try
    Enum := Self.Create(FOwner);
  except
    Result := E_OUTOFMEMORY;
  end;
end;

function TStRegINISubKeys.Get__NewEnum: IUnknown;
begin
  Result := Self;
end;

function TStRegINISubKeys.Get_Count: Integer;
begin
  FOwner.Count;
end;

function TStRegINISubKeys.Get_Item(Index: Integer): IStRegINISubKey;
begin
  Result := TStRegIniSubKey.Create(FOwner.Strings[Index]);
end;
{ ******************************** }

{ ******** IRegIniValue ********** }
constructor TStRegIniValue.Create(AOwner: WideString);
begin
  inherited  Create(ComServer.TypeLib, IStRegIniValue);
  FOwner := AOwner;
end;

function TStRegIniValue.Get_Value: WideString;
begin
  Result := FOwner;
end;
{ ******************************** }

{ ******** IRegIniValues ********* }
constructor TStRegIniValues.Create(AOwner: TStrings);
begin
  inherited Create(ComServer.TypeLib, IStRegIniValues);
  FOwner := AOwner;
  FEnumPos := 0;
end;

destructor TStRegIniValues.Destroy;
begin
  FOwner.Free;
  inherited Destroy;
end;

function TStRegIniValues.Next(celt: LongInt; out elt; pceltFetched: PLongInt): HResult;
var
  V : OleVariant;
  I : Integer;
begin
  Result := S_FALSE;
  try
    if pceltFetched <> nil then
      pceltFetched^ := 0;
    for I := 0 to celt - 1 do begin
      if FEnumPos >= FOwner.Count then begin
        FEnumPos := 0;
        Exit;
      end;
      V := Get_Item(FEnumPos);
      TVariantArgList(elt)[I] := TVariantArg(V);

      // Prevent COM garbage collection
      TVarData(V).VType := varEmpty;
      TVarData(V).VInteger := 0;

      Inc(FEnumPos);
      if pceltFetched <> nil then
        Inc(pceltFetched^);
    end;
  except
  end;
  if (pceltFetched = nil) or ((pceltFetched <> nil) and (pceltFetched^ = celt)) then
   Result := S_OK;
end;

function TStRegIniValues.Skip(celt: LongInt): HResult;
begin
  Inc(FEnumPos, celt);
  Result := S_OK;
end;

function TStRegIniValues.Reset: HResult;
begin
  FEnumPos := 0;
  Result := S_OK;
end;

function TStRegIniValues.Clone(out Enum: IEnumVariant): HResult;
begin
  Enum := nil;
  Result := S_OK;
  try
    Enum := Self.Create(FOwner);
  except
    Result := E_OUTOFMEMORY;
  end;
end;

function TStRegINIValues.Get__NewEnum: IUnknown;
begin
  Result := Self;
end;

function TStRegINIValues.Get_Count: Integer;
begin
  FOwner.Count;
end;

function TStRegINIValues.Get_Item(Index: Integer): IStRegINIValue;
begin
  Result := TStRegIniValue.Create(FOwner.Strings[Index]);
end;
{ ******************************** }

{ ******** IStRegIni ************* }
procedure TStRegINI.Initialize;
begin
  inherited Initialize;
  {$IFDEF LICENSE}
  FIsLicensed := False;
  {$ELSE}
  FIsLicensed := True;
  {$ENDIF}
end;

destructor TStRegINI.Destroy;
begin
  inherited Destroy;
end;

procedure TStRegINI.Open(const RootName: WideString; IsIniFile: WordBool);
begin
  {$IFDEF LICENSE}
   if (not FIsLicensed) or (not COMHasBeenLicensed) then
     OleError(CLASS_E_NOTLICENSED);
  {$ENDIF}
  if Assigned(FOwner) then
    RaiseRegIniError(stscOpenKeyFail)
  else
    FOwner := StRegIni.TStRegIni.Create(RootName, IsIniFile);
end;

function TStRegINI.Get_CurrentSubKey: WideString;
begin
  {$IFDEF LICENSE}
   if (not FIsLicensed) or (not COMHasBeenLicensed) then
     OleError(CLASS_E_NOTLICENSED);
  {$ENDIF}
  Result := FOwner.CurSubKey;
end;

procedure TStRegINI.Set_CurrentSubKey(const Value: WideString);
begin
  {$IFDEF LICENSE}
   if (not FIsLicensed) or (not COMHasBeenLicensed) then
     OleError(CLASS_E_NOTLICENSED);
  {$ENDIF}
  FOwner.CurSubKey := Value;
end;

function TStRegINI.Get_IsIniFile: WordBool;
begin
  {$IFDEF LICENSE}
   if (not FIsLicensed) or (not COMHasBeenLicensed) then
     OleError(CLASS_E_NOTLICENSED);
  {$ENDIF}
  Result := FOwner.IsIniFile;
end;

procedure TStRegINI.CreateKey(const KeyName: WideString);
begin
  {$IFDEF LICENSE}
   if (not FIsLicensed) or (not COMHasBeenLicensed) then
     OleError(CLASS_E_NOTLICENSED);
  {$ENDIF}
  FOwner.CreateKey(KeyName);
end;

procedure TStRegINI.DeleteKey(const KeyName: WideString;
  DeleteSubKeys: WordBool);
begin
  {$IFDEF LICENSE}
   if (not FIsLicensed) or (not COMHasBeenLicensed) then
     OleError(CLASS_E_NOTLICENSED);
  {$ENDIF}
  FOwner.DeleteKey(KeyName, DeleteSubKeys);
end;

procedure TStRegINI.DeleteValue(const ValueName: WideString);
begin
  {$IFDEF LICENSE}
   if (not FIsLicensed) or (not COMHasBeenLicensed) then
     OleError(CLASS_E_NOTLICENSED);
  {$ENDIF}
  FOwner.DeleteValue(ValueName);
end;

function TStRegINI.Get_Primary: WideString;
begin
  {$IFDEF LICENSE}
   if (not FIsLicensed) or (not COMHasBeenLicensed) then
     OleError(CLASS_E_NOTLICENSED);
  {$ENDIF}
  Result := FOwner.GetPrimary;
end;

procedure TStRegINI.Set_Primary(const Value: WideString);
begin
  {$IFDEF LICENSE}
   if (not FIsLicensed) or (not COMHasBeenLicensed) then
     OleError(CLASS_E_NOTLICENSED);
  {$ENDIF}
  FOwner.SetPrimary(Value);
end;

function TStRegINI.KeyExists(const KeyName: WideString): WordBool;
begin
  {$IFDEF LICENSE}
   if (not FIsLicensed) or (not COMHasBeenLicensed) then
     OleError(CLASS_E_NOTLICENSED);
  {$ENDIF}
  Result := FOwner.KeyExists(KeyName);
end;

function TStRegINI.Get_SubKeys: IStRegIniSubKeys;
var
  Keys : TStringList;
begin
  {$IFDEF LICENSE}
   if (not FIsLicensed) or (not COMHasBeenLicensed) then
     OleError(CLASS_E_NOTLICENSED);
  {$ENDIF}
  Keys := nil;
  try
    Keys := TStringList.Create;
    FOwner.GetSubKeys(Keys);
    Result := TStRegIniSubKeys.Create(Keys);
  except
    Result := nil;
    Keys.Free;
  end;
end;

function TStRegINI.Get_Values: IStRegIniValues;
var
  Keys : TStringList;
begin
  {$IFDEF LICENSE}
   if (not FIsLicensed) or (not COMHasBeenLicensed) then
     OleError(CLASS_E_NOTLICENSED);
  {$ENDIF}
  Keys := nil;
  try
    Keys := TStringList.Create;
    FOwner.GetValues(Keys);
    Result := TStRegIniValues.Create(Keys);
  except
    Result := nil;
    Keys.Free;
  end;
end;

function TStRegINI.QueryKey: IStRegIniQueryKeyInfo;
var
  QKI : StRegIni.TQueryKeyInfo;
begin
  {$IFDEF LICENSE}
   if (not FIsLicensed) or (not COMHasBeenLicensed) then
     OleError(CLASS_E_NOTLICENSED);
  {$ENDIF}
  FOwner.QueryKey(QKI);
  Result := TStRegIniQueryKeyInfo.Create(QKI);
end;

function TStRegINI.ReadBoolean(const ValueName: WideString;
  Default: WordBool): WordBool;
begin
  {$IFDEF LICENSE}
   if (not FIsLicensed) or (not COMHasBeenLicensed) then
     OleError(CLASS_E_NOTLICENSED);
  {$ENDIF}
  Result := FOwner.ReadBoolean(ValueName, Default);
end;

function TStRegINI.ReadDate(const ValueName: WideString;
  Default: TDateTime): TDateTime;
begin
  {$IFDEF LICENSE}
   if (not FIsLicensed) or (not COMHasBeenLicensed) then
     OleError(CLASS_E_NOTLICENSED);
  {$ENDIF}
  Result := FOwner.ReadDate(ValueName, StDate.DateTimeToStDate(Default));
end;

function TStRegINI.ReadDateTime(const ValueName: WideString;
  Default: TDateTime): TDateTime;
begin
  {$IFDEF LICENSE}
   if (not FIsLicensed) or (not COMHasBeenLicensed) then
     OleError(CLASS_E_NOTLICENSED);
  {$ENDIF}
  Result := FOwner.ReadDateTime(ValueName, Default);
end;

function TStRegINI.ReadInteger(const ValueName: WideString;
  Default: Integer): Integer;
begin
  {$IFDEF LICENSE}
   if (not FIsLicensed) or (not COMHasBeenLicensed) then
     OleError(CLASS_E_NOTLICENSED);
  {$ENDIF}
  Result := FOwner.ReadInteger(ValueName, Default);
end;

function TStRegINI.ReadString(const ValueName,
  Default: WideString): WideString;
begin
  {$IFDEF LICENSE}
   if (not FIsLicensed) or (not COMHasBeenLicensed) then
     OleError(CLASS_E_NOTLICENSED);
  {$ENDIF}
  Result := FOwner.ReadString(ValueName, Default);
end;

function TStRegINI.ReadTime(const ValueName: WideString;
  Default: TDateTime): TDateTime;
begin
  {$IFDEF LICENSE}
   if (not FIsLicensed) or (not COMHasBeenLicensed) then
     OleError(CLASS_E_NOTLICENSED);
  {$ENDIF}
  Result := FOwner.ReadTime(ValueName, StDate.DateTimeToStTime(Default));
end;

procedure TStRegINI.WriteBoolean(const ValueName: WideString;
  Value: WordBool);
begin
  {$IFDEF LICENSE}
   if (not FIsLicensed) or (not COMHasBeenLicensed) then
     OleError(CLASS_E_NOTLICENSED);
  {$ENDIF}
  FOwner.WriteBoolean(ValueName, Value);
end;

procedure TStRegINI.WriteDate(const ValueName: WideString;
  Value: TDateTime);
begin
  {$IFDEF LICENSE}
   if (not FIsLicensed) or (not COMHasBeenLicensed) then
     OleError(CLASS_E_NOTLICENSED);
  {$ENDIF}
  FOwner.WriteDate(ValueName, StDate.DateTimeToStDate(Value));
end;

procedure TStRegINI.WriteDateTime(const ValueName: WideString;
  Value: TDateTime);
begin
  {$IFDEF LICENSE}
   if (not FIsLicensed) or (not COMHasBeenLicensed) then
     OleError(CLASS_E_NOTLICENSED);
  {$ENDIF}
  FOwner.WriteDateTime(ValueName, Value);
end;

procedure TStRegINI.WriteInteger(const ValueName: WideString;
  Value: Integer);
begin
  {$IFDEF LICENSE}
   if (not FIsLicensed) or (not COMHasBeenLicensed) then
     OleError(CLASS_E_NOTLICENSED);
  {$ENDIF}
  FOwner.WriteInteger(ValueName, Value);
end;

procedure TStRegINI.WriteString(const ValueName, Value: WideString);
begin
  {$IFDEF LICENSE}
   if (not FIsLicensed) or (not COMHasBeenLicensed) then
     OleError(CLASS_E_NOTLICENSED);
  {$ENDIF}
  FOwner.WriteString(ValueName, Value);
end;

procedure TStRegINI.WriteTime(const ValueName: WideString;
  Value: TDateTime);
begin
  {$IFDEF LICENSE}
   if (not FIsLicensed) or (not COMHasBeenLicensed) then
     OleError(CLASS_E_NOTLICENSED);
  {$ENDIF}
  FOwner.WriteTime(ValueName, StDate.DateTimeToStTime(Value));
end;
{ ******************************** }


function TStRegINI.License(const Key: WideString): WordBool;
begin
  {$IFDEF LICENSE}
  Result := COMIsValidKey(Key);
  {$ELSE}
  Result := True;
  {$ENDIF}
  FIsLicensed := Result;
end;

initialization
  TAutoObjectFactory.Create(ComServer, TStRegINI, Class_StRegINI, ciMultiInstance, tmBoth);
end.

