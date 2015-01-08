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
{*                   _STUTIL.PAS 3.00                    *}
{*********************************************************}

{$I STDEFINE.INC}
{$I STCOMDEF.INC}
unit _StUtil;

interface

uses
  Windows, ComObj, ActiveX, AxCtrls, Classes, SysTools_TLB, StdVcl;

type
  IEnumVariant = interface(IUnknown)
    ['{00020404-0000-0000-C000-000000000046}']
    function Next(celt: Longint; out elt; pceltFetched: PLongint): HResult; stdcall;
    function Skip(celt: Longint): HResult; stdcall;
    function Reset: HResult; stdcall;
    function Clone(out Enum: IEnumVariant): HResult; stdcall;
  end;

  TStStringList = class(TAutoObject, IConnectionPointContainer, IEnumVariant, IStStringList)
   private   { Private declarations }
    FConnectionPoints: TConnectionPoints;
    FConnectionPoint : TConnectionPoint;
    FSinkList        : TList;
    FEvents          : IStStringListEvents;

    FStringList      : Classes.TStringList;
    FExternalList    : Boolean;
    FEnumPos         : Integer;
    FIsLicensed      : Boolean;

    function GetStringList: TStringList;
    procedure SetStringList(Value: TStringList);

    procedure _OnChange(Sender: TObject);
    procedure _OnChanging(Sender: TObject);
   public    { Public declarations }
    constructor Create(AList: TStringList); reintroduce; overload;
    procedure Initialize; override;
    destructor Destroy; override;

    property StringList : TStringList read GetStringList write SetStringList;
   protected { Protected declarations }
    { IConnectionPointContainer }
    property ConnectionPoints: TConnectionPoints read FConnectionPoints
      implements IConnectionPointContainer;
    procedure EventSinkChanged(const EventSink: IUnknown); override;

    { IEnumVariant }
    function Next(celt: Longint; out elt; pceltFetched: PLongint): HResult; stdcall;
    function Skip(celt: Longint): HResult; stdcall;
    function Reset: HResult; stdcall;
    function Clone(out Enum: IEnumVariant): HResult; stdcall;

    { IStStringList - Properties }
    function Get__NewEnum: IUnknown; safecall;
    function Get_CommaText: WideString; safecall;
    function Get_Count: Integer; safecall;
    function Get_Duplicates: Integer; safecall;
    function Get_Item(Index: Integer): WideString; safecall;
    function Get_Names(Index: Integer): WideString; safecall;
    function Get_Sorted: WordBool; safecall;
    function Get_Stream: OleVariant; safecall;
    function Get_Strings(Index: Integer): WideString; safecall;
    function Get_Text: WideString; safecall;
    function Get_Values(const Name: WideString): WideString; safecall;

    procedure Set_CommaText(const Value: WideString); safecall;
    procedure Set_Duplicates(Value: Integer); safecall;
    procedure Set_Item(Index: Integer; const Value: WideString); safecall;
    procedure Set_Sorted(Value: WordBool); safecall;
    procedure Set_Stream(Value: OleVariant); safecall;
    procedure Set_Strings(Index: Integer; const Value: WideString); safecall;
    procedure Set_Text(const Value: WideString); safecall;
    procedure Set_Values(const Name, Value: WideString); safecall;

    { IStStringList - Methods }
    function Add(const S: WideString): Integer; safecall;
    procedure Append(const S: WideString); safecall;
    procedure Clear; safecall;
    procedure Delete(Index: Integer); safecall;
    function Equals(const Strings: IStStringList): WordBool; safecall;
    procedure Exchange(Index1, Index2: Integer); safecall;
    function Find(const S: WideString; var Index: Integer): WordBool; safecall;
    function IndexOf(const S: WideString): Integer; safecall;
    function IndexOfName(const Name: WideString): Integer; safecall;
    procedure Insert(Index: Integer; const S: WideString); safecall;
    procedure LoadFromFile(const FileName: WideString); safecall;
    procedure Move(CurIndex, NewIndex: Integer); safecall;
    procedure SaveToFile(const FileName: WideString); safecall;
    procedure Sort; safecall;
    function License(const Key: WideString): WordBool; safecall;
  end;

  function StStreamToOleVariant(Value: TStream): OleVariant;
  function StOleVariantToStream(Value: OleVariant; NewStream: Boolean): TStream;

  function StTextToOleVariant(Value: string): OleVariant;
  function StOleVariantToText(Value: OleVariant): string;


implementation

uses ComServ {$IFDEF LICENSE}, StComLic {$ENDIF};

{ Converts a TStream class to an OleVariant [array of byte] }
function StStreamToOleVariant(Value: TStream): OleVariant;
var
  Info : array of Byte;
begin
  Value.Position := 0;
  SetLength(Info, Value.Size);
  Value.Read(Info[0], Value.Size);
  Result := Info;
end;

{$WARNINGS OFF}
{ Converts an OleVariant [array of byte] to a TStream class }
function StOleVariantToStream(Value: OleVariant; NewStream: Boolean): TStream;
var
  Info : array of Byte;
begin
  if NewStream then
    Result := TMemoryStream.Create;
  Info := Value;
  Result.Write(Info[0], Length(Info));
  Result.Position := 0;
end;
{$WARNINGS ON}

{ Converts a text string to an OleVariant [array of byte] }
function StTextToOleVariant(Value: string): OleVariant;
var
  SL   : TStringList;
  MS   : TStream;
begin
  SL := nil;
  MS := nil;
  try
    SL := TStringList.Create;
    MS := TMemoryStream.Create;

    SL.Text := Value;
    SL.SaveToStream(MS);

    Result := StStreamToOleVariant(MS);
  finally
    MS.Free;
    SL.Free;
  end;
end;

{ Converts an OleVariant [array of byte] to a text string }
function StOleVariantToText(Value: OleVariant): string;
var
  SL : TStringList;
  MS : TStream;
begin
  SL := nil;
  MS := nil;
  try
    SL := TStringList.Create;
    MS := StOleVariantToStream(Value, True);

    SL.LoadFromStream(MS);
    Result := SL.Text;
  finally
    MS.Free;
    SL.Free;
  end;
end;

{ ******** TStStringList Interface - IConnectionPointContainer Methods ******** }
procedure TStStringList.EventSinkChanged(const EventSink: IUnknown);
begin
  FEvents := EventSink as IStStringListEvents;
  if FConnectionPoint <> nil then
     FSinkList := FConnectionPoint.SinkList;
end;

{ ******** TStStringList Interface - IEnumVariant Methods ******** }
function TStStringList.Next(celt: Longint; out elt; pceltFetched: PLongint): HResult;
var
  V : OleVariant;
  I : Integer;
begin
  Result := S_FALSE;
  try
    if pceltFetched <> nil then
      pceltFetched^ := 0;
    for I := 0 to celt - 1 do begin
      if FEnumPos >= FStringList.Count then begin
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

function TStStringList.Skip(celt: Longint): HResult;
begin
  Inc(FEnumPos, celt);
  Result := S_OK;
end;

function TStStringList.Reset: HResult;
begin
  FEnumPos := 0;
  Result := S_OK;
end;

function TStStringList.Clone(out Enum: IEnumVariant): HResult;
begin
  Enum := nil;
  Result := S_OK;
  try
    Enum := Self.Create;
    TStStringList(Enum).FStringList.Assign(FStringList);
  except
    Result := E_OUTOFMEMORY;
  end;
end;

{ ********** TStStringList Interface ***************************************************}
constructor TStStringList.Create(AList: TStringList);
begin
  FExternalList := True;
  FStringList   := AList;
  inherited Create;
end;

procedure TStStringList.Initialize;
begin
  inherited Initialize;
  FConnectionPoints := TConnectionPoints.Create(Self);
  if AutoFactory.EventTypeInfo <> nil then
    FConnectionPoint := FConnectionPoints.CreateConnectionPoint(
      AutoFactory.EventIID, ckSingle, EventConnect)
  else FConnectionPoint := nil;

  {$IFDEF LICENSE}
  FIsLicensed := False;
  {$ELSE}
  FIsLicensed := True;
  {$ENDIF}

  if not FExternalList then
    FStringList := TStringList.Create;

  FEnumPos    := 0;

  FStringList.OnChange := _OnChange;
  FStringList.OnChanging := _OnChanging;
end;

destructor TStStringList.Destroy;
begin
  if (FStringList <> nil) and (not FExternalList) then
    FStringList.Free;

  inherited Destroy;
end;

function TStStringList.GetStringList: TStringList;
begin
  Result := FStringList;
end;

procedure TStStringList.SetStringList(Value: TStringList);
begin
  FStringList.Assign(Value);
end;

{ ********** TStStringList Events *********************************************************}
procedure TStStringList._OnChange(Sender: TObject);
begin
  if Assigned(FEvents) then
    FEvents.OnChange;
end;

procedure TStStringList._OnChanging(Sender: TObject);
begin
  if Assigned(FEvents) then
    FEvents.OnChanging;
end;

{ ********** TStStringList Properties *** (Get) *******************************************}
function TStStringList.Get__NewEnum: IUnknown;
begin
  Result := Self;
end;

function TStStringList.Get_Item(Index: Integer): WideString;
begin
  {$IFDEF LICENSE}
   if (not FIsLicensed) or (not COMHasBeenLicensed) then
     OleError(CLASS_E_NOTLICENSED);
  {$ENDIF}
  Result := FStringList.Strings[Index];
end;

function TStStringList.Get_CommaText: WideString;
begin
  {$IFDEF LICENSE}
   if (not FIsLicensed) or (not COMHasBeenLicensed) then
     OleError(CLASS_E_NOTLICENSED);
  {$ENDIF}
  Result := FStringList.CommaText;
end;

function TStStringList.Get_Count: Integer;
begin
  {$IFDEF LICENSE}
   if (not FIsLicensed) or (not COMHasBeenLicensed) then
     OleError(CLASS_E_NOTLICENSED);
  {$ENDIF}
  Result := FStringList.Count;
end;

function TStStringList.Get_Duplicates: Integer;
begin
  {$IFDEF LICENSE}
   if (not FIsLicensed) or (not COMHasBeenLicensed) then
     OleError(CLASS_E_NOTLICENSED);
  {$ENDIF}
  Result := Ord(FStringList.Duplicates);
end;

function TStStringList.Get_Names(Index: Integer): WideString;
begin
  {$IFDEF LICENSE}
   if (not FIsLicensed) or (not COMHasBeenLicensed) then
     OleError(CLASS_E_NOTLICENSED);
  {$ENDIF}
  Result := FStringList.Names[Index];
end;

function TStStringList.Get_Sorted: WordBool;
begin
  {$IFDEF LICENSE}
   if (not FIsLicensed) or (not COMHasBeenLicensed) then
     OleError(CLASS_E_NOTLICENSED);
  {$ENDIF}
  Result := FStringList.Sorted;
end;

function TStStringList.Get_Stream: OleVariant;
begin
  {$IFDEF LICENSE}
   if (not FIsLicensed) or (not COMHasBeenLicensed) then
     OleError(CLASS_E_NOTLICENSED);
  {$ENDIF}
  Result := StTextToOleVariant(FStringList.Text);
end;

function TStStringList.Get_Strings(Index: Integer): WideString;
begin
  {$IFDEF LICENSE}
   if (not FIsLicensed) or (not COMHasBeenLicensed) then
     OleError(CLASS_E_NOTLICENSED);
  {$ENDIF}
  Result := FStringList.Strings[Index];
end;

function TStStringList.Get_Text: WideString;
begin
  {$IFDEF LICENSE}
   if (not FIsLicensed) or (not COMHasBeenLicensed) then
     OleError(CLASS_E_NOTLICENSED);
  {$ENDIF}
  Result := FStringList.Text;
end;

function TStStringList.Get_Values(const Name: WideString): WideString;
begin
  {$IFDEF LICENSE}
   if (not FIsLicensed) or (not COMHasBeenLicensed) then
     OleError(CLASS_E_NOTLICENSED);
  {$ENDIF}
  Result := FStringList.Values[Name];
end;

{ ********** TStStringList Properties *** (Set) *******************************************}
procedure TStStringList.Set_CommaText(const Value: WideString);
begin
  {$IFDEF LICENSE}
   if (not FIsLicensed) or (not COMHasBeenLicensed) then
     OleError(CLASS_E_NOTLICENSED);
  {$ENDIF}
  FStringList.CommaText := Value;
end;

procedure TStStringList.Set_Duplicates(Value: Integer);
begin
  {$IFDEF LICENSE}
   if (not FIsLicensed) or (not COMHasBeenLicensed) then
     OleError(CLASS_E_NOTLICENSED);
  {$ENDIF}
  FStringList.Duplicates := Classes.TDuplicates(Value);
end;

procedure TStStringList.Set_Item(Index: Integer; const Value: WideString);
begin
  {$IFDEF LICENSE}
   if (not FIsLicensed) or (not COMHasBeenLicensed) then
     OleError(CLASS_E_NOTLICENSED);
  {$ENDIF}
  FStringList.Strings[Index] := Value;
end;

procedure TStStringList.Set_Sorted(Value: WordBool);
begin
  {$IFDEF LICENSE}
   if (not FIsLicensed) or (not COMHasBeenLicensed) then
     OleError(CLASS_E_NOTLICENSED);
  {$ENDIF}
  FStringList.Sorted := Value;
end;

procedure TStStringList.Set_Stream(Value: OleVariant);
begin
  {$IFDEF LICENSE}
   if (not FIsLicensed) or (not COMHasBeenLicensed) then
     OleError(CLASS_E_NOTLICENSED);
  {$ENDIF}
  FStringList.Text := StOleVariantToText(Value);
end;

procedure TStStringList.Set_Strings(Index: Integer;
  const Value: WideString);
begin
  {$IFDEF LICENSE}
   if (not FIsLicensed) or (not COMHasBeenLicensed) then
     OleError(CLASS_E_NOTLICENSED);
  {$ENDIF}
  FStringList.Strings[Index] := Value;
end;

procedure TStStringList.Set_Text(const Value: WideString);
begin
  {$IFDEF LICENSE}
   if (not FIsLicensed) or (not COMHasBeenLicensed) then
     OleError(CLASS_E_NOTLICENSED);
  {$ENDIF}
  FStringList.Text := Value;
end;

procedure TStStringList.Set_Values(const Name, Value: WideString);
begin
  {$IFDEF LICENSE}
   if (not FIsLicensed) or (not COMHasBeenLicensed) then
     OleError(CLASS_E_NOTLICENSED);
  {$ENDIF}
  FStringList.Values[Name] := Value;
end;

{ ********** TStStringList Methods *****************************************************}
function TStStringList.Add(const S: WideString): Integer;
begin
  {$IFDEF LICENSE}
   if (not FIsLicensed) or (not COMHasBeenLicensed) then
     OleError(CLASS_E_NOTLICENSED);
  {$ENDIF}
  FStringList.Add(S);
end;

procedure TStStringList.Append(const S: WideString);
begin
  {$IFDEF LICENSE}
   if (not FIsLicensed) or (not COMHasBeenLicensed) then
     OleError(CLASS_E_NOTLICENSED);
  {$ENDIF}
  FStringList.Append(S);
end;

procedure TStStringList.Clear;
begin
  {$IFDEF LICENSE}
   if (not FIsLicensed) or (not COMHasBeenLicensed) then
     OleError(CLASS_E_NOTLICENSED);
  {$ENDIF}
  FStringList.Clear;
end;

procedure TStStringList.Delete(Index: Integer);
begin
  {$IFDEF LICENSE}
   if (not FIsLicensed) or (not COMHasBeenLicensed) then
     OleError(CLASS_E_NOTLICENSED);
  {$ENDIF}
  FStringList.Delete(Index);
end;

function TStStringList.Equals(const Strings: IStStringList): WordBool;
begin
  {$IFDEF LICENSE}
   if (not FIsLicensed) or (not COMHasBeenLicensed) then
     OleError(CLASS_E_NOTLICENSED);
  {$ENDIF}
  Result := FStringList.Equals(TStStringList(Strings).FStringList);
end;

procedure TStStringList.Exchange(Index1, Index2: Integer);
begin
  {$IFDEF LICENSE}
   if (not FIsLicensed) or (not COMHasBeenLicensed) then
     OleError(CLASS_E_NOTLICENSED);
  {$ENDIF}
  FStringList.Exchange(Index1, Index2);
end;

function TStStringList.Find(const S: WideString;
  var Index: Integer): WordBool;
begin
  {$IFDEF LICENSE}
   if (not FIsLicensed) or (not COMHasBeenLicensed) then
     OleError(CLASS_E_NOTLICENSED);
  {$ENDIF}
  Result := FStringList.Find(S, Index);
end;

function TStStringList.IndexOf(const S: WideString): Integer;
begin
  {$IFDEF LICENSE}
   if (not FIsLicensed) or (not COMHasBeenLicensed) then
     OleError(CLASS_E_NOTLICENSED);
  {$ENDIF}
  Result := FStringList.IndexOf(S);
end;

function TStStringList.IndexOfName(const Name: WideString): Integer;
begin
  {$IFDEF LICENSE}
   if (not FIsLicensed) or (not COMHasBeenLicensed) then
     OleError(CLASS_E_NOTLICENSED);
  {$ENDIF}
  Result := FStringList.IndexOfName(Name);
end;

procedure TStStringList.Insert(Index: Integer; const S: WideString);
begin
  {$IFDEF LICENSE}
   if (not FIsLicensed) or (not COMHasBeenLicensed) then
     OleError(CLASS_E_NOTLICENSED);
  {$ENDIF}
  FStringList.Insert(Index, S);
end;

procedure TStStringList.LoadFromFile(const FileName: WideString);
begin
  {$IFDEF LICENSE}
   if (not FIsLicensed) or (not COMHasBeenLicensed) then
     OleError(CLASS_E_NOTLICENSED);
  {$ENDIF}
  FStringList.LoadFromFile(FileName);
end;

procedure TStStringList.Move(CurIndex, NewIndex: Integer);
begin
  {$IFDEF LICENSE}
   if (not FIsLicensed) or (not COMHasBeenLicensed) then
     OleError(CLASS_E_NOTLICENSED);
  {$ENDIF}
  FStringList.Move(CurIndex, NewIndex);
end;

procedure TStStringList.SaveToFile(const FileName: WideString);
begin
  {$IFDEF LICENSE}
   if (not FIsLicensed) or (not COMHasBeenLicensed) then
     OleError(CLASS_E_NOTLICENSED);
  {$ENDIF}
  FStringList.SaveToFile(FileName);
end;

procedure TStStringList.Sort;
begin
  {$IFDEF LICENSE}
   if (not FIsLicensed) or (not COMHasBeenLicensed) then
     OleError(CLASS_E_NOTLICENSED);
  {$ENDIF}
  FStringList.Sort;
end;


function TStStringList.License(const Key: WideString): WordBool;
begin
  {$IFDEF LICENSE}
  Result := COMIsValidKey(Key);
  {$ELSE}
  Result := True;
  {$ENDIF}
  FIsLicensed := Result;
end;

initialization
  TAutoObjectFactory.Create(ComServer, TStStringList, Class_StStringList, ciMultiInstance, tmBoth);
end.
