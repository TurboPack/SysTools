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
{*                   _STREGEX.PAS 3.00                   *}
{*********************************************************}

{$I STDEFINE.INC}
{$I STCOMDEF.INC}
unit _StRegEx;

interface

uses
  ComObj, ActiveX, AxCtrls, Classes, StUtils, StStrms, StRegEx, SysTools_TLB, StdVcl;

type
  TStRegEx = class(TAutoObject, IConnectionPointContainer, IStRegEx)
  private   { Private declarations }
    FConnectionPoints: TConnectionPoints;
    FConnectionPoint: TConnectionPoint;
    FSinkList: TList;
    FEvents: IStRegExEvents;

    FStRegEx         : StRegEx.TStStreamRegEx;
    FMatchPattern    : IStStringList;
    FSelAvoidPattern : IStStringList;
    FReplacePattern  : IStStringList;

    FIsLicensed      : Boolean;
    procedure _OnProgress(Sender : TObject; Percent : Word);
  public    { Public declarations }
    procedure Initialize; override;
    destructor Destroy; override;
  protected { Protected declarations }
    { IConnectionPointContainer }
    property ConnectionPoints: TConnectionPoints read FConnectionPoints
      implements IConnectionPointContainer;
    procedure EventSinkChanged(const EventSink: IUnknown); override;

    { IStRegEx properties (GET) }
    function Get_Avoid: WordBool; safecall;
    function Get_IgnoreCase: WordBool; safecall;
    function Get_InFixedLineLength: Integer; safecall;
    function Get_InLineTermChar: WideString; safecall;
    function Get_InLineTerminator: TStLineTerminator; safecall;
    function Get_LineCount: Integer; safecall;
    function Get_LineNumbers: WordBool; safecall;
    function Get_LinesMatched: Integer; safecall;
    function Get_LinesPerSecond: Integer; safecall;
    function Get_LinesReplaced: Integer; safecall;
    function Get_LinesSelected: Integer; safecall;
    function Get_MatchPattern: IStStringList; safecall;
    function Get_OutFixedLineLength: Integer; safecall;
    function Get_OutLineTermChar: WideString; safecall;
    function Get_OutLineTerminitor: Integer; safecall;
    function Get_OutputOptions: TStOutputOption; safecall;
    function Get_ReplacePattern: IStStringList; safecall;
    function Get_SelAvoidPattern: IStStringList; safecall;
    function Get_Stream: OleVariant; safecall;

    { IStRegEx properties (SET) }
    procedure Set_Avoid(Value: WordBool); safecall;
    procedure Set_IgnoreCase(Value: WordBool); safecall;
    procedure Set_InFixedLineLength(Value: Integer); safecall;
    procedure Set_InLineTermChar(const Value: WideString); safecall;
    procedure Set_InLineTerminator(Value: TStLineTerminator); safecall;
    procedure Set_LineNumbers(Value: WordBool); safecall;
    procedure Set_MatchPattern(const Value: IStStringList); safecall;
    procedure Set_OutFixedLineLength(Value: Integer); safecall;
    procedure Set_OutLineTermChar(const Value: WideString); safecall;
    procedure Set_OutLineTerminitor(Value: Integer); safecall;
    procedure Set_OutputOptions(Value: TStOutputOption); safecall;
    procedure Set_ReplacePattern(const Value: IStStringList); safecall;
    procedure Set_SelAvoidPattern(const Value: IStStringList); safecall;
    procedure Set_Stream(Value: OleVariant); safecall;

    { IStRegEx Methods }
    function CheckString(const S: WideString; var StartPos, EndPos, Length: Integer): WordBool; safecall;
    function DOSMaskToRegEx(const Masks: WideString): WordBool; safecall;
    function Execute: WordBool; safecall;
    procedure LoadFromFile(const FileName: WideString); safecall;
    procedure SaveToFile(const FileName: WideString); safecall;
    function License(const Key: WideString): WordBool; safecall;
  end;

implementation

uses ComServ, _StUtil {$IFDEF LICENSE}, StComLic {$ENDIF};

{ ********** TStRegExp Interface - IConnectionPointContainer Methods ********************* }
procedure TStRegEx.EventSinkChanged(const EventSink: IUnknown);
begin
  FEvents := EventSink as IStRegExEvents;
  if FConnectionPoint <> nil then
     FSinkList := FConnectionPoint.SinkList;
end;

{ ********** TStRegExp Interface ********************************************************* }
procedure TStRegEx.Initialize;
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

  FStRegEx := StRegEx.TStStreamRegEx.Create;
  FStRegEx.InputStream := Classes.TMemoryStream.Create;
  FStRegEx.OutputStream := Classes.TMemoryStream.Create;

  FMatchPattern    := TStStringList.Create(FStRegEx.MatchPattern);
  FSelAvoidPattern := TStStringList.Create(FStRegEx.SelAvoidPattern);
  FReplacePattern  := TStStringList.Create(FStRegEx.ReplacePattern);

  FStRegEx.OnProgress := _OnProgress;
end;

destructor TStRegEx.Destroy;
begin
  if Assigned(FStRegEx.InputStream) then
    FStRegEx.InputStream.Free;

  if Assigned(FStRegEx.OutputStream) then
    FStRegEx.OutputStream.Free;

  FMatchPattern    := nil;
  FSelAvoidPattern := nil;
  FReplacePattern  := nil;

  if Assigned(FStRegEx) then
    FStRegEx.Free;

  inherited Destroy;
end;

{ ********** TStRegExp Events ************************************************************ }
procedure TStRegEx._OnProgress(Sender : TObject; Percent : Word);
begin
  if Assigned(FEvents) then
    FEvents.OnProgress(Integer(Percent));
end;

{ ********** TStRegExp Properties *** (Get) ********************************************** }
function TStRegEx.Get_Avoid: WordBool;
begin
  {$IFDEF LICENSE}
   if (not FIsLicensed) or (not COMHasBeenLicensed) then
     OleError(CLASS_E_NOTLICENSED);
  {$ENDIF}
  Result := FStRegEx.Avoid;
end;

function TStRegEx.Get_IgnoreCase: WordBool;
begin
  {$IFDEF LICENSE}
   if (not FIsLicensed) or (not COMHasBeenLicensed) then
     OleError(CLASS_E_NOTLICENSED);
  {$ENDIF}
  Result := FStRegEx.IgnoreCase;
end;

function TStRegEx.Get_InFixedLineLength: Integer;
begin
  {$IFDEF LICENSE}
   if (not FIsLicensed) or (not COMHasBeenLicensed) then
     OleError(CLASS_E_NOTLICENSED);
  {$ENDIF}
  Result := FStRegEx.InFixedLineLength;
end;

function TStRegEx.Get_InLineTermChar: WideString;
begin
  {$IFDEF LICENSE}
   if (not FIsLicensed) or (not COMHasBeenLicensed) then
     OleError(CLASS_E_NOTLICENSED);
  {$ENDIF}
  Result := FStRegEx.InLineTermChar;
end;

function TStRegEx.Get_InLineTerminator: TStLineTerminator;
begin
  {$IFDEF LICENSE}
   if (not FIsLicensed) or (not COMHasBeenLicensed) then
     OleError(CLASS_E_NOTLICENSED);
  {$ENDIF}
  Result := TStLineTerminator(FStRegEx.InLineTerminator);
end;

function TStRegEx.Get_LineCount: Integer;
begin
  {$IFDEF LICENSE}
   if (not FIsLicensed) or (not COMHasBeenLicensed) then
     OleError(CLASS_E_NOTLICENSED);
  {$ENDIF}
  Result := FStRegEx.LineCount;
end;

function TStRegEx.Get_LineNumbers: WordBool;
begin
  {$IFDEF LICENSE}
   if (not FIsLicensed) or (not COMHasBeenLicensed) then
     OleError(CLASS_E_NOTLICENSED);
  {$ENDIF}
  Result := FStRegEx.LineNumbers;
end;

function TStRegEx.Get_LinesMatched: Integer;
begin
  {$IFDEF LICENSE}
   if (not FIsLicensed) or (not COMHasBeenLicensed) then
     OleError(CLASS_E_NOTLICENSED);
  {$ENDIF}
  Result := FStRegEx.LinesMatched;
end;

function TStRegEx.Get_LinesPerSecond: Integer;
begin
  {$IFDEF LICENSE}
   if (not FIsLicensed) or (not COMHasBeenLicensed) then
     OleError(CLASS_E_NOTLICENSED);
  {$ENDIF}
  Result := FStRegEx.LinesPerSecond;
end;

function TStRegEx.Get_LinesReplaced: Integer;
begin
  {$IFDEF LICENSE}
   if (not FIsLicensed) or (not COMHasBeenLicensed) then
     OleError(CLASS_E_NOTLICENSED);
  {$ENDIF}
  Result := FStRegEx.LinesReplaced;
end;

function TStRegEx.Get_LinesSelected: Integer;
begin
  {$IFDEF LICENSE}
   if (not FIsLicensed) or (not COMHasBeenLicensed) then
     OleError(CLASS_E_NOTLICENSED);
  {$ENDIF}
  Result := FStRegEx.LinesSelected;
end;

function TStRegEx.Get_MatchPattern: IStStringList;
begin
  {$IFDEF LICENSE}
   if (not FIsLicensed) or (not COMHasBeenLicensed) then
     OleError(CLASS_E_NOTLICENSED);
  {$ENDIF}
  Result := FMatchPattern;
end;

function TStRegEx.Get_OutFixedLineLength: Integer;
begin
  {$IFDEF LICENSE}
   if (not FIsLicensed) or (not COMHasBeenLicensed) then
     OleError(CLASS_E_NOTLICENSED);
  {$ENDIF}
  Result := FStRegEx.OutFixedLineLength;
end;

function TStRegEx.Get_OutLineTermChar: WideString;
begin
  {$IFDEF LICENSE}
   if (not FIsLicensed) or (not COMHasBeenLicensed) then
     OleError(CLASS_E_NOTLICENSED);
  {$ENDIF}
  Result := FStRegEx.OutLineTermChar;
end;

function TStRegEx.Get_OutLineTerminitor: Integer;
begin
  {$IFDEF LICENSE}
   if (not FIsLicensed) or (not COMHasBeenLicensed) then
     OleError(CLASS_E_NOTLICENSED);
  {$ENDIF}
  Result := TStLineTerminator(FStRegEx.OutLineTerminator);
end;

function TStRegEx.Get_OutputOptions: TStOutputOption;
begin
  {$IFDEF LICENSE}
   if (not FIsLicensed) or (not COMHasBeenLicensed) then
     OleError(CLASS_E_NOTLICENSED);
  {$ENDIF}
  Result := 0;
  if (StRegEx.ooUnSelected in FStRegEx.OutputOptions) then
    Result := Result + ooUnSelected;

  if (StRegEx.ooModified in FStRegEx.OutputOptions) then
    Result := Result + ooModified;

  if (StRegEx.ooUnSelected in FStRegEx.OutputOptions) then
    Result := Result + ooCountOnly;
end;

function TStRegEx.Get_ReplacePattern: IStStringList;
begin
  {$IFDEF LICENSE}
   if (not FIsLicensed) or (not COMHasBeenLicensed) then
     OleError(CLASS_E_NOTLICENSED);
  {$ENDIF}
  Result := FReplacePattern;
end;

function TStRegEx.Get_SelAvoidPattern: IStStringList;
begin
  {$IFDEF LICENSE}
   if (not FIsLicensed) or (not COMHasBeenLicensed) then
     OleError(CLASS_E_NOTLICENSED);
  {$ENDIF}
  Result := FSelAvoidPattern;
end;

function TStRegEx.Get_Stream: OleVariant;
begin
  {$IFDEF LICENSE}
   if (not FIsLicensed) or (not COMHasBeenLicensed) then
     OleError(CLASS_E_NOTLICENSED);
  {$ENDIF}
  Result := StStreamToOleVariant(FStRegEx.OutputStream);
end;

{ ********** TStRegExp Properties *** (Set) ********************************************** }
procedure TStRegEx.Set_Avoid(Value: WordBool);
begin
  {$IFDEF LICENSE}
   if (not FIsLicensed) or (not COMHasBeenLicensed) then
     OleError(CLASS_E_NOTLICENSED);
  {$ENDIF}
  FStRegEx.Avoid := Value;
end;

procedure TStRegEx.Set_IgnoreCase(Value: WordBool);
begin
  {$IFDEF LICENSE}
   if (not FIsLicensed) or (not COMHasBeenLicensed) then
     OleError(CLASS_E_NOTLICENSED);
  {$ENDIF}
  FStRegEx.IgnoreCase := Value;
end;

procedure TStRegEx.Set_InFixedLineLength(Value: Integer);
begin
  {$IFDEF LICENSE}
   if (not FIsLicensed) or (not COMHasBeenLicensed) then
     OleError(CLASS_E_NOTLICENSED);
  {$ENDIF}
  FStRegEx.InFixedLineLength := Value;
end;

procedure TStRegEx.Set_InLineTermChar(const Value: WideString);
begin
  {$IFDEF LICENSE}
   if (not FIsLicensed) or (not COMHasBeenLicensed) then
     OleError(CLASS_E_NOTLICENSED);
  {$ENDIF}
  FStRegEx.InLineTermChar := Char(Value[1]);
end;

procedure TStRegEx.Set_InLineTerminator(Value: TStLineTerminator);
begin
  {$IFDEF LICENSE}
   if (not FIsLicensed) or (not COMHasBeenLicensed) then
     OleError(CLASS_E_NOTLICENSED);
  {$ENDIF}
  FStRegEx.InLineTerminator := StStrms.TStLineTerminator(Value);
end;

procedure TStRegEx.Set_LineNumbers(Value: WordBool);
begin
  {$IFDEF LICENSE}
   if (not FIsLicensed) or (not COMHasBeenLicensed) then
     OleError(CLASS_E_NOTLICENSED);
  {$ENDIF}
  FStRegEx.LineNumbers := Value;
end;

procedure TStRegEx.Set_MatchPattern(const Value: IStStringList);
var
  MS : TStream;
begin
  {$IFDEF LICENSE}
   if (not FIsLicensed) or (not COMHasBeenLicensed) then
     OleError(CLASS_E_NOTLICENSED);
  {$ENDIF}
  MS := nil;
  try
    MS := StOleVariantToStream(Value.Stream, True);
    FStRegEx.MatchPattern.LoadFromStream(MS);
  finally
    MS.Free;
  end;
end;

procedure TStRegEx.Set_OutFixedLineLength(Value: Integer);
begin
  {$IFDEF LICENSE}
   if (not FIsLicensed) or (not COMHasBeenLicensed) then
     OleError(CLASS_E_NOTLICENSED);
  {$ENDIF}
  FStRegEx.OutFixedLineLength := Value;
end;

procedure TStRegEx.Set_OutLineTermChar(const Value: WideString);
begin
  {$IFDEF LICENSE}
   if (not FIsLicensed) or (not COMHasBeenLicensed) then
     OleError(CLASS_E_NOTLICENSED);
  {$ENDIF}
  FStRegEx.OutLineTermChar := Char(Value[1]);
end;

procedure TStRegEx.Set_OutLineTerminitor(Value: Integer);
begin
  {$IFDEF LICENSE}
   if (not FIsLicensed) or (not COMHasBeenLicensed) then
     OleError(CLASS_E_NOTLICENSED);
  {$ENDIF}
  FStRegEx.OutLineTerminator := StStrms.TStLineTerminator(Value);
end;

procedure TStRegEx.Set_OutputOptions(Value: TStOutputOption);
begin
  {$IFDEF LICENSE}
   if (not FIsLicensed) or (not COMHasBeenLicensed) then
     OleError(CLASS_E_NOTLICENSED);
  {$ENDIF}
  FStRegEx.OutputOptions := [];
  if FlagIsSet(ooUnSelected, Value) then
    FStRegEx.OutputOptions := FStRegEx.OutputOptions + [StRegEx.ooUnselected];

  if FlagIsSet(ooModified, Value) then
    FStRegEx.OutputOptions := FStRegEx.OutputOptions + [StRegEx.ooModified];

  if FlagIsSet(ooCountOnly, Value) then
    FStRegEx.OutputOptions := FStRegEx.OutputOptions + [StRegEx.ooCountOnly];
end;

procedure TStRegEx.Set_ReplacePattern(const Value: IStStringList);
var
  MS : TStream;
begin
  {$IFDEF LICENSE}
   if (not FIsLicensed) or (not COMHasBeenLicensed) then
     OleError(CLASS_E_NOTLICENSED);
  {$ENDIF}
  MS := nil;
  try
    MS := StOleVariantToStream(Value.Stream, True);
    FStRegEx.ReplacePattern.LoadFromStream(MS);
  finally
    MS.Free;
  end;
end;

procedure TStRegEx.Set_SelAvoidPattern(const Value: IStStringList);
var
  MS : TStream;
begin
  {$IFDEF LICENSE}
   if (not FIsLicensed) or (not COMHasBeenLicensed) then
     OleError(CLASS_E_NOTLICENSED);
  {$ENDIF}
  MS := nil;
  try
    MS := StOleVariantToStream(Value.Stream, True);
    FStRegEx.SelAvoidPattern.LoadFromStream(MS);
  finally
    MS.Free;
  end;
end;

procedure TStRegEx.Set_Stream(Value: OleVariant);
var
  MS : TStream;
begin
  {$IFDEF LICENSE}
   if (not FIsLicensed) or (not COMHasBeenLicensed) then
     OleError(CLASS_E_NOTLICENSED);
  {$ENDIF}
  MS := nil;
  try
    MS := StOleVariantToStream(Value, True);
    FStRegEx.InputStream.CopyFrom(MS, 0);
    FStRegEx.InputStream.Position := 0;
  finally
    MS.Free;
  end;
end;

{ ********** TStRegExp Methods *********************************************************** }
function TStRegEx.CheckString(const S: WideString; var StartPos, EndPos,
  Length: Integer): WordBool;
var
  MP : TMatchPosition;
begin
  {$IFDEF LICENSE}
   if (not FIsLicensed) or (not COMHasBeenLicensed) then
     OleError(CLASS_E_NOTLICENSED);
  {$ENDIF}
  MP.StartPos := StartPos;
  MP.EndPos   := EndPos;
  MP.Length   := Length;
  Result := FStRegEx.CheckString(S, MP);
  StartPos := MP.StartPos;
  EndPos   := MP.EndPos;
  Length   := MP.Length;
end;

function TStRegEx.DOSMaskToRegEx(const Masks: WideString): WordBool;
begin
  {$IFDEF LICENSE}
   if (not FIsLicensed) or (not COMHasBeenLicensed) then
     OleError(CLASS_E_NOTLICENSED);
  {$ENDIF}
  Result := FStRegEx.DOSMasksToRegEx(Masks);
end;

function TStRegEx.Execute: WordBool;
begin
  {$IFDEF LICENSE}
   if (not FIsLicensed) or (not COMHasBeenLicensed) then
     OleError(CLASS_E_NOTLICENSED);
  {$ENDIF}
  Result := FStRegEx.Execute;
end;

procedure TStRegEx.LoadFromFile(const FileName: WideString);
begin
  {$IFDEF LICENSE}
   if (not FIsLicensed) or (not COMHasBeenLicensed) then
     OleError(CLASS_E_NOTLICENSED);
  {$ENDIF}
  TMemoryStream(FStRegEx.InputStream).LoadFromFile(FileName);
end;

procedure TStRegEx.SaveToFile(const FileName: WideString);
begin
  {$IFDEF LICENSE}
   if (not FIsLicensed) or (not COMHasBeenLicensed) then
     OleError(CLASS_E_NOTLICENSED);
  {$ENDIF}
  TMemoryStream(FStRegEx.OutputStream).SaveToFile(FileName);
end;


function TStRegEx.License(const Key: WideString): WordBool;
begin
  {$IFDEF LICENSE}
  Result := COMIsValidKey(Key);

  { License the objects used in this class }
  FMatchPattern.License(Key);
  FSelAvoidPattern.License(Key);
  FReplacePattern.License(Key);

  {$ELSE}
  Result := True;
  {$ENDIF}
  FIsLicensed := Result;
end;

initialization
  TAutoObjectFactory.Create(ComServer, TStRegEx, Class_StRegEx, ciMultiInstance, tmBoth);
end.
