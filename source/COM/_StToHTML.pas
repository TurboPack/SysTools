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
{*                  _STTOHTML.PAS 3.00                   *}
{*********************************************************}

{$I STDEFINE.INC}
{$I STCOMDEF.INC}
unit _StToHTML;

interface

uses
  ComObj, ActiveX, AxCtrls, Classes, StStrms, StToHTML, SysTools_TLB, _StUtil, StdVcl;

type
  TStToHTML = class(TAutoObject, IConnectionPointContainer, IStToHTML)
   private  { Private declarations }
    FConnectionPoints: TConnectionPoints;
    FConnectionPoint: TConnectionPoint;
    FSinkList: TList;
    FEvents: IStToHTMLEvents;

    FSourceToHTML  : StToHTML.TStStreamToHTML;
    FKeywords      : IStStringList;
    FCommentMarkers: IStStringList;
    FEmbeddedHTML  : IStStringList;
    FPageFooter    : IStStringList;
    FPageHeader    : IStStringList;
    FStringMarkers : IStStringList;

    FIsLicensed    : Boolean;

    procedure _OnProgress(Sender : TObject; Percent : Word);
   public    { Public declarations }
    procedure Initialize; override;
    destructor Destroy; override;
   protected {Protected declarations }
    { IConnectionPointContainer }
    property ConnectionPoints: TConnectionPoints read FConnectionPoints
      implements IConnectionPointContainer;
    procedure EventSinkChanged(const EventSink: IUnknown); override;

    { IStToHTML properties (GET) }
    function Get_CaseSensitve: WordBool; safecall;
    function Get_CommentMarkers: IStStringList; safecall;
    function Get_EmbeddedHTML: IStStringList; safecall;
    function Get_FixedLineLength: Integer; safecall;
    function Get_Keywords: IStStringList; safecall;
    function Get_LineTermChar: WideString; safecall;
    function Get_LineTerminator: TStLineTerminator; safecall;
    function Get_PageFooter: IStStringList; safecall;
    function Get_PageHeader: IStStringList; safecall;
    function Get_Stream: OleVariant; safecall;
    function Get_StringMarkers: IStStringList; safecall;
    function Get_WordDelimeters: WideString; safecall;

    { IStToHTML properties (SET) }
    procedure Set_CaseSensitve(Value: WordBool); safecall;
    procedure Set_CommentMarkers(const Value: IStStringList); safecall;
    procedure Set_EmbeddedHTML(const Value: IStStringList); safecall;
    procedure Set_FixedLineLength(Value: Integer); safecall;
    procedure Set_Keywords(const Value: IStStringList); safecall;
    procedure Set_LineTermChar(const Value: WideString); safecall;
    procedure Set_LineTerminator(Value: TStLineTerminator); safecall;
    procedure Set_PageFooter(const Value: IStStringList); safecall;
    procedure Set_PageHeader(const Value: IStStringList); safecall;
    procedure Set_Stream(Value: OleVariant); safecall;
    procedure Set_StringMarkers(const Value: IStStringList); safecall;
    procedure Set_WordDelimeters(const Value: WideString); safecall;

    { IStToHTML methods }
    procedure Clear; safecall;
    procedure GenerateHTML; safecall;
    procedure LoadFromFile(const FileName: WideString); safecall;
    procedure SaveToFile(const FileName: WideString); safecall;
    function License(const Key: WideString): WordBool; safecall;
end;

implementation

uses ComServ {$IFDEF LICENSE}, StComLic {$ENDIF};

{ ********** TStSourceToHTML Interface - IConnectionPointContainer Methods *************** }
procedure TStToHTML.EventSinkChanged(const EventSink: IUnknown);
begin
  FEvents := EventSink as IStToHTMLEvents;
  if FConnectionPoint <> nil then
     FSinkList := FConnectionPoint.SinkList;
end;

{ ********** TStSourceToHTML Interface *************************************************** }
procedure TStToHTML.Initialize;
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

  FSourceToHTML              := StToHTML.TStStreamToHTML.Create;
  FSourceToHTML.InputStream  := Classes.TMemoryStream.Create;
  FSourceToHTML.OutputStream := Classes.TMemoryStream.Create;

  FKeywords       := TStStringList.Create(FSourceToHTML.Keywords);
  FCommentMarkers := TStStringList.Create(FSourceToHTML.CommentMarkers);
  FEmbeddedHTML   := TStStringList.Create(FSourceToHTML.EmbeddedHTML);
  FPageFooter     := TStStringList.Create(FSourceToHTML.PageFooter);
  FPageHeader     := TStStringList.Create(FSourceToHTML.PageHeader);
  FStringMarkers  := TStStringList.Create(FSourceToHTML.StringMarkers);

  FSourceToHTML.OnProgress   := _OnProgress;
end;

destructor TStToHTML.Destroy;
begin
  if Assigned(FSourceToHTML.InputStream) then
    FSourceToHTML.InputStream.Free;

  if Assigned(FSourceToHTML.OutputStream) then
    FSourceToHTML.OutputStream.Free;

  FKeywords       := nil;
  FCommentMarkers := nil;
  FEmbeddedHTML   := nil;
  FPageFooter     := nil;
  FPageHeader     := nil;
  FStringMarkers  := nil;

  FSourceToHTML.Free;

  inherited Destroy;
end;

{ ********** TStSourceToHTML Events ****************************************************** }
procedure TStToHTML._OnProgress(Sender : TObject; Percent : Word);
begin
  if Assigned(FEvents) then
    FEvents.OnProgress(Percent);
end;

{ ********** TStSourceToHTML Properties *** (Get) **************************************** }
function TStToHTML.Get_CaseSensitve: WordBool;
begin
  {$IFDEF LICENSE}
   if (not FIsLicensed) or (not COMHasBeenLicensed) then
     OleError(CLASS_E_NOTLICENSED);
  {$ENDIF}
  Result := FSourceToHTML.CaseSensitive;
end;

function TStToHTML.Get_CommentMarkers: IStStringList;
begin
  {$IFDEF LICENSE}
   if (not FIsLicensed) or (not COMHasBeenLicensed) then
     OleError(CLASS_E_NOTLICENSED);
  {$ENDIF}
  Result := FCommentMarkers;
end;

function TStToHTML.Get_EmbeddedHTML: IStStringList;
begin
  {$IFDEF LICENSE}
   if (not FIsLicensed) or (not COMHasBeenLicensed) then
     OleError(CLASS_E_NOTLICENSED);
  {$ENDIF}
  Result := FEmbeddedHTML;
end;

function TStToHTML.Get_FixedLineLength: Integer;
begin
  {$IFDEF LICENSE}
   if (not FIsLicensed) or (not COMHasBeenLicensed) then
     OleError(CLASS_E_NOTLICENSED);
  {$ENDIF}
  Result := FSourceToHTML.InFixedLineLength;
end;

function TStToHTML.Get_LineTermChar: WideString;
begin
  {$IFDEF LICENSE}
   if (not FIsLicensed) or (not COMHasBeenLicensed) then
     OleError(CLASS_E_NOTLICENSED);
  {$ENDIF}
  Result := FSourceToHTML.InLineTermChar;
end;

function TStToHTML.Get_LineTerminator: TStLineTerminator;
begin
  {$IFDEF LICENSE}
   if (not FIsLicensed) or (not COMHasBeenLicensed) then
     OleError(CLASS_E_NOTLICENSED);
  {$ENDIF}
  Result := TStLineTerminator(FSourceToHTML.InLineTerminator);
end;

function TStToHTML.Get_PageFooter: IStStringList;
begin
  {$IFDEF LICENSE}
   if (not FIsLicensed) or (not COMHasBeenLicensed) then
     OleError(CLASS_E_NOTLICENSED);
  {$ENDIF}
  Result := FPageFooter;
end;

function TStToHTML.Get_PageHeader: IStStringList;
begin
  {$IFDEF LICENSE}
   if (not FIsLicensed) or (not COMHasBeenLicensed) then
     OleError(CLASS_E_NOTLICENSED);
  {$ENDIF}
  Result := FPageHeader;
end;

function TStToHTML.Get_Stream: OleVariant;
begin
  {$IFDEF LICENSE}
   if (not FIsLicensed) or (not COMHasBeenLicensed) then
     OleError(CLASS_E_NOTLICENSED);
  {$ENDIF}
  Result := StStreamToOleVariant(FSourceToHTML.OutputStream);
end;

function TStToHTML.Get_StringMarkers: IStStringList;
begin
  {$IFDEF LICENSE}
   if (not FIsLicensed) or (not COMHasBeenLicensed) then
     OleError(CLASS_E_NOTLICENSED);
  {$ENDIF}
  Result := FStringMarkers;
end;

function TStToHTML.Get_WordDelimeters: WideString;
begin
  {$IFDEF LICENSE}
   if (not FIsLicensed) or (not COMHasBeenLicensed) then
     OleError(CLASS_E_NOTLICENSED);
  {$ENDIF}
  Result := FSourceToHTML.WordDelimiters;
end;

{ ********** TStSourceToHTML Properties *** (Set) **************************************** }
procedure TStToHTML.Set_CaseSensitve(Value: WordBool);
begin
  {$IFDEF LICENSE}
   if (not FIsLicensed) or (not COMHasBeenLicensed) then
     OleError(CLASS_E_NOTLICENSED);
  {$ENDIF}
  FSourceToHTML.CaseSensitive := Value;
end;

procedure TStToHTML.Set_CommentMarkers(const Value: IStStringList);
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
    FSourceToHTML.CommentMarkers.LoadFromStream(MS);
  finally
    MS.Free;
  end;
end;

procedure TStToHTML.Set_EmbeddedHTML(const Value: IStStringList);
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
    FSourceToHTML.EmbeddedHTML.LoadFromStream(MS);
  finally
    MS.Free;
  end;
end;

procedure TStToHTML.Set_FixedLineLength(Value: Integer);
begin
  {$IFDEF LICENSE}
   if (not FIsLicensed) or (not COMHasBeenLicensed) then
     OleError(CLASS_E_NOTLICENSED);
  {$ENDIF}
  FSourceToHTML.InFixedLineLength := Value;
end;

function TStToHTML.Get_Keywords: IStStringList;
begin
  {$IFDEF LICENSE}
   if (not FIsLicensed) or (not COMHasBeenLicensed) then
     OleError(CLASS_E_NOTLICENSED);
  {$ENDIF}
  Result := FKeywords;
end;

procedure TStToHTML.Set_Keywords(const Value: IStStringList);
begin
  {$IFDEF LICENSE}
   if (not FIsLicensed) or (not COMHasBeenLicensed) then
     OleError(CLASS_E_NOTLICENSED);
  {$ENDIF}
  FKeywords := Value;
end;

procedure TStToHTML.Set_LineTermChar(const Value: WideString);
begin
  {$IFDEF LICENSE}
   if (not FIsLicensed) or (not COMHasBeenLicensed) then
     OleError(CLASS_E_NOTLICENSED);
  {$ENDIF}
  FSourceToHTML.InLineTermChar := Char(Value[1]);
end;

procedure TStToHTML.Set_LineTerminator(Value: TStLineTerminator);
begin
  {$IFDEF LICENSE}
   if (not FIsLicensed) or (not COMHasBeenLicensed) then
     OleError(CLASS_E_NOTLICENSED);
  {$ENDIF}
  FSourceToHTML.InLineTerminator := StStrms.TStLineTerminator(Value);
end;

procedure TStToHTML.Set_PageFooter(const Value: IStStringList);
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
    FSourceToHTML.PageFooter.LoadFromStream(MS);
  finally
    MS.Free;
  end;
end;

procedure TStToHTML.Set_PageHeader(const Value: IStStringList);
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
    FSourceToHTML.PageHeader.LoadFromStream(MS);
  finally
    MS.Free;
  end;
end;

procedure TStToHTML.Set_Stream(Value: OleVariant);
begin
  {$IFDEF LICENSE}
   if (not FIsLicensed) or (not COMHasBeenLicensed) then
     OleError(CLASS_E_NOTLICENSED);
  {$ENDIF}
  FSourceToHTML.InputStream.CopyFrom(StOleVariantToStream(Value, True), 0);
  FSourceToHTML.InputStream.Position := 0;
end;

procedure TStToHTML.Set_StringMarkers(const Value: IStStringList);
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
    FSourceToHTML.StringMarkers.LoadFromStream(MS);
  finally
    MS.Free;
  end;
end;

procedure TStToHTML.Set_WordDelimeters(const Value: WideString);
begin
  {$IFDEF LICENSE}
   if (not FIsLicensed) or (not COMHasBeenLicensed) then
     OleError(CLASS_E_NOTLICENSED);
  {$ENDIF}
  FSourceToHTML.WordDelimiters := Value;
end;

{ ********** TStSourceToHTML Methods ***************************************************** }
procedure TStToHTML.Clear;
begin
  {$IFDEF LICENSE}
   if (not FIsLicensed) or (not COMHasBeenLicensed) then
     OleError(CLASS_E_NOTLICENSED);
  {$ENDIF}
  TMemoryStream(FSourceToHTML.InputStream).Clear;
end;

procedure TStToHTML.GenerateHTML;
begin
  {$IFDEF LICENSE}
   if (not FIsLicensed) or (not COMHasBeenLicensed) then
     OleError(CLASS_E_NOTLICENSED);
  {$ENDIF}
  FSourceToHTML.GenerateHTML;
end;

procedure TStToHTML.LoadFromFile(const FileName: WideString);
begin
  {$IFDEF LICENSE}
   if (not FIsLicensed) or (not COMHasBeenLicensed) then
     OleError(CLASS_E_NOTLICENSED);
  {$ENDIF}
  TMemoryStream(FSourceToHTML.InputStream).LoadFromFile(FileName);
end;

procedure TStToHTML.SaveToFile(const FileName: WideString);
begin
  {$IFDEF LICENSE}
   if (not FIsLicensed) or (not COMHasBeenLicensed) then
     OleError(CLASS_E_NOTLICENSED);
  {$ENDIF}
  TMemoryStream(FSourceToHTML.OutputStream).SaveToFile(FileName);
end;

function TStToHTML.License(const Key: WideString): WordBool;
begin
  {$IFDEF LICENSE}
  Result := COMIsValidKey(Key);

  { License the objects used in this class }
  FKeywords.License(Key);
  FCommentMarkers.License(Key);
  FEmbeddedHTML.License(Key);
  FPageFooter.License(Key);
  FPageHeader.License(Key);
  FStringMarkers.License(Key);

  {$ELSE}
  Result := True;
  {$ENDIF}
  FIsLicensed := Result;
end;

initialization
  TAutoObjectFactory.Create(ComServer, TStToHTML, Class_StToHTML, ciMultiInstance, tmBoth);
end.
