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
{*                  _STSTRING.PAS 3.00                   *}
{*********************************************************}

{$I STDEFINE.INC}
{$I STCOMDEF.INC}
unit _StString;

interface

uses
  ComObj, Classes, SysTools_TLB, StdVcl;

type
  TStString = class(TAutoObject, IStString)
   private   {Private declarations }
    FTokens     : IStStringList;
    FTokensList : TStringList;
    FIsLicensed : Boolean;
   public    {Public declarations }
    procedure Initialize; override;
    destructor Destroy; override;
   protected {Protected declarations }
    { IStString - Methods }
    function AddBackSlash(const DirName: WideString): WideString; safecall;
    function AsciiCount(const S, WordDelims, Quote: WideString): Integer; safecall;
    function AsciiPosition(N: Integer; const S, WordDelims, Quote: WideString; var Pos: Integer): WordBool; safecall;
    function BinaryB(B: Byte): WideString; safecall;
    function BinaryW(W: Integer): WideString; safecall;
    function BinaryL(L: Integer): WideString; safecall;
    function Center(const S: WideString; Len: Integer): WideString; safecall;
    function CenterCh(const S, C: WideString; Len: Integer): WideString; safecall;
    function CharCount(const S, C: WideString): Integer; safecall;
    function CharExists(const S, C: WideString): WordBool; safecall;
    function CharStr(const C: WideString; Len: Integer): WideString; safecall;
    function CleanPathName(const PathName: WideString): WideString; safecall;
    function Commaize(L: Integer): WideString; safecall;
    function CommaizeCh(L: Integer; const Ch: WideString): WideString; safecall;
    function CompString(const S1, S2: WideString): Integer; safecall;
    function CompUCString(const S1, S2: WideString): Integer; safecall;
    function ContainsOnly(const S, Chars: WideString; var BadPos: Integer): WordBool; safecall;
    function ContainsOtherThan(const S, Chars: WideString; var BadPos: Integer): WordBool; safecall;
    function CopyFromNthWord(const S, WordDelims, AWord: WideString; N: Integer; var SubString: WideString): WordBool; safecall;
    function CopyFromToWord(const S, WordDelims, Word1, Word2: WideString; N1, N2: Integer; var SubString: WideString): WordBool; safecall;
    function CopyLeft(const S: WideString; Len: Integer): WideString; safecall;
    function CopyMid(const S: WideString; First, Len: Integer): WideString; safecall;
    function CopyRight(const S: WideString; First: Integer): WideString; safecall;
    function CopyWithin(const S, Delimiter: WideString; Strip: WordBool): WideString; safecall;
    function DefaultExtension(const Name, Ext: WideString): WideString; safecall;
    function DeleteFromNthWord(const S, WordDelims, AWord: WideString; N: Integer; var SubString: WideString): WordBool; safecall;
    function DeleteFromToWord(const S, WordDelims, Word1, Word2: WideString; N1, N2: Integer; var SubString: WideString): WordBool; safecall;
    function DeleteWithin(const S, Delimeter: WideString): WideString; safecall;
    function Detab(const S: WideString; TabSize: Byte): WideString; safecall;
    function Entab(const S: WideString; TabSize: Byte): WideString; safecall;
    function Ext2Str(R: OleVariant; Width, Places: Byte): WideString; safecall;
    function ExtractAscii(N: Integer; const S, WordDelims, Quote: WideString): WideString; safecall;
    function ExtractTokens(const S, Delims, QuoteChar: WideString; AllowNulls: WordBool; out Tokens: IStStringList): Integer; safecall;
    function ExtractWord(N: Integer; const S, WordDelims: WideString): WideString; safecall;
    function Filter(const S, Filters: WideString): WideString; safecall;
    function FloatForm(const Mask: WideString; R: Double; L: Integer; const LtCurr, RtCurr, Sep, DecPt: WideString): WideString; safecall;
    function ForceExtension(const Name, Ext: WideString): WideString; safecall;
    function HasExtension(const Name: WideString; var DotPos: Integer): WordBool; safecall;
    function HexB(B: Byte): WideString; safecall;
    function HexW(W: Integer): WideString; safecall;
    function HexL(L: Integer): WideString; safecall;
    function IsChAlpha(const C: WideString): WordBool; safecall;
    function IsChAlphaNumeric(const C, Numbers: WideString): WordBool; safecall;
    function IsChNumeric(const C, Numbers: WideString): WordBool; safecall;
    function IsStrAlpha(const S: WideString): WordBool; safecall;
    function IsStrAlphaNumeric(const S, Numbers: WideString): WordBool; safecall;
    function IsStrNumeric(const S, Numbers: WideString): WordBool; safecall;
    function JustExtension(const Name: WideString): WideString; safecall;
    function JustFilename(const PathName: WideString): WideString; safecall;
    function JustName(const PathName: WideString): WideString; safecall;
    function JustPathname(const PathName: WideString): WideString; safecall;
    function KeepChars(const S, Chars: WideString): WideString; safecall;
    function LastString(const S, AString: WideString; var Position: Integer): WordBool; safecall;
    function LastWord(const S, WordDelims, AWord: WideString; var Position: Integer): WordBool; safecall;
    function LastWordAbs(const S, WordDelims: WideString; var Position: Integer): WordBool; safecall;
    function LeftPad(const S: WideString; Len: Integer): WideString; safecall;
    function LeftPadCh(const S, C: WideString; Len: Integer): WideString; safecall;
    function LeftTrimChars(const S, Chars: WideString): WideString; safecall;
    function Long2Str(L: Integer): WideString; safecall;
    function LongIntForm(const Mask: WideString; L: Integer; const LtCurr, RtCurr, Sep: WideString): WideString; safecall;
    function OctalB(B: Byte): WideString; safecall;
    function OctalW(W: Integer): WideString; safecall;
    function OctalL(L: Integer): WideString; safecall;
    function Pad(const S: WideString; Len: Integer): WideString; safecall;
    function PadCh(const S, C: WideString; Len: Integer): WideString; safecall;
    function Real2Str(R: Double; Width, Places: Byte): WideString; safecall;
    function RepeatString(const S: WideString; var Repetitions: Integer; MaxLen: Integer): WideString; safecall;
    function ReplaceWord(const S, WordDelims, OldWord, NewWord: WideString; N: Integer; var Replacements: Integer): WideString; safecall;
    function ReplaceWordAll(const S, WordDelims, OldWord, NewWord: WideString; var Replacements: Integer): WideString; safecall;
    function ReplaceString(const S, OldString, NewString: WideString; N: Integer; var Replacements: Integer): WideString; safecall;
    function ReplaceStringAll(const S, OldString, NewString: WideString; var Replacements: Integer): WideString; safecall;
    function RightTrimChars(const S, Chars: WideString): WideString; safecall;
    function Scramble(const S, Key: WideString): WideString; safecall;
    function Str2Ext(const S: WideString; var R: OleVariant): WordBool; safecall;
    function Str2Int16(const S: WideString; var I: Smallint): WordBool; safecall;
    function Str2Long(const S: WideString; var I: Integer): WordBool; safecall;
    function Str2Real(const S: WideString; var R: Double): WordBool; safecall;
    function Str2Word(const S: WideString; var W: Integer): WordBool; safecall;
    function StrChDelete(const S: WideString; Pos: Integer): WideString; safecall;
    function StrChInsert(const S, C: WideString; Pos: Integer): WideString; safecall;
    function StrChPos(const P, C: WideString; var Pos: Integer): WordBool; safecall;
    function StrStCopy(const S: WideString; Pos, Count: Integer): WideString; safecall;
    function StrStDelete(const S: WideString; Pos, Count: Integer): WideString; safecall;
    function StrStInsert(const S1, S2: WideString; Pos: Integer): WideString; safecall;
    function StrStPos(const P, S: WideString; var Pos: Integer): WordBool; safecall;
    function StrWithin(const S, SearchStr: WideString; Start: Integer; var Position: Integer): WordBool; safecall;
    function Substitute(const S, FromStr, ToStr: WideString): WideString; safecall;
    function Trim(const S: WideString): WideString; safecall;
    function TrimChars(const S, Chars: WideString): WideString; safecall;
    function TrimLead(const S: WideString): WideString; safecall;
    function TrimSpaces(const S: WideString): WideString; safecall;
    function TrimTrail(const S: WideString): WideString; safecall;
    function ValPrep(const S: WideString): WideString; safecall;
    function WordCount(const S, WordDelims: WideString): Integer; safecall;
    function WordPos(const S, WordDelims, AWord: WideString; N: Integer; var Position: Integer): WordBool; safecall;
    function WordPosition(N: Integer; const S, WordDelims: WideString; var Position: Integer): WordBool; safecall;
    procedure WordWrap(const InSt: WideString; var OutSt, Overlap: WideString; Margin: Integer; PadToMArgin: WordBool); safecall;
    function License(const Key: WideString): WordBool; safecall;
    function Soundex(const S: WideString): WideString; safecall;
  end;

implementation

uses ComServ, StStrL, StStrW, _StUtil {$IFDEF LICENSE}, ActiveX, StComLic {$ENDIF};

{ ********** TStString Interface ********************************************************* }
procedure TStString.Initialize;
begin
  inherited Initialize;
  FTokensList := TStringList.Create;
  FTokens := TStStringList.Create(FTokensList);
  {$IFDEF LICENSE}
  FIsLicensed := False;
  {$ELSE}
  FIsLicensed := True;
  {$ENDIF}
end;

destructor TStString.Destroy;
begin
  if Assigned (FTokensList) then
    FTokensList.Free;

  FTokens := nil;
  inherited Destroy;
end;

{ ********** TStString Methods *********************************************************** }
function TStString.AddBackSlash(const DirName: WideString): WideString;
begin
  {$IFDEF LICENSE}
   if (not FIsLicensed) or (not COMHasBeenLicensed) then
     OleError(CLASS_E_NOTLICENSED);
  {$ENDIF}
  Result := StStrW.AddBackSlashW(DirName);
end;

function TStString.AsciiCount(const S, WordDelims,
  Quote: WideString): Integer;
begin
  {$IFDEF LICENSE}
   if (not FIsLicensed) or (not COMHasBeenLicensed) then
     OleError(CLASS_E_NOTLICENSED);
  {$ENDIF}
  Result := StStrW.AsciiCountW(S, WordDelims, Quote[1])
end;

function TStString.AsciiPosition(N: Integer; const S, WordDelims,
  Quote: WideString; var Pos: Integer): WordBool;
begin
  {$IFDEF LICENSE}
   if (not FIsLicensed) or (not COMHasBeenLicensed) then
     OleError(CLASS_E_NOTLICENSED);
  {$ENDIF}
  Result := StStrW.AsciiPositionW(Cardinal(N), S, WordDelims, Quote[1], Cardinal(Pos));
end;

function TStString.BinaryB(B: Byte): WideString;
begin
  {$IFDEF LICENSE}
   if (not FIsLicensed) or (not COMHasBeenLicensed) then
     OleError(CLASS_E_NOTLICENSED);
  {$ENDIF}
  Result := StStrW.BinaryBW(B);
end;

function TStString.BinaryW(W: Integer): WideString;
begin
  {$IFDEF LICENSE}
   if (not FIsLicensed) or (not COMHasBeenLicensed) then
     OleError(CLASS_E_NOTLICENSED);
  {$ENDIF}
  Result := StStrW.BinaryWW(W);
end;

function TStString.BinaryL(L: Integer): WideString;
begin
  {$IFDEF LICENSE}
   if (not FIsLicensed) or (not COMHasBeenLicensed) then
     OleError(CLASS_E_NOTLICENSED);
  {$ENDIF}
  Result := StStrW.BinaryLW(Cardinal(L));
end;

function TStString.Center(const S: WideString; Len: Integer): WideString;
begin
  {$IFDEF LICENSE}
   if (not FIsLicensed) or (not COMHasBeenLicensed) then
     OleError(CLASS_E_NOTLICENSED);
  {$ENDIF}
  Result := StStrW.CenterW(S, Cardinal(Len));
end;

function TStString.CenterCh(const S, C: WideString;
  Len: Integer): WideString;
begin
  {$IFDEF LICENSE}
   if (not FIsLicensed) or (not COMHasBeenLicensed) then
     OleError(CLASS_E_NOTLICENSED);
  {$ENDIF}
  Result := StStrW.CenterChW(S, C[1], Cardinal(Len));
end;

function TStString.CharCount(const S, C: WideString): Integer;
begin
  {$IFDEF LICENSE}
   if (not FIsLicensed) or (not COMHasBeenLicensed) then
     OleError(CLASS_E_NOTLICENSED);
  {$ENDIF}
  Result := StStrW.CharCountW(S, C[1]);
end;

function TStString.CharExists(const S, C: WideString): WordBool;
begin
  {$IFDEF LICENSE}
   if (not FIsLicensed) or (not COMHasBeenLicensed) then
     OleError(CLASS_E_NOTLICENSED);
  {$ENDIF}
  Result := StStrW.CharExistsW(S, C[1]);
end;

function TStString.CharStr(const C: WideString; Len: Integer): WideString;
begin
  {$IFDEF LICENSE}
   if (not FIsLicensed) or (not COMHasBeenLicensed) then
     OleError(CLASS_E_NOTLICENSED);
  {$ENDIF}
  Result := StStrW.CharStrW(C[1], Cardinal(Len));
end;

function TStString.CleanPathName(const PathName: WideString): WideString;
begin
  {$IFDEF LICENSE}
   if (not FIsLicensed) or (not COMHasBeenLicensed) then
     OleError(CLASS_E_NOTLICENSED);
  {$ENDIF}
  Result := StStrW.CleanPathNameW(PathName);
end;

function TStString.Commaize(L: Integer): WideString;
begin
  {$IFDEF LICENSE}
   if (not FIsLicensed) or (not COMHasBeenLicensed) then
     OleError(CLASS_E_NOTLICENSED);
  {$ENDIF}
  Result := StStrW.CommaizeW(LongInt(L));
end;

function TStString.CommaizeCh(L: Integer;
  const Ch: WideString): WideString;
begin
  {$IFDEF LICENSE}
   if (not FIsLicensed) or (not COMHasBeenLicensed) then
     OleError(CLASS_E_NOTLICENSED);
  {$ENDIF}
  Result := StStrW.CommaizeChW(LongInt(L), Ch[1]);
end;

function TStString.CompString(const S1, S2: WideString): Integer;
begin
  {$IFDEF LICENSE}
   if (not FIsLicensed) or (not COMHasBeenLicensed) then
     OleError(CLASS_E_NOTLICENSED);
  {$ENDIF}
  Result := StStrW.CompStringW(S1, S2);
end;

function TStString.CompUCString(const S1, S2: WideString): Integer;
begin
  {$IFDEF LICENSE}
   if (not FIsLicensed) or (not COMHasBeenLicensed) then
     OleError(CLASS_E_NOTLICENSED);
  {$ENDIF}
  Result := StStrW.CompUCStringW(S1, S2);
end;

function TStString.ContainsOnly(const S, Chars: WideString;
  var BadPos: Integer): WordBool;
begin
  {$IFDEF LICENSE}
   if (not FIsLicensed) or (not COMHasBeenLicensed) then
     OleError(CLASS_E_NOTLICENSED);
  {$ENDIF}
  Result := StStrW.ContainsOnlyW(S, Chars, Cardinal(BadPos));
end;

function TStString.ContainsOtherThan(const S, Chars: WideString;
  var BadPos: Integer): WordBool;
begin
  {$IFDEF LICENSE}
   if (not FIsLicensed) or (not COMHasBeenLicensed) then
     OleError(CLASS_E_NOTLICENSED);
  {$ENDIF}
  Result := StStrW.ContainsOtherThanW(S, Chars, Cardinal(BadPos));
end;

function TStString.CopyFromNthWord(const S, WordDelims, AWord: WideString;
  N: Integer; var SubString: WideString): WordBool;
begin
  {$IFDEF LICENSE}
   if (not FIsLicensed) or (not COMHasBeenLicensed) then
     OleError(CLASS_E_NOTLICENSED);
  {$ENDIF}
  Result := StStrW.CopyFromNthWordW(S, WordDelims, AWord, Cardinal(N), SubString);
end;

function TStString.CopyFromToWord(const S, WordDelims, Word1,
  Word2: WideString; N1, N2: Integer; var SubString: WideString): WordBool;
begin
  {$IFDEF LICENSE}
   if (not FIsLicensed) or (not COMHasBeenLicensed) then
     OleError(CLASS_E_NOTLICENSED);
  {$ENDIF}
  Result := StStrW.CopyFromToWordW(S, WordDelims, Word1, Word2, Cardinal(N1), Cardinal(N2), SubString);
end;

function TStString.CopyLeft(const S: WideString; Len: Integer): WideString;
begin
  {$IFDEF LICENSE}
   if (not FIsLicensed) or (not COMHasBeenLicensed) then
     OleError(CLASS_E_NOTLICENSED);
  {$ENDIF}
  Result := StStrW.CopyLeftW(S, Cardinal(Len));
end;

function TStString.CopyMid(const S: WideString; First,
  Len: Integer): WideString;
begin
  {$IFDEF LICENSE}
   if (not FIsLicensed) or (not COMHasBeenLicensed) then
     OleError(CLASS_E_NOTLICENSED);
  {$ENDIF}
  Result := StStrW.CopyMidW(S, Cardinal(First), Cardinal(Len));
end;

function TStString.CopyRight(const S: WideString;
  First: Integer): WideString;
begin
  {$IFDEF LICENSE}
   if (not FIsLicensed) or (not COMHasBeenLicensed) then
     OleError(CLASS_E_NOTLICENSED);
  {$ENDIF}
  Result := StStrW.CopyRightW(S, Cardinal(First));
end;

function TStString.CopyWithin(const S, Delimiter: WideString;
  Strip: WordBool): WideString;
begin
  {$IFDEF LICENSE}
   if (not FIsLicensed) or (not COMHasBeenLicensed) then
     OleError(CLASS_E_NOTLICENSED);
  {$ENDIF}
  Result := StStrW.CopyWithinW(S, Delimiter, Strip);
end;

function TStString.DefaultExtension(const Name,
  Ext: WideString): WideString;
begin
  {$IFDEF LICENSE}
   if (not FIsLicensed) or (not COMHasBeenLicensed) then
     OleError(CLASS_E_NOTLICENSED);
  {$ENDIF}
  Result := StStrW.DefaultExtensionW(Name, Ext);
end;

function TStString.DeleteFromNthWord(const S, WordDelims,
  AWord: WideString; N: Integer; var SubString: WideString): WordBool;
begin
  {$IFDEF LICENSE}
   if (not FIsLicensed) or (not COMHasBeenLicensed) then
     OleError(CLASS_E_NOTLICENSED);
  {$ENDIF}
  Result := StStrW.DeleteFromNthWordW(S, WordDelims, AWord, Cardinal(N), SubString);
end;

function TStString.DeleteFromToWord(const S, WordDelims, Word1,
  Word2: WideString; N1, N2: Integer; var SubString: WideString): WordBool;
begin
  {$IFDEF LICENSE}
   if (not FIsLicensed) or (not COMHasBeenLicensed) then
     OleError(CLASS_E_NOTLICENSED);
  {$ENDIF}
  Result := StStrW.DeleteFromToWordW(S, WordDelims, Word1, Word2, Cardinal(N1), Cardinal(N2), SubString);
end;

function TStString.DeleteWithin(const S,
  Delimeter: WideString): WideString;
begin
  {$IFDEF LICENSE}
   if (not FIsLicensed) or (not COMHasBeenLicensed) then
     OleError(CLASS_E_NOTLICENSED);
  {$ENDIF}
  Result := StStrW.DeleteWithinW(S, Delimeter);
end;

function TStString.Detab(const S: WideString; TabSize: Byte): WideString;
begin
  {$IFDEF LICENSE}
   if (not FIsLicensed) or (not COMHasBeenLicensed) then
     OleError(CLASS_E_NOTLICENSED);
  {$ENDIF}
  Result := StStrW.DetabW(S, TabSize);
end;

function TStString.Entab(const S: WideString; TabSize: Byte): WideString;
begin
  {$IFDEF LICENSE}
   if (not FIsLicensed) or (not COMHasBeenLicensed) then
     OleError(CLASS_E_NOTLICENSED);
  {$ENDIF}
  Result := StStrW.EntabW(S, TabSize);
end;

function TStString.Ext2Str(R: OleVariant; Width, Places: Byte): WideString;
var
  X : Extended;
begin
  {$IFDEF LICENSE}
   if (not FIsLicensed) or (not COMHasBeenLicensed) then
     OleError(CLASS_E_NOTLICENSED);
  {$ENDIF}
  {!! Will this work with an OleVariant instead of Extended? !!}
  X := R;
  Result := StStrW.Ext2StrW(X, Width, ShortInt(Places));
end;

function TStString.ExtractAscii(N: Integer; const S, WordDelims,
  Quote: WideString): WideString;
begin
  {$IFDEF LICENSE}
   if (not FIsLicensed) or (not COMHasBeenLicensed) then
     OleError(CLASS_E_NOTLICENSED);
  {$ENDIF}
  Result := StStrW.ExtractAsciiW(Cardinal(N), S, WordDelims, Quote[1]);
end;

function TStString.ExtractTokens(const S, Delims, QuoteChar: WideString;
  AllowNulls: WordBool; out Tokens: IStStringList): Integer;
begin
  {$IFDEF LICENSE}
   if (not FIsLicensed) or (not COMHasBeenLicensed) then
     OleError(CLASS_E_NOTLICENSED);
  {$ENDIF}
  FTokensList.Clear;
  Result := StStrW.ExtractTokensW(S, Delims, QuoteChar[1], AllowNulls, FTokensList);
  Tokens := FTokens;
end;

function TStString.ExtractWord(N: Integer; const S,
  WordDelims: WideString): WideString;
begin
  {$IFDEF LICENSE}
   if (not FIsLicensed) or (not COMHasBeenLicensed) then
     OleError(CLASS_E_NOTLICENSED);
  {$ENDIF}
  Result := StStrW.ExtractWordW(Cardinal(N), S, WordDelims);
end;

function TStString.Filter(const S, Filters: WideString): WideString;
begin
  {$IFDEF LICENSE}
   if (not FIsLicensed) or (not COMHasBeenLicensed) then
     OleError(CLASS_E_NOTLICENSED);
  {$ENDIF}
  Result := StStrW.FilterW(S, Filters);
end;

function TStString.FloatForm(const Mask: WideString; R: Double; L: Integer;
  const LtCurr, RtCurr, Sep, DecPt: WideString): WideString;
begin
  {$IFDEF LICENSE}
   if (not FIsLicensed) or (not COMHasBeenLicensed) then
     OleError(CLASS_E_NOTLICENSED);
  {$ENDIF}
  Result := StStrW.FloatFormW(Mask, R, LtCurr, RtCurr, Sep[1], DecPt[1])
end;

function TStString.ForceExtension(const Name, Ext: WideString): WideString;
begin
  {$IFDEF LICENSE}
   if (not FIsLicensed) or (not COMHasBeenLicensed) then
     OleError(CLASS_E_NOTLICENSED);
  {$ENDIF}
  Result := StStrW.ForceExtensionW(Name, Ext);
end;

function TStString.HasExtension(const Name: WideString;
  var DotPos: Integer): WordBool;
begin
  {$IFDEF LICENSE}
   if (not FIsLicensed) or (not COMHasBeenLicensed) then
     OleError(CLASS_E_NOTLICENSED);
  {$ENDIF}
  Result := StStrW.HasExtensionW(Name, Cardinal(DotPos));
end;

function TStString.HexB(B: Byte): WideString;
begin
  {$IFDEF LICENSE}
   if (not FIsLicensed) or (not COMHasBeenLicensed) then
     OleError(CLASS_E_NOTLICENSED);
  {$ENDIF}
  Result := StStrW.HexBW(B);
end;

function TStString.HexW(W: Integer): WideString;
begin
  {$IFDEF LICENSE}
   if (not FIsLicensed) or (not COMHasBeenLicensed) then
     OleError(CLASS_E_NOTLICENSED);
  {$ENDIF}
  Result := StStrW.HexWW(W);
end;

function TStString.HexL(L: Integer): WideString;
begin
  {$IFDEF LICENSE}
   if (not FIsLicensed) or (not COMHasBeenLicensed) then
     OleError(CLASS_E_NOTLICENSED);
  {$ENDIF}
  Result := StStrW.HexLW(LongInt(L));
end;

function TStString.IsChAlpha(const C: WideString): WordBool;
begin
  {$IFDEF LICENSE}
   if (not FIsLicensed) or (not COMHasBeenLicensed) then
     OleError(CLASS_E_NOTLICENSED);
  {$ENDIF}
  Result := StStrW.IsChAlphaW(C[1]);
end;

function TStString.IsChAlphaNumeric(const C,
  Numbers: WideString): WordBool;
begin
  {$IFDEF LICENSE}
   if (not FIsLicensed) or (not COMHasBeenLicensed) then
     OleError(CLASS_E_NOTLICENSED);
  {$ENDIF}
  Result := StStrW.IsChAlphaNumericW(C[1], Numbers);
end;

function TStString.IsChNumeric(const C, Numbers: WideString): WordBool;
begin
  {$IFDEF LICENSE}
   if (not FIsLicensed) or (not COMHasBeenLicensed) then
     OleError(CLASS_E_NOTLICENSED);
  {$ENDIF}
  Result := StStrW.IsChNumericW(C[1], Numbers);
end;

function TStString.IsStrAlpha(const S: WideString): WordBool;
begin
  {$IFDEF LICENSE}
   if (not FIsLicensed) or (not COMHasBeenLicensed) then
     OleError(CLASS_E_NOTLICENSED);
  {$ENDIF}
  Result := StStrW.IsStrAlphaW(S);
end;

function TStString.IsStrAlphaNumeric(const S,
  Numbers: WideString): WordBool;
begin
  {$IFDEF LICENSE}
   if (not FIsLicensed) or (not COMHasBeenLicensed) then
     OleError(CLASS_E_NOTLICENSED);
  {$ENDIF}
  Result := StStrW.IsStrAlphaNumericW(S, Numbers);
end;

function TStString.IsStrNumeric(const S, Numbers: WideString): WordBool;
begin
  {$IFDEF LICENSE}
   if (not FIsLicensed) or (not COMHasBeenLicensed) then
     OleError(CLASS_E_NOTLICENSED);
  {$ENDIF}
  Result := StStrW.IsStrNumericW(S, Numbers);
end;

function TStString.JustExtension(const Name: WideString): WideString;
begin
  {$IFDEF LICENSE}
   if (not FIsLicensed) or (not COMHasBeenLicensed) then
     OleError(CLASS_E_NOTLICENSED);
  {$ENDIF}
  Result := StStrW.JustExtensionW(Name);
end;

function TStString.JustFilename(const PathName: WideString): WideString;
begin
  {$IFDEF LICENSE}
   if (not FIsLicensed) or (not COMHasBeenLicensed) then
     OleError(CLASS_E_NOTLICENSED);
  {$ENDIF}
  Result := StStrW.JustFilenameW(PathName);
end;

function TStString.JustName(const PathName: WideString): WideString;
begin
  {$IFDEF LICENSE}
   if (not FIsLicensed) or (not COMHasBeenLicensed) then
     OleError(CLASS_E_NOTLICENSED);
  {$ENDIF}
  Result := StStrW.JustNameW(PathName);
end;

function TStString.JustPathname(const PathName: WideString): WideString;
begin
  {$IFDEF LICENSE}
   if (not FIsLicensed) or (not COMHasBeenLicensed) then
     OleError(CLASS_E_NOTLICENSED);
  {$ENDIF}
  Result := StStrW.JustPathnameW(PathName);
end;

function TStString.KeepChars(const S, Chars: WideString): WideString;
begin
  {$IFDEF LICENSE}
   if (not FIsLicensed) or (not COMHasBeenLicensed) then
     OleError(CLASS_E_NOTLICENSED);
  {$ENDIF}
  Result := StStrW.KeepCharsW(S, Chars);
end;

function TStString.LastString(const S, AString: WideString;
  var Position: Integer): WordBool;
begin
  {$IFDEF LICENSE}
   if (not FIsLicensed) or (not COMHasBeenLicensed) then
     OleError(CLASS_E_NOTLICENSED);
  {$ENDIF}
  Result := StStrW.LastStringW(S, AString, Cardinal(Position));
end;

function TStString.LastWord(const S, WordDelims, AWord: WideString;
  var Position: Integer): WordBool;
begin
  {$IFDEF LICENSE}
   if (not FIsLicensed) or (not COMHasBeenLicensed) then
     OleError(CLASS_E_NOTLICENSED);
  {$ENDIF}
  Result := StStrW.LastWordW(S, WordDelims, AWord, Cardinal(Position));
end;

function TStString.LastWordAbs(const S, WordDelims: WideString;
  var Position: Integer): WordBool;
begin
  {$IFDEF LICENSE}
   if (not FIsLicensed) or (not COMHasBeenLicensed) then
     OleError(CLASS_E_NOTLICENSED);
  {$ENDIF}
  Result := StStrW.LastWordAbsW(S, WordDelims, Cardinal(Position));
end;

function TStString.LeftPad(const S: WideString; Len: Integer): WideString;
begin
  {$IFDEF LICENSE}
   if (not FIsLicensed) or (not COMHasBeenLicensed) then
     OleError(CLASS_E_NOTLICENSED);
  {$ENDIF}
  Result := StStrW.LeftPadW(S, Cardinal(Len));
end;

function TStString.LeftPadCh(const S, C: WideString;
  Len: Integer): WideString;
begin
  {$IFDEF LICENSE}
   if (not FIsLicensed) or (not COMHasBeenLicensed) then
     OleError(CLASS_E_NOTLICENSED);
  {$ENDIF}
  Result := StStrW.LeftPadChW(S, C[1], Cardinal(Len));
end;

function TStString.LeftTrimChars(const S, Chars: WideString): WideString;
begin
  {$IFDEF LICENSE}
   if (not FIsLicensed) or (not COMHasBeenLicensed) then
     OleError(CLASS_E_NOTLICENSED);
  {$ENDIF}
  Result := StStrW.LeftTrimCharsW(S, Chars);
end;

function TStString.Long2Str(L: Integer): WideString;
begin
  {$IFDEF LICENSE}
   if (not FIsLicensed) or (not COMHasBeenLicensed) then
     OleError(CLASS_E_NOTLICENSED);
  {$ENDIF}
  Result := StStrW.Long2StrW(LongInt(L));
end;

function TStString.LongIntForm(const Mask: WideString; L: Integer;
  const LtCurr, RtCurr, Sep: WideString): WideString;
begin
  {$IFDEF LICENSE}
   if (not FIsLicensed) or (not COMHasBeenLicensed) then
     OleError(CLASS_E_NOTLICENSED);
  {$ENDIF}
  Result := StStrW.LongIntFormW(Mask, LongInt(L), LtCurr, RtCurr, Sep[1]);
end;

function TStString.OctalB(B: Byte): WideString;
begin
  {$IFDEF LICENSE}
   if (not FIsLicensed) or (not COMHasBeenLicensed) then
     OleError(CLASS_E_NOTLICENSED);
  {$ENDIF}
  Result := StStrW.OctalBW(B);
end;

function TStString.OctalW(W: Integer): WideString;
begin
  {$IFDEF LICENSE}
   if (not FIsLicensed) or (not COMHasBeenLicensed) then
     OleError(CLASS_E_NOTLICENSED);
  {$ENDIF}
  Result := StStrW.OctalWW(W);
end;

function TStString.OctalL(L: Integer): WideString;
begin
  {$IFDEF LICENSE}
   if (not FIsLicensed) or (not COMHasBeenLicensed) then
     OleError(CLASS_E_NOTLICENSED);
  {$ENDIF}
  Result := StStrW.OctalLW(LongInt(L));
end;

function TStString.Pad(const S: WideString; Len: Integer): WideString;
begin
  {$IFDEF LICENSE}
   if (not FIsLicensed) or (not COMHasBeenLicensed) then
     OleError(CLASS_E_NOTLICENSED);
  {$ENDIF}
  Result := StStrW.PadW(S, Cardinal(Len));
end;

function TStString.PadCh(const S, C: WideString; Len: Integer): WideString;
begin
  {$IFDEF LICENSE}
   if (not FIsLicensed) or (not COMHasBeenLicensed) then
     OleError(CLASS_E_NOTLICENSED);
  {$ENDIF}
  Result := StStrW.PadChW(S, C[1], Cardinal(Len));
end;

function TStString.Real2Str(R: Double; Width, Places: Byte): WideString;
begin
  {$IFDEF LICENSE}
   if (not FIsLicensed) or (not COMHasBeenLicensed) then
     OleError(CLASS_E_NOTLICENSED);
  {$ENDIF}
  Result := StStrW.Real2StrW(R, Width, ShortInt(Places));
end;

function TStString.RepeatString(const S: WideString;
  var Repetitions: Integer; MaxLen: Integer): WideString;
begin
  {$IFDEF LICENSE}
   if (not FIsLicensed) or (not COMHasBeenLicensed) then
     OleError(CLASS_E_NOTLICENSED);
  {$ENDIF}
  Result := StStrW.RepeatStringW(S, Cardinal(Repetitions), Cardinal(MaxLen));
end;

function TStString.ReplaceWord(const S, WordDelims, OldWord,
  NewWord: WideString; N: Integer; var Replacements: Integer): WideString;
begin
  {$IFDEF LICENSE}
   if (not FIsLicensed) or (not COMHasBeenLicensed) then
     OleError(CLASS_E_NOTLICENSED);
  {$ENDIF}
  Result := StStrW.ReplaceWordW(S, WordDelims, OldWord, NewWord, Cardinal(N), Cardinal(Replacements));
end;

function TStString.ReplaceWordAll(const S, WordDelims, OldWord,
  NewWord: WideString; var Replacements: Integer): WideString;
begin
  {$IFDEF LICENSE}
   if (not FIsLicensed) or (not COMHasBeenLicensed) then
     OleError(CLASS_E_NOTLICENSED);
  {$ENDIF}
  Result := StStrW.ReplaceWordAllW(S, WordDelims, OldWord, NewWord, Cardinal(Replacements));
end;

function TStString.ReplaceString(const S, OldString, NewString: WideString;
  N: Integer; var Replacements: Integer): WideString;
begin
  {$IFDEF LICENSE}
   if (not FIsLicensed) or (not COMHasBeenLicensed) then
     OleError(CLASS_E_NOTLICENSED);
  {$ENDIF}
  Result := StStrW.ReplaceStringW(S, OldString, NewString, Cardinal(N), Cardinal(Replacements));
end;

function TStString.ReplaceStringAll(const S, OldString,
  NewString: WideString; var Replacements: Integer): WideString;
begin
  {$IFDEF LICENSE}
   if (not FIsLicensed) or (not COMHasBeenLicensed) then
     OleError(CLASS_E_NOTLICENSED);
  {$ENDIF}
  Result := StStrW.ReplaceStringAllW(S, OldString, NewString, Cardinal(Replacements));
end;

function TStString.RightTrimChars(const S, Chars: WideString): WideString;
begin
  {$IFDEF LICENSE}
   if (not FIsLicensed) or (not COMHasBeenLicensed) then
     OleError(CLASS_E_NOTLICENSED);
  {$ENDIF}
  Result := StStrW.RightTrimCharsW(S, Chars);
end;

function TStString.Scramble(const S, Key: WideString): WideString;
begin
  {$IFDEF LICENSE}
   if (not FIsLicensed) or (not COMHasBeenLicensed) then
     OleError(CLASS_E_NOTLICENSED);
  {$ENDIF}
  Result := StStrW.ScrambleW(S, Key);
end;

function TStString.Str2Ext(const S: WideString;
  var R: OleVariant): WordBool;
var
  R1 : Extended;
begin
  {$IFDEF LICENSE}
   if (not FIsLicensed) or (not COMHasBeenLicensed) then
     OleError(CLASS_E_NOTLICENSED);
  {$ENDIF}
  R1 := R;
  Result := StStrW.Str2ExtW(S, R1);
  R := R1;
end;

function TStString.Str2Int16(const S: WideString;
  var I: Smallint): WordBool;
begin
  {$IFDEF LICENSE}
   if (not FIsLicensed) or (not COMHasBeenLicensed) then
     OleError(CLASS_E_NOTLICENSED);
  {$ENDIF}
  Result := StStrW.Str2Int16W(S, I);
end;

function TStString.Str2Long(const S: WideString; var I: Integer): WordBool;
begin
  {$IFDEF LICENSE}
   if (not FIsLicensed) or (not COMHasBeenLicensed) then
     OleError(CLASS_E_NOTLICENSED);
  {$ENDIF}
  Result := StStrW.Str2LongW(S, LongInt(I));
end;

function TStString.Str2Real(const S: WideString; var R: Double): WordBool;
begin
  {$IFDEF LICENSE}
   if (not FIsLicensed) or (not COMHasBeenLicensed) then
     OleError(CLASS_E_NOTLICENSED);
  {$ENDIF}
  Result := StStrW.Str2RealW(S, R);
end;

function TStString.Str2Word(const S: WideString; var W: Integer): WordBool;
var
  W1 : Word;
begin
  {$IFDEF LICENSE}
   if (not FIsLicensed) or (not COMHasBeenLicensed) then
     OleError(CLASS_E_NOTLICENSED);
  {$ENDIF}
  Result := StStrW.Str2WordW(S, W1);
  W := W1;
end;

function TStString.StrChDelete(const S: WideString;
  Pos: Integer): WideString;
begin
  {$IFDEF LICENSE}
   if (not FIsLicensed) or (not COMHasBeenLicensed) then
     OleError(CLASS_E_NOTLICENSED);
  {$ENDIF}
  Result := StStrW.StrChDeleteW(S, Cardinal(Pos));
end;

function TStString.StrChInsert(const S, C: WideString;
  Pos: Integer): WideString;
begin
  {$IFDEF LICENSE}
   if (not FIsLicensed) or (not COMHasBeenLicensed) then
     OleError(CLASS_E_NOTLICENSED);
  {$ENDIF}
  Result := StStrW.StrChInsertW(S, C[1], Cardinal(Pos));
end;

function TStString.StrChPos(const P, C: WideString;
  var Pos: Integer): WordBool;
begin
  {$IFDEF LICENSE}
   if (not FIsLicensed) or (not COMHasBeenLicensed) then
     OleError(CLASS_E_NOTLICENSED);
  {$ENDIF}
  Result := StStrW.StrChPosW(P, C[1], Cardinal(Pos));
end;

function TStString.StrStCopy(const S: WideString; Pos,
  Count: Integer): WideString;
begin
  {$IFDEF LICENSE}
   if (not FIsLicensed) or (not COMHasBeenLicensed) then
     OleError(CLASS_E_NOTLICENSED);
  {$ENDIF}
  Result := StStrW.StrStCopyW(S, Cardinal(Pos), Cardinal(Count));
end;

function TStString.StrStDelete(const S: WideString; Pos,
  Count: Integer): WideString;
begin
  {$IFDEF LICENSE}
   if (not FIsLicensed) or (not COMHasBeenLicensed) then
     OleError(CLASS_E_NOTLICENSED);
  {$ENDIF}
  Result := StStrW.StrStDeleteW(S, Cardinal(Pos), Cardinal(Count));
end;

function TStString.StrStInsert(const S1, S2: WideString;
  Pos: Integer): WideString;
begin
  {$IFDEF LICENSE}
   if (not FIsLicensed) or (not COMHasBeenLicensed) then
     OleError(CLASS_E_NOTLICENSED);
  {$ENDIF}
  Result := StStrW.StrStInsertW(S1, S2, Cardinal(Pos));
end;

function TStString.StrStPos(const P, S: WideString;
  var Pos: Integer): WordBool;
begin
  {$IFDEF LICENSE}
   if (not FIsLicensed) or (not COMHasBeenLicensed) then
     OleError(CLASS_E_NOTLICENSED);
  {$ENDIF}
  Result := StStrW.StrStPosW(P, S, Cardinal(Pos));
end;

function TStString.StrWithin(const S, SearchStr: WideString;
  Start: Integer; var Position: Integer): WordBool;
begin
  {$IFDEF LICENSE}
   if (not FIsLicensed) or (not COMHasBeenLicensed) then
     OleError(CLASS_E_NOTLICENSED);
  {$ENDIF}
  Result := StStrW.StrWithinW(S, SearchStr, Cardinal(Start), Cardinal(Position));
end;

function TStString.Substitute(const S, FromStr,
  ToStr: WideString): WideString;
begin
  {$IFDEF LICENSE}
   if (not FIsLicensed) or (not COMHasBeenLicensed) then
     OleError(CLASS_E_NOTLICENSED);
  {$ENDIF}
  Result := StStrW.SubstituteW(S, FromStr, ToStr);
end;

function TStString.Trim(const S: WideString): WideString;
begin
  {$IFDEF LICENSE}
   if (not FIsLicensed) or (not COMHasBeenLicensed) then
     OleError(CLASS_E_NOTLICENSED);
  {$ENDIF}
  Result := StStrW.TrimW(S);
end;

function TStString.TrimChars(const S, Chars: WideString): WideString;
begin
  {$IFDEF LICENSE}
   if (not FIsLicensed) or (not COMHasBeenLicensed) then
     OleError(CLASS_E_NOTLICENSED);
  {$ENDIF}
  Result := StStrW.TrimCharsW(S, Chars);
end;

function TStString.TrimLead(const S: WideString): WideString;
begin
  {$IFDEF LICENSE}
   if (not FIsLicensed) or (not COMHasBeenLicensed) then
     OleError(CLASS_E_NOTLICENSED);
  {$ENDIF}
  Result := StStrW.TrimLeadW(S);
end;

function TStString.TrimSpaces(const S: WideString): WideString;
begin
  {$IFDEF LICENSE}
   if (not FIsLicensed) or (not COMHasBeenLicensed) then
     OleError(CLASS_E_NOTLICENSED);
  {$ENDIF}
  Result := StStrW.TrimSpacesW(S);
end;

function TStString.TrimTrail(const S: WideString): WideString;
begin
  {$IFDEF LICENSE}
   if (not FIsLicensed) or (not COMHasBeenLicensed) then
     OleError(CLASS_E_NOTLICENSED);
  {$ENDIF}
  Result := StStrW.TrimTrailW(S);
end;

function TStString.ValPrep(const S: WideString): WideString;
begin
  {$IFDEF LICENSE}
   if (not FIsLicensed) or (not COMHasBeenLicensed) then
     OleError(CLASS_E_NOTLICENSED);
  {$ENDIF}
  Result := StStrW.ValPrepW(S);
end;

function TStString.WordCount(const S, WordDelims: WideString): Integer;
begin
  {$IFDEF LICENSE}
   if (not FIsLicensed) or (not COMHasBeenLicensed) then
     OleError(CLASS_E_NOTLICENSED);
  {$ENDIF}
  Result := StStrW.WordCountW(S, WordDelims);
end;

function TStString.WordPos(const S, WordDelims, AWord: WideString;
  N: Integer; var Position: Integer): WordBool;
begin
  {$IFDEF LICENSE}
   if (not FIsLicensed) or (not COMHasBeenLicensed) then
     OleError(CLASS_E_NOTLICENSED);
  {$ENDIF}
  Result := StStrW.WordPosW(S, WordDelims, AWord, Cardinal(N), Cardinal(Position));
end;

function TStString.WordPosition(N: Integer; const S,
  WordDelims: WideString; var Position: Integer): WordBool;
begin
  {$IFDEF LICENSE}
   if (not FIsLicensed) or (not COMHasBeenLicensed) then
     OleError(CLASS_E_NOTLICENSED);
  {$ENDIF}
  Result := StStrW.WordPositionW(Cardinal(N), S, WordDelims, Cardinal(Position));
end;

procedure TStString.WordWrap(const InSt: WideString; var OutSt,
  Overlap: WideString; Margin: Integer; PadToMArgin: WordBool);
begin
  {$IFDEF LICENSE}
   if (not FIsLicensed) or (not COMHasBeenLicensed) then
     OleError(CLASS_E_NOTLICENSED);
  {$ENDIF}
  StStrW.WordWrapW(InSt, OutSt, Overlap, Cardinal(Margin), PadToMargin);
end;

function TStString.License(const Key: WideString): WordBool;
begin
  {$IFDEF LICENSE}
  Result := COMIsValidKey(Key);

  { License the objects used in this class }
  FTokens.License(Key);

  {$ELSE}
  Result := True;
  {$ENDIF}
  FIsLicensed := Result;
end;

function TStString.Soundex(const S: WideString): WideString;
begin
  {$IFDEF LICENSE}
   if (not FIsLicensed) or (not COMHasBeenLicensed) then
     OleError(CLASS_E_NOTLICENSED);
  {$ENDIF}
  Result := StStrL.SoundexL(S);
end;

initialization
  TAutoObjectFactory.Create(ComServer, TStString, Class_StString, ciMultiInstance, tmBoth);
end.
