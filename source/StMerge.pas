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
{* SysTools: StMerge.pas 4.04                            *}
{*********************************************************}
{* SysTools: "Mail Merge" functionality                  *}
{*********************************************************}

{$include StDefine.inc}

unit StMerge;

interface

uses
  Windows, SysUtils, Classes;

const
  StDefaultTagStart = '<';
  StDefaultTagEnd   = '>';
  StDefaultEscapeChar = '\';

type
  TStGotMergeTagEvent = procedure (Sender : TObject; Tag : AnsiString;
    var Value : AnsiString; var Discard : Boolean) of object;

  TStMergeProgressEvent = procedure (Sender : TObject; Index : Integer; var Abort : Boolean);

  TStTextMerge = class
  private
    FBadTag: AnsiString;
    FDefaultTags: TStrings;
    FEscapeChar: AnsiChar;
    FMergedText : TStrings;
    FMergeTags: TStrings;
    FTagEnd: AnsiString;
    FTagStart: AnsiString;
    FTemplate : TStrings;
    FOnMergeStart: TNotifyEvent;
    FOnMergeDone: TNotifyEvent;
    FOnLineStart: TStMergeProgressEvent;
    FOnLineDone: TStMergeProgressEvent;
    FOnGotMergeTag: TStGotMergeTagEvent;
    FOnGotUnknownTag: TStGotMergeTagEvent;
  protected {private}
    procedure DoGotUnknownTag(Tag: AnsiString; var Value: AnsiString;
      var Discard: Boolean);
    procedure DoGotMergeTag(Tag : AnsiString; var Value : AnsiString;
      var Discard : Boolean);
    procedure SetEscapeChar(const Value: AnsiChar);
    procedure SetTagEnd(const Value: AnsiString);
    procedure SetTagStart(const Value: AnsiString);
  public
    constructor Create;
    destructor Destroy; override;

    { Access and Update Methods }
    procedure Merge;

    { Persistence and streaming methods }
    {template }
    procedure LoadTemplateFromFile(const AFile : TFileName);
    procedure LoadTemplateFromStream(AStream : TStream);
    procedure SaveTemplateToFile(const AFile : TFileName);
    procedure SaveTemplateToStream(AStream : TStream);
    { merge result text }
    procedure SaveMergeToFile(const AFile : TFileName);
    procedure SaveMergeToStream(AStream : TStream);

    { properties }
    property BadTag : AnsiString
      read FBadTag write FBadTag;
    property DefaultTags : TStrings
      read FDefaultTags;
    property EscapeChar : AnsiChar
      read FEscapeChar write SetEscapeChar;
    property MergedText : TStrings
      read FMergedText;
    property MergeTags : TStrings
      read FMergeTags;
    property TagEnd : AnsiString
      read FTagEnd write SetTagEnd;
    property TagStart : AnsiString
      read FTagStart write SetTagStart;
    property Template : TStrings
      read FTemplate;

    { events }
    property OnGotMergeTag : TStGotMergeTagEvent
      read FOnGotMergeTag write FOnGotMergeTag;
    property OnGotUnknownTag : TStGotMergeTagEvent
      read FOnGotUnknownTag write FOnGotUnknownTag;
    property OnLineDone : TStMergeProgressEvent
      read FOnLineDone write FOnLineDone;
    property OnLineStart : TStMergeProgressEvent
      read FOnLineStart write FOnLineStart;
    property OnMergeDone : TNotifyEvent
      read FOnMergeDone write FOnMergeDone;
    property OnMergeStart : TNotifyEvent
      read FOnMergeStart write FOnMergeStart;
  end;

implementation

uses
  AnsiStrings;

{ TStTextMerge }

constructor TStTextMerge.Create;
begin

  inherited Create;
  FDefaultTags := TStringList.Create;
  FMergeTags   := TStringList.Create;
  FMergedText  := TStringList.Create;
  FTemplate    := TStringList.Create;

  FTagEnd      := StDefaultTagEnd;
  FTagStart    := StDefaultTagStart;
  FEscapeChar  := StDefaultEscapeChar;
  FBadTag      := '';
end;

destructor TStTextMerge.Destroy;
begin
  FDefaultTags.Free;
  FMergeTags.Free;
  FMergedText.Free;
  FTemplate.Free;
  inherited Destroy;
end;

procedure TStTextMerge.DoGotMergeTag(Tag : AnsiString;
  var Value : AnsiString; var Discard : Boolean);
begin
  if Assigned(FOnGotMergeTag) then
    FOnGotMergeTag(self, Tag, Value, Discard);
end;

procedure TStTextMerge.DoGotUnknownTag(Tag : AnsiString;
  var Value : AnsiString; var Discard : Boolean);
begin
  if Assigned(FOnGotUnknownTag) then
    FOnGotUnknownTag(self, Tag, Value, Discard)
  else
    Value := FBadTag;
end;

procedure TStTextMerge.LoadTemplateFromFile(const AFile: TFileName);
var
  FS : TFileStream;
begin
  FS := TFileStream.Create(AFile, fmOpenRead or fmShareDenyNone);
  try
    LoadTemplateFromStream(FS);
  finally
    FS.Free;
  end;
end;

procedure TStTextMerge.LoadTemplateFromStream(AStream: TStream);
begin
  FTemplate.Clear;
  FTemplate.LoadFromStream(AStream);
end;

procedure TStTextMerge.Merge;
{ merge template with current DataTags }
const
  TagIDChars = ['A'..'Z', 'a'..'z', '0'..'9', '_'];

  function MatchDelim(Delim : AnsiString; var PC : PAnsiChar) : Boolean;
  { see if current sequence matches specified Tag delimiter }
  var
    Match : PAnsiChar;
    Len : Integer;
  begin

    { compare text starting at PC with Tag delimiter }
    Len := Length(Delim);
    GetMem(Match, Len + 1);
    FillChar(Match^, Len + 1, #0);
    AnsiStrings.StrLCopy(Match, PC, Len);

    Result := AnsiStrings.StrPas(Match) = Delim;
    if Result then
      Inc(PC, Len);  {advance past Tag delimiter }

    FreeMem(Match, Len + 1);
  end;

  function GetTag(const Tag: AnsiString; var Discard : Boolean) : AnsiString;
  var
    IdxMerge, IdxDef : Integer;
    TagID : string;
  begin
    { extract TagID from delimiters }
    TagID := Copy(string(Tag), Length(TagStart) + 1, Length(Tag));
    TagID := Copy(TagID, 1, Length(TagID) - Length(TagEnd));

    { see if it matches Tag in MergeTags or DefaultTags }
    IdxMerge := FMergeTags.IndexOfName(TagID);
    IdxDef   := FDefaultTags.IndexOfName(TagID);

    { fire events as needed }
    if (IdxMerge < 0) and (IdxDef < 0) then begin { no match }
      DoGotUnknownTag(AnsiString(TagID), Result, Discard)
    end
    else begin  { found match }
      if (IdxMerge > -1) then begin { match in MergeTags }
        Result := AnsiString(FMergeTags.Values[TagID]);
        DoGotMergeTag(AnsiString(TagID), Result, Discard);
      end
      else { not in MergTags, use Default }
      if (IdxDef > -1) then begin
        Result := AnsiString(FDefaultTags.Values[TagID]);
        DoGotMergeTag(AnsiString(TagID), Result, Discard);
      end;
    end;
  end;

  procedure ReplaceTags(Idx : Integer);
  type
    TagSearchStates = (fsCollectingText, fsCollectingTagID);
  var
    i, Len : Integer;
    P, Cur : PAnsiChar;
    Buff, NewBuff, TagBuff, DataBuff, TextBuff : AnsiString;
    State : TagSearchStates;
    FS, FE, Prev : AnsiChar;
    {Escaped,} Discard : Boolean;
  begin
    { copy current template line }
    Buff := AnsiString(FTemplate[Idx]);
    Len := Length(Buff);

    { output line starts empty }
    NewBuff := '';
    TagBuff := '';
    TextBuff := '';

    { starts of delimiter strings }
    FS := FTagStart[1];
    FE := FTagEnd[1];
    Prev := ' ';

    { point at start of current line }
    P := PAnsiChar(Buff);
    Cur := P;

    { start looking for Tags }
    State := fsCollectingText;
    for i := 1 to Len do begin
      case State of
        { accumulating non-Tag text }
        fsCollectingText: begin
          { matching the start of a Tag? }
          if (Cur^ = FS) and (Prev <> EscapeChar) and
            MatchDelim(FTagStart, Cur) then
          begin
            { dump what we've got }
            NewBuff := NewBuff + TextBuff;
            TextBuff := '';

            { start accumulating a TagID }
            TagBuff := TagStart;
            State := fsCollectingTagID;
          end

          else
          if (Cur^ = FS) and (Prev = EscapeChar) and
            MatchDelim(FTagStart, Cur) then
          begin
            { overwrite escape character }
            TextBuff[Length(TextBuff)] := Cur^;

            { go to next character }
            Prev := Cur^;
            Inc(Cur);
          end

          else
          { accumulate text }
          begin
            TextBuff := TextBuff + Cur^;

            { go to next character }
            Prev := Cur^;
            Inc(Cur);
          end;
        end;

        { accumulating a possible Tag }
        fsCollectingTagID: begin
          { matching the end of a Tag? }
          if (Cur^ = FE) and (Prev <> EscapeChar) and
            MatchDelim(FTagEnd, Cur) then
          begin
            { insert Tag value in place of TagID }
            TagBuff := TagBuff + TagEnd;
            DataBuff := GetTag(TagBuff, Discard);
            if not Discard then
              NewBuff := NewBuff + DataBuff;

            { switch back to accumulating non-Tag text }
            State := fsCollectingText;
          end

          else
          { accumulate TagID }
          if (Cur^ in TagIDChars) then begin
            TagBuff := TagBuff + Cur^;
            { go to next character }
            Prev := Cur^;
            Inc(Cur);
          end

          else
          { doesn't look like a TagID; pass it back to text collection logic }
          begin
            { turn the "failed Tag" into regular accumulated text }
            TextBuff := TagBuff + Cur^;
            TagBuff := '';

            { go to next character }
            Prev := Cur^;
            Inc(Cur);

            { switch back to accumulating non-Tag text }
            State := fsCollectingText;
          end;

        end;
      end; {case State}

    end; {for}

    { append anything remaining }
    if State = fsCollectingText then
      NewBuff := NewBuff + TextBuff
    else
      NewBuff := NewBuff + TagBuff;

    { update merge text with current line }
    FMergedText.Add(string(NewBuff));
  end;

var
  i : Integer;
  Abort : Boolean;

begin
  { notify start of merge }
  if Assigned(FOnMergeStart) then
    FOnMergeStart(self);

  FMergedText.Clear;

  Abort := False;
  { iterate Template }
  for i := 0 to Pred(FTemplate.Count) do begin
    if Assigned(FOnLineStart) then
      FOnLineStart(self, i, Abort);
      
    if Abort then Break;

    ReplaceTags(i);

    if Assigned(FOnLineDone) then
      FOnLineDone(self, i, Abort);

    if Abort then Break;
  end; {for}

  { notify end of merge }
  if Assigned(FOnMergeDone) then
    FOnMergeDone(self);
end;

procedure TStTextMerge.SaveMergeToFile(const AFile: TFileName);
var
  FS : TFileStream;
begin
  FS := TFileStream.Create(AFile, fmCreate);
  try
    SaveMergeToStream(FS);
  finally
    FS.Free;
  end;
end;

procedure TStTextMerge.SaveMergeToStream(AStream: TStream);
begin
  FMergedText.SaveToStream(AStream);
end;

procedure TStTextMerge.SaveTemplateToFile(const AFile: TFileName);
var
  FS : TFileStream;
begin
  FS := TFileStream.Create(AFile, fmCreate);
  try
    SaveTemplateToStream(FS);
  finally
    FS.Free;
  end;
end;

procedure TStTextMerge.SaveTemplateToStream(AStream: TStream);
begin
  FTemplate.SaveToStream(AStream);
end;

procedure TStTextMerge.SetEscapeChar(const Value: AnsiChar);
begin
  FEscapeChar := Value;
end;

procedure TStTextMerge.SetTagEnd(const Value: AnsiString);
begin
  FTagEnd := Value;
end;

procedure TStTextMerge.SetTagStart(const Value: AnsiString);
begin
  FTagStart := Value;
end;

end.
