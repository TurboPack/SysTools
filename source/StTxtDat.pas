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
{* SysTools: StTxtDat.pas 4.04                           *}
{*********************************************************}
{* SysTools: Formatted Text Data Handling                *}
{*********************************************************}

{$include StDefine.inc}

unit StTxtDat;

interface
uses
  SysUtils, Classes, TypInfo, StConst, StBase, StStrms, StStrL;

const
  StDefaultDelim = ',';
  StDefaultQuote = '"';
  StDefaultComment = ';';
  StDefaultFixedSep = ' ';                                               {!!.01}
  StDefaultLineTerm = #13#10;
  St_WhiteSpace = #8#9#10#13' ';  {page feed, tab, LF, CR, space}        {!!.01}

type
  TStSchemaLayoutType = (ltUnknown, ltFixed, ltVarying);
  TStSchemaFieldType = (sftUnknown, sftChar, sftFloat, sftNumber, sftBool, sftLongInt, sftDate, sftTime, sftTimeStamp);
  TStOnQuoteFieldEvent = procedure (Sender : TObject; var Field : String) of object;

  { Text Data Layout descriptors (Schemas)}
  TStDataField = class
  protected {private}
    FFieldDecimals: Integer;
    FFieldLen: Integer;
    FFieldName: String;
    FFieldOffset: Integer;
    FFieldType: TStSchemaFieldType;
    function GetAsString: String;
    procedure SetFieldDecimals(const Value: Integer);
    procedure SetFieldLen(const Value: Integer);
    procedure SetFieldName(const Value: String);
    procedure SetFieldOffset(const Value: Integer);
    procedure SetFieldType(const Value: TStSchemaFieldType);
  public
    { properties }
    property AsString : String read GetAsString;
    property FieldDecimals: Integer read FFieldDecimals write SetFieldDecimals;
    property FieldLen: Integer read FFieldLen write SetFieldLen;
    property FieldName : String read FFieldName write SetFieldName;
    property FieldOffset: Integer read FFieldOffset write SetFieldOffset;
    property FieldType: TStSchemaFieldType read FFieldType write SetFieldType;
  end;


  TStDataFieldList = class
  private
    FList : TStringList;
  protected {private}
    function GetCount: Integer;
    function GetField(Index: Integer): TStDataField;
    function GetFieldByName(const FieldName: String): TStDataField;
    procedure SetField(Index: Integer; const Value: TStDataField);
    procedure SetFieldByName(const FieldName: String;
      const Value: TStDataField);
  public
    constructor Create;
    destructor Destroy; override;

    { Access and Update Methods }
    procedure AddField(const FieldName: String; FieldType: TStSchemaFieldType;
      FieldLen, FieldDecimals, FieldOffset: Integer);
    procedure AddFieldStr(const FieldDef : String);
    procedure Clear;
    procedure RemoveField(const FieldName: String);

    { properties }
    property Count : Integer read GetCount;
    property Fields[Index : Integer] : TStDataField
      read GetField write SetField; default;
    property FieldByName[const FieldName: String] : TStDataField
      read GetFieldByName write SetFieldByName;
  end;

  TStTextDataSchema = class
  private
    FCommentDelimiter: Char;
    FFieldDelimiter: Char;
    FLayoutType: TStSchemaLayoutType;
    FLineTermChar : Char;
    FLineTerminator : TStLineTerminator;
    FQuoteDelimiter: Char;
    FFixedSeparator : Char;                                      {!!.01}
    FSchema: TStrings;
    FSchemaName: String;
    dsFieldList : TStDataFieldList;
  protected {private}
    function GetCaptions: TStrings;
    function GetField(Index: Integer): TStDataField;
    function GetFieldByName(const FieldName: String): TStDataField;
    function GetFieldCount: Integer;
    function GetSchema: TStrings;
    procedure SetCommentDelimiter(const Value: Char);
    procedure SetField(Index: Integer; const Value: TStDataField);
    procedure SetFieldByName(const FieldName: String; const Value: TStDataField);
    procedure SetFieldDelimiter(const Value: Char);
    procedure SetLayoutType(const Value: TStSchemaLayoutType);
    procedure SetQuoteDelimiter(const Value: Char);
    procedure SetFixedSeparator(const Value: Char);              {!!.01}
    procedure SetSchema(const Value: TStrings);
    procedure SetSchemaName(const Value: String);
  public
    constructor Create;
    destructor Destroy; override;
    procedure Assign(ASchema : TStTextDataSchema);

    { Access and Update Methods }
    procedure AddField(const FieldName : String; FieldType : TStSchemaFieldType;
      FieldLen, FieldDecimals : Integer);
    function IndexOf(const FieldName : String) : Integer;
    procedure RemoveField(const FieldName: String);
    procedure Update(AList : TStrings);                               {!!.01}
    procedure ClearFields;                                            {!!.01}
    procedure BuildSchema(AList: TStrings);                           {!!.01}

    { Persistence and streaming methods }
    procedure LoadFromFile(const AFileName : TFileName);
    procedure LoadFromStream(AStream : TStream);
    procedure SaveToFile(const AFileName : TFileName);
    procedure SaveToStream(AStream : TStream);

    { properties }
    property Captions : TStrings
      read GetCaptions;
    property CommentDelimiter : Char
      read FCommentDelimiter write SetCommentDelimiter default StDefaultComment;
    property FieldByName[const FieldName: String] : TStDataField
      read GetFieldByName write SetFieldByName;
    property FieldCount : Integer
      read GetFieldCount;
    property FieldDelimiter : Char
      read FFieldDelimiter write SetFieldDelimiter default StDefaultDelim;
    property Fields[Index : Integer] : TStDataField
      read GetField write SetField; default;
    property LayoutType : TStSchemaLayoutType
      read FLayoutType write SetLayoutType;
    property LineTermChar : Char
      read FLineTermChar write FLineTermChar default #0;
    property LineTerminator : TStLineTerminator
      read FLineTerminator write FLineTerminator default ltCRLF;
    property QuoteDelimiter : Char
      read FQuoteDelimiter write SetQuoteDelimiter default StDefaultQuote;
    property FixedSeparator : Char                                        {!!.01}
      read FFixedSeparator write SetFixedSeparator default StDefaultFixedSep; {!!.01}
    property Schema : TStrings
      read GetSchema write SetSchema;
    property SchemaName : String
      read FSchemaName write SetSchemaName;
  end;

  { Text Data Records and Data Sets }
  TStTextDataRecord = class
  private
    FFieldList: TStrings;
    FQuoteAlways: Boolean;
    FQuoteIfSpaces: Boolean;
    FSchema: TStTextDataSchema;
    FValue : String;
    FOnQuoteField : TStOnQuoteFieldEvent;
  protected {private}
    function GetField(Index: Integer): String;
    function GetFieldCount: Integer;
    function GetFieldByName(const FieldName: String): String;
    function GetFieldList: TStrings;
    function GetValues: TStrings;
    procedure SetField(Index: Integer; const NewValue: String);
    procedure SetFieldByName(const FieldName: String; const NewValue: String);
    procedure SetQuoteAlways(const Value: Boolean);
    procedure SetQuoteIfSpaces(const Value: Boolean);
    procedure SetSchema(const Value: TStTextDataSchema);
  public
    constructor Create;
    destructor Destroy; override;

    { Access and Update Methods }
    procedure BuildRecord(Values: TStrings; var NewRecord: String); virtual;
    function GetRecord : String;                                   {!!.02}
    procedure DoQuote(var Value: String); virtual;
    procedure FillRecordFromArray(Values: array of const);
    procedure FillRecordFromList(Items: TStrings);
    procedure FillRecordFromValues(Values: TStrings);
    procedure MakeEmpty; virtual;

    { properties }
    property AsString : String                                     {!!.02}
//      read FValue {write SetValue};                                  {!!.02}
      read GetRecord;
    property FieldByName[const FieldName : String] : String
      read GetFieldByName write SetFieldByName;
    property FieldCount : Integer
      read GetFieldCount;
    property FieldList : TStrings
      read GetFieldList;
    property Fields[Index : Integer] : String
      read GetField write SetField;
    property QuoteAlways : Boolean
      read FQuoteAlways write SetQuoteAlways default False;
    property QuoteIfSpaces : Boolean
      read FQuoteIfSpaces write SetQuoteIfSpaces default False;
    property Schema : TStTextDataSchema
      read FSchema write SetSchema;
    property Values : TStrings
      read GetValues;

    { events }
    property OnQuoteField : TStOnQuoteFieldEvent
      read FOnQuoteField write FOnQuoteField;
  end;

  TStTextDataRecordSet = class
  private
    FActive: Boolean;
    FCurrentIndex : Integer;
    FIsDirty: Boolean;
    FRecords: TList;
    FSchema: TStTextDataSchema;
    FAtEndOfFile : Boolean;                                  {!!.01}
    FIgnoreStartingLines : Integer;                                    {!!.02}
  protected {private}
    function GetCount: Integer;
    function GetCurrentRecord: TStTextDataRecord;
    function GetRecord(Index: Integer): TStTextDataRecord;
    function GetSchema: TStTextDataSchema;
    procedure SetActive(const Value: Boolean);
    procedure SetCurrentRecord(const Value: TStTextDataRecord);
    procedure SetRecord(Index: Integer; const Value: TStTextDataRecord);
    procedure SetSchema(const Value: TStTextDataSchema);

  public
    constructor Create;
    destructor Destroy; override;

    { Access and Update Methods }
    procedure Append;
    procedure AppendArray(Values : array of const);
    procedure AppendList(Items : TStrings);
    procedure AppendValues(Values : TStrings);
    procedure Clear;
    procedure Delete;
    procedure Insert(Index : Integer);
    procedure InsertArray(Index: Integer; Values : array of const);
    procedure InsertList(Index : Integer; Items : TStrings);
    procedure InsertValues(Index : Integer; Values : TStrings);

    { navigation methods }
    function BOF : Boolean;
    function EOF : Boolean;
    procedure First;
    procedure Last;
    function Next : Boolean;
    function Prior : Boolean;

    { Persistence and streaming methods }
    procedure LoadFromFile(const AFile : TFileName);
    procedure LoadFromStream(AStream : TStream);
    procedure SaveToFile(const AFile : TFileName);
    procedure SaveToStream(AStream : TStream);

    { properties }
    property Active : Boolean
      read FActive write SetActive;
    property Count : Integer
      read GetCount;
    property CurrentRecord : TStTextDataRecord
      read GetCurrentRecord write SetCurrentRecord;
    property IsDirty : Boolean
      read FIsDirty;
    property Records[Index : Integer] : TStTextDataRecord
      read GetRecord write SetRecord;
    property Schema : TStTextDataSchema
      read GetSchema write SetSchema;
    property IgnoreStartingLines : Integer                             {!!.02}
      read FIgnoreStartingLines write FIgnoreStartingLines default 0;  {!!.02}
  end;

procedure StParseLine(const Data : String; Schema : TStTextDataSchema; Result : TStrings);
function StFieldTypeToStr(FieldType : TStSchemaFieldType) : String;
function StStrToFieldType(const S : String) : TStSchemaFieldType;
function StDeEscape(const EscStr : String): Char;
function StDoEscape(Delim : Char): String;
function StTrimTrailingChars(const S : String; Trailer : Char) : String; {!!.01}

implementation

procedure StParseLine(const Data : String; Schema : TStTextDataSchema;
  Result : TStrings);
{ split a line of delimited data according to provided schema into
  <name>=<value> pairs into Result }
var
  DataLine : TStTextDataRecord;
  ownSchema : Boolean;
begin
  { need a valid TStrings to work with }
  if not Assigned(Result) then Exit;


  ownSchema := False;
  { if no Schema to use passed in, create a default schema }
  if not Assigned(Schema) then begin
    Schema := TStTextDataSchema.Create;
    ownSchema := True; { we made it we, s have to free it }
  end;

  DataLine := TStTextDataRecord.Create;
  try
    DataLine.Schema := Schema;
    DataLine.FValue := Data;
    Result.Assign(DataLine.FieldList);
  finally
    DataLine.Free;
    { free the Schema if needed }
    if ownSchema then
      Schema.Free;
  end;
end;

{ TStDataField }

function StFieldTypeToStr(FieldType : TStSchemaFieldType) : String;
{ convert TStSchemaFieldType enum into matching string for BDE schema }
begin
  Result := '';
  case FieldType of
    sftChar	 : Result := 'CHAR';
    sftFloat	 : Result := 'FLOAT';
    sftNumber	 : Result := 'NUMBER';
    sftBool	 : Result := 'BOOL';
    sftLongInt	 : Result := 'LONGINT';
    sftDate	 : Result := 'DATE';
    sftTime	 : Result := 'TIME';
    sftTimeStamp : Result := 'TIMESTAMP';

    else
      Result := '';
  end;
end;

function StStrToFieldType(const S : String) : TStSchemaFieldType;
{ convert string to TStSchemaFieldType constant }
var
  Value : Integer;
begin
  Value := GetEnumValue(TypeInfo(TStSchemaFieldType), S);
  if Value > -1 then
    Result := TStSchemaFieldType(Value)
  else
    Result := sftUnknown;
end;

{!!.01 - Added}
function StTrimTrailingChars(const S : String; Trailer : Char) : String;
{
Return a string with specified trailing character removed,
useful for cleanup of fixed data records
}
var
  Len : LongInt;
begin
  Result := S;
  Len := Length(S);
  while (Len > 0) and (Result[Len] = Trailer) do
    Dec(Len);
  SetLength(Result, Len);
end;
{!!.01 - End Added}

function TStDataField.GetAsString: String;
{ build string representation of field to match BDE style }
{
Format :
  <name>,<type>,<width>,<decimals>,<offset>
}
begin
  Result := FFieldName + ',' + StFieldTypeToStr(FFieldType) + ',' +
    { zero pad width, decimals, and offset to at least two places
      to match BDE Schema formatting }
      Format('%.2d,%.2d,%.2d', [FFieldLen, FFieldDecimals, FFieldOffset]);
end;

procedure TStDataField.SetFieldDecimals(const Value: Integer);
begin
  FFieldDecimals := Value;
end;

procedure TStDataField.SetFieldLen(const Value: Integer);
begin
  FFieldLen := Value;
end;

procedure TStDataField.SetFieldName(const Value: String);
begin
  FFieldName := Value;
end;

procedure TStDataField.SetFieldOffset(const Value: Integer);
begin
  FFieldOffset := Value;
end;

procedure TStDataField.SetFieldType(const Value: TStSchemaFieldType);
begin
  FFieldType := Value;
end;


{ TStDataFieldList }

function CharPosIdx(C: Char; const S : String; Idx: Integer): Integer;
{ Find leftmost occurrence of character C in string S past location Idx }
{
If C not found returns 0
}
var
  Len : Integer;
begin
  Len := Length(S);
  if (Idx > Len) or (Idx < 1) then begin
    Result := 0;
    Exit;
  end;

  Result := Idx;
  while (Result <= Len) and (S[Result] <> C) do
    Inc(Result);
  if Result > Len then
    Result := 0;
end;

procedure SplitFieldStr(const Source: String; var Name: String;
  var FieldType: TStSchemaFieldType; var ValLen, Decimals, Offset: Integer);
{ split field description string according to BDE Schema layout }
{
Format :
  <name>,<type>,<width>,<decimals>,<offset>
}
var
  CommaPos, LastPos : Cardinal;
  TempS : String;
begin
  CommaPos := 1;
  LastPos := CommaPos;
  CommaPos := CharPosIdx(',', Source, CommaPos);
  if CommaPos = 0 then CommaPos := Length(Source) + 1;
  Name := Copy(Source, LastPos, CommaPos - LastPos);

  Inc(CommaPos);
  LastPos := CommaPos;
  CommaPos := CharPosIdx(',', Source, CommaPos);
  if CommaPos = 0 then CommaPos := Length(Source) + 1;
  TempS := Copy(Source, LastPos, CommaPos - LastPos);
  FieldType := StStrToFieldType('sft' + TempS);

  Inc(CommaPos);
  LastPos := CommaPos;
  CommaPos := CharPosIdx(',', Source, CommaPos);
  if CommaPos = 0 then CommaPos := Length(Source) + 1;
  ValLen := StrToInt(Copy(Source, LastPos, CommaPos - LastPos));

  Inc(CommaPos);
  LastPos := CommaPos;
  CommaPos := CharPosIdx(',', Source, CommaPos);
  if CommaPos = 0 then CommaPos := Length(Source) + 1;
  Decimals := StrToInt(Copy(Source, LastPos, CommaPos - LastPos));

  Inc(CommaPos);
  LastPos := CommaPos;
  CommaPos := CharPosIdx(',', Source, CommaPos);
  if CommaPos = 0 then CommaPos := Length(Source) + 1;
  Offset := StrToInt(Copy(Source, LastPos, CommaPos - LastPos));
end;

constructor TStDataFieldList.Create;
begin
  inherited Create;
  FList := TStringList.Create;
end;

destructor TStDataFieldList.Destroy;
begin
  FList.Free;
  inherited Destroy;
end;

procedure TStDataFieldList.AddField(const FieldName: String;
  FieldType: TStSchemaFieldType; FieldLen, FieldDecimals, FieldOffset: Integer);
var
  Item : TStDataField;
  Idx : Integer;
begin
  { see if another field with the name exists }
  Idx := FList.IndexOf(FieldName);
  if (Idx > -1) then
    raise EStException.CreateResTP(stscTxtDatUniqueNameRequired, 0);

  { build new item }
  Item := TStDataField.Create;
  try
    Item.FieldName := FieldName;
    Item.FieldType := FieldType;
    Item.FieldLen := FieldLen;
    Item.FieldDecimals := FieldDecimals;
    Item.FieldOffset := FieldOffset;

    { add to list }
    FList.AddObject(FieldName, Item);
  except
    Item.Free;
  end;
end;

procedure TStDataFieldList.AddFieldStr(const FieldDef: String);
var
  Name: String;
  FieldType: TStSchemaFieldType;
  ValLen, Decimals, Offset: Integer;
begin
  SplitFieldStr(FieldDef, Name, FieldType, ValLen, Decimals, Offset);
  AddField(Name, FieldType, ValLen, Decimals, Offset);
end;

procedure TStDataFieldList.Clear;
var
  Idx : Integer;
begin
  for Idx := Pred(FList.Count) downto 0 do begin
    { Free associated object and then delete the StringList entry }
    FList.Objects[Idx].Free;
    FList.Delete(Idx);
  end;
end;

procedure TStDataFieldList.RemoveField(const FieldName: String);
var
  Idx : Integer;
begin
  { locate field }
  Idx := FList.IndexOf(FieldName);

  { if it exists }
  if Idx > -1 then begin
    { Free associated object and then delete the StringList entry }
    FList.Objects[Idx].Free;
    FList.Delete(Idx);
  end
  else
    { no such field, complain... }
    raise EStException.CreateResTP(stscTxtDatNoSuchField, 0);
end;

function TStDataFieldList.GetFieldByName(
  const FieldName: String): TStDataField;
var
  Idx : Integer;
begin
  { locate field }
  Idx := FList.IndexOf(FieldName);

  { if it exists }
  if Idx > -1 then begin
    { return associated object }
    Result := TStDataField(FList.Objects[Idx]);
  end
  else
    { no such field, complain... }
    raise EStException.CreateResTP(stscTxtDatNoSuchField, 0);
end;

function TStDataFieldList.GetField(Index: Integer): TStDataField;
{ return requested field if in range }
begin
  if (Index > -1) and (Index < FList.Count) then
    Result := TStDataField(FList.Objects[Index])
  else
    { no such field, complain... }
    raise EStException.CreateResTP(stscBadIndex, 0);
end;

procedure TStDataFieldList.SetFieldByName(const FieldName: String;
  const Value: TStDataField);
var
  Idx : Integer;
begin
  { see if another field with the name exists }
  Idx := FList.IndexOf(FieldName);

  { delete field at that index replace with new field }
  if (Idx > -1) then begin
    FList.Objects[Idx].Free;
    FList.Objects[Idx] := Value;
  end
  else
    { no such field, complain... }
    raise EStException.CreateResTP(stscTxtDatNoSuchField, 0);
end;

procedure TStDataFieldList.SetField(Index: Integer;
  const Value: TStDataField);
var
  Idx : Integer;
begin
  { see if another field with the name exists }
  Idx := FList.IndexOf(Value.FieldName);
  if (Idx > -1) and (Idx <> Index) then
    raise EStException.CreateResTP(stscTxtDatUniqueNameRequired, 0);

  { delete field at that index replace with new field }
  if (Index > -1) and (Index < FList.Count) then begin
    RemoveField(FList[Index]);
    FList.InsertObject(Index, Value.FieldName, Value);
  end else
    { no such field, complain... }
    raise EStException.CreateResTP(stscBadIndex, 0);
end;


function TStDataFieldList.GetCount: Integer;
{ return count of maintained Field Items }
begin
  Result := FList.Count;
end;


{ TStTextDataSchema }

constructor TStTextDataSchema.Create;
begin
  inherited Create;

  { set default values }
  FFieldDelimiter := StDefaultDelim;
  FQuoteDelimiter := StDefaultQuote;
  FCommentDelimiter := StDefaultComment;
  FFixedSeparator   := StDefaultFixedSep;                           {!!.01}
  FLineTermChar := #0;
  FLineTerminator := ltCRLF;
  FLayoutType := ltUnknown;

  { create internal instances }
  dsFieldList := TStDataFieldList.Create;
  FSchema := TStringList.Create;
end;

destructor TStTextDataSchema.Destroy;
begin
  { clean up the fields list }
  dsFieldList.Clear;

  { free internal instances }
  dsFieldList.Free;
  FSchema.Free;

  inherited Destroy;
end;

procedure TStTextDataSchema.AddField(const FieldName : String;
  FieldType : TStSchemaFieldType; FieldLen, FieldDecimals : Integer);
{ add new field with requested characteristics }
var
  Offset : Integer;
  LastField : TStDataField;
begin
  { calculate the offset based on the length and offset of previous fields }
  if dsFieldList.Count > 0 then begin
    LastField := dsFieldList.Fields[Pred(dsFieldList.Count)];
    Offset := LastField.FieldOffset + LastField.FieldLen;
  end
  else
    Offset := 0;

  dsFieldList.AddField(FieldName, FieldType, FieldLen, FieldDecimals, Offset);
end;

procedure TStTextDataSchema.Assign(ASchema: TStTextDataSchema);
{ deep copy another schema }
var
  i : Integer;
begin
  if not Assigned(ASchema) then Exit;

  { copy properties }
  FLayoutType           := ASchema.LayoutType;
  FFieldDelimiter       := ASchema.FieldDelimiter;
  FCommentDelimiter     := ASchema.CommentDelimiter;
  FQuoteDelimiter       := ASchema.QuoteDelimiter;
  FSchemaName           := ASchema.SchemaName;
  FLineTermChar         := ASchema.LineTermChar;
  FLineTerminator       := ASchema.LineTerminator;

  { copy fields }
  dsFieldList.Clear;
  for i := 0 to Pred(ASchema.FieldCount) do
    dsFieldList.AddFieldStr(ASchema.Fields[i].AsString);
end;

{!!.01 -- Added }
procedure TStTextDataSchema.BuildSchema(AList : TStrings);
var
  i : Integer;
  Field : TStDataField;
begin
  { put schema name in brackets }
  AList.Add('[' + FSchemaName + ']');

  { layout type }
  if FLayoutType = ltVarying then begin
    AList.Add('FileType=VARYING');
    AList.Add('Separator=' + StDoEscape(FFieldDelimiter));
  end
  else begin
    AList.Add('FileType=FIXED');
    AList.Add('Separator=' + StDoEscape(FFixedSeparator));
  end;

  { other parameters }
  AList.Add('Delimiter=' + StDoEscape(FQuoteDelimiter));
  AList.Add('Comment=' + StDoEscape(FCommentDelimiter));
  AList.Add('CharSet=ASCII');

  { write fields }
  for i := 0 to Pred(dsFieldList.Count) do begin
    Field := dsFieldList.Fields[i];
    AList.Add('Field' + IntToStr(i + 1) + '=' + Field.AsString);
  end;
end;
{!!.01 -- End Added }

{!!.01 -- Added }
procedure TStTextDataSchema.ClearFields;
{ remove field definitions from schema }
var
  i : Integer;
begin
  dsFieldList.Clear;
  for i := Pred(FSchema.Count) downto 0 do
    if Pos('Field', Trim(FSchema[i])) = 1 then
      FSchema.Delete(i);
end;
{!!.01 -- End Added }

function TStTextDataSchema.GetCaptions: TStrings;
begin
  Result := dsFieldList.FList;
end;

function TStTextDataSchema.GetFieldByName(const FieldName: String): TStDataField;
begin
  Result := dsFieldList.FieldByName[FieldName];
end;

function TStTextDataSchema.GetFieldCount: Integer;
begin
  Result := dsFieldList.Count;
end;

function TStTextDataSchema.GetField(Index: Integer): TStDataField;
begin
  Result := dsFieldList.Fields[Index];
end;

{!!.01 -- Added }
function TStTextDataSchema.GetSchema: TStrings;
begin
  FSchema.Clear;
  BuildSchema(FSchema);
  Result := FSchema;
end;
{!!.01 -- End Added }

function TStTextDataSchema.IndexOf(const FieldName : String): Integer;
{ return index of field with provided name, returns -1 if no such field is found }
begin
  Result := 0;
  while (Result < dsFieldList.Count) and
//    (dsFieldList.Fields[Result].FieldName <> FieldName) do     {!!.01}
    (AnsiCompareText(dsFieldList.Fields[Result].FieldName,       {!!.01}
      FieldName) <> 0)                                           {!!.01}
  do                                                             {!!.01}
    Inc(Result);
  if Result >= dsFieldList.Count then
    Result := -1;  { not found }
end;

procedure TStTextDataSchema.LoadFromFile(const AFileName: TFileName);
var
  FS : TFileStream;
begin
  FS := TFileStream.Create(AFileName, fmOpenRead or fmShareDenyNone);
  try
    LoadFromStream(FS);
  finally
    FS.Free;
  end;
end;

function StDoEscape(Delim : Char): String;
{
Escapes non-printable characters to Borlandish Pascal "#nnn" constants
}
begin
  if Delim in [#33..#126, #128..#255] then
    Result := Delim
  else
    Result := '#' + IntToStr(Ord(Delim));
end;


function StDeEscape(const EscStr : String): Char;
{
converts "escaped" strings of the forms:
 "#nn" Borlandish Pascal numeric character constants
 ^l    Borlandish Pascal control character constants
into equivalent characters, "##" is treated as the '#' character alone

if the string doesn't constitute such an escape sequence, the first
character is returned
}
var
  S : String;
  C : Char;
  ChrVal : Byte;
begin
  S := Trim(EscStr);

  { if string doesn't start with escape or it's only one character long
    just return first character }
  if (Length(S) = 1) or ((S[1] <> '#') and (S[1] <> '^')) then begin
    Result := S[1];
    Exit;
  end;

  { treat '##' as escape for '#' and '^^' as escape for '^' }
  if ((S[1] = '#') and (S[2] = '#')) or
     ((S[1] = '^') and (S[2] = '^')) then
  begin
    Result := '#';
    Exit;
  end;

  { otherwise try to handle escaped character }
  case S[1] of
    '#':begin
      ChrVal := StrToIntDef(Copy(S, 2,Length(S)-1), Ord(StDefaultDelim));
      if Chr(ChrVal) in [#1..#126] then
        Result := Chr(ChrVal)
      else
        Result := StDefaultDelim;
    end;

    '^': begin { control character format }
      C := Chr(Ord(S[2]) - $40);
      if C in [^A..^_] then
        Result := C
      else
        Result := StDefaultDelim;
    end;

  else
    Result := S[1];
  end; {case}
end;

procedure TStTextDataSchema.LoadFromStream(AStream: TStream);
var
  TS : TStAnsiTextStream;
begin
  TS := TStAnsiTextStream.Create(AStream);
  try
    FSchema.Clear;                                                    {!!.01}
    while not TS.AtEndOfStream do
      FSchema.Add(TS.ReadLine);
    { code to extract Schema properties moved to Update routine }     {!!.01}
    Update(FSchema);                                                  {!!.01}

  finally
    TS.Free;
  end;
end;

procedure TStTextDataSchema.RemoveField(const FieldName: String);
begin
  dsFieldList.RemoveField(FieldName);
end;

procedure TStTextDataSchema.SaveToFile(const AFileName: TFileName);
var
  FS : TFileStream;
begin
  if not FileExists(AFileName) then begin
    FS := TFileStream.Create(AFileName, fmCreate);
    FS.Free;
  end;

  if FSchemaName = '' then
    FSchemaName := JustNameL(AFileName);

  FS := TFileStream.Create(AFileName, fmOpenWrite or fmShareDenyNone);

  try
    SaveToStream(FS);
  finally
    FS.Free;
  end;
end;


{
General format of a Schema file, based on BDE ASCII driver schema files:

; this is a comment
[NAME]
Filetype=<VARYING>|<FIXED>
Separator=char (default = ',' comma)
Delimiter=char (default = '"' double quote)
FieldN=<FieldName>,<FieldType>,<FieldWidth>,<FieldDecimals>,<FieldOffset>
; example fields:
Field1=Name,CHAR,20,00,00
Field2=Rating,CHAR,2,00,20
Field3=Date,DATE,10,00,22
Field4=Weight,Float,7,2,32
}

{!!.01 -- Rewritten}
procedure TStTextDataSchema.SaveToStream(AStream: TStream);
var
  TS : TStAnsiTextStream;
  i : Integer;
  SL : TStringList;
begin
  SL := nil;
  TS := nil;

  try
    SL := TStringList.Create;
    BuildSchema(SL);

    TS := TStAnsiTextStream.Create(AStream);
    for i := 0 to Pred(SL.Count) do
      TS.WriteLine(SL[i]);

  finally
    TS.Free;
    SL.Free;
  end;
end;
{!!.01 -- End Rewritten}

procedure TStTextDataSchema.SetCommentDelimiter(const Value: Char);
begin
  FCommentDelimiter := Value;
end;

procedure TStTextDataSchema.SetFieldByName(const FieldName: String;
  const Value: TStDataField);
begin
  dsFieldList.FieldByName[FieldName] := Value;
end;

procedure TStTextDataSchema.SetFieldDelimiter(const Value: Char);
begin
  FFieldDelimiter := Value;
end;

procedure TStTextDataSchema.SetField(Index: Integer;
  const Value: TStDataField);
begin
  dsFieldList.Fields[Index] := Value;
end;

{!!.01 -- Added }
procedure TStTextDataSchema.SetFixedSeparator(const Value: Char);
begin
  FFixedSeparator := Value;
end;
{!!.01 -- End Added }

procedure TStTextDataSchema.SetLayoutType(const Value: TStSchemaLayoutType);
begin
  FLayoutType := Value;
end;

procedure TStTextDataSchema.SetQuoteDelimiter(const Value: Char);
begin
  FQuoteDelimiter := Value;
end;

procedure TStTextDataSchema.SetSchema(const Value: TStrings);
begin
  FSchema.Assign(Value);                                                 {!!.01}
  Update(FSchema);                                                       {!!.01}
end;

procedure TStTextDataSchema.SetSchemaName(const Value: String);
begin
  FSchemaName := Value;
end;

{!!.01 -- Added }
procedure TStTextDataSchema.Update(AList : TStrings);
var
  ValStr : String;
  Idx : Integer;
begin
  for Idx := 0 to Pred(AList.Count) do begin
    ValStr := AList[Idx];

    { if line isn't blank }
    if ValStr <> '' then begin

      { assume it's the schema name }
      if (ValStr[1] = '[') and (ValStr[Length(ValStr)] = ']') then
        SchemaName := Copy(ValStr, 2, Length(ValStr) - 2)
      else
        { assume the line is a comment }
      if ValStr[1] = FCommentDelimiter {';'} then
        { ignore it };
      { else, it's blank, so skip it }
    end;

  end;

  { extract other Schema Info }
  { get layout type }
  ValStr := AList.Values['Filetype'];
  if UpperCase(ValStr) = 'VARYING' then
    FLayoutType := ltVarying
  else
  if UpperCase(ValStr) = 'FIXED' then
    FLayoutType := ltFixed
  else
    FLayoutType := ltUnknown;

  { get field separator for schema }
  ValStr := AList.Values['Separator'];
  if Length(ValStr) > 0 then
    FFieldDelimiter := StDeEscape(ValStr)
  else
  case FLayoutType of                                                 {!!.01}
    ltFixed :   FFieldDelimiter := StDefaultFixedSep;                 {!!.01}
    ltVarying:  FFieldDelimiter := StDefaultDelim;                    {!!.01}
  end;                                                                {!!.01}

  { get quote delimiter for schema }
  ValStr := AList.Values['Delimiter'];
  if Length(ValStr) > 0 then
    FQuoteDelimiter := StDeEscape(ValStr)
  else
    FQuoteDelimiter := StDefaultQuote;

  { get quote delimiter for schema }
  ValStr := AList.Values['Comment'];
  if Length(ValStr) > 0 then
    FCommentDelimiter := StDeEscape(ValStr)
  else
    FCommentDelimiter := StDefaultQuote;

  { build fields list }
  Idx := 1;
  dsFieldList.Clear;
  ValStr := AList.Values['Field' + IntToStr(Idx)];
  while ValStr <> '' do begin
    dsFieldList.AddFieldStr(ValStr);
    Inc(Idx);
    ValStr := AList.Values['Field' + IntToStr(Idx)];
  end;
end;
{!!.01 -- End Added }


{ TStTextDataRecord }

constructor TStTextDataRecord.Create;
begin
  inherited Create;

  { set default values }
  FValue := '';
  FQuoteAlways := False;
  FQuoteIfSpaces := False;

  { create internal instances }
  FFieldList := TStringList.Create;
end;

destructor TStTextDataRecord.Destroy;
begin
  { free internal instances }
  FFieldList.Free;

  inherited Destroy;
end;

procedure TStTextDataRecord.BuildRecord(Values : TStrings; var NewRecord : String);
{ re-construct record structure from list of field values }
var
  i : Integer;
  Temp : String;
begin
  NewRecord := '';

  for i := 0 to Pred(Values.Count) do begin
    Temp := Values[i];

    { re-quote value if needed }
    DoQuote(Temp);

    { add value onto record }
    if i = 0 then
      NewRecord := Temp
    else
      NewRecord := NewRecord + FSchema.FieldDelimiter + Temp;
  end;
end;

procedure TStTextDataRecord.DoQuote(var Value : String);
{ quote field string if needed or desired }
var
  QuoteIt : Boolean;
begin
  { fire event if available }
  if Assigned(FOnQuoteField) then begin
    FOnQuoteField(self, Value);
  end
  else begin { use default quoting policy }
    QuoteIt := False;
    if FQuoteAlways then
      QuoteIt := True
    else
    if ((Pos(' ', Value) > 0) and FQuoteIfSpaces)
      or (Pos(FSchema.FieldDelimiter, Value) > 0)
    then
      QuoteIt := True;

    if QuoteIt then
      Value := FSchema.QuoteDelimiter + Value + FSchema.QuoteDelimiter;
  end;
end;

function ConvertValue(Value : TVarRec) : String;
{ convert variant record to equivalent string }
const
  BoolChars: array[Boolean] of Char = ('F', 'T');
begin
  case Value.VType of
    vtAnsiString: Result := AnsiString(Value.VAnsiString);
    {$IFDEF UNICODE}
    vtUnicodeString: Result := UnicodeString(Value.VUnicodeString);
    vtWideString: Result := WideString(Value.VWideString);
    {$ENDIF}
    vtBoolean:    Result := BoolChars[Value.VBoolean];
    vtChar:       Result := Value.VChar;
    vtCurrency:   Result := CurrToStr(Value.VCurrency^);
    vtExtended:   Result := FloatToStr(Value.VExtended^);
    vtInteger:    Result := IntToStr(Value.VInteger);
    vtPChar:      Result := Value.VPChar;
    vtString:     Result := Value.VString^;
    {$IFDEF VERSION4}
    vtInt64:      Result := IntToStr(Value.VInt64^);
    {$ENDIF VERSION4}
    else
      raise EStException.CreateResTP(stscTxtDatUnhandledVariant, 0);
  end;
end;

procedure TStTextDataRecord.FillRecordFromArray(Values : array of const);
{ supply field values from a variant open array }
var
  i, j : Integer;
begin
  {$IFDEF Version4}
  if Length(Values) > 0 then begin
  {$ENDIF}
    i := 0;
    j := Low(Values);
    while (j <= High(Values)) and (i < Schema.FieldCount) do begin
      SetField(i, ConvertValue(Values[j]));
      Inc(i);
      Inc(j);
    end;
  {$IFDEF Version4}
  end;
  {$ENDIF}
end;

procedure TStTextDataRecord.FillRecordFromList(Items : TStrings);
{ supply field values from <name>=<value> pairs }
{
  Fields filled from pairs provided in TStrings
  <NAME> entries in Items that don't match Field Names are ignored
  Fields with Names having no corresponding entry in Items are left empty
}
var
  i : Integer;
  FN : String;
begin
  if Assigned(Items) then begin
    for i := 0 to Pred(Schema.FieldCount) do begin
      FN := Schema.Fields[i].FieldName;
      FieldByName[FN] := Items.Values[FN];
    end;
  end;
end;

procedure TStTextDataRecord.FillRecordFromValues(Values : TStrings);
{ supply field values from a list of values }
{
  Fields filled from Values provided in TStrings
  if more Values than Fields, extras are ignored
  if fewer Values than Fields, remaining Fields are left empty
}
var
  i : Integer;
begin
  if Assigned(Values) then begin
    i := 0;
    while (i < Values.Count) and (i < Schema.FieldCount) do begin
      SetField(i, Values[i]);
      Inc(i);
    end;
  end;
end;


function TStTextDataRecord.GetFieldByName(const FieldName: String): String;
{ retrieve value of field in current record with given name }
var
  Idx : Integer;
begin
  Result := '';
  Idx := FSchema.IndexOf(FieldName);
  if Idx > -1 then
    Result := GetField(Idx)
  else
    raise EStException.CreateResTP(stscTxtDatNoSuchField, 0);
end;

function TStTextDataRecord.GetField(Index: Integer): String;
{ retrieve value of field in current record at given index }
var
  Len, Offset: Integer;
  DataField : TStDataField;
  Fields : TStringList;
begin
  if (Index < -1) or (Index > Pred(FSchema.FieldCount)) then
    raise EStException.CreateResTP(stscBadIndex, 0);

  { get characteristics of the field of interest }
  DataField := FSchema.Fields[Index];
  Len := DataField.FieldLen;
  { Decimals := DataField.FieldDecimals; }
  Offset := DataField.FFieldOffset;


  { extract field data from record }
  case FSchema.LayoutType of
    ltFixed   : begin
      { note: Offset is zero based, strings are 1 based }              {!!.01}
      Result := Copy(FValue, Offset + 1, Len);                         {!!.01}
    end;

    ltVarying : begin
      Fields := TStringList.Create;
      try
        ExtractTokensL(FValue, FSchema.FieldDelimiter, FSchema.QuoteDelimiter,
          True, Fields);
        Result := Fields[Index];
      finally
        Fields.Free;
      end;
    end;

    ltUnknown : begin
      raise EStException.CreateResTP(stscTxtDatInvalidSchema, 0);
    end;
  end; {case}
end;

function TStTextDataRecord.GetFieldCount: Integer;
begin
  GetFieldList;                                                        {!!.02}
  Result := FFieldList.Count;
end;

function TStTextDataRecord.GetFieldList: TStrings;
{ convert fields of current record into TStrings collection
  of <name>=<value> pairs }
var
  i : Integer;
  FN : String;
begin
  FFieldList.Clear;

  for i := 0 to Pred(FSchema.FieldCount) do begin
    FN := FSchema.Fields[i].FieldName;
    FFieldList.Add(FN + '=' + FieldByName[FN]);
  end;

  Result := FFieldList;
end;

function TStTextDataRecord.GetValues: TStrings;
var
  i : Integer;
  FN : String;
begin
  FFieldList.Clear;

  for i := 0 to Pred(FSchema.FieldCount) do begin
    FN := FSchema.Fields[i].FieldName;
    FFieldList.Add(FieldByName[FN]);
  end;

  Result := FFieldList;
end;

procedure TStTextDataRecord.MakeEmpty;
{ create an empty record according to schema layout }
var
  i, Width, FieldPos : Integer;
begin
  case FSchema.LayoutType of
    { string of spaces, length equal to total record width }
    ltFixed: begin
      Width := 0;
      for i := 0 to Pred(FSchema.FieldCount) do begin                 {!!.01}
        FieldPos := FSchema.Fields[i].FieldLen +                      {!!.01}
                    FSchema.Fields[i].FieldOffset + 1;                {!!.01}
        if Width < FieldPos then                                      {!!.01}
           Width := FieldPos;                                         {!!.01}
      end;                                                            {!!.01}
      FValue := StringOfChar(FSchema.FixedSeparator, Width);          {!!.01}
    end;

    { string of field separators, length equal to one less than no. of fields }
    ltVarying: begin
      FValue := StringOfChar(FSchema.FieldDelimiter, Pred(FSchema.FieldCount));
    end;

    ltUnknown : begin
      raise EStException.CreateResTP(stscTxtDatInvalidSchema, 0);
    end;
  end;
end;

procedure TStTextDataRecord.SetFieldByName(const FieldName: String;
  const NewValue: String);
{ set value of field in current record with given name }
var
  Idx : Integer;
begin
  Idx := FSchema.IndexOf(FieldName);
  if Idx > -1 then
    SetField(Idx, NewValue)
  else
    raise EStException.CreateResTP(stscTxtDatNoSuchField, 0);
end;

procedure TStTextDataRecord.SetField(Index: Integer;
  const NewValue: String);
{ set value of field in current record at given index }
var
  Len, Offset: Integer;
  Temp, FieldVal : String;
  Fields : TStringList;
  Idx : Integer;
  DataField : TStDataField;
begin
  if (Index < -1) or (Index > Pred(FSchema.FieldCount)) then
    raise EStException.CreateResTP(stscBadIndex, 0);

  { get characteristics of the field of interest }
  DataField := FSchema.Fields[Index];
  Len := DataField.FieldLen;
  Offset := DataField.FFieldOffset;

  Temp := '';

  case FSchema.LayoutType of
    ltFixed   : begin
      for Idx := 0 to Pred(FSchema.FieldCount) do begin
        if Idx = Index then begin
          { replace field with Value right buffered or trimmed to to fit field length }
          if Length(NewValue) < Len then
            FieldVal := PadChL(NewValue, FSchema.FFixedSeparator, Len)   {!!.01}
          else
            FieldVal := Copy(NewValue, 1, Len);

          { note: Offset is zero based, strings are 1 based }
          Move(FieldVal[1], FValue[Offset + 1], Len * SizeOf(Char));
        end;
      end;
    end;

    ltVarying : begin
      Fields := TStringList.Create;
      try
        { parse out the field values }
        ExtractTokensL(FValue, FSchema.FFieldDelimiter,                  {!!.01}
          FSchema.QuoteDelimiter, True, Fields);                         {!!.01}


{!!.02 - rewritten }
//        { find field of interest }
//        for Idx := 0 to Pred(FSchema.FieldCount) do begin
//          if Idx = Index then
//            { set the new value }
//            Fields[Idx] := NewValue;

          { set field of interest }
          Fields[Index] := NewValue;

          { reconstruct the record }
          BuildRecord(Fields, FValue);
//        end;
{!!.02 - rewritten end }

      finally
        Fields.Free;
      end;
    end;

    ltUnknown : begin
      raise EStException.CreateResTP(stscTxtDatInvalidSchema, 0);
    end;
  end; {case}
end;

procedure TStTextDataRecord.SetQuoteAlways(const Value: Boolean);
begin
  FQuoteAlways := Value;
end;

procedure TStTextDataRecord.SetQuoteIfSpaces(const Value: Boolean);
begin
  FQuoteIfSpaces := Value;
end;

procedure TStTextDataRecord.SetSchema(const Value: TStTextDataSchema);
begin
  FSchema := Value;
end;

{!!.02 - Added }
function TStTextDataRecord.GetRecord: String;
var
  Idx : Integer;
  Field : String;
begin
  Result := '';
  for Idx := 0 to (FSchema.FieldCount - 2) do begin
    Field := self.Fields[Idx];
    DoQuote(Field);
    Result := Result + Field + FSchema.FFieldDelimiter;
  end;
  Field := self.Fields[FSchema.FieldCount-1];
  DoQuote(Field);
  Result := Result + Field;
end;
{!!.02 - End Added }

{ TStTextDataRecordSet }

(*
TStLineTerminator = ( {possible line terminators...}
   ltNone,            {..no terminator, ie fixed length lines}
   ltCR,              {..carriage return (#13)}
   ltLF,              {..line feed (#10)}
   ltCRLF,            {..carriage return/line feed (#13/#10)}
   ltOther);          {..another character}
*)

constructor TStTextDataRecordSet.Create;
begin
  inherited Create;
  FCurrentIndex := 0;
  FRecords := TList.Create;
  FIsDirty := False;
  FAtEndOfFile := False;                    {!!.01}
  FIgnoreStartingLines := 0;                                           {!!.02}
end;

destructor TStTextDataRecordSet.Destroy;
begin
  FRecords.Free;
  inherited Destroy;
end;

procedure TStTextDataRecordSet.Append;
{ append new empty record to dataset }
var
  Rec : TStTextDataRecord;
begin
  Rec := TStTextDataRecord.Create;
  Rec.Schema := Schema;
  Rec.MakeEmpty;
  FRecords.Add(Rec);
  FIsDirty := True;
  Last;
end;

procedure TStTextDataRecordSet.AppendArray(Values : array of const);
{ append new record to dataset, set field values from a variant open array }
begin
  Append;
  CurrentRecord.FillRecordFromArray(Values);
end;

procedure TStTextDataRecordSet.AppendList(Items: TStrings);
{ append new record to dataset, set field values from <NAME>=<VALUE> pairs}
begin
  Append;
  CurrentRecord.FillRecordFromList(Items);
end;

procedure TStTextDataRecordSet.AppendValues(Values: TStrings);
{ append new record to dataset, set field values from TStrings}
begin
  Append;
  CurrentRecord.FillRecordFromValues(Values);
end;

function TStTextDataRecordSet.BOF: Boolean;
{ test if at beginning of record set }
begin
  Result := (FCurrentIndex = 0);
end;

procedure TStTextDataRecordSet.Clear;
{ empty record set }
var
  i : Integer;
begin
  for i := 0 to Pred(FRecords.Count) do
    TStTextDataRecord(FRecords[i]).Free;
  FRecords.Clear;
  FIsDirty := False;
end;

procedure TStTextDataRecordSet.Delete;
{ delete record at current position }
begin
  TStTextDataRecord(FRecords[FCurrentIndex]).Free;
  FRecords.Delete(FCurrentIndex);
  FIsDirty := True;
  Next;
end;

function TStTextDataRecordSet.EOF: Boolean;
{ test if at end of record set }
begin
  if FAtEndOfFile then                                          {!!.01}
    FAtEndOfFile := FCurrentIndex = Pred(FRecords.Count);       {!!.01}
  Result := FAtEndOfFile                                        {!!.01}
end;

procedure TStTextDataRecordSet.First;
{ make first record in set current }
begin
  FCurrentIndex := 0;
end;

function TStTextDataRecordSet.GetCount: Integer;
{ return count of records in set }
begin
  Result := FRecords.Count;
end;

function TStTextDataRecordSet.GetRecord(Index: Integer): TStTextDataRecord;
{ return particular record by index }
begin
  if (Index > -1) and (Index < FRecords.Count) then
    Result := FRecords[Index]
  else
    raise EStException.CreateResTP(stscBadIndex, 0);
end;

function TStTextDataRecordSet.GetCurrentRecord: TStTextDataRecord;
{ return current record }
begin
  Result := FRecords[FCurrentIndex];
end;

function TStTextDataRecordSet.GetSchema: TStTextDataSchema;
{ return reference to associated schema, create default one if needed }
begin
  if not Assigned(FSchema) then
    FSchema := TStTextDataSchema.Create;
  Result := FSchema;
end;

procedure TStTextDataRecordSet.Insert(Index: Integer);
{ insert new empty record into dataset at specified location,
  shifts the record set down one }
var
  Rec : TStTextDataRecord;
begin
  Rec := TStTextDataRecord.Create;
  Rec.Schema := Schema;
  Rec.MakeEmpty;
  FRecords.Insert(Index, Rec);
  FIsDirty := True;
  FCurrentIndex := Index;
end;

procedure TStTextDataRecordSet.InsertArray(Index: Integer; Values : array of const);
{ insert new record into dataset dataset at specified location,
  shifts the record set down one,
  set field values from a variant open array }
begin
  Insert(Index);
  CurrentRecord.FillRecordFromArray(Values);
end;

procedure TStTextDataRecordSet.InsertList(Index: Integer;
  Items: TStrings);
{ insert new record into dataset dataset at specified location,
  shifts the record set down one,
  set field values from <NAME>=<VALUE> pairs}
begin
  Insert(Index);
  CurrentRecord.FillRecordFromList(Items);
end;

procedure TStTextDataRecordSet.InsertValues(Index: Integer;
  Values: TStrings);
{ insert new record into dataset dataset at specified location,
  shifts the record set down one,
  set field values from TStrings}
begin
  Insert(Index);
  CurrentRecord.FillRecordFromValues(Values);
end;

procedure TStTextDataRecordSet.Last;
{ make final record in set current }
begin
  FCurrentIndex := Pred(FRecords.Count);
end;

procedure TStTextDataRecordSet.LoadFromFile(const AFile: TFileName);
var
  FS : TFileStream;
begin
  FS := TFileStream.Create(AFile, fmOpenRead or fmShareDenyNone);
  try
    LoadFromStream(FS);
  finally
    FS.Free;
  end;
end;

procedure TStTextDataRecordSet.LoadFromStream(AStream: TStream);
var
  TS : TStAnsiTextStream;
  NewRec : TStTextDataRecord;
  i, Len : Integer;                                                   {!!.02}
begin
  if FActive then
    raise EStException.CreateResTP(stscTxtDatRecordSetOpen, 0);

  Clear;

  TS := TStAnsiTextStream.Create(AStream);

  { match Ansi Stream terminator to schema's }
  TS.LineTermChar   := AnsiChar(Schema.LineTermChar);
  TS.LineTerminator := Schema.LineTerminator;

{!!.02 - added }
  { calculate length of fixed record }
  if Schema.LayoutType = ltFixed then begin
    Len := 0;
    for i := 0 to Pred(Schema.FieldCount) do
     Len := Len + Schema.Fields[i].FieldLen;
    TS.FixedLineLength := Len;
  end;
{!!.02 - added end }

  try
{!!.02 - added }
    { ignore starting lines }
    for i := 1 to FIgnoreStartingLines do
      TS.ReadLine;
{!!.02 - added end }

    while not TS.AtEndOfStream do begin
      { new record }
      NewRec := TStTextDataRecord.Create;

      { set record data }
      NewRec.FValue := TS.ReadLine;

{!!.01 - Rewritten }
      if TrimCharsL(NewRec.FValue, St_WhiteSpace) <> '' then begin
        { set the schema to match }
        NewRec.Schema := Schema;

        { append new record }
        FRecords.Add(NewRec);

      end
      else {ignore blank lines}
        NewRec.Free;
{!!.01 - End Rewritten }
    end;


    FActive := True;
    FIsDirty := False;
  finally
    TS.Free;
  end;
end;

function TStTextDataRecordSet.Next : Boolean;
{ make next record in set current }
begin
  Result := True;

  { if already on last record, stay there }
  if FCurrentIndex = Pred(FRecords.Count) then begin          {!!.01}
    FAtEndOfFile := True; { yep, we're at the end }           {!!.01}
    Result := False;                                          {!!.01}
  end                                                         {!!.01}
  else                                                        {!!.01}
    Inc(FCurrentIndex);                                       {!!.01}
end;

function TStTextDataRecordSet.Prior : Boolean;
{ make previous record in set current }
begin
  Result := True;
  Dec(FCurrentIndex);

  { if already on first record, stay there }
  if FCurrentIndex < 0 then begin
    FCurrentIndex := 0;
    Result := False;
  end;
end;

procedure TStTextDataRecordSet.SaveToFile(const AFile: TFileName);
var
  FS : TFileStream;
begin
  if not FileExists(AFile) then begin
    FS := TFileStream.Create(AFile, fmCreate);
    FS.Free;
  end;

  FS := TFileStream.Create(AFile, fmOpenWrite or fmShareDenyNone);

  try
    SaveToStream(FS);
  finally
    FS.Free;
  end;
end;

procedure TStTextDataRecordSet.SaveToStream(AStream: TStream);
var
  TS : TStAnsiTextStream;
  i : Integer;
begin
  TS := TStAnsiTextStream.Create(AStream);

  { match Ansi Stream terminator to schema's }
  TS.LineTermChar   := AnsiChar(Schema.LineTermChar);
  TS.LineTerminator := Schema.LineTerminator;

  { write the records }
  try
    for i := 0 to Pred(FRecords.Count) do
      TS.WriteLine(TStTextDataRecord(FRecords[i]).AsString);

    FIsDirty := False;
  finally
    TS.Free;
  end;
end;

procedure TStTextDataRecordSet.SetActive(const Value: Boolean);
{ activate or close record set }
begin
  FActive := Value;
  if not FActive then begin
    Clear;
    FSchema := nil;
  end;
end;

procedure TStTextDataRecordSet.SetCurrentRecord(
  const Value: TStTextDataRecord);
begin
  TStTextDataRecord(FRecords[FCurrentIndex]).Free;
  FRecords.Insert(FCurrentIndex, Value);
  FIsDirty := True;
end;

procedure TStTextDataRecordSet.SetRecord(Index: Integer;
  const Value: TStTextDataRecord);
begin
  TStTextDataRecord(FRecords[Index]).Free;
  FRecords.Insert(Index, Value);
  FIsDirty := True;
end;

procedure TStTextDataRecordSet.SetSchema(const Value: TStTextDataSchema);
{ assign new schema, only works on inactive record set }
begin
  if not FActive then begin
    if Assigned(FSchema) then
      FSchema.Free;
      FSchema := Value;
  end
  else
    raise EStException.CreateResTP(stscTxtDatRecordSetOpen, 0);
end;



end.
