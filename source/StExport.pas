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
{* SysTools: StExport.pas 4.04                           *}
{*********************************************************}
{* SysTools: DB Exporter Classes                         *}
{*********************************************************}

{$include StDefine.inc}

unit StExport;

interface

uses
  Windows, SysUtils, Classes, DB, DbConsts,
  StBase, StTxtDat;

const
  DefaultDateFmt = 'mm/dd/yyyy';
  DefaultTimeFmt = 'hh:mm:ss';
  DefaultDateTimeFmt = 'mm/dd/yyyy hh:mm:ss';

type
  TStExportProgressEvent = procedure (Sender : TObject; Index : Integer;
    var Abort : Boolean) of object;

  TStDBtoCSVExport = class
  private
    FDataSet: TDataSet;
    FFieldDelimiter: Char;
    FIncludeHeader: Boolean;
    FLineTermChar : AnsiChar;
    FLineTerminator : TStLineTerminator;
    FQuoteAlways: Boolean;
    FQuoteDelimiter: Char;
    FQuoteIfSpaces: Boolean;

    FDateFmt: string;
    FTimeFmt, FDateTimeFmt : string;

    FOnExportProgress : TStExportProgressEvent;
    FOnQuoteField : TStOnQuoteFieldEvent;
  protected {private}
    function BuildCSVHeader: string;
    function BuildCSVRec : string;

    procedure SetDataSet(const Value: TDataSet);
    procedure SetFieldDelimiter(const Value: Char);
    procedure SetIncludeHeader(const Value: Boolean);
    procedure SetQuoteAlways(const Value: Boolean);
    procedure SetQuoteDelimiter(const Value: Char);
    procedure SetQuoteIfSpaces(const Value: Boolean);
  public
    constructor Create;

    { Access and Update Methods }
    procedure DoQuote(var Value: String); virtual;

    { Persistence and streaming methods }
    procedure ExportToStream(AStream : TStream);
    procedure ExportToFile(AFile : TFileName);

    { properties }
    property DataSet : TDataSet read FDataSet write SetDataSet;
    property FieldDelimiter : Char
      read FFieldDelimiter write SetFieldDelimiter default StDefaultDelim;
    property IncludeHeader : Boolean
      read FIncludeHeader write SetIncludeHeader default False;
    property LineTermChar : AnsiChar
      read FLineTermChar write FLineTermChar default #0;
    property LineTerminator : TStLineTerminator
      read FLineTerminator write FLineTerminator default ltCRLF;
    property QuoteAlways : Boolean
      read FQuoteAlways write SetQuoteAlways default False;
    property QuoteDelimiter : Char
      read FQuoteDelimiter write SetQuoteDelimiter default StDefaultQuote;
    property QuoteIfSpaces : Boolean
      read FQuoteIfSpaces write SetQuoteIfSpaces default False;

    property DateFmt : string
      read FDateFmt write FDateFmt;
    property TimeFmt : string
      read FTimeFmt write FTimeFmt;
    property DateTimeFmt : string
      read FDateTimeFmt write FDateTimeFmt;
    { events }
    property OnQuoteField : TStOnQuoteFieldEvent
      read FOnQuoteField write FOnQuoteField;
    property OnExportProgress : TStExportProgressEvent
      read FOnExportProgress write FOnExportProgress;
  end;


  TStDbSchemaGenerator = class
  private
    FDataSet : TDataSet;
    FSchema : TStTextDataSchema;
  protected {private}
    function GetFieldDelimiter: Char;
    function GetQuoteDelimiter: Char;
    function GetSchemaName: string;
    procedure SetDataSet(const Value: TDataSet);
    procedure SetFieldDelimiter(const Value: Char);
    procedure SetQuoteDelimiter(const Value: Char);
    procedure SetSchemaName(const Value: string);
  public
    constructor Create;
    destructor Destroy; override;

    { Persistence and streaming methods }
    procedure ExportToStream(AStream : TStream);
    procedure ExportToFile(AFile : TFileName);

    { properties }
    property DataSet : TDataSet
      read FDataSet write SetDataSet;
    property FieldDelimiter : Char
      read GetFieldDelimiter write SetFieldDelimiter default StDefaultDelim;
    property QuoteDelimiter : Char
      read GetQuoteDelimiter write SetQuoteDelimiter default StDefaultQuote;
    property SchemaName : string
      read GetSchemaName write SetSchemaName;
  end;

implementation

uses
  StStrms;

{
  TFieldType = (ftUnknown, ftString, ftSmallint, ftInteger, ftWord,
    ftBoolean, ftFloat, ftCurrency, ftBCD, ftDate, ftTime, ftDateTime,
    ftBytes, ftVarBytes, ftAutoInc, ftBlob, ftMemo, ftGraphic, ftFmtMemo,
    ftParadoxOle, ftDBaseOle, ftTypedBinary, ftCursor, ftFixedChar, ftWideString,
    ftLargeint, ftADT, ftArray, ftReference, ftDataSet, ftOraBlob, ftOraClob,
    ftVariant, ftInterface, ftIDispatch, ftGuid);
}
const
{ see DB unit for full set of field types }
  DBValidFields = [ftString, ftSmallInt, ftInteger, ftAutoInc, ftWord, ftBoolean,
                   ftFloat,  ftCurrency, ftBCD, ftDate, ftTime, ftDateTime];
  DBFloatFields = [ftFloat, ftCurrency, ftBCD];

{ TStDBtoCSVExport }

constructor TStDBtoCSVExport.Create;
begin
  inherited Create;
  FFieldDelimiter := StDefaultDelim;
  FQuoteDelimiter := StDefaultQuote;
  FLineTermChar := #0;
  FLineTerminator := ltCRLF;
  FQuoteAlways := False;
  FQuoteIfSpaces := False;

  FDateFmt := DefaultDateFmt;
  FTimeFmt := DefaultTimeFmt;
  FDateTimeFmt := DefaultDateTimeFmt;
end;

function TStDBtoCSVExport.BuildCSVHeader: string;
{ generate CSV header from Data Set field data }
var
  i : Integer;
  Name : string;
  TheField : TField;
begin
  Result := '';
  for i := 0 to Pred(FDataSet.FieldCount) do begin
    TheField := FDataSet.Fields[i];

    { is field is among supported types? }
    if (TheField.FieldKind = fkData) and
      (TheField.DataType in DBValidFields) then begin
      { get name of current field }
      Name := TheField.FieldName;

      if i = 0 then { no field delimiter before first field }
        Result := Result + Name
      else
        Result := Result + FFieldDelimiter + Name;
    end;
  end;
end;

function TStDBtoCSVExport.BuildCSVRec: string;
{ generate record of CSV data from Data Set field data }
var
  i : Integer;
  FieldStr : String;
  TheField : TField;
begin
  Result := '';
  for i := 0 to Pred(FDataSet.FieldCount) do begin
    TheField := FDataSet.Fields[i];

    { is field is among supported types? }
    if (TheField.FieldKind = fkData) and
      (TheField.DataType in DBValidFields) then
    begin
      { get value of current field as a string }
      case TheField.DataType of
        ftDate : FieldStr := FormatDateTime(FDateFmt, TheField.AsDateTime);
        ftTime : FieldStr := FormatDateTime(FTimeFmt, TheField.AsDateTime);
        ftDateTime : FieldStr := FormatDateTime(FDateTimeFmt, TheField.AsDateTime);
        else
          FieldStr := TheField.AsString;
      end;


      { quote if needed }
      DoQuote(FieldStr);

      if i = 0 then { no field delimiter before first field }
        Result := Result + FieldStr
      else
        Result := Result + FFieldDelimiter + FieldStr;
    end;
  end;
end;

procedure TStDBtoCSVExport.DoQuote(var Value : String);
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
       or (Pos(FFieldDelimiter, Value) > 0)
       or (Pos(FQuoteDelimiter, Value) > 0)
    then
      QuoteIt := True;

    if QuoteIt then
      Value := FQuoteDelimiter + Value + FQuoteDelimiter;
  end;
end;

procedure TStDBtoCSVExport.ExportToFile(AFile: TFileName);
var
  FS : TFileStream;
begin
  FS := TFileStream.Create(AFile, fmCreate);
  try
    ExportToStream(FS);
  finally
    FS.Free;
  end;
end;

procedure TStDBtoCSVExport.ExportToStream(AStream: TStream);
var
  TS : TStAnsiTextStream;
  Abort : Boolean;
  Count : Integer;
begin
  { table must be open and active }
  if not FDataSet.Active then
    DatabaseError(SDataSetClosed, FDataSet);

  TS := TStAnsiTextStream.Create(AStream);
  TS.LineTerminator := FLineTerminator;
  TS.LineTermChar   := FLineTermChar;
  try
    { generate header line if desired }
    if FIncludeHeader then
      TS.WriteLine(BuildCSVHeader);

    { iterate table }
    FDataSet.First;
    Count := 0;
    Abort := False;
    while not FDataSet.Eof and not Abort do begin
      { write CSV formatted data for current record }
      TS.WriteLine(BuildCSVRec);
      Inc(Count);

      if Assigned(FOnExportProgress) then
        FOnExportProgress(self, Count, Abort);

      { next record }
      FDataSet.Next;
    end;
  finally
    TS.Free;
  end;
end;

procedure TStDBtoCSVExport.SetDataSet(const Value: TDataSet);
begin
  FDataSet := Value;
end;

procedure TStDBtoCSVExport.SetFieldDelimiter(const Value: Char);
begin
  FFieldDelimiter := Value;
end;

procedure TStDBtoCSVExport.SetIncludeHeader(const Value: Boolean);
begin
  FIncludeHeader := Value;
end;

procedure TStDBtoCSVExport.SetQuoteAlways(const Value: Boolean);
begin
  FQuoteAlways := Value;
end;

procedure TStDBtoCSVExport.SetQuoteIfSpaces(const Value: Boolean);
begin
  FQuoteIfSpaces := Value;
end;

procedure TStDBtoCSVExport.SetQuoteDelimiter(const Value: Char);
begin
  FQuoteDelimiter := Value;
end;

{
TStSchemaFieldType = (sftUnknown, sftChar, sftFloat, sftNumber,
 sftBool, sftLongInt, sftDate, sftTime, sftTimeStamp);
}

function ConvertFieldType(DBFieldType : TFieldType) : TStSchemaFieldType;
{ convert table field type to schema field type }
begin
  case DBFieldType of
    ftString   : Result := sftChar;

    ftSmallInt : Result := sftNumber;
    ftInteger  : Result := sftLongInt;
    ftAutoInc  : Result := sftLongInt;
    ftWord     : Result := sftNumber;

    ftBoolean  : Result := sftBool;

    ftFloat    : Result := sftFloat;
    ftCurrency : Result := sftFloat;
    ftBCD      : Result := sftFloat;

    ftDate     : Result := sftDate;
    ftTime     : Result := sftTime;
    ftDateTime : Result := sftTimeStamp;

    else
      Result := sftUnknown;
  end;
end;

function GetDecimals(const DataStr : string): Integer;
{ determine decimal places for float formatted string }
begin
  Result := Length(DataStr) - Pos(FormatSettings.DecimalSeparator, DataStr);
  try
    StrToFloat(DataStr);
  except
    Result := 0;
  end;
end;


{ TStDbSchemaGenerator }

constructor TStDbSchemaGenerator.Create;
begin
  inherited Create;

  FSchema := TStTextDataSchema.Create;
  { set defaults for compatible schema }
  FSchema.LayoutType := ltVarying;
  FSchema.FieldDelimiter := StDefaultDelim;
  FSchema.QuoteDelimiter := StDefaultQuote;
  FSchema.CommentDelimiter := StDefaultComment;
end;

destructor TStDbSchemaGenerator.Destroy;
begin
  FSchema.Free;
  inherited Destroy;
end;

procedure TStDbSchemaGenerator.ExportToFile(AFile: TFileName);
var
  FS : TFileStream;
begin
  FS := TFileStream.Create(AFile, fmCreate);
  try
    ExportToStream(FS);
  finally
    FS.Free;
  end;
end;

procedure TStDbSchemaGenerator.ExportToStream(AStream: TStream);
var
  i, Width, Decimals : Integer;
  TheField : TField;
begin
  { table must be open and active }

  if not FDataSet.Active then
    DatabaseError(SDataSetClosed, FDataSet);

  { build field definitions }
  for i := 0 to Pred(FDataSet.FieldCount) do begin
    TheField := FDataSet.Fields[i];

    { is field is among supported types? }
    if (TheField.FieldKind = fkData) and
       (TheField.DataType in DBValidFields) then
    begin
      Width := TheField.DisplayWidth;

      { if it's a floating point type field, need decimals }
      if (FDataSet.Fields[i].DataType in DBFloatFields) then
        Decimals := GetDecimals(TheField.AsString)
      else
        Decimals := 0;

      { add field definition to Schema }
      FSchema.AddField(TheField.FieldName,
        ConvertFieldType(TheField.DataType), Width, Decimals);

    end;
  end;

  { save the schema }
  FSchema.SaveToStream(AStream);
end;

function TStDbSchemaGenerator.GetFieldDelimiter: Char;
begin
  Result := FSchema.FieldDelimiter;
end;

function TStDbSchemaGenerator.GetQuoteDelimiter: Char;
begin
  Result := FSchema.QuoteDelimiter;
end;

function TStDbSchemaGenerator.GetSchemaName: string;
begin
  Result := FSchema.SchemaName;
end;

procedure TStDbSchemaGenerator.SetDataSet(const Value: TDataSet);
begin
  FDataSet := Value;
end;

procedure TStDbSchemaGenerator.SetFieldDelimiter(const Value: Char);
begin
  FSchema.FieldDelimiter := Value;
end;

procedure TStDbSchemaGenerator.SetQuoteDelimiter(const Value: Char);
begin
  FSchema.QuoteDelimiter := Value;
end;

procedure TStDbSchemaGenerator.SetSchemaName(const Value: string);
begin
  FSchema.SchemaName:= Value;
end;

end.
