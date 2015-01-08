// Upgraded to Delphi 2009: Sebastian Zierer
// FIXME: TStAnsiTextStream

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
{* SysTools: StIniStm.pas 4.04                           *}
{*********************************************************}
{* SysTools: .INI file-like stream class                 *}
{*********************************************************}

{$include StDefine.inc}

unit StIniStm;


interface

uses
  Windows, SysUtils, Classes, StStrms;

type

  TStIniStream = class(TObject)
  private
    FAnsiStream : TStAnsiTextStream;
    FSections : TStringList;
    procedure GetSecStrings(Strs: TStrings);
  protected
    procedure GotoSection(const Section : String);
    procedure UpdateSections;
    procedure WriteSectionName(const Section : String);
    procedure WriteValue(const Key, Value : String);
  public
    constructor Create(aStream : TStream);
    destructor Destroy; override;

    function SectionExists(const Section : String): Boolean;
    function ReadString(const Section, Ident, Default : String) : String;
    procedure WriteString(const Section, Ident, Value : String);
    procedure WriteSection(const Section : String; Strings: TStrings);
    procedure ReadSection(const Section : String; Strings: TStrings);
    procedure ReadSections(Strings: TStrings);
    procedure ReadSectionValues(const Section : String; Strings: TStrings);
    procedure EraseSection(const Section : String);
    procedure DeleteKey(const Section, Ident : String);
    function ValueExists(const Section, Ident : String): Boolean;
  end;

procedure SplitNameValue(const Line : string; var Name, Value : string); {!!.04}

implementation

{!!.04 - Added }
procedure SplitNameValue(const Line : string; var Name, Value : string);
var
  P : Integer;
begin
  P := Pos('=', Line);
  if P < 1 then begin
    Name := Line;
    Value := '';
    Exit;
  end;

  Name := Copy(Line, 1, P-1);
  Value := Copy(Line, P+1, Length(Line) - P);
end;
{!!.04 - Added End}

function IsHeader(const AString : String) : Boolean;
{ see if passed in text looks like an .INI header }
var
  Temp : String;
begin
  if AString = '' then begin
    Result := False;
    Exit;
  end;

  Temp := Trim(AString);
  Result := (Temp[1] = '[') and (Temp[Length(Temp)] = ']')
end;


{ TStIniStream }

constructor TStIniStream.Create(aStream: TStream);
begin
  inherited Create;
  FAnsiStream := TStAnsiTextStream.Create(aStream);
  FSections := TStringList.Create;
  FSections.Sorted := True;
  FSections.Duplicates := dupIgnore;

  if aStream.Size > 0 then { not an empty stream }
    UpdateSections;
end;

destructor TStIniStream.Destroy;
begin
  FSections.Free;
  FAnsiStream.Free;
  inherited Destroy;
end;


procedure TStIniStream.DeleteKey(const Section, Ident : String);
{ delete specified item from Section }
var
  SecStrs : TStringList;
  SecIdx  : Integer;
  MS : TMemoryStream;
  TS : TStAnsiTextStream;
  i, Idx : Integer;
begin
  SecStrs := TStringList.Create;
  MS := TMemoryStream.Create;
  TS := TStAnsiTextStream.Create(MS);

  try
    { locate and read section }
    GotoSection(Section);
    GetSecStrings(SecStrs);
    Idx := SecStrs.IndexOfName(Ident);

    if Idx > - 1 then begin
      { remove desired key }
      SecStrs.Delete(Idx);

      { locate subsequent section }
      SecIdx := FSections.IndexOf(Section);
      if SecIdx < Pred(FSections.Count) then begin
        GotoSection(FSections[SecIdx+1]);

        { copy remaining sections }
        while not FAnsiStream.AtEndOfStream do
          TS.WriteLine(FAnsiStream.ReadLine);
      end;
      { else this is the last section }

      { seek back and truncate }
      GotoSection(Section);
      FAnsiStream.Size := FAnsiStream.Position;
//      FAnsiStream.SetSize(FAnsiStream.Position);

      { write updated section }
      WriteSectionName(Section);
      for i := 0 to Pred(SecStrs.Count) do
        FAnsiStream.WriteLine(SecStrs[i]);
      FAnsiStream.Stream.Seek(0, soFromEnd);

      { append saved subsequent sections }
      TS.SeekLine(0);
      while not TS.AtEndOfStream do
        FAnsiStream.WriteLine(TS.ReadLine);

    end; { if Ident > -1 }
    { else the Ident doesn't exist so don't alter anything }

  finally
    SecStrs.Free;
    TS.Free;
    MS.Free;
  end;
end;

procedure TStIniStream.EraseSection(const Section : String);
{ erase specified section from Ini data }
var
  SecIdx  : Integer;
  MS : TMemoryStream;
  TS : TStAnsiTextStream;
begin
  MS := TMemoryStream.Create;
  TS := TStAnsiTextStream.Create(MS);

  { locate section }
  SecIdx := FSections.IndexOf(Section);

  { if section found }
  if SectionExists(Section) then begin
    try
      { if this is not the last section }
      if (SecIdx < Pred(FSections.Count)) then begin
        { locate subsequent section }
        GotoSection(FSections[SecIdx+1]);

        { copy remaining sections to temporary stream}
        while not FAnsiStream.AtEndOfStream do
          TS.WriteLine(FAnsiStream.ReadLine);
      end;
      { else this is the last section }

      { locate section to delete and truncate }
      GotoSection(Section);
      FAnsiStream.Size := FAnsiStream.Position;
//      FAnsiStream.SetSize(FAnsiStream.Position);

      { append saved subsequent sections }
      TS.SeekLine(0);
      while not TS.AtEndOfStream do
        FAnsiStream.WriteLine(TS.ReadLine);

    finally
      TS.Free;
      MS.Free;
    end;
    UpdateSections;
  end;
  { else section doesn't exist, do nothing }
end;

procedure TStIniStream.GetSecStrings(Strs : TStrings);
{ read strings from a section, preserving comments and blanks }
var
  LineVal : String;
begin
  { assume we're at the start of a section }
  FAnsiStream.ReadLine; { skip section header }

  LineVal := FAnsiStream.ReadLine;
  while not (FAnsiStream.AtEndOfStream) and not (IsHeader(LineVal)) do begin
    Strs.Add(LineVal); { add it to the list }
    LineVal := FAnsiStream.ReadLine; { get next line }
  end;
end;

procedure TStIniStream.GotoSection(const Section: String);
{ position stream to requested section header }
var
  Idx : Integer;
begin
 Idx := FSections.IndexOf(Section);
  if Idx > -1 then
    FAnsiStream.SeekLine(Integer(FSections.Objects[Idx]));
end;

procedure TStIniStream.ReadSectionValues(const Section : String;
  Strings: TStrings);
{ return <Name>=<Value> pairs of requested Section in Strings }
var
  Strs : TStringList;
  LineVal : String;
  i : Integer;
begin
  if not Assigned(Strings) then Exit;

  Strs := TStringList.Create;
  if SectionExists(Section) then begin { section exists }
    Strings.Clear;
    try
      { locate section }
      GotoSection(Section);

      { retrieve section contents, comments, blank lines and all }
      GetSecStrings(Strs);

      { iterate section lines looking for entries }
      for i := 0 to Pred(Strs.Count) do begin
        LineVal := Strs[i];
        if (Trim(LineVal) <> '') and (Trim(LineVal[1]) <> ';') and (Pos('=', LineVal) > 0) then {!!.02}
      { not empty and not a comment and at least superficially resembles a
        <Name>=<Value> pair }
        Strings.Add(Trim(LineVal)); { add it to the list }             {!!.02}
      end;
    finally
      Strs.Free;
    end;
  end;
  { else section doesn't exist, do nothing }
end;

procedure TStIniStream.ReadSections(Strings: TStrings);
var
  i : Integer;
begin
  if not Assigned(Strings) then Exit;

  { omit the pseudo section }
  for i := 1 to Pred(FSections.Count) do
    Strings.Add(Trim(FSections[i]));                                   {!!.02}
end;

procedure TStIniStream.ReadSection(const Section : String;
  Strings: TStrings);
{ return Name strings for all entries in requested section }
var
  SecStrs : TStringList;
  i : Integer;
  LineVal, Name : String;
begin
  if not Assigned(Strings) then Exit;

  SecStrs := TStringList.Create;
  try
//    ReadSection(Section, SecStrs);
{!!.02 - Rewritten }
    Strings.Clear;
    { locate section }
    GotoSection(Section);

    { retrieve section contents, comments, blank lines and all }
    GetSecStrings(SecStrs);

    { iterate section lines looking for entries }
    for i := 0 to Pred(SecStrs.Count) do begin
      LineVal := SecStrs[i];
      if (Trim(LineVal) <> '') and (Trim(LineVal[1]) <> ';') and (Pos('=', LineVal) > 0) then begin
      { not empty and not a comment and at least superficially resembles a
      <Name>=<Value> pair }
        SplitNameValue(LineVal, Name, LineVal);
        Strings.Add(Trim(Name));
      end;
    end;

//    for i := 0 to Pred(SecStrs.Count) do
//      Strings.Add(SecStrs.Names[i]);
{!!.02 - Rewritten End }


  finally
    SecStrs.Free;
  end;
end;

function TStIniStream.ReadString(const Section, Ident,
  Default : String) : String;
{
return a particular string selected by Ident from Section
if empty or doesn't exist, return Default
}
var
  SecStrs : TStringList;
begin
  SecStrs := TStringList.Create;
  try
    ReadSectionValues(Section, SecStrs);                            {!!.04}

    Result := SecStrs.Values[Ident];
    if Result = '' then
      Result := Default;

  finally
    SecStrs.Free;
  end;
end;

function TStIniStream.SectionExists(const Section : String): Boolean;
{ returns True if Section exists in section list, False otherwise }
begin
  Result := FSections.IndexOf(Section) > -1;
end;

procedure TStIniStream.UpdateSections;
{ refresh Sections list }
var
  i : Integer;
  Line : String;
begin
  i := 0;
  FSections.Clear;
  FAnsiStream.SeekLine(0);

  { pseudo section to account for any comments or whitespace prior to first
    real section in data }
  FSections.AddObject('[]', TObject(0));

  { iterate data looking for section headers: '[blah]' }
  while not FAnsiStream.AtEndOfStream do begin
    Line := Trim(FAnsiStream.ReadLine);
    { if it looks like a header }
    if IsHeader(Line) then
      { add it to the list with a line index }
      FSections.AddObject(Copy(Line, 2, Length(Line) - 2), TObject(i));
    { go to next line }
    Inc(i);
  end;
end;

function TStIniStream.ValueExists(const Section, Ident : String): Boolean;
{
see if requested section contains requested Ident
implies "<Ident>=" exists in section, not that there's necessarily any
explicit Value associated, i.e. Value may be blank
}
var
  SecStrs : TStringList;
  i : Integer;
begin
  Result := False;
  SecStrs := TStringList.Create;
  try
    { get section }
    ReadSection(Section, SecStrs);

    { see if Ident exists in Names collection }
    for i := 0 to Pred(SecStrs.Count) do
      if SecStrs.Names[i] = Ident then begin
        Result := True;
        Break;
      end;
  finally
    SecStrs.Free;
  end;
end;

procedure TStIniStream.WriteString(const Section, Ident, Value : String);
{ write individual string value to IniStream }
var
  SecStrs : TStringList;
  SecIdx  : Integer;
  MS : TMemoryStream;
  TS : TStAnsiTextStream;
  i : Integer;
begin
  if SectionExists(Section) then begin
    SecStrs := TStringList.Create;
    MS := TMemoryStream.Create;
    TS := TStAnsiTextStream.Create(MS);

    try
      { locate and read section }
      GotoSection(Section);
      GetSecStrings(SecStrs);

      { locate subsequent section }
      SecIdx := FSections.IndexOf(Section);
      if SecIdx < Pred(FSections.Count) then begin
        GotoSection(FSections[SecIdx+1]);

        { copy remaining sections }
        while not FAnsiStream.AtEndOfStream do
          TS.WriteLine(FAnsiStream.ReadLine);
      end;
      { else this is the last section }

      { seek back and truncate }
      GotoSection(Section);
      FAnsiStream.Size := FAnsiStream.Position;

//      FAnsiStream.SetSize(FAnsiStream.Position);

      { insert new value }
      SecStrs.Add(Ident + '=' + Value);

      { write updated section }
      WriteSectionName(Section);
      for i := 0 to Pred(SecStrs.Count) do
        FAnsiStream.WriteLine(SecStrs[i]);
      FAnsiStream.Stream.Seek(0, soFromEnd);

      { append saved subsequent sections }
      TS.SeekLine(0);
      while not TS.AtEndOfStream do
        FAnsiStream.WriteLine(TS.ReadLine);

    finally
      SecStrs.Free;
      TS.Free;
      MS.Free;
    end;

  end
  else begin { no such section exists, append new one }
    FAnsiStream.Seek(0, soFromEnd);
    WriteSectionName(Section);
    WriteValue(Ident, Value);
    UpdateSections;
  end;

end;

procedure TStIniStream.WriteSectionName(const Section: String);
{ write section header at current location }
begin
  FAnsiStream.WriteLine('[' + Section + ']');
end;

procedure TStIniStream.WriteValue(const Key, Value: String);
{ write <Name>=<Value> pair at current location }
begin
  FAnsiStream.WriteLine(Key + '=' + Value);
end;

procedure TStIniStream.WriteSection(const Section: String;
  Strings: TStrings);
{ write entire section described by Strings }
var
  SecStrs : TStringList;
  SecIdx  : Integer;
  MS : TMemoryStream;
  TS : TStAnsiTextStream;
  i : Integer;
  L : LongInt;
  Name : String;
begin
  if not Assigned(Strings) then Exit;

  if SectionExists(Section) then begin
    SecStrs := TStringList.Create;
    MS := TMemoryStream.Create;
    TS := TStAnsiTextStream.Create(MS);

    try
      { locate and read section }
      GotoSection(Section);
      GetSecStrings(SecStrs);

      { locate subsequent section }
      SecIdx := FSections.IndexOf(Section);
      if SecIdx < Pred(FSections.Count) then begin
        GotoSection(FSections[SecIdx+1]);

        { copy remaining sections }
        while not FAnsiStream.AtEndOfStream do
          TS.WriteLine(FAnsiStream.ReadLine);
      end;
      { else this is the last section }

      { seek back and truncate }
      GotoSection(Section);
      FAnsiStream.Size := FAnsiStream.Position;
//      FAnsiStream.SetSize(FAnsiStream.Position);

      { update section }
      for i := 0 to Pred(Strings.Count) do begin
        Name := Strings.Names[i];
        if SecStrs.IndexOfName(Name) > -1 then { entry exists, change value }
          SecStrs.Values[Name] := Strings.Values[Name]
        else { new entry, just append it }
          SecStrs.Add(Strings[i]);
      end;

      { write updated section }
      WriteSectionName(Section);
      for i := 0 to Pred(SecStrs.Count) do
        FAnsiStream.WriteLine(SecStrs[i]);
      FAnsiStream.Stream.Seek(0, soFromEnd);

      { append saved subsequent sections }
      TS.SeekLine(0);
      while not TS.AtEndOfStream do
        FAnsiStream.WriteLine(TS.ReadLine);

    finally
      SecStrs.Free;
      TS.Free;
      MS.Free;
    end;

  end
  else begin { no such section exists, append new one }
    L := FAnsiStream.LineCount;
    FAnsiStream.Seek(0, soFromEnd);
    WriteSectionName(Section);
    FSections.AddObject(Section, TObject(L+1));
    for i := 0 to Pred(Strings.Count) do
      FAnsiStream.WriteLine(Strings[i]);
//    UpdateSections;
  end;
end;

end.
