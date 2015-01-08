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
{* SysTools: StDateSt.pas 4.04                           *}
{*********************************************************}
{* SysTools: Date and time string manipulation           *}
{*********************************************************}

{$I StDefine.inc}

unit StDateSt;

interface

uses
  Windows, SysUtils,
  StStrS,
  StStrL,
  StConst,
  StBase,
  StUtils,
  StDate;

const
  {the following characters are meaningful in date Picture strings}
  MonthOnly = 'm';      {Formatting character for a date string picture mask}
  DayOnly = 'd';        {Formatting character for a date string picture mask}
  YearOnly = 'y';       {Formatting character for a date string picture mask}
  MonthOnlyU = 'M';     {Formatting character for a date string picture mask.
  Uppercase means pad with ' ' rather than '0'}
  DayOnlyU = 'D';       {Formatting character for a date string picture mask.
  Uppercase means pad with ' ' rather then '0'}
  DateSlash = '/';      {Formatting character for a date string picture mask}

  {'n'/'N' may be used in place of 'm'/'M' when the name of the month is
   desired instead of its number. E.g., 'dd/nnn/yyyy' -\> '01-Jan-1980'.
   'dd/NNN/yyyy' -\> '01-JAN-1980' (if SlashChar = '-'). The abbreviation used
   is based on the width of the subfield (3 in the example) and the current
   contents of the MonthString array.}
  NameOnly = 'n';       {Formatting character for a date string picture mask}
  NameOnlyU = 'N';      {Formatting character for a date string picture mask.
  Uppercase causes the output to be in uppercase}

  {'w'/'W' may be used to include the day of the week in a date string. E.g.,
  'www dd nnn yyyy' -\> 'Mon 01 Jan 1989'. The abbreviation used is based on
  the width of the subfield (3 in the example) and the current contents of the
  DayString array. Note that TurboPower Entry Fields will not allow the user to
  enter text into a subfield containing 'w' or 'W'. The day of the week will be
  supplied automatically when a valid date is entered.}
  WeekDayOnly = 'w';    {Formatting character for a date string picture mask}
  WeekDayOnlyU = 'W';   {Formatting character for a date string picture mask.
  Uppercase causes the output to be in uppercase}

  LongDateSub1 = 'f';   {Mask character used strictly for dealing with Window's
  long date format}
  LongDateSub2 = 'g';   {Mask character used strictly for dealing with Window's
  long date format}
  LongDateSub3 = 'h';   {Mask character used strictly for dealing with Window's
  long date format}

  HourOnly = 'h';       {Formatting character for a time string picture mask}
  MinOnly = 'm';        {Formatting character for a time string picture mask}
  SecOnly = 's';        {Formatting character for a time string picture mask}
  {if uppercase letters are used, numbers are padded with ' ' rather than '0'}
  HourOnlyU = 'H';      {Formatting character for a time string picture mask.
  Uppercase means pad with ' ' rather than '0'}
  MinOnlyU = 'M';       {Formatting character for a time string picture mask.
  Uppercase means pad with ' ' rather than '0'}
  SecOnlyU = 'S';       {Formatting character for a time string picture mask.
  Uppercase means pad with ' ' rather than '0'}
  {'hh:mm:ss tt' -\> '12:00:00 pm', 'hh:mmt' -\> '12:00p'}
  TimeOnly = 't';       {Formatting character for a time string picture mask.
  This generates 'AM' or 'PM'}
  TimeColon = ':';      {Formatting character for a time string picture mask}


  {-------julian date routines---------------}

function DateStringHMStoAstJD(const Picture, DS : string;              {!!.02}
  H,M,S,Epoch : integer) : Double;
  {-Returns the Astronomical Julian Date using a Date String,
  Hours, Minutes, Seconds}

function MonthToString(const Month : Integer) : string;
  {-Return the month as a string}

  {-------date string routines---------------}

function DateStringToStDate(const Picture, S : string; Epoch : Integer) : TStDate;
  {-Convert a string to a Julian date}

function DateStringToDMY(const Picture, S : string;
                         Epoch : Integer;
                         var D, M, Y : Integer) : Boolean;
  {-Extract day, month, and year from a date string}

function StDateToDateString(const Picture : string; const Julian : TStDate;
                            Pack : Boolean) : string;
  {-Convert a Julian date to a string}

function DayOfWeekToString(const WeekDay : TStDayType) : string;
  {-Return the day of the week specified by WeekDay as a string in Dest.}

function DMYtoDateString(const Picture : string;
                         Day, Month, Year, Epoch : Integer;
                         Pack : Boolean) : string;
  {-Merge the month, day, and year into the picture}

function CurrentDateString(const Picture : string; Pack : Boolean) : string;
  {-Return today's date as a string}

  {-------time routines---------------}

function CurrentTimeString(const Picture : string;
                           Pack : Boolean) : string;
  {-Return the current time as a string of the specified form}

function TimeStringToHMS(const Picture, St : string;
                         var H, M, S : Integer) : Boolean;
  {-Extract hours, minutes, seconds from a time string}

function TimeStringToStTime(const Picture, S : string) : TStTime;
  {-Convert a time string to a time variable}

function StTimeToAmPmString(const Picture : string;
                             const T : TStTime; Pack : Boolean) : string;
  {-Convert a time variable to a time string in am/pm format}

function StTimeToTimeString(const Picture : string; const T : TStTime;
                            Pack : Boolean) : string;
  {-Convert a time variable to a time string}


  {-------- routines for international date/time strings ---------}

function DateStringIsBlank(const Picture, S : string) : Boolean;
  {-Return True if the month, day, and year in S are all blank}

function InternationalDate(ForceCentury : Boolean) : string;
  {-Return a picture mask for a short date string, based on Windows' international
    information}

function InternationalLongDate(ShortNames : Boolean;
                               ExcludeDOW : Boolean) : string;
  {-Return a picture mask for a date string, based on Windows' international
    information}

function InternationalTime(ShowSeconds : Boolean) : string;
  {-Return a picture mask for a time string, based on Windows' international
    information}

procedure ResetInternationalInfo;
  {-Update internal info to match Windows' international info}


implementation

const
  First2Months = 59;           {1600 was a leap year}
  FirstDayOfWeek = Saturday;   {01/01/1600 was a Saturday}
  DateLen = 40;                {maximum length of Picture strings}
  MaxMonthName = 15;
  MaxDayName   = 15;

//type
{  DateString = string[DateLen];}
//  SString = string[255];

var
  wLongDate  : string;//[40];
  wldSub1    : string[6];  //SZ: careful if converting to string; some code depends on sizeof (search for [*] around line 1021)
  wldSub2    : string[6];
  wldSub3    : string[6];
  wShortDate : string;//[31];
  w1159      : string[7];
  w2359      : string[7];
  wSlashChar : Char;
  wColonChar : Char;
  wTLZero    : Boolean;
  w12Hour    : Boolean;
  DefaultYear : Integer;     {default year--used by DateStringToDMY}
  DefaultMonth : ShortInt;   {default month}

  procedure ExtractFromPicture(const Picture, S : string; Ch : Char; {!!.02}
    var I : Integer; Blank, Default : Integer); forward;

  procedure AppendChar(var S : String; Ch : Char);
  begin
    SetLength(S,Succ(Length(S)));
    S[Length(S)] := Ch;
  end;

  function DayOfWeekToString(const WeekDay : TStDayType) : string;
    {-Return the day of the week specified by WeekDay as a string in Dest.
    Will honor international names}
  begin
    Result := {$IFDEF DELPHIXE2}FormatSettings.{$ENDIF}LongDayNames[Ord(WeekDay)+1];
  end;

  function MonthToString(const Month : Integer) : string;
  {-Return the month as a string. Will honor international names}
  begin
    if (Month >= 1) and (Month <= 12) then
      Result := {$IFDEF DELPHIXE2}FormatSettings.{$ENDIF}LongMonthNames[Month]
    else
       Result := '';
  end;

  function AstJulianDatePrim(Year,Month,Date : Integer) : Double;
  var
    A, B : integer;
  begin
    if Month <= 2 then                                                  {!!.01}
    begin
      Dec(Year);
      Inc(Month,12);
    end;
    A := Trunc(Year/100);
    B := 2 - A + Trunc(A/4);

    Result := Trunc(365.25 * (Year+4716))
            + Trunc(30.6001 * (Month+1))
            + Date + B - 1524.5;
  end;

  function DateStringHMSToAstJD(const Picture, DS : string;            {!!.02}
    H,M,S,Epoch : Integer) : Double;
    {-Returns the Astronomical Julian Date using a Date String,
    Hours, Minutes, Seconds}
  var
    Date, Month, Year : Integer;
  begin
    ExtractFromPicture(Picture, DS, NameOnly, Month, -1, 0);
    if Month = 0 then
      ExtractFromPicture(Picture, DS, MonthOnly, Month, -1, DefaultMonth);
    ExtractFromPicture(Picture, DS, DayOnly, Date, -1, 1);
    ExtractFromPicture(Picture, DS, YearOnly, Year, -1, DefaultYear);

    Year := ResolveEpoch(Year, Epoch);
    Result := AstJulianDatePrim(Year,Month,Date)
            + H/HoursInDay + M/MinutesInDay + S/SecondsInDay;
  end;

  function MonthStringToMonth(const MSt : string; Width : Byte) : Byte;{!!.02}
    {-Convert the month name in MSt to a month (1..12)}
  var
    S   : String;
    T   : String;
    Len : Byte;
    I   : Word;
  begin
    S := UpperCase(MSt);
    Len := Length(S);
//    SetLength(S,Width);
//    if Width > Len then
//      FillChar(S[Len+1], Length(S)-Len, ' ');
    S := S + StringOfChar(' ', Width - Len);

    for I := 1 to 12 do begin
      T := UpperCase({$IFDEF DELPHIXE2}FormatSettings.{$ENDIF}LongMonthNames[I]);
      Len := Length(T);
//      SetLength(T,Width);
//      if Width > Len then
//        FillChar(T[Len+1], Length(T)-Len, ' ');
      T := T + StringOfChar(' ', Width - Len);

      if S = T then begin
        Result := I;
        Exit;
      end;
    end;
    Result := 0;
  end;

  procedure ExtractFromPicture(const Picture, S : string; Ch : Char; {!!.02}
    var I : Integer; Blank, Default : Integer);
    {-Extract the value of the subfield specified by Ch from S and return in
      I. I will be set to -1 in case of an error, Blank if the subfield exists
      in Picture but is empty, Default if the subfield doesn't exist in
      Picture.}
  var
    PTmp : string;
    C, posLCCh, posUCCh : Cardinal;
    Code : Integer;
  begin
    {find the start of the subfield}
    I := Default;

    StrChPosL(Picture, Ch, posLCCh);
    Ch := StBase.Upcase(Ch);
    StrChPosL(Picture, Ch, posUCCh);

    if (posLCCh < 1) or ((posUCCh > 0) and (posUCCh < posLCCh)) then
      posLCCh := posUCCh;
    if (posLCCh < 1)  or (Length(S) <> Length(Picture)) then
       Exit;

    {extract the substring}

    PTmp := '';
    C := Length(Picture);
    while (posLCCh <= C) and (StBase.Upcase(Picture[posLCCh]) = Ch) do begin
      if S[posLCCh] <> ' ' then
        AppendChar(PTmp,Char(S[posLCCh]));
      Inc(posLCCh);
    end;

    if Length(PTmp) = 0 then
      I := Blank
    else if Ch = NameOnlyU then begin
      I := MonthStringToMonth(PTmp, Length(PTmp));
      if I = 0 then
        I := -1;
    end
    else begin
      {convert to a value}
      Val(PTmp, I, Code);
      if Code <> 0 then
        I := -1;
    end;
  end;

  function DateStringToDMY(const Picture, S : string;
                           Epoch : Integer;
                           var D, M, Y : Integer) : Boolean;
    {-Extract day, month, and year from S, returning true if string is valid}
  begin
    ExtractFromPicture(Picture, S, NameOnly, M, -1, 0);
    if M = 0 then
      ExtractFromPicture(Picture, S, MonthOnly, M, -1, DefaultMonth);
    ExtractFromPicture(Picture, S, DayOnly, D, -1, 1);
    ExtractFromPicture(Picture, S, YearOnly, Y, -1, DefaultYear);
    if ValidDate(D, M, Y, Epoch) then begin                          
      Result := True;                                                
      Y := ResolveEpoch(Y, Epoch);                                   
    end else                                                         
      Result := False;                                               
  end;

  function DateStringIsBlank(const Picture, S : string) : Boolean;
    {-Return True if the month, day, and year in S are all blank}
  var
    M, D, Y : Integer;
  begin
    ExtractFromPicture(Picture, S, NameOnly,  M, -2, 0);
    if M = 0 then
      ExtractFromPicture(Picture, S, MonthOnly, M, -2, -2);
    ExtractFromPicture(Picture, S, DayOnly,   D, -2, -2);
    ExtractFromPicture(Picture, S, YearOnly,  Y, -2, -2);
    Result := (M = -2) and (D = -2) and (Y = -2);
  end;


  function DateStringToStDate(const Picture, S : string; Epoch : Integer) : TStDate;
    {-Convert S, a string of the form indicated by Picture, to a julian date.
      Picture and S must be of equal lengths}
  var
    Month, Day, Year : Integer;
  begin
    {extract day, month, year from S}
    if DateStringToDMY(Picture, S, Epoch, Day, Month, Year) then
      {convert to julian date}
      Result := DMYtoStDate(Day, Month, Year, Epoch)
    else
      Result := BadDate;
  end;

  function SubstCharSim(P : string; OC, NC : Char) : string;
  var
     step : integer;
  begin
    for step := 1 to Length(P) do
    begin
      if P[step] = OC then
        P[step] := NC;
    end;
    Result := P;
  end;

  function SubstChar(Picture : string; OldCh, NewCh : Char) : string;
    {-Replace all instances of OldCh in Picture with NewCh}
  var
    I    : Integer;
    UpCh : Char;
    P    : Cardinal;
  begin
    UpCh := StBase.Upcase(OldCh);
    if (StrChPosL(Picture,OldCh,P)) or (StrChPosL(Picture,UpCh,P)) then
      for I := 1 to Length(Picture) do
        if StBase.Upcase(Picture[I]) = UpCh then
          Picture[I] := NewCh;
    Result := Picture;
  end;

  function PackResult(const Picture, S : string) : string;             {!!.02}
    {-Remove unnecessary blanks from S}
  var
    step     : Integer;
  begin
    Result := '';

    for step := 1 to Length(Picture) do
    begin
      case Picture[step] of
        MonthOnlyU, DayOnlyU, NameOnly, NameOnlyU, WeekDayOnly,
        WeekDayOnlyU, HourOnlyU, SecOnlyU :
          if S[step] <> ' ' then
            AppendChar(Result,S[Step]);
        TimeOnly :
          if S[step] <> ' ' then
            AppendChar(Result,S[step]);
      else
        AppendChar(Result,S[step]);
      end;
    end;
  end;

  procedure MergeIntoPicture(var Picture : string; Ch : Char; I : Integer);
    {-Merge I into location in Picture indicated by format character Ch}
  var
    Tmp     : string;
    C,
    J, K, L : Cardinal;
    UCh,
    CPJ,
    CTI     : Char;
    OK, Done: Boolean;
    step  : Cardinal;
  begin
    {find the start of the subfield}
    OK := StrChPosL(Picture,Ch,J);
    UCh := StBase.Upcase(Ch);
    if (NOT OK) then
    begin
      if NOT (StrChPosL(Picture, UCh, J)) then
        Exit;
    end;

    {find the end of the subfield}
    K := J;
    C := Length(Picture);
    while (J <= C) and (StBase.Upcase(Picture[J]) = UCh) do
      Inc(J);
    Dec(J);

    if (UCh = WeekDayOnlyU) or (UCh = NameOnlyU) then begin
      if UCh = WeekDayOnlyU then
        case I of
          Ord(Sunday)..Ord(Saturday) :
            Tmp := {$IFDEF DELPHIXE2}FormatSettings.{$ENDIF}LongDayNames[I+1];
          else
            Tmp := '';
        end
      else
        case I of
          1..12 :
            Tmp := {$IFDEF DELPHIXE2}FormatSettings.{$ENDIF}LongMonthNames[I];
          else
            Tmp := '';
        end;
      K := Succ(J-K);
      if K > Length(Tmp) then
        for step := 1 to (K-Length(Tmp)) do
          Tmp := Tmp + ' ';
      Tmp := Copy(Tmp,1,K);
    end else
      {convert I to a string}
      Str(I:DateLen, Tmp);

    {now merge}
    L := Length(Tmp);
    Done := False;
    CPJ := Picture[J];

    while (stBase.Upcase(CPJ) = UCh) and not Done do
    begin
      CTI := Tmp[L];
      if (UCh = NameOnlyU) or (UCh = WeekDayOnlyU) then
      begin
        case CPJ of
          NameOnlyU, WeekDayOnlyU :
            CTI := stBase.Upcase(CTI);
        end;
      end
      else{change spaces to 0's if desired}
        if (CPJ >= 'a') and (CTI = ' ') then
          CTI := '0';
      Picture[J] := CTI;
      Done := (J = 1) or (L = 0);
      if not Done then
      begin
        Dec(J);
        Dec(L);
      end;
      CPJ := Picture[J];
    end;
  end;


  procedure MergePictureSt(const Picture : string; var P : string;     {!!.02}
                          MC : Char; const SP : string);           {!!.02}
  var
    I, J : Cardinal;
    L    : Cardinal;
  begin
    if NOT (StrChPosL(Picture,MC,I)) then
       Exit;
    J := 1;
    L := Length(SP);
    while Picture[I] = MC do begin
      {if J <= Length(SP) then}
      if (L = 0) or (J > L) then
        P[I] := ' '
      else begin
        P[I] := SP[J];
        Inc(J);
      end;
      Inc(I);
    end;
  end;


  function DMYtoDateString(const Picture : string; Day, Month, Year, Epoch : Integer;
                           Pack : Boolean) : string;
    {-Merge the month, day, and year into the picture}
  var
    DOW  : Integer;

  begin
    Result := Picture;

    Year := ResolveEpoch(Year, Epoch);

    DOW := Integer( DayOfWeekDMY(Day, Month, Year, 0) );
    MergeIntoPicture(Result, MonthOnly,   Month);
    MergeIntoPicture(Result, DayOnly,     Day);
    MergeIntoPicture(Result, YearOnly,    Year);
    MergeIntoPicture(Result, NameOnly,    Month);
    MergeIntoPicture(Result, WeekDayOnly, DOW);

    {map slashes}
    Result := SubstChar(Result, DateSlash, wSlashChar);

    MergePictureSt(Picture, Result, LongDateSub1, wldSub1);
    MergePictureSt(Picture, Result, LongDateSub2, wldSub2);
    MergePictureSt(Picture, Result, LongDateSub3, wldSub3);

    if Pack then
      Result:= PackResult(Picture, Result);
  end;

  function StDateToDateString(const Picture : string; const Julian : TStDate;
                              Pack : Boolean) : string;
    {-Convert Julian to a string of the form indicated by Picture}
  var
    Month, Day, Year : Integer;
  begin
    Result := Picture;
    if (Julian = BadDate) or (Julian > MaxDate) then begin             {!!.04}
      {map picture characters to spaces}
      Result := SubstChar(Result, MonthOnly,   ' ');
      Result := SubstChar(Result, NameOnly,    ' ');
      Result := SubstChar(Result, DayOnly,     ' ');
      Result := SubstChar(Result, YearOnly,    ' ');
      Result := SubstChar(Result, WeekDayOnly, ' ');

      MergePictureSt(Picture, Result, LongDateSub1, wldSub1);
      MergePictureSt(Picture, Result, LongDateSub2, wldSub2);
      MergePictureSt(Picture, Result, LongDateSub3, wldSub3);

      {map slashes}
      Result := SubstChar(Result, DateSlash, wSlashChar);
    end
    else begin
      {convert Julian to day/month/year}
      StDateToDMY(Julian, Day, Month, Year);

      {merge the month, day, and year into the picture}
      Result := DMYtoDateString(Picture, Day, Month, Year, 0, Pack);
    end;
  end;

  function CurrentDateString(const Picture : string; Pack : Boolean) : string;
    {-Returns today's date as a string of the specified form}
  begin
    Result := StDateToDateString(Picture, CurrentDate, Pack);
  end;

  function TimeStringToHMS(const Picture, St : string; var H, M, S : Integer) : Boolean;
    {-Extract Hours, Minutes, Seconds from St, returning true if string is valid}
  var
    I,
    J      : Cardinal;
    Tmp,
    t1159,
    t2359  : string;
  begin
    {extract hours, minutes, seconds from St}
    ExtractFromPicture(Picture, St, HourOnly, H, -1, 0);
    ExtractFromPicture(Picture, St, MinOnly,  M, -1, 0);
    ExtractFromPicture(Picture, St, SecOnly,  S, -1, 0);
    if (H = -1) or (M = -1) or (S = -1) then begin
      Result := False;
      Exit;
    end;

    {check for TimeOnly}
    if (StrChPosL(Picture, TimeOnly, I)) and
       (Length(w1159) > 0) and (Length(w2359) > 0) then begin

      Tmp := '';
      J := 1;
      while (I <= Cardinal(Length(Picture))) and (Picture[I] = TimeOnly) do begin{!!.02}
  //      while (Picture[I] = TimeOnly) do begin
        //SZ Inc(Tmp[0]);
        //SZ Tmp[J] := St[I];
        Tmp := Tmp + St[I];
        Inc(J);
        Inc(I);
      end;
      Tmp := TrimRight(Tmp);

      t1159 := w1159;
      t2359 := w2359;
      if (Length(Tmp) = 0) then
         H := -1
      else if (UpperCase(Tmp) = UpperCase(t2359)) then begin
        if (H < 12) then
          Inc(H,12)
        else if (H=0) or (H > 12) then
          {force BadTime}
          H := -1;
      end
      else if (UpperCase(Tmp) = UpperCase(t1159)) then begin
        if H = 12 then
          H := 0
        else if (H = 0) or (H > 12) then
          {force BadTime}
          H := -1;
      end
      else
        {force BadTime}
        H := -1;
      end;
    Result := ValidTime(H, M, S);
  end;

  function TimeStringToStTime(const Picture, S : string) : TStTime;
    {-Convert S, a string of the form indicated by Picture, to a Time variable}
  var
    Hours, Minutes, Seconds : Integer;
  begin
    if TimeStringToHMS(Picture, S, Hours, Minutes, Seconds) then
      Result := HMStoStTime(Hours, Minutes, Seconds)
    else
      Result := BadTime;
  end;

  function TimeToTimeStringPrim(const Picture : string; T : TStTime;   {!!.02}
                                Pack : Boolean;
                                const t1159, t2359 : string) : string; {!!.02}
    {-Convert T to a string of the form indicated by Picture}
  var
    Hours,
    Minutes,
    Seconds  : Byte;
    L, I,
    TPos     : Cardinal;
    P        : string;
    OK       : Boolean;
    C        : string;//[1];
  begin
    {merge the hours, minutes, and seconds into the picture}
    StTimeToHMS(T, Hours, Minutes, Seconds);
    Result := Picture;

    {check for TimeOnly}
    OK := StrChPosL(Result, TimeOnly, TPos);
    if OK then begin
      if (Hours >= 12) then
        P := t2359
      else
        P := t1159;
      if (Length(t1159) > 0) and (Length(t2359) > 0) then
        case Hours of
          0 : Hours := 12;
          13..23 : Dec(Hours, 12);
        end;
    end;

    if T = BadTime then begin
      {map picture characters to spaces}
      Result := SubstChar(Result, HourOnly, ' ');
      Result := SubstChar(Result, MinOnly, ' ');
      Result := SubstChar(Result, SecOnly, ' ');
    end
    else begin
      {merge the numbers into the picture}
      MergeIntoPicture(Result, HourOnly, Hours);
      MergeIntoPicture(Result, MinOnly, Minutes);
      MergeIntoPicture(Result, SecOnly, Seconds);
    end;

    {map colons}
    Result := SubstChar(Result, TimeColon, wColonChar);

    {plug in AM/PM string if appropriate}
    if OK then begin
      if (Length(t1159) = 0) and (Length(t2359) = 0) then begin
        C := SubstCharSim(Result[TPos], TimeOnly, ' ');
        Result[TPos] := C[1];
      end else if (T = BadTime) and (Length(t1159) = 0) then begin
        C := SubstCharSim(Result[TPos], TimeOnly, ' ');
        Result[TPos] := C[1];
      end else begin
        I := 1;
        L := Length(P);
  //        while (I <= L) and (Result[TPos] = TimeOnly) do begin            {!!.01} {!!.03}
        while (I <= L) and                                             {!!.03}
          (TPos <= Length(Result)) and (Result[TPos] = TimeOnly) do    {!!.03}
        begin                                                          {!!.03}
          Result[TPos] := P[I];
          Inc(I);
          Inc(TPos);
        end;
      end;
    end;

    if Pack and (T <> BadTime) then
      Result := PackResult(Picture, Result);
  end;

  function StTimeToTimeString(const Picture : string; const T : TStTime;
                              Pack : Boolean) : string;
    {-Convert T to a string of the form indicated by Picture}
  begin
    Result := TimeToTimeStringPrim(Picture, T, Pack, w1159, w2359);
  end;

  function StTimeToAmPmString(const Picture : string; const T : TStTime;
                            Pack : Boolean) : string;
    {-Convert T to a string of the form indicated by Picture. Times are always
      displayed in am/pm format.}
  const
    t1159 = 'AM';
    t2359 = 'PM';
  var
    P    : Cardinal;
  begin
    Result := Picture;
    if NOT (StrChPosL(Result, TimeOnly, P)) then
      Result := Result + TimeOnly;
    Result := TimeToTimeStringPrim(Result, T, Pack, t1159, t2359);
  end;

  function CurrentTime : TStTime;
    {-Returns current time in seconds since midnight}
  begin
    Result := Trunc(SysUtils.Time * SecondsInDay);
  end;

  function CurrentTimeString(const Picture : string; Pack : Boolean) : string;
    {-Returns current time as a string of the specified form}
  begin
    Result := StTimeToTimeString(Picture, CurrentTime, Pack);
  end;

  function MaskCharCount(const P : string; MC : Char) : Integer;   {!!.02}
  var
    I, R,
    Len  : Cardinal;
    OK   : Boolean;
  begin
    OK := StrChPosL(P, MC, I);
    R := Ord(OK);
    Len := Length(P);
    if OK then
      while (I+R <= Len) and (P[I+R] = MC) do                            {!!.01}
        Inc(R);
    Result := R;
  end;

  function InternationalDate(ForceCentury : Boolean) : string;
    {-Return a picture mask for a date string, based on Windows' int'l info}

    procedure FixMask(MC : Char; DL : Integer);
    var
      I, J, AL, D : Cardinal;
      MCT : Char;
      OK  : Boolean;
    begin
      {find number of matching characters}
      OK := StrChPosL(Result, MC, I);
      MCT := MC;
      if not OK then begin
        MCT := StBase.UpCase(MC);
        OK := StrChPosL(Result, MCT, I);
      end;
      if NOT OK then
        Exit;

      D := DL;
      {pad substring to desired length}
      AL := MaskCharCount(Result, MCT);
      if AL < D then
        for J := 1 to D-AL do
          Result := StrChInsertL(Result, MCT, I);

      if MC <> YearOnly then begin
        {choose blank/zero padding}
        case AL of
          1 : if MCT = MC then
                Result := SubstCharSim(Result, MCT, StBase.UpCase(MCT));
          2 : if MCT <> MC then
                Result := SubstCharSim(Result, MCT, MC);
        end;
      end;
    end;

  begin
    {copy Windows mask into our var}
    Result := wShortDate;

    {if single Day marker, make double}
    FixMask(DayOnly, 2);

    {if single Month marker, make double}
    FixMask(MonthOnly, 2);

    {force yyyy if desired}
    FixMask(YearOnly, 2 shl Ord(ForceCentury));
  end;


  function InternationalLongDate(ShortNames : Boolean;
                                 ExcludeDOW : Boolean) : string;
    {-Return a picture mask for a date string, based on Windows' int'l info}
  var
    I, WC : Cardinal;
    OK,
    Stop : Boolean;
    Temp : string[81];

    function LongestMonthName : Integer;
    var
      L, I : Integer;
    begin
      L := 0;
      for I := 1 to 12 do
        L := Maxword(L, Length({$IFDEF DELPHIXE2}FormatSettings.{$ENDIF}LongMonthNames[I]));
      LongestMonthName := L;
    end;

    function LongestDayName : Integer;
    var
      D : TStDayType;
      L : Integer;
    begin
      L := 0;
      for D := Sunday to Saturday do
        L := Maxword(L, Length({$IFDEF DELPHIXE2}FormatSettings.{$ENDIF}LongDayNames[Ord(D)+1]));
      LongestDayName := L;
    end;

    procedure FixMask(MC : Char; DL : Integer);
    var
      I, J, AL, D : Cardinal;                                          
      MCT : Char;
    begin
      {find first matching mask character}
      OK := StrChPosS(Temp, MC, I);
      MCT := MC;
      if NOT OK then begin
        MCT := StBase.UpCase(MC);
        OK := StrChPosS(Temp, MCT, I);
      end;
      if NOT OK then
        Exit;

      D := DL;                                                         
      {pad substring to desired length}
      AL := MaskCharCount(Temp, MCT);
      if AL < D then begin
        for J := 1 to D-AL do                                          
          Temp := StrChInsertL(Temp, MCT, I);
      end else if (AL > D) then
        Temp := StrStDeleteL(Temp, I, AL-D);

      if MC <> YearOnly then
        {choose blank/zero padding}
        case AL of
          1 : if MCT = MC then
                Temp := SubstCharSim(Temp, MCT, StBase.UpCase(MCT));
          2 : if MCT <> MC then
                Temp := SubstCharSim(Temp, MCT, MC);
        end;
    end;

  begin
    {copy Windows mask into temporary var}
    Temp := wLongDate;

    if ExcludeDOW then begin
      {remove day-of-week and any junk that follows}
      if (StrChPosS(Temp, WeekDayOnly,I)) then begin
        Stop := False;
        WC := I+1;
        while (WC <= Length(Temp)) AND (NOT Stop) do
        begin
          if LoCase(Temp[WC]) in [MonthOnly,DayOnly,YearOnly,NameOnly] then
            Stop := TRUE
          else
            Inc(WC);
        end;
        if (NOT ShortNames) then
          Dec(WC);
        Temp := StrStDeleteS(Temp, I, WC);
      end;
    end
    else if ShortNames then
      FixMask(WeekDayOnly, 3)
    else if MaskCharCount(Temp, WeekdayOnly) = 4 then
      FixMask(WeekDayOnly, LongestDayName);

    {fix month names}
    if ShortNames then
      FixMask(NameOnly, 3)
    else if MaskCharCount(Temp, NameOnly) = 4 then
      FixMask(NameOnly, LongestMonthName);

    {if single Day marker, make double}
    FixMask(DayOnly, 2);

    {if single Month marker, make double}
    FixMask(MonthOnly, 2);

    {force yyyy}
    FixMask(YearOnly, 4);

    Result := Temp;
  end;

  function InternationalTime(ShowSeconds : Boolean) : string;
    {-Return a picture mask for a time string, based on Windows' int'l info}
  var
    ML,
    I  : Integer;
  begin
    {format the default string}

    SetLength(Result,21);
    Result := 'hh:mm:ss';
    if not wTLZero then
       Result[1] :=  HourOnlyU;

    {show seconds?}
    if not ShowSeconds then
      SetLength(Result,5);

    {handle international AM/PM markers}
    if w12Hour then begin
      ML := Maxword(Length(w1159), Length(w2359));
      if (ML <> 0) then begin
        AppendChar(Result,' ');
        for I := 1 to ML do
          AppendChar(Result, TimeOnly);
      end;
    end;
  end;

  procedure SetDefaultYear;
    {-Initialize DefaultYear and DefaultMonth}
  var
    Month, Day : Word;
    T : TDateTime;
    W : Word;
  begin
    T := Now;
    W := DefaultYear;
    DecodeDate(T,W,Month,Day);
    DefaultYear := W;
    DefaultMonth := Month;
  end;

  procedure ResetInternationalInfo;
  var
    I : Integer;
    S : array[0..20] of char;

    procedure ExtractSubString(SubChar : Char; Dest : string);
    var
      I, L, P : Cardinal;
    begin
//      SetLength(Dest,sizeof(wldSub1));
//      FillChar(Dest[1], SizeOf(wldSub1), 0);
      Dest := StringOfChar(#0, Succ(High(wldSub1))); //SZ: not length! [*]
      if NOT (StrChPosS(wLongDate, '''',I)) then
        Exit;

      {delete the first quote}
      wLongDate := StrChDeleteS(wLongDate, I);

      {assure that there is another quote}
      if NOT (StrChPosS(wLongDate, '''',P)) then
        Exit;

      {copy substring into Dest, replace substring with SubChar}
      L := 1;
      while wLongDate[I] <> '''' do
        if L < SizeOf(wldSub1) then begin
          Dest[L] := wLongDate[I];
          Inc(L);
          wLongDate[I] := SubChar;
          Inc(I);
        end else
          wLongDate := StrChDeleteL(wLongDate, I);

      {delete the second quote}
      wLongDate := StrChDeleteL(wLongDate, I);
    end;

  begin
    wTLZero := {$IFDEF DELPHIXE2}FormatSettings.{$ENDIF}LongTimeFormat[2] = 'h';
    w12Hour := {$IFDEF DELPHIXE2}FormatSettings.{$ENDIF}LongTimeFormat[length({$IFDEF DELPHIXE2}FormatSettings.{$ENDIF}LongTimeFormat)] = 'M';

    wColonChar := {$IFDEF DELPHIXE2}FormatSettings.{$ENDIF}TimeSeparator;
    wSlashChar := {$IFDEF DELPHIXE2}FormatSettings.{$ENDIF}DateSeparator;

    GetProfileString('intl','s1159','AM', S, Length(S));
    w1159 := StrPas(S);
    GetProfileString('intl','s2359','PM', S, Length(S));
    w2359 := StrPas(S);

    {get short date mask and fix it up}
    wShortDate := {$IFDEF DELPHIXE2}FormatSettings.{$ENDIF}ShortDateFormat;
    for I := 1 to Length(wShortDate) do
      if (wShortDate[I] = wSlashChar) then
        wShortDate[I] := '/';

    {get long date mask and fix it up}
    wLongDate := {$IFDEF DELPHIXE2}FormatSettings.{$ENDIF}LongDateFormat;
    ExtractSubString(LongDateSub1, wldSub1);
    ExtractSubString(LongDateSub2, wldSub2);
    ExtractSubString(LongDateSub3, wldSub3);

    {replace ddd/dddd with www/wwww}
    I := pos('ddd',wLongDate);
    if I > 0 then begin
      while wLongDate[I] = 'd' do begin
        wLongDate[I] := 'w';
        Inc(I);
      end;
    end;

    {replace MMM/MMMM with nnn/nnnn}
    if pos('MMM',wLongDate) > 0 then
      while (pos('M',wLongDate) > 0) do
        wLongDate[pos('M',wLongDate)] := 'n';

    {deal with oddities concerning . and ,}
    for I := 1 to Length(wLongDate)-1 do begin
      case wLongDate[I] of
        '.', ',' :
          if wLongDate[I+1] <> ' ' then
            wLongDate := StrChInsertS(wLongDate, ' ', I+1);
      end;
    end;
  end;


initialization
  {initialize DefaultYear and DefaultMonth}
  SetDefaultYear;
  ResetInternationalInfo;
end.
