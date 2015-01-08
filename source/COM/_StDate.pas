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
{*                   _STDATE.PAS 3.00                    *}
{*********************************************************}

{$I STDEFINE.INC}
{$I STCOMDEF.INC}
unit _StDate;

interface

uses
  ComObj, StDate, SysTools_TLB, StdVcl;

type
  TStDate = class(TAutoObject, IStDate)
   private
    FIsLicensed : Boolean;
   public
    procedure Initialize; override;
   protected {Protected declarations }
    { IStDate - Methods }
    function AstJulianDate(Julian: TDateTime): Double; safecall;
    function AstJulianDatePrim(Year, Month, Day: Integer; UT: TDateTime): Double; safecall;
    function AstJulianDateToStDate(AstJD: Double; Truncate: WordBool): TDateTime; safecall;
    function BondDateDiff(Date1, Date2: TDateTime; DayBasis: TStBondDateType): TDateTime; safecall;
    function CurrentDate: TDateTime; safecall;
    function CurrentDateString(const Picture: WideString; Pack: WordBool): WideString; safecall;
    function CurrentTime: TDateTime; safecall;
    function CurrentTimeString(const Picture: WideString; Pack: WordBool): WideString; safecall;
    procedure DateDiff(Date1, Date2: TDateTime; var Days, Months, Years: Integer); safecall;
    function DateStringHMStoAstJD(const Picture, DS: WideString; Hours, Minutes, Seconds, Epoch: Integer): Double; safecall;
    function DateStringToDMY(const Picture, S: WideString; var Day, Month, Year: Integer; Epoch: Integer): WordBool; safecall;
    function DateStringToStDate(const Picture, S: WideString; Epoch: Integer): TDateTime; safecall;
    procedure DateTimeDiff(DT1, DT2: TDateTime; var Days, Seconds: Integer); safecall;
    function DayOfWeek(Julian: TDateTime): TStDayType; safecall;
    function DayOfWeekDMY(Day, Month, Year, Epoch: Integer): TStDayType; safecall;
    function DayOfWeekToString(WeekDay: TStDayType): WideString; safecall;
    function DaysInMonth(Month, Year, Epoch: Integer): Integer; safecall;
    function DecTime(T: TDateTime; Hours, Minutes, Seconds: Byte): TDateTime; safecall;
    function DMYtoStDate(Day, Month, Year, Epoch: Integer): TDateTime; safecall;
    function DMYtoDateString(const Picture: WideString; Day, Month, Year, Epoch: Integer; Pack: WordBool): WideString; safecall;
    function HMStoStTime(Hours, Minutes, Seconds: Byte): TDateTime; safecall;
    function IncDate(Julian: TDateTime; Days, Months, Years: Integer): TDateTime; safecall;
    procedure IncDateTime(DT1: TDateTime; var DT2: TDateTime; Days,
      Seconds: Integer); safecall;
    function IncDateTrunc(Julian: TDateTime; Months, Years: Integer): TDateTime; safecall;
    function IncTime(T: TDateTime; Hours, Minutes, Seconds: Byte): TDateTime; safecall;
    function InternationalDate(ForceCentury: WordBool): WideString; safecall;
    function InternationalLongDate(ShortNames, ExcludeDOW: WordBool): WideString; safecall;
    function InternationalTime(ShowSeconds: WordBool): WideString; safecall;
    function IsLeapYear(Year: Integer): WordBool; safecall;
    function MonthToString(Month: Integer): WideString; safecall;
    function RoundToNearestHour(T: TDateTime; Truncate: WordBool): TDateTime; safecall;
    function RoundToNearestMinute(T: TDateTime; Truncate: WordBool): TDateTime; safecall;
    function StDateToDateString(const Picture: WideString; Julian: TDateTime; Pack: WordBool): WideString; safecall;
    procedure StDateToDMY(Julian: TDateTime; var Day, Month, Year: Integer);  safecall;
    function StTimeToAmPmString(const Picture: WideString; T: TDateTime; Pack: WordBool): WideString; safecall;
    procedure StTimeToHMS(T: TDateTime; var Hours, Minutes, Seconds: Byte);  safecall;
    function StTimeToTimeString(const Picture: WideString; T: TDateTime; Pack: WordBool): WideString; safecall;
    procedure TimeDiff(T1, T2: TDateTime; var Hours, Minutes, Seconds: Byte); safecall;
    function TimeStringToHMS(const Picture, TS: WideString; var Hours, Minutes, Seconds: Integer): WordBool; safecall;
    function TimeStringToStTime(const Picture, S: WideString): TDateTime; safecall;
    function ValidDate(Day, Month, Year, Epoch: Integer): WordBool; safecall;
    function ValidTime(Hours, Minutes, Seconds: Integer): WordBool; safecall;
    function WeekOfYear(Julian: TDateTime): Byte; safecall;
    function License(const Key: WideString): WordBool; safecall;
  end;

implementation

uses ComServ, StDateSt {$IFDEF LICENSE}, ActiveX, StComLic {$ENDIF};

procedure TStDate.Initialize;
begin
  inherited Initialize;
  {$IFDEF LICENSE}
  FIsLicensed := False;
  {$ELSE}
  FIsLicensed := True;
  {$ENDIF}
end;

{ ********** TStDate Methods ********************************************************** }
function TStDate.AstJulianDate(Julian: TDateTime): Double;
begin
  {$IFDEF LICENSE}
   if (not FIsLicensed) or (not COMHasBeenLicensed) then
     OleError(CLASS_E_NOTLICENSED);
  {$ENDIF}
  Result := StDate.AstJulianDate(StDate.DateTimeToStDate(Julian));
end;

function TStDate.AstJulianDatePrim(Year, Month, Day: Integer;
  UT: TDateTime): Double;
begin
  {$IFDEF LICENSE}
   if (not FIsLicensed) or (not COMHasBeenLicensed) then
     OleError(CLASS_E_NOTLICENSED);
  {$ENDIF}
  Result := StDate.AstJulianDatePrim(Year, Month, Day, StDate.DateTimeToStTime(UT));
end;

function TStDate.AstJulianDateToStDate(AstJD: Double;
  Truncate: WordBool): TDateTime;
begin
  {$IFDEF LICENSE}
   if (not FIsLicensed) or (not COMHasBeenLicensed) then
     OleError(CLASS_E_NOTLICENSED);
  {$ENDIF}
  Result := StDate.StDateToDateTime(StDate.AstJulianDatetoStDate(AstJD, Truncate));
end;

function TStDate.BondDateDiff(Date1, Date2: TDateTime;
  DayBasis: TStBondDateType): TDateTime;
begin
  {$IFDEF LICENSE}
   if (not FIsLicensed) or (not COMHasBeenLicensed) then
     OleError(CLASS_E_NOTLICENSED);
  {$ENDIF}
  Result := StDate.StDateToDateTime(StDate.BondDateDiff(StDate.DateTimeToStDate(Date1),
                                                        StDate.DateTimeToStDate(Date2),
                                                        StDate.TStBondDateType(DayBasis)));
end;

function TStDate.CurrentDate: TDateTime;
begin
  {$IFDEF LICENSE}
   if (not FIsLicensed) or (not COMHasBeenLicensed) then
     OleError(CLASS_E_NOTLICENSED);
  {$ENDIF}
  Result := StDate.StDateToDateTime(StDate.CurrentDate);
end;

function TStDate.CurrentDateString(const Picture: WideString;
  Pack: WordBool): WideString;
begin
  {$IFDEF LICENSE}
   if (not FIsLicensed) or (not COMHasBeenLicensed) then
     OleError(CLASS_E_NOTLICENSED);
  {$ENDIF}
  Result := StDateSt.CurrentDateString(Picture, Pack);
end;

function TStDate.CurrentTime: TDateTime;
begin
  {$IFDEF LICENSE}
   if (not FIsLicensed) or (not COMHasBeenLicensed) then
     OleError(CLASS_E_NOTLICENSED);
  {$ENDIF}
  Result := StDate.StTimeToDateTime(StDate.CurrentTime);
end;

function TStDate.CurrentTimeString(const Picture: WideString;
  Pack: WordBool): WideString;
begin
  {$IFDEF LICENSE}
   if (not FIsLicensed) or (not COMHasBeenLicensed) then
     OleError(CLASS_E_NOTLICENSED);
  {$ENDIF}
  Result := StDateSt.CurrentTimeString(Picture, Pack);
end;

procedure TStDate.DateDiff(Date1, Date2: TDateTime; var Days, Months,
  Years: Integer);
begin
  {$IFDEF LICENSE}
   if (not FIsLicensed) or (not COMHasBeenLicensed) then
     OleError(CLASS_E_NOTLICENSED);
  {$ENDIF}
  StDate.DateDiff(StDate.DateTimeToStDate(Date1),
                  StDate.DateTimeToStDate(Date2), Days, Months, Years);
end;

function TStDate.DateStringHMStoAstJD(const Picture, DS: WideString; Hours,
  Minutes, Seconds, Epoch: Integer): Double;
begin
  {$IFDEF LICENSE}
   if (not FIsLicensed) or (not COMHasBeenLicensed) then
     OleError(CLASS_E_NOTLICENSED);
  {$ENDIF}
  Result := StDateSt.DateStringHMStoAstJD(Picture, DS, Hours, Minutes, Seconds, Epoch);
end;

function TStDate.DateStringToDMY(const Picture, S: WideString; var Day,
  Month, Year: Integer; Epoch: Integer): WordBool;
begin
  {$IFDEF LICENSE}
   if (not FIsLicensed) or (not COMHasBeenLicensed) then
     OleError(CLASS_E_NOTLICENSED);
  {$ENDIF}
  Result := StDateSt.DateStringToDMY(Picture, S, Epoch, Day, Month, Year);
end;

function TStDate.DateStringToStDate(const Picture, S: WideString;
  Epoch: Integer): TDateTime;
begin
  {$IFDEF LICENSE}
   if (not FIsLicensed) or (not COMHasBeenLicensed) then
     OleError(CLASS_E_NOTLICENSED);
  {$ENDIF}
  Result := StDate.StDateToDateTime(StDateSt.DateStringToStDate(Picture, S, Epoch));
end;

procedure TStDate.DateTimeDiff(DT1, DT2: TDateTime; var Days,
  Seconds: Integer);
var
  DTR1, DTR2 : StDate.TStDateTimeRec;
begin
  {$IFDEF LICENSE}
   if (not FIsLicensed) or (not COMHasBeenLicensed) then
     OleError(CLASS_E_NOTLICENSED);
  {$ENDIF}
  DTR1.D := StDate.DateTimeToStDate(DT1);
  DTR1.T := StDate.DateTimeToStTime(DT1);

  DTR2.D := StDate.DateTimeToStDate(DT2);
  DTR2.T := StDate.DateTimeToStTime(DT2);

  StDate.DateTimeDiff(DTR1, DTR2, Days, Seconds);
end;

function TStDate.DayOfWeek(Julian: TDateTime): TStDayType;
begin
  {$IFDEF LICENSE}
   if (not FIsLicensed) or (not COMHasBeenLicensed) then
     OleError(CLASS_E_NOTLICENSED);
  {$ENDIF}
  Result := TStDayType(StDate.DayOfWeek(StDate.DateTimeToStDate(Julian)));
end;

function TStDate.DayOfWeekDMY(Day, Month, Year,
  Epoch: Integer): TStDayType;
begin
  {$IFDEF LICENSE}
   if (not FIsLicensed) or (not COMHasBeenLicensed) then
     OleError(CLASS_E_NOTLICENSED);
  {$ENDIF}
  Result := TStDayType(StDate.DayOfWeekDMY(Day, Month, Year, Epoch));
end;

function TStDate.DayOfWeekToString(WeekDay: TStDayType): WideString;
begin
  {$IFDEF LICENSE}
   if (not FIsLicensed) or (not COMHasBeenLicensed) then
     OleError(CLASS_E_NOTLICENSED);
  {$ENDIF}
  Result := StDateSt.DayOfWeekToString(StDate.TStDayType(WeekDay));
end;

function TStDate.DaysInMonth(Month, Year, Epoch: Integer): Integer;
begin
  {$IFDEF LICENSE}
   if (not FIsLicensed) or (not COMHasBeenLicensed) then
     OleError(CLASS_E_NOTLICENSED);
  {$ENDIF}
  Result := StDate.DaysInMonth(Month, Year, Epoch);
end;

function TStDate.DecTime(T: TDateTime; Hours, Minutes,
  Seconds: Byte): TDateTime;
begin
  {$IFDEF LICENSE}
   if (not FIsLicensed) or (not COMHasBeenLicensed) then
     OleError(CLASS_E_NOTLICENSED);
  {$ENDIF}
  Result := StDate.StTimeToDateTime(StDate.DecTime(StDate.DateTimeToStTime(T),
                                                   Hours,
                                                   Minutes,
                                                   Seconds));
end;

function TStDate.DMYtoStDate(Day, Month, Year, Epoch: Integer): TDateTime;
begin
  {$IFDEF LICENSE}
   if (not FIsLicensed) or (not COMHasBeenLicensed) then
     OleError(CLASS_E_NOTLICENSED);
  {$ENDIF}
  Result := StDate.StDateToDateTime(StDate.DMYtoStDate(Day, Month, Year, Epoch));
end;

function TStDate.DMYtoDateString(const Picture: WideString; Day, Month,
  Year, Epoch: Integer; Pack: WordBool): WideString;
begin
  {$IFDEF LICENSE}
   if (not FIsLicensed) or (not COMHasBeenLicensed) then
     OleError(CLASS_E_NOTLICENSED);
  {$ENDIF}
  Result := StDateSt.DMYtoDateString(Picture, Day, Month, Year, Epoch, Pack);
end;

function TStDate.HMStoStTime(Hours, Minutes, Seconds: Byte): TDateTime;
begin
  {$IFDEF LICENSE}
   if (not FIsLicensed) or (not COMHasBeenLicensed) then
     OleError(CLASS_E_NOTLICENSED);
  {$ENDIF}
  Result := StDate.StTimeToDateTime(StDate.HMStoStTime(Hours, Minutes, Seconds));
end;

function TStDate.IncDate(Julian: TDateTime; Days, Months,
  Years: Integer): TDateTime;
begin
  {$IFDEF LICENSE}
   if (not FIsLicensed) or (not COMHasBeenLicensed) then
     OleError(CLASS_E_NOTLICENSED);
  {$ENDIF}
  Result := StDate.StDateToDateTime(StDate.IncDate(StDate.DateTimeToStDate(Julian),
                                                   Days,
                                                   Months,
                                                   Years));
end;

procedure TStDate.IncDateTime(DT1: TDateTime; var DT2: TDateTime; Days,
  Seconds: Integer);
var
  DTR1, DTR2 : StDate.TStDateTimeRec;
begin
  {$IFDEF LICENSE}
   if (not FIsLicensed) or (not COMHasBeenLicensed) then
     OleError(CLASS_E_NOTLICENSED);
  {$ENDIF}
  DTR1.D := StDate.DateTimeToStDate(DT1);
  DTR1.T := StDate.DateTimeToStTime(DT1);

  StDate.IncDateTime(DTR1, DTR2, Days, Seconds);

  {!! This is not right }
  DT2 := DTR2.D + DTR2.T;
end;

function TStDate.IncDateTrunc(Julian: TDateTime; Months,
  Years: Integer): TDateTime;
begin
  {$IFDEF LICENSE}
   if (not FIsLicensed) or (not COMHasBeenLicensed) then
     OleError(CLASS_E_NOTLICENSED);
  {$ENDIF}
  Result := StDate.StDateToDateTime(StDate.IncDateTrunc(StDate.DateTimeToStDate(Julian),
                                                        Months,
                                                        Years));
end;

function TStDate.IncTime(T: TDateTime; Hours, Minutes,
  Seconds: Byte): TDateTime;
begin
  {$IFDEF LICENSE}
   if (not FIsLicensed) or (not COMHasBeenLicensed) then
     OleError(CLASS_E_NOTLICENSED);
  {$ENDIF}
  Result := StDate.StTimeToDateTime(StDate.IncTime(StDate.DateTimeToStTime(T),
                                                   Hours,
                                                   Minutes,
                                                   Seconds));
end;

function TStDate.InternationalDate(ForceCentury: WordBool): WideString;
begin
  {$IFDEF LICENSE}
   if (not FIsLicensed) or (not COMHasBeenLicensed) then
     OleError(CLASS_E_NOTLICENSED);
  {$ENDIF}
  Result := StDateSt.InternationalDate(ForceCentury);
end;

function TStDate.InternationalLongDate(ShortNames,
  ExcludeDOW: WordBool): WideString;
begin
  {$IFDEF LICENSE}
   if (not FIsLicensed) or (not COMHasBeenLicensed) then
     OleError(CLASS_E_NOTLICENSED);
  {$ENDIF}
  Result := StDateSt.InternationalLongDate(ShortNames, ExcludeDOW);
end;

function TStDate.InternationalTime(ShowSeconds: WordBool): WideString;
begin
  {$IFDEF LICENSE}
   if (not FIsLicensed) or (not COMHasBeenLicensed) then
     OleError(CLASS_E_NOTLICENSED);
  {$ENDIF}
  Result := StDateSt.InternationalTime(ShowSeconds);
end;

function TStDate.IsLeapYear(Year: Integer): WordBool;
begin
  {$IFDEF LICENSE}
   if (not FIsLicensed) or (not COMHasBeenLicensed) then
     OleError(CLASS_E_NOTLICENSED);
  {$ENDIF}
  Result := StDate.IsLeapYear(Year);
end;

function TStDate.MonthToString(Month: Integer): WideString;
begin
  {$IFDEF LICENSE}
   if (not FIsLicensed) or (not COMHasBeenLicensed) then
     OleError(CLASS_E_NOTLICENSED);
  {$ENDIF}
  Result := StDateSt.MonthToString(Month);
end;

function TStDate.RoundToNearestHour(T: TDateTime;
  Truncate: WordBool): TDateTime;
begin
  {$IFDEF LICENSE}
   if (not FIsLicensed) or (not COMHasBeenLicensed) then
     OleError(CLASS_E_NOTLICENSED);
  {$ENDIF}
  Result := StDate.StTimeToDateTime(StDate.RoundToNearestHour(StDate.DateTimeToStTime(T),
                                                              Truncate));
end;

function TStDate.RoundToNearestMinute(T: TDateTime;
  Truncate: WordBool): TDateTime;
begin
  {$IFDEF LICENSE}
   if (not FIsLicensed) or (not COMHasBeenLicensed) then
     OleError(CLASS_E_NOTLICENSED);
  {$ENDIF}
  Result := StDate.StTimeToDateTime(StDate.RoundToNearestMinute(StDate.DateTimeToStTime(T),
                                                                Truncate));
end;

function TStDate.StDateToDateString(const Picture: WideString;
  Julian: TDateTime; Pack: WordBool): WideString;
begin
  {$IFDEF LICENSE}
   if (not FIsLicensed) or (not COMHasBeenLicensed) then
     OleError(CLASS_E_NOTLICENSED);
  {$ENDIF}
  Result := StDateSt.StDateToDateString(Picture, StDate.DateTimeToStDate(Julian), Pack);
end;

procedure TStDate.StDateToDMY(Julian: TDateTime; var Day, Month,
  Year: Integer);
begin
  {$IFDEF LICENSE}
   if (not FIsLicensed) or (not COMHasBeenLicensed) then
     OleError(CLASS_E_NOTLICENSED);
  {$ENDIF}
  StDate.StDateToDMY(StDate.DateTimeToStDate(Julian), Day, Month, Year);
end;

function TStDate.StTimeToAmPmString(const Picture: WideString;
  T: TDateTime; Pack: WordBool): WideString;
begin
  {$IFDEF LICENSE}
   if (not FIsLicensed) or (not COMHasBeenLicensed) then
     OleError(CLASS_E_NOTLICENSED);
  {$ENDIF}
  StDateSt.StTimeToAmPmString(Picture, StDate.DateTimeToStTime(T), Pack);
end;

procedure TStDate.StTimeToHMS(T: TDateTime; var Hours, Minutes,
  Seconds: Byte);
begin
  {$IFDEF LICENSE}
   if (not FIsLicensed) or (not COMHasBeenLicensed) then
     OleError(CLASS_E_NOTLICENSED);
  {$ENDIF}
  StDate.StTimeToHMS(StDate.DateTimeToStTime(T), Hours, Minutes, Seconds);
end;

function TStDate.StTimeToTimeString(const Picture: WideString;
  T: TDateTime; Pack: WordBool): WideString;
begin
  {$IFDEF LICENSE}
   if (not FIsLicensed) or (not COMHasBeenLicensed) then
     OleError(CLASS_E_NOTLICENSED);
  {$ENDIF}
  Result := StDateSt.StTimeToTimeString(Picture, StDate.DateTimeToStTime(T), Pack);
end;

procedure TStDate.TimeDiff(T1, T2: TDateTime; var Hours, Minutes,
  Seconds: Byte);
begin
  {$IFDEF LICENSE}
   if (not FIsLicensed) or (not COMHasBeenLicensed) then
     OleError(CLASS_E_NOTLICENSED);
  {$ENDIF}
  StDate.TimeDiff(StDate.DateTimeToStTime(T1),
                  StDate.DateTimeToStTime(T2),
                  Hours,
                  Minutes,
                  Seconds);
end;

function TStDate.TimeStringToHMS(const Picture, TS: WideString; var Hours,
  Minutes, Seconds: Integer): WordBool;
begin
  {$IFDEF LICENSE}
   if (not FIsLicensed) or (not COMHasBeenLicensed) then
     OleError(CLASS_E_NOTLICENSED);
  {$ENDIF}
  Result := StDateSt.TimeStringToHMS(Picture, TS, Hours, Minutes, Seconds);
end;

function TStDate.TimeStringToStTime(const Picture,
  S: WideString): TDateTime;
begin
  {$IFDEF LICENSE}
   if (not FIsLicensed) or (not COMHasBeenLicensed) then
     OleError(CLASS_E_NOTLICENSED);
  {$ENDIF}
  Result := StDate.StTimeToDateTime(StDateSt.TimeStringToStTime(Picture, S));
end;

function TStDate.ValidDate(Day, Month, Year, Epoch: Integer): WordBool;
begin
  {$IFDEF LICENSE}
   if (not FIsLicensed) or (not COMHasBeenLicensed) then
     OleError(CLASS_E_NOTLICENSED);
  {$ENDIF}
  Result := StDate.ValidDate(Day, Month, Year, Epoch);
end;

function TStDate.ValidTime(Hours, Minutes, Seconds: Integer): WordBool;
begin
  {$IFDEF LICENSE}
   if (not FIsLicensed) or (not COMHasBeenLicensed) then
     OleError(CLASS_E_NOTLICENSED);
  {$ENDIF}
  Result := StDate.ValidTime(Hours, Minutes, Seconds);
end;

function TStDate.WeekOfYear(Julian: TDateTime): Byte;
begin
  {$IFDEF LICENSE}
   if (not FIsLicensed) or (not COMHasBeenLicensed) then
     OleError(CLASS_E_NOTLICENSED);
  {$ENDIF}
  Result := StDate.WeekOfYear(StDate.DateTimeToStDate(Julian));
end;

function TStDate.License(const Key: WideString): WordBool;
begin
  {$IFDEF LICENCE}
  Result := COMIsValidKey(Key);
  {$ELSE}
  Result := True;
  {$ENDIF}
  FIsLicensed := Result;
end;

initialization
  TAutoObjectFactory.Create(ComServer, TStDate, Class_StDate, ciMultiInstance, tmBoth);
end.
