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
{* SysTools: StMoney.pas 4.04                            *}
{*********************************************************}
{* SysTools: Currency and Money Related Classes          *}
{*********************************************************}

{$include StDefine.inc}

unit StMoney;

interface

uses
  Windows, SysUtils, Classes,

  StConst, StBase, StStrms, StDecMth, StIniStm;


type
{
; Layout of currency entries
[ISOCode]
Name=Country-Currency Name
ISOName=<ISO 4217 3 Letter Currency ID>
ISOCode=<ISO 4217 3 Digit Currency Number>
UnitMajor=<Major Currency Name>
UnitMinor=<Minor Currency Name>
Ratio=<ratio of minor currency to major>
}

  TStCurrency = class(TObject)
  { representation of a national currency, based on ISO 4217 specification }
  private
    FName: String;
    FISOCode: String;
    FISOName: String;
    FRatio: Integer;
    FUnitMajor: String;
    FUnitMinor: String;

  public
    { Persistence and streaming methods }
    procedure LoadFromList(List : TStrings);
    procedure SaveToList(List : TStrings);

    { properties }
    property ISOCode: String
      read FISOCode write FISOCode;
    property ISOName: String
      read FISOName write FISOName;
    property Name: String
      read FName write FName;
    property Ratio: Integer
      read FRatio write FRatio;
    property UnitMajor: String
      read FUnitMajor write FUnitMajor;
    property UnitMinor: String
      read FUnitMinor write FUnitMinor;
  end;

  TStCurrencyList = class (TObject)
  { collection of national currencies }
  private
    FItems: TStringList;
  protected {private}
    function GetCount: Integer;
    function GetCurrency(const ISOName : String): TStCurrency;
    function GetItem(Index : Integer): TStCurrency;
    procedure SetCurrency(const ISOName : String; Value: TStCurrency);
    procedure SetItem(Index : Integer; Value: TStCurrency);

    procedure FreeCurrencyByIndex(Index : Integer);
  public
    constructor Create;
    destructor Destroy; override;

    { Access and Update Methods }
    procedure Add(ACurrency : TStCurrency);
    procedure Clear;
    function Contains(ACurrency : TStCurrency): Boolean;
    function ContainsName(const ISOName : String): Boolean;
    procedure Delete(const ISOName: String);
    function IndexOf(const ISOName : String) : Integer;

    { Persistence and streaming methods }
    procedure LoadFromFile(const AFileName: TFileName);
    procedure LoadFromStream(AStream: TStream);
    procedure SaveToFile(const AFileName: TFileName);
    procedure SaveToStream(AStream: TStream);

    { properties }
    property Count : Integer
      read GetCount;
    property Currencies[const ISOName : String]: TStCurrency
      read GetCurrency write SetCurrency;
    property Items[Index : Integer] : TStCurrency
      read GetItem write SetItem; default;
  end;


{
Conversion  Methods
===================
When converting money of one currency into money of another currency, three
conversion methods are commonly encountered:

1)
"Triangular": the source currency amount is converted to an intermediate
currency amount, then the intermediate currency amount is converted to
the target amount.

Note: This is the method required by members of the European Monetary
Union (EMU), for converting among national currencies that are transitioning
to the Euro; the Euro should be used as the Intermediate currency for such
conversions.

2)
"Multiply" the source currency amount is multiplied by a conversion Rate
to obtain the target currency amount.

3)
"Divide" the source currency amount is divided by a conversion Rate to
obtain the target currency amount.
}
  TStConversionType = (ctUnknown, ctTriangular, ctMultiply, ctDivide);

  TStGetRateUpdateEvent = procedure (Sender: TObject; NewRate : TStDecimal;
    var NewDate : TDateTime) of object;
{
; Layout of exchange entries
[SRC:TRG]
source=SRC
target=TRG
; empty/ignored if not a triangular exchange
intermediate=XXX
rate=xxx
; error if tri and intermediate not set
type=<tri|mul|div>
date=<date>
}

  TStExchangeRate = class (TObject)
  { particular Exchange Rate between two currencies }
  private
    FRate: TStDecimal;
    FSource: String;
    FTarget : String;
    FIntermediate : String;
    FConversionType : TStConversionType;
    FDateUpdated      : TDateTime;
    FOnGetRateUpdate: TStGetRateUpdateEvent;
    procedure SetRate(const Value: TStDecimal);
  public
    constructor Create;
    destructor Destroy; override;

    { Access and Update Methods }
    procedure Assign(ARate : TStExchangeRate);
    procedure Clear;
    procedure Convert(Amount, Result: TStDecimal);
    function EqualsRate(aRate : TStExchangeRate) : Boolean;
    function IsValid : Boolean;
    function SameSourceAndTarget(aRate : TStExchangeRate) : Boolean;
    procedure Update;

    { Persistence and streaming methods }
    procedure LoadFromList(List : TStrings);
    procedure SaveToList(List : TStrings);

    { properties }
    property ConversionType : TStConversionType
      read FConversionType write FConversionType;
    property DateUpdated : TDateTime
      read FDateUpdated write FDateUpdated;
    property Intermediate : String
      read FIntermediate write FIntermediate;
    property Rate : TStDecimal
      read FRate write SetRate;
    property Source : String
      read FSource write FSource;
    property Target : String
      read FTarget write FTarget;

    { events }
    property OnGetRateUpdate : TStGetRateUpdateEvent
      read FOnGetRateUpdate write FOnGetRateUpdate;
  end;

  TStExchangeRateList = class (TObject)
  { collection of currency conversions (TStExchangeRate) }
  private
    FRates : TStringList;
  protected {private}
    procedure DeleteRate(Index: Integer);
    function GetCount: Integer;
    function GetRate(const Source, Target: String): TStExchangeRate;
    function GetItem(Index: Integer): TStExchangeRate;
    function MakeEntry(const Source, Target: String): String; virtual;

    procedure ConvertPrim(const aSource, aTarget : string;
                                aAmount : TStDecimal;
                                aAllowTriangular : boolean);
  public
    constructor Create;
    destructor Destroy; override;

    { Access and Update Methods }
    procedure Add(ARate : TStExchangeRate);
    procedure AddByValues(const Source, Target, Intermediate: String;
      Rate: Double; ConversionType: TStConversionType; DateUpdated: TDateTime);
    procedure Assign(AList : TStExchangeRateList);
    procedure Clear;
    function Contains(ARate : TStExchangeRate) : Boolean;
    function ContainsByName(const Source, Target : String) : Boolean;
    procedure Convert(const Source, Target : String;
      Amount, Result : TStDecimal);
    procedure Delete(ARate : TStExchangeRate);
    procedure DeleteByName(const Source, Target : String);
    procedure UpdateRate(const Source, Target : String; Rate : TStDecimal);

    { Persistence and streaming methods }
    procedure LoadFromFile(const AFileName: TFileName);
    procedure LoadFromStream(AStream: TStream);
    procedure SaveToFile(const AFileName: TFileName);
    procedure SaveToStream(AStream: TStream);

    { properties }
    property Count : Integer
      read GetCount;
      { Returns the number of exchange rates in this table. }
    property Items[Index : Integer] : TStExchangeRate
      read GetItem;
      { access to all of the exchange rates in the collection by numeric index }
    property Rates[const Source, Target : String] : TStExchangeRate
      read GetRate;
      { access to all of the exchange rates in the collection by Source and Target }
  end;

  TStMoney = class (TObject)
  { representation of an amount of Currency and operations on same }
  private
    FAmount : TStDecimal;
    FCurrency : String;
    FExchangeRates : TStExchangeRateList;

    function GetAsFloat: Double;
    function GetAsString: String;
    procedure SetAmount(const Value: TStDecimal);
    procedure SetAsFloat(const Value: Double);
    procedure SetAsString(const Value: String);
    procedure Validate(Source, Operand, Result: TStMoney);
    function ValidateCurrencies(Source, Dest: TStMoney) : Boolean;
  public
    constructor Create;
    destructor Destroy; override;
    procedure Assign(AMoney : TStMoney);

    { basic math operations }
    procedure Abs(Result : TStMoney);
    procedure Add(Addend, Sum : TStMoney);
    procedure Divide(Divisor : Double; Quotient : TStMoney);
    procedure DivideByDecimal(Divisor : TStDecimal; Quotient : TStMoney);
    procedure Multiply(Multiplier : Double; Product : TStMoney);
    procedure MultiplyByDecimal(Multiplier : TStDecimal; Product : TStMoney);
    procedure Negate(Result : TStMoney);
    procedure Subtract(Subtrahend, Remainder : TStMoney);

    { logical comparisons }
    function Compare(CompareTo : TStMoney): Integer;
    function IsEqual(AMoney : TStMoney): Boolean;
    function IsGreaterThan(AMoney : TStMoney): Boolean;
    function IsGreaterThanOrEqual(AMoney : TStMoney): Boolean;
    function IsLessThan(AMoney : TStMoney): Boolean;
    function IsLessThanOrEqual(AMoney : TStMoney): Boolean;
    function IsNegative: Boolean;
    function IsNotEqual(AMoney : TStMoney): Boolean;
    function IsPositive: Boolean;
    function IsZero: Boolean;

    { Conversion Methods }
    procedure Convert(const Target : String; Result : TStMoney);
    procedure Round(Method : TStRoundMethod;  Decimals : Integer; Result : TStMoney);
    { See definition of TStRoundMethod in the StDecMth unit for more
      information on rounding }

    { properties }
    property Amount: TStDecimal
      read FAmount write SetAmount;
    property AsFloat: Double
      read GetAsFloat write SetAsFloat;
    property AsString: String
      read GetAsString write SetAsString;
    property Currency: String
      read FCurrency write FCurrency;
    property ExchangeRates : TStExchangeRateList
      read FExchangeRates write FExchangeRates;
  end;

implementation

var
  ExchBaseDate : TDateTime; // the base date for exchange rates

{ TStCurrency }

procedure TStCurrency.LoadFromList(List : TStrings);
{
assign currency properties from a set of <Name>=<Value> pairs

BuildItem expects data in the form:

Name=Country-Currency Name
ISOName=<ISO 4217 3 Letter Currency ID>
ISOCode=<ISO 4217 3 Digit Currency Number>
UnitMajor=<Major Currency Name>
UnitMinor=<Minor Currency Name>
Ratio=<ratio of minor currency to major>
}
begin
  if Assigned(List) then begin
    FName      := List.Values['Name'];
    FISOCode   := List.Values['ISOCode'];
    FISOName   := List.Values['ISOName'];
    FUnitMajor := List.Values['UnitMajor'];
    FUnitMinor := List.Values['UnitMinor'];
    FRatio     := StrToIntDef(List.Values['Ratio'], 100);
  end;
end;

procedure TStCurrency.SaveToList(List : TStrings);
{ write Currency data to <Name>=<Value> pairs for persistence }
begin
  if Assigned(List) then begin
    List.Clear;
    List.Add('Name=' + FName);
    List.Add('ISOCode=' + FISOCode);
    List.Add('ISOName=' + FISOName);
    List.Add('UnitMajor=' + FUnitMajor);
    List.Add('UnitMinor=' + FUnitMinor);
    List.Add('Ratio=' +   IntToStr(FRatio));
  end;
end;

{ TStCurrencyList }

constructor TStCurrencyList.Create;
begin
  inherited Create;
  FItems := TStringList.Create;
  FItems.Sorted := True;
  FItems.Duplicates := dupIgnore;
end;

destructor TStCurrencyList.Destroy;
begin
  Clear;
  FItems.Free;
  inherited Destroy;
end;

procedure TStCurrencyList.Add(ACurrency: TStCurrency);
{ add a new currency to the list }
begin
  if Assigned(ACurrency) then
    FItems.AddObject(ACurrency.ISOName, ACurrency);
end;

procedure TStCurrencyList.Clear;
{ Clear the list of currencies }
var
  i: Integer;
begin
  for i := Pred(FItems.Count) downto 0 do
    FreeCurrencyByIndex(i);
end;

function TStCurrencyList.Contains(ACurrency: TStCurrency): Boolean;
{ returns true if there's an entry for such a currency }
begin
  Result := False;
  if Assigned(ACurrency) then
    Result := FItems.IndexOf(ACurrency.ISOName) >= 0;
end;

function TStCurrencyList.ContainsName(const ISOName: String): Boolean;
{ returns true if there's an entry for such a currency ID }
begin
  Result := FItems.IndexOf(ISOName) >= 0;
end;

procedure TStCurrencyList.Delete(const ISOName: String);
{ delete the requested currency from the list }
begin
  FreeCurrencyByIndex(FItems.IndexOf(ISOName));
end;

procedure TStCurrencyList.FreeCurrencyByIndex(Index: Integer);
{ release a currency by the requested numeric index in the list }
begin
  { if index in range }
  if (0 <= Index) and (Index < FItems.Count) then begin
    { free StCurrency data at that index }
    (FItems.Objects[Index] as TStCurrency).Free;
    { delete item from list }
    FItems.Delete(Index);
  end;
  { else, item doesn't exist, so do nothing }
end;

function TStCurrencyList.GetCount : Integer;
{ just return count of maintained items }
begin
  Result := FItems.Count;
end;

function TStCurrencyList.GetCurrency(const ISOName: String): TStCurrency;
{
return reference to requested currency item indexed by ISOName
returns nil if item doesn't exist
}
var
  Index : Integer;
begin
  { find index of item }
  Index := FItems.IndexOf(ISOName);
  { return item as a TStCurrency reference, or nil if it wasn't found }
  if (Index >= 0) then
    Result := GetItem(Index)
  else
    Result := nil;
end;

function TStCurrencyList.GetItem(Index : Integer): TStCurrency;
{
return reference to requested currency item indexed by position in list
returns nil if item doesn't exist
}
begin
  if not ((0 <= Index) and (Index < FItems.Count)) then
    raise EStException.CreateResFmtTP(stscBadIndex, [IntToStr(Index)], 0);

  Result := (FItems.Objects[Index] as TStCurrency);
end;

function TStCurrencyList.IndexOf(const ISOName: String): Integer;
{
locate index of requested item in list,
returns -1 if item doesn't exist
}
begin
  Result := FItems.IndexOf(ISOName);
end;

procedure TStCurrencyList.LoadFromFile(const AFileName: TFileName);
var
  FS : TFileStream;
begin
  FS := TFileStream.Create(AFileName, fmOpenRead or fmShareDenyWrite);
  try
    LoadFromStream(FS);
  finally
    FS.Free;
  end;
end;

procedure TStCurrencyList.LoadFromStream(AStream : TStream);
var
  IniStr : TStIniStream;
  Currencies, Section : TStrings;
  ACurrency : TStCurrency;
  i : Integer;
begin
  {clear out the current currency items}
  Clear;

  IniStr := nil;
  Currencies := nil;
  Section := nil;
  ACurrency := nil;
  try
    IniStr := TStIniStream.Create(AStream);
    Currencies := TStringList.Create;
    Section := TStringList.Create;
    { create an "index" of the sections }
    IniStr.ReadSections(Currencies);

    { read a currency definition }
    for i := 0 to Pred(Currencies.Count) do begin
      { get settings as .INI style items }
      IniStr.ReadSectionValues(Currencies[i], Section);

      { create a new currency item }
      ACurrency := TStCurrency.Create;

      { set its properties }
      ACurrency.LoadFromList(Section);

      { add it to the list }
      FItems.AddObject(ACurrency.ISOName, ACurrency);
      ACurrency := nil;
    end;
  finally
    IniStr.Free;
    Section.Free;
    Currencies.Free;
    // note: this only does something if either the LoadFromList or
    //       AddObject calls failed
    ACurrency.Free;
  end;
end;

procedure TStCurrencyList.SaveToFile(const AFileName: TFileName);
var
  FS : TFileStream;
begin
  if not FileExists(AFileName) then begin
    FS := TFileStream.Create(AFileName, fmCreate);
    FS.Free;
  end;

  FS := TFileStream.Create(AFileName, fmOpenWrite or fmShareDenyNone);
  try
    SaveToStream(FS);
  finally
    FS.Free;
  end;
end;

procedure TStCurrencyList.SaveToStream(AStream : TStream);
var
  IniStr : TStIniStream;
  Strs   : TStringList;
  i : Integer;
begin
  IniStr := nil;
  Strs := nil;
  try
    IniStr := TStIniStream.Create(AStream);
    Strs := TStringList.Create;
    for i := 0 to Pred(FItems.Count) do begin
      { clear the string list to contain the ccy definition }
      Strs.Clear;
      { get item properties as string list }
      (FItems.Objects[i] as TStCurrency).SaveToList(Strs);
      { add new section to .INI data }
      IniStr.WriteSection(FItems[i], Strs);
    end;
  finally
    Strs.Free;
    IniStr.Free;
  end;
end;

procedure TStCurrencyList.SetCurrency(const ISOName: String;
  Value: TStCurrency);
var
  Idx : Integer;
begin
  { locate item }
  Idx := FItems.IndexOf(ISOName);
  if (Idx >= 0) then
    SetItem(Idx, Value);
end;

procedure TStCurrencyList.SetItem(Index : Integer;
  Value: TStCurrency);
begin
  if not ((0 <= Index) and (Index < FItems.Count)) then
    raise EStException.CreateResFmtTP(stscBadIndex, [IntToStr(Index)], 0);

  if Assigned(Value) then begin
    { release current currency info }
    (FItems.Objects[Index] as TStCurrency).Free;
    { replace with new info }
    FItems.Objects[Index] := Value;
  end;
end;


{ TStMoney }

constructor TStMoney.Create;
begin
  inherited Create;
  FAmount := TStDecimal.Create;
end;

destructor TStMoney.Destroy;
begin
  FAmount.Free;
  inherited Destroy;
end;

procedure TStMoney.Abs(Result : TStMoney);
{ Returns a new money which has the absolute value of this money's amount. }
begin
  Result.Assign(Self);
  Result.Amount.Abs;
end;

procedure TStMoney.Add(Addend, Sum : TStMoney);
begin
  Validate(Self, Addend, Sum);
  Sum.Assign(Self);
  Sum.Amount.Add(Addend.Amount);
end;

procedure TStMoney.Assign(AMoney : TStMoney);
begin
  if Assigned(AMoney) then begin
    Amount.Assign(AMoney.Amount);
    Currency := AMoney.Currency;
    ExchangeRates := AMoney.ExchangeRates;
  end;
end;

function TStMoney.Compare(CompareTo : TStMoney): Integer;
{
Compares this money to the specified money.

Returns <0 if this money is less than the other money, 0 if they are equal,
and >0 if it is greater

Note: Currencies must also be the same
}
begin
  Validate(Self, CompareTo, Self);
  Result := Amount.Compare(CompareTo.Amount);
end;

procedure TStMoney.Convert(const Target : String; Result : TStMoney);
{
Converts the value to a different currency, utilizes TStExchangeRateList
}
begin
  { check that exchange rates are available }
  if not Assigned(ExchangeRates) then
    raise EStException.CreateResTP(stscMoneyNoExchangeRatesAvail, 0);

  { check validity of operands and result }
  if not Assigned(Result) then
    raise EStException.CreateResTP(stscMoneyNilResult, 0);

  Result.Assign(Self);
  ExchangeRates.Convert(Currency, Target, Amount, Result.Amount);
end;

procedure TStMoney.DivideByDecimal(Divisor : TStDecimal; Quotient : TStMoney);
{ Returns a new money which is the quotient of the money divided by
the decimal divisor. }
begin
  if not Assigned(Divisor) then
    raise EStException.CreateResTP(stscMoneyNilParameter, 0);

  if not Assigned(Quotient) then
    raise EStException.CreateResTP(stscMoneyNilResult, 0);

  Quotient.Assign(Self);
  Quotient.Amount.Divide(Divisor);
end;

procedure TStMoney.Divide(Divisor : Double; Quotient : TStMoney);
{ Returns a new money which is the quotient of the money divided by
the floating point divisor. }
var
  DecDiv : TStDecimal;
begin
  DecDiv := TStDecimal.Create;
  try
    DecDiv.AssignFromFloat(Divisor);
    DivideByDecimal(DecDiv, Quotient);
  finally
    DecDiv.Free;
  end;
end;

function TStMoney.GetAsFloat: Double;
{ return money amount as a Floating point value }
begin
  Result := Amount.AsFloat;
end;

function TStMoney.GetAsString: String;
{ return money amount as a string }
begin
  Result := Amount.AsString;
end;

function TStMoney.IsEqual(AMoney : TStMoney): Boolean;
{ Returns true if this money and the specified money are equal }
begin
  Result := Compare(AMoney) = 0;
end;

function TStMoney.IsGreaterThan(AMoney : TStMoney): Boolean;
{ Returns true if this money's amount is greater than that of the specified money. }
begin
  Result := Compare(AMoney) > 0;
end;

function TStMoney.IsGreaterThanOrEqual(AMoney : TStMoney): Boolean;
{ Returns true if this money's amount is greater than or equal to the specified money. }
begin
  Result := Compare(AMoney) >= 0;
end;

function TStMoney.IsPositive : Boolean;
{ Returns true if this money's amount is greater than zero. }
begin
  Result := Amount.IsPositive;
end;

function TStMoney.IsZero: Boolean;
{ Returns true if this money's amount is equal to zero. }
begin
  Result := Amount.IsZero;
end;

function TStMoney.IsLessThan(AMoney : TStMoney): Boolean;
{ Returns true if this money's amount is less than that of the specified money. }
begin
  Result := Compare(AMoney) < 0;
end;

function TStMoney.IsLessThanOrEqual(AMoney : TStMoney): Boolean;
{ Returns true if this money's amount is less than or equal to that of the specified money. }
begin
  Result := Compare(AMoney) <= 0;
end;

function TStMoney.IsNegative: Boolean;
{ Returns true if this money's amount is less than zero. }
begin
  Result := Amount.IsNegative;
end;

function TStMoney.IsNotEqual(AMoney : TStMoney): Boolean;
{ Returns true if this money and the specified money are not equal }
begin
  Result := Compare(AMoney) <> 0;
end;

procedure TStMoney.MultiplyByDecimal(Multiplier : TStDecimal;
  Product : TStMoney);
{ Returns a new money which is the product of the money and the decimal value. }
begin
  if not Assigned(Multiplier) then
    raise EStException.CreateResTP(stscMoneyNilParameter, 0);

  if not Assigned(Product) then
    raise EStException.CreateResTP(stscMoneyNilResult, 0);

  Product.Assign(Self);
  Product.Amount.Multiply(Multiplier);
end;

procedure TStMoney.Multiply(Multiplier : Double; Product : TStMoney);
{ Returns a new money which is the product of the money and the floating point value. }
var
  MulDec : TStDecimal;
begin
  MulDec := TStDecimal.Create;
  try
    MulDec.AssignFromFloat(Multiplier);
    MultiplyByDecimal(MulDec, Product);
  finally
    MulDec.Free;
  end;
end;

procedure TStMoney.Negate(Result : TStMoney);
{ Returns a new money which is the negation of this money's amount. }
begin
  if not Assigned(Result) then
    raise EStException.CreateResTP(stscMoneyNilResult, 0);

  Result.Assign(Self);
  Result.Amount.ChangeSign;
end;

procedure TStMoney.Round(Method : TStRoundMethod; Decimals : Integer; Result : TStMoney);
{
Returns a new money with the rounded value of this money using the specified accuracy.
and using the specified rounding method

See definition of TStRoundMethod in the StDecMth unit for more
information on rounding
}
begin
  if not Assigned(Result) then
    raise EStException.CreateResTP(stscMoneyNilResult, 0);

  Result.Assign(Self);
  Result.Amount.Round(Method, Decimals);
end;

procedure TStMoney.SetAmount(const Value: TStDecimal);
begin
  Amount.Assign(Value);
end;

procedure TStMoney.SetAsFloat(const Value: Double);
begin
  Amount.AssignFromFloat(Value);
end;

procedure TStMoney.SetAsString(const Value: String);
begin
  Amount.AsString := Value;
end;

procedure TStMoney.Subtract(Subtrahend, Remainder : TStMoney);
{ Returns a new money which is the difference between this money and the given money. }
begin
  Validate(Self, Subtrahend, Remainder);
  Remainder.Assign(Self);
  Remainder.Amount.Subtract(Subtrahend.Amount);
end;

function TStMoney.ValidateCurrencies(Source, Dest : TStMoney) : Boolean;
begin
  Result := Source.Currency = Dest.Currency;
end;

procedure TStMoney.Validate(Source, Operand, Result : TStMoney);
begin
  { check validity of operands and result }
  if not Assigned(Source) or not Assigned(Operand) then
    raise EStException.CreateResTP(stscMoneyNilParameter, 0);

  if not Assigned(Result) then
    raise EStException.CreateResTP(stscMoneyNilResult, 0);

  if not ValidateCurrencies(Source, Operand) then
    raise EStException.CreateResTP(stscMoneyCurrenciesNotMatch, 0);
end;

{ TStExchangeRate }

constructor TStExchangeRate.Create;
begin
  inherited Create;
  FRate := TStDecimal.Create;
  Clear;
end;

destructor TStExchangeRate.Destroy;
begin
  FRate.Free;
  inherited Destroy;
end;

procedure TStExchangeRate.Assign(ARate: TStExchangeRate);
begin
  if Assigned(ARate) then begin
    Source := ARate.Source;
    Target := ARate.Target;
    Intermediate := ARate.Intermediate;
    ConversionType := ARate.ConversionType;
    DateUpdated := ARate.DateUpdated;
    Rate.Assign(ARate.Rate);
  end else
  begin
    Clear;
  end;
end;

procedure TStExchangeRate.Clear;
{ clear item fields }
begin
  FSource := '';
  FTarget := '';
  FIntermediate := '';
  FConversionType := ctMultiply;
  FDateUpdated := ExchBaseDate;
  FRate.SetToOne;
end;

procedure TStExchangeRate.Convert(Amount, Result: TStDecimal);
{ convert supplied amount using current ConversionType and Exchange Rate }
begin
  {the parameters must be present}
  if not Assigned(Amount) or not Assigned(Result) then
    raise EStException.CreateResTP(stscMoneyNilParameter, 0);

  {the exchange rate must be valid}
  if not IsValid then
    raise EStException.CreateResTP(stscMoneyInvalidExchRate, 0);

  {set the result equal to the amount prior to converting it}
  Result.Assign(Amount);

  case ConversionType of
    { multiplication conversion }
    ctMultiply   :
      begin
        Result.Multiply(Rate);
      end;

    { division conversion }
    ctDivide     :
      begin
        Result.Divide(Rate);
      end;

    { triangular conversion }
    ctTriangular   :
      begin
        {this can't be done by a single exchange rate}
        raise EStException.CreateResTP(stscMoneyInvalidTriangleExchange, 0);
      end;

  else
    raise EStException.CreateResTP(stscMoneyInvalidExchangeParams, 0);
  end; { case }
end;

function TStExchangeRate.EqualsRate(aRate: TStExchangeRate): Boolean;
{
Returns true if this exchange rate and specified exchange rate have
identical Exchange types, Source currencies, Target currencies,
and conversion Rates or are both Triangular exchanges with the same
Source, Target, and Intermediate currencies
}
var
  CurrenciesMatch, TypesMatch : Boolean;
begin
  Result := False;
  if not Assigned(aRate) then Exit;

  { check if currencies match }
  CurrenciesMatch := (AnsiCompareText(Source, aRate.Source) = 0) and
                     (AnsiCompareText(Target, aRate.Target) = 0);

  { check if exchange types match }
  TypesMatch := (ConversionType = aRate.ConversionType);

  if TypesMatch and CurrenciesMatch then
    case ConversionType of
      ctTriangular : { both triangular }
        { equal if same intermediate currency }
        Result := (FIntermediate = aRate.FIntermediate);

      ctMultiply,
      ctDivide     : { both multiply or divide }
        { equal if same conversion rate }
        Result := (Rate.Compare(aRate.Rate) = 0);
    else
      raise EStException.CreateResTP(stscMoneyInvalidExchangeParams, 0);
    end; { case }
end;

function TStExchangeRate.IsValid: Boolean;
{
Checks to see if this exchange rate has its source, target and Rate
fields set to non-default values, or if a Triangular exchange, that
the intermediate currency is set
}
begin
  {assume the exchange rate is invalid}
  Result := false;

  {the source cannot be empty}
  if (Source = '') then
    Exit;

  {the target cannot be empty}
  if (Target = '') then
    Exit;

  {the source and target must be different}
  if (AnsiCompareText(Source, Target) = 0) then
    Exit;

  {for a multiply/divide conversion, the rate must be > 0.0}
  if (ConversionType = ctMultiply) or (ConversionType = ctDivide) then begin
    Result := FRate.IsPositive;
    Exit;
  end;

  {for a triangular conversion, the intermediate currency must be set
   and cannot be equal to either Source or Target to avoid infinite
   loops in TStExchangeList.Convert <g>}
  if (ConversionType = ctTriangular) then begin
    if (Intermediate = '') then
      Exit;
    if (AnsiCompareText(Source, Intermediate) = 0) then
      Exit;
    if (AnsiCompareText(Target, Intermediate) = 0) then
      Exit;
    Result := true;
    Exit;
  end;

  {otherwise the exchange rate is invalid}
end;

function MakeXChgStr(ConversionType : TStConversionType) : String;
{ convert TStConversionType to string for persistence }
begin
  case ConversionType of
    ctTriangular : Result := 'tri';
    ctMultiply   : Result := 'mul';
    ctDivide     : Result := 'div';
  else
    raise Exception.Create('Unknown conversion type');
  end; { case }
end;

function MakeXChg(const XchStr : String) : TStConversionType;
{ convert persistence string to TStConversionType }
begin
  if (AnsiCompareText(XchStr, 'mul') = 0) then
    Result := ctMultiply
  else if (AnsiCompareText(XchStr, 'div') = 0) then
    Result := ctDivide
  else if (AnsiCompareText(XchStr, 'tri') = 0) then
    Result := ctTriangular
  else begin
    raise Exception.Create('Unknown conversion type in INI file');
    Result := ctUnknown;
  end;
end;

procedure ReplaceCh(var S : String; aFromCh : Char; aToCh : Char);
var
  i : integer;
begin
  {replace the first occurrence of aFromCh with aToCh in string S}
  for i := 0 to length(S) do
    if (S[i] = aFromCh) then begin
      S[i] := aToCh;
      Exit;
    end;
end;

procedure TStExchangeRate.LoadFromList(List: TStrings);
{
set item properties from Exchange Rate data
expects data in the format:

source=<source currency>
target=<target currency>
intermediate=<intermediate currency>
rate=<exchange rate>
type=<tri|mul|div>
date=<date of setting>
}
var
  Str : String;
  DayCount : integer;
  ec       : integer;
begin
  if Assigned(List) then begin
    Clear;
    FSource       := List.Values['source'];
    FTarget       := List.Values['target'];
    FIntermediate := List.Values['intermediate'];
    FConversionType := MakeXChg(List.Values['type']);

    Str := List.Values['date'];
    Val(Str, DayCount, ec);
    if (ec <> 0) then
      DayCount := 0;
    FDateUpdated := ExchBaseDate + DayCount;

    Str := List.Values['rate'];
    if Str = '' then
      FRate.SetToOne
    else begin
      {the INI file stores rates with a decimal *point*; if the locale
       uses something else (eg, a comma) we'll need to switch it for
       the AsString property, which obeys the locale}
      if ({$IFDEF DELPHIXE2}FormatSettings.{$ENDIF}DecimalSeparator <> '.') then
        ReplaceCh(Str, '.', {$IFDEF DELPHIXE2}FormatSettings.{$ENDIF}DecimalSeparator);
      FRate.AsString := Str;
    end;
  end;
end;

function TStExchangeRate.SameSourceAndTarget(
  aRate: TStExchangeRate): Boolean;
{
Tests whether the specified rate has the same source and target currencies.
Returns True of the Source and Target currencies are the same, False otherwise
}
begin
  Result := False;
  if Assigned(aRate) then
    Result := (AnsiCompareText(Source, aRate.Source) = 0) and
              (AnsiCompareText(Target, aRate.Target) = 0);
end;

procedure TStExchangeRate.SaveToList(List: TStrings);
{ create persistent representation of item }
var
  Str : String;
  DayCount : integer;
begin
  if Assigned(List) then begin
    List.Clear;
    List.Add('source=' + FSource);
    List.Add('target=' + FTarget);
    List.Add('intermediate=' + FIntermediate);
    Str := FRate.AsString;
    if ({$IFDEF DELPHIXE2}FormatSettings.{$ENDIF}DecimalSeparator <> '.') then
      ReplaceCh(Str, {$IFDEF DELPHIXE2}FormatSettings.{$ENDIF}DecimalSeparator, '.');
    List.Add('rate=' + Str);
    List.Add('type=' + MakeXChgStr(FConversionType));
    DayCount := trunc(FDateUpdated - ExchBaseDate);
    if DayCount < 0 then
      DayCount := 0;
    List.Add('date=' + IntToStr(DayCount));
  end;
end;

procedure TStExchangeRate.SetRate(const Value: TStDecimal);
begin
  FRate.Assign(Value);
end;

procedure TStExchangeRate.Update;
{ fire update event }
var
  NewDate : TDateTime;
begin
  if Assigned(FOnGetRateUpdate) then begin
    NewDate := DateUpdated;
    FOnGetRateUpdate(Self, Rate, NewDate);
    DateUpdated := NewDate;
  end;
end;


{ TStExchangeRateList }
constructor TStExchangeRateList.Create;
begin
  inherited Create;
  FRates := TStringList.Create;
  FRates.Sorted := True;
  FRates.Duplicates := dupIgnore;
end;

destructor TStExchangeRateList.Destroy;
begin
  Clear;
  FRates.Free;
  inherited Destroy;
end;

procedure TStExchangeRateList.Add(ARate: TStExchangeRate);
{
Adds the given exchange rate to the list

Since FRates list is set for dupIgnore, if Rate already exists, the
new values will be discarded

To modify an existing rate, use the Rates property or the UpdateRate
method, or delete the existing Rate and re-add it
}
begin
  if Assigned(ARate) then
    FRates.AddObject(MakeEntry(ARate.Source, ARate.Target), ARate);
end;

procedure TStExchangeRateList.AddByValues(const Source, Target,
  Intermediate : String; Rate : Double; ConversionType : TStConversionType;
  DateUpdated : TDateTime);
{
Create new rate with provided characteristics and add it to the list

Since FRates list is set for dupIgnore, if Rate already exists, the
new values will be discarded

To modify an existing rate, use the Rates property or the UpdateRate
method, or delete the existing Rate and re-add it
}
var
  TempRate : TStExchangeRate;
begin
  TempRate := TStExchangeRate.Create;
  TempRate.Source := Source;
  TempRate.Target := Target;
  TempRate.Intermediate := Intermediate;
  TempRate.ConversionType := ConversionType;
  TempRate.DateUpdated := DateUpdated;
  TempRate.Rate.AssignFromFloat(Rate);
  Add(TempRate);
end;

procedure TStExchangeRateList.Assign(AList: TStExchangeRateList);
var
  i : Integer;
begin
  if Assigned(AList) then begin
    { if Rate Lists already point to same list then don't do anything }
    if FRates = AList.FRates then Exit;

    { empty list }
    Clear;

    { add items from new list }
    for i := 0 to Pred(AList.Count) do
      Add(AList.Items[i]);
  end;
end;

procedure TStExchangeRateList.Clear;
{ Clears all of the exchange rates from this table. }
var
  i : Integer;
begin
  for i := Pred(FRates.Count) downto 0 do begin
    DeleteRate(i);
  end;
end;

function TStExchangeRateList.Contains(
  ARate: TStExchangeRate): Boolean;
{
Returns true if an exchange rate already exists with this rate's source,
target pair.
}
begin
  Result := False;
  if Assigned(ARate) then
    Result := ContainsByName(ARate.Source, ARate.Target);
end;

function TStExchangeRateList.ContainsByName(const Source,
  Target: String): Boolean;
{
Returns true if an exchange rate already exists with this one's
source and target ISOName Strings
}
begin
  Result := FRates.IndexOf(MakeEntry(Source, Target)) >= 0;
end;

procedure TStExchangeRateList.Convert(const Source, Target: String;
  Amount, Result: TStDecimal);
{
convert Amount from Source currency to Target currency,
return new value in Result
}
begin
  {Amount and Result must be created}
  if (Amount = nil) or (Result = nil) then
    raise EStException.CreateResTP(stscMoneyInvalidExchangeParams, 0);

  {set the result value equal to the amount being converted}
  Result.Assign(Amount);

  {convert, allowing triangular exchanges}
  ConvertPrim(Source, Target, Result, true);
end;

procedure TStExchangeRateList.ConvertPrim(const aSource, aTarget : string;
                                                aAmount : TStDecimal;
                                                aAllowTriangular : boolean);
var
  Rate : TStExchangeRate;
begin
  { do we have an entry for a Source->Target conversion? }
  if not ContainsByName(aSource, aTarget) then
    raise EStException.CreateResFmtTP(stscMoneyNoSuchExchange,
                                      [aSource, aTarget], 0);

  {get the exchange rate}
  Rate := Rates[aSource, aTarget];

  {for a simple multiply or divide conversion, the Rate object can
   handle that by itself}
  if (Rate.ConversionType = ctMultiply) or
     (Rate.ConversionType = ctDivide) then begin
    Rate.Convert(aAmount, aAmount);
    Exit;
  end;

  {if a triangular exchange is not allowed, raise an error}
  if not aAllowTriangular then
    raise EStException.CreateResTP(stscMoneyTriExchUsesTriExch, 0);

  {if the exchange rate is not triangular, raise an error}
  if (Rate.ConversionType <> ctTriangular) then
    raise EStException.CreateResTP(stscMoneyInvalidExchangeParams, 0);

  {the conversion is triangular: check the intermediate currency}
  if (Rate.Intermediate = '') then
    raise EStException.CreateResTP(stscMoneyInvalidExchangeParams, 0);

  {check to see if we have the two exchange rates}
  if (not ContainsByName(aSource, Rate.Intermediate)) or
     (not ContainsByName(Rate.Intermediate, aTarget)) then
    raise EStException.CreateResFmtTP(stscMoneyMissingIntermediateRate,
                                      [aSource, aTarget], 0);

  {convert the amount from the Source to the Intermediate currency,
   and then the result from the Intermediate to the Target currency;
   triangular exchanges are *not* allowed to avoid infinite loops}
  ConvertPrim(aSource, Rate.Intermediate, aAmount, false);
  ConvertPrim(Rate.Intermediate, aTarget, aAmount, false);
end;

procedure TStExchangeRateList.Delete(ARate: TStExchangeRate);
{
delete specified rate from list
fails silently if no matching rate exists in list
}
begin
  DeleteByName(ARate.Source, ARate.Target);
end;

procedure TStExchangeRateList.DeleteByName(const Source,
  Target: String);
{
delete rate from list as determined by Source and Target
fails silently if no matching rate exists in list
}
var
  Idx : Integer;
begin
  { find item in list }
  Idx := FRates.IndexOf(MakeEntry(Source, Target));

  { if it exists, remove it }
  if Idx >= 0 then
    DeleteRate(Idx);
end;

procedure TStExchangeRateList.DeleteRate(Index : Integer);
{ remove Rate from list by index }
{ no error checking that Index is in Range, should be done by caller }
begin
  (FRates.Objects[Index] as TStExchangeRate).Free;
  FRates.Delete(Index);
end;

function TStExchangeRateList.GetCount: Integer;
begin
  Result := FRates.Count;
end;

function TStExchangeRateList.GetItem(Index: Integer): TStExchangeRate;
{ return Exchange rate by index }
begin
  if not ((0 <= Index) and (Index < FRates.Count)) then
    raise EStException.CreateResFmtTP(stscBadIndex, [IntToStr(Index)], 0);
  Result := (FRates.Objects[Index] as TStExchangeRate);
end;

function TStExchangeRateList.GetRate(const Source,
  Target: String): TStExchangeRate;
{ return Exchange rate by Source and Target }
var
  Idx : Integer;
begin
  Idx := FRates.IndexOf(MakeEntry(Source, Target));
  if Idx >= 0 then begin
    Result := (FRates.Objects[Idx] as TStExchangeRate);
  end
  else
    raise EStException.CreateResFmtTP(stscMoneyNoSuchExchange, [Source, Target], 0);
end;

procedure TStExchangeRateList.LoadFromFile(const AFileName: TFileName);
var
  FS : TFileStream;
begin
  FS := TFileStream.Create(AFileName, fmOpenRead or fmShareDenyWrite);
  try
    LoadFromStream(FS);
  finally
    FS.Free;
  end;
end;

procedure TStExchangeRateList.LoadFromStream(AStream: TStream);
{ build Rate list from stream of Rate data }
var
  i : Integer;
  IniStrm : TStIniStream;
  Entries, Sections : TStringList;
  CurRate : TStExchangeRate;
begin
  IniStrm := nil;
  Entries := nil;
  Sections := nil;
  CurRate := nil;
  try
    IniStrm := TStIniStream.Create(AStream);
    Entries := TStringList.Create;
    Sections := TStringList.Create;
    { create "index" of sections }
    IniStrm.ReadSections(Sections);

    { iterate sections }
    for i := 0 to Pred(Sections.Count) do begin
      { get settings as a list of <Name>=<Value> pairs }
      IniStrm.ReadSectionValues(Sections[i], Entries);

      { build new rate item from settings }
      CurRate := TStExchangeRate.Create;
      CurRate.LoadFromList(Entries);

      { add to list }
      Add(CurRate);
      CurRate := nil;
    end;
  finally
    Sections.Free;
    Entries.Free;
    IniStrm.Free;
    CurRate.Free;
  end;
end;

function TStExchangeRateList.MakeEntry(const Source, Target : String) : String;
{ format conversion entry header from Source and Target }
begin
  Result := Source + ':' + Target;
end;

procedure TStExchangeRateList.SaveToFile(const AFileName: TFileName);
var
  FS : TFileStream;
begin
  if not FileExists(AFileName) then begin
    FS := TFileStream.Create(AFileName, fmCreate);
    FS.Free;
  end;

  FS := TFileStream.Create(AFileName, fmOpenWrite or fmShareDenyNone);
  try
    SaveToStream(FS);
  finally
    FS.Free;
  end;
end;

procedure TStExchangeRateList.SaveToStream(AStream: TStream);
{ persist list of Rate data to a stream }
var
  i : Integer;
  IniStrm : TStIniStream;
  Entries : TStringList;
  CurRate : TStExchangeRate;
begin
  IniStrm := nil;
  Entries := nil;
  try
    IniStrm := TStIniStream.Create(AStream);
    Entries := TStringList.Create;
    { for each maintained Rate item }
    for i := 0 to Pred(FRates.Count) do begin

      { get reference to the Rate }
      CurRate := (FRates.Objects[i] as TStExchangeRate);

      { make entries for Rate }
      CurRate.SaveToList(Entries);

      { write entries as a new section to INI stream }
      IniStrm.WriteSection(MakeEntry(CurRate.Source, CurRate.Target),
        Entries);
    end;
  finally
    Entries.Free;
    IniStrm.Free;
  end;
end;

procedure TStExchangeRateList.UpdateRate(const Source,
  Target: String; Rate: TStDecimal);
{
Modifies the exchange rate specified by the source and target
assumes rate already exists, use Add or AddByValues to add new rates
}
var
  Idx : Integer;
begin
  if not Assigned(Rate) then
    raise EStException.CreateResTP(stscMoneyNilParameter, 0);

  Idx := FRates.IndexOf(MakeEntry(Source, Target));
  if Idx >= 0 then begin { conversion already exists for source and target }
    { update Rate to reflect new rate }
    (FRates.Objects[Idx] as TStExchangeRate).Rate.Assign(Rate);
  end
  { else no such rate }
end;

initialization
  ExchBaseDate := EncodeDate(1980, 1, 1);
end.

