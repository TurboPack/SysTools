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
{* SysTools: StDb2DBC.pas 4.04                           *}
{*********************************************************}
{* SysTools: Data-aware Two-Dimensional Barcodes         *}
{*********************************************************}

{$I StDefine.inc}

unit StDb2DBC;

interface

uses
  Windows,
  Messages,
  SysUtils,
  Classes,
  St2DBarC,
  Db,
  DbCtrls;

type

  TStDbPDF417Barcode = class(TStPDF417Barcode)
    protected {private}
      {.Z+}
      FCaptionDataLink : TFieldDataLink;
      FCodeDataLink    : TFieldDataLink;

      procedure CaptionDataChange(Sender : TObject);
      procedure CodeDataChange(Sender : TObject);
      function GetCaptionDataField : string;
      function GetCodeDataField : string;
      function GetDataSource : TDataSource;
      procedure SetCaptionDataField(const Value : string);
      procedure SetCodeDataField(const Value : string);
      procedure SetDataSource(Value : TDataSource);

    public
      constructor Create(AOwner : TComponent); override;
      destructor Destroy;
        override;
      {.Z+}
    published
      property Code stored False;
      property Caption stored False;

      property CaptionDataField : string
               read GetCaptionDataField write SetCaptionDataField;
      property CodeDataField : string
               read GetCodeDataField write SetCodeDataField;

      property DataSource : TDataSource read GetDataSource write SetDataSource;
  end;

  TStDbMaxiCodeBarcode = class(TStMaxiCodeBarcode)
    protected {private}
      {.Z+}
      FCaptionDataLink      : TFieldDataLink;
      FCodeDataLink         : TFieldDataLink;
      FCountryCodeDataLink  : TFieldDataLink;
      FPostalCodeDataLink   : TFieldDataLink;
      FServiceClassDataLink : TFieldDataLink;

      procedure CaptionDataChange (Sender : TObject);
      procedure CodeDataChange (Sender : TObject);
      procedure CountryCodeChange (Sender : TObject);

      function GetCaptionDataField : string;
      function GetCodeDataField : string;
      function GetCountryCodeDataField : string;
      function GetDataSource : TDataSource;
      function GetPostalCodeDataField : string;
      function GetServiceClassDataField : string;

      procedure PostalCodeChange (Sender : TObject);
      procedure ServiceClassChange (Sender : TObject);

      procedure SetCaptionDataField (const Value : string);
      procedure SetCodeDataField (const Value : string);
      procedure SetCountryCodeDataField (const Value : string);
      procedure SetDataSource (Value : TDataSource);
      procedure SetPostalCodeDataField (const Value : string);
      procedure SetServiceClassDataField (const Value : string);

    public
      constructor Create(AOwner : TComponent); override;
      destructor Destroy;
        override;
      {.Z+}
    published
      property Code stored False;
      property Caption stored False;

      property CaptionDataField : string
               read GetCaptionDataField write SetCaptionDataField;
      property CarrierCountryCodeDataField : string
               read GetCountryCodeDataField write SetCountryCodeDataField;
      property CarrierPostalCodeDataField : string
               read GetPostalCodeDataField write SetPostalCodeDataField;
      property CarrierServiceClassDataField : string
               read GetServiceClassDataField write SetServiceClassDataField;
      property CodeDataField : string
               read GetCodeDataField write SetCodeDataField;

      property DataSource : TDataSource read GetDataSource write SetDataSource;
  end;

implementation

{ TStDbPDF417Barcode }

constructor TStDbPDF417Barcode.Create(AOwner : TComponent);
begin
  inherited Create(AOwner);

  FCaptionDataLink := TFieldDataLink.Create;
  FCaptionDataLink.OnDataChange := CaptionDataChange;
  FCodeDataLink := TFieldDataLink.Create;
  FCodeDataLink.OnDataChange := CodeDataChange;
end;

destructor TStDbPDF417Barcode.Destroy;
begin
  FCaptionDataLink.OnDataChange := nil;
  FCaptionDataLink.Free;
  FCaptionDataLink := nil;
  FCodeDataLink.OnDataChange := nil;
  FCodeDataLink.Free;
  FCodeDataLink := nil;

  inherited Destroy;
end;

procedure TStDbPDF417Barcode.CaptionDataChange(Sender : TObject);
begin
  if FCaptionDataLink.Field = nil then
    Caption := '12345678922'
  else
    Caption := FCaptionDataLink.Field.DisplayText;
end;

procedure TStDbPDF417Barcode.CodeDataChange(Sender : TObject);
begin
  if FCodeDataLink.Field = nil then
    Code := '12345678922'
  else
    Code := FCodeDataLink.Field.DisplayText;
end;


function TStDbPDF417Barcode.GetCaptionDataField : string;
begin
  Result := FCaptionDataLink.FieldName;
end;

function TStDbPDF417Barcode.GetCodeDataField : string;
begin
  Result := FCodeDataLink.FieldName;
end;

function TStDbPDF417Barcode.GetDataSource : TDataSource;
begin
  Result := FCaptionDataLink.DataSource
end;

procedure TStDbPDF417Barcode.SetCaptionDataField(const Value : string);
begin
  FCaptionDataLink.FieldName := Value;
end;

procedure TStDbPDF417Barcode.SetCodeDataField(const Value : string);
begin
  FCodeDataLink.FieldName := Value;
end;

procedure TStDbPDF417Barcode.SetDataSource(Value : TDataSource);
begin
  FCaptionDataLink.DataSource := Value;
  FCodeDataLink.DataSource := Value;
end;

{ TStDbMaxiCodeBarcode }

constructor TStDbMaxiCodeBarcode.Create(AOwner : TComponent);
begin
  inherited Create(AOwner);

  FCaptionDataLink := TFieldDataLink.Create;
  FCaptionDataLink.OnDataChange := CaptionDataChange;
  FCodeDataLink := TFieldDataLink.Create;
  FCodeDataLink.OnDataChange := CodeDataChange;
  FCountryCodeDataLink := TFieldDataLink.Create;
  FCountryCodeDataLink.OnDataChange := CountryCodeChange;
  FPostalCodeDataLink := TFieldDataLink.Create;
  FPostalCodeDataLink.OnDataChange := PostalCodeChange;
  FServiceClassDataLink := TFieldDataLink.Create;
  FServiceClassDataLink.OnDataChange := ServiceClassChange; 
end;

destructor TStDbMaxiCodeBarcode.Destroy;
begin
  FCaptionDataLink.OnDataChange := nil;
  FCaptionDataLink.Free;
  FCaptionDataLink := nil;
  FCodeDataLink.OnDataChange := nil;
  FCodeDataLink.Free;
  FCodeDataLink := nil;
  FCountryCodeDataLink.OnDataChange := nil;
  FCountryCodeDataLink.Free;
  FCountryCodeDataLink := nil;
  FPostalCodeDataLink.OnDataChange := nil;
  FPostalCodeDataLink.Free;
  FPostalCodeDataLink := nil;
  FServiceClassDataLink.OnDataChange := nil;
  FServiceClassDataLink.Free;
  FServiceClassDataLink := nil;

  inherited Destroy;
end;

procedure TStDbMaxiCodeBarcode.CaptionDataChange(Sender : TObject);
begin
  if FCaptionDataLink.Field = nil then
    Caption := '12345678922'
  else
    Caption := FCaptionDataLink.Field.DisplayText;
end;

procedure TStDbMaxiCodeBarcode.CodeDataChange(Sender : TObject);
begin
  if FCodeDataLink.Field = nil then
    Code := '12345678922'
  else
    Code := FCodeDataLink.Field.DisplayText;
end;

procedure TStDbMaxiCodeBarcode.CountryCodeChange (Sender : TObject);
begin
  if FCountryCodeDataLink.Field = nil then
    CarrierCountryCode := 0
  else
    CarrierCountryCode := FCountryCodeDataLink.Field.AsInteger;
end;

function TStDbMaxiCodeBarcode.GetCaptionDataField : string;
begin
  Result := FCaptionDataLink.FieldName;
end;

function TStDbMaxiCodeBarcode.GetCodeDataField : string;
begin
  Result := FCodeDataLink.FieldName;
end;

function TStDbMaxiCodeBarcode.GetCountryCodeDataField : string;
begin
  Result := FCountryCodeDataLink.FieldName;
end;

function TStDbMaxiCodeBarcode.GetDataSource : TDataSource;
begin
  Result := FCaptionDataLink.DataSource
end;

function TStDbMaxiCodeBarcode.GetPostalCodeDataField : string;
begin
  Result := FPostalCodeDataLink.FieldName;
end;

function TStDbMaxiCodeBarcode.GetServiceClassDataField : string;
begin
  Result := FServiceClassDataLink.FieldName;
end;

procedure TStDbMaxiCodeBarcode.PostalCodeChange (Sender : TObject);
begin
  if FPostalCodeDataLink.Field = nil then
    CarrierPostalCode := '000'
  else
    CarrierPostalCode := FPostalCodeDataLink.Field.DisplayText;
end;

procedure TStDbMaxiCodeBarcode.ServiceClassChange (Sender : TObject);
begin
  if FServiceClassDataLink.Field = nil then
    CarrierServiceClass := 0
  else
    CarrierServiceClass := FServiceClassDataLink.Field.AsInteger;
end;

procedure TStDbMaxiCodeBarcode.SetCaptionDataField(const Value : string);
begin
  FCaptionDataLink.FieldName := Value;
end;

procedure TStDbMaxiCodeBarcode.SetCodeDataField(const Value : string);
begin
  FCodeDataLink.FieldName := Value;
end;

procedure TStDbMaxiCodeBarcode.SetCountryCodeDataField (const Value : string);
begin
  FCountryCodeDataLink.FieldName := Value;
end;

procedure TStDbMaxiCodeBarcode.SetDataSource(Value : TDataSource);
begin
  FCaptionDataLink.DataSource := Value;
  FCodeDataLink.DataSource := Value;
  FCountryCodeDataLink.DataSource := Value;
  FPostalCodeDataLink.DataSource := Value;
  FServiceClassDataLink.DataSource := Value;
end;

procedure TStDbMaxiCodeBarcode.SetPostalCodeDataField (const Value : string);
begin
  FPostalCodeDataLink.FieldName := Value;
end;

procedure TStDbMaxiCodeBarcode.SetServiceClassDataField (const Value : string);
begin
  FServiceClassDataLink.FieldName := Value;
end;

end.
