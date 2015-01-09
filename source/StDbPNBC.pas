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
{* SysTools: StDbPNBC.pas 4.04                           *}
{*********************************************************}
{* SysTools: data aware PostNet Bar Code component       *}
{*********************************************************}

{$I STDEFINE.INC}

unit StDbPNBC;

interface

uses
  Windows, Classes, ClipBrd, Controls, Graphics, Messages, SysUtils,
  Db, DbCtrls, StConst, StBarPN;

type
  TStDbPNBarCode = class(TStPNBarCode)
  protected {private}
    {.Z+}
    FDataLink : TFieldDataLink;

    procedure DataChange(Sender : TObject);
    function GetDataField : string;
    function GetDataSource : TDataSource;
    procedure SetDataField(const Value : string);
    procedure SetDataSource(Value : TDataSource);

  public
    constructor Create(AOwner : TComponent);
      override;
    destructor Destroy;
      override;
    {.Z+}
  published
    property PostalCode
      stored False;

    property DataField : string
      read GetDataField
      write SetDataField;

    property DataSource : TDataSource
      read GetDataSource
      write SetDataSource;
  end;


implementation

{*** TStDbPNBarCode ***}

constructor TStDbPNBarCode.Create(AOwner : TComponent);
begin
  inherited Create(AOwner);

  FDataLink := TFieldDataLink.Create;
  FDataLink.OnDataChange := DataChange;
end;

procedure TStDbPNBarCode.DataChange(Sender : TObject);
begin
  if FDataLink.Field = nil then
    PostalCode := '12345'
  else
    PostalCode := FDataLink.Field.DisplayText;
end;

destructor TStDbPNBarCode.Destroy;
begin
  FDataLink.OnDataChange := nil;
  FDataLink.Free;
  FDataLink := nil;

  inherited Destroy;
end;

function TStDbPNBarCode.GetDataField : string;
begin
  Result := FDataLink.FieldName;
end;

function TStDbPNBarCode.GetDataSource : TDataSource;
begin
  Result := FDataLink.DataSource
end;

procedure TStDbPNBarCode.SetDataField(const Value : string);
begin
  FDataLink.FieldName := Value;
end;

procedure TStDbPNBarCode.SetDataSource(Value : TDataSource);
begin
  FDataLink.DataSource := Value;
end;


end.
