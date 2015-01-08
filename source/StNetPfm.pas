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
{* SysTools: StNetPfm.pas 4.04                           *}
{*********************************************************}
{* SysTools: Net Performance Class                       *}
{*********************************************************}

{$I StDefine.inc}

{$H+} {Huge strings}

unit StNetPfm;

interface

uses
  Windows, Classes, StBase;

type
  TStCPFlags = (cpfForNetCard, cpfNotRouted, cpfSlowLink, cpfDynamic);
  TStCPFlagsSet = set of TStCPFlags;

  TStNetPerformance = class(TStComponent)
   private   { Private Methods/Properties   }
    FGotData     : Boolean;

    { Input }
    FLocalName   : string;
    FRemoteName  : string;
    FProviderName: string;

    { Output }
    FFlags       : TStCPFlagsSet;
    FSpeed       : DWord;
    FDelay       : DWord;
    FOptDataSize : DWord;
   protected { Protected Methods/Properties }
    function GetFlags: TStCPFlagsSet;
    function GetSpeed: DWord;
    function GetDelay: DWord;
    function GetOptDataSize : DWord;

    procedure SetLocalName(Value: string);
    procedure SetRemoteName(Value: string);
    procedure SetProviderName(Value: string);
   public    { Public Methods/Properties    }
    procedure QueryPerformance;

    property Flags: TStCPFlagsSet read GetFlags;
    property Speed: DWord read GetSpeed;
    property Delay: DWord read GetDelay;
    property OptDataSize: DWord read GetOptDataSize;

   published { Published Methods/Properties }
    property LocalName: string
      read FLocalName write SetLocalName;
    property RemoteName: string
      read FRemoteName write SetRemoteName;
    property ProviderName: string
      read FProviderName write SetProviderName;
  end;


implementation

uses
  SysUtils;

procedure TStNetPerformance.QueryPerformance;
var
  Err : DWord;
  NR  : TNetResource;
  CI  : TNetConnectInfoStruct;
begin
  FillChar(NR, SizeOf(NR), 0);
  FillChar(CI, SizeOf(CI), 0);

  if FLocalName <> '' then
    NR.lpLocalName := PChar(FLocalName)
  else
    NR.lpRemoteName := PChar(FRemoteName);

  if Length(FProviderName) > 0 then
    NR.lpProvider := PChar(FProviderName);

  CI.cbStructure := SizeOf(CI);

  Err := MultinetGetConnectionPerformance(@NR, @CI);
  if Err = NO_ERROR then begin
    FFlags := [];
    if (CI.dwFlags and WNCON_FORNETCARD) = WNCON_FORNETCARD then
      Include(FFlags, cpfForNetCard);
    if (CI.dwFlags and WNCON_NOTROUTED) = WNCON_NOTROUTED then
      Include(FFlags, cpfNotRouted);
    if (CI.dwFlags and WNCON_SLOWLINK) = WNCON_SLOWLINK then
      Include(FFlags, cpfSlowLink);
    if (CI.dwFlags and WNCON_DYNAMIC) = WNCON_DYNAMIC then
      Include(FFlags, cpfDynamic);

    FSpeed       := CI.dwSpeed;
    FDelay       := CI.dwDelay;
    FOptDataSize := CI.dwOptDataSize;

    FGotData := True;
  end else
    RaiseStWin32Error(EStNetException, Err);
end;

function TStNetPerformance.GetFlags: TStCPFlagsSet;
begin
  if not FGotData then
    QueryPerformance;
  Result := FFlags;
end;

function TStNetPerformance.GetSpeed: DWord;
begin
  if not FGotData then
    QueryPerformance;
  Result := FSpeed;
end;

function TStNetPerformance.GetDelay: DWord;
begin
  if not FGotData then
    QueryPerformance;
  Result := FDelay;
end;

function TStNetPerformance.GetOptDataSize : DWord;
begin
  if not FGotData then
    QueryPerformance;
  Result := FOptDataSize;
end;

procedure TStNetPerformance.SetLocalName(Value: string);
begin
  if Value <> FLocalName then begin
    FLocalName := Value;
    FGotData := False;
  end;
end;

procedure TStNetPerformance.SetRemoteName(Value: string);
begin
  if Value <> FRemoteName then begin
    FRemoteName := Value;
    FGotData := False;
  end;
end;

procedure TStNetPerformance.SetProviderName(Value: string);
begin
  if Value <> FProviderName then begin
    FProviderName := Value;
    FGotData := False;
  end;
end;


end.
