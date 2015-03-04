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
{* SysTools: StExpLog.pas 4.04                           *}
{*********************************************************}
{* SysTools: Exception Logging                           *}
{*********************************************************}

{$I StDefine.inc}

unit StExpLog;

interface

uses
  Windows, SysUtils, Classes, StBase;

type
  TStOnExceptionFilter = procedure(Sender : TObject; E : Exception;
    var PutInLog : Boolean) of object;

  TStExceptionLog = class(TStComponent)
  strict private class var
    ExpLog : TStExceptionLog;
  strict private
    { Property variables }
    FEnabled : Boolean;
    FFileName : TFileName;
    FRipInfo : string;
    { Event variables }
    FOnExceptionFilter : TStOnExceptionFilter;
  public
    constructor Create(Owner : TComponent); override;
    destructor Destroy; override;
    procedure DoExceptionFilter(E : Exception; var PutInLog : Boolean); virtual;
  published
    { Published properties }
    property Enabled : Boolean read FEnabled write FEnabled default True;
    property FileName : TFileName read FFileName write FFileName;
    property RipInfo : string read FRipInfo write FRipInfo;
    { Published events }
    property OnExceptionFilter : TStOnExceptionFilter
      read FOnExceptionFilter write FOnExceptionFilter;
  end;

implementation

{ TStExceptionLog }

constructor TStExceptionLog.Create(Owner : TComponent);
begin
  inherited Create(Owner);
  ExpLog := Self;
  FEnabled := True;
end;

destructor TStExceptionLog.Destroy;
begin
  ExpLog := nil;
  inherited;
end;

procedure TStExceptionLog.DoExceptionFilter(E : Exception; var PutInLog : Boolean);
begin
  if Assigned(FOnExceptionFilter) then
    FOnExceptionFilter(Self, E, PutInLog);
end;

end.
