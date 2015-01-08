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
{*                   _STEXPR.PAS 3.00                    *}
{*********************************************************}

{$I STDEFINE.INC}
{$I STCOMDEF.INC}
unit _StExpr;

interface

uses
  ComObj, ActiveX, SysTools_TLB, StdVcl;

type
  TStExpr = class(TAutoObject, IStExpr)
   private
    FIsLicensed : Boolean;
   public
    procedure Initialize; override;
   protected { Protected declarations }
    { IStExpr methods }
    function AnalyzeExpr(const Expr: WideString): Double; safecall;
    function License(const Key: WideString): WordBool; safecall;
    { Protected declarations }
  end;

implementation

{$DEFINE UseMathUnit}
uses ComServ, StExpr {$IFDEF LICENSE}, StComLic {$ENDIF};

procedure TStExpr.Initialize;
begin
  inherited Initialize;
  {$IFDEF LICENSE}
  FIsLicensed := False;
  {$ELSE}
  FIsLicensed := True;
  {$ENDIF}
end;

{ ********** TStExpr Methods ************************************************************* }
function TStExpr.AnalyzeExpr(const Expr: WideString): Double;
begin
  {$IFDEF LICENSE}
   if (not FIsLicensed) or (not COMHasBeenLicensed) then
     OleError(CLASS_E_NOTLICENSED);
  {$ENDIF}
  Result := StExpr.AnalyzeExpr(Expr);
end;

function TStExpr.License(const Key: WideString): WordBool;
begin
  {$IFDEF LICENSE}
  Result := COMIsValidKey(Key);
  {$ELSE}
  Result := True;
  {$ENDIF}
  FIsLicensed := Result;
end;

initialization
  TAutoObjectFactory.Create(ComServer, TStExpr, Class_StExpr, ciMultiInstance, tmBoth);
end.

