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

library systools;

uses
  ComServ,
  SysTools_TLB in 'SysTools_TLB.pas',
  _StAstro in '_StAstro.pas' {StAstro: CoClass},
  _StBase in '_StBase.pas' {StBase: CoClass},
  _StCRC in '_StCRC.pas' {StCRC: CoClass},
  _StDate in '_StDate.pas' {StDate: CoClass},
  _StEclipses in '_StEclipses.pas' {StEclipses: CoClass},
  _StExpr in '_StExpr.pas' {StExpr: CoClass},
  _StFin in '_StFin.pas' {StFin: CoClass},
  _StMime in '_StMime.pas' {StMime: CoClass},
  _StRegINI in '_StRegINI.pas' {StRegINI: CoClass},
  _StStat in '_StStat.pas' {StStat: CoClass},
  _StStr in '_StStr.pas' {StStr: CoClass},
  _StUtil in '_StUtil.pas' {StUtil: CoClass};

exports
  DllGetClassObject,
  DllCanUnloadNow,
  DllRegisterServer,
  DllUnregisterServer;

{$R *.TLB}

{$R *.RES}

begin
end.
