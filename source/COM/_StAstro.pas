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

unit _StAstro;

interface

uses
  ComObj, SysTools_TLB;

type
  TStAstro = class(TAutoObject, IStAstro)
   private
   public
    procedure Initialize; override;
    destructor Destroy; override;
   protected
  end;

implementation

uses ComServ;

procedure TStAstro.Initialize;
begin
  inherited Initialize;
end;

destructor TStAstro.Destroy;
begin
  inherited Destroy;
end;


initialization
  TAutoObjectFactory.Create(ComServer, TStAstro, Class_StAstro, ciMultiInstance, tmBoth);
end.
