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
{* SysTools: StPropEd.pas 4.04                           *}
{*********************************************************}
{* SysTools: Property Editors                            *}
{*********************************************************}

{$I StDefine.inc}
{$IFDEF FPC}
  {$MODE Delphi}
{$ENDIF}

unit StPropEd;

interface

uses
  Dialogs,
{$IFnDEF FPC}
  DesignIntf,
  DesignEditors,
{$ELSE}
  PropEdits,
  Delphi.PropEd,
{$ENDIF}
  Forms,
  Controls;

type
  TStFileNameProperty = class(TStringProperty)
  public
    function GetAttributes: TPropertyAttributes; override;
    procedure Edit; override;
  end;

  TStGenericFileNameProperty = class(TStringProperty)
  public
    function GetAttributes: TPropertyAttributes; override;
    procedure Edit; override;
  end;

implementation

function TStFileNameProperty.GetAttributes: TPropertyAttributes;
begin
  Result := [paDialog];
end;

procedure TStFileNameProperty.Edit;
var
  Dlg : TOpenDialog;
begin
  Dlg := TOpenDialog.Create(Application);
  try
    Dlg.DefaultExt := '*.exe';
    Dlg.Filter := 'Executable Files (*.exe)|*.exe' +
                  '|Dynamic Link Libraries (*.dll)|*.dll';
    Dlg.FilterIndex := 0;
    Dlg.Options := [];
    if GetName = 'ShortcutFileName' then
      Dlg.Options := [ofNoDereferenceLinks];
    Dlg.FileName := Value;
    if Dlg.Execute then
      Value := Dlg.FileName;
  finally
    Dlg.Free;
  end;
end;


function TStGenericFileNameProperty.GetAttributes: TPropertyAttributes;
begin
  Result := [paDialog];
end;

procedure TStGenericFileNameProperty.Edit;
var
  Dlg : TOpenDialog;
begin
  Dlg := TOpenDialog.Create(Application);
  try
    Dlg.DefaultExt := '*.*';
    Dlg.Filter := 'Text files (*.txt)|*.txt' +
                  '|Pascal files (.pas)|*.pas' +
                  '|C++ files (*.cpp)|*.cpp' +
                  '|All files (*.*)|*.*';
    Dlg.FilterIndex := 0;
    Dlg.Options := [];
    Dlg.FileName := Value;
    if Dlg.Execute then
      Value := Dlg.FileName;
  finally
    Dlg.Free;
  end;
end;

end.
