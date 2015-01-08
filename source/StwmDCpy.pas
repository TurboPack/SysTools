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
{* SysTools: StWmDCpy.pas 4.04                           *}
{*********************************************************}
{* SysTools: Class for handling WM_COPYDATA exchanges    *}
{*********************************************************}

{$I StDefine.inc}

unit StWmDCpy;

interface

uses
  Windows,
  SysUtils,
  Messages,
  Classes,
  Forms,
  Controls,
  Dialogs,

  StBase;

type
  TStOnDataReceivedEvent = procedure(Sender   : TObject;
                                     CopyData : TCopyDataStruct) of object;

  TStWMDataCopy = class(TStComponent)
  protected {private}
    { Private declarations }
    NewWndProc      : TFarProc;
    PrevWndProc     : TFarProc;
    FOnDataReceived : TStOnDataReceivedEvent;

    procedure AppWndProc(var Msg : TMessage);
    procedure HookForm(Value : Boolean);
  protected
    { Protected declarations }

  public
    { Public declarations }

    constructor Create(AOwner : TComponent); override;
    destructor Destroy; override;
  published
    { Published declarations }

    property OnDataReceived : TStOnDataReceivedEvent
      read FOnDataReceived
      write FOnDataReceived;
  end;


implementation


constructor TStWMDataCopy.Create(AOwner : TComponent);
begin
  inherited Create(AOwner);

  if not (csDesigning in ComponentState) then begin
{$IFDEF Version6} {$WARN SYMBOL_DEPRECATED OFF} {$ENDIF}
    NewWndProc := MakeObjectInstance(AppWndProc);
{$IFDEF Version6} {$WARN SYMBOL_DEPRECATED ON} {$ENDIF}
    HookForm(True);
  end;
end;

destructor TStWMDataCopy.Destroy;
begin
  if Assigned(NewWndProc) then begin
    HookForm(False);
{$IFDEF Version6} {$WARN SYMBOL_DEPRECATED OFF} {$ENDIF}
    FreeObjectInstance(NewWndProc);
{$IFDEF Version6} {$WARN SYMBOL_DEPRECATED ON} {$ENDIF}
  end;

  inherited Destroy;
end;

procedure TStWMDataCopy.HookForm(Value : Boolean);
begin
  if (not (csDesigning in ComponentState))
      and not (csDestroying in ComponentState) then begin
    if Assigned(PrevWndProc) then
      Exit;
    if Value then begin
      PrevWndProc:= Pointer(
        SetWindowLong(TForm(Owner).Handle, GWL_WNDPROC, LongInt(NewWndProc)))
    end else if Assigned(PrevWndProc) then begin
      SetWindowLong(TForm(Owner).Handle, GWL_WNDPROC, LongInt(PrevWndProc));
      PrevWndProc := nil;
    end;
  end;
end;

procedure TStWMDataCopy.AppWndProc(var Msg : TMessage);
var
  CDS   : TCopyDataStruct;
begin
  with Msg do begin
    if (Msg = WM_COPYDATA) then begin
      CDS := PCopyDataStruct(Pointer(lParam))^;
      if (CDS.dwData = WMCOPYID) then begin
        if (Assigned(FOnDataReceived)) then
          FOnDataReceived(Self, CDS);
      end else
        if Assigned(PrevWndProc) then
          Result :=
            CallWindowProc(PrevWndProc, TForm(Owner).Handle, Msg, wParam, lParam);
    end else
      if Assigned(PrevWndProc) then
        Result :=
          CallWindowProc(PrevWndProc, TForm(Owner).Handle, Msg, wParam, lParam);
  end;
end;

end.
