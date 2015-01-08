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

{
  TODO:
   1. What is the 'Path' of TStEclipseRecord?
}

unit _StEclipses;

interface

uses
  Windows, ComObj, ActiveX, StEclpse, SysTools_TLB;

type
  TStEclipseRecord = class(TAutoIntfObject, IStEclipseRecord)
   private
     FOwner : StEclpse.TStEclipseRecord;
   public
    constructor Create(AOwner: StEclpse.TStEclipseRecord);
   protected
    function Get_EType: TStEclipseType; safecall;
    function Get_Magnitude: Double; safecall;
    function Get_Hemisphere: TStHemisphereType; safecall;
    function Get_LContacts: IStContactTimes; safecall;
    function Get_Path: OleVariant; safecall;
  end;

  TStContactTimes = class(TAutoIntfObject, IStContactTimes)
   private
     FOwner : StEclpse.TStContactTimes;
   public
    constructor Create(AOwner: StEclpse.TStContactTimes);
   protected
    function Get_UT1: TDateTime; safecall;
    function Get_UT2: TDateTime; safecall;
    function Get_FirstContact: TDateTime; safecall;
    function Get_SecondContact: TDateTime; safecall;
    function Get_MidEclipse: TDateTime; safecall;
    function Get_ThirdContact: TDateTime; safecall;
    function Get_FourthContact: TDateTime; safecall;
  end;

  TStEclipses = class(TAutoObject, IStEclipses, IEnumVariant)
   private
    FOwner : StEclpse.TStEclipses;
    FEnumPos : Integer;
   public
    procedure Initialize; override;
    destructor Destroy; override;
   protected
    function Get__NewEnum: IUnknown; safecall;

    { IEnumVariant }
    function Next(celt: LongInt; out elt; pceltFetched: PLongInt): HResult; stdcall;
    function Skip(celt: LongInt): HResult; stdcall;
    function Reset: HResult; stdcall;
    function Clone(out Enum: IEnumVariant): HResult; stdcall;

    function Get_Eclipses(Index: Integer): IStEclipseRecord; safecall;
    procedure FindEclipses(Year: Integer); safecall;
    function Get_Count: Integer; safecall;
  end;

implementation

uses ComServ, StList;

constructor TStEclipseRecord.Create(AOwner: StEclpse.TStEclipseRecord);
begin
  inherited  Create(ComServer.TypeLib, IStEclipseRecord);
  FOwner := AOwner;
end;

function TStEclipseRecord.Get_EType: TStEclipseType;
begin
  Result := TStEclipseType(FOwner.EType);
end;

function TStEclipseRecord.Get_Magnitude: Double;
begin
  Result := FOwner.Magnitude;
end;

function TStEclipseRecord.Get_Hemisphere: TStHemisphereType;
begin
  Result := TStHemisphereType(FOwner.Hemisphere);
end;

function TStEclipseRecord.Get_LContacts: IStContactTimes;
begin
  Result := TStContactTimes.Create(FOwner.LContacts);
end;

function TStEclipseRecord.Get_Path: OleVariant;
begin
//  Result := 0;
end;


constructor TStContactTimes.Create(AOwner: StEclpse.TStContactTimes);
begin
  inherited  Create(ComServer.TypeLib, IStContactTimes);
  FOwner := AOwner;
end;

function TStContactTimes.Get_UT1: TDateTime;
begin
  Result := FOwner.UT1;
end;

function TStContactTimes.Get_UT2: TDateTime;
begin
  Result := FOwner.UT2;
end;

function TStContactTimes.Get_FirstContact: TDateTime;
begin
  Result := FOwner.FirstContact;
end;

function TStContactTimes.Get_SecondContact: TDateTime;
begin
  Result := FOwner.SecondContact;
end;

function TStContactTimes.Get_MidEclipse: TDateTime;
begin
  Result := FOwner.MidEclipse;
end;

function TStContactTimes.Get_ThirdContact: TDateTime;
begin
  Result := FOwner.ThirdContact;
end;

function TStContactTimes.Get_FourthContact: TDateTime;
begin
  Result := FOwner.FourthContact;
end;

procedure TStEclipses.Initialize;
begin
  inherited Initialize;
  FOwner := StEclpse.TStEclipses.Create(TStListNode);
  FEnumPos := 0;
end;

destructor TStEclipses.Destroy;
begin
  FOwner.Free;
  inherited Destroy;
end;

function TStEclipses.Get__NewEnum: IUnknown;
begin
  Result := Self;
end;

function TStEclipses.Next(celt: LongInt; out elt; pceltFetched: PLongInt): HResult;
var
  V : OleVariant;
  I : Integer;
begin
  Result := S_FALSE;
  try
    if pceltFetched <> nil then
      pceltFetched^ := 0;
    for I := 0 to celt - 1 do begin
      if FEnumPos >= FOwner.Count then begin
        FEnumPos := 0;
        Exit;
      end;
      V := Get_Eclipses(FEnumPos);
      TVariantArgList(elt)[I] := TVariantArg(V);

      // Prevent COM garbage collection
      TVarData(V).VType := varEmpty;
      TVarData(V).VInteger := 0;

      Inc(FEnumPos);
      if pceltFetched <> nil then
        Inc(pceltFetched^);
    end;
  except
  end;
  if (pceltFetched = nil) or ((pceltFetched <> nil) and (pceltFetched^ = celt)) then
   Result := S_OK;
end;

function TStEclipses.Skip(celt: LongInt): HResult;
begin
  Inc(FEnumPos, celt);
  Result := S_OK;
end;

function TStEclipses.Reset: HResult;
begin
  FEnumPos := 0;
  Result := S_OK;
end;

function TStEclipses.Clone(out Enum: IEnumVariant): HResult;
begin
  Enum := nil;
  Result := S_OK;
  try
    Enum := Self.Create;
    TStEclipses(Enum).FOwner := FOwner;
  except
    Result := E_OUTOFMEMORY;
  end;
end;

function TStEclipses.Get_Eclipses(Index: Integer): IStEclipseRecord;
begin
  Result := TStEclipseRecord.Create(FOwner.Eclipses[Index]^);
end;

procedure TStEclipses.FindEclipses(Year: Integer);
begin
  FOwner.FindEclipses(Year);
end;

function TStEclipses.Get_Count: Integer;
begin
  Result := FOwner.Count;
end;

initialization
  TAutoObjectFactory.Create(ComServer, TStEclipses, Class_StEclipses, ciMultiInstance, tmBoth);
end.
