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
{* SysTools: StPtrns.pas 4.04                            *}
{*********************************************************}
{* SysTools: Pattern Classes                             *}
{*********************************************************}

{$include StDefine.inc}

unit StPtrns;

interface

uses
  Windows, SysUtils, Classes;

{------ S I N G L E T O N ---------------------}
type
  TStSingleton = class(TObject)
    private
      FRefCount : integer;
    protected
    public
      class function NewInstance : TObject; override;
      procedure FreeInstance; override;

      procedure AllocResources; virtual;
      procedure FreeResources; virtual;
  end;

{------ M E D I A T O R ------------------------}
type
  TStMediatorAction = procedure(aInputData, aResultData : TObject) of object;

  TStMediator = class
    private
      FEventTable : TStringList;
    protected
      function  GetCount : Integer;

    public
      constructor Create;
      destructor  Destroy; override;

      procedure Add(const aEventName : string; aHandler : TStMediatorAction);
      procedure Remove(const aEventName : string);

      procedure Handle(const aEventName : string; aInputData, aResultData : TObject);
      function  IsHandled(const aEventName : string) : boolean;

      property  Count : Integer read GetCount;
  end;

{-------O B S E R V E R  ------------------------}
type
  TStObserverAction = procedure(aInputData : TObject) of object;

  TStObserver = class
    private
      FEventTable : TList;
    protected
      function  GetObserver(Index : Integer) : TStObserverAction;
      procedure SetObserver(Index : Integer; InObserver : TStObserverAction);
      function  GetCount : Integer;

    public
      constructor Create;
      destructor  Destroy; override;

      procedure Add(aHandler : TStObserverAction);
      procedure Remove(aIndex : Integer);
      procedure Notify(aInputData : TObject);
      property  Handler[aIndex : Integer] : TStObserverAction
                   read GetObserver write SetObserver;
      property  Count : Integer read GetCount;
  end;

{------- C H A I N ---------------------------------}
type
  TStChainAction = procedure(aInputData, aResultData : TObject; var aStopNow : boolean) of object;

  TStChain = class
    private
      FEventTable : TList;
    protected
      function  GetHandler(Index : Integer) : TStChainAction;
      procedure SetHandler(Index : Integer; InHandler : TStChainAction);
      function  GetCount : Integer;

    public
      constructor Create;
      destructor  Destroy; override;

      procedure Add(aHandler : TStChainAction);
      procedure Remove(aIndex : Integer);
      procedure Handle(aInputData, aResultData : TObject);
      procedure Insert(aIndex : Integer; aHandler : TStChainAction);
      property Handler[aIndex : Integer] : TStChainAction
                  read GetHandler write SetHandler;
      property  Count : Integer read GetCount;
  end;

{====================================================================}
{====================================================================}
implementation

{------ S I N G L E T O N ---------------------}

var
  Instances : TStringList;
  SingletonLock : TRTLCriticalSection;

procedure TStSingleton.AllocResources;
begin
  {nothing at this level}
end;
{--------}

procedure TStSingleton.FreeInstance;
var
  Temp : pointer;
  Inx  : integer;
begin
  EnterCriticalSection(SingletonLock);
  try
    dec(FRefCount);
    if (FRefCount = 0) then begin
      FreeResources;
      Temp := Self;
      CleanupInstance;
      if Instances.Find(ClassName, Inx) then
        Instances.Delete(Inx);
      FreeMem(Temp);
    end;
  finally
    LeaveCriticalSection(SingletonLock);
  end;
end;
{--------}
procedure TStSingleton.FreeResources;
begin
  {nothing at this level}
end;
{--------}
class function TStSingleton.NewInstance : TObject;
var
  Inx : integer;
begin
  EnterCriticalSection(SingletonLock);
  try
    if not Instances.Find(ClassName, Inx) then begin
      GetMem(pointer(Result), InstanceSize);
      InitInstance(Result);
      Instances.AddObject(ClassName, Result);
      TStSingleton(Result).AllocResources;
    end
    else
      Result := Instances.Objects[Inx];
    inc(TStSingleton(Result).FRefCount);
  finally
    LeaveCriticalSection(SingletonLock);
  end;
end;
{====================================================================}

{------ M E D I A T O R ------------------------}
{The action holder is a class that encapsulates the action method}
type
  TStMedActionHolder = class(TObject)
    private
      FAction : TStMediatorAction;
    public
      property Action : TStMediatorAction read FAction write FAction;
  end;
{--------}
constructor TStMediator.Create;
begin
  inherited Create;
  FEventTable := TStringList.Create;
  FEventTable.Sorted := true;
end;

destructor TStMediator.Destroy;
var
  i : integer;
begin
  if (FEventTable <> nil) then begin
    for i := 0 to pred(FEventTable.Count) do
      FEventTable.Objects[i].Free;
    FEventTable.Free;
  end;
  inherited Destroy;
end;

procedure TStMediator.Add(const aEventName : string; aHandler : TStMediatorAction);
var
  MedAction : TStMedActionHolder;
begin
  MedAction := TStMedActionHolder.Create;
  MedAction.Action := aHandler;
  if (FEventTable.AddObject(aEventName, MedAction) = -1) then begin
    MedAction.Free;
    raise Exception.Create(
             Format('TStMediator.Add: event name [%s] already exists',
                    [aEventName]));
  end;
end;

function TStMediator.GetCount : Integer;
begin
  Result := FEventTable.Count;
end;

procedure TStMediator.Handle(const aEventName : string; aInputData, aResultData : TObject);
var
  Index : Integer;
  MediatorActionHolder : TStMedActionHolder;
begin
  Index := FEventTable.IndexOf(aEventName);
  if (Index < 0) then
    raise Exception.Create(
             Format('TStMediator.Handle: event name [%s] not found',
                    [aEventName]));
  MediatorActionHolder := TStMedActionHolder(FEventTable.Objects[Index]);
  MediatorActionHolder.Action(aInputData, aResultData);
end;

function  TStMediator.IsHandled(const aEventName : string) : boolean;
var
  Index : Integer;
begin
  Result := FEventTable.Find(aEventName, Index);
end;

procedure TStMediator.Remove(const aEventName : string);
var
  Index : Integer;
begin
  Index := FEventTable.IndexOf(aEventName);
  if (Index >= 0) then begin
    FEventTable.Objects[Index].Free;
    FEventTable.Delete(Index);
  end;
end;
{====================================================================}

{-------O B S E R V E R  ------------------------}
{The action holder is a class that encapsulates the action method}
type
  TStObActionHolder = class(TObject)
    private
      FAction : TStObserverAction;
    public
      property Action : TStObserverAction read FAction write FAction;
  end;
{--------}
constructor TStObserver.Create;
begin
  inherited Create;
  FEventTable := TList.Create;
end;

destructor TStObserver.Destroy;
var
  i : integer;
begin
  if (FEventTable <> nil) then begin
    for i := 0 to pred(FEventTable.Count) do
      TStObActionHolder(FEventTable[i]).Free;
    FEventTable.Free;
  end;
  inherited Destroy;
end;

procedure TStObserver.Add(aHandler : TStObserverAction);
var
  ObsAction : TStObActionHolder;
begin
  ObsAction := TStObActionHolder.Create;
  try
    ObsAction.Action := aHandler;
    FEventTable.Add(TObject(ObsAction));
  except
    ObsAction.Free;
    raise;
  end;
end;

function TStObserver.GetCount : Integer;
begin
  Result := FEventTable.Count;
end;

function TStObserver.GetObserver(Index : Integer) : TStObserverAction;
var
  ObserverHolder : TStObActionHolder;
begin
  Assert((Index >= 0) and (Index < FEventTable.Count),
         Format('TStObserver.GetObserver: Invalid index value: %d', [Index]));
  ObserverHolder := TStObActionHolder(FEventTable.Items[Index]);
  Result := ObserverHolder.Action;
end;

procedure TStObserver.Notify(aInputData : TObject);
var
  Index : integer;
  ObserverHolder : TStObActionHolder;
begin
  for Index := 0 to FEventTable.Count-1 do begin
    ObserverHolder := TStObActionHolder(FEventTable.Items[Index]);
    ObserverHolder.Action(aInputData);
  end;
end;

procedure TStObserver.Remove(aIndex : Integer);
begin
  Assert((aIndex >= 0) and (aIndex < FEventTable.Count),
         Format('TStObserver.Remove: Invalid index value: %d', [aIndex]));
  TStObActionHolder(FEventTable.Items[aIndex]).Free;
  FEventTable.Delete(aIndex);
end;

procedure TStObserver.SetObserver(Index : Integer;
  InObserver : TStObserverAction);
begin
  Assert((Index >= 0) and (Index < FEventTable.Count),
         Format('TStObserver.SetObserver: Invalid index value: %d', [Index]));
  TStObActionHolder(FEventTable.Items[Index]).Action := InObserver;
end;
{====================================================================}

{------- C H A I N ---------------------------------}
{The action holder is a class that encapsulates the action method}
type
  TStChActionHolder = class(TObject)
    private
      FAction : TStChainAction;
    public
      property Action : TStChainAction read FAction write FAction;
  end;
{--------}
constructor TStChain.Create;
begin
  inherited Create;
  FEventTable := TList.create;
end;

destructor TStChain.Destroy;
var
  i : integer;
begin
  if (FEventTable <> nil) then begin
    for i := 0 to pred(FEventTable.Count) do
      TStChActionHolder(FEventTable[i]).Free;
    FEventTable.Free;
  end;
  inherited Destroy;
end;

procedure TStChain.Add(aHandler : TStChainAction);
var
  ChainAction : TStChActionHolder;
begin
  ChainAction := TStChActionHolder.Create;
  try
    ChainAction.Action := aHandler;
    FEventTable.Add(TObject(ChainAction));
  except
    ChainAction.Free;
    raise;
  end;
end;

function TStChain.GetCount : Integer;
begin
  Result := FEventTable.Count;
end;

function TStChain.GetHandler(Index : Integer) : TStChainAction;
var
  ChainAction : TStChActionHolder;
begin
  Assert((Index >= 0) and (Index < FEventTable.Count),
         Format('TStChain.GetHandler: Invalid index value: %d', [Index]));
  ChainAction := TStChActionHolder(FEventTable.Items[Index]);
  Result := ChainAction.Action;
end;

procedure TStChain.Handle(aInputData, aResultData : TObject);
var
  Index : integer;
  Stop  : boolean;
  ChainAction : TStChActionHolder;
begin
  Stop := false;

  for Index := 0 to (FEventTable.Count - 1) do begin
    ChainAction := TStChActionHolder(FEventTable.Items[Index]);
    ChainAction.Action(aInputData, aResultData, Stop);
    if Stop then
      Exit;
  end;
end;

procedure TStChain.Insert(aIndex : integer; aHandler : TStChainAction);
var
  ChainAction : TStChActionHolder;
begin
  ChainAction := TStChActionHolder.Create;
  try
    ChainAction.Action := aHandler;
    FEventTable.Insert(aIndex, ChainAction);
  except
    ChainAction.Free;
    raise;
  end;
end;

procedure TStChain.Remove(aIndex : Integer);
begin
  Assert((aIndex >= 0) and (aIndex < FEventTable.Count),
         Format('TStChain.Remove: Invalid index value: %d', [aIndex]));
  TStChActionHolder(FEventTable.Items[aIndex]).Free;
  FEventTable.Delete(aIndex);
end;

procedure TStChain.SetHandler(Index : Integer; InHandler : TStChainAction);
begin
  Assert((Index >= 0) and (Index < FEventTable.Count),
         Format('TStObserver.SetObserver: Invalid index value: %d', [Index]));
  TStChActionHolder(FEventTable.Items[Index]).Action := InHandler;
end;

procedure InitUnit;
begin
  InitializeCriticalSection(SingletonLock);
  Instances := TStringList.Create;
  Instances.Sorted := true;
end;

procedure DoneUnit;
var
  i : integer;
  OldCount : integer;
begin
  EnterCriticalSection(SingletonLock);

  {continue 'freeing' the last singleton object in the Instances
   stringlist until its FreeInstance method actually frees the object
   and removes the class name from the stringlist: we detect this
   condition by the fact that the number of items in the stringlist
   decreases.}
  OldCount := Instances.Count;
  for i := pred(OldCount) downto 0 do begin
    repeat
      Instances.Objects[i].Free;
    until (Instances.Count <> OldCount);
    OldCount := Instances.Count;
  end;

  {free the global variables}
  Instances.Free;
  DeleteCriticalSection(SingletonLock);
end;

initialization
  InitUnit;

finalization
  DoneUnit;

end.

