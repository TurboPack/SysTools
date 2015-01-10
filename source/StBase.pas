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
{* SysTools: StBase.pas 4.04                             *}
{*********************************************************}
{* SysTools: Base unit for SysTools                      *}
{*********************************************************}

{$I StDefine.inc}

unit StBase;

interface

uses
  Windows,
  Classes, SysUtils, Messages, StdCtrls,

  StConst;

const
{.Z+}
  StMaxBlockSize = MaxLongInt;
{.Z-}

type
{!!.01 - moved from StBase.pas }
  TStLineTerminator = ( {possible line terminators...}
     ltNone,            {..no terminator, ie fixed length lines}
     ltCR,              {..carriage return (#13)}
     ltLF,              {..line feed (#10)}
     ltCRLF,            {..carriage return/line feed (#13/#10)}
     ltOther);          {..another character}
{!!.01 - end moved }

type
  TStHwnd = HWND;

{-SysTools exception class tree}
type
  EStException = class(Exception)     {ancestor to all SysTools exceptions}
    protected {private}
      FErrorCode : Integer;

    public
      constructor CreateResTP(Ident : Integer; Dummy : Word);
      constructor CreateResFmtTP(Ident : Integer; const Args : array of const;
                                 Dummy : Word);
      property ErrorCode : Integer
        read FErrorCode
        write FErrorCode;
  end;
  EStExceptionClass = class of EStException;

  EStContainerError = class(EStException);   {container exceptions}
  EStSortError = class(EStException);        {sorting exceptions}
  EStRegIniError = class(EStException);      {registry/INI file exceptions}
  EStBCDError = class(EStException);         {Bcd exceptions}
  EStStringError = class(EStException);      {String class exceptions}
  EStVersionInfoError = class(EStException); {Version info exception}
  EStNetException = class(EStException);     {Network exception}
  EStBarCodeError = class(EStException);     {BarCode exception}
  EStPNBarCodeError = class(EStException);   {PostNet BarCode exception}
  EStStatError = class(EStException);        {statistics exceptions}
  EStFinError = class(EStException);         {Financial exceptions}
  EStMimeError = class(EStException);        {Mime exceptions}
  EStToHTMLError = class(EStException);      {ToHTML exceptions}
  EStSpawnError = class(EStException);       {SpawnApplication errors}
  EStMMFileError = class(EStException);      {MemoryMappedFile errors}
  EStBufStreamError =class(EStException);    {Buffered stream errors}
  EStRegExError = class(EStException);       {RegEx errors}
  EStDecMathError = class(EStException);     {Decimal math errors}
  EStPRNGError = class(EStException);        {Random number errors}

  EStExprError = class(EStException) {expression evaluator exceptions}
    protected {private}
      FErrorCol : Integer;
  public
    constructor CreateResTPCol(Ident : Integer; Column : Integer; Dummy : Integer);
    property ErrorColumn : Integer
      {-Returns the string position at the start of the token where
        the error was detected}
        read FErrorCol;
  end;


const
{.Z+}
  StMaxFileLen  = 260;

  StRLEMaxCount = 127;      { Used by RLE }
  StRLERunMode = $80;       { Used by RLE }
{.Z-}

const
{.Z+}
  StHexDigits  : array[0..$F] of AnsiChar = '0123456789ABCDEF';
  DosDelimSet  : set of AnsiChar = ['\', ':', #0];
  StHexDigitsW : WideString = '0123456789ABCDEF';
  DosDelimSetW : WideString = '\:';

{.Z-}

type
{.Z+}
  TSmallArrayA = array[0..StMaxFileLen-1] of AnsiChar;
  TSmallArray = array[0..StMaxFileLen-1] of Char;
  BTable  = array[0..255] of Byte;  {Table used by Boyer-Moore search routines}
  BTableU = array[0..$FFFF] of Byte;
{.Z-}

type
{.Z+}
  PDouble = ^Double;
  TDoubleArray = array[0..(stMaxBlockSize div SizeOf(Double))-1] of Double;
  PDoubleArray = ^TDoubleArray;
  TIntArray = array[0..(StMaxBlockSize div SizeOf(Integer))-1] of Integer;
  PIntArray = ^TIntArray;
{.Z-}

type
  {the SysTools floating point type}
    {$IFOPT N+}
    TStFloat = Extended;
    {$ELSE}
    TStFloat = Real;
    {$ENDIF}

const
  WMCOPYID : DWORD = $AFAF;

type
  TStNode = class(TPersistent)
{.Z+}
  protected {private}
    FData : Pointer;
{.Z-}
  public
    constructor Create(AData : Pointer);
      virtual;
    property Data : Pointer
       read FData
       write FData;
  end;

{.Z+}
  TStNodeClass = class of TStNode;
{.Z-}

  TStContainer = class;

  TCompareFunc =
    function(Data1, Data2 : Pointer) : Integer;
  TStCompareEvent =
    procedure(Sender : TObject; Data1, Data2 : Pointer;  var Compare : Integer)
    of object;

  TDisposeDataProc =
    procedure(Data : Pointer);
  TStDisposeDataEvent =
    procedure(Sender : TObject; Data : Pointer)
    of object;

  TLoadDataFunc =
    function(Reader : TReader) : Pointer;
  TStLoadDataEvent =
    procedure(Sender : TObject; Reader : TReader; var Data : Pointer)
    of object;

  TStoreDataProc =
    procedure(Writer : TWriter; Data : Pointer);
  TStStoreDataEvent =
    procedure(Sender : TObject; Writer : TWriter; Data : Pointer)
    of object;

  TStringCompareFunc =
    function(const String1, String2 : string) : Integer;
  TStStringCompareEvent =
    procedure(Sender : TObject; const String1, String2 : string; var Compare : Integer)
    of object;

  TUntypedCompareFunc =
    function(const El1, El2) : Integer;
  TStUntypedCompareEvent =
    procedure(Sender : TObject; const El1, El2; var Compare : Integer)
    of object;

  TIterateFunc =
    function(Container : TStContainer; Node : TStNode; OtherData : Pointer) : Boolean;
  TIteratePointerFunc =
    function(Container : TStContainer; Data, OtherData : Pointer) : Boolean;
  TIterateUntypedFunc =
    function(Container : TStContainer; var Data; OtherData : Pointer) : Boolean;

  TStContainer = class(TPersistent)
  {.Z+}
  protected {private}
    {property instance variables}
    FCompare     : TCompareFunc;
    FDisposeData : TDisposeDataProc;
    FLoadData    : TLoadDataFunc;
    FStoreData   : TStoreDataProc;

    {event variables}
    FOnCompare     : TStCompareEvent;
    FOnDisposeData : TStDisposeDataEvent;
    FOnLoadData    : TStLoadDataEvent;
    FOnStoreData   : TStStoreDataEvent;

    {private instance variables}
    {$IFDEF ThreadSafe}
    conThreadSafe  : TRTLCriticalSection;
    {$ENDIF}

    procedure SetCompare(C : TCompareFunc);
    procedure SetDisposeData(D : TDisposeDataProc);
    procedure SetLoadData(L : TLoadDataFunc);
    procedure SetStoreData(S : TStoreDataProc);

  protected
    conNodeClass : TStNodeClass;
    conNodeProt  : Integer;
    FCount       : Integer;

    {protected undocumented methods}
    function AssignPointers(Source : TPersistent; AssignData : TIteratePointerFunc) : boolean;
    function AssignUntypedVars(Source : TPersistent; AssignData : TIterateUntypedFunc) : boolean;
    procedure ForEachPointer(Action : TIteratePointerFunc; OtherData : Pointer);
      virtual;
    procedure ForEachUntypedVar(Action : TIterateUntypedFunc; OtherData : pointer);
      virtual;
    procedure GetArraySizes(var RowCount, ColCount, ElSize : Cardinal);
      virtual;
    procedure SetArraySizes(RowCount, ColCount, ElSize : Cardinal);
      virtual;
    function StoresPointers : boolean;
      virtual;
    function StoresUntypedVars : boolean;
      virtual;

    {protected documented}
    procedure IncNodeProtection;
      {-Prevent container Destroy from destroying its nodes}
    procedure DecNodeProtection;
      {-Allow container Destroy to destroy its nodes}
    procedure EnterCS;
      {-Enter critical section for this instance}
    procedure LeaveCS;
      {-Leave critical section for this instance}
    {.Z-}
  public
    constructor CreateContainer(NodeClass : TStNodeClass; Dummy : Integer);
      {-Create an abstract container (called by descendants)}
    destructor Destroy;
      override;
      {-Destroy a collection, and perhaps its nodes}
    procedure Clear;
      virtual; abstract;
      {-Remove all elements from collection}
    procedure DisposeNodeData(P : TStNode);
      {-Destroy the data associated with a node}

    {wrapper methods for using events or proc/func pointers}
    function DoCompare(Data1, Data2 : Pointer) : Integer;
      virtual;
    procedure DoDisposeData(Data : Pointer);
      virtual;
    function DoLoadData(Reader : TReader) : Pointer;
      virtual;
    procedure DoStoreData(Writer : TWriter; Data : Pointer);
      virtual;

    procedure LoadFromFile(const FileName : string);
      dynamic;
      {-Create a container and its data from a file}
    procedure LoadFromStream(S : TStream);
      dynamic; abstract;
      {-Create a container and its data from a stream}
    procedure StoreToFile(const FileName : string);
      dynamic;
      {-Create a container and its data from a file}
    procedure StoreToStream(S : TStream);
      dynamic; abstract;
      {-Write a container and its data to a stream}

    property Count : Integer
      {-Return the number of elements in the collection}
      read FCount;

    property Compare : TCompareFunc
      {-Set or read the node comparison function}
      read FCompare
      write SetCompare;

    property DisposeData : TDisposeDataProc
      {-Set or read the node data dispose function}
      read FDisposeData
      write SetDisposeData;

    property LoadData : TLoadDataFunc
      {-Set or read the node data load function}
      read FLoadData
      write SetLoadData;

    property StoreData : TStoreDataProc
      {-Set or read the node data load function}
      read FStoreData
      write SetStoreData;

    {events}
    property OnCompare : TStCompareEvent
      read FOnCompare
      write FOnCompare;

    property OnDisposeData : TStDisposeDataEvent
      read FOnDisposeData
      write FOnDisposeData;

    property OnLoadData : TStLoadDataEvent
      read FOnLoadData
      write FOnLoadData;

    property OnStoreData : TStStoreDataEvent
      read FOnStoreData
      write FOnStoreData;
  end;

  TAssignRowData = record
    RowNum : Integer;
    Data   : array [0..0] of Byte;
  end;

  {.Z+}
  { base component for SysTools non-visual components}
  TStComponent = class(TComponent)
  protected {private}
    function GetVersion : string;
    procedure SetVersion(const Value : string);

  published
    property Version : string
      read GetVersion
      write SetVersion
      stored False;
  end;

  { base component for TStExpressionEdit component }
  TStBaseEdit = class(TEdit)
  protected {private}
    function GetVersion : string;
    procedure SetVersion(const Value : string);

  published
    property Version : string
      read GetVersion
      write SetVersion
      stored False;
  end;
  {.Z-}

{---Generic node routines---}
function DestroyNode(Container : TStContainer; Node : TStNode;
                     OtherData : Pointer) : Boolean;
  {-Generic function to pass to iterator to destroy a container node}


{---WIN32 short string routines---}

{.Z+}
{---Huge memory routines---}
procedure HugeFillStruc(var ADest; ADestSize: Integer; const ASource; ASourceSize: Integer);
  {-Fill huge memory block with structure value}

procedure HugeFreeMem(var P : Pointer; Size : Integer);
  {-Free huge memory block allocation}
{.Z-}


{---Miscellaneous---}

{.Z+}
function IsOrInheritsFrom(Root, Candidate : TClass) : boolean;
  {-Return true if the classes are equal or Candidate is a descendant of Root}

procedure RaiseContainerError(Code : Integer);
  {-Internal routine: raise an exception for a container}

procedure RaiseContainerErrorFmt(Code : Integer; Data : array of const);
  {-Internal routine: raise an exception for a container}

function ProductOverflow(A, B : Integer) : Boolean;
  {-Return True if A*B exceeds MaxLongInt}

{.Z-}


{---primitives for converting strings to integers}
procedure ValLongInt(S :string; var LI : Integer; var ErrorCode : integer);
procedure ValSmallint(const S : string; var SI : smallint; var ErrorCode : integer);
procedure ValWord(const S : string; var Wd : word; var ErrorCode : integer);

{.Z+}
{general routine to raise a specific class of SysTools exception}
procedure RaiseStError(ExceptionClass : EStExceptionClass; Code : Integer);
{.Z-}

{.Z+}
{general routines to raise a specific Win32 exception in SysTools}
procedure RaiseStWin32Error(ExceptionClass : EStExceptionClass; Code : Integer);
procedure RaiseStWin32ErrorEx(ExceptionClass : EStExceptionClass; Code : Integer; Info : string);
{.Z-}

implementation

uses
  Math, Character;

procedure RaiseStError(ExceptionClass : EStExceptionClass; Code : Integer);
var
  E : EStException;
begin
  E := ExceptionClass.CreateResTP(Code, 0);
  E.ErrorCode := Code;
  raise E;
end;

procedure RaiseStWin32Error(ExceptionClass : EStExceptionClass; Code : Integer);
var
  E : EStException;
begin
  E := ExceptionClass.Create(SysErrorMessage(Code));
  E.ErrorCode := Code;
  raise E;
end;

procedure RaiseStWin32ErrorEx(ExceptionClass : EStExceptionClass; Code : Integer;
          Info : string);
var
  E : EStException;
begin
  E := ExceptionClass.Create(SysErrorMessage(Code) + ' [' + Info + ']');
  E.ErrorCode := Code;
  raise E;
end;

constructor EStException.CreateResTP(Ident : Integer; Dummy : Word);
begin
  inherited Create(SysToolsStr(Ident));
end;

constructor EStException.CreateResFmtTP(Ident : Integer;
            const Args : array of const; Dummy : Word);
begin
  inherited CreateFmt(SysToolsStr(Ident), Args);
end;

constructor EStExprError.CreateResTPCol(Ident : Integer; Column : Integer; Dummy : Integer);
begin
  inherited CreateResTP(Ident, 0);

  FErrorCol := Column;
end;


function AbstractCompare(Data1, Data2 : Pointer) : Integer; far;
begin
  raise ESTContainerError.CreateResTP(stscNoCompare, 0);
end;

function DestroyNode(Container : TStContainer;
                     Node : TStNode;
                     OtherData : Pointer) : Boolean;
begin
  Container.DisposeNodeData(Node);
  Node.Free;
  Result := True;
end;

procedure HugeFillStruc(var ADest; ADestSize: Integer; const ASource; ASourceSize: Integer);
var
  iSize: Integer;
  pDest: Pointer;
begin
  pDest := @ADest;
  iSize := Min(ADestSize, ASourceSize);
  while iSize > 0 do
  begin
    Move(ASource, pDest^, iSize);
    Inc(NativeInt(pDest), iSize);
    ADestSize := ADestSize - iSize;
    iSize := Min(ADestSize, ASourceSize);
  end;
end;

procedure HugeFreeMem(var P : Pointer; Size : Integer);
begin
  if Assigned(P) then begin
    FreeMem(P, Size);
    P := nil;
  end;
end;

function ProductOverflow(A, B : Integer) : Boolean;
register;
asm
  mov ecx,False
  {A is in eax already, B is in edx already}
  imul eax,edx
  jno @1
  mov ecx,True
@1:
  mov eax,ecx
end;

{---primitives for converting strings to integers---}
procedure ValLongInt(S : string; var LI : Integer; var ErrorCode : integer);
var
  LenS   : byte absolute S;
  Offset : Integer;
  NBCInx : Integer;
begin
  {trim trailing spaces}
  while (LenS > 0) and (S[LenS] = ' ') do
    dec(LenS);
  {empty strings are invalid}
  if (LenS = 0) then begin
    LI := 0;
    ErrorCode := -1;
  end;
  {from now on S must have at least one non-blank char}

  {find the first non-blank char}
  NBCInx := 1;
  while (S[NBCInx] = ' ') do
    inc(NBCInx);

  {check for a string of the form nnnnH}
  Offset := 0;
  if S[LenS].ToUpper = 'H' then begin
    {if the first non-blank char is the final character, then the
     string is just of the form <spaces>H and is invalid}
    if (NBCInx = LenS) then begin
      LI := 0;
      ErrorCode := LenS;
      Exit;
    end;
    Move(S[NBCInx], S[NBCInx+1], LenS-NBCInx);
    S[NBCInx] := '$';
    Offset := -1;
  end
  {check for a string of the form 0Xnnnn}
  else begin
    if (NBCInx < LenS) and
       (S[NBCInx] = '0') and (S[NBCInx+1].ToUpper = 'X') then begin
      S[NBCInx] := ' ';
      S[NBCInx+1] := '$';
    end;
  end;
  Val(S, LI, ErrorCode);
  if (ErrorCode <> 0) then begin
    LI := 0;
    Inc(ErrorCode, Offset);
  end;
end;

procedure ValSmallint(const S : string; var SI : smallint; var ErrorCode : integer);
const
  SmallestInt16 = -32767;
  LargestInt16 = 32767;
var
  LI : Integer;
begin
  ValLongInt(S, LI, ErrorCode);
  if (ErrorCode <> 0) then
    SI := 0
  else {the conversion succeeded} begin
    if (SmallestInt16 <= LI) and (LI <= LargestInt16) then
      SI := LI
    else begin
      ErrorCode := length(S);
      SI := 0;
    end;
  end;
end;

procedure ValWord(const S : string; var Wd : word; var ErrorCode : integer);
const
  SmallestWord = 0;
  LargestWord = 65535;
var
  LI : Integer;
begin
  ValLongInt(S, LI, ErrorCode);
  if (ErrorCode <> 0) then
    Wd := 0
  else {the conversion succeeded} begin
    if (SmallestWord <= LI) and (LI <= LargestWord) then
      Wd := LI
    else begin
      ErrorCode := length(S);
      Wd := 0;
    end;
  end;
end;
{---------------------------------------------------}


function IsOrInheritsFrom(Root, Candidate : TClass) : boolean;
  begin
    Result := (Root = Candidate) or Candidate.InheritsFrom(Root);
  end;

procedure RaiseContainerError(Code : Integer);
var
  E : ESTContainerError;
begin
  E := ESTContainerError.CreateResTP(Code, 0);
  E.ErrorCode := Code;
  raise E;
end;

procedure RaiseContainerErrorFmt(Code : Integer; Data : array of const);
var
  E : ESTContainerError;
begin
  E := ESTContainerError.CreateResFmtTP(Code, Data, 0);
  E.ErrorCode := Code;
  raise E;
end;

{----------------------------------------------------------------------}

constructor TStNode.Create(AData : Pointer);
begin
  Data := AData;
end;

{----------------------------------------------------------------------}

function TStContainer.AssignPointers(Source : TPersistent;
                                     AssignData : TIteratePointerFunc) : boolean;
begin
  Result := false;
  if (Source is TStContainer) then
    if TStContainer(Source).StoresPointers then
      begin
        Clear;
        TStContainer(Source).ForEachPointer(AssignData, Self);
        Result := true;
      end;
end;

function TStContainer.AssignUntypedVars(Source : TPersistent;
                                        AssignData : TIterateUntypedFunc) : boolean;
var
  RowCount : Cardinal;
  ColCount : Cardinal;
  ElSize : Cardinal;
begin
  Result := false;
  if (Source is TStContainer) then
    if TStContainer(Source).StoresUntypedVars then
      begin
        Clear;
        TStContainer(Source).GetArraySizes(RowCount, ColCount, ElSize);
        SetArraySizes(RowCount, ColCount, ElSize);
        TStContainer(Source).ForEachUntypedVar(AssignData, Self);
        Result := true;
      end;
end;

procedure TStContainer.ForEachPointer(Action : TIteratePointerFunc;
                                      OtherData : pointer);
begin
  {do nothing}
end;

procedure TStContainer.ForEachUntypedVar(Action : TIterateUntypedFunc;
                                            OtherData : pointer);
begin
  {do nothing}
end;

procedure TStContainer.GetArraySizes(var RowCount, ColCount, ElSize : Cardinal);
begin
  RowCount := 0;
  ColCount := 0;
  ElSize := 0;
end;

procedure TStContainer.SetArraySizes(RowCount, ColCount, ElSize : Cardinal);
begin
  {do nothing}
end;

procedure TStContainer.SetCompare(C : TCompareFunc);
begin
  FCompare := C;
end;

procedure TStContainer.SetDisposeData(D : TDisposeDataProc);
begin
  FDisposeData := D;
end;

procedure TStContainer.SetLoadData(L : TLoadDataFunc);
begin
  FLoadData := L;
end;

procedure TStContainer.SetStoreData(S : TStoreDataProc);
begin
  FStoreData := S;
end;

function TStContainer.StoresPointers : boolean;
begin
  Result := false;
end;

function TStContainer.StoresUntypedVars : boolean;
begin
  Result := false;
end;

constructor TStContainer.CreateContainer(NodeClass : TStNodeClass; Dummy : Integer);
begin
{$IFDEF ThreadSafe}
  Windows.InitializeCriticalSection(conThreadSafe);
{$ENDIF}

  FCompare := AbstractCompare;
  conNodeClass := NodeClass;

  inherited Create;
end;

procedure TStContainer.DecNodeProtection;
begin
  Dec(conNodeProt);
end;

destructor TStContainer.Destroy;
begin
  if conNodeProt = 0 then
    Clear;
{$IFDEF ThreadSafe}
  Windows.DeleteCriticalSection(conThreadSafe);
{$ENDIF}
  inherited Destroy;
end;

procedure TStContainer.DisposeNodeData(P : TStNode);
begin
{$IFDEF ThreadSafe}
  EnterCS;
  try
{$ENDIF}
    if Assigned(P) then
      DoDisposeData(P.Data);
{$IFDEF ThreadSafe}
  finally
    LeaveCS;
  end;
{$ENDIF}
end;

function TStContainer.DoCompare(Data1, Data2 : Pointer) : Integer;
begin
  Result := 0;
  if Assigned(FOnCompare) then
    FOnCompare(Self, Data1, Data2, Result)
  else if Assigned(FCompare) then
    Result := FCompare(Data1, Data2);
end;

procedure TStContainer.DoDisposeData(Data : Pointer);
begin
  if Assigned(FOnDisposeData) then
    FOnDisposeData(Self, Data)
  else if Assigned(FDisposeData) then
    FDisposeData(Data);
end;

function TStContainer.DoLoadData(Reader : TReader) : Pointer;
begin
  Result := nil;
  if Assigned(FOnLoadData) then
    FOnLoadData(Self, Reader, Result)
  else if Assigned(FLoadData) then
    Result := FLoadData(Reader)
  else
    RaiseContainerError(stscNoLoadData);
end;

procedure TStContainer.DoStoreData(Writer : TWriter; Data : Pointer);
begin
  if Assigned(FOnStoreData) then
    FOnStoreData(Self, Writer, Data)
  else if Assigned(FStoreData) then
    FStoreData(Writer, Data)
  else
    RaiseContainerError(stscNoStoreData);
end;

procedure TStContainer.EnterCS;
begin
{$IFDEF ThreadSafe}
  EnterCriticalSection(conThreadSafe);
{$ENDIF}
end;

procedure TStContainer.IncNodeProtection;
begin
  Inc(conNodeProt);
end;

procedure TStContainer.LeaveCS;
begin
{$IFDEF ThreadSafe}
  LeaveCriticalSection(conThreadSafe);
{$ENDIF}
end;

procedure TStContainer.LoadFromFile(const FileName : string);
var
  S : TStream;
begin
  S := TFileStream.Create(FileName, fmOpenRead + fmShareDenyWrite);
  try
    LoadFromStream(S);
  finally
    S.Free;
  end;
end;

procedure TStContainer.StoreToFile(const FileName : string);
var
  S : TStream;
begin
  S := TFileStream.Create(FileName, fmCreate);
  try
    StoreToStream(S);
  finally
    S.Free;
  end;
end;


{*** TStComponent ***}

function TStComponent.GetVersion : string;
begin
  Result := StVersionStr;
end;

procedure TStComponent.SetVersion(const Value : string);
begin
end;

{ TStBaseEdit }

function TStBaseEdit.GetVersion : string;
begin
  Result := StVersionStr;
end;

procedure TStBaseEdit.SetVersion(const Value : string);
begin
end;

end.


