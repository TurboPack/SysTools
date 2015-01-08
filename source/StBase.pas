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
{$IFDEF CBuilder}
  TStHwnd = Integer;
{$ELSE}
  TStHwnd = HWND;
{$ENDIF}

{-SysTools exception class tree}
type
  EStException = class(Exception)     {ancestor to all SysTools exceptions}
    protected {private}
      FErrorCode : Longint;

    public
      constructor CreateResTP(Ident : LongInt; Dummy : Word);
      constructor CreateResFmtTP(Ident : Longint; const Args : array of const;
                                 Dummy : Word);
      property ErrorCode : LongInt
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
    constructor CreateResTPCol(Ident : Longint; Column : Integer; Dummy : Integer);
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
  {used by CompareLetterSets for estimating word similarity}
  StLetterValues : array['A'..'Z'] of Byte = (
    3 {A} , 6 {B} , 5 {C} , 4 {D} , 3 {E} , 5 {F} , 5 {G} , 4 {H} , 3 {I} ,
    8 {J} , 7 {K} , 4 {L} , 5 {M} , 3 {N} , 3 {O} , 5 {P} , 7 {Q} , 4 {R} ,
    3 {S} , 3 {T} , 4 {U} , 6 {V} , 5 {W} , 8 {X} , 8 {Y} , 9 {Z} );

  StHexDigits  : array[0..$F] of AnsiChar = '0123456789ABCDEF';
  DosDelimSet  : set of AnsiChar = ['\', ':', #0];
{$IFDEF VERSION4} { Delphi/Builder 3 doesn't like widestring typed constants }
  StHexDigitsW : WideString = '0123456789ABCDEF';
  DosDelimSetW : WideString = '\:';
{$ENDIF}

{.Z-}

type
{.Z+}
  TSmallArrayA = array[0..StMaxFileLen-1] of AnsiChar;
  TSmallArray = array[0..StMaxFileLen-1] of Char;
  BTable  = array[0..255] of Byte;  {Table used by Boyer-Moore search routines}
  {$IFDEF UNICODE}
  BTableU = array[0..$FFFF] of Byte;
  {$ENDIF}
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
    FCount       : Longint;

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

    property Count : LongInt
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
{$IFDEF WStrings}
function AnsiUpperCaseShort32(const S : string) : string;
  {-Ansi uppercase for H- strings in WIN32}

function AnsiCompareTextShort32(const S1, S2: string): Integer;
  {-Case-insensitive compare function for H- strings in WIN32}

function AnsiCompareStrShort32(const S1, S2: string): Integer;
  {-Case-sensitive compare function for H- strings in WIN32}
{$ENDIF}


{.Z+}
{---Huge memory routines---}
function HugeCompressRLE(const InBuffer; InLen : Longint;
                         var OutBuffer) : Longint;
  {-Run length encode a buffer}

function HugeDecompressRLE(const InBuffer; InLen : Longint;
                           var OutBuffer; OutLen : LongInt) : Longint;
  {-Run length decode a buffer}

procedure HugeFillChar(var Dest; Count : Longint; Value : Byte);
  {-Fill huge memory block with byte value}

procedure HugeFillStruc(var Dest; Count : Longint;
                        const Value; ValSize : Cardinal);
  {-Fill huge memory block with structure value}

procedure HugeMove(const Src; var Dest; Count : LongInt);
  {-Copy huge memory block to another}

procedure HugeGetMem(var P : Pointer; Size : LongInt);
  {-Get huge memory block allocation}

procedure HugeFreeMem(var P : Pointer; Size : LongInt);
  {-Free huge memory block allocation}
{.Z-}


{---General purpose character manipulation---}

function Upcase(C : AnsiChar) : AnsiChar; overload;
function Upcase(C : WideChar) : WideChar; overload;
  {-Return the uppercase of a character. Provides international character
    support.}

function LoCase(C : AnsiChar) : AnsiChar; overload;
function LoCase(C : WideChar) : WideChar; overload;
  {-Return the lowercase of a character. Provides international character
    support.}

{---General comparison and searching---}

function CompareLetterSets(Set1, Set2 : LongInt) : Cardinal;
  {-Return the sum of the values of the letters common to Set1 and Set2.}

function CompStruct(const S1, S2; Size : Cardinal) : Integer;
  {-Compare two fixed size structures.}

function Search(const Buffer; BufLength : Cardinal; const Match;
                MatLength : Cardinal; var Pos : Cardinal) : Boolean;
  {-Search a buffer for the specified pattern of bytes.}

function SearchUC(const Buffer; BufLength : Cardinal; const Match;
                  MatLength : Cardinal; var Pos : Cardinal) : Boolean;
  {-Search a buffer for a specified pattern of bytes. This search is not case
    sensitive.}


{---Miscellaneous---}

{.Z+}
function IsOrInheritsFrom(Root, Candidate : TClass) : boolean;
  {-Return true if the classes are equal or Candidate is a descendant of Root}

procedure RaiseContainerError(Code : longint);
  {-Internal routine: raise an exception for a container}

procedure RaiseContainerErrorFmt(Code : Longint; Data : array of const);
  {-Internal routine: raise an exception for a container}

function ProductOverflow(A, B : LongInt) : Boolean;
  {-Return True if A*B exceeds MaxLongInt}

{$IFNDEF HStrings}
function StNewStr(S : string) : PShortString;
  {-Allocate a short string on the heap}

procedure StDisposeStr(PS : PShortString);
  {-Deallocate a short string from the heap}
{$ENDIF}
{.Z-}


{---primitives for converting strings to integers}
procedure ValLongInt(S : ShortString; var LI : Longint; var ErrorCode : integer);
procedure ValSmallint(const S : ShortString; var SI : smallint; var ErrorCode : integer);
procedure ValWord(const S : ShortString; var Wd : word; var ErrorCode : integer);

{.Z+}
{general routine to raise a specific class of SysTools exception}
procedure RaiseStError(ExceptionClass : EStExceptionClass; Code : LongInt);
{.Z-}

{.Z+}
{general routines to raise a specific Win32 exception in SysTools}
procedure RaiseStWin32Error(ExceptionClass : EStExceptionClass; Code : LongInt);
procedure RaiseStWin32ErrorEx(ExceptionClass : EStExceptionClass; Code : LongInt; Info : string);
{.Z-}

{$IFDEF VERSION3ONLY}
var
  StHexDigitsW : WideString;
  DosDelimSetW : WideString;
{$ENDIF}


implementation

procedure RaiseStError(ExceptionClass : EStExceptionClass; Code : LongInt);
var
  E : EStException;
begin
  E := ExceptionClass.CreateResTP(Code, 0);
  E.ErrorCode := Code;
  raise E;
end;

procedure RaiseStWin32Error(ExceptionClass : EStExceptionClass; Code : LongInt);
var
  E : EStException;
begin
  E := ExceptionClass.Create(SysErrorMessage(Code));
  E.ErrorCode := Code;
  raise E;
end;

procedure RaiseStWin32ErrorEx(ExceptionClass : EStExceptionClass; Code : LongInt;
          Info : string);
var
  E : EStException;
begin
  E := ExceptionClass.Create(SysErrorMessage(Code) + ' [' + Info + ']');
  E.ErrorCode := Code;
  raise E;
end;

constructor EStException.CreateResTP(Ident : LongInt; Dummy : Word);
begin
  inherited Create(SysToolsStr(Ident));
end;

constructor EStException.CreateResFmtTP(Ident : Longint;
            const Args : array of const; Dummy : Word);
begin
  inherited CreateFmt(SysToolsStr(Ident), Args);
end;

constructor EStExprError.CreateResTPCol(Ident : Longint; Column : Integer; Dummy : Integer);
begin
  inherited CreateResTP(Ident, 0);

  FErrorCol := Column;
end;


function AbstractCompare(Data1, Data2 : Pointer) : Integer; far;
begin
  raise ESTContainerError.CreateResTP(stscNoCompare, 0);
end;

{$IFDEF WStrings}
function AnsiCompareStrShort32(const S1, S2: AnsiString): Integer; assembler;
asm
  push esi
  push edi
  mov esi,S1
  mov edi,S2
  xor eax,eax
  xor edx,edx
  xor ecx,ecx
  mov dl,[esi]
  inc esi
  mov dh,[edi]
  inc edi
  mov cl,dl
  cmp cl,dh
  jbe @1
  mov cl,dh
@1:
  or ecx, ecx
  je @CheckLengths
  repe cmpsb
  jb @LT
  ja @GT
@CheckLengths:
  cmp dl, dh
  je @Exit
  jb @LT
@GT:
  inc eax
  inc eax
@LT:
  dec eax
@Exit:
  pop edi
  pop esi
end;

function AnsiCompareTextShort32(const S1, S2: string): Integer;
begin
  Result := AnsiCompareStrShort32(AnsiUpperCaseShort32(S1),
                                  AnsiUpperCaseShort32(S2));
end;

function AnsiUpperCaseShort32(const S : string) : string;
begin
  Result := S;
  AnsiUpperBuff(PChar(@Result[1]), Length(S));
end;
{$ENDIF}

function DestroyNode(Container : TStContainer;
                     Node : TStNode;
                     OtherData : Pointer) : Boolean;
begin
  Container.DisposeNodeData(Node);
  Node.Free;
  Result := True;
end;

procedure HugeFillChar(var Dest; Count : Longint; Value : Byte);
begin
  FillChar(Dest, Count, Value);
end;

function HugeCompressRLE(const InBuffer; InLen : Longint;
                         var OutBuffer) : Longint;
    {assumes OutBuffer is at least InLen long}
    {returns -1 if InLen <= 1 or if output length would exceed InLen}
    {otherwise returns compressed length}
    {does not initialize OutBuffer if the result is -1}
  asm
    {InBuffer = eax, InLen = edx, OutBuffer = ecx}
    push ebx
    push esi
    push edi

    push OutBuffer       {save output base for later}

    cmp InLen,1
    jle @A               {can't compress if input length <= 1}

    mov esi,InBuffer     {esi = current input offset}
    mov edi,OutBuffer    {edi = current output offset}
    mov eax,InLen
    mov ebx,edi          {ebx = control byte offset}
    mov byte ptr [ebx],0 {reset first control byte}
    mov edx,edi
    add edx,eax          {edx = endpoint of output buffer}
    dec edx              {reserve an extra space for control byte}
    mov ecx,esi
    add ecx,eax          {ecx = endpoint of input buffer}
    dec ecx              {reduce by one for convenience below}
    dec esi              {decrement first time through}

@1: inc esi              {next input byte}
    cmp esi,ecx
    ja  @9               {exit at end of input}
    mov al,[esi]         {load compare byte}
    jae @5               {can't be a match if on last byte of input}
    cmp [esi+1],al       {is it a run?}
    jne @5               {jump if not}

    {starting a run}
    mov ebx,edi          {start a new control sequence}
    mov byte ptr [ebx],1 {first byte in run}
    mov [ebx+1],al       {store run byte}
@2: inc esi              {next input byte}
    cmp esi,ecx          {end of input?}
    ja  @3               {exit this loop if so}
    cmp [esi],al         {next byte a match?}
    jne @3               {jump if not a run}
    cmp byte ptr [ebx],StRLEMaxCount {max run length?}
    je  @3               {exit this loop if so}
    inc byte ptr [ebx]   {increment control byte}
    jmp @2               {stay in the run loop}
@3: or byte ptr [ebx],StRLERunMode {flag control byte as a run}
    inc edi              {step past control and run bytes}
    inc edi
    cmp edi,edx          {filled up output buffer?}
    jae @A               {jump if so}
    mov ebx,edi          {set up new control byte}
    mov byte ptr [ebx],0 {first byte in non-run}
    dec esi              {back up one byte}
    jmp @1               {classify run status again}

@5: {not a run}
    cmp edi,ebx          {the start of a new non-run?}
    ja  @6               {jump if not}
    inc edi              {next output position, guaranteed ok}
@6: cmp byte ptr [ebx],StRLEMaxCount {max non-run length?}
    jb  @7
    mov ebx,edi          {start a new control sequence}
    mov byte ptr [ebx],0 {reset control byte}
    inc edi              {next output position}
    cmp edi,edx          {filled up output buffer?}
    jae @A               {jump if so}
@7: inc byte ptr [ebx]   {increment control byte}
    mov [edi],al         {copy input byte}
    inc edi              {next output position}
    cmp edi,edx          {filled up output buffer?}
    jae @A               {jump if so}
    jmp @1               {back to outer loop}

@9: pop eax              {get output base again}
    sub edi,eax          {get output length}
    jmp @B
@A: pop eax              {balance stack}
    mov edi,-1           {could not compress input}
@B: mov eax,edi          {return output length}

    pop edi
    pop esi
    pop ebx
  end;

function HugeDecompressRLE(const InBuffer; InLen : Longint;
                           var OutBuffer; OutLen : LongInt) : Longint;
    {returns -1 if InLen is <= 0 or output length > OutLen}
    {otherwise returns decompressed length}
  asm
    {InBuffer = eax, InLen = edx, OutBuffer = ecx, OutLen = stack}
    push ebx
    push esi
    push edi

    push OutBuffer       {save output base for later}

    cmp InLen,0          {anything to decompress?}
    jle @A               {jump if not}

    mov esi,InBuffer     {esi = current input offset}
    mov edi,OutBuffer    {edi = current output offset}
    mov ebx,esi
    add ebx,InLen        {ebx = endpoint of input buffer}
    mov edx,OutLen       {edx = space free in output buffer}

@1: cmp esi,ebx          {end of input?}
    jae @9               {jump if so}
    mov al,[esi]         {get next control byte}
    inc esi              {move to run data byte}
    mov cl,al
    and ecx,StRLEMaxCount{ecx = bytes for output}
    sub edx,ecx          {is there space?}
    jc  @A               {jump if not}
    test al,StRLERunMode {is it a run?}
    jz @5                {jump if not}

    {a run}
    mov al,[esi]         {get run data}
    inc esi              {next input position}
    rep stosb            {store it}
    jmp @1               {loop}

@5: {not a run}
    rep movsb            {copy them}
    jmp @1               {loop}

@9: pop eax              {get output base again}
    sub edi,eax          {get output length}
    jmp @B
@A: pop eax              {balance stack}
    mov edi,-1           {could not decompress input}
@B: mov eax,edi          {return output length}

    pop  edi
    pop  esi
    pop  ebx
  end;

procedure HugeFillStruc(var Dest; Count : Longint;
                        const Value; ValSize : Cardinal); assembler;
register;
  asm
    {eax = Dest, edx = Count, ecx = Value}
    push ebx
    push esi
    push edi
    mov edi,Dest            {edi -> Dest}
    mov eax,Value           {eax -> Value}
    {mov edx,Count}         {edx = Count, register parameter}
    mov ebp,ValSize         {ebp = ValSize}
    jmp @2
@1: mov ecx,ebp             {ecx = element ValSize}
    mov esi,eax             {esi -> Value}
    mov bx,cx
    shr ecx,2
    rep movsd
    mov cx,bx
    and cx,3
    rep movsb
@2: sub edx,1               {decrement elements left to fill}
    jnc @1                  {loop for all elements}
    pop edi
    pop esi
    pop ebx
  end;

procedure HugeFreeMem(var P : Pointer; Size : LongInt);
begin
  if Assigned(P) then begin
    FreeMem(P, Size);
    P := nil;
  end;
end;

procedure HugeGetMem(var P : Pointer; Size : LongInt);
begin
  GetMem(P, Size);
end;

procedure HugeMove(const Src; var Dest; Count : LongInt);
begin
  Move(Src, Dest, Count);
end;

function UpCase(C: AnsiChar) : AnsiChar;
asm
  and   eax, 0FFh
  push  eax
  call  CharUpperA
end;

function UpCase(C: WideChar) : WideChar;
asm
  and   eax, 0FFFFh
  push  eax
  call  CharUpperW
end;

function LoCase(C: AnsiChar) : AnsiChar; assembler;
asm
  and   eax, 0FFh
  push  eax
  call  CharLowerA
end;

function LoCase(C: WideChar) : WideChar; assembler;
asm
  and   eax, 0FFFFh
  push  eax
  call  CharLowerW
end;


function ProductOverflow(A, B : LongInt) : Boolean;
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

function CompareLetterSets(Set1, Set2 : LongInt) : Cardinal;
  {-Returns the sum of the values of the letters common to Set1 and Set2.}
asm
  push   ebx                       { Save registers }
  push   edi
  and    eax, edx                  { EAX = EAX and EDX }
  xor    edx, edx                  { Zero EDX }
  mov    ecx, ('Z'-'A')            { Set up counter }
  mov    edi, offset StLetterValues{ Point EBX to table }
  xor    ebx, ebx
  jmp    @@Start

@@Next:
  dec    ecx                       { Decrement counter }
  shl    eax, 1                    { Shift next bit into position }

@@Start:
  test   eax, 2000000h             { Test 26th bit }
  jnz    @@Add                     { If set, add corresponding letter value }
  or     ecx, ecx
  jz     @@Exit                    { Done if ECX is zero }
  jmp    @@Next                    { Test next bit }

@@Add:
  mov    bl, [ecx+edi]             { Do table lookup }
  add    edx, ebx                  { Add value to result }
  or     ecx, ecx
  jnz    @@Next                    { Test next bit }

@@Exit:
  mov    eax, edx                  { Move EDX to result }
  pop    edi                       { Restore registers }
  pop    ebx
end;

function CompStruct(const S1, S2; Size : Cardinal) : Integer;
  {-Compare two fixed size structures}
asm
  push   edi
  push   esi
  mov    esi, eax
  mov    edi, edx
  xor    eax, eax
  or     ecx, ecx
  jz     @@CSDone

  repe   cmpsb
  je     @@CSDone

  inc    eax
  ja     @@CSDone
  or     eax, -1

@@CSDone:
  pop    esi
  pop    edi
end;


function Search(const Buffer; BufLength : Cardinal; const Match;
                MatLength : Cardinal; var Pos : Cardinal) : Boolean;
asm
  push   ebx
  push   edi
  push   esi

  cld
  mov    edi, eax
  mov    ebx, eax
  mov    esi, ecx
  mov    ecx, edx
  mov    edx, MatLength
  or     edx, edx
  jz     @@NotFound

  mov    al, [esi]
  inc    esi
  dec    edx
  sub    ecx, edx
  jbe    @@NotFound

@@Next:
  repne  scasb
  jne    @@NotFound
  or     edx, edx
  jz     @@Found

  push   ecx
  push   edi
  push   esi

  mov    ecx, edx
  repe   cmpsb

  pop    esi
  pop    edi
  pop    ecx

  jne    @@Next            {Try again if no match}

{Calculate number of bytes searched and return}
@@Found:
  mov    esi, Pos
  dec    edi
  sub    edi, ebx
  mov    eax, 1
  mov    [esi], edi
  jmp    @@SDone

{Match was not found}
@@NotFound:
  xor    eax, eax

@@SDone:
  pop    esi
  pop    edi
  pop    ebx
end;

function SearchUC(const Buffer; BufLength : Cardinal; const Match;
                  MatLength: Cardinal; var Pos : Cardinal) : Boolean;

asm
  push   ebx                { Save registers }
  push   edi
  push   esi
  push   eax

  mov    edi, eax           { EDI = ^Buffer }
  mov    esi, ecx           { ESI = ^Match }
  mov    ecx, edx           { ECX = BufLength }
  mov    edx, MatLength     { EDX = MatLength }
  xor    ebx, ebx           { EBX will be used for comparison }
  or     edx, edx           { Is MatLength 0? }
  jz     @@NotFound

  mov    al, [esi]          { Get first character }
  inc    esi
  and    eax, 0FFh          { Zero all but lower byte }

  push   ecx                { Save registers }
  push   edx
  push   eax
  call   CharUpper          { Upcase character }
  pop    edx
  pop    ecx

  mov    bl, al             { Move uppercased char to BL }
  dec    edx                { Dec MatLength }
  sub    ecx, edx           { Is MatLength > BufLength? }
  jbe    @@NotFound

@@Next:
  mov    al, [edi]
  inc    edi

  push   ecx                { Save registers }
  push   edx
  push   eax
  call   CharUpper          { Upcase character in buffer }
  pop    edx
  pop    ecx

  cmp    bl, al             { Match? }
  je     @@CompRest         { Compare rest of string }
@@RestNoMatch:
  dec    ecx                { End of string? }
  jnz    @@Next             { Try next char }
  jmp    @@NotFound         { Done if not found }

@@CompRest:
  or     edx, edx           { Was there only one character? }
  jz     @@Found            { If so, we're done }

  push   ebx                { Save registers }
  push   ecx
  push   edi
  push   esi

  mov    ecx, edx

@@CompLoop:
  mov    al, [esi]
  inc    esi

  push   ecx                { Save registers }
  push   edx
  push   eax
  call   CharUpper          { Upcase character in buffer }

  mov    bl, al
  mov    al, [edi]
  inc    edi

  push   eax
  call   CharUpper          { Upcase character in buffer }
  pop    edx
  pop    ecx

  cmp    bl, al
  jne    @@NoComp
  dec    ecx
  jnz    @@CompLoop

@@NoComp:
  pop    esi                { Restore registers }
  pop    edi
  pop    ecx
  pop    ebx

  jne    @@RestNoMatch      { Try again if no match }

{Calculate number of bytes searched and return}
@@Found:
  pop    ebx
  mov    esi, Pos
  dec    edi
  sub    edi, ebx
  mov    eax, 1
  mov    [esi], edi
  jmp    @@SDone

{Match was not found}
@@NotFound:
  pop    eax
  xor    eax, eax

@@SDone:
  pop    esi
  pop    edi
  pop    ebx
end;

{---primitives for converting strings to integers---}
procedure ValLongInt(S : ShortString; var LI : Longint; var ErrorCode : integer);
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
  if (stbase.upcase(S[LenS]) = 'H') then begin
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
       (S[NBCInx] = '0') and (stbase.upcase(S[NBCInx+1]) = 'X') then begin
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

procedure ValSmallint(const S : ShortString; var SI : smallint; var ErrorCode : integer);
const
  SmallestInt16 = -32767;
  LargestInt16 = 32767;
var
  LI : Longint;
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

procedure ValWord(const S : ShortString; var Wd : word; var ErrorCode : integer);
const
  SmallestWord = 0;
  LargestWord = 65535;
var
  LI : Longint;
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

procedure RaiseContainerError(Code : LongInt);
var
  E : ESTContainerError;
begin
  E := ESTContainerError.CreateResTP(Code, 0);
  E.ErrorCode := Code;
  raise E;
end;

procedure RaiseContainerErrorFmt(Code : Longint; Data : array of const);
var
  E : ESTContainerError;
begin
  E := ESTContainerError.CreateResFmtTP(Code, Data, 0);
  E.ErrorCode := Code;
  raise E;
end;

{$IFNDEF HStrings}
function StNewStr(S : AnsiString) : PShortString;
begin
  GetMem(Result, succ(length(S)));
  Result^ := S;
end;

procedure StDisposeStr(PS : PShortString);
begin
  if (PS <> nil) then
    FreeMem(PS, succ(length(PS^)));
end;
{$ENDIF}

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



initialization
{$IFDEF VERSION3ONLY} { Delphi/Builder 3 doesn't like widestring typed constants }
  StHexDigitsW := '0123456789ABCDEF';
  DosDelimSetW := '\:';
{$ENDIF}
end.


