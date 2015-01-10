// CodeGear C++Builder
// Copyright (c) 1995, 2015 by Embarcadero Technologies, Inc.
// All rights reserved

// (DO NOT EDIT: machine generated header) 'StSpawn.pas' rev: 29.00 (Windows)

#ifndef StspawnHPP
#define StspawnHPP

#pragma delphiheader begin
#pragma option push
#pragma option -w-      // All warnings off
#pragma option -Vx      // Zero-length empty class member 
#pragma pack(push,8)
#include <System.hpp>
#include <SysInit.hpp>
#include <System.SysUtils.hpp>
#include <Winapi.Windows.hpp>
#include <Vcl.ExtCtrls.hpp>
#include <Winapi.Messages.hpp>
#include <System.Classes.hpp>
#include <Winapi.ShellAPI.hpp>
#include <StBase.hpp>
#include <StConst.hpp>

//-- user supplied -----------------------------------------------------------

namespace Stspawn
{
//-- type declarations -------------------------------------------------------
class DELPHICLASS TStWaitThread;
class PASCALIMPLEMENTATION TStWaitThread : public System::Classes::TThread
{
	typedef System::Classes::TThread inherited;
	
protected:
	int FTimeOut;
	virtual void __fastcall Execute(void);
	
public:
	NativeUInt CancelWaitEvent;
	unsigned WaitResult;
	Winapi::Windows::TWOHandleArray *WaitFors;
	__fastcall TStWaitThread(NativeUInt aInst, NativeUInt CancelIt, int ATimeOut);
	__fastcall virtual ~TStWaitThread(void);
};


enum DECLSPEC_DENUM TStSpawnCommand : unsigned char { scOpen, scPrint, scOther };

enum DECLSPEC_DENUM TStShowState : unsigned char { ssMinimized, ssMaximized, ssNormal, ssMinNotActive };

typedef void __fastcall (__closure *TStSpawnErrorEvent)(System::TObject* Sender, System::Word Error);

typedef void __fastcall (__closure *TStSpawnCompletedEvent)(System::TObject* Sender);

typedef void __fastcall (__closure *TStSpawnTimeOutEvent)(System::TObject* Sender);

class DELPHICLASS TStSpawnApplication;
class PASCALIMPLEMENTATION TStSpawnApplication : public Stbase::TStComponent
{
	typedef Stbase::TStComponent inherited;
	
protected:
	bool FAllowChange;
	NativeUInt FCancelEvent;
	System::UnicodeString FDefaultDir;
	System::UnicodeString FFileName;
	NativeUInt FInstance;
	bool FNotifyWhenDone;
	TStSpawnCompletedEvent FOnCompleted;
	TStSpawnErrorEvent FOnSpawnError;
	TStSpawnTimeOutEvent FOnSpawnTimeOut;
	System::UnicodeString FRunParameters;
	TStShowState FShowState;
	TStSpawnCommand FSpawnCommand;
	Vcl::Extctrls::TTimer* FTimer;
	int FTimeOut;
	unsigned FWaitResult;
	TStWaitThread* FWaitThread;
	System::UnicodeString FSpawnCommandStr;
	int CountDownValue;
	void __fastcall DoOnThreadEnd(System::TObject* Sender);
	void __fastcall SetDefaultDir(const System::UnicodeString Value);
	void __fastcall SetFileName(const System::UnicodeString Value);
	void __fastcall SetOnCompleted(TStSpawnCompletedEvent Value);
	void __fastcall SetOnSpawnError(TStSpawnErrorEvent Value);
	void __fastcall SetNotifyWhenDone(bool Value);
	void __fastcall SetRunParameters(const System::UnicodeString Value);
	void __fastcall SetShowState(TStShowState Value);
	void __fastcall SetSpawnCommand(TStSpawnCommand Value);
	void __fastcall SetSpawnTimeOut(TStSpawnTimeOutEvent Value);
	void __fastcall SetTimeOut(int Value);
	
public:
	__fastcall virtual TStSpawnApplication(System::Classes::TComponent* AOwner);
	__fastcall virtual ~TStSpawnApplication(void);
	void __fastcall CancelWait(void);
	NativeUInt __fastcall Execute(void);
	
__published:
	__property System::UnicodeString DefaultDir = {read=FDefaultDir, write=SetDefaultDir};
	__property System::UnicodeString FileName = {read=FFileName, write=SetFileName};
	__property bool NotifyWhenDone = {read=FNotifyWhenDone, write=SetNotifyWhenDone, default=1};
	__property TStSpawnCompletedEvent OnCompleted = {read=FOnCompleted, write=SetOnCompleted};
	__property TStSpawnErrorEvent OnSpawnError = {read=FOnSpawnError, write=SetOnSpawnError};
	__property TStSpawnTimeOutEvent OnTimeOut = {read=FOnSpawnTimeOut, write=SetSpawnTimeOut};
	__property System::UnicodeString RunParameters = {read=FRunParameters, write=SetRunParameters};
	__property TStShowState ShowState = {read=FShowState, write=SetShowState, default=2};
	__property TStSpawnCommand SpawnCommand = {read=FSpawnCommand, write=SetSpawnCommand, default=0};
	__property int TimeOut = {read=FTimeOut, write=SetTimeOut, default=0};
	__property System::UnicodeString SpawnCommandStr = {read=FSpawnCommandStr, write=FSpawnCommandStr};
};


//-- var, const, procedure ---------------------------------------------------
}	/* namespace Stspawn */
#if !defined(DELPHIHEADER_NO_IMPLICIT_NAMESPACE_USE) && !defined(NO_USING_NAMESPACE_STSPAWN)
using namespace Stspawn;
#endif
#pragma pack(pop)
#pragma option pop

#pragma delphiheader end.
//-- end unit ----------------------------------------------------------------
#endif	// StspawnHPP
