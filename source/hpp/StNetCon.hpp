// CodeGear C++Builder
// Copyright (c) 1995, 2014 by Embarcadero Technologies, Inc.
// All rights reserved

// (DO NOT EDIT: machine generated header) 'StNetCon.pas' rev: 28.00 (Windows)

#ifndef StnetconHPP
#define StnetconHPP

#pragma delphiheader begin
#pragma option push
#pragma option -w-      // All warnings off
#pragma option -Vx      // Zero-length empty class member 
#pragma pack(push,8)
#include <System.hpp>	// Pascal unit
#include <SysInit.hpp>	// Pascal unit
#include <Winapi.Windows.hpp>	// Pascal unit
#include <System.Classes.hpp>	// Pascal unit
#include <StBase.hpp>	// Pascal unit

//-- user supplied -----------------------------------------------------------

namespace Stnetcon
{
//-- type declarations -------------------------------------------------------
enum DECLSPEC_DENUM TStNetConnectOptions : unsigned char { coUseConnectDialog, coPersistentConnection, coReadOnlyPath, coUseMRU, coHideRestoreBox, coPromptForAccount, coAlwaysPromptForAccount, coRedirectIfNeeded };

enum DECLSPEC_DENUM TStNetDisconnectOptions : unsigned char { doUseDisconnectDialog, doUpdateProfile, doForceFilesClosed, doPromptToForceFilesClosed };

typedef System::Set<TStNetConnectOptions, TStNetConnectOptions::coUseConnectDialog, TStNetConnectOptions::coRedirectIfNeeded> TStNetConnectOptionsSet;

typedef System::Set<TStNetDisconnectOptions, TStNetDisconnectOptions::doUseDisconnectDialog, TStNetDisconnectOptions::doPromptToForceFilesClosed> TStNetDisconnectOptionsSet;

typedef void __fastcall (__closure *TOnConnectFailEvent)(System::TObject* Sender, unsigned ErrorCode);

typedef void __fastcall (__closure *TOnConnectCancelEvent)(System::TObject* Sender, unsigned ErrorCode);

typedef void __fastcall (__closure *TOnDisconnectFailEvent)(System::TObject* Sender, unsigned ErrorCode);

typedef void __fastcall (__closure *TOnDisconnectCancelEvent)(System::TObject* Sender, unsigned ErrorCode);

class DELPHICLASS TStNetConnection;
class PASCALIMPLEMENTATION TStNetConnection : public Stbase::TStComponent
{
	typedef Stbase::TStComponent inherited;
	
protected:
	System::UnicodeString FLocalDevice;
	System::UnicodeString FPassword;
	System::UnicodeString FServerName;
	System::UnicodeString FShareName;
	System::UnicodeString FUserName;
	TStNetConnectOptionsSet FConnectOptions;
	TStNetDisconnectOptionsSet FDisconnectOptions;
	System::Classes::TNotifyEvent FOnConnect;
	TOnConnectFailEvent FOnConnectFail;
	TOnConnectCancelEvent FOnConnectCancel;
	System::Classes::TNotifyEvent FOnDisconnect;
	TOnDisconnectFailEvent FOnDisconnectFail;
	TOnDisconnectCancelEvent FOnDisconnectCancel;
	
private:
	System::UnicodeString __fastcall GetServerName(void);
	void __fastcall SetServerName(System::UnicodeString Value);
	
public:
	__fastcall virtual TStNetConnection(System::Classes::TComponent* AOwner);
	__fastcall virtual ~TStNetConnection(void);
	unsigned __fastcall Connect(void);
	unsigned __fastcall Disconnect(void);
	__property System::UnicodeString Password = {read=FPassword, write=FPassword};
	__property System::UnicodeString UserName = {read=FUserName, write=FUserName};
	
__published:
	__property TStNetConnectOptionsSet ConnectOptions = {read=FConnectOptions, write=FConnectOptions, nodefault};
	__property TStNetDisconnectOptionsSet DisconnectOptions = {read=FDisconnectOptions, write=FDisconnectOptions, nodefault};
	__property System::UnicodeString LocalDevice = {read=FLocalDevice, write=FLocalDevice};
	__property System::UnicodeString ServerName = {read=GetServerName, write=SetServerName};
	__property System::UnicodeString ShareName = {read=FShareName, write=FShareName};
	__property System::Classes::TNotifyEvent OnConnect = {read=FOnConnect, write=FOnConnect};
	__property TOnConnectFailEvent OnConnectFail = {read=FOnConnectFail, write=FOnConnectFail};
	__property TOnConnectCancelEvent OnConnectCancel = {read=FOnConnectCancel, write=FOnConnectCancel};
	__property System::Classes::TNotifyEvent OnDisconnect = {read=FOnDisconnect, write=FOnDisconnect};
	__property TOnDisconnectFailEvent OnDisconnectFail = {read=FOnDisconnectFail, write=FOnDisconnectFail};
	__property TOnDisconnectCancelEvent OnDisconnectCancel = {read=FOnDisconnectCancel, write=FOnDisconnectCancel};
};


//-- var, const, procedure ---------------------------------------------------
}	/* namespace Stnetcon */
#if !defined(DELPHIHEADER_NO_IMPLICIT_NAMESPACE_USE) && !defined(NO_USING_NAMESPACE_STNETCON)
using namespace Stnetcon;
#endif
#pragma pack(pop)
#pragma option pop

#pragma delphiheader end.
//-- end unit ----------------------------------------------------------------
#endif	// StnetconHPP
