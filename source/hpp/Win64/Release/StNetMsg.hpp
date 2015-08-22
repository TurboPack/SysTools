// CodeGear C++Builder
// Copyright (c) 1995, 2015 by Embarcadero Technologies, Inc.
// All rights reserved

// (DO NOT EDIT: machine generated header) 'StNetMsg.pas' rev: 30.00 (Windows)

#ifndef StnetmsgHPP
#define StnetmsgHPP

#pragma delphiheader begin
#pragma option push
#pragma option -w-      // All warnings off
#pragma option -Vx      // Zero-length empty class member 
#pragma pack(push,8)
#include <System.hpp>
#include <SysInit.hpp>
#include <Winapi.Windows.hpp>
#include <System.Classes.hpp>
#include <StBase.hpp>

//-- user supplied -----------------------------------------------------------

namespace Stnetmsg
{
//-- forward type declarations -----------------------------------------------
class DELPHICLASS TStNetMessage;
//-- type declarations -------------------------------------------------------
class PASCALIMPLEMENTATION TStNetMessage : public Stbase::TStComponent
{
	typedef Stbase::TStComponent inherited;
	
private:
	System::Classes::TStringList* FAliasNames;
	System::UnicodeString FMsgFrom;
	System::UnicodeString FMsgText;
	System::UnicodeString FMsgTo;
	System::UnicodeString FServerName;
	System::Classes::TNotifyEvent FOnMessageSent;
	
protected:
	System::UnicodeString __fastcall GetServer(void);
	void __fastcall SetServer(System::UnicodeString Value);
	
public:
	__fastcall virtual TStNetMessage(System::Classes::TComponent* AOwner);
	__fastcall virtual ~TStNetMessage(void);
	void __fastcall AddAlias(System::UnicodeString AName);
	System::Classes::TStringList* __fastcall AliasNames(void);
	void __fastcall RemoveAlias(System::UnicodeString AName);
	void __fastcall Send(void);
	
__published:
	__property System::UnicodeString MsgFrom = {read=FMsgFrom, write=FMsgFrom};
	__property System::UnicodeString MsgText = {read=FMsgText, write=FMsgText};
	__property System::UnicodeString MsgTo = {read=FMsgTo, write=FMsgTo};
	__property System::UnicodeString Server = {read=GetServer, write=SetServer};
	__property System::Classes::TNotifyEvent OnMessageSent = {read=FOnMessageSent, write=FOnMessageSent};
};


//-- var, const, procedure ---------------------------------------------------
}	/* namespace Stnetmsg */
#if !defined(DELPHIHEADER_NO_IMPLICIT_NAMESPACE_USE) && !defined(NO_USING_NAMESPACE_STNETMSG)
using namespace Stnetmsg;
#endif
#pragma pack(pop)
#pragma option pop

#pragma delphiheader end.
//-- end unit ----------------------------------------------------------------
#endif	// StnetmsgHPP
