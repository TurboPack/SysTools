// CodeGear C++Builder
// Copyright (c) 1995, 2016 by Embarcadero Technologies, Inc.
// All rights reserved

// (DO NOT EDIT: machine generated header) 'StExpr.pas' rev: 31.00 (Windows)

#ifndef StexprHPP
#define StexprHPP

#pragma delphiheader begin
#pragma option push
#pragma option -w-      // All warnings off
#pragma option -Vx      // Zero-length empty class member 
#pragma pack(push,8)
#include <System.hpp>
#include <SysInit.hpp>
#include <Winapi.Windows.hpp>
#include <System.Classes.hpp>
#include <Vcl.Controls.hpp>
#include <Winapi.Messages.hpp>
#include <Vcl.StdCtrls.hpp>
#include <System.SysUtils.hpp>
#include <System.Math.hpp>
#include <StBase.hpp>
#include <StConst.hpp>
#include <StMath.hpp>

//-- user supplied -----------------------------------------------------------

namespace Stexpr
{
//-- forward type declarations -----------------------------------------------
class DELPHICLASS TStExpression;
class DELPHICLASS TStExpressionEdit;
//-- type declarations -------------------------------------------------------
typedef System::Extended *PStFloat;

typedef System::Extended __fastcall (*TStFunction0Param)(void);

typedef System::Extended __fastcall (*TStFunction1Param)(System::Extended Value1);

typedef System::Extended __fastcall (*TStFunction2Param)(System::Extended Value1, System::Extended Value2);

typedef System::Extended __fastcall (*TStFunction3Param)(System::Extended Value1, System::Extended Value2, System::Extended Value3);

typedef System::Extended __fastcall (__closure *TStMethod0Param)(void);

typedef System::Extended __fastcall (__closure *TStMethod1Param)(System::Extended Value1);

typedef System::Extended __fastcall (__closure *TStMethod2Param)(System::Extended Value1, System::Extended Value2);

typedef System::Extended __fastcall (__closure *TStMethod3Param)(System::Extended Value1, System::Extended Value2, System::Extended Value3);

typedef void __fastcall (__closure *TStGetIdentValueEvent)(System::TObject* Sender, const System::UnicodeString Identifier, System::Extended &Value);

enum DECLSPEC_DENUM TStToken : unsigned char { ssStart, ssInIdent, ssInNum, ssInSign, ssInExp, ssEol, ssNum, ssIdent, ssLPar, ssRPar, ssComma, ssPlus, ssMinus, ssTimes, ssDiv, ssEqual, ssPower };

class PASCALIMPLEMENTATION TStExpression : public Stbase::TStComponent
{
	typedef Stbase::TStComponent inherited;
	
protected:
	bool FAllowEqual;
	int FLastError;
	int FErrorPos;
	System::UnicodeString FExpression;
	System::Classes::TNotifyEvent FOnAddIdentifier;
	TStGetIdentValueEvent FOnGetIdentValue;
	bool eBusyFlag;
	System::WideChar eCurChar;
	int eExprPos;
	System::Classes::TList* eIdentList;
	System::Classes::TList* eStack;
	TStToken eToken;
	System::UnicodeString eTokenStr;
	System::Extended lhs;
	System::Extended rhs;
	int __fastcall GetAsInteger(void);
	System::UnicodeString __fastcall GetAsString(void);
	int __fastcall FindIdent(System::UnicodeString Name);
	void __fastcall StackClear(void);
	int __fastcall StackCount(void);
	void __fastcall StackPush(const System::Extended Value);
	System::Extended __fastcall StackPeek(void);
	System::Extended __fastcall StackPop(void);
	bool __fastcall StackEmpty(void);
	void __fastcall DoOnAddIdentifier(void);
	void __fastcall GetBase(void);
	void __fastcall GetExpression(void);
	void __fastcall GetFactor(void);
	void __fastcall GetFunction(void);
	void __fastcall GetParams(int N);
	void __fastcall GetTerm(void);
	void __fastcall GetToken(void);
	System::Extended __fastcall PopOperand(void);
	void __fastcall RaiseExprError(int Code, int Column);
	
public:
	__fastcall virtual TStExpression(System::Classes::TComponent* AOwner);
	__fastcall virtual ~TStExpression(void);
	System::Extended __fastcall AnalyzeExpression(void);
	void __fastcall AddConstant(const System::UnicodeString Name, System::Extended Value);
	void __fastcall AddFunction0Param(const System::UnicodeString Name, TStFunction0Param FunctionAddr);
	void __fastcall AddFunction1Param(const System::UnicodeString Name, TStFunction1Param FunctionAddr);
	void __fastcall AddFunction2Param(const System::UnicodeString Name, TStFunction2Param FunctionAddr);
	void __fastcall AddFunction3Param(const System::UnicodeString Name, TStFunction3Param FunctionAddr);
	void __fastcall AddInternalFunctions(void);
	void __fastcall AddMethod0Param(const System::UnicodeString Name, TStMethod0Param MethodAddr);
	void __fastcall AddMethod1Param(const System::UnicodeString Name, TStMethod1Param MethodAddr);
	void __fastcall AddMethod2Param(const System::UnicodeString Name, TStMethod2Param MethodAddr);
	void __fastcall AddMethod3Param(const System::UnicodeString Name, TStMethod3Param MethodAddr);
	void __fastcall AddVariable(const System::UnicodeString Name, PStFloat VariableAddr);
	void __fastcall ClearIdentifiers(void);
	void __fastcall GetIdentList(System::Classes::TStrings* S);
	void __fastcall RemoveIdentifier(const System::UnicodeString Name);
	__property int AsInteger = {read=GetAsInteger, nodefault};
	__property System::Extended AsFloat = {read=AnalyzeExpression};
	__property System::UnicodeString AsString = {read=GetAsString};
	__property int ErrorPosition = {read=FErrorPos, nodefault};
	__property System::UnicodeString Expression = {read=FExpression, write=FExpression};
	__property int LastError = {read=FLastError, nodefault};
	
__published:
	__property bool AllowEqual = {read=FAllowEqual, write=FAllowEqual, default=1};
	__property System::Classes::TNotifyEvent OnAddIdentifier = {read=FOnAddIdentifier, write=FOnAddIdentifier};
	__property TStGetIdentValueEvent OnGetIdentValue = {read=FOnGetIdentValue, write=FOnGetIdentValue};
};


typedef void __fastcall (__closure *TStExprErrorEvent)(System::TObject* Sender, int ErrorNumber, const System::UnicodeString ErrorStr);

class PASCALIMPLEMENTATION TStExpressionEdit : public Stbase::TStBaseEdit
{
	typedef Stbase::TStBaseEdit inherited;
	
protected:
	bool FAutoEval;
	TStExpression* FExpr;
	TStExprErrorEvent FOnError;
	System::Classes::TNotifyEvent __fastcall GetOnAddIdentifier(void);
	TStGetIdentValueEvent __fastcall GetOnGetIdentValue(void);
	void __fastcall SetOnAddIdentifier(System::Classes::TNotifyEvent Value);
	void __fastcall SetOnGetIdentValue(TStGetIdentValueEvent Value);
	HIDESBASE MESSAGE void __fastcall CMExit(Winapi::Messages::TMessage &Msg);
	void __fastcall DoEvaluate(void);
	DYNAMIC void __fastcall KeyPress(System::WideChar &Key);
	
public:
	__fastcall virtual TStExpressionEdit(System::Classes::TComponent* AOwner);
	__fastcall virtual ~TStExpressionEdit(void);
	System::Extended __fastcall Evaluate(void);
	__property TStExpression* Expr = {read=FExpr};
	
__published:
	__property bool AutoEval = {read=FAutoEval, write=FAutoEval, default=0};
	__property System::Classes::TNotifyEvent OnAddIdentifier = {read=GetOnAddIdentifier, write=SetOnAddIdentifier};
	__property TStExprErrorEvent OnError = {read=FOnError, write=FOnError};
	__property TStGetIdentValueEvent OnGetIdentValue = {read=GetOnGetIdentValue, write=SetOnGetIdentValue};
public:
	/* TWinControl.CreateParented */ inline __fastcall TStExpressionEdit(HWND ParentWindow) : Stbase::TStBaseEdit(ParentWindow) { }
	
};


//-- var, const, procedure ---------------------------------------------------
extern DELPHI_PACKAGE System::StaticArray<System::WideChar, 9> StExprOperators;
extern DELPHI_PACKAGE double __fastcall AnalyzeExpr(const System::UnicodeString Expr);
extern DELPHI_PACKAGE void __fastcall TpVal(const System::UnicodeString S, System::Extended &V, int &Code);
}	/* namespace Stexpr */
#if !defined(DELPHIHEADER_NO_IMPLICIT_NAMESPACE_USE) && !defined(NO_USING_NAMESPACE_STEXPR)
using namespace Stexpr;
#endif
#pragma pack(pop)
#pragma option pop

#pragma delphiheader end.
//-- end unit ----------------------------------------------------------------
#endif	// StexprHPP
