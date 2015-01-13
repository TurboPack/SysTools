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
{* SysTools: StReg.pas 4.04                              *}
{*********************************************************}
{* SysTools: Component Registration Unit                 *}
{*********************************************************}

{$I StDefine.inc}

{$R StReg.r32}

unit StReg;

interface

uses
  Classes,
  DesignIntf,
  DesignEditors;

procedure Register;

implementation

uses
  StBase,
  StAbout0,

  { components }
  St2DBarC,
  StBarC,
  StBarPN,
  StNetCon,
  StNetMsg,
  StNetPfm,
  StNVBits,
  StNVColl,
  StNVDict,
  StNVDQ,
  StNVLAry,
  StNVList,
  StNVLMat,
  StNVSCol,
  StNVTree,
  StRegEx,
  StSpawn,
  StToHTML,
  StVInfo,
  StWMDCpy,

  {forces these units to be compiled when components are installed}
  {vvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvv}
  StAstro,
  StAstroP,
  StBCD,
  StBits,
  StColl,
  StConst,
  StCrc,
  StDate,
  StDateSt,
  StDict,
  StDQue,
  StEclpse,
  StExpr,
  StFIN,
  StFirst,
  StHASH,
  StJup,
  StJupsat,
  StLArr,
  StList,
  StMars,
  StMath,
  StMerc,
  StMime,
  StNeptun,
  StNet,
  StNetApi,
  StNVCont,
  StOStr,
  StPluto,
  StPQueue,
  StRegIni,
  StSaturn,
  StSort,
  StStat,
  StStrL,
  StStrms,
  StStrZ,
  StText,
  StTree,
  StUranus,
  StUtils,
  StVArr,
  StVenus,
  { new units in ver 4: }
  StIniStm,
  StMerge,
  StSystem,
  StTxtDat,
  StDecMth,
  StMoney,
  StRandom,
  StNTLog,
  { !!! StExpEng unit designed to handle problem with initialization }
  { section in C++Builder; should NOT be included in Registration unit }
  { nor in Run-time package !!! }
  {StExpEng,}
  StExpLog,
  StGenLog,
  StPtrns,


  {^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^}
  StPropEd;

procedure Register;
begin
  RegisterPropertyEditor(TypeInfo(string), TStComponent, 'Version',
                         TStVersionProperty);
  RegisterPropertyEditor(TypeInfo(string), TStBaseEdit, 'Version',
                         TStVersionProperty);
  RegisterPropertyEditor(TypeInfo(string), TStBarCode, 'Version',
                         TStVersionProperty);
  RegisterPropertyEditor(TypeInfo(string), TStPNBarCode, 'Version',
                         TStVersionProperty);
  RegisterPropertyEditor(TypeInfo(string), TStRegEx, 'InputFile',
                         TStGenericFileNameProperty);
  RegisterPropertyEditor(TypeInfo(string), TStRegEx, 'OutputFile',
                         TStGenericFileNameProperty);
  RegisterPropertyEditor(TypeInfo(string), TStFileToHTML, 'InFileName',
                         TStGenericFileNameProperty);
  RegisterPropertyEditor(TypeInfo(string), TStFileToHTML, 'OutFileName',
                         TStGenericFileNameProperty);
  RegisterPropertyEditor(TypeInfo(string), TStVersionInfo, 'FileName',
                         TStFileNameProperty);
  RegisterPropertyEditor(TypeInfo(string), TStSpawnApplication, 'FileName',
                         TStGenericFileNameProperty);

  RegisterComponents('SysTools',
    [
     TStNetConnection,
     TStNetPerformance,
     TStNetMessage,
     TStVersionInfo,
     TStExpression,
     TStExpressionEdit,
     TStBarCode,
     TStPNBarCode,
     TStRegEx,
     TStWMDataCopy,
     TStFileToHTML,
     TStSpawnApplication,
// new in SysTools 4
     TStPDF417Barcode,
     TStMaxiCodeBarcode,
     TStGeneralLog,
     TStExceptionLog,
     TStNTEventLog
     ]);

  {non-visual container class components}
  RegisterComponents('SysTools (CC)',
    [TStNVBits,
     TStNVCollection,
     TStNVDictionary,
     TStNVDQue,
     TStNVLArray,
     TStNVList,
     TStNVLMatrix,
     TStNVSortedCollection,
     TStNVTree]);
end;

end.
