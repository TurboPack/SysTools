{ This file was automatically created by Lazarus. Do not edit!
  This source is only used to compile and install the package.
 }

unit SysToolsDR;

{$warn 5023 off : no warning about unused units}
interface

uses
  St2DBarC, StAstro, StAstroP, StBarC, StBarPN, StBase, StBCD, StBits, StColl, 
  StConst, StCRC, StDate, StDateSt, StDecMth, StDict, StDque, StEclpse, 
  StExpLog, StExpr, StFin, StFirst, StGenLog, StHASH, StIniStm, StJup, 
  StJupSat, StLArr, StList, StMars, StMath, StMerc, StMerge, StMime, StMoney, 
  StNeptun, StNet, StNetApi, StNetCon, StNetMsg, StNetPfm, StNTLog, StNVBits, 
  StNVColl, StNVCont, StNVDict, StNVDQ, StNVLAry, StNVList, StNVLMat, 
  StNVSCol, StNVTree, StOStr, StPluto, StPQueue, StPtrns, StRandom, StRegEx, 
  StRegIni, StSaturn, StSort, StSpawn, StStat, StStrL, StStrms, StSystem, 
  StText, StToHTML, StTree, StTxtDat, StUranus, StUtils, StVArr, StVenus, 
  StVInfo, StwmDCpy, graphics_delphi, System.Hash, AnsiStrings, 
  Delphi.Windows, Delphi.Character, LazarusPackageIntf;

implementation

procedure Register;
begin
end;

initialization
  RegisterPackage('SysToolsDR', @Register);
end.
