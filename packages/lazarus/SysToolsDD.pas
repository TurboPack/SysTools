{ This file was automatically created by Lazarus. Do not edit!
  This source is only used to compile and install the package.
 }

unit SysToolsDD;

{$warn 5023 off : no warning about unused units}
interface

uses
  StAbout0, StPropEd, StReg, Delphi.PropEd, LazarusPackageIntf;

implementation

procedure Register;
begin
end;

initialization
  RegisterPackage('SysToolsDD', @Register);
end.
