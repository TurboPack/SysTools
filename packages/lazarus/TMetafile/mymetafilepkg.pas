{ This file was automatically created by Lazarus. Do not edit!
This source is only used to compile and install the package.
 }

unit mymetafilepkg; 

interface

uses
  mymetafile, LazarusPackageIntf; 

implementation

procedure Register; 
begin
end; 

initialization
  RegisterPackage('mymetafilepkg', @Register); 
end.
