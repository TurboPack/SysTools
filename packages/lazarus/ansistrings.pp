unit AnsiStrings;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils;

function StrLen(const Str: AnsiString): SizeInt;inline;overload;
function StrLen(const Str: PAnsiChar): SizeInt;inline;overload;
function StrLCopy(Dest: PAnsiChar; const Source: PAnsiChar; MaxLen: Cardinal): PAnsiChar;overload;inline;
function StrPas(const Str: PAnsiChar): AnsiString;overload;inline;

implementation


function StrLen(const Str: AnsiString): SizeInt;
begin
  result := length(Str);
end;

function StrLen(const Str: PAnsiChar): SizeInt;
begin
  result := system.StrLen(Str);
end;

function StrLCopy(Dest: PAnsiChar; const Source: PAnsiChar; MaxLen: Cardinal): PAnsiChar;
begin
  result := sysutils.StrLCopy(dest, source, maxlen);
end;

function StrPas(const Str: PAnsiChar): AnsiString;
begin
  result := sysutils.StrPas(Str);
end;

end.

