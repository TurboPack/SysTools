unit Delphi.Character;

{$mode objfpc}{$H+}
{$MODESWITCH TYPEHELPERS}
{$MODESWITCH ADVANCEDRECORDS}

interface

uses
  Classes, SysUtils
  , Character
  ;

type

TCharHelper = type helper for Char
  public
  function IsLetter: Boolean; overload;inline;

  function ToLower: Char; overload;inline;
  function ToUpper: Char; overload;inline;
  class function ToLower(const S: string): string; overload; static; inline;
  class function ToUpper(const S: string): string; overload; static; inline;

end;

implementation

function TCharHelper.IsLetter: Boolean;
begin
  result := TCharacter.IsLetter(Self);
end;

function TCharHelper.ToLower: Char;
begin
  result := TCharacter.ToLower(Self);
end;

function TCharHelper.ToUpper: Char;
begin
  result := TCharacter.ToUpper(Self);
end;

class function TCharHelper.ToLower(const S: string): string;
begin
  result := TCharacter.ToLower(S);
end;

class function TCharHelper.ToUpper(const S: string): string;
begin
  result := TCharacter.ToUpper(S);
end;

end.

