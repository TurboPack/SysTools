unit TestStStrZ;

interface

uses
  SysUtils, TestFramework;

type
  TTestStStrZ = class(TTestCase)
  published
    procedure TestCharStrZ;
  end;

implementation

uses
  AnsiStrings, StStrZ;

{ TTestStStrZ }

procedure TTestStStrZ.TestCharStrZ;
var
  pBuffer: PAnsiChar;
  sBuffer: AnsiString;
begin
  SetLength(sBuffer, 5);
  pBuffer := pAnsiChar(sBuffer);
  CharStrZ(pBuffer, 't', Length(sBuffer));

  Check(sBuffer = 'ttttt');
end;

initialization
  RegisterTest(TTestStStrZ.Suite);

end.

