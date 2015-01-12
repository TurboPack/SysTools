unit TestStStrZ;

interface

uses
  SysUtils, TestFramework;

type
  TTestStStrZ = class(TTestCase)
  published
    procedure TestCharStrZ;
    procedure TestPadChPrimZ;
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

procedure TTestStStrZ.TestPadChPrimZ;
var
  pBuffer: PAnsiChar;
  sBuffer: AnsiString;
begin
  SetLength(sBuffer, 5);
  sBuffer[1] := '1';
  sBuffer[2] := '2';
  sBuffer[3] := '3';
  pBuffer := pAnsiChar(sBuffer);
  PadChPrimZ(pBuffer, '4', 5);
  Check(sBuffer = '12344');

  sBuffer := '';
  SetLength(sBuffer, 5);
  sBuffer[1] := '1';
  sBuffer[2] := '2';
  sBuffer[3] := #0;
  pBuffer := pAnsiChar(sBuffer);
  PadChPrimZ(pBuffer, '4', 5);
  Check(sBuffer = '12444');
end;

initialization
  RegisterTest(TTestStStrZ.Suite);

end.

