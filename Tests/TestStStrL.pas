unit TestStStrL;

interface

uses
  SysUtils, TestFramework;

type
  TTestStStrL = class(TTestCase)
  published
    procedure TestMakeLetterSetL;
  end;

implementation

uses
  StStrL;

{ TTestStStrL }

procedure TTestStStrL.TestMakeLetterSetL;
var
  sBuffer: string;
  iBuffer: Integer;
begin
  sBuffer := 'A';
  iBuffer := MakeLetterSetL(sBuffer);
  Check(iBuffer = 1);

  sBuffer := 'a';
  iBuffer := MakeLetterSetL(sBuffer);
  Check(iBuffer = 1);

  sBuffer := 'b';
  iBuffer := MakeLetterSetL(sBuffer);
  Check(iBuffer = 2);

  sBuffer := 'c';
  iBuffer := MakeLetterSetL(sBuffer);
  Check(iBuffer = 4);

  sBuffer := 'd';
  iBuffer := MakeLetterSetL(sBuffer);
  Check(iBuffer = 8);

  sBuffer := 'z';
  iBuffer := MakeLetterSetL(sBuffer);
  Check(iBuffer = 33554432);

  sBuffer := '0';
  iBuffer := MakeLetterSetL(sBuffer);
  Check(iBuffer = 0);
end;

initialization
  RegisterTest(TTestStStrL.Suite);

end.

