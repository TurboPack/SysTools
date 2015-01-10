unit TestStUtils;

interface

uses
  SysUtils, TestFramework;

type
  TTestStUtils = class(TTestCase)
  published
    procedure TestExchangeLongInts;
  end;

implementation

uses
  StUtils;

{ TTestStUtils }

procedure TTestStUtils.TestExchangeLongInts;
var
  iLeft, iRight: Integer;
begin
  iLeft := 2;
  iRight := 3;

  ExchangeLongInts(iLeft, iRight);
  Check(iLeft = 3);
  Check(iRight = 2);
end;

initialization
  RegisterTest(TTestStUtils.Suite);

end.

