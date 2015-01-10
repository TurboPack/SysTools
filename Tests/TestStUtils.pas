unit TestStUtils;

interface

uses
  SysUtils, TestFramework;

type
  TTestStUtils = class(TTestCase)
  strict private type
    TStruct = record
      Left: Integer;
      Right: Integer;
    end;
    TWordStruct = record
      Left: Word;
      Right: Word;
    end;
  published
    procedure TestExchangeLongInts;
    procedure TestExchangeStructs;
    procedure TestFillWord;
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

procedure TTestStUtils.TestExchangeStructs;
var
  pLeft: TStruct;
  pRight: TStruct;
begin
  pLeft.Left := 1;
  pLeft.Right := 2;

  pRight.Left := 3;
  pRight.Right := 4;

  ExchangeStructs(pLeft, pRight, SizeOf(TStruct));

  Check(pLeft.Left = 3);
  Check(pLeft.Right = 4);

  Check(pRight.Left = 1);
  Check(pRight.Right = 2);
end;

procedure TTestStUtils.TestFillWord;
var
  pStruct: TWordStruct;
  iWord: Word;
begin
  pStruct.Left := 1;
  pStruct.Right := 2;

  iWord := 3;
  FillWord(pStruct, SizeOf(pStruct), iWord);

  Check(pStruct.Left = 3);
  Check(pStruct.Right = 3);
end;

initialization
  RegisterTest(TTestStUtils.Suite);

end.

