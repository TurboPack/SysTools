unit TestStBase;

interface

uses
  SysUtils, TestFramework;

type
  TTestStBase = class(TTestCase)
  published
    procedure TestHugeFillStruc;
  end;

implementation

uses
  StBase;

{ TTestStBase }

procedure TTestStBase.TestHugeFillStruc;
var
  pDest: TBytes;
  pSource: TBytes;
begin
  pDest := TBytes.Create(0, 1, 2, 3);
  pSource := TBytes.Create(4, 5);
  HugeFillStruc(pDest[0], Length(pDest), pSource[0], Length(pSource));
  Check(pDest[0] = 4);
  Check(pDest[1] = 5);
  Check(pDest[2] = 4);
  Check(pDest[3] = 5);

  pDest := TBytes.Create(0, 1, 2);
  pSource := TBytes.Create(4, 5);
  HugeFillStruc(pDest[0], Length(pDest), pSource[0], Length(pSource));
  Check(pDest[0] = 4);
  Check(pDest[1] = 5);
  Check(pDest[2] = 4);

  pDest := TBytes.Create(0, 1);
  pSource := TBytes.Create(4, 5);
  HugeFillStruc(pDest[0], Length(pDest), pSource[0], Length(pSource));
  Check(pDest[0] = 4);
  Check(pDest[1] = 5);

  pDest := TBytes.Create(0);
  pSource := TBytes.Create(4, 5);
  HugeFillStruc(pDest[0], Length(pDest), pSource[0], Length(pSource));
  Check(pDest[0] = 4);

  pDest := nil;
  pSource := TBytes.Create(4, 5);
  HugeFillStruc(pDest, Length(pDest), pSource[0], Length(pSource));
  Check(pDest = nil);
end;

initialization
  RegisterTest(TTestStBase.Suite);

end.

