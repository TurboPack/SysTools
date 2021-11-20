unit Delphi.PropEd;

{$mode objfpc}{$H+}

interface

uses
  PropEdits;

type
  TStringProperty = class(PropEdits.TStringProperty)
  public
    property
      Value     : ansistring read GetValue write SetValue;
  end;

implementation

end.

