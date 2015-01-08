(* ***** BEGIN LICENSE BLOCK *****
 * Version: MPL 1.1
 *
 * The contents of this file are subject to the Mozilla Public License Version
 * 1.1 (the "License"); you may not use this file except in compliance with
 * the License. You may obtain a copy of the License at
 * http://www.mozilla.org/MPL/
 *
 * Software distributed under the License is distributed on an "AS IS" basis,
 * WITHOUT WARRANTY OF ANY KIND, either express or implied. See the License
 * for the specific language governing rights and limitations under the
 * License.
 *
 * The Original Code is TurboPower SysTools
 *
 * The Initial Developer of the Original Code is
 * TurboPower Software
 *
 * Portions created by the Initial Developer are Copyright (C) 1996-2002
 * the Initial Developer. All Rights Reserved.
 *
 * Contributor(s):
 *
 * ***** END LICENSE BLOCK ***** *)

unit RIEditU1;

interface

uses
  Windows,
  Messages,
  Graphics,
  Classes,
  SysUtils,
  Dialogs,
  Controls,
  Forms,
  StdCtrls,
  Outline,
  ExtCtrls,
  Buttons,
  Menus,
  Grids,

{$IFOPT H+}
  STStrL,
{$ELSE}
  STStrS,
{$ENDIF}
  STConst,
  STBase;

type
  TForm1 = class(TForm)
    Outline1: TOutline;
    Panel1: TPanel;
    IniFileCB: TCheckBox;
    Label1: TLabel;
    Edit1: TEdit;
    CancelBtn: TBitBtn;
    BrowseBtn: TButton;
    OpenDialog1: TOpenDialog;
    LoadBtn: TButton;
    PopupMenu1: TPopupMenu;
    DeleteAKey: TMenuItem;
    AddKey: TMenuItem;
    AddValue: TMenuItem;
    ListBox1: TListBox;
    N1: TMenuItem;
    ListBoxMenu: TPopupMenu;
    ModifyValue: TMenuItem;
    RenameValue: TMenuItem;
    DeleteValue: TMenuItem;
    N2: TMenuItem;
    AddItem: TMenuItem;
    procedure CancelBtnClick(Sender: TObject);
    procedure FormCreate(Sender: TObject);
    procedure BrowseBtnClick(Sender: TObject);
    procedure IniFileCBClick(Sender: TObject);
    procedure LoadBtnClick(Sender: TObject);
    procedure DeleteAKeyClick(Sender: TObject);
    procedure AddKeyClick(Sender: TObject);
    procedure AddValueClick(Sender: TObject);
    procedure Outline1Expand(Sender: TObject; Index: Longint);
    procedure Outline1Click(Sender: TObject);
    procedure ListBox1DrawItem(Control: TWinControl; Index: Integer;
      Rect: TRect; State: TOwnerDrawState);
    procedure FormActivate(Sender: TObject);
    procedure Outline1Collapse(Sender: TObject; Index: Longint);
    procedure Outline1DblClick(Sender: TObject);
    procedure DeleteValueClick(Sender: TObject);
    procedure RenameValueClick(Sender: TObject);
    procedure ModifyValueClick(Sender: TObject);
    procedure ListBox1DblClick(Sender: TObject);
  private
    { Private declarations }
  public
    { Public declarations }

    procedure SetBusy(Busy : Boolean);

    procedure FillListBox;

    procedure LoadIniFileData;
    procedure LoadRegistryData;

    procedure GetIniSectionName(var SN : string; var Index : integer);

    procedure ModifyIniItem(IniItem : string);
    procedure ModifyRegItem(RegItem : string; ModifyValue : Boolean);

    procedure RenameIniItem(IniItem : string);
    procedure RenameRegItem(RegItem : string);
  end;

var
  Form1: TForm1;

implementation

{$R *.DFM}

uses
  STDate,
  STDateSt,
  STRegIni,
  RIEditU2;

var
  TC : TStRegIni;

procedure TForm1.CancelBtnClick(Sender: TObject);
begin
  Close;
end;

procedure TForm1.FormCreate(Sender: TObject);
begin
  ListBox1.Clear;
  Outline1.Clear;

{DO NOT ERASE THE FOLLOWING SECITON - FOR INI STARTUP}
  IniFileCB.Checked := True;
{End of Section}

{DO NOT ERASE THE FOLLOWING SECITON - FOR REG STARTUP}
{
  IniFileCB.Checked := False;
  Edit1.Text := 'HKEY_CLASSES_ROOT';
  TC := TStRegIni.Create(Edit1.Text, False);
  TC.CurSubKey := '';
}
{End of Seciton}

  BrowseBtn.Enabled := IniFileCB.Checked;
  Edit1.Enabled := IniFileCB.Checked;
  Edit1.ReadOnly := NOT IniFileCB.Checked;

  if Assigned(TC) and not TC.IsIniFile then
    LoadRegistryData;
end;


procedure TForm1.SetBusy(Busy : Boolean);
begin
  if Busy then
    Screen.Cursor := crHourGlass
  else
    Screen.Cursor := crDefault;
end;


procedure TForm1.FillListBox;
begin
  ListBox1.Clear;
  ListBox1.Perform(WM_SetRedraw,0,0);
  try
    TC.GetValues(ListBox1.Items);
  finally
    ListBox1.Perform(WM_SetRedraw,1,0);
    ListBox1.Update;
  end;
end;


procedure TForm1.LoadIniFileData;
var
  I  : Integer;
  S  : string;
  SKList   : TStringList;

begin
  SetBusy(True);

  Outline1.Clear;
  TC.CurSubKey := '';
  SKList := TStringList.Create;
  try
    S := Edit1.Text;
    I := pos('.',S);
    if (I > 0) then
      Delete(S,I,Length(S)-I+1);
    I := Length(S);
    while S[I] <> '\' do
      Dec(I);
    Delete(S,1,I);

    Outline1.Add(0,S);
    TC.GetSubKeys(SKList);
    if (SKList.Count > 0) then
    begin
      for I := 0 to SKList.Count-1 do
      begin
        with Outline1 do
        begin
          AddChild(1,SKList[I]);

          SelectedItem := GetTextItem(SKList[I]);
          Items[SelectedItem].Expanded := False;
        end;
      end;
      TC.CurSubKey := SKList[0];
    end;
  finally
    SKList.Free;

    Outline1.SelectedItem := 1;
    Outline1.Refresh;

    SetBusy(False);
  end;
end;

procedure TForm1.Outline1Click(Sender: TObject);
var
  S  : string;
  I  : Integer;

begin
  if NOT (TC.IsIniFile) then
  begin
    S := Outline1.Items[Outline1.SelectedItem].FullPath;
    I := pos('=',S);
    if I > 0 then
      Delete(S,I,Length(S)-I+1);
    Edit1.Text := S;
  end;
end;


procedure TForm1.Outline1Expand(Sender: TObject; Index: Longint);
var
  Idx, I, J : integer;

  PriKey,
  S,
  HldSK,
  SelStr  : string;

  SK      : TStringList;

begin
  if (TC.IsIniFile) then
  begin
    with Outline1 do
    begin
      if SelectedItem < 2 then
        Exit
      else begin
        S := Items[Outline1.SelectedItem].Text;
        TC.CurSubKey := S;
        FillListBox;
      end;
    end;
    Exit;
  end;

  ListBox1.Clear;
  SetBusy(True);
  HldSK := TC.CurSubKey;
  with Outline1 do
  begin
    SelStr := Items[Index].FullPath;
    if pos('HKEY_LOCAL_MACHINE',SelStr) > 0 then
      PriKey := RIMachine
    else if pos('HKEY_USERS',SelStr) > 0 then
      PriKey := RIUsers
    else if pos('HKEY_CURRENT_USER',SelStr) > 0 then
      PriKey := RICUser
    else if pos('HKEY_CLASSES_ROOT',SelStr) > 0 then
      PriKey := RIRoot;
    TC.SetPrimary(PriKey);

    I := pos('\',SelStr);
    if (I = 0) then begin
      Edit1.Text := SelStr;
      SetBusy(False);
    end else
    begin
      SK := TStringList.Create;
      try
        System.Delete(SelStr,1,I);
        TC.CurSubKey := SelStr;

        FillListBox;
        if NOT (Items[Index].HasItems) then
        begin
          TC.GetSubKeys(SK);
          for J := 0 to SK.Count-1 do
            AddChild(Index,SK[J]);
        end else
        begin
          Idx := Items[Index].GetFirstChild;
          while (Idx <> -1) do
          begin
            SelStr := Items[Idx].FullPath;
            System.Delete(SelStr,1,pos('\',SelStr));
            TC.CurSubKey := SelStr;
            if NOT (Items[Idx].HasItems) then
            begin
              TC.GetSubKeys(SK);
              for J := 0 to SK.Count-1 do
                AddChild(Idx,SK[J]);
            end;
            SK.Clear;
            Idx := Items[Index].GetNextChild(Idx);
          end;
        end;
      finally
        SK.Free;
        TC.CurSubKey := HldSK;
        SetBusy(False);
      end;
    end;
  end;
  Outline1.Refresh;
end;


procedure TForm1.LoadRegistryData;
var
  Idx,
  I, J, K  : Integer;

  TheKey,
  PriKey   : string;

  ISKList,
  SKList   : TStringList;

begin
  if not Assigned(TC) then
    Exit;

  SetBusy(True);
  Outline1.Clear;
  SKList := TStringList.Create;
  try
    Edit1.Text := 'HKEY_CLASSES_ROOT';
    AddValue.Visible := True;
    RenameValue.Visible := True;
    DeleteValue.Visible := True;
    N2.Visible := True;

    for I := 1 to 4 do
    begin
      case I of
        1 : begin
              TheKey := 'HKEY_CLASSES_ROOT';
              PriKey := RIRoot;
            end;
        2 : begin
              TheKey := 'HKEY_CURRENT_USER';
              PriKey := RICUser;
            end;
        3 : begin
              TheKey := 'HKEY_LOCAL_MACHINE';
              PriKey := RIMachine;
            end;
        4 : begin
              TheKey := 'HKEY_USERS';
              PriKey := RIUsers;
            end;
      end;
      SKList.Clear;

      Outline1.Add(0,TheKey);

      TC.CurSubKey := '';
      TC.SetPrimary(PriKey);
      TC.GetSubKeys(SKList);

      with Outline1 do
      begin
        SelectedItem := GetTextItem(TheKey);
        for J := 0 to SKList.Count-1 do
        begin
          AddChild(SelectedItem,SKList[J]);
          Idx := Items[SelectedItem].GetLastChild;
          ISKList := TStringList.Create;
          try
            TC.CurSubKey := SKList[J];
            try
              TC.GetSubKeys(ISKList);
              if (ISKList.Count > 0) then
                for K := 0 to ISKList.Count-1 do
                  AddChild(Idx,ISKList[K]);
            except
              {In some cases, WinNT in particularl, GetSubKeys raises an
               exception because it tries to access a key to which *no one* has
               access. Here we throw away the exception so the outline can
               continue being filled}
            end;
          finally
            ISKList.Free;
          end;
        end;
        Items[SelectedItem].Expanded := False;
      end;
    end;
  finally
    SKList.Free;
    TC.CurSubKey := '';
    SetBusy(False);
    Outline1.SelectedItem := 1;
    Outline1.Refresh;
  end;
end;


procedure TForm1.GetIniSectionName(var SN : string; var Index : integer);
var
  p : integer;
  S : string;
begin
  with Outline1 do
  begin
    p := SelectedItem;
    S := Items[p].Text;

    while (p > 0) AND (pos('=',S) > 0) do
    begin
      S := Items[p].Text;
      if (pos('=',S) > 0) then
        Dec(p);
    end;
    SN := Items[p].Text;
    Index := p;
  end;
end;


procedure TForm1.BrowseBtnClick(Sender: TObject);
begin
  if (OpenDialog1.Execute) then
  begin
    Edit1.Text := OpenDialog1.FileName;
    TC.Free;
    TC := TStRegIni.Create(Edit1.Text,True);
    LoadIniFileData;
  end;
end;


procedure TForm1.IniFileCBClick(Sender: TObject);
begin
  Outline1.Clear;
  ListBox1.Clear;

  BrowseBtn.Enabled := IniFileCB.Checked;
  Edit1.Enabled := IniFileCB.Checked;
  Edit1.ReadOnly := NOT IniFileCB.Checked;

  if NOT IniFileCB.Checked then
  begin
    LoadBtn.Caption := '&Refresh';
    Edit1.Text := 'HKEY_CLASSES_ROOT';
    TC.Free;
    TC := TStRegIni.Create(Edit1.Text,False);
    TC.CurSubKey := '';
    LoadRegistryData;
  end else
  begin
    Edit1.Text := '';
    LoadBtn.Caption := 'Loa&d';
  end;
end;


procedure TForm1.LoadBtnClick(Sender: TObject);
begin
  ListBox1.Clear;
  if (IniFileCB.Checked) then
  begin
    if NOT FileExists(Edit1.Text) then Exit;
    TC.Free;
    TC := nil;
    TC := TStRegIni.Create(Edit1.Text,True);
    LoadIniFileData;
  end else
  begin
    TC.Free;
    TC := nil;
    TC := TStRegIni.Create(Edit1.Text,False);
    LoadRegistryData;
  end;
end;


procedure TForm1.DeleteAKeyClick(Sender: TObject);
var
  p,
  Idx     : Integer;
  SK      : string;

begin
  if Outline1.SelectedItem = 0 then
      Exit;
  Outline1.Perform(WM_SETREDRAW,0,0);
  try
    if (TC.IsIniFile) then
    begin
      GetIniSectionName(SK,Idx);
      TC.CurSubKey := SK;
    end else
    begin
      SK := Edit1.Text;
      p := pos('\',SK);
      if (p = 0) then
      begin
        ShowMessage('Can not delete primary key');
        Exit;
      end;
      Delete(SK,1,p);
      TC.CurSubKey := SK;
      Idx := Outline1.SelectedItem;
    end;
    TC.DeleteKey(SK,False);
    Outline1.Delete(Outline1.SelectedItem);

  finally
    Outline1.Perform(WM_SETREDRAW,1,0);
    ListBox1.Clear;
    Outline1.Refresh;
  end;
end;


procedure TForm1.AddKeyClick(Sender: TObject);
var
  SK,
  NewName : string;
begin
  NewName := '';
  if InputQuery('New Name','',NewName) then
  begin
    Outline1.Perform(WM_SETREDRAW,0,0);
    try
      if (TC.IsIniFile) then
      begin
        TC.CreateKey(NewName);
        TC.CurSubKey := NewName;
        with Outline1 do
        begin
          Add(0,NewName);
          SelectedItem := GetTextItem(NewName);
        end;
      end else
      begin
        with Outline1 do
        begin
          TC.CurSubKey := '';
          SK := Items[SelectedItem].FullPath + '\' + NewName;
          System.Delete(SK,1,pos('\',SK));
          TC.CreateKey(SK);
          AddChild(SelectedItem,NewName);
        end;
      end;
    finally
      Outline1.Perform(WM_SETREDRAW,1,0);
      Outline1.Refresh;
    end;
  end;
end;


procedure TForm1.AddValueClick(Sender: TObject);
var
  len,
  Code,
  SectionIndex : integer;
  SValue,
  NewName      : string;
  SectionName  : string;
  TmpVal       : array[1..127] of byte;
  ADate        : TStDate;
  ATime        : TStTime;
  AFloat       : Double;
  ALongInt     : LongInt;

begin
  DataDlg.ValueName.Text := '';
  DataDlg.IData.Text := '';
  DataDlg.EditingState := etAll;

  if (DataDlg.ShowModal = mrOK) then
  begin
    Outline1.Perform(WM_SETREDRAW,0,0);
    try
      NewName := DataDlg.ValueName.Text;
      SValue := DataDlg.IData.Text;

      if (TC.IsIniFile) then
      begin
        GetIniSectionName(SectionName,SectionIndex);
        TC.CurSubKey := SectionName;
      end else
      begin
        SectionName := Edit1.Text;
        Delete(SectionName,1,pos('\',SectionName));
        TC.CurSubKey := SectionName;
        SectionIndex := Outline1.SelectedItem;
      end;

      case DataDlg.DataTypeRG.ItemIndex of
        0 : begin
              len := Length(SValue);
              if ((len mod 2) <> 0) then
              begin
                if (len > 2) then
                begin
                  Delete(SValue,len,1);
                  Dec(len);
                end else
                begin
                  SValue := '00';
                  len := 2;
                end;
                ShowMessage('String was adjusted to even number of characters');
              end;
              if (TC.StringToBytes(SValue, TmpVal, len div 2)) then
                TC.WriteBinaryData(NewName, TmpVal,len div 2)
              else
                ShowMessage('Error converting string to Byte array');
            end;
        1 : begin
              if (CompareText(SValue,LoadStr(stscTrueString)) = 0) then
              begin
                TC.WriteBoolean(NewName,True);
                SValue := LoadStr(stscTrueString);
              end else
              begin
                TC.WriteBoolean(NewName,False);
                SValue := LoadStr(stscFalseString);
              end;
            end;
        2 : begin
              ADate := DateStringToStDate(InternationalDate(False),SValue, 1950);
              if (ADate <> BadDate) then
                TC.WriteDate(NewName,ADate)
              else
                ShowMessage('Invalid date or string did not match Windows short date mask');
            end;
        3 : begin
              Val(SValue,ALongInt,Code);
              if (Code = 0) then
                TC.WriteInteger(NewName,ALongInt)
              else
                ShowMessage('String could not be converted to a LongInt');
            end;
        4 : begin
              Val(SValue,AFloat,Code);
              if (Code = 0) then
                TC.WriteFloat(NewName,AFloat)
              else
                ShowMessage('String could not be converted to a Double');
            end;
        5 : begin
              TC.WriteString(NewName,DataDlg.IData.Text);
              SValue := DataDlg.IData.Text;
            end;
        6 : begin
              SectionName := InternationalTime(True);
              ATime := TimeStringToStTime(InternationalTime(True),SValue);
              if (ATime <> BadTime) then
                TC.WriteTime(NewName,ATime)
              else
                ShowMessage('Invalid time or string did not match Windows time mask');
            end;
      end;
    finally
      Outline1.Perform(WM_SETREDRAW,1,0);
      FilLListBox;
      Outline1.Refresh;
    end;
  end;
end;

procedure TForm1.DeleteValueClick(Sender: TObject);
var
  p,
  lbidx,
  len,
  Idx    : Integer;

  SK,
  VN     : string;
begin
  lbidx := ListBox1.ItemIndex;
  if (lbidx) < 0 then
  begin
    ShowMessage('No value selected');
    Exit;
  end;

  VN := ListBox1.Items[lbidx];
  p := pos('=',VN);
  len := Length(VN);
  System.Delete(VN,p,len-p+1);

  if (TC.IsIniFile) then
  begin
    GetIniSectionName(SK,Idx);
    TC.CurSubKey := SK;
  end else
  begin
    SK := Edit1.Text;
    Delete(SK,1,pos('\',SK));
    TC.CurSubKey := SK;
  end;
  TC.DeleteValue(VN);
  ListBox1.Items.Delete(lbidx);
end;


procedure TForm1.ListBox1DblClick(Sender: TObject);
begin
  ModifyValueClick(Sender);
end;


procedure TForm1.ModifyValueClick(Sender: TObject);
var
  Idx : Integer;
begin
  Idx := ListBox1.ItemIndex;
  if (Idx < 0) then
  begin
    ShowMessage('No value selected');
    Exit;
  end;

  if (TC.IsIniFile) then
    ModifyIniItem(ListBox1.Items[Idx])
  else
    ModifyRegItem(ListBox1.Items[Idx],True);
end;


procedure TForm1.ModifyIniItem(IniItem : string);
var
  p,
  len,
  SIndex  : integer;

  SName,
  NewVal,
  ValName : string;

begin
  p := pos('=',IniItem);
  len := Length(IniItem);

  ValName := IniItem;
  NewVal := IniItem;

  Delete(ValName,p,len-p+1);
  Delete(NewVal,1,p);

  with DataDlg do
  begin
    EditingState := etValue;
    ValueName.Text := ValName;
    IData.Text := NewVal;
    RGIdx := 5;
  end;

  if (DataDlg.ShowModal = mrOK) then
  begin
    NewVal := DataDlg.IData.Text;

    {test for empty value which would delete entry from section}
    if (Length(NewVal) = 0) then
      NewVal := ' ';

    GetIniSectionName(SName,SIndex);
    TC.CurSubKey := SName;

    try
      TC.WriteString(ValName,NewVal);
    finally
      FillListBox;
      DataDlg.EditingState := etAll;
    end;
  end else
    DataDlg.EditingState := etAll;
end;


procedure TForm1.ModifyRegItem(RegItem : string; ModifyValue : Boolean);
var
  p,
  len      : Integer;

  Size     : LongInt;

  DType    : DWORD;

  TDbl     : Double;
  BA       : array[1..127] of Byte;

  SKN,
  OldName,
  ValName,
  NewVal   : string;

begin
  p := pos('=',RegItem);
  ValName := RegItem;
  Delete(ValName,p,Length(ValName)-p+1);
  OldName := ValName;

  NewVal := RegItem;
  Delete(NewVal,1,p);
  while pos('"',NewVal) > 0 do
    Delete(NewVal,pos('"',NewVal),1);

  SKN := Edit1.Text;

  Delete(SKN,1,pos('\',SKN));
  TC.CurSubKey := SKN;

  TC.GetDataInfo(0,ValName,Size,DType);

  with DataDlg do
  begin
    if (ModifyValue) then
      EditingState := etValue
    else
      EditingState := etName;
    ValueName.Text := ValName;
    IData.Text := NewVal;
    case DType of
      REG_SZ,
      REG_EXPAND_SZ : RGIdx := 5;

      REG_DWORD     : RGIdx := 3;

      REG_BINARY    : begin
                        case Size of
                          8  : begin
                                 RGIdx := 4;
                                 TDbl := TC.ReadFloat(ValName,0);
                                 Str(TDbl,NewVal);
                                 IData.Text := NewVal;
                               end;
                        else
                          RGIdx := 0;
                        end;
                      end;
    end;
  end;

  if (DataDlg.ShowModal = mrOK) then
  begin
    ValName := DataDlg.ValueName.Text;
    NewVal := DataDlg.IData.Text;
    len := Length(NewVal);
    if NOT (ModifyValue) then
      TC.DeleteValue(OldName);
    try
      case DType of
        REG_SZ,
        REG_EXPAND_SZ : TC.WriteString(ValName,NewVal);

        REG_DWORD     : TC.WriteInteger(ValName,StrToInt(NewVal));

        REG_BINARY    : begin
                          if DataDlg.DataTypeRG.ItemIndex = 1 then
                            TC.WriteBoolean(ValName,StrToInt(NewVal) = 1);
                          if DataDlg.DataTypeRG.ItemIndex = 4 then
                          begin
                            Val(NewVal,TDbl,p);
                            if (p = 0) then
                              TC.WriteFloat(ValName,TDbl);
                          end;
                          if DataDlg.DataTypeRG.ItemIndex = 0 then
                          begin
                            TC.StringToBytes(NewVal,BA,len);
                            TC.WriteBinaryData(NewVal,BA,len div 2);
                          end;
                        end;

      end;
    finally
      DataDlg.EditingState := etAll;
      FillListBox;
    end;
  end else
    DataDlg.EditingState := etAll;
end;


procedure TForm1.RenameValueClick(Sender: TObject);
var
  Idx : Integer;
  VN  : string;

begin
  Idx := ListBox1.ItemIndex;
  if (Idx < 0) then
  begin
    ShowMessage('No value selected');
    Exit;
  end;

  VN := ListBox1.Items[Idx];

  OutLine1.Perform(WM_SETREDRAW,0,0);
  try
    if (TC.IsIniFile) then
      RenameIniItem(VN)
    else
      RenameRegItem(VN);
  finally
    Outline1.Perform(WM_SETREDRAW, 1, 0);
    Outline1.Refresh;
  end;
end;

procedure TForm1.RenameIniItem(IniItem : string);
var
  p, len,
  SIndex   : integer;

  SName,
  NewName,
  OldVal,
  ValName  : string;

begin
  ValName := IniItem;
  p := pos('=',ValName);
  len := Length(ValName);
  Delete(ValName,p,len-p+1);
  NewName := ValName;

  OldVal := IniItem;
  Delete(OldVal,1,p);

  if InputQuery('Change Name Dialog',ValName,NewName) then
  begin
    GetIniSectionName(SName,SIndex);
    TC.CurSubKey := SName;

    TC.DeleteValue(ValName);
    TC.WriteString(NewName,OldVal);

    FillListBox;
  end;
end;


procedure TForm1.RenameRegItem(RegItem : string);
begin
  ModifyRegItem(RegItem,False);
end;


procedure TForm1.ListBox1DrawItem(Control: TWinControl; Index: Integer;
  Rect: TRect; State: TOwnerDrawState);
var
  ValName,
  IData     : string;
  Bmp       : TBitMap;
  OS1,
  OS2,
  len,
  p         : Integer;

begin
  OS1 := 25;
  OS2 := 132;

  with (Control as TListBox).Canvas do
  begin
    FillRect(Rect);
    Bmp := TBitMap(TListBox(Control).Items.Objects[Index]);
    if (Bmp <> nil) then
      Draw(Rect.Left+2,Rect.Top,Bmp);
    ValName := TListBox(Control).Items[Index];
    IData := ValName;
    len := Length(ValName);
    p := pos('=',ValName);

    Delete(ValName,p,len-p+1);
    Delete(IData,1,p);

    len := Length(ValName);
    if (len > 15) then
    begin
      Delete(ValName,16,len-15);
      Insert('...',ValName,15);
      Delete(ValName,18,1);
    end;

    if (TC.IsIniFile) then
    begin
      TextOut(Rect.Left+OS1,Rect.Top+1,ValName);
      TextOut(Rect.Left+OS2,Rect.Top+1,IData);
    end else
    begin
      TextOut(Rect.Left + OS1,Rect.Top+1,ValName);
      TextOut(Rect.Left + OS2,Rect.Top+1,IData);
    end;
  end;
end;

procedure TForm1.FormActivate(Sender: TObject);
begin
  SendMessage(ListBox1.Handle,lb_SetHorizontalExtent,2500,longint(0));
end;

procedure TForm1.Outline1Collapse(Sender: TObject; Index: Longint);
var
  I      : integer;
  S      : string;

begin
  if (TC.IsIniFile) then
  begin
    if Outline1.SelectedItem = 1 then
      ListBox1.Clear;
    Exit;
  end;
  ListBox1.Clear;
  SetBusy(True);

  S := Outline1.Items[Index].FullPath;
  I := System.pos('\',S);
  if (I = 0) then begin
    SetBusy(False);
    Exit;
  end;

  System.Delete(S,1,I);
  TC.CurSubKey := S;

  FillListBox;
  Outline1.Refresh;
  SetBusy(False);
end;

procedure TForm1.Outline1DblClick(Sender: TObject);
var
  S : string;
begin
  if (TC.IsIniFile) then
  begin
    with Outline1 do
    begin
      if SelectedItem > 1 then
      begin
        S := Items[Outline1.SelectedItem].Text;
        TC.CurSubKey := S;
        FillListBox;
      end;
    end;
  end;
end;

end.
