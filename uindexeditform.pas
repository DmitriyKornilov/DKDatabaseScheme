unit UIndexEditForm;

{$mode ObjFPC}{$H+}

interface

uses
  Classes, SysUtils, Forms, Controls, Graphics, Dialogs, StdCtrls, CheckLst,
  ExtCtrls, Buttons, rxctrls, USchemeTypes, DK_Dialogs, DK_Vector, DK_StrUtils;

type

  { TIndexEditForm }

  TIndexEditForm = class(TForm)
    CancelButton: TSpeedButton;
    FieldCheckListBox: TCheckListBox;
    FieldDownButton: TRxSpeedButton;
    FieldUpButton: TRxSpeedButton;
    Label5: TLabel;
    Label6: TLabel;
    WhereMemo: TMemo;
    Panel2: TPanel;
    SaveButton: TSpeedButton;
    UniqueCheckBox: TCheckBox;
    IndexNameEdit: TEdit;
    Label4: TLabel;
    procedure CancelButtonClick(Sender: TObject);
    procedure FieldCheckListBoxClickCheck(Sender: TObject);
    procedure FieldCheckListBoxSelectionChange(Sender: TObject; User: boolean);
    procedure FieldDownButtonClick(Sender: TObject);
    procedure FieldUpButtonClick(Sender: TObject);
    procedure FormCloseQuery(Sender: TObject; var CanClose: Boolean);
    procedure FormShow(Sender: TObject);
    procedure SaveButtonClick(Sender: TObject);
  private
    CanFormClose: Boolean;
    EditMode: Byte;
    BaseScheme: TBaseScheme;
    FieldNames: TStrVector;
    FieldFlags: TBoolVector;
    SelectedIndex: Integer;

    procedure EditSave;
    procedure EditCancel;

    procedure FieldListSet;

    procedure FieldMove(ADirection: Integer);
  public
    procedure BaseSchemeSet(const ABaseScheme: TBaseScheme; const AEditMode: Byte);
  end;

var
  IndexEditForm: TIndexEditForm;

implementation

{$R *.lfm}

{ TIndexEditForm }

procedure TIndexEditForm.CancelButtonClick(Sender: TObject);
begin
  EditCancel;
end;

procedure TIndexEditForm.FieldCheckListBoxClickCheck(Sender: TObject);
var
  i: Integer;
begin
  for i:=0 to FieldCheckListBox.Items.Count-1 do
    FieldFlags[i]:= FieldCheckListBox.Checked[i];
end;

procedure TIndexEditForm.FieldCheckListBoxSelectionChange(Sender: TObject;
  User: boolean);
var
  i: Integer;
begin
  FieldUpButton.Enabled:= (FieldCheckListBox.SelCount=1) and
                          (not FieldCheckListBox.Selected[0]);
  FieldDownButton.Enabled:= (FieldCheckListBox.SelCount=1) and
     (not FieldCheckListBox.Selected[FieldCheckListBox.Items.Count-1]);

  SelectedIndex:= -1;
  for i:=0 to FieldCheckListBox.Items.Count-1 do
  begin
    if FieldCheckListBox.Selected[i] then
    begin
      SelectedIndex:= i;
      break;
    end;
  end;
end;

procedure TIndexEditForm.FieldDownButtonClick(Sender: TObject);
begin
  FieldMove(1);
end;

procedure TIndexEditForm.FieldUpButtonClick(Sender: TObject);
begin
  FieldMove(-1);
end;

procedure TIndexEditForm.FormCloseQuery(Sender: TObject; var CanClose: Boolean);
begin
  CanClose:= CanFormClose;
end;

procedure TIndexEditForm.FormShow(Sender: TObject);
begin
  CanFormClose:= True;
  SelectedIndex:= -1;
  FieldListSet;

end;

procedure TIndexEditForm.SaveButtonClick(Sender: TObject);
begin
  EditSave;
end;

procedure TIndexEditForm.EditSave;
var
  TmpIndex: TIndex;
  S: String;
  i: Integer;
begin
  CanFormClose:= False;

  S:= STrim(IndexNameEdit.Text);
  if S=EmptyStr then
  begin
    ShowInfo('Не указано наименование индекса!');
    Exit;
  end;

  if not VIsTrue(FieldFlags) then
  begin
    ShowInfo('Не указано ни одного поля для индекса!');
    Exit;
  end;

  TmpIndex:= IndexCreate(S);
  TmpIndex.Unique:= UniqueCheckBox.Checked;
  for i:=0 to High(FieldNames) do
    if FieldFlags[i] then
      VAppend(TmpIndex.FieldNames, FieldNames[i]);
  TmpIndex.Where:= STrim(WhereMemo.Text);

  try
    if EditMode=1 {new} then
      BaseScheme.IndexAdd(TmpIndex)
    else if EditMode=2 {edit} then
      BaseScheme.IndexSet(TmpIndex);
    ModalResult:= mrOK;
    CanFormClose:= True;
  except
    on E: EDuplicateException do ShowInfo(E.Message);
  end;
end;

procedure TIndexEditForm.EditCancel;
begin
  CanFormClose:= True;
  ModalResult:= mrCancel;
end;

procedure TIndexEditForm.FieldListSet;
var
  i: Integer;
begin
  FieldCheckListBox.Items.Clear;
  VToStrings(FieldNames, FieldCheckListBox.Items);
  for i:= 0 to High(FieldNames) do
    FieldCheckListBox.Checked[i]:= FieldFlags[i];
  if SelectedIndex>=0 then
    FieldCheckListBox.Selected[SelectedIndex]:= True;
end;

procedure TIndexEditForm.FieldMove(ADirection: Integer);
var
  OtherIndex: Integer;
begin
  OtherIndex:= SelectedIndex + ADirection;
  VSwap(FieldNames, SelectedIndex, OtherIndex);
  VSwap(FieldFlags, SelectedIndex, OtherIndex);
  SelectedIndex:= OtherIndex;
  FieldListSet;
end;

procedure TIndexEditForm.BaseSchemeSet(const ABaseScheme: TBaseScheme;
  const AEditMode: Byte);
var
  i, n: Integer;
begin
  BaseScheme:= ABaseScheme;
  EditMode:= AEditMode;

  FieldNames:= nil;
  FieldFlags:= nil;
  n:= Length(BaseScheme.ActiveTable.Fields);
  VDim(FieldFlags, n, False);

  if EditMode=1 {new} then
  begin
    for i:=0 to n-1 do
      VAppend(FieldNames, BaseScheme.ActiveTable.Fields[i].FieldName);
  end
  else if EditMode=2 {edit} then
  begin
    FieldNames:= VCut(BaseScheme.ActiveIndex.FieldNames);
    VChangeIn(FieldFlags, True, 0, High(FieldNames));
    for i:=0 to n-1 do
      if VIndexOf(FieldNames, BaseScheme.ActiveTable.Fields[i].FieldName)<0 then
        VAppend(FieldNames, BaseScheme.ActiveTable.Fields[i].FieldName);
    IndexNameEdit.Text:= BaseScheme.ActiveIndex.IndexName;
    UniqueCheckBox.Checked:= BaseScheme.ActiveIndex.Unique;
    WhereMemo.Text:= BaseScheme.ActiveIndex.Where;
  end;

end;

end.

