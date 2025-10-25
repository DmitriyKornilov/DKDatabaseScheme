unit UFieldEditForm;

{$mode ObjFPC}{$H+}

interface

uses
  Classes, SysUtils, Forms, Controls, Graphics, Dialogs, ExtCtrls, StdCtrls,
  Buttons, DateUtils,
  //DK packages utils
  DK_Vector, DK_StrUtils, DK_CtrlUtils, DK_MsgDialogs,
  //Project utils
  UTypes, UScheme;

type

  { TFieldEditForm }

  TFieldEditForm = class(TForm)
    CancelButton: TSpeedButton;
    EmptyStrCheckBox: TCheckBox;
    DefaultValueCheckBox: TCheckBox;
    FieldNameEdit: TEdit;
    DefaultValueEdit: TEdit;
    FKCheckBox: TCheckBox;
    Label1: TLabel;
    Label2: TLabel;
    Label3: TLabel;
    Label4: TLabel;
    Label5: TLabel;
    Label6: TLabel;
    FieldDescriptionMemo: TMemo;
    NotNullCheckBox: TCheckBox;
    RefOnUpdateComboBox: TComboBox;
    Panel1: TPanel;
    Panel2: TPanel;
    Panel3: TPanel;
    PKCheckBox: TCheckBox;
    RadioButton1: TRadioButton;
    RadioButton2: TRadioButton;
    RadioButton3: TRadioButton;
    RadioButton4: TRadioButton;
    RadioButton5: TRadioButton;
    RefFieldComboBox: TComboBox;
    RefTableComboBox: TComboBox;
    RefOnDeleteComboBox: TComboBox;
    SaveButton: TSpeedButton;
    StatusCheckBox: TCheckBox;
    procedure DefaultValueCheckBoxClick(Sender: TObject);
    procedure EmptyStrCheckBoxChange(Sender: TObject);
    procedure FKCheckBoxChange(Sender: TObject);
    procedure FormShow(Sender: TObject);
    procedure PKCheckBoxClick(Sender: TObject);
    procedure RadioButton1Click(Sender: TObject);
    procedure RadioButton2Click(Sender: TObject);
    procedure RadioButton3Click(Sender: TObject);
    procedure RadioButton4Click(Sender: TObject);
    procedure RadioButton5Click(Sender: TObject);
    procedure RefTableComboBoxChange(Sender: TObject);
    procedure CancelButtonClick(Sender: TObject);
    procedure SaveButtonClick(Sender: TObject);
  private
    EditMode: Byte;
    BaseScheme: TBaseScheme;

    procedure SetStatusCheckBoxCaption;
    procedure TableListSet;
    procedure FieldListSet;
    procedure ActionListSet;
    procedure SetRefComboboxEnabled;

    procedure SetDefaultValueControls;
    procedure SetDefaultEmptyStr;
  public
    procedure BaseSchemeSet(const ABaseScheme: TBaseScheme; const AEditMode: Byte);
  end;

var
  FieldEditForm: TFieldEditForm;

implementation

{$R *.lfm}

{ TFieldEditForm }

procedure TFieldEditForm.FormShow(Sender: TObject);
var
  Ref: TReferenceTo;
begin
  SetControlSize([SaveButton, CancelButton], 140, 40);

  TableListSet;
  ActionListSet;

  if (EditMode=2 {edit}) or (EditMode=3 {new from old}) then
  begin
    FieldNameEdit.Text:= BaseScheme.ActiveField.FieldName;
    PKCheckBox.Checked:= BaseScheme.ActiveField.PrimaryKey;
    SetStatusCheckBoxCaption;
    StatusCheckBox.Checked:= BaseScheme.ActiveField.Status;
    NotNullCheckBox.Checked:= BaseScheme.ActiveField.NotNull;

    case BaseScheme.ActiveField.FieldType of
      'INTEGER' : RadioButton1.Checked:= True;
      'DATETIME': RadioButton2.Checked:= True;
      'TEXT'    : RadioButton3.Checked:= True;
      'REAL'    : RadioButton4.Checked:= True;
      'BLOB'    : RadioButton5.Checked:= True;
    end;

    DefaultValueCheckBox.Checked:= BaseScheme.ActiveField.DefaultValue<>EmptyStr;
    SetDefaultValueControls;
    DefaultValueEdit.Text:= BaseScheme.ActiveField.DefaultValue;

    VToStrings(BaseScheme.ActiveField.Description, FieldDescriptionMemo.Lines);
    //VToStrings(BaseScheme.ActiveField.ExistingValues, FieldExistingValuesMemo.Lines);

    Ref:= BaseScheme.ActiveField.ReferenceTo;
    if Ref.TableName<>EmptyStr then
    begin
      FKCheckBox.Checked:= True;
      RefTableComboBox.ItemIndex:= RefTableComboBox.Items.IndexOf(Ref.TableName);
      FieldListSet;
      RefFieldComboBox.ItemIndex:= RefFieldComboBox.Items.IndexOf(Ref.FieldName);
      RefOnDeleteComboBox.ItemIndex:= Ord(Ref.OnDelete);
      RefOnUpdateComboBox.ItemIndex:= Ord(Ref.OnUpdate);
    end;
  end;
end;

procedure TFieldEditForm.PKCheckBoxClick(Sender: TObject);
begin
  SetStatusCheckBoxCaption;
end;

procedure TFieldEditForm.RadioButton1Click(Sender: TObject);
begin
  SetDefaultValueControls;
end;

procedure TFieldEditForm.RadioButton2Click(Sender: TObject);
begin
  SetDefaultValueControls;
end;

procedure TFieldEditForm.RadioButton3Click(Sender: TObject);
begin
  SetDefaultValueControls;
end;

procedure TFieldEditForm.RadioButton4Click(Sender: TObject);
begin
  SetDefaultValueControls;
end;

procedure TFieldEditForm.RadioButton5Click(Sender: TObject);
begin
  SetDefaultValueControls;
end;

procedure TFieldEditForm.RefTableComboBoxChange(Sender: TObject);
begin
  FieldListSet;
end;

procedure TFieldEditForm.CancelButtonClick(Sender: TObject);
begin
  ModalResult:= mrCancel;
end;

procedure TFieldEditForm.SaveButtonClick(Sender: TObject);
var
  S: String;
  N, i: Integer;
  TmpField: TField;
begin
  S:= STrim(FieldNameEdit.Text);
  if S=EmptyStr then
  begin
    MsgInform('Не указано наименование поля!');
    Exit;
  end;

  if FKCheckBox.Checked then
  begin
    if RefTableComboBox.Text=EmptyStr then
    begin
      MsgInform('Не указана таблица для внешнего ключа!');
      Exit;
    end;
    if RefFieldComboBox.Text=EmptyStr then
    begin
      MsgInform('Не указано поле для внешнего ключа!');
      Exit;
    end;
  end;

  if EditMode=1 {new} then
  begin
    TmpField:= FieldCreate(EmptyStr, EmptyStr);
    if Length(BaseScheme.ActiveTable.Fields)>0 then
    begin
      TmpField.ExistingValues:= nil;
      N:= Length(BaseScheme.ActiveTable.Fields[0].ExistingValues);
      if N>0 then
        for i:= 0 to N-1 do
          VAppend(TmpField.ExistingValues, EmptyStr);
    end;
  end
  else if (EditMode=2 {edit}) or (EditMode=3 {new from old}) then
  begin
    TmpField:= FieldCopy(BaseScheme.ActiveField);
  end;

  TmpField.FieldName:= S;
  TmpField.Description:= VFromStrings(FieldDescriptionMemo.Lines);

  TmpField.PrimaryKey:= PKCheckBox.Checked;
  TmpField.Status:= StatusCheckBox.Checked;
  TmpField.NotNull:= NotNullCheckBox.Checked;

  if RadioButton1.Checked then
    TmpField.FieldType:= 'INTEGER'
  else if RadioButton2.Checked then
    TmpField.FieldType:= 'DATETIME'
  else if RadioButton3.Checked then
    TmpField.FieldType:= 'TEXT'
  else if RadioButton4.Checked then
    TmpField.FieldType:= 'REAL'
  else if RadioButton5.Checked then
    TmpField.FieldType:= 'BLOB';

  TmpField.DefaultValue:= EmptyStr;
  if DefaultValueCheckBox.Checked then
  begin
    S:= STrim(DefaultValueEdit.Text);
    if FieldValueCheck(S, TmpField.FieldType) then
      TmpField.DefaultValue:= S
    else begin
      MsgInform('Некорректное значение по умолчанию!');
      Exit;
    end;
  end;

  if FKCheckBox.Checked then
  begin
    TmpField.ReferenceTo.TableName:= RefTableComboBox.Text;
    TmpField.ReferenceTo.FieldName:= RefFieldComboBox.Text;
    TmpField.ReferenceTo.OnDelete:= TForeignKeyAction(RefOnDeleteComboBox.ItemIndex);
    TmpField.ReferenceTo.OnUpdate:= TForeignKeyAction(RefOnUpdateComboBox.ItemIndex);
  end
  else
    ReferenceToClear(TmpField.ReferenceTo);

  try
    if (EditMode=1 {new}) or ((EditMode=3 {new from old})) then
      BaseScheme.FieldAdd(TmpField)
    else if EditMode=2 {edit} then
      BaseScheme.FieldSet(TmpField);
    ModalResult:= mrOK;
  except
    on E: EDuplicateException do MsgInform(E.Message);
  end;
end;

procedure TFieldEditForm.SetStatusCheckBoxCaption;
begin
  if PKCheckBox.Checked then
    StatusCheckBox.Caption:= 'Автоинкремент'
  else
    StatusCheckBox.Caption:= 'Уникальное значение';
end;

procedure TFieldEditForm.TableListSet;
var
  i: Integer;
begin
  RefTableComboBox.Items.Clear;
  for i:= 0 to High(BaseScheme.TableNames) do
    if not SSame(BaseScheme.ActiveTable.TableName, BaseScheme.TableNames[i], False) then
      RefTableComboBox.Items.Add(BaseScheme.TableNames[i]);
end;

procedure TFieldEditForm.FieldListSet;
var
  V: TStrVector;
begin
  V:= BaseScheme.FieldsList(RefTableComboBox.Text);
  VToStrings(V, RefFieldComboBox.Items);
end;

procedure TFieldEditForm.ActionListSet;
var
  V: TStrVector;
begin
  V:= ForeignKeyActionStrList;
  if VIsNil(V) then Exit;
  VToStrings(V, RefOnDeleteComboBox.Items);
  RefOnDeleteComboBox.ItemIndex:= 0;
  VtoStrings(V, RefOnUpdateComboBox.Items);
  RefOnUpdateComboBox.ItemIndex:= 0;
end;

procedure TFieldEditForm.SetRefComboboxEnabled;
begin
  RefTableComboBox.Enabled:= FKCheckBox.Checked;
  RefFieldComboBox.Enabled:= RefTableComboBox.Enabled;
  RefOnDeleteComboBox.Enabled:= RefTableComboBox.Enabled;
  RefOnUpdateComboBox.Enabled:= RefTableComboBox.Enabled;
end;

procedure TFieldEditForm.SetDefaultEmptyStr;
begin
  EmptyStrCheckBox.Visible:= RadioButton3.Checked{TEXT} and
                             DefaultValueCheckBox.Checked;
end;

procedure TFieldEditForm.SetDefaultValueControls;
begin
  EmptyStrCheckBox.Checked:= False;
  DefaultValueCheckBox.Visible:= not RadioButton5.Checked{BLOB};
  DefaultValueEdit.Visible:= (not RadioButton5.Checked{BLOB});
  SetDefaultEmptyStr;

  Label3.Visible:= DefaultValueCheckBox.Checked and
                   RadioButton2.Checked{DATETIME} ;
end;

procedure TFieldEditForm.BaseSchemeSet(const ABaseScheme: TBaseScheme;
  const AEditMode: Byte);
begin
  BaseScheme:= ABaseScheme;
  EditMode:= AEditMode;
end;

procedure TFieldEditForm.DefaultValueCheckBoxClick(Sender: TObject);
begin
  EmptyStrCheckBox.Checked:= False;
  DefaultValueEdit.Enabled:= DefaultValueCheckBox.Checked;
  if DefaultValueCheckBox.Checked and DefaultValueEdit.Visible  then
    DefaultValueEdit.SetFocus;
  Label3.Visible:= DefaultValueCheckBox.Checked and RadioButton2.Checked;
  SetDefaultEmptyStr;
end;

procedure TFieldEditForm.EmptyStrCheckBoxChange(Sender: TObject);
begin
  if EmptyStrCheckBox.Checked then
  begin
    DefaultValueEdit.Text:= 'EmptyStr';
    DefaultValueEdit.Enabled:= False;
  end
  else begin
    DefaultValueEdit.Text:= EmptyStr;
    DefaultValueEdit.Enabled:= True;
  end;
end;

procedure TFieldEditForm.FKCheckBoxChange(Sender: TObject);
begin
  SetRefComboboxEnabled;
end;

end.

