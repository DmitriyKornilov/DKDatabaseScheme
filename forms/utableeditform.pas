unit UTableEditForm;

{$mode ObjFPC}{$H+}

interface

uses
  Classes, SysUtils, Forms, Controls, Graphics, Dialogs, StdCtrls, ExtCtrls,
  Buttons, fpspreadsheetgrid,
  //DK packages utils
  DK_Vector, DK_StrUtils, DK_CtrlUtils, DK_MsgDialogs,
  //Project utils
  UTypes, UScheme;

type

  { TTableEditForm }

  TTableEditForm = class(TForm)
    Label1: TLabel;
    Label2: TLabel;
    Label3: TLabel;
    Panel2: TPanel;
    SaveButton: TSpeedButton;
    CancelButton: TSpeedButton;
    TableDescriptionEdit: TEdit;
    TableNameEdit: TEdit;
    TableNotesMemo: TMemo;
    procedure CancelButtonClick(Sender: TObject);
    procedure FormShow(Sender: TObject);
    procedure SaveButtonClick(Sender: TObject);
  private
    EditMode: Byte;
    BaseScheme: TBaseScheme;
  public
    procedure BaseSchemeSet(const ABaseScheme: TBaseScheme; const AEditMode: Byte);
  end;

var
  TableEditForm: TTableEditForm;

implementation

{$R *.lfm}

{ TTableEditForm }

procedure TTableEditForm.FormShow(Sender: TObject);
begin
  SetControlSize([SaveButton, CancelButton], 140, 40);

  if EditMode=2 {edit} then
  begin
    TableNameEdit.Text:= BaseScheme.ActiveTable.TableName;
    TableDescriptionEdit.Text:= BaseScheme.ActiveTable.Description;
    VToStrings(BaseScheme.ActiveTable.Notes, TableNotesMemo.Lines);
  end;
  TableNameEdit.SetFocus;
end;

procedure TTableEditForm.BaseSchemeSet(const ABaseScheme: TBaseScheme;
  const AEditMode: Byte);
begin
  BaseScheme:= ABaseScheme;
  EditMode:= AEditMode;
end;

procedure TTableEditForm.CancelButtonClick(Sender: TObject);
begin
  ModalResult:= mrCancel;
end;

procedure TTableEditForm.SaveButtonClick(Sender: TObject);
var
  S: String;
  TmpTable: TTable;
begin
  S:= STrim(TableNameEdit.Text);
  if S=EmptyStr then
  begin
    MsgInform('Не указано наименование таблицы!');
    Exit;
  end;

  if EditMode=1 {new} then
    TmpTable:= TableCreate(EmptyStr)
  else if EditMode=2 {edit} then
    TmpTable:= TableCopy(BaseScheme.ActiveTable);

  TmpTable.TableName:= S;
  TmpTable.Description:= STrim(TableDescriptionEdit.Text);
  TmpTable.Notes:= VTrim(VFromStrings(TableNotesMemo.Lines));

  try
    if EditMode=1 {new} then
      BaseScheme.TableAdd(TmpTable)
    else if EditMode=2 {edit} then
      BaseScheme.TableSet(TmpTable);
    ModalResult:= mrOK;
  except
    on E: EDuplicateException do MsgInform(E.Message);
  end;
end;

end.

