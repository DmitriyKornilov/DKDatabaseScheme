unit UTableEditForm;

{$mode ObjFPC}{$H+}

interface

uses
  Classes, SysUtils, Forms, Controls, Graphics, Dialogs, StdCtrls, ExtCtrls,
  Buttons, fpspreadsheetgrid, USchemeTypes, DK_Vector, DK_StrUtils, DK_Dialogs;

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
    procedure FormCloseQuery(Sender: TObject; var CanClose: Boolean);
    procedure FormShow(Sender: TObject);
    procedure SaveButtonClick(Sender: TObject);

  private
    CanFormClose: Boolean;
    EditMode: Byte;
    BaseScheme: TBaseScheme;
    procedure EditSave;
    procedure EditCancel;
  public
    procedure BaseSchemeSet(const ABaseScheme: TBaseScheme; const AEditMode: Byte);
  end;

var
  TableEditForm: TTableEditForm;

implementation

{$R *.lfm}

{ TTableEditForm }



procedure TTableEditForm.EditSave;
var
  S: String;
  TmpTable: TTable;
begin
  CanFormClose:= False;

  S:= STrim(TableNameEdit.Text);
  if S=EmptyStr then
  begin
    ShowInfo('Не указано наименование таблицы!');
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
    CanFormClose:= True;
  except
    on E: EDuplicateException do ShowInfo(E.Message);
  end;

end;

procedure TTableEditForm.EditCancel;
begin
  CanFormClose:= True;
  ModalResult:= mrCancel;
end;

procedure TTableEditForm.BaseSchemeSet(const ABaseScheme: TBaseScheme;
  const AEditMode: Byte);
begin
  BaseScheme:= ABaseScheme;
  EditMode:= AEditMode;
end;

procedure TTableEditForm.FormCloseQuery(Sender: TObject; var CanClose: Boolean);
begin
  CanClose:= CanFormClose;
end;

procedure TTableEditForm.CancelButtonClick(Sender: TObject);
begin
  EditCancel;
end;

procedure TTableEditForm.FormShow(Sender: TObject);
begin
  CanFormClose:= True;
  if EditMode=2 {edit} then
  begin
    TableNameEdit.Text:= BaseScheme.ActiveTable.TableName;
    TableDescriptionEdit.Text:= BaseScheme.ActiveTable.Description;
    VToStrings(BaseScheme.ActiveTable.Notes, TableNotesMemo.Lines);
  end;
  TableNameEdit.SetFocus;
end;

procedure TTableEditForm.SaveButtonClick(Sender: TObject);
begin
  EditSave;
end;

end.

