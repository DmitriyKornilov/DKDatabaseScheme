unit UValuesEditForm;

{$mode ObjFPC}{$H+}

interface

uses
  Classes, SysUtils, Forms, Controls, Graphics, Dialogs, ExtCtrls, Buttons,
  VirtualTrees, DateUtils,
  //DK packages utils
  DK_Vector, DK_StrUtils, DK_VSTEdit, DK_VSTTypes, DK_CtrlUtils, DK_MsgDialogs,
  //Project utils
  UTypes, UScheme;

type

  { TValuesEditForm }

  TValuesEditForm = class(TForm)
    AddButton: TSpeedButton;
    CancelButton: TSpeedButton;
    DelButton: TSpeedButton;
    Panel1: TPanel;
    Panel2: TPanel;
    SaveButton: TSpeedButton;
    VT1: TVirtualStringTree;
    procedure AddButtonClick(Sender: TObject);
    procedure CancelButtonClick(Sender: TObject);
    procedure DelButtonClick(Sender: TObject);
    procedure FormCreate(Sender: TObject);
    procedure FormDestroy(Sender: TObject);
    procedure FormShow(Sender: TObject);
    procedure SaveButtonClick(Sender: TObject);
  private
    VSTValues: TVSTEdit;

    procedure ValuesCreate;
    procedure ValuesLoad;
    procedure ValuesSelect;

    procedure SetRowNumbers;
  public
    BaseScheme: TBaseScheme;
  end;

var
  ValuesEditForm: TValuesEditForm;

implementation

{$R *.lfm}

{ TValuesEditForm }

procedure TValuesEditForm.FormCreate(Sender: TObject);
begin
  ValuesCreate;
end;

procedure TValuesEditForm.FormDestroy(Sender: TObject);
begin
  FreeAndNil(VSTValues);
end;

procedure TValuesEditForm.FormShow(Sender: TObject);
begin
  SetControlSize([SaveButton, CancelButton], 140, 40);
  SetControlSize([AddButton, DelButton], 36, 36);

  ValuesLoad;
end;

procedure TValuesEditForm.CancelButtonClick(Sender: TObject);
begin
  ModalResult:= mrCancel;
end;

procedure TValuesEditForm.SaveButtonClick(Sender: TObject);
var
  i, j: Integer;
  Tbl: TTable;
  ColType: TVSTColumnType;
  V: TStrVector;
begin
  Tbl:= BaseScheme.ActiveTable;

  VSTValues.EndEdit(True{save});
  for i:= 0 to High(Tbl.Fields) do
  begin
    ColType:= FieldTypeToColumnType(Tbl.Fields[i].FieldType);
    if ColType=ctUndefined then continue; //skip blob
    VSTValues.ColumnAsString(V, Tbl.Fields[i].FieldName);
    if ColType<>ctString then
      for j:= 0 to High(V) do
        if SEmpty(V[j]) then
        begin
          VSTValues.Select(j, Tbl.Fields[i].FieldName);
          MsgInform('Не указано значение №' + IntToStr(j+1) +
                 ' для поля "' + Tbl.Fields[i].FieldName + '"!');
          Exit;
        end;
    Tbl.Fields[i].ExistingValues:= V
  end;

  ModalResult:= mrOK;
end;

procedure TValuesEditForm.DelButtonClick(Sender: TObject);
begin
  VSTValues.EndEdit(False{no save});
  VSTValues.RowDelete(VSTValues.SelectedRowIndex);
  SetRowNumbers;
end;

procedure TValuesEditForm.AddButtonClick(Sender: TObject);
var
  i: Integer;
  Tbl: TTable;
  V: TStrVector;
begin
  Tbl:= BaseScheme.ActiveTable;
  VDim(V{%H-}, Length(Tbl.Fields) + 1, EmptyStr);
  //row order number
  V[0]:= IntToStr(VSTValues.RowCount + 1);
  //default values
  for i:= 0 to High(Tbl.Fields) do
    if not SEmpty(Tbl.Fields[i].DefaultValue) then
      V[i+1{skip order col}]:= Tbl.Fields[i].DefaultValue;
  VSTValues.RowAppend(V);
end;

procedure TValuesEditForm.SetRowNumbers;
begin
  VSTValues.SetColumnRowTitles(VOrderStr(VSTValues.RowCount));
end;

procedure TValuesEditForm.ValuesCreate;
begin
  VSTValues:= TVSTEdit.Create(VT1);
  VSTValues.AutosizeColumnDisable;
  VSTValues.IsShowZeros:= True;
  VSTValues.OnSelect:= @ValuesSelect;
  VSTValues.HeaderBGColor:= clBtnFace;
  VSTValues.ColumnRowTitlesBGColor:= clBtnFace;
end;

procedure TValuesEditForm.ValuesLoad;
var
  i: Integer;
  Tbl: TTable;
  ColType: TVSTColumnType;
begin
  Tbl:= BaseScheme.ActiveTable;

  VSTValues.AddColumnRowTitles('№ п/п', 50);
  for i:= 0 to High(Tbl.Fields) do
  begin
    ColType:= FieldTypeToColumnType(Tbl.Fields[i].FieldType);
    if ColType=ctUndefined then continue; //skip blob
    if ColType=ctInteger then
    begin
      VSTValues.AddColumnInteger(Tbl.Fields[i].FieldName, 200);
      VSTValues.SetColumnInteger(Tbl.Fields[i].FieldName, VStrToInt(Tbl.Fields[i].ExistingValues));
    end
    else if ColType=ctDateTime then
    begin
      VSTValues.AddColumnDateTime(Tbl.Fields[i].FieldName, 'dd.mm.yyyy hh:mm:ss', 200);
      VSTValues.SetColumnDateTime(Tbl.Fields[i].FieldName, VStrToDateTime(Tbl.Fields[i].ExistingValues));
    end
    else if ColType=ctString then
    begin
      VSTValues.AddColumnString(Tbl.Fields[i].FieldName, 200);
      VSTValues.SetColumnString(Tbl.Fields[i].FieldName, Tbl.Fields[i].ExistingValues);
    end
    else if ColType=ctDouble then
    begin
      VSTValues.AddColumnDouble(Tbl.Fields[i].FieldName, 200);
      VSTValues.SetColumnDouble(Tbl.Fields[i].FieldName, VStrToFloat(Tbl.Fields[i].ExistingValues));
    end;
  end;
  SetRowNumbers;
  VSTValues.Draw;
end;

procedure TValuesEditForm.ValuesSelect;
begin
  DelButton.Enabled:= Assigned(VSTValues) and VSTValues.IsSelected;
end;

end.

