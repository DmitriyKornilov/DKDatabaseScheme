unit UValuesEditForm;

{$mode ObjFPC}{$H+}

interface

uses
  Classes, SysUtils, Forms, Controls, Graphics, Dialogs, ExtCtrls, Buttons,
  Grids, StdCtrls, rxctrls, DateUtils,
  //DK packages utils
  DK_Vector, DK_Dialogs, DK_StrUtils,
  //Project utils
  UTypes, UScheme;

type

  { TValuesEditForm }

  TValuesEditForm = class(TForm)
    AddButton: TRxSpeedButton;
    DelButton: TRxSpeedButton;
    CancelButton: TSpeedButton;
    Label1: TLabel;
    Panel1: TPanel;
    Panel2: TPanel;
    SaveButton: TSpeedButton;
    ValuesGrid: TStringGrid;
    procedure AddButtonClick(Sender: TObject);
    procedure CancelButtonClick(Sender: TObject);
    procedure DelButtonClick(Sender: TObject);
    procedure FormShow(Sender: TObject);
    procedure SaveButtonClick(Sender: TObject);
    procedure ValuesGridBeforeSelection(Sender: TObject; aCol, aRow: Integer);
  private
    BaseScheme: TBaseScheme;
    procedure SetDelButtonEnabled;
    procedure SetRowNumbers;
  public
    procedure BaseSchemeSet(const ABaseScheme: TBaseScheme);
  end;

var
  ValuesEditForm: TValuesEditForm;

implementation

{$R *.lfm}

{ TValuesEditForm }

procedure TValuesEditForm.FormShow(Sender: TObject);
var
  i: Integer;
  Tbl: TTable;
  Cln: TGridColumn;

  procedure AddColumn(const ACaption: String;
                      const AValues: TStrVector;
                      const AWidth: Integer = 0);
  var
    k: Integer;
  begin
    Cln:= ValuesGrid.Columns.Add;
    Cln.Alignment:= taCenter;
    Cln.Title.Alignment:= taCenter;
    Cln.Title.Caption:= ACaption;
    if AWidth>0 then Cln.Width:= AWidth;
    for k:= 0 to High(AValues) do
      ValuesGrid.Cells[Cln.Index, k+1]:= AValues[k];
  end;

begin
  Tbl:= BaseScheme.ActiveTable;
  ValuesGrid.RowCount:= Length(Tbl.Fields[0].ExistingValues) + 1;
  ValuesGrid.FixedRows:= 1;

  AddColumn('№', nil, 50);
  SetRowNumbers;
  for i:= 0 to High(Tbl.Fields) do
    AddColumn(Tbl.Fields[i].FieldName, Tbl.Fields[i].ExistingValues);

  SetDelButtonEnabled;
end;

procedure TValuesEditForm.CancelButtonClick(Sender: TObject);
begin
  ModalResult:= mrCancel;
end;

procedure TValuesEditForm.SaveButtonClick(Sender: TObject);
var
  i, j: Integer;
  Tbl: TTable;

  function CheckValues: Boolean;
  var
    R,C: Integer;
    RowNum, FldName, FldType, Value: String;
  begin
    Result:= False;
    for C:= 1 to ValuesGrid.ColCount-1 do
    begin
      FldName:= Tbl.Fields[C-1].FieldName;
      FldType:= Tbl.Fields[C-1].FieldType;
      for R:= 1 to ValuesGrid.RowCount-1 do
      begin
        RowNum:= IntToStr(R);
        if FldType='BLOB' then
          Value:= EmptyStr
        else if Tbl.Fields[C-1].NotNull and
                (FldType<>'TEXT')  then
        begin
          Value:= ValuesGrid.Cells[C,R];
          if (Value=EmptyStr) and SEmpty(Tbl.Fields[C-1].DefaultValue) then
          begin
            Inform('Не указано значение №' + RowNum + ' для поля "' + FldName + '"!');
            Exit;
          end;
          if SEmpty(Tbl.Fields[C-1].DefaultValue) and (not FieldValueCheck(Value, FldType)) then
          begin
            Inform('Некорректное значение №' +
                      RowNum + ' для поля "' + FldName  + '" (' + FldType + ')!');
            Exit;
          end;
          ValuesGrid.Cells[C,R]:= Value;
        end;
      end;
    end;
    Result:= True;
  end;

begin
  Tbl:= BaseScheme.ActiveTable;

  if not CheckValues then Exit;

  for i:= 1 to ValuesGrid.ColCount-1 do
  begin
    Tbl.Fields[i-1].ExistingValues:= nil;
    for j:= 1 to ValuesGrid.RowCount-1 do
      VAppend(Tbl.Fields[i-1].ExistingValues, ValuesGrid.Cells[i,j]);
  end;

  ModalResult:= mrOK;
end;

procedure TValuesEditForm.ValuesGridBeforeSelection(Sender: TObject; aCol, aRow: Integer);
begin
  if ACol=0 then
    ValuesGrid.Options:= ValuesGrid.Options - [goEditing]
  else
    ValuesGrid.Options:= ValuesGrid.Options + [goEditing];
end;

procedure TValuesEditForm.DelButtonClick(Sender: TObject);
begin
  ValuesGrid.DeleteRow(ValuesGrid.Row);
  SetRowNumbers;
  SetDelButtonEnabled;
end;

procedure TValuesEditForm.AddButtonClick(Sender: TObject);
begin
  ValuesGrid.RowCount:= ValuesGrid.RowCount+1;
  SetRowNumbers;
  SetDelButtonEnabled;
end;

procedure TValuesEditForm.SetDelButtonEnabled;
begin
  DelButton.Enabled:= ValuesGrid.RowCount>1;
end;

procedure TValuesEditForm.SetRowNumbers;
var
  i: Integer;
begin
  for i:= 1 to ValuesGrid.RowCount-1 do
    ValuesGrid.Cells[0, i]:= IntToStr(i);
end;

procedure TValuesEditForm.BaseSchemeSet(const ABaseScheme: TBaseScheme);
begin
  BaseScheme:= ABaseScheme;
end;

end.

