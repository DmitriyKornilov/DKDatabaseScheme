unit UValuesEditForm;

{$mode ObjFPC}{$H+}

interface

uses
  Classes, SysUtils, Forms, Controls, Graphics, Dialogs, ExtCtrls, Buttons,
  Grids, StdCtrls, VirtualTrees, DateUtils,
  //DK packages utils
  DK_Vector, DK_Dialogs, DK_StrUtils, DK_VSTEdit, DK_VSTTypes,
  //Project utils
  UTypes, UScheme;

type

  { TValuesEditForm }

  TValuesEditForm = class(TForm)
    AddButton: TSpeedButton;
    CancelButton: TSpeedButton;
    DelButton: TSpeedButton;
    Label1: TLabel;
    Panel1: TPanel;
    Panel2: TPanel;
    SaveButton: TSpeedButton;
    ValuesGrid: TStringGrid;
    VT1: TVirtualStringTree;
    procedure AddButtonClick(Sender: TObject);
    procedure CancelButtonClick(Sender: TObject);
    procedure DelButtonClick(Sender: TObject);
    procedure FormDestroy(Sender: TObject);
    procedure FormShow(Sender: TObject);
    procedure SaveButtonClick(Sender: TObject);
    procedure ValuesGridBeforeSelection(Sender: TObject; aCol, aRow: Integer);
  private
    BaseScheme: TBaseScheme;
    ValuesEdit: TVSTEdit;
    procedure SetDelButtonEnabled;
    procedure SetRowNumbers;

    procedure ValueSelect;
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
  ColType: TVSTColumnType;

  Cln: TGridColumn;

  procedure AddColumn(const ACaption: String;      //to delete
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

  ValuesGrid.RowCount:= Length(Tbl.Fields[0].ExistingValues) + 1;   //to delete
  ValuesGrid.FixedRows:= 1;                                         //to delete
  AddColumn('№', nil, 50);                                          //to delete
  for i:= 0 to High(Tbl.Fields) do                                  //to delete
    AddColumn(Tbl.Fields[i].FieldName, Tbl.Fields[i].ExistingValues);



  ValuesEdit:= TVSTEdit.Create(VT1);
  ValuesEdit.AutosizeColumnDisable;
  //ValuesEdit.SelectedBGColor:= clMoneyGreen;
  ValuesEdit.IsShowZeros:= True;
  //ValuesEdit.CanUnselect:= False;
  //ValuesEdit.IsBeginEditOnKeyPress:= False;
  ValuesEdit.OnSelect:= @ValueSelect;
  //ValuesEdit.OnEdititingBegin:= @EditingBegin;
  //if Assigned(AFont) then ValuesEdit.SetSingleFont(AFont);
  ValuesEdit.AddColumnRowTitles('№ п/п', 50);
  for i:= 0 to High(Tbl.Fields) do
  begin
    ColType:= FieldTypeToColumnType(Tbl.Fields[i].FieldType);
    if ColType=ctUndefined then continue;
    if ColType=ctInteger then
    begin
      ValuesEdit.AddColumnInteger(Tbl.Fields[i].FieldName, 200);
      ValuesEdit.SetColumnInteger(Tbl.Fields[i].FieldName, VStrToInt(Tbl.Fields[i].ExistingValues));
    end
    else if ColType=ctDateTime then
    begin
      ValuesEdit.AddColumnDateTime(Tbl.Fields[i].FieldName, 'dd.mm.yyyy hh:mm:ss', 200);
      ValuesEdit.SetColumnDateTime(Tbl.Fields[i].FieldName, VStrToDateTime(Tbl.Fields[i].ExistingValues));
    end
    else if ColType=ctString then
    begin
      ValuesEdit.AddColumnString(Tbl.Fields[i].FieldName, 200);
      ValuesEdit.SetColumnString(Tbl.Fields[i].FieldName, Tbl.Fields[i].ExistingValues);
    end
    else if ColType=ctDouble then
    begin
      ValuesEdit.AddColumnDouble(Tbl.Fields[i].FieldName, 200);
      ValuesEdit.SetColumnDouble(Tbl.Fields[i].FieldName, VStrToFloat(Tbl.Fields[i].ExistingValues));
    end;
  end;
  SetRowNumbers;
  ValuesEdit.Draw;

  SetDelButtonEnabled;
end;

procedure TValuesEditForm.FormDestroy(Sender: TObject);
begin
  //if FEdit.IsEditing then
  //  ActionCancel(nil);
  FreeAndNil(ValuesEdit);
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

procedure TValuesEditForm.ValuesGridBeforeSelection(Sender: TObject; aCol, aRow: Integer);  //to delete
begin
  if ACol=0 then
    ValuesGrid.Options:= ValuesGrid.Options - [goEditing]
  else
    ValuesGrid.Options:= ValuesGrid.Options + [goEditing];
end;

procedure TValuesEditForm.DelButtonClick(Sender: TObject);
begin
  ValuesGrid.DeleteRow(ValuesGrid.Row); //to delete

  ValuesEdit.RowDelete(ValuesEdit.SelectedRowIndex);
  SetRowNumbers;
  SetDelButtonEnabled;
end;

procedure TValuesEditForm.AddButtonClick(Sender: TObject);
begin
  ValuesGrid.RowCount:= ValuesGrid.RowCount+1; //to delete

  ValuesEdit.RowAppend;
  SetRowNumbers;
  SetDelButtonEnabled;
end;

procedure TValuesEditForm.SetDelButtonEnabled;
begin
  //DelButton.Enabled:= ValuesGrid.RowCount>1; //to delete
  DelButton.Enabled:= Assigned(ValuesEdit) and ValuesEdit.IsSelected;// and (ValuesEdit.RowCount>0);
end;

procedure TValuesEditForm.SetRowNumbers;
var
  i: Integer;
begin
  for i:= 1 to ValuesGrid.RowCount-1 do    //to delete
    ValuesGrid.Cells[0, i]:= IntToStr(i);

  ValuesEdit.SetColumnRowTitles(VOrderStr(ValuesEdit.RowCount));
end;

procedure TValuesEditForm.ValueSelect;
begin
  SetDelButtonEnabled;
end;

procedure TValuesEditForm.BaseSchemeSet(const ABaseScheme: TBaseScheme);
begin
  BaseScheme:= ABaseScheme;
end;

end.

