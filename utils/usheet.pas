unit USheet;

{$mode ObjFPC}{$H+}

interface

uses
  Classes, SysUtils, Graphics, Grids, fpspreadsheetgrid, fpstypes,
  //DK packages utils
  DK_SheetWriter, DK_Vector, DK_Const, DK_Color, DK_StrUtils,
  //Project utils
  UTypes, UConst;

type

  { TSchemeSheet }

  TSchemeSheet = class (TObject)
  private
    FGrid: TsWorksheetGrid;
    FWriter: TSheetWriter;
    FTable: TTable;
    FRow, FFirstRow, FLastRow: Integer;
    FBeginRows, FEndRows: TIntVector;
    procedure CalcFieldsRowNumbers;
    procedure DrawTitleAndDescription;
    procedure DrawNotes;
    function GetFieldsBeginRow: Integer;
    function GetFieldsEndRow: Integer;
    procedure DrawTable(const ATable: TTable;
                        const AFieldSelectedIndex, AIndexSelectedIndex: Integer;
                        var AFirstRow: Integer);
    procedure MouseWheel(Sender: TObject; {%H-}Shift: TShiftState;
                         {%H-}WheelDelta: Integer; {%H-}MousePos: TPoint;
                         var {%H-}Handled: Boolean);
  public
    constructor Create(const AGrid: TsWorksheetGrid;
                       const AExistingValuesCount: Integer);
    destructor  Destroy; override;
    procedure DrawFieldLine(const AIndex: Integer; const ASelected: Boolean);
    procedure DrawIndexLine(const AIndex: Integer; const ASelected: Boolean);
    procedure Draw(const ATable: TTable;
                   const AFieldSelectedIndex, AIndexSelectedIndex: Integer);
    procedure Draw(const ATables: TTables);
    procedure Zoom(const APercents: Integer);
    function RowToFieldIndex(const ARow: Integer): Integer;
    function RowToIndexIndex(const ARow: Integer): Integer;
    property FirstRow: Integer read FFirstRow;
    property LastRow: Integer read FLastRow;
    property FieldsBeginRow: Integer read GetFieldsBeginRow;
    property FieldsEndRow: Integer read GetFieldsEndRow;
  end;

implementation

{ TSchemeSheet }

constructor TSchemeSheet.Create(const AGrid: TsWorksheetGrid;
  const AExistingValuesCount: Integer);
var
  ColWidths: TIntVector;
begin
  FGrid:= AGrid;
  FGrid.MouseWheelOption:= mwGrid;
  FGrid.ShowGridLines:= False;
  FGrid.ShowHeaders:= False;
  FGrid.SelectionPen.Style:= psClear;
  FGrid.OnMouseWheel:= @MouseWheel;

  ColWidths:= VCreateInt([
    30,   // Левый отступ
    200,  // Наименование поля
    100,  // Тип поля
    30,   // PK
    30,   // Autoinc/Unique
    30,   // Null/NotNull
    120,  // Default value
    30,   // Стрелки ссылок
    200,  // Cсылки
    400   // Описание полей
  ]);

  if AExistingValuesCount>0 then
  begin
    VAppend(ColWidths, 30); //пустой столбец перед таблицей существующих значений
    VReDim(ColWidths, Length(ColWidths) + AExistingValuesCount, 100);
  end;

  FWriter:= TSheetWriter.Create(ColWidths, FGrid.Worksheet, FGrid);
end;

destructor TSchemeSheet.Destroy;
begin
  if Assigned(FWriter) then FreeAndNil(FWriter);
  inherited Destroy;
end;

procedure TSchemeSheet.DrawTitleAndDescription;
var
  R, N: Integer;
begin
  FWriter.SetBackground(COLOR_BACKGROUND_EXTRA);

  //Заголовок
  R:= FRow;
  FWriter.SetFont(SCHEME_FONT_NAME_DEFAULT,
                  TABLE_TITLE_FONT_SIZE, TABLE_TITLE_FONT_STYLE,
                  COLOR_TEXT);
  FWriter.SetAlignment(haLeft, vaCenter);
  FWriter.WriteText(R, 1, R, 10, FTable.TableName, cbtNone, True, True);

  //Описание
  R:= R + 1;
  FWriter.SetFont(SCHEME_FONT_NAME_DEFAULT,
                  TABLE_DESCRIPTION_FONT_SIZE, TABLE_DESCRIPTION_FONT_STYLE,
                  COLOR_TEXT);
  FWriter.SetAlignment(haLeft, vaCenter);
  FWriter.WriteText(R, 1, R, 10, FTable.Description, cbtNone, True, True);

  //Границы
  FWriter.SetBorders(TABLE_MAIN_LINE_STYLE, COLOR_GRIDLINE);
  FWriter.DrawBorders(FRow, 1, R, 10, cbtOuter);
  FWriter.DrawBorders(FRow, 11, R, 11, cbtLeft);

  //существующие значения
  if not FieldsIsNil(FTable.Fields) then
  begin
    N:= Length(FTable.Fields[0].ExistingValues);
    if N>0 then
    begin
      FWriter.WriteText(FRow, 12, R, 11+N, 'Записанные'+SYMBOL_BREAK+'значения', cbtOuter);
      FWriter.DrawBorders(FRow, 12, R, 11+N, cbtOuter);
      FWriter.DrawBorders(FRow, 12+N, R, 12+N, cbtLeft);
    end;
  end;

  FRow:= R + 1;
end;

procedure TSchemeSheet.CalcFieldsRowNumbers;
var
  i, R1, R2: Integer;

  function GetLineRowCount(const AIndex: Integer): Integer;
  var
    x: Integer;
  begin
    Result:= 1;
    if FTable.Fields[AIndex].ReferenceTo.TableName<>EmptyStr then Result:= 2;
    x:= Length(FTable.Fields[AIndex].Description);
    if x>Result then Result:= x;
    x:= Length(FTable.Fields[AIndex].ReferenceFrom.TableNames);
    if x>Result then Result:= x;
  end;

begin
  FBeginRows:= nil;
  FEndRows:= nil;

  if FieldsIsNil(FTable.Fields) then
  begin
    VAppend(FBeginRows, FRow);
    VAppend(FEndRows, FRow);
    Exit;
  end;

  R2:= FRow - 1;
  for i:= 0 to High(FTable.Fields) do
  begin
    R1:= R2 + 1;
    R2:= R1 + GetLineRowCount(i) - 1;
    VAppend(FBeginRows, R1);
    VAppend(FEndRows, R2);
  end;
end;

procedure TSchemeSheet.DrawFieldLine(const AIndex: Integer; const ASelected: Boolean);
var
  i, R1, R2, N: Integer;
  S: String;

  procedure SetBackground;
  begin
    if ASelected then
      FWriter.SetBackground(DefaultSelectionBGColor)
    else
      FWriter.SetBackgroundClear;
  end;

  procedure SetFont(const ASize: Single; const AStyle: TFontStyles);
  begin
     if ASelected then
       FWriter.SetFont(SCHEME_FONT_NAME_DEFAULT,
                       ASize, AStyle{+[fsBold]}, COLOR_TEXT_SELECTED)
     else
       FWriter.SetFont(SCHEME_FONT_NAME_DEFAULT,
                       ASize, AStyle{-[fsBold]}, COLOR_TEXT);
  end;

  procedure DrawEmptyCells(const ARow1, ARow2, ACol: Integer);
  var
    k: Integer;
  begin
    for k:= ARow1+1 to ARow2 do
      FWriter.WriteText(k, ACol, EmptyStr, cbtNone);
  end;

begin
  if VIsNil(FBeginRows) then Exit;

  R1:= FBeginRows[AIndex];
  R2:= FEndRows[AIndex];

  SetBackground;

  //1-й столбец (пустой)
  SetFont(FIELD_NAME_FONT_SIZE, FIELD_NAME_FONT_STYLE);
  FWriter.SetAlignment(haLeft, vaCenter);
  FWriter.WriteText(R1, 1, EmptyStr, cbtNone);
  DrawEmptyCells(R1, R2, 1);
  //2-й столбец (FieldName)
  SetFont(FIELD_NAME_FONT_SIZE, FIELD_NAME_FONT_STYLE);
  FWriter.WriteText(R1, 2, FTable.Fields[AIndex].FieldName, cbtNone, True, True);
  DrawEmptyCells(R1, R2, 2);
  //3-й столбец (FieldType)
  SetFont(FIELD_TYPE_FONT_SIZE, FIELD_TYPE_FONT_STYLE);
  FWriter.WriteText(R1, 3, FTable.Fields[AIndex].FieldType, cbtNone, True, True);
  DrawEmptyCells(R1, R2, 3);
  //4-й столбец (PrimaryKey)
  FWriter.SetAlignment(haCenter, vaCenter);
  S:= EmptyStr;
  if FTable.Fields[AIndex].PrimaryKey then
  begin
    S:= SYMBOL_PRIMARYKEY;
    SetFont(FIELD_SYMBOLS_FONT_SIZE, []{FIELD_SYMBOLS_FONT_STYLE});
  end;
  FWriter.WriteText(R1, 4, S, cbtNone, True, True);
  DrawEmptyCells(R1, R2, 4);
  //5-й столбец (Autoinc|Unique)
  S:= EmptyStr;
  if FTable.Fields[AIndex].Status then
  begin
    if FTable.Fields[AIndex].PrimaryKey then
    begin
      S:= SYMBOL_AUTOINC;
      SetFont(FIELD_SYMBOLS_FONT_SIZE, [fsBold]{FIELD_SYMBOLS_FONT_STYLE});
    end
    else begin
      S:= SYMBOL_UNIQUE;
      SetFont(FIELD_SYMBOLS_FONT_SIZE, []{FIELD_SYMBOLS_FONT_STYLE});
    end;
  end;
  FWriter.WriteText(R1, 5, S, cbtNone, True, True);
  DrawEmptyCells(R1, R2, 5);
  //6-й столбец (NotNull)
  FWriter.SetAlignment(haCenter, vaCenter);
  SetFont(FIELD_SYMBOLS_FONT_SIZE, [fsBold]{FIELD_SYMBOLS_FONT_STYLE});
  S:= SYMBOL_NULL;
  if FTable.Fields[AIndex].NotNull then
    S:= SYMBOL_NOTNULL;
  FWriter.WriteText(R1, 6, S, cbtNone, True, True);
  DrawEmptyCells(R1, R2, 6);

  //7-й столбец (DefaultValue)
  FWriter.SetAlignment(haLeft, vaCenter);
  SetFont(FIELD_DEFVALUE_FONT_SIZE, FIELD_DEFVALUE_FONT_STYLE);
  S:= EmptyStr;
  if FTable.Fields[AIndex].DefaultValue<>EmptyStr then
    S:= 'Def=' + FTable.Fields[AIndex].DefaultValue;
  //else if FTable.Fields[AIndex].FieldType='TEXT' then
  //  S:= 'Def=' + QuotedStr(S);

  FWriter.WriteText(R1, 7, S, cbtNone, True, True);
  DrawEmptyCells(R1, R2, 7);
  //8-й столбец (Стрелки ссылок)
  FWriter.SetAlignment(haCenter, vaCenter);
  SetFont(FIELD_SYMBOLS_FONT_SIZE, []{FIELD_SYMBOLS_FONT_STYLE});
  S:= EmptyStr;
  if not VIsNil(FTable.Fields[AIndex].ReferenceFrom.TableNames) then
    S:= SYMBOL_REFERENCEFROM
  else if FTable.Fields[AIndex].ReferenceTo.TableName<>EmptyStr then
    S:= SYMBOL_REFERENCETO;
  FWriter.WriteText(R1, 8, S, cbtNone, True, True);
  DrawEmptyCells(R1, R2, 8);
  //9-й столбец (Ссылки)
  FWriter.SetAlignment(haLeft, vaCenter);
  if S=EmptyStr then
  begin
    FWriter.WriteText(R1, 9, S, cbtNone, True, True);
    DrawEmptyCells(R1, R2, 9);
  end
  else begin
    SetFont(FIELD_REFERENCE_FONT_SIZE, FIELD_REFERENCE_FONT_STYLE);
    if not VIsNil(FTable.Fields[AIndex].ReferenceFrom.TableNames) then
    begin
      for i:=0 to High(FTable.Fields[AIndex].ReferenceFrom.TableNames) do
      begin
        S:= FTable.Fields[AIndex].ReferenceFrom.TableNames[i] + '.' +
            FTable.Fields[AIndex].ReferenceFrom.FieldNames[i];
        FWriter.WriteText(R1+i, 9, S, cbtNone, True, True);
      end;
      DrawEmptyCells(R1+i, R2, 9);
    end
    else begin
      S:= FTable.Fields[AIndex].ReferenceTo.TableName + '.' +
          FTable.Fields[AIndex].ReferenceTo.FieldName;
      FWriter.WriteText(R1, 9, S, cbtNone, True, True);
      S:= 'Upd(' +
          ForeignKeyActionToStr(FTable.Fields[AIndex].ReferenceTo.OnUpdate) +
          ') Del(' +
          ForeignKeyActionToStr(FTable.Fields[AIndex].ReferenceTo.OnDelete) +
          ') ';
      FWriter.WriteText(R1+1, 9, S, cbtNone, True, True);
      DrawEmptyCells(R1+1, R2, 9);
    end;
  end;
  //10-й столбец (Примечания к полям)
  SetFont(FIELD_DESCRIPTION_FONT_SIZE, FIELD_DESCRIPTION_FONT_STYLE);
  FWriter.SetAlignment(haLeft, vaCenter);
  if VIsNil(FTable.Fields[AIndex].Description) then
  begin
    FWriter.WriteText(R1, 10, EmptyStr, cbtNone);
    DrawEmptyCells(R1, R2, 10);
  end
  else begin
    for i:=0 to High(FTable.Fields[AIndex].Description) do
    begin
      S:= FTable.Fields[AIndex].Description[i];
      FWriter.WriteText(R1+i, 10, S, cbtNone, True, True);
    end;
    DrawEmptyCells(R1+i, R2, 10);
  end;

  //существующие значения
  N:= 0;
  if not FieldsIsNil(FTable.Fields) then
  begin
    N:= Length(FTable.Fields[AIndex].ExistingValues);
    if N>0 then
    begin
      FWriter.SetBackgroundClear;
      FWriter.SetBordersDefault;
      FWriter.SetFont(SCHEME_FONT_NAME_DEFAULT,
                      FIELD_DESCRIPTION_FONT_SIZE,
                      FIELD_DESCRIPTION_FONT_STYLE, COLOR_TEXT);
      FWriter.SetAlignment(haCenter, vaCenter);
      for i:=0 to N-1 do
      begin
        S:= FTable.Fields[AIndex].ExistingValues[i];
        FWriter.WriteText(R1, 12+i, R2, 12+i, S, cbtNone, True, True);
        if FTable.Fields[AIndex].FieldType='DATETIME' then
        begin
          if S='0' then
            FWriter.WriteText(R1, 12+i, R2, 12+i, S)
          else
            FWriter.WriteText(R1, 12+i, R2, 12+i,
                            SCutRight(S, Length(S)-10)+SYMBOL_BREAK+SCutLeft(S, 10));
        end;
      end;
    end;
  end;

  //границы
  if Length(FTable.Fields)=1 then
    FWriter.SetBordersStyle(TABLE_MAIN_LINE_STYLE, TABLE_MAIN_LINE_STYLE, TABLE_MAIN_LINE_STYLE, TABLE_MAIN_LINE_STYLE, TABLE_MAIN_LINE_STYLE)
  else if AIndex=0 then
    FWriter.SetBordersStyle(TABLE_MAIN_LINE_STYLE, TABLE_MAIN_LINE_STYLE, TABLE_MAIN_LINE_STYLE, lsDotted, lsDotted)
  else if AIndex=High(FTable.Fields) then
    FWriter.SetBordersStyle(TABLE_MAIN_LINE_STYLE, TABLE_MAIN_LINE_STYLE, lsDotted, TABLE_MAIN_LINE_STYLE, lsDotted)
  else
    FWriter.SetBordersStyle(TABLE_MAIN_LINE_STYLE, TABLE_MAIN_LINE_STYLE, lsDotted, lsDotted, lsDotted);
  FWriter.DrawBorders(R1, 1, R2, 9, cbtOuter);
  FWriter.DrawBorders(R1, 10, R2, 10, cbtOuter);
  FWriter.DrawBorders(R1, 11, R2, 11, cbtLeft);
  if N>0 then
  begin
    for i:=0 to N-1 do
      FWriter.DrawBorders(R1, 12+i, R2, 12+i, cbtOuter);
    FWriter.DrawBorders(R1, 12+N, R2, 12+N, cbtLeft);
  end;
end;

procedure TSchemeSheet.DrawIndexLine(const AIndex: Integer; const ASelected: Boolean);
var
  R: Integer;
  S: String;

  procedure SetBackground;
  begin
    if ASelected then
      FWriter.SetBackground(DefaultSelectionBGColor)
    else
      FWriter.SetBackgroundClear;
  end;

  procedure SetFont(const ASize: Single; const AStyle: TFontStyles);
  begin
     if ASelected then
       FWriter.SetFont(SCHEME_FONT_NAME_DEFAULT,
                       ASize, AStyle{+[fsBold]}, COLOR_TEXT_SELECTED)
     else
       FWriter.SetFont(SCHEME_FONT_NAME_DEFAULT,
                       ASize, AStyle{-[fsBold]}, COLOR_TEXT);
  end;

begin
  R:= FEndRows[High(FEndRows)] + 2 + 2*AIndex;
  SetBackground;

  //1-й столбец (пустой)
  SetFont(FIELD_NAME_FONT_SIZE, FIELD_NAME_FONT_STYLE);
  FWriter.SetAlignment(haLeft, vaCenter);
  FWriter.WriteText(R, 1, R+1, 1, EmptyStr, cbtNone);
  //2,3,4 имя индекса
  S:= {'IDX_' + SUpper(FTable.TableName) + '_' +}
      {SUpper(}FTable.Indexes[AIndex].IndexName{)};
  SetFont(FIELD_NAME_FONT_SIZE, FIELD_NAME_FONT_STYLE);
  FWriter.SetAlignment(haLeft, vaCenter);
  FWriter.WriteText(R, 2, R+1, 4, S, cbtNone);

  //5-й столбец (Unique)
  FWriter.SetAlignment(haCenter, vaCenter);
  if FTable.Indexes[AIndex].Unique then
  begin
    S:= SYMBOL_UNIQUE;
    SetFont(FIELD_SYMBOLS_FONT_SIZE, []{FIELD_SYMBOLS_FONT_STYLE});
  end
  else begin
    S:= EmptyStr;
    SetFont(FIELD_NAME_FONT_SIZE, FIELD_NAME_FONT_STYLE);
  end;
  FWriter.WriteText(R, 5, R+1, 5, S, cbtNone);

  //6,7,8,9,10 список полей + where
  S:= VVectorToStr(FTable.Indexes[AIndex].FieldNames, ', ');
  SetFont(FIELD_NAME_FONT_SIZE, FIELD_NAME_FONT_STYLE);
  FWriter.SetAlignment(haLeft, vaCenter);
  FWriter.WriteText(R, 6, R, 10, S, cbtNone, True, True);
  S:= FTable.Indexes[AIndex].Where;
  FWriter.WriteText(R+1, 6, R+1, 10, S, cbtNone, True, True);
  if S=EmptyStr then FWriter.RowHeight[R+1]:= 0;

  //границы
  if Length(FTable.Indexes)=1 then
    FWriter.SetBordersStyle(TABLE_MAIN_LINE_STYLE, TABLE_MAIN_LINE_STYLE, TABLE_MAIN_LINE_STYLE, TABLE_MAIN_LINE_STYLE, TABLE_MAIN_LINE_STYLE)
  else if AIndex=0 then
    FWriter.SetBordersStyle(TABLE_MAIN_LINE_STYLE, TABLE_MAIN_LINE_STYLE, TABLE_MAIN_LINE_STYLE, lsDotted, lsDotted)
  else if AIndex=High(FTable.Indexes) then
    FWriter.SetBordersStyle(TABLE_MAIN_LINE_STYLE, TABLE_MAIN_LINE_STYLE, lsDotted, TABLE_MAIN_LINE_STYLE, lsDotted)
  else
    FWriter.SetBordersStyle(TABLE_MAIN_LINE_STYLE, TABLE_MAIN_LINE_STYLE, lsDotted, lsDotted, lsDotted);
  FWriter.DrawBorders(R, 1, R+1, 10, cbtOuter);
  FWriter.DrawBorders(R, 11, R+1, 11, cbtLeft);
end;

procedure TSchemeSheet.DrawNotes;
var
  i, R1, R2: Integer;
begin
  if VIsNil(FTable.Notes) then Exit;

  R1:= FRow;
  FWriter.SetBackground(COLOR_BACKGROUND_EXTRA);
  FWriter.SetBorders(TABLE_MAIN_LINE_STYLE, COLOR_GRIDLINE);
  FWriter.SetFont(SCHEME_FONT_NAME_DEFAULT,
              TABLE_DESCRIPTION_FONT_SIZE, TABLE_DESCRIPTION_FONT_STYLE,
              COLOR_TEXT);
  FWriter.SetAlignment(haLeft, vaCenter);
  FWriter.WriteText(R1, 1, R1, 10, 'Примечания', cbtOuter, True, True);

  FWriter.SetBackgroundClear;
  FWriter.WriteText(R1, 11, EmptyStr, cbtLeft);
  //FWriter.RowHeight[R1]:= MIDDLE_ROW_HEIGHT;


  R1:= R1 + 1;
  FWriter.SetAlignment(haLeft, vaCenter);
  FWriter.SetFont(SCHEME_FONT_NAME_DEFAULT,
                  {TABLE_NOTES_FONT_SIZE, TABLE_NOTES_FONT_STYLE,}
                  FIELD_NAME_FONT_SIZE, FIELD_NAME_FONT_STYLE,
                  COLOR_TEXT);
  for i:=0 to High(FTable.Notes) do
    FWriter.WriteText(R1+i, 1, R1+i, 10, FTable.Notes[i], cbtNone, True, True);
  FWriter.SetBorders(TABLE_MAIN_LINE_STYLE, COLOR_GRIDLINE);
  R2:= R1 + High(FTable.Notes);
  FWriter.DrawBorders(R1, 1, R2, 10, cbtOuter);
  FWriter.DrawBorders(R1, 11, R2, 11, cbtLeft);

  //FRow:= R2;
end;

function TSchemeSheet.GetFieldsBeginRow: Integer;
begin
  Result:= VFirst(FBeginRows);
end;

function TSchemeSheet.GetFieldsEndRow: Integer;
begin
  Result:= VLast(FEndRows);
end;

procedure TSchemeSheet.DrawTable(const ATable: TTable;
                        const AFieldSelectedIndex, AIndexSelectedIndex: Integer;
                        var AFirstRow: Integer);
var
  i, N: Integer;
begin
  FRow:= AFirstRow;
  FTable:= ATable;

  FWriter.SetBackgroundClear;

  //Заголовок и описание таблицы
  DrawTitleAndDescription;
  //Начальные и конечные номера строк для полей таблицы
  CalcFieldsRowNumbers;
  //Поля
  if FieldsIsNil(FTable.Fields) then
  begin
    FWriter.SetBackgroundClear;
    FWriter.SetBordersStyle(TABLE_MAIN_LINE_STYLE, TABLE_MAIN_LINE_STYLE,
                            TABLE_MAIN_LINE_STYLE, TABLE_MAIN_LINE_STYLE,
                            TABLE_MAIN_LINE_STYLE);
    FWriter.WriteText(FRow, 1, FRow, 10, EmptyStr, cbtOuter);
  end
  else begin
    for i:= 0 to High(FTable.Fields) do
      DrawFieldLine(i, i=AFieldSelectedIndex);
    //полоса под существующими значениями
    if Length(FTable.Fields)>0 then
    begin
      N:= Length(FTable.Fields[0].ExistingValues);
      if N>0 then
      begin
        FRow:= VLast(FEndRows) + 1;
        FWriter.SetBackground(COLOR_BACKGROUND_EXTRA);
        FWriter.WriteText(FRow, 12, FRow, 11+N, EmptyStr, cbtOuter);
        FWriter.DrawBorders(FRow, 12+N, cbtLeft);
      end;
    end;
    //индексы
    if not IndexesIsNil(FTable.Indexes) then
    begin
      FRow:= VLast(FEndRows) + 1;
      FWriter.SetBackground(COLOR_BACKGROUND_EXTRA);
      FWriter.SetFont(SCHEME_FONT_NAME_DEFAULT,
                  TABLE_DESCRIPTION_FONT_SIZE, TABLE_DESCRIPTION_FONT_STYLE,
                  COLOR_TEXT);
      FWriter.SetAlignment(haLeft, vaCenter);
      FWriter.WriteText(FRow, 1, FRow, 10, 'Индексы', cbtOuter);
      FWriter.SetBackgroundClear;
      FWriter.WriteText(FRow, 11, EmptyStr, cbtLeft);

      for i:= 0 to High(FTable.Indexes) do
        DrawIndexLine(i, i=AIndexSelectedIndex);
    end;
  end;

  //примечания к таблице
  i:= 2*Length(FTable.Indexes);
  FRow:= VLast(FEndRows) + 1 + i;
  if i>0 then FRow:= FRow + 1;
  DrawNotes;

  FWriter.SetBordersDefault;
  i:= Length(FTable.Notes);
  FRow:= FRow + i;
  if i>0 then FRow:= FRow + 1;
  FWriter.SetBackground(COLOR_BACKGROUND_EXTRA);
  FWriter.WriteText(FRow, 1, FRow, 10, EmptyStr, cbtOuter);
  FWriter.DrawBorders(FRow, 11, cbtLeft);
  FWriter.RowHeight[FRow]:= MIDDLE_ROW_HEIGHT;


  FFirstRow:= AFirstRow;
  FLastRow:= FRow;
  AFirstRow:= FLastRow;
end;

procedure TSchemeSheet.MouseWheel(Sender: TObject; Shift: TShiftState;
  WheelDelta: Integer; MousePos: TPoint; var Handled: Boolean);
begin
  (Sender as TsWorksheetGrid).Invalidate;
end;

procedure TSchemeSheet.Draw(const ATable: TTable;
                     const AFieldSelectedIndex, AIndexSelectedIndex: Integer);
var
  R: Integer;
begin
  R:= 1;
  FWriter.BeginEdit;
  DrawTable(ATable, AFieldSelectedIndex, AIndexSelectedIndex, R);
  FWriter.EndEdit;
end;

procedure TSchemeSheet.Draw(const ATables: TTables);
var
  R, DeltaR, i: Integer;
begin
  DeltaR:= 2;
  R:= 1 - DeltaR;
  FWriter.BeginEdit;
  for i:= 0 to High(ATables) do
  begin
    R:= R + DeltaR;
    DrawTable(ATables[i], -1, -1, R);
  end;
  FWriter.EndEdit;
end;

procedure TSchemeSheet.Zoom(const APercents: Integer);
begin
  FWriter.SetZoom(APercents);
end;

function TSchemeSheet.RowToFieldIndex(const ARow: Integer): Integer;
var
  i: Integer;
begin
  Result:= -1;
  if VIsNil(FBeginRows) then Exit;
  if (ARow<VFirst(FBeginRows)) or (ARow>VLast(FEndRows)) then Exit;
  for i:= 0 to High(FBeginRows) do
  begin
    if (ARow>=FBeginRows[i]) and (ARow<=FEndRows[i]) then
    begin
      Result:= i;
      break;
    end;
  end;
end;

function TSchemeSheet.RowToIndexIndex(const ARow: Integer): Integer;
var
  i, R1, R2: Integer;
begin
  Result:= -1;
  if VIsNil(FEndRows) then Exit; //нет ни одного поля
  i:= Length(FTable.Indexes);
  if i=0 then Exit; //нет ни одного индекса
  R1:= VLast(FEndRows) + 2;
  R2:= R1 + 2*i - 1;
  if (ARow>=R1) and (ARow<=R2) then
    Result:= (ARow - R1) div 2;
end;

end.

