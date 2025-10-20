unit UScheme;

{$mode ObjFPC}{$H+}

interface

uses
  Classes, SysUtils, fpspreadsheetgrid,
  //DK packages utils
  DK_Vector, DK_StrUtils,
  //Project utils
  UTypes, USheet;

type

  { TBaseScheme }

  TBaseScheme = class (TObject)
  private
    FTables: TTables;
    FTableNames: TStrVector;
    FTableSelectedIndex: Integer;
    FFieldSelectedIndex: Integer;
    FIndexSelectedIndex: Integer;
    FGrid: TsWorksheetGrid;
    FSheet: TSchemeSheet;
    FSheetZoomPercent: Integer;

    function GetActiveTable: TTable;
    function GetActiveField: TField;
    function GetActiveIndex: TIndex;

    procedure TableIns(const ATable: TTable);
    procedure FieldMove(const ADirection: Integer);

    function FindField(const ATableName, AFieldName: String;
                                 out ATableIndex, AFieldIndex: Integer): Boolean;

    procedure ReferenceToUpdate(const ATableName, AFieldName: String;
                                const AToTableNames, AToFieldNames: TStrVector);
    procedure ReferenceToDel(const AToTableNames, AToFieldNames: TStrVector);

    procedure ReferenceFromDel(const ATableName, AFieldName,
                                 AFromTableName, AFromFieldName: String);
    procedure ReferenceFromAdd(const ATableName, AFieldName,
                                 AFromTableName, AFromFieldName: String);
    procedure FieldInIndexDel(const AFieldName: String);

    function GetTableSQL(const ATableIndex: Integer): TStrVector;

    function TableIndexesToSQL: TIntVector;
  public
    constructor Create(const AViewGrid: TsWorksheetGrid);
    destructor  Destroy; override;
    procedure Clear;

    procedure ReadFromJSON(const AJSON: String);
    function WriteToJSON: String;

    function ActiveTableSQL: TStrVector;
    function SQL: TStrVector;

    procedure TableAdd(const ATable: TTable);
    procedure TableSet(const ATable: TTable);
    procedure TableDel(const ANeedReferenceToDelete: Boolean = True; //удалять из других таблиц внешние ключи на удаляемую таблицу
                       const ANeedReferenceFromDelete: Boolean = True); //удалять из других таблиц упоминания внешних ключей с удаляемой таблицы
    procedure TableSelect(const AIndex: Integer);
    procedure TableUnselect;
    function IsTableSelected: Boolean;

    procedure FieldAdd(const AField: TField);
    procedure FieldSet(const AField: TField);
    procedure FieldDel;
    procedure FieldUnselect;
    procedure FieldSelect(const AFieldIndex: Integer);
    procedure FieldOrIndexSelect(const ARow: Integer);
    procedure Unselect;
    procedure FieldMoveUp;
    procedure FieldMoveDown;
    function FieldCanUp: Boolean;
    function FieldCanDown: Boolean;
    function FieldsList(const ATableName: String): TStrVector;

    procedure IndexAdd(const AIndex: TIndex);
    procedure IndexSet(const AIndex: TIndex);
    procedure IndexDel;
    procedure IndexUnselect;
    procedure IndexSelect(const AIndex: Integer);

    function IsFieldSelected: Boolean;
    function IsIndexSelected: Boolean;

    function IsTablesEmpty: Boolean;
    function IsFieldsEmpty: Boolean;
    function IsIndexesEmpty: Boolean;

    procedure Zoom(const APercents: Integer);
    procedure AtiveTableDraw;
    procedure Draw;

    property ActiveTable: TTable read GetActiveTable;
    property ActiveField: TField read GetActiveField;
    property ActiveIndex: TIndex read GetActiveIndex;

    property Tables: TTables read FTables;
    property TableNames: TStrVector read FTableNames;
    property TableSelectedIndex: Integer read FTableSelectedIndex;
    property FieldSelectedIndex: Integer read FFieldSelectedIndex;
    property IndexSelectedIndex: Integer read FIndexSelectedIndex;
  end;

implementation

{ TBaseScheme }

procedure TBaseScheme.TableAdd(const ATable: TTable);
begin
  //проверяем на дублирование имён
  if VIndexOf(FTableNames, ATable.TableName, False)>=0 then
    raise EDuplicateException.Create('В схеме уже есть таблица "' +
                                     ATable.TableName + '"!');
  //вставляем таблицу
  TableIns(ATable);
end;

procedure TBaseScheme.TableSet(const ATable: TTable);
var
  Ind: Integer;
begin
  if not IsTableSelected then Exit;

  //проверяем на дублирование имён
  Ind:= VIndexOf(FTableNames, ATable.TableName, False);
  if (Ind<>FTableSelectedIndex) and (Ind>=0) then
    raise EDuplicateException.Create('В схеме уже есть таблица "' +
                                     ATable.TableName + '"!');

  //если изменилось имя таблицы
  //меняем ReferenceTo во всех полях всех таблиц, которые ссылаются на поля этой таблицы
  if not SSame(ATable.TableName, ActiveTable.TableName, False) then
    for Ind:=0 to High(ActiveTable.Fields) do
      ReferenceToUpdate(ATable.TableName, ActiveTable.Fields[Ind].FieldName,
                    ActiveTable.Fields[Ind].ReferenceFrom.TableNames,
                    ActiveTable.Fields[Ind].ReferenceFrom.FieldNames);

  //удаляем старый вариант таблицы
  TableDel(False);
  //вставляем новый вариант таблицы
  TableIns(ATable);
end;

procedure TBaseScheme.TableIns(const ATable: TTable);
var
  Ind: Integer;
begin
  //ищем позицию для таблицы
  Ind:= VIndexOfAsc(FTableNames, ATable.TableName, False);
  //вставляем таблицу
  TablesIns(FTables, Ind, ATable);
  VIns(FTableNames, Ind, ATable.TableName);
  //меняем соответственно индекс активной таблицы
  TableSelect(Ind);
end;

procedure TBaseScheme.TableDel(const ANeedReferenceToDelete: Boolean = True;
                         const ANeedReferenceFromDelete: Boolean = True);
var
  i: Integer;
  ThisTableName, ThisFieldName, OtherTableName, OtherFieldName: String;
begin
  if not IsTableSelected then Exit;

  ThisTableName:= ActiveTable.TableName;
  for i:=0 to High(ActiveTable.Fields) do
  begin
    //удаляем из других таблиц упоминания о внешних ключах от этой таблицы
    if ANeedReferenceFromDelete then
    begin
      ThisFieldName:= ActiveTable.Fields[i].FieldName;
      OtherTableName:= ActiveTable.Fields[i].ReferenceTo.TableName;
      if not SSame(OtherTableName, EmptyStr) then
      begin
        OtherFieldName:= ActiveTable.Fields[i].ReferenceTo.FieldName;
        ReferenceFromDel(OtherTableName, OtherFieldName, ThisTableName, ThisFieldName);
      end;
    end;
    //удаляем из других таблиц внешние ключи на эту таблицу
    if ANeedReferenceToDelete then
      ReferenceToDel(ActiveTable.Fields[i].ReferenceFrom.TableNames,
                     ActiveTable.Fields[i].ReferenceFrom.FieldNames);
  end;

  //удаляем таблицу
  TablesDel(FTables, FTableSelectedIndex);
  VDel(FTableNames, FTableSelectedIndex);
  TableUnselect;
end;

procedure TBaseScheme.Clear;
begin
  if IsTablesEmpty then
  begin
    FTableSelectedIndex:= -1;
    FFieldSelectedIndex:= -1;
    FIndexSelectedIndex:= -1;
  end
  else
    TableUnselect;

  FTables:= nil;
  FTableNames:= nil;
end;

procedure TBaseScheme.ReadFromJSON(const AJSON: String);
var
  i: Integer;
begin
  Clear;
  FTables:= JSONToTables(AJSON);
  for i:= 0 to High(FTables) do
    VAppend(FTableNames, FTables[i].TableName);
end;

function TBaseScheme.WriteToJSON: String;
begin
  Result:= EmptyStr;
  if IsTablesEmpty then Exit;
  Result:= TablesToJSON(FTables);
end;

function TBaseScheme.ActiveTableSQL: TStrVector;
begin
  Result:= nil;
  if not IsTableSelected then Exit;
  Result:= GetTableSQL(FTableSelectedIndex);
end;

function TBaseScheme.TableIndexesToSQL: TIntVector;
{выстраивает индексы таблиц в зависимости от внешних ключей,
 чтобы внешние таблицы создавались раньше, чем те, которые на них ссылаются}
var
  i, j, CurrentTableIndex, ForeignTableIndex: Integer;
  S: String;
begin
  Result:= nil;
  if IsTablesEmpty then Exit;
  Result:= VOrder(High(FTableNames), True);

  i:= 0;
  while i<Length(FTableNames) do
  begin
    CurrentTableIndex:= Result[i];
    ForeignTableIndex:= -1;
    for j:= 0 to High(FTables[CurrentTableIndex].Fields) do
    begin
      S:= FTables[CurrentTableIndex].Fields[j].ReferenceTo.TableName;
      if S<>EmptyStr then
      begin
        ForeignTableIndex:= VIndexOf(Result, VIndexOf(FTableNames, S));
        if ForeignTableIndex>i then break;
      end;
    end;
    if ForeignTableIndex>i then
    begin
      VSwap(Result, i, ForeignTableIndex);
      continue;
    end;
    i:= i+1;
  end;
end;

function TBaseScheme.SQL: TStrVector;
var
  i: Integer;
  Indexes: TIntVector;
begin
  Result:= nil;
  if IsTablesEmpty then Exit;

  Indexes:= TableIndexesToSQL;

  VAppend(Result, 'PRAGMA ENCODING     = "UTF-8";');
  VAppend(Result, 'PRAGMA FOREIGN_KEYS = ON;');

  for i:=0 to High(FTables) do
  begin
    VAppend(Result, EmptyStr);
    Result:= VAdd(Result, GetTableSQL(Indexes[i]));
  end;
end;

procedure TBaseScheme.TableSelect(const AIndex: Integer);
begin
  TableUnselect;
  FTableSelectedIndex:= AIndex;
end;

procedure TBaseScheme.TableUnselect;
begin
  Unselect;
  FTableSelectedIndex:= -1;
end;

function TBaseScheme.IsTableSelected: Boolean;
begin
  Result:= FTableSelectedIndex>=0;
end;

function TBaseScheme.FindField(const ATableName, AFieldName: String;
    out ATableIndex, AFieldIndex: Integer): Boolean;
begin
  Result:= False;
  ATableIndex:= TablesIndexOf(FTables, ATableName, False);
  if ATableIndex<0 then Exit;
  AFieldIndex:= FieldsIndexOf(FTables[ATableIndex].Fields, AFieldName, False);
  Result:= AFieldIndex>=0;
end;

procedure TBaseScheme.ReferenceToUpdate(const ATableName, AFieldName: String;
    const AToTableNames, AToFieldNames: TStrVector);
var
  TableInd, FieldInd, i: Integer;
begin
  for i:= 0 to High(AToTableNames) do
  begin
    if FindField(AToTableNames[i], AToFieldNames[i], TableInd, FieldInd) then
    begin
      FTables[TableInd].Fields[FieldInd].ReferenceTo.TableName:= ATableName;
      FTables[TableInd].Fields[FieldInd].ReferenceTo.FieldName:= AFieldName;
    end;
  end;
end;

procedure TBaseScheme.ReferenceToDel(const AToTableNames,
  AToFieldNames: TStrVector);
var
  i, TableInd, FieldInd: Integer;
begin
  for i:= 0 to High(AToTableNames) do
    if FindField(AToTableNames[i], AToFieldNames[i], TableInd, FieldInd) then
      ReferenceToClear(FTables[TableInd].Fields[FieldInd].ReferenceTo);
end;

procedure TBaseScheme.ReferenceFromDel(const ATableName, AFieldName,
                                             AFromTableName, AFromFieldName: String);
var
  TableInd, FieldInd, Ind, i: Integer;
begin
  if (ATableName=EmptyStr) or (AFieldName=EmptyStr) then Exit;

  if FindField(ATableName, AFieldName, TableInd, FieldInd) then
  begin
    Ind:= -1;
    for i:= 0 to High(FTables[TableInd].Fields[FieldInd].ReferenceFrom.TableNames) do
    begin
      if SSame(FTables[TableInd].Fields[FieldInd].ReferenceFrom.TableNames[i], AFromTableName, False) and
         SSame(FTables[TableInd].Fields[FieldInd].ReferenceFrom.FieldNames[i], AFromFieldName, False) then
      begin
        Ind:= i;
        break;
      end;
    end;
    if Ind>=0 then
    begin
      VDel(FTables[TableInd].Fields[FieldInd].ReferenceFrom.TableNames, Ind);
      VDel(FTables[TableInd].Fields[FieldInd].ReferenceFrom.FieldNames, Ind);
    end;
  end;
end;

procedure TBaseScheme.ReferenceFromAdd(const ATableName, AFieldName,
                                             AFromTableName, AFromFieldName: String);
var
  TableInd, FieldInd: Integer;
begin
  if SEmpty(ATableName) or SEmpty(AFieldName) then Exit;

  if FindField(ATableName, AFieldName, TableInd, FieldInd) then
  begin
    VAppend(FTables[TableInd].Fields[FieldInd].ReferenceFrom.TableNames, AFromTableName);
    VAppend(FTables[TableInd].Fields[FieldInd].ReferenceFrom.FieldNames, AFromFieldName);
  end;
end;

procedure TBaseScheme.FieldInIndexDel(const AFieldName: String);
var
  i, n: Integer;
  V: TStrVector;
begin
  i:= 0;
  while i<= High(FTables[FTableSelectedIndex].Indexes) do
  begin
    V:= FTables[FTableSelectedIndex].Indexes[i].FieldNames;
    n:= VIndexOf(V, AFieldName);
    if n>=0 then //если поле есть в этом индексе
    begin
      VDel(V, n); //удаляем поле из индекса
      if VIsNil(V) then //если полей в индексе не осталось
        IndexesDel(FTables[FTableSelectedIndex].Indexes, i); //удаляем сам индекс
      continue;
    end;
    i:= i + 1;
  end;
end;

function TBaseScheme.GetTableSQL(const ATableIndex: Integer): TStrVector;
var
  i, len, PrimaryKeyIndex: Integer;
  MaxFieldNameLength, MaxFieldTypeLength: Integer;
  MaxFieldNameWithFKLength: Integer;
  MaxForeignTableNameLength, MaxForeignFieldNameLength: Integer;
  MaxForeignActionUpdLength: Integer;
  S: String;
const
  INDENT = '    ';
  INTERV = ' ';
  INSERT_OR_STR = 'IGNORE'; //'REPLACE'

  procedure CalcParameters;
  var
    k: Integer;
    Fld: TField;
  begin
    MaxFieldNameLength:= 0;         //макс длина имени поля
    MaxFieldTypeLength:= 0;         //макс длина типа поля
    MaxFieldNameWithFKLength:= 0;   //макс длина имени поля с внешним ключом
    MaxForeignTableNameLength:= 0;  //макс длина имени таблицы внешнего ключа
    MaxForeignFieldNameLength:= 0;  //макс длина имени поля внешнего ключа
    MaxForeignActionUpdLength:= 0;  //макс длина действия OnDel внешнего ключа
    PrimaryKeyIndex:= -1;           //индекс первичного ключа (если есть)

    for k:= 0 to High(FTables[ATableIndex].Fields) do
    begin
      Fld:= FTables[ATableIndex].Fields[k];

      len:= Length(Fld.FieldName);
      if len>MaxFieldNameLength then MaxFieldNameLength:= len;
      if (Fld.ReferenceTo.TableName<>EmptyStr) and
         (len>MaxFieldNameWithFKLength) then MaxFieldNameWithFKLength:= len;

      len:= Length(Fld.FieldType);
      if len>MaxFieldTypeLength then MaxFieldTypeLength:= len;

      len:= Length(Fld.ReferenceTo.TableName);
      if len>MaxForeignTableNameLength then MaxForeignTableNameLength:= len;

      len:= Length(Fld.ReferenceTo.FieldName);
      if len>MaxForeignFieldNameLength then MaxForeignFieldNameLength:= len;

      if Fld.ReferenceTo.TableName<>EmptyStr then
      begin
        len:= Length(ForeignKeyActionToStr(Fld.ReferenceTo.OnUpdate));
        if len>MaxForeignActionUpdLength then MaxForeignActionUpdLength:= len;
      end;

      if FTables[ATableIndex].Fields[k].PrimaryKey then
        PrimaryKeyIndex:= k;
    end;
  end;

  //формирование строки SQL для создания индекса
  function IndexSQL(const AIndex: TIndex): String;
  var
    Str: String;
  begin
    Result:= 'CREATE ';
    if AIndex.Unique then
      Result:= Result + 'UNIQUE ';
    Str:= SUpper(FTables[ATableIndex].TableName);
    Result:= Result + 'INDEX IF NOT EXISTS IDX_' + Str + '_' +
             SUpper(AIndex.IndexName) + ' ON ' + Str;
    Str:= VVectorToStr(AIndex.FieldNames, ', ');
    Result:= Result + '(' + Str + ')';
    if AIndex.Where<>EmptyStr then
      Result:= Result + ' WHERE (' + AIndex.Where + ')';
    Result:= Result + ';';
  end;

  //формирование строки SQL для создания поля
  function FieldSQL(const AField: TField): String;
  begin
    Result:= INDENT + SFillRight(AField.FieldName, MaxFieldNameLength) +
             INTERV + SFillRight(AField.FieldType, MaxFieldTypeLength);
    if AField.PrimaryKey then
      Result:= Result + INTERV + 'PRIMARY KEY';
    if AField.Status then
    begin
      if AField.PrimaryKey then
        Result:= Result + INTERV + 'AUTOINCREMENT'
      else
        Result:= Result + INTERV + 'UNIQUE';
    end;
    if AField.NotNull then
      Result:= Result + INTERV + 'NOT NULL';
    if AField.DefaultValue<>EmptyStr then
    begin
      Result:= Result + INTERV + 'DEFAULT ' +
               FieldValueToSQLString(AField.DefaultValue, AField.FieldType);
    end;

    Result:= Result + ',';
  end;

  function ForeignKeySQL(const AField: TField): String;
  begin
    Result:= EmptyStr;
    if AField.ReferenceTo.TableName=EmptyStr then Exit;
    Result:=
      INDENT + 'CONSTRAINT' +
      INTERV + 'FK_' + SUpper(FTables[ATableIndex].TableName) +
                 '_' + SFillRight(SUpper(AField.FieldName), MaxFieldNameWithFKLength) +
      INTERV + 'FOREIGN KEY' +
      INTERV + SFillRight('('+AField.FieldName+')', MaxFieldNameWithFKLength+2)+
      INTERV + 'REFERENCES' +
      INTERV + SFillRight(AField.ReferenceTo.TableName +
                         '('+AField.ReferenceTo.FieldName+')',
                         MaxForeignTableNameLength+
                         MaxForeignFieldNameLength+2) +
      INTERV + 'ON UPDATE' +
      INTERV + SFillRight(ForeignKeyActionToStr(AField.ReferenceTo.OnUpdate),
                          MaxForeignActionUpdLength) +
      INTERV + 'ON DELETE' +
      INTERV + ForeignKeyActionToStr(AField.ReferenceTo.OnDelete);
  end;

  procedure ExistingValues;
  var
    StrValue, StrValues, StrFields, StrBegin, StrMiddle, StrEnd: String;
    Fld: TField;
    k, n : Integer;
  begin
    if Length(FTables[ATableIndex].Fields)=0 then Exit;

    StrBegin:= 'INSERT OR ' + INSERT_OR_STR + ' INTO ' +
               INTERV + FTables[ATableIndex].TableName +
               INTERV + '(';
               // StrFields here
    StrMiddle:= ')' +
                INTERV + 'VALUES' +
                INTERV + '(';
               // StrValues here
    StrEnd:= ');';

    //пробегаем по всем строкам предзаписанных значений
    for k:= 0 to High(FTables[ATableIndex].Fields[0].ExistingValues) do
    begin
      StrFields:= EmptyStr;
      StrValues:= EmptyStr;
      //проебегаем по всем полям
      for n:= 0 to High(FTables[ATableIndex].Fields) do
      begin
        Fld:= FTables[ATableIndex].Fields[n];
        StrValue:= Fld.ExistingValues[k];
        if SEmpty(StrValue) then continue;
        StrValue:= FieldValueToSQLString(StrValue, Fld.FieldType);
        //список полей
        if SEmpty(StrFields) then
          StrFields:= Fld.FieldName
        else
          StrFields:= StrFields + ', ' + Fld.FieldName;
        //список значений
        if SEmpty(StrValues) then
          StrValues:= StrValue
        else
          StrValues:= StrValues + ', ' + StrValue;
      end;
      //SQL строка для записи
      VAppend(Result, StrBegin + StrFields + StrMiddle + StrValues + StrEnd);
    end;
  end;

begin
  Result:= nil;
  if (ATableIndex<0) or (ATableIndex>High(FTables)) then Exit;

  CalcParameters;

  //записываем описание таблицы
  if FTables[ATableIndex].Description<>EmptyStr then
  begin
    S:= '/* ' +  FTables[ATableIndex].Description + ' */';
    VAppend(Result, S);
  end;

  //записываем создание таблицы
  S:= 'CREATE TABLE IF NOT EXISTS ' + FTables[ATableIndex].TableName + ' (';
  VAppend(Result, S);

  //нет полей
  if Length(FTables[ATableIndex].Fields)=0 then
  begin
    // конец создания таблицы
    S:= ');';
    VAppend(Result, S);
    Exit;
  end;

  //записываем создание полей - если есть PK, то он идет первой строкой
  if PrimaryKeyIndex>=0 then
  begin
    S:= FieldSQL(FTables[ATableIndex].Fields[PrimaryKeyIndex]);
    VAppend(Result, S);
  end;
  for i:= 0 to High(FTables[ATableIndex].Fields) do
  begin
    if i<>PrimaryKeyIndex then
    begin
      S:= FieldSQL(FTables[ATableIndex].Fields[i]);
      VAppend(Result, S);
    end;
  end;
  // удаление запятой в конце последней строки создания полей
  Result[High(Result)]:= SCutRight(VLast(Result), 1);

  //записываем внешние ключи
  for i:= 0 to High(FTables[ATableIndex].Fields) do
  begin
    S:= ForeignKeySQL(FTables[ATableIndex].Fields[i]);
    if S<>EmptyStr then
    begin
      Result[High(Result)]:= VLast(Result) + ',';
      VAppend(Result, S);
    end;
  end;

  // конец создания таблицы
  S:= ');';
  VAppend(Result, S);

  //индексы
  for i:= 0 to High(FTables[ATableIndex].Indexes) do
  begin
    S:= IndexSQL(FTables[ATableIndex].Indexes[i]);
    VAppend(Result, S);
  end;

  //записываем существующие значения
  ExistingValues;
end;

procedure TBaseScheme.FieldAdd(const AField: TField);
begin
  if not IsTableSelected then Exit;

  //проверяем на дублирование имён
  if FieldsIndexOf(FTables[FTableSelectedIndex].Fields, AField.FieldName, False)>=0 then
    raise EDuplicateException.Create('В таблице уже есть поле "' +
                                     AField.FieldName + '"!');
  //записываем ReferenceFrom во внешней таблице
  ReferenceFromAdd(AField.ReferenceTo.TableName, AField.ReferenceTo.FieldName,
               ActiveTable.TableName, AField.FieldName);

  //записываем поле
  FieldsAppend(FTables[FTableSelectedIndex].Fields, AField);
end;

procedure TBaseScheme.FieldSet(const AField: TField);
var
  Ind: Integer;
begin
  if not IsFieldSelected then Exit;
  //проверяем на дублирование имён
  Ind:= FieldsIndexOf(FTables[FTableSelectedIndex].Fields, AField.FieldName, False);
  if (Ind<>FFieldSelectedIndex) and (Ind>=0) then
    raise EDuplicateException.Create('В таблице уже есть поле "' +
                                     AField.FieldName + '"!');

  if (not SSame(AField.FieldName, ActiveField.FieldName, False)) or //если изменилось имя поля
     (not IsSameReferenceTo(AField.ReferenceTo, ActiveField.ReferenceTo)) //или параметры внешнего ключа
  then
  begin
    //меняем ReferenceFrom во  внешней таблице
    //удаляем старую ссылку
    ReferenceFromDel(ActiveField.ReferenceTo.TableName,
                 ActiveField.ReferenceTo.FieldName,
                 ActiveTable.TableName,
                 ActiveField.FieldName);
    //добавляем новую ссылку
    ReferenceFromAdd(AField.ReferenceTo.TableName, AField.ReferenceTo.FieldName,
                 ActiveTable.TableName, AField.FieldName);
  end;

  //если изменилось имя поля
  if not SSame(AField.FieldName, ActiveField.FieldName, False) then
  begin
    //меняем ReferenceTo во всех полях всех таблиц, которые ссылаются на это поле
    ReferenceToUpdate(ActiveTable.TableName, AField.FieldName,
                      ActiveField.ReferenceFrom.TableNames,
                      ActiveField.ReferenceFrom.FieldNames);
  end;

  //меняем поле
  FTables[FTableSelectedIndex].Fields[FFieldSelectedIndex]:= FieldCopy(AField);
end;

procedure TBaseScheme.FieldDel;
begin
  if not IsFieldSelected then Exit;
  //удаляем ссылку от этого поля во внешней таблице (если есть)
  ReferenceFromDel(ActiveField.ReferenceTo.TableName,
               ActiveField.ReferenceTo.FieldName,
               ActiveTable.TableName,
               ActiveField.FieldName);
  //удаляем ссылки на это поле из остальных таблиц
  ReferenceToDel(ActiveField.ReferenceFrom.TableNames,
                 ActiveField.ReferenceFrom.FieldNames);
  //удаляем это поле из индексов этой таблицы
  FieldInIndexDel(ActiveField.FieldName);
  //удаляем поле
  FieldsDel(FTables[FTableSelectedIndex].Fields, FFieldSelectedIndex);

  Unselect;
end;

procedure TBaseScheme.FieldUnselect;
begin
  if not IsFieldSelected then Exit;
  //убираем отрисовку выделения старого поля
  FSheet.DrawFieldLine(FFieldSelectedIndex, False);
  FFieldSelectedIndex:= -1;
end;

procedure TBaseScheme.FieldSelect(const AFieldIndex: Integer);
begin
  if IsFieldsEmpty or (AFieldIndex<0) then Exit;
  FieldUnselect;
  FFieldSelectedIndex:= AFieldIndex;
  FSheet.DrawFieldLine(FFieldSelectedIndex, True);
end;

procedure TBaseScheme.FieldOrIndexSelect(const ARow: Integer);
var
  Ind: Integer;
begin
  if not IsFieldsEmpty then
  begin
    Ind:= FSheet.RowToFieldIndex(ARow);
    if Ind>=0 then //есть новое выделение поля
    begin
      IndexUnselect;
      FieldSelect(Ind);
      Exit;
    end;
  end;

  if not IsIndexesEmpty then
  begin
    Ind:= FSheet.RowToIndexIndex(ARow);
    if Ind>=0 then //есть новое выделение индекса
    begin
      FieldUnselect;
      IndexSelect(Ind);
    end;
  end;
end;

procedure TBaseScheme.Unselect;
begin
  if IsFieldSelected then
  begin
    FSheet.DrawFieldLine(FFieldSelectedIndex, False);
    FFieldSelectedIndex:= -1;
  end
  else if IsIndexSelected then
  begin
    FSheet.DrawIndexLine(FIndexSelectedIndex, False);
    FIndexSelectedIndex:= -1;
  end;
end;

procedure TBaseScheme.FieldMove(const ADirection: Integer);
var
  Ind: Integer;
begin
  Ind:= FFieldSelectedIndex + ADirection;
  FieldsSwap(FTables[FTableSelectedIndex].Fields, FFieldSelectedIndex, Ind);
  FieldSelect(Ind);
  //FieldOrIndexSelect(Ind);
end;

procedure TBaseScheme.FieldMoveUp;
begin
  FieldMove(-1);
end;

procedure TBaseScheme.FieldMoveDown;
begin
  FieldMove(1);
end;

function TBaseScheme.FieldCanUp: Boolean;
begin
  Result:= IsFieldSelected and (FFieldSelectedIndex>0);
end;

function TBaseScheme.FieldCanDown: Boolean;
begin
  Result:= IsFieldSelected and
           (FFieldSelectedIndex<High(FTables[FTableSelectedIndex].Fields));
end;

function TBaseScheme.FieldsList(const ATableName: String): TStrVector;
var
  Ind, i: Integer;
begin
  Result:= nil;
  if ATableName=EmptyStr then Exit;
  Ind:= VIndexOf(FTableNames, ATableName);
  if Ind<0 then Exit;
  for i:= 0 to High(FTables[Ind].Fields) do
    VAppend(Result, FTables[Ind].Fields[i].FieldName);
end;

procedure TBaseScheme.IndexAdd(const AIndex: TIndex);
begin
  if not IsTableSelected then Exit;

  //проверяем на дублирование имён
  if IndexesIndexOf(FTables[FTableSelectedIndex].Indexes, AIndex.IndexName, False)>=0 then
    raise EDuplicateException.Create('В таблице уже есть индекс "' +
                                     AIndex.IndexName + '"!');
  //записываем индекс
  IndexesAppend(FTables[FTableSelectedIndex].Indexes, AIndex);
end;

procedure TBaseScheme.IndexSet(const AIndex: TIndex);
begin
  FTables[FTableSelectedIndex].Indexes[FIndexSelectedIndex]:= IndexCopy(AIndex);
end;

procedure TBaseScheme.IndexDel;
begin
  if not IsIndexSelected then Exit;

  //удаляем индекс
  IndexesDel(FTables[FTableSelectedIndex].Indexes, FIndexSelectedIndex);

  Unselect;
end;

procedure TBaseScheme.IndexUnselect;
begin
  if not IsIndexSelected then Exit;
  FSheet.DrawIndexLine(FIndexSelectedIndex, False);
  FIndexSelectedIndex:= -1;
end;

procedure TBaseScheme.IndexSelect(const AIndex: Integer);
begin
  if IsIndexesEmpty or (AIndex<0) then Exit;
  IndexUnselect;
  FIndexSelectedIndex:= AIndex;
  FSheet.DrawIndexLine(FIndexSelectedIndex, True);
end;

function TBaseScheme.IsFieldSelected: Boolean;
begin
  Result:= IsTableSelected and (FFieldSelectedIndex>=0);
end;

function TBaseScheme.IsIndexSelected: Boolean;
begin
  Result:= IsTableSelected and (FIndexSelectedIndex>=0);
end;

function TBaseScheme.IsTablesEmpty: Boolean;
begin
  Result:= TablesIsNil(FTables);
end;

function TBaseScheme.IsFieldsEmpty: Boolean;
begin
  Result:= True;
  if not IsTableSelected then Exit;
  Result:= FieldsIsNil(GetActiveTable.Fields);
end;

function TBaseScheme.IsIndexesEmpty: Boolean;
begin
  Result:= True;
  if not IsTableSelected then Exit;
  Result:= IndexesIsNil(GetActiveTable.Indexes);
end;

procedure TBaseScheme.Zoom(const APercents: Integer);
begin
  FSheetZoomPercent:= APercents;
end;

procedure TBaseScheme.AtiveTableDraw;
var
  N: Integer;
begin
  if IsTablesEmpty then Exit;
  if Assigned(FSheet) then FreeAndNil(FSheet);
  N:= 0;
  if not FieldsIsNil(ActiveTable.Fields) then
    N:= Length(ActiveTable.Fields[0].ExistingValues);
  FSheet:= TSchemeSheet.Create(FGrid, N);
  FSheet.Zoom(FSheetZoomPercent);
  FSheet.Draw(ActiveTable, FFieldSelectedIndex, FIndexSelectedIndex);
end;

procedure TBaseScheme.Draw;
var
  N, Count, i: Integer;
begin
  if IsTablesEmpty then Exit;
  if Assigned(FSheet) then FreeAndNil(FSheet);
  N:= 0;
  for i:= 0 to High(FTables) do
  begin
    Count:= Length(FTables[i].Fields[0].ExistingValues);
    if Count>N then N:= Count;
  end;
  FSheet:= TSchemeSheet.Create(FGrid, N);
  FSheet.Zoom(FSheetZoomPercent);
  FSheet.Draw(FTables);
end;

function TBaseScheme.GetActiveTable: TTable;
begin
  Result:= FTables[FTableSelectedIndex];
end;

function TBaseScheme.GetActiveIndex: TIndex;
begin
  Result:= GetActiveTable.Indexes[FIndexSelectedIndex];
end;

function TBaseScheme.GetActiveField: TField;
begin
  Result:= GetActiveTable.Fields[FFieldSelectedIndex];
end;

constructor TBaseScheme.Create(const AViewGrid: TsWorksheetGrid);
begin
  inherited Create;
  FGrid:= AViewGrid;
  Clear;
  FSheetZoomPercent:= 100;
end;

destructor TBaseScheme.Destroy;
begin
  if Assigned(FSheet) then FreeAndNil(FSheet);
  inherited Destroy;
end;

end.

