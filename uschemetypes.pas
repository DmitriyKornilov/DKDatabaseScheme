unit USchemeTypes;

{$mode ObjFPC}{$H+}
{$Codepage UTF8}  //for normal use cyrillic symbols in json

interface

uses
  Classes, SysUtils, Graphics, Grids,
  fpjson, fpspreadsheetgrid, fpstypes, DateUtils,
  DK_Vector, DK_StrUtils, DK_SheetWriter, DK_Const;

const
  SCHEME_FONT_FILENAME     = 'JetBrains Mono Regular Nerd Font Complete.ttf';
  SCHEME_FONT_NAME_DEFAULT = 'JetBrainsMono Nerd Font';
  SCHEME_FONT_SIZE_DEFAULT = 14;

  TABLE_MAIN_LINE_STYLE = lsThin;

  TABLE_TITLE_FONT_SIZE = SCHEME_FONT_SIZE_DEFAULT-2;
  TABLE_TITLE_FONT_STYLE = [fsBold];

  TABLE_DESCRIPTION_FONT_SIZE = SCHEME_FONT_SIZE_DEFAULT-4;
  TABLE_DESCRIPTION_FONT_STYLE = [];

  TABLE_NOTES_FONT_SIZE = SCHEME_FONT_SIZE_DEFAULT-4;
  TABLE_NOTES_FONT_STYLE = [];

  FIELD_NAME_FONT_SIZE = SCHEME_FONT_SIZE_DEFAULT-6;
  FIELD_NAME_FONT_STYLE = [];

  FIELD_TYPE_FONT_SIZE = SCHEME_FONT_SIZE_DEFAULT-6;
  FIELD_TYPE_FONT_STYLE = [];

  FIELD_SYMBOLS_FONT_SIZE = SCHEME_FONT_SIZE_DEFAULT-3;
  FIELD_SYMBOLS_FONT_STYLE = [fsBold];

  FIELD_DEFVALUE_FONT_SIZE = SCHEME_FONT_SIZE_DEFAULT-6;
  FIELD_DEFVALUE_FONT_STYLE = [];

  FIELD_REFERENCE_FONT_SIZE = SCHEME_FONT_SIZE_DEFAULT-8;
  FIELD_REFERENCE_FONT_STYLE = [];

  FIELD_DESCRIPTION_FONT_SIZE = SCHEME_FONT_SIZE_DEFAULT-7;
  FIELD_DESCRIPTION_FONT_STYLE = [];

  COLOR_BACKGROUND = clWindow;
  //COLOR_BACKGROUND_SELECTED   = $00FBDEBB;
  COLOR_BACKGROUND_EXTRA      = $00D6E9D6;
  COLOR_TEXT = clWindowText;
  COLOR_TEXT_SELECTED = clBlack;
  COLOR_GRIDLINE = clWindowText;

  MIDDLE_ROW_HEIGHT = 15;

  SYMBOL_PRIMARYKEY = '';
  SYMBOL_AUTOINC = '﭅';
  SYMBOL_NULL = 'ﱤ';
  SYMBOL_NOTNULL = 'ﰸ';
  SYMBOL_UNIQUE = '';
  SYMBOL_REFERENCETO = '';
  SYMBOL_REFERENCEFROM = '';



type

  TForeignKeyAction = (fkaCascade,
                       fkaSetNull,
                       fkaSetDefault,
                       fkaRestrict,
                       fkaNoAction);
  function ForeignKeyActionToStr(const A: TForeignKeyAction): String;
  function ForeignKeyActionStrList: TStrVector;

type

  TReferenceTo = record
    TableName: String;
    FieldName: String;
    OnDelete:  TForeignKeyAction;
    OnUpdate:  TForeignKeyAction;
  end;
  procedure ReferenceToClear(var ARef: TReferenceTo);
  function IsSameReferenceTo(const ARef1, ARef2: TReferenceTo): Boolean;

type
  TReferenceFrom = record
    TableNames: TStrVector;
    FieldNames: TStrVector;
  end;
  procedure ReferenceFromClear(var ARef: TReferenceFrom);

type
  TField = record
    FieldName: String;
    FieldType: String;
    PrimaryKey: Boolean;
    Status: Boolean; //true - autoinc for primary key, unique value for other
    NotNull: Boolean;
    DefaultValue: String;
    ReferenceTo: TReferenceTo;     //reference to other table
    ReferenceFrom: TReferenceFrom; //references from other tables info
    Description: TStrVector;
    ExistingValues: TStrVector;
  end;

  TFields = array of TField;

  function FieldCreate(const AFieldName, AFieldType: String): TField;
  procedure FieldDescriptionSet(var AFieldInfo: TField;
                                    const ADescription: TStrVector);
  function FieldsIsNil(const V: TFields): Boolean;
  function FieldCopy(const Value: TField): TField;
  procedure FieldsAppend(var V: TFields; const Value: TField);
  procedure FieldsSwap(var V: TFields; const Index1, Index2: Integer);
  procedure FieldsDel(var V: TFields; const Index: Integer);
  function FieldsIndexOf(const V: TFields; const AName: String;
                         const ACaseSensitivity: Boolean = True): Integer;

  function FieldValueToSQLString(const AFieldValue, AFieldType: String): String;
  function FieldValueCheck(var AFieldValue: String; const AFieldType: String): Boolean;

type
  TIndex = record
    IndexName: String;
    Unique: Boolean;
    Where: String;
    FieldNames: TStrVector;
  end;

  TIndexes = array of TIndex;

  function IndexCreate(const AIndexName: String): TIndex;
  function IndexCopy(const AIndex: TIndex): TIndex;
  function IndexesIsNil(const V: TIndexes): Boolean;
  procedure IndexesAppend(var V: TIndexes; const Value: TIndex);
  procedure IndexesDel(var V: TIndexes; const Index: Integer);
  function IndexesIndexOf(const V: TIndexes; const AName: String;
                         const ACaseSensitivity: Boolean = True): Integer;

type
  TTable = record
    TableName: String;
    Description: String;
    Fields: TFields;
    Indexes: TIndexes;
    Notes: TStrVector;
  end;

  TTables = array of TTable;

  function TableCreate(const ATableName: String): TTable;
  function TablesIsNil(const V: TTables): Boolean;
  function TableCopy(const Value: TTable): TTable;
  procedure TablesIns(var V: TTables; const Index: Integer; const Value: TTable);
  procedure TablesAppend(var V: TTables; const Value: TTable);
  procedure TablesDel(var V: TTables; const Index: Integer);
  function TablesIndexOf(const V: TTables; const AName: String;
                         const ACaseSensitivity: Boolean = True): Integer;


  function TablesToJSON(const ATables: TTables): String;
  function JSONToTables(const AJSON: String): TTables;

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

type
  EDuplicateException = class (Exception);


implementation


function ForeignKeyActionToStr(const A: TForeignKeyAction): String;
begin
  case A of
  fkaCascade:    Result:= 'CASCADE';
  fkaSetNull:    Result:= 'SET NULL';
  fkaSetDefault: Result:= 'SET DEFAULT';
  fkaRestrict:   Result:= 'RESTRICT';
  fkaNoAction:   Result:= 'NO ACTION';
  end;
end;

function ForeignKeyActionStrList: TStrVector;
begin
  Result:= VCreateStr([
    'CASCADE',
    'SET NULL',
    'SET DEFAULT',
    'RESTRICT',
    'NO ACTION'
  ]);
end;

procedure ReferenceToClear(var ARef: TReferenceTo);
begin
  ARef.TableName:= EmptyStr;
  ARef.FieldName:= EmptyStr;
  ARef.OnDelete:=  fkaNoAction;
  ARef.OnUpdate:=  fkaNoAction;
end;

function IsSameReferenceTo(const ARef1, ARef2: TReferenceTo): Boolean;
begin
  Result:= SSame(ARef1.TableName, ARef2.TableName) and
           SSame(ARef1.FieldName, ARef2.FieldName) and
           (ARef1.OnDelete = ARef2.OnDelete) and
           (ARef1.OnUpdate = ARef2.OnUpdate);
end;

procedure ReferenceFromClear(var ARef: TReferenceFrom);
begin
  ARef.TableNames:= nil;
  ARef.FieldNames:= nil;
end;

function FieldCreate(const AFieldName, AFieldType: String): TField;
begin
  Result.FieldName    := AFieldName;
  Result.FieldType    := AFieldType;
  Result.PrimaryKey   := False;
  Result.Status       := False;
  Result.NotNull      := False;
  Result.DefaultValue := EmptyStr;
  ReferenceToClear(Result.ReferenceTo);
  ReferenceFromClear(Result.ReferenceFrom);
end;

procedure FieldDescriptionSet(var AFieldInfo: TField;
  const ADescription: TStrVector);
begin
  AFieldInfo.Description:= VCut(ADescription);
end;

function TableCreate(const ATableName: String): TTable;
begin
  Result.TableName:= ATableName;
  Result.Description:= EmptyStr;
  Result.Fields:= nil;
  Result.Indexes:= nil;
  Result.Notes:= nil;
end;

function FieldsIsNil(const V: TFields): Boolean;
begin
  Result:= Length(V)=0;
end;

function TablesIsNil(const V: TTables): Boolean;
begin
  Result:= Length(V)=0;
end;

function FieldCopy(const Value: TField): TField;
begin
  Result.FieldName    := Value.FieldName;
  Result.FieldType    := Value.FieldType;
  Result.PrimaryKey   := Value.PrimaryKey;
  Result.Status       := Value.Status;
  Result.NotNull      := Value.NotNull;
  Result.DefaultValue := Value.DefaultValue;
  Result.ReferenceTo:= Value.ReferenceTo;
  Result.ReferenceFrom.TableNames:= VCut(Value.ReferenceFrom.TableNames);
  Result.ReferenceFrom.FieldNames:= VCut(Value.ReferenceFrom.FieldNames);
  Result.Description:= VCut(Value.Description);
  Result.ExistingValues:= VCut(Value.ExistingValues);
end;

procedure FieldsAppend(var V: TFields; const Value: TField);
begin
  SetLength(V,Length(V)+1);
  V[High(V)]:= FieldCopy(Value);
end;

function TableCopy(const Value: TTable): TTable;
var
  i: Integer;
begin
  Result.TableName:= Value.TableName;
  Result.Description:= Value.Description;
  Result.Notes:= VCut(Value.Notes);
  Result.Indexes:= nil;
  for i:= 0 to High(Value.Indexes) do
    IndexesAppend(Result.Indexes, Value.Indexes[i]);
  Result.Fields:= nil;
  for i:= 0 to High(Value.Fields) do
    FieldsAppend(Result.Fields, Value.Fields[i]);
end;

function TablesCopy(const V: TTables): TTables;
var
  i: Integer;
begin
  Result:= nil;
  for i:= 0 to High(V) do
    TablesAppend(Result, V[i]);
end;

procedure TablesIns(var V: TTables; const Index: Integer; const Value: TTable);
var
  i, VectorSize: Integer;
begin
  VectorSize:= Length(V);

  if (Index>VectorSize) Or (Index<0) then Exit;

  if Index=VectorSize then
    TablesAppend(V, Value)
  else begin
    SetLength(V, VectorSize+1);
    for i:= VectorSize downto Index+1 do
      V[i]:= V[i-1];
    V[Index]:= TableCopy(Value);
  end;
end;

procedure TablesAppend(var V: TTables; const Value: TTable);
begin
  SetLength(V,Length(V)+1);
  V[High(V)]:= TableCopy(Value);
end;

procedure TablesDel(var V: TTables; const Index: Integer);
var
  i, VectorSize: Integer;
begin
  VectorSize:= Length(V);
  if VectorSize=0 then Exit;
  if (Index>VectorSize-1) Or (Index<0) then Exit;
  for i := Index+1 to VectorSize-1 do  V[i-1]:= V[i];
  SetLength(V, VectorSize-1);
end;

function TablesIndexOf(const V: TTables; const AName: String;
                      const ACaseSensitivity: Boolean = True): Integer;
var
  i: Integer;
begin
  Result:= -1;
  for i:= 0 to High(V) do
  begin
    if SSame(V[i].TableName, AName, ACaseSensitivity) then
    begin
      Result:= i;
      break;
    end;
  end;
end;

procedure FieldsSwap(var V: TFields; const Index1, Index2: Integer);
var
  TmpValue: TField;
begin
  if FieldsIsNil(V) then Exit;
  TmpValue:= V[Index1];
  V[Index1]:= V[Index2];
  V[Index2]:= TmpValue;
end;

procedure FieldsDel(var V: TFields; const Index: Integer);
var
  i, VectorSize: Integer;
begin
  VectorSize:= Length(V);
  if VectorSize=0 then Exit;
  if (Index>VectorSize-1) Or (Index<0) then Exit;
  for i := Index+1 to VectorSize-1 do  V[i-1]:= V[i];
  SetLength(V, VectorSize-1);
end;

function FieldsIndexOf(const V: TFields; const AName: String;
    const ACaseSensitivity: Boolean = True): Integer;
var
  i: Integer;
begin
  Result:= -1;
  for i:= 0 to High(V) do
  begin
    if SSame(V[i].FieldName, AName, ACaseSensitivity)  then
    begin
      Result:= i;
      break;
    end;
  end;
end;

function FieldValueCheck(var AFieldValue: String; const AFieldType: String): Boolean;
var
  i: Int64;
  d: TDateTime;
  f: Double;
begin
  Result:= True;
  case AFieldType of
  'INTEGER' : Result:= TryStrToInt64(AFieldValue, i);
  'DATETIME': if AFieldValue<>'0' then
                  Result:= TryISO8601ToDate(AFieldValue, d);
  'REAL'    : Result:= TryStrToFloat(AFieldValue, f);
  //'TEXT'  not need
  //'BLOB'  not need
  end;
end;

function FieldValueToSQLString(const AFieldValue, AFieldType: String): String;
begin
  case AFieldType of
  'INTEGER' : Result:= AFieldValue;
  'DATETIME': if SameStr(AFieldValue, '0') then
                Result:= '0'
              else
                Result:= FloatToStr(DateTimeToJulianDate(ISO8601ToDate(AFieldValue)));

  'TEXT'    : if SameStr(AFieldValue, 'EmptyStr') then
                Result:= QuotedStr(EmptyStr)
              else
                Result:= QuotedStr(AFieldValue);

  'REAL'    : Result:= AFieldValue;
  //'BLOB'  not need
  end;
end;


function IndexCreate(const AIndexName: String): TIndex;
begin
  Result.IndexName:= AIndexName;
  Result.Unique:= False;
  Result.Where:= EmptyStr;
  Result.FieldNames:= nil;
end;

function IndexCopy(const AIndex: TIndex): TIndex;
begin
  Result.IndexName:= AIndex.IndexName;
  Result.Unique:= AIndex.Unique;
  Result.Where:= AIndex.Where;
  Result.FieldNames:= VCut(AIndex.FieldNames);
end;

function IndexesIsNil(const V: TIndexes): Boolean;
begin
  Result:= Length(V)=0;
end;

procedure IndexesAppend(var V: TIndexes; const Value: TIndex);
begin
  SetLength(V,Length(V)+1);
  V[High(V)]:= IndexCopy(Value);
end;

procedure IndexesDel(var V: TIndexes; const Index: Integer);
var
  i, VectorSize: Integer;
begin
  VectorSize:= Length(V);
  if VectorSize=0 then Exit;
  if (Index>VectorSize-1) or (Index<0) then Exit;
  for i := Index+1 to VectorSize-1 do  V[i-1]:= V[i];
  SetLength(V, VectorSize-1);
end;

function IndexesIndexOf(const V: TIndexes; const AName: String;
                         const ACaseSensitivity: Boolean = True): Integer;
var
  i: Integer;
begin
  Result:= -1;
  for i:= 0 to High(V) do
  begin
    if SSame(V[i].IndexName, AName, ACaseSensitivity)  then
    begin
      Result:= i;
      break;
    end;
  end;
end;

function FieldToJSON(const AField: TField): TJSONObject;
var
  i: Integer;
  JArr: TJSONArray;
  JObj: TJSONObject;
begin
  Result:= TJSONObject.Create;
  //основные свойства
  Result.Add('FieldType', AField.FieldType);
  Result.Add('PrimaryKey', AField.PrimaryKey);
  Result.Add('Status', AField.Status);
  Result.Add('NotNull', AField.NotNull);
  Result.Add('DefaultValue', AField.DefaultValue);
  //Description
  JArr:= TJSONArray.Create;
  for i:= 0 to High(AField.Description) do
    JArr.Add(AField.Description[i]);
  Result.Add('Description', JArr);
  //ExistingValues
  JArr:= TJSONArray.Create;
  for i:= 0 to High(AField.ExistingValues) do
    JArr.Add(AField.ExistingValues[i]);
  Result.Add('ExistingValues', JArr);
  //ReferenceTo
  JObj:= TJSONObject.Create;
  JObj.Add('TableName', AField.ReferenceTo.TableName);
  JObj.Add('FieldName', AField.ReferenceTo.FieldName);
  JObj.Add('OnDelete', Ord(AField.ReferenceTo.OnDelete));
  JObj.Add('OnUpdate', Ord(AField.ReferenceTo.OnUpdate));
  Result.Add('ReferenceTo', JObj);
  //ReferenceFrom
  JObj:= TJSONObject.Create;
  JArr:= TJSONArray.Create;
  for i:= 0 to High(AField.ReferenceFrom.TableNames) do
    JArr.Add(AField.ReferenceFrom.TableNames[i]);
  JObj.Add('TableNames', JArr);
  JArr:= TJSONArray.Create;
  for i:= 0 to High(AField.ReferenceFrom.FieldNames) do
    JArr.Add(AField.ReferenceFrom.FieldNames[i]);
  JObj.Add('FieldNames', JArr);
  Result.Add('ReferenceFrom', JObj);
end;

function IndexToJSON(const AIndex: TIndex): TJSONObject;
var
  i: Integer;
  JArr: TJSONArray;
begin
  Result:= TJSONObject.Create;
  Result.Add('Unique', AIndex.Unique);
  Result.Add('Where', AIndex.Where);

  //FieldNames
  JArr:= TJSONArray.Create;
  for i:= 0 to High(AIndex.FieldNames) do
    JArr.Add(AIndex.FieldNames[i]);
  Result.Add('IndexFieldNames', JArr);
end;

function TableToJSON(const ATable: TTable): TJSONObject;
var
  i: Integer;
  JArr: TJSONArray;
  JObj: TJSONObject;
begin
  Result:= TJSONObject.Create;
  //основные свойства
  Result.Add('Description', ATable.Description);
  JArr:= TJSONArray.Create;
  for i:= 0 to High(ATable.Notes) do
    JArr.Add(ATable.Notes[i]);
  Result.Add('Notes', JArr);

  //список полей
  JArr:= TJSONArray.Create;
  for i:= 0 to High(ATable.Fields) do
    JArr.Add(ATable.Fields[i].FieldName);
  Result.Add('FieldsList', JArr);
  //пробегаем по всем полям
  for i:= 0 to High(ATable.Fields) do
  begin
    //создаем json объект для поля
    JObj:= FieldToJSON(ATable.Fields[i]);
    //запись поля в json таблицы
    Result.Add(ATable.Fields[i].FieldName, JObj);
  end;

  //список индексов
  JArr:= TJSONArray.Create;
  for i:= 0 to High(ATable.Indexes) do
    JArr.Add(ATable.Indexes[i].IndexName);
  Result.Add('IndexesList', JArr);
  //пробегаем по всем индексам
  for i:= 0 to High(ATable.Indexes) do
  begin
    //создаем json объект для поля
    JObj:= IndexToJSON(ATable.Indexes[i]);
    //запись поля в json таблицы
    Result.Add(ATable.Indexes[i].IndexName, JObj);
  end;
end;

function TablesToJSON(const ATables: TTables): String;
var
  i: Integer;
  JMainObj, JTableObj: TJSONObject;
  JArr: TJSONArray;
begin
  Result:= EmptyStr;
  JMainObj:= TJSONObject.Create;
  try
    //список таблиц в json
    JArr:= TJSONArray.Create;
    for i:= 0 to High(ATables) do
      JArr.Add(ATables[i].TableName);
    JMainObj.Add('TablesList', JArr);

    //пробегаем по всем таблицам
    for i:= 0 to High(ATables) do
    begin
      //создаем json объект для таблицы
      JTableObj:= TableToJSON(ATables[i]);
      //запись таблицы в json базы
      JMainObj.Add(ATables[i].TableName, JTableObj);
    end;

    Result:= JMainObj.FormatJSON;

  finally
    FreeAndNil(JMainObj);
  end;
end;

procedure JSONToField(var AField: TField; const AFieldObj: TJSONObject);
var
  i, x: Integer;
  JArr: TJSONArray;
  JObj: TJSONObject;
begin
  AField.FieldType:= AFieldObj.Find('FieldType').AsString;
  AField.PrimaryKey:= AFieldObj.Find('PrimaryKey').AsBoolean;
  AField.Status:= AFieldObj.Find('Status').AsBoolean;
  AField.NotNull:= AFieldObj.Find('NotNull').AsBoolean;
  AField.DefaultValue:= AFieldObj.Find('DefaultValue').AsString;
  JArr:= AFieldObj.Find('Description') as TJSONArray;
  for i:= 0 to JArr.Count-1 do
    VAppend(AField.Description, JArr[i].AsString);
  JArr:= AFieldObj.Find('ExistingValues') as TJSONArray;
  for i:= 0 to JArr.Count-1 do
    VAppend(AField.ExistingValues, JArr[i].AsString);
  JObj:= AFieldObj.Find('ReferenceTo') as TJSONObject;
  AField.ReferenceTo.TableName:= JObj.Find('TableName').AsString;
  AField.ReferenceTo.FieldName:= JObj.Find('FieldName').AsString;
  x:= JObj.Find('OnDelete').AsInteger;
  AField.ReferenceTo.OnDelete:= TForeignKeyAction(x);
  x:= JObj.Find('OnUpdate').AsInteger;
  AField.ReferenceTo.OnUpdate:= TForeignKeyAction(x);
  JObj:= AFieldObj.Find('ReferenceFrom') as TJSONObject;
  JArr:= JObj.Find('TableNames') as TJSONArray;
  for i:= 0 to JArr.Count-1 do
    VAppend(AField.ReferenceFrom.TableNames, JArr[i].AsString);
  JArr:= JObj.Find('FieldNames') as TJSONArray;
  for i:= 0 to JArr.Count-1 do
    VAppend(AField.ReferenceFrom.FieldNames, JArr[i].AsString);
end;

procedure JSONToIndex(var AIndex: TIndex; const AIndexObj: TJSONObject);
var
  i: Integer;
  JArr: TJSONArray;
begin
  AIndex.Unique:= AIndexObj.Find('Unique').AsBoolean;
  AIndex.Where:= AIndexObj.Find('Where').AsString;
  JArr:= AIndexObj.Find('IndexFieldNames') as TJSONArray;
  for i:= 0 to JArr.Count-1 do
    VAppend(AIndex.FieldNames, JArr[i].AsString);
end;

procedure JSONToTable(var ATable: TTable; const ATableObj: TJSONObject);
var
  i: Integer;
  Field: TField;
  Index: TIndex;
  JArr: TJSONArray;
  JObj: TJSONObject;
begin
  //основные свойства
  ATable.Description:= ATableObj.Find('Description').AsString;
  JArr:= ATableObj.Find('Notes') As TJSONArray;
  for i:= 0 to JArr.Count-1 do
    VAppend(ATable.Notes, JArr[i].AsString);
  //создаем вектор полей таблицы на основе списка полей
  JArr:= ATableObj.Find('FieldsList') As TJSONArray;
  for i:= 0 to JArr.Count-1 do
  begin
    Field:= FieldCreate(JArr[i].AsString, EmptyStr);
    FieldsAppend(ATable.Fields, Field);
  end;
  //пробегаем по каждому полю и заполняем его свойства
  for i:= 0 to High(ATable.Fields) do
  begin
    //получаем json объект поля
    JObj:= ATableObj.Find(ATable.Fields[i].FieldName) As TJSONObject;
    //формируем запись для поля
    JSONToField(ATable.Fields[i], JObj);
  end;

  //создаем вектор индексов таблицы на основе списка индексов
  JArr:= ATableObj.Find('IndexesList') As TJSONArray;
  for i:= 0 to JArr.Count-1 do
  begin
    Index:= IndexCreate(JArr[i].AsString);
    IndexesAppend(ATable.Indexes, Index);
  end;
  //пробегаем по каждому индексу и заполняем его свойства
  for i:= 0 to High(ATable.Indexes) do
  begin
    //получаем json объект индекса
    JObj:= ATableObj.Find(ATable.Indexes[i].IndexName) As TJSONObject;
    //формируем запись для индекса
    JSONToIndex(ATable.Indexes[i], JObj);
  end;

end;

function JSONToTables(const AJSON: String): TTables;
var
  i: Integer;
  JMainObj, JObj: TJSONObject;
  JArr: TJSONArray;
  TableInfo: TTable;
begin
  Result:= nil;
  JMainObj:= GetJSON(AJSON) As TJSONObject;
  try
    //берем список таблиц и на его основе создаем вектор таблиц
    JArr:= JMainObj.Find('TablesList') As TJSONArray;
    for i:= 0 to JArr.Count-1 do
    begin
      TableInfo:= TableCreate(JArr[i].AsString);
      TablesAppend(Result, TableInfo);
    end;

    //пробегаем по каждой таблице
    for i:= 0 to High(Result) do
    begin
      //получаем json объект таблицы
      JObj:= JMainObj.Find(Result[i].TableName) As TJSONObject;
      //формируем запись для таблицы
      JSONToTable(Result[i], JObj);
    end;

  finally
    FreeAndNil(JMainObj);
  end;
end;

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
  Ind:= VInsOfAsc(FTableNames, ATable.TableName, False);
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

procedure TBaseScheme.ReferenceFromDel(const
    ATableName, AFieldName,
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
  if (ATableName=EmptyStr) or (AFieldName=EmptyStr) then Exit;

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

  ////если изменилось имя поля
  //if not SSame(AField.FieldName, ActiveField.FieldName, False) then
  //begin
  //  //меняем ReferenceFrom во  внешней таблице
  //  //удаляем старую ссылку
  //  ReferenceFromDel(ActiveField.ReferenceTo.TableName,
  //               ActiveField.ReferenceTo.FieldName,
  //               ActiveTable.TableName,
  //               ActiveField.FieldName);
  //  //добавляем новую ссылку
  //  ReferenceFromAdd(AField.ReferenceTo.TableName, AField.ReferenceTo.FieldName,
  //               ActiveTable.TableName, AField.FieldName);
  //
  //  //меняем ReferenceTo во всех полях всех таблиц, которые ссылаются на это поле
  //  ReferenceToUpdate(ActiveTable.TableName, AField.FieldName,
  //                    ActiveField.ReferenceFrom.TableNames,
  //                    ActiveField.ReferenceFrom.FieldNames);
  //end;

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

  ColWidths:= VCreateInt([
    30,  // Левый отступ
    200, // Наименование поля
    100,  // Тип поля
    30,  // PK
    30,  // Autoinc/Unique
    30,  // Null/NotNull
    120,  // Default value
    30,  // Стрелки ссылок
    200, // Cсылки
    400  // Описание полей
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
  //FWriter.SetBackgroundClear;
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

procedure TSchemeSheet.DrawIndexLine(const AIndex: Integer;
  const ASelected: Boolean);
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
  //R:= FEndRows[High(FEndRows)] + 2 + AIndex;
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
  if S=EmptyStr then FWriter.SetRowHeight(R+1, 0);


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
  //FWriter.SetRowHeight(R1, MIDDLE_ROW_HEIGHT);


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
  FWriter.SetRowHeight(FRow, MIDDLE_ROW_HEIGHT);

  FFirstRow:= AFirstRow;
  FLastRow:= FRow;
  AFirstRow:= FLastRow;
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

