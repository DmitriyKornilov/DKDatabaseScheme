unit UTypes;

{$mode ObjFPC}{$H+}
{$Codepage UTF8}  //for normal use cyrillic symbols in json

interface

uses
  Classes, SysUtils, Graphics, fpjson, fpstypes, DateUtils,
  //DK packages utils
  DK_Vector, DK_StrUtils, DK_VSTTypes;

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

  function FieldTypeToColumnType(const AFieldType: String): TVSTColumnType;

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

procedure FieldDescriptionSet(var AFieldInfo: TField; const ADescription: TStrVector);
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

function FieldTypeToColumnType(const AFieldType: String): TVSTColumnType;
begin
  Result:= ctUndefined;
  case AFieldType of
    'INTEGER' : Result:= ctInteger;
    'DATETIME': Result:= ctDateTime;
    'TEXT'    : Result:= ctString;
    'REAL'    : Result:= ctDouble;
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

end.

