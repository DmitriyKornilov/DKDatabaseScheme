unit UMainForm;

{$mode objfpc}{$H+}

//{$DEFINE DEBUG}

interface

uses
  Classes, SysUtils, Forms, Controls, Graphics, Dialogs, StdCtrls, ExtCtrls,
  ComCtrls, fpspreadsheetgrid, VirtualTrees, fpstypes, DividerBevel,
  SynEdit, SynHighlighterSQL, Buttons,
  //DK packages utils
  {$IFDEF DEBUG}
  DK_HeapTrace,
  {$ENDIF}
  DK_LCLStrRus, DK_Vector, DK_VSTTableTools, DK_Zoom, DK_Fonts, DK_MsgDialogs,
  DK_SheetExporter, DK_CtrlUtils,
  //Project utils
  UTypes, UConst, UScheme,
  //Forms
  UTableEditForm, UFieldEditForm, UValuesEditForm, UIndexEditForm, UAboutForm;

type

  { TMainForm }

  TMainForm = class(TForm)
    BaseNewButton: TSpeedButton;
    AboutButton: TSpeedButton;
    BaseOpenButton: TSpeedButton;
    BaseSaveAsButton: TSpeedButton;
    BaseSaveButton: TSpeedButton;
    Bevel1: TBevel;
    CheckBox1: TCheckBox;
    CheckBox2: TCheckBox;
    DividerBevel1: TDividerBevel;
    DividerBevel2: TDividerBevel;
    DividerBevel3: TDividerBevel;
    DividerBevel4: TDividerBevel;
    DividerBevel5: TDividerBevel;
    ExportSpreadsheetButton: TSpeedButton;
    ExportSQLScriptButton: TSpeedButton;
    FieldAddButton: TSpeedButton;
    FieldDeleteButton: TSpeedButton;
    FieldDownButton: TSpeedButton;
    FieldEditButton: TSpeedButton;
    FieldUpButton: TSpeedButton;
    FieldValuesButton: TSpeedButton;
    ImageList1: TImageList;
    IndexAddButton: TSpeedButton;
    IndexDeleteButton: TSpeedButton;
    IndexEditButton: TSpeedButton;
    Panel3: TPanel;
    LeftPanel: TPanel;
    SaveDialog2: TSaveDialog;
    SchemeGrid: TsWorksheetGrid;
    Splitter1: TSplitter;
    SQLSynEdit: TSynEdit;
    StatusBar1: TStatusBar;
    SynSQLSyn1: TSynSQLSyn;
    Splitter2: TSplitter;
    TableAddButton: TSpeedButton;
    OpenDialog1: TOpenDialog;
    MainPanel: TPanel;
    TableDeleteButton: TSpeedButton;
    TableEditButton: TSpeedButton;
    ToolPanel: TPanel;
    SaveDialog1: TSaveDialog;
    VT1: TVirtualStringTree;
    ZoomPanel: TPanel;

    procedure AboutButtonClick(Sender: TObject);
    procedure BaseNewButtonClick(Sender: TObject);
    procedure CheckBox1Change(Sender: TObject);
    procedure CheckBox2Change(Sender: TObject);
    procedure ExportSpreadsheetButtonClick(Sender: TObject);
    procedure ExportSQLScriptButtonClick(Sender: TObject);
    procedure FieldAddButtonClick(Sender: TObject);
    procedure FieldDeleteButtonClick(Sender: TObject);
    procedure FieldDownButtonClick(Sender: TObject);
    procedure FieldEditButtonClick(Sender: TObject);
    procedure FieldUpButtonClick(Sender: TObject);
    procedure FieldValuesButtonClick(Sender: TObject);
    procedure FormCloseQuery(Sender: TObject; var CanClose: Boolean);
    procedure FormShow(Sender: TObject);
    procedure IndexAddButtonClick(Sender: TObject);
    procedure IndexDeleteButtonClick(Sender: TObject);
    procedure IndexEditButtonClick(Sender: TObject);
    procedure SchemeGridMouseDown(Sender: TObject; Button: TMouseButton;
      {%H-}Shift: TShiftState; X, Y: Integer);
    procedure TableDeleteButtonClick(Sender: TObject);
    procedure TableEditButtonClick(Sender: TObject);
    procedure TableAddButtonClick(Sender: TObject);
    procedure BaseOpenButtonClick(Sender: TObject);
    procedure BaseSaveButtonClick(Sender: TObject);
    procedure BaseSaveAsButtonClick(Sender: TObject);
    procedure FormCreate(Sender: TObject);
    procedure FormDestroy(Sender: TObject);
  private
    BaseScheme: TBaseScheme;
    IsNewScheme: Boolean;
    IsSchemeChanged: Boolean;

    TableList: TVSTStringList;
    ZoomPercent: Integer;

    procedure SetSchemeChanged(const AIsChanged: Boolean);
    procedure SchemeDraw(const AZoomPercent: Integer);

    procedure SetFieldAndIndexButtonsEnabled;
    procedure SetTableButtonsEnabled;
    procedure SetFileButtonsEnabled;

    procedure BaseNew;
    procedure BaseOpen;
    procedure BaseWrite;
    procedure BaseSave(const AFileName: String);
    procedure BaseSaveAs;
    procedure BaseClose;

    procedure TableEdit(const AEditMode: Byte); //1-new, 2-edit
    procedure TableDelete;
    procedure TableSelect;

    procedure FieldEdit(const AEditMode: Byte); //1-new, 2-edit, 3-new from old
    procedure FieldDelete;

    procedure IndexEdit(const AEditMode: Byte); //1-new, 2-edit
    procedure IndexDelete;

    procedure ValuesEdit;
    procedure TableListSet;

    procedure ExportSpreadsheet;
    procedure ExportSQLScript;

  end;

var
  MainForm: TMainForm;

implementation

{$R *.lfm}

{ TMainForm }

procedure TMainForm.FormCreate(Sender: TObject);
var
  AFontPath: String;
begin
  {$IFDEF DEBUG}
  HeapTraceOutputFile('trace.trc');
  {$ENDIF}

  AFontPath:= ExtractFilePath(Application.ExeName) + 'font' + DirectorySeparator;
  LoadExtraFont(AFontPath, SCHEME_FONT_FILENAME);
  SQLSynEdit.Font.Name:= SCHEME_FONT_NAME_DEFAULT;
  SQLSynEdit.Font.Size:= FIELD_NAME_FONT_SIZE;

  ZoomPercent:= 100;
  CreateZoomControls(50, 150, ZoomPercent, ZoomPanel, @SchemeDraw, True);
  BaseScheme:= TBaseScheme.Create(SchemeGrid);
  TableList:= TVSTStringList.Create(VT1, EmptyStr, @TableSelect);
  TableList.ValuesFont.Name:= SCHEME_FONT_NAME_DEFAULT;
  TableList.ValuesFont.Size:= FIELD_NAME_FONT_SIZE;
  TableList.SelectedFont.Name:= SCHEME_FONT_NAME_DEFAULT;
  TableList.SelectedFont.Size:= FIELD_NAME_FONT_SIZE;
  IsNewScheme:= True;
  SetSchemeChanged(False);
end;

procedure TMainForm.FormDestroy(Sender: TObject);
var
  AFontPath: String;
begin
  if Assigned(BaseScheme) then FreeAndNil(BaseScheme);
  if Assigned(TableList) then FreeAndNil(TableList);

  AFontPath:= ExtractFilePath(Application.ExeName) + 'font' + DirectorySeparator;
  UnloadExtraFont(AFontPath, SCHEME_FONT_FILENAME);
end;

procedure TMainForm.FormShow(Sender: TObject);
begin
  SQLSynEdit.Height:= (MainPanel.Height - Splitter1.Height) div 3;
  SetControlHeight([ToolPanel], 40);
  SetControlWidth([
    BaseNewButton, BaseOpenButton, BaseSaveButton, BaseSaveAsButton,
    ExportSpreadsheetButton, ExportSQLScriptButton,
    TableAddButton, TableEditButton, TableDeleteButton,
    FieldAddButton, FieldEditButton, FieldDeleteButton,
    FieldDownButton, FieldUpButton, FieldValuesButton,
    IndexAddButton, IndexEditButton, IndexDeleteButton,
    AboutButton
  ], 36);
  SchemeGrid.SetFocus;
end;

procedure TMainForm.TableAddButtonClick(Sender: TObject);
begin
  TableEdit(1);
end;

procedure TMainForm.TableEditButtonClick(Sender: TObject);
begin
  TableEdit(2);
end;

procedure TMainForm.TableDeleteButtonClick(Sender: TObject);
begin
  TableDelete;
end;

procedure TMainForm.FieldAddButtonClick(Sender: TObject);
begin
  if BaseScheme.IsFieldSelected then
    FieldEdit(3)
  else
    FieldEdit(1);
end;

procedure TMainForm.BaseNewButtonClick(Sender: TObject);
begin
  BaseClose;
  BaseNew;
end;

procedure TMainForm.AboutButtonClick(Sender: TObject);
begin
  FormModalShow(TAboutForm);
end;

procedure TMainForm.CheckBox1Change(Sender: TObject);
begin
  if CheckBox1.Checked then
  begin
    Splitter2.Visible:= False;
    LeftPanel.Visible:= False;
    MainPanel.BorderSpacing.Left:= 6;
    TableAddButton.Enabled:= False;
    TableEditButton.Enabled:= False;
    TableDeleteButton.Enabled:= False;
    FieldAddButton.Enabled:= False;

    FieldDeleteButton.Enabled:= False;
    FieldEditButton.Enabled:= False;
    FieldUpButton.Enabled:= False;
    FieldDownButton.Enabled:= False;
    FieldValuesButton.Enabled:= False;
  end
  else begin
    MainPanel.BorderSpacing.Left:= 0;
    MainPanel.Align:= alRight;
    Splitter2.Align:= alRight;
    LeftPanel.Visible:= True;
    Splitter2.Align:= alLeft;
    Splitter2.Visible:= True;
    MainPanel.Align:= alClient;

    TableAddButton.Enabled:= True;
    SetFieldAndIndexButtonsEnabled;
    SetTableButtonsEnabled;
  end;

  SchemeDraw(ZoomPercent);
end;

procedure TMainForm.CheckBox2Change(Sender: TObject);
begin
  SQLSynEdit.Visible:= CheckBox2.Checked;
  Splitter1.Visible:= SQLSynEdit.Visible;
end;

procedure TMainForm.ExportSpreadsheetButtonClick(Sender: TObject);
begin
  ExportSpreadsheet;
end;

procedure TMainForm.ExportSQLScriptButtonClick(Sender: TObject);
begin
  ExportSQLScript;
end;

procedure TMainForm.FieldEditButtonClick(Sender: TObject);
begin
  FieldEdit(2);
end;

procedure TMainForm.TableSelect;
begin
  BaseScheme.TableSelect(TableList.SelectedIndex);
  SchemeDraw(ZoomPercent);
  SetFieldAndIndexButtonsEnabled;
end;

procedure TMainForm.FieldDeleteButtonClick(Sender: TObject);
begin
  FieldDelete;
end;

procedure TMainForm.FieldDownButtonClick(Sender: TObject);
begin
  BaseScheme.FieldMoveDown;
  SchemeDraw(ZoomPercent);
  SetSchemeChanged(True);
  SetFieldAndIndexButtonsEnabled;
end;

procedure TMainForm.FieldUpButtonClick(Sender: TObject);
begin
  BaseScheme.FieldMoveUp;
  SchemeDraw(ZoomPercent);
  SetSchemeChanged(True);
  SetFieldAndIndexButtonsEnabled;
end;

procedure TMainForm.FieldValuesButtonClick(Sender: TObject);
begin
  ValuesEdit;
end;

procedure TMainForm.FormCloseQuery(Sender: TObject; var CanClose: Boolean);
begin
  BaseClose;
  CanClose:= True;
end;

procedure TMainForm.BaseClose;
begin
  if not IsSchemeChanged then Exit;
  if MsgConfirm('В схеме ' + Statusbar1.Panels[1].Text +
             ' есть несохраненные изменения! Сохранить?') then BaseWrite;
end;

procedure TMainForm.IndexAddButtonClick(Sender: TObject);
begin
  IndexEdit(1);
end;

procedure TMainForm.IndexDeleteButtonClick(Sender: TObject);
begin
  IndexDelete;
end;

procedure TMainForm.IndexEditButtonClick(Sender: TObject);
begin
  IndexEdit(2);
end;

procedure TMainForm.SchemeGridMouseDown(Sender: TObject; Button: TMouseButton;
  Shift: TShiftState; X, Y: Integer);
var
  R,C: Integer;
begin
  if Checkbox1.Checked then Exit;
  if Button=mbRight then
    BaseScheme.Unselect;
  if Button=mbLeft  then
  begin
    (Sender As TsWorksheetGrid).MouseToCell(X,Y,C{%H-},R{%H-});
    BaseScheme.FieldOrIndexSelect(R);
  end;
  SetFieldAndIndexButtonsEnabled;
end;

procedure TMainForm.BaseOpenButtonClick(Sender: TObject);
begin
  BaseClose;
  BaseOpen;
end;

procedure TMainForm.BaseSaveButtonClick(Sender: TObject);
begin
  BaseSave(StatusBar1.Panels[1].Text);
end;

procedure TMainForm.BaseSaveAsButtonClick(Sender: TObject);
begin
  BaseSaveAs;
end;

procedure TMainForm.SetSchemeChanged(const AIsChanged: Boolean);
begin
  IsSchemeChanged:= AIsChanged;
  if IsSchemeChanged then
    StatusBar1.Panels[0].Text:= '[*]'
  else
    StatusBar1.Panels[0].Text:= EmptyStr;
  SetFileButtonsEnabled;
end;

procedure TMainForm.SetFieldAndIndexButtonsEnabled;
begin
  FieldDeleteButton.Enabled:= BaseScheme.IsFieldSelected;
  FieldEditButton.Enabled:= FieldDeleteButton.Enabled;
  FieldUpButton.Enabled:= BaseScheme.FieldCanUp;
  FieldDownButton.Enabled:= BaseScheme.FieldCanDown;
  FieldValuesButton.Enabled:= not BaseScheme.IsFieldsEmpty;

  IndexDeleteButton.Enabled:= BaseScheme.IsIndexSelected;
  IndexEditButton.Enabled:= IndexDeleteButton.Enabled;

  IndexAddButton.Enabled:= FieldValuesButton.Enabled;
end;

procedure TMainForm.SetTableButtonsEnabled;
begin
  TableEditButton.Enabled:= not BaseScheme.IsTablesEmpty;
  TableDeleteButton.Enabled:= TableEditButton.Enabled;
  FieldAddButton.Enabled:= TableEditButton.Enabled;
  ExportSpreadsheetButton.Enabled:= TableEditButton.Enabled;
  ExportSQLScriptButton.Enabled:= TableEditButton.Enabled;
end;

procedure TMainForm.BaseOpen;
var
  SL: TStringList;
begin
  if not OpenDialog1.Execute then Exit;

  SL:= TStringList.Create;
  try
    SL.LoadFromFile(OpenDialog1.FileName);
    BaseScheme.ReadFromJSON(SL.Text);
    TableListSet;
    StatusBar1.Panels[1].Text:= OpenDialog1.FileName;
    IsNewScheme:= False;
    SetSchemeChanged(False);
  finally
    FreeAndNil(SL);
  end;
end;

procedure TMainForm.BaseWrite;
var
  FileName: String;
begin
  FileName:= StatusBar1.Panels[1].Text;
  if FileName=EmptyStr then
    BaseSaveAs
  else
    BaseSave(FileName);
end;

procedure TMainForm.BaseSave(const AFileName: String);
var
  SL: TStringList;
begin
  SL:= TStringList.Create;
  try
    SL.Add(BaseScheme.WriteToJSON);
    SL.SaveToFile(AFileName);
    StatusBar1.Panels[1].Text:= AFileName;
    IsNewScheme:= False;
    SetSchemeChanged(False);
  finally
    FreeAndNil(SL);
  end;
end;

procedure TMainForm.BaseSaveAs;
begin
  if not SaveDialog1.Execute then Exit;
  BaseSave(SaveDialog1.FileName);
end;

procedure TMainForm.TableListSet;
begin
  SchemeGrid.Clear;
  SQLSynEdit.Lines.Clear;
  TableList.Update(BaseScheme.TableNames, BaseScheme.TableSelectedIndex);
  SetTableButtonsEnabled;
end;

procedure TMainForm.ExportSQLScript;
begin
  if not SaveDialog2.Execute then Exit;
  SQLSynEdit.Lines.SaveToFile(SaveDialog2.FileName);
end;

procedure TMainForm.ExportSpreadsheet;
var
  Exporter: TGridExporter;
begin
  Exporter:= TGridExporter.Create(SchemeGrid);
  try
    Exporter.PageSettings(spoPortrait, pfWidth);
    Exporter.Save('Выполнено!');
  finally
    FreeAndNil(Exporter);
  end;
end;

procedure TMainForm.SetFileButtonsEnabled;
begin
  BaseSaveAsButton.Enabled:= not BaseScheme.IsTablesEmpty;
  BaseSaveButton.Enabled:= (not IsNewScheme) and  IsSchemeChanged and
                           (not BaseScheme.IsTablesEmpty);
end;

procedure TMainForm.BaseNew;
begin
  SchemeGrid.Clear;
  SQLSynEdit.Lines.Clear;
  BaseScheme.Clear;
  TableList.ValuesClear;
  IsNewScheme:= True;
  SetSchemeChanged(False);
  SetFieldAndIndexButtonsEnabled;
  SetTableButtonsEnabled;
end;

procedure TMainForm.SchemeDraw(const AZoomPercent: Integer);
begin
  SchemeGrid.Clear;
  SQLSynEdit.Lines.Clear;

  ZoomPercent:= AZoomPercent;
  BaseScheme.Zoom(ZoomPercent);

  if Checkbox1.Checked then
  begin
    if not BaseScheme.IsTablesEmpty then
    begin
      BaseScheme.Draw;
      VToStrings(BaseScheme.SQL, SQLSynEdit.Lines);
    end;
  end
  else begin
    if BaseScheme.IsTableSelected then
    begin
      BaseScheme.AtiveTableDraw;
      VToStrings(BaseScheme.ActiveTableSQL, SQLSynEdit.Lines);
    end;
  end;
  SetFieldAndIndexButtonsEnabled;
end;

procedure TMainForm.TableEdit(const AEditMode: Byte);
var
  TableEditForm: TTableEditForm;
begin
  TableEditForm:= TTableEditForm.Create(MainForm);
  try
    TableEditForm.BaseSchemeSet(BaseScheme, AEditMode);
    if TableEditForm.ShowModal=mrOK then
    begin
      TableListSet;
      SetSchemeChanged(True);
    end;
  finally
    FreeAndNil(TableEditForm);
  end;
end;

procedure TMainForm.TableDelete;
begin
  if not MsgConfirm('Удалить таблицу "' +
                 BaseScheme.ActiveTable.TableName +
                 '"?') then Exit;

  BaseScheme.TableDel;
  TableListSet;
  SetSchemeChanged(True);
end;

procedure TMainForm.FieldEdit(const AEditMode: Byte);
var
  FieldEditForm: TFieldEditForm;
begin
  FieldEditForm:= TFieldEditForm.Create(MainForm);
  try
    FieldEditForm.BaseSchemeSet(BaseScheme, AEditMode);
    if FieldEditForm.ShowModal=mrOK then
    begin
      SchemeDraw(ZoomPercent);
      SetSchemeChanged(True);
    end;
  finally
    FreeAndNil(FieldEditForm);
  end;
end;

procedure TMainForm.FieldDelete;
begin
  if not MsgConfirm('Удалить поле "' +
                 BaseScheme.ActiveField.FieldName +
                 '"?') then Exit;
  BaseScheme.FieldDel;
  SchemeDraw(ZoomPercent);
  SetSchemeChanged(True);
end;

procedure TMainForm.IndexEdit(const AEditMode: Byte);
var
  IndexEditForm: TIndexEditForm;
begin
  IndexEditForm:= TIndexEditForm.Create(MainForm);
  try
    IndexEditForm.BaseSchemeSet(BaseScheme, AEditMode);
    if IndexEditForm.ShowModal=mrOK then
    begin
      SchemeDraw(ZoomPercent);
      SetSchemeChanged(True);
    end;
  finally
    FreeAndNil(IndexEditForm);
  end;
end;

procedure TMainForm.IndexDelete;
begin
  if not MsgConfirm('Удалить индекс "' +
                 BaseScheme.ActiveIndex.IndexName +
                 '"?') then Exit;
  BaseScheme.IndexDel;
  SchemeDraw(ZoomPercent);
  SetSchemeChanged(True);
end;

procedure TMainForm.ValuesEdit;
var
  ValuesEditForm: TValuesEditForm;
begin
  ValuesEditForm:= TValuesEditForm.Create(MainForm);
  try
    ValuesEditForm.BaseScheme:= BaseScheme;
    if ValuesEditForm.ShowModal=mrOK then
    begin
      SchemeDraw(ZoomPercent);
      SetSchemeChanged(True);
    end;
  finally
    FreeAndNil(ValuesEditForm);
  end;
end;

end.

