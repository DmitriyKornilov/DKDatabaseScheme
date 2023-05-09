unit UExtraFont;

{$mode ObjFPC}{$H+}

interface

uses
  {$IFDEF WINDOWS} Windows, {$ENDIF}
  {$IFDEF LINUX} FileUtil, {$ENDIF}
  Classes, SysUtils, Forms, USchemeTypes;

procedure LoadExtraFont;
procedure UnloadExtraFont;

implementation

procedure LoadExtraFont;
var
  FilePath, FileName: String;
begin
  FilePath:= ExtractFilePath(Application.ExeName) + 'font' + DirectorySeparator;
  FileName:= FilePath + SCHEME_FONT_FILENAME;

  {$IFDEF WINDOWS}
  AddFontResource(PChar(FileName));
  {$ENDIF}

  {$IFDEF LINUX}
  CopyFile(FileName, GetUserDir + '.fonts/' + SCHEME_FONT_FILENAME, [cffCreateDestDirectory]);
  ExecuteProcess(Application.ExeName, []);
  Application.Terminate;
  {$ENDIF}
end;

procedure UnloadExtraFont;
var
  FileName: String;
begin
  {$IFDEF WINDOWS}
  FileName:= ExtractFilePath(Application.ExeName) + 'font\' + SCHEME_FONT_FILENAME;
  RemoveFontResource(PChar(FileName));
  {$ENDIF}
  {$IFDEF LINUX}
  FileName:= GetUserDir + '.fonts/' + SCHEME_FONT_FILENAME;
  DeleteFile(FileName);
  {$ENDIF}
end;

end.

