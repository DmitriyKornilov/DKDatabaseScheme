unit UAboutForm;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils, FileUtil, Forms, Controls, Graphics, Dialogs, ExtCtrls,
  StdCtrls, Buttons, LCLIntf,
  //DK packages utils
  DK_CtrlUtils;

const
  URL_PREFIX = 'https://';
  MAIL_PREFIX = 'mailto:';

  MAIN_CAPTION = 'DKDatabaseScheme v1.1';

  PROJECT_NOTE = 'Редактор схем баз данных для SQLite3';
  PROJECT_URL  = 'www.github.com/DmitriyKornilov/DKDatabaseScheme';

  AUTOR_NAME   = 'Дмитрий Корнилов';
  AUTOR_MAIL   = 'dakor2017@yandex.ru';

  COMPILER_VER = 'Free Pascal Compiler v3.2.2';
  COMPILER_URL = 'www.freepascal.org';
  LAZARUS_VER  = 'Lazarus IDE v4.2';
  LAZARUS_URL  = 'www.lazarus-ide.org';

  APPICON_INFO = 'Application icon created by prettycons - Flaticon (Flaticon License):';
  APPICON_URL  = 'www.flaticon.com/free-icon/databases_977506';

  PNG_INFO     = 'Buttons icons created by Tomas Hubelbauer (MIT License):';
  PNG_URL      = 'www.github.com/TomasHubelbauer/fatcow-icons';

  FONT_INFO    = 'JetBrains Mono Regular Nerd Font Complete v1.0.2 (SIL OFL 1.1 License):';
  FONT_URL     = 'www.github.com/ryanoasis/nerd-fonts';

type

  { TAboutForm }

  TAboutForm = class(TForm)
    AutorMailBeginLabel: TLabel;
    CompilerURLBeginLabel: TLabel;
    LazarusURLEndLabel: TLabel;
    LazarusURLLabel: TLabel;
    LazarusURLBeginLabel: TLabel;
    CompilerURLEndLabel: TLabel;
    CompilerURLLabel: TLabel;
    CompilerVerLabel: TLabel;
    LazarusVerLabel: TLabel;
    AppIconInfoLabel: TLabel;
    FontInfoLabel: TLabel;
    PNGURLLabel: TLabel;
    AppIconURLLabel: TLabel;
    FontURLLabel: TLabel;
    UsedLabel: TLabel;
    AutorNameLabel: TLabel;
    AutorMailEndLabel: TLabel;
    LazarusImage: TImage;
    AppNameLabel: TLabel;
    AutorLabel: TLabel;
    ProjectURLLabel: TLabel;
    ProjectNoteLabel: TLabel;
    ProjectLabel: TLabel;
    AutorMailLabel: TLabel;
    PNGInfoLabel: TLabel;
    procedure FormShow(Sender: TObject);
  private
    procedure DoMailTo(Sender: TObject);
    procedure DoOpenURL(Sender: TObject);
  public

  end;

var
  AboutForm: TAboutForm;

implementation

{$R *.lfm}

{ TAboutForm }

procedure TAboutForm.DoMailTo(Sender: TObject);
begin
  OpenURL(MAIL_PREFIX + (Sender as TLabel).Caption);
end;

procedure TAboutForm.DoOpenURL(Sender: TObject);
begin
  OpenURL(URL_PREFIX + (Sender as TLabel).Caption);
end;

procedure TAboutForm.FormShow(Sender: TObject);

  procedure SetLabel(const ALabel: TLabel;
                     const ACaption: String;
                     const AOnClick: TNotifyEvent = nil);
  begin
    ALabel.Caption:= ACaption;
    if Assigned(AOnClick) then
      ALabel.OnClick:= AOnClick;
  end;

begin
  SetLabel(AppNameLabel, MAIN_CAPTION);

  SetLabel(ProjectNoteLabel, PROJECT_NOTE);
  SetLabel(ProjectURLLabel, PROJECT_URL, @DoOpenURL);

  SetLabel(AutorNameLabel, AUTOR_NAME);
  SetLabel(AutorMailLabel, AUTOR_MAIL, @DoMailTo);

  SetLabel(CompilerVerLabel, COMPILER_VER);
  SetLabel(CompilerURLLabel, COMPILER_URL, @DoOpenURL);

  SetLabel(LazarusVerLabel, LAZARUS_VER);
  SetLabel(LazarusURLLabel, LAZARUS_URL, @DoOpenURL);

  SetLabel(AppIconInfoLabel, APPICON_INFO);
  SetLabel(AppIconURLLabel, APPICON_URL, @DoOpenURL);

  SetLabel(PNGInfoLabel, PNG_INFO);
  SetLabel(PNGURLLabel, PNG_URL, @DoOpenURL);

  SetLabel(FontInfoLabel, FONT_INFO);
  SetLabel(FontURLLabel, FONT_URL, @DoOpenURL);

  FormKeepMinSize(Self);
end;

end.

