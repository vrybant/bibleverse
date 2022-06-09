unit UnitAbout;

interface

uses
  System.Classes, System.UITypes,
  FMX.Types, FMX.Graphics, FMX.Controls, FMX.Forms, FMX.StdCtrls, FMX.Objects, FMX.Effects, FMX.Layouts,
  FMX.Ani, FMX.Controls.Presentation;

type
  TFormAbout = class(TForm)
    LabelApp: TLabel;
    Button: TButton;
    LabelVersion: TLabel;
    LabelCopyright: TLabel;
    LabelAuthor: TLabel;
    TextWWW: TText;
    Memo: TLabel;
    Line1: TLine;
    Line2: TLine;
    procedure FormActivate(Sender: TObject);
    procedure TextWWWClick(Sender: TObject);
    procedure TextWWWMouseMove(Sender: TObject; Shift: TShiftState; X, Y: Single);
    procedure TextWWWMouseLeave(Sender: TObject);
    procedure FormShow(Sender: TObject);
    procedure ButtonClick(Sender: TObject);
  private
    { Private declarations }
    procedure Translate;
  public
    { Public declarations }
  end;

var
  FormAbout: TFormAbout;

implementation

{$R *.fmx}

uses
  UnitMain, UnitList, UnitLang, UnitLib;

procedure TFormAbout.Translate;
begin
  Lang := TLang.Create;
  Caption := T('About');
  Lang.Free;

  if GetDefaultLanguage = 'russian' then
    begin
      LabelVersion.Text := 'Версия 4.0';
      LabelAuthor .Text := 'Владимир Рыбант';
    end;
end;

procedure TFormAbout.ButtonClick(Sender: TObject);
begin
  OpenUrl('https://www.facebook.com/bible.verse.desktop');
end;

procedure TFormAbout.FormActivate(Sender: TObject);
begin
  Memo.Text := List.Info;
//  caption :=
end;

procedure TFormAbout.FormShow(Sender: TObject);
begin
  Translate;
end;

procedure TFormAbout.TextWWWClick(Sender: TObject);
begin
  if GetDefaultLanguage = 'russian'
    then OpenUrl('http://vladimirrybant.org/ru/')
    else OpenUrl('http://vladimirrybant.org/');
end;

procedure TFormAbout.TextWWWMouseLeave(Sender: TObject);
begin
  TextWWW.Color := TAlphaColorRec.Black;
end;

procedure TFormAbout.TextWWWMouseMove(Sender: TObject; Shift: TShiftState; X, Y: Single);
begin
  TextWWW.Color := TAlphaColorRec.Navy;
end;

end.
