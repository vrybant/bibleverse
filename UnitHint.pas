unit UnitHint;

interface

uses
  System.SysUtils, System.Types, System.UITypes, System.Classes, System.Variants,
  FMX.Types, FMX.Graphics, FMX.Controls, FMX.Forms, FMX.StdCtrls, FMX.Layouts, FMX.Memo, FMX.Objects,
  FMX.Controls.Presentation;

type
  THint = record
    Text : string;
    Enable : boolean;
    Once : boolean; // показать только один раз
  end;

type
  TFormHint = class(TForm)
    LabelMain: TLabel;
    Shape: TRectangle;
    Timer: TTimer;
    Image: TImage;
    procedure FormActivate(Sender: TObject);
    procedure LabelMainClick(Sender: TObject);
    procedure ShapeClick(Sender: TObject);
    procedure TimerTimer(Sender: TObject);
    procedure FormClose(Sender: TObject; var Action: TCloseAction);
    procedure FormCreate(Sender: TObject);
  private
    { Private declarations }
    Text : string;
    procedure Translate;
    procedure Rebuild;
  public
    { Public declarations }
    procedure Show(var Hint: THint); overload;
  end;

var
  FormHint: TFormHint;

  HintClick   : THint;
  HintMenu    : THint;
  HintList    : THint;
  HintSize    : THint;
  HintFont    : THint;
  HintOpacity : THint;

implementation

uses UnitMain, UnitLang, UnitLib;

{$R *.fmx}

procedure TFormHint.FormCreate(Sender: TObject);
begin
  HintClick   .Once := False;
  HintMenu    .Once := False;
  HintList    .Once := True;
  HintSize    .Once := True;
  HintFont    .Once := True;
  HintOpacity .Once := True;

  Translate;
end;

procedure TFormHint.FormActivate(Sender: TObject);
begin
  Rebuild;
  Timer.Enabled := True;
end;

procedure TFormHint.FormClose(Sender: TObject; var Action: TCloseAction);
begin
  LabelMain.Text := ''; // иначе видна перерисовка текста
  Timer.Enabled := False;
end;

procedure TFormHint.Show(var Hint: THint);
begin
  {$ifdef macos} Exit; {$endif}

  Text := Hint.Text;
  inherited Show;
end;

procedure TFormHint.Translate;
begin
  Lang := TLang.Create;

  HintClick.Text   := T('HintClick'  );
  HintMenu.Text    := T('HintMenu'   );
  HintList.Text    := T('HintList'   );
  HintSize.Text    := T('HintSize'   );
  HintFont.Text    := T('HintFont'   );
  HintOpacity.Text := T('HintOpacity');

  Lang.Free;
end;

procedure TFormHint.TimerTimer(Sender: TObject);
begin
  Close;
end;

procedure TFormHint.LabelMainClick(Sender: TObject);
begin
  Close;
end;

procedure TFormHint.ShapeClick(Sender: TObject);
begin
  Close;
end;

procedure TFormHint.Rebuild;
const
  MaxWidth = 170;
  border = 20;
begin
  LabelMain.Text := Self.Text;
  WordWrap(LabelMain, MaxWidth);

  Height := Round(LabelMain.Height + LabelMain.Position.Y * 2);
  Width  := Round(LabelMain.Width  + LabelMain.Position.X + border);

//  Left := FormMain.Left;
//  if (FormMain.Top * 2) < Screen.Size.Height then Top := FormMain.Top + FormMain.Height + border
//                                             else Top := FormMain.Top - Height - border;

  Left := FormMain.Left;
  if (FormMain.Top * 2) < Screen.Size.Height
    then Top := FormMain.Top + Round(FormMain.Shape.Height) + border
    else Top := FormMain.Top - Height - border;
end;

end.
