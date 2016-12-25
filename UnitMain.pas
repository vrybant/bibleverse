unit UnitMain;

{ $define menu}

interface

uses
  System.SysUtils, System.Types, System.UITypes, System.Classes, System.Variants,
  FMX.Types, FMX.Graphics, FMX.Controls, FMX.Forms, FMX.Dialogs, FMX.StdCtrls, FMX.ActnList,
  FMX.Objects, FMX.Layouts, FMX.Memo, FMX.ListBox, FMX.Effects, FMX.Menus, FMX.MagnifierGlass,
  IniFiles, UnitHint, System.Actions, FMX.Controls.Presentation;

type
  TFormMain = class(TForm)
    LabelMain: TLabel;
    Shape: TRectangle;
    LabelSign: TLabel;
    ShadowEffectMain: TShadowEffect;
    ShadowEffectSign: TShadowEffect;
    PopupMenu: TPopupMenu;
    miAbout: TMenuItem;
    miExit: TMenuItem;
    miSeparator1: TMenuItem;
    miCopy: TMenuItem;
    miList: TMenuItem;
    miFont: TMenuItem;
    miSeparator2: TMenuItem;
    miTransparent: TMenuItem;
    miRandom: TMenuItem;
    miTimer: TMenuItem;
    miWallpaper: TMenuItem;
    OpenDialog: TOpenDialog;
    miTimerOff: TMenuItem;
    miSeparator3: TMenuItem;
    miTimer10sec: TMenuItem;
    miTimer30sec: TMenuItem;
    miTimer1min: TMenuItem;
    miTimer30min: TMenuItem;
    miTimer60min: TMenuItem;
    Timer: TTimer;
    miLang: TMenuItem;
    OpenDialogLang: TOpenDialog;
    TimerHint: TTimer;
    Rectangle: TRectangle;
    ActionList: TActionList;
    cmAbout: TAction;
    cmExit: TAction;
    MainMenu: TMainMenu;
    mmOptions: TMenuItem;
    mmList: TMenuItem;
    mmFont: TMenuItem;
    mmTransparent: TMenuItem;
    mmTimer: TMenuItem;
    mmRandom: TMenuItem;
    mmAbout: TMenuItem;
    cmList: TAction;
    cmFont: TAction;
    cmTransparent: TAction;
    cmRandom: TAction;
    cmCopy: TAction;
    cmWallpaper: TAction;
    cmLocalisation: TAction;
    cmTimerOff: TAction;
    cmTimer10sec: TAction;
    cmTimer30sec: TAction;
    cmTimer1min: TAction;
    cmTimer30min: TAction;
    cmTimer60min: TAction;
    mmTimerOff: TMenuItem;
    mmSeparator1: TMenuItem;
    mmTimer10sec: TMenuItem;
    mmTimer30sec: TMenuItem;
    mmTimer1min: TMenuItem;
    mmTimer30min: TMenuItem;
    mmTimer60min: TMenuItem;
    mm01: TMenuItem;
    mm02: TMenuItem;
    mm03: TMenuItem;
    mm04: TMenuItem;
    mm05: TMenuItem;
    mm06: TMenuItem;
    mm07: TMenuItem;
    mm09: TMenuItem;
    mm10: TMenuItem;
    mm11: TMenuItem;
    mm12: TMenuItem;
    mm13: TMenuItem;
    mm14: TMenuItem;
    mm15: TMenuItem;
    mm17: TMenuItem;
    mm18: TMenuItem;
    mm19: TMenuItem;
    mm00: TMenuItem;
    mm08: TMenuItem;
    cmListItem: TAction;
    procedure ShapeMouseDown(Sender: TObject; Button: TMouseButton; Shift: TShiftState; X, Y: Single);
    procedure ShapeMouseMove(Sender: TObject; Shift: TShiftState; X, Y: Single);
    procedure ShapeMouseUp(Sender: TObject; Button: TMouseButton; Shift: TShiftState; X, Y: Single);
    procedure FormCreate(Sender: TObject);
    procedure FormClose(Sender: TObject; var Action: TCloseAction);
    procedure ShapeMouseWheel(Sender: TObject; Shift: TShiftState; WheelDelta: Integer; var Handled: Boolean);
    procedure FormPaint(Sender: TObject; Canvas: TCanvas; const ARect: TRectF);
    procedure PopupMenuPopup(Sender: TObject);
    procedure TimerTimer(Sender: TObject);
    procedure FormActivate(Sender: TObject);
    procedure TimerHintTimer(Sender: TObject);
    procedure cmAboutExecute(Sender: TObject);
    procedure cmExitExecute(Sender: TObject);
    procedure cmCopyExecute(Sender: TObject);
    procedure cmListExecute(Sender: TObject);
    procedure cmFontExecute(Sender: TObject);
    procedure cmTransparentExecute(Sender: TObject);
    procedure cmRandomExecute(Sender: TObject);
    procedure cmWallpaperExecute(Sender: TObject);
    procedure cmLocalisationExecute(Sender: TObject);
    procedure cmTimer10secExecute(Sender: TObject);
    procedure cmTimer30secExecute(Sender: TObject);
    procedure cmTimer1minExecute(Sender: TObject);
    procedure cmTimer30minExecute(Sender: TObject);
    procedure cmTimer60minExecute(Sender: TObject);
    procedure cmTimerOffExecute(Sender: TObject);
    procedure cmListItemExecute(Sender: TObject);
    procedure mm00Click(Sender: TObject);
    procedure mm01Click(Sender: TObject);
    procedure mm02Click(Sender: TObject);
    procedure mm03Click(Sender: TObject);
    procedure mm04Click(Sender: TObject);
    procedure mm05Click(Sender: TObject);
    procedure mm06Click(Sender: TObject);
    procedure mm07Click(Sender: TObject);
    procedure mm08Click(Sender: TObject);
    procedure mm09Click(Sender: TObject);
    procedure mm10Click(Sender: TObject);
    procedure mm11Click(Sender: TObject);
    procedure mm12Click(Sender: TObject);
    procedure mm13Click(Sender: TObject);
    procedure mm14Click(Sender: TObject);
    procedure mm15Click(Sender: TObject);
    procedure mm16Click(Sender: TObject);
    procedure mm17Click(Sender: TObject);
    procedure mm18Click(Sender: TObject);
    procedure mm19Click(Sender: TObject);
  private
    { Private declarations }
    FontList       : TStringList;
    FontDefault    : TFont;
    FileList       : String;
    PrevList       : String;
    ShapeWidth     : integer;
    nTimer         : integer;
    maxTimer       : integer;
    font_idx       : integer;
    Semaphore      : boolean;
    function IncFontName(delta: integer) : String;
    procedure ChangeList(NewList : String);
    procedure ChangeTimer(seconds: integer);
    {$ifdef menu} procedure CreateListMenu; {$endif}
    procedure mmClick(number: integer);
    procedure SetMenuItems;
    procedure SetFontIndex;
    procedure Rebuild;
    procedure Repaint;
    procedure SaveIniFile;
    procedure ReadIniFile;
    procedure Translate;
    procedure ShowHint(var Hint: THint);
  public
    { Public declarations }
  end;

var
  FormMain: TFormMain;

implementation

{$R *.fmx}

uses
  System.IOUtils,
  {$ifdef mswindows} Vcl.Clipbrd,  {$endif}
  // UFontDlg,
  UnitLang, UnitLib, UnitList, UnitAbout;

var
  XX : Single = 0;
  YY : Single = 0;

var
  oLeft : integer = 0;
  oTop  : integer = 0;

const
  Shadow = not True;

  MAX_OPACITY = 0.25;
  DEFAULT_OPACITY = 0.05;

const
   PopupHeight = 250;

{$ifdef mswindows} var FirstPaint : boolean = True; {$endif}
{$ifdef macos} var macbag : integer = 0; {$endif}

//-------------------------------------------------------------------------------------------------

//   Position = Designed
//   Visible = False

procedure TFormMain.FormCreate(Sender: TObject);
begin
  // Установка Transparency в True визуально приводит к ошибке из за конфликта с MainMenu

  {$if defined(macos) or not defined(menu)} Transparency := True; {$endif}
  {$ifndef menu} MainMenu.Free; {$endif}

  FontDefault := FMX.Graphics.TFont.Create;
  FontList  := TStringList.Create;

  Rectangle.Align := alClient;

  Shape.Position.x := 0;
  Shape.Position.y := 0;

  Width  := Screen.Size.Width  div 2;
  Height := Screen.Size.Height div 2;

  nTimer := 0;
  Semaphore := False;
  Randomize;

  ReadIniFile;

  {$ifdef mswindows} GetFonts(FontList); {$endif}
  {$ifdef menu} CreateListMenu; {$endif}

  List.LoadFile(FileList);
  List.Next;

  SetMenuItems;

  Translate;

  {$ifdef macos}
  LabelMain.Text := ' ';
  LabelSign.Text := ' ';
  Shape.Width := 0;
  {$endif}

  Width  := Screen.Size.Width  div 2;
  Height := Screen.Size.Height div 2;
end;

//-------------------------------------------------------------------------------------------------

procedure TFormMain.FormActivate(Sender: TObject);
begin
 {$ifdef mswindows} HideAppOnTaskbar(Self); {$endif}
end;

//-------------------------------------------------------------------------------------------------

procedure TFormMain.FormPaint(Sender: TObject; Canvas: TCanvas; const ARect: TRectF);
begin
  {$ifdef mswindows}
  if FirstPaint then
    begin
      Rebuild;
      FirstPaint := False;
    end;
  {$endif}
end;

//-------------------------------------------------------------------------------------------------

procedure TFormMain.Repaint;
begin
  {$ifdef macos} macbag := 0; {$endif}
  Rebuild;
  Self.Invalidate;
  Application.ProcessMessages;
end;

//-------------------------------------------------------------------------------------------------

procedure TFormMain.FormClose(Sender: TObject; var Action: TCloseAction);
begin
  SaveIniFile;
  FontDefault.Free;
  FontList   .Free;
end;

//-------------------------------------------------------------------------------------------------

procedure TFormMain.Translate;
begin
  Lang := TLang.Create;

  miAbout       .Text := T('About'       );
  miCopy        .Text := T('Copy'        );
  miList        .Text := T('List'        );
  miFont        .Text := T('Font'        );
  miExit        .Text := T('Exit'        );
  miTransparent .Text := T('Transparent' );
  miRandom      .Text := T('Random'      );
  miWallpaper   .Text := T('Wallpaper'   );
  miLang        .Text := T('Localization');
  miTimer       .Text := T('Timer'       );
  miTimerOff    .Text :=   '  ' + T('off');
  miTimer10sec  .Text :=  '10 ' + T('sec');
  miTimer30sec  .Text :=  '30 ' + T('sec');
  miTimer1min   .Text := '  1 ' + T('min');
  miTimer30min  .Text :=  '30 ' + T('min');
  miTimer60min  .Text :=  '60 ' + T('min');

  {$ifdef menu}
  mmAbout       .Text := T('About'       );
  mmList        .Text := T('List'        );
  mmFont        .Text := T('Font'        );
  mmOptions     .Text := T('Options'     );
  mmTransparent .Text := T('Transparent' );
  mmRandom      .Text := T('Random'      );
  mmTimer       .Text := T('Timer'       );
  mmTimerOff    .Text :=   '  ' + T('off');
  mmTimer10sec  .Text :=  '10 ' + T('sec');
  mmTimer30sec  .Text :=  '30 ' + T('sec');
  mmTimer1min   .Text := '  1 ' + T('min');
  mmTimer30min  .Text :=  '30 ' + T('min');
  mmTimer60min  .Text :=  '60 ' + T('min');
  {$endif}

  Lang.Free;
end;

//-------------------------------------------------------------------------------------------------

function TFormMain.IncFontName(delta: integer) : String;
begin
  if delta > 0 then font_idx := font_idx - 1
               else font_idx := font_idx + 1;

  if font_idx < 0 then font_idx := FontList.Count-1 else
  if font_idx = FontList.Count then font_idx := 0;
  Result := FontList[font_idx];
end;

procedure TFormMain.SetFontIndex;
var i : integer;
begin
  font_idx := 0;
  for i:=0 to FontList.Count-1 do
    if FontList[i] = LabelMain.Font.Family then font_idx := i;
end;

function IncVerseWidth(Width, delta : integer) : integer;
const
  MIN = 5;
  MAX = 25;
begin
  Result := Width;
  if delta > 0 then Inc(Result) else Dec(Result);

  if Result < MIN then Result := MIN;
  if Result > MAX then Result := MAX;
end;

function IncFontSize(Size, d : integer) : integer;
begin
  Result := Size;
  Size := Size + d;
  if (Size >=  8) and (Size <= 128) then Result := Size;  // 48
end;

//-----------------------------------------------------------------------------------------
                                {$region 'MouseButtons'}
//-----------------------------------------------------------------------------------------

procedure TFormMain.ShapeMouseDown(Sender: TObject; Button: TMouseButton; Shift: TShiftState; X, Y: Single);
begin
  if (Button = mbLeft) then
    begin
      XX := X;
      YY := Y;
      Semaphore := True;
      Repaint;
    end;

  oLeft := Left;
  oTop  := Top ;
end;

procedure TFormMain.ShapeMouseMove(Sender: TObject; Shift: TShiftState; X, Y: Single);
begin
  if not (ssLeft in Shift) then Exit;
  if not Semaphore then exit;

  Left := Left + Round(X-XX);
  Top  := Top  + Round(Y-YY);
end;

procedure TFormMain.ShapeMouseUp(Sender: TObject; Button: TMouseButton; Shift: TShiftState; X, Y: Single);
begin
  if (Left <> oLeft) or (Top <> oTop) then SaveIniFile;;
  if (Left <> oLeft) or (Top <> oTop) then Exit;

  if (Button = mbLeft) {$ifdef mswindows} and ([] = Shift) {$endif} and Semaphore then
    begin
      nTimer := 0;
      List.Next;
      Repaint;
      HintClick.Enable := False;
    end;

  if (Button = mbLeft) and (ssAlt in Shift) then
    begin
      List.Undo;
      Repaint;
    end;

   if (Button = mbLeft) and (ssCtrl  in Shift) then
    begin
      cmCopy.Execute;
    end;

  if (Button = mbMiddle) and ([] =  Shift) then
    begin
      nTimer := 0;
      ChangeList(PrevList);
    end;

  if (Button = mbRight) then
    begin
      {$ifdef mswindows}
      nTimer := 0;
      if (Top + y + PopupHeight) < (Screen.Size.Height - 50) then
        PopupMenu.Popup(Left + x, Top + y) else PopupMenu.Popup(Left + x, Top + y - PopupHeight);
      HintMenu.Enable := False;
      {$endif}

      {$ifdef macos} Close; {$endif} //////////////////////////////////////////////////////////////
    end;

  SaveIniFile;
  Semaphore := False;
end;

//-------------------------------------------------------------------------------------------------

procedure TFormMain.ShapeMouseWheel(Sender: TObject; Shift: TShiftState; WheelDelta: Integer; var Handled: Boolean);
begin
  if Shift = [ssShift] then LabelMain.Font.Family := IncFontName(WheelDelta);
  if Shift = [ssCtrl]  then LabelMain.Font.Size := LabelMain.Font.Size + WheelDelta/100;

  if Shift = [] then
    begin
      Shape.Opacity := Shape.Opacity + WheelDelta/20000;
      if Shape.Opacity > MAX_OPACITY then Shape.Opacity := MAX_OPACITY;
      {$ifdef mswindows} miTransparent.IsChecked := (Shape.Opacity = 0); {$endif}
      {$ifdef menu}      mmTransparent.IsChecked := (Shape.Opacity = 0); {$endif}
    end;

  Repaint;
  if Shift = [ssShift,ssCtrl] then ShapeWidth := IncVerseWidth(ShapeWidth, WheelDelta);
  SaveIniFile;
end;
{$endregion}
//-------------------------------------------------------------------------------------------------

procedure TFormMain.TimerTimer(Sender: TObject);
begin
  if maxTimer = 0 then Exit;
  Inc(nTimer);
  if nTimer < maxTimer then Exit;
  nTimer := 0;
  List.Next;
  Repaint;
end;

procedure TFormMain.TimerHintTimer(Sender: TObject);
begin
  if HintClick.Enable then ShowHint(HintClick) else ShowHint(HintMenu);
  TimerHint.Enabled := False;
end;

//-------------------------------------------------------------------------------------------------

procedure TFormMain.mmClick(number: integer);
var
  FileName : string;
  i: integer;
begin
  {$ifdef menu}
  for i := 0 to mmList.ItemsCount-1 do mmList.Items[i].IsChecked := False;
  mmList.Items[number].IsChecked := not mmList.Items[number].IsChecked;
  FileName := ApplicationPath + 'lists' + slash + mmList.Items[number].Text + '.txt' ;
  ChangeList(FileName);
// ShowHint(HintList);
  {$endif}
end;

//-------------------------------------------------------------------------------------------------

procedure TFormMain.mm00Click(Sender: TObject); begin mmClick(00) end;
procedure TFormMain.mm01Click(Sender: TObject); begin mmClick(01) end;
procedure TFormMain.mm02Click(Sender: TObject); begin mmClick(02) end;
procedure TFormMain.mm03Click(Sender: TObject); begin mmClick(03) end;
procedure TFormMain.mm04Click(Sender: TObject); begin mmClick(04) end;
procedure TFormMain.mm05Click(Sender: TObject); begin mmClick(05) end;
procedure TFormMain.mm06Click(Sender: TObject); begin mmClick(06) end;
procedure TFormMain.mm07Click(Sender: TObject); begin mmClick(07) end;
procedure TFormMain.mm08Click(Sender: TObject); begin mmClick(08) end;
procedure TFormMain.mm09Click(Sender: TObject); begin mmClick(09) end;
procedure TFormMain.mm10Click(Sender: TObject); begin mmClick(10) end;
procedure TFormMain.mm11Click(Sender: TObject); begin mmClick(11) end;
procedure TFormMain.mm12Click(Sender: TObject); begin mmClick(12) end;
procedure TFormMain.mm13Click(Sender: TObject); begin mmClick(13) end;
procedure TFormMain.mm14Click(Sender: TObject); begin mmClick(14) end;
procedure TFormMain.mm15Click(Sender: TObject); begin mmClick(15) end;
procedure TFormMain.mm16Click(Sender: TObject); begin mmClick(16) end;
procedure TFormMain.mm17Click(Sender: TObject); begin mmClick(17) end;
procedure TFormMain.mm18Click(Sender: TObject); begin mmClick(18) end;
procedure TFormMain.mm19Click(Sender: TObject); begin mmClick(19) end;

//-------------------------------------------------------------------------------------------------

procedure TFormMain.ShowHint(var Hint: THint);
begin
  if not Hint.Enable then Exit;
  FormHint.Show(Hint);
  self.BringToFront;
  if not Hint.Once then Exit;
  Hint.Enable := False;
  SaveIniFile;
end;

//-------------------------------------------------------------------------------------------------

procedure TFormMain.Rebuild;
var
  MaxWidth : Single;
      Temp : String;
    border : Single;
         w : integer;
         h : Single;
       L,Y : boolean;
const
  Min_Width = 500;

  k_border = 0.1;
  k_radius = 0.5;

begin
  MaxWidth := ShapeWidth * LabelMain.Font.Size;

  LabelSign.Font.Assign(LabelMain.Font);
  LabelSign.FontColor := LabelMain.FontColor;

  LabelMain.Text := '*'; LabelMain.Text := List.GetVerse;
  LabelSign.Text := '*'; LabelSign.Text := List.GetSign;

  WordWrap(LabelMain, MaxWidth);
  MaxWidth := LabelMain.Width;

  Temp := LabelMain.Text;
  LabelMain.Text := LabelMain.Text + ' ' + LabelSign.Text;
  WordWrap(LabelMain, MaxWidth);
  h := LabelMain.Height;
  LabelMain.Text := Temp;

  Border := LabelMain.Width * k_border;

  LabelMain.Position.X := Border;
  LabelMain.Position.Y := Border;

  L := (LabelMain.Height = h);
  Y := (LabelMain.Width > LabelSign.Width);

  if L then LabelSign.Position.Y := Border + LabelMain.Height - LabelSign.Height
       else LabelSign.Position.Y := Border + LabelMain.Height;

  if L then Shape.Height := Round(Border * 2 + LabelMain.Height)
       else Shape.Height := Round(Border * 2 + LabelMain.Height + LabelSign.Height);

  if Y then LabelSign.Position.X := Border + LabelMain.Width  - LabelSign.Width
       else LabelSign.Position.X := Border;

  if Y then w := Round(Border * 2 + LabelMain.Width)
       else w := Round(Border * 2 + LabelSign.Width);

  if w < 100 then w := 100;

  Shape.Width := w; // использование напрямую (Width := Width + ..) приводит к ошибки

//{$ifndef menu} Width  := Round(Shape.Width ); {$endif} // без этого работает быстрее
//{$ifndef menu} Height := Round(Shape.Height); {$endif}

  //-------  oкно не должно выходить за пределы экрана

  if (Left + Shape.Width ) > Screen.Size.Width  then Left := Screen.Size.Width  - Round(Shape.Width );
  if (Top  + Shape.Height) > Screen.Size.Height then Top  := Screen.Size.Height - Round(Shape.Height);

  if Left < 0 then Left := 0;
  if Top  < 0 then Top  := 0;

  Shape.XRadius := LabelMain.Font.Size * k_Radius;
  Shape.YRadius := Shape.XRadius;
end;

//-----------------------------------------------------------------------------------------

{$ifdef menu}
procedure TFormMain.CreateListMenu;
var
  Strings : TStringDynArray;
  i : integer;
begin
  Strings := TDirectory.GetFiles(ApplicationPath + 'lists','*.txt');

  for i := 0 to mmList.ItemsCount - 1 do
    mmList.Items[i].Visible := False;

  for i := 0 to Length(Strings) - 1 do
    begin
      mmList.Items[i].Text := TPath.GetFileNameWithoutExtension(Strings[i]);
      mmList.Items[i].Visible := True;
      if Strings[i] = FileList then mmList.Items[i].IsChecked := True;
    end;
end;
{$endif}

//-----------------------------------------------------------------------------------------

procedure TFormMain.SetMenuItems;
begin
{$ifdef mswindows}
  miTimerOff  .IsChecked := (maxTimer =  0 );
  miTimer10sec.IsChecked := (maxTimer =  3 ); /////////////////////////////////////////////////////////////////////////////////////
  miTimer30sec.IsChecked := (maxTimer = 30 );
  miTimer1min .IsChecked := (maxTimer =  1 * 60);
  miTimer30min.IsChecked := (maxTimer = 30 * 60);
  miTimer60min.IsChecked := (maxTimer = 60 * 60);
{$endif}
{$ifdef menu}
  mmTimerOff  .IsChecked := (maxTimer =  0 );
  mmTimer10sec.IsChecked := (maxTimer =  3 ); /////////////////////////////////////////////////////////////////////////////////////
  mmTimer30sec.IsChecked := (maxTimer = 30 );
  mmTimer1min .IsChecked := (maxTimer =  1 * 60);
  mmTimer30min.IsChecked := (maxTimer = 30 * 60);
  mmTimer60min.IsChecked := (maxTimer = 60 * 60);
{$endif}
  miTransparent.IsChecked := (Shape.Opacity = 0);
  miRandom.IsChecked := List.Randomly;
end;

//-------------------------------------------------------------------------------------------------

procedure TFormMain.ChangeTimer(seconds: integer);
begin
  maxTimer := seconds;
  SetMenuItems;
  nTimer := 0;
  SaveIniFile;
end;

//-------------------------------------------------------------------------------------------------

procedure TFormMain.ChangeList(NewList : String);
begin
  PrevList := FileList;
  FileList := NewList;

  List.LoadFile(FileList);
  Rebuild;
end;

//-------------------------------------------------------------------------------------------------

procedure TFormMain.PopupMenuPopup(Sender: TObject);
begin
  {$ifdef mswindows} miLang.Visible := KeyShiftDown; {$endif}
end;

//-----------------------------------------------------------------------------------------
                                {$region 'ActionList'}
//-----------------------------------------------------------------------------------------

procedure TFormMain.cmAboutExecute(Sender: TObject);
begin
  FormAbout.ShowModal;
  Self.BringToFront;
end;

procedure TFormMain.cmCopyExecute(Sender: TObject);
begin
  {$ifdef mswindows}
  Clipboard.Clear;
  Clipboard.AsText := List.GetQuote;
  {$endif}
end;

procedure TFormMain.cmListExecute(Sender: TObject);
begin
  OpenDialog.InitialDir := ApplicationPath + 'lists';
  OpenDialog.Filter     := 'verses|*.txt';
  if OpenDialog.Execute then
    begin
      ChangeList(OpenDialog.FileName);
      ShowHint(HintList);
    end;
end;

procedure TFormMain.cmListItemExecute(Sender: TObject);
begin
  caption := (Sender as TMenuItem).Text;
end;

{$ifdef macos}
procedure ChangeFontProcedure;
begin
  FormMain.Repaint;
  FormMain.SaveIniFile;
end;
{$endif}

procedure TFormMain.cmFontExecute(Sender: TObject);
begin
  {$ifdef mswindows}
//  if ExecuteFontDialog(LabelMain) then
//    begin
//      SetFontIndex;
//      Rebuild;
//      if HintSize.Enable then ShowHint(HintSize) else ShowHint(HintFont);
//      SaveIniFile;
//    end;
  {$endif}

  {$ifdef macos}
/////////////////////  ExecuteFontDialog(LabelMain, ChangeFontProcedure);
  {$endif}
end;

//-------------------------------------------------------------------------------------------------

procedure TFormMain.cmTimerOffExecute  (Sender: TObject); begin ChangeTimer( 0)    end;
procedure TFormMain.cmTimer10secExecute(Sender: TObject); begin ChangeTimer( 3)    end; //****************************************
procedure TFormMain.cmTimer30secExecute(Sender: TObject); begin ChangeTimer(30)    end;
procedure TFormMain.cmTimer1minExecute (Sender: TObject); begin ChangeTimer( 1*60) end;
procedure TFormMain.cmTimer30minExecute(Sender: TObject); begin ChangeTimer(30*60) end;
procedure TFormMain.cmTimer60minExecute(Sender: TObject); begin ChangeTimer(60*60) end;

//-------------------------------------------------------------------------------------------------

procedure TFormMain.cmTransparentExecute(Sender: TObject);
begin
  miTransparent.IsChecked := not miTransparent.IsChecked;

  if miTransparent.IsChecked then Shape.Opacity := 0
                             else Shape.Opacity := DEFAULT_OPACITY;
  Repaint;
  ShowHint(HintOpacity);
  SaveIniFile;
end;

procedure TFormMain.cmRandomExecute(Sender: TObject);
begin
  List.Randomly := not List.Randomly;
  miRandom.IsChecked := List.Randomly; // иначе заметно как меню появляется с галочкой и потом убирается
  if List.Randomly then Exit;
  List.ResetPos;
  Rebuild;
end;

procedure TFormMain.cmWallpaperExecute(Sender: TObject);
begin
  {$ifdef mswindows} OpenDesktopControl; {$endif}
end;

procedure TFormMain.cmLocalisationExecute(Sender: TObject);
begin
  OpenDialogLang.InitialDir := ApplicationPath + 'localization';
  OpenDialogLang.Filter     := 'language file|*.lng';

  if OpenDialogLang.Execute then
    begin
      Localization := TPath.GetFileNameWithoutExtension(OpenDialogLang.FileName);
      Translate;
    end;
end;

procedure TFormMain.cmExitExecute(Sender: TObject);
begin
  Close;
end;

{$endregion}
//-----------------------------------------------------------------------------------------
                                {$region 'IniFile'}
//-----------------------------------------------------------------------------------------

procedure TFormMain.SaveIniFile;
var
  IniFile : TIniFile;
begin

//  exit;

  IniFile := TIniFile.Create(ConfigFileName);

  IniFile.WriteInteger('App'    ,'Left'        ,Left                                );
  IniFile.WriteInteger('App'    ,'Top'         ,Top                                 );

  IniFile.WriteString ('Font'   ,'Name'        ,LabelMain.Font.Family               );
  IniFile.WriteFloat  ('Font'   ,'Size'        ,LabelMain.Font.Size                 );
  IniFile.WriteBool   ('Font'   ,'Bold'        ,fsBold      in LabelMain.Font.Style );
  IniFile.WriteBool   ('Font'   ,'Italic'      ,fsItalic    in LabelMain.Font.Style );
  IniFile.WriteBool   ('Font'   ,'Underline'   ,fsUnderline in LabelMain.Font.Style );

  IniFile.WriteInteger('Colors' ,'Text'        ,LabelMain.FontColor                 );

  IniFile.WriteFloat  ('Options','Opacity'     ,Shape.Opacity                       );
  IniFile.WriteString ('Options','Localization',Localization                        );
  IniFile.WriteString ('Options','FileList'    ,FileList                            );
  IniFile.WriteString ('Options','PrevList'    ,PrevList                            );
  IniFile.WriteInteger('Options','Position'    ,List.Position                       );
  IniFile.WriteInteger('Options','Timer'       ,MaxTimer                            );
  IniFile.WriteInteger('Options','Width'       ,ShapeWidth                          );
  IniFile.WriteBool   ('Options','Random'      ,List.Randomly                       );

  IniFile.WriteBool   ('Hints'  ,'Click'       ,HintClick.Enable                    );
  IniFile.WriteBool   ('Hints'  ,'Menu'        ,HintMenu.Enable                     );
  IniFile.WriteBool   ('Hints'  ,'List'        ,HintList.Enable                     );
  IniFile.WriteBool   ('Hints'  ,'Size'        ,HintSize.Enable                     );
  IniFile.WriteBool   ('Hints'  ,'Font'        ,HintFont.Enable                     );
  IniFile.WriteBool   ('Hints'  ,'Opacity'     ,HintOpacity.Enable                  );

  IniFile.Free;
end;

//-------------------------------------------------------------------------------------------------

procedure TFormMain.ReadIniFile;
var
  IniFile : TIniFile;
begin
 (*
                  Left := Screen.Size.Width - 480;
                  Top  := 120                    ;
  LabelMain.Font.Size  := 20                     ;
         Shape.Opacity := DEFAULT_OPACITY        ;
          Localization := GetDefaultLanguage     ;
              FileList := GetDefaultList         ;
              PrevList := GetDefaultList         ;
         List.Position := 0                      ;
              MaxTimer := 30                     ;
            ShapeWidth := 13                     ;
         List.Randomly := True                   ;
      HintClick.Enable := True                   ;
       HintMenu.Enable := True                   ;
       HintList.Enable := True                   ;
       HintSize.Enable := True                   ;
       HintFont.Enable := True                   ;
    HintOpacity.Enable := True                   ;
  LabelMain.Font.Style := [];

  exit;
  *)
  IniFile := TIniFile.Create(ConfigFileName);

                  Left := IniFile.ReadInteger('App'    ,'Left'        ,Screen.Size.Width - 480);
                  Top  := IniFile.ReadInteger('App'    ,'Top'         ,120                    );

 LabelMain.Font.Family := IniFile.ReadString ('Font'   ,'Name'        ,'Verdana' {'Comic Sans MS'}        ); SetFontIndex; //*******
////////////////////////////////////// LabelMain.Font.Family := 'Times';
  LabelMain.Font.Size  := IniFile.ReadFloat  ('Font'   ,'Size'        ,20                     );

   LabelMain.FontColor := IniFile.ReadInteger('Colors' ,'Text'        ,LabelMain.FontColor    );

         Shape.Opacity := IniFile.ReadFloat  ('Options','Opacity'     ,DEFAULT_OPACITY        );
          Localization := IniFile.ReadString ('Options','Localization',GetDefaultLanguage     );
              FileList := IniFile.ReadString ('Options','FileList'    ,GetDefaultList         );
              PrevList := IniFile.ReadString ('Options','PrevList'    ,GetDefaultList         );
         List.Position := IniFile.ReadInteger('Options','Position'    ,0                      );
              MaxTimer := IniFile.ReadInteger('Options','Timer'       ,30                     );
            ShapeWidth := IniFile.ReadInteger('Options','Width'       ,13                     );
         List.Randomly := IniFile.ReadBool   ('Options','Random'      ,True                   );

      HintClick.Enable := IniFile.ReadBool   ('Hints'  ,'Click'       ,True                   );
       HintMenu.Enable := IniFile.ReadBool   ('Hints'  ,'Menu'        ,True                   );
       HintList.Enable := IniFile.ReadBool   ('Hints'  ,'List'        ,True                   );
       HintSize.Enable := IniFile.ReadBool   ('Hints'  ,'Size'        ,True                   );
       HintFont.Enable := IniFile.ReadBool   ('Hints'  ,'Font'        ,True                   );
    HintOpacity.Enable := IniFile.ReadBool   ('Hints'  ,'Opacity'     ,True                   );

  LabelMain.Font.Style := [];

  if IniFile.ReadBool('Font','Bold'     ,False) then LabelMain.Font.Style := LabelMain.Font.Style + [fsBold];
  if IniFile.ReadBool('Font','Italic'   ,False) then LabelMain.Font.Style := LabelMain.Font.Style + [fsItalic];
  if IniFile.ReadBool('Font','Underline',False) then LabelMain.Font.Style := LabelMain.Font.Style + [fsUnderline];

  IniFile.Free;
end;
{$endregion}
//-----------------------------------------------------------------------------------------

end.
