unit UnitLib;

interface

uses
  Windows, VCL.Graphics, Winapi.ShellAPI, ShFolder,
  FMX.Forms, FMX.Types, FMX.StdCtrls, FMX.Graphics, FMX.Platform.Win,
  System.SysUtils, System.Classes, System.UITypes, System.IOUtils;

// Paths

function ApplicationPath: string;
function ConfigFileName: string;

// Text functions

function LeftChar(s: string): string;
function LeftStr(s: string): string;
function RightStr(s: string): string;

// Language functions

function GetDefaultLanguage: string;
function GetDefaultList: string;

// Windows system functions

function KeyShiftDown: boolean;
procedure HideAppOnTaskbar(Form: TForm);
function GetDeskWallpaper: WideString;
procedure OpenDesktopControl;

// Fonts

procedure GetFonts(List: TStringList);

// Firemonkey functions

procedure OpenUrl(s: string);
procedure WordWrap(L: TLabel; Max: Single);

// Mutex

function CreateMutex(name: PWideChar): boolean;
procedure CloseMutex;

const
  ApplicationName = 'Bible Verse Desktop';

const
  Slash = '\';

const
  CRLF = #10; // #13 + #10

const
  mbRight  = TMouseButton.mbRight;
  mbLeft   = TMouseButton.mbLeft;
  mbMiddle = TMouseButton.mbMiddle;

  fsBold      = TFontStyle.fsBold;
  fsItalic    = TFontStyle.fsItalic;
  fsUnderline = TFontStyle.fsUnderline;

implementation

var
  Mutex : THandle;

//-----------------------------------------------------------------------------
//                                Paths
//-----------------------------------------------------------------------------

function AppDataPath: string; // Roaming
begin
  Result := TPath.GetHomePath + Slash + ApplicationName;
  if not DirectoryExists(Result) then ForceDirectories(Result);
end;

function ApplicationPath: string;
begin
  Result := ExtractFilePath(ParamStr(0));
  if Result.Contains('Debug') then Result := '..\..\';
end;

function ConfigFileName: string;
begin
  Result := AppDataPath + Slash + 'config.cfg';
end;

//-----------------------------------------------------------------------------
//                             Text functions
//-----------------------------------------------------------------------------

function LeftChar(s: string): string;
begin
  Result := '';
  if Length(s) < 1 then Exit;
  Result := Copy(s,1,1);
end;

function LeftStr(s: string): string;
var
  n: integer;
begin
  Result := s;
  n := Pos(chr(09),s);
  if n > 0 then Result := Trim(Copy(s,1,n-1));
end;

function RightStr(s: string): string;
var
  n: integer;
begin
  Result := '';
  n := Pos(chr(09),s);
  if n > 0 then Result := Trim(Copy(s,n+1,Length(s)));
end;

//-----------------------------------------------------------------------------
//                          Language functions
//-----------------------------------------------------------------------------

function GetDefaultLanguage: string;
begin
  Result := 'english';
  case Lo(GetSystemDefaultLangID) of
    LANG_RUSSIAN            : Result := 'russian';
    LANG_CHINESE            : Result := 'chinese-simplified';
  end;
end;

function GetDefaultList: string;
begin
   Result := 'english-kjv';
   Result := 'russian'; // ****************************************************

  case Lo(GetSystemDefaultLangID) of
    LANG_RUSSIAN            : Result := 'russian';
    LANG_CHINESE            : Result := 'chinese-simplified';
    LANG_FRENCH             : Result := 'french';
    LANG_GERMAN             : Result := 'german';
    LANG_UKRAINIAN          : Result := 'ukrainian';
    LANG_THAI               : Result := 'thai';
  end;

  Result := ApplicationPath + 'lists' + Slash + Result + '.txt';
end;

//-----------------------------------------------------------------------------
//                      Windows system functions
//-----------------------------------------------------------------------------

function KeyShiftDown: boolean;
begin
  Result := (GetKeyState(VK_SHIFT) < 0);
end;

function GetDeskWallpaper: WideString;
var
  name: array [0..MAX_PATH] of Char;
const
  SPI_GETDESKWALLPAPER = 115;
begin
  Result := '';
  if SystemParametersInfo(SPI_GETDESKWALLPAPER, MAX_PATH, @name, 0) then Result := WideString(name);
end;

procedure HideAppOnTaskbar (Form : TForm);
var
  AppHandle : HWND;
begin
  AppHandle := GetParent(FmxHandleToHWND(Form.Handle));
  ShowWindow(AppHandle, SW_HIDE);
  SetWindowLong(AppHandle, GWL_EXSTYLE, GetWindowLong(AppHandle, GWL_EXSTYLE) or WS_EX_TOOLWINDOW);
end;

//-----------------------------------------------------------------------------

procedure OpenDesktopControl;
begin
  ShellExecute(0,'open','rundll32.exe','shell32.dll, Control_RunDLL desk.cpl desk,@Desktop','',SW_SHOW);
end;

//-----------------------------------------------------------------------------
//                                  Firemonkey functions
//-----------------------------------------------------------------------------

procedure OpenUrl(s: String);
begin
  ShellExecute(0,'open',PChar(s),'','',SW_SHOW)
end;

//-----------------------------------------------------------------------------

procedure WordWrap(L: TLabel; Max: Single);
var
  Text : String;
     s : String;
   i,k : integer;
     c : char;
begin
  Text := L.Text;
  L.Text := '*'; // '' приводит к ошибке

  k := 0;
  for i:=1 to Text.Length do
    begin
      c := Text[i];
      if c = ' ' then k := i;
      if i = 1 then L.Text := c else L.Text := L.Text + c;
      // L.Text := L.Text + c; // вариант до бага

      if (L.Width > Max) and (k > 0) then
        begin
          s := L.Text;
          s[k] := CRLF;
          L.Text := s;
          k := 0;
        end;
    end;
end;

//-----------------------------------------------------------------------------
                                {$region 'Fonts'}
//-----------------------------------------------------------------------------

function EnumFontsProc(var LogFont: TLogFont; var TextMetric: TTextMetric; FontType: Integer; Data: Pointer): Integer; stdcall;
var
  S: TStrings;
  Temp: string;
begin
  S := TStrings(Data);
  Temp := LogFont.lfFaceName;
  if (S.Count = 0) or (AnsiCompareText(S[S.Count-1], Temp) <> 0) then S.Add(Temp);
  Result := 1;
end;

procedure GetFonts(List: TStringList);
var
  LogFont : TLogFont;
       DC : HDC;
begin
  DC := GetDC(0);
  try
    List.Clear;
    FillChar(LogFont, sizeof(LogFont), 0);
    LogFont.lfCharset := DEFAULT_CHARSET;
    EnumFontFamiliesEx(DC, LogFont, @EnumFontsProc, LongInt(List), 0);
    List.Sort;
  finally
    ReleaseDC(0, DC);
  end;
end;
{$endregion}

//-----------------------------------------------------------------------------
                             {$region 'Mutex'}
//-----------------------------------------------------------------------------

function GetWinInfo(h: HWND): string;
var
  temp : PWideChar;
begin
  GetMem(temp,255);
  GetWindowText(h,temp,255);
  Result := temp;
  FreeMem(temp);
end;

{---}

procedure GetWinTree;
var
  hw : HWND;
begin
  hw := Windows.FindWindow(nil,nil);           // Loop through all Top Level Windows
  while hw > 32 do
    begin
      if GetWinInfo(hw) = ApplicationName then
        begin
          ShowWindow(hw,SW_SHOW);
          SetForegroundWindow(hw);
        end;
      hw:=GetWindow(hw,GW_HWNDNEXT);   // Get the next window
    end;
  end;

function CreateMutex(name: PWideChar): boolean;
begin
  Mutex := Windows.CreateMutex(nil, False, name);
  Result := GetLastError <> ERROR_ALREADY_EXISTS;
  if not Result then GetWinTree;
end;

procedure CloseMutex;
begin
  CloseHandle(Mutex);
end;
{$endregion}

end.

