unit UnitList;

interface

uses
  System.SysUtils, System.Types, System.UITypes, System.Classes, System.Variants;

type
  TVerseList = class(TStringList)
  public
    Randomly  : boolean;
    Position  : integer;

    ListName  : String;
    Abbr      : String;
    Language  : String;
    TextSize  : integer;
    Alignment : TAlignment;
    Info      : String;

    constructor Create;
    procedure LoadFile(FileName : String);
    procedure Next;
    procedure ResetPos;
    procedure Undo;
    function GetVerse : String;
    function GetSign : String;
    function GetQuote : String;
    destructor Destroy; override;
  private
    PrevPosition  : integer;
    procedure CleanTags;
    procedure Tag(s: String);
    procedure AddInfo(s: String);
  end;

var
  List: TVerseList;

implementation

uses UnitLib, UnitLang;

//-----------------------------------------------------------------------------------------

constructor TVerseList.Create;
begin
  Position := 0;
  PrevPosition := 0;
  Randomly := True;

  CleanTags;
  inherited Create;
end;

//-----------------------------------------------------------------------------------------

destructor TVerseList.Destroy;
begin
  inherited Destroy;
end;

//-----------------------------------------------------------------------------------------

procedure TVerseList.CleanTags;
begin
  ListName  := '';
  Abbr      := '';
  Language  := '';
  Info      := '';
  Alignment := taLeftJustify;
end;

//-----------------------------------------------------------------------------------------

function AlignmentFromLang(lang: String): TAlignment;
begin
  Result := taLeftJustify;
  if lang = 'arabic' then Result := taRightJustify;
  if lang = 'hebrew' then Result := taRightJustify;
end;

//-----------------------------------------------------------------------------------------

procedure TVerseList.Tag(s: String);
var
  l,r : String;
begin
  l := LeftStr(s);
  r := RightStr(s);

  if l = '#name' then ListName  := r;
  if l = '#abbr' then Abbr      := r;

  if l = '#language'  then
    begin
      Language  := LowerCase(r);
      Alignment := AlignmentFromLang(Language);
    end;
end;

//-----------------------------------------------------------------------------------------

procedure TVerseList.AddInfo(s: String);
begin
  System.Delete(s,1,1);
  Info := Info + s + CRLF;
end;

//-----------------------------------------------------------------------------------------

procedure TVerseList.LoadFile(FileName : String);
var
  Utf8List : TStringList;
  s : String;
  i : integer;
begin
  if not FileExists(FileName) then Exit;

  Utf8List := TStringList.Create;
  Utf8List.LoadFromFile(FileName);

  Clear;
  CleanTags;

  for i := 0 to Utf8List.Count-1 do
    begin
      s := Trim(Utf8List[i]);

      if Length(s) > 5 then
        if LeftChar(s) = '#' then Tag(s) else
          if LeftChar(s) = ';' then AddInfo(s) else Add(s);
    end;

  if Position > (Count-1) then ResetPos;

  Utf8List.Free;
// if Count = 0 then ...
end;

//-----------------------------------------------------------------------------------------

procedure TVerseList.ResetPos;
begin
  Position := 0;
end;

//-----------------------------------------------------------------------------------------

procedure TVerseList.Next;
begin
  if Count = 0 then Exit;

  PrevPosition := Position;
  if Randomly then Position := Random(Count)
              else Position := Position + 1;

  if Position > (Count-1) then Position := 0;
end;

//-----------------------------------------------------------------------------------------

procedure TVerseList.Undo;
begin
  if Randomly then Position := PrevPosition else
    if Position > 0 then Position := Position - 1;
end;

//-----------------------------------------------------------------------------------------

function TVerseList.GetVerse : String;
begin
  Result := 'error: file not found';
  if Count = 0 then Exit;
  while (Pos('***',Strings[Position]) > 0) do Next;
  Result := LeftStr(Strings[Position]);
//if Length(w) < 5 then w := 'error: short string';
end;

//-----------------------------------------------------------------------------------------

function TVerseList.GetSign : String;
begin
  Result := '';
  if Count = 0 then Exit;
  Result := RightStr(Strings[Position]);
  if Abbr <> '' then Result := Result + '(' + Utf8ToString(Abbr) + ')';
end;

//-----------------------------------------------------------------------------------------

function TVerseList.GetQuote : String;
begin
  Result := GetVerse + ' ' + GetSign;
end;

//-----------------------------------------------------------------------------------------

initialization
  List := TVerseList.Create;

finalization
  List.Free;

end.
