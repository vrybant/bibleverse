unit UnitLang;

interface

uses
  System.UITypes, IniFiles;

type
  TLang = class(TIniFile)
  public
    constructor Create;
  end;

const
  CharsetMax   = 15;

var
  Lang : TLang;
  Localization : String;

function T(const id : string): String;

implementation

uses
   UnitLib;

//-----------------------------------------------------------------------------------------

constructor TLang.Create;
begin
  inherited Create(ApplicationPath + 'localization' + Slash + Localization + '.lng');
end;

//-----------------------------------------------------------------------------------------

function T(const id : string): String;
begin
  Result := Utf8ToString( Lang.ReadString('interface',id,id) );
end;

//-----------------------------------------------------------------------------------------

end.

