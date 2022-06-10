unit UnitFontDlg;

interface

uses
  FMX.StdCtrls, FMX.Graphics;

function ExecuteFontDialog(TextControl: TPresentedTextControl): boolean;

implementation

uses
  VCL.Graphics,  Vcl.Dialogs, System.UITypes;

function ColorToAlphaColor(Color: TColor): TAlphaColor;
begin
  case Color of
    clAqua       : Result := TAlphaColorRec.Aqua;
    clBlack      : Result := TAlphaColorRec.Black;
    clBlue       : Result := TAlphaColorRec.Blue;
    clCream      : Result := TAlphaColorRec.Cream;
    clFuchsia    : Result := TAlphaColorRec.Fuchsia;
    clGray       : Result := TAlphaColorRec.Gray;
    clGreen      : Result := TAlphaColorRec.Green;
    clLime       : Result := TAlphaColorRec.Lime;
    clMaroon     : Result := TAlphaColorRec.Maroon;
    clMedGray    : Result := TAlphaColorRec.MedGray;
    clMoneyGreen : Result := TAlphaColorRec.MoneyGreen;
    clNavy       : Result := TAlphaColorRec.Navy;
    clOlive      : Result := TAlphaColorRec.Olive;
    clPurple     : Result := TAlphaColorRec.Purple;
    clRed        : Result := TAlphaColorRec.Red;
    clSilver     : Result := TAlphaColorRec.Silver;
    clSkyBlue    : Result := TAlphaColorRec.SkyBlue;
    clTeal       : Result := TAlphaColorRec.Teal;
    clWhite      : Result := TAlphaColorRec.White;
    clYellow     : Result := TAlphaColorRec.Yellow;
  else
     Result := TAlphaColorRec.White;
  end;
end;

function AlphaColorToColor(Color: TAlphaColor): TColor;
begin
  case Color of
    TAlphaColorRec.Aqua       : Result := clAqua;
    TAlphaColorRec.Black      : Result := clBlack;
    TAlphaColorRec.Blue       : Result := clBlue;
    TAlphaColorRec.Cream      : Result := clCream;
    TAlphaColorRec.Fuchsia    : Result := clFuchsia;
    TAlphaColorRec.Gray       : Result := clGray;
    TAlphaColorRec.Green      : Result := clGreen;
    TAlphaColorRec.Lime       : Result := clLime;
    TAlphaColorRec.Maroon     : Result := clMaroon;
    TAlphaColorRec.MedGray    : Result := clMedGray;
    TAlphaColorRec.MoneyGreen : Result := clMoneyGreen;
    TAlphaColorRec.Navy       : Result := clNavy;
    TAlphaColorRec.Olive      : Result := clOlive;
    TAlphaColorRec.Purple     : Result := clPurple;
    TAlphaColorRec.Red        : Result := clRed;
    TAlphaColorRec.Silver     : Result := clSilver;
    TAlphaColorRec.SkyBlue    : Result := clSkyBlue;
    TAlphaColorRec.Teal       : Result := clTeal;
    TAlphaColorRec.White      : Result := clWhite;
    TAlphaColorRec.Yellow     : Result := clYellow;
  else
     Result := clWhite;
  end;
end;

procedure FontToVclFont(Font: FMX.Graphics.TFont; VclFont: Vcl.Graphics.TFont);
begin
  VclFont.Name  := Font.Family;
  VclFont.Size  := Round(Font.Size);
  VclFont.Style := [];

  if fsBold      in Font.Style then VclFont.Style := VclFont.Style + [Vcl.Graphics.TFontStyle.fsBold];
  if fsItalic    in Font.Style then VclFont.Style := VclFont.Style + [Vcl.Graphics.TFontStyle.fsItalic];
  if fsUnderline in Font.Style then VclFont.Style := VclFont.Style + [Vcl.Graphics.TFontStyle.fsUnderline];
end;

procedure VclFontToFont(VclFont: Vcl.Graphics.TFont; Font: FMX.Graphics.TFont);
begin
  Font.Family := VclFont.Name;
  Font.Size   := Round(VclFont.Size);
  Font.Style  := [];

  if Vcl.Graphics.TFontStyle.fsBold      in VclFont.Style then Font.Style := Font.Style + [fsBold];
  if Vcl.Graphics.TFontStyle.fsItalic    in VclFont.Style then Font.Style := Font.Style + [fsItalic];
  if Vcl.Graphics.TFontStyle.fsUnderline in VclFont.Style then Font.Style := Font.Style + [fsUnderline];
end;

function ExecuteFontDialog(TextControl: TPresentedTextControl): boolean;
var
  FontDialog : TFontDialog;
begin
  FontDialog := TFontDialog.Create(nil);
  FontToVclFont(TextControl.Font, FontDialog.Font);
  FontDialog.Font.Color := AlphaColorToColor(TextControl.FontColor);
  Result := FontDialog.Execute;

  if Result then
    begin
      VclFontToFont(FontDialog.Font, TextControl.Font);
      TextControl.FontColor := ColorToAlphaColor(FontDialog.Font.Color);
    end;

  FontDialog.Free;
end;

end.

