program BibleVerseDesktop;

uses
  Math,
  FMX.Forms,
  UnitMain in 'UnitMain.pas' {FormMain},
  UnitAbout in 'UnitAbout.pas' {FormAbout},
  UnitHint in 'UnitHint.pas' {FormHint},
  UnitLang in 'UnitLang.pas',
  UnitLib in 'UnitLib.pas',
  UnitList in 'UnitList.pas';

{$R *.res}

begin
  {$ifdef mswindows} if not CreateMutex('BibleVerseMutexName') then Exit; {$endif}

  {$ifdef mswindows} SetExceptionMask(exAllArithmeticExceptions); {$endif}
  // отключаем ошибку Floating point division by zero
  // которая появляется в Windows 7 под Virtual Box

  Application.Initialize;
  Application.Title := 'Bible Verse Desktop';
  Application.CreateForm(TFormMain, FormMain);
  Application.CreateForm(TFormAbout, FormAbout);
  Application.CreateForm(TFormHint, FormHint);
  Application.Run;

  {$ifdef mswindows} CloseMutex; {$endif}
end.

