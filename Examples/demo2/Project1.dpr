program Project1;

uses
  Vcl.Forms,
  FH.LIBLTC.LTC in '../../Include/FH.LIBLTC.LTC.pas',
  FH.LIBLTC.TIMECODE in '../../Include/FH.LIBLTC.TIMECODE.pas',
  FH.LIBLTC.DECODER in '../../Include/FH.LIBLTC.DECODER.pas';
  CalcFrq in 'CalcFrq.pas',
  uAudioFH in 'uAudioFH.pas',
  Unit1 in 'Unit1.pas' {Form1},
  Vcl.Themes,
  Vcl.Styles;

{$R *.res}

begin
  Application.Initialize;
  Application.MainFormOnTaskbar := True;
  Application.CreateForm(TForm1, Form1);
  Application.Run;
end.
