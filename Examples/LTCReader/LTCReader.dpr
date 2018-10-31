program LTCReader;



uses
  System.StartUpCopy,
  FMX.Forms,
  portaudio in '..\..\Include\portaudio-delphi\Include\portaudio.pas',
  FH.LIBLTC.LTC in '..\..\Source\FH.LIBLTC.LTC.pas',
  FH.LIBLTC.TIMECODE in '..\..\Source\FH.LIBLTC.TIMECODE.pas',
  FH.LIBLTC.DECODER in '..\..\Source\FH.LIBLTC.DECODER.pas',
  FH.LIBLTC in '..\..\Source\FH.LIBLTC.pas',
  Unit1 in 'Unit1.pas' {Form1},
  Unit2 in 'Unit2.pas' {Form2};

{$R *.res}

begin
  Application.Initialize;
  Application.CreateForm(TForm1, Form1);
  Application.CreateForm(TForm2, Form2);
  Application.Run;
end.
