program LTCReader;



uses
  System.StartUpCopy,
  FMX.Forms,
  ctypes in '..\..\lib\portaudio\src\ctypes.pas',
  portaudio in '..\..\lib\portaudio\src\portaudio.pas',
  FH.LIBLTC.LTC in '..\..\Include\FH.LIBLTC.LTC.pas',
  FH.LIBLTC.TIMECODE in '..\..\Include\FH.LIBLTC.TIMECODE.pas',
  FH.LIBLTC.DECODER in '..\..\Include\FH.LIBLTC.DECODER.pas',
  FH.LIBLTC in '..\..\Include\FH.LIBLTC.pas',
  Unit1 in 'Unit1.pas' {Form1},
  Unit2 in 'Unit2.pas' {Form2};

{$R *.res}

begin
  Application.Initialize;
  Application.CreateForm(TForm1, Form1);
  Application.CreateForm(TForm2, Form2);
  Application.Run;
end.
