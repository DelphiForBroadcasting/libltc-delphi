program Project1;

uses
  System.StartUpCopy,
  FMX.Forms,
  ctypes in '../ctypes.pas',
  portaudio in '../portaudio.pas',
  Unit1 in 'Unit1.pas' {Form1};

{$R *.res}

begin
  Application.Initialize;
  Application.CreateForm(TForm1, Form1);
  Application.Run;
end.
