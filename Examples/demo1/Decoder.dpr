program Decoder;

{$APPTYPE CONSOLE}

{$R *.res}

uses
  System.SysUtils,
  FH.LIBLTC.LTC in '../../Source/FH.LIBLTC.LTC.pas',
  FH.LIBLTC.TIMECODE in '../../Source/FH.LIBLTC.TIMECODE.pas',
  FH.LIBLTC.DECODER in '../../Source/FH.LIBLTC.DECODER.pas';

const
  apvDef = 1920;
  BUFFER_SIZE = 1024;

function RDTSC: Int64; assembler;
asm
  rdtsc
end;


var
  LTCDecoder : TLTCDecoder;
  apv : integer;
  filename  : string;
  F : File;
  sound : array[0..BUFFER_SIZE-1] of ltcsnd_sample_t;
  n : integer;
  total : integer;
  frame : TLTCFrameExt;
  stime : TSMPTETimecode;
  t1, t2 : int64;
begin

  try
    apv:= apvDef;
    if (ParamCount >= 1) then
    begin
      filename := ParamStr(1);
      if (ParamCount > 2) then
      begin
        try
          apv:= strtoint(ParamStr(2));
        except
          apv:= apvDef;
        end;
      end;
    end else
    begin
      writeln(Format('Usage: %s <filename> [audio-frames-per-video-frame]', [extractfilename(ParamStr(0))]));
      exit;
    end;

    if not fileexists(filename) then
    begin
      writeln(Format('file not find %s', [filename]));
      exit;
    end;
    t1:=RDTSC;

    AssignFile(F, filename);
    try
      try
        Reset(F, 1);
      except
		    writeln(Format('error opening %s', [filename]));
        exit;
      end;
      writeln(Format('* reading from: %s', [filename]));
      total := 0;
      LTCDecoder:=TLTCDecoder.Create();
      LTCDecoder.apv := apv;
      while not Eof(F) do
      begin
        BlockRead(F, sound, BUFFER_SIZE, n);
        LTCDecoder.Write(sound, n, total);
        while (LTCDecoder.Read(frame)) do
        begin
          ltc_frame_to_time(stime, frame.ltc, 1);
          writeln(Format('%02.2d:%02.2d:%02.2d:%02.2d  | %8d %8d', [stime.hours, stime.mins,
               stime.secs, stime.frame, 	frame.off_start, frame.off_end]));
        end;
        total:=total+n;
      end;
    finally
      CloseFile(F);
    end;
    t2:=RDTSC;
    writeln(format('Decode RDTSC=%d',[t2-t1]));

  except
    on E: Exception do
      Writeln(E.ClassName, ': ', E.Message);
  end;
end.
