unit Unit1;

interface

uses
  Winapi.Windows, Winapi.Messages, System.SysUtils, System.Variants, System.Classes, Vcl.Graphics,
  Vcl.Controls, Vcl.Forms, Vcl.Dialogs, uAudioFH, MMSystem, Vcl.StdCtrls, math, ltc, ltcDecoder,
  GradProgress, Vcl.Menus, Vcl.ComCtrls, DateUtils;

  const
  Buffers            = 4;
  BufferLengthInt    = 8192;
  DtmfFreq: 	    array[0..7] of Double = (697, 770, 852, 941, 1209, 1336, 1477, 1633);
  BUFFER_SIZE = 1024;

type
  TForm1 = class(TForm)
    Memo1: TMemo;
    OpenDialog1: TOpenDialog;
    GroupBox1: TGroupBox;
    ComboBox1: TComboBox;
    ComboBox2: TComboBox;
    Button1: TButton;
    Button2: TButton;
    GroupBox2: TGroupBox;
    GradProgress1: TGradProgress;
    Label1: TLabel;
    StatusBar1: TStatusBar;
    MainMenu1: TMainMenu;
    exit1: TMenuItem;
    N1: TMenuItem;
    exit2: TMenuItem;
    GroupBox3: TGroupBox;
    Label2: TLabel;
    Button3: TButton;
    procedure Button1Click(Sender: TObject);
    procedure AudioFHOnDataBuffer(Data: Pointer; Size: Longint);
    procedure Button2Click(Sender: TObject);
    procedure LTCDecoderOnDebug(const S: string);
    procedure LTCDecoderOnRead(Sender: TObject; timecode : TDateTime);
    procedure FormCreate(Sender: TObject);
    Procedure WaveInOnError(Sender: TObject);
    procedure  WaveInOnVolume(const PeakLevel :TPeakLevel);
    procedure FormDestroy(Sender: TObject);
    procedure ComboBox1Change(Sender: TObject);
    procedure ComboBox2Change(Sender: TObject);
    procedure exit2Click(Sender: TObject);
    procedure FormClose(Sender: TObject; var Action: TCloseAction);
    procedure Button3Click(Sender: TObject);
  private
    { Private declarations }
  public
    { Public declarations }
  end;

var
  Form1: TForm1;
  FAudioFH        : TAudioFH;
  AudioFHInfo     : TAudioFHInfo;
  WFormat         : TWaveFormatEx;

  LTCDecoder       : TLTCDecoder;
  prevframe: word;


  tFreeRun : UINT; //идентификатор таймера
  lastPeriod : cardinal;
  FreeRunDate : TDateTime;
  LTCDate : TDateTime;

  differenceDate : TDateTime;

    FAvgFirstMax: Integer;
    FAvgFirstTime: Cardinal;
    FAvgSecondMax: Integer;
    FAvgSecondTime: Cardinal;


  CritSect : TRTLCriticalSection;
implementation



{$R *.dfm}

function timeGetMinPeriod(): DWORD;
var  time: TTimeCaps;
begin
  timeGetDevCaps(Addr(time), SizeOf(time));
  timeGetMinPeriod := time.wPeriodMin;
end;

function timeGetMaxPeriod(): Cardinal;
var time: TTimeCaps;
begin
  timeGetDevCaps(Addr(time), SizeOf(time));
  timeGetMaxPeriod := time.wPeriodMax;
end;

function timeSetTimerPeriod(period: Cardinal): Boolean;
begin
  if timeBeginPeriod(period) = TIMERR_NOERROR then
    begin
      //Сохраним значение для восстановления состояния таймера
      lastPeriod := period;
      timeSetTimerPeriod := True;
    end
  else//Неудача
    timeSetTimerPeriod := False;
end;


function timeRestoreTimerPeriod(): Boolean;
begin
  lastPeriod := timeEndPeriod(TIMERR_NOERROR);
end;

procedure TimerProc(uTimerID, uMessage: UINT; dwUser, dw1, dw2: DWORD) stdcall;
var curPos: TPoint;
  delta:real;
  h, m, s, ms, f : word;
begin
 // EnterCriticalSection(CritSect);

    FreeRunDate:= IncMilliSecond(FreeRunDate, 40);

    decodetime(FreeRunDate, h,m,s,ms);
    try
      f:=ms div 40;
    except
      f:=0;
    end;

  // Form1.label2.Caption:= format('%02.2d:%02.2d:%02.2d:%02.2d',[h,m,s,f]);
  //LeaveCriticalSection(CritSect);

end;


procedure  TForm1.WaveInOnError(Sender: TObject);
begin
    Application.MessageBox(PChar(FAudioFH.LastErrorText), 'Ошибка', MB_ICONERROR);
end;

procedure  TForm1.WaveInOnVolume(const PeakLevel :TPeakLevel);
begin
  form1.GradProgress1.Position:= PeakLevel.DB_CH[0];
end;

procedure TForm1.Button1Click(Sender: TObject);
begin
{
with WFormat do begin
          wFormatTag      := WAVE_FORMAT_PCM;
          wBitsPerSample  := 16;
          nChannels       := 1;
          nSamplesPerSec  := 44100;
          nBlockAlign     := (nChannels * wBitsPerSample) div 8;
          nAvgBytesPerSec := (nChannels * nSamplesPerSec * wBitsPerSample) div 8;
          cbSize          := 0;
end;}

  LTCDecoder:=TLTCDecoder.Create(WFormat.nSamplesPerSec div 25, 32);
  LTCDecoder.OnRead:=LTCDecoderOnRead;

  try
    FAudioFH.SetWaveFormat(addr(WFormat));
    FAudioFH.OnError:=WaveInOnError;
    FAudioFH.BufferLength:=BUFFER_SIZE;
    if FAudioFH.open(combobox1.ItemIndex) then
    begin
      FAudioFH.OnVolume:=WaveInOnVolume;
      FAudioFH.OnDataBuffer:=AudioFHOnDataBuffer;
      //FAudioFH.OnDTMF:=WaveInOnDTMF;
     // FAudioFH.OpenDTMFDetect(1024,false);
    end;
  except
    on E: Exception do begin
      Application.MessageBox(PChar('procedure FormCreate:'#13#10 + E.Message), 'Ошибка', MB_ICONERROR);
    end;
  end;
end;


procedure TForm1.Button2Click(Sender: TObject);
begin
 // FAudioFH.CloseDTMFDetect;
  FAudioFH.Close;
  LTCDecoder.Destroy;
end;

procedure TForm1.Button3Click(Sender: TObject);
var
  h,m,s,ms, f : word;
  _ltc, _freerun: TDateTime;
begin

  _ltc:=LTCDate;
  _freerun:=FreeRunDate;

  DecodeTime(differenceDate, h,m,s,ms);
  try
    f:=ms div 40;
  except
    f:=0;
  end;
  memo1.Lines.Add(format('Difference LTC and FreeRun: %02.2d:%02.2d:%02.2d:%02.2d',[h,m,s,f]));


  DecodeTime(abs(_ltc-_freerun), h,m,s,ms);
  try
    f:=ms div 40;
  except
    f:=0;
  end;
  memo1.Lines.Add(format('Compare result: %02.2d:%02.2d:%02.2d:%02.2d',[h,m,s,f]));

  DecodeTime(abs(_ltc-_freerun-differenceDate), h,m,s,ms);
  try
    f:=ms div 40;
  except
    f:=0;
  end;
  memo1.Lines.Add(format('Compare result with diff: %02.2d:%02.2d:%02.2d:%02.2d',[h,m,s,f]));


  DecodeTime(_ltc, h,m,s,ms);
  try
    f:=ms div 40;
  except
    f:=0;
  end;
  memo1.Lines.Add(format('LTC: %02.2d:%02.2d:%02.2d:%02.2d',[h,m,s,f]));



  DecodeTime(_freerun, h,m,s,ms);
  try
    f:=ms div 40;
  except
    f:=0;
  end;
  memo1.Lines.Add(format('FreeRun: %02.2d:%02.2d:%02.2d:%02.2d',[h,m,s,f]));

  memo1.Lines.Add('');
end;

procedure TForm1.LTCDecoderOnDebug(const S: string);
begin
  memo1.Lines.Add(s);
end;

procedure TForm1.LTCDecoderOnRead(Sender: TObject; timecode : TDateTime);

var
  h,m,s,ms,f : word;
begin
  if differenceDate=0 then differenceDate:=abs(timecode-FreeRunDate);

  LTCDate:=timecode;
  decodetime(timecode, h,m,s,ms);
  try
    f:=ms div 40;
  except
    f:=0;
  end;

  if ((f-prevframe=1) or (f=0)) then
  begin
    label1.Caption:= format('%02.2d:%02.2d:%02.2d:%02.2d',[h,m,s,f]);

  end else
  begin
    memo1.Lines.Add('');
    memo1.Lines.Add(format('%s - %02.2d:%02.2d:%02.2d:%02.2d',[formatdatetime('yyyy-mm-dd hh:mm:ss.zzz', now),h,m,s,f]));
    memo1.Lines.Add(format('%s - %02.2d:%02.2d:%02.2d:%02.2d',[formatdatetime('yyyy-mm-dd hh:mm:ss.zzz', now),h,m,s,  prevframe]));
    memo1.Lines.Add('');
  end;

  prevframe:=f;
end;

procedure TForm1.ComboBox1Change(Sender: TObject);
var
  i: integer;
  WaveDeviceInFormat: TWaveDeviceFormats;
begin
  combobox2.Items.Clear;
  WaveDeviceInFormat:= AudioFHInfo.GetInDeviceFormats(ComboBox1.ItemIndex);
  for i := 0 to Length(GetPCMFormat)-1 do
    if GetPCMFormat[i] in WaveDeviceInFormat then combobox2.Items.Add(GetPCMFormatToStr[i]);
  combobox2.ItemIndex:=0;
  combobox2.OnChange(self);
end;

procedure TForm1.ComboBox2Change(Sender: TObject);
begin
  FAudioFH.SetPCMAudioFormatS(addr(WFormat),StrToPCMFormat(combobox2.Items.Strings[combobox2.ItemIndex]));
end;

procedure TForm1.exit2Click(Sender: TObject);
begin
  close;
end;

function ConvertU16ToS8BIT(const sample: word): ShortInt;
begin
  result:=((sample div 256) - 128) and $FF;
end;

function ConvertS16ToS8BIT(const sample: SmallInt): ShortInt;
begin
  result:=(sample div 256) and $FF;
end;

function ConvertS16ToU8BIT(const sample: SmallInt): byte;
begin
  result:=((sample div 256) + 128) and $FF;
end;

function ConvertU16ToU8BIT(const sample: word): byte;
begin
  result:=(sample div 256) and $FF;
end;


procedure TForm1.AudioFHOnDataBuffer(Data: Pointer; Size: Longint);
var
  total : int64;
  sound : array[0..BUFFER_SIZE-1] of ltcsnd_sample_t;

  buffU8BIT  : array of Byte;
  buffU16BIT  : array of Word;
  i,n : integer;

  PCMFormat : TPCMFormat;
  bytePerSample   : integer;
  sampleCount     : integer;
begin


  PCMFormat:=FAudioFH.GetPCMAudioFormat(addr(WFormat));
  case PCMFormat of
    Mono8Bit8000Hz,
    Stereo8bit8000Hz,
    Mono8bit11025Hz,
    Stereo8bit11025Hz,
    Mono8bit22050Hz,
    Stereo8bit22050Hz,
    Mono8bit44100Hz,
    Stereo8bit44100Hz,
    Mono8bit48000Hz   : bytePerSample:=1;

    Mono16bit8000Hz,
    Stereo16bit8000Hz,
    Mono16bit11025Hz,
    Stereo16bit11025Hz,
    Mono16bit22050Hz,
    Stereo16bit22050Hz,
    Mono16bit44100Hz,
    Stereo16bit44100Hz,
    Mono16bit48000Hz,
    Stereo16bit48000Hz  : bytePerSample:=2;
  end;

  if size>0 then
    sampleCount:=Size div bytePerSample;

  SetLength(buffU8BIT, sampleCount);

  case bytePerSample of
    2:
    begin
      SetLength(buffU16BIT, sampleCount);
      move(pByte(Data)[0], buffU16BIT[0], Size);
      for I := 0 to sampleCount-1 do
        buffU8BIT[i]:=ConvertU16ToU8BIT(buffU16BIT[i]);
    end;
    1:
    begin
      move(pByte(Data)[0], buffU8BIT[0], Size);
    end;
  end;


  total:=0;
  n:=0;
  for I := 0 to (sampleCount-1) div BUFFER_SIZE  do
  begin
    try
    if sampleCount-total< BUFFER_SIZE then
      n:=sampleCount-total else n:=BUFFER_SIZE;
      move(buffU8BIT[total], sound[0], n);
      LTCDecoder.Write(sound, n, total);
    finally
      inc(total, n);
    end;
  end


 { total:=0;
  n:=0;
  for I := 0 to (size-1) div BUFFER_SIZE  do
  begin
    try
    if Size-total< BUFFER_SIZE then
      n:=Size-total else n:=BUFFER_SIZE;
      copymemory(@sound, @pByte(Data)[total], n);
      LTCDecoder.Write(sound, n, total);
    finally
      inc(total, n);
    end;
  end;}
end;


procedure TForm1.FormClose(Sender: TObject; var Action: TCloseAction);
begin
  timeKillEvent(tFreeRun);
end;

procedure TForm1.FormCreate(Sender: TObject);
var
  i : integer;
begin
  FAudioFH:= TAudioFH.Create;
  combobox1.Clear;
  for I := 0 to AudioFHInfo.GetNumInDevs-1 do
  begin
    combobox1.Items.Add(AudioFHInfo.GetInDeviceName(i));
  end;
  combobox1.ItemIndex:=0;
  combobox1.OnChange(self);


  differenceDate:=0;
  (* Start freerun timer *)
  timeSetTimerPeriod(40);
  FreeRunDate:=0;
  tFreeRun:=timeSetEvent(40, timeGetMinPeriod, @TimerProc, 0, TIME_CALLBACK_FUNCTION or TIME_PERIODIC);
end;

procedure TForm1.FormDestroy(Sender: TObject);
begin
  FAudioFH.Destroy;
end;


initialization
  InitializeCriticalSection(CritSect);

finalization
  DeleteCriticalSection(CritSect);

end.
