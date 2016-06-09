unit uAudioFH;

interface

uses
  Windows, MMSystem, Classes, Forms, SysUtils, CalcFrq;

  {$M+}

const
  Buffers            = 4;
  BufferLengthInt    = 8192;
  DtmfFreq: 	    array[0..7] of Double = (697, 770, 852, 941, 1209, 1336, 1477, 1633);
  OutRangeFreq: 	array[0..7] of Double = (733.5, 811, 896.5, 1075, 1272.5, 1406.5, 1555, 1711);
  Encoding2x8   	= '....123A....456B....789C....*0#D147*....2580....369#....ABCD....';


type
  TData8 = array [0..BufferLengthInt-1 ] of byte;
  PData8 = ^TData8;
  TData16 = array [0..BufferLengthInt-1 ] of smallint;
  PData16 = ^TData16;
  T24Byte = array [0..2] of byte;
  P24Byte = ^T24Byte;
  TData24 = array [0..BufferLengthInt-1 ] of P24Byte;
  PData24 = ^TData24;
  TData32 = array [0..BufferLengthInt-1 ] of integer;
  PData32 = ^TData32;

Type
TPeakLevel = packed record
  DB_CH: array[0..15] of integer
end;

type
  TDataEvent = procedure (Data: Pointer; Size: Longint) of object;
  TDataVolume = procedure (const PeakLevel :TPeakLevel) of object;
  TDataDTMF = Procedure (dtmf: integer) of object;
  TWaveAudioEvent = procedure(Sender: TObject) of object;

 // Wave Audio Exceptions
  EWaveAudioError = class(Exception);
  EWaveAudioSysError = class(EWaveAudioError);
  EWaveAudioInvalidOperation = class(EWaveAudioError);

    // Standard PCM Audio Format
  TPCMChannel = (cMono, cStereo);
  TPCMSamplesPerSec = (ss8000Hz, ss11025Hz, ss22050Hz, ss44100Hz, ss48000Hz);
  TPCMBitsPerSample = (bs8Bit, bs16Bit);
  TPCMFormat = (nonePCM, Mono8Bit8000Hz, Stereo8bit8000Hz, Mono16bit8000Hz,
    Stereo16bit8000Hz, Mono8bit11025Hz, Stereo8bit11025Hz, Mono16bit11025Hz,
    Stereo16bit11025Hz, Mono8bit22050Hz, Stereo8bit22050Hz, Mono16bit22050Hz,
    Stereo16bit22050Hz, Mono8bit44100Hz, Stereo8bit44100Hz, Mono16bit44100Hz,
    Stereo16bit44100Hz, Mono8bit48000Hz, Stereo8bit48000Hz, Mono16bit48000Hz,
    Stereo16bit48000Hz);

  // Wave Device Supported PCM Formats
  TWaveDeviceFormats = set of TPCMFormat;

 Const
  GetPCMFormat  :  array[0..20] of TPCMFormat = (nonePCM, Mono8Bit8000Hz, Stereo8bit8000Hz, Mono16bit8000Hz,
    Stereo16bit8000Hz, Mono8bit11025Hz, Stereo8bit11025Hz, Mono16bit11025Hz,
    Stereo16bit11025Hz, Mono8bit22050Hz, Stereo8bit22050Hz, Mono16bit22050Hz,
    Stereo16bit22050Hz, Mono8bit44100Hz, Stereo8bit44100Hz, Mono16bit44100Hz,
    Stereo16bit44100Hz, Mono8bit48000Hz, Stereo8bit48000Hz, Mono16bit48000Hz,
    Stereo16bit48000Hz);

  GetPCMFormatToStr  :  array[0..20] of string = ('nonePCM', '8000Hz, Mono, 8Bit', '8000Hz, Stereo, 8bit', '8000Hz, Mono, 16bit',
    '8000Hz, Stereo, 16bit', '11025Hz, Mono,8bit', '11025Hz, Stereo, 8bit', '11025Hz, Mono, 16bit',
    '11025Hz, Stereo, 16bit', '22050Hz, Mono, 8bit', '22050Hz, Stereo, 8bit', '22050Hz, Mono, 16bit',
    '22050Hz, Stereo, 16bit', '44100Hz, Mono, 8bit', '44100Hz, Stereo, 8bit', '44100Hz, Mono, 16bit',
    '44100Hz, Stereo, 16bit', '48000Hz, Mono, 8bit', '48000Hz, Stereo, 8bit', '48000Hz, Mono, 16bit',
    '48000Hz, Stereo, 16bit');


Type
   TAudioFHInfo = class
  private

  public
    constructor Create; overload;
    function GetInDeviceFormats(ADeviceID: DWORD): TWaveDeviceFormats;
    Function ChackInFormat(device : integer; WaveFormat :  PWaveFormatEx):boolean;
    function ValidateInDeviceID(ADeviceID: DWORD): MMRESULT;
    function GetNumInDevs: integer;
    function GetInDeviceName(ADeviceID:integer): String;
    function ValidateOutDeviceID(ADeviceID: DWORD): MMRESULT;
    function GetNumOutDevs: integer;
    function GetOutDeviceName(ADeviceID:integer): String;
  protected

  published
    //property LastErrorText: String read GetLastErrorText;
  end;


  TAudioFH = class
  private
    FPWFormat     : PWaveFormatEx;
    FHeader       : pWaveHdr;
    FSilenceIn    : boolean;
    FDeviceIn     : Byte;
    FDeviceOut    : Byte;
    FRedirector   : Boolean;
    FActive       : Boolean;
    FClose        : Boolean;
    FHWO          : HWaveOut;
    FHWI          : HWaveIn;
    FLastError    : MMRESULT;
    FLastErrorText: string[255];
    FFindDTMF     : boolean;
    FBufSize      : integer;
    FNoiseFilter  : Boolean;
    FBuffersCount : Integer;
    FBufferLengthInt  : Integer;
    FVolumeIn     : Integer;
    FOnData       : TDataEvent;
    FOnDataBuffer : TDataEvent;
    FOnVolume     : TDataVolume;
    FOnDTMF       : TDataDTMF;
    FOnError      : TWaveAudioEvent;
    FWaveInHeader :  array [0..Buffers-1] of TWaveHdr;
    FWaveInBuffers:  array [0..Buffers-1] of array [0..BufferLengthInt] of SmallInt;
    CS: TRTLCriticalSection;
    function GetLastErrorText: String;
    procedure WaveInOnData(Data: Pointer; Size: Longint);
    Function AFindDTMF(Const Data: Pointer; DataSize: DWORD; pWaveFormat: PWaveFormatEx; ANoiseFilter:boolean; ABufSize: integer): integer;
  public
    constructor Create; overload;
    destructor Destroy;  override;
    function Open(ADeviceIn: integer): boolean;
    function ReOpen(ADeviceIn, ADeviceOut: Byte): Boolean;
    procedure Close;
    function GetPosition(HWI: HWaveIn): DWORD;
    procedure Lock;
    procedure Unlock;
    Function OpenAudioRedirector(ADeviceOut: integer): boolean;
    Function CloseAudioRedirector: boolean;
    Function OpenDTMFDetect(ABufSize: integer;  ANoiseFilter: boolean): boolean;
    Function CloseDTMFDetect: boolean;
    Function GetChannels:word;
    Procedure  SetWaveFormat(AFormat: PWaveFormatEx);
    function GetPCMAudioFormat(const pWaveFormat: PWaveFormatEx): TPCMFormat;
    procedure SetPCMAudioFormatS(const pWaveFormat: PWaveFormatEx; PCMFormat: TPCMFormat);
    procedure SetPCMAudioFormat(const pWaveFormat: PWaveFormatEx;
  Channels: TPCMChannel; SamplesPerSec: TPCMSamplesPerSec;
  BitsPerSample: TPCMBitsPerSample);
  protected
    procedure DoError; virtual;
    function Success(mmResult: MMRESULT): Boolean;
    function SystemError(mmResult: string): Boolean;
  published
    property HWI: HWaveIn read FHWI;
    property Active: Boolean read FActive;
    property Redirector: Boolean read FRedirector write FRedirector;
    property FindDTMF: Boolean read FFindDTMF;
    property NoiseFilter: Boolean read FNoiseFilter;
    property BufSizeDTMF: Integer read FBufSize;
    property SilenceIn: Boolean read FSilenceIn write FSilenceIn default false;
    property VolumeIn: integer read FVolumeIn write FVolumeIn default 100;
    property BufferLength: Integer read FBufferLengthInt write FBufferLengthInt;
    property OnVolume : TDataVolume  read FOnVolume write FOnVolume;
    property OnDataBuffer : TDataEvent  read FOnDataBuffer write FOnDataBuffer;
    property OnDTMF : TDataDTMF  read FOnDTMF write FOnDTMF;
    property OnError: TWaveAudioEvent read FOnError write FOnError;
    property LastError: MMRESULT read fLastError;
    property LastErrorText: String read GetLastErrorText;
  end;

procedure WaveInProc(HWI: HWaveIn; uMsg, dwInstance, dwParam1, dwParam2: DWORD); stdcall;
Function PCMFormatToStr(PCMFormat: TPCMFormat):string;
Function StrToPCMFormat(strformat: string): TPCMFormat;
function GetErrorText(ErrorCode: MMRESULT): String;

implementation

//uses unit1;

var
  FTAudioFH: TAudioFH;
  CritSect : TRTLCriticalSection;

  //+++++++++++++++++ Returns the wave's length in milliseconds.
function GetWaveAudioLength(const pWaveFormat: PWaveFormatEx; DataSize: DWORD): DWORD;
begin
  with pWaveFormat^ do
    if nAvgBytesPerSec <> 0 then
      Result := MulDiv(1000, DataSize, nAvgBytesPerSec)
    else
      Result := 0;
end;

//++++++++++++++++ Returns the wave data peak level in percent (PCM format only).
function GetWaveAudioPeakLevel(const Data: Pointer; DataSize: DWORD;
  pWaveFormat: PWaveFormatEx): TPeakLevel;

  function GetAudioPeakLevel8Bit: TPeakLevel;
  var
    pSample: PByte;
    Max: array[0..15] of Byte;
    cur_channel: word;
    i: integer;
  begin
    cur_channel:=0;
    for I := 0 to 15 do result.DB_CH[i]:=0;
    fillchar(max,length(max)*sizeof(byte),0);
    pSample := Data;
    for I := 0 to DataSize do
    begin
    if cur_channel>=pWaveFormat^.nChannels then  cur_channel:=0;
      if pSample^ > Max[cur_channel] then Max[cur_channel] := pSample^;
      inc(pSample);
      inc(cur_channel,1);
    end;
    for I := 0 to pWaveFormat^.nChannels-1 do
    begin
    if ByteBool(Max[i] and $80) then
      Max[i] := Max[i] and $7F
    else  Max[i] := 0;
    Result.DB_CH[i] := (100 * Max[i]) div $7F;
    end;
  end;

  function GetAudioPeakLevel16Bit: TPeakLevel;
  var
    pSample: PSmallInt;
    Max: array[0..15] of SmallInt;
    cur_channel: word;
    i: integer;
  begin
    cur_channel:=0;
    for I := 0 to 15 do result.DB_CH[i]:=0;
    fillchar(max,length(max)*sizeof(smallint),0);
    pSample := PSmallint(Data);
    for I := 0 to DataSize div 2 do
    begin
      if cur_channel>=pWaveFormat^.nChannels then  cur_channel:=0;
      if pSample^ > Max[cur_channel] then  Max[cur_channel] := pSample^;
      inc(pSample);
      inc(cur_channel,1);
    end;
    for I := 0 to pWaveFormat^.nChannels-1 do
    Result.DB_CH[i] := (100 * Max[i]) div $7FFF;
  end;

var
  i: integer;
begin
for I := 0 to 15 do result.DB_CH[i]:=0;
  case pWaveFormat^.wBitsPerSample of
    8: Result := GetAudioPeakLevel8Bit;
    16: Result := GetAudioPeakLevel16Bit;
  end;
end;


// Returns the wave data peak level in percent (PCM format only).
function GetWaveAudioMixPeakLevel(const Data: Pointer; DataSize: DWORD;
  pWaveFormat: PWaveFormatEx): Integer;

  function GetAudioPeakLevel8Bit: Integer;
  var
    pSample: PByte;
    Max: Byte;
  begin
    Max := 0;
    pSample := Data;
    while DataSize > 0 do
    begin
      if pSample^ > Max then
        Max := pSample^;
      Inc(pSample);
      Dec(DataSize);
    end;
    if ByteBool(Max and $80) then
      Max := Max and $7F
    else
      Max := 0;
    Result := (100 * Max) div $7F;
  end;

  function GetAudioPeakLevel16Bit: Integer;
  var
    pSample: PSmallInt;
    Max: SmallInt;
  begin
    Max := 0;
    pSample := Data;
    while DataSize > 0 do
    begin
      if pSample^ > Max then
        Max := pSample^;
      Inc(pSample);
      Dec(DataSize, 2);
    end;
    Result := (100 * Max) div $7FFF;
  end;
begin
  case pWaveFormat^.wBitsPerSample of
    8: Result := GetAudioPeakLevel8Bit;
    16: Result := GetAudioPeakLevel16Bit;
  else
    Result := -1;
  end;
end;

// +++++++++++++++Fills the wave data with silence
procedure SilenceWaveAudioMix(const Data: Pointer; DataSize: DWORD;
  BitsPerSample: WORD);
begin
  case BitsPerSample of
    8: FillChar(Data^, DataSize, $7F);
    16: FillChar(Data^, DataSize, 0);
  end;
end;

// Increases/Decreases the wave data volume by the specified percentage (PCM format only).
Procedure ChangeWaveAudioMixVolume(Const Data: Pointer; DataSize: DWORD;
  BitsPerSample: WORD; Percent: Integer);

  procedure ChangeVolume8Bit;
  var
    pSample: PByte;
    Value: byte;
  begin
    pSample := Data;
    while DataSize > 0 do
    begin
      Value := (pSample^ * Percent) div 100;
      if Value > High(Byte) then
        Value := High(Byte)
      else if Value < 0 then
        Value := 0;
      pSample^ := Value;
      Inc(pSample);
      Dec(DataSize, SizeOf(Byte));
    end;
    //copymemory(Data,pSample,DataSize);
  end;

  procedure ChangeVolume16Bit;
  var
    pSample: PSmallInt;
    Value: SmallInt;
  begin
   pSample := PSmallint(Data);
    while DataSize > 0 do
    begin
      Value :=  (pSample^ * Percent) div 100;
      if Value > High(SmallInt) then
        Value := High(SmallInt)
      else if Value < -High(SmallInt) then
        Value := -High(SmallInt);
      pSample^ := Value;
      Inc(pSample);
      Dec(DataSize, SizeOf(SmallInt));
    end;
  end;

begin
  case BitsPerSample of
    8: ChangeVolume8Bit;
    16: ChangeVolume16Bit;
  end;
end;


function GetErrorText(ErrorCode: MMRESULT): String;
var
  ErrorText: array[0..255] of Char;
begin
  if WaveInGetErrorText(ErrorCode, ErrorText, SizeOf(ErrorText)) = MMSYSERR_NOERROR then
    Result := StrPas(ErrorText)
  else
    Result := '';
end;

function translate_mm_error (error_number: word): pChar;
begin
case error_number of
  mmsyserr_NoError: translate_mm_error := 'no error';
  mmsyserr_Error: translate_mm_error := 'unspecified error';
  mmsyserr_BadDeviceID: translate_mm_error := 'device ID out of range';
  mmsyserr_NotEnabled: translate_mm_error := 'driver failed enable';
  mmsyserr_Allocated: translate_mm_error := 'device already allocated';
  mmsyserr_InvalHandle: translate_mm_error := 'device handle is invalid';
  mmsyserr_NoDriver: translate_mm_error := 'no device driver present';
  mmsyserr_NoMem: translate_mm_error := 'memory allocation error';
  mmsyserr_NotSupported: translate_mm_error := 'function isn''t supported';
  mmsyserr_BadErrNum: translate_mm_error := 'error value out of range';
  mmsyserr_InvalFlag: translate_mm_error := 'invalid flag passed';
  mmsyserr_InvalParam: translate_mm_error := 'invalid parameter passed';
  waverr_BadFormat: translate_mm_error := 'unsupported wave format';
  waverr_StillPlaying: translate_mm_error := 'still something playing';
  waverr_Unprepared: translate_mm_error := 'header not prepared';
  waverr_Sync: translate_mm_error := 'device is synchronous';
  midierr_Unprepared: translate_mm_error := 'header not prepared';
  midierr_StillPlaying: translate_mm_error := 'still something playing';
  midierr_NoMap: translate_mm_error := 'no current map';
  midierr_NotReady: translate_mm_error := 'hardware is still busy';
  midierr_NoDevice: translate_mm_error := 'port no longer connected';
  midierr_InvalidSetup: translate_mm_error := 'invalid setup';
  midierr_LastError: translate_mm_error := 'last error in range';
  timerr_NoCanDo: translate_mm_error := 'request not completed';
  timerr_Struct: translate_mm_error := 'time struct size';
  joyerr_Parms: translate_mm_error := 'bad parameters';
  joyerr_NoCanDo: translate_mm_error := 'request not completed';
  joyerr_Unplugged: translate_mm_error := 'joystick is unplugged';
  mmioerr_FileNotFound: translate_mm_error := 'file not found';
  mmioerr_OutOfMemory: translate_mm_error := 'out of memory';
  mmioerr_CannotOpen: translate_mm_error := 'cannot open';
  mmioerr_CannotClose: translate_mm_error := 'cannot close';
  mmioerr_CannotRead: translate_mm_error := 'cannot read';
  mmioerr_CannotWrite: translate_mm_error := 'cannot write';
  mmioerr_CannotSeek: translate_mm_error := 'cannot seek';
  mmioerr_CannotExpand: translate_mm_error := 'cannot expand file';
  mmioerr_ChunkNotFound: translate_mm_error := 'chunk not found';
  mmioerr_Unbuffered: translate_mm_error := 'file is unbuffered';
else
  translate_mm_error := 'invalid parameter passed to error translation routine!';
end;
end;

function mmTimeToMS(const mmTime: TMMTime; WaveFormatEx  : PWaveFormatEx): DWORD;
begin
  case mmTime.wType of
    TIME_MS:
      Result := mmTime.ms;
    TIME_BYTES:
      if WaveFormatEx.nAvgBytesPerSec <> 0 then
        Result := MulDiv(1000, mmTime.cb, WaveFormatEx.nAvgBytesPerSec)
      else
        Result := 0;
    TIME_SAMPLES:
      if WaveFormatEx.nSamplesPerSec <> 0 then
        Result := MulDiv(1000, mmTime.sample, WaveFormatEx.nSamplesPerSec)
      else
        Result := 0;
    TIME_SMPTE:
      Result := 1000 * ((mmTime.hour * 3600) + (mmTime.min * 60) + mmTime.sec);
  else
    Result := 0;
  end;
end;

Function StrToPCMFormat(strformat: string): TPCMFormat;
begin
  result:=nonePCM;
  if AnsiCompareStr(strformat,'nonePCM')=0 then result:=nonePCM else
  if AnsiCompareStr(strformat,'8000Hz, Mono, 8Bit')=0 then result:=Mono8bit8000Hz else
  if AnsiCompareStr(strformat,'8000Hz, Stereo, 8bit')=0 then result:=Stereo8bit8000Hz else
  if AnsiCompareStr(strformat,'8000Hz, Mono, 16bit')=0 then result:=Mono16bit8000Hz else
  if AnsiCompareStr(strformat,'8000Hz, Stereo, 16bit')=0 then result:=Stereo16bit8000Hz else
  if AnsiCompareStr(strformat,'11025Hz, Mono,8bit')=0 then result:=Mono8bit11025Hz else
  if AnsiCompareStr(strformat,'11025Hz, Stereo, 8bit')=0 then result:=Stereo8bit11025Hz else
  if AnsiCompareStr(strformat,'11025Hz, Mono, 16bit')=0 then result:=Mono16bit11025Hz else
  if AnsiCompareStr(strformat,'11025Hz, Stereo, 16bit')=0 then result:=Stereo16bit11025Hz else
  if AnsiCompareStr(strformat,'22050Hz, Mono, 8bit')=0 then result:=Mono8bit22050Hz else
  if AnsiCompareStr(strformat,'22050Hz, Stereo, 8bit')=0 then result:=Stereo8bit22050Hz else
  if AnsiCompareStr(strformat,'22050Hz, Mono, 16bit')=0 then result:=Mono16bit22050Hz else
  if AnsiCompareStr(strformat,'22050Hz, Stereo, 16bit')=0 then result:=Stereo16bit22050Hz else
  if AnsiCompareStr(strformat,'44100Hz, Mono, 8bit')=0 then result:=Mono8bit44100Hz else
  if AnsiCompareStr(strformat,'44100Hz, Stereo, 8bit')=0 then result:=Stereo8bit44100Hz else
  if AnsiCompareStr(strformat,'44100Hz, Mono, 16bit')=0 then result:=Mono16bit44100Hz else
  if AnsiCompareStr(strformat,'44100Hz, Stereo, 16bit')=0 then result:=Stereo16bit44100Hz else
  if AnsiCompareStr(strformat,'48000Hz, Mono, 8bit')=0 then result:=Mono8bit48000Hz else
  if AnsiCompareStr(strformat,'48000Hz, Stereo, 8bit')=0 then result:=Stereo8bit48000Hz else
  if AnsiCompareStr(strformat,'48000Hz, Mono, 16bit')=0 then result:=Mono16bit48000Hz else
  if AnsiCompareStr(strformat,'48000Hz, Stereo, 16bit')=0 then result:=Stereo16bit48000Hz;
end;

Function PCMFormatToStr(PCMFormat: TPCMFormat):string;
begin
  case PCMFormat of
    nonePCM:
      result:='nonePCM';
    Mono8Bit8000Hz:
      result:='Mono8Bit8000Hz';
    Mono8Bit11025Hz:
      result:='Mono8Bit11025Hz';
    Mono8Bit22050Hz:
      result:='Mono8Bit22050Hz';
    Mono8Bit44100Hz:
      result:='Mono8Bit44100Hz';
    Mono8Bit48000Hz:
      result:='Mono8Bit48000Hz';
    Mono16Bit8000Hz:
      result:='Mono16Bit8000Hz';
    Mono16Bit11025Hz:
      result:='Mono16Bit11025Hz';
    Mono16Bit22050Hz:
      result:='Mono16Bit22050Hz';
    Mono16Bit44100Hz:
      result:='Mono16Bit44100Hz';
    Mono16Bit48000Hz:
      result:='Mono16Bit48000Hz';
    Stereo8bit8000Hz:
      result:='Stereo8bit8000Hz';
    Stereo8bit11025Hz:
      result:='Stereo8bit11025Hz';
    Stereo8bit22050Hz:
      result:='Stereo8bit22050Hz';
    Stereo8bit44100Hz:
      result:='Stereo8bit44100Hz';
    Stereo8bit48000Hz:
      result:='Stereo8bit48000Hz';
    Stereo16bit8000Hz:
      result:='Stereo16bit8000Hz';
    Stereo16bit11025Hz:
      result:='Stereo16bit11025Hz';
    Stereo16bit22050Hz:
      result:='Stereo16bit22050Hz';
    Stereo16bit44100Hz:
      result:='Stereo16bit44100Hz';
    Stereo16bit48000Hz:
      result:='Stereo16bit48000Hz';
  end;
end;

{ TAudioFHInfo }

constructor TAudioFHInfo.Create;
begin
inherited;
end;


function TAudioFHInfo.GetInDeviceFormats(ADeviceID: DWORD): TWaveDeviceFormats;
var
  DevCaps: TWaveInCaps;
begin
  Result := [];
  if WaveInGetDevCaps(ADeviceID, @DevCaps, SizeOf(DevCaps)) = MMSYSERR_NOERROR then
  begin
    Include(Result, Mono8bit8000Hz);
    Include(Result, Stereo8bit8000Hz);
    Include(Result, Mono16bit8000Hz);
    Include(Result, Stereo16bit8000Hz);
    if LongBool(DevCaps.dwFormats and WAVE_FORMAT_1M08) then
      Include(Result, Mono8bit11025Hz);
    if LongBool(DevCaps.dwFormats and WAVE_FORMAT_1S08) then
      Include(Result, Stereo8bit11025Hz);
    if LongBool(DevCaps.dwFormats and WAVE_FORMAT_1M16) then
      Include(Result, Mono16bit11025Hz);
    if LongBool(DevCaps.dwFormats and WAVE_FORMAT_1S16) then
      Include(Result, Stereo16bit11025Hz);
    if LongBool(DevCaps.dwFormats and WAVE_FORMAT_2M08) then
      Include(Result, Mono8bit22050Hz);
    if LongBool(DevCaps.dwFormats and WAVE_FORMAT_2S08) then
      Include(Result, Stereo8bit22050Hz);
    if LongBool(DevCaps.dwFormats and WAVE_FORMAT_2M16) then
      Include(Result, Mono16bit22050Hz);
    if LongBool(DevCaps.dwFormats and WAVE_FORMAT_2S16) then
      Include(Result, Stereo16bit22050Hz);
    if LongBool(DevCaps.dwFormats and WAVE_FORMAT_4M08) then
      Include(Result, Mono8bit44100Hz);
    if LongBool(DevCaps.dwFormats and WAVE_FORMAT_4S08) then
      Include(Result, Stereo8bit44100Hz);
    if LongBool(DevCaps.dwFormats and WAVE_FORMAT_4M16) then
      Include(Result, Mono16bit44100Hz);
    if LongBool(DevCaps.dwFormats and WAVE_FORMAT_4S16) then
      Include(Result, Stereo16bit44100Hz);
  end;
end;

function TAudioFHInfo.ValidateInDeviceID(ADeviceID: DWORD): MMRESULT;
var
  DevCaps: TWaveInCaps;
begin
 Result := WaveInGetDevCaps(ADeviceID, @DevCaps, SizeOf(DevCaps));
end;

function TAudioFHInfo.GetNumInDevs: integer;
begin
  Result := WaveInGetNumDevs;
end;

function TAudioFHInfo.GetInDeviceName(ADeviceID:integer): String;
var
  DevCaps: TWaveInCaps;
begin
  if WaveInGetDevCaps(ADeviceID, @DevCaps, SizeOf(DevCaps))=0 then
    Result := StrPas(DevCaps.szPname)
  else
    Result := 'Unsupported Device';
end;

function TAudioFHInfo.ValidateOutDeviceID(ADeviceID: DWORD): MMRESULT;
var
  DevCaps: TWaveOutCaps;
begin
 Result := WaveInGetDevCaps(ADeviceID, @DevCaps, SizeOf(DevCaps));
end;

function TAudioFHInfo.GetNumOutDevs: integer;
begin
  Result := WaveOutGetNumDevs;
end;

function TAudioFHInfo.GetOutDeviceName(ADeviceID:integer): String;
var
  DevCaps: TWaveOutCaps;
begin
  if WaveOutGetDevCaps(ADeviceID, @DevCaps, SizeOf(DevCaps))=0 then
    Result := IntToStr(ADeviceID) + ' : ' + StrPas(DevCaps.szPname)
  else
    Result := IntToStr(ADeviceID) + ' : ' + 'Unsupported Device';
end;

Function TAudioFHInfo.ChackInFormat(device : integer; WaveFormat :  PWaveFormatEx):boolean;
begin
if waveInOpen(nil, device, WaveFormat, 0, 0, WAVE_FORMAT_QUERY)=0 then
result:=true else
begin
result:=false;
end;
end;



procedure WaveInProc(HWI: HWaveIn; uMsg, dwInstance, dwParam1, dwParam2: DWORD); stdcall;
begin
try
  with TAudioFH(dwInstance) do begin
    case uMsg of
      WIM_OPEN: begin
        FActive:=true;
        FClose:=false;
      end;
      WIM_CLOSE: begin
        if FHWO <> 0 then begin
          success(WaveOutReset(FHWO));
          success(WaveOutClose(FHWO));
          FHWO:=0;
        end;
        FActive:=false;
      end;
      WIM_DATA: begin
        if (FActive) and (not FClose) then begin
          if Assigned(FOnData) then begin
            FHeader:= pWaveHdr(dwParam1); // dwParam1 is a DWORD ???

          {  i:=WaveInUnPrepareHeader(FHWI^, FHeader, SizeOf(TWaveHdr));
            if i <> 0 then begin
              FLastError:=PChar('procedure WaveInProc -> WaveInUnPrepareHeader: ' + IntToStr(i)), 'Œ¯Ë·Í‡', MB_ICONERROR);
              Close;
              Exit;
            end;   }
           FOnData(PByte(FHeader^.lpData), FHeader^.dwBytesRecorded);


          end;
          {with FHeader^ do begin
            dwbufferlength:=BufferLengthInt;
            dwbytesrecorded:=0;
            dwUser:=0;
            dwflags:=0;
            dwloops:=0;
          end;
          i:=WaveInPrepareHeader(FHWI^, FHeader, SizeOf(TWaveHdr));
          if i <> 0 then begin
            FLastError:=PChar('procedure WaveInProc -> WaveInPrepareHeader: ' + IntToStr(i)), 'Œ¯Ë·Í‡', MB_ICONERROR);
            Close;
            Exit;
          end;   }

          if not Success(WaveInAddBuffer(FHWI, FHeader, SizeOf(TWaveHdr))) then
          begin
            Close;
            Exit;
          end;
        end;
      end;
    end;
    end;
except
  on E: Exception do begin
    FTAudioFH.SystemError(PChar('procedure WaveInProc: ' + E.Message));
  end;
end;
end;



{ TAudioFH }

constructor TAudioFH.Create;
begin
  InitializeCriticalSection(CS);
  FVolumeIn:=100;
  FSilenceIn:=false;
  FLastErrorText := '';
  FLastError:=0;
  FFindDTMF:=False;
  FDeviceIn:=0;
  FDeviceOut:=0;
  FClose:=true;
  FBuffersCount:=Buffers;
  FBufferLengthInt:=BufferLengthInt;
  {FPWFormat.wFormatTag      := WAVE_FORMAT_PCM;
  FPWFormat.wBitsPerSample  := 16;
  FPWFormat.nChannels       := 1;
  FPWFormat.nSamplesPerSec  := 44100;
  FPWFormat.nBlockAlign     := (FPWFormat.nChannels * FPWFormat.wBitsPerSample) div 8;
  FPWFormat.nAvgBytesPerSec := (FPWFormat.nChannels * FPWFormat.nSamplesPerSec * FPWFormat.wBitsPerSample) div 8;
  FPWFormat.cbSize          := 0;    }

  FOnData:=WaveInOnData;
end;


destructor TAudioFH.Destroy;
begin
  FActive := False;
  FOnData:=nil;
  DeleteCriticalSection(CS);
  inherited Destroy;
end;


Function TAudioFH.AFindDTMF(Const Data: Pointer; DataSize: DWORD;
  pWaveFormat: PWaveFormatEx; ANoiseFilter:boolean; ABufSize: integer): integer;

var
i : integer;
data16  : PData16;
data8   : Pdata8;
SmallWavArray     :  array [0..BufferLengthInt] of SmallInt;
SmallArrayCnt     :  Longint;
AverageGain       :  Int64;
j                 :  longint;
GainFactor        :  Double;
LowGain           :  boolean;
dtmf              :  integer;

Function CalcDTMF:  integer;
var   C          : Char;
      Factor1   : Double;
      Factor2   : Double;
	    rFactor, rMaxFactor1, rMaxFactor2, rMaxFactor3: Double;
	    i, nMaxFactorIndex1, nMaxFactorIndex2:	Integer;
begin
     result              := 0;
 	   rMaxFactor1 				:= 0.0;
	   rMaxFactor2    		:= 0.0;
	   rMaxFactor3    		:= 0.0;
     Factor1           := 0.0;
     Factor2           := 0.0;
	   nMaxFactorIndex1 	:= 0;
	   nMaxFactorIndex2 	:= 0;

	   for i := 0 to 7 do
		 begin
		      rFactor	:= CalcFreqFactor(DtmfFreq[i], pWaveFormat.nSamplesPerSec, @SmallWavArray, 1024-1);
          if ANoiseFilter then
          begin
               Factor1 :=  Factor1 + rFactor;
               Factor2 :=  Factor2 + CalcFreqFactor(OutRangeFreq[i], pWaveFormat.nSamplesPerSec, @SmallWavArray, 1024-1);
          end else Factor1:= 100.0;

		      if rFactor > rMaxFactor1 then
			    begin
			         rMaxFactor3				:= rMaxFactor2;
			         rMaxFactor2				:= rMaxFactor1;
			         rMaxFactor1			  := rFactor;
			         nMaxFactorIndex2	  := nMaxFactorIndex1;
			         nMaxFactorIndex1		:= i;
			    end
		      else
			    if rFactor > rMaxFactor2 then
				  begin
				       rMaxFactor3				:= rMaxFactor2;
				       rMaxFactor2				:= rFactor;
				       nMaxFactorIndex2	  := i;
				  end;
     end;
	   C	:= ' ';
	   if (rMaxFactor3 > 0.0001) and (Factor1 > Factor2 * 1.8) then
     begin
		      if (rMaxFactor1 / rMaxFactor3 >= 2.05) and (rMaxFactor2 / rMaxFactor3 >= 1.95) then
          begin
			         I	:= nMaxFactorIndex1 or (nMaxFactorIndex2 shl 3);
			         C	:= Encoding2x8[i + 1];
          end;
     end;
     C	:= AddDtmfDigit(C);
     if (C <> ' ') and (C <> '') then
     begin
       result:=ord(C);
     end;
end;

begin
  result:=0;
  SmallArrayCnt:=0;
  case pWaveFormat.wBitsPerSample of
    8:
      begin
      try
         Data8:=PData8(data);
            AverageGain:=0;
            for i:= 0 to BufferLengthInt -1 do
            begin
               j:= abs(Data8^[i]);
               Inc(AverageGain, j);
            end;
            AverageGain:= AverageGain div BufferLengthInt;
            if AverageGain > GainFactor then  LowGain:= False else LowGain:= True;

            for i:= 0 to BufferLengthInt -1 do
              begin
               if LowGain then Data8^[i]:= 0;
               SmallWavArray[SmallArrayCnt]:= Data8^[i];
               Inc(SmallArrayCnt);
               if SmallArrayCnt > ABufSize then
               begin
                    SmallArrayCnt:=0;
                    dtmf:=CalcDTMF;
                    if dtmf<>0 then
                    if Assigned(FOnDTMF) then FOnDTMF(dtmf);
                end;
              end;

      except
        on E: Exception do begin
          SystemError(PChar('CalcDTMF: ' + E.Message));
      end;
      end;
      end;
    16:
      begin
      try
         Data16:=PData16(data);
            AverageGain:=0;
            for i:= 0 to BufferLengthInt -1 do
            begin
               j:= abs(Data16^[i]);
               Inc(AverageGain, j);
            end;
            AverageGain:= AverageGain div BufferLengthInt;
            if AverageGain > GainFactor then  LowGain:= False else LowGain:= True;
            for i:= 0 to BufferLengthInt -1 do
              begin
               if LowGain then Data16^[i]:= 0;
               SmallWavArray[SmallArrayCnt]:= Data16^[i];
               Inc(SmallArrayCnt);
               if SmallArrayCnt > ABufSize then
               begin
                    SmallArrayCnt:=0;
                    dtmf:=CalcDTMF;
                    if dtmf<>0 then
                    if Assigned(FOnDTMF) then FOnDTMF(dtmf);
                end;
              end;

      except
        on E: Exception do begin
          SystemError(PChar('CalcDTMF: ' + E.Message));
      end;
      end;
      end;
  end;
end;

Procedure  TAudioFH.SetWaveFormat(AFormat: PWaveFormatEx);
begin
  FPWFormat:=AFormat;
end;


procedure TAudioFH.SetPCMAudioFormat(const pWaveFormat: PWaveFormatEx;
  Channels: TPCMChannel; SamplesPerSec: TPCMSamplesPerSec;
  BitsPerSample: TPCMBitsPerSample);
begin
  with pWaveFormat^ do
  begin
    wFormatTag := WAVE_FORMAT_PCM;
    case Channels of
      cMono: nChannels := 1;
      cStereo: nChannels := 2;
    end;
    case SamplesPerSec of
      ss8000Hz: nSamplesPerSec := 8000;
      ss11025Hz: nSamplesPerSec := 11025;
      ss22050Hz: nSamplesPerSec := 22050;
      ss44100Hz: nSamplesPerSec := 44100;
      ss48000Hz: nSamplesPerSec := 48000;
    end;
    case BitsPerSample of
      bs8Bit: wBitsPerSample := 8;
      bs16Bit: wBitsPerSample := 16;
    end;
    nBlockAlign := MulDiv(nChannels, wBitsPerSample, 8);
    nAvgBytesPerSec := nSamplesPerSec * nBlockAlign;
    cbSize := 0;
  end;
end;


procedure TAudioFH.SetPCMAudioFormatS(const pWaveFormat: PWaveFormatEx; PCMFormat: TPCMFormat);
begin
  case PCMFormat of
    Mono8Bit8000Hz:
      SetPCMAudioFormat(pWaveFormat, cMono, ss8000Hz, bs8Bit);
    Mono8Bit11025Hz:
      SetPCMAudioFormat(pWaveFormat, cMono, ss11025Hz, bs8Bit);
    Mono8Bit22050Hz:
      SetPCMAudioFormat(pWaveFormat, cMono, ss22050Hz, bs8Bit);
    Mono8Bit44100Hz:
      SetPCMAudioFormat(pWaveFormat, cMono, ss44100Hz, bs8Bit);
    Mono8Bit48000Hz:
      SetPCMAudioFormat(pWaveFormat, cMono, ss48000Hz, bs8Bit);
    Mono16Bit8000Hz:
      SetPCMAudioFormat(pWaveFormat, cMono, ss8000Hz, bs16Bit);
    Mono16Bit11025Hz:
      SetPCMAudioFormat(pWaveFormat, cMono, ss11025Hz, bs16Bit);
    Mono16Bit22050Hz:
      SetPCMAudioFormat(pWaveFormat, cMono, ss22050Hz, bs16Bit);
    Mono16Bit44100Hz:
      SetPCMAudioFormat(pWaveFormat, cMono, ss44100Hz, bs16Bit);
    Mono16Bit48000Hz:
      SetPCMAudioFormat(pWaveFormat, cMono, ss48000Hz, bs16Bit);
    Stereo8bit8000Hz:
      SetPCMAudioFormat(pWaveFormat, cStereo, ss8000Hz, bs8Bit);
    Stereo8bit11025Hz:
      SetPCMAudioFormat(pWaveFormat, cStereo, ss11025Hz, bs8Bit);
    Stereo8bit22050Hz:
      SetPCMAudioFormat(pWaveFormat, cStereo, ss22050Hz, bs8Bit);
    Stereo8bit44100Hz:
      SetPCMAudioFormat(pWaveFormat, cStereo, ss44100Hz, bs8Bit);
    Stereo8bit48000Hz:
      SetPCMAudioFormat(pWaveFormat, cStereo, ss48000Hz, bs8Bit);
    Stereo16bit8000Hz:
      SetPCMAudioFormat(pWaveFormat, cStereo, ss8000Hz, bs16Bit);
    Stereo16bit11025Hz:
      SetPCMAudioFormat(pWaveFormat, cStereo, ss11025Hz, bs16Bit);
    Stereo16bit22050Hz:
      SetPCMAudioFormat(pWaveFormat, cStereo, ss22050Hz, bs16Bit);
    Stereo16bit44100Hz:
      SetPCMAudioFormat(pWaveFormat, cStereo, ss44100Hz, bs16Bit);
    Stereo16bit48000Hz:
      SetPCMAudioFormat(pWaveFormat, cStereo, ss48000Hz, bs16Bit);
  end;
end;

// Returns the standard PCM format specifier of a wave format.
function TAudioFH.GetPCMAudioFormat(const pWaveFormat: PWaveFormatEx): TPCMFormat;
begin
  Result := nonePCM;
  with pWaveFormat^ do
    if wFormatTag = WAVE_FORMAT_PCM then
    begin
      if (nChannels = 1) and (nSamplesPerSec = 8000) and (wBitsPerSample = 8) then
        Result := Mono8Bit8000Hz
      else if (nChannels = 2) and (nSamplesPerSec = 8000) and (wBitsPerSample = 8) then
        Result := Stereo8Bit8000Hz
      else if (nChannels = 1) and (nSamplesPerSec = 8000) and (wBitsPerSample = 16) then
        Result := Mono16bit8000Hz
      else if (nChannels = 2) and (nSamplesPerSec = 8000) and (wBitsPerSample = 16) then
        Result := Stereo16Bit8000Hz
      else if (nChannels = 1) and (nSamplesPerSec = 11025) and (wBitsPerSample = 8) then
        Result := Mono8Bit11025Hz
      else if (nChannels = 2) and (nSamplesPerSec = 11025) and (wBitsPerSample = 8) then
        Result := Stereo8Bit11025Hz
      else if (nChannels = 1) and (nSamplesPerSec = 11025) and (wBitsPerSample = 16) then
        Result := Mono16bit11025Hz
      else if (nChannels = 2) and (nSamplesPerSec = 11025) and (wBitsPerSample = 16) then
        Result := Stereo16Bit11025Hz
      else if (nChannels = 1) and (nSamplesPerSec = 22050) and (wBitsPerSample = 8) then
        Result := Mono8Bit22050Hz
      else if (nChannels = 2) and (nSamplesPerSec = 22050) and (wBitsPerSample = 8) then
        Result := Stereo8Bit22050Hz
      else if (nChannels = 1) and (nSamplesPerSec = 22050) and (wBitsPerSample = 16) then
        Result := Mono16bit22050Hz
      else if (nChannels = 2) and (nSamplesPerSec = 22050) and (wBitsPerSample = 16) then
        Result := Stereo16Bit22050Hz
      else if (nChannels = 1) and (nSamplesPerSec = 44100) and (wBitsPerSample = 8) then
        Result := Mono8Bit44100Hz
      else if (nChannels = 2) and (nSamplesPerSec = 44100) and (wBitsPerSample = 8) then
        Result := Stereo8Bit44100Hz
      else if (nChannels = 1) and (nSamplesPerSec = 44100) and (wBitsPerSample = 16) then
        Result := Mono16bit44100Hz
      else if (nChannels = 2) and (nSamplesPerSec = 44100) and (wBitsPerSample = 16) then
        Result := Stereo16Bit44100Hz
      else if (nChannels = 1) and (nSamplesPerSec = 48000) and (wBitsPerSample = 8) then
        Result := Mono8Bit48000Hz
      else if (nChannels = 2) and (nSamplesPerSec = 48000) and (wBitsPerSample = 8) then
        Result := Stereo8Bit48000Hz
      else if (nChannels = 1) and (nSamplesPerSec = 48000) and (wBitsPerSample = 16) then
        Result := Mono16bit48000Hz
      else if (nChannels = 2) and (nSamplesPerSec = 48000) and (wBitsPerSample = 16) then
        Result := Stereo16Bit48000Hz
    end;
end;




procedure TAudioFH.Lock;
begin
  EnterCriticalSection(CS);
end;

procedure TAudioFH.Unlock;
begin
  LeaveCriticalSection(CS);
end;

Function TAudioFH.OpenDTMFDetect(ABufSize: integer;  ANoiseFilter: boolean): boolean;
var
  DevCaps: TWaveInCaps;
begin
result:=false;
FFindDTMF:=true;
if ABufSize<512 then  ABufSize:=512;
if ABufSize>8192 then  ABufSize:=8192;
FBufSize:=ABufSize;
FNoiseFilter:=ANoiseFilter;
result:=FFindDTMF;
end;

Function TAudioFH.CloseDTMFDetect: boolean;
var
  DevCaps: TWaveInCaps;
begin
result:=false;
FFindDTMF:=false;
result:=FFindDTMF;
end;


Function TAudioFH.OpenAudioRedirector(ADeviceOut: integer): boolean;
var
  DevCaps: TWaveInCaps;
begin
result:=false;
FRedirector:=False;
if WaveInGetDevCaps(ADeviceOut, @DevCaps, SizeOf(DevCaps))=0 then
begin
FDeviceOut:=ADeviceOut;
if WaveOutOpen(@FHWO, FDeviceOut, FPWFormat, 0, 0, CALLBACK_NULL or WAVE_MAPPED)=0 then FRedirector:=true;
end;
result:=FRedirector;
end;

Function TAudioFH.CloseAudioRedirector: boolean;
var
  DevCaps: TWaveInCaps;
begin
result:=false;
if FHWO <> 0 then begin
  WaveOutReset(FHWO);
  WaveOutClose(FHWO);
  FHWO:=0;
  result:=true;
  FRedirector:=false;
end else
begin
  FRedirector:=false;
  result:=true;
end;
end;

function TAudioFH.GetChannels:word;
begin
result:=FPWFormat.nChannels;
end;

function TAudioFH.GetPosition(HWI: HWaveIn): DWORD;
var
  mmTime: TMMTime;
begin
  Result := 0;
  mmTime.wType := TIME_MS;
  if WaveInGetPosition(HWI, @mmTime, SizeOf(mmTime)) = MMSYSERR_NOERROR then
    Result := mmTimeToMS(mmTime, FPWFormat);
end;

function TAudioFH.GetLastErrorText: String;
begin
if fLastError=0 then
  Result := fLastErrortext else    Result := GetErrorText(fLastError) ;
end;

function TAudioFH.Success(mmResult: MMRESULT): Boolean;
begin
  Result := True;
  fLastError := mmResult;
  if mmResult <> MMSYSERR_NOERROR then
  begin
    Result := False;
    DoError;
  end;
end;

function TAudioFH.SystemError(mmResult: string): Boolean;
begin
  Result := True;
  fLastErrorText := mmResult;
  if mmResult <> '' then
  begin
    Result := False;
    DoError;
  end;
end;


procedure TAudioFH.DoError;
begin
  if Assigned(fOnError) then
    fOnError(Self)
  else
    raise EWaveAudioSysError.Create(LastErrorText);
end;


function TAudioFH.Open(ADeviceIn: integer): boolean;
var
  j: Integer;
begin
Result:=false;
if FActive then Exit;
FDeviceIn:=ADeviceIn;
try

  if not Success(WaveInOpen(@FHWI, FDeviceIn, FPWFormat, DWORD(@WaveInProc), DWORD(Self), CALLBACK_FUNCTION or WAVE_MAPPED)) then
  begin
    Exit;
  end;
  for j:= 0 to FBuffersCount - 1 do begin
  begin
  with FWaveInHeader[j] do
  begin
  lpdata:= @FWaveInBuffers[j];
  dwBufferlength:= FBufferLengthInt * FPWFormat.nBlockAlign;
  dwBytesrecorded:= 0;
  dwFlags:= 0;
  dwLoops:= 0;
  lpNext:=  nil;
  reserved:=  0;
  end;

    if not Success(WaveInPrepareHeader(FHWI, Addr(FWaveInHeader[j]), SizeOf(TWaveHdr))) then  Exit;
    if not Success(WaveInAddBuffer(FHWI, Addr(FWaveInHeader[j]), SizeOf(TWaveHdr))) then  Exit;
  end;
  end;
  if not Success(WaveInStart(FHWI)) then Exit;
  Result:=true;
except
  on E: Exception do begin
    SystemError(PChar('function Open: ' + E.Message));
  end;
end;
end;

procedure TAudioFH.Close;
begin
FClose:=true;
if FHWI <> 0 then begin
 Success(WaveInReset(FHWI));
  Success(WaveInClose(FHWI));
  FHWI:=0;
end;
end;

function TAudioFH.ReOpen(ADeviceIn, ADeviceOut: Byte): Boolean;
begin
Result:=false;
try
  if FActive then
    Close;
  Open(ADeviceIn);
  Result:=true;
except
  on E: Exception do begin
    SystemError(PChar('function ReOpen: ' + E.Message));
  end;
end;
end;

procedure TAudioFH.WaveInOnData(Data: Pointer; Size: Integer);
var
  Header  : PWaveHdr;
{data8   : PData8;
data16  : PData16;
data24  : PData24;
data32  : PData32;  }

begin


  if FVolumeIn<>100 then ChangeWaveAudioMixVolume(Data,size,FPWFormat.wBitsPerSample, FVolumeIn);
  if FSilenceIn then SilenceWaveAudioMix(Data,size,FPWFormat.wBitsPerSample);

  if (Assigned(FOnDTMF) and FFindDTMF) then AFindDTMF(data, Size, FPWFormat, FNoiseFilter, FBufSize);

  if Assigned(FOnVolume) then
  begin
    FOnVolume(GetWaveAudioPeakLevel(data, Size, FPWFormat));
  end;


  if Assigned(FOnDataBuffer) then
  begin
    FOnDataBuffer(Data, Size);
  end;

  try
    if FHWO <> 0 then begin
      Header:=New(PWaveHdr);
      with Header^ do begin
        lpdata:=Pointer(Data);
        dwbufferlength:=Size;
        dwbytesrecorded:=Size;
        dwUser:=0;
        dwflags:=0;
        dwloops:=0;
      end;
      WaveOutPrepareHeader(FHWO, Header, SizeOf(TWaveHdr));
      WaveOutWrite(FHWO, Header, SizeOf(TWaveHdr));
    end;
  except
    on E: Exception do begin
      SystemError(PChar('procedure WaveInOnData: ' + E.Message));
    end;
  end;


end;

initialization
  InitializeCriticalSection(CritSect);

finalization
  DeleteCriticalSection(CritSect);

end.
