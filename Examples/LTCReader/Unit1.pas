unit Unit1;

interface

uses
{$IFDEF MSWINDOWS}
  Winapi.Windows, Winapi.Messages,
{$ENDIF}
  System.SysUtils, System.Types, System.UITypes, System.Classes, System.Variants,
  System.Generics.Collections,  System.IOUtils, System.math, System.Threading, System.SyncObjs,
  FMX.Types, FMX.Controls, FMX.Forms, FMX.Graphics, FMX.Dialogs, FMX.Menus,
  FMX.StdCtrls, FMX.ListBox, FMX.ExtCtrls, FMX.Layouts, FMX.Objects,
  FMX.Controls.Presentation, FMX.Edit,
{$IFDEF MACOS}
  FMX.FontGlyphs.Mac, Macapi.Foundation,
{$ENDIF}
  portaudio, FH.LIBLTC.LTC, FH.LIBLTC.DECODER, FH.LIBLTC, FMX.DateTimeCtrls, FMX.Ani;

type
  TForm1 = class(TForm)
    Switch1: TSwitch;
    StatusBar1: TStatusBar;
    ComboBox2: TComboBox;
    Label2: TLabel;
    RoundRect2: TRoundRect;
    StyleBook1: TStyleBook;
    Label5: TLabel;
    Expander1: TExpander;
    Label1: TLabel;
    ComboBox1: TComboBox;
    GroupBox1: TGroupBox;
    Label3: TLabel;
    ComboBox3: TComboBox;
    Label4: TLabel;
    ComboBox4: TComboBox;
    Rectangle1: TRectangle;
    Layout1: TLayout;
    ArcDial1: TArcDial;
    Label6: TLabel;
    PopupMenu1: TPopupMenu;
    MenuItem1: TMenuItem;
    procedure EnumerationHostApi(var list: TList<PPaHostApiInfo>);
    procedure EnumerationDevice(const hostApiIndex: integer; var list: TList<PPaDeviceInfo>; const isInput: boolean = false);
    procedure EnumerationSampleRate(const deviceIndex: integer; var list: TList<double>);
    procedure EnumerationFormats(const deviceIndex: integer; var list: TList<TPaSampleFormat>);
    procedure FormCreate(Sender: TObject);
    procedure FormDestroy(Sender: TObject);
    procedure LTCDecoderOnRead(Sender: TObject; timecode : TDateTime);
    procedure LTCDecoderOnDebug(const S: string);

    procedure Switch1Switch(Sender: TObject);
    procedure ComboBox1Change(Sender: TObject);
    procedure ComboBox2Change(Sender: TObject);
    procedure FormCloseQuery(Sender: TObject; var CanClose: Boolean);
    procedure ArcDial1Change(Sender: TObject);
    procedure FormShow(Sender: TObject);
    procedure MenuItem1Click(Sender: TObject);
    procedure FormKeyUp(Sender: TObject; var Key: Word; var KeyChar: Char;
      Shift: TShiftState);
    procedure DoDebug(const msg: string);
  private
    { Private declarations }
  public
    ListHostApi     : TList<PPaHostApiInfo>;
    ListDevice      : TList<PPaDeviceInfo>;
    AudioStream     : PPaStream;
    PaError         : TPaError;
    HostApiIndexDef : TPaHostApiIndex;
    HostApiInfoDef  :PPaHostApiInfo;
    outputParameters  : PPaStreamParameters;
  end;

var
  Form1: TForm1;
  inputParameters   : PPaStreamParameters;
  LibLtc            : TLibLTC;
  CurTimecode       : TDateTime;
  crit : TCriticalSection;
implementation

{$R *.fmx}

uses Unit2;

{$R DSDIGI.res}

{$IFDEF MSWINDOWS}
function SetSystemPrivilege(sPrivilege: string; bEnabled: Boolean): Boolean;
var
  hToken: THandle;
  TokenPriv: TOKEN_PRIVILEGES;
  PrevTokenPriv: TOKEN_PRIVILEGES;
  ReturnLength: Cardinal;
begin
  Result := false;
  if OpenProcessToken(GetCurrentProcess(), TOKEN_ADJUST_PRIVILEGES or TOKEN_QUERY, hToken) then
  begin
    try
      // Get the locally unique identifier (LUID) .
      if LookupPrivilegeValue(nil, PChar(sPrivilege), TokenPriv.Privileges[0].Luid) then
      begin
        TokenPriv.PrivilegeCount := 1; // one privilege to set

        case bEnabled of
          True: TokenPriv.Privileges[0].Attributes := SE_PRIVILEGE_ENABLED;
          False: TokenPriv.Privileges[0].Attributes := 0;
        end;

        ReturnLength := 0; // replaces a var parameter
        PrevTokenPriv := TokenPriv;

        // enable or disable the privilege
        AdjustTokenPrivileges(hToken, False, TokenPriv, SizeOf(PrevTokenPriv),
        PrevTokenPriv, ReturnLength);
      end;
    finally
      CloseHandle(hToken);
    end;
  end;
  // test the return value of AdjustTokenPrivileges.
  Result := GetLastError = ERROR_SUCCESS;
  {if not Result then
    raise Exception.Create(SysErrorMessage(GetLastError));}
end;
{$ENDIF}

{$IFDEF MACOS}
function DateTimeToStringDelphi(): string;
var
    formatter: NSDateFormatter;
    stringDate: NSString;
begin
    formatter := TNSDateFormatter.Create();
    try
    formatter.setDateFormat(NSSTR('YYYY-MM-dd HH:mm:ss Z'));
    stringDate := formatter.stringFromDate( TNSDate.Wrap(TNSDate.OCClass.date) );
    finally
      formatter.release;
    end;
    result := string(stringDate.UTF8String);
end;
{$ENDIF}

{$IFDEF MACOS}
function StringToNSDate(): NSDate;
var
  date : NSDate;
  formatter: NSDateFormatter;
  stringDate: NSString;
begin
  formatter := TNSDateFormatter.Create();
  try
    stringDate:=NSSTR('2015-03-08 12:00:00 +0200');
    formatter.setDateFormat(NSSTR('YYYY-MM-dd HH:mm:ss Z'));
    date:=formatter.dateFromString(stringDate);
  finally
    formatter.release;
  end;
  result:=date;
end;
{$ENDIF}

function ChangeSystemDateTime(dtNeeded: TDateTime): boolean;
var
{$IFDEF MSWINDOWS}
  tzi: TTimeZoneInformation;
  dtSystem: TSystemTime;
{$ENDIF}
  Year, Month, Day, Hour, Minu, Sec, MSec: Word;
begin
{$IFDEF MSWINDOWS}
  GetTimeZoneInformation(tzi);
  dtNeeded := dtNeeded + tzi.Bias / 1440;
  DecodeDate(dtNeeded, Year, Month, Day);
  DecodeTime(dtNeeded, Hour, Minu, Sec, MSec);
  with dtSystem do
  begin
    wYear := Year;
    wMonth := Month;
    wDay := Day;
    wHour := Hour;
    wMinute := Minu;
    wSecond := Sec;
    wMilliseconds := MSec;
  end;
  //DateTimeToSystemTime
  //SetLocalTime
  result:=SetSystemTime(dtSystem);
  // Tell windows, that the Time changed!
  PostMessage(HWND_BROADCAST, WM_TIMECHANGE, 0, 0); // *
{$ENDIF}

{$IFDEF MACOS}
{$ENDIF}
end;

function PaSampleFormat2Bit(const PaSampleFormat: TPaSampleFormat):integer;
begin
  result:=0;
  case PaSampleFormat of
    paFloat32 : result:=32;
    paInt32   : result:=32;
    paInt24   : result:=24;
    paInt16   : result:=16;
    paInt8    : result:=8;
    paUInt8   : result:=8;
    paCustomFormat  : result:=0;
    paNonInterleaved  : result:=0;
  end;
end;

function PaSampleFormat2Byte(const PaSampleFormat: TPaSampleFormat):integer;
begin
  result:=0;
  case PaSampleFormat of
    paFloat32 : result:=4;
    paInt32   : result:=4;
    paInt24   : result:=3;
    paInt16   : result:=2;
    paInt8    : result:=1;
    paUInt8   : result:=1;
    paCustomFormat  : result:=0;
    paNonInterleaved  : result:=0;
  end;
end;

function String2PaSampleFormat(const PaSampleFormat: string):TPaSampleFormat;
begin
  if sametext(PaSampleFormat, 'paFloat32') then result:=paFloat32  else
  if sametext(PaSampleFormat, 'paInt32') then result:=paInt32  else
  if sametext(PaSampleFormat, 'paInt24') then result:=paInt24  else
  if sametext(PaSampleFormat, 'paInt16') then result:=paInt16  else
  if sametext(PaSampleFormat, 'paInt8') then result:=paInt8  else
  if sametext(PaSampleFormat, 'paUInt8') then result:=paUInt8  else
  if sametext(PaSampleFormat, 'paCustomFormat') then result:=paCustomFormat else
  if sametext(PaSampleFormat, 'paNonInterleaved') then result:=paNonInterleaved else
  result:=paInt16;
end;

function PaSampleFormat2String(const PaSampleFormat: TPaSampleFormat):string;
begin
  result:='';
  case PaSampleFormat of
    paFloat32 : result:='paFloat32';
    paInt32   : result:='paInt32';
    paInt24   : result:='paInt24';
    paInt16   : result:='paInt16';
    paInt8    : result:='paInt8';
    paUInt8   : result:='paUInt8';
    paCustomFormat  : result:='paCustomFormat';
    paNonInterleaved  : result:='paNonInterleaved';
  end;
end;

function GetWaveAudioMixPeakLevel(const Data: Pointer; frameCount: cardinal;
  ASampleFormat: TPaSampleFormat): Integer;

  function GetAudioPeakLevel8Bit: Integer;
  var
    pSample: PByte;
    Max: byte;
    DataSize : cardinal;
  begin
    Max := 0;
    DataSize:=frameCount;
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
    DataSize : cardinal;
  begin
    Max := 0;
    DataSize:=frameCount * 2;
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
  case PaSampleFormat2Bit(ASampleFormat) of
    8: Result := GetAudioPeakLevel8Bit;
    16: Result := GetAudioPeakLevel16Bit;
  else
    Result := -1;
  end;
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


function AudioCallback(input: pointer; output: pointer; frameCount: cardinal;
      timeInfo: PPaStreamCallbackTimeInfo; statusFlags: TPaStreamCallbackFlags;
      inputDevice: pointer): integer; cdecl;
const
  BUFFER_SIZE = 1024;
var
  total : int64;
  i,n : integer;
  datasize  : cardinal;
  sampleFormat  : TPaSampleFormat;
  bytePerSample : cardinal;

  bufltc : array[0..BUFFER_SIZE-1] of ltcsnd_sample_t;
  buffU8BIT  : array of Byte;
  buffU16BIT  : array of word;
begin
  try
    if not assigned(inputDevice) then exit;
    sampleFormat:=PPaStreamParameters(inputDevice).sampleFormat;
    bytePerSample:=PaSampleFormat2Byte(sampleFormat);

    DataSize:=frameCount*bytePerSample;
    SetLength(buffU8BIT, frameCount);

    case bytePerSample of
      2:
      begin
        SetLength(buffU16BIT, frameCount);
        move(pByte(input)[0], buffU16BIT[0], DataSize);
        for I := 0 to frameCount-1 do
          buffU8BIT[i]:=ConvertU16ToU8BIT(buffU16BIT[i]);
      end;
      1:
      begin

        move(pByte(input)[0], buffU8BIT[0], DataSize);
      end;
    end;

    total:=0;
    n:=0;
    for I := 0 to (frameCount-1) div BUFFER_SIZE  do
    begin
      try
      if frameCount-total< BUFFER_SIZE then
        n:=frameCount-total else n:=BUFFER_SIZE;
        move(buffU8BIT[total], bufltc[0], n);
        LibLtc.Write(bufltc, n, total);
      finally
        inc(total, n);
      end;
    end;


   TTask.Create (procedure ()
     begin
      crit.Enter;
      try
        if assigned(inputParameters) then
          form1.Rectangle1.width:=GetWaveAudioMixPeakLevel(input, frameCount, sampleFormat)*4;
      finally
        crit.Leave;
      end;
    end).Start;


  finally
    result := paContinue;
  end;
end;


procedure TForm1.EnumerationHostApi(var list: TList<PPaHostApiInfo>);
const
    ApiPreferenceOrder: array[0..12] of TPaHostApiTypeId = ( paDirectSound,
      paMME,
      paASIO,
      paSoundManager,
      paCoreAudio,
      paOSS,
      paALSA,
      paAL,
      paBeOS,
      paWDMKS,
      paJACK,
      paWASAPI,
      paAudioScienceHPI);
var
  i:        integer;
  apiIndex: TPaHostApiIndex;
  apiInfo:  PPaHostApiInfo;
begin
  list.Clear;
  // select preferred sound-API
  for i:= 0 to High(ApiPreferenceOrder) do
  begin
      // check if API is available
      apiIndex := Pa_HostApiTypeIdToHostApiIndex(ApiPreferenceOrder[i]);
      if apiIndex >= 0 then
      begin
        // we found an API but we must check if it works
        // (on linux portaudio might detect OSS but does not provide
        // any devices if ALSA is enabled)
        new(apiInfo);
        apiInfo := Pa_GetHostApiInfo(apiIndex);
        if apiInfo^.deviceCount > 0 then
        begin
          list.Add(apiInfo)
        end;
      end;
  end;
end;


procedure TForm1.DoDebug(const msg: string);
begin
  form2.Memo1.Lines.Add(msg);
end;

procedure TForm1.LTCDecoderOnDebug(const S: string);
begin
  ;
end;

procedure TForm1.LTCDecoderOnRead(Sender: TObject; timecode : TDateTime);

var
  h,m,s,ms,f : word;
begin
  if timecode<0 then exit;
  CurTimecode:=timecode;

  decodetime(timecode, h,m,s,ms);
  try
    f:=ms div 40;
  except
    f:=0;
  end;

  label5.text:=format('%02.2d:%02.2d:%02.2d:%02.2d',[h,m,s,f]);

end;

procedure TForm1.MenuItem1Click(Sender: TObject);
var
  bSystemtimePrivilege: boolean;
begin
{$IFDEF MSWINDOWS}
  bSystemtimePrivilege:=SetSystemPrivilege('SeSystemtimePrivilege', true);
  if not bSystemtimePrivilege then
  begin
    showmessage('To change system time, you need administrator privilege!');
    exit;
  end;
  //ChangeSystemDateTime();
{$ENDIF}
{$IFDEF MACOS}
    showmessage('Changing system time in MACOS not support!');
{$ENDIF}
end;

procedure TForm1.ArcDial1Change(Sender: TObject);
begin
  label6.Text:=format('%d%%',[round(ArcDial1.Value)]);
end;

procedure TForm1.ComboBox1Change(Sender: TObject);
var
  i             : integer;
  HostApiTypeId : integer;
  HostApiIndex  : integer;

  DefaultInputDeviceIndex: TPaDeviceIndex;
  DeviceInfo  : PPaDeviceInfo;
begin
  HostApiTypeId:=ListHostApi.Items[combobox1.ItemIndex]^.typeId;
  HostApiIndex:=Pa_HostApiTypeIdToHostApiIndex(HostApiTypeId);
  EnumerationDevice(HostApiIndex, ListDevice, true);

  combobox2.Clear;
  for I := 0 to ListDevice.Count-1 do
    combobox2.Items.Add(string(ListDevice.Items[i]^.name));


  (* Set default *)
  DefaultInputDeviceIndex:=Pa_GetDefaultInputDevice();
  DeviceInfo:=Pa_GetDeviceInfo(DefaultInputDeviceIndex);
  for I := 0 to combobox2.Count-1 do
    if SameText(string(DeviceInfo^.name), combobox2.Items.Strings[i]) then
      combobox2.ItemIndex:=i;
end;

procedure TForm1.ComboBox2Change(Sender: TObject);
var
  i                 : integer;
  ListSampleRate    : TList<double>;
  HostApiIndex      : integer;
  deviceIndex       : integer;

  //HostApiIndex  : TPaHostApiIndex;
  //DeviceIndex: TPaDeviceIndex;
  DeviceInfo  : PPaDeviceInfo;

  ListFormats    : TList<TPaSampleFormat>;
  //HostApiIndex      : integer;
begin

  (* List support samplerate *)
  ListSampleRate  := TList<double>.create;
  try
    HostApiIndex:=ListDevice.Items[combobox2.ItemIndex]^.hostApi;    deviceIndex := Pa_HostApiDeviceIndexToDeviceIndex(HostApiIndex, combobox2.ItemIndex);

    EnumerationSampleRate(deviceIndex, ListSampleRate);
    combobox3.Items.Clear;
    for I := 0 to ListSampleRate.Count-1 do
      combobox3.Items.Add(floattostr(ListSampleRate.Items[i]));
  finally
    ListSampleRate.Free;
  end;

  (* Set default samplerate *)
  HostApiIndex:=ListDevice.Items[combobox2.ItemIndex]^.hostApi;
  deviceIndex := Pa_HostApiDeviceIndexToDeviceIndex(HostApiIndex, combobox2.ItemIndex);
  DeviceInfo:=Pa_GetDeviceInfo(deviceIndex);
  for I := 0 to combobox3.Count-1 do
    if SameText(floattostr(DeviceInfo^.defaultSampleRate), combobox3.Items.Strings[i]) then
      combobox3.ItemIndex:=i;

  (* List support formats *)
  ListFormats  := TList<TPaSampleFormat>.create;
  try
    HostApiIndex:=ListDevice.Items[combobox2.ItemIndex]^.hostApi;
    EnumerationFormats(HostApiIndex, ListFormats);
    combobox4.Items.Clear;
    for I := 0 to ListFormats.Count-1 do
      combobox4.Items.Add(PaSampleFormat2String(ListFormats.Items[i]));
  finally
    ListFormats.Free;
  end;

  (* Set default formats paInt16 *)
  for I := 0 to combobox4.Count-1 do
    if SameText(PaSampleFormat2String(paInt16), combobox4.Items.Strings[i]) then
      combobox4.ItemIndex:=i;

end;

procedure TForm1.EnumerationDevice(const hostApiIndex: integer; var list: TList<PPaDeviceInfo>; const isInput: boolean = false);
var
  i           : integer;
  paApiInfo   : PPaHostApiInfo;
  deviceIndex : TPaDeviceIndex;
  DeviceInfo  : PPaDeviceInfo;
begin
  list.Clear;
  paApiInfo  := Pa_GetHostApiInfo(hostApiIndex);
  for i:= 0 to paApiInfo^.deviceCount - 1 do
  begin
    deviceIndex := Pa_HostApiDeviceIndexToDeviceIndex(hostApiIndex, i);
    DeviceInfo:=Pa_GetDeviceInfo(deviceIndex);
    if ((DeviceInfo.maxInputChannels>0) and (isInput)) then
      list.Add(DeviceInfo)
    else
    if ((DeviceInfo.maxOutputChannels>0) and (not isInput)) then
      list.Add(DeviceInfo);
  end;
end;


procedure TForm1.EnumerationSampleRate(const deviceIndex: integer; var list: TList<double>);
const
  standardSampleRates: array[1..13] of double =
    ( 8000.0,  9600.0,  11025.0,  12000.0,  16000.0,
     22050.0, 24000.0,  32000.0,  44100.0,  48000.0,
     88200.0, 96000.0, 192000.0
    );
var
  i : integer;
  deviceInfo:  PPaDeviceInfo;
  inputParameters:  PPaStreamParameters;
  outputParameters: PPaStreamParameters;
  sampleRate:       double;
  PaError:     TPaError;
begin
  deviceInfo  := Pa_GetDeviceInfo(deviceIndex);
  New(inputParameters);
  New(outputParameters);
  try
    if deviceInfo^.maxInputChannels > 0 then
    begin
      inputParameters^.device                    := deviceIndex;
      inputParameters^.channelCount              := deviceInfo^.maxInputChannels;
      inputParameters^.sampleFormat              := paInt16;
      inputParameters^.suggestedLatency          := 0;
      inputParameters^.hostApiSpecificStreamInfo := nil;
      outputParameters := nil;
    end
    else
    begin
      inputParameters := nil;
      outputParameters^.device                    := deviceIndex;
      outputParameters^.channelCount              := deviceInfo^.maxOutputChannels;
      outputParameters^.sampleFormat              := paInt16;
      outputParameters^.suggestedLatency          := 0;
      outputParameters^.hostApiSpecificStreamInfo := nil;
    end;

    for i := low(standardSampleRates) to high(standardSampleRates) do
    begin
      sampleRate := standardSampleRates[i];
      PaError    := Pa_IsFormatSupported(inputParameters, outputParameters, sampleRate);
      if PaError = paFormatIsSupported then
        list.Add(sampleRate);
    end;
  finally
    Dispose(inputParameters);
    Dispose(outputParameters);
  end;
end;

procedure TForm1.EnumerationFormats(const deviceIndex: integer; var list: TList<TPaSampleFormat>);
const
  SampleFormat: array[1..8] of cardinal =
    (paFloat32, paInt32, paInt24,        paInt16,
     paInt8,    paUInt8, paCustomFormat, paNonInterleaved
    );

  SampleFormatName: array[1..8] of string =
    ('paFloat32', 'paInt32', 'paInt24',        'paInt16',
     'paInt8',    'paUInt8', 'paCustomFormat', 'paNonInterleaved'
    );

var
  i : integer;
  deviceInfo:  PPaDeviceInfo;
  inputParameters:  PPaStreamParameters;
  outputParameters: PPaStreamParameters;
  PaError:     TPaError;
begin
  deviceInfo  := Pa_GetDeviceInfo(deviceIndex);
  New(inputParameters);
  New(outputParameters);
  try
    if deviceInfo^.maxInputChannels > 0 then
    begin
      inputParameters^.device                    := deviceIndex;
      inputParameters^.channelCount              := deviceInfo^.maxInputChannels;
      inputParameters^.sampleFormat              := paInt16;
      inputParameters^.suggestedLatency          := 0;
      inputParameters^.hostApiSpecificStreamInfo := nil;
      outputParameters := nil;
    end
    else
    begin
      inputParameters := nil;
      outputParameters^.device                    := deviceIndex;
      outputParameters^.channelCount              := deviceInfo^.maxOutputChannels;
      outputParameters^.sampleFormat              := paInt16;
      outputParameters^.suggestedLatency          := 0;
      outputParameters^.hostApiSpecificStreamInfo := nil;
    end;

    for i := low(SampleFormat) to high(SampleFormat) do
    begin
      if inputParameters <> nil then
        inputParameters^.sampleFormat := SampleFormat[i]
      else
        outputParameters^.sampleFormat := SampleFormat[i];
      PaError := Pa_IsFormatSupported(inputParameters, outputParameters, deviceInfo^.defaultSampleRate);
      if PaError = paFormatIsSupported then
        list.Add(SampleFormat[i])
    end;

  finally
    Dispose(inputParameters);
    Dispose(outputParameters);
  end;
end;

procedure TForm1.FormCloseQuery(Sender: TObject; var CanClose: Boolean);
begin
  if assigned(AudioStream) then
  begin
     CanClose:= Pa_IsStreamActive(AudioStream) = 0  ;
  end;
end;

procedure TForm1.FormCreate(Sender: TObject);
begin
  crit := TCriticalSection.Create;

  ListHostApi := TList<PPaHostApiInfo>.create;
  ListDevice  := TList<PPaDeviceInfo>.create;
  inputParameters   := nil;
  outputParameters  := nil;
end;

procedure TForm1.FormDestroy(Sender: TObject);
begin
  ListHostApi.Clear;
  ListHostApi.Free;
  ListDevice.Clear;
  ListDevice.Free;
  PaError := Pa_Terminate;

  crit.Free;
end;

procedure TForm1.FormKeyUp(Sender: TObject; var Key: Word; var KeyChar: Char;
  Shift: TShiftState);
begin
  if ((ssCtrl in Shift) and ((KeyChar='d') or (KeyChar='D') or (Key=68)))  then
  begin
    form2.Show;
  end;
end;

procedure TForm1.FormShow(Sender: TObject);
const
    RT_FONT       = PChar(8);
var
  i : integer;
{$IFDEF MSWINDOWS}
  Res : TResourceStream;
  FontCount: cardinal;
  hFontRes :THandle;
{$ENDIF}

{$IFDEF MACOS}
  FontGlyphManager : TMacFontGlyphManager;
{$ENDIF}
begin
  DoDebug('Pa_Initialize');

  PaError    := Pa_Initialize;
  if PaError>0 then
    DoDebug(format('paError: %s', [Pa_GetErrorText(PaError)]));

  DoDebug('*** Test of Pa_GetVersion and Pa_GetVersionText ***');
  DoDebug(format('Pa_GetVersion:     %d', [Pa_GetVersion]));
  DoDebug(format('Pa_GetVersionText: %s', [string(Pa_GetVersionText)]));

  (* List audio firmware*)
  EnumerationHostApi(ListHostApi);
  combobox1.Items.Clear;
  for I := 0 to ListHostApi.Count-1 do
    combobox1.Items.Add(string(ListHostApi.Items[i]^.name));
  (* Set Default firmware *)
  HostApiIndexDef:=Pa_GetDefaultHostApi;
  HostApiInfoDef:=Pa_GetHostApiInfo(HostApiIndexDef);
  for I := 0 to combobox1.Count-1 do
    if SameText(string(HostApiInfoDef.name), combobox1.Items.Strings[i]) then
      combobox1.ItemIndex:=i;


  ArcDial1.ValueRange.Max:=100;
  ArcDial1.ValueRange.Min:=0;
  ArcDial1.ValueRange.Value:=50;

{$IFDEF MACOS}
  //Typeface := TJTypeface.JavaClass.Create(FamilyName, TypefaceFlag);
  //FontGlyphManager:=TMacFontGlyphManager.Create;
  //label5.Font.Family:='DS-Digital';
{$ENDIF}

{$IFDEF MSWINDOWS}
  try
    Res := TResourceStream.Create(hInstance, 'DSDIGI', RT_FONT);
    try
      hFontRes := AddFontMemResourceEx(Res.Memory, Res.Size, nil, @FontCount);
      if hFontRes > 0 then
      begin
        label5.Font.Family := 'DS-Digital';
      end;
    finally
      Res.Free;
    end;
  except
    ;
  end;
{$ENDIF}

end;

procedure TForm1.Switch1Switch(Sender: TObject);
var
  HostApiIndex      : integer;
  deviceIndex       : integer;
  deviceInfo        : PPaDeviceInfo;
  sampleRate        : double;
  framesPerBuffer   : cardinal;
  streamFlags       : TPaStreamFlags;
  streamCallback    : PPaStreamCallback;
  userData          : Pointer;
  sampleFormat      : TPaSampleFormat;
begin

  if Switch1.IsChecked then
  begin
    (* Disable GUI *)
    combobox1.Enabled:=false;
    combobox2.Enabled:=false;
    combobox3.Enabled:=false;
    combobox4.Enabled:=false;

    HostApiIndex:=ListDevice.Items[combobox2.ItemIndex]^.hostApi;
    deviceIndex := Pa_HostApiDeviceIndexToDeviceIndex(HostApiIndex, combobox2.ItemIndex);
    deviceInfo  := Pa_GetDeviceInfo(deviceIndex);
    //format('Device[%d]: %s', [i, deviceInfo^.name]);

    New(inputParameters);
    New(outputParameters);

    sampleFormat:=String2PaSampleFormat(ComboBox4.Items[ComboBox4.ItemIndex]);

    if deviceInfo^.maxInputChannels > 0 then
    begin
          inputParameters^.device                    := deviceIndex;
          inputParameters^.channelCount              := 1;
          inputParameters^.sampleFormat              := sampleFormat;
          inputParameters^.suggestedLatency          := deviceInfo.defaultHighInputLatency;
          inputParameters^.hostApiSpecificStreamInfo := nil;
          outputParameters := nil;
    end
    else
    begin
          inputParameters := nil;
          outputParameters^.device                    := deviceIndex;
          outputParameters^.channelCount              := deviceInfo^.maxOutputChannels;
          outputParameters^.sampleFormat              := sampleFormat;
          outputParameters^.suggestedLatency          := deviceInfo.defaultLowOutputLatency;
          outputParameters^.hostApiSpecificStreamInfo := nil;
    end;

    sampleRate      := strtofloat(combobox3.Items[combobox3.ItemIndex]);
    framesPerBuffer := paFramesPerBufferUnspecified;
    streamFlags     := paNoFlag;
    streamCallback  := @AudioCallback;
    userData        := inputParameters;



    PaError := Pa_OpenStream(
                         AudioStream,
                         inputParameters,
                         outputParameters,
                         sampleRate,
                         framesPerBuffer,
                         streamFlags,
                         streamCallback,
                         userData
             );
    if (PaError <> paNoError) or (AudioStream = nil) then
      showmessage(format('Pa_OpenStream: %s', [Pa_GetErrorText(PaError)]));

    if (PaError = paNoError) and (AudioStream <> nil) then
    begin
      PaError := Pa_StartStream(AudioStream);
      if PaError <> paNoError then
        showmessage(format('Pa_StartStream: %s', [Pa_GetErrorText(PaError)]));
    end;



    LibLtc:=TLibLtc.Create();
    LibLtc.decoder.queue_len:=32;
    LibLtc.OnRead:=LTCDecoderOnRead;
    LibLtc.decoder.APV:=44100 div 25;
    LibLtc.Start;
  end else
  begin
    if Pa_IsStreamStopped(AudioStream) = 0 then
    begin
      PaError := Pa_StopStream(AudioStream);
      if (PaError <> paNoError) then
        showmessage(format('Pa_StopStream: %s', [Pa_GetErrorText(PaError)]));
    end else
    begin
      showmessage('Error: Non working device');
      PaError := Pa_AbortStream(AudioStream);
      if (PaError <> paNoError) then
        showmessage(format('Pa_AbortStream: %s', [Pa_GetErrorText(PaError)]));
    end;

    PaError := Pa_CloseStream(AudioStream);
    if PaError <> paNoError then
      showmessage(format('Pa_CloseStream: %s', [Pa_GetErrorText(PaError)]));

    AudioStream:=nil;

    dispose(inputParameters);
    dispose(outputParameters);




    LibLtc.Destroy;

    (* Enable GUI *)
    combobox1.Enabled:=true;
    combobox2.Enabled:=true;
    combobox3.Enabled:=true;
    combobox4.Enabled:=true;
  end;

end;

end.
