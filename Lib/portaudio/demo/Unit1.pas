unit Unit1;

interface

uses
  System.SysUtils, System.Types, System.UITypes, System.Classes, System.Variants,
  FMX.Types, FMX.Controls, FMX.Forms, FMX.Graphics, FMX.Dialogs, portaudio,
  FMX.StdCtrls, FMX.Layouts, FMX.Memo, FMX.Menus, System.Threading, System.SyncObjs;

const
  paDefaultApi = -1;

  ApiPreferenceOrder:
{$IF Defined(MSWINDOWS)}
    // Note1: Portmixer has no mixer support for paASIO and paWASAPI at the moment
    // Note2: Windows Default-API is MME, but DirectSound is faster
    array[0..0] of TPaHostApiTypeId = ( paDirectSound );
{$ELSEIF Defined(MACOS)}
    array[0..0] of TPaHostApiTypeId = ( paCoreAudio ); // paCoreAudio
{$ELSEIF Defined(UNIX)}
    // Note: Portmixer has no mixer support for JACK at the moment
    array[0..2] of TPaHostApiTypeId = ( paALSA, paJACK, paOSS );
{$ELSE}
    array[0..0] of TPaHostApiTypeId = ( paDefaultApi );
{$IFEND}

  standardSampleRates: array[1..13] of double =
    ( 8000.0,  9600.0,  11025.0,  12000.0,  16000.0,
     22050.0, 24000.0,  32000.0,  44100.0,  48000.0,
     88200.0, 96000.0, 192000.0
    );

  SampleFormat: array[1..8] of cardinal =
    (paFloat32, paInt32, paInt24,        paInt16,
     paInt8,    paUInt8, paCustomFormat, paNonInterleaved
    );
  SampleFormatName: array[1..8] of string =
    ('paFloat32', 'paInt32', 'paInt24',        'paInt16',
     'paInt8',    'paUInt8', 'paCustomFormat', 'paNonInterleaved'
    );



type
  TForm1 = class(TForm)
    Memo1: TMemo;
    StatusBar1: TStatusBar;
    Panel1: TPanel;
    Button1: TButton;
    Button2: TButton;
    Button3: TButton;
    Button4: TButton;
    Button5: TButton;
    Button6: TButton;
    MainMenu1: TMainMenu;
    MenuItem1: TMenuItem;
    procedure DoDebug(msg: string);
    procedure Button1Click(Sender: TObject);
    procedure TestVersion();
    procedure TestApiInfo();
    procedure TestDeviceInfo();
    procedure TestFormatInfo();
    procedure TestErrorText();
    procedure TestStreams();
    procedure Button2Click(Sender: TObject);
    procedure Button3Click(Sender: TObject);
    procedure Button4Click(Sender: TObject);
    procedure Button5Click(Sender: TObject);
    procedure Button6Click(Sender: TObject);
    procedure MenuItem1Click(Sender: TObject);
    procedure FormCreate(Sender: TObject);
    procedure FormDestroy(Sender: TObject);
  private
    { Private declarations }
  public
    { Public declarations }
  end;
var
  Form1: TForm1;
  PaError:     TPaError;
  paApiIndex:  TPaHostApiIndex;
  paApiInfo:   PPaHostApiInfo;
  deviceIndex: TPaDeviceIndex;
  deviceInfo:  PPaDeviceInfo;
  inputParameters:  PPaStreamParameters;
  outputParameters: PPaStreamParameters;
  sampleRate:       double;
  stream:           PPaStream;
  framesPerBuffer:  cardinal;
  streamFlags:      TPaStreamFlags;
  streamCallback:   PPaStreamCallback;
  callbackStartTime: TDateTime;
  callbackWorks:    boolean;
  userData:         Pointer;
  FLock: TCriticalSection;

implementation

{$R *.fmx}

function GetPreferredApiIndex(): TPaHostApiIndex;
var
  i:        integer;
  apiIndex: TPaHostApiIndex;
  apiInfo:  PPaHostApiInfo;
begin
  result := -1;

  // select preferred sound-API
  for i:= 0 to High(ApiPreferenceOrder) do
  begin
    if (ApiPreferenceOrder[i] <> paDefaultApi) then
    begin
      // check if API is available
      apiIndex := Pa_HostApiTypeIdToHostApiIndex(ApiPreferenceOrder[i]);
      if apiIndex >= 0 then
      begin
        // we found an API but we must check if it works
        // (on linux portaudio might detect OSS but does not provide
        // any devices if ALSA is enabled)
        apiInfo := Pa_GetHostApiInfo(apiIndex);
        if apiInfo^.deviceCount > 0 then
        begin
          Result := apiIndex;
          break;
        end;
      end;
    end;
  end;

  // None of the preferred APIs is available -> use default
  if (result < 0) then
  begin
    result := Pa_GetDefaultHostApi();
  end;
end;



procedure TForm1.DoDebug(msg: string);
begin
  FLock.Enter;
  try
    memo1.lines.add(msg);
  finally
    FLock.Leave;
  end;
end;

procedure TForm1.FormCreate(Sender: TObject);
begin
  FLock:= TCriticalSection.Create;
end;

procedure TForm1.FormDestroy(Sender: TObject);
begin
  FLock.Free;
end;

procedure TForm1.TestErrorText();
var
  i : integer;
begin
  DoDebug('*** Test of Pa_GetErrorText ***');
  PaError := Pa_Initialize;
  DoDebug(format('paNoError (0): %s', [Pa_GetErrorText(PaError)]));
  DoDebug('Code   Text');
  DoDebug('------------------------------------');
  i := paNotInitialized;
  repeat
    DoDebug(format('%d: %s', [i, Pa_GetErrorText(i)]));
    i := succ(i);
  until SameText(Pa_GetErrorText(i), 'Invalid error code') or (i = paNotInitialized + 100);
  DoDebug(format('%d: %s', [i, Pa_GetErrorText(i)]));
  PaError := Pa_Terminate;
end;


procedure TForm1.TestVersion();
begin
  DoDebug('*** Test of Pa_GetVersion and Pa_GetVersionText ***');
  PaError := Pa_Initialize;
  DoDebug(format('Pa_GetVersion:     %d', [Pa_GetVersion]));
  DoDebug(format('Pa_GetVersionText: %s', [string(Pa_GetVersionText)]));
  PaError := Pa_Terminate;
end;

procedure TForm1.Button2Click(Sender: TObject);
begin
  TTask.Create (procedure ()
   begin
    TestVersion;
   end).Start;
end;

procedure TForm1.Button3Click(Sender: TObject);
begin
  TestApiInfo;
end;

procedure TForm1.Button4Click(Sender: TObject);
begin
  TestDeviceInfo;
end;

procedure TForm1.Button5Click(Sender: TObject);
begin
  TTask.Create (procedure ()
   begin
    TestFormatInfo;
   end).Start;
end;

procedure TForm1.Button6Click(Sender: TObject);
begin
  TTask.Create (procedure ()
   begin
    TestStreams;
   end).Start;
end;

procedure TForm1.MenuItem1Click(Sender: TObject);
begin
  showmessage('FreeHand 2015');
end;

procedure TForm1.TestApiInfo();
var
  i : integer;
begin
  DoDebug('*** Test of GetPreferredApiIndex ***');
  PaError    := Pa_Initialize;
  paApiIndex := GetPreferredApiIndex();
  if paApiIndex = -1 then
    DoDebug('GetPreferredApiIndex: No working Audio-API found.')
  else
    DoDebug(format('GetPreferredApiIndex: working Audio-API found. No: %d', [paApiIndex]));
  PaError := Pa_Terminate;


  DoDebug('*** Test of Pa_GetHostApiInfo ***');
  PaError    := Pa_Initialize;
  paApiIndex := GetPreferredApiIndex();
  paApiInfo  := Pa_GetHostApiInfo(paApiIndex);
  DoDebug('Pa_GetHostApiInfo:');
  DoDebug(format('paApiInfo.structVersion:       %d', [paApiInfo.structVersion]));
  DoDebug(format('paApiInfo._type:               %d', [paApiInfo._type]));
  DoDebug(format('paApiInfo.name:                %s', [paApiInfo.name]));
  DoDebug(format('paApiInfo.deviceCount:         %d', [paApiInfo.deviceCount]));
  DoDebug(format('paApiInfo.defaultInputDevice:  %d', [paApiInfo.defaultInputDevice]));
  DoDebug(format('paApiInfo.defaultOutputDevice: %d', [paApiInfo.defaultOutputDevice]));
  PaError := Pa_Terminate;


  DoDebug('*** Test of Pa_HostApiDeviceIndexToDeviceIndex ***');
  PaError    := Pa_Initialize;
  paApiIndex := GetPreferredApiIndex();
  paApiInfo  := Pa_GetHostApiInfo(paApiIndex);
  for i:= 0 to paApiInfo^.deviceCount-1 do
  begin
    deviceIndex := Pa_HostApiDeviceIndexToDeviceIndex(paApiIndex, i);
    DoDebug(format('deviceIndex[%d]: ', [deviceIndex]));
  end;
  PaError := Pa_Terminate;

end;


procedure TForm1.TestDeviceInfo();
var
  i : integer;
begin
  DoDebug ('*** Test of Pa_GetDeviceCount ***');
  PaError := Pa_Initialize;
  DoDebug(format('Pa_GetDeviceCount: %d', [Pa_GetDeviceCount]));
  PaError := Pa_Terminate;

  DoDebug ('*** Test of Pa_GetDefaultInputDevice ***');
  PaError := Pa_Initialize;
  DoDebug(format('Pa_GetDefaultInputDevice: %d', [Pa_GetDefaultInputDevice]));
  PaError := Pa_Terminate;

  DoDebug ('*** Test of Pa_GetDefaultOutputDevice ***');
  PaError := Pa_Initialize;
  DoDebug(format('Pa_GetDefaultOutputDevice: %d', [Pa_GetDefaultOutputDevice]));
  PaError := Pa_Terminate;

  DoDebug ('*** Test of Pa_GetDeviceInfo ***');
// Note: the fields of deviceInfo can also be used without the '^'.
// deviceInfo.name works as well as deviceInfo^.name
  PaError    := Pa_Initialize;
  paApiIndex := GetPreferredApiIndex();
  paApiInfo  := Pa_GetHostApiInfo(paApiIndex);
  for i:= 0 to paApiInfo^.deviceCount - 1 do
  begin
    deviceIndex := Pa_HostApiDeviceIndexToDeviceIndex(paApiIndex, i);
    deviceInfo  := Pa_GetDeviceInfo(deviceIndex);
     DoDebug(format('deviceInfo[%d].name:                     %s', [i, deviceInfo^.name]));
     DoDebug(format('deviceInfo[%d].structVersion:            %d (should be 2)',[i, deviceInfo^.structVersion]));
     DoDebug(format('deviceInfo[%d].hostApi:                  %d', [i, deviceInfo^.hostApi]));
     DoDebug(format('deviceInfo[%d].maxInputChannels:         %d', [i, deviceInfo^.maxInputChannels]));
     DoDebug(format('deviceInfo[%d].maxOutputChannels:        %d', [i, deviceInfo^.maxOutputChannels]));
     DoDebug(format('deviceInfo[%d].defaultLowInputLatency:   %f', [i, deviceInfo^.defaultLowInputLatency]));
     DoDebug(format('deviceInfo[%d].defaultLowOutputLatency:  %f', [i, deviceInfo^.defaultLowOutputLatency]));
     DoDebug(format('deviceInfo[%d].defaultHighInputLatency:  %f', [i, deviceInfo^.defaultHighInputLatency]));
     DoDebug(format('deviceInfo[%d].defaultHighOutputLatency: %f', [i, deviceInfo^.defaultHighOutputLatency]));
     DoDebug(format('deviceInfo[%d].defaultSampleRate:        %f', [i, deviceInfo^.defaultSampleRate]));
  end;
  PaError := Pa_Terminate;
end;


procedure TForm1.TestFormatInfo();
var
  i, j  : integer;
  console : TStringList;
begin

  console := TStringList.Create;
  try
    console.add('*** Test of Pa_IsFormatSupported ***');
    PaError    := Pa_Initialize;
    paApiIndex := GetPreferredApiIndex();
    paApiInfo  := Pa_GetHostApiInfo(paApiIndex);
    for i:= 0 to paApiInfo^.deviceCount - 1 do
    begin
      deviceIndex := Pa_HostApiDeviceIndexToDeviceIndex(paApiIndex, i);
      deviceInfo  := Pa_GetDeviceInfo(deviceIndex);
      console.add(format('Device[%d]: %s', [i, deviceInfo^.name]));
      New(inputParameters);
      New(outputParameters);

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

      sampleRate := deviceInfo^.defaultSampleRate;
      PaError    := Pa_IsFormatSupported(inputParameters, outputParameters, sampleRate);
      if PaError = paFormatIsSupported then
        console.add(format('Sample rate: %f: supported',[sampleRate]))
      else
        console.add(format('Sample rate: %f : Error: %s', [sampleRate, Pa_GetErrorText(PaError)]));

      for j := low(standardSampleRates) to high(standardSampleRates) do
      begin
        sampleRate := standardSampleRates[j];
        PaError    := Pa_IsFormatSupported(inputParameters, outputParameters, sampleRate);
        if PaError = paFormatIsSupported then
          console.add(format('Sample rate: %f: supported',[sampleRate]))
        else
          console.add(format('Sample rate: %f : Error: %s', [sampleRate, Pa_GetErrorText(PaError)]));
      end;

      for j := low(SampleFormat) to high(SampleFormat) do
      begin
        if inputParameters <> nil then
          inputParameters^.sampleFormat := SampleFormat[j]
        else
          outputParameters^.sampleFormat := SampleFormat[j];
        PaError := Pa_IsFormatSupported(inputParameters, outputParameters, sampleRate);
        if PaError = paFormatIsSupported then
          console.add(format('Sample Format %s: supported', [SampleFormatName[j]]))
        else
          console.add(format('Sample Format %s: %s', [SampleFormatName[j], Pa_GetErrorText(PaError)]));
      end;

      Dispose(inputParameters);
      Dispose(outputParameters);
    end;
    PaError := Pa_Terminate;
    memo1.text:=console.text;
  finally
    console.free;
  end;
end;


function AudioCallback(input: pointer; output: pointer; frameCount: cardinal;
      timeInfo: PPaStreamCallbackTimeInfo; statusFlags: TPaStreamCallbackFlags;
      inputDevice: pointer): integer; cdecl;
var
  duration: real;
begin
  duration := (Now() - callbackStartTime) * 24 * 3600;
  if duration < 2.0 then
    result := paContinue
  else
  begin
    callbackWorks := true;
    result := paComplete;
  end;
end;

procedure TForm1.TestStreams();
var
  i : integer;
  console : TStringList;
begin
  console:=TStringList.create;
  try
    console.add('*** Test of Pa_OpenStream and Pa_CloseStream ***');
    PaError    := Pa_Initialize;
    paApiIndex := GetPreferredApiIndex();
    paApiInfo  := Pa_GetHostApiInfo(paApiIndex);
    for i:= 0 to paApiInfo^.deviceCount - 1 do
    begin
    deviceIndex := Pa_HostApiDeviceIndexToDeviceIndex(paApiIndex, i);
      deviceInfo  := Pa_GetDeviceInfo(deviceIndex);
      console.add(format('Device[%d]: %s', [i, deviceInfo^.name]));
      New(inputParameters);
      New(outputParameters);
      if deviceInfo^.maxInputChannels > 0 then
      begin
        inputParameters^.device                    := deviceIndex;
        inputParameters^.channelCount              := deviceInfo^.maxInputChannels;
        inputParameters^.sampleFormat              := paInt16;
        inputParameters^.suggestedLatency          := deviceInfo.defaultHighInputLatency;
        inputParameters^.hostApiSpecificStreamInfo := nil;
        outputParameters := nil;
      end
      else
      begin
        inputParameters := nil;
        outputParameters^.device                    := deviceIndex;
        outputParameters^.channelCount              := deviceInfo^.maxOutputChannels;
        outputParameters^.sampleFormat              := paInt16;
        outputParameters^.suggestedLatency          := deviceInfo.defaultLowOutputLatency;
        outputParameters^.hostApiSpecificStreamInfo := nil;
      end;

      sampleRate      := deviceInfo^.defaultSampleRate;
      framesPerBuffer := paFramesPerBufferUnspecified;
      streamFlags     := paNoFlag;
      streamCallback  := @AudioCallback;
      userData        := nil;

      PaError := Pa_OpenStream(
                       stream,
                       inputParameters,
                       outputParameters,
                       sampleRate,
                       framesPerBuffer,
                       streamFlags,
                       streamCallback,
                       userData
           );
      if (PaError = paNoError) and (stream <> nil) then
        console.add('Pa_OpenStream: success')
      else
        console.add(format('Pa_OpenStream: %s', [Pa_GetErrorText(PaError)]));

      if (PaError = paNoError) and (stream <> nil) then
      begin
        callbackStartTime := Now();

        PaError := Pa_StartStream(stream);
        if PaError = paNoError then
          console.add('Pa_StartStream: success')
        else
          console.add(format('Pa_StartStream: %s', [Pa_GetErrorText(PaError)]));

        callbackWorks := false;

        // wait twice the time a successful callback would need for termination
        console.add('Wait for callback');
        sleep(4000);

        if callbackWorks and (Pa_IsStreamStopped(stream) = 0) then
        begin
         console.add('Success: Device works');
          PaError := Pa_StopStream(stream);
          if (PaError = paNoError) then
            console.add('Pa_StopStream: success')
          else
            console.add(format('Pa_StopStream: %s', [Pa_GetErrorText(PaError)]));
        end
        else
        begin
          console.add('Error: Non working device');
          PaError := Pa_AbortStream(stream);
          if (PaError = paNoError) then
            console.add('Pa_AbortStream: success')
          else
            console.add(format('Pa_AbortStream: %s', [Pa_GetErrorText(PaError)]));

        end;
      end;

      PaError := Pa_CloseStream(stream);
      if PaError = paNoError then
        console.add('Pa_CloseStream: success')
      else
        console.add(format('Pa_CloseStream: %s', [Pa_GetErrorText(PaError)]));

      Dispose(inputParameters);
      Dispose(outputParameters);

    end;
    PaError := Pa_Terminate;
    memo1.text:=console.text;
  finally
    console.free;
  end;
end;



procedure TForm1.Button1Click(Sender: TObject);
begin
  TestErrorText;
end;

end.
