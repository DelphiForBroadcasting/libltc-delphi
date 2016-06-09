unit FH.LIBLTC;

interface

uses
  System.SysUtils, System.Classes, System.Types, System.Generics.Collections, FH.LIBLTC.LTC, FH.LIBLTC.TIMECODE, FH.LIBLTC.DECODER;

Type
  TfhVer = record
    name        : string;
    version     : cardinal;
    versionStr  : string;
    dateRelease : TDateTime;
  end;

Const
  FHLIBLTSVER  : TfhVer = (Name : 'FH.LIBLTC'; version : $01000011; versionStr : ''; dateRelease : 41534.0);

Const
 MSG_READ = 1;
 MSG_DEBUG = 1;

type
 TReadMessage = record
  Msg: DWORD;
  TC : TDateTime
 end;

type
  TDecoderOnRead = procedure(Sender: TObject; timecode : TDateTime) of object;
  TNotifyEvent = procedure(Sender: TObject) of object;
  TGetStrProc = procedure(const S: string) of object;

type
  TLibLTC = class(TThread)
  private
    FOnError              : TGetStrProc;
    FOnDebug              : TGetStrProc;
    FOnRead               : TDecoderOnRead;

    FDecoder              : TLTCDecoder;

    Function GetVersion : TfhVer;

    procedure DoError(const str : string);
    procedure DoDebug(const str : string);
    procedure DoRead(var Msg: TReadMessage); message MSG_READ;
    procedure QueueNotifyEvent(Sender: TObject; const Item: TLTCFrameExt;
        Action: TCollectionNotification);

  protected
    procedure Execute; override;
 public
    constructor create(); overload;
    destructor Destroy; override;
    procedure Terminate;

    property decoder: TLTCDecoder read FDecoder write FDecoder;
    property Version : TfhVer read GetVersion;
    procedure Write(buf: array of ltcsnd_sample_t; size : size_t;  posinfo : ltc_off_t);
    function Read: TLTCFrameExt;
    property OnError: TGetStrProc read FOnError write  FOnError;
    property OnDebug: TGetStrProc read FOnDebug write  FOnDebug;
    property OnRead: TDecoderOnRead read FOnRead write  FOnRead;
  end;


implementation

constructor TLibLTC.create();
begin
  inherited Create(true);

  FDecoder := TLTCDecoder.create();
  FDecoder.queue_len:=32;
  //FDecoder.Queue.OnNotify:=QueueNotifyEvent;
  // Priority := TThreadPriority.tpLower;
  FreeOnTerminate:=false;
end;

destructor TLibLTC.Destroy;
begin
  FDecoder.Destroy;
  inherited Destroy;
end;

procedure TLibLTC.Terminate;
begin
  inherited Terminate;
end;

procedure TLibLTC.Execute;
var
  LTCFrameExt: TLTCFrameExt;
  SMPTETimecode : TSMPTETimecode;
  r_time: TDateTime;
  r_date : TDateTime;
  DateTimeTimecode  : TDateTime;
begin
  while not Terminated do
  begin

    if FDecoder.Read(LTCFrameExt) then
    begin
      ltc_frame_to_time(SMPTETimecode, LTCFrameExt.ltc, 0);
      TryEncodeTime(SMPTETimecode.hours, SMPTETimecode.mins, SMPTETimecode.secs, SMPTETimecode.frame * 40, r_time);
      TryEncodeDate(SMPTETimecode.years, SMPTETimecode.months, SMPTETimecode.days, r_date);
      DateTimeTimecode:=r_date+r_time;
      synchronize(procedure
      begin
        if assigned(FOnRead) then FOnRead(self, DateTimeTimecode);
      end);
    end;
    sleep(10);
  end;
end;

Function TLibLTC.GetVersion : TfhVer;
begin
  result.name:=FHLIBLTSVER.name;
  result.version:=FHLIBLTSVER.version;
  result.versionStr:=format('%s-%d.%d.%d.%d', [FHLIBLTSVER.name,
  FHLIBLTSVER.version shr 24 and $FF,
  FHLIBLTSVER.version shr 16 and $FF,
  FHLIBLTSVER.version shr 8 and $FF,
  FHLIBLTSVER.version and $FF]);
  result.dateRelease:=FHLIBLTSVER.dateRelease;
end;


procedure TLibLTC.DoError(const str : string);
begin
  if assigned(FOnError) then FOnError(str);
  {$IfDef OnDebug}
    DoDebug(format('TfhDSPlayer.DoError(str=%s)', [str]));
  {$EndIf}
end;

procedure TLibLTC.DoDebug(const str : string);
begin
  if assigned(FOnDebug) then FOnDebug(str);
end;

procedure TLibLTC.QueueNotifyEvent(Sender: TObject; const Item: TLTCFrameExt;
    Action: TCollectionNotification);
var
  stime : TSMPTETimecode;
  r_time: TDateTime;
  r_date : TDateTime;
  Msg: TReadMessage;
begin
  case Action of
    cnAdded:
      begin
        ltc_frame_to_time(stime, Item.ltc, 0);

        TryEncodeTime(stime.hours, stime.mins, stime.secs, stime.frame * 40, r_time);
        TryEncodeDate(stime.years, stime.months, stime.days, r_date);

        with Msg do
         begin
          Msg := MSG_READ;
          TC := r_date+r_time;
         end;
        self.Dispatch(Msg);
      end;
  end;
end;

procedure TLibLTC.DoRead(var Msg: TReadMessage);
begin
  if assigned(FOnRead) then
  begin
    TMonitor.Enter(Self);
    try
      FOnRead(self, Msg.TC);
      TMonitor.PulseAll(Self);
    finally
      TMonitor.Exit(Self);
    end;
  end;
end;

procedure TLibLTC.Write(buf: array of ltcsnd_sample_t; size : size_t;  posinfo : ltc_off_t);
begin
  FDecoder.Write(buf, size, posinfo);
end;

function TLibLTC.Read: TLTCFrameExt;
var
  LTCFrameExt: TLTCFrameExt;
begin
  FDecoder.Read(LTCFrameExt);
  result:=LTCFrameExt;
end;


end.