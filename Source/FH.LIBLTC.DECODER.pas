(*
   libltc - en+decode linear timecode

   Copyright (C) 2005 Maarten de Boer <mdeboer@iua.upf.es>
   Copyright (C) 2006-2012 Robin Gareus <robin@gareus.org>
   Copyright (C) 2008-2009 Jan <jan@geheimwerk.de>

   Binary constant generator macro for endianess conversion
   by Tom Torfs - donated to the public domain

   This program is free software; you can redistribute it and/or modify
   it under the terms of the GNU Lesser General Public License as
   published by the Free Software Foundation, either version 3 of the
   License, or (at your option) any later version.

   This program is distributed in the hope that it will be useful,
   but WITHOUT ANY WARRANTY; without even the implied warranty of
   MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
   GNU Lesser General Public License for more details.

   You should have received a copy of the GNU Lesser General Public
   License along with this library.
   If not, see <http://www.gnu.org/licenses/>.

   Conversion to Pascal Copyright 2015 (c) Oleksandr Nazaruk <support@freehand.com.ua>
   based on libltc-1.1.4

*)

unit FH.LIBLTC.DECODER;

interface

uses
   System.SysUtils, System.Types,  System.Generics.Collections, Math, FH.LIBLTC.LTC, FH.LIBLTC.TIMECODE;

{$M+}

type
  TLTCDecoder = class
  private
    FQueue                : TQueue<TLTCFrameExt>;
    FQueue_Len            : integer;
	  FBiphase_state        : byte;
	  FBiphase_prev         : byte;
	  FSnd_to_biphase_state : byte;
	  FSnd_to_biphase_cnt   : int64;		///< counts the samples in the current period
	  FSnd_to_biphase_lmt   : integer;	///< specifies when a state-change is considered biphase-clock or 2*biphase-clock
	  fSnd_to_biphase_period: Single;	///< track length of a period - used to set snd_to_biphase_lmt
    FSnd_to_biphase_min   : ltcsnd_sample_t;
	  FSnd_to_biphase_max   : ltcsnd_sample_t;
	  FDecoder_sync_word    : word;
	  FLtc_frame            : TLTCFrameRAW;
	  FBit_cnt              : integer;
	  FFrame_start_off      : ltc_off_t;
	  FFrame_start_prev     : ltc_off_t;
	  FBiphase_tics         : array[0..LTC_FRAME_BIT_COUNT-1] of Single;
	  FBiphase_tic          : integer;
    function  GetBiphaseTics(index: integer): Single;
    procedure SetBiphaseTics(index: integer; const Value: Single);
    procedure SetAPV(value: integer);
  protected
    property  biphase_tics[index: integer]: Single read GetBiphaseTics write SetBiphaseTics;
  public
    constructor create(); overload;
    destructor Destroy; override;
    procedure Write(buf: array of ltcsnd_sample_t; size : size_t;  posinfo : ltc_off_t);
    function Read(var frame: TLTCFrameExt): boolean;
    property Queue: TQueue<TLTCFrameExt> read FQueue write FQueue;
    property queue_len: integer read FQueue_len write FQueue_len;
    property decoder_sync_word : word read FDecoder_sync_word write FDecoder_sync_word;
    property frame_start_off : ltc_off_t read FFrame_start_off write FFrame_start_off;
    property frame_start_prev : ltc_off_t read FFrame_start_prev write FFrame_start_prev;
    property ltc_frame : TLTCFrameRAW read FLtc_frame write FLtc_frame;
    property biphase_prev : byte read FBiphase_prev write  FBiphase_prev;
    property biphase_state : byte read FBiphase_state write  FBiphase_state;
    property biphase_tic : integer read FBiphase_tic write  FBiphase_tic;
    property bit_cnt : integer read FBit_cnt write  FBit_cnt;
    property snd_to_biphase_period : Single read fSnd_to_biphase_period write  fSnd_to_biphase_period;
    property snd_to_biphase_lmt : integer read FSnd_to_biphase_lmt write  FSnd_to_biphase_lmt;
    property snd_to_biphase_cnt : int64 read FSnd_to_biphase_cnt write  FSnd_to_biphase_cnt;
    property snd_to_biphase_state : byte read FSnd_to_biphase_state write  FSnd_to_biphase_state;
    property snd_to_biphase_min : ltcsnd_sample_t read FSnd_to_biphase_min write  FSnd_to_biphase_min;
    property snd_to_biphase_max : ltcsnd_sample_t read FSnd_to_biphase_max write  FSnd_to_biphase_max;
    property apv: integer write SetAPV;
  end;


function calc_volume_db(d : TLTCDecoder): Single;
Procedure parse_ltc(d : TLTCDecoder; bit: byte; offset: integer;  posinfo: ltc_off_t);
procedure biphase_decode2(d : TLTCDecoder; offset : integer; pos: ltc_off_t); inline;
function decode_ltc(d: TLTCDecoder; sound: array of ltcsnd_sample_t; size: size_t; posinfo: ltc_off_t): integer;


implementation

(* OK *)
function calc_volume_db(d : TLTCDecoder): Single;
begin
	if (d.snd_to_biphase_max <= d.snd_to_biphase_min) then
  begin
		result:= NegInfinity;
    exit;
  end;
	result:= (20.0 * log10((d.snd_to_biphase_max - d.snd_to_biphase_min) / 255.0));
end;

(* OK *)
Procedure parse_ltc(d : TLTCDecoder; bit: byte; offset: integer;  posinfo: ltc_off_t);
var
	bit_num, bit_set, byte_num : integer;
  k : integer;
  bi , bo : byte;
  bc : integer;
  temp_Queue: TLTCFrameExt;
  btc : integer;
  byte_num_max : integer;
begin


  if (d.bit_cnt = 0) then
  begin
    fillchar(addr(d.ltc_frame)^, sizeof(TLTCFrameRAW), #0);
    if (d.frame_start_prev < 0) then
    begin
      d.frame_start_off := trunc(posinfo - d.snd_to_biphase_period);
      {$IfDef OnDebug}
        d.DoDebug(format('parse_ltc -> d.frame_start_off = %d',[d.frame_start_off, posinfo, d.snd_to_biphase_period]));
      {$EndIf}
    end
    else
      d.frame_start_off := d.frame_start_prev;
  end;
  d.frame_start_prev := ltc_off_t(offset + posinfo);

  if (d.bit_cnt >= LTC_FRAME_BIT_COUNT) then
  begin
    byte_num_max := LTC_FRAME_BIT_COUNT shr 3;
    (* shift bits backwards *)
    for k:=0 to byte_num_max-1 do
    begin
      bi := PByte(addr(d.ltc_frame))[k];
      bo := 0;
      if (bi and $80)>0 then
        bo := bo or $40
      else
        bo := bo or $0;
      if (bi and $40)>0 then
        bo := bo or $20
      else
        bo := bo or $0;
      if (bi and $20)>0 then
        bo := bo or $10
      else
        bo := bo or $0;
      if (bi and $10)>0 then
        bo := bo or $08
      else
        bo := bo or $0;
      if (bi and $08)>0 then
        bo := bo or $04
      else
        bo := bo or $0;
      if (bi and $04)>0 then
        bo := bo or $02
      else
        bo := bo or $0;
      if (bi and $02)>0 then
        bo := bo or $01
      else
        bo := bo or $0;
      if (k+1 < byte_num_max) then
      begin
        if (PByte(@d.ltc_frame)[k+1] and $01)>0 then
          bo := bo or $80
        else
          bo := bo or $0;
      end;
				PByte(@d.ltc_frame)[k] := bo;
      {$IfDef OnDebug}
        d.DoDebug(format('parse_ltc -> d.ltc_frame[%d]= %d',[k, bo]));
      {$EndIf}
    end;

    d.frame_start_off := d.frame_start_off+ceil(d.snd_to_biphase_period);
    {$IfDef OnDebug}
      d.DoDebug(format('parse_ltc -> d.frame_start_off = %d (frame_start_off - %f)',[d.frame_start_off, d.snd_to_biphase_period]));
    {$EndIf}
    d.bit_cnt:=d.bit_cnt-1;
	end;

  (*  corrected:2013-01-12,
      + "and $FF" - range check error
  *)
  d.decoder_sync_word :=  word(d.decoder_sync_word shl $0001) ;
  if bit>0  then
  begin
    d.decoder_sync_word := d.decoder_sync_word or $0001;
    if ((d.bit_cnt < LTC_FRAME_BIT_COUNT) and (d.bit_cnt>=0))  then
    begin
      // Isolating the lowest three bits: the location of this bit in the current byte
      bit_num := (d.bit_cnt and $7);
      // Using the bit number to define which of the eight bits to set
      bit_set := $01 shl bit_num;
      // Isolating the higher bits: the number of the byte/char the target bit is contained in
      byte_num := d.bit_cnt shr 3;
      PByte(@(d.ltc_frame))[byte_num] := PByte(@(d.ltc_frame))[byte_num] or bit_set;
      {$IfDef OnDebug}
        d.DoDebug(format('parse_ltc -> d.ltc_frame[%d]= %d bit_set=%d',[byte_num, PByte(@(d.ltc_frame))[byte_num], bit_set]));
      {$EndIf}
    end;
  end;
  d.bit_cnt:=d.bit_cnt+1;

  if (d.decoder_sync_word = $3ffd (*LTC Sync Word 0x3ffd*)) then
  begin
    if (d.bit_cnt = LTC_FRAME_BIT_COUNT) then
    begin
      move(d.ltc_frame, temp_Queue.ltc, sizeof(TLTCFrameRAW));

      for bc := 0 to LTC_FRAME_BIT_COUNT-1 do
      begin
        btc := (d.biphase_tic + bc ) mod LTC_FRAME_BIT_COUNT;
        temp_Queue.biphase_tics[bc] := d.biphase_tics[btc];
      end;

      temp_Queue.off_start := d.frame_start_off;
      temp_Queue.off_end  := posinfo + offset - 1;
      temp_Queue.reverse := 0;
      temp_Queue.volume := calc_volume_db(d);
      temp_Queue.sample_min := d.snd_to_biphase_min;
      temp_Queue.sample_max := d.snd_to_biphase_max;
      {$IfDef OnDebug}
        d.DoDebug(format('parse_ltc -> temp_Queue.off_start = %d (%d 16 * %f)',[temp_Queue.off_start, d.frame_start_off, d.snd_to_biphase_period]));
        d.DoDebug(format('parse_ltc -> temp_Queue.off_end = %d (%d  + %d -1 - 16 * %f)',[temp_Queue.off_end, posinfo, offset, d.snd_to_biphase_period]));
        d.DoDebug(format('parse_ltc -> temp_Queue.reverse = %d (%d 8 * %f)',[temp_Queue.reverse, (LTC_FRAME_BIT_COUNT shr 3), d.snd_to_biphase_period]));
      {$EndIf}

      d.queue.Enqueue(temp_Queue);
      if d.queue.Count>d.queue_len then d.queue.Dequeue;
      {d.queue[d.queue_write_off]:=temp_Queue;
      d.queue_write_off := d.queue_write_off + 1;
      if (d.queue_write_off = d.queue_len) then d.queue_write_off := 0; }



    end;
    d.bit_cnt := 0;
  end;

  if (d.decoder_sync_word = $BFFC (*LTC Sync Word 0xBFFC*)) then
  begin
    if (d.bit_cnt = LTC_FRAME_BIT_COUNT) then
    begin
      byte_num_max := LTC_FRAME_BIT_COUNT shr 3;

      (* swap bits *)
      for k:=0 to byte_num_max-1 do
      begin
        bi := PByte(addr(d.ltc_frame))[k];
        bo := 0;
          if (bi and $80)>0 then
            bo := bo or $01
          else
            bo := bo or $0;
          if (bi and $40)>0 then
            bo := bo or $02
          else
            bo := bo or $0;
          if (bi and $20)>0 then
            bo := bo or $04
          else
            bo := bo or $0;
          if (bi and $10)>0 then
            bo := bo or $08
          else
            bo := bo or $0;
          if (bi and $08)>0 then
            bo := bo or $10
          else
            bo := bo or $0;
          if (bi and $04)>0 then
            bo := bo or $20
          else
            bo := bo or $0;
          if (bi and $02)>0 then
            bo := bo or $40
          else
            bo := bo or $0;
          if (bi and $01)>0 then
            bo := bo or $80
          else
            bo := bo or $0;
        PByte(@d.ltc_frame)[k] := bo;
      end;

      (* swap bytes *)
      byte_num_max := byte_num_max - 2; // skip sync-word
      for k:=0 to (byte_num_max-1) div 2 do
      begin
        bi := PByte(addr(d.ltc_frame))[k];
        PByte(addr(d.ltc_frame))[k] := PByte(addr(d.ltc_frame))[byte_num_max-1-k];
        PByte(addr(d.ltc_frame))[byte_num_max-1-k] := bi;
      end;

      move(d.ltc_frame, temp_Queue.ltc, sizeof(TLTCFrameRAW));

      for bc := 0 to LTC_FRAME_BIT_COUNT-1 do
      begin
        btc := (d.biphase_tic + bc ) mod LTC_FRAME_BIT_COUNT;
        temp_Queue.biphase_tics[bc] := d.biphase_tics[btc];
      end;

      temp_Queue.off_start := trunc(d.frame_start_off - 16 * d.snd_to_biphase_period);
      temp_Queue.off_end := trunc(posinfo + offset - 1 - 16 * d.snd_to_biphase_period);
      temp_Queue.reverse := trunc((LTC_FRAME_BIT_COUNT shr 3) * 8 * d.snd_to_biphase_period);
      temp_Queue.volume := calc_volume_db(d);
      temp_Queue.sample_min := d.snd_to_biphase_min;
      temp_Queue.sample_max := d.snd_to_biphase_max;
      {$IfDef OnDebug}
        d.DoDebug(format('parse_ltc -> temp_Queue.off_start = %d (%d 16 * %f)',[temp_Queue.off_start, d.frame_start_off, d.snd_to_biphase_period]));
        d.DoDebug(format('parse_ltc -> temp_Queue.off_end = %d (%d  + %d -1 - 16 * %f)',[temp_Queue.off_end, posinfo, offset, d.snd_to_biphase_period]));
        d.DoDebug(format('parse_ltc -> temp_Queue.reverse = %d (%d 8 * %f)',[temp_Queue.reverse, (LTC_FRAME_BIT_COUNT shr 3), d.snd_to_biphase_period]));
      {$EndIf}


      d.queue.Enqueue(temp_Queue);
      if d.queue.Count>d.queue_len then d.queue.Dequeue;

      {d.queue[d.queue_write_off]:=temp_Queue;
      d.queue_write_off := d.queue_write_off + 1;

      if (d.queue_write_off = d.queue_len) then  d.queue_write_off := 0; }
    end;
    d.bit_cnt := 0;
  end;
end;


(* OK *)
procedure biphase_decode2(d : TLTCDecoder; offset : integer; pos: ltc_off_t); inline;
begin

	d.biphase_tics[d.biphase_tic] := d.snd_to_biphase_period;
	d.biphase_tic := (d.biphase_tic + 1) Mod LTC_FRAME_BIT_COUNT;
  {$IfDef OnDebug}
    d.DoDebug(format('biphase_decode2 -> d->biphase_tic = %d',[d.biphase_tic]));
  {$EndIf}
	if (d.snd_to_biphase_cnt <= 2 * d.snd_to_biphase_period) then
  begin
    (*
    var
      OldRM: TRoundingMode;

    OldRM := GetRoundMode; { Save the original setting for the Round Mode }
    SetRoundMode(rmDown);
		pos := pos - (ceil(roundto(d.snd_to_biphase_period, -4)) - d.snd_to_biphase_cnt);
    SetRoundMode(OldRM); { Restore the original Rounding Mode }
    *)
		pos := pos - (round(d.snd_to_biphase_period) - d.snd_to_biphase_cnt);
    {$IfDef OnDebug}
      d.DoDebug(format('biphase_decode2 -> pos = %d (%.10f - %d)',[pos, d.snd_to_biphase_period, d.snd_to_biphase_cnt]));
    {$EndIf}
  end;

	if (d.snd_to_biphase_state = d.biphase_prev) then
  begin
		d.biphase_state := 1;
		parse_ltc(d, 0, offset, pos);
	end else
  begin
		d.biphase_state := 1 - d.biphase_state;
	 	if (d.biphase_state = 1) then
			parse_ltc(d, 1, offset, pos);
	end;
	d.biphase_prev := d.snd_to_biphase_state;
end;


(* OK *)
function decode_ltc(d: TLTCDecoder; sound: array of ltcsnd_sample_t; size: size_t; posinfo: ltc_off_t): integer;
var
  i : size_t;
  max_threshold, min_threshold : ltcsnd_sample_t;
begin
  result:=0;

  for I := 0 to size-1 do
  begin
		(* track minimum and maximum values *)
    d.snd_to_biphase_min := SAMPLE_CENTER - round(((SAMPLE_CENTER - d.snd_to_biphase_min) * 15) div 16);
    d.snd_to_biphase_max := SAMPLE_CENTER + round(((d.snd_to_biphase_max - SAMPLE_CENTER) * 15) div 16);

		if (sound[i] < d.snd_to_biphase_min) then
			d.snd_to_biphase_min := sound[i];
		if (sound[i] > d.snd_to_biphase_max) then
			d.snd_to_biphase_max := sound[i];

		(* set the thresholds for hi/lo state tracking *)
    min_threshold := SAMPLE_CENTER - round(((SAMPLE_CENTER - d.snd_to_biphase_min) * 8) div 16);
    max_threshold := SAMPLE_CENTER + round(((d.snd_to_biphase_max - SAMPLE_CENTER) * 8) div 16);

    {$IfDef OnDebug}
      d.DoDebug(format('decode_ltc -> d.snd_to_biphase_min = %d ',[d.snd_to_biphase_min]));
      d.DoDebug(format('decode_ltc -> d.snd_to_biphase_max = %d ',[d.snd_to_biphase_max]));
      d.DoDebug(format('decode_ltc -> min_threshold = %d ',[min_threshold]));
      d.DoDebug(format('decode_ltc -> max_threshold = %d ',[max_threshold]));
    {$EndIf}

    (* Check for a biphase state change *)
		if (((d.snd_to_biphase_state>0) and (sound[i] > max_threshold)) // Check for a biphase state change
			or ((d.snd_to_biphase_state<=0) and (sound[i] < min_threshold))) then
    begin
			// There is a state change at sound[i]

			// If the sample count has risen above the biphase length limit
			if (d.snd_to_biphase_cnt > d.snd_to_biphase_lmt) then
      begin
				(* single state change within a biphase priod. decode to a 0 *)
			 	biphase_decode2(d, i, posinfo);
			 	biphase_decode2(d, i, posinfo);
      end else
      begin
				(* "short" state change covering half a period
				 * together with the next or previous state change decode to a 1
				 *)
				d.snd_to_biphase_cnt := d.snd_to_biphase_cnt * 2;
			 	biphase_decode2(d, i, posinfo);
      end;

      if (d.snd_to_biphase_cnt > (d.snd_to_biphase_period * 4)) then
      begin
				(* "long" silence in between
				 * -> reset parser, don't use it for phase-tracking
				 *)
				d.bit_cnt := 0;
			end else
      begin
				(* track speed variations
				 * As this is only executed at a state change,
				 * d->snd_to_biphase_cnt is an accurate representation of the current period length.
				 *)
				d.snd_to_biphase_period := (d.snd_to_biphase_period * 3.0 + d.snd_to_biphase_cnt) / 4.0;

				(* This limit specifies when a state-change is
				 * considered biphase-clock or 2*biphase-clock.
				 * The relation with period has been determined
				 * empirically through trial-and-error *)
				d.snd_to_biphase_lmt := trunc((d.snd_to_biphase_period * 3) / 4);
			end;

			d.snd_to_biphase_cnt := 0;
			d.snd_to_biphase_state := not d.snd_to_biphase_state;
    end;
		d.snd_to_biphase_cnt:=d.snd_to_biphase_cnt+1;
  end;
end;




(* TLTCDecoder *)
constructor TLTCDecoder.create();
begin
  FQueue_Len := 32;
  FQueue:= TQueue<TLTCFrameExt>.create;
	FBiphase_state := 1;

  SetAPV(44100 div 25);

	FSnd_to_biphase_min := SAMPLE_CENTER;
	FSnd_to_biphase_max := SAMPLE_CENTER;
	FFrame_start_prev := -1;
	FBiphase_tic := 0;
end;

destructor TLTCDecoder.Destroy;
begin
  FQueue.Clear;
  FreeAndNil(FQueue);
  inherited Destroy;
end;

procedure TLTCDecoder.Write(buf: array of ltcsnd_sample_t; size : size_t;  posinfo : ltc_off_t);
begin
  decode_ltc(Self, buf, size, posinfo);
end;

function TLTCDecoder.Read(var frame: TLTCFrameExt ): boolean;
begin
  result:=false;
  if FQueue.Count>0 then
  begin
    frame:=FQueue.Dequeue;
    result:=true;
  end;
end;

function  TLTCDecoder.GetBiphaseTics(index: integer): Single;
begin
  result:=FBiphase_tics[index];
end;

procedure TLTCDecoder.SetBiphaseTics(index: integer; const Value: Single);
begin
  FBiphase_tics[index]:=Value;
end;

procedure TLTCDecoder.SetAPV(value: integer);
begin
	FSnd_to_biphase_period := value / 80;
	FSnd_to_biphase_lmt := trunc((FSnd_to_biphase_period * 3) / 4);
end;


end.
