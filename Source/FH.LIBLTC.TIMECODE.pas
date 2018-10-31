(*
   libltc - en+decode linear timecode

   Copyright (C) 2006-2012 Robin Gareus <robin@gareus.org>

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
   
   Conversion to Pascal Copyright 2014 (c) Oleksandr Nazaruk <support@freehand.com.ua>
   based on libltc-1.1.3
*)


unit FH.LIBLTC.TIMECODE;

interface

uses
   System.SysUtils, FH.LIBLTC.LTC;

(**
 * SMPTE Timezones
 *)
Type
  SMPTETimeZonesStruct =  packed record
	  code    : byte; //actually 6 bit!
	  timezone  : array[0..5] of char;
  end;

(**
 * SMPTE Timezone codes as per http://www.barney-wol.net/time/timecode.html
 *)
const
smpte_time_zones : array[0..57] of SMPTETimeZonesStruct =
(
    (*  code;  timezone: timezone (UTC+)     Standard time                    Daylight saving   *)
    ( code:  $00;  timezone: '+0000'             (* Greenwich *)                 (* - *)             ),
    ( code:  $00;  timezone: '-0000'             (* Greenwich *)                 (* - *)             ),
    ( code:  $01;  timezone: '-0100'             (* Azores *)                    (* - *)             ),
    ( code:  $02;  timezone: '-0200'             (* Mid-Atlantic *)              (* - *)             ),
    ( code:  $03;  timezone: '-0300'             (* Buenos Aires *)              (* Halifax *)       ),
    ( code:  $04;  timezone: '-0400'             (* Halifax *)                   (* New York *)      ),
    ( code:  $05;  timezone: '-0500'             (* New York *)                  (* Chicago *)       ),
    ( code:  $06;  timezone: '-0600'             (* Chicago Denver *)            (* - *)             ),
    ( code:  $07;  timezone: '-0700'             (* Denver *)                    (* Los Angeles *)   ),
    ( code:  $08;  timezone: '-0800'             (* Los Angeles *)               (* - *)             ),
    ( code:  $09;  timezone: '-0900'             (* Alaska *)                    (* - *)             ),
    ( code:  $10;  timezone: '-1000'             (* Hawaii *)                    (* - *)             ),
    ( code:  $11;  timezone: '-1100'             (* Midway Island *)             (* - *)             ),
    ( code:  $12;  timezone: '-1200'             (* Kwaialein *)                 (* - *)             ),
    ( code:  $13;  timezone: '+1300'             (* - *)                         (* New Zealand *)   ),
    ( code:  $14;  timezone: '+1200'             (* New Zealand *)               (* - *)             ),
    ( code:  $15;  timezone: '+1100'             (* Solomon Islands *)           (* - *)             ),
    ( code:  $16;  timezone: '+1000'             (* Guam *)                      (* - *)             ),
    ( code:  $17;  timezone: '+0900'             (* Tokyo *)                     (* - *)             ),
    ( code:  $18;  timezone: '+0800'             (* Beijing *)                   (* - *)             ),
    ( code:  $19;  timezone: '+0700'             (* Bangkok *)                   (* - *)             ),
    ( code:  $20;  timezone: '+0600'             (* Dhaka *)                     (* - *)             ),
    ( code:  $21;  timezone: '+0500'             (* Islamabad *)                 (* - *)             ),
    ( code:  $22;  timezone: '+0400'             (* Abu Dhabi *)                 (* - *)             ),
    ( code:  $23;  timezone: '+0300'             (* Moscow *)                    (* - *)             ),
    ( code:  $24;  timezone: '+0200'             (* Eastern Europe *)            (* - *)             ),
    ( code:  $25;  timezone: '+0100'             (* Central Europe *)            (* - *)             ),
(*  ( code:  $26;  timezone: 'Undefined'         Reserved; do not use                                ),*)
(*  ( code:  $27;  timezone: 'Undefined'         Reserved; do not use                                ),*)
    ( code:  $28;  timezone: 'TP-03'             (* Time precision class 3 *)    (* - *)             ),
    ( code:  $29;  timezone: 'TP-02'             (* Time precision class 2 *)    (* - *)             ),
    ( code:  $30;  timezone: 'TP-01'             (* Time precision class 1 *)    (* - *)             ),
    ( code:  $31;  timezone: 'TP-00'             (* Time precision class 0 *)    (* - *)             ),
    ( code:  $0A;  timezone: '-0030'             (* - *)                         (* - *)             ),
    ( code:  $0B;  timezone: '-0130'             (* - *)                         (* - *)             ),
    ( code:  $0C;  timezone: '-0230'             (* - *)                         (* Newfoundland *)  ),
    ( code:  $0D;  timezone: '-0330'             (* Newfoundland *)              (* - *)             ),
    ( code:  $0E;  timezone: '-0430'             (* - *)                         (* - *)             ),
    ( code:  $0F;  timezone: '-0530'             (* - *)                         (* - *)             ),
    ( code:  $1A;  timezone: '-0630'             (* - *)                         (* - *)             ),
    ( code:  $1B;  timezone: '-0730'             (* - *)                         (* - *)             ),
    ( code:  $1C;  timezone: '-0830'             (* - *)                         (* - *)             ),
    ( code:  $1D;  timezone: '-0930'             (* Marquesa Islands *)          (* - *)             ),
    ( code:  $1E;  timezone: '-1030'             (* - *)                         (* - *)             ),
    ( code:  $1F;  timezone: '-1130'             (* - *)                         (* - *)             ),
    ( code:  $2A;  timezone: '+1130'             (* Norfolk Island *)            (* - *)             ),
    ( code:  $2B;  timezone: '+1030'             (* Lord Howe Is. *)             (* - *)             ),
    ( code:  $2C;  timezone: '+0930'             (* Darwin *)                    (* - *)             ),
    ( code:  $2D;  timezone: '+0830'             (* - *)                         (* - *)             ),
    ( code:  $2E;  timezone: '+0730'             (* - *)                         (* - *)             ),
    ( code:  $2F;  timezone: '+0630'             (* Rangoon *)                   (* - *)             ),
    ( code:  $3A;  timezone: '+0530'             (* Bombay *)                    (* - *)             ),
    ( code:  $3B;  timezone: '+0430'             (* Kabul *)                     (* - *)             ),
    ( code:  $3C;  timezone: '+0330'             (* Tehran *)                    (* - *)             ),
    ( code:  $3D;  timezone: '+0230'             (* - *)                         (* - *)             ),
    ( code:  $3E;  timezone: '+0130'             (* - *)                         (* - *)             ),
    ( code:  $3F;  timezone: '+0030'             (* - *)                         (* - *)             ),
    ( code:  $32;  timezone: '+1245'             (* Chatham Island *)            (* - *)             ),
(*  ( code:  $33;  timezone: 'Undefined'         Reserved; do not use                                ),*)
(*  ( code:  $34;  timezone: 'Undefined'         Reserved; do not use                                ),*)
(*  ( code:  $35;  timezone: 'Undefined'         Reserved; do not use                                ),*)
(*  ( code:  $36;  timezone: 'Undefined'         Reserved; do not use                                ),*)
(*  ( code:  $37;  timezone: 'Undefined'         Reserved; do not use                                ),*)
    ( code:  $38;  timezone: '+XXXX'             (* User defined time offset *)  (* - *)             ),
(*  ( code:  $39;  timezone: 'Undefined'         Unknown                         Unknown             ),*)
(*  ( code:  $39;  timezone: 'Undefined'         Unknown                         Unknown             ),*)

    ( code:  $FF;  timezone: ''                  (* The End *)                                       )
);

  procedure smpte_set_timezone_string(frame: TLTCFrameRAW; var stime: TSMPTETimecode);
  procedure ltc_frame_to_time(var stime: TSMPTETimecode; frame: TLTCFrameRAW; flags: integer);

implementation

procedure smpte_set_timezone_string(frame: TLTCFrameRAW; var stime: TSMPTETimecode);
var
  i             : integer;
  code          : byte;
  timezone      : array[0..5] of char;
  user7, user8  : byte;
begin
  user7:= (frame.b7 and $10) or
  (frame.b7 and $20) or
  (frame.b7 and $40) or
  (frame.b7 and $80);

  user8:= (frame.b8 and $10) or
  (frame.b8 and $20) or
  (frame.b8 and $40) or
  (frame.b8 and $80);
	code := user7 + (user8 shl 4);

	strpcopy(timezone, '+0000');

	for i:= 0 to length(smpte_time_zones)-1 do
  begin
		if ( smpte_time_zones[i].code = code ) then
    begin
	    StrPCopy(timezone, smpte_time_zones[i].timezone);
			break;
    end;
  end;
	StrPCopy(stime.timezone, timezone);
end;

(* OK *)
procedure ltc_frame_to_time(var stime: TSMPTETimecode; frame: TLTCFrameRAW; flags: integer);
var
  frame_units   : byte;
  frame_tens    : byte;
  secs_units    : byte;
  secs_tens     : byte;
  hours_units   : byte;
  hours_tens    : byte;
  mins_units    : byte;
  mins_tens     : byte;

	user5, user6  : byte;
	user3, user4  : byte;
	user1, user2  : byte;
begin

 	if (LTC_BG_FLAGS(flags) = LTC_USE_DATE) then
  begin
		smpte_set_timezone_string(frame, stime);


    user1:= (frame.b1 and $10) or
    (frame.b1 and $20) or
    (frame.b1 and $40) or
    (frame.b1 and $80);

    user2:= (frame.b2 and $10) or
    (frame.b2 and $20) or
    (frame.b2 and $40) or
    (frame.b2 and $80);

    user3:= (frame.b3 and $10) or
    (frame.b3 and $20) or
    (frame.b3 and $40) or
    (frame.b3 and $80);

    user4:= (frame.b4 and $10) or
    (frame.b4 and $20) or
    (frame.b4 and $40) or
    (frame.b4 and $80);

    user5:= (frame.b5 and $10) or
    (frame.b5 and $20) or
    (frame.b5 and $40) or
    (frame.b5 and $80);

    user6:= (frame.b6 and $10) or
    (frame.b6 and $20) or
    (frame.b6 and $40) or
    (frame.b6 and $80);

		stime.years  := user5 + user6*10;
		stime.months := user3 + user4*10;
		stime.days   := user1 + user2*10;
	end else
  begin
		stime.years  := 0;
		stime.months := 0;
		stime.days   := 0;
		strpCopy(stime.timezone,'+0000');
	end;

    frame_units:= (frame.b1 and $01) or
    (frame.b1 and $02) or
    (frame.b1 and $04) or
    (frame.b1 and $08);

    frame_tens:= (frame.b2 and $01) or
    (frame.b2 and $02);

    secs_units:= (frame.b3 and $01) or
    (frame.b3 and $02) or
    (frame.b3 and $04) or
    (frame.b3 and $08);

    secs_tens:= (frame.b4 and $01) or
    (frame.b4 and $02) or
    (frame.b4 and $04);


    mins_units:= (frame.b5 and $01) or
    (frame.b5 and $02) or
    (frame.b5 and $04) or
    (frame.b5 and $08);

    mins_tens:= (frame.b6 and $01) or
    (frame.b6 and $02) or
    (frame.b6 and $04);

    hours_units:= (frame.b7 and $01) or
    (frame.b7 and $02) or
    (frame.b7 and $04) or
    (frame.b7 and $08);

    hours_tens:= (frame.b8 and $01) or
    (frame.b8 and $02);

    stime.hours := hours_units + hours_tens*10;
    stime.mins  := mins_units  + mins_tens*10;
    stime.secs  := secs_units  + secs_tens*10;
    stime.frame := frame_units + frame_tens*10;
end;

end.