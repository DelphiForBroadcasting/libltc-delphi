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


unit FH.LIBLTC.LTC;

interface

(**
 * default audio sample type: 8bit unsigned (mono)
 *)
type  size_t = NativeUInt;  ltcsnd_sample_t  = byte;  pltcsnd_sample_t = ^ltcsnd_sample_t;
(* * sample-count offset - 64bit wide
 *)
type
  ltc_off_t = int64;

Const  LTC_FRAME_BIT_COUNT =	80;  SAMPLE_CENTER = 128;


(* Little Endian version -- and doxygen doc *)
type
  TLTCFrame  = packed record
	   frame_units : byte; ///< SMPTE framenumber BCD unit 0..9
	   user1 : byte;

	   frame_tens : byte; ///< SMPTE framenumber BCD tens 0..3
	   dfbit : byte; ///< indicated drop-frame timecode
	   col_frame : byte;///< colour-frame: timecode intentionally synchronized to a colour TV field sequence
	   user2 : byte;

	   secs_units : byte; ///< SMPTE seconds BCD unit 0..9
	   user3 : byte;

	   secs_tens : byte; ///< SMPTE seconds BCD tens 0..6
	   biphase_mark_phase_correction : byte; ///< see note on Bit 27 in description and \ref ltc_frame_set_parity .
	   user4 : byte;

	   mins_units : byte; ///< SMPTE minutes BCD unit 0..9
	   user5 : byte;

	   mins_tens : byte; ///< SMPTE minutes BCD tens 0..6
	   binary_group_flag_bit0 : byte; ///< indicate user-data char encoding, see table above - bit 43
	   user6 : byte;

	   hours_units : byte; ///< SMPTE hours BCD unit 0..9
	   user7 : byte;

	   hours_tens : byte; ///< SMPTE hours BCD tens 0..2
	   binary_group_flag_bit1 : byte; ///< indicate timecode is local time wall-clock, see table above - bit 58
	   binary_group_flag_bit2 : byte; ///< indicate user-data char encoding (or parity with 25fps), see table above - bit 59
	   user8 : byte;

	   sync_word: word;
  end;


type
  TLTCFrameRAW  = packed record
    b1 : byte;
    b2 : byte;
    b3 : byte;
    b4 : byte;
    b5 : byte;
    b6 : byte;
    b7 : byte;
    b8 : byte;
    sync_word: word;
  end;
  PLTCFrameRAW = ^TLTCFrameRAW;


(* the standard defines the assignment of the binary-group-flag bits
 * basically only 25fps is different, but other standards defined in
 * the SMPTE spec have been included for completeness.
 *)
Type
  LTC_TV_STANDARD = (
    LTC_TV_525_60, ///< 30fps
    LTC_TV_625_50, ///< 25fps
    LTC_TV_1125_60,///< 30fps
    LTC_TV_FILM_24 ///< 24fps
  );

(** encoder and LTCframe <> timecode operation flags *)
type
  LTC_BG_FLAGS = (
    LTC_USE_DATE  = 1, ///< LTCFrame <> SMPTETimecode converter and LTCFrame increment/decrement use date, also set BGF2 to '1' when encoder is initialized or re-initialized (unless LTC_BGF_DONT_TOUCH is given)
    LTC_TC_CLOCK  = 2,///< the Timecode is wall-clock aka freerun. This also sets BGF1 (unless LTC_BGF_DONT_TOUCH is given)
    LTC_BGF_DONT_TOUCH = 4, ///< encoder init or re-init does not touch the BGF bits (initial values after initialization is zero)
    LTC_NO_PARITY = 8 ///< parity bit is left untouched when setting or in/decrementing the encoder frame-number
  );

(**
 * Extended LTC frame - includes audio-sample position offsets, volume, etc
 *
 * Note: For TV systems, the sample in the LTC audio data stream where the LTC Frame starts is not neccesarily at the same time
 * as the video-frame which is described by the LTC Frame.
 *
 * \ref off_start denotes the time of the first transition of bit 0 in the LTC frame.
 *
 * For 525/60 Television systems, the first transition shall occur at the beginning of line 5 of the frame with which it is
 * associated. The tolerance is ± 1.5 lines.
 *
 * For 625/50 systems, the first transition shall occur at the beginning of line 2  ± 1.5 lines of the frame with which it is associated.
 *
 * Only for 1125/60 systems, the first transition occurs exactly at the vertical sync timing reference of the frame. ± 1 line.
 *
 *)
type
  TLTCFrameExt = packed record
    ltc           : TLTCFrameRAW; ///< the actual LTC frame. see \ref LTCFrame
    off_start     : ltc_off_t; ///< \anchor off_start the approximate sample in the stream corresponding to the start of the LTC frame.
    off_end       : ltc_off_t; ///< \anchor off_end the sample in the stream corresponding to the end of the LTC frame.
    reverse       : integer; ///< if non-zero, a reverse played LTC frame was detected. Since the frame was reversed, it started at off_end and finishes as off_start (off_end > off_start). (Note: in reverse playback the (reversed) sync-word of the next/previous frame is detected, this offset is corrected).
    biphase_tics  : array[0..LTC_FRAME_BIT_COUNT-1] of Single; ///< detailed timing info: phase of the LTC signal; the time between each bit in the LTC-frame in audio-frames. Summing all 80 values in the array will yield audio-frames/LTC-frame = (\ref off_end - \ref off_start + 1).
    sample_min    : ltcsnd_sample_t; ///< the minimum input sample signal for this frame (0..255)
    sample_max    : ltcsnd_sample_t; ///< the maximum input sample signal for this frame (0..255)
    volume        : Single; ///< the volume of the input signal in dbFS
  end;
  PLTCFrameExt = ^TLTCFrameExt;


type
  TSMPTETimecode = packed record
  // these are only set when compiled with ENABLE_DATE
    timezone  : array[0..5] of char;
    years     : byte;
    months    : byte;
    days      : byte;
  //
    hours     : byte;
    mins      : byte;
    secs      : byte;
    frame     : byte;
  end;
  PSMPTETimecode = ^TSMPTETimecode;


implementation


end.