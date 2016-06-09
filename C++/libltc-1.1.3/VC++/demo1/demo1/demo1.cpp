// demo1.cpp: определяет точку входа для консольного приложения.
//

#include "stdafx.h"
#include <stdio.h>
#include <stdlib.h>
#include <string.h>
#include <math.h>
#include "ltc.h"
#include "decoder.h"


	FILE *inputf;

/** turn a numeric literal into a hex constant
 *  (avoids problems with leading zeroes)
 *  8-bit constants max value 0x11111111, always fits in unsigned long
 */
#define HEX__(n) 0x##n##LU

/**
 * 8-bit conversion function
 */
#define B8__(x) ((x&0x0000000FLU)?1:0)	\
	+((x&0x000000F0LU)?2:0)	 \
	+((x&0x00000F00LU)?4:0)	 \
	+((x&0x0000F000LU)?8:0)	 \
	+((x&0x000F0000LU)?16:0) \
	+((x&0x00F00000LU)?32:0) \
	+((x&0x0F000000LU)?64:0) \
	+((x&0xF0000000LU)?128:0)

/** for upto 8-bit binary constants */
#define B8(d) ((unsigned char)B8__(HEX__(d)))

/** for upto 16-bit binary constants, MSB first */
#define B16(dmsb,dlsb) (((unsigned short)B8(dmsb)<<8) + B8(dlsb))

/** turn a numeric literal into a hex constant
 *(avoids problems with leading zeroes)
 * 8-bit constants max value 0x11111111, always fits in unsigned long
 */
#define HEX__(n) 0x##n##LU

/** 8-bit conversion function */
#define B8__(x) ((x&0x0000000FLU)?1:0)	\
	+((x&0x000000F0LU)?2:0)  \
	+((x&0x00000F00LU)?4:0)  \
	+((x&0x0000F000LU)?8:0)  \
	+((x&0x000F0000LU)?16:0) \
	+((x&0x00F00000LU)?32:0) \
	+((x&0x0F000000LU)?64:0) \
	+((x&0xF0000000LU)?128:0)


/** for upto 8-bit binary constants */
#define B8(d) ((unsigned char)B8__(HEX__(d)))

/** for upto 16-bit binary constants, MSB first */
#define B16(dmsb,dlsb) (((unsigned short)B8(dmsb)<<8) + B8(dlsb))

/* Example usage:
 * B8(01010101) = 85
 * B16(10101010,01010101) = 43605
 */

#define BUFFER_SIZE (1024)

int ltc_decoder_free(LTCDecoder *d) {
	if (!d) return 1;
	if (d->queue) free(d->queue);
	free(d);

	return 0;
}

void ltc_frame_to_time(SMPTETimecode *stime, LTCFrame *frame, int flags) {
	if (!stime) return;

	stime->hours = frame->hours_units + frame->hours_tens*10;
	stime->mins  = frame->mins_units  + frame->mins_tens*10;
	stime->secs  = frame->secs_units  + frame->secs_tens*10;
	stime->frame = frame->frame_units + frame->frame_tens*10;

	fprintf(inputf, "frame.hours_units=%d frame.hours_tens=%d\n", frame->hours_units , frame->hours_tens);
	fprintf(inputf, "frame->mins_units=%d frame->mins_tens=%d\n", frame->mins_units , frame->mins_tens);
	fprintf(inputf, "frame->secs_units=%d  frame->secs_tens=%d\n", frame->secs_units ,  frame->secs_tens);
	fprintf(inputf, "frame->frame_units=%d frame->frame_tens=%d\n", frame->frame_units , frame->frame_tens);

}

int ltc_decoder_read(LTCDecoder* d, LTCFrameExt* frame) {
	if (!frame) return -1;
	if (d->queue_read_off != d->queue_write_off) {
		memcpy(frame, &d->queue[d->queue_read_off], sizeof(LTCFrameExt));
		d->queue_read_off++;
		if (d->queue_read_off == d->queue_len)
			d->queue_read_off = 0;
		return 1;
	}
	return 0;
}

void ltc_decoder_write(LTCDecoder *d, ltcsnd_sample_t *buf, size_t size, ltc_off_t posinfo) {
	decode_ltc(d, buf, size, posinfo);
}

static double calc_volume_db(LTCDecoder *d) {
	if (d->snd_to_biphase_max <= d->snd_to_biphase_min)
		return INT_MIN;
	return (20.0 * log10((d->snd_to_biphase_max - d->snd_to_biphase_min) / 255.0));
}

static void parse_ltc(LTCDecoder *d, unsigned char bit, int offset, ltc_off_t posinfo) {
	int bit_num, bit_set, byte_num;

	if (d->bit_cnt == 0) {
		memset(&d->ltc_frame, 0, sizeof(LTCFrame));

		if (d->frame_start_prev < 0) {
			d->frame_start_off = posinfo - d->snd_to_biphase_period;
			//fprintf(inputf, "parse_ltc -> d.frame_start_off = %d\n", d->frame_start_off, posinfo, d->snd_to_biphase_period);
		} else {
			d->frame_start_off = d->frame_start_prev;
		}
	}
	d->frame_start_prev = offset + posinfo;

	if (d->bit_cnt >= LTC_FRAME_BIT_COUNT) {
		/* shift bits backwards */
		int k = 0;
		const int byte_num_max = LTC_FRAME_BIT_COUNT >> 3;

		for (k=0; k< byte_num_max; k++) {
			const unsigned char bi = ((unsigned char*)&d->ltc_frame)[k];
			unsigned char bo = 0;
			bo |= (bi & B8(10000000) ) ? B8(01000000) : 0;
			bo |= (bi & B8(01000000) ) ? B8(00100000) : 0;
			bo |= (bi & B8(00100000) ) ? B8(00010000) : 0;
			bo |= (bi & B8(00010000) ) ? B8(00001000) : 0;
			bo |= (bi & B8(00001000) ) ? B8(00000100) : 0;
			bo |= (bi & B8(00000100) ) ? B8(00000010) : 0;
			bo |= (bi & B8(00000010) ) ? B8(00000001) : 0;
			if (k+1 < byte_num_max) {
				bo |= ( (((unsigned char*)&d->ltc_frame)[k+1]) & B8(00000001) ) ? B8(10000000): B8(00000000);
			}
			((unsigned char*)&d->ltc_frame)[k] = bo;

            //fprintf(inputf, "parse_ltc -> d.ltc_frame[%d]= %d\n",k, bo);
		}

		d->frame_start_off += ceil(d->snd_to_biphase_period);
       //fprintf(inputf, "parse_ltc -> d.frame_start_off = %d (frame_start_off - %d)\n",d->frame_start_off, d->snd_to_biphase_period);

		d->bit_cnt--;
	}

	d->decoder_sync_word <<= 1;
	if (bit) {

		d->decoder_sync_word |= B16(00000000,00000001);

		if (d->bit_cnt < LTC_FRAME_BIT_COUNT) {
			// Isolating the lowest three bits: the location of this bit in the current byte
			bit_num = (d->bit_cnt & B8(00000111));
			// Using the bit number to define which of the eight bits to set
			bit_set = (B8(00000001) << bit_num);
			// Isolating the higher bits: the number of the byte/char the target bit is contained in
			byte_num = d->bit_cnt >> 3;

			(((unsigned char*)&d->ltc_frame)[byte_num]) |= bit_set;
			//fprintf(inputf, "parse_ltc -> d.ltc_frame[%d]= %d bit_set=%d\n", byte_num, (((unsigned char*)&d->ltc_frame)[byte_num]), bit_set);


		}

	}
	d->bit_cnt++;

	if (d->decoder_sync_word == B16(00111111,11111101) /*LTC Sync Word 0x3ffd*/) {
		if (d->bit_cnt == LTC_FRAME_BIT_COUNT) {
			int bc;
        

			memcpy( &d->queue[d->queue_write_off].ltc,
				&d->ltc_frame,
				sizeof(LTCFrame));

			for(bc = 0; bc < LTC_FRAME_BIT_COUNT; ++bc) {
				const int btc = (d->biphase_tic + bc ) % LTC_FRAME_BIT_COUNT;
				d->queue[d->queue_write_off].biphase_tics[bc] = d->biphase_tics[btc];
			}

			d->queue[d->queue_write_off].off_start = d->frame_start_off;
			d->queue[d->queue_write_off].off_end = posinfo + (ltc_off_t) offset - 1LL;
			d->queue[d->queue_write_off].reverse = 0;
			d->queue[d->queue_write_off].volume = calc_volume_db(d);
			d->queue[d->queue_write_off].sample_min = d->snd_to_biphase_min;
			d->queue[d->queue_write_off].sample_max = d->snd_to_biphase_max;

	    //fprintf(inputf, "parse_ltc -> temp_Queue.off_start = %d (%d 16 * %d)\n",d->queue[d->queue_write_off].off_start, d->frame_start_off, d->snd_to_biphase_period);
        //fprintf(inputf, "parse_ltc -> temp_Queue.off_end = %d (%d  + %d -1 - 16 * %d)\n",d->queue[d->queue_write_off].off_end, posinfo, offset, d->snd_to_biphase_period);
        //fprintf(inputf, "parse_ltc -> temp_Queue.reverse = %d (%d 8 * %d)\n",d->queue[d->queue_write_off].reverse, (LTC_FRAME_BIT_COUNT >> 3), d->snd_to_biphase_period);


			d->queue_write_off++;

			if (d->queue_write_off == d->queue_len)
				d->queue_write_off = 0;
		}
		d->bit_cnt = 0;
	}

	if (d->decoder_sync_word == B16(10111111,11111100) /* reverse sync-word*/) {
		if (d->bit_cnt == LTC_FRAME_BIT_COUNT) {
			/* reverse frame */
			int bc;
			int k = 0;
			int byte_num_max = LTC_FRAME_BIT_COUNT >> 3;

			/* swap bits */
			for (k=0; k< byte_num_max; k++) {
				const unsigned char bi = ((unsigned char*)&d->ltc_frame)[k];
				unsigned char bo = 0;
				bo |= (bi & B8(10000000) ) ? B8(00000001) : 0;
				bo |= (bi & B8(01000000) ) ? B8(00000010) : 0;
				bo |= (bi & B8(00100000) ) ? B8(00000100) : 0;
				bo |= (bi & B8(00010000) ) ? B8(00001000) : 0;
				bo |= (bi & B8(00001000) ) ? B8(00010000) : 0;
				bo |= (bi & B8(00000100) ) ? B8(00100000) : 0;
				bo |= (bi & B8(00000010) ) ? B8(01000000) : 0;
				bo |= (bi & B8(00000001) ) ? B8(10000000) : 0;
				((unsigned char*)&d->ltc_frame)[k] = bo;
			}

			/* swap bytes */
			byte_num_max-=2; // skip sync-word
			for (k=0; k< (byte_num_max)/2; k++) {
				const unsigned char bi = ((unsigned char*)&d->ltc_frame)[k];
				((unsigned char*)&d->ltc_frame)[k] = ((unsigned char*)&d->ltc_frame)[byte_num_max-1-k];
				((unsigned char*)&d->ltc_frame)[byte_num_max-1-k] = bi;
			}

			memcpy( &d->queue[d->queue_write_off].ltc,
				&d->ltc_frame,
				sizeof(LTCFrame));

			for(bc = 0; bc < LTC_FRAME_BIT_COUNT; ++bc) {
				const int btc = (d->biphase_tic + bc ) % LTC_FRAME_BIT_COUNT;
				d->queue[d->queue_write_off].biphase_tics[bc] = d->biphase_tics[btc];
			}

			d->queue[d->queue_write_off].off_start = d->frame_start_off - 16 * d->snd_to_biphase_period;
			d->queue[d->queue_write_off].off_end = posinfo + (ltc_off_t) offset - 1LL - 16 * d->snd_to_biphase_period;
			d->queue[d->queue_write_off].reverse = (LTC_FRAME_BIT_COUNT >> 3) * 8 * d->snd_to_biphase_period;
			d->queue[d->queue_write_off].volume = calc_volume_db(d);
			d->queue[d->queue_write_off].sample_min = d->snd_to_biphase_min;
			d->queue[d->queue_write_off].sample_max = d->snd_to_biphase_max;


			


			d->queue_write_off++;

			if (d->queue_write_off == d->queue_len)
				d->queue_write_off = 0;
		}
		d->bit_cnt = 0;
	}
}

static inline void biphase_decode2(LTCDecoder *d, int offset, ltc_off_t pos) {

	d->biphase_tics[d->biphase_tic] = d->snd_to_biphase_period;
	d->biphase_tic = (d->biphase_tic + 1) % LTC_FRAME_BIT_COUNT;
	//fprintf(inputf, "biphase_decode2 -> d->biphase_tic = %d\n", d->biphase_tic);


	if (d->snd_to_biphase_cnt <= 2 * d->snd_to_biphase_period) {
		double test = d->snd_to_biphase_period;
		int test1 = (d->snd_to_biphase_cnt);
		//fprintf(inputf, "biphase_decode2 -> pos = %d\n",pos);
		pos -= (d->snd_to_biphase_period - d->snd_to_biphase_cnt);
		//fprintf(inputf, "biphase_decode2 -> pos = %d\n",pos);
	}

	if (d->snd_to_biphase_state == d->biphase_prev) {
		d->biphase_state = 1;
		parse_ltc(d, 0, offset, pos);
	} else {
		d->biphase_state = 1 - d->biphase_state;
		if (d->biphase_state == 1) {
			parse_ltc(d, 1, offset, pos);
		}
	}
	d->biphase_prev = d->snd_to_biphase_state;
}

void decode_ltc(LTCDecoder *d, ltcsnd_sample_t *sound, size_t size, ltc_off_t posinfo) {
	size_t i;

	for (i = 0 ; i < size ; i++) {
		ltcsnd_sample_t max_threshold, min_threshold;

		/* track minimum and maximum values */
		d->snd_to_biphase_min = SAMPLE_CENTER - (((SAMPLE_CENTER - d->snd_to_biphase_min) * 15) / 16);
		d->snd_to_biphase_max = SAMPLE_CENTER + (((d->snd_to_biphase_max - SAMPLE_CENTER) * 15) / 16);

		if (sound[i] < d->snd_to_biphase_min)
			d->snd_to_biphase_min = sound[i];
		if (sound[i] > d->snd_to_biphase_max)
			d->snd_to_biphase_max = sound[i];

		/* set the thresholds for hi/lo state tracking */
		min_threshold = SAMPLE_CENTER - (((SAMPLE_CENTER - d->snd_to_biphase_min) * 8) / 16);
		max_threshold = SAMPLE_CENTER + (((d->snd_to_biphase_max - SAMPLE_CENTER) * 8) / 16);

		//fprintf(inputf, "decode_ltc -> d.snd_to_biphase_min = %d \n",d->snd_to_biphase_min);
		//fprintf(inputf, "decode_ltc -> d.snd_to_biphase_max = %d \n",d->snd_to_biphase_max);
		//fprintf(inputf, "decode_ltc -> min_threshold = %d \n",min_threshold);
		//fprintf(inputf, "decode_ltc -> max_threshold = %d \n",max_threshold);



		if ( /* Check for a biphase state change */
			   (  d->snd_to_biphase_state && (sound[i] > max_threshold) )
			|| ( !d->snd_to_biphase_state && (sound[i] < min_threshold) )
		   ) {

			/* If the sample count has risen above the biphase length limit */
			if (d->snd_to_biphase_cnt > d->snd_to_biphase_lmt) {
				/* single state change within a biphase priod. decode to a 0 */
		//fprintf(inputf, "posinfo1 = %d \n",posinfo);
				biphase_decode2(d, i, posinfo);
		//fprintf(inputf, "posinfo2 = %d \n",posinfo);
				biphase_decode2(d, i, posinfo);

			} else {
				/* "short" state change covering half a period
				 * together with the next or previous state change decode to a 1
				 */
				d->snd_to_biphase_cnt *= 2;
		//fprintf(inputf, "posinfo3 = %d \n",posinfo);
				biphase_decode2(d, i, posinfo);

			}

			if (d->snd_to_biphase_cnt > (d->snd_to_biphase_period * 4)) {
				/* "long" silence in between
				 * -> reset parser, don't use it for phase-tracking
				 */
				d->bit_cnt = 0;
			} else  {
				/* track speed variations
				 * As this is only executed at a state change,
				 * d->snd_to_biphase_cnt is an accurate representation of the current period length.
				 */
				d->snd_to_biphase_period = (d->snd_to_biphase_period * 3.0 + d->snd_to_biphase_cnt) / 4.0;

				/* This limit specifies when a state-change is
				 * considered biphase-clock or 2*biphase-clock.
				 * The relation with period has been determined
				 * empirically through trial-and-error */
				d->snd_to_biphase_lmt = (d->snd_to_biphase_period * 3) / 4;
			}

			d->snd_to_biphase_cnt = 0;
			d->snd_to_biphase_state = !d->snd_to_biphase_state;
		}
		d->snd_to_biphase_cnt++;
	}
}

LTCDecoder* ltc_decoder_create(int apv, int queue_len) {
	LTCDecoder* d = (LTCDecoder*) calloc(1, sizeof(LTCDecoder));
	if (!d) return NULL;

	d->queue_len = queue_len;
	d->queue = (LTCFrameExt*) calloc(d->queue_len, sizeof(LTCFrameExt));
	if (!d->queue) {
		free(d);
		return NULL;
	}
	d->biphase_state = 1;
	d->snd_to_biphase_period = apv / 80;
	d->snd_to_biphase_lmt = (d->snd_to_biphase_period * 3) / 4;

	d->snd_to_biphase_min = SAMPLE_CENTER;
	d->snd_to_biphase_max = SAMPLE_CENTER;
	d->frame_start_prev = -1;
	d->biphase_tic = 0;

	return d;
}


int _tmain(int argc, _TCHAR* argv[])
{
int apv = 1920;
	ltcsnd_sample_t sound[BUFFER_SIZE];
	size_t n;
	long int total;
	FILE* f;
	char* filename = "C:\\Users\\mail_000\\Desktop\\libltc-1.1.3\\VC++\\demo1\\Debug\\test.raw";


	inputf = fopen("C:\\Users\\mail_000\\Desktop\\libltc-1.1.3\\VC++\\demo1\\Debug\\result_.txt","w");	
	
	LTCDecoder *decoder;

	LTCFrameExt frame;

	// fIXME
	if (argc > 1) {
		filename = argv[1];
		if (argc > 2) {
			sscanf(argv[2], "%i", &apv);
		}
	}

	f = fopen(filename, "r");

	if (!f) {
		fprintf(stderr, "error opening '%s'\n", filename);
		return -1;
	}
	fprintf(stderr, "* reading from: %s\n", filename);

	total = sizeof(LTCFrame);

	total = 0;

	decoder = ltc_decoder_create(apv, 32);

	do {
		n = fread(sound, sizeof(ltcsnd_sample_t), BUFFER_SIZE, f);

		 fprintf(inputf, "total = %d - n= %d\n",total , n);
		ltc_decoder_write(decoder, sound, n, total);

		while (ltc_decoder_read(decoder, &frame)) {
			SMPTETimecode stime;

			ltc_frame_to_time(&stime, &frame.ltc, 1);

			printf("%04d-%02d-%02d %s ",
				((stime.years < 67) ? 2000+stime.years : 1900+stime.years),
				stime.months,
				stime.days,
				stime.timezone
				);

			printf("%02d:%02d:%02d%c%02d | %8lld %8lld%s\n",
					stime.hours,
					stime.mins,
					stime.secs,
					(frame.ltc.dfbit) ? '.' : ':',
					stime.frame,
					frame.off_start,
					frame.off_end,
					frame.reverse ? "  R" : ""
					);
		}

		total += n;

	} while (n);

	fclose(f);
	ltc_decoder_free(decoder);

	return 0;
}

