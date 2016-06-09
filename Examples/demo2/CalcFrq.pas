unit CalcFrq;

interface

function CalcFreqFactor(Freq: Double; SampleRate: Double; PData: PSmallInt; SampleCount: Longint): Double;
function CalcFreqGoertzel(Freq: Double; SampleRate: Double; PData: PSmallInt; SampleCount: Longint): Double;
function AddDtmfDigit(C: Char): Char;

implementation

uses math;

type
    PDtmfDecoder	= ^TDtmfDecoder;
	  TDtmfDecoder = record
		cPrev:					Char;
    nCount:					Integer;
  	nGarbageCount:	Integer;
    end;

var dtmf_decoder      :	TDtmfDecoder;    

function CalcFreqFactor(Freq: Double; SampleRate: Double; PData: PSmallInt; SampleCount: Longint): Double;
var  i          :	Longint;
     fSin, fCos :	Extended;
     fSinSum    : Double;
     fCosSum    : Double;
     d          : Double;
begin
     fSinSum  := 0.0;
     fCosSum  := 0.0;
     d        := 2 * Pi * Freq / SampleRate;
     for i := 0 to SampleCount - 1 do
     begin
          SinCos(d * i, fSin, fCos);
          fSinSum := fSinSum + pData^ * fSin;
          fCosSum := fCosSum + pData^ * fCos;
          Inc(pData);
     end;
     Result:= ( abs(fSinSum) + abs(fCosSum) ) / SampleCount;
end;

function CalcFreqGoertzel(Freq: Double; SampleRate: Double; PData: PSmallInt; SampleCount: Longint): Double;
var  i     : Longint;
     Sk0   : Double;
     Sk1   : Double;
     Sk2   : Double;
     d     : Double;
begin
     Sk0:= 0.0;
     Sk1:= 0.0;
     for i := 0 to SampleCount - 1 do
     begin
          Sk2:= Sk1;
          Sk1:= Sk0;
          Sk0:= 2 * cos( 2 * Pi * Freq / SampleRate ) * Sk1 - Sk2 + pData^;
          Inc(pData);
     end;
     d:= exp( -2 * Pi * Freq / SampleRate );
     Result:= abs( Sk0 - d * Sk1 ) / SampleCount;
end;

function AddDtmfDigit(C: Char): Char;
begin
     Result	:= ' ';
     if (dtmf_decoder.cPrev = C) and (Pos(C, '0123456789ABCD*#') > 0)then
     begin
  	           Inc(dtmf_decoder.nCount);
		           if dtmf_decoder.nCount = 1 then
               Result	:= dtmf_decoder.cPrev;
     end	else
     begin
          Inc(dtmf_decoder.nGarbageCount);
          if dtmf_decoder.nGarbageCount = 1 then
          begin
               dtmf_decoder.nCount					:= 0;
               dtmf_decoder.nGarbageCount	:= 0;
               dtmf_decoder.cPrev					:= C;
          end;
     end;
end;

end.
 