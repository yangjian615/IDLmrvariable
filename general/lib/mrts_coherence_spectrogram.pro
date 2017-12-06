; docformat = 'rst'
;
; NAME:
;       MrTS_Coherence_Spectrogram
;
;*****************************************************************************************
;   Copyright (c) 2017, Matthew Argall                                                   ;
;   All rights reserved.                                                                 ;
;                                                                                        ;
;   Redistribution and use in source and binary forms, with or without modification,     ;
;   are permitted provided that the following conditions are met:                        ;
;                                                                                        ;
;       * Redistributions of source code must retain the above copyright notice,         ;
;         this list of conditions and the following disclaimer.                          ;
;       * Redistributions in binary form must reproduce the above copyright notice,      ;
;         this list of conditions and the following disclaimer in the documentation      ;
;         and/or other materials provided with the distribution.                         ;
;       * Neither the name of the <ORGANIZATION> nor the names of its contributors may   ;
;         be used to endorse or promote products derived from this software without      ;
;         specific prior written permission.                                             ;
;                                                                                        ;
;   THIS SOFTWARE IS PROVIDED BY THE COPYRIGHT HOLDERS AND CONTRIBUTORS "AS IS" AND ANY  ;
;   EXPRESS OR IMPLIED WARRANTIES, INCLUDING, BUT NOT LIMITED TO, THE IMPLIED WARRANTIES ;
;   OF MERCHANTABILITY AND FITNESS FOR A PARTICULAR PURPOSE ARE DISCLAIMED. IN NO EVENT  ;
;   SHALL THE COPYRIGHT HOLDER OR CONTRIBUTORS BE LIABLE FOR ANY DIRECT, INDIRECT,       ;
;   INCIDENTAL, SPECIAL, EXEMPLARY, OR CONSEQUENTIAL DAMAGES (INCLUDING, BUT NOT LIMITED ;
;   TO, PROCUREMENT OF SUBSTITUTE GOODS OR SERVICES; LOSS OF USE, DATA, OR PROFITS; OR   ;
;   BUSINESS INTERRUPTION) HOWEVER CAUSED AND ON ANY THEORY OF LIABILITY, WHETHER IN     ;
;   CONTRACT, STRICT LIABILITY, OR TORT (INCLUDING NEGLIGENCE OR OTHERWISE) ARISING IN   ;
;   ANY WAY OUT OF THE USE OF THIS SOFTWARE, EVEN IF ADVISED OF THE POSSIBILITY OF SUCH  ;
;   DAMAGE.                                                                              ;
;*****************************************************************************************
;
; PURPOSE:
;+
;   Create a spectrogram of the cross-spectral density. Note that only the trace
;   of the CSD is used.
;
; :Params:
;       VAR:            in, optional, type=integer/string/objref
;                       The name, number or object reference of a MrTimeSeries object
;                           for which the cross-spectral density is to be computed.
;       TFFT:           in, optional, type=int, default=TFFT/6
;                       The number of points in each spectral estimate.
;       TSHIFT:         in, optional, type=integer, default=TFFT/2
;                       Number of points to shift between each spectral estimate.
;       NFFT:           in, optional, type=int, default=nPts
;                       Number of points in each FFT. If NFFT is shorter than the
;                           `TFFT`, then the PSD of each NFFT segment that fits within
;                           `TFFT` will be averaged together.
;       NSHIFT:         in, optional, type=integer, default=NFFT/2
;                       Number of points to shift between consecutive FFTs.
;
; :Keywords:
;       CACHE:          in, optional, type=boolean, default=0
;                       If set, `RESULT` will be added to the variable cache.
;       COINCIDENCE:    out, optional, type=objref
;                       A named variable to receive the real part of the cross-spectral
;                           density, returned as a MrTimeSeries object.
;       NAME:           in, optional, type=string, default='CrossSpectram('+<name>+','+<name>+')'
;                       Name to be given to `RESULT`.
;       PHASE:          out, optional, type=objref
;                       A named variable to receive the phase of the cross-spectrum,
;                           returned as a MrTimeSeries object.
;       QUADRATURE:     out, optional, type=objref
;                       A named variable to receive the imaginary part of the cross-spectral
;                           density, returned as a MrTimeSeries object.
;       RANGE:          in, optional, type=intarr(2)/strarr(2), default="[0,nPts-1]"
;                       A time or index range outlining a subset of the data for which
;                           the CSD is computed.
;       WINDOW:         in, optional, type=string, default='Rectangular'
;                       The name of a tapering window to be applied to the data. See
;                           the FFT_Window method for valid names.
;       _REF_EXTRA:     in, optional, type=any
;                       Any keyword accepted by the ::FFT2 method is also accepted here.
;
; :Returns:
;       RESULT:         out, required, type=MrTimeSeries object
;                       A spectrogram of the cross-spectral density.
;
; :Author:
;   Matthew Argall::
;       University of New Hampshire
;       Morse Hall, Room 348
;       8 College Rd.
;       Durham, NH, 03824
;       matthew.argall@unh.edu
;
; :History:
;   Modification History::
;       2017/08/04  -   Written by Matthew Argall
;-
FUNCTION MrTS_Coherence_Spectrogram, var1, var2, tfft, tshift, nfft, nshift, $
CACHE=cache, $
NAME=name, $
RANGE=range
	Compile_Opt idl2
	On_Error, 2
	
	;Get the variable
	oV1 = MrVar_Get(var1)
	oV2 = MrVar_Get(var2)
	
	;Defaults
	N = oV1 -> GetNPts()
	tf_phase = Arg_Present(oPhase)
	IF N_Elements(tfft)   EQ 0 THEN tfft   = N / 6
	IF N_Elements(nfft)   EQ 0 THEN nfft   = tfft
	IF N_Elements(tshift) EQ 0 THEN tshift = tfft / 2
	IF N_Elements(nshift) EQ 0 THEN nshift = nfft / 2
	IF N_Elements(name)   EQ 0 THEN name   = 'CoherenceSpectrum(' + oV1.name + ',' + oV2.name + ')'
	IF N_Elements(range)  GT 0 THEN MrPrintF, 'LogWarn', 'RANGE keyword is ignored.'
	
	;Restrictions
	IF tfft GT N THEN Message, 'NFFT > number of records.'
	
	;Gain from window
;	NG = mean(win^2)
;	CG = mean(win)

;-----------------------------------------------------
; Loop Through Each Interval \\\\\\\\\\\\\\\\\\\\\\\\\
;-----------------------------------------------------
	;Total number of FFTs to perform
	nIter = Floor( (N - tfft) / tshift ) + 1
	
	;Initial loop conditions
	iStart = 0
	iEnd   = tfft - 1
	
	;Begin loop
	FOR i = 0, nIter - 1 DO BEGIN
	;-----------------------------------------------------
	; Compute the CSD \\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\
	;-----------------------------------------------------
		oCoh = MrTS_Coherence( oV1, oV2, nfft, nshift, $
		                       RANGE = [iStart, iEnd] )
		IF ~Obj_Valid(oCoh) THEN RETURN, !Null
	
	;-----------------------------------------------------
	; Create Variables \\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\
	;-----------------------------------------------------
		IF i EQ 0 THEN BEGIN
			;Time
			oTime  = MrTimeVar( Replicate('0000-00-00T00:00:00', nIter) )
			
			;Cross-spectrum
			oCohSpec = MrTimeSeries( oTime, FltArr( [nIter, Size(oCoh, /DIMENSIONS)] ), $
			                         CACHE = cache, $
			                         NAME  = name )
			oCohSpec['DEPEND_1'] = oCoh['DEPEND_0']
			nDims = Size(oCoh, /N_DIMENSIONS)
		ENDIF
	
	;-----------------------------------------------------
	; Store Data \\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\
	;-----------------------------------------------------
		;Convert to proper ISO format
		MrTimeParser, oCoh['TIME_STAMP'], '%d-%c-%Y %H:%m:%S%f', '%Y-%M-%dT%H:%m:%S%f%z', tstamp
		
		;Time
		oTime[i] = tstamp
		
		;CSD
		CASE nDims OF
			1: oCohSpec[i,*]             = oCoh['DATA']
			2: oCohSpec[i,*,*]           = oCoh['DATA']
			3: oCohSpec[i,*,*,*]         = oCoh['DATA']
			4: oCohSpec[i,*,*,*,*]       = oCoh['DATA']
			5: oCohSpec[i,*,*,*,*,*]     = oCoh['DATA']
			6: oCohSpec[i,*,*,*,*,*,*]   = oCoh['DATA']
			7: oCohSpec[i,*,*,*,*,*,*,*] = oCoh['DATA']
			ELSE: Message, 'Invalid number of demensions for VAR1.'
		ENDCASE
	
	;-----------------------------------------------------
	; Next Interval \\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\
	;-----------------------------------------------------
		iStart += tshift
		iEnd    = iStart + tfft - 1
	ENDFOR
	
;-----------------------------------------------------
; Done! \\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\
;-----------------------------------------------------
	RETURN, oCohSpec
END