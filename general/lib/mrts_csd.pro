; docformat = 'rst'
;
; NAME:
;       MrTS_CSD
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
;   Compute the cross-spectral density of two time-series signals. Note that only the
;   trace of the CSD is computed.
;
; :Params:
;       VAR:            in, optional, type=integer/string/objref
;                       The name, number or object reference of a MrTimeSeries object
;                           for which the cross-spectral density is to be computed.
;       NFFT:           in, optional, type=int, default=nPts
;                       Number of points in each FFT. If NFFT is shorter than the
;                           signal length defined by `RANGE`, then the PSD of each NFFT
;                           segment that fits within `RANGE` will be averaged together.
;       NSHIFT:         in, optional, type=integer, default=NFFT/2
;                       Number of points to shift between consecutive FFTs.
;
; :Keywords:
;       CACHE:          in, optional, type=boolean, default=0
;                       If set, `RESULT` will be added to the variable cache.
;       COINCIDENCE:    out, optional, type=objref
;                       A named variable to receive the real part of the cross-spectral
;                           density, returned as a MrVariable object.
;       NAME:           in, optional, type=string, default='PSD('+<name>+')'
;                       Name to be given to `RESULT`.
;       PHASE:          out, optional, type=objref
;                       A named variable to receive the phase of the cross-spectrum,
;                           returned as a MrVariable object.
;       QUADRATURE:     out, optional, type=objref
;                       A named variable to receive the imaginary part of the cross-spectral
;                           density, returned as a MrVariable object.
;       RANGE:          in, optional, type=intarr(2)/strarr(2), default="[0,nPts-1]"
;                       A time or index range outlining a subset of the data for which
;                           the PSD is computed.
;       WINDOW:         in, optional, type=string, default='Rectangular'
;                       The name of a tapering window to be applied to the data. See
;                           the FFT_Window method for valid names.
;       _REF_EXTRA:     in, optional, type=any
;                       Any keyword accepted by the MrTimeSeries::FFT method is also accepted here.
;
; :Returns:
;       OCSD:           out, required, type=MrVariable object
;                       The cross-spectral density.
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
FUNCTION MrTS_CSD, var1, var2, nfft, nshift, $
CACHE=cache, $
COINCIDENCE=oCo, $
NAME=name, $
PHASE=oPhase, $
QUADRATURE=oQuad, $
RANGE=range, $
WINDOW=win, $
_REF_EXTRA=extra
	Compile_Opt idl2
	
	Catch, the_error
	IF the_error NE 0 THEN BEGIN
		Catch, /CANCEL
		MrPrintF, 'LogErr'
		RETURN, !Null
	ENDIF
	
	;Must have the same time stamps.
	oV1 = MrVar_Get(var1)
	oV2 = MrVar_Get(var2)
	IF ~oV1 -> IsTimeIdentical(oV2) $
		THEN Message, 'VAR must have the same time stamps as the implicit array.'
	IF ~Obj_IsA(oV1, 'MrTimeSeries') THEN Message, 'VAR1 must be a MrTimeSeries variable.'
	IF ~Obj_IsA(oV2, 'MrTimeSeries') THEN Message, 'VAR2 must be a MrTimeSeries variable.'
	
	;Must be the same size
	IF ~Array_Equal(Size(oV1, /DIMENSIONS), Size(oV2, /DIMENSIONS)) $
		THEN Message, 'VAR must be the same size as the implicit array.'
	
	;Range
	IF N_Elements(range)  EQ 0 THEN range  = [0, (oV1 -> GetNPts()) - 1]
	IF Size(range, /TNAME) EQ 'STRING' $
		THEN irange = oV1 -> Value_Locate(range) $
		ELSE irange = range
	
	;Other defaults
	tf_rms    = Keyword_Set(rms)
	tf_noise  = Keyword_Set(noise)
	tf_power  = Keyword_Set(power)
	tf_window = Keyword_Set(win)
	IF N_Elements(nfft)   EQ 0 THEN nfft   = irange[1] - irange[0] + 1
	IF N_Elements(nshift) EQ 0 THEN nshift = nfft/2
	IF N_Elements(name)   EQ 0 THEN name   = 'CSD(' + oV1.name + ',' + oV2.name + ')'
	
	;Gain from window
;	NG = mean(win^2)
;	CG = mean(win)

;-----------------------------------------------------
; Compute FFT \\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\
;-----------------------------------------------------

	oFFT1 = oV1 -> FFT( nfft, nshift, $
	                    RANGE         = irange, $
	                    WINDOW        = win, $
	                    _STRICT_EXTRA = extra )

	oFFT2 = oV2 -> FFT( nfft, nshift, $
	                    RANGE         = irange, $
	                    WINDOW        = win, $
	                    _STRICT_EXTRA = extra )

;-----------------------------------------------------
; Normalizations \\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\
;-----------------------------------------------------
	;Zero padding
	zeroNorm = (oFFT1['NZEROPAD'] + nfft)^2.0 / Float(nfft)
	
	;Tapering window
	winNorm = Total(oFFT1['WINDOW']^2) / nfft

;-----------------------------------------------------
; One-Sided PSD \\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\
;-----------------------------------------------------
	tf_ts = IsA(oFFT1, 'MrTimeSeries')
	
	;Time series
	IF tf_ts THEN BEGIN
		;If a time series, minimum # dims = 2: [time, freq]
		CASE Size(oFFT1, /N_DIMENSIONS) OF
			2: temp = oFFT1['DATA',*,0:nfft/2]             * Conj( oFFT2['DATA',*,0:nfft/2] )
			3: temp = oFFT1['DATA',*,0:nfft/2,*]           * Conj( oFFT2['DATA',*,0:nfft/2,*] )
			4: temp = oFFT1['DATA',*,0:nfft/2,*,*]         * Conj( oFFT2['DATA',*,0:nfft/2,*,*] )
			5: temp = oFFT1['DATA',*,0:nfft/2,*,*,*]       * Conj( oFFT2['DATA',*,0:nfft/2,*,*,*] )
			6: temp = oFFT1['DATA',*,0:nfft/2,*,*,*,*]     * Conj( oFFT2['DATA',*,0:nfft/2,*,*,*,*] )
			7: temp = oFFT1['DATA',*,0:nfft/2,*,*,*,*,*]   * Conj( oFFT2['DATA',*,0:nfft/2,*,*,*,*,*] )
			8: BEGIN
				fft1_data = oFFT1 -> GetData()
				fft2_data = oFFT2 -> GetData()
				temp     = fft1_data[*,0:nfft/2,*,*,*,*,*,*] * Conj( fft2_data[*,0:nfft/2,*,*,*,*,*,*] )
				fft1_data = !Null
				fft2_data = !Null
			ENDCASE
			ELSE: message, 'FFT must have <= 8 dimensions.'
		ENDCASE
	
	;Single CSD
	ENDIF ELSE BEGIN
		CASE Size(oFFT1, /N_DIMENSIONS) OF
			1: temp = oFFT1['DATA',0:nfft/2]             * Conj( oFFT2['DATA',0:nfft/2] )
			2: temp = oFFT1['DATA',0:nfft/2,*]           * Conj( oFFT2['DATA',0:nfft/2,*] )
			3: temp = oFFT1['DATA',0:nfft/2,*,*]         * Conj( oFFT2['DATA',0:nfft/2,*,*] )
			4: temp = oFFT1['DATA',0:nfft/2,*,*,*]       * Conj( oFFT2['DATA',0:nfft/2,*,*,*] )
			5: temp = oFFT1['DATA',0:nfft/2,*,*,*,*]     * Conj( oFFT2['DATA',0:nfft/2,*,*,*,*] )
			6: temp = oFFT1['DATA',0:nfft/2,*,*,*,*,*]   * Conj( oFFT2['DATA',0:nfft/2,*,*,*,*,*] )
			7: temp = oFFT1['DATA',0:nfft/2,*,*,*,*,*,*] * Conj( oFFT2['DATA',0:nfft/2,*,*,*,*,*,*] )
			8: BEGIN
				fft1_data = oFFT1 -> GetData()
				fft2_data = oFFT2 -> GetData()
				temp     = fft1_data[0:nfft/2,*,*,*,*,*,*,*] * Conj( fft2_data[0:nfft/2,*,*,*,*,*,*,*] )
				fft1_data = !Null
				fft2_data = !Null
			ENDCASE
			ELSE: message, 'FFT must have <= 8 dimensions.'
		ENDCASE
	ENDELSE
	
	;Normalize
	csd = 2.0 *  zeroNorm / winNorm / oFFT1['SAMPLE_RATE'] * Temporary(temp)
	
	;Average over time
	IF tf_ts THEN csd = Mean(csd, DIMENSION=1)

;-----------------------------------------------------
; Set Properties \\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\
;-----------------------------------------------------
	
	;PWR attributes
	oCSD = MrVariable( csd, CACHE=cache, NAME=name )
	oFFT1 -> CopyAttrTo, oCSD, ['NFFT', 'NSHIFT', 'NZEROPAD', 'SAMPLE_RATE', 'WINDOW']
	IF ~tf_ts THEN oFFT1 -> CopyAttrTo, oCSD, 'TIME_STAMP'
	
	;Frequency attributes
	oFreq               = tf_ts ? oFFT1['DEPEND_1'] : oFFT1['DEPEND_0']
	oFreq['AXIS_RANGE'] = [oFreq['DATA',1], oFreq['DATA',nfft/2]]
	oFreq['LOG']        = 1B
	oFreq['TITLE']      = 'Freq (' + oFreq['UNITS'] + ')'
	
	oCSD['DEPEND_0'] = oFreq[0:nfft/2]
	oCSD['TITLE']    = 'CSD'
	oCSD['LOG']      = 1

;-----------------------------------------------------
; Phase \\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\
;-----------------------------------------------------
	IF Arg_Present(oPhase) THEN BEGIN
		;Create the variable
		oPhase = MrVariable( !radeg * ATan( csd, /PHASE ), $
		                     CACHE = cache, $
		                     NAME  = 'Phase(' + oV1.name + ',' + oV2.name + ')' )
		
		;Add attributes
		oPhase['AXIS_RANGE']   = [-180.0, 180.0]
		oPhase['DEPEND_0']     = oFreq[0:nfft/2]
		oPhase['UNITS']        = 'Degrees'
		oPhase['TICKINTERVAL'] = 90.0
		oPhase['TITLE']        = 'Phase!C(Deg)'
	ENDIF

;-----------------------------------------------------
; Coincidence \\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\
;-----------------------------------------------------
	IF Arg_Present(oCo) THEN BEGIN
		;Create the variable
		oCo = MrVariable( Real_Part( csd ), $
		                  CACHE = cache, $
		                  NAME  = 'Coincidence(' + oV1.name + ',' + oV2.name + ')' )
		
		;Add attributes
		oCo['DEPEND_0']   = oFreq[0:nfft/2]
		oCo['TITLE']      = 'Coincidence'
	ENDIF

;-----------------------------------------------------
; Quadrature \\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\
;-----------------------------------------------------
	IF Arg_Present(oQuad) THEN BEGIN
		;Create the variable
		oQuad = MrVariable( Imaginary( csd ), $
		                    CACHE = cache, $
		                    NAME  = 'Quadrature(' + oV1.name + ',' + oV2.name + ')' )
		
		;Add attributes
		oQuad['DEPEND_0']   = oFreq[0:nfft/2]
		oQuad['TITLE']      = 'Quadrature'
	ENDIF

;-----------------------------------------------------
; Done! \\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\
;-----------------------------------------------------

	;Normalize
;	CASE 1 OF
;		tf_rms:   pwr = NG * pwr / CG^2
;		tf_noise: pwr = pwr / (NG * df)
;		tf_power: pwr = pwr / NG
;		ELSE:     ;Do nothing
;	ENDCASE
	
	;Normalize
;	CASE 1 OF
;		tf_rms:   pwr = NG * pwr / CG^2
;		tf_noise: pwr = NG * df * pwr / CG^2
;		tf_power: pwr = NG * pwr / CG^2
;		ELSE:   ;Nothing
;	ENDCASE
	
	RETURN, oCSD
END
