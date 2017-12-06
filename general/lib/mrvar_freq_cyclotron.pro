; docformat = 'rst'
;
; NAME:
;       MrVar_Freq_Cyclotron
;
;*****************************************************************************************
;   Copyright (c) 2016, Matthew Argall                                                   ;
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
;   Compute the cyclotron frequency.
;
;       f = 1/(2*!pi) * q * |B| / m
;
; :Categories:
;   MrVariable
;
; :Params:
;       BMAG:           in, required, type=string/integer/objref
;                       Name, number, or MrTimeSeries object of the magnetic field magnitude (nT).
;       MASS:           in, required, type=string/float
;                       Mass (kg) of particle species. If a string, any mass recognized by
;                           MrConstants is acceptable.
;
; :Keywords:
;       CACHE:          in, optional, type=boolean, default=0
;                       If set, the output will be added to the variable cache.
;       NAME:           in, optional, type=string, default='Cyclotron_Frequency'
;                       Name to be given to the output variable.
;       NO_CLOBBER:     in, optional, type=boolean, default=0
;                       If set, cached variables with name `NAME` are not over-written.
;                           Instead, the output variable will have "_#" appended, were "#"
;                           represents a unique number.
;
; :Returns:
;       FC:             out, required, type=objref
;                       Cyclotron frequency (Hz) of the same variable class as `BMAG`.
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
;       2016/12/08  -   Written by Matthew Argall
;       2017/02/23  -   CACHE keyword was not being checked. Fixed. - MRA
;       2017/05/11  -   Check BMAG for units or SI conversion. - MRA
;-
function MrVar_Freq_Cyclotron, Bmag, mass, $
CACHE=cache, $
NAME=name, $
NO_CLOBBER=no_clobber
	compile_opt idl2
	on_error, 2
	
	
	;Constants
	q   = MrConstants('q')
	m   = Size(mass, /TNAME) EQ 'STRING' ? MrConstants(mass) : mass
	if n_elements(name) eq 0 then name = 'Cyclotron_Frequency'
	
;-----------------------------------------------------
; Units \\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\
;-----------------------------------------------------
	;Get the variables
	oBmag = MrVar_Get(Bmag)
	IF ~Obj_IsA(oBmag, 'MrScalarTS') THEN Message, 'BMAG must be a MrScalarTS variable.'
	
	;B UNITS
	IF oBmag -> HasAttr('SI_CONVERSION') THEN BEGIN
		b_si = StrSplit(oBmag['SI_CONVERSION'], '>', /EXTRACT)
		b_si = b_si[0] EQ '' ? 1.0 : Float(b_si[0])
	ENDIF ELSE IF oBmag -> HasAttr('UNITS') THEN BEGIN
		CASE StrLowCase(oBmag['UNITS']) OF
			'nt': b_si = 1e-9
			't':  b_si = 1.0
			ELSE: BEGIN
				MrPrintF, 'LogWarn', 'Unrecognized unis for BMAG: "' + oBmag['UNITS'] + '". Assuming nT.'
				b_si = 1e-9
			ENDCASE
		ENDCASE
	ENDIF ELSE BEGIN
		MrPrintF, 'LogWarn', 'BMAG has no SI_CONVERSION or UNITS attribute. Assuming nT.'
		b_si = 1e-9
	ENDELSE
	
;-----------------------------------------------------
; Cyclotron Frequency \\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\
;-----------------------------------------------------

	;Cyclotron frequency
	fc = (b_si*q/(m*2*!pi)) * oBmag ;Hz
	
;-----------------------------------------------------
; Finish Up \\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\
;-----------------------------------------------------
	
	;Name & cache
	fc -> SetName, name
	if Keyword_Set(cache) then fc -> Cache, NO_CLOBBER=no_clobber
	
	;Attributes
	fc['CATDESC']       = 'Cyclotron frequency: f = q * |B| / m.'
	fc['PLOT_TITLE']    = 'Cyclotron frequency'
	fc['TITLE']         = 'fc!C(Hz)'
	fc['UNITS']         = 'Hz'
	fc['SI_CONVERSION'] = '>'
	
	;Cleanup variables
	return, fc
end