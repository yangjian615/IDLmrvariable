; docformat = 'rst'
;
; NAME:
;       MrVar_E_VxB
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
;   Compute the convective electric field.
;
;       E = -VxB
;
; :Params:
;       V:              in, required, type=string/integer/objref
;                       Name, number, or MrVectorTS variable of the plasma bulk velocity.
;       B:              in, required, type=string/integer/objref
;                       Name, number, or MrVectorTS variable of the vector magnetic field.
;                           If B does not have the same time tags as `V`, then it will
;                           be interpolated to `V`.
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
;       OE_VXB:         out, required, type=objref
;                       Convective electric field as a MrVectorTS object.
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
;       2017/05/10  -   Written by Matthew Argall
;-
function MrVar_E_VxB, v, b, $
CACHE=cache, $
NAME=name, $
NO_CLOBBER=no_clobber
	compile_opt idl2
	on_error, 2
	
	;Grab the variables
	oV = MrVar_Get(v)
	oB = MrVar_Get(b)
	IF ~Obj_IsA(oV, 'MrVectorTS') THEN Message, 'V must be a MrVectorTS variable.'
	IF ~Obj_IsA(oB, 'MrVectorTS') THEN Message, 'B must be a MrVectorTS variable.'
	
	;V UNITS
	IF oV -> HasAttr('SI_CONVERSION') THEN BEGIN
		v_si = StrSplit(oV['SI_CONVERSION'], '>', /EXTRACT)
		v_si = v_si[0] EQ '' ? 1.0 : Float(v_si[0])
	ENDIF ELSE IF oV -> HasAttr('UNITS') THEN BEGIN
		CASE StrLowCase(oV['UNITS']) OF
			'km/s': v_si = 1e3
			'm/s':  v_si = 1.0
			ELSE: BEGIN
				MrPrintF, 'LogWarn', 'Unrecognized unis for V: "' + oV['UNITS'] + '". Assuming km/s.'
				v_si = 1e3
			ENDCASE
		ENDCASE
	ENDIF ELSE BEGIN
		MrPrintF, 'LogWarn', 'V has no SI_CONVERSION or UNITS attribute. Assuming km/s.'
		v_si = 1e3
	ENDELSE
	
	;B UNITS
	IF oB -> HasAttr('SI_CONVERSION') THEN BEGIN
		b_si = StrSplit(oB['SI_CONVERSION'], '>', /EXTRACT)
		b_si = b_si[0] EQ '' ? 1.0 : Float(b_si[0])
	ENDIF ELSE IF oB -> HasAttr('UNITS') THEN BEGIN
		CASE StrLowCase(oB['UNITS']) OF
			'nt': b_si = 1e-9
			't':  b_si = 1.0
			ELSE: BEGIN
				MrPrintF, 'LogWarn', 'Unrecognized unis for B: "' + oB['UNITS'] + '". Assuming nT.'
				b_si = 1e-9
			ENDCASE
		ENDCASE
	ENDIF ELSE BEGIN
		MrPrintF, 'LogWarn', 'B has no SI_CONVERSION or UNITS attribute. Assuming nT.'
		b_si = 1e-9
	ENDELSE
	
	;Time tags
	IF ~oB -> IsTimeIdentical(oV) THEN oB = Interpol(oV)
	
	;Compute the convective electric field
	;   - 1e3 Converts to mV/m
	oE_VxB = -1e3 * b_si * v_si * oV -> Cross(oB)

	;Attributes
	oE_VxB['CATDESC']       = 'Convective electric field computed as VxB.'
	oE_VxB['LABEL']         = ['Ex', 'Ey', 'Ez']
	oE_VxB['PLOT_TITLE']    = 'Convective Electric Field'
	oE_VxB['TITLE']         = 'E!C(mV/m)'
	oE_VxB['UNITS']         = 'mV/m'
	oE_VxB['SI_CONVERSION'] = '1e-3>V/m'
	
	;Cleanup variables
	RETURN, oE_VxB
END