; docformat = 'rst'
;
; NAME:
;       MrTS_Coherency
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
;   Compute the coherency between two time series signals.
;
; :Params:
;       VAR1:           in, required, type=string/integer/objref
;                       Name, number, or MrTimeSeries variable.
;       VAR2:           in, required, type=string/integer/objref
;                       Name, number, or MrTimeSeries variable.
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
;       OV_AVG:         out, required, type=objref
;                       A MrTimeSeries objref containing the coherence.
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
FUNCTION MrTS_Coherency, var1, var2, nfft, nshift, $
CACHE=cache, $
NAME=name, $
NO_CLOBBER=no_clobber, $
_REF_EXTRA=extra
	Compile_Opt idl2
	On_Error, 2
	
	;Get the variables
	oV1 = MrVar_Get(var1)
	oV2 = MrVar_Get(var2)
	IF ~Obj_IsA(oV1, 'MrTimeSeries') THEN Message, 'VAR1 must be a MrTimeSeries object.'
	IF ~Obj_IsA(oV2, 'MrTimeSeries') THEN Message, 'VAR2 must be a MrTimeSeries object.'
	
	;Defaults
	IF N_Elements(name) EQ 0 THEN name = 'Coherency(' + oV1.name + ',' + oV2.name + ')'
	
	;Cross-spectrum
	oCSD = MrTS_CSD(oV1, oV2, nfft, nshift, _STRICT_EXTRA=extra)
	
	;Individual PSDs
	oPSD1 = oV1 -> PSD(nfft, nshift, _STRICT_EXTRA=extra)
	oPSD2 = oV2 -> PSD(nfft, nshift, _STRICT_EXTRA=extra)
	
	;Coherence
	oCoh = Abs(oCSD['DATA'])^2 / (oPSD1 * oPSD2)
	oCoh -> SetData, Sqrt(oCoh['DATA'])
	
	;Attributes
	oPSD1 -> CopyAttrTo, oCoh
	oCoh['TITLE']      = 'Coherency'
	oCoh['AXIS_RANGE'] = [0,1]
	oCoh['LOG']        = 0B
	
	;Cleanup variables
	oCoh -> SetName, name
	IF Keyword_Set(cache) THEN oCoh -> Cache, NO_CLOBBER=no_clobber
	RETURN, oCoh
END