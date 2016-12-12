; docformat = 'rst'
;
; NAME:
;       MrMMS_Anc_Parse_Time
;
;*****************************************************************************************
;   Copyright (c) 2015, University of New Hampshire                                      ;
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
;       * Neither the name of the University of New Hampshire nor the names of its       ;
;         contributors may  be used to endorse or promote products derived from this     ;
;         software without specific prior written permission.                            ;
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
;       Parse a time segment of a valid MMS ancillary file name.
;
; :Categories:
;       MMS
;
; :Params:
;       TSTART:     in, required, type=string
;                   Either the start or end time from an ancillary date file name.
;       YEAR:       out, optional, type=string
;                   The year component.
;       DOY:        out, optional, type=string
;                   The day-of-year component.
;       MONTH:      out, optional, type=string
;                   The month component.
;       DAY:        out, optional, type=string
;                   The day component.
;
; :Keywords:
;       TT2000:     out, optional, type=string
;                   `TSTART` converted to a CDF TT2000 epoch time.
;       INTEGER:    out, optional, type=boolean, default=0
;                   If set, `YEAR`, `MONTH`, `DAY`, `HOUR`, `MINUTE`, `SECONDS` are
;                       returned as integers instead of strings.
;
; :Author:
;   Matthew Argall::
;       University of New Hampshire
;       Morse Hall, Room 348
;       8 College Rd.
;       Durham, NH, 03824
;       matthew.argall@unh.edu
;
; :Copyright:
;       Matthew Argall 2015
;
; :History::
;   Modification History::
;       2016/11/18  -   Written by Matthew Argall
;-
pro MrMMS_Anc_Parse_Time, time, year, doy, month, day, $
INTEGER=integer, $
TT2000=tt2000
	compile_opt strictarr
	on_error, 2

	;Must contain at least 'yyyymmdd'
	if ~array_equal(strlen(time), 7) then $
		message, 'Improperly formatted TIME string.'

	;Parse the components
	year = strmid(time, 0, 4)
	doy  = strmid(time, 4, 3)
	
	;Compute month and day
	date  = MrDOY2Date(fix(doy), fix(year))
	month = reform(date[1,*])
	day   = reform(date[0,*])
	
	;Compute the CDF TT2000 epoch value
	if arg_present(tt2000) $
		then tt2000 = MrCDF_Epoch_Compute(fix(year), fix(month), fix(day), 0, 0, 0)
	
	;Convert to integers?
	if keyword_set(integer) then begin
		year  = fix(year)
		doy   = fix(doy)
	endif else begin
		month = string(month, FORMAT='(i02)')
		day   = string(day,   FORMAT='(i02)')
	endelse
end