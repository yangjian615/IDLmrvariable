; docformat = 'rst'
;
; NAME:
;       MrMMS_Parse_Time
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
;       Parse the TSTART segment of a valid MMS file name.
;
; :Categories:
;       MMS
;
; :Params:
;       TSTART:     in, required, type=string
;                   The filename of the MMS file to be dissected. Format
;                       is yyyymmddHHMMSS, yyyymmdd, or yyyyDDD. Missing fields
;                       default to 0.
;       YEAR:       out, optional, type=string
;                   The year component.
;       MONTH:      out, optional, type=string
;                   The month component.
;       DAY:        out, optional, type=string
;                   The day component.
;       HOUR:       out, optional, type=string
;                   The hour component.
;       MINUTE:     out, optional, type=string
;                   The minutes component.
;       SECOND:     out, optional, type=string
;                   The seconds component.
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
;       2015/11/20  -   Written by Matthew Argall
;       2016/06/29  -   Renamed to MrMMS_Parse_Time from mms_parse_time. - MRA
;       2017/07/12  -   Also parse start and end times of ancillary files. - MRA
;-
pro MrMMS_Parse_Time, tstart, year, month, day, hour, minute, second, $
INTEGER=integer, $
TT2000=tt2000
	compile_opt strictarr
	on_error, 2
	
	;Default
	tf_tt2000 = Arg_Present(tt2000)
	tf_int    = Keyword_Set(integer)
	
	;Validate times
	If ~Array_Equal( MrTokens_IsMatch(tstart, '%Y%M%d') OR $
	                 MrTokens_IsMatch(tstart, '%Y%M%d%H%m%S') OR $
	                 MrTokens_IsMatch(tstart, '%Y%M%D'), 1 ) $
		THEN Message, 'Improperly formatted value for TSTART.'
	
	;Allocate memory
	nTimes = N_Elements(tstart)
	year   = StrArr(nTimes)
	month  = StrArr(nTimes)
	day    = StrArr(nTimes)
	hour   = StrArr(nTimes)
	minute = StrArr(nTimes)
	second = StrArr(nTimes)
	
	;Separate data and ancillary times
	iAnc = Where( StRegEx(tstart, '^[0-9]{4}[0-9]{3}$', /BOOLEAN), nAnc, $
	              COMPLEMENT  = iData, $
	              NCOMPLEMENT = nData )
	
	;DATA
	IF nData GT 0 THEN BEGIN
		year[iData]   = StrMid(tstart[iData],  0, 4)
		month[iData]  = StrMid(tstart[iData],  4, 2)
		day[iData]    = StrMid(tstart[iData],  6, 2)
		hour[iData]   = StrMid(tstart[iData],  8, 2)
		minute[iData] = StrMid(tstart[iData], 10, 2)
		second[iData] = StrMid(tstart[iData], 12, 2)
	ENDIF
	
	;ANCILLARY
	IF nAnc GT 0 THEN BEGIN
		year[iAnc]  = StrMid(tstart[iAnc], 0, 4)
		doy         = StrMid(tstart[iAnc], 4, 3)
		temp        = MrDOY2Date( fix(doy), fix(year[iAnc]) )
		month[iAnc] = String( Reform(temp[1,*]), FORMAT='(i0)' )
		day[iAnc]   = String( Reform(temp[0,*]), FORMAT='(i0)' )
	ENDIF
	
	;Compute the CDF TT2000 epoch value
	IF tf_tt2000 THEN BEGIN
		tt2000 = MrCDF_Epoch_Compute( fix(year), fix(month),  fix(day), $
		                              fix(hour), fix(minute), fix(second) )
	ENDIF
	
	;Convert to integers?
	IF tf_int THEN BEGIN
		year   = fix(year)
		month  = fix(month)
		day    = fix(day)
		hour   = fix(hour)
		minute = fix(minute)
		second = fix(second)
	ENDIF
	
	;Scalar
	IF nTimes EQ 1 THEN BEGIN
		year   = year[0]
		month  = month[0]
		day    = day[0]
		hour   = hour[0]
		minute = minute[0]
		second = second[0]
	ENDIF
END