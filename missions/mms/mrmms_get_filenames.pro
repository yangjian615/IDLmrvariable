; docformat = 'rst'
;
; NAME:
;       MrMMS_Get_Data
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
;   Query the MMS SDC for the available file names.
;
; :Categories:
;       CDF Utilities
;
; :Params:
;       SC:                 in, optional, type=string/strarr, default=''
;                           The MMS spacecraft identifier. Options are:
;                               {'mms1' | 'mms2' | 'mms3' | 'mms4'}
;       INSTR:              in, optional, type=string/strarr, default=''
;                           Instrument ID for which data is read.
;       MODE:               in, optional, type=string/strarr, default=''
;                           Telemetry mode of the data. Options include:
;                               {'slow' | 'fast' | 'srvy' | 'brst'}
;       LEVEL:              in, optional, type=string/strarr, default=''
;                           Data quality level. Options include:
;                               {'l1a' | 'l1b' | 'l2pre' | 'l2'}
;
; :Keywords:
;       OPTDESC:            in, optional, type=string, default=''
;                           Optional descriptor of the data.
;       TEAM_SITE:          in, optional, type=boolean, default=0
;                           If set, requests will be sent to the team site (password
;                               required, L1A and above). Automatically set if `LEVEL` 
;                               is below "L2". This option is sticky.
;       TRANGE:             out, optional, type=string, default=MrVar_GetTRange()
;                           Start and end times over which to read data.
;       VARFORMAT:          out, optional, type=string, default='*'
;                           Variables that match this search pattern will be read,
;                               others are ignored.
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
;       2014/07/22  -   Written by Matthew Argall
;-
function MrMMS_Get_Filenames, sc, instr, mode, level, $
COUNT=count, $
OPTDESC=optdesc, $
TEAM_SITE=team_site, $
TRANGE=trange, $
_REF_EXTRA=extra
	compile_opt idl2

	catch, the_error
	if the_error ne 0 then begin
		catch, /CANCEL
		MrPrintF, 'LogErr'
		return, ''
	endif
	
	;Defaults
	count = 0
	if n_elements(trange) eq 0 then trange = MrVar_GetTRange()
	if n_elements(sc)     eq 0 then sc     = ''
	if n_elements(instr)  eq 0 then instr  = ''
	if n_elements(mode)   eq 0 then mode   = ''
	if n_elements(level)  eq 0 then level  = ''
	
	;Initialize MMS
	MrMMS_Init
	
;-----------------------------------------------------
; Web \\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\
;-----------------------------------------------------
	;Change direstories to the proper request
	;   - Always start with a new request.
	!MrMMS -> SetProperty, /RESET_PATH, $
	                       /RESET_QUERY, $
	                       /FILE_NAMES, $
	                       SC_ID         = sc, $
	                       INSTR         = instr, $
	                       MODE          = mode, $
	                       LEVEL         = level, $
	                       OPTDESC       = optdesc, $
	                       PUBLIC_SITE   = public_site, $
	                       TEAM_SITE     = team_site, $
	                       DATE_START    = trange[0], $
	                       DATE_END      = trange[1], $
	                       _STRICT_EXTRA = extra
	
	;Attempt to get the data
	files = !MrMMS -> FileNames(COUNT=count)

	;Return
	return, files
end