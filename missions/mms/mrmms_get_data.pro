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
;   Search for, and download if necessary, MMS data files.
;
; :Categories:
;       MMS
;
; :Params:
;       SC:                 in, required, type=string/strarr
;                           The MMS spacecraft identifier. Options are:
;                               {'mms1' | 'mms2' | 'mms3' | 'mms4'}
;       INSTR:              in, required, type=string/strarr
;                           Instrument ID for which data is read.
;       MODE:               in, required, type=string/strarr
;                           Telemetry mode of the data. Options include:
;                               {'slow' | 'fast' | 'srvy' | 'brst'}
;       LEVEL:              in, optional, type=string/strarr, default='l2'
;                           Data quality level. Options include:
;                               {'l1a' | 'l1b' | 'l2pre' | 'l2'}
;
; :Keywords:
;       COUNT:              out, optional, type=integer
;                           Number of files matching request.
;       OPTDESC:            in, optional, type=string, default=''
;                           Optional descriptor of the data.
;       TEAM_SITE:          in, optional, type=boolean, default=0
;                           If set, requests will be sent to the team site (password
;                               required, L1A and above). Automatically set if `LEVEL`
;                               is below level 2. This option is sticky.
;       TRANGE:             in, optional, type=string, default=MrVar_GetTRange()
;                           Start and end times over which to read data.
;       _REF_EXTRA:         in, optional, type=any
;                           Any keyword accepted by MrMMS_SDC_Query::CD is also accepted
;                               via keyword inheritance.
;
; :Returns:
;       FILES:              out, required, type=string/strarr
;                           Names of the downloaded files.
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
;       2014/08/22  -   Written by Matthew Argall
;-
function MrMMS_Get_Data, sc, instr, mode, level, $
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
		count = 0
		return, ''
	endif
	
	;Defaults
	count = 0
	if n_elements(sc)        eq 0 then sc        = ''
	if n_elements(instr)     eq 0 then instr     = ''
	if n_elements(mode)      eq 0 then mode      = ''
	if n_elements(level)     eq 0 then level     = 'l2'
	if n_elements(trange)    eq 0 then trange    = MrVar_GetTRange()
	if n_elements(varformat) eq 0 then varformat = '*'
	
	;Check spacecraft
	tf_sc    = MrIsMember(['', 'mms1', 'mms2', 'mms3', 'mms4'], sc)
	tf_mode  = MrIsMember(['', 'slow', 'fast', 'srvy', 'brst'], mode)
	tf_level = MrIsMember(['', 'l1a', 'l1b', 'l2pre', 'l2', 'l2plus'], level)
	tf_instr = MrIsMember(['', 'afg', 'aspoc', 'dfg', 'dsp', 'edi', 'edp', 'epd-eis', 'feeps', 'fgm', 'fpi', 'hpca', 'mec', 'scm'], instr)
	if ~array_equal(tf_sc, 1)    then message, 'SC must be "mms1", "mms2", "mms3", "mms4".'
	if ~array_equal(tf_mode, 1)  then message, 'MODE must be "slow", "fast", "srvy", "brst".'
	if ~array_equal(tf_level, 1) then message, 'LEVEL must be "l1a", "l1b", "l2pre", "l2", "l2plus".'
	if ~array_equal(tf_instr, 1) then message, 'Invalid value for INSTR: "' + instr + '".'
	
	;Initialize MMS
	MrMMS_Init
	
;-----------------------------------------------------
; Web \\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\
;-----------------------------------------------------
	;Use the team site?
	if max( MrIsMember(['l1a', 'l1b', 'l2pre'], level) ) then team_site = 1B
	if max( MrIsMember(['afg', 'dfg'],          instr) ) then team_site = 1B
	
	;Change direstories to the proper request
	;   - Always start with a new request.
	!MrMMS -> CD, /RESET_PATH, $
	              /RESET_QUERY, $
	              /DOWNLOAD, $
	              SUCCESS       = success, $
	              SC_ID         = sc, $
	              INSTR         = instr, $
	              MODE          = mode, $
	              LEVEL         = level, $
	              OPTDESC       = optdesc, $
	              TEAM_SITE     = team_site, $
	              TSTART        = trange[0], $
	              TEND          = trange[1], $
	              _STRICT_EXTRA = extra
	if success eq 0 then return, ''

	;Attempt to get the data
	files = !MrMMS -> Get(COUNT=count)
	
	;Return
	return, files
end