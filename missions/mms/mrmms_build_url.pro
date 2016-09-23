; docformat = 'rst'
;
; NAME:
;       MMS_BUILD_FILENAME
;
; PURPOSE:
;+
;   The purpose of this program is to create an MMS CDF file name using the user
;   input information::
;
;       sc_instr_mode_level[_optdesc]_tstart_version.cdf
;
;   With the option of appending an SDC-like directory structure, formatted as
;
;       url_root/sc/instr/mode/level[/optdesc]/year/month[/day]
;
; :Categories:
;   MMS
;
;
; :Author:
;       Matthew Argall::
;       University of New Hampshire
;       Morse Hall, Room 348
;       8 College Rd.
;       Durham, NH, 03824
;       matthew.argall@unh.edu
;
; :History:
;   Modification History::
;       2016/05/29  -   Written by Matthew Argall
;-
;*****************************************************************************************
;+
;   Create an MMS SDC-like directory structure.
;
; :Params:
;       ROOT:       in, optional, type=string, default=''
;                   Root of an SDC-like directory structure.
;       SC:         in, required, type=string/strarr
;                   Spacecraft identifier. Options are 'mms1', 'mms2', 'mms3', and 'mms4'
;       INSTR:      in, required, type=string/strarr
;                   Instrument identifier.
;       MODE:       in, required, type=string/strarr
;                   Data telemetry mode.
;       LEVEL:      in, required, type=string/strarr
;                   Data quality level.
;       OPTDESC:    in, optional, type=string, default=''
;                   Optional descriptor.
;-
function MrMMS_Build_URL_Dir, root, sc, instr, mode, level, optdesc, tstart
	compile_opt idl2
	on_error, 2

	;Use tokens if the start time was not given
	if tstart eq '' then begin
		year  = '%Y'
		month = '%M'
		day   = mode eq 'brst' ? '%d' : ''
	endif else begin
		year  = strmid(tstart, 0, 4) 
		month = strmid(tstart, 5, 2)
		day   = mode eq 'brst' ? strmid(tstart, 8, 2) : ''
	endelse

	;Build the URL
	url = strjoin( [root, sc, instr, mode, level, optdesc, year, month, day], '/')
	
	return, url
end


;+
;   Create an MMS file name
;
; :Params:
;       SC:         in, required, type=string/strarr
;                   Spacecraft identifier. Options are 'mms1', 'mms2', 'mms3', and 'mms4'
;       INSTR:      in, required, type=string/strarr
;                   Instrument identifier.
;       MODE:       in, required, type=string/strarr
;                   Data telemetry mode.
;       LEVEL:      in, required, type=string/strarr
;                   Data quality level.
;       OPTDESC:    in, required, type=string, default=''
;                   Optional descriptor.
;       TSTART:     in, required, type=string, default=''
;                   Start time of the data file to be created, formatted as an ISO-8601
;                       string: 'YYYY-MM-DDThh:mm:ssZ'. If the empty string is given,
;                       MrTokens will be used.
;       VERSION:    in, required, type=string, default=''
;                   File version number.
;-
function MrMMS_Build_URL_Join, sc, instr, mode, level, optdesc, tstart, version
	compile_opt idl2
	on_error, 2

	;Use tokens if the start time was not given
	if tstart eq '' then begin
		fstart = mode eq 'brst' ? '%Y%M%d%H%m%S' : '%Y%M%d'
	endif else begin
		fstart = strmid(tstart, 0, 4) + strmid(tstart, 5, 2) + strmid(tstart, 8, 2)
		if mode eq 'brst' then fstart += strmid(tstart, 11, 2) + strmid(tstart, 14, 2) + strmid(tstart, 17, 2)
	endelse

	;Separate an optional descriptor form the start time.
	if optdesc eq '' $
		then fdesc = optdesc $
		else fdesc = optdesc + '_'

	;Build the file name
	filename = sc     + '_' + $
	           instr  + '_' + $
	           mode   + '_' + $
	           level  + '_' + $
	           fdesc  + $
	           fstart + '_' + $
	           'v'    + version + $
	           '.cdf'
	
	return, filename
end


;+
;   Create MMS file names
;
; :Params:
;       SC:         in, required, type=string/strarr
;                   Spacecraft identifier. Options are 'mms1', 'mms2', 'mms3', and 'mms4'
;       INSTR:      in, required, type=string/strarr
;                   Instrument identifier.
;       MODE:       in, required, type=string/strarr
;                   Data telemetry mode.
;       LEVEL:      in, required, type=string/strarr
;                   Data quality level.
;
; :Keywords:
;       COUNT:      out, optional, type=integer
;                   Number of URLs returned.
;       OPTDESC:    in, optional, type=string, default=''
;                   Optional descriptor.
;       URL_ROOT:   in, optional, type=string, default='https://lasp.colorado.edu/mms/sdc/public/data/'
;                   URL (host, scheme, path) of the data root. Cannot be used with `TEAM`.
;       TEAM:       in, optional, type=boolean, default=0
;                   If set, `URL_ROOT` will be set to the team SDC site:
;                       'https://lasp.colorado.edu/mms/sdc/team/' (password required). 
;       TSTART:     in, optional, type=string, default=''
;                   Start time of the data file to be created, formatted as an ISO-8601
;                       string: 'YYYY-MM-DDThh:mm:ssZ'. If the empty string is given,
;                       MrTokens will be used.
;       VERSION:    in, optional, type=string, default=''
;                   File version number.
;
; :Returns:
;       FNAME:      out, reqired, type=string/strarr
;                   URLs of MMS CDF files.
;-
function MrMMS_Build_URL, sc, instr, mode, level, $
COUNT=count, $
OPTDESC=optdesc, $
TEAM=team, $
TSTART=tstart, $
URL_ROOT=url_root, $
VERSION=version
	compile_opt strictarr
	on_error, 2
	
	;Defaults
	if n_elements(tstart)   eq 0 then tstart  = ''
	if n_elements(optdesc)  eq 0 then optdesc = ''
	if n_elements(version)  eq 0 then version = '*'
	if n_elements(url_root) eq 0 then begin
		if keyword_set(team) $
			then url_root = 'https://lasp.colorado.edu/mms/sdc/team/' $
			else url_root = 'https://lasp.colorado.edu/mms/sdc/public/data/'
	endif else begin
		if n_elements(team) gt 0 then message, 'TEAM and URL_ROOT are mutually exclusive.'
	endelse

	;Number of values given
	nSC      = n_elements(sc)
	nInstr   = n_elements(instr)
	nMode    = n_elements(mode)
	nLevel   = n_elements(level)
	nDesc    = n_elements(optdesc)
	nTStart  = n_elements(tstart)
	nVersion = n_elements(version)
	
	;Conflicts
	if nTStart  gt 1 then message, 'TSTART must be scalar.'
	if nVersion gt 1 then message, 'VERSION must be scalar.'

;-----------------------------------------------------
; Non-Uniform Output \\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\
;-----------------------------------------------------
	;Allocate memory to output
	fname = strarr(nSC*nInstr*nMode*nLevel*nDesc)
	count = 0

	;Create the MMS filename
	;   - Must use loops because:
	;   - Replicate does not work with arrays.
	;   - Rebin does not work with strings.
	for i = 0, nSC    - 1 do $
	for j = 0, nInstr - 1 do $
	for k = 0, nMode  - 1 do $
	for l = 0, nLevel - 1 do $
	for m = 0, nDesc  - 1 do begin
		
		;Filename
		fname[count] = MrMMS_Build_URL_Join(sc[i], instr[j], mode[k], level[l], optdesc[m], $
		                                    tstart, version)
		
		;Directory
		url_base = MrMMS_Build_URL_Dir(url_root, sc[i], instr[j], mode[k], level[l], optdesc[m], tstart)
		
		;Append the directory name
		fname[count] = url_base + '/' + fname[count]
		
		;Number of files
		count += 1
	endfor ;Loop over m
	
	;Return a scalar
	if count eq 1 then fname = fname[0]
	return, fname
end