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
FUNCTION MrMMS_Build_URL_Dir, root, sc, instr, mode, level, optdesc, tstart
	Compile_Opt idl2
	On_Error, 2

	;Use tokens IF the start time was not given
	IF tstart EQ '' THEN BEGIN
		year  = '%Y'
		month = '%M'
		day   = mode EQ 'brst' ? '%d' : ''
	ENDIF ELSE BEGIN
		year  = StrMid(tstart, 0, 4) 
		month = StrMid(tstart, 5, 2)
		day   = mode EQ 'brst' ? StrMid(tstart, 8, 2) : ''
	ENDELSE

	;Build the URL
	url = StrJoin( [root, sc, instr, mode, level, optdesc, year, month, day], '/')
	
	RETURN, url
END


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
FUNCTION MrMMS_Build_URL_Join, sc, instr, mode, level, optdesc, tstart, version
	Compile_Opt idl2
	On_Error, 2

	;Use tokens IF the start time was not given
	IF tstart EQ '' THEN BEGIN
		fstart = mode EQ 'brst' ? '%Y%M%d%H%m%S' : '%Y%M%d'
	ENDIF ELSE BEGIN
		fstart = StrMid(tstart, 0, 4) + StrMid(tstart, 5, 2) + StrMid(tstart, 8, 2)
		IF mode EQ 'brst' THEN fstart += StrMid(tstart, 11, 2) + StrMid(tstart, 14, 2) + StrMid(tstart, 17, 2)
	ENDELSE

	;Separate an optional descriptor form the start time.
	IF optdesc EQ '' || optdesc EQ '*' $
		THEN fdesc = optdesc $
		ELSE fdesc = optdesc + '_'

	;Build the file name
	filename = sc     + '_' + $
	           instr  + '_' + $
	           mode   + '_' + $
	           level  + '_' + $
	           fdesc  + $
	           fstart + '_' + $
	           'v'    + version + $
	           '.cdf'
	
	RETURN, filename
END


;+
;   Create MMS file names
;
; :Keywords:
;       COUNT:      out, optional, type=integer
;                   Number of URLs returned.
;       INSTR:      in, required, type=string/strarr, default=all
;                   Instrument identifier. All instruments are used by default.
;       LEVEL:      in, required, type=string/strarr
;                   Data quality level. All levels are used by default.
;       MODE:       in, required, type=string/strarr
;                   Data telemetry mode. All modes are used by default
;       OPTDESC:    in, optional, type=string, default=''
;                   Optional descriptor.
;       URL_ROOT:   in, optional, type=string, default='https://lasp.colorado.edu/mms/sdc/public/data/'
;                   URL (host, scheme, path) of the data root. Cannot be used with `TEAM`.
;       SC:         in, required, type=string/strarr, default=all
;                   Spacecraft identifier. Options are 'mms1', 'mms2', 'mms3', and 'mms4'.
;                       All spacecraft are used by default.
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
FUNCTION MrMMS_Build_URL, sc, instr, mode, level, $
COUNT=count, $
OPTDESC=optdesc, $
TEAM=team, $
TSTART=tstart, $
URL_ROOT=url_root, $
VERSION=version
	Compile_Opt idl2
	On_Error, 2
	
	;Defaults
	IF N_Elements(tstart)   EQ 0 THEN tstart  = ''
	IF N_Elements(optdesc)  EQ 0 THEN optdesc = ''
	IF N_Elements(version)  EQ 0 THEN version = '*'
	IF N_Elements(url_root) EQ 0 THEN BEGIN
		IF Keyword_Set(team) $
			THEN url_root = 'https://lasp.colorado.edu/mms/sdc/team/' $
			ELSE url_root = 'https://lasp.colorado.edu/mms/sdc/public/data/'
	ENDIF ELSE BEGIN
		IF N_Elements(team) GT 0 THEN Message, 'TEAM and URL_ROOT are mutually exclusive.'
	ENDELSE

	;Number OF values given
	nSC      = N_Elements(sc)
	nInstr   = N_Elements(instr)
	nMode    = N_Elements(mode)
	nLevel   = N_Elements(level)
	nDesc    = N_Elements(optdesc)
	nTStart  = N_Elements(tstart)
	nVersion = N_Elements(version)
	
	;Conflicts
	IF nTStart  GT 1 THEN Message, 'TSTART must be scalar.'
	IF nVersion GT 1 THEN Message, 'VERSION must be scalar.'

;-----------------------------------------------------
; Non-Uniform Output \\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\
;-----------------------------------------------------
	;Allocate memory to output
	fname = StrArr(nSC*nInstr*nMode*nLevel*nDesc)
	count = 0

	;Create the MMS filename
	;   - Must use loops because:
	;   - Replicate does not work with arrays.
	;   - Rebin does not work with strings.
	FOR i = 0, nSC    - 1 DO $
	FOR j = 0, nInstr - 1 DO $
	FOR k = 0, nMode  - 1 DO $
	FOR l = 0, nLevel - 1 DO $
	FOR m = 0, nDesc  - 1 DO BEGIN
		
		;Filename
		fname[count] = MrMMS_Build_URL_Join(sc[i], instr[j], mode[k], level[l], optdesc[m], $
		                                    tstart, version)
		
		;Directory
		url_base = MrMMS_Build_URL_Dir(url_root, sc[i], instr[j], mode[k], level[l], optdesc[m], tstart)
		
		;Append the directory name
		fname[count] = url_base + '/' + fname[count]
		
		;Number OF files
		count += 1
	ENDFOR ;Loop over m
	
	;Return a scalar
	IF count EQ 1 THEN fname = fname[0]
	RETURN, fname
END