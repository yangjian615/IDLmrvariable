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
;       /root/sc/instr/mode/level[/optdesc]/year/month[/day]
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
;       2015/03/31  -   Written by Matthew Argall
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
function mms_build_filename_dir, root, sc, instr, mode, level, optdesc, tstart
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

	;Append the SDC directory structure to the file name.
	dirname = filepath('', $
	                   ROOT_DIR     = root,      $
	                   SUBDIRECTORY = [ sc,      $
	                                    instr,   $
	                                    mode,    $
	                                    level,   $
	                                    optdesc, $
	                                    year,    $
	                                    month,   $
	                                    day      $
	                                  ]          $
	                  )
	
	return, dirname
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
function mms_build_filename_join, sc, instr, mode, level, optdesc, tstart, version
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
	fdesc = optdesc eq '' ? '' : optdesc + '_'

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
;       TSTART:     in, optional, type=string, default=''
;                   Start time of the data file to be created, formatted as an ISO-8601
;                       string: 'YYYY-MM-DDThh:mm:ssZ'. If the empty string is given,
;                       MrTokens will be used.
;       OPTDESC:    in, optional, type=string, default=''
;                   Optional descriptor.
;       DIRECTORY:  in, optional, type=string, default=''
;                   Directory to be prepended to the file name
;       SDC_ROOT:   in, optional, type=string, default=''
;                   Root of an SDC-like directory structure. The SDC directory chain
;                       will be prepended to the filename.
;       VERSION:    in, optional, type=string, default=''
;                   File version number.
;-
function MrMMS_Build_Filename, sc, instr, mode, level, $
COUNT=count, $
TSTART=tstart, $
OPTDESC=optdesc, $
DIRECTORY=directory, $
SDC_ROOT=sdc_root, $
VERSION=version
	compile_opt strictarr
	on_error, 2
	
	;Defaults
	if n_elements(sdc_root)  eq 0 then sdc_root  = ''
	if n_elements(directory) eq 0 then directory = ''
	if n_elements(tstart)    eq 0 then tstart    = ''
	if n_elements(optdesc)   eq 0 then optdesc   = ''
	if n_elements(version)   eq 0 then version   = '*'
	
	;Number of values given
	nSC      = n_elements(sc)
	nInstr   = n_elements(instr)
	nMode    = n_elements(mode)
	nLevel   = n_elements(level)
	nDesc    = n_elements(optdesc)
	nTStart  = n_elements(tstart)
	nVersion = n_elements(version)
	
	;Conflicts
	if sdc_root ne '' && directory ne '' then $
		message, 'SDC_ROOT and DIRECTORY are mutually exclusive.'
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
		fname[count] = mms_build_filename_join(sc[i], instr[j], mode[k], level[l], optdesc[m], $
		                                       tstart, version)
		
		;Directory
		if directory eq '' $
			then dir = mms_build_filename_dir(sdc_root, sc[i], instr[j], mode[k], level[l], optdesc[m], tstart) $
			else dir = directory

		;Append the directory name
		if dir ne '' then fname[count] = filepath(fname[count], ROOT_DIR=dir)
		
		;Number of files
		count += 1
	endfor ;Loop over m
	
	;Return a scalar
	if count eq 1 then fname = fname[0]
	return, fname
end