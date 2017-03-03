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
;       2017/02/02  -   Accept file names as input. Accepts dates with and without
;                           delimiters. - MRA
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
FUNCTION MrMMS_Build_Filename_Dir, root, sc, instr, mode, level, optdesc, tstart
	Compile_Opt idl2
	On_Error, 2

	;Use tokens IF the start time was not given
	IF tstart EQ '' THEN BEGIN
		year  = '%Y'
		month = '%M'
		day   = mode EQ 'brst' ? '%d' : ''
	ENDIF ELSE BEGIN
		;YYYY-MM-DD vs YYYYMMDD
		tlen = StrLen(tstart)
		IF tlen EQ 8 || tlen EQ 14 THEN BEGIN
			year  = StrMid(tstart, 0, 4) 
			month = StrMid(tstart, 4, 2)
			day   = mode EQ 'brst' ? StrMid(tstart, 6, 2) : ''
		ENDIF ELSE BEGIN
			year  = StrMid(tstart, 0, 4) 
			month = StrMid(tstart, 5, 2)
			day   = mode EQ 'brst' ? StrMid(tstart, 8, 2) : ''
		ENDELSE
	ENDELSE

	;Append the SDC directory structure to the file name.
	dirname = FilePath('', $
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
	
	RETURN, dirname
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
FUNCTION MrMMS_Build_Filename_Join, sc, instr, mode, level, optdesc, tstart, version
	Compile_Opt idl2
	On_Error, 2

	;Use tokens if the start time was not given
	IF tstart EQ '' THEN BEGIN
		IF mode EQ 'brst' THEN BEGIN
			fstart = '%Y%M%d%H%m%S'
		ENDIF ELSE BEGIN
			CASE instr OF
				'fpi': fstart = '%Y%M%d%H%m%S'
				'fsm': BEGIN
					IF optdesc NE '' $
						THEN fstart = '%Y%M%d%H%m%S' $
						ELSE fstart = '%Y%M%d'
				ENDCASE
				'edp': BEGIN
					IF optdesc EQ 'dce' $
						THEN fstart = (level EQ 'l2') ? '%Y%M%d' : '%Y%M%d%H%m%S' $
						ELSE fstart = '%Y%M%d%H%m%S'
				ENDCASE
				ELSE: fstart = '%Y%M%d'
			ENDCASE
		ENDELSE

	ENDIF ELSE BEGIN
		;YYYY-MM-DD vs YYYYMMDD
		tlen = StrLen(tstart)
		IF tlen EQ 8 || tlen EQ 14 $
			THEN fstart = StrMid(tstart, 0, 4) + StrMid(tstart, 4, 2) + StrMid(tstart, 6, 2) $
			ELSE fstart = StrMid(tstart, 0, 4) + StrMid(tstart, 5, 2) + StrMid(tstart, 8, 2)
		
		;Burst mode has HHMMSS
		IF mode EQ 'brst' THEN BEGIN
			IF tlen EQ 8 || tlen EQ 14 $
				THEN fstart += StrMid(tstart,  8, 2) + StrMid(tstart, 10, 2) + StrMid(tstart, 12, 2) $
				ELSE fstart += StrMid(tstart, 11, 2) + StrMid(tstart, 14, 2) + StrMid(tstart, 17, 2)
		
		;Some slow/fast/survey files have HHMMSS as well
		ENDIF ELSE IF tlen EQ 14 THEN BEGIN
			fstart += StrMid(tstart,  8, 2) + StrMid(tstart, 10, 2) + StrMid(tstart, 12, 2)
		ENDIF
	ENDELSE

	;Separate an optional descriptor form the start time.
	fdesc = optdesc EQ '' ? '' : optdesc + '_'

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
;   Calling Sequence:
;       fnames = MrMMS_Build_Filename(files)
;       fnames = MrMMS_Build_Filename(sc, instr, mode, level)
;
; :Params:
;       SC:         in, required, type=string/strarr
;                   Spacecraft identifier or (an array of) file names. If the former,
;                       valid options are 'mms1', 'mms2', 'mms3', and 'mms4'. If the latter,
;                       file names are parsed for their components, then built back up
;                       again. This is especially useful if `SDC_ROOT` is provided.
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
FUNCTION MrMMS_Build_Filename, sc, instr, mode, level, $
COUNT=count, $
TSTART=tstart, $
OPTDESC=optdesc, $
DIRECTORY=directory, $
SDC_ROOT=sdc_root, $
VERSION=version
	Compile_Opt idl2
	On_Error, 2
	
	;Defaults
	IF N_Elements(sdc_root)  EQ 0 THEN sdc_root  = ''
	IF N_Elements(directory) EQ 0 THEN directory = ''
	IF N_Elements(tstart)    EQ 0 THEN tstart    = ''
	IF N_Elements(optdesc)   EQ 0 THEN optdesc   = ''
	IF N_Elements(version)   EQ 0 THEN version   = '*'
	
	;Number OF values given
	nSC      = N_Elements(sc)
	nInstr   = N_Elements(instr)
	nMode    = N_Elements(mode)
	nLevel   = N_Elements(level)
	nDesc    = N_Elements(optdesc)
	nTStart  = N_Elements(tstart)
	nVersion = N_Elements(version)
	
	;Conflicts
	IF sdc_root NE '' && directory NE '' THEN $
		Message, 'SDC_ROOT and DIRECTORY are mutually exclusive.'
	IF nTStart  GT 1 THEN Message, 'TSTART must be scalar.'
	IF nVersion GT 1 THEN Message, 'VERSION must be scalar.'

;-----------------------------------------------------
; File Names \\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\
;-----------------------------------------------------
	IF nInstr EQ 0 THEN BEGIN
		;Parse the file names
		MrMMS_Parse_Filename, sc, $
		                      SC      = sc_id, $
		                      INSTR   = instr, $
		                      MODE    = mode, $
		                      LEVEL   = level, $
		                      OPTDESC = optdesc, $
		                      TSTART  = tstart, $
		                      VERSION = version

		;Allocate memory
		nFiles = N_Elements(sc)
		fname  = StrArr(nFiles)
		
		;Loop through file names
		FOR i = 0, nFiles - 1 DO BEGIN
			;Filename
			fname[i] = MrMMS_Build_Filename_Join(sc_id[i], instr[i], mode[i], level[i], optdesc[i], tstart[i], version[i])
			
			;Path
			IF directory EQ '' $
				THEN dir = MrMMS_Build_Filename_Dir(sdc_root, sc_id[i], instr[i], mode[i], level[i], optdesc[i], tstart[i]) $
				ELSE dir = directory
			
			;Append path to file name
			IF dir NE '' THEN fname[i] = FilePath(fname[i], ROOT_DIR=dir)
		ENDFOR
		
		;Return
		RETURN, fname
	ENDIF

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
		fname[count] = MrMMS_Build_Filename_Join(sc[i], instr[j], mode[k], level[l], optdesc[m], $
		                                         tstart, version)
		
		;Directory
		IF directory EQ '' $
			THEN dir = MrMMS_Build_Filename_Dir(sdc_root, sc[i], instr[j], mode[k], level[l], optdesc[m], tstart) $
			ELSE dir = directory

		;Append the directory name
		IF dir NE '' THEN fname[count] = FilePath(fname[count], ROOT_DIR=dir)
		
		;Number OF files
		count += 1
	ENDFOR ;Loop over m
	
	;Return a scalar
	IF count EQ 1 THEN fname = fname[0]
	RETURN, fname
END