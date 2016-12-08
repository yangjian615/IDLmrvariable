; docformat = 'rst'
;
; NAME:
;       MrMMS_Anc_Build_Filename
;
; PURPOSE:
;+
;   The purpose of this program is to create an MMS CDF file name using the user
;   input information::
;
;       sc_instr_tstart_tend.version
;
;   With the option of appending an SDC-like directory structure, formatted as
;
;       /root/ancillary/sc/instr/
;
; :Categories:
;   MMS
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
;       2016/11/18  -   Written by Matthew Argall
;-
;*****************************************************************************************
;+
;   Create MMS file names
;
; :Params:
;       SC:         in, required, type=string/strarr
;                   Spacecraft identifier. Options are 'mms1', 'mms2', 'mms3', and 'mms4'
;       INSTR:      in, required, type=string/strarr
;                   Instrument identifier.
;
; :Keywords:
;       TSTART:     in, optional, type=string, default=''
;                   Start time of the data file to be created, formatted as an ISO-8601
;                       string: 'YYYY-MM-DDThh:mm:ssZ'. If the empty string is given,
;                       MrTokens will be used.
;       TEND:       in, optional, type=string, default=''
;                   End time of the data file to be created, formatted as an ISO-8601
;                       string: 'YYYY-MM-DDThh:mm:ssZ'. If the empty string is given,
;                       MrTokens will be used.
;       DIRECTORY:  in, optional, type=string, default=''
;                   Directory to be prepended to the file name
;       SDC_ROOT:   in, optional, type=string, default=''
;                   Root of an SDC-like directory structure. The SDC directory chain
;                       will be prepended to the filename.
;       VERSION:    in, optional, type=string, default=''
;                   File version number.
;-
function MrMMS_Anc_Build_Filename, sc, instr, $
COUNT=count, $
TSTART=tstart, $
TEND=tend, $
DIRECTORY=directory, $
SDC_ROOT=sdc_root, $
VERSION=version
	compile_opt strictarr
	on_error, 2
	
	;Defaults
	if n_elements(sdc_root)  eq 0 then sdc_root  = ''
	if n_elements(directory) eq 0 then directory = ''
	if n_elements(tstart)    eq 0 then tstart    = '%Y%D'
	if n_elements(tend)      eq 0 then tend      = '%Y%D'
	if n_elements(version)   eq 0 then version   = '*'
	
	;Number of values given
	nSC      = n_elements(sc)
	nInstr   = n_elements(instr)
	nTStart  = n_elements(tstart)
	nTEnd    = n_elements(tend)
	nVersion = n_elements(version)
	
	;Conflicts
	if sdc_root ne '' && directory ne '' then $
		message, 'SDC_ROOT and DIRECTORY are mutually exclusive.'
	if nTStart  gt 1 then message, 'TSTART must be scalar.'
	if nTEnd    gt 1 then message, 'TEND must be scalar.'
	if nVersion gt 1 then message, 'VERSION must be scalar.'

;-----------------------------------------------------
; Non-Uniform Output \\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\
;-----------------------------------------------------
	;Allocate memory to output
	fname = strarr(nSC*nInstr)
	count = 0
	
	;Lower case
	loSC    = strlowcase(sc)
	loInstr = strlowcase(instr)
	
	;Upper case
	upSC    = strupcase(sc)
	upInstr = strupcase(instr)

	;Create the MMS filename
	;   - Must use loops because:
	;   - Replicate does not work with arrays.
	;   - Rebin does not work with strings.
	for i = 0, nSC    - 1 do $
	for j = 0, nInstr - 1 do begin
		
		;Filename
		fname[count] = strjoin([upSC[i], upInstr[i], tstart, tend, 'V' + version], '_')
		
		;Directory
		if directory eq '' $
			then dir = filepath('', ROOT_DIR='ancillary', SUBDIRECTORY=[loSC[i], loInstr[j]]) $
			else dir = directory

		;Append the directory name
		if dir ne '' then fname[count] = filepath(fname[count], ROOT_DIR=dir)
		
		;Number of files
		count += 1
	endfor ;Loop over j
	
	;Return a scalar
	if count eq 1 then fname = fname[0]
	return, fname
end