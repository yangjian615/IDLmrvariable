; docformat = 'rst'
;
; NAME:
;       MrMMS_Anc_Build_Path
;
; PURPOSE:
;+
;   The purpose of this program is to create a path for MMS ancillary files::
;
;       /root/sc/instr/
;
;   Calling Sequence:
;       PATHS = MrMMS_Anc_Build_Path(files)
;       PATHS = MrMMS_Anc_Build_Path(sc, instr)
;
; :Params:
;       SC:         in, required, type=string/strarr
;                   Spacecraft identifier or file names. Options for spacecraft IDs are 
;                       {'mms1' | 'mms2' | 'mms3' | 'mms4'}
;       INSTR:      in, optional, type=string/strarr
;                   Instrument identifier.
;
; :Keywords:
;       SDC_ROOT:   in, optional, type=string, default=
;                   Root of an SDC-like directory structure. The SDC directory chain
;                       will be prepended to the filename.
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
function MrMMS_Anc_Build_Path, sc, instr, $
COUNT=count, $
SDC_ROOT=sdc_root
	compile_opt strictarr
	on_error, 2
	
	;Defaults
	if n_elements(sdc_root)  eq 0 then cd, CURRENT=sdc_root
	
	;Number of values given
	nSC      = n_elements(sc)
	nInstr   = n_elements(instr)

;-----------------------------------------------------
; File Names \\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\
;-----------------------------------------------------
	if nInstr eq 0 then begin
		;Parse the file names
		MrMMS_Anc_Parse_Filename, sc, $
		                          SC      = sc_id, $
		                          INSTR   = instr, $
		                          TSTART  = tstart, $
		                          TEND    = tend
		
		;Convert to lowercase
		sc_id = strlowcase(sc_id)
		instr = strlowcase(instr)
		
		;Allocate memory
		nFiles = n_elements(sc)
		paths  = strarr(nFiles)
		
		;Loop through file names
		for i = 0, nFiles - 1 $
			do paths[i] = filepath('', ROOT_DIR=sdc_root, SUBDIRECTORY=['ancillary', sc_id[i], instr[i]])
		
		;Return
		return, paths
	endif

;-----------------------------------------------------
; Other Inputs \\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\
;-----------------------------------------------------
	
	;Allocate memory to output
	paths = strarr(nSC*nInstr)
	count = 0

	;Create the MMS filename
	;   - Must use loops because:
	;   - Replicate does not work with arrays.
	;   - Rebin does not work with strings.
	for i = 0, nSC    - 1 do $
	for j = 0, nInstr - 1 do begin
		;Build Path
		paths[count] = filepath('', ROOT_DIR=sdc_root, SUBDIRECTORY=['ancillary', sc[i], instr[i]])
		
		;Number of files
		count += 1
	endfor ;Loop over j
	
	;Return a scalar
	if count eq 1 then paths = paths[0]
	return, paths
end