; docformat = 'rst'
;
; NAME:
;       MrMMS_Load_Data
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
;   Read CDF varialbes into MrVariables and cache the data.
;
; :Categories:
;       MrVariable, MMS, CDF
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
;       2016/08/16  -   Written by Matthew Argall
;       2016/09/02  -   Rename "Epoch" variables to follow the MMS variable naming
;                           convention. - MRA
;-
;*****************************************************************************************
;+
;   Provide unique variable names to CDF Epoch variables.
;
;   CDF data variables are saved to the cache with their original
;   names. Many have a DEPEND_0 variable that is support data, e.g.
;   Epoch. If the original data variable is removed from the cache,
;   there will no longer be a connection between the CDF and the
;   cache. The next time the CDF is read, Epoch will be read as a
;   new variable "Epoch_1".
;
;   To prevent duplicate data from being stored in the cache, we
;   must rename the Epoch variable before deleting data variables.
;
; :Private:
;
; :Params:
;       VARNAMES:       in, required, type=string/strarr
;                       Names of CDF variables for which the DEPEND_0 attribute
;                           is to be renamed.
;-
pro MrMMS_Load_Data_RenameEpoch, varnames
	compile_opt idl2
	on_error, 2

	;Build variable names
	for i = 0, n_elements(varnames) - 1 do begin
		;Get the variable
		;   - Epoch variables are being renamed, so it may no longer exist
		oVar = MrVar_Get(varnames[i], COUNT=count)
		if count eq 0 then continue
		
		;Change the DEPEND_0 attribute, if there is one
		if oVar -> HasAttr('DEPEND_0') then begin
			;Old epoch name
			epoch_name = oVar['DEPEND_0']
			
			;Rename only the "Epoch" variables.
			;   - Allow "_#" in case /NO_CLOBBER caused Epoch to be renamed.
			if stregex(epoch_name, '^(Epoch|Epoch_[0-9]+)$', /BOOLEAN) then begin
				;Dissect the variable name
				parts    = strsplit(varnames[i], '_', /EXTRACT)
				
				;Create a new name
				;   - Variable names are suppose to follow the format
				;       sc_instr_param_[coordsys_][optdesc_]mode_level
				;   - We want to make "Epoch" look like
				;       sc_instr_epoch_mode_level
				;   - But some instruments do not follow the veriable naming
				;     convention exactly.
				;   - Here, we follow the naming convention of the instrument
				if stregex(varnames[i], '(des|dis)', /BOOLEAN) $
					then new_name = strjoin( [parts[0:1], 'epoch', parts[-1]], '_' ) $
					else new_name = strjoin( [parts[0:1], 'epoch', parts[-2:-1]], '_' )
			
				;Rename the epoch variable
				oVar -> SetAttrValue, 'DEPEND_0', new_name
				
				;If the epoch variable itself has not been renamed, rename it
				if MrVar_IsCached(epoch_name) then begin
					oEpoch  = MrVar_Get(epoch_name)
					oEpoch -> SetName, new_name
				endif
			endif
		endif
	endfor
end


;+
; :Params:
;       SC:                 in, required, type=string/strarr
;                           The MMS spacecraft identifier. Options are:
;                               {'mms1' | 'mms2' | 'mms3' | 'mms4'}
;       INSTR:              in, required, type=string/strarr
;                           Instrument ID for which data is read.
;       MODE:               in, required, type=string/strarr
;                           Telemetry mode of the data. Options include:
;                               {'slow' | 'fast' | 'srvy' | 'brst'}
;       LEVEL:              in, required, type=string/strarr
;                           Data quality level. Options include:
;                               {'l1a' | 'l1b' | 'l2pre' | 'l2'}
;
; :Keywords:
;       OPTDESC:            in, optional, type=string, default=''
;                           Optional descriptor of the data.
;       SUPPORT_DATA:       in, optional, type=boolean, default=0
;                           If set, support data will be read as well. Regardless of
;                               the status of this keyword, variable data associated
;                               with DEPEND_# variables will be read. This keyword
;                               is ignored if `VARFORMAT` is set.
;       TEAM_SITE:          in, optional, type=boolean, default=0
;                           If set, requests will be sent to the team site (password
;                               required, L1A and above). Automatically set if `LEVEL`
;                               is below level 2. This option is sticky.
;       TRANGE:             out, optional, type=string, default=MrVar_GetTRange()
;                           Start and end times over which to read data.
;       VARFORMAT:          out, optional, type=string, default='*'
;                           Variables that match this search pattern will be read,
;                               others are ignored.
;-
pro MrMMS_Load_Data, sc, instr, mode, level, $
OPTDESC=optdesc, $
SUPPORT_DATA=support_data, $
TEAM_SITE=team_site, $
TRANGE=trange, $
VARFORMAT=varformat, $
VARNAMES=varnames
	compile_opt idl2

	catch, the_error
	if the_error ne 0 then begin
		catch, /CANCEL
		MrPrintF, 'LogErr'
		return
	endif
	
	;Defaults
	if n_elements(trange) eq 0 then trange = MrVar_GetTRange()
	
	;Check spacecraft
	tf_sc    = MrIsMember('mms' + ['1', '2', '3', '4'], sc)
	tf_mode  = MrIsMember(['slow', 'fast', 'srvy', 'brst'], mode)
	tf_level = MrIsMember(['l1a', 'l1b', 'l2pre', 'l2', 'l2plus'], level)
	tf_instr = MrIsMember(['afg', 'aspoc', 'dfg', 'dsp', 'edi', 'edp', 'epd-eis', 'feeps', 'fgm', 'fpi', 'hpca', 'mec', 'scm'], instr)
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
	              SUCCESS     = success, $
	              SC_ID       = sc, $
	              INSTR       = instr, $
	              MODE        = mode, $
	              LEVEL       = level, $
	              OPTDESC     = optdesc, $
	              PUBLIC_SITE = public_site, $
	              TEAM_SITE   = team_site, $
	              TSTART      = trange[0], $
	              TEND        = trange[1]
	if success eq 0 then return
	
	;Attempt to get the data
	files = !MrMMS -> Get(COUNT=count)
	if count eq 0 then return

;-----------------------------------------------------
; Read Files \\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\
;-----------------------------------------------------
	;Parse the file names
	MrMMS_Parse_Filename, files, SC=fsc, INSTR=finstr, MODE=fmode, LEVEL=flevel, OPTDESC=foptdesc
	
	;Determine file types
	fType = fsc + '_' + finstr + '_' + fmode + '_' + flevel
	iOptDesc = where(foptdesc ne '', nOptDesc)
	if nOptDesc gt 0 then fType[iOptDesc] += '_' + foptdesc[iOptDesc]
	
	;Pick unique file types
	fbase = file_basename(files)
	iUniq = uniq(fType, sort(fType))
	nUniq = n_elements(iUniq)

	;Step through each file type
	for i = 0, nUniq - 1 do begin
		;Find similar file types
		iFiles = where(strmid( fbase, 0, strlen(fType[i])) eq fTYpe[i], nFiles)
		if nFiles eq 0 then continue
		
		;
		; TODO: Sort files by time (and slow/fast srvy)
		;
	
		;Read files
		MrVar_ReadCDF, files[iFiles], $
		               SUPPORT_DATA = support_data, $
		               TRANGE       = trange, $
		               VARFORMAT    = varformat, $
		               VARNAMES     = varnames, $
		               VERBOSE      = !MrMMS.verbose
	endfor

;-----------------------------------------------------
; Rename the Epoch Variables \\\\\\\\\\\\\\\\\\\\\\\\\
;-----------------------------------------------------
	MrMMS_Load_Data_RenameEpoch, varnames
end
