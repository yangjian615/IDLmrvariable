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
;   Read CDF varialbes into MrVariables and cache the data.
;
; :Categories:
;       MMS, FGM
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
;-
;*****************************************************************************************
;+
;   FGM CDF files combine B and |B| into a 4D array. Split up the vector and magnitude.
;
; :Params:
;       B_NAME:         in, required, type=string
;                       Name of the magnetic field variable.
;-
pro MrMMS_FGM_Load_Data_SplitB, b_name
	compile_opt idl2
	on_error, 2

	;Create new names, adding "vec" and "mag"
	pos       = strpos(b_name, '_b_')
	bvec_name = strmid(b_name, 0, pos+2) + 'vec' + strmid(b_name, pos+2)
	bmag_name = strmid(b_name, 0, pos+2) + 'mag' + strmid(b_name, pos+2)

	;Grab the variable
	oB = MrVar_Get(b_name)
	
	;Separate magnitude from vector
	Bxyz  = MrVectorTS( oB[0:2,*], NAME=bvec_name, /CACHE )
	Bmag  = MrScalarTS( oB[3,*],   NAME=bmag_name, /CACHE )
	
	;Copy over all attributes
	oB -> CopyAttrTo, Bxyz
	oB -> CopyAttrTo, Bmag

	;BVEC - Set new attributes
	Bxyz -> AddAttr,      'COLOR',     ['blue', 'forest green', 'red']
	Bxyz -> AddAttr,      'DIMENSION', 1
	Bxyz -> SetAttrValue, 'LABEL',     ['Bx', 'By', 'Bz']
	Bxyz -> SetAttrValue, 'MIN_VALUE', min(Bxyz['MIN_VALUE'])
	Bxyz -> SetAttrValue, 'MAX_VALUE', max(Bxyz['MAX_VALUE'])
	Bxyz -> SetAttrValue, 'TITLE',     'B!C(nT)'
	
	;BMAG - Set new attributes
	Bmag -> AddAttr,      'COLOR',     'black'
	Bmag -> SetAttrValue, 'MIN_VALUE', 0
	Bmag -> SetAttrValue, 'MAX_VALUE', max(Bmag['MAX_VALUE'])
	Bmag -> SetAttrValue, 'TITLE',     '|B|!C(nT)'
	
	;Destroy the old variable
	MrVar_Delete, oB
end



;+
;   Find and read MMS FGM data.
;
;   The following quantities are split. When using VARFORMAT, the original
;   names must be provided.
;       b_bcs    ->    bvec_bcs  & bmag_bcs
;       b_dmpa   ->    bvec_dmap & bmag_dmpa
;       b_gse    ->    bvec_gse  & bmag_gse
;       b_gsm    ->    bvec_gsm  & bmag_gsm
;
;
; :Params:
;       SC:                 in, required, type=string/strarr
;                           The MMS spacecraft identifier. Options are:
;                               {'mms1' | 'mms2' | 'mms3' | 'mms4'}
;       MODE:               in, required, type=string/strarr
;                           Telemetry mode of the data. Options include:
;                               {'slow' | 'fast' | 'srvy' | 'brst'}
;
; :Keywords:
;       INSTR:              in, optional, type=string/strarr, default='dfg'
;                           Instrument ID for which data is read. Options are 
;                               {'fgm', 'dfg', 'afg'}
;       LEVEL:              in, optional, type=string/strarr, default='l2'
;                           Data quality level. Options include:
;                               {'l1a' | 'l1b' | 'l2pre' | 'l2'}
;       OPTDESC:            in, optional, type=string, default=''
;                           Optional descriptor of the data.
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
pro MrMMS_FGM_Load_Data, sc, mode, $
INSTR=instr, $
LEVEL=level, $
OPTDESC=optdesc, $
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
	if n_elements(instr) eq 0 then instr = 'fgm'
	if n_elements(level) eq 0 then level = 'l2'
	
	;Check spacecraft
	tf_instr = MrIsMember(['afg', 'dfg', 'fgm'], instr)
	if ~array_equal(tf_instr, 1) then message, 'Invalid value for INSTR: "' + instr + '".'
	
	;Get the data
	MrMMS_Load_Data, sc, instr, mode, level, $
	                 OPTDESC   = optdesc, $
	                 TEAM_SITE = team_site, $
	                 TRANGE    = trange, $
	                 VARFORMAT = varformat, $
	                 VARNAMES  = varnames

;-----------------------------------------------------
; Prettify Things \\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\
;-----------------------------------------------------

	;Build variable names
	for i = 0, n_elements(varnames) - 1 do begin
	
	;-----------------------------------------------------
	; Rename Epoch Variable \\\\\\\\\\\\\\\\\\\\\\\\\\\\\\
	;-----------------------------------------------------
		;
		; We are deleting original CDF data, which means that the next time
		; it is read, Epoch will be read as Epoch_1, etc., unless we rename
		; the epoch variable to something else.
		;

		;Get the variable
		;   - Epoch variables are being renamed, so it may no longer exist
		oVar = MrVar_Get(varnames[i], COUNT=count)
		if count eq 0 then continue
		
		;Change the DEPEND_0 attribute, if there is one
		if oVar -> HasAttr('DEPEND_0') then begin
			;Old epoch name
			epoch_name = oVar['DEPEND_0']
			
			;Do not rename the Epoch_state variable.
			if stregex(epoch_name, '^(Epoch|Epoch_[0-9]+)$', /BOOLEAN) then begin
				;Dissect the variable name
				parts    = strsplit(varnames[i], '_', /EXTRACT)
				new_name = strjoin( [parts[0:1], 'epoch', parts[-2:-1]], '_' )
			
				;Rename the epoch variable
				oVar -> SetAttrValue, 'DEPEND_0', new_name
				
				;If the epoch variable itself has not been renamed, rename it
				if MrVar_IsCached(epoch_name) then begin
					oEpoch  = MrVar_Get(epoch_name)
					oEpoch -> SetName, new_name
				endif
			endif
		endif
	
	;-----------------------------------------------------
	; Separate B and |B| \\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\
	;-----------------------------------------------------
		;Separate B from |B|
		if stregex(varnames[i], '(afg|dfg|fgm)_b_(gse|gsm|dmpa|bcs)', /BOOLEAN) $
			then MrMMS_FGM_Load_Data_SplitB, varnames[i]
	endfor
end
