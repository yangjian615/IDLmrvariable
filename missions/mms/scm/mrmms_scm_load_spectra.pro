; docformat = 'rst'
;
; NAME:
;       MrMMS_Get_FPI_Data
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
;   Read data from the Fast Plasma Instrument on MMS.
;
; TIME TAGS:
;   FPI time tags are shifted to the center of the sampling interval to match
;   the rest of the MMS data.
;
; MOMENTS FILES:
;   The following quantities are combined. When using VARFORMAT, the original
;   names must be provided.
;       bulkx                   heatx
;       bulky  -->  bulkv       heaty  -->  heat
;       bulkz                   heatz
;
;       presxx                  tempxx
;       presxy                  tempxy
;       presxz  --> pres        tempxz  -->  temp
;       presyy                  tempyy
;       presyz                  tempyz
;       preszz                  tempzz
;
; DIST FILES:
;   Energy tables are converted to an Nx32 time-series array that combines the
;   information in the two energy tables and the parity table. Also, an energy
;   table that is the geometric mean of the to original tables is created. The
;   variable attributes of the distribution are updated to be physical arrays
;   instead of indices. When reading distributions, either use the /SUPPORT_DATA
;   keyword or 
;       VARFORMAT=['*dist_*', '*energy0*', '*energy1*', '*phi*', '*theta*', '*steptable*']
;
; :Categories:
;       CDF Utilities
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
;       2016/08/20  -   Written by Matthew Argall
;-
;*****************************************************************************************
;+
;   Compute spectra
;
; :Params:
;       NAME:           in, required, type=strarr(3)
;                       Variable name of the three-component magnetic field.
;-
pro MrMMS_SCM_Load_Spectra_Compute, name, nfft, nshift
	compile_opt idl2
	on_error, 2

	;Get components
	oB    = MrVar_Get(name)
	oT    = MrVar_Get(oB['DEPEND_0'])
	t_ssm = oT -> GetData('SSM')

	;Compute the power spectral density of each component
	psd = MrPSD2( oB['DATA'], nfft, nshift, $
	              FREQUENCIES = f, $
	              DIMENSION   = 1, $
	              DT          = t_ssm, $
	              T0          = t_ssm[0], $
	              TIME        = t_psd, $
	              FILLVAL     = oB -> GetAttrValue('FILLVAL', /NULL) )

	;Create new names
	parts   = strsplit(name, '_', /EXTRACT)
	f_name  = strjoin( [parts[0:1], 'fpsd',   parts[-3:-1]], '_' )
	t_name  = strjoin( [parts[0:1], 'tpsd',   parts[-3:-1]], '_' )
	bx_name = strjoin( [parts[0:1], 'bxpsd', parts[-3:-1]], '_' )
	by_name = strjoin( [parts[0:1], 'bypsd', parts[-3:-1]], '_' )
	bz_name = strjoin( [parts[0:1], 'bzpsd', parts[-3:-1]], '_' )
	
	;
	;Create variables
	;

	;TIME
	oTime = MrTimeVar( MrCDF_ssm2epoch(t_psd, oT[0, 'TT2000']), 'TT2000', NAME=t_name, /CACHE )
	
	;FREQUENCY
	oFreq = MrVariable(f, NAME=f_name, /NO_COPY, /CACHE)
	oFreq -> AddAttr, 'LOG',   1
	oFreq -> AddAttr, 'TITLE', 'f!C(Hz)'
	oFreq -> AddAttr, 'UNITS', 'Hz'
	
	;Bx
	oBx = MrVariable( psd[*,*,0], NAME=bx_name, /CACHE )
	oBx -> AddAttr, 'DEPEND_0', t_name
	oBx -> AddAttr, 'DEPEND_1', f_name
	oBx -> AddAttr, 'LOG',      1
	oBx -> AddAttr, 'SCALE',    1
	oBx -> AddAttr, 'TITLE',    'Bx PSD!C(nT^2/Hz)'
	oBx -> AddAttr, 'UNITS',    'nT^2/Hz'
	
	;By
	oBy = MrVariable( psd[*,*,1], NAME=by_name, /CACHE )
	oBy -> AddAttr, 'DEPEND_0', t_name
	oBy -> AddAttr, 'DEPEND_1', f_name
	oBy -> AddAttr, 'LOG',      1
	oBy -> AddAttr, 'SCALE',    1
	oBy -> AddAttr, 'TITLE',    'By PSD!C(nT^2/Hz)'
	oBy -> AddAttr, 'UNITS',    'nT^2/Hz'
	
	;Bz
	oBz = MrVariable( psd[*,*,2], NAME=bz_name, /CACHE )
	oBz -> AddAttr, 'DEPEND_0', t_name
	oBz -> AddAttr, 'DEPEND_1', f_name
	oBz -> AddAttr, 'LOG',      1
	oBz -> AddAttr, 'SCALE',    1
	oBz -> AddAttr, 'TITLE',    'Bz PSD!C(nT^2/Hz)'
	oBz -> AddAttr, 'UNITS',    'nT^2/Hz'
end


;+
;   Find and read MMS FPI data.
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
;       LEVEL:              in, optional, type=string/strarr, default='l2'
;                           Data quality level. Options include:
;                               {'l1a' | 'l1b' | 'l2pre' | 'l2'}
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
pro MrMMS_SCM_Load_Spectra, sc, mode, nfft, nshift, $
LEVEL=level, $
OPTDESC=optdesc, $
TEAM_SITE=team_site, $
TRANGE=trange, $
VARNAMES=varnames
	compile_opt idl2

	catch, the_error
	if the_error ne 0 then begin
		catch, /CANCEL
		MrPrintF, 'LogErr'
		return
	endif
	
	;Constants and defaults
	instr = 'scm'
	if n_elements(level)   eq 0 then level   = 'l2'
	if n_elements(optdesc) eq 0 then begin
		case mode of
			'brst': optdesc = 'scb'
			'srvy': optdesc = 'scsrvy'
			'fast': optdesc = 'scf'
			'slow': optdesc = 'scs'
			else:   message, 'LEVEL not recognized: "' + level + '".'
		endcase
	endif

;-----------------------------------------------------
; Load the Data \\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\
;-----------------------------------------------------
	;Get the FPI distribution function
	;   - TODO: Loop over MODE and OPTDESC to create VARFORMAT filter.
	MrMMS_Load_Data, sc, instr, mode, level, $
	                 OPTDESC   = optdesc, $
	                 VARFORMAT = '*acb_*_' + optdesc + '_' + mode + '_' + level, $
	                 VARNAMES  = varnames

;-----------------------------------------------------
; Rename DEPEND_0 \\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\
;-----------------------------------------------------

	;Build variable names
	for i = 0, n_elements(varnames) - 1 do begin
	
	;-----------------------------------------------------
	; Rename Epoch Variable \\\\\\\\\\\\\\\\\\\\\\\\\\\\\\
	;-----------------------------------------------------
		;
		; CDF data variables are saved to the cache with their original
		; names. Many have a DEPEND_0 variable that is support data, e.g.
		; Epoch. If the original data variable is removed from the cache,
		; there will no longer be a connection between the CDF and the
		; cache. The next time the CDF is read, Epoch will be read as a
		; new variable "Epoch_1".
		;
		; To prevent duplicate data from being stored in the cache, we
		; must rename the Epoch variable before deleting data variables.
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
				new_name = strjoin( [parts[0:1], 'epoch', parts[-3:-1]], '_' )
			
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
	; Distribution \\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\
	;-----------------------------------------------------
		;Calculate spectra
		if stregex(varnames[i], '_acb_.*_(scs|scf|scsrvy|scb)_(slow|fast|srvy|brst)_(l1a|l1b|l2)$', /BOOLEAN) $ begin
			then MrMMS_SCM_Load_Spectra_Compute, varnames[i], nfft, nshift
	endfor
end