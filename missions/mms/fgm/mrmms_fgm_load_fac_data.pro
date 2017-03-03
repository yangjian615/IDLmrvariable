; docformat = 'rst'
;
; NAME:
;       MrMMS_FGM_Get_TFAC
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
;   Load data required to create a transformation to a field-aligned coordinate system.
;   Data rates and modes for each instrument are taken into account.
;
; :Categories:
;       MMS, FGM, MrVariable
;
; :Params:
;       SC:                 in, required, type=string/strarr
;                           The MMS spacecraft identifier. Options are:
;                               {'mms1' | 'mms2' | 'mms3' | 'mms4'}
;       MODE:               in, required, type=string/strarr
;                           Telemetry mode of the data. Options include:
;                               {'slow' | 'fast' | 'srvy' | 'brst'}
;       FAC:                in, optional, type=string, default=''
;                           The field-aligned coordinate system to create. Magnetic field
;                               data is loaded for any option. If a perpendicular vector
;                               is also loaded, it is interpolated onto the time stamps of
;                               the magnetic field. Choices are:
;                                   'EXB'       -  Loads electric field data
;                                   'VXB'       -  Loads bulk velocity data
;                                   [anything]  -  No perpendicular vector is loaded.
;       VAR:                in, optional, type=string/objref
;                           A MrVariable name or object for which the time tags define
;                               the grid onto which the transformation matrix is
;                               interpolated. If not present, then the magnetic field
;                               is interpolated to the time of whichever variable defines
;                               the perpendicular direction, as per `FAC`.
;
; :Keywords:
;       COORD_SYS:          in, optional, type=string, default='GSE'
;                           Original coordinate system.
;       INSTR:              in, optional, type=string, default='dfg'
;                           The fluxgate instrument to use. The default is 'DFG' for
;                               `MODE`='srvy' and 'FGM' for `MODE`='brst'.
;       LEVEL:              in, optional, type=float, default='L2'
;                           Data quality level to use.
;       NO_LOAD:            in, optional, type=boolean, default=0
;                           Set if the data that define the field-aligned coordinate
;                               system has already been loaded into the variable cache.
;                               Data will not be re-loaded but will be re-interpolated,
;                               notably if `VAR` is given.
;       TRANGE:             in, optional, type=strarr(2), default=MrVar_GetTRange()
;                           Time interval over which to create the transformation.
;       SUFFIX:             in, optional, type=string, default=''
;                           A suffix to be appended to variable names.
;       VARNAMES:           out, optional, type=string/strarr(2)
;                           Names of the variables that define the field-aligned coordinate
;                               system. The first element will be the name of the magnetic
;                               field vector while the second element will be the name
;                               of the vector that defines the perpendicular direction,
;                               if one is implied by `FAC`.
;
; :See Also:
;    MrMMS_FGM_Load_Data.pro
;    MrMMS_FPI_Load_Data.pro
;    MrMMS_Load_Data.pro
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
;       2016/10/07  -   Written by Matthew Argall
;       2017/01/05  -   Allow FAC to be anything.
;-
pro MrMMS_FGM_Load_FAC_Data, sc, mode, fac, var, $
COORD_SYS=coord_sys, $
INSTR=instr, $
LEVEL=level, $
NO_LOAD=no_load, $
SUFFIX=suffix, $
TRANGE=trange, $
VARNAMES=varnames
	compile_opt idl2

	catch, the_error
	if the_error ne 0 then begin
		catch, /CANCEL
		MrPrintF, 'LogErr'
		return
	endif
	
	;Defaults
	tf_load = ~keyword_set(no_load)
	if n_elements(trange)    gt 0 then MrVar_SetTRange, trange
	if n_elements(coord_sys) eq 0 then coord_sys = 'gse'
	if n_elements(fac)       eq 0 then fac    = ''
	if n_elements(instr)     eq 0 then instr  = 'fgm'
	if n_elements(level)     eq 0 then level  = 'l2'
	if n_elements(suffix)    eq 0 then suffix = ''
	
	;Variable type
	type = level eq 'l2' ? 'flux' : 'counts'
	_fac = strupcase(fac)
	_cs  = strlowcase(coord_sys)

;-------------------------------------------
; Sampling Rates ///////////////////////////
;-------------------------------------------

	;--------------------------------|
	; instr    | slow | fast |  brst |
	;---------------------------------
	; afg      |   4  |   8  |    64 |
	; dfg      |   8  |  16  |   128 |
	; fgm      |   4  |   8  |   128 |
	; scm      |  32  |  32  |  4096 |
	; edp      |  10  |  32  |  4096 |
	; edi      |  32  |  32  |  1024 |
	; fpi      |  --  |  0.2 | 31.25 |
	;--------------------------------|
	
	;
	;We want to pair like-sample rates
	;   BRST:
	;       fgm_brst
	;       edp_fast
	;       fpi_brst
	;
	
	if mode eq 'brst' then begin
		fgm_mode  = 'brst'
		fgm_instr = 'fgm'
		fgm_level = 'l2'
		fpi_mode  = 'brst'
		edp_mode  = 'fast'
	endif else begin
		if _fac eq 'VXB' then message, 'FAC VxB only available in brst mode.'
		MrPrintF, 'LogWarn', 'TODO: Combine slow and fast survey data. Using fast.'
		fgm_mode  = 'srvy'
		fgm_instr = 'dfg'
		fgm_level = 'l2pre'
		edp_mode  = 'fast'
	endelse
	
	;Coordinate system
	fgm_coords = MrIsMember(['dbcs', 'dsl'],  _cs) ? 'dmpa' : _cs
	fpi_coords = MrIsMember(['dmpa', 'dsl'],  _cs) ? 'dbcs' : _cs
	edp_coords = MrIsMember(['dbcs', 'dmpa'], _cs) ? 'dsl'  : _cs
	
;-------------------------------------------
; Load Data ////////////////////////////////
;-------------------------------------------
	if tf_load then begin
	
	;-------------------------------------------
	; FGM //////////////////////////////////////
	;-------------------------------------------
		MrMMS_FGM_Load_Data, sc, fgm_mode, $
		                     LEVEL     = fgm_level, $
		                     INSTR     = fgm_instr, $
		                     SUFFIX    = suffix, $
		                     VARFORMAT = ['*b_' + fgm_coords + '*', '*fg*' + fgm_coords]

	;-------------------------------------------
	; FPI //////////////////////////////////////
	;-------------------------------------------
		if _fac eq 'VXB' then begin
			;Reminder warning
			;   - GSE is not available yet
			;   - FPI does not operate in slow survey, so there is no "srvy" product.
			if _cs eq 'gse' then begin
				MrPrintF, 'LogWarn', 'FPI data is not available in GSE. Using DBCS.'
				fpi_cs = 'dbcs'
			endif
			
			;Load the data
			MrMMS_FPI_Load_Data, sc, fpi_mode, $
			                     LEVEL     = level, $
			                     OPTDESC   = 'des-moms', $
			                     SUFFIX    = suffix, $
			                     VARFORMAT = '*bulk?_' + fpi_cs + '*'
	
	;-------------------------------------------
	; EDP //////////////////////////////////////
	;-------------------------------------------
		endif else if _fac eq 'EXB' then begin
			;EDP does not make a srvy product; slow and fast are kept separate.
			if mode eq 'srvy' then MrPrintF, 'LogWarn', 'TODO: Combine slow and fast survey data.'
			edp_mode = 'fast'

			;Load the data
			MrMMS_Load_Data, sc, 'edp', edp_mode, level, $
			                 OPTDESC   = 'dce', $
			                 SUFFIX    = suffix, $
			                 VARFORMAT = '*dce_' + edp_coords + '*'
		endif
	endif

;-------------------------------------------
; Get the Data /////////////////////////////
;-------------------------------------------
	;Get the magnetic field vector
	b_vname = fgm_level eq 'l2' $
	              ? strjoin( [sc, fgm_instr, 'bvec', fgm_coords, fgm_mode, fgm_level], '_' ) + suffix $
	              : strjoin( [sc, fgm_instr, 'vec', fgm_mode, fgm_level, fgm_coords], '_' ) + suffix
	oB      = MrVar_Get(b_vname)

	;Vector that defines the perpendicular direction
	case _fac of
		'VXB': oPerp = MrVar_Get( strjoin( [sc, 'des_bulkv', fpi_cs, fpi_mode], '_' ) + suffix )
		'EXB': oPerp = MrVar_Get( strjoin( [sc, 'edp_dce',      _cs, edp_mode, level], '_' ) + suffix )
		else:  ;Do nothing
	endcase

;-------------------------------------------
; Output Variable Names ////////////////////
;-------------------------------------------
	
	;Output names
	bname_out = strjoin( [sc, fgm_instr, 'bvec', fgm_mode, fgm_level, 'facdata'], '_' ) + suffix
	case _fac of
		'VXB': perp_name_out = strjoin( [sc, 'des_bulkv', fpi_cs, fpi_mode,        'facdata'], '_' ) + suffix
		'EXB': perp_name_out = strjoin( [sc, 'edp_dce',      _cs, edp_mode, level, 'facdata'], '_' ) + suffix
		else:  ;Do nothing
	endcase

;-------------------------------------------
; Inteprolate Data /////////////////////////
;-------------------------------------------
	;Specific time cadence desired
	if n_elements(var) gt 0 then begin
		;Get the variable and its time tags
		oVar = MrVar_Get(var)
		if obj_isa(oVar, 'MrTimeVar') $
			then depend_0 = oVar $
			else depend_0 = oVar['DEPEND_0']

		;Interpolate magnetic field
		oB_fac = oB -> Interpol(oVar)
		oB_fac -> SetName, bname_out
		oB_fac -> Cache
		
		;Interpolate perpendicular vector
		if n_elements(oPerp) gt 0 then begin
			oV_fac   = oPerp -> Interpol(oVar)
			oV_fac  -> SetName, perp_name_out
			oV_fac  -> Cache
		endif
	
	;Common time base for FAC
	endif else begin
		;Perpendicular vector is defined.
		if n_elements(oPerp) gt 0 then begin
			;Interpolate the magnetic field
			oB_fac  = oB -> Interpol(oPerp)
			oB_fac -> SetName, bname_out
			oB_fac -> Cache
			
			;New copy of the perpendicular vector
			;   - Leave the original data with original name
			oV_fac  = oPerp -> Copy(perp_name_out, /CACHE)
		
		;Only parallel direction is explicitly defined.
		endif else begin
			oB_fac = oB -> Copy(bname_out, /CACHE)
		endelse
	endelse

;-------------------------------------------
; Variable Names ///////////////////////////
;-------------------------------------------
	if arg_present(varnames) then begin
		if n_elements(oV_fac) gt 0 $
			then varnames = [oB_fac.name, oV_fac.name] $
			else varnames = oB_fac.name
	endif
end