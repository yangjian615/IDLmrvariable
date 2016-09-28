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
;   Create a transformation matrix to field-aligned coordinates (FAC).
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
;                           The field-aligned coordinate system to create. Options are:
;                               {'' | 'VXB' | 'EXB' }.
;       VAR:                in, optional, type=string/objref
;                           A MrVariable name or object for which the time tags define
;                               the grid onto which the transformation matrix is
;                               interpolated. If not present, then the magnetic field
;                               is interpolated to the time of whichever variable defines
;                               the perpendicular direction, as per `FAC`.
;
; :Keywords:
;       COORDS:             in, optional, type=string, default='GSE'
;                           Original coordinate system.
;       INSTR:              in, optional, type=string, default='dfg'
;                           The fluxgate instrument to use. The default is 'DFG' for
;                               `MODE`='srvy' and 'FGM' for `MODE`='brst'.
;       LEVEL:              in, optional, type=float, default='L2'
;                           Data quality level to use.
;       NO_LOAD:            in, optional, type=boolean, default=0
;                           Set if the data that define the field-aligned coordinate
;                               system has already been loaded into the variable cache.
;       TRANGE:             in, optional, type=strarr(2), default=MrVar_GetTRange()
;                           Time interval over which to create the transformation.
;
; :See Also:
;    MrVar_FAC.pro
;    MrVar_SetTRange.pro
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
;       2016/09/24  -   Written by Matthew Argall
;-
function MrMMS_FGM_Get_TFAC, sc, mode, fac, var, $
COORDS=coords, $
INSTR=instr, $
LEVEL=level, $
NO_LOAD=no_load, $
TRANGE=trange
	compile_opt idl2

	catch, the_error
	if the_error ne 0 then begin
		catch, /CANCEL
		if n_elements(win) gt 0 then obj_destroy, win
		MrPrintF, 'LogErr'
		return, obj_new()
	endif
	
	;Defaults
	tf_load = ~keyword_set(no_load)
	if n_elements(trange) gt 0 then MrVar_SetTRange, trange
	if n_elements(coords) eq 0 then coords = 'gse'
	if n_elements(fac)    eq 0 then fac    = 'VXB'
	if n_elements(instr)  eq 0 then instr  = 'fgm'
	if n_elements(level)  eq 0 then level  = 'l2'
	
	;Variable type
	type = level eq 'l2' ? 'flux' : 'counts'
	_fac = strupcase(fac)
	_cs  = strlowcase(coords)

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
	fgm_coords = MrIsMember(['dbcs', 'dsl'],  _cs) ? 'dmpa' : coords
	fpi_coords = MrIsMember(['dmpa', 'dsl'],  _cs) ? 'dbcs' : coords
	edp_coords = MrIsMember(['dbcs', 'dmpa'], _cs) ? 'dsl'  : coords
	
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
		                     VARFORMAT = ['*b_' + fgm_coords + '*', '*fg*' + fgm_coords]

	;-------------------------------------------
	; FPI //////////////////////////////////////
	;-------------------------------------------
		if _fac eq 'VXB' then begin
			;Reminder warning
			;   - GSE is not available yet
			;   - FPI does not operate in slow survey, so there is no "srvy" product.
			if coords eq 'gse' then begin
				MrPrintF, 'LogWarn', 'FPI data is not available in GSE. Using DBCS.'
				fpi_cs = 'dbcs'
			endif
			
			;Load the data
			MrMMS_FPI_Get_Data, sc, 'fpi', fpi_mode, level, $
			                    OPTDESC   = 'des-moms', $
			                    VARFORMAT = '*bulk?_' + fpi_cs + '*'
	
	;-------------------------------------------
	; EDP //////////////////////////////////////
	;-------------------------------------------
		endif else if _fac eq 'EXB' then begin
			;EDP does not make a srvy product; slow and fast are kept separate.
			if mode eq 'srvy' then MrPrintF, 'LogWarn', 'TODO: Combine slow and fast survey data.'
			edp_mode = 'fast'
			
			;Load the data
			MrMMS_Get_Data, sc, 'edp', edp_mode, level, $
			                OPTDESC   = 'dce', $
			                VARFORMAT = E_vname
	
	;-------------------------------------------
	; ??? //////////////////////////////////////
	;-------------------------------------------
		endif else if _fac ne '' then begin
			message, 'FAC must be {"VxB" | "ExB" | ""}'
		endif
	endif

;-------------------------------------------
; Get the Data /////////////////////////////
;-------------------------------------------
	
	;Get the magnetic field vector
	b_vname = fgm_level eq 'l2' || fgm_level eq 'l2pre' $
	              ? strjoin( [sc, fgm_instr, 'bvec', fgm_coords, fgm_mode, fgm_level], '_' ) $
	              : strjoin( [sc, fgm_instr, 'vec', fgm_mode, fgm_level, fgm_coords], '_' )
	oB      = MrVar_Get(b_vname)
	
	;Vector that defines the perpendicular direction
	case _fac of
		'VXB': oPerp = MrVar_Get( sc + '_des_bulkv_' + fpi_cs + '_' + fpi_mode )
		'EXB': oPerp = MrVar_Get( sc + '_edp_dce_' + cs + '_' + edp_mode + '_' + level )
		'':    ;Do nothing
	endcase

;-------------------------------------------
; Create Transformation Matrix /////////////
;-------------------------------------------
	;Specific time cadence desired
	if n_elements(var) gt 0 then begin
		;Get the variable and its time tags
		oVar     = MrVar_Get(var)
		depend_0 = oVar['DEPEND_0']

		;Interpolate onto the given variable
		oB_fac = oB -> Interpol(oVar)
		if n_elements(oPerp) gt 0 then oV_fac = oPerp -> Interpol(oVar)
	
	;Common time base for FAC
	endif else begin
		if n_elements(oPerp) gt 0 then begin
			oB_fac = oB -> Interpol(oPerp)
			depend_0 = oPerp['DEPEND_0']
		endif else begin
			depend_0 = oB['DEPEND_0']
		endelse
	endelse
	
	;Transformation
	oT = MrVar_FAC(oB_fac, oV_fac, TYPE=_fac)

;-------------------------------------------
; Set Properties ///////////////////////////
;-------------------------------------------
	oT -> AddAttr, 'CATDESC',  'Transformation matrix from ' + strupcase(coords) + ' to ' + _fac + '.'
	oT -> AddAttr, 'DEPEND_0', depend_0

	return, oT
end