; docformat = 'rst'
;
; NAME:
;       MrMMS_EDI_Q0_GPDist
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
;   Compute the pitch and gyrophase angle of EDI particle trajectory information.
;   Resulting data is added to the MrVariable cache.
;
; :Categories:
;   MMS, EDI, MrVariable
;
; :Params:
;       SC:         in, required, type=string
;                   MMS spacecraft ID. Options are {'mms1' | 'mms2' | 'mms3' | 'mms4'}
;       MODE:       in, required, type=string
;                   Data rate mode. Options are {'srvy' | 'brst'}
;       TSTART:     in, optional, type=string/strarr
;                   Date-time string, formatted as YYYY-MM-DDThh:mm:ss.fff indicating the
;                       time of the first distribution. If an array, `TEND` and `TSTRIDE`
;                       are ignored. If not provided, the first time stamp will be used.
;       TEND:       in, optional, type=string/integer, default=1
;                   Date-time string, formatted as YYYY-MM-DDThh:mm:ss[.fff] indicating
;                       the time of the last distribution. If an integer, the total
;                       number of distributions to be plotted.
;       TSTRIDE:    in, optional, type=integer, defualt=1
;                   The number of distributions to skip between successive plots.
;
; :Keywords:
;       DT:                 in, optional, type=string/objref, default=2.5
;                           Duration, in seconds, of each time bin.
;       DGA:                in, optional, type=float, default=11.25
;                           Width, in degrees, of each gyrophase bin.
;       FAC:                in, optional, type=string, default='EXB' for srvy and 'VXB' for brst
;                           Name of the field-aligned coordinate system used
;                               to define the directions perpendicular to B.
;                               Options include 'VXB' and 'EXB'
;       GARANGE:            out, optional, type=string, default=[-180\, 180]
;                           Range of gyrophase angles over which to bin.
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
;       2014/06/28  -   Written by Matthew Argall
;-
function MrMMS_E_VxB, $
NO_LOAD=no_load
	compile_opt idl2
	
	catch, the_error
	if the_error ne 0 then begin
		catch, /CANCEL
		if n_elements(win) gt 0 then obj_destroy, win
		MrPrintF, 'LogErr'
		return, !Null
	endif
	
	sc   = 'mms1'
	mode = 'srvy'
	MrVar_SetTRange, ['2016-01-01T00:59:00', '2016-01-01T01:01:00']
	tf_load = ~keyword_set(no_load)
	
;-------------------------------------------
; Get Data /////////////////////////////////
;-------------------------------------------
	level    = 'l2'
	edp_mode = 'fast'
	fgm_mode = 'brst'
	fpi_mode = 'brst'
	
	if tf_load then begin
		;FGM
		MrMMS_FGM_Load_Data, sc, fgm_mode, $
		                     VARFORMAT = '*b_dmpa*'
		
		;EDP E-Field
		MrMMS_Load_Data, sc, 'edp', edp_mode, level, $
		                 OPTDESC   = 'dce'
		                 VARFORMAT = '*_dce_dsl_*'
	
		;DES
		MrMMS_FPI_Load_Data, sc, fpi_mode, $
		                     OPTDESC   = 'dis-moms', $
		                     VARFORMAT = '*bulk?_dbcs_*'
	
		;DIS
		MrMMS_FPI_Load_Data, sc, fpi_mode, $
		                     OPTDESC   = 'des-moms', $
		                     VARFORMAT = '*bulk?_dbcs_*'
	endif

;-------------------------------------------
; Variable Names ///////////////////////////
;-------------------------------------------
	;Source names
	ve_vname = sc + '_des_bulkv_dbcs_' + fpi_mode
	vi_vname = sc + '_dis_bulkv_dbcs_' + fpi_mode
	b_vname  = sc + '_fgm_bvec_dmpa_'  + fgm_mode + '_' + level
	e_vname  = sc + '_edp_dce_dsl_'    + edp_mode + '_' + level
	
	;Output names
	Ex_des_vname  = sc + '_des_ex_dsl_'       + fpi_mode + '_' + level
	Ey_des_vname  = sc + '_des_ey_dsl_'       + fpi_mode + '_' + level
	Ez_des_vname  = sc + '_des_ez_dsl_'       + fpi_mode + '_' + level
	Ex_dis_vname  = sc + '_dis_ex_dsl_'       + fpi_mode + '_' + level
	Ey_dis_vname  = sc + '_dis_ey_dsl_'       + fpi_mode + '_' + level
	Ez_dis_vname  = sc + '_dis_ez_dsl_'       + fpi_mode + '_' + level
	EperpX_vname  = sc + '_edp_ex_dsl_perp_'  + edp_mode + '_' + level
	EperpY_vname  = sc + '_edp_ey_dsl_perp_'  + edp_mode + '_' + level
	EperpZ_vname  = sc + '_edp_ez_dsl_perp_'  + edp_mode + '_' + level
	Epar_vname    = sc + '_edp_epar_'         + edp_mode + '_' + level

;-------------------------------------------
; V x B ////////////////////////////////////
;-------------------------------------------

	;Get the data
	Ve = MrVar_Get(ve_vname)
	Vi = MrVar_Get(vi_vname)
	B  = MrVar_Get(b_vname)
	E  = MrVar_Get(e_vname)
	
	;Interpolate to DES
	Vi_des = Vi -> Interpol(Ve)
	B_des  = B  -> Interpol(Ve)
	
	;Compute the convective electric field
	;   - Convert km/s * nT to mV/m
	VexB = -1e-3 * Ve     -> Cross(B_des)
	VixB = -1e-3 * Vi_des -> Cross(B_des)
	
	;Set properties
	VexB -> AddAttr, 'DEPEND_0', Ve['DEPEND_0']
	VexB -> AddAttr, 'UNITS',    'mV/m'
	VexB -> Cache
	
	VixB -> AddAttr, 'DEPEND_0', Ve['DEPEND_0']
	VixB -> AddAttr, 'UNITS',    'mV/m'
	VixB -> Cache
	
	;Split into components
	VexB -> Split, oEx_des, oEy_des, oEz_des, NAMES=[Ex_des_vname, Ey_des_vname, Ez_des_vname], /CACHE
	VixB -> Split, oEx_dis, oEy_dis, oEz_dis, NAMES=[Ex_dis_vname, Ey_dis_vname, Ez_dis_vname], /CACHE

	;Clear data
	obj_destroy, [VexB, VixB, Vi_des]
	
	;
	; Set Properties
	;
	
	;DES
	oEx_des -> AddAttr, 'COLOR', 'Red'
	oEx_des -> AddAttr, 'TITLE', 'Ex!C(mV/m)'
	
	oEy_des -> AddAttr, 'COLOR', 'Red'
	oEy_des -> AddAttr, 'TITLE', 'Ey!C(mV/m)'
	
	oEz_des -> AddAttr, 'COLOR', 'Red'
	oEz_des -> AddAttr, 'TITLE', 'Ez!C(mV/m)'
	
	;DIS
	oEx_dis -> AddAttr, 'COLOR', 'Blue'
	oEx_dis -> AddAttr, 'TITLE', 'Ex!C(mV/m)'
	
	oEy_dis -> AddAttr, 'COLOR', 'Blue'
	oEy_dis -> AddAttr, 'TITLE', 'Ey!C(mV/m)'
	
	oEz_dis -> AddAttr, 'COLOR', 'Blue'
	oEz_dis -> AddAttr, 'TITLE', 'Ez!C(mV/m)'

;-------------------------------------------
; E Perp & Par /////////////////////////////
;-------------------------------------------

	;Interpolate B to E
	E_des     = E     -> Interpol(Ve)
	b_hat_des = B_des -> Normalize()

	;Parallel
	Epar = E_des -> Dot(b_hat_des)
	
	;Perpendicular
	Ex_perp = E_des[*,0] - E_des[*,0] * b_hat_des[*,0]
	Ey_perp = E_des[*,1] - E_des[*,1] * b_hat_des[*,1]
	Ez_perp = E_des[*,2] - E_des[*,2] * b_hat_des[*,2]
	Eperp = MrVectorTS( [ [temporary(Ex_perp)], [temporary(Ey_perp)], [temporary(Ez_perp)] ] )

	;Set properties
	Epar -> SetName, Epar_vname
	Epar -> AddAttr, 'DEPEND_0', Ve['DEPEND_0']
	Epar -> AddAttr, 'TITLE',    'Epar!C(mV/m)'
	Epar -> AddAttr, 'UNITS',    'mV/m'
	Epar -> Cache

	Eperp -> AddAttr, 'DEPEND_0', Ve['DEPEND_0']
	Eperp -> AddAttr, 'UNITS',    'mV/m'
	Eperp -> Cache
	
	;Split perp into components
	Eperp -> Split, oEx, oEy, oEz, NAMES=[EperpX_vname, EperpY_vname, EperpZ_vname], /CACHE
	
	;Clear data
	obj_destroy, [E_des, B_des, b_hat_des, Eperp]
	
	;
	; Set Properties
	;
	oEx -> AddAttr, 'TITLE', 'EperpX!C(mV/m)'
	
	oEy -> AddAttr, 'TITLE', 'EperpY!C(mV/m)'
	
	oEz -> AddAttr, 'TITLE', 'EperpZ!C(mV/m)'

;-------------------------------------------
; More Properties //////////////////////////
;-------------------------------------------
	oB = MrVar_Get(B_vname)
	oB -> SetAttrValue, 'TITLE', strupcase(sc)

;-------------------------------------------
; Plot Data ////////////////////////////////
;-------------------------------------------
	;Plot variables
	win = MrVar_PlotTS([B_vname, EperpX_vname, EperpY_vname, EperpZ_vname, Epar_vname], YSIZE=750)
	win -> Refresh, /DISABLE
	
	;Overplot
	win = MrVar_OPlotTS( [EperpX_vname, EperpY_vname, EperpZ_vname], $
	                     [Ex_des_vname, Ey_des_vname, Ez_des_vname] )
	win = MrVar_OPlotTS( [EperpX_vname, EperpY_vname, EperpZ_vname], $
	                     [Ex_dis_vname, Ey_dis_vname, Ez_dis_vname] )
	
	;Create legends
	oLx = MrLegend( ALIGNMENT    = 'NW', $
	                POSITION     = [1.0, 1.0], $
	                /RELATIVE, $
	                LABEL        = ['EDP', 'DES', 'DIS'], $
	                TARGET       = win[EperpX_vname], $
	                TEXT_COLOR   = ['Black', 'Red', 'Blue'], $
	                SAMPLE_WIDTH = 0.0 )
	
	oLy = MrLegend( ALIGNMENT    = 'NW', $
	                POSITION     = [1.0, 1.0], $
	                /RELATIVE, $
	                LABEL        = ['EDP', 'DES', 'DIS'], $
	                TARGET       = win[EperpY_vname], $
	                TEXT_COLOR   = ['Black', 'Red', 'Blue'], $
	                SAMPLE_WIDTH = 0.0 )
	
	oLz = MrLegend( ALIGNMENT    = 'NW', $
	                POSITION     = [1.0, 1.0], $
	                /RELATIVE, $
	                LABEL        = ['EDP', 'DES', 'DIS'], $
	                TARGET       = win[EperpZ_vname], $
	                TEXT_COLOR   = ['Black', 'Red', 'Blue'], $
	                SAMPLE_WIDTH = 0.0 )
	
	win.OXMargin = [10,10]
	win -> refresh
	return, win
end