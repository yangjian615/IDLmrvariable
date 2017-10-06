; docformat = 'rst'
;
; NAME:
;       MrMMS_Plot_E_VxB
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
;   Plot the parallel and perpendicular components of the electric field.
;       1. Bx, By, Bz, |B|
;       2. Ex: VexB, VixB, Eperp
;       3. Ey: VexB, VixB, Eperp
;       4. Ez: VexB, VixB, Eperp
;       5. Epar
;       6. E'x, E'y, E'z
;       7. Jx, Jy, Jz
;       8. J.E'
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
;       2016/06/28  -   Written by Matthew Argall
;       2017/01/18  -   Added E', J, and J.E' to the plot. - MRA
;-
function MrMMS_Plot_E_VxB, sc, mode, $
NO_LOAD=no_load, $
TRANGE=trange
	compile_opt idl2
	
	catch, the_error
	if the_error ne 0 then begin
		catch, /CANCEL
		if n_elements(win) gt 0 then obj_destroy, win
		MrPrintF, 'LogErr'
		return, !Null
	endif
	
	if n_elements(trange) gt 0 then MrVar_SetTRange, trange
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
		                 OPTDESC   = 'dce', $
		                 VARFORMAT = '*_dce_dsl_*'
	
		;DES
		MrMMS_FPI_Load_Data, sc, fpi_mode, $
		                     OPTDESC   = 'dis-moms', $
		                     VARFORMAT = ['*numberdensity*', '*bulk?_dbcs_*']
	
		;DIS
		MrMMS_FPI_Load_Data, sc, fpi_mode, $
		                     OPTDESC   = 'des-moms', $
		                     VARFORMAT = ['*numberdensity*', '*bulk?_dbcs_*']
	endif

;-------------------------------------------
; Variable Names ///////////////////////////
;-------------------------------------------
	;Source names
	ne_vname   = sc + '_des_numberdensity_' + fpi_mode
	ni_vname   = sc + '_dis_numberdensity_' + fpi_mode
	ve_vname   = sc + '_des_bulkv_dbcs_'    + fpi_mode
	vi_vname   = sc + '_dis_bulkv_dbcs_'    + fpi_mode
	b_vname    = sc + '_fgm_b_dmpa_'        + fgm_mode + '_' + level
	bvec_vname = sc + '_fgm_bvec_dmpa_'     + fgm_mode + '_' + level
	e_vname    = sc + '_edp_dce_dsl_'       + edp_mode + '_' + level
	
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
	j_vname       = sc + '_currentdensity_'   + edp_mode + '_' + level
	eprime_vname  = sc + '_eprime_'           + edp_mode + '_' + level
	je_vname      = sc + '_jdoteprime_'       + edp_mode + '_' + level

;-------------------------------------------
; Ve,i x B /////////////////////////////////
;-------------------------------------------

	;Get the data
	oVe = MrVar_Get(ve_vname)
	oVi = MrVar_Get(vi_vname)
	oB  = MrVar_Get(bvec_vname)
	oE  = MrVar_Get(e_vname)
	
	;Interpolate to DES
	oVi_des = oVi -> Interpol(oVe)
	oB_des  = oB  -> Interpol(oVe)
	
	;Compute the convective electric field
	;   - Convert km/s * nT to mV/m
	oVexB = -1e-3 * oVe     -> Cross(oB_des)
	oVixB = -1e-3 * oVi_des -> Cross(oB_des)
	
	;Set properties
	oVexB['DEPEND_0'] = oVe['DEPEND_0']
	oVexB['UNITS']    = 'mV/m'
	oVexB -> Cache
	
	oVixB['DEPEND_0'] = oVe['DEPEND_0']
	oVixB['UNITS']    = 'mV/m'
	oVixB -> Cache
	
	;Split into components
	oVexB -> Split, oEx_des, oEy_des, oEz_des, NAMES=[Ex_des_vname, Ey_des_vname, Ez_des_vname], /CACHE
	oVixB -> Split, oEx_dis, oEy_dis, oEz_dis, NAMES=[Ex_dis_vname, Ey_dis_vname, Ez_dis_vname], /CACHE

	;Clear data
	obj_destroy, oVixB

;-------------------------------------------
; Current Density //////////////////////////
;-------------------------------------------
	oNe = MrVar_Get(ne_vname)

	;Compute current density
	;   - 1e15 converts to micro-A/m^3
	oJ = 1e15 * MrConstants('q') * oNe * (oVi_des - oVe)
	oJ -> SetName, j_vname
	oJ -> Cache
	
	;Clear data
	obj_destroy, oVi_des
	
;-------------------------------------------
; E Perp & Par /////////////////////////////
;-------------------------------------------

	;Interpolate B to E
	oE_des     = oE     -> Interpol(oVe)
	ob_hat_des = oB_des -> Normalize()

	;Parallel
	oEpar = oE_des -> Dot(ob_hat_des)

	;Perpendicular
	oEx_perp = oE_des[*,0] - oE_des[*,0] * ob_hat_des[*,0]
	oEy_perp = oE_des[*,1] - oE_des[*,1] * ob_hat_des[*,1]
	oEz_perp = oE_des[*,2] - oE_des[*,2] * ob_hat_des[*,2]
	oEperp = MrVectorTS( oE_des['TIMEVAR'], [ [ oEx_perp['DATA'] ], $
	                                          [ oEy_perp['DATA'] ], $
	                                          [ oEz_perp['DATA'] ] ] )

	;Set properties
	oEpar -> SetName, Epar_vname
	oEpar['DEPEND_0'] = oVe['DEPEND_0']
	oEpar['TITLE']    = 'Epar!C(mV/m)'
	oEpar['UNITS']    = 'mV/m'
	oEpar -> Cache

	oEperp['DEPEND_0'] = oVe['DEPEND_0']
	oEperp['UNITS']    = 'mV/m'
	
	;Split perp into components
	oEperp -> Split, oEx, oEy, oEz, NAMES=[EperpX_vname, EperpY_vname, EperpZ_vname], /CACHE
	
	;Clear data
	obj_destroy, [oB_des, ob_hat_des, oEperp, oEx_perp, oEy_perp, oEz_perp]
	
;-------------------------------------------
; J.Eprime /////////////////////////////////
;-------------------------------------------
	;Electron rest frame
	oEprime = oE_des + oVexB
	oEprime -> SetName, eprime_vname
	oEprime -> Cache
	
	;Dissipation
	oJdotEprime = oJ -> Dot(oEprime)
	oJdotEprime -> SetName, je_vname
	oJdotEprime -> Cache
	
	;Clear data
	obj_destroy, [oE_des, oVexB]

;-------------------------------------------
; Attributes ///////////////////////////////
;-------------------------------------------
	;B
	oB = MrVar_Get(b_vname)
	oB['PLOT_TITLE'] = strupcase(sc)
	oB['TITLE']      = 'B!C(nT)'
	
	;Ex DES
	oEx_des['LABEL'] = 'DES'
	oEx_des['COLOR'] = 'Red'
	oEx_des['TITLE'] = 'Ex!C(mV/m)'
	
	;Ez DES
	oEy_des['LABEL'] = 'DES'
	oEy_des['COLOR'] = 'Red'
	oEy_des['TITLE'] = 'Ey!C(mV/m)'
	
	;Ez DES
	oEz_des['LABEL'] = 'DES'
	oEz_des['COLOR'] = 'Red'
	oEz_des['TITLE'] = 'Ez!C(mV/m)'
	
	;Ex DIS
	oEx_dis['LABEL'] = 'DIS'
	oEx_dis['COLOR'] = 'Blue'
	oEx_dis['TITLE'] = 'Ex!C(mV/m)'
	
	;Ex DIS
	oEy_dis['LABEL'] = 'DIS'
	oEy_dis['COLOR'] = 'Blue'
	oEy_dis['TITLE'] = 'Ey!C(mV/m)'
		
	;Ez DIS
	oEz_dis['LABEL'] = 'DIS'
	oEz_dis['COLOR'] = 'Blue'
	oEz_dis['TITLE'] = 'Ez!C(mV/m)'
	
	;CURRENT DENSITY
	oJ['LABEL'] = ['Jx', 'Jy', 'Jz']
	oJ['TITLE'] = 'J!C($\mu$A/m^2)'
	oJ['UNITS'] = '\muA/m^2'
	
	;EperpX EDP
	oEx['LABEL'] = 'EDP'
	oEx['TITLE'] = 'EperpX!C(mV/m)'
	
	;EperpY EDP
	oEy['LABEL'] = 'EDP'
	oEy['TITLE'] = 'EperpY!C(mV/m)'
	
	;EperpZ EDP
	oEz['LABEL'] = 'EDP'
	oEz['TITLE'] = 'EperpZ!C(mV/m)'
	
	;EPRIME
	oEprime['TITLE'] = "E'!C(mV/m)"
	oEprime['LABEL'] = ["E'x", "E'y", "E'z"]
	
	;J.EPRIME
	oJdotEprime['TITLE'] = "J.E'!C(nW/m^3)"

;-------------------------------------------
; Plot Data ////////////////////////////////
;-------------------------------------------
	;Plot variables
	win = MrVar_PlotTS( [B_vname, EperpX_vname, EperpY_vname, EperpZ_vname, Epar_vname, $
	                     eprime_vname, j_vname, je_vname], $
	                    /NO_REFRESH, $
	                    YSIZE=750 )
	win -> Refresh, /DISABLE
	
	;Overplot
	win = MrVar_OPlotTS( [EperpX_vname, EperpY_vname, EperpZ_vname], $
	                     [Ex_des_vname, Ey_des_vname, Ez_des_vname] )
	win = MrVar_OPlotTS( [EperpX_vname, EperpY_vname, EperpZ_vname], $
	                     [Ex_dis_vname, Ey_dis_vname, Ez_dis_vname] )
	
	win[0] -> SetLayout, [1,1]
	win    -> TrimLayout
	win.OXMargin = [10,10]
	win -> refresh
	return, win
end