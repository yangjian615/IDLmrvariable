; docformat = 'rst'
;
; NAME:
;       MrMMS_Plot_EDI_d29_EdotJ
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
;   Plot energy dissipation with EDI Data29 counts.
;       1. Bxyz, |B|
;       2. E par, perp (30ms)
;       3. J par, perp (30ms)
;       4. Data29 Flux (1ms)
;       5. J perp (data29 - 1ms)
;       6. J.E par, perp (30ms)
;       6. J.E perp (data29 - 1ms)
;
; :Categories:
;   MMS, EDI, MrVariable
;
; :Params:
;       SC:         in, required, type=string
;                   MMS spacecraft ID. Options are {'mms1' | 'mms2' | 'mms3' | 'mms4'}
;       MODE:       in, required, type=string
;                   Data rate mode. Options are {'srvy' | 'brst'}
;
; :Keywords:
;       COORDS:     in, optional, type=string, default='gse'
;                   Coordinate system in which to load the data. Options are: {'dbcs' | 'gse' | 'gsm'}
;       LEVEL:      in, optional, type=string, default='l2'
;                   Data quality level. Options are: {'l1a' | 'ql' | 'l2'}
;       NO_LOAD:    in, optional, type=boolean, default=0
;                   If set, data will not be loaded from source CDF files.
;       TRANGE:     in, optional, type=string/strarr(2), default=MrVar_GetTRange()
;                   The start and end times of the data interval to be plotted, formatted
;                       as 'YYYY-MM-DDThh:mm:ss'
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
;       2017/01/22  -   Written by Matthew Argall
;-
function MrMMS_Plot_EDI_d29_EdotJ, sc, mode, $
COORDS=coords, $
LEVEL=level, $
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
	
	tf_load = ~keyword_set(no_load)
	IF N_Elements(coords) EQ 0 THEN coords = 'dsl'
	IF N_Elements(trange) GT 0 THEN MrVar_SetTRange, trange
	
;-------------------------------------------
; Get Data /////////////////////////////////
;-------------------------------------------
	instr    = 'edp'
	level    = 'l2'
	optdesc  = 'dce'
	IF mode EQ 'srvy' THEN BEGIN
		MrPrintF, 'LogWarn', 'EDP has slow and fast mode data, not survey. Choosing fast.'
		edp_mode = 'fast'
	ENDIF ELSE BEGIN
		edp_mode = mode
	ENDELSE
	
	;Choose DFG data always
	fgm_instr  = 'dfg'
	fgm_mode   = mode EQ 'brst' ? mode : 'srvy'
	fgm_level  = 'l2pre'
	fgm_coords = coords EQ 'dsl' ? 'dmpa' : coords
	
	;FPI info
	species    = 'e'
	fpi_mode   = mode EQ 'brst' ? mode : 'fast'
	fpi_coords = coords EQ 'dsl' ? 'dbcs' : coords
	fpi_level  = 'l2'
	fpi_instr  = 'd' + species + 's'

	IF tf_load THEN BEGIN
		;FGM
		MrMMS_FGM_Load_Data, sc, fgm_mode, $
		                     INSTR     = fgm_instr, $
		                     LEVEL     = fgm_level, $
		                     VARFORMAT = '*' + fgm_instr + '_' + fgm_mode + '*dmpa'
		
		;EDP E-Field
		MrMMS_Load_Data, sc, instr, edp_mode, level, $
		                 OPTDESC   = 'dce', $
		                 VARFORMAT = '*_dce_dsl_*'
	
		;DATA29
		MrMMS_Load_Data, sc, 'edi', 'brst', 'l1a', $
		                 OPTDESC   = 'efield', $
		                 VARFORMAT = ['*phi_gd*', '*theta_gd*', '*data29_gd*']
		
		;EDI Calibrations
		MrMMS_EDI_Load_Cals, sc, $
		                     VARFORMAT='*abs*cal*'
	
		;FPI
		MrMMS_FPI_Load_Data, sc, fpi_mode, $
		                     LEVEL     = fpi_level, $
		                     OPTDESC   = ['des-moms', 'dis-moms'], $
		                     VARFORMAT = ['*density*', '*bulkv_'+fpi_coords+'*']
	ENDIF

;-------------------------------------------
; Variable Names ///////////////////////////
;-------------------------------------------
	;Source names
	b_vname        = StrJoin( [sc, fgm_instr, fgm_mode, fgm_level, fgm_coords], '_' )
	bvec_vname     = StrJoin( [sc, fgm_instr, 'vec', fgm_mode, fgm_level, fgm_coords], '_' )
	bmag_vname     = StrJoin( [sc, fgm_instr, 'mag', fgm_mode, fgm_level, fgm_coords], '_' )
	e_vname        = StrJoin( [sc, instr, optdesc, coords, edp_mode, level], '_')
	ne_vname       = StrJoin( [sc, 'des', 'numberdensity', fpi_mode], '_')
	ve_vname       = StrJoin( [sc, 'des', 'bulkv', fpi_coords, fpi_mode], '_')
	ni_vname       = StrJoin( [sc, 'dis', 'numberdensity', fpi_mode], '_')
	vi_vname       = StrJoin( [sc, 'dis', 'bulkv', fpi_coords, fpi_mode], '_')
	d29_gd12_vname = StrJoin( [sc, 'edi', 'data29', 'gd12'], '_')
	d29_gd21_vname = StrJoin( [sc, 'edi', 'data29', 'gd21'], '_')
	gdu1_abscal_vname = StrJoin( [sc, 'edi', 'abs',   'gdu1', 'cal', level], '_')
	gdu2_abscal_vname = StrJoin( [sc, 'edi', 'abs',   'gdu2', 'cal', level], '_')
	phi_gd12_vname    = StrJoin( [sc, 'edi', 'phi',   'gd12'], '_')
	phi_gd21_vname    = StrJoin( [sc, 'edi', 'phi',   'gd21'], '_')
	theta_gd12_vname  = StrJoin( [sc, 'edi', 'theta', 'gd12'], '_')
	theta_gd21_vname  = StrJoin( [sc, 'edi', 'theta', 'gd21'], '_')
	
	;Output names
	epar_fpi_vname  = StrJoin( [sc, instr, 'e',     'par',         edp_mode, level], '_')
	eperp_fpi_vname = StrJoin( [sc, instr, 'e',     'perp', 'fpi', edp_mode, level], '_')
	eperp_d29_vname = StrJoin( [sc, instr, 'e',     'perp', 'd29', edp_mode, level], '_')
	jpar_vname      = StrJoin( [sc, 'fpi', 'j',     'par',  fpi_mode, level], '_')
	jperp_vname     = StrJoin( [sc, 'fpi', 'j',     'perp', fpi_mode, level], '_')
	jdotepar_vname  = StrJoin( [sc,        'jdote', 'par',  edp_mode, level], '_')
	jdoteperp_vname = StrJoin( [sc,        'jdote', 'perp', edp_mode, level], '_')
	edotd29_vname   = StrJoin( [sc, 'edi', 'jdote', 'd29',  mode, level], '_')
	jperp_29_vname  = StrJoin( [sc, 'edi', 'j',     'perp', 'd29',  mode, level], '_')
	d29_abs1_vname  = StrJoin( [sc, 'edi', 'data29', 'abscal', 'gdu1'], '_')
	d29_abs2_vname  = StrJoin( [sc, 'edi', 'data29', 'abscal', 'gdu2'], '_')
	ang_diff_vname  = StrJoin( [sc, 'edi', 'efield', 'angdiff'], '_')
	tce_vname       = StrJoin( [sc, 'edi', 'd29',    'tce'], '_')

;-------------------------------------------
; Epar & Eperp /////////////////////////////
;-------------------------------------------
	;Grab the fields and electrons
	oBvec = MrVar_Get(bvec_vname)
	oE    = MrVar_Get(e_vname)
	oNe   = MrVar_Get(ne_vname)
	oVe   = MrVar_Get(ve_vname)
	
	;Interpolate to DES time stamps
	oBvec_des = oBvec -> Interpol(oNe)
	oE_des    = oE    -> Interpol(oNe)
	
	;Transformation matrix
	oT     = MrVar_FAC(oBvec_des)
	oE_fac = oT ## oE_des
	
	;Par & Perp
	oBvec_des_hat = oBvec_des -> Normalize()
	oEmag         = oE_fac -> Magnitude()
	oEpar         = oE_fac -> Dot(oBvec_des_hat)
	oEperp        = oEmag - oEpar
	
	;Delete data
	Obj_Destroy, [oBvec_des, oE_des, oE_fac, oEmag]

;-------------------------------------------
; Jpar & Jperp /////////////////////////////
;-------------------------------------------
	;Grab the ions
	oNi = MrVar_Get(ni_vname)
	oVi = MrVar_Get(vi_vname)
	
	;Interpolate to DES
	oNi_des = oNi -> Interpol(oNe)
	oVi_des = oVi -> Interpol(oVe)
	
	;Compute current density
	;   - 1e15 convets to uA/m^2
	oJ = 1e15 * MrConstants('q') * oNe * (oVi_des - oVe)
	
	;Rotate to FAC
	oJ_fac = oT ## oJ
	
	;Par & Perp
	oJmag  = oJ_fac -> Magnitude()
	oJpar  = oJ_fac -> Dot(oBvec_des_hat)
	oJperp = oJmag - oJpar
	
	;Destroy data
	Obj_Destroy, [oBvec_des_hat, oT, oNi_des, oVi_des, oJ, oJ_fac, oJmag]

;-------------------------------------------
; Calibrate Data29 /////////////////////////
;-------------------------------------------
	;Grab the data
	oD29_GD12 = MrVar_Get(d29_gd12_vname)
	oD29_GD21 = MrVar_Get(d29_gd21_vname)
	oAbs_GDU1 = MrVar_Get(gdu1_abscal_vname)
	oAbs_GDU2 = MrVar_Get(gdu2_abscal_vname)

	;Load calibrations
	ot_gdu1   = oD29_GD21['TIMEVAR']
	ot_gdu2   = oD29_GD12['TIMEVAR']
	iAbs_gdu1 = oAbs_GDU1['TIMEVAR'] -> Value_Locate( ot_gdu1[0, 'TT2000'], 'TT2000' )
	iAbs_gdu2 = oAbs_GDU2['TIMEVAR'] -> Value_Locate( ot_gdu2[0, 'TT2000'], 'TT2000' )
	
	;Apply absolute calibrations
	oD29_GDU1 = oD29_GD21 * oAbs_GDU1[iAbs_gdu1]
	oD29_GDU2 = oD29_GD12 * oAbs_GDU2[iAbs_gdu2]

;-------------------------------------------
; Jperp Data29 /////////////////////////////
;-------------------------------------------
	
	;Compute current density
	;   - 1e10 converts to uA/m^2
	oJperp_d29 = 1e10 * MrConstants('q') * (oD29_GDU2 - oD29_GDU1)
	
	;Interpolate E to Data29
	oBvec_d29 = oBvec -> Interpol(oD29_GDU1)
	oE_d29    = oE    -> Interpol(oD29_GDU1)
	
	;Transformation matrix
	oT     = MrVar_FAC(oBvec_d29)
	oE_fac = oT ## oE_d29
	
	;Par & Perp
	oBvec_d29_hat = oBvec_d29 -> Normalize()
	oEmag_d29     = oE_fac    -> Magnitude()
	oEpar_d29     = oE_fac    -> Dot(oBvec_d29_hat)
	oEperp_d29    = oEmag_d29 - oEpar_d29
	
	;Destroy data
	Obj_Destroy, [oBvec_d29, oE_d29, oT, oBvec_d29_hat, oE_fac, oEmag_d29, oEpar_d29]

;-------------------------------------------
; Let Time = 1 / Fce ///////////////////////
;-------------------------------------------
	;Get the magnetic field at Data29 cadence
	oBmag     = MrVar_Get(bmag_vname)
	oBmag_d29 = oBmag -> Interpol(oD29_GDU1)
	
	;Cyclotron frequency
	oFce = MrVar_Freq_Cyclotron(oBmag_d29, 'm_e')
	
	;Number of cyclotron periods between samples
	oTime = oD29_GDU1['TIMEVAR']
	dt    = oTime[1:*, 'SSM'] - oTime -> GetData('SSM')
	Tce   = dt * oFce[0:-1]
	
	;Create variable
	oTce = MrTimeSeries( oTime[0:-2], Tce, $
	                     /CACHE, $
	                     NAME = tce_vname )

;-------------------------------------------
; E.J //////////////////////////////////////
;-------------------------------------------

	;E.J
	oJdotEpar  = oEpar * oJpar
	oJdotEperp = oEperp * oJperp
	
	;E.D29
	oEdotD29 = oEperp_d29 * Abs(oJperp_d29['DATA'])

;-------------------------------------------
; Look Directions //////////////////////////
;-------------------------------------------
	oPhi_gd12   = MrVar_Get(phi_gd12_vname)   * !dtor
	oPhi_gd21   = MrVar_Get(phi_gd21_vname)   * !dtor
	oTheta_gd12 = MrVar_Get(theta_gd12_vname) * !dtor
	oTheta_gd21 = MrVar_Get(theta_gd21_vname) * !dtor
		
	;Remove points that are not aligned
	t_gd12 = oPhi_gd12['TIME', 'TT2000']
	t_gd21 = oPhi_gd21['TIME', 'TT2000']
	
	;Unique time tags only
	u12 = uniq(t_gd12, sort(t_gd12))
	u21 = uniq(t_gd21, sort(t_gd21))
	!Null = MrIsMember( t_gd21[u21], t_gd12[u12], iGD12, A_INDICES=iGD21 )

	IF iGD12[0] NE -1 THEN BEGIN
		idx        = u12[ iGD12 ]
		t_gd12     = t_gd12[idx]
		phi_gd12   = oPhi_GD12[idx]
		theta_gd12 = oTheta_GD12[idx]
	ENDIF

	IF iGD21[0] NE -1 THEN BEGIN
		idx        = u21[ iGD21 ]
		t_gd21     = t_gd21[idx]
		phi_gd21   = oPhi_GD21[idx]
		theta_gd21 = oTheta_GD21[idx]
	ENDIF

	;Convert to cartesian coords
	x1 = Sin( theta_gd12 ) * Cos( phi_gd12 )
	y1 = Sin( theta_gd12 ) * Sin( phi_gd12 )
	z1 = Cos( theta_gd12 )
	
	;Angles are in GDU coordinates, so one vector must be negated
	x2 = - (Sin( theta_gd21 ) * Cos( phi_gd21 ))
	y2 = - (Sin( theta_gd21 ) * Sin( phi_gd21 ))
	z2 = - Cos( theta_gd21 )

	;Angular difference between GDU1 & GDU2
	angdiff = ACos( x1*x2 + y1*y2 + z1*z2 ) * !radeg
	oDAng   = MrScalarTS( t_gd12, angdiff, $
	                      T_TYPE = 'TT2000', $
	                      /CACHE, $
	                      NAME   = ang_diff_vname )
	

;-------------------------------------------
; Set Attributes ///////////////////////////
;-------------------------------------------
	;B
	oB = MrVar_Get(b_vname)
	oB['PLOT_TITLE'] = StrUpCase(sc)
	
	;Epar
	oEpar -> SetName, epar_fpi_vname
	oEpar -> Cache
	oEpar['AXIS_RANGE'] = [oEpar.min, oEperp.max]
	oEpar['LABEL']      = 'Epar'
	oEpar['TITLE']      = 'E!C(mV/m)'
	oEpar['UNITS']      = 'mV/m'
	
	;Eperp
	oEperp -> SetName, eperp_fpi_vname
	oEperp -> Cache
	oEperp['COLOR'] = 'Red'
	oEperp['LABEL'] = '|Eperp|'
	oEperp['TITLE'] = 'E!C(mV/m)'
	oEperp['UNITS'] = 'mV/m'
	
	;Jpar
	oJpar -> SetName, jpar_vname
	oJpar -> Cache
	oJpar['AXIS_RANGE'] = [oJpar.min, oJperp.max]
	oJpar['LABEL']      = 'Jpar'
	oJpar['TITLE']      = 'J!C($\mu$J/m^2)'
	oJpar['UNITS']      = '\muJ/m^2'
	
	;Jperp
	oJperp -> SetName, jperp_vname
	oJperp -> Cache
	oJperp['COLOR'] = 'Red'
	oJperp['LABEL'] = '|Jperp|'
	oJperp['TITLE'] = 'J!C($\mu$J/m^2)'
	oJperp['UNITS'] = '\muJ/m^2'
	
	;J.E PAR
	oJdotEpar -> SetName, jdotepar_vname
	oJdotEpar -> Cache
	oJdotEpar['AXIS_RANGE'] = [oJdotEpar.min, oJdotEperp.max]
	oJdotEpar['LABEL']      = '(J.E)$\down||$'
	oJdotEpar['TITLE']      = 'J.E!C(nW/m^3)'
	oJdotEpar['UNITS']      = 'nW/m^2'
	
	;J.E PERP
	oJdotEperp -> SetName, jdoteperp_vname
	oJdotEperp -> Cache
	oJdotEperp['COLOR'] = 'Red'
	oJdotEperp['LABEL'] = '|J.E|$\downPerp$'
	oJdotEperp['TITLE'] = 'J.E!C(nW/m^3)'
	oJdotEperp['UNITS'] = 'nW/m^2'
	
	;Eperp DATA29
	oEperp_d29 -> SetName, eperp_d29_vname
	oEperp_d29 -> Cache
	oEperp_d29['COLOR'] = 'Red'
	oEperp_d29['LABEL'] = 'Eperp_d29'
	oEperp_d29['TITLE'] = 'E!C(mV/m)'
	oEperp_d29['UNITS'] = 'mV/m'
	
	;J.E DATA29
	oEdotD29 -> SetName, edotd29_vname
	oEdotD29 -> Cache
	oEdotD29['COLOR'] = 'Red'
	oEdotD29['TITLE'] = 'J.E!CData29!C(nW/m^3)'
	oEdotD29['UNITS'] = 'nW/m^2'
	
	;Data29 GDU1
	oD29_GDU1 -> SetName, d29_abs1_vname
	oD29_GDU1 -> Cache
	oD29_GDU1['AXIS_RANGE'] = [ oAbs_GDU1[iAbs_gdu1], Max( [oD29_GDU1.max, oD29_GDU2.max] ) ]
	oD29_GDU1['LABEL']      = 'GDU1'
	oD29_GDU1['LOG']        = 1B
	oD29_GDU1['TITLE']      = 'Flux!CPerp!C(cm^-2 s^-1)'
	oD29_GDU1['UNITS']      = 'cm^-2 s^-1'
	
	;Data29 GDU2
	oD29_GDU2 -> SetName, d29_abs2_vname
	oD29_GDU2 -> Cache
	oD29_GDU2['COLOR'] = 'Blue'
	oD29_GDU2['LABEL'] = 'GDU2'
	oD29_GDU2['TITLE'] = 'Flux!CPerp!C(cm$\up-2$s$\up-1$)'
	oD29_GDU2['UNITS'] = 'cm^-2 s^-1'
	
	;JPERP D29
	oJperp_D29 -> SetName, jperp_29_vname
	oJperp_D29 -> Cache
	oJperp_D29['COLOR'] = 'Red'
	oJperp_D29['TITLE'] = 'Jperp!Cdata29!C($\mu$A/m^2)'
	oJperp_D29['UNITS'] = '\mu A/m^2'
	
	;ANGULAR DIFFERENCE
	oDAng['AXIS_RANGE'] = [0, 180]
	oDAng['SYMBOL']     = 6
	oDAng['TITLE']      = '$\Delta$$\theta$!CQ0!C(deg)'
	oDAng['UNITS']      = 'degrees'
	
	;TCE
	oTce['PLOT_TITLE'] = 'Cyclotron Periods per Sample'
	oTce['UNITS']      = 'cycles/sec'
	oTce['TITLE']      = '# fce!C(cyc/sample)'

;-------------------------------------------
; Plot Data ////////////////////////////////
;-------------------------------------------
	;Plot variables
	win = MrVar_PlotTS( [ b_vname, epar_fpi_vname, jpar_vname, d29_abs1_vname, ang_diff_vname, $
	                      jperp_29_vname, jdotepar_vname, edotd29_vname, tce_vname ], $
	                    /NO_REFRESH, $
	                    YSIZE=750 )
	win -> Refresh, /DISABLE
	
	;Overplot
	win = MrVar_OPlotTS( [epar_fpi_vname,  jpar_vname,  d29_abs1_vname, jdotepar_vname], $
	                     [eperp_fpi_vname, jperp_vname, d29_abs2_vname, jdoteperp_vname] )
	
	win[0] -> SetLayout, [1,1]
	win    -> TrimLayout
	win.OXMargin = [14,10]
	win -> refresh
	return, win
end