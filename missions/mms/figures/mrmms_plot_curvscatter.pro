; docformat = 'rst'
;
; NAME:
;       MrMMS_Plot_CurvScatter
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
;   Generate a plot to provide an overview of reconnection quantities:
;       1. |B|
;       2. PAD + loss-cone
;       3. R Curvature
;       4. R Err
;       5. Adiabatic parameter K
;
;   Figure 3 of:
;       Lavraud, B. et al. (2016), Currents and associated electron scattering and
;           bouncing near the diffusion region at Earth’s magnetopause, Geophys. Res.
;           Lett., 3042–3050, doi:10.1002/2016GL068359.
;
; :Categories:
;   MMS
;
; :Params:
;       SC:         in, required, type=string
;                   MMS spacecraft ID. Options are {'mms1' | 'mms2' | 'mms3' | 'mms4'}
;       MODE:       in, required, type=string
;                   Data rate mode. Options are {'srvy' | 'brst'}
;
; :Keywords:
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
;       2016/11/27  -   Written by Matthew Argall
;-
function MrMMS_Plot_CurvScatter, sc, mode, species, $
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
	
	Bmirror  = [33,47]                   ;nT
	energies = [20, 100, 200, 500]       ;eV
	tf_load = ~keyword_set(no_load)
	if n_elements(species) eq 0 then species = 'e'
	if n_elements(trange)  gt 0 then MrVar_SetTRange, trange
	
	;Get the mass
	case species of
		'e': mass = MrConstants('m_e')
		'i': mass = MrConstants('m_p')
		else: message, 'SPECIES must be {"e" | "i"}.'
	endcase
	
;-------------------------------------------
; Variable Names ///////////////////////////
;-------------------------------------------
	fgm_instr = 'dfg'
	case fgm_instr of
		'afg': fgm_level = 'l2pre'
		'dfg': fgm_level = 'l2pre'
		'fgm': fgm_level = 'l2'
		else:  message, 'Invalid FGM instrument: "' + fgm_instr + '".'
	endcase
	
	level    = 'l2'
	edp_mode = mode eq 'srvy' ? 'fast' : mode
	fgm_mode = mode
	dsp_mode = 'fast'
	fpi_mode = mode eq 'brst' ? mode : 'fast'
	coords   = 'gse'

	;Source names
	bvec_vname    = sc + '_' + fgm_instr + '_bvec_' + coords + '_' + fgm_mode + '_' + fgm_level
	bmag_vname    = sc + '_' + fgm_instr + '_bmag_' + coords + '_' + fgm_mode + '_' + fgm_level
	b1_vec_vname  = 'mms1_'  + fgm_instr + '_bvec_' + coords + '_' + fgm_mode + '_' + fgm_level
	b2_vec_vname  = 'mms2_'  + fgm_instr + '_bvec_' + coords + '_' + fgm_mode + '_' + fgm_level
	b3_vec_vname  = 'mms3_'  + fgm_instr + '_bvec_' + coords + '_' + fgm_mode + '_' + fgm_level
	b4_vec_vname  = 'mms4_'  + fgm_instr + '_bvec_' + coords + '_' + fgm_mode + '_' + fgm_level
	r1_vec_vname  = 'MMS1_DEFEPH_R'
	r2_vec_vname  = 'MMS2_DEFEPH_R'
	r3_vec_vname  = 'MMS3_DEFEPH_R'
	r4_vec_vname  = 'MMS4_DEFEPH_R'
	dist4d_vname  = sc + '_fpi_dist4d_'                + mode     + '_' + level
	pad_vname     = sc + '_fpi_pad_'                   + mode     + '_' + level
	
	;Derived names
	mir1_hi_vname = sc + '_' + fgm_instr + '_mirror1_low_'           + fgm_mode + '_' + fgm_level
	mir1_lo_vname = sc + '_' + fgm_instr + '_mirror1_high_'          + fgm_mode + '_' + fgm_level
	mir2_hi_vname = sc + '_' + fgm_instr + '_mirror2_low_'           + fgm_mode + '_' + fgm_level
	mir2_lo_vname = sc + '_' + fgm_instr + '_mirror2_high_'          + fgm_mode + '_' + fgm_level
	rcurv_vname   = sc + '_' + fgm_instr + '_rcurv_'                 + fgm_mode + '_' + fgm_level
	rerr_vname    = sc + '_' + fgm_instr + '_rerr_'                  + fgm_mode + '_' + fgm_level
	k1_vname      = sc + '_' + fgm_instr + '_k1_'                    + fgm_mode + '_' + fgm_level
	k2_vname      = sc + '_' + fgm_instr + '_k2_'                    + fgm_mode + '_' + fgm_level
	k3_vname      = sc + '_' + fgm_instr + '_k3_'                    + fgm_mode + '_' + fgm_level
	k4_vname      = sc + '_' + fgm_instr + '_k4_'                    + fgm_mode + '_' + fgm_level
	
;-------------------------------------------
; Get Data /////////////////////////////////
;-------------------------------------------
	if tf_load then begin
	;-------------------------------------------
	; FGM //////////////////////////////////////
	;-------------------------------------------
		;B & |B|
		oFGM = MrMMS_FGM_4sc( mode, $
		                      COORD_SYS = 'gse', $
		                      INSTR     = fgm_instr, $
		                      LEVEL     = fgm_level )
		oFGM -> LoadPosition, 'defeph'
		
	;-------------------------------------------
	; FPI //////////////////////////////////////
	;-------------------------------------------
		;Load FPI Data
		oFPI = MrMMS_FPI_Dist( sc, fpi_mode, species, $
		                       COORD_SYS = 'gse', $
		                       LEVEL     = level )
		oFPI -> Load_FAC, fac_type
		
		;4D Distribution
		oDist = oFPI -> GetDist4D(NAME=dist4d_vname, /CACHE)
		
		;Pitch angle distribution
		oPAD  = oDist -> GetThetaSpec( /CACHE, $
		                               E_RANGE = [150,250], $
		                               NAME = pad_vname )
	endif

;-------------------------------------------
; Calculations /////////////////////////////
;-------------------------------------------
	if ~obj_valid(oFGM) then begin
		oFGM = MrMMS_FGM_4sc( b1_vec_vname, b2_vec_vname, b3_vec_vname, b4_vec_vname, $
		                      r1_vec_vname, r2_vec_vname, r3_vec_vname, r4_vec_vname )
	endif

	;Loss cone
	nMirror = n_elements(Bmirror)
	oB     = MrMMS_FGM_BField(BFIELD=bvec_vname)
	
	;Mirror point
	oAngLo = oB -> MirrorAngle(Bmirror[0], NAME=mir1_lo_vname, /CACHE)
	oAngHi = 180.0 - oAngLo
	oAngHi -> SetName, mir1_hi_vname
	oAngHi -> Cache
	
	;Mirror point 2
	if nMirror eq 2 then begin
		oAngLo = oB -> MirrorAngle(Bmirror[1], NAME=mir2_lo_vname, /CACHE)
		oAngHi = 180.0 - oAngLo
		oAngHi -> SetName, mir2_hi_vname
		oAngHi -> Cache
	endif
	
	;Curvature
	oCurv = oFGM -> R_Curvature()
	oCurv = oCurv / (MrConstants('Re') / 1000.0)   ;km -> Earth radii
	oCurv -> SetName, rcurv_vname
	oCurv -> Cache
	
	;Error
	oErr = oFGM -> CurvErr(NAME=rerr_vname, /CACHE)
	
	;Adiabatic parameter
	oK1 = oFGM -> Kappa(energies[0], mass, NAME=k1_vname, /CACHE)
	oK2 = oFGM -> Kappa(energies[1], mass, NAME=k2_vname, /CACHE)
	oK3 = oFGM -> Kappa(energies[2], mass, NAME=k3_vname, /CACHE)
	oK4 = oFGM -> Kappa(energies[3], mass, NAME=k4_vname, /CACHE)

;-------------------------------------------
; Set Properties ///////////////////////////
;-------------------------------------------
	title = strupcase(sc)

	;|B|
	oB = MrVar_Get(bmag_vname)
	oB['AXIS_RANGE'] = [0, oB.max*1.1]
	oB['PLOT_TITLE'] = title
	
	;PAD
	oPAD = MrVar_Get(pad_vname)
	oPA  = MrVar_Get(oPAD['DEPEND_1'])
	oPAD['TITLE'] = 'PSD!C' + oPAD['UNITS']
	oPA['TITLE']  = 'PA!C200eV!C(deg)'
	
	;R CURVE
	oCurv['AXIS_RANGE'] = [oCurv.min < 1e-2, oCurv.max*1.1 > 1e2]
	oCurv['LOG']        = 1B
	oCurv['TITLE']      = 'R$\downc$!C(R$\downe$)'
	
	;Error
	oErr['AXIS_RANGE'] = [oErr.min < 1e-3, oErr.max*1.1 > 1e1]
	oErr['LOG']        = 1B
	oErr['TITLE']      = 'O(L/D)'
	
	;K1
	oK1['AXIS_RANGE'] = [oK4.min < 1e-1, oK1.max*1.1]
	oK1['COLOR']      = 'Blue'
	oK1['LABEL']      = string(energies[0], FORMAT='(i0)') + 'eV'
	oK1['LOG']        = 1
	oK1['TITLE']      = 'K$\up2$'
	
	;K2
	oK2['COLOR'] = 'Green'
	oK2['LABEL'] = string(energies[1], FORMAT='(i0)') + 'eV'
	oK2['LOG']   = 1
	
	;K3
	oK3 = MrVar_Get( k3_vname )
	oK3['COLOR'] = 'Red'
	oK3['LABEL'] = string(energies[2], FORMAT='(i0)') + 'eV'
	oK3['LOG']   = 1
	
	;K4
	oK4 = MrVar_Get( k4_vname )
	oK4['COLOR'] = 'Magenta'
	oK4['LABEL'] = string(energies[3], FORMAT='(i0)') + 'eV'
	oK4['LOG']   = 1

;-------------------------------------------
; Plot Data ////////////////////////////////
;-------------------------------------------
	;Create plot
	win = MrVar_PlotTS( [bmag_vname, pad_vname, rcurv_vname, rerr_vname, k1_vname], $
	                    YSIZE = 700)
	win    -> Refresh, /DISABLE
	
	;Overplot loss cone and scattering parameters
	win = MrVar_OPlotTS( [pad_vname,  pad_vname], [mir1_lo_vname, mir1_hi_vname] )
	win = MrVar_OPlotTS( [k1_vname, k1_vname, k1_vname], $
	                     [k2_vname, k3_vname, k4_vname] )
	if nMirror eq 2 then win = MrVar_OPlotTS( [pad_vname, pad_vname], [mir2_lo_vname, mir2_hi_vname] )
	
	;Legend for scattering parameters
	oL = MrVar_Legend( k1_vname, k2_vname, k3_vname, k4_vname, $
	                   NAME   = 'Lgd: k^2', $
	                   TARGET = win[k1_vname] )
	
	;Draw horizontal lines
	trange = MrVar_GetTRange('SSM')
	oHorz  = MrPlotS( trange, [0.1, 0.1], COLOR='Blue', LINESTYLE='--', NAME='Rerr=0.1', TARGET=win[rerr_vname])
	oHorz  = MrPlotS( trange, [0.5, 0.5], COLOR='Blue', LINESTYLE='--', NAME='Rerr=0.5', TARGET=win[rerr_vname])
	oHorz  = MrPlotS( trange, [10, 10], LINESTYLE='--', NAME='k=10', TARGET=win[k1_vname])
	oHorz  = MrPlotS( trange, [25, 25], LINESTYLE='--', NAME='k=25', TARGET=win[k1_vname])
	
	win.oxmargin = [12,13]
	win[0] -> SetLayout, [1,1]
	win    -> TrimLayout
	win    -> Refresh
	return, win
end