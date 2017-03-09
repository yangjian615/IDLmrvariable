; docformat = 'rst'
;
; NAME:
;       MrMMS_Plot_EDI_Q0
;
;*****************************************************************************************
;   Copyright (c) 2017, Matthew Argall                                                   ;
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
;   Generate a plot of EDP quantities:
;       1. FGM Bxyz
;       2. EDP Exyz
;       3. GDU Phi
;       4. GDU Theta
;       5. FAC Phi
;       6. FAC Theta
;       7. PAD
;       8. GPD
;
; :Categories:
;   MMS
;
; :Params:
;       SC:         in, required, type=string
;                   Spacecraft ID. Options are {'mms1' | 'mms2' | 'mms3' | 'mms4' }
;       MODE:       in, required, type=string, default='srvy'
;                   Data telemetry rate of the data. Options are: { 'slow' | 'fast' | 'srvy' | 'brst' }
;
; :Keywords:
;       FGM_INSTR:  in, optional, type=string, default='fgm'
;                   FGM instrument to use. Options are: { 'afg' | 'dfg' | 'fgm' }
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
FUNCTION MrMMS_Plot_EDI_Q0, sc, mode, $
FGM_INSTR=fgm_instr, $
LEVEL=level, $
NO_LOAD=no_load, $
TRANGE=trange
	Compile_Opt idl2
	
	Catch, the_error
	IF the_error NE 0 THEN BEGIN
		Catch, /CANCEL
		IF N_Elements(win) GT 0 THEN Obj_Destroy, win
		MrPrintF, 'LogErr'
		RETURN, !Null
	ENDIF
	
	tf_load = ~Keyword_Set(no_load)
	IF N_Elements(mode)   EQ 0 THEN mode       = 'srvy'
	IF N_Elements(level)  EQ 0 THEN level      = 'l2'
	IF N_Elements(trange) GT 0 THEN MrVar_SetTRange, trange
	
;-------------------------------------------
; Variable Names ///////////////////////////
;-------------------------------------------
	instr      = 'edi'
	optdesc    = 'q0'
	coords     = 'gse'
	fgm_instr  = 'dfg'
	fgm_level  = 'l2pre'
	fgm_mode   = mode eq 'brst' ? mode : 'srvy'
	fgm_coords = coords EQ 'dbcs'  ? 'dmpa' : coords
	edp_instr  = 'edp'
	edp_mode   = mode eq 'brst' ? mode : 'fast'
	edp_coords = coords EQ 'dbcs'  ? 'dsl'  : coords

	;Source names
	IF fgm_level EQ 'l2pre' THEN BEGIN
		fgm_b_vname     = StrJoin( [sc, fgm_instr,        fgm_mode, fgm_level, fgm_coords], '_' )
		fgm_bvec_vname  = StrJoin( [sc, fgm_instr, 'vec', fgm_mode, fgm_level, fgm_coords], '_' )
		fgm_bmag_vname  = StrJoin( [sc, fgm_instr, 'mag', fgm_mode, fgm_level, fgm_coords], '_' )
	ENDIF ELSE BEGIN
		fgm_b_vname     = StrJoin( [sc, fgm_instr, 'b',    fgm_coords, fgm_mode, fgm_level], '_' )
		fgm_bvec_vname  = StrJoin( [sc, fgm_instr, 'bvec', fgm_coords, fgm_mode, fgm_level], '_' )
		fgm_bmag_vname  = StrJoin( [sc, fgm_instr, 'bmag', fgm_coords, fgm_mode, fgm_level], '_' )
	ENDELSE
	e_vname         = StrJoin( [sc, edp_instr, 'dce',  edp_coords, edp_mode, level], '_' )
	cts_gdu1_vname  = StrJoin( [sc, instr, 'counts', 'gdu1', mode, level], '_' )
	cts_gdu2_vname  = StrJoin( [sc, instr, 'counts', 'gdu2', mode, level], '_' )
	traj_gdu1_vname = StrJoin( [sc, instr, 'traj',   coords, 'gdu1', mode, level], '_' )
	traj_gdu2_vname = StrJoin( [sc, instr, 'traj',   coords, 'gdu2', mode, level], '_' )

	;Derived names
	phi_gdu1_vname       = StrJoin( [sc, instr, 'phi',   coords, 'gdu1', mode, level], '_' )
	phi_gdu2_vname       = StrJoin( [sc, instr, 'phi',   coords, 'gdu2', mode, level], '_' )
	theta_gdu1_vname     = StrJoin( [sc, instr, 'theta', coords, 'gdu1', mode, level], '_' )
	theta_gdu2_vname     = StrJoin( [sc, instr, 'theta', coords, 'gdu2', mode, level], '_' )
	phi_fac_gdu1_vname   = StrJoin( [sc, instr, 'phi',   'fac',  'gdu1', mode, level], '_' )
	phi_fac_gdu2_vname   = StrJoin( [sc, instr, 'phi',   'fac',  'gdu2', mode, level], '_' )
	theta_fac_gdu1_vname = StrJoin( [sc, instr, 'theta', 'fac',  'gdu1', mode, level], '_' )
	theta_fac_gdu2_vname = StrJoin( [sc, instr, 'theta', 'fac',  'gdu2', mode, level], '_' )
	pad_vname            = StrJoin( [sc, instr, 'pad', mode, level], '_' )
	gpd_vname            = StrJoin( [sc, instr, 'gpd', mode, level], '_' )

;-------------------------------------------
; Get Data /////////////////////////////////
;-------------------------------------------
	IF tf_load THEN BEGIN
		;FGM
		MrMMS_FGM_Load_Data, sc, fgm_mode, $
		                     INSTR     = fgm_instr, $
		                     LEVEL     = fgm_level, $
		                     VARFORMAT = fgm_b_vname

		;EDP
		MrMMS_Load_Data, sc, edp_instr, edp_mode, level, $
		                 OPTDESC   = 'dce', $
		                 VARFORMAT = e_vname

		;EDI
		MrMMS_Load_Data, sc, 'edi', mode, level, $
		                 OPTDESC   = optdesc, $
		                 VARFORMAT = [ cts_gdu1_vname, cts_gdu2_vname, $
		                               traj_gdu1_vname, traj_gdu2_vname ]
	ENDIF

;-------------------------------------------
; Extract Data in GSE //////////////////////
;-------------------------------------------
	;Get Data
	oTraj_GDU1 = MrVar_Get(traj_gdu1_vname)
	oTraj_GDU2 = MrVar_Get(traj_gdu2_vname)
	
	;Extract Phi & Theta
	oPhi_GDU1   = MrScalarTS( oTraj_GDU1['TIMEVAR'], oTraj_GDU1[*,0], NAME=phi_gdu1_vname,   /CACHE)
	oPhi_GDU2   = MrScalarTS( oTraj_GDU2['TIMEVAR'], oTraj_GDU2[*,0], NAME=phi_gdu2_vname,   /CACHE)
	oTheta_GDU1 = MrScalarTS( oTraj_GDU1['TIMEVAR'], oTraj_GDU1[*,1], NAME=theta_gdu1_vname, /CACHE)
	oTheta_GDU2 = MrScalarTS( oTraj_GDU2['TIMEVAR'], oTraj_GDU2[*,1], NAME=theta_gdu2_vname, /CACHE)

;-------------------------------------------
; Create the Distributions /////////////////
;-------------------------------------------
	;Create the distribution object and set its data
	oEDI  = MrMMS_EDI_Dist()
	oEDI -> SetData, [cts_gdu1_vname, cts_gdu2_vname], [traj_gdu1_vname, traj_gdu2_vname], [1,1], [1,2], [90,90]
	oEDI -> SetFAC, fgm_bvec_vname, e_vname, 'EXB'
	
	;PAD
	oPAD = oEDI -> ThetaSpec(/CACHE, NAME=pad_vname, THETA_RANGE=[80,100], DTHETA=0.1)
	oGPD = oEDI -> PhiSpec(/CACHE, NAME=gpd_vname)

;-------------------------------------------
; Extract FAC Angles ///////////////////////
;-------------------------------------------
	;Get the data
	oEDI -> GetData, !Null, oPhi_FAC_GDU1, oTheta_FAC_GDU1, GDU=1
	oEDI -> GetData, !Null, oPhi_FAC_GDU2, oTheta_FAC_GDU2, GDU=2
	
	;Set Names
	oPhi_FAC_GDU1   -> SetName, phi_fac_gdu1_vname
	oPhi_FAC_GDU2   -> SetName, phi_fac_gdu2_vname
	oTheta_FAC_GDU1 -> SetName, theta_fac_gdu1_vname
	oTheta_FAC_GDU2 -> SetName, theta_fac_gdu2_vname
	
	;Cache
	oPhi_FAC_GDU1   -> Cache
	oPhi_FAC_GDU2   -> Cache
	oTheta_FAC_GDU1 -> Cache
	oTheta_FAC_GDU2 -> Cache
	
	;Clean up
	Obj_Destroy, oEDI

;-------------------------------------------
; Properties ///////////////////////////////
;-------------------------------------------
	;BMAG
	oB = MrVar_Get(fgm_b_vname)
	oB['PLOT_TITLE'] = StrUpCase( StrJoin( [sc, mode, level, optdesc[0]], ' ' ) )
	
	;E FIELD
	oE = MrVar_Get(e_vname)
	oE['TITLE'] = 'E!C(mV/m)'
	
	;CTS GDU1
	oCts_GDU1 = MrVar_Get(cts_gdu1_vname)
	oCts_GDU2 = MrVar_Get(cts_gdu2_vname)
	oCts_GDU1['AXIS_RANGE'] = [1, Max( [oCts_GDU1.max, oCts_GDU2.max] )]
	oCts_GDU1['COLOR']      = 'Blue'
	oCts_GDU1['LABEL']      = 'GDU1'
	
	;CTS GDU2
	oCts_GDU2['COLOR']      = 'Red'
	oCts_GDU2['LABEL']      = 'GDU2'
	
	;PHI GDU1
	oPhi_GDU1['AXIS_RANGE']   = [-180, 180]
	oPhi_GDU1['COLOR']        = 'Blue'
	oPhi_GDU1['LABEL']        = 'GDU1'
	oPhi_GDU1['LINESTYLE']    = 'None'
	oPhi_GDU1['PLOT_TITLE']   = 'Azimuthal angle of electron angle of incidence into GDU1'
	oPhi_GDU1['SYMBOL']       = 'Dot'
	oPhi_GDU1['TICKINTERVAL'] = 90
	oPhi_GDU1['TITLE']        = 'Phi!C' + coords + '!C(deg)'
	oPhi_GDU1['UNITS']        = 'deg'
	
	;PHI GDU2
	oPhi_GDU2['COLOR']      = 'Red'
	oPhi_GDU2['LABEL']      = 'GDU2'
	oPhi_GDU2['LINESTYLE']  = 'None'
	oPhi_GDU2['PLOT_TITLE'] = 'Azimuthal angle of electron angle of incidence into GDU2'
	oPhi_GDU2['SYMBOL']     = 'Dot'
	oPhi_GDU2['TITLE']      = 'Phi!C' + coords + '!C(deg)'
	oPhi_GDU2['UNITS']      = 'deg'
	
	;THETA GDU1
	oTheta_GDU1['COLOR']      = 'Blue'
	oTheta_GDU1['LABEL']      = 'GDU1'
	oTheta_GDU1['LINESTYLE']  = 'None'
	oTheta_GDU1['PLOT_TITLE'] = 'Polar angle of electron angle of incidence into GDU1'
	oTheta_GDU1['TITLE']      = 'Theta!C' + coords + '!C(deg)'
	oTheta_GDU1['SYMBOL']     = 'Dot'
	oTheta_GDU1['UNITS']      = 'deg'
	
	;THETA GDU2
	oTheta_GDU2['COLOR']      = 'Red'
	oTheta_GDU2['LABEL']      = 'GDU2'
	oTheta_GDU2['LINESTYLE']  = 'None'
	oTheta_GDU2['PLOT_TITLE'] = 'Polar angle of electron angle of incidence into GDU2'
	oTheta_GDU2['SYMBOL']     = 'Dot'
	oTheta_GDU2['TITLE']      = 'Theta (GDU)!C(deg)'
	oTheta_GDU2['UNITS']      = 'deg'
	
	;PHI FAC GDU1
	oPhi_FAC_GDU1['AXIS_RANGE']   = [-180, 180]
	oPhi_FAC_GDU1['COLOR']        = 'Blue'
	oPhi_FAC_GDU1['LABEL']        = 'GDU1'
	oPhi_FAC_GDU1['LINESTYLE']    = 'None'
	oPhi_FAC_GDU1['PLOT_TITLE']   = 'Gyrophase angle of electron angle from GDU1'
	oPhi_FAC_GDU1['SYMBOL']       = 'Dot'
	oPhi_FAC_GDU1['TICKINTERVAL'] = 90
	oPhi_FAC_GDU1['TITLE']        = 'Gyrophase!C(deg)'
	oPhi_FAC_GDU1['UNITS']        = 'deg'
	
	;PHI FAC GDU2
	oPhi_FAC_GDU2['COLOR']      = 'Red'
	oPhi_FAC_GDU2['LABEL']      = 'GDU2'
	oPhi_FAC_GDU2['LINESTYLE']  = 'None'
	oPhi_FAC_GDU2['PLOT_TITLE'] = 'Gyrophase angle of electron angle from GDU2'
	oPhi_FAC_GDU2['SYMBOL']     = 'Dot'
	oPhi_FAC_GDU2['TITLE']      = 'Gyrophase!C(deg)'
	oPhi_FAC_GDU2['UNITS']      = 'deg'
	
	;THETA FAC GDU1
	oTheta_FAC_GDU1['AXIS_RANGE'] = [80,100]
	oTheta_FAC_GDU1['COLOR']      = 'Blue'
	oTheta_FAC_GDU1['LABEL']      = 'GDU1'
	oTheta_FAC_GDU1['LINESTYLE']  = 'None'
	oTheta_FAC_GDU1['PLOT_TITLE'] = 'Pitch angle of electron angle from GDU1'
	oTheta_FAC_GDU1['TITLE']      = 'Pitch!C(deg)'
	oTheta_FAC_GDU1['SYMBOL']     = 'Dot'
	oTheta_FAC_GDU1['UNITS']      = 'deg'
	
	;THETA FAC GDU2
	oTheta_FAC_GDU2['COLOR']      = 'Red'
	oTheta_FAC_GDU2['LABEL']      = 'GDU2'
	oTheta_FAC_GDU2['LINESTYLE']  = 'None'
	oTheta_FAC_GDU2['PLOT_TITLE'] = 'Pitch angle of electron angle from GDU2'
	oTheta_FAC_GDU2['SYMBOL']     = 'Dot'
	oTheta_FAC_GDU2['TITLE']      = 'Pitch!C(deg)'
	oTheta_FAC_GDU2['UNITS']      = 'deg'
	
	;Gyrophase Distribution
	oPhi_GDP = oGPD['DEPEND_1']
	oPhi_GDP['AXIS_RANGE']   = [-180,180]
	oPhi_GDP['TICKINTERVAL'] = 90
	oGPD['AXIS_RANGE']       = [1, oGPD.Max]
	oGPD['LOG']              = 1B
	oGPD['TITLE']            = 'Counts'
	
	;Pitch Angle Distribution
	oTheta = oPAD['DEPEND_1']
	oTheta['TITLE']    = 'Pitch!C(deg)'
	oPAD['AXIS_RANGE'] = [1, oPAD.Max]
	oPAD['LOG']        = 1B
	oPAD['TITLE']      = 'Counts'
	
	;Pitch Angle Distribution
	oPhi = oGPD['DEPEND_1']
	oPhi['TITLE'] = 'Gyrophase!C(deg)'
	
;-------------------------------------------
; Plot Data ////////////////////////////////
;-------------------------------------------
	;Plot data
	win = MrVar_PlotTS( [fgm_b_vname, e_vname, cts_gdu1_vname, theta_gdu1_vname, phi_gdu1_vname, $
	                     theta_fac_gdu1_vname, phi_fac_gdu1_vname, pad_vname, gpd_vname], $
	                    /NO_REFRESH, $
	                    XSIZE = 680, $
	                    YSIZE = 700 )
	
	win = MrVar_OPlotTS( cts_gdu1_vname,       cts_gdu2_vname )
	win = MrVar_OPlotTS( phi_gdu1_vname,       phi_gdu2_vname )
	win = MrVar_OPlotTS( theta_gdu1_vname,     theta_gdu2_vname )
	win = MrVar_OPlotTS( phi_fac_gdu1_vname,   phi_fac_gdu2_vname )
	win = MrVar_OPlotTS( theta_fac_gdu1_vname, theta_fac_gdu2_vname )

	;Pretty-up the window
	win[0] -> SetLayout, [1,1]
	win    -> TrimLayout
	win    -> SetProperty, OXMARGIN=[13, 14]
	win    -> Refresh

	RETURN, win
END