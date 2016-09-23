; docformat = 'rst'
;
; NAME:
;       MrMMS_EDI_Q0_PGA_Overview
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
;
; :Keywords:
;       DT:                 in, optional, type=string/objref, default=2.5
;                           Duration, in seconds, of each time bin.
;       DPA:                in, optional, type=float, default=1.0
;                           Width, in degrees, of each pitch angle bin.
;       DGA:                in, optional, type=float, default=11.25
;                           Width, in degrees, of each gyrophase bin.
;       FAC:                in, optional, type=string, default='EXB' for srvy and 'VXB' for brst
;                           Name of the field-aligned coordinate system used
;                               to define the directions perpendicular to B.
;                               Options include 'VXB' and 'EXB'
;       PARANGE:            out, optional, type=string, default=[80\, 100]
;                           Range of pitch angles over which to bin.
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
;       2064/08/12  -   Written by Matthew Argall
;-
function MrMMS_EDI_Q0_PGA_Overview, sc, mode, $
DT      = dt, $
DPA     = dPA, $
DGA     = dGA, $
FAC     = fac, $
PARANGE = paRange, $
GARANGE = gaRange
	compile_opt idl2
	
;-------------------------------------------
; Get Data /////////////////////////////////
;-------------------------------------------
	
	;EDI, FGM, FPI moments
	if 0 then $
	MrMMS_EDI_Load_PGA, sc, mode, 'q0', $
	                    DT      = dt, $
	                    DPA     = dPA, $
	                    DGA     = dGA, $
	                    FAC     = fac, $
	                    PARANGE = paRange, $
	                    GARANGE = gaRange
	
	;Load E-Field Data
	edp_mode = 'brst'
	MrMMS_Get_Data, sc, 'edp', edp_mode, 'l2', $
	                OPTDESC   = 'dce', $
	                VARFORMAT = '*dce_gse*'

;-------------------------------------------
; Variables to Plot ////////////////////////
;-------------------------------------------
	
	;Create variable names
	prefix = sc + '_'
	suffix = '_' + mode + '_l2'
	
	b_vname         = mode eq 'srvy' ? prefix + 'dfg_srvy_l2pre_gse' : prefix + 'fgm_b_gse' + suffix
	e_vname         = prefix + 'edp_dce_gse'       + '_' + edp_mode + '_l2'
	traj_gdu1_vname = prefix + 'edi_traj_gse_gdu1' + suffix
	traj_gdu2_vname = prefix + 'edi_traj_gse_gdu2' + suffix
	ga_gdu1_vname   = prefix + 'edi_ga_gdu1'       + suffix
	ga_gdu2_vname   = prefix + 'edi_ga_gdu2'       + suffix
	pad_vname       = prefix + 'edi_pad'           + suffix
	gpd_vname       = prefix + 'edi_gpd'           + suffix

;-------------------------------------------
; Separate Components //////////////////////
;-------------------------------------------
	;Extract trajectories
	traj_gdu1 = MrVar_Get(traj_gdu1_vname)
	traj_gdu2 = MrVar_Get(traj_gdu2_vname)
	
	;Variable names
	phi_gdu1_vname   = prefix + '_edi_phi_gse_gdu1'   + suffix
	phi_gdu2_vname   = prefix + '_edi_phi_gse_gdu2'   + suffix
	theta_gdu1_vname = prefix + '_edi_theta_gse_gdu1' + suffix
	theta_gdu2_vname = prefix + '_edi_theta_gse_gdu2' + suffix
	
	;Separate THETA & PHi
	phi_gdu1   = MrScalarTS( traj_gdu1[1,*], NAME=phi_gdu1_vname )
	phi_gdu2   = MrScalarTS( traj_gdu2[1,*], NAME=phi_gdu2_vname )
	theta_gdu1 = MrScalarTS( traj_gdu1[0,*], NAME=theta_gdu1_vname )
	theta_gdu2 = MrScalarTS( traj_gdu2[0,*], NAME=theta_gdu2_vname )
	
	;Set Attributes
	traj_gdu1 -> CopyAttrTo,   phi_gdu1,       'DEPEND_0'
	phi_gdu1  -> SetAttrValue, 'AXIS_RANGE',   [-180.0, 180.0], /CREATE
	phi_gdu1  -> SetAttrValue, 'LABEL',        'Phi GDU1'
	phi_gdu1  -> SetAttrValue, 'LINESTYLE',    'None',          /CREATE
	phi_gdu1  -> SetAttrValue, 'SYMBOL',       17,              /CREATE
	phi_gdu1  -> SetAttrValue, 'COLOR',        'Black',         /CREATE
	phi_gdu1  -> SetAttrValue, 'SYM_COLOR',    'Black',         /CREATE
	phi_gdu1  -> SetAttrValue, 'TICKINTERVAL', 90.0,            /CREATE
	phi_gdu1  -> SetAttrValue, 'TITLE',        '-Look!C(deg)'
	
	traj_gdu2 -> CopyAttrTo,   phi_gdu2,       'DEPEND_0'
	phi_gdu2  -> SetAttrValue, 'AXIS_RANGE',   [-180.0, 180.0], /CREATE
	phi_gdu2  -> SetAttrValue, 'LABEL',        'Phi GDU2'
	phi_gdu2  -> SetAttrValue, 'LINESTYLE',    'None',          /CREATE
	phi_gdu2  -> SetAttrValue, 'SYMBOL',       17,              /CREATE
	phi_gdu2  -> SetAttrValue, 'COLOR',        'Blue',          /CREATE
	phi_gdu2  -> SetAttrValue, 'SYM_COLOR',    'Blue',          /CREATE
	phi_gdu2  -> SetAttrValue, 'TICKINTERVAL', 90.0,            /CREATE
	phi_gdu2  -> SetAttrValue, 'TITLE',        '-Look!C(deg)'
	
	traj_gdu1  -> CopyAttrTo,   theta_gdu1,     'DEPEND_0'
	theta_gdu1 -> SetAttrValue, 'AXIS_RANGE',   [-180.0, 180.0], /CREATE
	theta_gdu1 -> SetAttrValue, 'LABEL',        'Theta GDU1'
	theta_gdu1 -> SetAttrValue, 'LINESTYLE',    'None',          /CREATE
	theta_gdu1 -> SetAttrValue, 'SYMBOL',       17,              /CREATE
	theta_gdu1 -> SetAttrValue, 'COLOR',        'Red',           /CREATE
	theta_gdu1 -> SetAttrValue, 'SYM_COLOR',    'Red',           /CREATE
	theta_gdu1 -> SetAttrValue, 'TICKINTERVAL', 90.0,            /CREATE
	theta_gdu1 -> SetAttrValue, 'TITLE',        '-Look!C(deg)'
	
	traj_gdu2  -> CopyAttrTo,   theta_gdu2,     'DEPEND_0'
	theta_gdu2 -> SetAttrValue, 'AXIS_RANGE',   [-180.0, 180.0], /CREATE
	theta_gdu2 -> SetAttrValue, 'LABEL',        'Theta GDU2'
	theta_gdu2 -> SetAttrValue, 'LINESTYLE',    'None',          /CREATE
	theta_gdu2 -> SetAttrValue, 'SYMBOL',       17,              /CREATE
	theta_gdu2 -> SetAttrValue, 'COLOR',        'Forest Green',  /CREATE
	theta_gdu2 -> SetAttrValue, 'SYM_COLOR',    'Forest Green',  /CREATE
	theta_gdu2 -> SetAttrValue, 'TICKINTERVAL', 90.0,            /CREATE
	theta_gdu2 -> SetAttrValue, 'TITLE',        '-Look!C(deg)'

;-------------------------------------------
; Set Properties ///////////////////////////
;-------------------------------------------
	;Magnetic field
	oB  = MrVar_Get(b_vname)
	oB -> SetAttrValue, 'COLOR',      ['Blue', 'Forest Green', 'Red', 'Black'], /CREATE
	oB -> SetAttrValue, 'PLOT_TITLE', 'EDI Q0 Overview'
	oB -> SetAttrValue, 'TITLE',      'B!C(nT)'
	oB -> SetAttrValue, 'MAX_VALUE',  max(oB['MAX_VALUE'])
	oB -> SetAttrValue, 'MIN_VALUE',  min(oB['MIN_VALUE'])
	
	;Pitch angle distribution
	oPAD  = MrVar_Get(pad_vname)
	oPAD -> SetAttrValue, 'MISSING_VALUE', 0, /CREATE
	
	;Gyrophase distribution
	oGPD  = MrVar_Get(gpd_vname)
	oGPD -> SetAttrValue, 'MISSING_VALUE', 0, /CREATE
	
	;Gyrophase angle GDU1
	oGA_GDU1  = MrVar_Get(ga_gdu1_vname)
	oGA_GDU1 -> SetAttrValue, 'AXIS_RANGE',   [-180.0, 180.0], /CREATE
	oGA_GDU1 -> SetAttrValue, 'LABEL',        'GDU1'
	oGA_GDU1 -> SetAttrValue, 'LINESTYLE',    'None',          /CREATE
	oGA_GDU1 -> SetAttrValue, 'SYMBOL',       17,              /CREATE
	oGA_GDU1 -> SetAttrValue, 'COLOR',        'Red',           /CREATE
	oGA_GDU1 -> SetAttrValue, 'SYM_COLOR',    'Red',           /CREATE
	oGA_GDU1 -> SetAttrValue, 'TICKINTERVAL', 90.0,            /CREATE
	oGA_GDU1 -> SetAttrValue, 'TITLE',        'Gyrophase!C(deg)'
	
	;Gyrophase angle GDU1
	oGA_GDU2  = MrVar_Get(ga_gdu2_vname)
	oGA_GDU2 -> SetAttrValue, 'AXIS_RANGE',   [-180.0, 180.0], /CREATE
	oGA_GDU2 -> SetAttrValue, 'LABEL',        'GDU2'
	oGA_GDU2 -> SetAttrValue, 'LINESTYLE',    'None',          /CREATE
	oGA_GDU2 -> SetAttrValue, 'SYMBOL',       17,              /CREATE
	oGA_GDU2 -> SetAttrValue, 'COLOR',        'Forest Green',  /CREATE
	oGA_GDU2 -> SetAttrValue, 'SYM_COLOR',    'Forest Green',  /CREATE
	oGA_GDU2 -> SetAttrValue, 'TICKINTERVAL', 90.0,            /CREATE
	oGA_GDU2 -> SetAttrValue, 'TITLE',        'Gyrophase!C(deg)'

;-------------------------------------------
; Plot /////////////////////////////////////
;-------------------------------------------
	;Plot the data
	win = MrWindow(YSIZE=800, YGAP=0.5, REFRESH=0)

	;B
	p1 = MrVar_Plot( b_vname, $
	                 /CURRENT, $
	                 DIMENSION   = 2, $
	                 TITLE       = '', $
	                 XTICKFORMAT = '(a1)', $
	                 XTITLE      = '' )
	l1 = MrVar_Legend( b_vname, $
	                   SAMPLE_WIDTH = 0, $
	                   TARGET       = p1 )

	;E
	p2 = MrVar_Plot( e_vname, $
	                 /CURRENT, $
	                 DIMENSION   = 1, $
	                 TITLE       = '', $
	                 XTICKFORMAT = '(a1)', $
	                 XTITLE      = '' )
	
	;Phi, THETA, GDU1
	p3 = MrVar_Plot( phi_gdu2, $
	                 /CURRENT, $
	                 TITLE       = '', $
	                 XTICKFORMAT = '(a1)', $
	                 XTITLE      = '' )
	p4 = MrVar_Plot( theta_gdu2, $
	                 OVERPLOT = p3 )
	l3 = MrVar_Legend( phi_gdu2, theta_gdu2, $
	                   SAMPLE_WIDTH = 0, $
	                   TARGET       = p3 )
	
	;GPD, GDU1/2
	p5 = MrVar_Plot( ga_gdu1_vname, $
	                 /CURRENT, $
	                 TITLE       = '', $
	                 XTICKFORMAT = '(a1)', $
	                 XTITLE      = '' )
	p6 = MrVar_Plot( ga_gdu2_vname, $
	                 OVERPLOT = p5 )
	l5 = MrVar_Legend( ga_gdu1_vname, ga_gdu2_vname, $
	                   SAMPLE_WIDTH = 0, $
	                   TARGET       = p5 )
	
	;PAD
	im1 = MrVar_Image( pad_vname, $
	                   /CURRENT, $
	                   /SCALE, $
	                   TITLE       = '', $
	                   XTICKFORMAT = '(a1)', $
	                   XTITLE      = '' )
	im2 = MrVar_Image(gpd_vname, /SCALE, /CURRENT)

	
	win -> Refresh
	return, win
end