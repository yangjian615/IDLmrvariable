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
;       OUTPUT_DIR: in, optional, type=string, default='~/figures/'
;                   Directory in which to save the figure. If neither `OUTPUT_DIR` or
;                       `OUTPUT_EXT` are given, no file is made.
;       OUTPUT_EXT: in, optional, type=string/strarr, default='png'
;                   Extension (and file type) of the figure. Options include
;                       'png', 'jpeg', 'tiff', 'ps', 'eps', 'pdf'. If neither
;                       `OUTPUT_DIR` or `OUTPUT_EXT` are given, no file is made.
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
function MrMMS_Plot_EDI_d29_EdotJ, sc, $
COORDS=coords, $
LEVEL=level, $
NML_COORDS=nml_coords, $
NO_LOAD=no_load, $
OUTPUT_DIR=output_dir, $
OUTPUT_EXT=output_ext, $
RAGER=rager, $
SUFFIX=suffix, $
TRANGE=trange
	compile_opt idl2
	
	catch, the_error
	if the_error ne 0 then begin
		catch, /CANCEL
		if n_elements(win) gt 0 then obj_destroy, win
		MrPrintF, 'LogErr'
		return, !Null
	endif
	
	mode     = 'brst'
	tf_nml   = N_Elements(nml_coords) GT 0
	tf_rager = Keyword_Set(rager)
	tf_load  = ~Keyword_Set(no_load)
	IF N_Elements(coords) EQ 0 THEN coords = 'dsl'
	IF N_Elements(suffix) EQ 0 THEN suffix = ''
	IF N_Elements(trange) GT 0 THEN MrVar_SetTRange, trange
	
;-------------------------------------------
; Get Data /////////////////////////////////
;-------------------------------------------
	;EDP
	instr    = 'edp'
	level    = 'l2'
	optdesc  = 'dce'
	edp_fast = tf_rager ? 'brst' : 'fast' ;Use brst EDP with 7.5ms moments
	edp_brst = 'brst'
	
	;EDI
	edi_instr  = 'edi'
	edi_coords = coords eq 'dsl' ? 'dbcs' : coords
	
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
		                     SUFFIX    = suffix, $
		                     VARFORMAT = ['*' + fgm_instr + '_' + fgm_mode + '*' + fgm_coords, $
		                                  '*' + fgm_instr + '_b_' + fgm_coords + '*' ]
		
		;EDP E-Field
		MrMMS_Load_Data, sc, instr, 'brst', level, $
		                 OPTDESC   = 'dce', $
		                 SUFFIX    = suffix, $
		                 VARFORMAT = '*_dce_'+coords+'_*'
		
		MrMMS_Load_Data, sc, instr, 'fast', level, $
		                 OPTDESC   = 'dce', $
		                 SUFFIX    = suffix, $
		                 VARFORMAT = '*_dce_'+coords+'_*'
		
		;DATA29
		MrMMS_EDI_Load_EField_Cts, sc, mode, 'data29', $
		                           SUFFIX = suffix
		
;		MrMMS_EDI_Load_D29, sc, mode, $
;		                    SUFFIX = suffix
		
		;30-ms Moments
		IF tf_rager THEN BEGIN
			;DIS
			MrMMS_FPI_Load_Data, sc, fpi_mode, $
			                     LEVEL     = fpi_level, $
			                     OPTDESC   = 'dis-moms', $
			                     SUFFIX    = suffix, $
			                     VARFORMAT = ['*density*', '*bulkv_'+fpi_coords+'*']
			
			;DES
			MrMMS_FPI_Load_7p5ms, sc, species
		ENDIF ELSE BEGIN
			MrMMS_FPI_Load_Data, sc, fpi_mode, $
			                     LEVEL     = fpi_level, $
			                     OPTDESC   = ['des-moms', 'dis-moms'], $
			                     SUFFIX    = suffix, $
			                     VARFORMAT = ['*density*', '*bulkv_'+fpi_coords+'*']
		ENDELSE
	ENDIF

;-------------------------------------------
; Variable Names ///////////////////////////
;-------------------------------------------
	;Source names
	b_vname         = StrJoin( [sc, fgm_instr, 'b',    fgm_coords, fgm_mode, fgm_level], '_' ) + suffix
	bvec_vname      = StrJoin( [sc, fgm_instr, 'bvec', fgm_coords, fgm_mode, fgm_level], '_' ) + suffix
	bmag_vname      = StrJoin( [sc, fgm_instr, 'bmag', fgm_coords, fgm_mode, fgm_level], '_' ) + suffix
	IF ~MrVar_IsCached(b_vname) THEN BEGIN
		b_vname         = StrJoin( [sc, fgm_instr,        fgm_mode, fgm_level, fgm_coords], '_' ) + suffix
		bvec_vname      = StrJoin( [sc, fgm_instr, 'vec', fgm_mode, fgm_level, fgm_coords], '_' ) + suffix
		bmag_vname      = StrJoin( [sc, fgm_instr, 'mag', fgm_mode, fgm_level, fgm_coords], '_' ) + suffix
		
		IF ~MrVar_IsCached(b_vname) $
			THEN Message, 'Unknown FGM variable naming convention.'
	ENDIF
	e_fast_vname    = StrJoin( [sc, instr, optdesc, coords, edp_fast, level], '_') + suffix
	e_brst_vname    = StrJoin( [sc, instr, optdesc, coords, edp_brst, level], '_') + suffix
	ne_vname        = StrJoin( [sc, 'des', 'numberdensity',             fpi_mode], '_') + suffix
	ve_vname        = StrJoin( [sc, 'des', 'bulkv',         fpi_coords, fpi_mode], '_') + suffix
	ni_vname        = StrJoin( [sc, 'dis', 'numberdensity',             fpi_mode], '_') + suffix
	vi_vname        = StrJoin( [sc, 'dis', 'bulkv',         fpi_coords, fpi_mode], '_') + suffix
	d29_gdu1_vname  = StrJoin( [sc, edi_instr, 'flux',               'gdu1',   'd29', mode, 'l1a'], '_' ) + suffix
	d29_gdu2_vname  = StrJoin( [sc, edi_instr, 'flux',               'gdu2',   'd29', mode, 'l1a'], '_' ) + suffix
	traj_gdu1_vname = StrJoin( [sc, edi_instr, 'traj',   edi_coords, 'gdu1',   'd29', mode, 'l1a'], '_' ) + suffix
	traj_gdu2_vname = StrJoin( [sc, edi_instr, 'traj',   edi_coords, 'gdu2',   'd29', mode, 'l1a'], '_' ) + suffix
	ang_diff_vname  = StrJoin( [sc, edi_instr, 'dlook',                        'd29', mode, 'l1a'], '_' ) + suffix
	q_gdu1_vname    = StrJoin( [sc, edi_instr, 'q',                  'gdu1',          mode, 'l1a'], '_' ) + suffix
	q_gdu2_vname    = StrJoin( [sc, edi_instr, 'q',                  'gdu2',          mode, 'l1a'], '_' ) + suffix
	
	;Output names
	e_par_fpi_vname          = StrJoin( [sc, instr, 'e',     'par',   'fpi',   edp_fast, level], '_') + suffix
	e_perp_fpi_vname         = StrJoin( [sc, instr, 'e',     'perp',  'fpi',   edp_fast, level], '_') + suffix
	e_prime_fpi_vname        = StrJoin( [sc, instr, 'e',     'prime', 'fpi',   edp_fast, level], '_') + suffix
	e_prime_perp1_fpi_vname  = StrJoin( [sc, instr, 'e',     'prime', 'perp1', edp_fast, level], '_') + suffix
	e_prime_perp2_fpi_vname  = StrJoin( [sc, instr, 'e',     'prime', 'perp2', edp_fast, level], '_') + suffix
	e_prime_d29_fpi_vname    = StrJoin( [sc, instr, 'e',     'prime', 'd29',   edp_fast, level], '_') + suffix
	e_perp1_fpi_vname        = StrJoin( [sc, instr, 'e',     'perp1', 'fpi',   edp_fast, level], '_') + suffix
	e_perp2_fpi_vname        = StrJoin( [sc, instr, 'e',     'perp2', 'fpi',   edp_fast, level], '_') + suffix
	e_fac_d29_vname          = StrJoin( [sc, instr, 'e',     'fac',   'd29',   edp_brst, level], '_') + suffix
	e_par_d29_vname          = StrJoin( [sc, instr, 'e',     'par',   'd29',   edp_brst, level], '_') + suffix
	e_perp_d29_vname         = StrJoin( [sc, instr, 'e',     'perp',  'd29',   edp_brst, level], '_') + suffix
	e_perp_edi_vname         = StrJoin( [sc, instr, 'e',     'perp',  'edi',   edp_fast, level], '_') + suffix
	j_par_fpi_vname          = StrJoin( [sc, 'fpi', 'j',     'par',            fpi_mode, level], '_') + suffix
	j_perp_fpi_vname         = StrJoin( [sc, 'fpi', 'j',     'perp',           fpi_mode, level], '_') + suffix
	j_perp1_fpi_vname        = StrJoin( [sc, 'fpi', 'j',     'perp1',          fpi_mode, level], '_') + suffix
	j_perp2_fpi_vname        = StrJoin( [sc, 'fpi', 'j',     'perp2',          fpi_mode, level], '_') + suffix
	j_perp_d29_fpi_vname     = StrJoin( [sc, 'fpi', 'j',     'perp',  'd29',   fpi_mode, level], '_') + suffix
	j_perp_d29_vname         = StrJoin( [sc, 'edi', 'j',     'perp',  'd29',   mode,     level], '_') + suffix
	jdote_par_fpi_vname      = StrJoin( [sc, 'fpi', 'jdote', 'par',            edp_fast, level], '_') + suffix
	jdote_perp_fpi_vname     = StrJoin( [sc, 'fpi', 'jdote', 'perp',           edp_fast, level], '_') + suffix
	jdote_perp1_fpi_vname    = StrJoin( [sc, 'fpi', 'jdote', 'perp1',          edp_fast, level], '_') + suffix
	jdote_perp2_fpi_vname    = StrJoin( [sc, 'fpi', 'jdote', 'perp2',          edp_fast, level], '_') + suffix
	jdote_perp_d29_fpi_vname = StrJoin( [sc, 'fpi', 'jdote', 'perp',  'd29',   mode,     level], '_') + suffix
	jdote_perp_d29_vname     = StrJoin( [sc, 'edi', 'jdote', 'perp',  'd29',   mode,     level], '_') + suffix
	jdote_prime_vname        = StrJoin( [sc, 'fpi', 'jdote', 'prime',          mode,     level], '_') + suffix
	jdote_prime_perp1_vname  = StrJoin( [sc, 'fpi', 'jdote', 'prime', 'perp1', mode,     level], '_') + suffix
	jdote_prime_perp2_vname  = StrJoin( [sc, 'fpi', 'jdote', 'prime', 'perp2', mode,     level], '_') + suffix
	jdote_prime_d29_vname    = StrJoin( [sc, 'fpi', 'jdote', 'prime', 'd29',   mode,     level], '_') + suffix
	tce_vname                = StrJoin( [sc, 'edi', 'd29',    'tce'], '_') + suffix
	theta_gdu1_E_vname       = StrJoin( [sc, edi_instr, 'theta', 'gdu1', 'e',      mode, 'l1a'], '_' ) + suffix
	theta_gdu1_perp1_vname   = StrJoin( [sc, edi_instr, 'theta', 'gdu1', 'perp1',  mode, 'l1a'], '_' ) + suffix
	theta_gdu1_nml_vname     = StrJoin( [sc, edi_instr, 'theta', 'gdu1', 'nml',    mode, 'l1a'], '_' ) + suffix
	theta_gdu1_eperp_vname   = StrJoin( [sc, edi_instr, 'theta', 'gdu1', 'eperp',  mode, 'l1a'], '_' ) + suffix

;-------------------------------------------
; Parallel and Perpendicular Directions ////
;-------------------------------------------
	;Incident trajectory of GDU1
	oBvec      = MrVar_Get( bvec_vname )
;	oPerp_d29  = MrMMS_EDI_traj2cart( traj_gdu1_vname )
	oTraj_GDU1 = MrVar_Get( traj_gdu1_vname )
	
	azimuth = oTraj_GDU1['DATA', *, 0] * !dpi / 180D
	polar   = oTraj_GDU1['DATA', *, 1] * !dpi / 180D

	;Cartesian coordinates
	x = Sin(polar) * Cos(azimuth)
	y = Sin(polar) * Sin(azimuth)
	z = Cos(polar)
	
	;Parallel direction
	oPar = oBvec -> Normalize()
	
	;Perpendicular vector
	;   - Convert from incident trajectory to look direction
	oPerp_d29 = MrVectorTS( oTraj_GDU1['TIMEVAR'], Float([ [x], [y], [z] ]) )
	
	;VxB Coordinate system
	oT_fac_fpi = MrVar_FAC(bvec_vname, ve_vname, 'VXB', TIME=ve_vname)
	oPerp_fpi  = oT_fac_fpi[*,*,0]   ; (BxV)xB direction (ExB)
	
	;Normal direction
	IF tf_nml THEN BEGIN
		oNML     = MrMatrixTS( MrVar_GetTRange(), Rebin( Reform(nml_coords, 1, 3, 3), 2, 3, 3 ) )
		oNML_d29 = oNML -> Interpol(oTraj_GDU1['TIMEVAR'])
	ENDIF
	
	;Delete data
	x = !Null
	y = !Null
	z = !Null

;-------------------------------------------
; Epar & E' FPI ////////////////////////////
;-------------------------------------------
	;Grab the fields and electrons
	oB      = MrVar_Get(bvec_vname)
	oE_fast = MrVar_Get(e_fast_vname)
	oNe     = MrVar_Get(ne_vname)
	oVe     = MrVar_Get(ve_vname)
	oT_des  = MrVar_FAC(oB, oVe, 'VXB', TIME=oVe)
	
	;Interpolate B & E to DES
	oB_des        = oB        -> Interpol(oNe)
	oE_des        = oE_fast   -> Interpol(oNe)
	oPerp_d29_des = oPerp_d29 -> Interpol(oNe)
	oPerp_d29_des = oPerp_d29_des -> Normalize()
	
	;E Par & Perp
	ob_hat_des  = oB_des -> Normalize()
	oE_par_des  = oE_des -> Dot(ob_hat_des)
	oE_perp_des = MrVectorTS(oNe['TIMEVAR'], [ [oE_des['DATA',*,0] - oE_des['DATA',*,0]*ob_hat_des['DATA',*,0]], $
	                                           [oE_des['DATA',*,1] - oE_des['DATA',*,1]*ob_hat_des['DATA',*,1]], $
	                                           [oE_des['DATA',*,2] - oE_des['DATA',*,2]*ob_hat_des['DATA',*,2]] ] )
	
	;Ec = -Ve x B
	oE_VxB_des = MrVar_E_VxB(oVe, oB_des)
	
	;E' = E - Ec = E + Ve x B
	oE_prime_des = oE_des - oE_VxB_des
	
	;Rotate to FAC
	oE_prime_fac_des = oT_des ## oE_prime_des
	oE_fac_des       = oT_des ## oE_des
	
	;Component along D29
	oE_prime_d29_des   = oE_prime_des -> Dot(oPerp_d29_des)
	oE_perp_d29_des    = oE_perp_des  -> Dot(oPerp_d29_des)
	
	;Delete data
	Obj_Destroy, [oB_des, oE_des, oE_VxB_des]
	
	;
	; ATTRIBUTES
	;
	
	;Epar DES
	oE_par_des -> SetName, e_par_fpi_vname
	oE_par_des -> Cache
	oE_par_des['CATDESC']    = "Parallel components of the EDP electric field, " + $
	                           'interpolated onto DES time tags.'
	oE_par_des['AXIS_RANGE'] = [ Min( [oE_par_des.min, oE_prime_des.min, oE_prime_d29_des.min] ), $
	                             Max( [oE_par_des.max, oE_prime_des.max, oE_prime_d29_des.max] ) ]
	oE_par_des['LABEL']      = 'E$\down||$'
	oE_par_des['TITLE']      = 'E!C(mV/m)'
	oE_par_des['UNITS']      = 'mV/m'
	
	;Eperp DES
	oE_perp_des -> SetName, e_perp_fpi_vname
	oE_perp_des -> Cache
	oE_perp_des['CATDESC'] = "Perpendicular components of the EDP electric field, " + $
	                         'interpolated onto DES time tags.'
	oE_perp_des['LABEL']   = ['E$\downPerpX$', 'E$\downPerpY$', 'E$\downPerpZ$']
	oE_perp_des['TITLE']   = 'E!C(mV/m)'
	oE_perp_des['UNITS']   = 'mV/m'
	
	;Eperp1 DES
	oE_perp1_des  = oE_fac_des[*,0]
	oE_perp1_des -> SetName, e_perp1_fpi_vname
	oE_perp1_des -> Cache
	oE_perp1_des['CATDESC'] = 'Component of the EDP electric field along the Perp1=' + $
	                          '(BxV)xB direction, interpolated onto DES time tags.'
	oE_perp1_des['COLOR']   = 'Forest Green'
	oE_perp1_des['LABEL']   = 'E$\downPerp1$'
	oE_perp1_des['TITLE']   = 'E!C(mV/m)'
	oE_perp1_des['UNITS']   = 'mV/m'
	
	;Eperp2 DES
	oE_perp2_des  = oE_fac_des[*,1]
	oE_perp2_des -> SetName, e_perp2_fpi_vname
	oE_perp2_des -> Cache
	oE_perp2_des['CATDESC'] = 'Component of the EDP electric field along the Perp2=' + $
	                          'BxV direction, interpolated onto DES time tags.'
	oE_perp2_des['COLOR']   = 'Blue'
	oE_perp2_des['LABEL']   = 'E$\downPerp2$'
	oE_perp2_des['TITLE']   = 'E!C(mV/m)'
	oE_perp2_des['UNITS']   = 'mV/m'
	
	;Eprime DES
	oE_prime_des -> SetName, e_prime_fpi_vname
	oE_prime_des -> Cache
	oE_prime_des['CATDESC'] = "EDP electric field in the electron rest from: E'=E+VexB, " + $
	                          'interpolated onto DES time tags.'
	oE_prime_des['LABEL']   = ["E'$\downX$", "E'$\downY$", "E'$\downZ$"]
	oE_prime_des['TITLE']   = "E'!C(mV/m)"
	oE_prime_des['UNITS']   = 'mV/m'
	
	;Eprime Perp1 DES
	oE_prime_perp1_des  = oE_prime_fac_des[*,0]
	oE_prime_perp1_des -> SetName, e_prime_perp1_fpi_vname
	oE_prime_perp1_des -> Cache
	oE_prime_perp1_des['CATDESC'] = "EDP electric field in the electron rest from: E'=E+VexB, " + $
	                                'along the (BxV)xB direction and interpolated onto DES time tags.'
	oE_prime_perp1_des['COLOR']   = 'Forest Green'
	oE_prime_perp1_des['LABEL']   = "E'$\downPerp1$"
	oE_prime_perp1_des['TITLE']   = "E'!C(mV/m)"
	oE_prime_perp1_des['UNITS']   = 'mV/m'
	
	;Eprime Perp2 DES
	oE_prime_perp2_des  = oE_prime_fac_des[*,1]
	oE_prime_perp2_des -> SetName, e_prime_perp2_fpi_vname
	oE_prime_perp2_des -> Cache
	oE_prime_perp2_des['CATDESC'] = "EDP electric field in the electron rest from: E'=E+VexB, " + $
	                                'along the BxV direction and interpolated onto DES time tags.'
	oE_prime_perp2_des['COLOR']   = 'Blue'
	oE_prime_perp2_des['LABEL']   = "E'$\downPerp2$"
	oE_prime_perp2_des['TITLE']   = "E'!C(mV/m)"
	oE_prime_perp2_des['UNITS']   = 'mV/m'
	
	;Eprime D29 DES
	oE_prime_d29_des -> SetName, e_prime_d29_fpi_vname
	oE_prime_d29_des -> Cache
	oE_prime_d29_des['CATDESC'] = "EDP electric field in the electron rest from: E'=E+VexB, " + $
	                              'along the GDu1 look-direction and interpolated onto DES time tags.'
	oE_prime_d29_des['COLOR']   = 'Red'
	oE_prime_d29_des['LABEL']   = "E'$\downGDU1$"
	oE_prime_d29_des['TITLE']   = "E'!C(mV/m)"
	oE_prime_d29_des['UNITS']   = 'mV/m'
	
;-------------------------------------------
; Jpar & Jperp FPI /////////////////////////
;-------------------------------------------
	;Grab the ions
	oNi = MrVar_Get(ni_vname)
	oVi = MrVar_Get(vi_vname)

	;Interpolate to DES
	oPar_des = oPar -> Interpol(oNe)
	oNi_des  = oNi  -> Interpol(oNe)
	oVi_des  = oVi  -> Interpol(oVe)

	;Compute current density
	;   - 1e15 convets to uA/m^2
	oJ_fpi = 1e15 * MrConstants('q') * oNe * (oVi_des - oVe)

	;Par & Perp
	oJ_par_fpi  = oJ_fpi -> Dot(ob_hat_des)
	oJ_perp_fpi = MrVectorTS(oNe['TIMEVAR'], [ [oJ_fpi['DATA',*,0] - oJ_fpi['DATA',*,0]*ob_hat_des['DATA',*,0]], $
	                                           [oJ_fpi['DATA',*,1] - oJ_fpi['DATA',*,1]*ob_hat_des['DATA',*,1]], $
	                                           [oJ_fpi['DATA',*,2] - oJ_fpi['DATA',*,2]*ob_hat_des['DATA',*,2]] ] )
	
	;FAC
	oJ_fac_fpi = oT_des ## oJ_fpi
	
	;D29-direction
	oJ_perp_d29_fpi = oJ_fpi -> Dot(oPerp_d29_des)
	
	;Perp1 & D29
;	oJ_perp1_fpi    = oJ_fpi -> Dot(oPerp_fpi)
;	oJ_perp_d29_fpi = oJ_fpi -> Dot(oPerp_d29_des)
	
	;Destroy data
	Obj_Destroy, [ob_hat_des, oPerp_d29_des, oNi_des, oVi_des, oT_des]
	
	;
	; ATTRIBUTES
	;
	
	;Jpar FPI
	oJ_par_fpi -> SetName, j_par_fpi_vname
	oJ_par_fpi -> Cache
	oJ_par_fpi['CATDESC']    = 'Parallel component of the FPI current density.'
	oJ_par_fpi['AXIS_RANGE'] = [ Min( [oJ_par_fpi.min, oJ_perp_fpi.min, oJ_fac_fpi.min] ), $
	                             Max( [oJ_par_fpi.max, oJ_perp_fpi.max, oJ_fac_fpi.max] ) ]
	oJ_par_fpi['LABEL']      = 'J$\down||$'
	oJ_par_fpi['TITLE']      = 'J!C($\mu$J/m^2)'
	oJ_par_fpi['UNITS']      = '\muA/m^2'
	
	;Jperp FPI
	oJ_perp_fpi -> SetName, j_perp_fpi_vname
	oJ_perp_fpi -> Cache
	oJ_perp_fpi['CATDESC'] = 'Perpendicular components of the FPI current density.'
	oJ_perp_fpi['LABEL']   = ['J$\downPerpX$', 'J$\downPerpy$', 'J$\downPerpZ$']
	oJ_perp_fpi['TITLE']   = 'J!C($\mu$A/m^2)'
	oJ_perp_fpi['UNITS']   = '\muA/m^2'
	
	;Jperp1 FPI
	oJ_perp1_fpi  = oJ_fac_fpi[*,0]
	oJ_perp1_fpi -> SetName, j_perp1_fpi_vname
	oJ_perp1_fpi -> Cache
	oJ_perp1_fpi['CATDESC'] = 'Component of the FPI current density along the Perp1=' + $
	                          '(BxV)xB direction.'
	oJ_perp1_fpi['COLOR']   = 'Forest Green'
	oJ_perp1_fpi['LABEL']   = 'J$\downPerp1$'
	oJ_perp1_fpi['TITLE']   = 'J!C($\mu$J/m^2)'
	oJ_perp1_fpi['UNITS']   = '\muA/m^2'
	
	;Jperp1 FPI
	oJ_perp2_fpi  = oJ_fac_fpi[*,1]
	oJ_perp2_fpi -> SetName, j_perp2_fpi_vname
	oJ_perp2_fpi -> Cache
	oJ_perp2_fpi['CATDESC'] = 'Component of the FPI current density along the Perp2=' + $
	                          'BxV direction.'
	oJ_perp2_fpi['COLOR']   = 'Blue'
	oJ_perp2_fpi['LABEL']   = 'J$\downPerp2$'
	oJ_perp2_fpi['TITLE']   = 'J!C($\mu$J/m^2)'
	oJ_perp2_fpi['UNITS']   = '\muA/m^2'
	
	;Jperp D29 FPI
	oJ_perp_d29_fpi -> SetName, j_perp_d29_fpi_vname
	oJ_perp_d29_fpi -> Cache
	oJ_perp_d29_fpi['CATDESC'] = 'Component of the FPI current density along the GDU1 look-direction.'
	oJ_perp_d29_fpi['COLOR']   = 'Red'
	oJ_perp_d29_fpi['LABEL']   = 'J$\downGDU1$'
	oJ_perp_d29_fpi['TITLE']   = 'J!C($\mu$J/m^2)'
	oJ_perp_d29_fpi['UNITS']   = '\muA/m^2'

;-------------------------------------------
; Epar & Eperp D29 /////////////////////////
;-------------------------------------------
	oE_brst   = MrVar_Get(e_brst_vname)
	oD29_GDU1 = MrVar_Get(d29_gdu1_vname)
	oD29_GDU2 = MrVar_Get(d29_gdu2_vname)
	
	;Transformation to FAC/EDI frame
	oT = MrVar_FAC( bvec_vname, oPerp_d29, TIME=oPerp_d29 )
	
	;Interpolate to D29
	oPar_d29 = oPar    -> Interpol(oD29_GDU1)
	oE_d29   = oE_brst -> Interpol(oD29_GDU1)
	
	;Perpendicular field
	oE_perp_edi      = oE_d29 -> Copy()
	oE_perp_edi[*,0] = oE_d29[*,0] - oE_d29[*,0]*oPar_d29[*,0]
	oE_perp_edi[*,1] = oE_d29[*,1] - oE_d29[*,1]*oPar_d29[*,1]
	oE_perp_edi[*,2] = oE_d29[*,2] - oE_d29[*,2]*oPar_d29[*,2]
	
	;Par & Perp
	;   - x = Perp1 - EDI
	;   - y = Perp2
	;   - z = Par
	oE_fac_d29  = oT ## oE_d29
	oE_par_d29  = oE_d29 -> Dot(oPar_d29)
	oE_perp_d29 = oE_d29 -> Dot(oPerp_d29)
	
	;Destroy data
	Obj_Destroy, oPar_d29
	
	;
	; ATTRIBUTES
	;
	
	;Eperp
	oE_perp_edi -> SetName, e_perp_edi_vname
	oE_perp_edi -> Cache
	oE_perp_edi['CATDESC'] = 'EDP perpendicular electric field. Generated by interpolating ' + $
	                         'the magnetic and electric fields onto the EDI time stamps ' + $
	                         'then subtracting the parallel component of E from the total ' + $
	                         'electric field: Eperp = E - (Ex*bx, Ey*by, Ez*bz).'
	oE_perp_edi['LABEL']   = 'E' + ['x', 'y', 'z'] + '_perp'
	oE_perp_edi['TITLE']   = 'Eperp!C(mV/m)'
	oE_perp_edi['UNITS']   = 'mV/m'
	
	;E FAC DATA29
	oE_fac_d29 -> SetName, e_fac_d29_vname
	oE_fac_d29 -> Cache
	oE_fac_d29['CATDESC']    = 'EDP electric field interpolated to EDI time stamps then ' + $
	                           'rotated into a coordinate system in which z is the direction ' + $
	                           'parallel to B, y is formed by crossing z with the EDI look ' + $
	                           'direction, and x completes the right-handed system.'
	oE_fac_d29['LABEL']      = 'E$\down' + ['Perp1', 'Perp2', '||'] + '$'
	oE_fac_d29['TITLE']      = 'E!C(mV/m)'
	oE_fac_d29['UNITS']      = 'mV/m'
	
	;Epar DATA29
	oE_par_d29 -> SetName, e_par_d29_vname
	oE_par_d29 -> Cache
	oE_par_d29['CATDESC']    = 'Parallel component of the EDP electric field interpolated ' + $
	                           'onto the Data29 time stamps.'
	oE_par_d29['AXIS_RANGE'] = [oE_par_d29.min, oE_par_d29.max]
	oE_par_d29['LABEL']      = 'Epar'
	oE_par_d29['TITLE']      = 'E!C(mV/m)'
	oE_par_d29['UNITS']      = 'mV/m'
	
	;Eperp DATA29
	oE_perp_d29 -> SetName, e_perp_d29_vname
	oE_perp_d29 -> Cache
	oE_perp_d29['CATDESC'] = 'Component of the EDP electric field along the GDU1 look-' + $
	                         'direction, interpolated onto the Data29 time stamps.'
	oE_perp_d29['COLOR']   = 'Black'
	oE_perp_d29['LABEL']   = 'E$\downGDU1$'
	oE_perp_d29['TITLE']   = 'E!C(mV/m)'
	oE_perp_d29['UNITS']   = 'mV/m'
	
;-------------------------------------------
; Jperp Data29 /////////////////////////////
;-------------------------------------------
	
	;Compute current density
	;   - 1e10 converts to uA/m^2
	oJ_perp_d29 = -1e10 * MrConstants('q') * (oD29_GDU1 - oD29_GDU2)
	
	;
	; ATTRIBUTES
	;
	
	;JPERP D29
	oJ_perp_D29 -> SetName, j_perp_d29_vname
	oJ_perp_D29 -> Cache
	oJ_perp_D29['CATDESC'] = 'Current density derived from DATA29.'
	oJ_perp_D29['COLOR']   = 'Red'
;	oJ_perp_D29['LABEL']   = 'J$\downGDU1$'
	oJ_perp_D29['TITLE']   = 'Jperp!Cdata29!C($\mu$A/m^2)'
	oJ_perp_D29['UNITS']   = '\mu A/m^2'
	
;-------------------------------------------
; Let Time = 1 / Fce ///////////////////////
;-------------------------------------------
	;Get the magnetic field at Data29 cadence
;	oBmag     = MrVar_Get(bmag_vname)
;	oBmag_d29 = oBmag -> Interpol(oD29_GDU1)
	
	;Cyclotron frequency
;	oFce = MrVar_Freq_Cyclotron(oBmag_d29, 'm_e')
	
	;Number of cyclotron periods between samples
;	oTime = oD29_GDU1['TIMEVAR']
;	dt    = oTime[1:*, 'SSM'] - oTime -> GetData('SSM')
;	Tce   = dt * oFce[0:-1]
	
	;Create variable
;	oTce = MrTimeSeries( oTime[0:-2], Tce, $
;	                     /CACHE, $
;	                     NAME = tce_vname )

;-------------------------------------------
; J.E //////////////////////////////////////
;-------------------------------------------

	;FPI
	oJdotE_par_fpi      = oE_par_des      * oJ_par_fpi
	oJdotE_perp_fpi     = oE_perp_des     * oJ_perp_fpi
	oJdotE_perp1_fpi    = oE_perp1_des    * oJ_perp1_fpi
	oJdotE_perp2_fpi    = oE_perp2_des    * oJ_perp2_fpi
	oJdotE_perp_d29_fpi = oE_perp_d29_des * oJ_perp_d29_fpi

	;E PRIME
	oJdotEprime_fpi          = oE_prime_des       * oJ_fpi
	oJdotEprime_perp1_fpi    = oE_prime_perp1_des * oJ_perp1_fpi
	oJdotEprime_perp2_fpi    = oE_prime_perp2_des * oJ_perp2_fpi
	oJdotEprime_perp_d29_fpi = oE_prime_d29_des   * oJ_perp_d29_fpi
	
	;D29
	oJdotE_perp_d29 = oE_perp_d29 * oJ_perp_d29
	
	;
	; ATTRIBUTES
	;
	
	;J.E PAR FPI
	oJdotE_par_fpi -> SetName, jdote_par_fpi_vname
	oJdotE_par_fpi -> Cache
	oJdotE_par_fpi['CATDESC']    = 'Energy dissipation J.E in the parallel direction from FPI.'
	oJdotE_par_fpi['AXIS_RANGE'] = [ Min( [oJdotE_par_fpi.min, oJdotEprime_perp1_fpi.min, oJdotEprime_perp_d29_fpi.min] ), $
	                                 Max( [oJdotE_par_fpi.max, oJdotEprime_perp1_fpi.max, oJdotEprime_perp_d29_fpi.max] ) ]
	oJdotE_par_fpi['LABEL']      = '(J.E)$\down||$'
	oJdotE_par_fpi['TITLE']      = 'J.E!C(nW/m^3)'
	oJdotE_par_fpi['UNITS']      = 'nW/m^3'
	
	;J.E PERP FPI
	oJdotE_perp_fpi -> SetName, jdote_perp_fpi_vname
	oJdotE_perp_fpi -> Cache
	oJdotE_perp_fpi['CATDESC'] = 'Energy dissipation J.E in the perpendicular directions from FPI.'
	oJdotE_perp_fpi['COLOR']   = 'Red'
	oJdotE_perp_fpi['LABEL']   = '(J.E)$\downGDU1$'
	oJdotE_perp_fpi['TITLE']   = 'J.E!C(nW/m^3)'
	oJdotE_perp_fpi['UNITS']   = 'nW/m^3'
	
	;J.E PERP1 FPI
	oJdotE_perp1_fpi -> SetName, jdote_perp1_fpi_vname
	oJdotE_perp1_fpi -> Cache
	oJdotE_perp1_fpi['CATDESC'] = 'Energy dissipation J.E in the Perp1=(BxV)xB direction from FPI.'
	oJdotE_perp1_fpi['COLOR']   = 'Forest Green'
	oJdotE_perp1_fpi['LABEL']   = '(J.E)$\downPerp1$'
	oJdotE_perp1_fpi['TITLE']   = 'J.E!C(nW/m^3)'
	oJdotE_perp1_fpi['UNITS']   = 'nW/m^3'
	
	;J.E PERP2 FPI
	oJdotE_perp2_fpi -> SetName, jdote_perp2_fpi_vname
	oJdotE_perp2_fpi -> Cache
	oJdotE_perp2_fpi['CATDESC'] = 'Energy dissipation J.E in the Perp2=BxV direction from FPI.'
	oJdotE_perp2_fpi['COLOR']   = 'Blue'
	oJdotE_perp2_fpi['LABEL']   = '(J.E)$\downPerp2$'
	oJdotE_perp2_fpi['TITLE']   = 'J.E!C(nW/m^3)'
	oJdotE_perp2_fpi['UNITS']   = 'nW/m^3'
	
	;J.E D29 FPI
	oJdotE_perp_d29_fpi -> SetName, jdote_perp_d29_fpi_vname
	oJdotE_perp_d29_fpi -> Cache
	oJdotE_perp_d29_fpi['CATDESC'] = 'Energy dissipation J.E in the GDU1 look-direction from FPI.'
	oJdotE_perp_d29_fpi['COLOR']   = 'Red'
	oJdotE_perp_d29_fpi['LABEL']   = '(J.E)$\downGDU1$'
	oJdotE_perp_d29_fpi['TITLE']   = 'J.E!C(nW/m^3)'
	oJdotE_perp_d29_fpi['UNITS']   = 'nW/m^3'
	
	;J.E'
	oJdotEprime_fpi -> SetName, jdote_prime_vname
	oJdotEprime_fpi -> Cache
	oJdotEprime_fpi['CATDESC'] = "Energy dissipation J.E' in the electron rest frame along " + $
	                             'the Perp1=(BxV)xB direction, by FPI.'
	oJdotEprime_fpi['LABEL']   = ["J.E'$\downX", "J.E'$\downY", "J.E'$\downZ"]
	oJdotEprime_fpi['TITLE']   = "J.E'!C(nW/m^3)"
	oJdotEprime_fpi['UNITS']   = 'nW/m^2'
	
	;J.E' Perp1
	oJdotEprime_perp1_fpi -> SetName, jdote_prime_perp1_vname
	oJdotEprime_perp1_fpi -> Cache
	oJdotEprime_perp1_fpi['CATDESC'] = "Energy dissipation J.E' in the electron rest frame along " + $
	                                   'the Perp1=(BxV)xB direction, by FPI.'
	oJdotEprime_perp1_fpi['COLOR']   = 'Forest Green'
	oJdotEprime_perp1_fpi['LABEL']   = "(J.E')$\downPerp1$"
	oJdotEprime_perp1_fpi['TITLE']   = "J.E'$\downPerp1$!C(nW/m^3)"
	oJdotEprime_perp1_fpi['UNITS']   = 'nW/m^2'
	
	;J.E' Perp2
	oJdotEprime_perp2_fpi -> SetName, jdote_prime_perp2_vname
	oJdotEprime_perp2_fpi -> Cache
	oJdotEprime_perp2_fpi['CATDESC'] = "Energy dissipation J.E' in the electron rest frame along " + $
	                                   'the Perp2=BxV direction, by FPI.'
	oJdotEprime_perp2_fpi['COLOR']   = 'Blue'
	oJdotEprime_perp2_fpi['LABEL']   = "(J.E')$\downPerp2$"
	oJdotEprime_perp2_fpi['TITLE']   = "J.E'$\downPerp2$!C(nW/m^3)"
	oJdotEprime_perp2_fpi['UNITS']   = 'nW/m^2'
	
	;J.E' D29
	oJdotEprime_perp_d29_fpi -> SetName, jdote_prime_d29_vname
	oJdotEprime_perp_d29_fpi -> Cache
	oJdotEprime_perp_d29_fpi['CATDESC'] = "Energy dissipation J.E' in the electron rest frame along " + $
	                                      'the GDU1 look-direction, by FPI.'
	oJdotEprime_perp_d29_fpi['COLOR']   = 'Red'
	oJdotEprime_perp_d29_fpi['LABEL']   = "(J.E')$\downGDU1$"
	oJdotEprime_perp_d29_fpi['TITLE']   = "J.E'$\downD29$!C(nW/m^3)"
	oJdotEprime_perp_d29_fpi['UNITS']   = 'nW/m^2'
	
	;J.E DATA29
	oJdotE_perp_d29 -> SetName, jdote_perp_d29_vname
	oJdotE_perp_d29 -> Cache
	oJdotE_perp_d29['CATDESC'] = 'Energy dissipation J.E in the GDU1 look-direction from EDI.'
	oJdotE_perp_d29['COLOR']   = 'Red'
	oJdotE_perp_d29['TITLE']   = 'J.E!CData29!C(nW/m^3)'
	oJdotE_perp_d29['UNITS']   = 'nW/m^2'

;-------------------------------------------
; Angle Between GDU1, (BxV)xB, and E ///////
;-------------------------------------------
	;Angular difference
	;   - Between GDU1 and ExB direction
	;   - Between GDU1 and E
	;   - Between GDU1 and the normal direction
	oPerp_FPI_d29   = oPerp_fpi -> Interpol(oPerp_d29)
	oE_d29_hat      = oE_d29 -> Normalize()
	oE_perp_edi_hat = oE_perp_edi -> Normalize()
	theta_d29_perp1 = ACos( (oPerp_d29 -> Dot(oPerp_FPI_d29))['DATA'] ) * !radeg
	theta_d29_E     = ACos( (oPerp_d29 -> Dot(oE_d29_hat))['DATA'] ) * !radeg
	theta_Eperp_edi = ACos( (oPerp_d29 -> Dot(oE_perp_edi_hat))['DATA'] ) * !radeg

	IF tf_nml THEN BEGIN
;		oPerp_NML = oNML_d29 ## oPerp_d29
;		phi       = ATan( oPerp_NML['DATA',*,1], oPerp_NML['DATA',*,0] )
;		theta     = ACos( oPerp_NML['DATA',*,2] )
		theta_d29_N = ACos( (oPerp_d29 -> Dot(oNML_d29[*,*,0]))['DATA'] ) * !radeg
		theta_d29_M = ACos( (oPerp_d29 -> Dot(oNML_d29[*,*,1]))['DATA'] ) * !radeg
		theta_d29_L = ACos( (oPerp_d29 -> Dot(oNML_d29[*,*,2]))['DATA'] ) * !radeg
		
		;Create a variable
		oTheta_d29_NML = MrTimeSeries( oPerp_d29['TIMEVAR'], [ [theta_d29_N], [theta_d29_M], [theta_d29_L] ], $
		                               /CACHE, $
		                               NAME = theta_gdu1_nml_vname, $
		                               /NO_COPY )
		oTheta_d29_NML['COLOR']     = ['Blue', 'Magenta', 'Red']
		oTheta_d29_NML['LABEL']     = '$\theta$$\down' + ['N', 'M', 'L'] + '$'
		oTheta_d29_NML['LINESTYLE'] = '--'
		oTheta_d29_NML['TITLE']     = 'GDU1$\downNML$'
		oTheta_d29_NML['UNITS']     = 'degrees'
		
;		oTheta_d29_NML = MrTimeSeries( oPerp_d29['TIMEVAR'], [ [phi], [theta] ] * !radeg, $
;		                               /CACHE, $
;		                               NAME = theta_gdu1_nml_vname, $
;		                               /NO_COPY )
;		oTheta_d29_NML['COLOR'] = ['Blue', 'Red']
;		oTheta_d29_NML['LABEL'] = ['$\phi$', '$\theta$']
;		oTheta_d29_NML['TITLE'] = 'GDU1$\downNML$'
;		oTheta_d29_NML['UNITS'] = 'degrees'
	ENDIF

	;Create a variable
	oTheta_d29_perp1 = MrScalarTS( oPerp_d29['TIMEVAR'], theta_d29_perp1, $
	                               /CACHE, $
	                               NAME = theta_gdu1_perp1_vname, $
	                               /NO_COPY )
	
	;Create a variable
	oTheta_d29_E = MrScalarTS( oPerp_d29['TIMEVAR'], theta_d29_E, $
	                           /CACHE, $
	                           NAME = theta_gdu1_E_vname, $
	                           /NO_COPY )
	
	;Create a variable
	oTheta_d29_Eperp = MrScalarTS( oPerp_d29['TIMEVAR'], theta_Eperp_edi, $
	                               /CACHE, $
	                               NAME = theta_gdu1_eperp_vname, $
	                               /NO_COPY )

	;Free memory
	Obj_Destroy, [oPerp_FPI_d29, oE_d29, oE_d29_hat, oE_perp_edi_hat]
	
	;THETA GDU1-E
	oTheta_d29_E['AXIS_RANGE'] = [0, 180]
	oTheta_d29_E['COLOR']      = 'Purple'
	oTheta_d29_E['LABEL']      = '$\theta$$\downE$'
	oTheta_d29_E['TITLE']      = '$\Delta$$\theta$$\downGDU1$!C(deg)'
	oTheta_d29_E['UNITS']      = 'degrees'
	
	;THETA GDU1-EPERP
	oTheta_d29_Eperp['AXIS_RANGE'] = [0, 180]
	oTheta_d29_Eperp['COLOR']      = 'Purple'
	oTheta_d29_Eperp['LABEL']      = '$\theta$$\downEperp$'
	oTheta_d29_Eperp['TITLE']      = '$\Delta$$\theta$$\downGDU1$!C(deg)'
	oTheta_d29_Eperp['UNITS']      = 'degrees'
	
	;THETA GDU1-(BxV)xB
	oTheta_d29_perp1['AXIS_RANGE'] = [0, 180]
	oTheta_d29_perp1['COLOR']      = 'Forest Green'
	oTheta_d29_perp1['LABEL']      = '$\theta$$\downPerp1$'
	oTheta_d29_perp1['TITLE']      = '$\Delta$$\theta$$\downGDU1$!C(deg)'
	oTheta_d29_perp1['UNITS']      = 'degrees'

;-------------------------------------------
; Set Attributes ///////////////////////////
;-------------------------------------------
	;B
	oB = MrVar_Get(b_vname)
	oB['PLOT_TITLE'] = StrUpCase(sc)
	
	;Data29 GDU1
	oD29_GDU1['AXIS_RANGE'] = [ 1e4, 1e8 ]
	oD29_GDU1['LABEL']      = 'GDU1'
	oD29_GDU1['LOG']        = 1B
	oD29_GDU1['TITLE']      = 'Flux!Cdata29!C(cm$\up-2$s$\up-1$)'
	oD29_GDU1['UNITS']      = 'cm^-2 s^-1'
	
	;Data29 GDU2
	oD29_GDU2['COLOR'] = 'Blue'
	oD29_GDU2['LABEL'] = 'GDU2'
	oD29_GDU2['TITLE'] = 'Flux!Cdata29!C(cm$\up-2$s$\up-1$)'
	oD29_GDU2['UNITS'] = 'cm^-2 s^-1'
	
	;THETA GDU1-GDU2
	oDAng = MrVar_Get(ang_diff_vname)
	oDAng['AXIS_RANGE'] = [0, 180]
	oDAng['COLOR']      = 'Black'
	oDAng['LABEL']      = '$\Theta$$\downGDU2$'
	oDAng['TITLE']      = '$\Delta$$\theta$$\downGDU1$!C(deg)'
	oDAng['UNITS']      = 'degrees'
	
	;THETA N
;	IF tf_nml THEN BEGIN
;		oTheta_d29_N['AXIS_RANGE'] = [0, 180]
;		oTheta_d29_N['COLOR']      = 'Magenta'
;		oTheta_d29_N['LABEL']      = '$\Theta$$\downN$'
;		oTheta_d29_N['TITLE']      = '$\Delta$$\theta$$\downN$!C(deg)'
;		oTheta_d29_N['UNITS']      = 'degrees'
;	ENDIF
	
	;Q GDU1
	oQ = MrVar_Get(q_gdu1_vname)
	oQ['AXIS_RANGE']   = [-0.5, 3.5]
	oQ['LABEL']        = 'GDU1'
	oQ['TICKINTERVAL'] = 1
	oQ['TICKFORMAT']   = '(i1)'
	oQ['TITLE']        = 'Quality'
	
	;Q GDU2
	oQ = MrVar_Get(q_gdu2_vname)
	oQ['COLOR'] = 'Blue'
	oQ['LABEL'] = 'GDU2'
	oQ['TITLE'] = 'Quality'
	
	;TCE
;	oTce['PLOT_TITLE'] = 'Cyclotron Periods per Sample'
;	oTce['UNITS']      = 'cycles/sec'
;	oTce['TITLE']      = '# fce!C(cyc/sample)'

;-------------------------------------------
; Plot Data ////////////////////////////////
;-------------------------------------------
	;Plot variables
	win = MrVar_PlotTS( [ b_vname, e_perp_d29_vname, d29_gdu1_vname, q_gdu1_vname, ang_diff_vname, $
	                      j_perp_d29_vname, j_par_fpi_vname, jdote_perp_d29_vname, jdote_par_fpi_vname ], $
	                    /NO_REFRESH, $
	                    YSIZE=750 )

	;Overplot
	win = MrVar_OPlotTS( e_perp_d29_vname, [e_prime_d29_fpi_vname, e_prime_perp1_fpi_vname, e_prime_perp2_fpi_vname] )
	win = MrVar_OPlotTS( [ d29_gdu1_vname, q_gdu1_vname ], [ d29_gdu2_vname, q_gdu2_vname ] )
	win = MrVar_OPlotTS( ang_diff_vname,      [theta_gdu1_Eperp_vname, theta_gdu1_perp1_vname] )
	win = MrVar_OPlotTS( j_par_fpi_vname,     [j_perp_d29_fpi_vname, j_perp1_fpi_vname, j_perp2_fpi_vname] )
	win = MrVar_OPlotTS( jdote_par_fpi_vname, [jdote_prime_d29_vname, jdote_prime_perp1_vname, jdote_prime_perp2_vname] )
	IF tf_nml THEN win = MrVar_OPlotTS( ang_diff_vname, theta_gdu1_nml_vname )

	;Create a polygon for flux errors
	oT            = oD29_GDU1['TIMEVAR']
	t_ssm         = oT['DATA', 'SSM']
	x_poly1       = [t_ssm, Reverse(t_ssm)] - Floor(t_ssm[0])
	y_temp        = oD29_GDU1['DATA']
	dyp_temp      = (oD29_GDU1['DELTA_PLUS_VAR'])['DATA']
	dym_temp      = (oD29_GDU1['DELTA_MINUS_VAR'])['DATA']
	y_poly1       = [y_temp + dyp_temp, Reverse(y_temp - dym_temp)]
	poly1         = MrPolygon( x_poly1, y_poly1, /DATA, $
	                           LINESTYLE  = 6, $
	                           FILL_COLOR = 'Medium Gray', $
	                           NAME       = 'Poly: ' + d29_gdu1_vname, $
	                           TARGET     = win[d29_gdu1_vname] )
	
	;GDU2
	oT            = oD29_GDU2['TIMEVAR']
	t_ssm         = oT['DATA', 'SSM']
	x_poly2       = [t_ssm, Reverse(t_ssm)] - Floor(t_ssm[0])
	y_temp        = oD29_GDU2['DATA']
	dyp_temp      = (oD29_GDU2['DELTA_PLUS_VAR'])['DATA']
	dym_temp      = (oD29_GDU2['DELTA_MINUS_VAR'])['DATA']
	y_poly2       = [y_temp + dyp_temp, Reverse(y_temp - dym_temp)]
	poly2         = MrPolygon( x_poly2, y_poly2, /DATA, $
	                           LINESTYLE  = 6, $
	                           FILL_COLOR = 'Powder Blue', $
	                           NAME       = 'Poly: ' + d29_gdu2_vname, $
	                           TARGET     = win[d29_gdu2_vname] )
	
	
;	win2  = MrVar_PlotTS(d29_gdu1_vname)
;	win2[0].setlayout, [1,1]
;	win2.trimlayout
;	poly2 = MrPolygon(x_poly1, y_poly1, /DATA, TARGET=win[d29_gdu1_vname])
;	win2[2] -> Order, /SEND_TO_BACK
;	win2[1].xrange = [0.15,0.3]
;	win2[1].ylog = 0 
;	win2[1].yrange = [1e4,2e7]
;	win2[0].fill_color = 'red'
	
	win[0] -> SetLayout, [1,1]
	win    -> TrimLayout
	win.OXMargin = [14,10]

;-------------------------------------------
; Save Figure //////////////////////////////
;-------------------------------------------
	IF N_Elements(output_dir) GT 0 || N_Elements(output_ext) GT 0 THEN BEGIN
		;Defaults
		IF N_Elements(output_dir) EQ 0 THEN output_dir = FilePath( '', ROOT_DIR=File_Search('~', /TEST_DIRECTORY), SUBDIRECTORY='figures' )
		IF N_Elements(output_ext) EQ 0 THEN output_ext = 'png'
		
		;Date and time range of plot
		ftime  = MrVar_GetTRange()
		dstart = StrJoin( StrSplit( StrMid(ftime[0],  0, 10), '-', /EXTRACT ) )
		dend   = StrJoin( StrSplit( StrMid(ftime[1],  0, 10), '-', /EXTRACT ) )
		tstart = StrJoin( StrSplit( StrMid(ftime[0], 11,  8), ':', /EXTRACT ) )
		tend   = StrJoin( StrSplit( StrMid(ftime[1], 11,  8), ':', /EXTRACT ) )
		
		;Sub-seconds time interval?
		ftime  = MrVar_GetTRange('SSM')
		dtime  = ftime[1] - ftime[0]
		pow    = ALog10(dtime)
		IF pow LT 0 THEN BEGIN
			fmt    = '(f0.' + String( Ceil(Abs(pow)), FORMAT='(i0)') + ')'
			tstart = tstart + 'p' + String(ftime[0] MOD 1, FORMAT=fmt)
			tend   = tend   + 'p' + String(ftime[1] MOD 1, FORMAT=fmt)
		ENDIF
		
		;Time of file
		ftime = dstart + '_' + tstart
		IF dend NE dstart THEN ftime += '_' + dend
		ftime += '_' + tend
		
		;Using 7.5ms moments?
		moms = tf_rager ? '-rager' : ''
		
		;File name
		fname  = StrJoin( [sc, edi_instr, mode, level, 'd29-jdote'+moms, ftime], '_' )
		fname += '.' + output_ext
		fname  = FilePath( fname, ROOT_DIR=output_dir )
		
		;Save the figure
		FOR i = 0, N_Elements(fname) - 1 DO win -> Save, fname
	ENDIF

;-------------------------------------------
; Done! ////////////////////////////////////
;-------------------------------------------
	win -> refresh
	return, win
end