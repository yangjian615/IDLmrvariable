; docformat = 'rst'
;
; NAME:
;       MrMMS_Plot_DES
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
;   Generate a plot of FPI distribution functions:
;       1. Par-Perp1
;       2. Par-Perp2
;       3. Perp1-Perp2
;       4. Cuts of Par, Perp1, Perp2
;       5. Cuts of Par, Perp, Anti-Par
;
; :Categories:
;   MMS
;
; :Params:
;       SC:         in, required, type=string
;                   Spacecraft ID. Options are {'mms1' | 'mms2' | 'mms3' | 'mms4' }
;       MODE:       in, required, type=string, default='srvy'
;                   Data telemetry rate of the data. Options are: { 'slow' | 'fast' | 'srvy' | 'brst' }
;       SPECIES:    in, required, type=string, default='e'
;                   Particle species. Options are: { 'e' | 'i' }
;
; :Keywords:
;       FGM_INSTR:  in, optional, type=string, default='dfg'
;                   FGM instrument to use. Options are: { 'afg' | 'dfg' | 'fgm' }
;       LEVEL:      in, optional, type=string, default='l2'
;                   Data quality level. Options are: {'l1a' | 'l1b' | 'ql' | 'l2pre' | 'l2'}
;       OPTDESC:    in, optional, type=string, default=''
;                   Optional filename descriptor.
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
;       2017/02/10  -   Written by Matthew Argall
;-
FUNCTION MrMMS_Plot_FPI_Dist, sc, mode, species, $
FGM_INSTR=fgm_instr, $
LEVEL=level, $
OPTDESC=optdesc, $
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
	theta_range = [-90.0,  90.0]
	phi_range   = [  0.0, 360.0]
	IF N_Elements(level)   EQ 0 THEN level   = 'l2'
	IF N_Elements(mode)    EQ 0 THEN mode    = 'fast'
	IF N_Elements(species) EQ 0 THEN species = 'e'
	IF N_Elements(trange)  GT 0 THEN MrVar_SetTRange, trange
	
;-------------------------------------------
; Variable Names ///////////////////////////
;-------------------------------------------
	instr   = 'd' + species + 's'
	optdesc = instr + '-dist'
	IF N_Elements(coords) EQ 0 THEN BEGIN
		CASE level OF
			'ql': coords = 'dbcs'
			ELSE: coords = 'gse'
		ENDCASE
	ENDIF
	IF N_Elements(fgm_instr) EQ 0 THEN BEGIN
		CASE level OF
			'l2': fgm_instr = 'fgm'
			ELSE: fgm_instr = 'dfg'
		ENDCASE
	ENDIF
	fgm_level  = fgm_instr EQ 'fgm'  ? 'l2'   : 'l2pre'
	fgm_coords = coords    EQ 'dbcs' ? 'dmpa' : coords
	fgm_mode   = mode      EQ 'brst' ? mode   : 'srvy'

	;Source names
	b_vname      = StrJoin( [sc, fgm_instr, 'b',     fgm_coords, fgm_mode, level], '_' )
	bvec_vname   = StrJoin( [sc, fgm_instr, 'bvec',  fgm_coords, fgm_mode, level], '_' )
	bmag_vname   = StrJoin( [sc, fgm_instr, 'bmag',  fgm_coords, fgm_mode, level], '_' )
	dist_vname   = StrJoin( [sc, instr, 'dist', mode], '_' )
	v_vname      = StrJoin( [sc, instr, 'bulkv', coords, mode], '_' )
	
	;
	; Derived names
	;
	
	;2D Time-Series Distributions
	par_perp1_vname     = StrJoin( [sc, instr, 'dist', 'par_perp1',               mode], '_' )
	par_perp2_vname     = StrJoin( [sc, instr, 'dist', 'par_perp2',               mode], '_' )
	perp1_perp2_vname   = StrJoin( [sc, instr, 'dist', 'perp1_perp2',             mode], '_' )
	par_perp_anti_vname = StrJoin( [sc, instr, 'dist', 'par_perp_anti', 'vspace', mode], '_' )
	
	;2D Distributions
	pp1_vname           = StrJoin( [sc, instr, 'dist', 'par_perp1',     'vspace', mode], '_' )
	pp2_vname           = StrJoin( [sc, instr, 'dist', 'par_perp2',     'vspace', mode], '_' )
	p1p2_vname          = StrJoin( [sc, instr, 'dist', 'perp1_perp2',   'vspace', mode], '_' )
	ppa_vname           = StrJoin( [sc, instr, 'dist', 'par_perp_anti', 'espace', mode], '_' )
	
	;1D Distributions
	par_vname           = pp1_vname  + '_x'
	perp1_vname         = p1p2_vname + '_x'
	perp2_vname         = p1p2_vname + '_y'
	ppa_0_vname         = ppa_vname  + '_0'
	ppa_90_vname        = ppa_vname  + '_90'
	ppa_180_vname       = ppa_vname  + '_180'

;-------------------------------------------
; Get Data /////////////////////////////////
;-------------------------------------------
	IF tf_load THEN BEGIN
		;FGM
		MrMMS_FGM_Load_Data, sc, fgm_mode, $
		                     INSTR     = fgm_instr, $
		                     LEVEL     = fgm_level, $
		                     VARFORMAT = b_vname

		;DIST
		;   - The distribution is corrected for internally
		;     generated photoelectrons.
		MrMMS_FPI_Load_Dist3d, sc, mode, species, $
		                       APPLY_MODEL=0, $
		                       COORD_SYS = coords, $
		                       LEVEL     = level
		
		;MOMENTS
		MrMMS_FPI_Load_Data, sc, mode, $
		                     OPTDESC   = instr+'-moms', $
		                     VARFORMAT = '*bulkv_'+coords+'*'
	ENDIF

	;Grab the distribution
	oDist      = MrVar_Get(dist_vname)
	theMass    = species EQ 'i' ? 'm_H' : 'm_e'
	theSpecies = species EQ 'i' ? 'H' : 'e'

;-----------------------------------------------------
; FAC Cartesian Grid \\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\
;-----------------------------------------------------

	;Create a cartesian grid
	MrVar_Grid_MakeCart, oDist['DEPEND_1'], oDist['DEPEND_2'], oX, oY, oZ, /DEGREES

	;Field-Aligned Coordinate System
	oT_fac = MrVar_FAC( bvec_vname, v_vname, 'VXB', $
	                    TIME = oDist['TIMEVAR'] )

	;Rotate the cartesian grid to field-aligned coordinates
	;   - Negative sign converts from look-dirction to incident trajectory
	MrVar_Grid_Cart2FAC, oT_fac, -temporary(oX), -temporary(oY), -temporary(oZ), oX1, oX2, oX3
	
	
;-------------------------------------------
; Par-Perp1 Distribution ///////////////////
;-------------------------------------------
	;Rotate to FAC
	;   - X = PERP1 = (B x V) x B
	;   - Y = PERP2 = B x V
	;   - Z = PAR   = B
	;   - ORIENTATION = 7
	;       THETA = Elevation angle from zx-plane
	;       PHI   = Positive from z-axis
	orientation = 7

	;Convert from instrument coordinates to field-aligned coordinates
	MrVar_Grid_cart2sphere, oX1, oX2, oX3, oPhi, oTheta, $
	                        /DEGREES, $
	                        ORIENTATION = orientation

	;Create new distribution
	oTemp = MrDist4D( oDist['DEPEND_0'], oDist, oPhi, oTheta, oDist['DEPEND_3'], $
	                  /ELEVATION, $
	                  SPECIES = theSpecies )
	
	;Integrate over the PERP2 direction
	oParPerp1 = oTemp -> PhiE( THETA_RANGE=theta_range, $
	                           /CACHE, $
	                           NAME = par_perp1_vname )
	
	Obj_Destroy, [oPhi, oTheta, oTemp]
	
;-------------------------------------------
; Par-Perp2 Distribution ///////////////////
;-------------------------------------------
	;Rotate to FAC
	;   - X = PERP1 = (B x V) x B
	;   - Y = PERP2 = B x V
	;   - Z = PAR   = B
	;   - ORIENTATION = 4
	;       THETA = Elevation angle from yz-plane
	;       PHI   = Positive from z-axis
	orientation = 12

	;Convert from instrument coordinates to field-aligned coordinates
	MrVar_Grid_cart2sphere, oX1, oX2, oX3, oPhi, oTheta, $
	                        /DEGREES, $
	                        ORIENTATION = orientation
	
	;Create new distribution
	oTemp = MrDist4D( oDist['DEPEND_0'], oDist, oPhi, oTheta, oDist['DEPEND_3'], $
	                  /ELEVATION, $
	                  SPECIES = theSpecies )
	
	;Integrate over the PERP2 direction
	oParPerp2 = oTemp -> PhiE( THETA_RANGE=theta_range, $
	                           /CACHE, $
	                           NAME = par_perp2_vname )
	
	Obj_Destroy, [oPhi, oTheta, oTemp]
	
;-------------------------------------------
; Perp1-Perp2 Distribution /////////////////
;-------------------------------------------
	;Rotate to FAC
	;   - X = PERP1 = (B x V) x B
	;   - Y = PERP2 = B x V
	;   - Z = PAR   = B
	;   - ORIENTATION = 3
	;       THETA = Elevation angle from xy-plane
	;       PHI   = Positive from x-axis
	orientation = 3

	;Convert from instrument coordinates to field-aligned coordinates
	MrVar_Grid_cart2sphere, oX1, oX2, oX3, oPhi, oTheta, $
	                        /DEGREES, $
	                        ORIENTATION = orientation
	
	;Create new distribution
	oTemp = MrDist4D( oDist['DEPEND_0'], oDist, oPhi, oTheta, oDist['DEPEND_3'], $
	                  /ELEVATION, $
	                  SPECIES = theSpecies )
	
	;Integrate over the PERP2 direction
	oPerp1Perp2 = oTemp -> PhiE( THETA_RANGE=theta_range, $
	                             /CACHE, $
	                             NAME = perp1_perp2_vname )
	
	Obj_Destroy, [oPhi, oTheta, oTemp]
	
;-------------------------------------------
; Par-Perp-AntiPar /////////////////////////
;-------------------------------------------
	;Rotate to FAC
	;   - X = PERP1 = (B x V) x B
	;   - Y = PERP2 = B x V
	;   - Z = PAR   = B
	;   - ORIENTATION = 3
	;       THETA - Polar angle from z-axis
	;       PHI   - Positive from x-axis
	orientation = 1

	;Convert from instrument coordinates to field-aligned coordinates
	MrVar_Grid_cart2sphere, temporary(oX1), temporary(oX2), temporary(oX3), oPhi, oTheta, $
	                        /DEGREES, $
	                        ORIENTATION = orientation
	
	;Create new distribution
	oTemp = MrDist4D( oDist['DEPEND_0'], oDist, oPhi, oTheta, oDist['DEPEND_3'], $
	                  SPECIES = theSpecies )
	
	;Integrate over the entire perpendicular plane
	oParPerpAnti = oTemp -> ThetaE( PHI_RANGE = [-180.0, 180.0], $
	                                /CACHE, $
	                                NAME      = par_perp_anti_vname )

	Obj_Destroy, [oPhi, oTheta, oTemp]
	
;-------------------------------------------
; Velocity-Space Distributions /////////////
;-------------------------------------------
	;Determine which distribution to plot
	t0    = MrCDF_Epoch_Compute(2015, 10, 16, 13, 07, 02, 165, /TT2000)
	oTime = oParPerp1['TIMEVAR']
	idx   = oTime -> Value_Locate(t0, 'TT2000') + 1
	
	;2D Distributions
	oPP1  = MrVar_Dist2D_Prep(oParPerp1,    theMass, idx, /CACHE, NAME=pp1_vname)
	oPP2  = MrVar_Dist2D_Prep(oParPerp2,    theMass, idx, /CACHE, NAME=pp2_vname)
	oP1P2 = MrVar_Dist2D_Prep(oPerp1Perp2,  theMass, idx, /CACHE, NAME=p1p2_vname)
	oPPA  = MrVar_Dist2D_Prep(oParPerpAnti, theMass, idx, /CACHE, NAME=ppa_vname)

	;1D Distributions
	MrVar_Dist1D_Prep, oPar, !Null,       oParPerp1,    theMass, idx, /CACHE
	MrVar_Dist1D_Prep, oP1,  oP2,         oPerp1Perp2,  theMass, idx, /CACHE
	MrVar_Dist1D_Prep, oE0,  oE90, oE180, oParPerpAnti, theMass, idx, /CACHE, /POLAR

	;Set Names
	oPar  -> SetName, par_vname
	oP1   -> SetName, perp1_vname
	oP2   -> SetName, perp2_vname
	oE0   -> SetName, ppa_0_vname
	oE90  -> SetName, ppa_90_vname
	oE180 -> SetName, ppa_180_vname
	
;-------------------------------------------
; Properties ///////////////////////////////
;-------------------------------------------
	vrange = [-1.5e4,  1.5e4]
	frange = [1e-30, 1e-25]
	
	;Pick out the distribution of interest
	time  = oParPerp1['TIME']
	MrTimeParser, (Temporary(time))[idx], '%Y-%M-%dT%H:%m:%S%f', '%Y-%M-%d %H:%m:%S%f', title
	title = StrMid(title, 0, StrLen(title)-6)
	
	;PAR-PERP1
	oV   = oPP1['DEPEND_0']
	oPhi = oPP1['DEPEND_1']
	oPP1['AXIS_RANGE']    = frange
	oPP1['MISSING_VALUE'] = 0B
	oPP1['MISSING_COLOR'] = 'Grey'
	oPP1['PLOT_TITLE']    = title
	oPP1['TITLE']         = 'PSD'
	oPhi['TITLE']         = 'V$\downperp1$!C(km/s)'
	oV['AXIS_RANGE']      = vrange
	oV['TITLE']           = 'V$\downpar$(km/s)'
	
	;PAR-PERP2
	oV   = oPP2['DEPEND_0']
	oPhi = oPP2['DEPEND_1']
	oPP2['AXIS_RANGE']    = frange
	oPP2['MISSING_VALUE'] = 0B
	oPP2['MISSING_COLOR'] = 'Grey'
	oPP2['TITLE']         = 'PSD'
	oPhi['TITLE']         = 'V$\downperp2$!C(km/s)'
	oV['AXIS_RANGE']      = vrange
	oV['TITLE']           = 'V$\downpar$(km/s)'
	
	;PERP1-PERP2
	oV   = oP1P2['DEPEND_0']
	oPhi = oP1P2['DEPEND_1']
	oP1P2['AXIS_RANGE']    = frange
	oP1P2['MISSING_VALUE'] = 0B
	oP1P2['MISSING_COLOR'] = 'Grey'
	oP1P2['TITLE']         = 'PSD'
	oPhi['TITLE']          = 'V$\downperp1$!C(km/s)'
	oV['AXIS_RANGE']       = vrange
	oV['TITLE']            = 'V$\downperp2$(km/s)'
	
	;PAR
	oV = oPar['DEPEND_0']
	oPar['AXIS_RANGE'] = frange
	oPar['LABEL']      = 'Par'
	oPar['TITLE']      = 'PSD!C(' + oPar['UNITS'] + ')'
	oV['AXIS_RANGE']   = vrange
	
	;PERP1
	oP1['COLOR']  = 'Blue'
	oP1['LABEL']  = 'Perp1'
	
	;PERP2
	oP2['COLOR'] = 'Red'
	oP2['LABEL'] = 'Perp2'
	
	;E0
	oE0['COLOR'] = 'Black'
	oE0['LABEL'] = '0'
	
	;E90
	oE90['COLOR'] = 'Red'
	oE90['LABEL'] = '90'
	
	;E180
	oE180['COLOR'] = 'Blue'
	oE180['LABEL'] = '180'

;-------------------------------------------
; Plot Data ////////////////////////////////
;-------------------------------------------
	;Window
	win = MrWindow(ASPECT=1.0, LAYOUT=[2,3], OXMARGIN=[12,5], XGAP=15, XSIZE=650, YGAP=4, YSIZE=600, REFRESH=0)

	;Plot the distributions
	im1 = MrVar_Image( pp1_vname,    /CURRENT, LAYOUT=[1,1] )
	im2 = MrVar_Image( pp2_vname,    /CURRENT, LAYOUT=[1,2] )
	im3 = MrVar_Image( p1p2_vname,   /CURRENT, LAYOUT=[1,3] )
	p1  = MrVar_Plot( par_vname,     /CURRENT, LAYOUT=[2,1] )
	p2  = MrVar_Plot( perp1_vname,   /CURRENT, OVERPLOT=p1 )
	p3  = MrVar_Plot( perp2_vname,   /CURRENT, OVERPLOT=p1 )
	p4  = MrVar_Plot( ppa_0_vname,   /CURRENT, LAYOUT=[2,2] )
	p5  = MrVar_Plot( ppa_90_vname,  /CURRENT, OVERPLOT=p4 )
	p6  = MrVar_Plot( ppa_180_vname, /CURRENT, OVERPLOT=p4 )
	
	
	;Velocity of 500eV electron
	;   - 1e-3 converts m/s -> km/s
	v500 = 1e-3 * Sqrt( 2.0 * 500 * MrConstants('eV2J') / MrConstants('m_e') )

	;Plot a vertical line at 500eV
	l1 = MrPlotS( [500, 500], p4.yrange, $
	              LINESTYLE = '--', $
	              NAME      = 'Line: 500eV', $
	              TARGET    = p4 )
	l2 = MrPlotS( [-v500, -v500], p1.yrange, $
	              LINESTYLE = '--', $
	              NAME      = 'Line: -v500', $
	              TARGET    = p1 )
	l3 = MrPlotS( [v500, v500], p1.yrange, $
	              LINESTYLE = '--', $
	              NAME      = 'Line: +v500', $
	              TARGET    = p1 )
	
	;Pretty-up the window
	win[0]  -> SetLayout, [1,1]
	win[2]  -> SetLayout, [1,2]
	win[4]  -> SetLayout, [1,3]
	win[6]  -> SetLayout, [2,1]
	win[10] -> SetLayout, [2,2]
	win    -> TrimLayout
	win    -> SetProperty, OXMARGIN=[12,5]
	win    -> Refresh

	RETURN, win
END