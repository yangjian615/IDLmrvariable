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
FUNCTION mrmms_yuri_grl_2016_fig5, sc, mode, species, $
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
	
	sc      = 'mms4'
	mode    = 'brst'
	species = 'e'
	t0      = '2015-12-06T23:38:31.560'
	n0      = 3
	MrVar_SetTRange, '2015-12-06T' + ['23:38:31.5', '23:38:31.75']
	
	tf_load     = ~Keyword_Set(no_load)
	theta_range = [-90.0,  90.0]
	phi_range   = [  0.0, 360.0]
	IF N_Elements(t0)      EQ 0 THEN t0      = (MrVar_GetTRange())[0]
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
	b_vname    = StrJoin( [sc, fgm_instr, 'b',     fgm_coords, fgm_mode, level], '_' )
	bvec_vname = StrJoin( [sc, fgm_instr, 'bvec',  fgm_coords, fgm_mode, level], '_' )
	bmag_vname = StrJoin( [sc, fgm_instr, 'bmag',  fgm_coords, fgm_mode, level], '_' )
	dist_vname = StrJoin( [sc, instr, 'dist', mode], '_' )
	v_vname    = StrJoin( [sc, instr, 'bulkv', coords, mode], '_' )
	e_vname    = StrJoin( [sc, 'edp', 'dce', coords, mode, level], '_' )
	f0_vname   = StrJoin( [sc, 'edi', 'flux',   '0', mode, level], '_' )
	f180_vname = StrJoin( [sc, 'edi', 'flux', '180', mode, level], '_' )
	
	;Derived names
	j_vname = StrJoin( [sc, 'edi', 'currentdensity', mode, level] )
	
	;2D Time-Series Distributions
	idx_str             = '_t' + String(IndGen(n0), FORMAT='(i0)')
	perp1_perp2_vname   = StrJoin( [sc, instr, 'dist', 'perp1_perp2',             mode], '_' )
	par_perp_anti_vname = StrJoin( [sc, instr, 'dist', 'par_perp_anti', 'vspace', mode], '_' )
	
	;2D Distributions
	p1p2_vname = StrJoin( [sc, instr, 'dist', 'perp1_perp2',   'vspace', mode], '_' ) + idx_str
	ppa_vname  = StrJoin( [sc, instr, 'dist', 'par_perp_anti', 'espace', mode], '_' ) + idx_str
	
	;1D Distributions
	perp12_cuts_vname = p1p2_vname + '_cuts'
	ppa_cuts_vname    = ppa_vname  + '_cuts'
	
	;Fits
	xfit_vname = StrJoin( [sc, instr, 'dist', 'ppa', 'xfit', mode], '_' ) + idx_str
	yfit_vname = StrJoin( [sc, instr, 'dist', 'ppa', 'yfit', mode], '_' ) + idx_str

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
		
		;EDP
		MrMMS_Load_Data, sc, 'edp', mode, 'l2', $
		                 OPTDESC = 'dce'
		
		;EDI
		MrMMS_EDI_Load_Data, sc, mode
	ENDIF

	;Grab the distribution
	oDist      = MrVar_Get(dist_vname)
	theMass    = species EQ 'i' ? 'm_H' : 'm_e'
	theSpecies = species EQ 'i' ? 'H' : 'e'

;-----------------------------------------------------
; EDI Current Density \\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\
;-----------------------------------------------------
	;Get the data
	of0   = MrVar_Get(f0_vname)
	of180 = MrVar_Get(f180_vname)
	
	;Current density
	;   - 1e4 gets A/m^2
	;   - 1e6 gets uA/m^2
	oJ = 1e10 * MrConstants('q') * (of180 - of0)
	oJ -> SetName, j_vname
	oJ -> Cache

;-----------------------------------------------------
; FAC Cartesian Grid \\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\
;-----------------------------------------------------

	;Create a cartesian grid
	MrVar_Grid_MakeCart, oDist['DEPEND_1'], oDist['DEPEND_2'], oX, oY, oZ, /DEGREES

	;Field-Aligned Coordinate System
	fac    = 'VXB'
	oT_fac = MrVar_FAC( bvec_vname, v_vname, 'VXB', $
	                    AXLABELS = axlabels, $
	                    TIME     = oDist['TIMEVAR'] )

	;Rotate the cartesian grid to field-aligned coordinates
	;   - Negative sign converts from look-dirction to incident trajectory
	MrVar_Grid_Cart2FAC, oT_fac, -temporary(oX), -temporary(oY), -temporary(oZ), oX1, oX2, oX3
	
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
; Create Distributions /////////////////////
;-------------------------------------------
	;Determine which distribution to plot
	t0_tt2000 = MrCDF_Epoch_Parse(t0, /TO_TT2000)
	oTime     = oPerp1Perp2['TIMEVAR']
	i0        = oTime -> Value_Locate(t0_tt2000, 'TT2000') + 1
	IF i0 EQ 0 THEN i0 = [i0]
	
	;Pick out the distribution of interest
	MrTimeParser, oTime['DATA',i0], '%Y-%M-%dT%H:%m:%S%f', '%Y-%M-%d %H:%m:%S%f', title
	title = StrMid(title, 0, StrLen(title)-6)
	
	;Allocate memory
	vrange = [-1.5e4,  1.5e4]
	erange = [10, 3e3]
	frange = [1e-29, 1e-26]
	ptitle = StrArr(n0)
	
	;Loop over each consecutive distribution
	FOR i = 0, 2 DO BEGIN
		idx = i0 + i
	
	;-------------------------------------------
	; 1D & 2D Distributions ////////////////////
	;-------------------------------------------
	
		;2D Distributions
		oP1P2 = MrVar_Dist2D_Prep(oPerp1Perp2,  theMass, idx, /CACHE, NAME=p1p2_vname[i])
		oPPA  = MrVar_Dist2D_Prep(oParPerpAnti, theMass, idx, /CACHE, NAME=ppa_vname[i])

		;1D Distributions
		oPerp12 = MrVar_Dist1D_Prep( oPerp1Perp2,  theMass, idx, /CACHE, NAME=perp12_cuts_vname[i] )
		oPPA    = MrVar_Dist1D_Prep( oParPerpAnti, theMass, idx, /CACHE, /POLAR, NAME=ppa_cuts_vname[i] )
	
	;-------------------------------------------
	; Fit the Energy Distributions /////////////
	;-------------------------------------------
		;Extract the data at 500eV +/-1 neighboring point
		oE    = oPPA['DEPEND_0']
		!Null = Min( Abs( oE['DATA'] - 500 ), iMin )
		x     = oE[ 'DATA', iMin + [-1, 0, 1] ]
		y     = oPPA[ 'DATA', iMin + [-1, 0, 1], 1 ]
		
		;Fit to y = b * x^a
		;   - ln(y) = ln(b) + a*ln(x)
		;   - y' = b' + a*x'
		a = LADFit( alog(x), alog(y) )
		x = oE['DATA']
		y = Exp(a[0])*x^a[1]
		
		;Use fit as plot title
		;   - Change bases for e to 10
		pow = Fix( Floor( a[0] * alog10(exp(1)) ) )
		num = Exp(a[0]) * 10.0^(-pow)
		str = String(num, pow, a[1], FORMAT='(%"y=%0.2fx10$\\up%0i$e$\\up%0.2f$")')
		ptitle[i] = cgCheckForSymbols( str )
		
		;Create variables
		xfit = MrVariable( x, NAME=xfit_vname[i] )
		yfit = MrVariable( y, /CACHE, NAME=yfit_vname[i] )
	
	;-------------------------------------------
	; Attributes ///////////////////////////////
	;-------------------------------------------
		vrange = [-1.5e4,  1.5e4]
		erange = [10, 3e3]
		frange = [1e-29, 1e-26]
	
		;PERP1-PERP2
		oV   = oP1P2['DEPEND_0']
		oPhi = oP1P2['DEPEND_1']
		oPhi['TITLE']          = 'V$\down'+axlabels[0]+'$!C(10$\up3$ km s$\up-1$)'
		oP1P2['AXIS_RANGE']    = frange
		oP1P2['MISSING_VALUE'] = 0B
		oP1P2['MISSING_COLOR'] = 'Grey'
		oP1P2['PLOT_TITLE']    = oTime['DATA', idx, 11:22]
		oP1P2['TITLE']         = 'PSD'

;		oV -> SetData, oV['DATA']*1e-3
		oV['AXIS_RANGE']       = vrange
		oV['TITLE']            = 'V$\down'+axlabels[1]+'$ (10$\up3$ km s$\up-1$)'
	
		;CUTS: PERP1-PEPR2
		oPerp12['COLOR'] = ['Blue',  'Red']
		oPerp12['LABEL'] = ['Perp1', 'Perp2']

		;CUTS: PAR-PERP-ANTI
		oE = oPPA['DEPEND_0']
		oE['AXIS_RANGE']   = erange
		oE['TITLE']        = 'Energy (eV)'
		oPPA['AXIS_RANGE'] = frange
		oPPA['COLOR']      = ['Black', 'Red', 'Blue']
		oPPA['LABEL']      = ['0', '90', '180']
		
		;XFIT
		xfit['LOG'] = 1
		
		;YFIT
		yfit['DEPEND_0']  = xfit
		yfit['LOG']       = 1
		yfit['LINESTYLE'] = '--'
	ENDFOR

;-------------------------------------------
; Attributes ///////////////////////////////
;-------------------------------------------
	;B
	oB = MrVar_Get(b_vname)
	oB['PLOT_TITLE'] = StrUpCase(sc)
	
	;V
	oV = MrVar_Get(v_vname)
	oV['TITLE'] = 'V!C(km/s)'
	oV['LABEL'] = ['Vx', 'Vy', 'Vz']
	
	;E
	oE = MrVar_Get(e_vname)
	oE['TITLE'] = 'E!C(mV/m)'
	
	;J
	oJ['TITLE'] = 'J$\downEDI$!C($\mu$A/m$\up2$)'
	
;-------------------------------------------
; Plot Time-Series /////////////////////////
;-------------------------------------------
	;Create a window
	win = MrWindow( LAYOUT   = [1,4], $
	                OXMARGIN = [9,13], $
	                OYMARGIN = [23,3], $
	                XSIZE    = 1040, $
	                YGAP     = 0.5, $
	                YSIZE    = 775, $
	                REFRESH  = 0 )
	
	;Plot the time-series data
	win = MrVar_PlotTS( [b_vname, v_vname, e_vname, j_vname], /CURRENT )
	
	ti_sse = oTime['DATA', i0[0]+IndGen(n0), 'SSM'] - Floor((MrVar_GetTRange('SSM'))[0])
	pV = win[v_vname]
	op = MrPlotS( [ti_sse[0], ti_sse[0]], pV.yrange, TARGET=pV, LINESTYLE='--' )
	op = MrPlotS( [ti_sse[1], ti_sse[1]], pV.yrange, TARGET=pV, LINESTYLE='--' )
	op = MrPlotS( [ti_sse[2], ti_sse[2]], pV.yrange, TARGET=pV, LINESTYLE='--' )
	
;-------------------------------------------
; Plot Distributions ///////////////////////
;-------------------------------------------
	;Positions of distributions
	;   - Create a 3x2 layout for the 6 distributions
	;   - Make the top margin super large to make room for
	;     the time-series plots
	pos = MrLayout( [3,2], $
	                ASPECT   = 1.0, $
	                CHARSIZE = 1.5, $
	                OXMARGIN = [9,13], $
	                OYMARGIN = [4,30], $
	                WDIMS    = [1040, 775], $
	                XGAP     = 16, $
	                YGAP     = 5 )

	;Plot the distributions
	p1  = MrVar_Plot( ppa_cuts_vname[0], /CURRENT, POSITION=pos[*,0], TITLE=ptitle[0] )
	p2  = MrVar_Plot( ppa_cuts_vname[1], /CURRENT, POSITION=pos[*,1], TITLE=ptitle[1] )
	p3  = MrVar_Plot( ppa_cuts_vname[2], /CURRENT, POSITION=pos[*,2], TITLE=ptitle[2] )
	im1 = MrVar_Image( p1p2_vname[0], /CURRENT, POSITION=pos[*,3] )
	im2 = MrVar_Image( p1p2_vname[1], /CURRENT, POSITION=pos[*,4] )
	im3 = MrVar_Image( p1p2_vname[2], /CURRENT, POSITION=pos[*,5] )
	
	;Overplot
	o3p1 = MrVar_Plot( yfit_vname[0], OVERPLOT=p1 )
	o3p2 = MrVar_Plot( yfit_vname[1], OVERPLOT=p2 )
	o3p3 = MrVar_Plot( yfit_vname[2], OVERPLOT=p3 )

	;Titles
	p1.title = o3p1.title
	p2.title = o3p2.title
	p3.title = o3p3.title
	
	;Plot a vertical line at 500eV
	l1 = MrPlotS( [500, 500], p1.yrange, $
	              LINESTYLE = '--', $
	              NAME      = 'Line: 500eV', $
	              TARGET    = p1 )
	l2 = MrPlotS( [500, 500], p2.yrange, $
	              LINESTYLE = '--', $
	              NAME      = 'Line: -v500', $
	              TARGET    = p2 )
	l3 = MrPlotS( [500, 500], p3.yrange, $
	              LINESTYLE = '--', $
	              NAME      = 'Line: +v500', $
	              TARGET    = p3 )
	
	;Pretty-up the window
	win[0]  -> SetLayout, [1,1]
;	win    -> TrimLayout
	win    -> SetProperty, OXMARGIN=[12,5]
	win    -> Refresh

	RETURN, win
END