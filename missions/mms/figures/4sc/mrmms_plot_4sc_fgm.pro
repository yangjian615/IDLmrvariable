; docformat = 'rst'
;
; NAME:
;       MrMMS_Plot_FGM_4sc
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
;   Generate a plot to provide an overview of reconnection quantities:
;       1. Bx MMS1-4
;       2. By MMS1-4
;       3. Bz MMS1-4
;       4. J Curlometer
;       5. K Scattering Parameter
;
; :Categories:
;   MMS
;
; :Params:
;       MODE:       in, required, type=string
;                   Data rate mode. Options are {'srvy' | 'brst'}
;       INSTR:      in, required, type=string, default='fgm'
;                   FGM strument to use. Options are: {'afg' | 'dfg' | 'fgm'}
;
; :Keywords:
;       ENERGY:     in, optional, type=intarr, default=[20\, 100\, 200\, 500]
;                   Energies of the particles for which the the scattering parameter
;                       is to be computed.
;       EPHDESC:    in, optional, type=string, default='ephts04d'
;                   Optional descriptor of the definitive ephemeris datatype to use.
;                       Options are: { 'epht89d' | 'epht89q' | 'ephts04d' | 'defeph' | 'predef' }
;       LEVEL:      in, optional, type=string, default='l2'
;                   Data quality level. Options are: {'l1a' | 'l1b' | 'ql' | 'l2pre' | 'l2'}
;       OPTDESC:    in, optional, type=string, default=''
;                   Optional filename descriptor.
;       NO_LOAD:    in, optional, type=boolean, default=0
;                   If set, data will not be loaded from source CDF files.
;       SPECIES:    in, optional, type=string, default='e'
;                   Species of particle for which the scattering parameter is calculated.
;                       Options are: { 'e' | 'i' }
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
;       2017/01/05  -   Written by Matthew Argall
;-
FUNCTION MrMMS_Plot_FGM_4sc, mode, instr, $
ENERGY=energy, $
EPHDESC=ephdesc, $
LEVEL=level, $
OPTDESC=optdesc, $
NO_LOAD=no_load, $
SPECIES=species, $
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
	IF N_Elements(energy)  EQ 0 THEN energy  = [20, 100, 200, 500]
	IF N_Elements(species) EQ 0 THEN species = 'e'
	IF N_Elements(ephdesc) EQ 0 THEN ephdesc = 'ephts04d'
	IF N_Elements(instr)   EQ 0 THEN instr   = 'fgm'
	IF N_Elements(trange)  GT 0 THEN MrVar_SetTRange, trange
	
;-------------------------------------------
; Variable Names ///////////////////////////
;-------------------------------------------
	IF N_Elements(level) EQ 0 THEN BEGIN
		CASE instr OF
			'afg': level = 'l2pre'
			'dfg': level = 'l2pre'
			'fgm': level = 'l2'
			ELSE: Message, 'Invalid FGM instrument: "' + instr + '".'
		ENDCASE
	ENDIF
	IF N_Elements(coords) EQ 0 THEN BEGIN
		CASE level OF
			'ql': coords = 'dmpa'
			ELSE: coords = 'gse'
		ENDCASE
	ENDIF

	;Source names
	sc          = 'mms' + ['1', '2', '3', '4']
	b_vnames    = sc + '_' + StrJoin( [instr, 'b',    coords, mode, level], '_' )
	bvec_vnames = sc + '_' + StrJoin( [instr, 'bvec', coords, mode, level], '_' )
	r_vnames    = StrUpCase(sc) + '_' + 'R'
	r_vnames    = sc + '_' + StrJoin( ['mec', 'r', coords], '_' )
	
	;Output names
	b1_vnames = bvec_vnames[0] + '_' + ['x', 'y', 'z']
	b2_vnames = bvec_vnames[1] + '_' + ['x', 'y', 'z']
	b3_vnames = bvec_vnames[2] + '_' + ['x', 'y', 'z']
	b4_vnames = bvec_vnames[3] + '_' + ['x', 'y', 'z']
	j_vname   = StrJoin( ['mms', instr, 'currentdensity', mode, level], '_' )
	k_vname   = StrJoin( ['mms', instr, 'scatparam'], '_' ) + '_' + String(energy, FORMAT='(i0)') + '_' + StrJoin( [mode, level], '_' )

;-------------------------------------------
; Get Data /////////////////////////////////
;-------------------------------------------
	IF tf_load THEN BEGIN
		; Magnetic field
		oFGM = MrMMS_FGM_4sc( mode, ephdesc, $
		                      COORD_SYS = coords, $
		                      INSTR     = instr, $
		                      LEVEL     = level )
	ENDIF ELSE BEGIN
		oFGM = MrMMS_FGM_4sc( bvec_vnames[0], bvec_vnames[1], bvec_vnames[2], bvec_vnames[3], $
		                      r_vnames[0],    r_vnames[1],    r_vnames[2],    r_vnames[3] )
	ENDELSE

;-------------------------------------------
; Current Density & Curvature //////////////
;-------------------------------------------
	;Mass of particle
	CASE species OF
		'e': mass = MrConstants('m_e')
		'i': mass = MrConstants('m_p')
		ELSE: Message, 'SPECIES must be {"e" | "i"}.'
	ENDCASE

	;Current density via reciprocal vectors
	oJ = oFGM -> J(NAME=j_vname, /CACHE)
	
	;Curvature scattering parameter
	nEnergy = N_Elements(energy)
	krange  = [1e-1, 1e4]
	colors  = MrDefaultColor(NCOLORS=nEnergy)
	FOR i = 0, nEnergy - 1 DO BEGIN
		oK = oFGM -> Kappa(energy[i], mass, NAME=k_vname[i], /CACHE)
		oK['COLOR']  = colors[i]
		oK['LABEL']  = String(energy[i], '(%"%i eV")')
		krange[0]   <= oK.min
		krange[1]   >= oK.max
	ENDFOR

;-------------------------------------------
; Extract Components ///////////////////////
;-------------------------------------------

	;Split into components
	xrange = [-100, 100]
	yrange = [-100, 100]
	zrange = [-100, 100]
	FOR i = 0, 3 DO BEGIN
		oB = MrVar_Get(bvec_vnames[i])
		oB -> Split, oBx, oBy, oBz, /CACHE
		
		CASE i OF
			0: color = 'Black'
			1: color = 'Red'
			2: color = 'Green'
			3: color = 'Blue'
			ELSE: Message, 'Invalid spacecraft number: ' + String(i, FORMAT='(i0)')
		ENDCASE
		
		;Bx
		oBx['COLOR'] = color
		oBx['LABEL'] = 'mms' + String(i+1, FORMAT='(i0)')
		xrange[0]   >= oBx.min
		xrange[1]   <= oBx.max
		
		;By
		oBy['COLOR'] = color
		oBy['LABEL'] = 'mms' + String(i+1, FORMAT='(i0)')
		yrange[0]   >= oBy.min
		yrange[1]   <= oBy.max
		
		;Bz
		oBz['COLOR'] = color
		oBz['LABEL'] = 'mms' + String(i+1, FORMAT='(i0)')
		zrange[0]   >= oBz.min
		zrange[1]   <= oBz.max
	ENDFOR
	
;-------------------------------------------
; Properties ///////////////////////////////
;-------------------------------------------
	;Bx
	oBx = MrVar_Get(b1_vnames[0])
	oBx['PLOT_TITLE'] = 'MMS1-4 ' + StrUpCase(instr)
	oBx['AXIS_RANGE'] = xrange
	oBx['TITLE']      = 'Bx!C(nT)'
	
	;By
	oBy = MrVar_Get(b1_vnames[1])
	oBy['AXIS_RANGE'] = yrange
	oBy['TITLE']      = 'By!C(nT)'
	
	;Bz
	oBz = MrVar_Get(b1_vnames[2])
	oBz['AXIS_RANGE'] = zrange
	oBz['TITLE']      = 'Bz!C(nT)'
	
	;J
	oJ['AXIS_RANGE'] = [-1e3 > oJ.min, 1e3 < oJ.max]
	oJ['LABEL']      = ['Jx', 'Jy', 'Jz']
	
	;Kappa
	oK = MrVar_Get(k_vname[0])
	oK['AXIS_RANGE'] = krange
	oK['TITLE']      = '$\kappa$$\up2$'


;-------------------------------------------
; Plot Data ////////////////////////////////
;-------------------------------------------
	;Plot data
	win = MrVar_PlotTS( [ b1_vnames, j_vname, k_vname[0] ], $
	                    /NO_REFRESH, $
	                    XSIZE = 600, $
	                    YSIZE = 650 )
	win = MrVar_OPlotTS( b1_vnames, b2_vnames )
	win = MrVar_OPlotTS( b1_vnames, b3_vnames )
	win = MrVar_OPlotTS( b1_vnames, b4_vnames )
	IF nEnergy GT 1 THEN win = MrVar_OPlotTS( Replicate(k_vname[0], nEnergy-1), k_vname[1:-1] )

	;Draw horizontal lines at k=[10, 25]
	trange = MrVar_GetTRange('SSM')
	l1     = MrPlotS( trange, [10, 10], LINESTYLE='--', NAME='k=10', TARGET=win[k_vname[0]] )
	l2     = MrPlotS( trange, [25, 25], LINESTYLE='--', NAME='k=25', TARGET=win[k_vname[0]] )
	ltxt   = MrText( 1.0, 0.05, '\kappa^2=10,25', $
	                 ALIGNMENT = 1.0
	                 /RELATIVE, $
	                 TARGET       = win[k_vname[0]] )
	
	;Pretty-up the window
	win    -> Refresh, /DISABLE
	win[0] -> SetLayout, [1,1]
	win    -> TrimLayout
	win    -> SetProperty, OXMARGIN=[13, 9]
	win    -> Refresh

	RETURN, win
END