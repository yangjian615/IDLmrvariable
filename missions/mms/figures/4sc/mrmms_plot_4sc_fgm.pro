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
;                       Options are: { 'epht89d' | 'epht89q' | 'ephts04d' | 'defeph' | 'predeph' }
;       LEVEL:      in, optional, type=string, default='l2'
;                   Data quality level. Options are: {'l1a' | 'l1b' | 'ql' | 'l2pre' | 'l2'}
;       OPTDESC:    in, optional, type=string, default=''
;                   Optional filename descriptor.
;       OUTPUT_DIR: in, optional, type=string, default=pwd
;                   A directory in which to save the figure. If neither `OUTPUT_DIR`
;                       nor `OUTPUT_EXT` are defined, no file is generated.
;       OUTPUT_EXT: in, optional, type=string, default=pwd
;                   File extensions for the output figure. Options include: 'eps', 'gif',
;                       'jpg', 'ps', 'pdf', 'png', 'tiff'. If neither `OUTPUT_DIR` nor
;                       `OUTPUT_EXT` are defined, no file is generated.
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
FUNCTION MrMMS_Plot_4sc_FGM, mode, instr, $
ENERGY=energy, $
EPHDESC=ephdesc, $
LEVEL=level, $
OPTDESC=optdesc, $
OUTPUT_DIR=output_dir, $
OUTPUT_EXT=output_ext, $
NO_LOAD=no_load, $
SPECIES=species, $
TAIL=tail, $
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
	tf_tail = Keyword_Set(tail)
	IF N_Elements(energy)  EQ 0 THEN energy  = tf_tail ? [100, 250, 500, 2500] : [20, 100, 200, 500]
	IF N_Elements(species) EQ 0 THEN species = 'e'
	IF N_Elements(trange)  GT 0 THEN MrVar_SetTRange, trange
	
	;INSTR
	IF N_Elements(instr) EQ 0 THEN BEGIN
		IF N_Elements(level) EQ 0 $
			THEN instr = 'fgm' $
			ELSE instr = level EQ 'ql' ? 'dfg' : 'fgm'
	ENDIF
	
	;EPHDESC
	IF N_Elements(ephdesc) EQ 0 THEN BEGIN
		IF N_Elements(level) EQ 0 $
			THEN ephdesc = 'ephts04d' $
			ELSE ephdesc = level EQ 'ql' ? 'predeph' : 'ephts04d'
	ENDIF
	
	;LEVEL
	IF N_Elements(level) EQ 0 THEN BEGIN
		CASE instr OF
			'afg': level = 'l2pre'
			'dfg': level = 'l2pre'
			'fgm': level = 'l2'
			ELSE: Message, 'Invalid FGM instrument: "' + instr + '".'
		ENDCASE
	ENDIF
	
;-------------------------------------------
; Variable Names ///////////////////////////
;-------------------------------------------
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
	bmag_vnames = sc + '_' + StrJoin( [instr, 'bmag', coords, mode, level], '_' )
	IF Array_Equal(ephdesc EQ ['defeph', 'predeph'], 0) $
		THEN r_vnames    = sc + '_' + StrJoin( ['mec', 'r', coords], '_' ) $
		ELSE r_vnames    = StrUpCase(sc) + '_' + StrUpCase(ephdesc) + '_' + 'R'
	
	;Output names
	b1_vnames  = [ bmag_vnames[0], bvec_vnames[0] + '_' + ['x', 'y', 'z'] ]
	b2_vnames  = [ bmag_vnames[1], bvec_vnames[1] + '_' + ['x', 'y', 'z'] ]
	b3_vnames  = [ bmag_vnames[2], bvec_vnames[2] + '_' + ['x', 'y', 'z'] ]
	b4_vnames  = [ bmag_vnames[3], bvec_vnames[3] + '_' + ['x', 'y', 'z'] ]
	j_vname    = StrJoin( ['mms', instr, 'currentdensity', mode, level], '_' )
	k_vname    = StrJoin( ['mms', instr, 'scatparam'], '_' ) + '_' + String(energy, FORMAT='(i0)') + '_' + StrJoin( [mode, level], '_' )
	null_vname = StrJoin( ['mms', instr, 'Rns', mode, level], '_' )
	pi_vname   = StrJoin( ['mms', instr, 'pi', mode, level], '_' )
	
;-------------------------------------------
; Get Data /////////////////////////////////
;-------------------------------------------
	IF tf_load THEN BEGIN
		oFGM = MrMMS_FGM_4sc( mode, ephdesc, $
		                      COORD_SYS = coords, $
		                      INSTR     = instr, $
		                      LEVEL     = level )
	ENDIF
	
	;Old naming convention
	IF ~MrVar_IsCached(b_vnames[0]) THEN BEGIN
		b_vnames    = sc + '_' + StrJoin( [instr,        mode, coords], '_' )
		bvec_vnames = sc + '_' + StrJoin( [instr, 'vec', mode, coords], '_' )
		b1_vnames   = bvec_vnames[0] + '_' + ['x', 'y', 'z']
		b2_vnames   = bvec_vnames[1] + '_' + ['x', 'y', 'z']
		b3_vnames   = bvec_vnames[2] + '_' + ['x', 'y', 'z']
		b4_vnames   = bvec_vnames[3] + '_' + ['x', 'y', 'z']
		IF ~MrVar_IsCached(b_vnames[0]) $
			THEN Message, 'Unexpected variable naming convention for FGM.'
	ENDIF
	
	IF Obj_Valid(oFGM) EQ 0 THEN BEGIN
		oFGM = MrMMS_FGM_4sc( bvec_vnames[0], bvec_vnames[1], bvec_vnames[2], bvec_vnames[3], $
		                      r_vnames[0],    r_vnames[1],    r_vnames[2],    r_vnames[3] )
	ENDIF

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
	
	;FOTE
	!Null = oFGM -> FOTE(NAME=null_vname, RMAG=oRmag)
	oRmag -> SetName, null_vname
	oRmag -> Cache
	
	;POINCARE INDEX
	oPI = oFGM -> Poincare(NAME=pi_vname, /CACHE)
	
;-------------------------------------------
; Extract Components ///////////////////////
;-------------------------------------------

	;Split into components
	xrange   = [!values.f_infinity, -!values.f_infinity]
	yrange   = [!values.f_infinity, -!values.f_infinity]
	zrange   = [!values.f_infinity, -!values.f_infinity]
	magrange = [!values.f_infinity, -!values.f_infinity]
	FOR i = 0, 3 DO BEGIN
		;Bxyz
		oBvec = MrVar_Get(bvec_vnames[i])
		oBvec -> Split, oBx, oBy, oBz, /CACHE
		
		;|B|
		oBmag = MrVar_Get(bmag_vnames[i])
		
		CASE i OF
			0: color = 'Black'
			1: color = 'Red'
			2: color = 'Green'
			3: color = 'Blue'
			ELSE: Message, 'Invalid spacecraft number: ' + String(i, FORMAT='(i0)')
		ENDCASE
		
		;|B|
		oBmag['COLOR'] = color
		oBmag['LABEL'] = 'mms' + String(i+1, FORMAT='(i0)')
		magrange[0]   <= oBmag.min
		magrange[1]   >= oBmag.max
		
		
		;Bx
		oBx['COLOR'] = color
		oBx['LABEL'] = 'mms' + String(i+1, FORMAT='(i0)')
		xrange[0]   <= oBx.min
		xrange[1]   >= oBx.max
		
		;By
		oBy['COLOR'] = color
		oBy['LABEL'] = 'mms' + String(i+1, FORMAT='(i0)')
		yrange[0]   <= oBy.min
		yrange[1]   >= oBy.max
		
		;Bz
		oBz['COLOR'] = color
		oBz['LABEL'] = 'mms' + String(i+1, FORMAT='(i0)')
		zrange[0]   <= oBz.min
		zrange[1]   >= oBz.max
	ENDFOR
	
	;Clamp at +/-100nT
	magrange[0]  = 0
	magrange[1] <= 100
	xrange[0]   >= -100
	xrange[1]   <= 100
	yrange[0]   >= -100
	yrange[1]   <= 100
	zrange[0]   >= -100
	zrange[1]   <= 100
	
;-------------------------------------------
; Properties ///////////////////////////////
;-------------------------------------------
	;|B|
	oBx = MrVar_Get(b1_vnames[0])
	oBx['PLOT_TITLE'] = 'MMS1-4 ' + StrUpCase(instr) + ' ' + StrUpCase(mode) + ' ' + StrUpCase(level)
	oBx['AXIS_RANGE'] = magrange
	oBx['TITLE']      = '|B|!C(nT)'

	;Bx
	oBx = MrVar_Get(b1_vnames[1])
	oBx['AXIS_RANGE'] = xrange
	oBx['TITLE']      = 'Bx!C(nT)'
	
	;By
	oBy = MrVar_Get(b1_vnames[2])
	oBy['AXIS_RANGE'] = yrange
	oBy['TITLE']      = 'By!C(nT)'
	
	;Bz
	oBz = MrVar_Get(b1_vnames[3])
	oBz['AXIS_RANGE'] = zrange
	oBz['TITLE']      = 'Bz!C(nT)'
	
	;J
	oJ['AXIS_RANGE'] = [-1e3 > oJ.min, 1e3 < oJ.max]
	oJ['LABEL']      = ['Jx', 'Jy', 'Jz']
	
	;Kappa
	oK = MrVar_Get(k_vname[-1])
	oK['AXIS_RANGE'] = krange
	oK['TITLE']      = '$\kappa$$\up2$'
	
	;Rns
	oRmag['AXIS_RANGE'] = [1e0, 5e3]
	oRmag['LABEL']      = ['mms1', 'mms2', 'mms3', 'mms4']
	oRmag['LOG']        = 1B

;-------------------------------------------
; Plot Data ////////////////////////////////
;-------------------------------------------
	;Plot data
	win = MrVar_PlotTS( [ b1_vnames, j_vname, k_vname[-1], null_vname ], $
	                    /NO_REFRESH, $
	                    XSIZE = 600, $
	                    YSIZE = 700 )
	win = MrVar_OPlotTS( b1_vnames, b2_vnames )
	win = MrVar_OPlotTS( b1_vnames, b3_vnames )
	win = MrVar_OPlotTS( b1_vnames, b4_vnames )
	IF nEnergy GT 1 THEN win = MrVar_OPlotTS( Replicate(k_vname[-1], nEnergy-1), k_vname[-2:0:-1] )

	;Draw horizontal lines at k=[10, 25]
	trange = MrVar_GetTRange('SSM')
	IF trange[1] - trange[0] LE 60.0 THEN trange = trange - Floor(trange[0])
	l1     = MrPlotS( trange, [10, 10], LINESTYLE='--', NAME='k=10', NOCLIP=0, TARGET=win[k_vname[0]] )
	l2     = MrPlotS( trange, [25, 25], LINESTYLE='--', NAME='k=25', NOCLIP=0, TARGET=win[k_vname[0]] )
	ltxt   = MrText( 0.95, 0.05, '\kappa^2=10,25', $
	                 ALIGNMENT = 1.0, $
	                 NAME      = 'Txt: Kappa', $
	                 NOCLIP    = 0B, $
	                 /RELATIVE, $
	                 TARGET    = win[k_vname[0]] )
	
	;Pretty-up the window
	win    -> Refresh, /DISABLE
	win[0] -> SetLayout, [1,1]
	win    -> TrimLayout
	win    -> SetProperty, OXMARGIN=[13, 9]
	win    -> Refresh

;-------------------------------------------
; Save Figure //////////////////////////////
;-------------------------------------------
	IF N_Elements(output_dir) GT 0 || N_Elements(output_ext) GT 0 THEN BEGIN
		;Defaults
		IF N_Elements(output_dir) EQ 0 THEN BEGIN
			CD, CURRENT=output_dir
		ENDIF ELSE IF ~File_Test(output_dir, /DIRECTORY) THEN BEGIN
			MrPrintF, 'LogText', 'Creating directory: "' + output_dir + '".'
			File_MKDir, output_dir
		ENDIF
		
		;File name
		fname   = StrJoin( ['mms1234', instr, mode, level, '4sc'], '_' )
		fname   = FilePath( fname, ROOT_DIR=output_dir )
		
		;Save the figure
		fout = MrVar_PlotTS_Save( win, fname, output_ext )
	ENDIF

;-------------------------------------------
; Done /////////////////////////////////////
;-------------------------------------------

	RETURN, win
END