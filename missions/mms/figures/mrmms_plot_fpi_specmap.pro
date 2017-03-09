; docformat = 'rst'
;
; NAME:
;       MrMMS_Plot_Rx_Overview
;
;*****************************************************************************************
;   Copyright (c) 2016, Matthew Argall                                                   ;
;   All rights reserved.                                                                 ;
;                                                                                        ;
;   Redistribution and use in source and binary forms, with or without modification,     ;
;   are permitted provided that the following conditions are met:                        ;
;                                                                                        ;
;       * Redistributions OF source code must retain the above copyright notice,         ;
;         this list OF conditions and the following disclaimer.                          ;
;       * Redistributions in binary form must reproduce the above copyright notice,      ;
;         this list OF conditions and the following disclaimer in the documentation      ;
;         and/or other materials provided with the distribution.                         ;
;       * Neither the name OF the <ORGANIZATION> nor the names OF its contributors may   ;
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
;   Generate a plot to provide an overview OF reconnection quantities:
;       1. B
;       2. E
;       3. ni & NE
;       4. Vi
;       5. E spectra (e)
;       6. E spectra (i)
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
;       TRANGE:     in, optional, type=string/strarr(2), default=MrVar_GetTRange()
;                   The start and end times OF the data interval to be plotted, formatted
;                       as 'YYYY-MM-DDThh:mm:ss'
;       
; :Author:
;   Matthew Argall::
;       University OF New Hampshire
;       Morse Hall, Room 348
;       8 College Rd.
;       Durham, NH, 03824
;       matthew.argall@unh.edu
;
; :History:
;   Modification History::
;       2016/09/21  -   Written by Matthew Argall
;-
FUNCTION MrMMS_Plot_FPI_SpecMap, sc, mode, energy, species, $
NO_LOAD=no_load, $
TRANGE=trange
	Compile_Opt idl2
	
	Catch, the_error
	IF the_error NE 0 THEN BEGIN
		Catch, /CANCEL
		IF N_Elements(win1) GT 0 THEN obj_destroy, win1
		IF N_Elements(win2) GT 0 THEN obj_destroy, win2
		MrPrintF, 'LogErr'
		RETURN, !Null
	ENDIF
	
	tf_load = ~Keyword_Set(no_load)
	IF N_Elements(energy)  EQ 0 THEN energy = 500
	IF N_Elements(species) EQ 0 THEN species = 'e'
	IF N_Elements(trange)  GT 0 THEN MrVar_SetTRange, trange

;-------------------------------------------
; Variable Names ///////////////////////////
;-------------------------------------------
	level     = 'l2'
	fgm_instr = 'fgm'
	CASE fgm_instr OF
		'afg': fgm_level = 'l2pre'
		'dfg': fgm_level = 'l2pre'
		'fgm': fgm_level = 'l2'
		ELSE: Message, 'Invalid FGM insrument name: "' + fgm_instr + '".'
	ENDCASE
	CASE mode OF
		'slow': scm_optdesc = 'scs'
		'fast': scm_optdesc = 'scf'
		'srvy': scm_optdesc = 'scsrvy'
		'brst': scm_optdesc = 'scb'
		ELSE: Message, 'Invalid data rate mode: "' + mode + '".'
	ENDCASE
	fpi_mode = mode EQ 'brst' ? mode : 'fast'
	
	coords     = 'gse'
	fpi_instr  = 'd' + species + 's'
	fpi_coords = 'dbcs'
	
	;Source names
	b_vname       = StrJoin( [sc, fgm_instr, 'b',    coords, mode, fgm_level], '_' )
	bvec_vname    = StrJoin( [sc, fgm_instr, 'bvec', coords, mode, fgm_level], '_' )
	acb_vname     = StrJoin( [sc, 'scm', 'acb', coords, scm_optdesc, mode, level], '_' )
	theta_vname   = StrJoin( [sc, fpi_instr, 'theta',  fpi_mode], '_' )
	phi_vname     = StrJoin( [sc, fpi_instr, 'phi',    fpi_mode], '_' )
	energy_vname  = StrJoin( [sc, fpi_instr, 'energy', fpi_mode], '_' )
	dist_vname    = StrJoin( [sc, fpi_instr, 'dist',   fpi_mode], '_' )
	
	;Derived names
	psd_vname  = StrJoin( [sc, 'scm', 'acb', 'psd', coords, scm_optdesc, mode, level], '_' )
	e0_vname   = StrJoin( [sc, fpi_instr, 'espec',   '0', fpi_mode], '_' )
	e90_vname  = StrJoin( [sc, fpi_instr, 'espec',  '90', fpi_mode], '_' )
	e180_vname = StrJoin( [sc, fpi_instr, 'espec', '180', fpi_mode], '_' )
	
;-------------------------------------------
; Get Data /////////////////////////////////
;-------------------------------------------
	
	;FGM
	IF tf_load THEN BEGIN
		;FGM
		MrMMS_FGM_Load_Data, sc, mode, $
		                     VARFORMAT = '*b_' + coords + '*'
		
		;SCM
		MrMMS_Load_Data, sc, 'scm', mode, level, $
		                 OPTDESC = scm_optdesc, $
		                 VARFORMAT = '*acb_' + coords + '*'
		
		;DES
		MrMMS_FPI_Load_Data, sc, fpi_mode, $
		                     OPTDESC = 'des-dist'
	ENDIF
	
;-------------------------------------------
; Power Spectral Density ///////////////////
;-------------------------------------------
	oACB = MrVar_Get(acb_vname)
	oACB -> Split, oBx, oBy, oBz
	oPSD = oBz -> Spectrogram( 512, 256, $
	                           /CACHE, $
	                           NAME   = psd_vname, $
	                           WINDOW = 'hanning')


;-------------------------------------------
; Energy Spectrograms //////////////////////
;-------------------------------------------
	;Create a coordinate grid
	MrVar_Grid, theta_vname, phi_vname, oTheta, oPhi, PAR=bvec_vname
	
	;Create a distribution object
	oDist   = MrVar_Get(dist_vname)
	oDist4D = MrDist4D( oDist['TIMEVAR'], oDist, oTheta, oPhi, oDist['DEPEND_3'] )

	;Obtain energy distributions at 0, 90, and 180 degrees
	oESpec0   = oDist4D -> GetESpec( THETA_RANGE=[  0,  20], /CACHE, NAME=e0_vname   )
	oESpec90  = oDist4D -> GetESpec( THETA_RANGE=[ 80, 100], /CACHE, NAME=e90_vname  )
	oESpec180 = oDist4D -> GetESpec( THETA_RANGE=[160, 180], /CACHE, NAME=e180_vname )
	
	;Time & Energy Array
	oTime   = oESpec0['TIMEVAR']
	oEnergy = MrVar_Get(oESpec0['DEPEND_1'])

;-------------------------------------------
; Attributes ///////////////////////////////
;-------------------------------------------
	;ENERGY
	oEnergy['TITLE'] = 'Energy!C(eV)'
	
	;Bz PSD
	oPSD['PLOT_TITLE'] = StrUpCase(sc)
	oPSD['TITLE']      = 'B!C(nT$\up2$/Hz)'
	
	;E0
	oESpec0['TITLE'] = 'PSD'
	
	;E90
	oESpec90['TITLE']  = 'PSD!C(' + oDist['UNITS'] + ')'
	
	;E180
	oESpec180['TITLE'] = 'PSD'

;-------------------------------------------
; Plot Layout for Spectral Map /////////////
;-------------------------------------------
	;Get time range
	tr_ssm = MrVar_GetTRange('SSM')

	;Establish layout
	nPts   = oESpec0 -> GetNPts()
	nCols  = nPts < 5
	nRows  = Ceil(nPts / Float(nCols)) < 4
	iStep  = nPts / (nCols * nRows)
	nPlots = nCols * nRows

;-------------------------------------------
; Plot Energy Spectra //////////////////////
;-------------------------------------------
	;Plot the spectrograms
	win1 = MrVar_PlotTS( [psd_vname, e0_vname, e90_vname, e180_vname], $
	                     /NO_REFRESH, $
	                     XSIZE   = 600, $
	                     YSIZE   = 650 )
	
	;Adjust Layout
	win1    -> Refresh, /DISABLE
	win1[0] -> SetLayout, [1,1]
	win1    -> TrimLayout
	win1.OXMARGIN = [12,13]
	
	;Add lines
	it = 6
	FOR i = 0, nPlots - 1 DO BEGIN
		;Seconds since start time
		t_ssm = oTime[it, 'SSM'] - Floor(tr_ssm[0])

		;Draw a verticle line
		ps = MrPlotS( [t_ssm, t_ssm], win1[e90_vname].yrange, $
		              COLOR     = 'White', $
		              LINESTYLE = '--', $
		              NAME      = 'Line' + String(i, FORMAT='(i0)'), $
		              TARGET    = win1[e90_vname], $
		              THICK     = 2 )
		
		;Next line
		it += iStep
	ENDFOR
	
	;Refresh the window
;	win1 -> Refresh

;-------------------------------------------
; Plot Spectral Map ////////////////////////
;-------------------------------------------
	
	;Create the window
	win2 = MrWindow(LAYOUT=[nCols, nRows], REFRESH=0, XGAP=0.5, YGAP=0.5, XSIZE=1000, YSIZE=650)
	
	;Plot title
	trange = MrVar_GetTRange()
	title  = StrUpCase(sc) + ' ' + StrMid(trange[0], 0, 10) + ' ' + StrMid(trange[0], 11, 8) + $
	         ' dt = ' + String(iStep*0.03, FORMAT='(f0.3)') + 's'

	;Spectral map
	it = 6
	yrange = [!values.f_infinity, -!values.f_infinity]
	FOR i = 0, nPlots - 1 DO BEGIN
		iRow = i / nCols
		iCol = i MOD nCols
		name = 'ESpec' + String(it, FORMAT='(i0)')

		;LEFT
		IF iCol EQ 0 THEN BEGIN
			ytitle      = 'PSD!C(s$\up3$/m$\up6$)'
			ytickformat = ''
		ENDIF ELSE BEGIN
			ytitle      = ''
			ytickformat = '(a1)'
		ENDELSE
		
		;BOTTOM
		IF iRow EQ nRows-1 THEN BEGIN
			xtitle = 'Energy (eV)'
			xtickformat = ''
		ENDIF ELSE BEGIN
			xtitle      = ''
			xtickformat = '(a1)'
		ENDELSE
		
		;Energy channel
		oEnergy = oESpec0['DEPEND_1']
		oneE    = MrVariable( Reform(oEnergy[it,*]) )
		oEnergy -> CopyAttrTo, oneE
		oneE    -> RemoveAttr, 'DEPEND_0'
		oneE['TICKFORMAT'] = xtickformat
		oneE['TITLE']      = xtitle
		
		;Data
		data  = Transpose([ oESpec0[it,*], oESpec90[it,*], oESpec180[it,*] ])
		oFlux = MrVariable( data, NAME=name )
		oFlux['DEPEND_0']   = oneE
		oFlux['DIMENSION']  = 1B
		oFlux['LOG']        = 1B
		oFlux['TICKFORMAT'] = ytickformat
		oFlux['TITLE']      = ytitle
		
		;Uniform axis range
		iGood      = where(data gt 0)
		yrange[0] <= min(data[iGood], MAX=ymax)
		yrange[1] >= ymax

		;Legend
		IF i EQ nPlots - 1 THEN oFlux['LABEL'] = ['0', '90', '180']
		
		;Plot
		p = MrVar_Plot( oFlux, /CURRENT )
		
		;Next piece of the map
		it += iStep
	ENDFOR
	
	;Adjust layout
	win2['Lgd: ' + name].alignment = 'NE'
	win2[0] -> SetLayout, [1,1]
	win2    -> TrimLayout
	win2    -> SetGlobal, YRANGE=yrange
	
	;Add title
	title    = MrText(0.5, 0.97, title, ALIGNMENT=0.5)

;-------------------------------------------
; Finish Up ////////////////////////////////
;-------------------------------------------
	
	win1 -> Refresh
	win2 -> Refresh
	RETURN, win1
END