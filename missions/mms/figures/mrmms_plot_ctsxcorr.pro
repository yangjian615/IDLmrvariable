; docformat = 'rst'
;
; NAME:
;       MrMMS_Plot_CtsXCorr
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
;       1. B
;       2. B PSD
;       3. DES E Spectra
;       4. DES PAD
;       5. EDI Ambient Counts
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
;       2016/11/02  -   Written by Matthew Argall
;       2017/06/04  -   Added the OUTPUT_EXT and OUTPUT_DIR keywords. Correlate
;                           counts with component of E or B projected onto the
;                           EDI look direction.
;-
FUNCTION MrMMS_Plot_CtsXCorr, sc, mode, chan, pa, field, $
NO_LOAD=no_load, $
OUTPUT_DIR=output_dir, $
OUTPUT_EXT=output_ext, $
TRANGE=trange
	Compile_Opt idl2
	
	Catch, the_error
	IF the_error NE 0 THEN BEGIN
		Catch, /CANCEL
		IF N_Elements(win) GT 0 THEN Obj_Destroy, win
		MrPrintF, 'LogErr'
		RETURN, !Null
	endif
	
	tf_load = ~Keyword_Set(no_load)
	IF N_Elements(chan)   EQ 0 THEN chan   = 1
	IF N_Elements(field)  EQ 0 THEN field  = 'Bpar'
	IF N_Elements(pa)     EQ 0 THEN pa     = 0
	IF N_Elements(f_wave) EQ 0 THEN f_wave = 135.0
	IF N_Elements(trange) EQ 0 THEN trange = MrVar_GetTRange()
	
	frange = [80, 200]
	A      = 50
	nTerms = 1024
	
;-------------------------------------------
; Variable Names ///////////////////////////
;-------------------------------------------
	fgm_instr = 'fgm'
	CASE fgm_instr OF
		'afg': fgm_level = 'l2pre'
		'dfg': fgm_level = 'l2pre'
		'fgm': fgm_level = 'l2'
		ELSE: Message, 'Invalid FGM instrument "' + fgm_instr + '".'
	ENDCASE

	level    = 'l2'
	coords   = 'gse'
	edp_mode = mode EQ 'srvy' ? 'fast' : mode
	
	CASE mode OF
		'slow': scm_desc = 'scs'
		'fast': scm_desc = 'scf'
		'srvy': scm_desc = 'scsrvy'
		'brst': scm_desc = 'scb'
		ELSE: Message, 'Invalid mode: "' + mode + '".'
	ENDCASE

	;Source names
	fgm_vname     = StrJoin( [sc, fgm_instr, 'bvec', coords, mode, fgm_level], '_' )
	scm_vname     = StrJoin( [sc, 'scm', 'acb',  coords, scm_desc, mode, level], '_' )
	edp_vname     = StrJoin( [sc, 'edp', 'dce',  coords, mode, level], '_' )
	
	channel        = ['1', '2', '3', '4']
	flux_0_vname   = sc + '_edi_flux' + channel +   '_0_' + mode + '_' + level
	flux_90_vname  = sc + '_edi_flux' + channel +  '_90_' + mode + '_' + level
	flux_180_vname = sc + '_edi_flux' + channel + '_180_' + mode + '_' + level
	traj_0_vname   = sc + '_edi_traj' + channel + '_' + coords +   '_0_' + mode + '_' + level
	traj_90_vname  = sc + '_edi_traj' + channel + '_' + coords +  '_90_' + mode + '_' + level
	traj_180_vname = sc + '_edi_traj' + channel + '_' + coords + '_180_' + mode + '_' + level
	
;-------------------------------------------
; Get Data /////////////////////////////////
;-------------------------------------------
	IF tf_load THEN BEGIN
	;-------------------------------------------
	; FGM //////////////////////////////////////
	;-------------------------------------------
		MrMMS_FGM_Load_Data, sc, mode, $
		                     INSTR     = fgm_instr, $
		                     VARFORMAT = ['*_b_*' + coords + '*', $
		                                  '*' + fgm_instr + '*' + coords]
	
	;-------------------------------------------
	; SCM //////////////////////////////////////
	;-------------------------------------------
		MrMMS_Load_Data, sc, 'scm', mode, level, $
		                 OPTDESC   = 'scb', $
		                 VARFORMAT = '*_scb_brst_l2'
	
	;-------------------------------------------
	; EDP //////////////////////////////////////
	;-------------------------------------------
		MrMMS_Load_Data, sc, 'edp', mode, level, $
		                 OPTDESC   = 'dce', $
		                 VARFORMAT = '*dce_' + coords + '*'
	
	;-------------------------------------------
	; EDI //////////////////////////////////////
	;-------------------------------------------
		MrMMS_Load_Data, sc, 'edi', mode, level, $
		                 OPTDESC   = ['amb', 'amb-noabs', 'amb-pm2', 'amb-alt-cc', 'amb-alt-oc', $
		                              'amb_alt-ooc', 'amb-alt-oob'], $
		                 VARFORMAT = ['*flux?*', '*counts*', '*traj?*']
	ENDIF

	IF ~MrVar_IsCached(fgm_vname) THEN BEGIN
		fgm_vname = StrJoin( [sc, fgm_instr, 'vec', mode, fgm_level, coords], '_' )
		IF ~MrVar_IsCached(fgm_vname) THEN Message, 'Problem with FGM instrument name.'
	ENDIF

;-------------------------------------------
; Compute dFlux ////////////////////////////
;-------------------------------------------
	
	CASE pa OF
		0: BEGIN
			oFlux = MrVar_Get( flux_0_vname[chan-1] )
			oTraj = MrVar_Get( traj_0_vname[chan-1] )
		ENDCASE
		90: BEGIN
			oFlux = MrVar_Get( flux_90_vname[chan-1] )
			oTraj = MrVar_Get( traj_90_vname[chan-1] )
		ENDCASE
		180: BEGIN
			oFlux = MrVar_Get( flux_180_vname[chan-1] )
			oTraj = MrVar_Get( traj_180_vname[chan-1] )
		ENDCASE
		ELSE: Message, 'Invalid pitch angle (' + String(pa, FORMAT='(i0)') + ').'
	ENDCASE

	;Filter
	!Null = oFlux['TIMEVAR'] -> GetSI(RATE=sr)
	fN     = sr / 2.0
	odFlux = oFlux -> Digital_Filter(frange[0]/fN, frange[1]/fN, A, nTerms)

;-------------------------------------------
; Compute dB ///////////////////////////////
;-------------------------------------------
	oDCB = MrVar_Get(fgm_vname)
	oACB = MrVar_Get(scm_vname)
	oE   = MrVar_Get(edp_vname)
	
	;Interpolate to SCM
	oDCB_scm = oDCB -> Interpol(oACB)
	
	;Create & rotate into FAC
	oT     = MrVar_FAC( Temporary(oDCB_scm), !Null, 'CROSSX')
	oB_fac = oT ## oACB
	
	;Filter
	!Null  = oB_fac['TIMEVAR'] -> GetSI(RATE=sr)
	fN_scm = sr / 2.0
	odB    = oB_fac -> Digital_Filter(frange[0]/fN_scm, frange[1]/fN_scm, A, nTerms)
	odB   -> Split, odB_perp1, odB_perp2, odB_par
	
	;Perpendicular component
	odB_perp = MrScalarTS( odB_perp1['TIMEVAR'], Sqrt(odB_perp1['DATA']^2 + odB_perp2['DATA']^2) )
	
	;Clean up
	Obj_Destroy, oB_fac

;-------------------------------------------
; Compute dE ///////////////////////////////
;-------------------------------------------

	;Ensure E and B have the same time tags
	oE_scm = oE -> Interpol(oACB)
	oE_fac = oT ## oE_scm
	
	;Filter
	!Null  = oE_fac['TIMEVAR'] -> GetSI(RATE=sr)
	fN_edp = sr / 2.0
	odE    = oE_fac -> Digital_Filter(frange[0]/fN_edp, frange[1]/fN_edp, A, nTerms)
	odE   -> Split, odE_perp1, odE_perp2, odE_par
	
	;Perpendicular component
	odE_perp = MrScalarTS( odE_perp1['TIMEVAR'], Sqrt(odE_perp1['DATA']^2 + odE_perp2['DATA']^2) )
	
	;Clean up
	Obj_Destroy, [oE_fac, odE, oT]

;-------------------------------------------
; Project Onto EDI /////////////////////////
;-------------------------------------------
	;Split the trajectory into components
	phi   = oTraj['DATA',*,0] * !dtor
	theta = oTraj['DATA',*,1] * !dtor
	
	;Cartesian coordinates
	x = Sin(theta) * Cos(phi)
	y = Sin(theta) * Sin(phi)
	z = Cos(theta)
	oVec = MrVectorTS( oTraj['TIMEVAR'], [ [x], [y], [z] ] )
	
	;Interpolate
	oVec = oVec -> Interpol(oACB)
	oVec = oVec -> Normalize()
	
	;Project onto EDI
	odE_edi = (oE_scm -> Dot(oVec)) -> Digital_Filter(frange[0]/fN_edp, frange[1]/fN_edp, A, nTerms)
	odB_edi = (oACB   -> Dot(oVec)) -> Digital_Filter(frange[0]/fN_scm, frange[1]/fN_scm, A, nTerms)
	
	;Clean up
	phi   = !Null
	theta = !Null
	x     = !Null
	y     = !Null
	z     = !Null
	Obj_Destroy, [ oVec, oE_scm ]

;-------------------------------------------
; Get Component ////////////////////////////
;-------------------------------------------
	;Reference time
	oTime = oFlux['TIMEVAR']
	
	;Interpolate to fluxes
	CASE StrUpCase(field) OF
		'BEDI':   oField = odB_edi   -> Interpol(oTime)
		'BPERP':  oField = odBperp   -> Interpol(oTime)
		'BPERP1': oField = odB_perp1 -> Interpol(oTime)
		'BPERP2': oField = odB_perp2 -> Interpol(oTime)
		'BPAR':   oField = odB_par   -> Interpol(oTime)
		'EEDI':   oField = odE_edi   -> Interpol(oTime)
		'EPERP':  oField = odEperp   -> Interpol(oTime)
		'EPERP1': oField = odE_perp1 -> Interpol(oTime)
		'EPERP2': oField = odE_perp2 -> Interpol(oTime)
		'EPAR':   oField = odE_par   -> Interpol(oTime)
		ELSE: Message, 'Invalid field component: "' + field + '".'
	ENDCASE
	
	;Cleanup
	Obj_Destroy, [ odB_edi, odB_perp1, odB_perp2, odB_par, odB_perp, $
	               odE_edi, odE_perp1, odE_perp2, odE_par, odE_perp ]
	
;-------------------------------------------
; Trim to Reduced Time /////////////////////
;-------------------------------------------

	;Limit time interval
	MrVar_TLimit, [odFlux, oField], trange

;-------------------------------------------
; Lagged Correlations //////////////////////
;-------------------------------------------
	nPts  = N_Elements(odFlux)
	lag   = IndGen(nPts) - nPts/2
	
	;Lag variable
	oLag = MrVariable( lag/1024.0 * f_wave, NAME='lag' )
	
	;Correlate
	oXCorr  = MrVariable( C_Correlate(odFlux['DATA'], oField['DATA'], lag) )
	
	;Linear fit
	;   - The field will be plotted on the x-axis
;	params  = linfit( oField['DATA'], odFlux['DATA'], $
;	                  CHISQR         = chi, $
;	                  MEASURE_ERRORS = Sqrt(Abs(odFlux['DATA'])), $
;	                  SIGMA          = sig, $
;	                  YFIT           = yfit )
;	txt_fit  = string(params[0], params[1], FORMAT='(%"y = %0.2e*x + %0.2e")')
;	txt_qual = string(sig[0], chi, FORMAT='(%"\\sigma=%0.4f, \\chi^2=%0.2e")')
	
	;Least Absolute Deviation fit
	params = LADFit( oField['DATA'], odFlux['DATA'], $
	                 ABSDEV = absdev )
	yfit     = params[1] * oField['DATA'] + params[0]
	txt_fit  = String(params[0], params[1], FORMAT='(%"y = %0.2e*x + %0.2e")')
	txt_qual = String(absdev, FORMAT='(%"absdev=%0.2e")')
	
	;Create the variable
	oFit = MrVariable( yfit )
	
;-------------------------------------------
; Attributes ///////////////////////////////
;-------------------------------------------
	units = StrUpCase(StrMid(field, 0, 1)) eq 'E' ? 'mV/m' : 'nT'
	
	;Flux
	odFlux['LOG']        = 0
	odFlux['PLOT_TITLE'] = String(chan, pa, frange, FORMAT='(%"Ch%i PA%i Filter: %i-%i Hz")')
	odFlux['TITLE']      = 'dFlux!C(cm^-2 s^-2)'
	
	;Field
	oField['PLOT_TITLE'] = String(field, frange, FORMAT='(%"%s Filtered %i-%i Hz")')
	oField['TITLE']      = 'd' + field + ' (' + units + ')'
	
	;Lag
	oLag['TITLE'] = String(f_wave, FORMAT='(%"Lag*fw, fw=%0.1f")')
	oLag['UNITS'] = ''
	
	;Correlation
	oXCorr['DEPEND_0']   = oLag
	oXCorr['PLOT_TITLE'] = 'Lagged Cross Correlation of Flux with ' + field
	oXCorr['TITLE']      = '$\chi$$\up2$'
	
	;Fit
	oFit['DEPEND_0'] = oField
	oFit['COLOR']    = 'Blue'

;-------------------------------------------
; Plot Data ////////////////////////////////
;-------------------------------------------
	win = MrWindow(XSIZE=900, YSIZE=900, REFRESH=0)
	p1 = MrVar_Plot(odFlux, /CURRENT)
	p2 = MrVar_Plot(oField, /CURRENT)
	p3 = MrVar_Plot(oXCorr, /CURRENT)
	p4 = MrVar_Plot(oField, odFlux, TITLE='Scatter Plot', LINESTYLE='None', PSYM=9, /CURRENT)
	p5 = MrVar_Plot(oFit, OVERPLOT=p4)
	
	;Place text
	oTxt1 = MrText(0.05, 0.90, txt_fit,  COLOR='Blue', /RELATIVE, TARGET=p4)
	oTxt2 = MrText(0.05, 0.75, txt_qual, COLOR='Blue', /RELATIVE, TARGET=p4)
	
	win.oxmargin = [14,4]
	win -> Refresh

;-------------------------------------------
; Save Figure //////////////////////////////
;-------------------------------------------
	IF N_Elements(output_dir) GT 0 || N_Elements(output_ext) GT 0 THEN BEGIN
		;Defaults
		IF N_Elements(output_dir) EQ 0 THEN CD, CURRENT=output_dir
		
		;File name
		chanstr = 'ch' + String(chan, FORMAT='(i0)')
		pastr   = 'pa' + String(pa,   FORMAT='(i0)')
		fname   = StrJoin( [sc, 'edi', mode, level, 'xcorr-'+chanstr+'-'+pastr+'-'+StrLowCase(field)], '_' )
		fname   = FilePath( fname, ROOT_DIR=output_dir )
		
		;Save the figure
		fout = MrVar_PlotTS_Save( win, fname, output_ext )
	ENDIF

;-------------------------------------------
; Done! ////////////////////////////////////
;-------------------------------------------
	RETURN, win
END