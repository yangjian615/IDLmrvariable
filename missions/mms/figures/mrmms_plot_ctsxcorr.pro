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
;-
FUNCTION MrMMS_Plot_CtsXCorr, sc, mode, chan, pa, field, $
NO_LOAD=no_load, $
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
	IF N_Elements(field)  EQ 0 THEN field  = 'Bx'
	IF N_Elements(pa)     EQ 0 THEN pa     = 0
	IF N_Elements(f_wave) EQ 0 THEN f_wave = 135.0
	IF N_Elements(trange) GT 0 THEN MrVar_SetTRange, trange
	
	fmin   = 100
	fmax   = 400
	A      = 50
	nTerms = 512
	
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
	f1pa0_vname   = StrJoin( [sc, 'edi', 'flux1',   '0', mode, level], '_' )
	f2pa0_vname   = StrJoin( [sc, 'edi', 'flux2',   '0', mode, level], '_' )
	f3pa0_vname   = StrJoin( [sc, 'edi', 'flux3',   '0', mode, level], '_' )
	f4pa0_vname   = StrJoin( [sc, 'edi', 'flux4',   '0', mode, level], '_' )
	f1pa90_vname  = StrJoin( [sc, 'edi', 'flux1',  '90', mode, level], '_' )
	f2pa90_vname  = StrJoin( [sc, 'edi', 'flux2',  '90', mode, level], '_' )
	f3pa90_vname  = StrJoin( [sc, 'edi', 'flux3',  '90', mode, level], '_' )
	f4pa90_vname  = StrJoin( [sc, 'edi', 'flux4',  '90', mode, level], '_' )
	f1pa180_vname = StrJoin( [sc, 'edi', 'flux1', '180', mode, level], '_' )
	f2pa180_vname = StrJoin( [sc, 'edi', 'flux2', '180', mode, level], '_' )
	f3pa180_vname = StrJoin( [sc, 'edi', 'flux3', '180', mode, level], '_' )
	f4pa180_vname = StrJoin( [sc, 'edi', 'flux4', '180', mode, level], '_' )
	
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
		                 VARFORMAT = ['*flux?*', '*counts*']
	ENDIF

	IF ~MrVar_IsCached(fgm_vname) THEN BEGIN
		fgm_vname = StrJoin( [sc, fgm_instr, 'vec', mode, fgm_level, coords], '_' )
		IF ~MrVar_IsCached(fgm_vname) THEN Message, 'Problem with FGM instrument name.'
	ENDIF

;-------------------------------------------
; Compute dFlux ////////////////////////////
;-------------------------------------------
	pa0_vnames   = [  f1pa0_vname,   f2pa0_vname,   f3pa0_vname,   f4pa0_vname]
	pa90_vnames  = [ f1pa90_vname,  f2pa90_vname,  f3pa90_vname,  f4pa90_vname]
	pa180_vnames = [f1pa180_vname, f2pa180_vname, f3pa180_vname, f4pa180_vname]
	
	CASE pa OF
		  0: oFlux = MrVar_Get(   pa0_vnames[chan-1] )
		 90: oFlux = MrVar_Get(  pa90_vnames[chan-1] )
		180: oFlux = MrVar_Get( pa180_vnames[chan-1] )
		ELSE: Message, 'Invalid pitch angle (' + String(pa, FORMAT='(i0)') + ').'
	ENDCASE

	;Filter
	odFlux = oFlux -> Digital_Filter(fmin/512.0, fmax/512.0, A, nTerms)

;-------------------------------------------
; Compute dB ///////////////////////////////
;-------------------------------------------
	oDCB   = MrVar_Get(fgm_vname)
	oB     = MrVar_Get(scm_vname)
	oE     = MrVar_Get(edp_vname)
	
	;Interpolate to SCM
	oDCB_scm = oDCB -> Interpol(oB)
	
	;Create & rotate into FAC
	oT     = MrVar_FAC( Temporary(oDCB_scm), 'CROSSX')
	oB_fac = oT ## oB
	
	;Filter
	odB  = oB_fac -> Digital_Filter(fmin/4096.0, fmax/4096.0, A, nTerms)
	odB -> Split, odBx, odBy, odBz
	
	;Perpendicular component
	odBperp = MrScalarTS( odBx['TIMEVAR'], Sqrt(odBx['DATA']^2 + odBy['DATA']^2) )
	
	;Clean up
	Obj_Destroy, oB_fac

;-------------------------------------------
; Compute dE ///////////////////////////////
;-------------------------------------------

	;Ensure E and B have the same time tags
	oE_scm = oE -> Interpol(oB)
	oE_fac = oT ## Temporary(oE_scm)
	
	;Filter
	odE  = oE_fac -> Digital_Filter(fmin/4096.0, fmax/4096.0, A, nTerms)
	odE -> Split, odEx, odEy, odEz
	
	;Perpendicular component
	odEperp = MrScalarTS( odEx['TIMEVAR'], Sqrt(odEx['DATA']^2 + odEy['DATA']^2) )
	
	;Clean up
	Obj_Destroy, [oE_fac, odE, oT]

;-------------------------------------------
; Get Component ////////////////////////////
;-------------------------------------------
	;Reference time
	oTime = oFlux['TIMEVAR']
	
	;Interpolate to fluxes
	CASE StrUpCase(field) OF
		'BX':    oField = odBx    -> Interpol(oTime)
		'BY':    oField = odBy    -> Interpol(oTime)
		'BZ':    oField = odBz    -> Interpol(oTime)
		'BPERP': oField = odBperp -> Interpol(oTime)
		'EX':    oField = odEx    -> Interpol(oTime)
		'EY':    oField = odEy    -> Interpol(oTime)
		'EZ':    oField = odEz    -> Interpol(oTime)
		'EPERP': oField = odEperp -> Interpol(oTime)
		ELSE: Message, 'Invalid field component: "' + field + '".'
	ENDCASE
	
	;Cleanup
	obj_destroy, [odBx, odBy, odBz, odBperp, odEx, odEy, odEz, odEperp]
	
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
	params = ladfit( oField['DATA'], odFlux['DATA'], $
	                 ABSDEV = absdev )
	yfit     = params[1] * oField['DATA'] + params[0]
	txt_fit  = string(params[0], params[1], FORMAT='(%"y = %0.2e*x + %0.2e")')
	txt_qual = string(absdev, FORMAT='(%"absdev=%0.2e")')
	
	;Create the variable
	oFit = MrVariable( yfit )
	
;-------------------------------------------
; Attributes ///////////////////////////////
;-------------------------------------------
	units = StrUpCase(StrMid(field, 0, 1)) eq 'E' ? 'mV/m' : 'nT'
	
	;Flux
	odFlux['LOG']        = 0
	odFlux['PLOT_TITLE'] = String(chan, pa, fmin, fmax, FORMAT='(%"Ch%i PA%i Filter: %i-%i Hz")')
	odFlux['TITLE']      = 'dFlux!C(cm^-2 s^-2)'
	
	;Field
	oField['PLOT_TITLE'] = String(field, fmin, fmax, FORMAT='(%"%s Filtered %i-%i Hz")')
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

	RETURN, win
END