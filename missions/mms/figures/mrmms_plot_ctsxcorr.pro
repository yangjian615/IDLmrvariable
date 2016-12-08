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
function MrMMS_Plot_CtsXCorr, sc, mode, cts, $
NO_LOAD=no_load, $
TRANGE=trange
	compile_opt idl2
	
	catch, the_error
	if the_error ne 0 then begin
		catch, /CANCEL
		if n_elements(win) gt 0 then obj_destroy, win
		MrPrintF, 'LogErr'
		return, !Null
	endif
	
	tf_load = ~keyword_set(no_load)
	if n_elements(cts)    eq 0 then cts  = '0'
	if n_elements(trange) gt 0 then MrVar_SetTRange, trange
	
;-------------------------------------------
; Variable Names ///////////////////////////
;-------------------------------------------
	level    = 'l2'
	edp_mode = mode eq 'srvy' ? 'fast' : mode
	fgm_mode = mode
	dsp_mode = 'fast'
	fpi_mode = mode eq 'brst' ? mode : 'fast'
	coords   = 'gse'
	
	case mode of
		'slow': scm_desc = 'scs'
		'fast': scm_desc = 'scf'
		'srvy': scm_desc = 'scsrvy'
		'brst': scm_desc = 'scb'
		else: message, 'Invalid mode: "' + mode + '".'
	endcase

	;Source names
	fgm_vname     = sc + '_fgm_bvec_'  + coords + '_' + fgm_mode + '_' + level
	scm_vname     = sc + '_scm_acb_'   + coords + '_' + scm_desc + '_' + mode + '_' + level
	edp_vname     = sc + '_edp_dce_'   + coords + '_' + mode + '_' + level
	edi1_vname    = sc + '_edi_flux1_' + cts + '_' + mode + '_' + level
	edi2_vname    = sc + '_edi_flux2_' + cts + '_' + mode + '_' + level
	edi3_vname    = sc + '_edi_flux3_' + cts + '_' + mode + '_' + level
	edi4_vname    = sc + '_edi_flux4_' + cts + '_' + mode + '_' + level
	
;-------------------------------------------
; Get Data /////////////////////////////////
;-------------------------------------------
	if tf_load then begin
	;-------------------------------------------
	; FGM //////////////////////////////////////
	;-------------------------------------------
		MrMMS_FGM_Load_Data, sc, fgm_mode, $
		                     VARFORMAT = '*b_' + coords + '*'
	
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
		                 OPTDESC   = ['amb', 'amb-pm2', 'amb-alt-cc', 'amb-alt-oc', $
		                              'amb_alt-ooc', 'amb-alt-oob'], $
		                 VARFORMAT = ['*flux?_' + cts + '_' + mode + '_*', '*counts*']
	endif

;-------------------------------------------
; Compute dB ///////////////////////////////
;-------------------------------------------
	oB     = MrVar_Get(scm_vname)
	oE     = MrVar_Get(edp_vname)
	fmin   = 100
	fmax   = 512
	A      = 50
	nTerms = 512
	
	;Create FAC
	oSCM    = MrMMS_SCM_BField( BFIELD=scm_vname, B0=fgm_vname )
	oSCM   -> SetFAC, 'CROSSX'
	oT      = oSCM -> T_FAC()
	obj_destroy, oSCM
	
	;Filter
	oB  -> Split, oBx, oBy, oBz
	odBx = oBx -> Digital_Filter(fmin/4096.0, fmax/4096.0, A, nTerms)
	odBy = oBy -> Digital_Filter(fmin/4096.0, fmax/4096.0, A, nTerms)
	odBz = oBz -> Digital_Filter(fmin/4096.0, fmax/4096.0, A, nTerms)
	
	;Rotate to FAC
	odB     = MrVectorTS(oB['TIMEVAR'], [ [odBx['DATA']], [odBy['DATA']], [odBz['DATA']] ])
	odB_fac = oT ## odB
	
	;Split into components
	odB_fac -> Split, odBx, odBy, odBz
	odBperp = MrScalarTS( odBx['TIMEVAR'], sqrt(odBx['DATA']^2 + odBy['DATA']^2) )

;-------------------------------------------
; Compute dE ///////////////////////////////
;-------------------------------------------

	;Ensure E and B have the same time tags
	oE_scm = oE -> Interpol(oB)
	
	;Filter
	oE     -> Split, oEx, oEy, oEz
	odEx    = oEx -> Digital_Filter(fmin/4096.0, fmax/4096.0, A, nTerms)
	odEy    = oEy -> Digital_Filter(fmin/4096.0, fmax/4096.0, A, nTerms)
	odEz    = oEz -> Digital_Filter(fmin/4096.0, fmax/4096.0, A, nTerms)
	
	;Rotate to FAC
	odE     = MrVectorTS(oE['TIMEVAR'], [ [odEx['DATA']], [odEy['DATA']], [odEz['DATA']] ])
	odE_fac = oT ## odB
	
	;Split into components
	odE_fac -> Split, odEx, odEy, odEz
	odEperp = MrScalarTS( odEx['TIMEVAR'], sqrt(odEx['DATA']^2 + odEy['DATA']^2) )

;-------------------------------------------
; Compute dFlux ////////////////////////////
;-------------------------------------------
	oCts1 = MrVar_Get(edi1_vname)
	oCts2 = MrVar_Get(edi2_vname)
	oCts3 = MrVar_Get(edi3_vname)
	oCts4 = MrVar_Get(edi4_vname)
	oTime = oCts1['TIMEVAR']

	;Detrend the counts
;	odC1  = oCts1 -> Detrend(512, /EDGE_TRUNCATE)
	odC1  = oCts1 -> Digital_Filter(fmin/512.0, fmax/512.0, A, nTerms)
	odC2  = oCts2 -> Digital_Filter(fmin/512.0, fmax/512.0, A, nTerms)
	odC3  = oCts3 -> Digital_Filter(fmin/512.0, fmax/512.0, A, nTerms)
	odC4  = oCts4 -> Digital_Filter(fmin/512.0, fmax/512.0, A, nTerms)

;-------------------------------------------
; Interpolate //////////////////////////////
;-------------------------------------------
	;Reference time
	oTime = oCts1['TIMEVAR']
	
	;Flux
	odC2 = odC2 -> Interpol(oTime)
	odC3 = odC3 -> Interpol(oTime)
	odC4 = odC4 -> Interpol(oTime)
	
	;B
	odBx    = odBx -> Interpol(oTime)
	odBy    = odBy -> Interpol(oTime)
	odBz    = odBz -> Interpol(oTime)
	odBperp = odBperp -> Interpol(oTime)
	
	;E
	odEx    = odEx -> Interpol(oTime)
	odEy    = odEy -> Interpol(oTime)
	odEz    = odEz -> Interpol(oTime)
	odEperp = odEperp -> Interpol(oTime)
	
;-------------------------------------------
; Correlate ////////////////////////////////
;-------------------------------------------
	
	;Find the values
	trange = MrTimeVar(['2015-12-06T23:38:31.3', '2015-12-06T23:38:31.5'])
;	trange = MrTimeVar(['2015-12-06T23:38:31.5', '2015-12-06T23:38:31.9'])
	tt2000 = trange -> GetData('TT2000')
	oT     = oCts1['TIMEVAR']
	it     = oT -> Value_Locate(tt2000, 'TT2000')
	
	nPts  = it[1]-it[0]+1
	lag   = indgen(nPts) - nPts/2
	bperp = odBperp[it[0]:it[1]]
	bx    = odBx[it[0]:it[1]]
	by    = odBy[it[0]:it[1]]
	bz    = odBz[it[0]:it[1]]
	eperp = odEperp[it[0]:it[1]]
	ex    = odEx[it[0]:it[1]]
	ey    = odEy[it[0]:it[1]]
	ez    = odEz[it[0]:it[1]]
	cts1  = odC1[it[0]:it[1]]
	cts2  = odC2[it[0]:it[1]]
	cts3  = odC3[it[0]:it[1]]
	cts4  = odC4[it[0]:it[1]]
	
	;Lag variable
	oLag = MrVariable( lag/1024.0, NAME='lag' )
	oLag['TITLE'] = 'Lag (s)'
	oLag['UNITS'] = 'sec'
	
	;Lagged cross-correlation
	oXC1Bx    = MrVariable( c_correlate(cts1, bx, lag) )
	oXC1By    = MrVariable( c_correlate(cts1, by, lag) )
	oXC1Bz    = MrVariable( c_correlate(cts1, bz, lag) )
	oXC1Bperp = MrVariable( c_correlate(cts1, bperp, lag) )
	oXC1Ex    = MrVariable( c_correlate(cts1, ex, lag) )
	oXC1Ey    = MrVariable( c_correlate(cts1, ey, lag) )
	oXC1Ez    = MrVariable( c_correlate(cts1, ez, lag) )
	oXC1Eperp = MrVariable( c_correlate(cts1, eperp, lag) )
	
;-------------------------------------------
; Properties ///////////////////////////////
;-------------------------------------------
	
	odC1['PLOT_TITLE'] = string(cts, fmin, fmax, FORMAT='(%"Flux1 PA%s Filter: %i-%i Hz")')
	odC1['TITLE']      = 'dFlux!C(cm^-2 s^-2)'
	
	odBx['PLOT_TITLE'] = string(fmin, fmax, FORMAT='(%"Bx Filtered %i-%i Hz")')
	odBx['TITLE']      = 'dBx (nT)'
	
	odBy['PLOT_TITLE'] = string(fmin, fmax, FORMAT='(%"By Filtered %i-%i Hz")')
	odBy['TITLE']      = 'dBy (nT)'
	
	odBz['PLOT_TITLE'] = string(fmin, fmax, FORMAT='(%"Bz Filtered %i-%i Hz")')
	odBz['TITLE']      = 'dBz (nT)'
	
	odBperp['PLOT_TITLE'] = string(fmin, fmax, FORMAT='(%"|Bperp| Filtered %i-%i Hz")')
	odBperp['TITLE']      = 'dBperp (nT)'
	
	odEx['PLOT_TITLE'] = string(fmin, fmax, FORMAT='(%"Ex Filtered %i-%i Hz")')
	odEx['TITLE']      = 'dEx (mV/m)'
	
	odEy['PLOT_TITLE'] = string(fmin, fmax, FORMAT='(%"Ey Filtered %i-%i Hz")')
	odEy['TITLE']      = 'dEy (mV/m)'
	
	odEz['PLOT_TITLE'] = string(fmin, fmax, FORMAT='(%"Ez Filtered %i-%i Hz")')
	odEz['TITLE']      = 'dEz (mV/m)'
	
	odEperp['PLOT_TITLE'] = string(fmin, fmax, FORMAT='(%"|Eperp| Filtered %i-%i Hz")')
	odEperp['TITLE']     = 'dEperp (mV/m)'
	
	;C1 vs Bx
	oXC1Bx['DEPEND_0']   = oLag
	oXC1Bx['PLOT_TITLE'] = 'Lagged Cross Correlation of Flux with Bx'
	oXC1Bx['TITLE']      = '$\chi$$\up2$'
	
	;C1 vs By
	oXC1By['DEPEND_0']   = oLag
	oXC1By['PLOT_TITLE'] = 'Lagged Cross Correlation of Flux with By'
	oXC1By['TITLE']      = '$\chi$$\up2$'
	
	;C1 vs Bz
	oXC1Bz['DEPEND_0']   = oLag
	oXC1Bz['PLOT_TITLE'] = 'Lagged Cross Correlation of Flux with Bz'
	oXC1Bz['TITLE']      = '$\chi$$\up2$'
	
	;C1 vs Bperp
	oXC1Bperp['DEPEND_0']   = oLag
	oXC1Bperp['PLOT_TITLE'] = 'Lagged Cross Correlation of Flux with |Bperp|'
	oXC1Bperp['TITLE']      = '$\chi$$\up2$'
	
	;C1 vs Ex
	oXC1Ex['DEPEND_0']   = oLag
	oXC1Ex['PLOT_TITLE'] = 'Lagged Cross Correlation of Flux with Ex'
	oXC1Ex['TITLE']      = '$\chi$$\up2$'
	
	;C1 vs Ey
	oXC1Ey['DEPEND_0']   = oLag
	oXC1Ey['PLOT_TITLE'] = 'Lagged Cross Correlation of Flux with Ey'
	oXC1Ey['TITLE']      = '$\chi$$\up2$'
	
	;C1 vs Ez
	oXC1Ez['DEPEND_0']   = oLag
	oXC1Ez['PLOT_TITLE'] = 'Lagged Cross Correlation of Flux with Ez'
	oXC1Ez['TITLE']      = '$\chi$$\up2$'
	
	;C1 vs Eperp
	oXC1Eperp['DEPEND_0']   = oLag
	oXC1Eperp['PLOT_TITLE'] = 'Lagged Cross Correlation of Flux with |Eperp|'
	oXC1Eperp['TITLE']      = '$\chi$$\up2$'

;-------------------------------------------
; Variable Attributes //////////////////////
;-------------------------------------------
	odC1['LOG'] = 0
	odC2['LOG'] = 0
	odC3['LOG'] = 0
	odC4['LOG'] = 0

;-------------------------------------------
; Plot Data ////////////////////////////////
;-------------------------------------------
	win = MrWindow(XSIZE=900, YSIZE=900)
	p1 = MrVar_Plot(odC1, /CURRENT, XRANGE=trange[*,'SSM'])
	win = p1.window
	win -> Refresh, /DISABLE
	win.oxmargin = [14,4]
	
	p2 = MrVar_Plot(odBperp, /CURRENT, XRANGE=trange[*,'SSM'])
	p3 = MrVar_Plot(oXC1Bperp, /CURRENT)
	p4 = MrVar_Plot(odBperp, odC1, TITLE='Scatter Plot', LINESTYLE='None', PSYM=9, /CURRENT) 

	win -> Refresh
	return, win
end