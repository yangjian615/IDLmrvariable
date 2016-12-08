; docformat = 'rst'
;
; NAME:
;       MrMMS_Plot_SCM_PoyntXYZ
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
;       1. Bx SCM       By SCM       Bz SCM
;       2. Ex EDP       Ey EDP       Ez EDP
;       3. Bx SCM PSD   By SCM PSD   Bz SCM PSD
;       4. Ex EDP PSD   Ey EDP PSD   Ez EDP PSD
;       5. Sx           Sy           Sz
;
; :Categories:
;   MMS
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
;       2016/11/27  -   Written by Matthew Argall
;-
function MrMMS_Plot_SCM_PoyntXYZ, sc, mode, $
OPTDESC=optdesc, $
NFFT=nfft, $
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
	if n_elements(nfft)   eq 0 then nfft   = 512
	if n_elements(nshift) eq 0 then nshift = nfft / 2
	if n_elements(trange) gt 0 then MrVar_SetTRange, trange
	
;-------------------------------------------
; Variable Names ///////////////////////////
;-------------------------------------------
	level  = 'l2'
	coords = 'gse'
	if n_elements(optdesc) eq 0 then begin
		case mode of
			'slow': optdesc = 'scs'
			'fast': optdesc = 'scf'
			'srvy': optdesc = 'scsrvy'
			'brst': optdesc = 'scb'       ;also schb
			else: message, 'Invalid MODE: "' + mode + '".'
		endcase
	endif

	;Source names
	acb_vname    = sc + '_scm_acb_'       + coords + '_' + optdesc + '_' + mode + '_' + level
	dce_vname    = sc + '_edp_dce_'       + coords + '_'                 + mode + '_' + level
	s_vname      = sc + '_scm_poyntspec_' + coords + '_' + optdesc + '_' + mode + '_' + level
	
	;Output names
	bx_vname     = sc + '_scm_acb_'       + coords + '_' + optdesc + '_' + mode + '_' + level + '_x'
	by_vname     = sc + '_scm_acb_'       + coords + '_' + optdesc + '_' + mode + '_' + level + '_y'
	bz_vname     = sc + '_scm_acb_'       + coords + '_' + optdesc + '_' + mode + '_' + level + '_z'
	bx_psd_vname = sc + '_scm_bx_psd_'    + coords + '_' + optdesc + '_' + mode + '_' + level
	by_psd_vname = sc + '_scm_by_psd_'    + coords + '_' + optdesc + '_' + mode + '_' + level
	bz_psd_vname = sc + '_scm_bz_psd_'    + coords + '_' + optdesc + '_' + mode + '_' + level
	ex_vname     = sc + '_edp_dce_'       + coords + '_'                 + mode + '_' + level + '_x'
	ey_vname     = sc + '_edp_dce_'       + coords + '_'                 + mode + '_' + level + '_y'
	ez_vname     = sc + '_edp_dce_'       + coords + '_'                 + mode + '_' + level + '_z'
	ex_psd_vname = sc + '_edp_ex_psd_'    + coords + '_'                 + mode + '_' + level
	ey_psd_vname = sc + '_edp_ey_psd_'    + coords + '_'                 + mode + '_' + level
	ez_psd_vname = sc + '_edp_ez_psd_'    + coords + '_'                 + mode + '_' + level
	sx_vname     = sc + '_scm_poyntspec_' + coords + '_' + optdesc + '_' + mode + '_' + level + '_x'
	sy_vname     = sc + '_scm_poyntspec_' + coords + '_' + optdesc + '_' + mode + '_' + level + '_y'
	sz_vname     = sc + '_scm_poyntspec_' + coords + '_' + optdesc + '_' + mode + '_' + level + '_z'
	
;-------------------------------------------
; Get Data /////////////////////////////////
;-------------------------------------------
	if tf_load then begin
	;-------------------------------------------
	; SCM //////////////////////////////////////
	;-------------------------------------------
		;B & |B|
		MrMMS_Load_Data, sc, 'scm', mode, level, $
		                 OPTDESC = optdesc
	
	;-------------------------------------------
	; EDP //////////////////////////////////////
	;-------------------------------------------
		MrMMS_Load_Data, sc, 'edp', mode, level, /TEAM_SITE, $
		                 OPTDESC   = 'dce', $
		                 VARFORMAT = dce_vname
	endif
	
;-------------------------------------------
; Poynting Flux ////////////////////////////
;-------------------------------------------
	oSCM = MrMMS_SCM_BField( BFIELD=acb_vname, EFIELD=dce_vname )
	oS   = oSCM -> PoyntingSpectra(nfft, nshift, NAME=s_vname, /CACHE, WINDOW='hanning')
	
	obj_destroy, oSCM
	
;-------------------------------------------
; PSD //////////////////////////////////////
;-------------------------------------------
	;Detrend data
	oB  = MrVar_Get(acb_vname)
	oE  = MrVar_Get(dce_vname)
	odB = oB -> Detrend(512)
	odE = oE -> Detrend(512)

	odB     -> Split, odBx, odBy, odBz, /CACHE, NAMES=[bx_vname, by_vname, bz_vname]
	oBx_psd = odBx -> Spectrogram(nfft, nshift, NAME=bx_psd_vname, /CACHE, WINDOW='hanning')
	oBy_psd = odBy -> Spectrogram(nfft, nshift, NAME=by_psd_vname, /CACHE, WINDOW='hanning')
	oBz_psd = odBz -> Spectrogram(nfft, nshift, NAME=bz_psd_vname, /CACHE, WINDOW='hanning')
	
	odE     -> Split, odEx, odEy, odEz, /CACHE
	oEx_psd = odEx -> Spectrogram(nfft, nshift, NAME=ex_psd_vname, /CACHE, WINDOW='hanning')
	oEy_psd = odEy -> Spectrogram(nfft, nshift, NAME=ey_psd_vname, /CACHE, WINDOW='hanning')
	oEz_psd = odEz -> Spectrogram(nfft, nshift, NAME=ez_psd_vname, /CACHE, WINDOW='hanning')
	
;-------------------------------------------
; Properties ///////////////////////////////
;-------------------------------------------
	trange = MrVar_GetTRange()
	title  = strupcase(sc)

	;BX
	odBx['PLOT_TITLE'] = ''
	odBx['TITLE']      = 'dBx!C(nT)'
	
	;BY
	odBy['PLOT_TITLE'] = title
	odBy['TITLE']      = 'dBy'
	
	;BZ
	odBz['PLOT_TITLE'] = ''
	odBz['TITLE']      = 'dBz'

	;EX
	odEx['TITLE'] = 'dEx!C(mV/m)'
	
	;EY
	odEy['TITLE'] = 'dEy'
	
	;EZ
	odEz['TITLE'] = 'dEz'
	
	;BX PSD
	oBx_psd['AXIS_RANGE'] = [1e-9, 1e-1]
	oBx_psd['TITLE']      = 'nT^2/Hz'
	
	;BY PSD
	oFreq = oBy_psd['DEPEND_1']
	oBy_psd['AXIS_RANGE'] = [1e-9, 1e-1]
	oBy_psd['TITLE']      = 'nT^2/Hz'
	oFreq['TITLE']        = 'Freq'
	
	;BZ PSD
	oFreq = oBz_psd['DEPEND_1']
	oBz_psd['AXIS_RANGE'] = [1e-9, 1e-1]
	oBz_psd['TITLE']      = 'nT^2/Hz'
	oFreq['TITLE']        = 'Freq'
	
	;EX PSD
	oEx_psd['AXIS_RANGE'] = [1e-6, 1e-2]
	oEx_psd['TITLE']      = '(mV/m)^2/Hz'
	
	;EY PSD
	oFreq = oEy_psd['DEPEND_1']
	oEy_psd['AXIS_RANGE'] = [1e-6, 1e-2]
	oEy_psd['TITLE']      = '(mV/m)^2/Hz'
	oFreq['TITLE']        = 'Freq'
	
	;EZ PSD
	oFreq = oEz_psd['DEPEND_1']
	oEz_psd['AXIS_RANGE'] = [1e-6, 1e-2]
	oEz_psd['TITLE']      = '(mV/m)^2/Hz'
	oFreq['TITLE']        = 'Freq'
	
	;SX
	oSx = MrVar_Get(sx_vname)
	oT  = oSx['DEPEND_0']
	oSx['TITLE']     = '$\mu$W/m^2'
	oT['TICKFORMAT'] = 'time_labels'
	
	;SY
	oSy   = MrVar_Get(sy_vname)
	oT    = oSy['DEPEND_0']
	oFreq = oSy['DEPEND_1']
	oSy['TITLE']     = '$\mu$W/m^2'
	oT['TICKFORMAT'] = 'time_labels'
	oFreq['TITLE']   = 'Freq'
	
	;SZ
	oSz   = MrVar_Get(sz_vname)
	oT    = oSz['DEPEND_0']
	oFreq = oSz['DEPEND_1']
	oSz['TITLE']     = '$\mu$W/m^2'
	oT['TICKFORMAT'] = 'time_labels'
	oFreq['TITLE']   = 'Freq'
	
	;Poynting Flux range
;	range = [min( [oSx.min, oSy.min, oSz.min] ), max( [oSx.max, oSy.max, oSz.max] ) ]
;	range = [-1,1] * max(abs(range))
	range = [1e-13, 1e-13]
	oSx['AXIS_RANGE'] = range
	oSy['AXIS_RANGE'] = range
	oSz['AXIS_RANGE'] = range
	
;-------------------------------------------
; Plot Data ////////////////////////////////
;-------------------------------------------

	win = MrVar_PlotTS( [bx_vname,     by_vname,     bz_vname, $
	                     ex_vname,     ey_vname,     ez_vname, $
	                     bx_psd_vname, by_psd_vname, bz_psd_vname, $
	                     ex_psd_vname, ey_psd_vname, ez_psd_vname, $
	                     sx_vname,     sy_vname,     sz_vname], $
	                    LAYOUT = [3,5], $
	                    XSIZE  = 1100, $
	                    YSIZE  = 700 )
	
	win      -> Refresh, /DISABLE
	win.XGAP  = 20
	win.YGAP  = 0.5
	win[0]        -> SetLayout, [1,1]
	win           -> TrimLayout
	win[sx_vname] -> SetProperty, XTICKFORMAT='time_labels'
	win[sy_vname] -> SetProperty, XTICKFORMAT='time_labels', XTITLE='Time from ' + strmid(trange[0], 0, 10)
	win[sz_vname] -> SetProperty, XTICKFORMAT='time_labels'
	win           -> SetGlobal, XTICKS=2
	win           -> Refresh
	

	return, win
end