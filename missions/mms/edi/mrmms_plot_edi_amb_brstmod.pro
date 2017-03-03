; docformat = 'rst'
;
; NAME:
;       MrMMS_Plot_EDI_Amb_BrstMod
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
function MrMMS_Plot_EDI_Amb_BrstMod, sc, mode, $
OPTDESC=optdesc, $
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
	if n_elements(edp_optdesc) eq 0 then edp_optdesc = 'dce'
	if n_elements(trange)      gt 0 then MrVar_SetTRange, trange
	
	fLow   = 15
	fHigh  = 200
	A      = 50
	nTerms = 256
	
	
;-------------------------------------------
; Variable Names ///////////////////////////
;-------------------------------------------
	level  = 'l2'
	coords = 'gse'
	if n_elements(optdesc) eq 0 then begin
		case mode of
			'slow': scm_optdesc = 'scs'
			'fast': scm_optdesc = 'scf'
			'srvy': scm_optdesc = 'scsrvy'
			'brst': scm_optdesc = 'scb'       ;also schb
			else: message, 'Invalid MODE: "' + mode + '".'
		endcase
	endif

	;Source names
	acb_vname       = sc + '_scm_acb_'     + coords + '_' + scm_optdesc + '_' + mode + '_' + level
	dce_vname       = sc + '_edp_dce_'     + coords + '_'                     + mode + '_' + level
	flux1_0_vname   = sc + '_edi_flux1_0_'                             + mode + '_' + level
	flux2_0_vname   = sc + '_edi_flux2_0_'                             + mode + '_' + level
	flux3_0_vname   = sc + '_edi_flux3_0_'                             + mode + '_' + level
	flux4_0_vname   = sc + '_edi_flux4_0_'                             + mode + '_' + level
	flux1_180_vname = sc + '_edi_flux1_180_'                             + mode + '_' + level
	flux2_180_vname = sc + '_edi_flux2_180_'                             + mode + '_' + level
	flux3_180_vname = sc + '_edi_flux3_180_'                             + mode + '_' + level
	flux4_180_vname = sc + '_edi_flux4_180_'                             + mode + '_' + level
	traj1_0_vname   = sc + '_edi_traj1_0_'   + coords                    + mode + '_' + level
	traj2_0_vname   = sc + '_edi_traj2_0_'   + coords                    + mode + '_' + level
	traj3_0_vname   = sc + '_edi_traj3_0_'   + coords                    + mode + '_' + level
	traj4_0_vname   = sc + '_edi_traj4_0_'   + coords                    + mode + '_' + level
	traj1_180_vname = sc + '_edi_traj1_180_' + coords                    + mode + '_' + level
	traj2_180_vname = sc + '_edi_traj2_180_' + coords                    + mode + '_' + level
	traj3_180_vname = sc + '_edi_traj3_180_' + coords                    + mode + '_' + level
	traj4_180_vname = sc + '_edi_traj4_180_' + coords                    + mode + '_' + level
	
;-------------------------------------------
; Get Data /////////////////////////////////
;-------------------------------------------
	if tf_load then begin
	;-------------------------------------------
	; SCM //////////////////////////////////////
	;-------------------------------------------
		;Bxyz
		MrMMS_Load_Data, sc, 'scm', mode, level, $
		                 OPTDESC = scm_ptdesc, $
		                 VARFORMAT = '*gse_scb*'

	;-------------------------------------------
	; EDP //////////////////////////////////////
	;-------------------------------------------
		;Exyz
;		MrMMS_Load_Data, sc, 'edp', mode, level, $
;		                 OPTDESC   = edp_optdesc, $
;		                 VARFORMAT = '*dce_gse*'

	;-------------------------------------------
	; EDI //////////////////////////////////////
	;-------------------------------------------
		;Flux
		MrMMS_Load_Data, sc, 'edi', mode, level, $
		                 OPTDESC   = ['amb', 'amb-noabs', 'amb-pm2', 'amb-alt-cc', $
		                              'amb-alt-oc', 'amb-alt-oob', 'amb-alt-oom'], $
		                 VARFORMAT = ['*flux?_0_brst*', '*flux?_180_brst*', '*traj?_gse*']
	endif
	

;-------------------------------------------
; Filter the Data //////////////////////////
;-------------------------------------------
	sr_edi = 1024.0
	sr_scm = 8192.0
	sr_edp = 8192.0

	;SCM
	oVar = MrVar_Get(acb_vname)
	odB  = oVar -> Digital_Filter( fLow/sr_scm*2.0 < 1.0, fHigh/sr_scm*2.0 < 1.0, A, nTerms, $
	                               /CACHE, $
	                               NAME = acb_vname + '_filter' )
	
	;EDP
;	oVar = MrVar_Get(dce_vname)
;	odE  = oVar -> Digital_Filter( fLow/sr_edp*2.0 < 1.0, fHigh/sr_edp*2.0 < 1.0, A, nTerms, $
;	                               /CACHE, $
;	                               NAME = dce_vname + '_filter' )
	
	;EDI
	theMax     = -!values.f_infinity
	edi_vnames = [ flux1_0_vname,   flux2_0_vname,   flux3_0_vname,   flux4_0_vname, $
	               flux1_180_vname, flux2_180_vname, flux3_180_vname, flux4_180_vname ]
	for i = 0, n_elements(edi_vnames) - 1 do begin
		;Filter the variable
		oVar = MrVar_Get(edi_vnames[i])
		oNew = oVar -> Digital_Filter( fLow/sr_edi*2.0 < 1.0, fHigh/sr_edi*2.0 < 1.0, A, nTerms, $
		                               /CACHE, $
		                               NAME = edi_vnames[i] + '_filter' )
		
		;Keep track of the range
		theMax = theMax > oNew -> Max(/ABSOLUTE)
	endfor
	
	;Output vnames
	acb_out_vname  = acb_vname  + '_filter'
	dce_out_vname  = dce_vname  + '_filter'
	edi_out_vnames = edi_vnames + '_filter'

;-------------------------------------------
; Attributes ///////////////////////////////
;-------------------------------------------
	title = string(strupcase(sc), fLow, fHigh, FORMAT='(%"%s Filter %i-%i Hz")')
	
	;B
	odB['LABEL']      = ['Bx', 'By', 'Bz']
	odB['PLOT_TITLE'] = title
	odB['TITLE']      = 'dB!C(nT)'
	
	;E
;;	odE['LABEL']      = ['Ex', 'Ey', 'Ez']
;	odE['TITLE']      = 'dE!C(mV/m)'
	
	;Flux
	for i = 0, n_elements(edi_vnames) - 1 do begin
		pa   = i le 3 ? '0' : '180'
		chan = (i mod 4) + 1
		oVar = MrVar_Get(edi_out_vnames[i])
		oVar['AXIS_RANGE'] = [-theMax, theMax]
		oVar['TITLE']      = string(pa, chan, FORMAT='(%"PA%s!CCh%i")')
	endfor

;-------------------------------------------
; Plot Data ////////////////////////////////
;-------------------------------------------
	win = MrVar_PlotTS( [acb_out_vname, edi_out_vnames], $  ;[acb_out_vname, dce_out_vname, edi_out_vnames], $
	                    XSIZE  = 800, $
	                    YSIZE  = 900 )
	
	win    -> Refresh, /DISABLE
	win[0] -> SetLayout, [1,1]
	win    -> TrimLayout
	win.oxmargin = [15,5]
	
	win -> refresh
	return, win
end