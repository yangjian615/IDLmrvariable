; docformat = 'rst'
;
; NAME:
;       MrMMS_Plot_Whistler_PSD
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
function MrMMS_Plot_Whistler_PSD, sc, mode, $
NDETREND=nDetrend, $
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
	if n_elements(nDetrend) eq 0 then nDetrend = 256
	if n_elements(trange)   gt 0 then MrVar_SetTRange, trange
	
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
	dcb_vec_vname   = sc + '_fgm_bvec_'    + coords                     + '_' + mode + '_' + level
	acb_vname       = sc + '_scm_acb_'     + coords + '_' + scm_optdesc + '_' + mode + '_' + level
	scpot_vname     = sc + '_edp_scpot_'     + mode + '_' + level
	flux1_0_vname   = sc + '_edi_flux1_0_'   + mode + '_' + level
	flux2_0_vname   = sc + '_edi_flux2_0_'   + mode + '_' + level
	flux3_0_vname   = sc + '_edi_flux3_0_'   + mode + '_' + level
	flux4_0_vname   = sc + '_edi_flux4_0_'   + mode + '_' + level
	flux1_180_vname = sc + '_edi_flux1_180_' + mode + '_' + level
	flux2_180_vname = sc + '_edi_flux2_180_' + mode + '_' + level
	flux3_180_vname = sc + '_edi_flux3_180_' + mode + '_' + level
	flux4_180_vname = sc + '_edi_flux4_180_' + mode + '_' + level
	
	;Combine EDI names
	edi_vnames = [flux1_0_vname,   flux2_0_vname,   flux3_0_vname,   flux4_0_vname, $
	              flux1_180_vname, flux2_180_vname, flux3_180_vname, flux4_180_vname]

;-------------------------------------------
; Get Data /////////////////////////////////
;-------------------------------------------
	if tf_load then begin
	;-------------------------------------------
	; FGM //////////////////////////////////////
	;-------------------------------------------
		;Bxyz
		MrMMS_FGM_Load_Data, sc, mode, $
		                     VARFORMAT = '*b_gse*'
		
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
		;Vsc
		MrMMS_Load_Data, sc, 'edp', mode, level, $
		                 OPTDESC   = 'scpot', $
		                 VARFORMAT = '*scpot*'

	;-------------------------------------------
	; EDI //////////////////////////////////////
	;-------------------------------------------
		;Flux
		MrMMS_Load_Data, sc, 'edi', mode, level, $
		                 OPTDESC   = ['amb', 'amb-pm2', 'amb-alt-cc', $
		                              'amb-alt-oc', 'amb-alt-oob', 'amb-alt-oom'], $
		                 VARFORMAT = ['*flux?_0_brst*', '*flux?_180_brst*']
	endif

;-------------------------------------------
; Background Field /////////////////////////
;-------------------------------------------
	db_vname      = acb_vname + '_db'
	dbpar_vname   = acb_vname + '_db_par'
	dbperp_vname  = acb_vname + '_db_perp'

	;Field-aligned coordinates
	oSCM  = MrMMS_SCM_BField( BFIELD=acb_vname, B0=dcb_vec_vname)
	oSCM -> SetFAC, 'CROSSX'
	odb   = oSCM -> db(/CACHE, NAME=db_vname)
	
	;Magnitude of perpendicular direction
	odb     -> Split, odb_par, odb_perp1, odb_perp2
	db_perp  = sqrt(odb_perp1['DATA']^2 + odb_perp2['DATA']^2)
	odb_perp = MrScalarTS( odb_perp1['TIMEVAR'], db_perp, $
	                       /CACHE, $
	                       NAME = dbperp_vname, $
	                       /NO_COPY )
	
	;Clear data
	obj_destroy, [oSCM, odb_perp1, odb_perp2]

;-------------------------------------------
; PSD //////////////////////////////////////
;-------------------------------------------
	;Output variable names
	dbperp_psd_vname = dbperp_vname + '_psd'
	dbpar_psd_vname  = dbpar_vname  + '_psd'
	scpot_psd_vname  = scpot_vname  + '_psd'
	edi_psd_vnames   = edi_vnames   + '_psd'

	;SCM
	oPSD = odb_par  -> PSD( /CACHE, NAME=dbpar_psd_vname, WINDOW='hanning' )
	oPSD = odb_perp -> PSD( /CACHE, NAME=dbperp_psd_vname, WINDOW='hanning' )
	
	;SCPOT
	oVar = MrVar_Get(scpot_vname)
	oPSD = oVar -> PSD( /CACHE, NAME=scpot_psd_vname, NDETREND=nDetrend )
	
	;EDI
	for i = 0, n_elements(edi_vnames) - 1 do begin
		oVar = MrVar_Get(edi_vnames[i])
		oPSD = oVar -> PSD( /CACHE, $
		                    NAME     = edi_psd_vnames[i], $
		                    NDETREND = nDetrend, $
		                    WINDOW   = 'hanning' )
	endfor

;-------------------------------------------
; Attributes ///////////////////////////////
;-------------------------------------------
	;Plot title
	tspan = MrVar_GetTRange()
	dt    = MrVar_GetTRange('SSM')
	dt    = dt[1] - dt[0]
	title = strupcase(sc) + ' ' + strmid(tspan[0], 0, 10) + ' ' + strmid(tspan[0], 11, 8) + $
	        ' (' + string(dt, FORMAT='(f0.2)') + 's)'

	;B PAR
	oPar               = MrVar_Get(dbpar_psd_vname)
	oT                 = MrVar_Get(oPar['DEPEND_0'])
	oPar['PLOT_TITLE'] = title
	oPar['COLOR']      = 'Blue'
	oPar['LABEL']      = 'Par'
	oPar['TITLE']      = 'dB!C(PSD)'
	oT['TICKFORMAT']   = '(a1)'
	oT['TITLE']        = ''
	
	;B PERP
	oPerp               = MrVar_Get(dbperp_psd_vname)
	oT                  = MrVar_Get(oPerp['DEPEND_0'])
	oPerp['PLOT_TITLE'] = title
	oPerp['COLOR']      = 'Red'
	oPerp['LABEL']      = 'Perp'
	oPerp['TITLE']      = 'dB!C(PSD)'
	oT['TICKFORMAT']    = '(a1)'
	oT['TITLE']         = ''
	
	;SCPOT
	oSCPot               = MrVar_Get(scpot_psd_vname)
	oT                   = MrVar_Get(oSCPot['DEPEND_0'])
	oSCPot['PLOT_TITLE'] = ''
	oSCPot['TITLE']      = 'SCPot!C(PSD)'
	oT['TICKFORMAT']     = '(a1)'
	oT['TITLE']          = ''
	
	;PA0
	for i = 0, 3 do begin
		oVar               = MrVar_Get(edi_psd_vnames[i])
		oT                 = MrVar_Get(oVar['DEPEND_0'])
		oVar['TITLE']      = string(i+1, FORMAT='(%"Flux%i PA0!C(PSD)")')
		oVar['PLOT_TITLE'] = ''
		oT['TICKFORMAT']   = '(a1)'
		oT['TITLE']        = ''
	endfor
	
	;PA180
	for i = 0, 3 do begin
		oVar               = MrVar_Get(edi_psd_vnames[i+4])
		oT                 = MrVar_Get(oVar['DEPEND_0'])
		oVar['TITLE']      = string(i+1, FORMAT='(%"Flux%i PA180!C(PSD)")')
		oVar['PLOT_TITLE'] = ''
		oT['TITLE']        = i eq 3 ? 'Frequency (Hz)' : ''
		if i ne 3 then oT['TICKFORMAT'] = '(a1)'
	endfor

;-------------------------------------------
; Plot Data ////////////////////////////////
;-------------------------------------------
	xrange = [1, 4096]

	win = MrWindow(NAME='WhistlerPSD', XSIZE=890, YGAP=0.5, YSIZE=1100, REFRESH=0)

	;DB
	p1 = MrVar_Plot( dbpar_psd_vname, /CURRENT )
	p2 = MrVar_Plot( dbperp_psd_vname, OVERPLOT=p1 )
	
	;LEGEND
	lB = MrVar_Legend(dbpar_psd_vname, dbperp_psd_vname, $
	                  ALIGNMENT = 'SW', $
	                  POSITION  = [0,0], $
	                  /RELATIVE, $
	                  NAME      = 'LGD: dB', $
	                  TARGET    = p1)
	
	;SCPOT
	p3 = MrVar_Plot( scpot_psd_vname, /CURRENT )
	
	;EDI
	nvars = n_elements(edi_psd_vnames)
	for i = 0, nvars - 1 do begin
		p = MrVar_Plot( edi_psd_vnames[i], /CURRENT )
	endfor
	
	win -> SetGlobal, XRANGE=xrange
	win -> Refresh
	return, win
end