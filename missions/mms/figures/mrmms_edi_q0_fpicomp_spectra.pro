; docformat = 'rst'
;
; NAME:
;       MrMMS_FluxPileUp_Overview
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
;       2. FPI PAD
;       3. EDI PAD
;       4. FPI GPD
;       5. EDI GPD
;
; :Categories:
;   MMS, EDI, Q0
;
; :Params:
;       SC:         in, required, type=string
;                   MMS spacecraft ID. Options are {'mms1' | 'mms2' | 'mms3' | 'mms4'}
;       MODE:       in, required, type=string
;                   Data rate mode. Options are {'srvy' | 'brst'}
;
; :Keywords:
;       NO_LOAD:    in, optional, type=boolean, default=0
;                   If set, data will not be loaded from file, but taken from the
;                       variable cache.
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
function MrMMS_EDI_Q0_FPIComp_Spectra, sc, mode, $
NO_LOAD=no_load, $
TRANGE=trange
	compile_opt idl2
	
	catch, the_error
	if the_error ne 0 then begin
		catch, /CANCEL
		if n_elements(win1) gt 0 then obj_destroy, win1
		if n_elements(win2) gt 0 then obj_destroy, win2
		MrPrintF, 'LogErr'
		return, !Null
	endif
	
	tf_load = ~keyword_set(no_load)
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

	;Source names
	fgm_vname     = sc + '_fgm_bvec_'      + coords  + '_' + fgm_mode + '_' + level
	edi_gpd_vname = sc + '_edi_phispec_q0_'                + mode     + '_' + level
	edi_pad_vname = sc + '_edi_thetaspec_q0_'              + mode     + '_' + level
	fpi_gpd_vname = sc + '_des_phispec_'                   + mode     + '_' + level
	fpi_pad_vname = sc + '_des_thetaspec_'                 + mode     + '_' + level
	fpi_d4d_vname = sc + '_des_dist4d_'                    + mode     + '_' + level
	
	dphi_edi = mode eq 'srvy' ? 22.5 : 11.25
	dt_edi   = mode eq 'srvy' ? 2.25  : 1.0
	
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
	; EDI //////////////////////////////////////
	;-------------------------------------------
		fac_type = mode eq 'srvy' ? 'ExB' : 'VxB'
		oEDI     = MrMMS_EDI_Dist( sc, mode, 'q0' )
		oGPD_EDI = oEDI -> GetPhiSpec( DPHI=dphi_edi, DTIME=dt_edi, TYPE_FAC=fac_type, /CACHE )
		oPAD_EDI = oEDI -> GetThetaSpec( DTIME=dt_edi, /CACHE )
	
	;-------------------------------------------
	; FPI //////////////////////////////////////
	;-------------------------------------------
		fpi_mode = mode eq 'brst' ? 'brst' : 'fast'
		oFPI     = MrMMS_FPI_Dist( sc, fpi_mode, 'e' )
		oFPI    -> Load_FAC, fac_type
		oDist    = oFPI -> GetDist4D( NAME=fpi_d4d_vname, /CACHE )
		oDist   -> ConvertUnits, 'DIFF FLUX'
		oGPD_FPI = oDist -> GetPhiSpec( NAME=fpi_gpd_vname, THETA_RANGE=[-80,100], /CACHE)
		oPAD_FPI = oDist -> GetThetaSpec( NAME=fpi_pad_vname, /CACHE)
	endif

;-------------------------------------------
; Plot Data ////////////////////////////////
;-------------------------------------------
	win1 = MrVar_PlotTS( [fgm_vname, fpi_pad_vname, edi_pad_vname, fpi_gpd_vname, edi_gpd_vname], $
	                     XSIZE = 750, $
	                     YSIZE = 850 )
	win1 -> Refresh, /DISABLE

;-------------------------------------------
; Disply Distributions /////////////////////
;-------------------------------------------
	;Times to display 1D distribution
	t0 = MrCDF_Epoch_Compute(2015, 10, 16, 13, 07, 01, 05, 00, 00)
	
	;Get the GPDs and their time variables
	oEDI_GPD  = MrVar_Get( edi_gpd_vname )
	oFPI_GPD  = MrVar_Get( fpi_gpd_vname )
	oEDI_time = oEDI_GPD['TIMEVAR']
	oFPI_time = oFPI_GPD['TIMEVAR']
	
	;Extract the center times
	it_edi = oEDI_time -> Value_Locate(t0, 'TT2000')
	it_fpi = oFPI_time -> Value_Locate(t0, 'TT2000')
	t_edi  = oEDI_time[it_edi, 'SSM']
	t_fpi  = oFPI_time[it_fpi, 'SSM']
	
	;Create a window
	nDist = n_elements(t0)
	win2 = MrWindow(ASPECT=1, LAYOUT=[2,nDist], NAME='Q0 GPD Cuts', YGAP=0.5, YSIZE=700, REFRESH=0)
	
	;Plot each 1D distribution
	for i = 0, nDist - 1 do begin
		;X-AXIS: label bottom plot only
		if i eq nDist - 1 then begin
			xtitle      = 'Phi (deg)'
			xtickformat = ''
		endif else begin
			xtitle      = ''
			xtickformat = '(a1)'
		endelse
		
		;Extract a single distribution
		phi = oEDI_GPD['DEPEND_1'] -> GetData()
		cts = float(reform(oEDI_GPD[it_edi,*]))
		cts = replace_fillval(cts, 0)
		
		;Plot the distribution
		p = MrPlot( phi, cts, $
		            /CURRENT, $
		            LAYOUT        = [2,nDist,i+1], $
		            LINESTYLE     = '-', $
		            NAME          = 'Q0 GPD ' + strtrim(i, 2), $
		            PSYM          = 2, $
		            TITLE         = 'Q0 GPD', $
		            XRANGE        = [-180.0, 180.0], $
		            XTICKINTERVAL = 90.0, $
		            XTICKFORMAT   = xtickformat, $
		            XTITLE        = xtitle, $
		            YTITLE        = 'Counts' )
		
		;Extract a single distribution
		phi = oFPI_GPD['DEPEND_1'] -> GetData()
		cts = float(reform(oFPI_GPD[it_fpi,*]))
		cts = replace_fillval(cts, 0)
		
		;Plot the distribution
		p = MrPlot( phi, cts, $
		            /CURRENT, $
		            LAYOUT        = [2,nDist,i+1], $
		            LINESTYLE     = '-', $
		            NAME          = 'FPI GPD ' + strtrim(i, 2), $
		            PSYM          = 2, $
		            TITLE         = 'FPI GPD', $
		            XRANGE        = [-180.0, 180.0], $
		            XTICKINTERVAL = 90.0, $
		            XTICKFORMAT   = xtickformat, $
		            XTITLE        = xtitle, $
;		            YTICKFORMAT   = '(a1)', $
		            YTITLE        = oFPI_GPD['UNITS'] )
		
		;Put a vertical line on the distributions
		ox = MrPlotS( [t_edi, t_edi], [-180,180], $
		              NAME      = 'VLine EDI ' + strtrim(i,2), $
		              COLOR     = 'Red', $
		              LINESTYLE = '--', $
		              TARGET    = win1[edi_gpd_vname] )
		ox = MrPlotS( [t_fpi, t_fpi], [-180,180], $
		              NAME      = 'VLine FPI ' + strtrim(i,2), $
		              COLOR     = 'Red', $
		              LINESTYLE = '--', $
		              TARGET    = win1[fpi_gpd_vname] )
	endfor
		

;-------------------------------------------
; Vertical Lines ///////////////////////////
;-------------------------------------------
	win1 -> Refresh
	win2 -> Refresh
	return, win1
end