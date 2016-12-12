; docformat = 'rst'
;
; NAME:
;       MrMMS_Plot_FluxPileUp
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
function MrMMS_Plot_FluxPileUp, sc, mode, $
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
	if n_elements(nfft)   eq 0 then nfft = 1024
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
	fgm_vname     = sc + '_fgm_bvec_'          + coords + '_' + fgm_mode + '_' + level
	scm_vname     = sc + '_scm_acb_gse_scb_'                  + mode     + '_' + level
	BPSD_vname    = sc + '_dsp_bpsd_omni_'                    + dsp_mode + '_' + level
	pad_lo_vname  = sc + '_des_pitchangdist_lowen_'           + fpi_mode
	pad_mid_vname = sc + '_des_pitchangdist_miden_'           + fpi_mode
	pad_hi_vname  = sc + '_des_pitchangdist_highen_'          + fpi_mode
	edi_0_vname   = sc + '_edi_flux2_0_'                      + mode     + '_' + level
	edi_180_vname = sc + '_edi_flux2_180_'                    + mode     + '_' + level
	
	;Derived names
	psdx_vname     = sc + '_scm_acbx_gse_psd_' + mode + '_' + level
	psdy_vname     = sc + '_scm_acby_gse_psd_' + mode + '_' + level
	psdz_vname     = sc + '_scm_acbz_gse_psd_' + mode + '_' + level
	psd_0_vname   = sc + '_edi_flux2_0_psd_'   + mode + '_' + level
	psd_180_vname = sc + '_edi_flux2_180_psd_' + mode + '_' + level
	
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
	; B PSD ////////////////////////////////////
	;-------------------------------------------
		;DSP for SRVY
		if mode eq 'srvy' then begin
			MrMMS_Load_Data, sc, 'dsp', dsp_mode, level, $
			                 OPTDESC   = 'bpsd', $
			                 VARFORMAT = '*bpsd_omni*'
		
		;SCM for BRST
		endif else begin
			MrMMS_Load_Data, sc, 'scm', mode, level, $
			                 OPTDESC   = 'scb', $
			                 VARFORMAT = '*_scb_brst_l2'
		endelse
	
	;-------------------------------------------
	; FPI //////////////////////////////////////
	;-------------------------------------------
		MrMMS_FPI_Load_Data, sc, fpi_mode, $
		                     OPTDESC   = 'des-moms', $
		                     VARFORMAT = '*pitchangdist*'
	
	;-------------------------------------------
	; EDI //////////////////////////////////////
	;-------------------------------------------
		MrMMS_Load_Data, sc, 'edi', mode, level, $
		                 OPTDESC   = ['amb', 'amb-pm2', 'amb-alt-cc', 'amb-alt-oc', $
		                              'amb_alt-ooc', 'amb-alt-oob'], $
		                 VARFORMAT = ['*flux*', '*counts*']
	endif

;-------------------------------------------
; Compute Power Spectra ////////////////////
;-------------------------------------------
	if mode eq 'brst' then begin
		;SCM
		oSCM = MrVar_Get(scm_vname)
		odB  = oSCM -> Detrend(nfft)
		oPSD = odB -> Spectrogram(nfft, nfft/2, WINDOW='hanning')
		oPSDx = MrTimeSeries( oPSD['TIMEVAR'], oPSD[*,*,0], NAME=psdx_vname, /CACHE )
		oPSDy = MrTimeSeries( oPSD['TIMEVAR'], oPSD[*,*,1], NAME=psdy_vname, /CACHE )
		oPSDz = MrTimeSeries( oPSD['TIMEVAR'], oPSD[*,*,2], NAME=psdz_vname, /CACHE )
		oPSD -> CopyAttrTo, oPSDx
		oPSD -> CopyAttrTo, oPSDy
		oPSD -> CopyAttrTo, oPSDz

		;EDI PA0
		oEDI_0 = MrVar_Get(edi_0_vname)
		d0     = oEDI_0 -> Detrend(nfft/8)
		oPSD_0 = d0     -> Spectrogram(nfft/8, nfft/16, WINDOW='hanning')
		oPSD_0 -> SetName, psd_0_vname
		oPSD_0 -> Cache
		
		;EDI PA180
		oEDI_180 = MrVar_Get(edi_180_vname)
		d180     = oEDI_180 -> Detrend(nfft/8)
		oPSD_180 = d180     -> Spectrogram(nfft/8, nfft/16, WINDOW='hanning')
		oPSD_180 -> SetName, psd_180_vname
		oPSD_180 -> Cache
		
		;Destroy objects
		obj_destroy, [odB, oPSD, d0, d180]
	endif

;-------------------------------------------
; Variable Attributes //////////////////////
;-------------------------------------------
	title = strupcase(sc)

	oB               = MrVar_Get(fgm_vname)
	oB['PLOT_TITLE'] = title
	
	oPAD = MrVar_Get(pad_lo_vname)
	oPAD['TITLE'] = 'EFlux Lo'
	
	oPAD = MrVar_Get(pad_hi_vname)
	oPAD['TITLE'] = 'EFlux Hi'
	
	;BURST
	if mode eq 'brst' then begin
		;Bx Power-spectra
		oBPSD = MrVar_Get(psdx_vname)
		oBPSD['LOG']        = 1
		oBPSD['SCALE']      = 1
		oBPSD['TITLE']      = 'Bx PSD!C(nT$\up2$/Hz)'
		
		;By Power-spectra
		oBPSD = MrVar_Get(psdy_vname)
		oBPSD['LOG']        = 1
		oBPSD['SCALE']      = 1
		oBPSD['TITLE']      = 'By PSD!C(nT$\up2$/Hz)'
		
		;Bz Power-spectra
		oBPSD = MrVar_Get(psdz_vname)
		oBPSD['LOG']        = 1
		oBPSD['SCALE']      = 1
		oBPSD['TITLE']      = 'Bz PSD!C(nT$\up2$/Hz)'
		spectra_vname = psdz_vname
		
		;Frequencies
		oFreq = MrVar_Get(oBPSD['DEPEND_1'])
		oFreq['AXIS_RANGE'] = [1, 8192]
		oFreq['LOG']        = 1
		
		;EDI PA0 Power spectra
		oEDI_0 = MrVar_Get(psd_0_vname)
		oEDI_0['LOG']        = 1
		oEDI_0['SCALE']      = 1
		oEDI_0['TITLE']      = 'Flux2 PSD'
		edi_0_vname = psd_0_vname
		
		oFreq = MrVar_Get(oEDI_0['DEPEND_1'])
		oFreq['AXIS_RANGE'] = [1,512]
		oFreq['LOG']        = 1
		
		;EDI PA180 Power spectra
		oEDI_180 = MrVar_Get(psd_180_vname)
		oEDI_180['LOG']        = 1
		oEDI_180['SCALE']      = 1
		oEDI_180['TITLE']      = 'Flux2 PSD!C(cm$\up2$ s)$\up-2$/Hz)'
		edi_180_vname = psd_180_vname
		
		oFreq = MrVar_Get(oEDI_180['DEPEND_1'])
		oFreq['AXIS_RANGE'] = [1,512]
		oFreq['LOG'] = 1
		
	
	;SRVY
	endif else begin
		;Power spectra
		oBPSD = MrVar_Get(BPSD_vname)
		oBPSD['LOG'] = 1B
		oBPSD['RANGE'] = [1e-9, 1e-2]
		spectra_vname = bpsd_vname
		
		;Frequencies
		oFreq = MrVar_Get(oBPSD['DEPEND_1'])
		oFreq['AXIS_RANGE'] = [40, 8192]
		oFreq['TITLE'] = 'Freq!C(Hz)'
	endelse
	

;-------------------------------------------
; Plot Data ////////////////////////////////
;-------------------------------------------
	win = MrVar_PlotTS( [fgm_vname, pad_lo_vname, pad_mid_vname, pad_hi_vname, $
	                     spectra_vname, edi_0_vname, edi_180_vname], YSIZE = 700)
	
	win    -> Refresh, /DISABLE
	win[0] -> SetLayout, [1,1]
	win    -> TrimLayout
	win    -> Refresh

	return, win
end