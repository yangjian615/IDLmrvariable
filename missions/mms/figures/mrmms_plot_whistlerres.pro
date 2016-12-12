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
function MrMMS_Plot_WhistlerRes, sc, mode, species, $
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
	if n_elements(nfft)    eq 0 then nfft    = 512
	if n_elements(species) eq 0 then species = 'e'
	if n_elements(trange)  gt 0 then MrVar_SetTRange, trange
	
;-------------------------------------------
; Variable Names ///////////////////////////
;-------------------------------------------
	level     = 'l2'
	fgm_mode  = mode
	dsp_mode  = 'fast'
	coords    = 'gse'
	fpi_mode  = mode eq 'brst' ? mode : 'fast'
	fpi_instr = 'd' + species + 's'

	case mode of
		'slow': scm_desc = 'scs'
		'fast': scm_desc = 'scf'
		'srvy': scm_desc = 'scsrvy'
		'brst': scm_desc = 'scb'
		else: message, 'Invalid mode: "' + mode + '".'
	endcase

	;Source names
	fgm_vname     = sc + '_fgm_b_'     + coords + '_' + fgm_mode + '_' + level
	bvec_vname    = sc + '_fgm_bvec_'  + coords + '_' + fgm_mode + '_' + level
	bmag_vname    = sc + '_fgm_bmag_'  + coords + '_' + fgm_mode + '_' + level
	scm_vname     = sc + '_scm_acb_'   + coords + '_' + scm_desc + '_' + mode + '_' + level
	n_vname       = sc + '_' + fpi_instr + '_numberdensity_'      + mode
	pad_mid_vname = sc + '_' + fpi_instr + '_pitchangdist_miden_' + mode
	e0_vname      = sc + '_' + fpi_instr + '_energyspectr_par_'   + mode
	e90_vname     = sc + '_' + fpi_instr + '_energyspectr_perp_'  + mode
	e180_vname    = sc + '_' + fpi_instr + '_energyspectr_anti_'  + mode
	
	;Derived names
	fc_vname   = sc + '_scm_fce_' + mode + '_' + level
	eres_vname = sc + '_resonant_energy_' + mode + '_' + level
	psd_vname  = sc + '_scm_psd_'   + coords + '_' + scm_desc + '_' + mode + '_' + level
	
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
		                     OPTDESC   = fpi_instr + '-moms', $
		                     VARFORMAT = ['*density*', '*energyspectr*', '*pitchangdist*']
	endif

;-------------------------------------------
; Compute Power Spectra ////////////////////
;-------------------------------------------
	;SCM
	if mode eq 'brst' then begin
		oSCM = MrVar_Get(scm_vname)
		oPSD = oSCM -> Spectrogram(nfft, nfft/2, WINDOW='hanning', NAME=psd_vname, /CACHE)
	endif

;-----------------------------------------------------
; Calculate Resonant Energy \\\\\\\\\\\\\\\\\\\\\\\\\\
;-----------------------------------------------------
	;Get data
	q     = MrConstants('q')
	mass  = species eq 'i' ? MrConstants('m_p') : MrConstants('m_e')
	mu0   = MrConstants('mu_0')
	oBmag = MrVar_Get(bmag_vname)
	oN    = MrVar_Get(n_vname)

	;Interpolate
	oBmag = oBmag -> Interpol(oN)

	;Energy density per particle (nano-Weber)
	;   - 1e-6 comes from converting cm^3 to m^3
	oEn = 1e-24/(2*mu0) * oBmag^2 / oN

	;Cyclotron frequency
	;   - 1e-9 comes from converting nT to T
	fc = (1e-9*q/(mass*2*!pi)) * oBmag ;Hz
	fc -> SetName, fc_vname
	fc -> Cache
	
	;Normalized frequency
	fw = 100 ;Hz
	f  = fw / fc

	;Resonant energy
	Er = (1-f)^3 * oEn / f
	Er /= 1.602e-19
	Er -> SetName, eres_vname
	Er -> Cache
	
	;Cleanup variables
	obj_destroy, [oEn, f]

;-----------------------------------------------------
; Set Attributes \\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\
;-----------------------------------------------------
	title = strupcase(sc)

	;B
	oB = MrVar_Get(fgm_vname)
	oB['COLOR']      = ['Blue', 'Forest Green', 'Red', 'Black']
	oB['PLOT_TITLE'] = title
	oB['TITLE']      = 'B!C(nT)'
	
	;PSD
	oPSD = MrVar_Get(psd_vname)
	oPSD['TITLE'] = 'nT$\up2$/Hz'
	
	;Fce
	fc['COLOR'] = 'White'
	fc['THICK'] = 2
	
	;ERes
	Er['COLOR'] = 'White'
	Er['THICK'] = 2
	
	;E0
	oE0 = MrVar_Get(e0_vname)
	oE  = MrVar_Get(oE0['DEPEND_1'])
	oE0['TITLE'] = 'DEF Par'
	oE['TITLE']  = 'Energy (eV)!CPA 0-30'
	
	;E90
	oE90 = MrVar_Get(e90_vname)
	oE  = MrVar_Get(oE90['DEPEND_1'])
	oE90['TITLE'] = 'DEF Perp!C' + oE90['UNITS']
	oE['TITLE']   = 'Energy (eV)!CPA 30-120'
	
	;E180
	oE180 = MrVar_Get(e180_vname)
	oE    = MrVar_Get(oE180['DEPEND_1'])
	oE180['TITLE'] = 'DEF Anti'
	oE['TITLE']    = 'Energy (eV)!CPA 120-180'


;-------------------------------------------
; Plot Data ////////////////////////////////
;-------------------------------------------

	;Plot data
	win = MrVar_PlotTS( [fgm_vname, psd_vname, pad_mid_vname, e0_vname, e90_vname, e180_vname] )
	win -> Refresh, /DISABLE
	
	;Overplot resonant energy
	win = MrVar_OPlotTS( [e0_vname,   e90_vname,  e180_vname], $
	                     [eres_vname, eres_vname, eres_vname] )
	win = MrVar_OPlotTS( psd_vname, fc_vname )
	
	;Set Window Properties
	win[0] -> SetLayout, [1,1]
	win    -> TrimLayout
	win    -> SetProperty, OXMARGIN=[13,15], XSIZE=700, YSIZE=800
	win    -> SetGlobal, XTICKS=5

	win -> Refresh
	return, win
end