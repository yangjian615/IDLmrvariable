; docformat = 'rst'
;
; NAME:
;       MrMMS_Plot_EDP
;
;*****************************************************************************************
;   Copyright (c) 2017, Matthew Argall                                                   ;
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
;   Generate a plot of EDP quantities:
;       1. FGM Bxyz
;       2. SCPot
;       3. Ex
;       4. Ey
;       5. Ez
;       6. Ex PSD + Gyrofrequency lines
;       7. Ey PSD + Gyrofrequency lines
;       8. Ez PSD + Gyrofrequency lines
;
; :Categories:
;   MMS
;
; :Params:
;       SC:         in, required, type=string
;                   Spacecraft ID. Options are {'mms1' | 'mms2' | 'mms3' | 'mms4' }
;       MODE:       in, required, type=string, default='srvy'
;                   Data telemetry rate of the data. Options are: { 'slow' | 'fast' | 'srvy' | 'brst' }
;
; :Keywords:
;       FGM_INSTR:  in, optional, type=string, default='fgm'
;                   FGM instrument to use. Options are: { 'afg' | 'dfg' | 'fgm' }
;       LEVEL:      in, optional, type=string, default='l2'
;                   Data quality level. Options are: {'l1a' | 'l1b' | 'ql' | 'l2pre' | 'l2'}
;       OPTDESC:    in, optional, type=string, default=''
;                   Optional filename descriptor.
;       NO_LOAD:    in, optional, type=boolean, default=0
;                   If set, data will not be loaded from source CDF files.
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
;       2017/01/13  -   Written by Matthew Argall
;-
FUNCTION MrMMS_Plot_EDP, sc, mode, nfft, nshift, $
FGM_INSTR=fgm_instr, $
LEVEL=level, $
OPTDESC=optdesc, $
NO_LOAD=no_load, $
TRANGE=trange
	Compile_Opt idl2
	
	Catch, the_error
	IF the_error NE 0 THEN BEGIN
		Catch, /CANCEL
		IF N_Elements(win) GT 0 THEN Obj_Destroy, win
		MrPrintF, 'LogErr'
		RETURN, !Null
	ENDIF
	
	tf_load = ~Keyword_Set(no_load)
	IF N_Elements(mode)   EQ 0 THEN mode       = 'fast'
	IF N_Elements(nfft)   EQ 0 THEN nfft       = 2048
	IF N_Elements(nshift) EQ 0 THEN nshift     = nfft/2
	IF N_Elements(level)  EQ 0 THEN level      = 'l2'
	IF N_Elements(trange) GT 0 THEN MrVar_SetTRange, trange
	
;-------------------------------------------
; Variable Names ///////////////////////////
;-------------------------------------------
	instr   = 'edp'
	optdesc = ['dce', 'scpot']
	IF N_Elements(fgm_instr) EQ 0 THEN BEGIN
		CASE level OF
			'l2': fgm_instr = 'fgm'
			ELSE: fgm_instr = 'dfg'
		ENDCASE
	ENDIF
	IF N_Elements(coords) EQ 0 THEN BEGIN
		CASE level OF
			'ql': coords = 'dsl'
			ELSE: coords = 'gse'
		ENDCASE
	ENDIF
	fgm_coords = coords EQ 'dsl'  ? 'dmpa' : coords
	fgm_mode   = mode   EQ 'brst' ? mode   : 'srvy'

	;Source names
	fgm_b_vname    = StrJoin( [sc, fgm_instr, 'b',     fgm_coords, fgm_mode, level], '_' )
	fgm_bvec_vname = StrJoin( [sc, fgm_instr, 'bvec',  fgm_coords, fgm_mode, level], '_' )
	fgm_bmag_vname = StrJoin( [sc, fgm_instr, 'bmag',  fgm_coords, fgm_mode, level], '_' )
	e_vname        = StrJoin( [sc, instr,     'dce',   coords, mode, level], '_' )
	epar_vname     = StrJoin( [sc, instr,     'dce',   'par', 'epar', mode, level], '_' )
	scpot_vname    = StrJoin( [sc, instr,     'scpot', mode, level], '_' )
	
	;Output names
	ex_vname    = e_vname + '_x'
	ey_vname    = e_vname + '_y'
	ez_vname    = e_vname + '_z'
	expsd_vname = StrJoin( [sc, instr, 'expsd',  coords, mode, level], '_' )
	eypsd_vname = StrJoin( [sc, instr, 'eypsd',  coords, mode, level], '_' )
	ezpsd_vname = StrJoin( [sc, instr, 'ezpsd',  coords, mode, level], '_' )
	
	;Gyrofrequencies
	IF mode EQ 'brst' THEN BEGIN
		f1_vname = StrJoin( [sc, instr, 'fcE',     mode, level], '_' )
		f2_vname = StrJoin( [sc, instr, 'halffce', mode, level], '_' )
	ENDIF ELSE BEGIN
		f1_vname = StrJoin( [sc, instr, 'fcH',  mode, level], '_' )
		f2_vname = StrJoin( [sc, instr, 'fcHe', mode, level], '_' )
		f3_vname = StrJoin( [sc, instr, 'fcO',  mode, level], '_' )
	ENDELSE

;-------------------------------------------
; Get Data /////////////////////////////////
;-------------------------------------------
	IF tf_load THEN BEGIN
		;FGM
		MrMMS_FGM_Load_Data, sc, fgm_mode, $
		                     INSTR     = fgm_instr, $
		                     LEVEL     = level, $
		                     VARFORMAT = fgm_b_vname

		;EDP
		MrMMS_Load_Data, sc, instr, mode, level, $
		                 OPTDESC   = optdesc, $
		                 VARFORMAT = [e_vname, epar_vname, scpot_vname]
	ENDIF

;-------------------------------------------
; Power Spectral Density ///////////////////
;-------------------------------------------
	;Split B into components
	oE  = MrVar_Get(E_vname)
	oE -> Split, oEx, oEy, oEz, /CACHE
	
	;Compute PSD
	oEx_psd = oEx -> Spectrogram(nfft, nshift, NAME=expsd_vname, /CACHE, WINDOW='hanning')
	oEy_psd = oEy -> Spectrogram(nfft, nshift, NAME=eypsd_vname, /CACHE, WINDOW='hanning')
	oEz_psd = oEz -> Spectrogram(nfft, nshift, NAME=ezpsd_vname, /CACHE, WINDOW='hanning')

;-------------------------------------------
; Gyrofrequency Lines //////////////////////
;-------------------------------------------
	oBmag  = MrVar_Get(fgm_bmag_vname)
	
	;Electron cyclotron frequency
	IF mode EQ 'brst' THEN BEGIN
		;fce
		of1 = MrVar_Freq_Cyclotron(oBmag, 'm_e', /CACHE, NAME=f1_vname)
		
		;0.5  * fce
		of2 = of1 / 2.0
		of2 -> SetName, f2_vname
		of2 -> Cache
		
		;FC_E
		of1['COLOR'] = 'White'
		of1['NSUM']  = 4
		
		;0.5*FC_$
		of2['COLOR']     = 'White'
		of2['LINESTYLE'] = '--'
		of2['NSUM']      = 4

	;Ion cyclotron frequencies
	ENDIF ELSE BEGIN
		of1 = MrVar_Freq_Cyclotron(oBmag, 'm_H',  /CACHE, NAME=f1_vname)
		of2 = MrVar_Freq_Cyclotron(oBmag, 'm_He', /CACHE, NAME=f2_vname)
		of3 = MrVar_Freq_Cyclotron(oBmag, 'm_O',  /CACHE, NAME=f3_vname)
		
		;FC_H
		of1['COLOR'] = 'Blue'
		of1['NSUM']  = 4
	
		;FC_HE
		of2['COLOR'] = 'Magenta'
		of2['NSUM']  = 4
	
		;FC_HE
		of3['COLOR'] = 'Orange'
		of3['NSUM']  = 4
	ENDELSE
	
;-------------------------------------------
; Properties ///////////////////////////////
;-------------------------------------------
	;BMAG
	oB = MrVar_Get(fgm_b_vname)
	oB['PLOT_TITLE'] = StrUpCase( StrJoin( [sc, mode, level, optdesc[0]], ' ' ) )

	;Epar
	oEpar = MrVar_Get(epar_vname)
	oEpar['COLOR'] = ['Yellow', 'Black']
	oEpar['TITLE'] = 'E$\down||$!C(mV/m)'
	
	;Ex
	oEx['TITLE'] = 'Ex!C(mV/m)'
	
	;Ey
	oEy['TITLE'] = 'Ey!C(mV/m)'
	
	;Ez
	oEz['TITLE'] = 'Ez!C(mV/m)'
	
	;Ex PSD
	oEx_psd['AXIS_RANGE'] = [1e-7, 1e-0]
	oEx_psd['TITLE']      = 'Ex PSD'
	
	;Ey PSD
	oEy_psd['AXIS_RANGE'] = [1e-7, 1e-0]
	oEy_psd['TITLE']      = 'Ey PSD!C(mV/m)$\up2$/Hz'
	
	;Ez PSD
	oEz_psd['AXIS_RANGE'] = [1e-7, 1e-0]
	oEz_psd['TITLE']      = 'Ez PSD'

;-------------------------------------------
; Plot Data ////////////////////////////////
;-------------------------------------------
	;Plot data
	win = MrVar_PlotTS( [fgm_b_vname, scpot_vname, $
	                     epar_vname, ex_vname, ey_vname, ez_vname, $
	                     expsd_vname, eypsd_vname, ezpsd_vname], $
	                    /NO_REFRESH, $
	                    XSIZE = 680, $
	                    YSIZE = 700 )
	
	;Cyclotron frequencies
	IF mode EQ 'brst' THEN BEGIN
		win = MrVar_OPlotTS( expsd_vname, [f1_vname, f2_vname] )
		win = MrVar_OPlotTS( eypsd_vname, [f1_vname, f2_vname] )
		win = MrVar_OPlotTS( ezpsd_vname, [f1_vname, f2_vname] )
	ENDIF ELSE BEGIN
		win = MrVar_OPlotTS( expsd_vname, [f1_vname, f2_vname, f3_vname] )
		win = MrVar_OPlotTS( eypsd_vname, [f1_vname, f2_vname, f3_vname] )
		win = MrVar_OPlotTS( ezpsd_vname, [f1_vname, f2_vname, f3_vname] )
	ENDELSE

	;Pretty-up the window
	win[0] -> SetLayout, [1,1]
	win    -> TrimLayout
	win    -> SetProperty, OXMARGIN=[13, 14]
	win    -> Refresh

	RETURN, win
END