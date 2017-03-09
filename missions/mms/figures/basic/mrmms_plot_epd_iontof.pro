; docformat = 'rst'
;
; NAME:
;       MrMMS_Plot_EPD_IonTOF
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
;   Generate a plot of EPD-EIS ExTOF quantities:
;       1. FGM Bxyz
;       2. Energy-time spectrogram for T0
;       3. Energy-time spectrogram for T1
;       4. Energy-time spectrogram for T2
;       5. Energy-time spectrogram for T3
;       6. Energy-time spectrogram for T4
;       7. Energy-time spectrogram for T5
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
;       OPTDESC:    in, optional, type=string, default='extof'
;                   Optional filename descriptor. Choices are: {'phxtof', 'extof'}
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
FUNCTION MrMMS_Plot_EPD_IonTOF, sc, mode, $
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
	IF N_Elements(level)   EQ 0 THEN level   = 'l2'
	IF N_Elements(mode)    EQ 0 THEN mode    = 'fast'
	IF N_Elements(trange)  GT 0 THEN MrVar_SetTRange, trange
	IF N_Elements(optdesc) EQ 0 THEN optdesc = 'extof'
	
;-------------------------------------------
; Variable Names ///////////////////////////
;-------------------------------------------
	instr = 'epd_eis'
	IF N_Elements(fgm_instr) EQ 0 THEN BEGIN
		CASE level OF
			'l2': fgm_instr = 'fgm'
			ELSE: fgm_instr = 'dfg'
		ENDCASE
	ENDIF
	IF N_Elements(coords) EQ 0 THEN BEGIN
		CASE level OF
			'ql': coords = 'dbcs'
			ELSE: coords = 'gse'
		ENDCASE
	ENDIF
	fgm_coords = coords EQ 'dbcs' ? 'dmpa' : coords
	fgm_mode   = mode   EQ 'brst' ? mode   : 'srvy'

	;Source names
	b_vname     = StrJoin( [sc, fgm_instr, 'b',     fgm_coords, fgm_mode, level], '_' )
	bvec_vname  = StrJoin( [sc, fgm_instr, 'bvec',  fgm_coords, fgm_mode, level], '_' )
	bmag_vname  = StrJoin( [sc, fgm_instr, 'bmag',  fgm_coords, fgm_mode, level], '_' )

	h_flux_t0_vname = StrJoin( [sc, instr, optdesc, 'proton', 'P3', 'flux', 't0'], '_' )
	h_flux_t1_vname = StrJoin( [sc, instr, optdesc, 'proton', 'P3', 'flux', 't1'], '_' )
	h_flux_t2_vname = StrJoin( [sc, instr, optdesc, 'proton', 'P3', 'flux', 't2'], '_' )
	h_flux_t3_vname = StrJoin( [sc, instr, optdesc, 'proton', 'P3', 'flux', 't3'], '_' )
	h_flux_t4_vname = StrJoin( [sc, instr, optdesc, 'proton', 'P3', 'flux', 't4'], '_' )
	h_flux_t5_vname = StrJoin( [sc, instr, optdesc, 'proton', 'P3', 'flux', 't5'], '_' )

	he_flux_t0_vname = StrJoin( [sc, instr, optdesc, 'alpha', 'P3', 'flux', 't0'], '_' )
	he_flux_t1_vname = StrJoin( [sc, instr, optdesc, 'alpha', 'P3', 'flux', 't1'], '_' )
	he_flux_t2_vname = StrJoin( [sc, instr, optdesc, 'alpha', 'P3', 'flux', 't2'], '_' )
	he_flux_t3_vname = StrJoin( [sc, instr, optdesc, 'alpha', 'P3', 'flux', 't3'], '_' )
	he_flux_t4_vname = StrJoin( [sc, instr, optdesc, 'alpha', 'P3', 'flux', 't4'], '_' )
	he_flux_t5_vname = StrJoin( [sc, instr, optdesc, 'alpha', 'P3', 'flux', 't5'], '_' )

	o_flux_t0_vname = StrJoin( [sc, instr, optdesc, 'oxygen', 'P3', 'flux', 't0'], '_' )
	o_flux_t1_vname = StrJoin( [sc, instr, optdesc, 'oxygen', 'P3', 'flux', 't1'], '_' )
	o_flux_t2_vname = StrJoin( [sc, instr, optdesc, 'oxygen', 'P3', 'flux', 't2'], '_' )
	o_flux_t3_vname = StrJoin( [sc, instr, optdesc, 'oxygen', 'P3', 'flux', 't3'], '_' )
	o_flux_t4_vname = StrJoin( [sc, instr, optdesc, 'oxygen', 'P3', 'flux', 't4'], '_' )
	o_flux_t5_vname = StrJoin( [sc, instr, optdesc, 'oxygen', 'P3', 'flux', 't5'], '_' )

;-------------------------------------------
; Get Data /////////////////////////////////
;-------------------------------------------
	IF tf_load THEN BEGIN
		;FGM
		MrMMS_FGM_Load_Data, sc, fgm_mode, $
		                     INSTR     = fgm_instr, $
		                     LEVEL     = level, $
		                     VARFORMAT = b_vname

		;EPD-EIS
		MrMMS_Load_Data, sc, 'epd-eis', mode, level, $
		                 OPTDESC   = optdesc, $
		                 VARFORMAT = [ h_flux_t0_vname, h_flux_t1_vname, h_flux_t2_vname, h_flux_t3_vname, h_flux_t4_vname, h_flux_t5_vname, $
		                               he_flux_t0_vname, he_flux_t1_vname, he_flux_t2_vname, he_flux_t3_vname, he_flux_t4_vname, he_flux_t5_vname, $
		                               o_flux_t0_vname, o_flux_t1_vname, o_flux_t2_vname, o_flux_t3_vname, o_flux_t4_vname, o_flux_t5_vname ]
	ENDIF
	
;-------------------------------------------
; Replace Fill Values //////////////////////
;-------------------------------------------
	
;-------------------------------------------
; Properties ///////////////////////////////
;-------------------------------------------
	trange = MrVar_GetTRange('SSM')

	;BMAG
	oB = MrVar_Get(b_vname)
	oB['PLOT_TITLE'] = StrUpCase( StrJoin( [sc, instr, mode, level], ' ' ) )


	;
	; The Epoch variable does not have the same number of elements as its
	; DELTA_PLUS_VAR and DELTA_MINUS_VAR attributes.
	;
	
	;H+ Flux
	oFlux_H_t3 = MrVar_Get(h_flux_t3_vname)
	Epoch      = oFlux_H_t3['DEPEND_0']
	Epoch['AXIS_RANGE'] = trange
	IF Epoch -> HasAttr('DELTA_PLUS_VAR')  THEN Epoch -> RemoveAttr, 'DELTA_PLUS_VAR'
	IF Epoch -> HasAttr('DELTA_MINUS_VAR') THEN Epoch -> RemoveAttr, 'DELTA_MINUS_VAR'
	oFlux_H_t3['AXIS_RANGE']    = [ Min(oFlux_H_t3[ oFlux_H_t3 -> Where(0, /GREATER) ]), oFlux_H_t3.Max ]
	oFlux_H_t3['MISSING_COLOR'] = 'Dark Grey'
	oFlux_H_t3['MISSING_VALUE'] = 0
	oFlux_H_t3['TITLE']         = 'Flux!CT3 H+!C' + oFlux_H_t3['UNITS']

	;He+ Flux
	IF optdesc EQ 'extof' THEN BEGIN
		oFlux_He_t3 = MrVar_Get(he_flux_t3_vname)
		oFlux_He_t3['AXIS_RANGE']    = [ Min(oFlux_He_t3[ oFlux_He_t3 -> Where(0, /GREATER) ]), oFlux_He_t3.Max ]
		oFlux_He_t3['MISSING_COLOR'] = 'Dark Grey'
		oFlux_He_t3['MISSING_VALUE'] = 0
		oFlux_He_t3['TITLE']         = 'Flux!CT3 He+!C' + oFlux_He_t3['UNITS']
	ENDIF
	
	;O+ Flux
	oFlux_O_t3 = MrVar_Get(o_flux_t3_vname)
	oFlux_O_t3['AXIS_RANGE']    = [ Min(oFlux_O_t3[ oFlux_O_t3 -> Where(0, /GREATER) ]), oFlux_O_t3.Max ]
	oFlux_O_t3['MISSING_COLOR'] = 'Dark Grey'
	oFlux_O_t3['MISSING_VALUE'] = 0
	oFlux_O_t3['TITLE']         = 'Flux!CT3 O+!C' + oFlux_O_t3['UNITS']

;-------------------------------------------
; Plot Data ////////////////////////////////
;-------------------------------------------
	;Plot data
	win = MrVar_PlotTS( [ b_vname, h_flux_t3_vname, he_flux_t3_vname, o_flux_t3_vname ], $
	                    /NO_REFRESH, $
	                    XSIZE = 680, $
	                    YSIZE = 700 )

	;Pretty-up the window
	win[0] -> SetLayout, [1,1]
	win    -> TrimLayout
	win    -> SetProperty, OXMARGIN=[13, 15]
	win    -> Refresh

	RETURN, win
END