; docformat = 'rst'
;
; NAME:
;       MrMMS_Plot_EPD_eEnergy
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
;   Generate a plot of EPD-EIS ElectronEnergy quantities:
;       1. FGM Bxyz
;       2. Integraged flux for Telescopes 0-5
;       3. Energy-time spectrogram for T0
;       4. Energy-time spectrogram for T1
;       5. Energy-time spectrogram for T2
;       6. Energy-time spectrogram for T3
;       7. Energy-time spectrogram for T4
;       8. Energy-time spectrogram for T5
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
FUNCTION MrMMS_Plot_EPD_eEnergy, sc, mode, $
FGM_INSTR=fgm_instr, $
LEVEL=level, $
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

	dump_t0_vname = StrJoin( [sc, instr, optdesc, 'dump', 'flux', 't0'], '_' )
	dump_t1_vname = StrJoin( [sc, instr, optdesc, 'dump', 'flux', 't1'], '_' )
	dump_t2_vname = StrJoin( [sc, instr, optdesc, 'dump', 'flux', 't2'], '_' )
	dump_t3_vname = StrJoin( [sc, instr, optdesc, 'dump', 'flux', 't3'], '_' )
	dump_t4_vname = StrJoin( [sc, instr, optdesc, 'dump', 'flux', 't4'], '_' )
	dump_t5_vname = StrJoin( [sc, instr, optdesc, 'dump', 'flux', 't5'], '_' )
	flux_t0_vname = StrJoin( [sc, instr, optdesc, 'electron', 'P3', 'flux', 't0'], '_' )
	flux_t1_vname = StrJoin( [sc, instr, optdesc, 'electron', 'P3', 'flux', 't1'], '_' )
	flux_t2_vname = StrJoin( [sc, instr, optdesc, 'electron', 'P3', 'flux', 't2'], '_' )
	flux_t3_vname = StrJoin( [sc, instr, optdesc, 'electron', 'P3', 'flux', 't3'], '_' )
	flux_t4_vname = StrJoin( [sc, instr, optdesc, 'electron', 'P3', 'flux', 't4'], '_' )
	flux_t5_vname = StrJoin( [sc, instr, optdesc, 'electron', 'P3', 'flux', 't5'], '_' )

;-------------------------------------------
; Get Data /////////////////////////////////
;-------------------------------------------
	IF tf_load THEN BEGIN
		;FGM
		MrMMS_FGM_Load_Data, sc, fgm_mode, $
		                     INSTR     = fgm_instr, $
		                     LEVEL     = level, $
		                     VARFORMAT = b_vname

		;HPCA
		MrMMS_Load_Data, sc, 'epd-eis', mode, level, $
		                 OPTDESC   = optdesc, $
		                 VARFORMAT = [ dump_t0_vname, dump_t1_vname, dump_t2_vname, dump_t3_vname, dump_t4_vname, dump_t5_vname, $
		                               flux_t0_vname, flux_t1_vname, flux_t2_vname, flux_t3_vname, flux_t4_vname, flux_t5_vname ]
	ENDIF
	
;-------------------------------------------
; Properties ///////////////////////////////
;-------------------------------------------
	trange = MrVar_GetTRange('SSM')

	;BMAG
	oB = MrVar_Get(b_vname)
	oB['PLOT_TITLE'] = StrUpCase( StrJoin( [sc, instr, mode, level], ' ' ) )

	colors = MrDefaultColor(NCOLORS=6)
	oDump_t0 = MrVar_Get(dump_t0_vname)
	oDump_t0['COLOR'] = colors[0]

	oDump_t1 = MrVar_Get(dump_t1_vname)
	oDump_t1['COLOR'] = colors[1]

	oDump_t2 = MrVar_Get(dump_t2_vname)
	oDump_t2['COLOR'] = colors[2]

	oDump_t3 = MrVar_Get(dump_t3_vname)
	oDump_t3['COLOR'] = colors[3]

	oDump_t4 = MrVar_Get(dump_t4_vname)
	oDump_t4['COLOR'] = colors[4]

	oDump_t5 = MrVar_Get(dump_t5_vname)
	oDump_t5['COLOR'] = colors[5]

	;
	; The Epoch variable does not have the same number of elements as its
	; DELTA_PLUS_VAR and DELTA_MINUS_VAR attributes.
	;
	oFlux_t0 = MrVar_Get(flux_t0_vname)
	Epoch    = oFlux_t0['DEPEND_0']
	Epoch['AXIS_RANGE'] = trange
	IF Epoch -> HasAttr('DELTA_PLUS_VAR')  THEN Epoch -> RemoveAttr, 'DELTA_PLUS_VAR'
	IF Epoch -> HasAttr('DELTA_MINUS_VAR') THEN Epoch -> RemoveAttr, 'DELTA_MINUS_VAR'
	
	;FLUX T0
	oFlux_t0['AXIS_RANGE']    = [ Min(oFlux_t0[ oFlux_t0 -> Where(0, /GREATER) ]), oFlux_t0.Max ]
	oFlux_t0['MISSING_COLOR'] = 'Dark Grey'
	oFlux_t0['MISSING_VALUE'] = 0
	oFlux_t0['TITLE']         = 'Flux!CT0 e-!C' + oFlux_t0['UNITS']
	
	;FLUX T1
	oFlux_t1 = MrVar_Get(flux_t1_vname)
	oFlux_t1['AXIS_RANGE']    = [ Min(oFlux_t1[ oFlux_t1 -> Where(0, /GREATER) ]), oFlux_t1.Max ]
	oFlux_t1['MISSING_COLOR'] = 'Dark Grey'
	oFlux_t1['MISSING_VALUE'] = 0
	oFlux_t1['TITLE']         = 'Flux!CT1 e-!C' + oFlux_t1['UNITS']
	
	;FLUX T2
	oFlux_t2 = MrVar_Get(flux_t2_vname)
	oFlux_t2['AXIS_RANGE']    = [ Min(oFlux_t2[ oFlux_t2 -> Where(0, /GREATER) ]), oFlux_t2.Max ]
	oFlux_t2['MISSING_COLOR'] = 'Dark Grey'
	oFlux_t2['MISSING_VALUE'] = 0
	oFlux_t2['TITLE']         = 'Flux!CT2 e-!C' + oFlux_t2['UNITS']
	
	;FLUX T3
	oFlux_t3 = MrVar_Get(flux_t3_vname)
	oFlux_t3['AXIS_RANGE']    = [ Min(oFlux_t3[ oFlux_t3 -> Where(0, /GREATER) ]), oFlux_t3.Max ]
	oFlux_t3['MISSING_COLOR'] = 'Dark Grey'
	oFlux_t3['MISSING_VALUE'] = 0
	oFlux_t3['TITLE']         = 'Flux!CT3 e-!C' + oFlux_t3['UNITS']
	
	;FLUX T4
	oFlux_t4 = MrVar_Get(flux_t4_vname)
	oFlux_t4['AXIS_RANGE']    = [ Min(oFlux_t4[ oFlux_t4 -> Where(0, /GREATER) ]), oFlux_t4.Max ]
	oFlux_t4['MISSING_COLOR'] = 'Dark Grey'
	oFlux_t4['MISSING_VALUE'] = 0
	oFlux_t4['TITLE']         = 'Flux!CT4 e-!C' + oFlux_t4['UNITS']
	
	;FLUX T5
	oFlux_t5 = MrVar_Get(flux_t5_vname)
	oFlux_t5['AXIS_RANGE']    = [ Min(oFlux_t5[ oFlux_t5 -> Where(0, /GREATER) ]), oFlux_t5.Max ]
	oFlux_t5['MISSING_COLOR'] = 'Dark Grey'
	oFlux_t5['MISSING_VALUE'] = 0
	oFlux_t5['TITLE']         = 'Flux!CT5 e-!C' + oFlux_t5['UNITS']

;-------------------------------------------
; Plot Data ////////////////////////////////
;-------------------------------------------
	;Plot data
	win = MrVar_PlotTS( [ b_vname, dump_t0_vname, flux_t0_vname, flux_t1_vname, $
	                      flux_t2_vname, flux_t3_vname, flux_t4_vname, flux_t5_vname ], $
	                    /NO_REFRESH, $
	                    XSIZE = 680, $
	                    YSIZE = 700 )
	
	;Overplots
	win = MrVar_OPlotTS( dump_t0_vname, [dump_t1_vname, dump_t2_vname, dump_t3_vname, dump_t4_vname, dump_t5_vname] )

	;Pretty-up the window
	win[0] -> SetLayout, [1,1]
	win    -> TrimLayout
	win    -> SetProperty, OXMARGIN=[13, 15]
	win    -> Refresh

	RETURN, win
END