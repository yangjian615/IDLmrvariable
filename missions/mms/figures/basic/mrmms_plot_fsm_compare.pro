; docformat = 'rst'
;
; NAME:
;       MrMMS_Plot_FSM_Compare
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
;   Generate a plot of FGM quantities:
;       1. Bx FSM, FGM
;       2. By FSM, FGM
;       3. Bz FSM, FGM
;       4. Bx FSM, SCM High-pass filtered at 4Hz
;       5. By FSM, SCM High-pass filtered at 4Hz
;       6. Bz FSM, SCM High-pass filtered at 4Hz
;
; :Categories:
;   MMS
;
; :Params:
;       MODE:       in, required, type=string
;                   Data rate mode. Options are {'srvy' | 'brst'}
;       INSTR:      in, required, type=string, default='fgm'
;                   FGM strument to use. Options are: {'afg' | 'dfg' | 'fgm'}
;
; :Keywords:
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
FUNCTION MrMMS_Plot_FSM_Compare, sc, mode, $
COORDS=coords, $
FGM_INSTR=fgm_instr, $
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
	
	MrVar_SetTRange, '2016-12-29T' + ['03:58:00', '03:59:00']
	
	tf_load = ~Keyword_Set(no_load)
	IF N_Elements(coords)     EQ 0 THEN coords = 'gse'
	IF N_Elements(fgm_instr)  EQ 0 THEN fgm_instr  = 'dfg'
	IF N_Elements(trange)     GT 0 THEN MrVar_SetTRange, trange
	
;-------------------------------------------
; Variable Names ///////////////////////////
;-------------------------------------------
	CASE fgm_instr OF
		'afg': fgm_level = 'l2pre'
		'dfg': fgm_level = 'l2pre'
		'fgm': fgm_level = 'l2'
		ELSE: Message, 'Invalid FGM instrument: "' + instr + '".'
	ENDCASE
	
	scm_level = 'l2'
	CASE mode OF
		'slow': scm_optdesc = 'scs'
		'fast': scm_optdesc = 'scf'
		'srvy': scm_optdesc = 'scsrvy'
		'brst': scm_optdesc = 'scb'
		ELSE: Message, 'Invalid value for MODE.'
	ENDCASE

	;Source names
	bmag_fsm_vname = StrJoin( [sc, 'fsm',     'b',    'mag',               mode, 'l3'],      '_' )
	bvec_fsm_vname = StrJoin( [sc, 'fsm',     'b',    coords,              mode, 'l3'],      '_' )
	bmag_fgm_vname = StrJoin( [sc, fgm_instr, 'bmag', coords,              mode, fgm_level], '_' )
	bvec_fgm_vname = StrJoin( [sc, fgm_instr, 'bvec', coords,              mode, fgm_level], '_' )
	b_scm_vname    = StrJoin( [sc, 'scm',     'acb',  coords, scm_optdesc, mode, scm_level], '_' )
	
	;Derived Names
	bx_fsm_vname = StrJoin( [sc, 'fsm',     'b',    coords, mode, 'l3'],     '_' ) + '_x'
	by_fsm_vname = StrJoin( [sc, 'fsm',     'b',    coords, mode, 'l3'],     '_' ) + '_y'
	bz_fsm_vname = StrJoin( [sc, 'fsm',     'b',    coords, mode, 'l3'],     '_' ) + '_z'
	bx_fgm_vname = StrJoin( [sc, fgm_instr, 'bvec', coords, mode, fgm_level], '_' ) + '_x'
	by_fgm_vname = StrJoin( [sc, fgm_instr, 'bvec', coords, mode, fgm_level], '_' ) + '_y'
	bz_fgm_vname = StrJoin( [sc, fgm_instr, 'bvec', coords, mode, fgm_level], '_' ) + '_z'

;-------------------------------------------
; Get Data /////////////////////////////////
;-------------------------------------------
	IF tf_load THEN BEGIN
		;FSM
		MrMMS_Load_Data, sc, 'fsm', mode, 'l3', $
		                 VARFORMAT = '*_b_' + ['mag', coords] + '*'
		
		;FGM
		MrMMS_FGM_Load_Data, sc, mode, $
		                     INSTR     = fgm_instr, $
		                     LEVEL     = fgm_level, $
		                     VARFORMAT = '*_b_' + coords + '_' + mode + '*'
		
		;SCM
		MrMMS_Load_Data, sc, 'scm', mode, 'l2', $
		                 OPTDESC   = scm_optdesc, $
		                 VARFORMAT = '*scb_' + coords + '*'
	ENDIF

;-------------------------------------------
; Filter SCM and FSM at 4Hz ////////////////
;-------------------------------------------


;-------------------------------------------
; Split into Components ////////////////////
;-------------------------------------------
	;Retrieve the data
	oB_fsm = MrVar_Get(bvec_fsm_vname)
	oB_fgm = MrVar_Get(bvec_fgm_vname)
	oB_scm = MrVar_Get(b_scm_vname)
	
	;Split into X-, Y-, and Z-components
	oB_fsm -> Split, /CACHE
	oB_fgm -> Split, /CACHE
	oB_scm -> Split, /CACHE
	
;-------------------------------------------
; Properties ///////////////////////////////
;-------------------------------------------
	title = StrUpCase( StrJoin([sc, mode], ' ') )
	
	;|B|
	oBmag_fsm = MrVar_Get(bmag_fsm_vname)
	oBmag_fsm['LABEL']      = 'FSM'
	oBmag_fsm['PLOT_TITLE'] = title
	
	;BX
	oBx_fsm = MrVar_Get(bx_fsm_vname)
	oBx_fsm['LABEL'] = 'FSM'
	oBx_fsm['TITLE'] = 'Bx'
	
	;BY
	oBy_fsm = MrVar_Get(by_fsm_vname)
	oBy_fsm['LABEL'] = 'FSM'
	oBy_fsm['TITLE'] = 'By'
	
	;BZ
	oBz_fsm = MrVar_Get(bz_fsm_vname)
	oBz_fsm['LABEL'] = 'FSM'
	oBz_fsm['TITLE'] = 'Bz'
	
	;|B|
	oBmag_fgm = MrVar_Get(bmag_fgm_vname)
	oBmag_fgm['COLOR'] = 'Blue'
	oBmag_fgm['LABEL'] = 'FGM'
	
	;BX
	oBx_fgm = MrVar_Get(bx_fgm_vname)
	oBx_fgm['COLOR'] = 'Blue'
	oBx_fgm['LABEL'] = 'FGM'
	
	;BY
	oBy_fgm = MrVar_Get(by_fgm_vname)
	oBy_fgm['COLOR'] = 'Blue'
	oBy_fgm['LABEL'] = 'FGM'
	
	;BZ
	oBz_fgm = MrVar_Get(bz_fgm_vname)
	oBz_fgm['COLOR'] = 'Blue'
	oBz_fgm['LABEL'] = 'FGM'

;-------------------------------------------
; Plot Data ////////////////////////////////
;-------------------------------------------
	;Plot data
	win = MrVar_PlotTS( [bmag_fsm_vname, bx_fsm_vname, by_fsm_vname, bz_fsm_vname], $
	                    /NO_REFRESH, $
	                    XSIZE = 680, $
	                    YSIZE = 700 )
	win = MrVar_OPlotTS( bmag_fsm_vname, bmag_fgm_vname )
	win = MrVar_OPlotTS( bx_fsm_vname, bx_fgm_vname )
	win = MrVar_OPlotTS( by_fsm_vname, by_fgm_vname )
	win = MrVar_OPlotTS( bz_fsm_vname, bz_fgm_vname )

	;Pretty-up the window
	win[0] -> SetLayout, [1,1]
	win    -> TrimLayout
	win    -> SetProperty, OXMARGIN=[10, 6]
	win    -> Refresh

	RETURN, win
END