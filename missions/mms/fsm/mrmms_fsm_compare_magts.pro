; docformat = 'rst'
;
; NAME:
;       MrMMS_FSM_Compare_MagTS
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
;+
;   Calculate moments of the distribution function and plot them against the official
;   FPI L2 dataset. The moments calculation takes into account the FPI internal photo-
;   electron model, but the method of integration is different.
;
;       1. |B| FGM & FSM
;       2. Bx FGM & FSM
;       3. By FGM & FSM
;       4. Bz FGM & FSM
;
; :Params:
;       SC:         in, required, type=string
;                   MMS spacecraft ID. Options are {'mms1' | 'mms2' | 'mms3' | 'mms4'}
;       MODE:       in, required, type=string
;                   Data rate mode. Options are {'srvy' | 'brst'}
;
; :Keywords:
;       NO_LOAD:    in, optional, type=boolean, default=0
;                   If set, data will not be loaded from source files.
;
; :Categories:
;    MMS
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
;       2017/02/19  -   Written by Matthew Argall
;-
FUNCTION MrMMS_FSM_Compare_MagTS, sc, mode, $
COORDS=coords, $
FGM_INSTR=fgm_instr, $
LEVEL=level, $
NO_LOAD=no_load
	Compile_Opt idl2
	
	Catch, the_error
	IF the_error NE 0 THEN BEGIN
		Catch, /CANCEL
		!MrMMS -> SetProperty, DROPBOX_ROOT=dropbox_in, MIRROR_ROOT=mirror_in, OFFLINE=offline_in
		MrPrintF, 'LogErr'
		RETURN, Obj_New()
	ENDIF
	
	;Set the local dropbox directory
	!MrMMS -> GetProperty, DROPBOX_ROOT=dropbox_in, MIRROR_ROOT=mirror_in, OFFLINE=offline_in
	!MrMMS -> SetProperty, DROPBOX_ROOT='/nfs/fsm/temp/', MIRROR_ROOT='/nfs/', /OFFLINE
	
	tf_load = ~Keyword_Set(no_load)
	IF N_Elements(coords)    EQ 0 THEN coords    = 'gse'
	IF N_Elements(fgm_instr) EQ 0 THEN fgm_instr = 'dfg'
	IF N_Elements(level)     EQ 0 THEN level     = 'l2'

;-------------------------------------------
; Variable Names ///////////////////////////
;-------------------------------------------
	fsm_level = 'l2plus'
	
	IF N_Elements(fgm_level) EQ 0 THEN BEGIN
		CASE fgm_instr OF
			'dfg': fgm_level = 'l2pre'
			'afg': fgm_level = 'l2pre'
			'fgm': fgm_level = 'l2'
			ELSE: Message, 'Invalid FGM instrument: "' + fgm_instr + '".'
		ENDCASE
	ENDIF

	;Source DFG
	IF fgm_instr EQ 'fgm' THEN BEGIN
		b_fgm_vname    = StrJoin( [sc, fgm_instr, 'b',    coords, mode, fgm_level], '_' )
		bvec_fgm_vname = StrJoin( [sc, fgm_instr, 'bvec', coords, mode, fgm_level], '_' )
		bmag_fgm_vname = StrJoin( [sc, fgm_instr, 'bmag', coords, mode, fgm_level], '_' )
	ENDIF ELSE BEGIN
		b_fgm_vname    = StrJoin( [sc, fgm_instr,        mode, fgm_level, coords], '_' )
		bvec_fgm_vname = StrJoin( [sc, fgm_instr, 'vec', mode, fgm_level, coords], '_' )
		bmag_fgm_vname = StrJoin( [sc, fgm_instr, 'mag', mode, fgm_level, coords], '_' )
	ENDELSE
	
	;Source FSM
	bvec_fsm_vname = StrJoin( [sc, 'fsm', 'b', coords, mode, fsm_level], '_' )
	
	;Derived names
	bx_fgm_vname = bvec_fgm_vname + '_x'
	by_fgm_vname = bvec_fgm_vname + '_y'
	bz_fgm_vname = bvec_fgm_vname + '_z'
	bx_fsm_vname = bvec_fsm_vname + '_x'
	by_fsm_vname = bvec_fsm_vname + '_y'
	bz_fsm_vname = bvec_fsm_vname + '_z'
	bmag_fsm_vname = StrJoin( [sc, 'fsm', 'bmag', coords, mode, fsm_level], '_' )

;-------------------------------------------
; Load Data ////////////////////////////////
;-------------------------------------------
	IF tf_load THEN BEGIN
		;FGM
		MrMMS_FGM_Load_Data, sc, mode, $
		                     INSTR  = fgm_instr, $
		                     LEVEL  = fgm_level, $
		                     VARFORMAT = ['*b_gse*', '*'+fgm_instr+'*'+coords], $
		                     SUFFIX = suffix
		
		;Spacecraft potential
		MrMMS_Load_Data, sc, 'fsm', mode, fsm_level, $
		                 SUFFIX    = suffix, $
		                 VARFORMAT = '*b_'+coords+'*'
	ENDIF

;-------------------------------------------
; Split Into Components ////////////////////
;-------------------------------------------
	;FGM
	oB = MrVar_Get(bvec_fgm_vname)
	oB -> Split, oBx_fgm, oBy_fgm, oBz_fgm, /CACHE
	
	;FSM
	oB    = MrVar_Get(bvec_fsm_vname)
	oB   -> Split, oBx_fsm, oBy_fsm, oBz_fsm, /CACHE
	oBmag = oB -> Magnitude( NAME=bmag_fsm_vname, /CACHE )

;-------------------------------------------
; Attributes ///////////////////////////////
;-------------------------------------------
	;
	; FGM
	;
	
	;BMAG
	oBmag = MrVar_Get(bmag_fgm_vname)
	oBmag['COLOR']      = 'Red'
	oBmag['LABEL']      = 'FGM'
	oBmag['PLOT_TITLE'] = StrUpCase(StrJoin([sc, fgm_instr+'-FSM', mode, coords], ' '))
	oBmag['TITLE']      = '|B|!C(nT)'
	
	;BX
	oBx_fgm['COLOR'] = 'Red'
	oBx_fgm['LABEL'] = 'FGM'
	oBx_fgm['TITLE'] = 'Bx!C(nT)'
	
	;BY
	oBy_fgm['COLOR'] = 'Red'
	oBy_fgm['LABEL'] = 'FGM'
	oBy_fgm['TITLE'] = 'By!C(nT)'
	
	;BZ
	oBz_fgm['COLOR'] = 'Red'
	oBz_fgm['LABEL'] = 'FGM'
	oBz_fgm['TITLE'] = 'Bz!C(nT)'
	
	;
	; FSM
	;
	
	;BMAG
	oBmag = MrVar_Get(bmag_fsm_vname)
	oBmag['COLOR'] = 'Black'
	oBmag['LABEL'] = 'FSM'
	oBmag['TITLE'] = '|B|!C(nT)'
	
	;BX
	oBx_fsm['COLOR'] = 'Black'
	oBx_fsm['LABEL'] = 'FSM'
	oBx_fsm['TITLE'] = 'Bx!C(nT)'
	
	;BY
	oBy_fsm['COLOR'] = 'Black'
	oBy_fsm['LABEL'] = 'FSM'
	oBy_fsm['TITLE'] = 'By!C(nT)'
	
	;BZ
	oBz_fsm['COLOR'] = 'Black'
	oBz_fsm['LABEL'] = 'FSM'
	oBz_fsm['TITLE'] = 'Bz!C(nT)'
	
;-------------------------------------------
; Plot /////////////////////////////////////
;-------------------------------------------
	win = MrVar_PlotTS( [bmag_fgm_vname, bx_fgm_vname, by_fgm_vname, bz_fgm_vname], $
	                    /NO_REFRESH, $
	                    YSIZE = 750 )
	
	;Overplot FSM
	win = MrVar_OPlotTS( [bmag_fgm_vname, bx_fgm_vname, by_fgm_vname, bz_fgm_vname], $
	                     [bmag_fsm_vname, bx_fsm_vname, by_fsm_vname, bz_fsm_vname] )

	win[0] -> SetLayout, [1,1]
	win -> TrimLayout
	win.oxmargin = [10,5]

	win -> Refresh
	!MrMMS -> SetProperty, DROPBOX_ROOT=dropbox_in, MIRROR_ROOT=mirror_in, OFFLINE=offline_in
	RETURN, win
END