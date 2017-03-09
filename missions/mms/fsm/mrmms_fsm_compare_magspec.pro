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
;       2017/02/23  -   Added cyclotron, plasma, and lowerhybrid lines. - MRA
;-
FUNCTION MrMMS_FSM_Compare_MagSpec, sc, mode, $
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
	MrMMS_Init
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
	
	IF N_Elements(scm_optdesc) EQ 0 THEN BEGIN
		CASE mode OF
			'slow': scm_optdesc = 'scs'
			'fast': scm_optdesc = 'scb'
			'srvy': scm_optdesc = 'scsrvy'
			'brst': scm_optdesc = 'scb'
			ELSE: Message, 'Invalid telemetry mode: "' + mode + '".'
		ENDCASE
	ENDIF
	
	IF N_Elements(fpi_instr) EQ 0 THEN fpi_instr = 'des'

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
	bvec_scm_vname = StrJoin( [sc, 'scm', 'acb', 'gse', scm_optdesc, mode, 'l2'], '_' )
	bvec_fsm_vname = StrJoin( [sc, 'fsm', 'b',   coords, mode, fsm_level], '_' )
	n_vname        = StrJoin( [sc, fpi_instr, 'numberdensity', mode], '_' )
	
	;Derived names
	bx_psd_fgm_vname = bvec_fgm_vname + '_x_psd'
	by_psd_fgm_vname = bvec_fgm_vname + '_y_psd'
	bz_psd_fgm_vname = bvec_fgm_vname + '_z_psd'
	bx_psd_fsm_vname = bvec_fsm_vname + '_x_psd'
	by_psd_fsm_vname = bvec_fsm_vname + '_y_psd'
	bz_psd_fsm_vname = bvec_fsm_vname + '_z_psd'
	bx_psd_scm_vname = bvec_scm_vname + '_x_psd'
	by_psd_scm_vname = bvec_scm_vname + '_y_psd'
	bz_psd_scm_vname = bvec_scm_vname + '_z_psd'
	bmag_psd_fgm_vname = bmag_fgm_vname + '_psd'
	bmag_psd_fsm_vname = StrJoin( [sc, 'fsm', 'bmag', coords, mode, fsm_level, 'psd'], '_' )

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

		;SCM
		MrMMS_Load_Data, sc, 'scm', mode, 'l2', $
		                 OPTDESC   = scm_optdesc, $
		                 VARFORMAT = '*acb_gse*', $
		                 SUFFIX    = suffix
		
		;FSM
		MrMMS_Load_Data, sc, 'fsm', mode, fsm_level, $
		                 SUFFIX    = suffix, $
		                 VARFORMAT = '*b_'+coords+'*'
		
		;FPI
		MrMMS_FPI_Load_Data, sc, mode, $
		                     LEVEL     = level, $
		                     OPTDESC   = fpi_instr + '-moms', $
		                     SUFFIX    = suffix, $
		                     VARFORMAT = '*numberdensity*'
	ENDIF

;-------------------------------------------
; Split Into Components ////////////////////
;-------------------------------------------
	;FGM
	oB        = MrVar_Get(bvec_fgm_vname)
	oB       -> Split, oBx_fgm, oBy_fgm, oBz_fgm
	oBmag_fgm = MrVar_Get(bmag_fgm_vname)
	
	;FSM
	oB        = MrVar_Get(bvec_fsm_vname)
	oB       -> Split, oBx_fsm, oBy_fsm, oBz_fsm
	oBmag_fsm = oB -> Magnitude( NAME=bmag_fsm_vname )
	
	;SCM
	oB        = MrVar_Get(bvec_scm_vname)
	oB       -> Split, oBx_scm, oBy_scm, oBz_scm

;-------------------------------------------
; Compute Spectra //////////////////////////
;-------------------------------------------

	;
	;Power spectrum
	;
	
	;FGM
	oBmag_psd_fgm = oBmag_fgm -> PSD( WINDOW='Blackman', NAME=bmag_psd_fgm_vname, /CACHE)
	oBx_psd_fgm   = oBx_fgm   -> PSD( WINDOW='Blackman', NAME=bx_psd_fgm_vname,   /CACHE)
	oBy_psd_fgm   = oBy_fgm   -> PSD( WINDOW='Blackman', NAME=by_psd_fgm_vname,   /CACHE)
	oBz_psd_fgm   = oBz_fgm   -> PSD( WINDOW='Blackman', NAME=bz_psd_fgm_vname,   /CACHE)
	
	;FSM
	oBmag_psd_fsm = oBmag_fsm -> PSD( WINDOW='Blackman', NAME=bmag_psd_fsm_vname, /CACHE)
	oBx_psd_fsm   = oBx_fsm   -> PSD( WINDOW='Blackman', NAME=bx_psd_fsm_vname,   /CACHE)
	oBy_psd_fsm   = oBy_fsm   -> PSD( WINDOW='Blackman', NAME=by_psd_fsm_vname,   /CACHE)
	oBz_psd_fsm   = oBz_fsm   -> PSD( WINDOW='Blackman', NAME=bz_psd_fsm_vname,   /CACHE)
	
	;SCM
	oBx_psd_scm   = oBx_scm -> PSD( WINDOW='Blackman', NAME=bx_psd_scm_vname, /CACHE)
	oBy_psd_scm   = oBy_scm -> PSD( WINDOW='Blackman', NAME=by_psd_scm_vname, /CACHE)
	oBz_psd_scm   = oBz_scm -> PSD( WINDOW='Blackman', NAME=bz_psd_scm_vname, /CACHE)

;-------------------------------------------
; Plasma Frequencies ///////////////////////
;-------------------------------------------
	;Get data
	oBmag = MrVar_Get(bmag_fgm_vname)
	oN    = MrVar_Get(n_vname)
	
	;Compute averages
	bmag = mean(oBmag['DATA'])
	n    = mean(oN['DATA'])
	
	;Compute gyrofrequencies
	q    = MrConstants('q')
	me   = MrConstants('m_e')
	mi   = MrConstants('m_H')
	eps0 = MrConstants('epsilon_0')
	fci  = (1e-9 * q / (mi * 2 *!pi)) * bmag                   ;1e-9 to rad/s .... 1/(2*pi) to Hz
	fce  = (1e-9 * q / (me * 2 *!pi)) * bmag                   ;1e-9 to rad/s .... 1/(2*pi) to Hz
	fpi  = 1e3 * Sqrt(n) * Sqrt(q^2 / (2 * !pi * mi * eps0))   ;1e3  to rad/s .... 1/(2*pi) to Hz
	fpe  = 1e3 * Sqrt(n) * Sqrt(q^2 / (2 * !pi * me * eps0))   ;1e3  to rad/s .... 1/(2*pi) to Hz
	flh  = ( 1.0 / ( fci * fce ) + 1.0 / fpi^2 )^(-1.0/2.0)

;-------------------------------------------
; Attributes ///////////////////////////////
;-------------------------------------------
	f_fsm  = oBx_psd_fsm['DEPEND_0']
	frange = [f_fsm[1], f_fsm[-1]]
	yrange = [oBx_psd_fsm.min, oBx_psd_fgm.max]
	
	;
	; FGM
	;

	;BMAG
	oBmag_psd_fgm['AXIS_RANGE'] = yrange
	oBmag_psd_fgm['COLOR']      = 'Red'
	oBmag_psd_fgm['LABEL']      = 'FGM'
	oBmag_psd_fgm['PLOT_TITLE'] = StrUpCase(StrJoin([sc, fgm_instr+'-FSM', mode, coords], ' '))
	oBmag_psd_fgm['TITLE']      = '|B|!C(nT)'
	ofreq                     = oBmag_psd_fgm['DEPEND_0']
	ofreq['AXIS_RANGE']       = frange
	ofreq['LOG']              = 1B
	
	;BX
	oBx_psd_fgm['AXIS_RANGE'] = yrange
	oBx_psd_fgm['COLOR']      = 'Red'
	oBx_psd_fgm['LABEL']      = 'FGM'
	oBx_psd_fgm['TITLE']      = 'Bx!C(nT^2/Hz)'
	ofreq                     = oBx_psd_fgm['DEPEND_0']
	ofreq['AXIS_RANGE']       = frange
	ofreq['LOG']              = 1B
	
	;BY
	oBy_psd_fgm['AXIS_RANGE'] = yrange
	oBy_psd_fgm['COLOR']      = 'Red'
	oBy_psd_fgm['LABEL']      = 'FGM'
	oBy_psd_fgm['TITLE']      = 'By!C(nT^2/Hz)'
	ofreq                     = oBy_psd_fgm['DEPEND_0']
	ofreq['AXIS_RANGE']       = frange
	ofreq['LOG']              = 1B
	
	;BZ
	oBz_psd_fgm['AXIS_RANGE'] = yrange
	oBz_psd_fgm['COLOR']      = 'Red'
	oBz_psd_fgm['LABEL']      = 'FGM'
	oBz_psd_fgm['TITLE']      = 'Bz!C(nT^2/Hz)'
	ofreq                     = oBz_psd_fgm['DEPEND_0']
	ofreq['AXIS_RANGE']       = frange
	ofreq['LOG']              = 1B
	
	;
	; FSM
	;
	
	;BMAG
	oBmag_psd_fsm['COLOR'] = 'Black'
	oBmag_psd_fsm['LABEL'] = 'FSM'
	oBmag_psd_fsm['TITLE'] = '|B|!C(nT^2/Hz)'
	
	;BX
	oBx_psd_fsm['COLOR'] = 'Black'
	oBx_psd_fsm['LABEL'] = 'FSM'
	oBx_psd_fsm['TITLE'] = 'Bx!C(nT^2/Hz)'
	
	;BY
	oBy_psd_fsm['COLOR'] = 'Black'
	oBy_psd_fsm['LABEL'] = 'FSM'
	oBy_psd_fsm['TITLE'] = 'By!C(nT^2/Hz)'
	
	;BZ
	oBz_psd_fsm['COLOR'] = 'Black'
	oBz_psd_fsm['LABEL'] = 'FSM'
	oBz_psd_fsm['TITLE'] = 'Bz!C(nT^2/Hz)'
	
	;
	; SCM
	;
	
	;BX
	oBx_psd_scm['COLOR'] = 'Blue'
	oBx_psd_scm['LABEL'] = 'SCM'
	oBx_psd_scm['TITLE'] = 'Bx!C(nT^2/Hz)'
	
	;BY
	oBy_psd_scm['COLOR'] = 'Blue'
	oBy_psd_scm['LABEL'] = 'SCM'
	oBy_psd_scm['TITLE'] = 'By!C(nT^2/Hz)'

	;BZ
	oBz_psd_scm['COLOR'] = 'Blue'
	oBz_psd_scm['LABEL'] = 'SCM'
	oBz_psd_scm['TITLE'] = 'Bz!C(nT^2/Hz)'
	
;-------------------------------------------
; Plot /////////////////////////////////////
;-------------------------------------------
	win = MrVar_PlotTS( [bmag_psd_fgm_vname, bx_psd_fgm_vname, by_psd_fgm_vname, bz_psd_fgm_vname], $
	                    /NO_REFRESH, $
	                    YSIZE = 750 )
	
	;Overplot SCM & FSM
	win = MrVar_OPlotTS( [bx_psd_fgm_vname, by_psd_fgm_vname, bz_psd_fgm_vname], $
	                     [bx_psd_scm_vname, by_psd_scm_vname, bz_psd_scm_vname] )
	win = MrVar_OPlotTS( [bmag_psd_fgm_vname, bx_psd_fgm_vname, by_psd_fgm_vname, bz_psd_fgm_vname], $
	                     [bmag_psd_fsm_vname, bx_psd_fsm_vname, by_psd_fsm_vname, bz_psd_fsm_vname] )
	
	;Mark plasma frequencies
	vnames = [bmag_psd_fgm_vname, bx_psd_fgm_vname, by_psd_fgm_vname, bz_psd_fgm_vname]
	lines  = [fci, fce, fpi, fpe, flh]
	names  = ['fci', 'fce', 'fpi', 'fpi', 'flh']
	FOR i = 0, N_Elements(vnames) - 1 DO BEGIN
		;Get the graphics object
		oGfx   = win[vnames[i]]
		yrange = oGfx.yrange
		xrange = oGfx.xrange
		yr     = alog10(yrange)

		;Add lines
		FOR j = 0, N_Elements(lines) - 1 DO BEGIN
			;Skip lines that are off scale
			IF lines[j] LT xrange[0] || lines[j] GT xrange[1] THEN CONTINUE
			
			
			;Create a line
			oLine = MrPlotS( [lines[j], lines[j]], yrange, LINESTYLE='--', $
			                 NAME   = 'Line: ' + names[j], $
			                 TARGET = oGfx )
			
			;Add text
			oLine = MrText( lines[j], 10^( (yr[1] - yr[0])*0.9 + yr[0] ), names[j], $
			                /DATA, $
			                NAME   = 'Text: ' + names[j], $
			                TARGET = oGfx )
		ENDFOR
	ENDFOR
	
	
	win[0] -> SetLayout, [1,1]
	win -> TrimLayout
	win.oxmargin = [10,5]

	win -> Refresh
	!MrMMS -> SetProperty, DROPBOX_ROOT=dropbox_in, MIRROR_ROOT=mirror_in, OFFLINE=offline_in
	RETURN, win
END