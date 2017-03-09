; docformat = 'rst'
;
; NAME:
;       MrMMS_Plot_SCM_PoyntXYZ
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
FUNCTION MrMMS_Plot_SCM_Poynt, sc, mode, component, $
OPTDESC=optdesc, $
NFFT=nfft, $
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
	IF N_Elements(fgm_instr) EQ 0 THEN fgm_instr = 'fgm'
	IF N_Elements(nfft)      EQ 0 THEN nfft      = 512
	IF N_Elements(nshIFt)    EQ 0 THEN nshIFt    = nfft / 2
	IF N_Elements(trange)    GT 0 THEN MrVar_SetTRange, trange
	
;-------------------------------------------
; Variable Names ///////////////////////////
;-------------------------------------------
	level  = 'l2'
	coords = 'gse'
	IF N_Elements(optdesc) EQ 0 THEN BEGIN
		CASE mode OF
			'slow': optdesc = 'scs'
			'fast': optdesc = 'scf'
			'srvy': optdesc = 'scsrvy'
			'brst': optdesc = 'scb'       ;also schb
			ELSE: Message, 'Invalid MODE: "' + mode + '".'
		ENDCASE
	ENDIF
	CASE fgm_instr OF
		'afg': fgm_level = 'l2pre'
		'dfg': fgm_level = 'l2pre'
		'fgm': fgm_level = 'l2'
		ELSE: Message, 'Invalid FGM instrument: "' + fgm_instr + '".'
	ENDCASE

	;Source names
	b_vname    = StrJoin( [sc, fgm_instr, 'b',    coords, mode, fgm_level], '_' )
	bvec_vname = StrJoin( [sc, fgm_instr, 'bvec', coords, mode, fgm_level], '_' )
	acb_vname  = StrJoin( [sc, 'scm', 'acb', coords, optdesc, mode, level], '_' )
	dce_vname  = StrJoin( [sc, 'edp', 'dce', coords, mode, level], '_' )
	
	;Output names
	db_par_vname   = StrJoin( [sc, 'scm', 'db', 'par', optdesc, mode, level], '_' )
	db_perp_vname  = StrJoin( [sc, 'scm', 'db', 'perp', optdesc, mode, level], '_' )
	s_vname  = StrJoin( [sc, 'poyntspec', 'fac', optdesc, mode, level], '_' )
	sx_vname = s_vname + '_x'
	sy_vname = s_vname + '_y'
	sz_vname = s_vname + '_z'
	
;-------------------------------------------
; Get Data /////////////////////////////////
;-------------------------------------------
	IF tf_load THEN BEGIN
	;-------------------------------------------
	; FGM //////////////////////////////////////
	;-------------------------------------------
		;B
		MrMMS_FGM_Load_Data, sc, mode, $
		                     INSTR     = fgm_instr, $
		                     LEVEL     = fgm_level, $
		                     VARFORMAT = ['*b_' + coords + '*', $
		                                  '*fgm_*' + coords]
	
	;-------------------------------------------
	; SCM //////////////////////////////////////
	;-------------------------------------------
		;B
		MrMMS_Load_Data, sc, 'scm', mode, level, $
		                 OPTDESC = optdesc, $
		                 VARFORMAT = '*acb_' + coords + '*'
	
	;-------------------------------------------
	; EDP //////////////////////////////////////
	;-------------------------------------------
		MrMMS_Load_Data, sc, 'edp', mode, level, /TEAM_SITE, $
		                 OPTDESC   = 'dce', $
		                 VARFORMAT = dce_vname
	ENDIF
	
;-------------------------------------------
; Field-Aligned Coordinates ////////////////
;-------------------------------------------
	;Get data
	oB  = MrVar_Get(bvec_vname)
	odB = MrVar_Get(acb_vname)
	oE  = MrVar_Get(dce_vname)

	;Interpolate
	oB = oB -> Interpol(odB)
	oE = oE -> Interpol(odB)
	
	;FAC
	oT  = MrVar_FAC(oB, 'CROSSX')
	odB = oT ## odB
	oE  = oT ## oE
	
;-------------------------------------------
; Parallel & Perpendicular Components //////
;-------------------------------------------
	
	;Par
	odB -> Split, odBx, odBy, odBpar
	odBpar -> SetName, db_par_vname
	odBpar -> Cache
	
	;Perp
	odBperp = MrScalarTS( odB['TIMEVAR'], sqrt(odBx['DATA']^2 + odBy['DATA']^2), $
	                      /CACHE, $
	                      NAME = db_perp_vname )
	
	
;-------------------------------------------
; Poynting Spectra /////////////////////////
;-------------------------------------------
	f0     = 50
	f1     = 1024
	A      = 50
	nTerms = nfft

	;Detrend
	oE     = oE  -> Digital_Filter(f0/4096.0, f1/4096.0, A, nTerms)
	odB    = odB -> Digital_Filter(f0/4096.0, f1/4096.0, A, nTerms)

	;Poynting flux
	oS = MrVar_PoyntingSpectra( oB, oE, nfft, nshift, $
	                            /CACHE, $
	                            NAME   = s_vname, $
	                            SPHERE = 1, $
	                            WINDOW = 'hanning' )

;-------------------------------------------
; Properties ///////////////////////////////
;-------------------------------------------
	title = String(StrUpCase(sc), f0, f1, FORMAT='(%"%s Poynting Flux Filtered %i-%i Hz")')
	
	odBpar['COLOR']      = 'Blue'
	odBpar['PLOT_TITLE'] = title
	odBpar['LABEL']      = 'Par'
	odBpar['TITLE']      = 'dB!C(nT)'
	
	odBperp['AXIS_RANGE'] = [ min( [odBpar.min, odBperp.min] ), max( [odBpar.max, odBperp.max] ) ]
	odBperp['LABEL']      = 'Perp'
	odBperp['PLOT_TITLE'] = title
	odBperp['TITLE']      = 'dB!C(nT)'
	
	oFreq = MrVar_Get((oS[0])['DEPEND_1'])
	oFreq['AXIS_RANGE'] = [f0, f1]

;-------------------------------------------
; Plot Data ////////////////////////////////
;-------------------------------------------

	win = MrVar_PlotTS( [db_perp_vname, sx_vname, sy_vname, sz_vname], $
	                    XSIZE  = 1100, $
	                    YSIZE  = 700 )
	win = MrVar_OPlotTS( db_perp_vname, db_par_vname )
	
	win    -> Refresh, /DISABLE
	win[0] -> SetLayout, [1,1]
	win    -> TrimLayout
	win    -> Refresh
	

	RETURN, win
END