; docformat = 'rst'
;
; NAME:
;       MrMMS_Plot_PoyntingFlux
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
;   Plot quantities related to the Poynting flux:
;       1. Bxyz, |B|
;       2. dBxyz
;       3. dExyz 
;       4. Sxyz
;       5. Sx Spectra
;       6. Sy Spectra
;       7. Sz Spectra
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
;       2017/02/01  -   Simplify plot to 1-column focusing of Poynting Flux - MRA
;-
FUNCTION MrMMS_Plot_PoyntingFlux, sc, mode, $
OPTDESC=optdesc, $
NFFT=nfft, $
NSHIFT=nShift, $
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
	IF N_Elements(nfft)     EQ 0 THEN nfft     = 512
	IF N_Elements(nshift)   EQ 0 THEN nshift   = nfft / 2
	IF N_Elements(ndetrend) EQ 0 THEN ndetrend = nfft
	IF N_Elements(trange)   GT 0 THEN MrVar_SetTRange, trange
	
;-------------------------------------------
; Variable Names ///////////////////////////
;-------------------------------------------
	level  = 'l2'
	coords = 'gse'
	IF n_elements(optdesc) EQ 0 THEN BEGIN
		CASE mode OF
			'slow': optdesc = 'scs'
			'fast': optdesc = 'scf'
			'srvy': optdesc = 'scsrvy'
			'brst': optdesc = 'scb'       ;also schb
			ELSE: Message, 'Invalid MODE: "' + mode + '".'
		ENDCASE
	ENDIF

	;Source names
	b_vname    = sc + '_fgm_b_'         + coords + '_'                 + mode + '_' + level
	bvec_vname = sc + '_fgm_bvec_'      + coords + '_'                 + mode + '_' + level
	bmag_vname = sc + '_fgm_bmag_'      + coords + '_'                 + mode + '_' + level
	acb_vname  = sc + '_scm_acb_'       + coords + '_' + optdesc + '_' + mode + '_' + level
	dce_vname  = sc + '_edp_dce_'       + coords + '_'                 + mode + '_' + level
	
	;Output names
	db_fac_vname  = StrJoin( [sc, 'scm', 'db',        coords, optdesc, mode, level],      '_' )
	de_fac_vname  = StrJoin( [sc, 'edp', 'de',        coords,          mode, level],      '_' )
	sspec_vname   = StrJoin( [sc, 'scm', 'poyntspec', coords, optdesc, mode, level],      '_' )
	sspec_x_vname = StrJoin( [sc, 'scm', 'poyntspec', coords, optdesc, mode, level, 'x'], '_' )
	sspec_y_vname = StrJoin( [sc, 'scm', 'poyntspec', coords, optdesc, mode, level, 'y'], '_' )
	sspec_z_vname = StrJoin( [sc, 'scm', 'poyntspec', coords, optdesc, mode, level, 'z'], '_' )
	svec_vname    = StrJoin( [sc, 'scm', 'poyntvec',  coords, optdesc, mode, level],      '_' )
	
;-------------------------------------------
; Get Data /////////////////////////////////
;-------------------------------------------
	IF tf_load THEN BEGIN
		;FGM
		MrMMS_FGM_Load_Data, sc, mode, $
		                     VARFORMAT = '*b_'+coords+'*'+level
		
		;SCM
		MrMMS_Load_Data, sc, 'scm', mode, level, $
		                 OPTDESC = optdesc
	
		;EDP
		MrMMS_Load_Data, sc, 'edp', mode, level, /TEAM_SITE, $
		                 OPTDESC   = 'dce', $
		                 VARFORMAT = dce_vname
	ENDIF
	
;-------------------------------------------
; Rotate to Field-Aligned System ///////////
;-------------------------------------------
	;Interpolate EDP to SCM
	oE     = MrVar_Get(dce_vname)
	oE_scm = oE -> Interpol(acb_vname)

	;Rotate into field-aligned coordinate system
	oB_fac = MrVar_FAC_Rotate(acb_vname, bvec_vname)
	oE_fac = MrVar_FAC_Rotate(oE_scm, bvec_vname)
	
	Obj_Destroy, oE_scm
	
;-------------------------------------------
; Poynting Flux ////////////////////////////
;-------------------------------------------
	
	;Detrend data
	odB_fac = oB_fac -> Detrend(ndetrend, /CACHE, NAME=db_fac_vname)
	odE_fac = oE_fac -> Detrend(ndetrend, /CACHE, NAME=de_fac_vname)
	Obj_Destroy, [oB_fac, oE_fac]
	
	;Poynting Flux Vector
	oSvec = MrVar_PoyntingFlux( odB_fac, odE_fac, $
	                            /CACHE, $
	                            NAME = svec_vname)
	
	;Poynting Flux Spectra
	oSspec = MrVar_PoyntingSpectra( odB_fac, odE_fac, nfft, nshift, $
	                                /CACHE, $
	                                NAME   = sspec_vname, $
	                                /SPHERE, $
	                                WINDOW = 'hanning')
	
;-------------------------------------------
; Properties ///////////////////////////////
;-------------------------------------------
	trange = MrVar_GetTRange()
	title  = strupcase(sc)

	;B
	oB = MrVar_Get(b_vname)
	oB['PLOT_TITLE'] = title
	
	;dB
	odB_fac['LABEL']      = ['Perp1', 'Perp2', 'Par']
	odB_fac['PLOT_TITLE'] = 'Detrended SCM magnetic field'
	odB_fac['TITLE']      = 'dB!C(nT)'
	odB_fac['UNITS']      = 'nT'
	
	;dE
	odE_fac['LABEL']      = ['Perp1', 'Perp2', 'Par']
	odE_fac['PLOT_TITLE'] = 'Detrended EDP electric field'
	odE_fac['TITLE']      = 'dE!C(mV/m)'
	odE_fac['UNITS']      = 'mV/m'
	
;-------------------------------------------
; Plot Data ////////////////////////////////
;-------------------------------------------
	win = MrVar_PlotTS( [b_vname, db_fac_vname, de_fac_vname, svec_vname, $
	                     sspec_x_vname, sspec_y_vname, sspec_z_vname], $
	                    /NO_REFRESH, $
	                    XSIZE  = 680, $
	                    YSIZE  = 750 )
	
	win.oxmargin = [12, 15]
	win[0]       -> SetLayout, [1,1]
	win          -> TrimLayout
	win          -> Refresh

	RETURN, win
END