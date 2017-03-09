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
;       2. Sx
;       3. Sy 
;       4. Sz
;       5. Div( S )
;
; :Categories:
;   MMS
;
; :Params:
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
;       2017/02/01  -   Written by Matthew Argall
;-
FUNCTION MrMMS_Plot_4sc_PoyntingFlux, mode, $
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
	IF N_Elements(ephdesc)  EQ 0 THEN ephdesc  = 'ephts04d'
	
;-------------------------------------------
; Variable Names ///////////////////////////
;-------------------------------------------
	sc     = 'mms' + ['1', '2', '3', '4']
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

	;Source names
	b_vname    = sc + '_' + StrJoin( ['fgm', 'b',    coords,          mode, level], '_' )
	bvec_vname = sc + '_' + StrJoin( ['fgm', 'bvec', coords,          mode, level], '_' )
	bmag_vname = sc + '_' + StrJoin( ['fgm', 'bmag', coords,          mode, level], '_' )
	acb_vname  = sc + '_' + StrJoin( ['scm', 'acb',  coords, optdesc, mode, level], '_' )
	dce_vname  = sc + '_' + StrJoin( ['edp', 'dce',  coords,          mode, level], '_' )
	eph_vname  = sc + '_' + StrJoin( ['mec', 'r',    coords],                       '_' )
	
	;Output names
	s_vname      = sc + '_' + StrJoin( ['poyntingflux', coords, optdesc, mode, level], '_' )
	s_fac_vname  = sc + '_' + StrJoin( ['poyntingflux', 'fac',  optdesc, mode, level], '_' )
	sx_fac_vname = s_fac_vname + '_x'
	sy_fac_vname = s_fac_vname + '_y'
	sz_fac_vname = s_fac_vname + '_z'
	divs_vname   = StrJoin( ['mms', 'div', 'poyntingflux', optdesc, mode, level], '_' )
	
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
		MrMMS_Load_Data, sc, 'edp', mode, level, $
		                 OPTDESC   = 'dce', $
		                 VARFORMAT = dce_vname
		
		;DEFEPH
		IF ephdesc EQ 'defeph' THEN BEGIN
			MrMMS_Load_data, sc, $
			                 ANC_PRODUCT = ephdesc, $
			                 /ANCILLARY, $
			                 VARFORMAT   = '*R*'
		
		;MEC files
		ENDIF ELSE BEGIN
			MrMMS_Load_Data, sc, 'mec', 'srvy', 'l2', $
			                 OPTDESC   = ephdesc, $
			                 VARFORMAT = '*_r_' + coords
		ENDELSE
	ENDIF

;-------------------------------------------
; Step Through Each Spacecraft /////////////
;-------------------------------------------
	;Interpolate everything to SCM on MMS1
	oB     = MrVar_Get(acb_vname[0])
	oT     = oB['TIMEVAR']
	colors = ['Red', 'Forest Green', 'Blue', 'Black']
	srange = [!values.f_infinity, -!values.f_infinity]
	
	oS     = ObjArr(4)
	oS_fac = ObjArr(4)
	FOR i = 0, 3 DO BEGIN
	
	;-------------------------------------------
	; Prepare Data /////////////////////////////
	;-------------------------------------------
		;Interpolate B
		oB     = MrVar_Get(acb_vname[i])
		oB_scm = oB -> Interpol(oT)
	
		;Interpolate E
		oE     = MrVar_Get(dce_vname[i])
		oE_scm = oE -> Interpol(oT)
	
		;Detrend data
		odB = oB_scm -> Detrend(ndetrend)
		odE = oE_scm -> Detrend(ndetrend)
		Obj_Destroy, [oB_scm, oE_scm]
	
	;-------------------------------------------
	; Compute Poynting Flux ////////////////////
	;-------------------------------------------
	
		;Poynting Flux Vector
		oS[i] = MrVar_PoyntingFlux( odB, odE, $
		                            /CACHE, $
		                            NAME = s_vname[i])
		Obj_Destroy, [odB, odE]
		
		;Rotate into field-aligned coordinate system
		oS_fac[i] = MrVar_FAC_Rotate( oS[i], bvec_vname[i], $
		                              /CACHE, $
		                              NAME = s_fac_vname[i] )
	
	;-------------------------------------------
	; Split S into Components //////////////////
	;-------------------------------------------
		oS_fac[i] -> Split, oSx, oSy, oSz, /CACHE
		
		;Range
		srange[0] <= Min( [oSx.min, oSy.min, oSz.min] )
		srange[1] >= Max( [oSx.max, oSy.max, oSz.max] )
		
		;SX
		oSx['COLOR']      = colors[i]
		oSx['LABEL']      = 'mms' + String(i+1, FORMAT='(i1)')
		oSx['PLOT_TITLE'] = 'Poynting Flux'
		oSx['TITLE']      = 'Sperp1!C$\mu$W/m^2'
		
		;SY
		oSy['COLOR']      = colors[i]
		oSy['LABEL']      = 'mms' + String(i+1, FORMAT='(i1)')
		oSy['PLOT_TITLE'] = 'Poynting Flux'
		oSy['TITLE']      = 'Sperp2!C$\mu$W/m^2'
		
		;SZ
		oSz['COLOR']      = colors[i]
		oSz['LABEL']      = 'mms' + String(i+1, FORMAT='(i1)')
		oSz['PLOT_TITLE'] = 'Poynting Flux'
		oSz['TITLE']      = 'Spar!C$\mu$W/m^2'
	ENDFOR
	
;-------------------------------------------
; Divergence of Poynting Flux //////////////
;-------------------------------------------

	;Prepare reciprocal vectors
	oPos = MrVar_RecipVec( eph_vname[0], eph_vname[1], eph_vname[2], eph_vname[3], $
	                       TIME = oT )
	
	;Compute the divergence
	oDivS = oPos -> Divergence( oS[0], oS[1], oS[2], oS[3], $
	                            /CACHE, $
	                            NAME = divs_vname )
	Obj_Destroy, oPos
	
	;Attributes
	oDivS['PLOT_TITLE'] = 'Divergence of Poynting Flux'
	oDivS['TITLE']      = 'Div(S)!C$\mu$W/m^3'
	oDivS['UNITS']      = '\mu W/m^3'
	
;-------------------------------------------
; Attributes ///////////////////////////////
;-------------------------------------------
	;B
	oB = MrVar_Get(b_vname[0])
	oB['PLOT_TITLE'] = ''
	oB['TITLE']      = 'B MMS1!C(nT)'
	
	;SX
	oSx = MrVar_Get(sx_fac_vname[0])
	oSx['AXIS_RANGE'] = srange
	
	;SY
	oSy = MrVar_Get(sy_fac_vname[0])
	oSy['AXIS_RANGE'] = srange
	
	;SZ
	oSz = MrVar_Get(sz_fac_vname[0])
	oSz['AXIS_RANGE'] = srange
	
;-------------------------------------------
; Plot /////////////////////////////////////
;-------------------------------------------
	win = MrVar_PlotTS( [b_vname[0], sx_fac_vname[0], sy_fac_vname[0], sz_fac_vname[0], divs_vname], $
	                    /NO_REFRESH, $
	                    XSIZE  = 680, $
	                    YSIZE  = 700 )
	
	;Overplot other spacecraft
	win = MrVar_OPlotTS( sx_fac_vname[0], sx_fac_vname[1:3] )
	win = MrVar_OPlotTS( sy_fac_vname[0], sy_fac_vname[1:3] )
	win = MrVar_OPlotTS( sz_fac_vname[0], sz_fac_vname[1:3] )
	
	;Fix the layout
	win.oxmargin = [12, 15]
	win[0]       -> SetLayout, [1,1]
	win          -> TrimLayout
	win          -> Refresh

	RETURN, win
END