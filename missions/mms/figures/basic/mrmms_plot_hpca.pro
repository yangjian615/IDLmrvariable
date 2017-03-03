; docformat = 'rst'
;
; NAME:
;       MrMMS_Plot_DIS
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
;   Generate a plot of FPI quantities:
;       1. FGM Bxyz
;       2. Energy Spectr Omni
;       3. Density
;       4. Vxyz
;       5. Tpara, Tperp
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
FUNCTION MrMMS_Plot_HPCA, sc, mode, $
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
	IF N_Elements(optdesc) EQ 0 THEN optdesc = 'moments'
	
;-------------------------------------------
; Variable Names ///////////////////////////
;-------------------------------------------
	instr = 'hpca'
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

	h_n_vname     = StrJoin( [sc, instr, 'hplus', 'number', 'density'],          '_' )
	h_v_vname     = StrJoin( [sc, instr, 'hplus', 'ion',    'bulk', 'velocity'], '_' )
	h_t_vname     = StrJoin( [sc, instr, 'hplus', 'scalar', 'temperature'],      '_' )
	h_tpar_vname  = StrJoin( [sc, instr, 'hplus', 'tparallel'],                  '_' )
	h_tperp_vname = StrJoin( [sc, instr, 'hplus', 'tperp'],                      '_' )

	he_n_vname     = StrJoin( [sc, instr, 'heplus', 'number', 'density'],          '_' )
	he_v_vname     = StrJoin( [sc, instr, 'heplus', 'ion',    'bulk', 'velocity'], '_' )
	he_t_vname     = StrJoin( [sc, instr, 'heplus', 'scalar', 'temperature'],      '_' )
	he_tpar_vname  = StrJoin( [sc, instr, 'heplus', 'tparallel'],                  '_' )
	he_tperp_vname = StrJoin( [sc, instr, 'heplus', 'tperp'],                      '_' )

	o_n_vname     = StrJoin( [sc, instr, 'oplus', 'number', 'density'],          '_' )
	o_v_vname     = StrJoin( [sc, instr, 'oplus', 'ion',    'bulk', 'velocity'], '_' )
	o_t_vname     = StrJoin( [sc, instr, 'oplus', 'scalar', 'temperature'],      '_' )
	o_tpar_vname  = StrJoin( [sc, instr, 'oplus', 'tparallel'],                  '_' )
	o_tperp_vname = StrJoin( [sc, instr, 'oplus', 'tperp'],                      '_' )

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
		MrMMS_Load_Data, sc, instr, mode, level, $
		                 OPTDESC   = optdesc, $
		                 VARFORMAT = [ h_n_vname,  h_v_vname,  h_t_vname,  h_tpar_vname,  h_tperp_vname, $
		                               he_n_vname, he_v_vname, he_t_vname, he_tpar_vname, he_tperp_vname, $
		                               o_n_vname,  o_v_vname,  o_t_vname,  o_tpar_vname,  o_tperp_vname ]
	ENDIF
	
;-------------------------------------------
; Replace Fill Values //////////////////////
;-------------------------------------------
	oN_H = MrVar_Get(h_n_vname)
	oN_He = MrVar_Get(he_n_vname)
	oN_O = MrVar_Get(o_n_vname)
	oT_H = MrVar_Get(h_t_vname)
	oT_He = MrVar_Get(he_t_vname)
	oT_O = MrVar_Get(o_t_vname)
	oTpar_H = MrVar_Get(h_tpar_vname)
	oTperp_H = MrVar_Get(h_tperp_vname)
	oTpar_He = MrVar_Get(he_tpar_vname)
	oTperp_He = MrVar_Get(he_tperp_vname)
	oTpar_O = MrVar_Get(o_tpar_vname)
	oTperp_O = MrVar_Get(o_tperp_vname)
	
	
	oN_H      -> ReplaceValue, 0
	oN_He     -> ReplaceValue, 0
	oN_O      -> ReplaceValue, 0
	oT_H      -> ReplaceValue, 0
	oT_He     -> ReplaceValue, 0
	oT_O      -> ReplaceValue, 0
	oTpar_H   -> ReplaceValue, 0
	oTperp_H  -> ReplaceValue, 0
	oTpar_He  -> ReplaceValue, 0
	oTperp_He -> ReplaceValue, 0
	oTpar_O   -> ReplaceValue, 0
	oTperp_O  -> ReplaceValue, 0
	
;-------------------------------------------
; Properties ///////////////////////////////
;-------------------------------------------
	;BMAG
	oB = MrVar_Get(b_vname)
	oB['PLOT_TITLE'] = StrUpCase( StrJoin( [sc, instr, mode, level], ' ' ) )

	;N H+
	oN_H['COLOR'] = 'Blue'
	oN_H['LABEL'] = 'H+'
	oN_H['LOG']   = 1B
	oN_H['TITLE'] = 'N!C(cm$\up-3$)'

	;N He+
	oN_He['COLOR'] = 'Magenta'
	oN_He['LABEL'] = 'He+'
	oN_He['LOG']   = 1B
	oN_He['TITLE'] = 'N!C(cm$\up-3$)'

	;N O+
	oN_O['COLOR'] = 'Orange'
	oN_O['LABEL'] = 'O+'
	oN_O['LOG']   = 1B
	oN_O['TITLE'] = 'N!C(cm$\up-3$)'

	;V H+
	oV = MrVar_Get(h_v_vname)
	oV['TITLE'] = 'V H+!C(km/s)'
	oV['LABEL'] = ['Vx', 'Vy', 'Vz']

	;V He+
	oV = MrVar_Get(he_v_vname)
	oV['TITLE'] = 'V He+!C(km/s)'
	oV['LABEL'] = ['Vx', 'Vy', 'Vz']

	;V O+
	oV = MrVar_Get(o_v_vname)
	oV['TITLE'] = 'V O+!C(km/s)'
	oV['LABEL'] = ['Vx', 'Vy', 'Vz']

	;T H+
	oT_H['COLOR'] = 'Blue'
	oT_H['LABEL'] = 'H+'
	oT_H['LOG']   = 1B
	oT_H['TITLE'] = 'T!C(eV)'

	;T He+
	oT_He['COLOR'] = 'Magenta'
	oT_He['LABEL'] = 'He+'
	oT_He['LOG']   = 1B
	oT_He['TITLE'] = 'T!C(eV)'

	;T O+
	oT_O['COLOR'] = 'Orange'
	oT_O['LABEL'] = 'O+'
	oT_O['LOG']   = 1B
	oT_O['TITLE'] = 'T!C(eV)'

	;TPARA H+
	oTpar_H['COLOR'] = 'Blue'
	oTpar_H['LABEL'] = 'Tpara'
	oTpar_H['LOG']   = 1B
	oTpar_H['TITLE'] = 'T H+!C(eV)'

	;TPERP H+
	oTperp_H['COLOR'] = 'Red'
	oTperp_H['LABEL'] = 'Tperp'
	oTperp_H['LOG']   = 1B
	oTperp_H['TITLE'] = 'T H+!C(eV)'

	;TPARA He+
	oTpar_He['COLOR'] = 'Blue'
	oTpar_He['LABEL'] = 'Tpara'
	oTpar_He['LOG']   = 1B
	oTpar_He['TITLE'] = 'T He+!C(eV)'

	;TPERP He+
	oTperp_He['COLOR'] = 'Red'
	oTperp_He['LABEL'] = 'Tperp'
	oTperp_He['LOG']   = 1B
	oTperp_HE['TITLE'] = 'T He+!C(eV)'

	;TPARA O+
	oTpar_O['COLOR'] = 'Blue'
	oTpar_O['LABEL'] = 'Tpara'
	oTpar_O['LOG']   = 1B
	oTpar_O['TITLE'] = 'T O+!C(eV)'

	;TPERP O+
	oTperp_O['COLOR'] = 'Red'
	oTperp_O['LABEL'] = 'Tperp'
	oTperp_O['LOG']   = 1B
	oTperp_O['TITLE'] = 'T O+!C(eV)'
	
	;Axis ranges
	oN_H['AXIS_RANGE'] = [1e-5 > oN_O.min, oN_H.max]
	oT_H['AXIS_RANGE'] = [ 1e-1 > min( [oT_O.min, oT_He.min, oT_H.min] ), max( [oT_O.max, oT_He.max, oT_H.max] ) ]
	oTpar_H['AXIS_RANGE']  = [ 1e-1 > min( [oTpar_H.min,  oTperp_H.min] ),  max( [oTpar_H.max,  oTperp_H.max] ) ]
	oTpar_He['AXIS_RANGE'] = [ 1e-1 > min( [oTpar_He.min, oTperp_He.min] ), max( [oTpar_He.max, oTperp_He.max] ) ]
	oTpar_O['AXIS_RANGE']  = [ 1e-1 > min( [oTpar_O.min,  oTperp_O.min] ),  max( [oTpar_O.max,  oTperp_O.max] ) ]

;-------------------------------------------
; Plot Data ////////////////////////////////
;-------------------------------------------
	;Plot data
	win = MrVar_PlotTS( [b_vname, h_n_vname, h_v_vname, he_v_vname, o_v_vname, $
	                     h_t_vname, h_tpar_vname, he_tpar_vname, o_tpar_vname], $
	                    /NO_REFRESH, $
	                    XSIZE = 680, $
	                    YSIZE = 700 )
	
	;Overplots
	win = MrVar_OPlotTS( h_n_vname, [he_n_vname, o_n_vname] )
	win = MrVar_OPlotTS( h_t_vname, [he_t_vname, o_t_vname] )
	win = MrVar_OPlotTS( [h_tpar_vname, he_tpar_vname, o_tpar_vname], $
	                     [h_tperp_vname, he_tperp_vname, o_tperp_vname] )

	;Pretty-up the window
	win[0] -> SetLayout, [1,1]
	win    -> TrimLayout
	win    -> SetProperty, OXMARGIN=[13, 8]
	win    -> Refresh

	RETURN, win
END