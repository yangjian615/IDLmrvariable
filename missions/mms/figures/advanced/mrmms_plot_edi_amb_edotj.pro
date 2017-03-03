; docformat = 'rst'
;
; NAME:
;       MrMMS_Plot_EDI_Amb_EdotJ
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
;   Generate a plot of EDI Ambient quantities:
;        1. FGM Bxyz, |B|
;        2. EDP Exyz
;        3. Flux 0-degrees
;        4. PSD Flux 0-degrees
;        5. Flux 180-degrees
;        6. PSD Flux 180-degrees
;        7. Phi: All channels & pitch angles
;        8. GPD
;        9. Theta: All channels & pitch angles
;       10. PAD
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
;                   Data quality level. Options are: {'l1a' | 'ql' | 'l2'}
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
;       2017/01/22  -   Written by Matthew Argall
;-
FUNCTION MrMMS_Plot_EDI_Amb_EdotJ, sc, mode, $
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
	IF N_Elements(mode)   EQ 0 THEN mode       = 'srvy'
	IF N_Elements(level)  EQ 0 THEN level      = 'l2'
	IF N_Elements(trange) GT 0 THEN MrVar_SetTRange, trange
	
;-------------------------------------------
; Variable Names ///////////////////////////
;-------------------------------------------
	;EDI
	instr      = 'edi'
	coords     = 'gse'
	
	;FGM
	fgm_instr  = 'dfg'
	fgm_level  = 'l2pre'
	fgm_mode   = mode eq 'brst' ? mode : 'srvy'
	fgm_coords = coords EQ 'dbcs'  ? 'dmpa' : coords
	
	;EDP
	edp_instr  = 'edp'
	edp_mode   = mode eq 'brst' ? mode : 'fast'
	edp_coords = coords EQ 'dbcs'  ? 'dsl'  : coords

	;Source names
	IF fgm_level EQ 'l2pre' THEN BEGIN
		fgm_b_vname     = StrJoin( [sc, fgm_instr,        fgm_mode, fgm_level, fgm_coords], '_' )
		fgm_bvec_vname  = StrJoin( [sc, fgm_instr, 'vec', fgm_mode, fgm_level, fgm_coords], '_' )
		fgm_bmag_vname  = StrJoin( [sc, fgm_instr, 'mag', fgm_mode, fgm_level, fgm_coords], '_' )
	ENDIF ELSE BEGIN
		fgm_b_vname     = StrJoin( [sc, fgm_instr, 'b',    fgm_coords, fgm_mode, fgm_level], '_' )
		fgm_bvec_vname  = StrJoin( [sc, fgm_instr, 'bvec', fgm_coords, fgm_mode, fgm_level], '_' )
		fgm_bmag_vname  = StrJoin( [sc, fgm_instr, 'bmag', fgm_coords, fgm_mode, fgm_level], '_' )
	ENDELSE
	e_vname       = StrJoin( [sc, edp_instr, 'dce',  edp_coords, edp_mode, level], '_' )
	
	channels      = ['1', '2', '3', '4']
	flux0_vname   = sc + '_' + instr + '_' + 'flux' + channels + '_'                + '0'   + '_' + mode + '_' + level
	flux180_vname = sc + '_' + instr + '_' + 'flux' + channels + '_'                + '180' + '_' + mode + '_' + level
	traj0_vname   = sc + '_' + instr + '_' + 'traj' + channels + '_' + coords + '_' + '0'   + '_' + mode + '_' + level
	traj180_vname = sc + '_' + instr + '_' + 'traj' + channels + '_' + coords + '_' + '180' + '_' + mode + '_' + level

	;Derived names
	e_edi_vname = sc + '_'+ edp_instr + '_' + 'edi'   + channels + '_' + mode + '_' + level
	j_vname     = sc + '_'+ instr     + '_' + 'j'     + channels + '_' + mode + '_' + level
	jdote_vname = sc                  + '_' + 'jdote' + channels + '_' + mode + '_' + level

;-------------------------------------------
; Get Data /////////////////////////////////
;-------------------------------------------
	IF tf_load THEN BEGIN
		;FGM
		MrMMS_FGM_Load_Data, sc, fgm_mode, $
		                     INSTR     = fgm_instr, $
		                     LEVEL     = fgm_level, $
		                     VARFORMAT = fgm_b_vname

		;EDP
		MrMMS_Load_Data, sc, edp_instr, edp_mode, level, $
		                 OPTDESC   = 'dce', $
		                 VARFORMAT = e_vname

		;EDI
		MrMMS_Load_Data, sc, instr, mode, level
	ENDIF
	
	;Determine which EDI file was loaded
	fnames = MrMMS_Get_FileNames(sc, 'edi', mode, level)
	MrMMS_Parse_Filename, fnames, OPTDESC=optdesc
	iUniq = Uniq(optdesc, Sort(optdesc))
	IF N_Elements(iUniq) NE 1 THEN BEGIN
		MrPrintF, 'LogWarn', 'More than one EDI file type found.'
		MrPrintF, 'LogWarn', '   ' + '[' + StrJoin(optdesc, ', ') + ']'
	ENDIF

;-------------------------------------------
; E-Field //////////////////////////////////
;-------------------------------------------
	
	;Interpolate EDP onto EDI time
	oFlux0 = MrVar_Get(flux0_vname[0])
	oE     = MrVar_Get(e_vname)
	oE_edi = oE -> Interpol(oFlux0)

;-------------------------------------------
; Current, E, E.J //////////////////////////
;-------------------------------------------
	FOR i = 0, 3 DO BEGIN
		str_chan = String(i+1, FORMAT='(i1)')
		
	;-------------------------------------------
	; Current Density //////////////////////////
	;-------------------------------------------
		;Current density
		;   - 1e10 converts to muA/m^2
		oFlux0   = MrVar_Get(flux0_vname[i])
		oFlux180 = MrVar_Get(flux180_vname[i])
		oJ       = 1e10 * MrConstants('q') * (oFlux180 - oFlux0)
		
		;Attributes
		oJ -> SetName, j_vname[i]
		oJ -> Cache
		oJ['PLOT_TITLE'] = 'Current Density from Channel ' + str_chan
		oJ['TITLE']      = 'J!Cch' + str_chan + '!C($\mu$A/m$\up2$)'
		oJ['UNITS']      = '\muA/m^2'
	
	;-------------------------------------------
	; Electric Field ///////////////////////////
	;-------------------------------------------
		;
		; Current is in the direction of 180 degree electrons
		; Project E onto these trajectories
		;
	
		;Get the trajectories, convert to radians
		oTraj_180 = MrVar_Get(traj180_vname[i])
		oTraj_pi  = oTraj_180 * !dtor
		
		;Compute Cartesian components
		x = Sin(oTraj_pi[*,1]) * Cos(oTraj_pi[*,1])
		y = Sin(oTraj_pi[*,1]) * Sin(oTraj_pi[*,1])
		z = Cos(oTraj_pi[*,1])
		
		;Create vector
		oCart = MrVectorTS( oTraj_180['TIMEVAR'], [[x], [y], [z]] )
		
		;Component of E along EDI particle trajectories
		oE_edi_proj = oE_edi -> Dot( oCart, $
		                             /CACHE, $
		                             NAME = e_edi_vname[i] )
		Obj_Destroy, [oTraj_pi, oCart]
		
		;Attributes
		oE_edi_proj['PLOT_TITLE'] = 'Electric field component along Channel ' + str_chan + ' trajectories.'
		oE_edi_proj['TITLE']      = 'E!Cch' + str_chan + '!C(mV/m)'
		oE_edi_proj['UNITS']      = 'mV/m'
	
	;-------------------------------------------
	; Energy Dissipation ///////////////////////
	;-------------------------------------------
		;Dissipation in nW/m^3
		JdotE = oJ * oE_edi_proj
		
		;Attributes
		JdotE -> SetName, jdote_vname[i]
		JdotE -> Cache
		JdotE['PLOT_TITLE'] = 'Energy Dissipation from Channel ' + str_chan
		JdotE['TITLE']      = 'J.E!Cch' + str_chan + '!C(nW/m^3)'
		JdotE['UNITS']      = 'nW/m^3'
	ENDFOR
	
	Obj_Destroy, oE_edi

;-------------------------------------------
; Properties ///////////////////////////////
;-------------------------------------------
	;B
	oB = MrVar_Get(fgm_b_vname)
	oB['PLOT_TITLE'] = StrUpCase( StrJoin( [sc, mode, level, optdesc], ' ' ) )
	
;-------------------------------------------
; Plot Data ////////////////////////////////
;-------------------------------------------
	;Plot data
	win = MrVar_PlotTS( [ fgm_b_vname, e_edi_vname, j_vname, jdote_vname ], $
	                    /NO_REFRESH, $
	                    XSIZE = 680, $
	                    YSIZE = 700 )

	;Pretty-up the window
	win[0] -> SetLayout, [1,1]
	win    -> TrimLayout
	win    -> SetProperty, OXMARGIN=[13, 14]
	win    -> Refresh

	RETURN, win
END