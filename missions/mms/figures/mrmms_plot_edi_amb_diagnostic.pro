; docformat = 'rst'
;
; NAME:
;       MrMMS_Plot_EDI_Amb
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
FUNCTION MrMMS_Plot_EDI_Amb_Diagnostic, sc, mode, channel, $
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
	IF N_Elements(channel) EQ 0 THEN channel    = 1
	IF N_Elements(mode)    EQ 0 THEN mode       = 'srvy'
	IF N_Elements(level)   EQ 0 THEN level      = 'l2'
	IF N_Elements(trange)  GT 0 THEN MrVar_SetTRange, trange
	
;-------------------------------------------
; Variable Names ///////////////////////////
;-------------------------------------------
	;EDI
	instr      = 'edi'
	coords     = 'gse'
	
	;FGM
	fgm_instr  = 'fgm'
	fgm_level  = 'l2'
	fgm_mode   = mode eq 'brst' ? mode : 'srvy'
	fgm_coords = coords EQ 'dbcs'  ? 'dmpa' : coords
	
	;EDP
	edp_instr  = 'edp'
	edp_mode   = mode eq 'brst' ? mode : 'fast'
	edp_coords = coords EQ 'dbcs'  ? 'dsl'  : coords
	
	;
	;Source names
	;
	
	;FGM
	IF fgm_level EQ 'l2pre' THEN BEGIN
		fgm_b_vname     = StrJoin( [sc, fgm_instr,        fgm_mode, fgm_level, fgm_coords], '_' )
		fgm_bvec_vname  = StrJoin( [sc, fgm_instr, 'vec', fgm_mode, fgm_level, fgm_coords], '_' )
		fgm_bmag_vname  = StrJoin( [sc, fgm_instr, 'mag', fgm_mode, fgm_level, fgm_coords], '_' )
	ENDIF ELSE BEGIN
		fgm_b_vname     = StrJoin( [sc, fgm_instr, 'b',    fgm_coords, fgm_mode, fgm_level], '_' )
		fgm_bvec_vname  = StrJoin( [sc, fgm_instr, 'bvec', fgm_coords, fgm_mode, fgm_level], '_' )
		fgm_bmag_vname  = StrJoin( [sc, fgm_instr, 'bmag', fgm_coords, fgm_mode, fgm_level], '_' )
	ENDELSE
	
	;EDP
	e_vname       = StrJoin( [sc, edp_instr, 'dce',  edp_coords, edp_mode, level], '_' )
	
	;EDI
	chan          = String(channel, FORMAT='(i1)')
	gdu0_srvy_vname    = StrJoin( [sc, instr, 'gdu',   '0'], '_' )
	gdu180_srvy_vname  = StrJoin( [sc, instr, 'gdu', '180'], '_' )
	gdu0_vname    = StrJoin( [sc, instr, 'gdu',   '0', mode, level], '_' )
	gdu180_vname  = StrJoin( [sc, instr, 'gdu', '180', mode, level], '_' )
	cts0_vname    = StrJoin( [sc, instr, 'counts1',   '0'], '_' )
	cts180_vname  = StrJoin( [sc, instr, 'counts1', '180'], '_' )
	flux0_vname   = sc + '_' + instr + '_' + 'flux' + chan + '_'                + '0'   + '_' + mode + '_' + level
	flux180_vname = sc + '_' + instr + '_' + 'flux' + chan + '_'                + '180' + '_' + mode + '_' + level
	traj0_vname   = sc + '_' + instr + '_' + 'traj' + chan + '_' + coords + '_' + '0'   + '_' + mode + '_' + level
	traj180_vname = sc + '_' + instr + '_' + 'traj' + chan + '_' + coords + '_' + '180' + '_' + mode + '_' + level

	;Derived BRST names
	cts0_gdu1_vname    = StrJoin( [sc, instr, 'counts',   '0', 'gdu1', mode, level], '_' )
	cts0_gdu2_vname    = StrJoin( [sc, instr, 'counts',   '0', 'gdu2', mode, level], '_' )
	cts180_gdu1_vname  = StrJoin( [sc, instr, 'counts', '180', 'gdu1', mode, level], '_' )
	cts180_gdu2_vname  = StrJoin( [sc, instr, 'counts', '180', 'gdu2', mode, level], '_' )
	flux0_gdu1_vname    = StrJoin( [sc, instr, 'flux'  + chan,   '0', 'gdu1', mode, level], '_' )
	flux0_gdu2_vname    = StrJoin( [sc, instr, 'flux'  + chan,   '0', 'gdu2', mode, level], '_' )
	flux180_gdu1_vname  = StrJoin( [sc, instr, 'flux'  + chan, '180', 'gdu1', mode, level], '_' )
	flux180_gdu2_vname  = StrJoin( [sc, instr, 'flux'  + chan, '180', 'gdu2', mode, level], '_' )
	phi0_gdu1_vname     = StrJoin( [sc, instr, 'phi'   + chan,   '0', 'gdu1', mode, level], '_' )
	phi0_gdu2_vname     = StrJoin( [sc, instr, 'phi'   + chan,   '0', 'gdu2', mode, level], '_' )
	phi180_gdu1_vname   = StrJoin( [sc, instr, 'phi'   + chan, '180', 'gdu1', mode, level], '_' )
	phi180_gdu2_vname   = StrJoin( [sc, instr, 'phi'   + chan, '180', 'gdu2', mode, level], '_' )
	theta0_gdu1_vname   = StrJoin( [sc, instr, 'theta' + chan,   '0', 'gdu1', mode, level], '_' )
	theta0_gdu2_vname   = StrJoin( [sc, instr, 'theta' + chan,   '0', 'gdu2', mode, level], '_' )
	theta180_gdu1_vname = StrJoin( [sc, instr, 'theta' + chan, '180', 'gdu1', mode, level], '_' )
	theta180_gdu2_vname = StrJoin( [sc, instr, 'theta' + chan, '180', 'gdu2', mode, level], '_' )

	;Derived SRVY names
	cts0_gdu1_vname    = StrJoin( [sc, instr, 'counts',   '0', 'gdu1', mode, level], '_' )
	cts0_gdu2_vname    = StrJoin( [sc, instr, 'counts',   '0', 'gdu2', mode, level], '_' )
	cts180_gdu1_vname  = StrJoin( [sc, instr, 'counts', '180', 'gdu1', mode, level], '_' )
	cts180_gdu2_vname  = StrJoin( [sc, instr, 'counts', '180', 'gdu2', mode, level], '_' )

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

		;EDI - L2
		void = Temporary(optdesc)
		MrMMS_EDI_Load_Data, sc, mode, optdesc, $
		                     LEVEL     = 'l2', $
		                     VARFORMAT = [ '*flux*0_'+mode+'*', '*flux*180_'+mode+'*', $
		                                   '*traj*'+coords+'*0_'+mode+'*', '*traj*'+coords+'*180_'+mode+'*', $
		                                   '*edi_gdu*' ]

		;EDI - QL
		void = Temporary(optdesc)
		MrMMS_EDI_Load_Data, sc, 'srvy', optdesc, $
		                     LEVEL     = 'ql', $
		                     VARFORMAT = [ '*counts*', '*gdu*' ]
		
		;The optional descriptor
		MrPrintF, 'LogText', 'EDI OptDesc: "' + optdesc + '".'
	ENDIF

;-------------------------------------------
; BRST: Separate by GDU ////////////////////
;-------------------------------------------
	;Separate GDUs
	oGDU_0  = MrVar_Get(gdu0_vname)
	iGDU1_0 = oGDU_0 -> Where(1, /EQUAL, COUNT=nGDU1_0)
	iGDU2_0 = oGDU_0 -> Where(2, /EQUAL, COUNT=nGDU2_0)
	
	oGDU_180  = MrVar_Get(gdu180_vname)
	iGDU1_180 = oGDU_180 -> Where(1, /EQUAL, COUNT=nGDU1_180)
	iGDU2_180 = oGDU_180 -> Where(2, /EQUAL, COUNT=nGDU2_180)
	
	;Sort data by GDU
	oFlux0       = MrVar_Get(flux0_vname)
	oTraj0       = MrVar_Get(traj0_vname)
	oFlux0_GDU1  = oFlux0[iGDU1_0]
	oFlux0_GDU2  = oFlux0[iGDU2_0]
	oPhi0_GDU1   = oTraj0[iGDU1_0, 0]
	oPhi0_GDU2   = oTraj0[iGDU2_0, 0]
	oTheta0_GDU1 = oTraj0[iGDU1_0, 1]
	oTheta0_GDU2 = oTraj0[iGDU2_0, 1]
	
	oFlux180       = MrVar_Get(flux180_vname)
	oTraj180       = MrVar_Get(traj180_vname)
	oFlux180_GDU1  = oFlux180[iGDU1_180]
	oFlux180_GDU2  = oFlux180[iGDU2_180]
	oPhi180_GDU1   = oTraj180[iGDU1_180, 0]
	oPhi180_GDU2   = oTraj180[iGDU2_180, 0]
	oTheta180_GDU1 = oTraj180[iGDU1_180, 1]
	oTheta180_GDU2 = oTraj180[iGDU2_180, 1]
	
	;Store Data
	oFlux0_GDU1    -> SetName, flux0_gdu1_vname
	oFlux0_GDU2    -> SetName, flux0_gdu2_vname
	oPhi0_GDU1     -> SetName, phi0_gdu1_vname
	oPhi0_GDU2     -> SetName, phi0_gdu2_vname
	oTheta0_GDU1   -> SetName, theta0_gdu1_vname
	oTheta0_GDU2   -> SetName, theta0_gdu2_vname
	oFlux180_GDU1  -> SetName, flux180_gdu1_vname
	oFlux180_GDU2  -> SetName, flux180_gdu2_vname
	oPhi180_GDU1   -> SetName, phi180_gdu1_vname
	oPhi180_GDU2   -> SetName, phi180_gdu2_vname
	oTheta180_GDU1 -> SetName, theta180_gdu1_vname
	oTheta180_GDU2 -> SetName, theta180_gdu2_vname
	
	;Cache variables
	oFlux0_GDU1    -> Cache
	oFlux0_GDU2    -> Cache
	oPhi0_GDU1     -> Cache
	oPhi0_GDU2     -> Cache
	oTheta0_GDU1   -> Cache
	oTheta0_GDU2   -> Cache
	oFlux180_GDU1  -> Cache
	oFlux180_GDU2  -> Cache
	oPhi180_GDU1   -> Cache
	oPhi180_GDU2   -> Cache
	oTheta180_GDU1 -> Cache
	oTheta180_GDU2 -> Cache

;-------------------------------------------
; SRVY: Separate by GDU ////////////////////
;-------------------------------------------
	;Separate the GDUs
	oGDU0   = MrVar_Get(gdu0_srvy_vname)
	iGDU1_0 = oGDU0 -> Where(1, /EQUAL, COUNT=nGDU1_0)
	iGDU2_0 = oGDU0 -> Where(2, /EQUAL, COUNT=nGDU1_0)
	
	oGDU180 = MrVar_Get(gdu180_srvy_vname)
	iGDU1_180 = oGDU180 -> Where(1, /EQUAL, COUNT=nGDU1_180)
	iGDU2_180 = oGDU180 -> Where(2, /EQUAL, COUNT=nGDU1_180)
	
	;Sort data by GDUs
	oCts0      = MrVar_Get(cts0_vname)
	oCts0_GDU1 = oCts0[iGDU1_0]
	oCts0_GDU2 = oCts0[iGDU2_0]
	
	oCts180      = MrVar_Get(cts180_vname)
	oCts180_GDU1 = oCts180[iGDU1_180]
	oCts180_GDU2 = oCts180[iGDU2_180]
	
	;Name data
	oCts0_GDU1   -> SetName, cts0_gdu1_vname
	oCts0_GDU2   -> SetName, cts0_gdu2_vname
	oCts180_GDU1 -> SetName, cts180_gdu1_vname
	oCts180_GDU2 -> SetName, cts180_gdu2_vname
	
	;Store data
	oCts0_GDU1   -> Cache
	oCts0_GDU2   -> Cache
	oCts180_GDU1 -> Cache
	oCts180_GDU2 -> Cache

;-------------------------------------------
; Attributes ///////////////////////////////
;-------------------------------------------
	title = StrUpCase( StrJoin([sc, 'Channel', chan, coords], ' ') )
	
	;B
	oB = MrVar_Get(fgm_b_vname)
	oB['PLOT_TITLE'] = title
	
	;COUNTS0 GDU1
	oCts0_GDU1['LABEL'] = 'GDU1'
	oCts0_GDU1['LINESTYLE'] = 'None'
	oCts0_GDU1['SYMBOL']    = 3
	oCts0_GDU1['TITLE']     = 'Counts!CPA0'
	
	;COUNTS0 GDU2
	oCts0_GDU2['COLOR']     = 'Blue'
	oCts0_GDU2['LABEL']     = 'GDU2'
	oCts0_GDU2['LINESTYLE'] = 'None'
	oCts0_GDU2['SYMBOL']    = 3
	oCts0_GDU2['TITLE']     = 'Counts!CPA0'

	;FLUX0 GDU1
	oFlux0_GDU1['LABEL']     = 'GDU1'
	oFlux0_GDU1['LINESTYLE'] = 'None'
	oFlux0_GDU1['SYMBOL']    = 3
	oFlux0_GDU1['TITLE']     = 'Flux!CCh' + chan + ' PA0!C(cm$\up-2$s$\up-1$)'
	
	;FLUX0 GDU2
	oFlux0_GDU2['COLOR']     = 'Blue'
	oFlux0_GDU2['LABEL']     = 'GDU2'
	oFlux0_GDU2['LINESTYLE'] = 'None'
	oFlux0_GDU2['SYMBOL']    = 3
	oFlux0_GDU2['TITLE']     = 'Flux!CCh' + chan + ' PA0!C(cm$\up-2$s$\up-1$)'
	
	;PHI0 GDU1
	oPhi0_GDU1['LABEL']     = 'GDU1'
	oPhi0_GDU1['LINESTYLE'] = 'None'
	oPhi0_GDU1['SYMBOL']    = 3
	oPhi0_GDU1['TITLE']     = 'Phi!CCh' + chan + ' PA0!C(deg)'
	
	;PHI0 GDU2
	oPhi0_GDU2['COLOR']     = 'Blue'
	oPhi0_GDU2['LABEL']     = 'GDU2'
	oPhi0_GDU2['LINESTYLE'] = 'None'
	oPhi0_GDU2['SYMBOL']    = 3
	oPhi0_GDU2['TITLE']     = 'Phi!CCh' + chan + ' PA0!C(deg)'
	
	;THETA0 GDU1
	oTheta0_GDU1['LABEL']     = 'GDU1'
	oTheta0_GDU1['LINESTYLE'] = 'None'
	oTheta0_GDU1['SYMBOL']    = 3
	oTheta0_GDU1['TITLE']     = 'Theta!CCh' + chan + ' PA0!C(deg)'
	
	;THETA0 GDU2
	oTheta0_GDU2['COLOR']     = 'Blue'
	oTheta0_GDU2['LABEL']     = 'GDU2'
	oTheta0_GDU2['LINESTYLE'] = 'None'
	oTheta0_GDU2['SYMBOL']    = 3
	oTheta0_GDU2['TITLE']     = 'Theta!CCh' + chan + ' PA0!C(deg)'

	;FLUX180 GDU1
	oFlux180_GDU1['LABEL']     = 'GDU1'
	oFlux180_GDU1['LINESTYLE'] = 'None'
	oFlux180_GDU1['SYMBOL']    = 3
	oFlux180_GDU1['TITLE']     = 'Flux!CCh' + chan + ' PA180!C(cm$\up-2$s$\up-1$)'
	
	;COUNTS 180 GDU1
	oCts180_GDU1['LABEL']     = 'GDU1'
	oCts180_GDU1['LINESTYLE'] = 'None'
	oCts180_GDU1['SYMBOL']    = 3
	oCts180_GDU1['TITLE']     = 'Counts!CPA180'
	
	;COUNTS 180 GDU2
	oCts180_GDU2['COLOR']     = 'Blue'
	oCts180_GDU2['LABEL']     = 'GDU2'
	oCts180_GDU2['LINESTYLE'] = 'None'
	oCts180_GDU2['SYMBOL']    = 3
	oCts180_GDU2['TITLE']     = 'Counts!CPA180'
	
	;FLUX180 GDU2
	oFlux180_GDU2['COLOR']     = 'Blue'
	oFlux180_GDU2['LABEL']     = 'GDU2'
	oFlux180_GDU2['LINESTYLE'] = 'None'
	oFlux180_GDU2['SYMBOL']    = 3
	oFlux180_GDU2['TITLE']     = 'Flux!CCh' + chan + ' PA180!C(cm$\up-2$s$\up-1$)'
	
	;PHI180 GDU1
	oPhi180_GDU1['LABEL']     = 'GDU1'
	oPhi180_GDU1['LINESTYLE'] = 'None'
	oPhi180_GDU1['SYMBOL']    = 3
	oPhi180_GDU1['TITLE']     = 'Phi!CCh' + chan + ' PA180!C(deg)'
	
	;PHI180 GDU2
	oPhi180_GDU2['COLOR']     = 'Blue'
	oPhi180_GDU2['LABEL']     = 'GDU2'
	oPhi180_GDU2['LINESTYLE'] = 'None'
	oPhi180_GDU2['SYMBOL']    = 3
	oPhi180_GDU2['TITLE']     = 'Phi!CCh' + chan + ' PA180!C(deg)'
	
	;THETA180 GDU1
	oTheta180_GDU1['LABEL']     = 'GDU1'
	oTheta180_GDU1['LINESTYLE'] = 'None'
	oTheta180_GDU1['SYMBOL']    = 3
	oTheta180_GDU1['TITLE']     = 'Theta!CCh' + chan + ' PA180!C(deg)'
	
	;THETA180 GDU2
	oTheta180_GDU2['COLOR']     = 'Blue'
	oTheta180_GDU2['LABEL']     = 'GDU2'
	oTheta180_GDU2['LINESTYLE'] = 'None'
	oTheta180_GDU2['SYMBOL']    = 3
	oTheta180_GDU2['TITLE']     = 'Theta!CCh' + chan + ' PA180!C(deg)'
	
;-------------------------------------------
; Plot Data ////////////////////////////////
;-------------------------------------------
	;Plot data
	win = MrVar_PlotTS( [ fgm_b_vname, cts0_gdu1_vname, flux0_gdu1_vname, phi0_gdu1_vname, theta0_gdu1_vname, $
	                      cts180_gdu1_vname, flux180_gdu1_vname, phi180_gdu1_vname, theta180_gdu1_vname ], $
	                    /NO_REFRESH, $
	                    XSIZE = 680, $
	                    YSIZE = 700 )

	win = MrVar_OPlotTS( [cts0_gdu1_vname, flux0_gdu1_vname, phi0_gdu1_vname, theta0_gdu1_vname], $
	                     [cts0_gdu2_vname, flux0_gdu2_vname, phi0_gdu2_vname, theta0_gdu2_vname] )
	win = MrVar_OPlotTS( [cts180_gdu1_vname, flux180_gdu1_vname, phi180_gdu1_vname, theta180_gdu1_vname], $
	                     [cts180_gdu2_vname, flux180_gdu2_vname, phi180_gdu2_vname, theta180_gdu2_vname] )

	;Pretty-up the window
	win[0] -> SetLayout, [1,1]
	win    -> TrimLayout
	win    -> SetProperty, OXMARGIN=[13, 8]
	win    -> Refresh

	RETURN, win
END