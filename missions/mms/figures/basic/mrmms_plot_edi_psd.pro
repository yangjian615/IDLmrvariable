; docformat = 'rst'
;
; NAME:
;       MrMMS_Plot_CtsXCorr
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
;   Plot the power spectra of B, E, and EDI counts:
;       1. B PSD
;       2. E PSD
;       3. EDI 0-degree Flux PSD
;       4. EDI 180-degree Flux PSD
;       5. EDI Pitch Angle of 0-Degree Channels
;       6. EDI Pitch Angle of 180-Degree Channels
;
; :Categories:
;   MMS, EDI, MrVariable
;
; :Params:
;       SC:         in, required, type=string
;                   MMS spacecraft ID. Options are {'mms1' | 'mms2' | 'mms3' | 'mms4'}
;       MODE:       in, required, type=string
;                   Data rate mode. Options are {'srvy' | 'brst'}
;       OUTPUT_DIR: in, optional, type=string, default=pwd
;                   A directory in which to save the figure. If neither `OUTPUT_DIR`
;                       nor `OUTPUT_EXT` are defined, no file is generated.
;       OUTPUT_EXT: in, optional, type=string, default=pwd
;                   File extensions for the output figure. Options include: 'eps', 'gif',
;                       'jpg', 'ps', 'pdf', 'png', 'tiff'. If neither `OUTPUT_DIR` nor
;                       `OUTPUT_EXT` are defined, no file is generated.
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
;       2016/11/02  -   Written by Matthew Argall
;       2017/06/04  -   Added the OUTPUT_EXT and OUTPUT_DIR keywords. Correlate
;                           counts with component of E or B projected onto the
;                           EDI look direction.
;-
FUNCTION MrMMS_Plot_EDI_PSD, sc, mode, $
NO_LOAD=no_load, $
OUTPUT_DIR=output_dir, $
OUTPUT_EXT=output_ext, $
TRANGE=trange
	Compile_Opt idl2
	
	Catch, the_error
	IF the_error NE 0 THEN BEGIN
		Catch, /CANCEL
		IF N_Elements(win) GT 0 THEN Obj_Destroy, win
		MrPrintF, 'LogErr'
		RETURN, !Null
	endif
	
	tf_load = ~Keyword_Set(no_load)
	IF N_Elements(trange) EQ 0 THEN trange = MrVar_GetTRange()
	
;-------------------------------------------
; Variable Names ///////////////////////////
;-------------------------------------------
	level    = 'l2'
	coords   = 'gse'
	
	;FGM
	fgm_instr = 'fgm'
	CASE fgm_instr OF
		'afg': fgm_level = 'l2pre'
		'dfg': fgm_level = 'l2pre'
		'fgm': fgm_level = 'l2'
		ELSE: Message, 'Invalid FGM instrument "' + fgm_instr + '".'
	ENDCASE

	;EDP
	edp_mode = mode EQ 'srvy' ? 'fast' : mode
	
	;SCM
	CASE mode OF
		'slow': scm_desc = 'scs'
		'fast': scm_desc = 'scf'
		'srvy': scm_desc = 'scsrvy'
		'brst': scm_desc = 'scb'
		ELSE: Message, 'Invalid mode: "' + mode + '".'
	ENDCASE
	
	;EDI
	;   - Which EDI optional descriptor?
	fnames = MrMMS_Get_Filenames(sc, 'edi', mode, level)
	MrMMS_Parse_Filename, fnames, OPTDESC=optdesc
	iUniq = Uniq(optdesc, SORT(opdesc))
	IF N_Elements(iUniq) GT 1 THEN BEGIN
		MrPrintF, 'LogWarn', 'More than one EDI optional descriptor found. Choosing first: "' + optdesc[0] + '".'
		optdesc = optdesc[0]
	ENDIF

	;Source names
	fgm_vname     = StrJoin( [sc, fgm_instr, 'bvec', coords, mode, fgm_level], '_' )
	scm_vname     = StrJoin( [sc, 'scm', 'acb',  coords, scm_desc, mode, level], '_' )
	edp_vname     = StrJoin( [sc, 'edp', 'dce',  coords, mode, level], '_' )
	
	channel        = ['1', '2', '3', '4']
	flux_0_vname   = sc + '_edi_flux' + channel +   '_0_' + mode + '_' + level
	flux_90_vname  = sc + '_edi_flux' + channel +  '_90_' + mode + '_' + level
	flux_180_vname = sc + '_edi_flux' + channel + '_180_' + mode + '_' + level
	traj_0_vname   = sc + '_edi_traj' + channel + '_' + coords +   '_0_' + mode + '_' + level
	traj_90_vname  = sc + '_edi_traj' + channel + '_' + coords +  '_90_' + mode + '_' + level
	traj_180_vname = sc + '_edi_traj' + channel + '_' + coords + '_180_' + mode + '_' + level
	
	;Derived Names
	db_psd_vname      = StrJoin( [sc, 'scm', 'acb', 'fac', 'psd', scm_desc, mode, level], '_' )
	e_psd_vname       = StrJoin( [sc, 'edp', 'dce',  'fac', 'psd', mode, level], '_' )
	pa_0_vname        = sc + '_edi_pa'  + channel +   '_0_' + mode + '_' + level
	pa_180_vname      = sc + '_edi_pa'  + channel + '_180_' + mode + '_' + level
	edi_psd_0_vname   = sc + '_edi_psd' + channel +   '_0_' + mode + '_' + level
	edi_psd_180_vname = sc + '_edi_psd' + channel + '_180_' + mode + '_' + level
	
;-------------------------------------------
; Get Data /////////////////////////////////
;-------------------------------------------
	IF tf_load THEN BEGIN
		;FGM
		MrMMS_FGM_Load_Data, sc, mode, $
		                     INSTR     = fgm_instr, $
		                     VARFORMAT = ['*_b_*' + coords + '*', $
		                                  '*' + fgm_instr + '*' + coords]
	
		;SCM
		MrMMS_Load_Data, sc, 'scm', mode, level, $
		                 OPTDESC   = 'scb', $
		                 VARFORMAT = '*_scb_brst_l2'
	
		;EDP
		MrMMS_Load_Data, sc, 'edp', mode, level, $
		                 OPTDESC   = 'dce', $
		                 VARFORMAT = '*dce_' + coords + '*'
	
		;EDI
		MrMMS_Load_Data, sc, 'edi', mode, level, $
		                 OPTDESC   = ['amb', 'amb-noabs', 'amb-pm2', 'amb-alt-cc', 'amb-alt-oc', $
		                              'amb_alt-ooc', 'amb-alt-oob'], $
		                 VARFORMAT = ['*flux?_0_'+mode+'*', '*flux?_180_'+mode+'*', $
		                              '*counts*', '*traj?_'+coords+'*']
	ENDIF

	;Which FGM naming convenction?
	IF ~MrVar_IsCached(fgm_vname) THEN BEGIN
		fgm_vname = StrJoin( [sc, fgm_instr, 'vec', mode, fgm_level, coords], '_' )
		IF ~MrVar_IsCached(fgm_vname) THEN Message, 'Problem with FGM instrument name.'
	ENDIF

;-------------------------------------------
; Compute B & E PSD in FAC /////////////////
;-------------------------------------------
	oB  = MrVar_Get(fgm_vname)
	odB = MrVar_Get(scm_vname)
	oE  = MrVar_Get(edp_vname)
	
	;Interpolate to same time scale
	oTime  = odB['DEPEND_0']
	oE_scm = oE -> Interpol(oTime)
	
	;Create the coordinate system transformation
	oT = MrVar_Fac(oB, TIME=oTime)
	
	;Transform to FAC
	odB_fac = oT ## odB
	oE_fac  = oT ## oE_scm
	
	;Compute PSD
	odB_PSD = odB_fac -> PSD( /CACHE, $
	                          NAME   = db_psd_vname, $
	                          WINDOW = 'hanning' )
	
	oE_PSD = oE_fac -> PSD( /CACHE, $
	                        NAME   = e_psd_vname, $
	                        WINDOW = 'hanning' )
	
	;Clean up
	Obj_Destroy, [oT, oE_scm, odB_fac, oE_fac]

;-------------------------------------------
; EDI Object ///////////////////////////////
;-------------------------------------------
	oEDI  = MrMMS_EDI_Dist()
	oEDI -> SetData, flux_0_vname,   traj_0_vname,   channel, Replicate(3, 4), Replicate(0, 4)
	oEDI -> SetData, flux_180_vname, traj_180_vname, channel, Replicate(3, 4), Replicate(180, 4)
	
	;Field-Aligned Coordinates
	oEDI -> SetFAC, fgm_vname, !Null, 'CrossX'
	
;-------------------------------------------
; EDI: Trajectories & Fluxes ///////////////
;-------------------------------------------
	colors = MrDefaultColor(NCOLORS=4)
	
	;Get the data
	FOR i = 0, 3 DO BEGIN
		oEDI -> GetData, oFlux_0,   !Null, oPA_0,   CHANNEL=i+1, PA_STATE=0
		oEDI -> GetData, oFlux_180, !Null, oPA_180, CHANNEL=i+1, PA_STATE=180
		
		;Power Spectral Density
		oPSD_0 = oFlux_0 -> PSD( /CACHE, $
		                         NAME   = edi_psd_0_vname[i], $
		                         WINDOW = 'hanning' )
		
		oPSD_180 = oFlux_180 -> PSD( /CACHE, $
		                             NAME   = edi_psd_180_vname[i], $
		                             WINDOW = 'hanning' )
		
		;Set names
		oPA_0   -> SetName, pa_0_vname[i]
		oPA_180 -> SetName, pa_180_vname[i]
		
		;Cache
		oPA_0   -> Cache
		oPA_180 -> Cache
		
		;Attributes
		chan = 'Ch' + String(i+1, FORMAT='(i0)')
		oF_0 = oPSD_0['DEPEND_0']
;		oF_0['AXIS_RANGE']   = 1.0 > f_band < 512.0
;		oF_0['TITLE']        = 'Freq!C' + chan + '!C(Hz)'
		oPSD_0['COLOR']      = colors[i]
		oPSD_0['LABEL']      = chan
;		oPSD_0['PLOT_TITLE'] = ''
;		oPSD_0['TITLE']      = ''
		
		oF_180 = oPSD_180['DEPEND_0']
;		oF_180['AXIS_RANGE']   = 1.0 > f_band < 512.0
;		oF_180['TITLE']        = ''
		oPSD_180['COLOR']      = colors[i]
		oPSD_180['LABEL']      = chan
;		oPSD_180['PLOT_TITLE'] = ''
		
		oPA_0['AXIS_RANGE']   = [0, 45]
		oPA_0['COLOR']        = colors[i]
		oPA_0['LABEL']        = chan
		oPA_0['MINOR']        = 5
		oPA_0['PLOT_TITLE']   = ''
		oPA_0['TICKINTERVAL'] = 15
		oPA_0['TITLE']        = 'Pitch!C(deg)'
		
		oPA_180['AXIS_RANGE']   = [135, 180]
		oPA_180['COLOR']        = colors[i]
		oPA_180['LABEL']        = chan
		oPA_180['MINOR']        = 5
		oPA_180['PLOT_TITLE']   = ''
		oPA_180['TICKINTERVAL'] = 15
		oPA_180['TITLE']        = 'Pitch!C(Deg)'
	ENDFOR
	
	;Clean Up
	Obj_Destroy, oEDI

;-------------------------------------------
; Attributes ///////////////////////////////
;-------------------------------------------
	;DB
	oFreq = odB_PSD['DEPEND_0']
	oFreq['TICKFORMAT']  = '(a1)'
	oFreq['TITLE']       = ''
	odB_PSD['DIMENSION'] = 1
	odB_PSD['LABEL']     = 'B$\down' + ['Perp1', 'Perp2', '||'] + '$'
	odB_PSD['TITLE']     = 'B PSD!CnT$\up2$/Hz'
	
	;E
	oE_PSD['DIMENSION']  = 1
	oE_PSD['LABEL']      = 'E$\down' + ['Perp1', 'Perp2', '||'] + '$'
	oE_PSD['TITLE']      = 'E PSD!C(mV/m)$\up2$/Hz'

	;PSD 0
	oPSD_0 = MrVar_Get(edi_psd_0_vname[0])
	oPSD_0['TITLE']     = 'Flux PSD!CPA0!C(1/cm$\up2$ s)$\up2$/Hz'
	oFreq = oPSD_0['DEPEND_0']
	oFreq['TICKFORMAT'] = '(a1)'
	oFreq['TITLE']      = ''

	;PSD 180
	oPSD_180 = MrVar_Get(edi_psd_180_vname[0])
	oPSD_180['TITLE'] = 'Flux PSD!CPA180!C(1/cm$\up2$ s)$\up2$/Hz'

;-------------------------------------------
; Plot PSD /////////////////////////////////
;-------------------------------------------
	trange = MrVar_GetTRange('SSM')
	nDig   = Ceil(Abs(ALog10(trange[1] - trange[0]))) + 1
	nDig   = nDig GT 0 ? nDig + 1 : nDig
	trange = MrVar_GetTRange()
	dstart = StrMid(trange[0], 0, 10)
	tstart = StrMid(trange[0], 11, 8+nDig)
	tend   = StrMid(trange[1], 11, 8+nDig)
	title  = StrUpCase(sc) + ' ' + dstart + ' ' + tstart + '-' + tend

	win = MrWindow( LAYOUT   = [2,2], $
	                REFRESH  = 0, $
	                OXMARGIN = [10,6], $
	                OYMARGIN = [23, 2], $
	                XSIZE    = 750, $
	                YGAP     = 0.5, $
	                YSIZE    = 700 )
	
	p1 = MrVar_Plot( db_psd_vname,         /CURRENT)
	p3 = MrVar_Plot( edi_psd_0_vname[0],   /CURRENT)
	p2 = MrVar_Plot( e_psd_vname,          /CURRENT)
	p4 = MrVar_Plot( edi_psd_180_vname[0], /CURRENT)
	
	FOR i = 1, 3 DO BEGIN
		op_0   = MrVar_Plot( edi_psd_0_vname[i],   OVERPLOT=p3 )
		op_180 = MrVar_Plot( edi_psd_180_vname[i], OVERPLOT=p4 )
	ENDFOR

;-------------------------------------------
; Time Series //////////////////////////////
;-------------------------------------------
	pos = MrLayout( [1,2], $
	                CHARSIZE = 1.5, $
	                OXMARGIN = [10,6], $
	                OYMARGIN = [4,36], $
	                YGAP     = 0.5, $
	                WDIMS    = [900, 900] )
	
	p5 = MrVar_Plot(pa_180_vname[0], /CURRENT, POSITION=pos[*,0], XTICKFORMAT='(a1)', XTITLE='')
	p6 = MrVar_Plot(pa_0_vname[0],   /CURRENT, POSITION=pos[*,1])
	
	FOR i = 1, 3 DO BEGIN
		op_180 = MrVar_Plot( pa_180_vname[i], OVERPLOT=p5 )
		op_0   = MrVar_Plot( pa_0_vname[i],   OVERPLOT=p6 )
	ENDFOR

;-------------------------------------------
; Make Pretty //////////////////////////////
;-------------------------------------------
	;Title
	oTxt = MrText( 0.5, 0.97, title, $
	               ALIGNMENT = 0.5, $
	               CHARSIZE  = 2, $
	               /CURRENT, $
	               NAME      = 'Txt: Title', $
	               /NORMAL )
	
	
	win['Lgd: ' + db_psd_vname]         -> SetProperty, ALIGNMENT='SW', POSITION=[0,0]
	win['Lgd: ' + e_psd_vname]          -> SetProperty, ALIGNMENT='SW', POSITION=[0,0]
	win['Lgd: ' + edi_psd_0_vname[3]]   -> SetProperty, ALIGNMENT='SW', POSITION=[0,0]
	win['Lgd: ' + edi_psd_180_vname[3]] -> SetProperty, ALIGNMENT='SW', POSITION=[0,0]
	
	win[0] -> SetLayout, [1,1]
	win    -> TrimLayout

	win -> Refresh

;-------------------------------------------
; Save Figure //////////////////////////////
;-------------------------------------------
	IF N_Elements(output_dir) GT 0 || N_Elements(output_ext) GT 0 THEN BEGIN
		;Defaults
		IF N_Elements(output_dir) EQ 0 THEN CD, CURRENT=output_dir
		
		;File name
		fname   = StrJoin( [sc, 'edi', mode, level, StrLowCase(optdesc)+'-psd-pa'], '_' )
		fname   = FilePath( fname, ROOT_DIR=output_dir )
		
		;Save the figure
		fout = MrVar_PlotTS_Save( win, fname, output_ext )
	ENDIF

;-------------------------------------------
; Done! ////////////////////////////////////
;-------------------------------------------
	RETURN, win
END