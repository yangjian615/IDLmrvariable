; docformat = 'rst'
;
; NAME:
;       MrMMS_Plot_EDI_AmbPSD
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
;       F_BAND:     in, optional, type=fltarr(2)
;                   The frequency range of a pass-band filter to be applied to the data.
;       FGM_INSTR:  in, optional, type=string, default='fgm'
;                   FGM instrument to use. Options are: { 'afg' | 'dfg' | 'fgm' }
;       LEVEL:      in, optional, type=string, default='l2'
;                   Data quality level. Options are: {'l1a' | 'ql' | 'l2'}
;       NO_LOAD:    in, optional, type=boolean, default=0
;                   If set, data will not be loaded from source CDF files.
;       OUTPUT_DIR: in, optional, type=string, default=pwd
;                   A directory in which to save the figure. If neither `OUTPUT_DIR`
;                       nor `OUTPUT_EXT` are defined, no file is generated.
;       OUTPUT_EXT: in, optional, type=string, default=pwd
;                   File extensions for the output figure. Options include: 'eps', 'gif',
;                       'jpg', 'ps', 'pdf', 'png', 'tiff'. If neither `OUTPUT_DIR` nor
;                       `OUTPUT_EXT` are defined, no file is generated.
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
;       2017/05/30  -   Written by Matthew Argall
;-
FUNCTION MrMMS_Plot_EDI_AmbFlux, sc, mode, $
F_BAND=f_band, $
FGM_INSTR=fgm_instr, $
LEVEL=level, $
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
	ENDIF
	
	tf_load = ~Keyword_Set(no_load)
	IF N_Elements(mode)   EQ 0 THEN mode       = 'srvy'
	IF N_Elements(level)  EQ 0 THEN level      = 'l2'
	IF N_Elements(nfft)   EQ 0 THEN nfft       = 512
	IF N_Elements(nshift) EQ 0 THEN nshift     = nfft/2
	IF N_Elements(trange) GT 0 THEN MrVar_SetTRange, trange
	
;-------------------------------------------
; Variable Names ///////////////////////////
;-------------------------------------------
	;EDI
	instr   = 'edi'
	coords  = 'gse'
	optdesc = ['amb', 'amb-pm2']
	
	;FGM
	fgm_instr  = mode EQ 'brst' ? 'fgm' : 'dfg'
	fgm_level  = mode EQ 'brst' ? 'l2'  : 'l2pre'
	fgm_mode   = mode EQ 'brst' ? mode : 'srvy'
	fgm_coords = coords EQ 'dbcs'  ? 'dmpa' : coords
	
	;EDP
	edp_instr  = 'edp'
	edp_mode   = mode EQ 'brst' ? mode : 'fast'
	edp_coords = coords EQ 'dbcs'  ? 'dsl'  : coords
	
	;FPI
	fpi_instr  = 'des'
	fpi_mode   = mode EQ 'brst' ? mode : 'fast'
	fgm_coords = coords

	;Source names
	;   - Different version of L2Pre use different naming conventions
	IF fgm_level EQ 'l2' THEN BEGIN ;fgm_level EQ 'l2pre' ||
		fgm_b_vname     = StrJoin( [sc, fgm_instr, 'b',    fgm_coords, fgm_mode, fgm_level], '_' )
		fgm_bvec_vname  = StrJoin( [sc, fgm_instr, 'bvec', fgm_coords, fgm_mode, fgm_level], '_' )
		fgm_bmag_vname  = StrJoin( [sc, fgm_instr, 'bmag', fgm_coords, fgm_mode, fgm_level], '_' )
	ENDIF ELSE BEGIN
		fgm_b_vname     = StrJoin( [sc, fgm_instr,        fgm_mode, fgm_level, fgm_coords], '_' )
		fgm_bvec_vname  = StrJoin( [sc, fgm_instr, 'vec', fgm_mode, fgm_level, fgm_coords], '_' )
		fgm_bmag_vname  = StrJoin( [sc, fgm_instr, 'mag', fgm_mode, fgm_level, fgm_coords], '_' )
	ENDELSE
	e_vname        = StrJoin( [sc, edp_instr, 'dce', edp_coords, edp_mode, level], '_' )
	
	channels       = ['1', '2', '3', '4']
	flux_0_vname   = sc + '_' + instr + '_' + 'flux' + channels + '_'                + '0'   + '_' + mode + '_' + level
	flux_180_vname = sc + '_' + instr + '_' + 'flux' + channels + '_'                + '180' + '_' + mode + '_' + level
	traj_0_vname   = sc + '_' + instr + '_' + 'traj' + channels + '_' + coords + '_' + '0'   + '_' + mode + '_' + level
	traj_180_vname = sc + '_' + instr + '_' + 'traj' + channels + '_' + coords + '_' + '180' + '_' + mode + '_' + level

	;Derived names
	f_0_vname       = sc + '_' + instr + '_' + 'f'     + channels + '_' +                 '0' + '_' + mode + '_' + level
	f_180_vname     = sc + '_' + instr + '_' + 'f'     + channels + '_' +               '180' + '_' + mode + '_' + level
	phi_0_vname     = sc + '_' + instr + '_' + 'phi'   + channels + '_' + 'fac' + '_' +   '0' + '_' + mode + '_' + level
	theta_0_vname   = sc + '_' + instr + '_' + 'theta' + channels + '_' + 'fac' + '_' +   '0' + '_' + mode + '_' + level
	phi_180_vname   = sc + '_' + instr + '_' + 'phi'   + channels + '_' + 'fac' + '_' + '180' + '_' + mode + '_' + level
	theta_180_vname = sc + '_' + instr + '_' + 'theta' + channels + '_' + 'fac' + '_' + '180' + '_' + mode + '_' + level

;-------------------------------------------
; Get Data /////////////////////////////////
;-------------------------------------------
	IF tf_load THEN BEGIN
		;FGM
		MrMMS_FGM_Load_Data, sc, fgm_mode, $
		                     INSTR     = fgm_instr, $
		                     LEVEL     = fgm_level, $
		                     VARFORMAT = fgm_b_vname
		
		IF fgm_level NE 'l2' && ~MrVar_IsCached(fgm_b_vname) $
			THEN Message, 'FGM L2PRE variable name incorrect. Try swapping naming conventions.'

		;EDP
		MrMMS_Load_Data, sc, edp_instr, edp_mode, level, $
		                 OPTDESC   = 'dce', $
		                 VARFORMAT = e_vname

		;EDI
		oEDI = MrMMS_EDI_Dist(sc, mode, optdesc)
		
		;FPI
		MrMMS_FPI_Load_Data, sc, fpi_mode, $
		                     OPTDESC   = fpi_instr + '-moms', $
		                     VARFORMAT = '*numberdensity_'+fpi_mode
	ENDIF
	
	;Determine which EDI file was loaded
	fnames = MrMMS_Get_FileNames(sc, 'edi', mode, level, OPTDESC=optdesc)
	MrMMS_Parse_Filename, fnames, OPTDESC=optdesc
	iUniq = Uniq(optdesc, Sort(optdesc))
	IF N_Elements(iUniq) NE 1 THEN BEGIN
		MrPrintF, 'LogWarn', 'More than one EDI file type found.'
		MrPrintF, 'LogWarn', '   ' + '[' + StrJoin(optdesc[iUniq], ', ') + ']'
		MrPrintF, 'LogWarn', '   Choosing "' + optdesc[0] + '".'
	ENDIF
	optdesc = optdesc[0]

;-------------------------------------------
; EDI Object ///////////////////////////////
;-------------------------------------------
	IF ~Obj_Valid(oEDI) THEN BEGIN
		oEDI  = MrMMS_EDI_Dist()
		oEDI -> SetData, flux_0_vname,   traj_0_vname,   channels, Replicate(3, 4), Replicate(0, 4)
		oEDI -> SetData, flux_180_vname, traj_180_vname, channels, Replicate(3, 4), Replicate(180, 4)
	ENDIF
	
	;Field-Aligned Coordinates
	oEDI -> SetFAC, fgm_bvec_vname, e_vname, 'EXB'
	
;-------------------------------------------
; EDI: Trajectories & Fluxes ///////////////
;-------------------------------------------
	colors     = MrDefaultColor(NCOLORS=4)
	flux_range = [!values.f_infinity, -!values.f_infinity]
	
	;Get the data
	FOR i = 0, 3 DO BEGIN
		oEDI -> GetData, oFlux_0, !Null, oTheta_0,   CHANNEL=i+1, PA_STATE=0
		oEDI -> GetData, oFlux_180, !Null, oTheta_180, CHANNEL=i+1, PA_STATE=180
		
		;Filter the data
		IF N_Elements(f_band) GT 0 THEN BEGIN
			si = oFlux_0['TIMEVAR'] -> GetSI(RATE=sr)
			fN = sr/2.0
			oFlux_0   = oFlux_0   -> Digital_Filter(f_band[0]/fN, f_band[1]/fN, 50, 512)
			oFlux_180 = oFlux_180 -> Digital_Filter(f_band[0]/fN, f_band[1]/fN, 50, 512)
		ENDIF
		
		;Set names
		oFlux_0    -> SetName, f_0_vname[i]
		oFlux_180  -> SetName, f_180_vname[i]
		oTheta_0   -> SetName, theta_0_vname[i]
		oTheta_180 -> SetName, theta_180_vname[i]
		
		;Cache
		oFlux_0    -> Cache
		oFlux_180  -> Cache
		oTheta_0   -> Cache
		oTheta_180 -> Cache
		
		;Flux range
		i0   = oFlux_0   -> Where(0, /GREATER)
		i180 = oFlux_180 -> Where(0, /GREATER)
		temp = [ Min( [oFlux_0[i0].min, oFlux_180[i180].min] ), $
		         Max( [oFlux_0[i0].max, oFlux_180[i180].max] ) ]
		flux_range[0] <= temp[0]
		flux_range[1] >= temp[1]
		
		;Attributes
		chan = 'Ch' + String(i+1, FORMAT='(i0)')
		oFlux_0['AXIS_RANGE'] = flux_range
		oFlux_0['PLOT_TITLE'] = ''
		oFlux_0['TITLE']      = 'Flux!C' + chan + '!Ccm$\up-2$ s$\up-1$'
		
		oFlux_180['AXIS_RANGE'] = flux_range
		oFlux_180['PLOT_TITLE'] = ''
		oFlux_180['TITLE']      = ''
		
		oTheta_0['AXIS_RANGE']   = [0, 45]
		oTheta_0['COLOR']        = colors[i]
		oTheta_0['LABEL']        = chan
		oTheta_0['MINOR']        = 5
		oTheta_0['PLOT_TITLE']   = ''
		oTheta_0['TICKINTERVAL'] = 15
		oTheta_0['TITLE']        = 'Pitch!C(deg)'
		
		oTheta_180['AXIS_RANGE']   = [135, 180]
		oTheta_180['COLOR']        = colors[i]
		oTheta_180['LABEL']        = chan
		oTheta_180['MINOR']        = 5
		oTheta_180['PLOT_TITLE']   = ''
		oTheta_180['TICKINTERVAL'] = 15
		oTheta_180['TITLE']        = ''
	ENDFOR
	
	;Clean Up
	Obj_Destroy, oEDI

;-------------------------------------------
; Properties ///////////////////////////////
;-------------------------------------------
	
	;PSD 0
	oFlux_0 = MrVar_Get(flux_0_vname[0])
	oFlux_0['PLOT_TITLE'] = 'Parallel'
	
	;PSD 180
	oFlux_180 = MrVar_Get(flux_180_vname[0])
	oFlux_180['PLOT_TITLE'] = 'Anti-Parallel'
	
;-------------------------------------------
; Plot Data ////////////////////////////////
;-------------------------------------------
	trange = MrVar_GetTRange('SSM')
	IF trange[1] - trange[0] LE 60.0 THEN trange -= Floor(trange[0])
	
	win = MrWindow( LAYOUT  = [2,4], $
	                REFRESH = 0, $
	                XGAP    = 10, $
	                XSIZE   = 800, $
	                YGAP    = 0.5, $
	                YSIZE   = 700 )
	
	;Plot PSD and plasma frequencies for each channel
	FOR i = 0, 3 DO BEGIN
		;PSD
		p0   = MrVar_Plot(f_0_vname[i],   /CURRENT, XTICKFORMAT='(a1)', XTITLE='')
		p180 = MrVar_Plot(f_180_vname[i], /CURRENT, XTICKFORMAT='(a1)', XTITLE='')
		
		;On the last iteration, plot pitch angles at bottom
		IF i EQ 3 THEN BEGIN
			p0   = MrVar_Plot(theta_0_vname[0],   /CURRENT)
			p180 = MrVar_Plot(theta_180_vname[0], /CURRENT)
		ENDIF
	ENDFOR
	
	;Overplot the pitch angles from channels 2-4.
	FOR i = 1, 3 DO BEGIN
		pPA = MrVar_Plot(theta_0_vname[i],   OVERPLOT=p0)
		pPA = MrVar_Plot(theta_180_vname[i], OVERPLOT=p180)
	ENDFOR

	;Pretty-up the window
	win[0] -> SetLayout, [1,1]
	win    -> TrimLayout
	win    -> SetProperty, OXMARGIN=[13, 14]
	win    -> SetGlobal, XRANGE=trange
	win    -> Refresh

;-------------------------------------------
; Save Figure //////////////////////////////
;-------------------------------------------
	IF N_Elements(output_dir) GT 0 || N_Elements(output_ext) GT 0 THEN BEGIN
		;Defaults
		IF N_Elements(output_dir) EQ 0 THEN BEGIN
			CD, CURRENT=output_dir
			MrPrintF, 'LogText', 'Saving file to: "' + output_dir + '".'
		ENDIF
		
		;Optional descriptor components
		pastr = 'pa' + String(pa, FORMAT='(i0)')
		IF N_Elements(f_band) GT 0 $
			THEN fstr = '-' + StrJoin(String(f_band, FORMAT='(i0)'), '-') + 'Hz' $
			ELSE fstr = ''
		
		;File name
		fname = StrJoin( [sc, instr, mode, level, 'amb-flux-'+pastr+fstr], '_' )
		fname = FilePath( fname, ROOT_DIR=output_dir )
		
		;Save the figure
		fout = MrVar_PlotTS_Save( win, fname, output_ext )
	ENDIF

;-------------------------------------------
; Done! ////////////////////////////////////
;-------------------------------------------
	RETURN, win
END