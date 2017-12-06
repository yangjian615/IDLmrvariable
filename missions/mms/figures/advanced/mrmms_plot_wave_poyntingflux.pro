; docformat = 'rst'
;
; NAME:
;       MrMMS_Plot_Wave_PoyntingFlux
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
;       NFFT:       in, optional, type=integer, default=512
;                   Number of points to use per FFT interval.
;       NSHIFT:     in, optional, type=integer, default=`NFFT`/2
;                   Number of points to shift after each FFT.
;       NO_LOAD:    in, optional, type=boolean, default=0
;                   If set, data will not be loaded from source CDF files.
;       OPTDESC:    in, optional, type=string
;                   SCM optional descriptor to use. Choices are: 'scs', 'scf', 'scsrvy',
;                       'scb', 'sch'. The default is to use 'scsrvy' when `MODE`='srvy'
;                       and 'scb' when `MODE`='brst'.
;       OUTPUT_DIR: in, optional, type=string, default='~/figures/'
;                   Directory in which to save the figure. If neither `OUTPUT_DIR` or
;                       `OUTPUT_EXT` are given, no file is made.
;       OUTPUT_EXT: in, optional, type=string/strarr, default='png'
;                   Extension (and file type) of the figure. Options include
;                       'png', 'jpeg', 'tiff', 'ps', 'eps', 'pdf'. If neither
;                       `OUTPUT_DIR` or `OUTPUT_EXT` are given, no file is made.
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
FUNCTION MrMMS_Plot_Wave_PoyntingFlux, sc, mode, nfft, nshift, $
NO_LOAD=no_load, $
OPTDESC=optdesc, $
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
	IF N_Elements(nfft)     EQ 0 THEN nfft     = 512
	IF N_Elements(nshift)   EQ 0 THEN nshift   = nfft / 2
	IF N_Elements(ndetrend) EQ 0 THEN ndetrend = nfft
	IF N_Elements(trange)   GT 0 THEN MrVar_SetTRange, trange
	
;-------------------------------------------
; Variable Names ///////////////////////////
;-------------------------------------------
	level  = 'l2'
	coords = 'gse'
	
	;SCM Parameters
	IF n_elements(optdesc) EQ 0 THEN BEGIN
		CASE mode OF
			'slow': optdesc = 'scs'
			'fast': optdesc = 'scf'
			'srvy': optdesc = 'scsrvy'
			'brst': optdesc = 'scb'       ;also schb
			ELSE: Message, 'Invalid MODE: "' + mode + '".'
		ENDCASE
	ENDIF
	
	;FPI parameters
	fpi_mode = mode EQ 'brst' ? mode : 'fast'

	;Source names
	b_vname    = sc + '_fgm_b_'         + coords + '_'                 + mode + '_' + level
	bvec_vname = sc + '_fgm_bvec_'      + coords + '_'                 + mode + '_' + level
	bmag_vname = sc + '_fgm_bmag_'      + coords + '_'                 + mode + '_' + level
	acb_vname  = sc + '_scm_acb_'       + coords + '_' + optdesc + '_' + mode + '_' + level
	dce_vname  = sc + '_edp_dce_'       + coords + '_'                 + mode + '_' + level
	ni_vname   = StrJoin( [sc, 'dis',     'numberdensity',                 fpi_mode], '_' )
	ne_vname   = StrJoin( [sc, 'des',     'numberdensity',                 fpi_mode], '_' )
	
	;Output names
	db_fac_vname  = StrJoin( [sc, 'scm', 'db',        coords, optdesc, mode, level],          '_' )
	de_fac_vname  = StrJoin( [sc, 'edp', 'de',        coords,          mode, level],          '_' )
	sspec_vname   = StrJoin( [sc, 'scm', 'poyntspec', coords, optdesc, mode, level],          '_' )
	sspec_x_vname = StrJoin( [sc, 'scm', 'poyntspec', coords, optdesc, mode, level, 'r'],     '_' )
	sspec_y_vname = StrJoin( [sc, 'scm', 'poyntspec', coords, optdesc, mode, level, 'theta'], '_' )
	sspec_z_vname = StrJoin( [sc, 'scm', 'poyntspec', coords, optdesc, mode, level, 'phi'],   '_' )
	svec_vname    = StrJoin( [sc, 'scm', 'poyntvec',  coords, optdesc, mode, level],          '_' )
	
	f_ceh_vname = StrJoin( [sc, 'plasma', 'fce', 'half', mode, level], '_' )
	f_ce_vname  = StrJoin( [sc, 'plasma', 'fce',          mode, level], '_' )
	f_pe_vname  = StrJoin( [sc, 'plasma', 'fpe',          mode, level], '_' )
	f_ci_vname  = StrJoin( [sc, 'plasma', 'fci',          mode, level], '_' )
	f_pi_vname  = StrJoin( [sc, 'plasma', 'fpi',          mode, level], '_' )
	f_lh_vname  = StrJoin( [sc, 'plasma', 'flh',          mode, level], '_' )
	
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
		
		;DENSITY
		MrMMS_FPI_Load_Data, sc, mode, $
		                     OPTDESC   = ['des-moms', 'dis-moms'], $
		                     VARFORMAT = '*numberdensity_'+fpi_mode
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
; Frequency Lines //////////////////////////
;-------------------------------------------
	oBmag = MrVar_Get(bmag_vname)
	oNi   = MrVar_Get(ni_vname)
	
	;"Upsample"
	oN_fgm = oNi -> Interpol(oBmag)

	;Calculate key frequencies
	oFce = MrVar_Freq_Cyclotron(oBmag, 'm_e', /CACHE, NAME=f_ce_vname)
	oFci = MrVar_Freq_Cyclotron(oBmag, 'm_p', /CACHE, NAME=f_ci_vname)
	oFpe = MrVar_Freq_Plasma(ne_vname, 'm_e', /CACHE, NAME=f_pe_vname)
	oFpi = MrVar_Freq_Plasma(oN_fgm,   'm_p', /CACHE, NAME=f_pi_vname)
	oFlh = MrVar_Freq_LowerHybrid(oBmag, ne_vname, /CACHE, NAME=f_lh_vname)
	
	;Half cyclotron
	oFce2  = oFce / 2.0
	oFce2 -> SetName, f_ceh_vname
	oFce2 -> Cache
	
	;Determine if the frequenies should be plotted
	f_objs   = []
	oFreq    = (oSspec[0])['DEPEND_1']
	f_range  = [ Min(oFreq['DATA',1:*], MAX=fmax), fmax ]
	IF ~(oFce.min  GT f_range[1] || oFce.max  LT f_range[0]) THEN f_objs = [f_objs, oFce]
	IF ~(oFce2.min GT f_range[1] || oFce2.max LT f_range[0]) THEN f_objs = [f_objs, oFce2]
	IF ~(oFci.min  GT f_range[1] || oFci.max  LT f_range[0]) THEN f_objs = [f_objs, oFci]
	IF ~(oFpe.min  GT f_range[1] || oFpe.max  LT f_range[0]) THEN f_objs = [f_objs, oFpe]
	IF ~(oFpi.min  GT f_range[1] || oFpi.max  LT f_range[0]) THEN f_objs = [f_objs, oFpi]
	IF ~(oFlh.min  GT f_range[1] || oFlh.max  LT f_range[0]) THEN f_objs = [f_objs, oFlh]
	
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
	
	;FCE
	oFce['COLOR']     = 'Black'
	oFce['LABEL']     = 'fce'
	oFce['LINESTYLE'] = '-'
	
	;FCE/2
	oFce2['COLOR']     = 'Black'
	oFce2['LABEL']     = 'fce/2'
	oFce2['LINESTYLE'] = '--'
	
	;FCI
	oFci['COLOR']     = 'White'
	oFci['LABEL']     = 'fci'
	oFci['LINESTYLE'] = '--'
	
	;FPE
	oFpe['COLOR']     = 'Black'
	oFpe['LABEL']     = 'fpe'
	oFpe['LINESTYLE'] = '-:'
	
	;FPI
	oFpi['COLOR']     = 'White'
	oFpi['LABEL']     = 'fpi'
	oFpi['LINESTYLE'] = '-:'
	
	;FLH
	oFlh['COLOR'] = 'Red'
	oFlh['LABEL'] = 'flh'
	
;-------------------------------------------
; Plot Data ////////////////////////////////
;-------------------------------------------
	win = MrVar_PlotTS( [b_vname, db_fac_vname, de_fac_vname, svec_vname, $
	                     sspec_x_vname, sspec_y_vname, sspec_z_vname], $
	                    /NO_REFRESH, $
	                    XSIZE  = 680, $
	                    YSIZE  = 750 )
	
	;Overplot frequencies
	win = MrVar_OPlotTS( sspec_x_vname, f_objs )
	FOR i = 0, N_Elements(f_objs) - 1 DO f_objs[i] -> RemoveAttr, 'LABEL'
	win = MrVar_OPlotTS( sspec_y_vname, f_objs )
	win = MrVar_OPlotTS( sspec_z_vname, f_objs )

	;Set legend properties
	lgds = win -> Get(/ALL, COUNT=nLgds, ISA='MrLegend')
	FOR i = 0, nLgds - 1 DO BEGIN
		IF StRegEx(lgds[i].name, 'f(ce|ci|pe|pi|lh)', /BOOLEAN) THEN BEGIN
			lgds[i] -> SetProperty, ALIGNMENT  ='NE', $
			                        COLOR      = 'Black', $
			                        FILL_COLOR ='Light Grey', $
			                        LINESTYLE  ='-'
		ENDIF
	ENDFOR
	
	;Make Pretty
	win.oxmargin = [12, 15]
	win[0]       -> SetLayout, [1,1]
	win          -> TrimLayout
	win -> Refresh

;-------------------------------------------
; Save Figure //////////////////////////////
;-------------------------------------------
	IF N_Elements(output_dir) GT 0 || N_Elements(output_ext) GT 0 THEN BEGIN
		;Defaults
		IF N_Elements(output_dir) EQ 0 THEN CD, CURRENT=output_dir
		
		;File name
		fname  = StrJoin( [sc, 'mag', mode, level, 'poynting-flux'], '_' )
		fname  = FilePath( fname, ROOT_DIR=output_dir )
		
		;Save the figure
		fout = MrVar_PlotTS_Save( win, fname, output_ext )
	ENDIF

;-------------------------------------------
; Done! ////////////////////////////////////
;-------------------------------------------
	RETURN, win
END