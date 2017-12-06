; docformat = 'rst'
;
; NAME:
;       MrMMS_Plot_EDI_CSD
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
;   Generate a plot of FGM quantities:
;       1. B or E (x,y,z,par,perp,EDI)
;       2. EDI Flux (ch=1,2,3,4; pa=0,90,180)
;       3. PSD of (1)
;       4. PSD of (2)
;       5. CSD of (1) & (2)
;       6. Phase Difference of (1) & (2)
;       7. Coherence of (1) & (2)
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
FUNCTION MrMMS_Plot_EDI_Amb_CSD, sc, instr, mode, pa, chan, $
COORDS=coords, $
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
	IF N_Elements(coords) EQ 0 THEN coords     = 'gse'
	IF N_Elements(pa)     EQ 0 THEN pa         = 0
	IF N_Elements(instr)  EQ 0 THEN instr      = 'scm'
	IF N_Elements(mode)   EQ 0 THEN mode       = 'srvy'
	IF N_Elements(level)  EQ 0 THEN level      = 'l2'
	IF N_Elements(nfft)   EQ 0 THEN nfft       = 512
	IF N_Elements(nshift) EQ 0 THEN nshift     = nfft/2
	IF N_Elements(trange) GT 0 THEN MrVar_SetTRange, trange
	
	;Restrictions
	IF Array_Equal(pa EQ [0, 180], 0) THEN Message, 'PA must be 0 or 180.'
	IF mode EQ 'brst' THEN BEGIN
		IF Array_Equal(chan EQ [1,2,3,4], 0) THEN Message, 'CHAN must be 1, 2, 3, or 4.'
	ENDIF ELSE BEGIN
		IF chan NE 1 THEN Message,'CHAN must be 1 in srvy mode.'
	ENDELSE
	
;-------------------------------------------
; Variable Names ///////////////////////////
;-------------------------------------------
	;EDI
	edi_instr   = 'edi'
	edi_coords  = 'gse'
	edi_optdesc = ['amb', 'amb-pm2']
	
	;FGM
	fgm_instr  = mode   EQ 'brst' ? 'fgm'  : 'dfg'
	fgm_level  = mode   EQ 'brst' ? 'l2'   : 'l2pre'
	fgm_mode   = mode   EQ 'brst' ? mode   : 'srvy'
	fgm_coords = coords EQ 'dbcs' ? 'dmpa' : coords
	
	;SCM
	IF coords NE 'gse' THEN MrPrintF, 'LogWarn', 'SCM has data in GSE only.'
	scm_coords = 'gse' 
	IF N_Elements(scm_optdesc) EQ 0 THEN BEGIN
		CASE mode OF
			'slow': scm_optdesc = 'scs'
			'fast': scm_optdesc = 'scf'
			'srvy': scm_optdesc = 'scsrvy'
			'brst': scm_optdesc = 'scb'
			ELSE: Message, 'Invalid value for MODE: "' + mode + '".'
		ENDCASE
	ENDIF
	
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
	fgm_b_vname    = StrJoin( [sc, fgm_instr, 'b',    fgm_coords, fgm_mode, fgm_level], '_' )
	fgm_bvec_vname = StrJoin( [sc, fgm_instr, 'bvec', fgm_coords, fgm_mode, fgm_level], '_' )
	fgm_bmag_vname = StrJoin( [sc, fgm_instr, 'bmag', fgm_coords, fgm_mode, fgm_level], '_' )
	acb_vname      = StrJoin( [sc, 'scm', 'acb', scm_coords, scm_optdesc, mode, level], '_' )
	dce_vname      = StrJoin( [sc, 'edp', 'dce', edp_coords, edp_mode, level], '_' )
	flux_vname     = StrJoin( [sc, 'edi', 'flux' + StrTrim(chan, 2), StrTrim(pa, 2), mode, level], '_' )
	
	;Derived names
	flux_psd_vname = StrJoin( [sc, 'edi', 'flux' + StrTrim(chan, 2), StrTrim(pa, 2), 'psd', mode, level], '_' )
	acb_psd_vname  = StrJoin( [sc, 'scm', 'psd', 'par', scm_optdesc, mode, level], '_' )
	dce_psd_vname  = StrJoin( [sc, 'edp', 'psd', 'par', edp_mode, level], '_' )
	csd_vname      = StrJoin( [sc, 'edi-edp', 'csd', 'par', edp_mode, level], '_' )
	coince_vname   = StrJoin( [sc, 'edi-edp', 'coincidence', 'par', edp_mode, level], '_' )
	quad_vname     = StrJoin( [sc, 'edi-edp', 'quadrature', 'par', edp_mode, level], '_' )
	phase_vname    = StrJoin( [sc, 'edi-edp', 'phase', 'par', edp_mode, level], '_' )
	coh_vname      = StrJoin( [sc, 'edi-edp', 'coherence', 'par', edp_mode, level], '_' )
	
	;Selection
	vec_vname     = instr EQ 'scm' ? acb_vname     : dce_vname
	vec_psd_vname = instr EQ 'scm' ? acb_psd_vname : dce_psd_vname
	vec_fac_vname = vec_vname + '_fac'

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
		IF instr EQ 'edp' THEN BEGIN
			MrMMS_Load_Data, sc, edp_instr, edp_mode, level, $
			                 OPTDESC   = 'dce', $
			                 VARFORMAT = e_vname
		
		;SCM
		ENDIF ELSE IF instr EQ 'scm' THEN BEGIN
			MrMMS_SCM_Load_Data, sc, mode, $
			                     OPTDESC = scm_optdesc
		
		;INVALID
		ENDIF ELSE BEGIN
			Message, 'Invalid value for INSTR: "' + instr + '".'
		ENDELSE

		;EDI
		MrMMS_EDI_Load_Data, sc, mode, edi_optdesc, $
		                     VARFORMAT=['*flux?_0_'+mode+'*', '*flux?_180_'+mode+'*']
		
		;FPI
;		MrMMS_FPI_Load_Data, sc, fpi_mode, $
;		                     OPTDESC   = fpi_instr + '-moms', $
;		                     VARFORMAT = '*numberdensity_'+fpi_mode
	ENDIF
	
	;Old FGM Conventions
	MrVar_Names, names, fgm_b_vname
	IF names[0] EQ '' THEN BEGIN
		fgm_b_vname     = StrJoin( [sc, fgm_instr,        fgm_mode, fgm_level, fgm_coords], '_' )
		fgm_bvec_vname  = StrJoin( [sc, fgm_instr, 'vec', fgm_mode, fgm_level, fgm_coords], '_' )
		fgm_bmag_vname  = StrJoin( [sc, fgm_instr, 'mag', fgm_mode, fgm_level, fgm_coords], '_' )
		
		;FGM
		IF tf_load THEN BEGIN
			MrMMS_FGM_Load_Data, sc, fgm_mode, $
			                     INSTR     = fgm_instr, $
			                     LEVEL     = fgm_level, $
			                     VARFORMAT = fgm_b_vname
		ENDIF
		
		IF fgm_level NE 'l2' && ~MrVar_IsCached(fgm_b_vname) $
			THEN Message, 'FGM L2PRE variable name incorrect.'
	ENDIF
	
	;Determine which EDI file was loaded
	fnames = MrMMS_Get_FileNames(sc, 'edi', mode, level, OPTDESC=edi_optdesc)
	MrMMS_Parse_Filename, fnames, OPTDESC=optdesc
	iUniq = Uniq(optdesc, Sort(optdesc))
	IF N_Elements(iUniq) NE 1 THEN BEGIN
		MrPrintF, 'LogWarn', 'More than one EDI file type found.'
		MrPrintF, 'LogWarn', '   ' + '[' + StrJoin(optdesc[iUniq], ', ') + ']'
		MrPrintF, 'LogWarn', '   Choosing "' + optdesc[0] + '".'
	ENDIF
	optdesc = optdesc[0]

;-------------------------------------------
; Rotate to MFA ////////////////////////////
;-------------------------------------------
	oB   = MrVar_Get(fgm_bvec_vname)
	oVec = MrVar_Get(vec_vname)

	;Rotate
	oTime    = oVec['TIMEVAR']
	oT       = MrVar_FAC( oB, TIME=oTime )
	oVec_fac = oT ## oVec
	
	;Keep around
	oVec_fac -> SetName, vec_fac_vname
	oVec_fac -> Cache
	
	;Select the parallel component
	oVec_par = oVec_fac[*,2]

;-------------------------------------------
; Equal Time Basis /////////////////////////
;-------------------------------------------
	;Extract EDI data
	oFlux = MrVar_Get(flux_vname)
	oTime = oFlux['DEPEND_0']
	
	;Average samples together
	oVec_edi = MrVar_Time_Avg(oVec_par, oTime)
	
	;Clear data
	Obj_Destroy, oVec_par

;-------------------------------------------
; Power Spectral Density ///////////////////
;-------------------------------------------
	nfft   = 256
	nshift = nfft/2.0
	
	;EDI
	oFlux_PSD = oFlux -> PSD( $;nfft, nshift, $
	                          /CACHE, $
	                          NAME = flux_psd_vname, $
	                          WINDOW = 'Hamming' )
	
	;FIELD
	oVec_PSD  = oVec_edi -> PSD( $;nfft, nshift, $
	                             /CACHE, $
	                             NAME = vec_psd_vname, $
	                             WINDOW = 'Hamming' )

;-------------------------------------------
; Cross-Spectral Density ///////////////////
;-------------------------------------------
	
	;CSD
	oCSD = MrTS_CSD( oFlux, oVec_edi, $;nfft, nshift, $
	                 /CACHE, $
	                 COINCIDENCE = oCoi, $
	                 PHASE       = oPhase, $
	                 QUADRATURE  = oQuad, $
	                 NAME        = csd_vname, $
	                 WINDOW      = 'Hamming' )
	
	;COHERENCY
	oCoh = MrTS_Coherency( oFlux, oVec_edi, nfft, nshift, $
	                       /CACHE, $
	                       NAME   = coh_vname, $
	                       WINDOW = 'Hamming' )
	
	;Absolute (Real) CSD
	oCSD -> SetData, Abs(oCSD['DATA'])
	
	;Rename
	oCoi   -> SetName, coince_vname
	oQuad  -> SetName, quad_vname
	oPhase -> SetName, phase_vname

;-------------------------------------------
; Properties ///////////////////////////////
;-------------------------------------------
	title = StrUpCase(sc)
	
	;FLUX
	oFlux['TITLE']      = 'Flux!CCh' + StrTrim(chan, 2) + ' PA' + StrTrim(pa, 2) + '!C(cm$\up-2$ s$\up-2$)'
	oFlux['PLOT_TITLE'] = title
	
	;FIELD
	oVec_fac['LABEL']      = (instr EQ 'scm' ? 'B' : 'E') + '$\down' + ['Perp1', 'Perp2', '||'] + '$'
	oVec_fac['TITLE']      = instr EQ 'scm' ? 'B!C(nT)' : 'E!C(mV/m)'
	oVec_fac['PLOT_TITLE'] = title

	;FLUX PSD
	oFlux_psd['TITLE'] = 'PSD!C(cm$\up-2$ s$\up-1$)$\up2$/Hz'
	
	;FIELD PSD
	oVec_PSD['TITLE'] = instr EQ 'scm' ? 'PSD!C(nT^2/Hz)' : 'PSD!C(mV/m)^2/Hz'
	
	;CSD
	oCSD['TITLE'] = 'CSD!C(mV/m/cm$\up2$/s/Hz)'
	
;-------------------------------------------
; Plot Data ////////////////////////////////
;-------------------------------------------
	
	win = MrWindow( LAYOUT   = [1,2], $
	                OYMARGIN = [35,2], $
	                REFRESH  = 0, $
	                XGAP     = 10, $
	                XSIZE    = 550, $
	                YGAP     = 0.5, $
	                YSIZE    = 700 )
	
	win = MrVar_PlotTS( [ vec_fac_vname, flux_vname ], $
	                    /CURRENT )
	
	pos = MrLayout( [1,5], $
	                CHARSIZE = 1.5, $
	                OXMARGIN = [12, 8], $
	                OYMARGIN = [4,15], $
	                XGAP     = 10, $
	                YGAP     = 0.5, $
	                WDIMS    = [550,700] )
	
	p3 = MrVar_Plot( vec_psd_vname,  /CURRENT, POSITION=pos[*,0], XTICKFORMAT='(a1)', XTITLE='' )
	p4 = MrVar_Plot( flux_psd_vname, /CURRENT, POSITION=pos[*,1], XTICKFORMAT='(a1)', XTITLE='' )
	p5 = MrVar_Plot( csd_vname,      /CURRENT, POSITION=pos[*,2], XTICKFORMAT='(a1)', XTITLE='' )
	p6 = MrVar_Plot( phase_vname,    /CURRENT, POSITION=pos[*,3], XTICKFORMAT='(a1)', XTITLE='' )
	p7 = MrVar_Plot( coh_vname,      /CURRENT, POSITION=pos[*,4] )
	
	;Pretty-up the window
	win[0] -> SetLayout, [1,1]
	win    -> TrimLayout
	win    -> SetProperty, OXMARGIN=[12, 8]
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
		
		;File name
		chanstr = 'ch' + String(chan, FORMAT='(i0)')
		pastr   = 'pa' + String(pa, FORMAT='(i0)')
		fname   = StrJoin( [sc, 'edi-'+instr, mode, level, 'csd-amb-'+chanstr+'-'+pastr], '_' )
		fname   = FilePath( fname, ROOT_DIR=output_dir )
		
		;Save the figure
		fout = MrVar_PlotTS_Save( win, fname, output_ext )
	ENDIF

;-------------------------------------------
; Done! ////////////////////////////////////
;-------------------------------------------
	RETURN, win
END