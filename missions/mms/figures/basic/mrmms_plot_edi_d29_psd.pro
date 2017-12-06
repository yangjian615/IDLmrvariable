; docformat = 'rst'
;
; NAME:
;       MrMMS_Plot_EDI_ElfCts_PSD
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
;   Generate a plot of EDP quantities:
;       1. FGM Bxyz
;       2. EDP Exyz
;       3. GDU Theta
;       4. GDU Phi
;       5. FAC Phi
;       6. FAC Theta
;       7. PAD
;       8. GPD
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
;       2017/01/22  -   Written by Matthew Argall
;-
FUNCTION MrMMS_Plot_EDI_D29_PSD, sc, nfft, nshift, $
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
	IF N_Elements(nshift) EQ 0 THEN nshift = nfft / 2
	IF N_Elements(trange) GT 0 THEN MrVar_SetTRange, trange
	
;-------------------------------------------
; Variable Names ///////////////////////////
;-------------------------------------------
	instr   = 'edi'
	mode    = 'brst'
	level   = 'l1a'
	optdesc = 'd29'

	;Source names
	flux_gdu1_vname = StrJoin( [sc, 'edi', 'flux',  'gdu1', optdesc, mode, level], '_')
	flux_gdu2_vname = StrJoin( [sc, 'edi', 'flux',  'gdu2', optdesc, mode, level], '_')
	q_gdu1_vname    = StrJoin( [sc, 'edi', 'q',     'gdu1',          mode, level], '_')
	q_gdu2_vname    = StrJoin( [sc, 'edi', 'q',     'gdu2',          mode, level], '_')
	dtraj_vname     = StrJoin( [sc, 'edi', 'dlook',         optdesc, mode, level], '_')
	bmag_vname      = StrJoin( [sc, 'fgm', 'bmag',  'gse',           mode, 'l2'], '_')
	
	;Derived names
	q0_gdu1_vname  = StrJoin( [sc, 'edi', 'q0',  'gdu1', optdesc, mode, level], '_')
	q1_gdu1_vname  = StrJoin( [sc, 'edi', 'q1',  'gdu1', optdesc, mode, level], '_')
	q2_gdu1_vname  = StrJoin( [sc, 'edi', 'q2',  'gdu1', optdesc, mode, level], '_')
	q3_gdu1_vname  = StrJoin( [sc, 'edi', 'q3',  'gdu1', optdesc, mode, level], '_')
	q0_gdu2_vname  = StrJoin( [sc, 'edi', 'q0',  'gdu2', optdesc, mode, level], '_')
	q1_gdu2_vname  = StrJoin( [sc, 'edi', 'q1',  'gdu2', optdesc, mode, level], '_')
	q2_gdu2_vname  = StrJoin( [sc, 'edi', 'q2',  'gdu2', optdesc, mode, level], '_')
	q3_gdu2_vname  = StrJoin( [sc, 'edi', 'q3',  'gdu2', optdesc, mode, level], '_')
	j_vname        = StrJoin( [sc, 'edi', 'j',           optdesc, mode, level], '_')
	psd_gdu1_vname = StrJoin( [sc, 'edi', 'psd', 'gdu1', optdesc, mode, level], '_')
	psd_gdu2_vname = StrJoin( [sc, 'edi', 'psd', 'gdu2', optdesc, mode, level], '_')
	fce_vname      = StrJoin( [sc, 'fgm', 'fce',                  mode, 'l2'], '_')
	fce2_vname     = StrJoin( [sc, 'fgm', 'fce', 'half',          mode, 'l2'], '_')

;-------------------------------------------
; Get Data /////////////////////////////////
;-------------------------------------------
	IF tf_load THEN BEGIN
		;FGM
		MrMMS_FGM_Load_Data, sc, 'brst', $
		                     VARFORMAT = '*_b_gse_*'
		
		
		;EDI
		MrMMS_EDI_Load_Efield_Cts, sc, 'brst', 'data29'
	ENDIF

;-------------------------------------------
; Compute Current //////////////////////////
;-------------------------------------------
	oFlux_GDU1 = MrVar_Get(flux_gdu1_vname)
	oFlux_GDU2 = MrVar_Get(flux_gdu2_vname)
	
	;Compute current density
	;   - 1e10 converts to uA/m^2
	;   - +J is current along the incident trajectories of GDU2
	oJ = -1e10 * MrConstants('q') * (oFlux_GDU2 - oFlux_GDU1)
	oJ -> SetName, j_vname
	oJ -> Cache

;-------------------------------------------
; Compute PSD //////////////////////////////
;-------------------------------------------
	oPSD_GDU1 = oFlux_GDU1 -> Spectrogram( nfft, nshift, $
	                                       /CACHE, $
	                                       NAME = psd_gdu1_vname )
	
	oPSD_GDU2 = oFlux_GDU2 -> Spectrogram( nfft, nshift, $
	                                       /CACHE, $
	                                       NAME = psd_gdu2_vname )

;-------------------------------------------
; Split Quality by Flag ////////////////////
;-------------------------------------------
	IF N_Elements(trange) EQ 0 THEN trange = MrVar_GetTRange()

	;Grab the data
	oQ_gdu1 = MrVar_Get(q_gdu1_vname)
	oQ_gdu2 = MrVar_Get(q_gdu2_vname)
	oT_gdu1 = oQ_gdu1['TIMEVAR']
	oT_gdu2 = oQ_gdu2['TIMEVAR']
	
	;
	; GDU1
	;
	iq0 = oQ_gdu1 -> Where(0, /EQUAL, COUNT=nq0)
	iq1 = oQ_gdu1 -> Where(1, /EQUAL, COUNT=nq1)
	iq2 = oQ_gdu1 -> Where(2, /EQUAL, COUNT=nq2)
	iq3 = oQ_gdu1 -> Where(3, /EQUAL, COUNT=nq3)
	
	oQ0_gdu1 = nq0 GT 0 ? oQ_gdu1[iq0] : MrScalarTS( trange, Replicate(!Values.F_NaN, 2) )
	oQ1_gdu1 = nq1 GT 0 ? oQ_gdu1[iq1] : MrScalarTS( trange, Replicate(!Values.F_NaN, 2) )
	oQ2_gdu1 = nq2 GT 0 ? oQ_gdu1[iq2] : MrScalarTS( trange, Replicate(!Values.F_NaN, 2) )
	oQ3_gdu1 = nq3 GT 0 ? oQ_gdu1[iq3] : MrScalarTS( trange, Replicate(!Values.F_NaN, 2) )
	
	oQ0_gdu1 -> SetName, q0_gdu1_vname
	oQ1_gdu1 -> SetName, q1_gdu1_vname
	oQ2_gdu1 -> SetName, q2_gdu1_vname
	oQ3_gdu1 -> SetName, q3_gdu1_vname
	
	oQ0_gdu1 -> Cache
	oQ1_gdu1 -> Cache
	oQ2_gdu1 -> Cache
	oQ3_gdu1 -> Cache

	;
	; GDU2
	;
	iq0 = oQ_gdu2 -> Where(0, /EQUAL, COUNT=nq0)
	iq1 = oQ_gdu2 -> Where(1, /EQUAL, COUNT=nq1)
	iq2 = oQ_gdu2 -> Where(2, /EQUAL, COUNT=nq2)
	iq3 = oQ_gdu2 -> Where(3, /EQUAL, COUNT=nq3)
	
	oQ0_gdu2 = nq0 GT 0 ? oQ_gdu2[iq0] : MrScalarTS( trange, Replicate(!Values.F_NaN, 2) )
	oQ1_gdu2 = nq1 GT 0 ? oQ_gdu2[iq1] : MrScalarTS( trange, Replicate(!Values.F_NaN, 2) )
	oQ2_gdu2 = nq2 GT 0 ? oQ_gdu2[iq2] : MrScalarTS( trange, Replicate(!Values.F_NaN, 2) )
	oQ3_gdu2 = nq3 GT 0 ? oQ_gdu2[iq3] : MrScalarTS( trange, Replicate(!Values.F_NaN, 2) )
	
	oQ0_gdu2 -> SetName, q0_gdu2_vname
	oQ1_gdu2 -> SetName, q1_gdu2_vname
	oQ2_gdu2 -> SetName, q2_gdu2_vname
	oQ3_gdu2 -> SetName, q3_gdu2_vname
	
	oQ0_gdu2 -> Cache
	oQ1_gdu2 -> Cache
	oQ2_gdu2 -> Cache
	oQ3_gdu2 -> Cache

;-------------------------------------------
; Gyrofrequency Lines //////////////////////
;-------------------------------------------
	oFce  = MrVar_Freq_Cyclotron(bmag_vname, 'm_e', /CACHE, NAME=fce_vname)
	oFce2 = MrVar_Freq_Cyclotron(bmag_vname, 'm_e') / 2.0
	oFce2 -> SetName, fce2_vname
	oFce2 -> Cache
	
;-------------------------------------------
; Properties ///////////////////////////////
;-------------------------------------------
	;Flux GDU1
	oFlux_GDU1['AXIS_RANGE'] = [ Min( [oFlux_GDU1.min, oFlux_GDU2.min] ), $
	                             Max( [oFlux_GDU1.max, oFlux_GDU2.max] ) ]
	oFlux_GDU1['PLOT_TITLE'] = StrUpCase( StrJoin( [sc, instr, mode, 'l2'], ' ' ) )
	
	;Flux GDU2
	oFlux_GDU2['AXIS_RANGE'] = oFlux_GDU1['AXIS_RANGE']
	
	;Current
	oJ['TITLE'] = 'J!C($\mu$A/m^2)'
	
	;PSD GDU1
	oPSD_GDU1['TITLE'] = 'PSD!CGDU1!Ccm$\up-4$s$\up-2$Hz$\up-1$'
	
	;PSD GDU2
	oPSD_GDU2['TITLE'] = 'PSD!CGDU2'
	
	;FCE
	oFce['COLOR'] = 'White'
	oFce['THICK'] = 2
	oFce['LABEL'] = 'fce'
	
	;FCE/2
	oFce2['COLOR']     = 'White'
	oFce2['LABEL']     = 'fce/2'
	oFce2['LINESTYLE'] = '--'
	oFce2['THICK']     = 2
	

	;
	; EFIELD - Quality
	;
	
	;GDU1
	oQ0_GDU1['AXIS_RANGE'] = [-1,4]
	oQ0_GDU1['COLOR']      = 'Purple'
	oQ0_GDU1['LABEL']      = 'q0'
	oQ0_GDU1['LINESTYLE']  = 'None'
	oQ0_GDU1['SYMBOL']     = 9
	oQ0_GDU1['TITLE']      = 'Quality'
	
	oQ1_GDU1['AXIS_RANGE'] = [-1,4]
	oQ1_GDU1['COLOR']      = 'Blue'
	oQ1_GDU1['LABEL']      = 'q1'
	oQ1_GDU1['LINESTYLE']  = 'None'
	oQ1_GDU1['SYMBOL']     = 9
	oQ1_GDU1['TITLE']      = 'Quality'

	oQ2_GDU1['AXIS_RANGE'] = [-1,4]
	oQ2_GDU1['COLOR']      = 'Yellow'
	oQ2_GDU1['LABEL']      = 'q2'
	oQ2_GDU1['LINESTYLE']  = 'None'
	oQ2_GDU1['SYMBOL']     = 9
	oQ2_GDU1['TITLE']      = 'Quality'
	
	oQ3_GDU1['AXIS_RANGE'] = [-1,4]
	oQ3_GDU1['COLOR']      = 'Red'
	oQ3_GDU1['LABEL']      = 'q3'
	oQ3_GDU1['LINESTYLE']  = 'None'
	oQ3_GDU1['SYMBOL']     = 9
	oQ3_GDU1['TITLE']      = 'Quality'
	
	;GDU2
	oQ0_GDU2['AXIS_RANGE'] = [-1,4]
	oQ0_GDU2['COLOR']      = 'Purple'
	oQ0_GDU2['LABEL']      = 'q0'
	oQ0_GDU2['LINESTYLE']  = 'None'
	oQ0_GDU2['SYMBOL']     = 9
	oQ0_GDU2['TITLE']      = 'Quality'
	
	oQ1_GDU2['AXIS_RANGE'] = [-1,4]
	oQ1_GDU2['COLOR']      = 'Blue'
	oQ1_GDU2['LABEL']      = 'q1'
	oQ1_GDU2['LINESTYLE']  = 'None'
	oQ1_GDU2['SYMBOL']     = 9
	oQ1_GDU2['TITLE']      = 'Quality'

	oQ2_GDU2['AXIS_RANGE'] = [-1,4]
	oQ2_GDU2['COLOR']      = 'Yellow'
	oQ2_GDU2['LABEL']      = 'q2'
	oQ2_GDU2['LINESTYLE']  = 'None'
	oQ2_GDU2['SYMBOL']     = 9
	oQ2_GDU2['TITLE']      = 'Quality'
	
	oQ3_GDU2['AXIS_RANGE'] = [-1,4]
	oQ3_GDU2['COLOR']      = 'Red'
	oQ3_GDU2['LABEL']      = 'q3'
	oQ3_GDU2['LINESTYLE']  = 'None'
	oQ3_GDU2['SYMBOL']     = 9
	oQ3_GDU2['TITLE']      = 'Quality'
	
;-------------------------------------------
; Plot Data ////////////////////////////////
;-------------------------------------------
	;Plot data
	win = MrVar_PlotTS( [ flux_gdu1_vname, q0_gdu1_vname, flux_gdu2_vname, q0_gdu2_vname, $
	                      dtraj_vname, j_vname, psd_gdu1_vname, psd_gdu2_vname ], $
	                    /NO_REFRESH, $
	                    XSIZE = 600, $
	                    YSIZE = 700 )

	win = MrVar_OPlotTS( q0_gdu1_vname, [q1_gdu1_vname, q2_gdu1_vname, q3_gdu1_vname] )
	win = MrVar_OPlotTS( q0_gdu2_vname, [q1_gdu2_vname, q2_gdu2_vname, q3_gdu2_vname] )
	win = MrVar_OPlotTS( psd_gdu1_vname, [fce_vname, fce2_vname] )
	win = MrVar_OPlotTS( psd_gdu2_vname, [fce_vname, fce2_vname] )
	
	win[19].alignment = 'NE'
	win[22].alignment = 'NE'
	
	;Pretty-up the window
	win[0] -> SetLayout, [1,1]
	win    -> TrimLayout
	win    -> SetProperty, OXMARGIN=[10, 13]
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
		fname = StrJoin( [sc, instr, mode, level, 'data29-psd'], '_' )
		fname = FilePath( fname, ROOT_DIR=output_dir )
		
		;Save the figure
		fout = MrVar_PlotTS_Save( win, fname, output_ext )
	ENDIF

;-------------------------------------------
; Done! ////////////////////////////////////
;-------------------------------------------
	RETURN, win
END