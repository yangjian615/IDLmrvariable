; docformat = 'rst'
;
; NAME:
;       MrMMS_Plot_EDI_d29_SpecPolQ
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
;       2017/10/17  -   Written by Matthew Argall
;-
FUNCTION MrMMS_Plot_EDI_d29_SpecPolQ, sc, instr, mode, nfft, nshift, $
FGM_INSTR=fgm_instr, $
NDETREND=nDetrend, $
NO_LOAD=no_load, $
OUTPUT_DIR=output_dir, $
OUTPUT_EXT=output_ext, $
P_MIN=p_min, $
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
	edi_instr   = 'edi'
	edi_mode    = 'brst'
	edi_level   = 'l1a'
	edi_optdesc = 'd29'

	;Source names
	flux_gdu1_vname = StrJoin( [sc, 'edi', 'flux',  'gdu1', edi_optdesc, edi_mode, edi_level], '_')
	flux_gdu2_vname = StrJoin( [sc, 'edi', 'flux',  'gdu2', edi_optdesc, edi_mode, edi_level], '_')
	
	;Derived names
	psd_gdu1_vname = StrJoin( [sc, 'edi', 'psd', 'gdu1', edi_optdesc, edi_mode, edi_level], '_')
	psd_gdu2_vname = StrJoin( [sc, 'edi', 'psd', 'gdu2', edi_optdesc, edi_mode, edi_level], '_')
	
;-------------------------------------------
; Get Data /////////////////////////////////
;-------------------------------------------
	;Polarization
	MrMMS_Load_Polarization, sc, instr, mode, nfft, nshift, $
	                         FGM_INSTR = fgm_instr, $
	                         NDETREND  = nDetrend, $
	                         NO_LOAD   = no_load, $
	                         P_MIN     = p_min, $
	                         VARNAMES  = pol_vnames
	
	;Q Measure
	MrMMS_FPI_Q, sc, mode, species, energies, $
	             COORDS    = coords, $
	             FGM_INSTR = fgm_instr, $
	             LEVEL     = level, $
	             NO_LOAD   = no_load, $
	             SUFFIX    = suffix, $
	             TAIL      = tail, $
	             VARNAMES  = q_vnames
	
	IF tf_load THEN BEGIN
		MrMMS_EDI_Load_Efield_Cts, sc, 'brst', 'data29'
	ENDIF

;-------------------------------------------
; Compute PSD //////////////////////////////
;-------------------------------------------
	oFlux_GDU1 = MrVar_Get(flux_gdu1_vname)
	oFlux_GDU2 = MrVar_Get(flux_gdu2_vname)
	oVec       = MrVar_Get(pol_vnames[11])
	
	;Make sure INSTR and EDI have the same time spacing
	dt_edi     = oFlux_GDU1['TIMEVAR'] -> GetSI()
	dt_vec     = oVec['TIMEVAR'] -> GetSI()
	nfft_edi   = Round(nfft * dt_vec / dt_edi)
	nshift_edi = Round(nshift * dt_vec / dt_edi)
	
	oPSD_GDU1 = oFlux_GDU1 -> Spectrogram( nfft_edi, nshift_edi, $
	                                       /CACHE, $
	                                       NAME = psd_gdu1_vname )
	
	oPSD_GDU2 = oFlux_GDU2 -> Spectrogram( nfft_edi, nshift_edi, $
	                                       /CACHE, $
	                                       NAME = psd_gdu2_vname )
	
;-------------------------------------------
; Properties ///////////////////////////////
;-------------------------------------------
	;PSD GDU1
	oPSD_GDU1['TITLE'] = 'PSD!CGDU1!Ccm$\up-4$s$\up-2$Hz$\up-1$'
	
	;PSD GDU2
	oPSD_GDU2['TITLE'] = 'PSD!CGDU2'
	
	;FCE
;	oFce['COLOR'] = 'White'
;	oFce['THICK'] = 2
;	oFce['LABEL'] = 'fce'
	
	;FCE/2
;	oFce2['COLOR']     = 'White'
;	oFce2['LABEL']     = 'fce/2'
;	oFce2['LINESTYLE'] = '--'
;	oFce2['THICK']     = 2
	
;-------------------------------------------
; Plot Data ////////////////////////////////
;-------------------------------------------
	title = StrUpCase( StrJoin([sc, instr, mode], ' ') )
	title += '!CN=' + String(nfft, FORMAT='(i0)') + ' ' + '$\DeltaN$=' + String(nshift, FORMAT='(i0)')


	;Plot data
	win = MrVar_PlotTS( [ pol_vnames[13], psd_gdu1_vname, psd_gdu2_vname, pol_vnames[0:4], q_vnames[0] ], $
	                    /NO_REFRESH, $
	                    XSIZE = 600, $
	                    YSIZE = 700 )

	win = MrVar_OPlotTS( psd_gdu1_vname, pol_vnames[5:6] )
	win = MrVar_OPlotTS( psd_gdu2_vname, pol_vnames[5:6] )
	win = MrVar_OPlotTS( pol_vnames[0:4], pol_vnames[5] )
	win = MrVar_OPlotTS( pol_vnames[0:4], pol_vnames[6] )
	
;	win[19].alignment = 'NE'
;	win[22].alignment = 'NE'
	
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
		
		nstr  = 'n'  + String(nfft,   FORMAT='(i0)')
		dnstr = 'dn' + String(nshift, FORMAT='(i0)')
		
		;File name
		fname = StrJoin( [sc, instr, mode, level, 'd29-spec-pol-q-'+nstr+'-'+dnstr], '_' )
		fname = FilePath( fname, ROOT_DIR=output_dir )
		
		;Save the figure
		fout = MrVar_PlotTS_Save( win, fname, output_ext )
	ENDIF

;-------------------------------------------
; Done! ////////////////////////////////////
;-------------------------------------------
	RETURN, win
END