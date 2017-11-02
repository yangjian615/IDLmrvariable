; docformat = 'rst'
;
; NAME:
;       MrMMS_Plot_FGM
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
;       1. |B|
;       2. Bxyz
;       3. Bphi
;       4. Btheta
;       5. Bx PSD + Gyrofrequency lines
;       6. By PSD + Gyrofrequency lines
;       7. Bz PSD + Gyrofrequency lines
;
; :Categories:
;   MMS
;
; :Params:
;       SC:         in, required, type=string
;                   Spacecraft identifier. Options are {'mms1' | 'mms2' | 'mms3' | 'mms4'}
;       MODE:       in, required, type=string
;                   Data rate mode. Options are {'srvy' | 'brst'}
;       NFFT:       in, optional, type=integer, default=2048
;                   Number of points per power spectral density estimate.
;       NSHIFT:     in, optional, type=integer, default=`NFFT`/2
;                   Number of points to shift between PSD estimates.
;
; :Keywords:
;       INSTR:      in, required, type=string, default='fgm'
;                   FGM strument to use. Options are: {'afg' | 'dfg' | 'fgm'}
;       LEVEL:      in, optional, type=string, default='l2'
;                   Data quality level. Options are: {'l1a' | 'l1b' | 'ql' | 'l2pre' | 'l2'}
;       OPTDESC:    in, optional, type=string, default=''
;                   Optional filename descriptor.
;       OUTPUT_DIR: in, optional, type=string, default=pwd
;                   A directory in which to save the figure. If neither `OUTPUT_DIR`
;                       nor `OUTPUT_EXT` are defined, no file is generated.
;       OUTPUT_EXT: in, optional, type=string, default=pwd
;                   File extensions for the output figure. Options include: 'eps', 'gif',
;                       'jpg', 'ps', 'pdf', 'png', 'tiff'. If neither `OUTPUT_DIR` nor
;                       `OUTPUT_EXT` are defined, no file is generated.
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
;       2017/11/01  -   Added the OUTPUT_DIR and OUTPUT_EXT keywords. Added NFFT and NSHIFT
;                           parameters. INSTR is now a keyword. Set y-axis range and tick
;                           interval for angles. - MRA
;-
FUNCTION MrMMS_Plot_FGM, sc, mode, nfft, nshift, $
INSTR=instr, $
LEVEL=level, $
OPTDESC=optdesc, $
OUTPUT_DIR=output_dir, $
OUTPUT_EXT=output_ext, $
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
	IF N_Elements(nfft)   EQ 0 THEN nfft   = 2048
	IF N_Elements(nshift) EQ 0 THEN nshift = nfft/2
	IF N_Elements(instr)  EQ 0 THEN instr  = 'fgm'
	IF N_Elements(trange) GT 0 THEN MrVar_SetTRange, trange
	
;-------------------------------------------
; Variable Names ///////////////////////////
;-------------------------------------------
	IF N_Elements(level) EQ 0 THEN BEGIN
		CASE instr OF
			'afg': level = 'l2pre'
			'dfg': level = 'l2pre'
			'fgm': level = 'l2'
			ELSE: Message, 'Invalid FGM instrument: "' + instr + '".'
		ENDCASE
	ENDIF
	IF N_Elements(coords) EQ 0 THEN BEGIN
		CASE level OF
			'ql': coords = 'dmpa'
			ELSE: coords = 'gse'
		ENDCASE
	ENDIF

	;Source names
	b_vname    = StrJoin( [sc, instr, 'b',    coords, mode, level], '_' )
	bvec_vname = StrJoin( [sc, instr, 'bvec', coords, mode, level], '_' )
	bmag_vname = StrJoin( [sc, instr, 'bmag', coords, mode, level], '_' )
	
	;Output names
	phi_vname   = StrJoin( [sc, instr, 'bphi',   coords, mode, level], '_' )
	theta_vname = StrJoin( [sc, instr, 'btheta', coords, mode, level], '_' )
	bxpsd_vname = StrJoin( [sc, instr, 'bxpsd',  coords, mode, level], '_' )
	bypsd_vname = StrJoin( [sc, instr, 'bypsd',  coords, mode, level], '_' )
	bzpsd_vname = StrJoin( [sc, instr, 'bzpsd',  coords, mode, level], '_' )
	fc_h_vname  = StrJoin( [sc, instr, 'fcH',    coords, mode, level], '_' )
	fc_he_vname = StrJoin( [sc, instr, 'fcHe',   coords, mode, level], '_' )
	fc_o_vname  = StrJoin( [sc, instr, 'fcO',    coords, mode, level], '_' )

;-------------------------------------------
; Get Data /////////////////////////////////
;-------------------------------------------
	IF tf_load THEN BEGIN
		MrMMS_FGM_Load_Data, sc, mode, $
		                     INSTR     = instr, $
		                     LEVEL     = level, $
		                     VARFORMAT = b_vname
	ENDIF

;-------------------------------------------
; Angles ///////////////////////////////////
;-------------------------------------------
	;Compute polar and azimuth angles of B
	oBvec   = MrVar_Get(bvec_vname)
	oBunit  = oBvec -> Normalize()
	Bphi   = ATan( oBunit['DATA',*,1], oBunit['DATA',*,0] ) * !radeg
	Btheta = ATan( oBunit['DATA',*,1], oBunit['DATA',*,2] ) * !radeg
	
	;Store Bphi
	oBphi = MrScalarTS( oBvec['TIMEVAR'], Bphi, /CACHE, NAME=phi_vname, /NO_COPY)
	oBphi -> Cache
	oBphi['AXIS_RANGE']   = [-180, 180]
	oBphi['PLOT_TITLE']   = 'Azimuth angle of the magnetic field.'
	oBphi['TITLE']        = 'B$\phi$!C(deg)'
	oBphi['TICKINTERVAL'] = 90.0
	oBphi['UNITS']        = 'deg'
	
	;Store Btheta
	oBtheta = MrScalarTS( oBvec['TIMEVAR'], Btheta, /CACHE, NAME=theta_vname, /NO_COPY)
	oBtheta -> Cache
	oBtheta['AXIS_RANGE']   = [-180, 180]
	oBtheta['PLOT_TITLE']   = 'Polar angle of the magnetic field.'
	oBtheta['TITLE']        = 'B$\theta$!C(deg)'
	oBtheta['TICKINTERVAL'] = 90.0
	oBtheta['UNITS']        = 'deg'
	
	;Destroy data
	Obj_Destroy, oBunit

;-------------------------------------------
; Power Spectral Density ///////////////////
;-------------------------------------------
	;Split B into components
	oBvec -> Split, oBx, oBy, oBz
	
	;Compute PSD
	oBx_psd = oBx -> Spectrogram(nfft, nshift, NAME=bxpsd_vname, /CACHE, WINDOW='hanning')
	oBy_psd = oBy -> Spectrogram(nfft, nshift, NAME=bypsd_vname, /CACHE, WINDOW='hanning')
	oBz_psd = oBz -> Spectrogram(nfft, nshift, NAME=bzpsd_vname, /CACHE, WINDOW='hanning')

	;Destroy data
	Obj_Destroy, [oBx, oBy, oBz]

;-------------------------------------------
; Gyrofrequency Lines //////////////////////
;-------------------------------------------
	oBmag  = MrVar_Get(bmag_vname)
	ofc_H  = MrVar_Freq_Cyclotron(oBmag, 'm_H',  /CACHE, NAME=fc_h_vname)
	ofc_He = MrVar_Freq_Cyclotron(oBmag, 'm_He', /CACHE, NAME=fc_he_vname)
	ofc_O  = MrVar_Freq_Cyclotron(oBmag, 'm_O',  /CACHE, NAME=fc_o_vname)
	
;-------------------------------------------
; Properties ///////////////////////////////
;-------------------------------------------
	;BMAG
	oBmag['PLOT_TITLE'] = StrUpCase( StrJoin( [sc, instr, mode, level], ' ' ) )
	
	;Bx PSD
	oBx_psd['AXIS_RANGE'] = [1e-6, 1e0]
	oBx_psd['TITLE']      = 'Bx PSD!C(nT$\up2$/Hz)'
	
	;By PSD
	oBy_psd['AXIS_RANGE'] = [1e-6, 1e0]
	oBy_psd['TITLE']      = 'By PSD!C(nT$\up2$/Hz)'
	
	;Bz PSD
	oBz_psd['AXIS_RANGE'] = [1e-6, 1e0]
	oBz_psd['TITLE']      = 'Bz PSD!C(nT$\up2$/Hz)'
	
	
	;FC_H
	ofc_H['COLOR'] = 'Blue'
	ofc_H['NSUM']  = 4
	
	;FC_HE
	ofc_He['COLOR'] = 'Magenta'
	ofc_He['NSUM']  = 4
	
	;FC_HE
	ofc_O['COLOR'] = 'Orange'
	ofc_O['NSUM']  = 4


;-------------------------------------------
; Plot Data ////////////////////////////////
;-------------------------------------------
	;Plot data
	win = MrVar_PlotTS( [bmag_vname, bvec_vname, phi_vname, theta_vname, bxpsd_vname, bypsd_vname, bzpsd_vname], $
	                    /NO_REFRESH, $
	                    XSIZE = 680, $
	                    YSIZE = 700 )
	win = MrVar_OPlotTS( bxpsd_vname, [fc_h_vname, fc_he_vname, fc_o_vname] )
	win = MrVar_OPlotTS( bypsd_vname, [fc_h_vname, fc_he_vname, fc_o_vname] )
	win = MrVar_OPlotTS( bzpsd_vname, [fc_h_vname, fc_he_vname, fc_o_vname] )

	;Pretty-up the window
	win[0] -> SetLayout, [1,1]
	win    -> TrimLayout
	win    -> SetProperty, OXMARGIN=[13, 14]
	win    -> Refresh

;-------------------------------------------
; Save Results /////////////////////////////
;-------------------------------------------
	IF N_Elements(output_dir) GT 0 || N_Elements(output_ext) GT 0 THEN BEGIN
		;Defaults
		IF N_Elements(output_dir) EQ 0 THEN BEGIN
			CD, CURRENT=output_dir
		ENDIF ELSE IF ~File_Test(output_dir, /DIRECTORY) THEN BEGIN
			MrPrintF, 'LogText', 'Creating directory: "' + output_dir + '".'
			File_MKDir, output_dir
		ENDIF
		
		;File name
		fname   = StrJoin( [sc, instr, mode, level], '_' )
		fname   = FilePath( fname, ROOT_DIR=output_dir )
		
		;Save the figure
		fout = MrVar_PlotTS_Save( win, fname, output_ext )
	ENDIF

;-------------------------------------------
; Done! ////////////////////////////////////
;-------------------------------------------

	RETURN, win
END