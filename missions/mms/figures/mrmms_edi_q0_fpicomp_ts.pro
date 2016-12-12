; docformat = 'rst'
;
; NAME:
;       MrMMS_EDI_FPI_TS
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
;   Generate a plot to provide an overview of reconnection quantities:
;       1. B
;       2. Tpar, Tperp
;       3. EF for 66, 230, 485, and 1023 keV
;       4. Q0 counts from GDU, GDU2, (and their difference)
;
; :Categories:
;   MMS, EDI, MrVariable
;
; :Params:
;       SC:         in, required, type=string
;                   MMS spacecraft ID. Options are {'mms1' | 'mms2' | 'mms3' | 'mms4'}
;       MODE:       in, required, type=string
;                   Data rate mode. Options are {'srvy' | 'brst'}
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
;       2016/11/08  -   Written by Matthew Argall
;-
function MrMMS_EDI_Q0_FPIComp_TS, sc, mode, $
NO_LOAD=no_load, $
TRANGE=trange
	compile_opt idl2
	
	catch, the_error
	if the_error ne 0 then begin
		catch, /CANCEL
		if n_elements(win) gt 0 then obj_destroy, win
		MrPrintF, 'LogErr'
		return, !Null
	endif
	
	tf_load = ~keyword_set(no_load)
	if n_elements(trange) gt 0 then MrVar_SetTRange, trange
	
;-------------------------------------------
; Variable Names ///////////////////////////
;-------------------------------------------
	level    = 'l2'
	edp_mode = mode eq 'srvy' ? 'fast' : mode
	fgm_mode = mode
	dsp_mode = 'fast'
	fpi_mode = mode eq 'brst' ? mode : 'fast'
	coords   = 'gse'

	;Source names
	fgm_vname       = sc + '_fgm_bvec_'      + coords  + '_' + fgm_mode + '_' + level
	q0_gdu1_vname   = sc + '_edi_counts_gdu2_'               + mode     + '_' + level
	q0_gdu2_vname   = sc + '_edi_counts_gdu1_'               + mode     + '_' + level
	q0_diff_vname   = sc + '_edi_counts_diff_'               + mode     + '_' + level
	fpi_tpara_vname = sc + '_des_temppara_'                  + fpi_mode
	fpi_tperp_vname = sc + '_des_tempperp_'                  + fpi_mode
	fpi_espec_vname = sc + '_des_espec_'                     + fpi_mode + '_' + level
	fpi_4chan_vname = sc + '_des_espec_4chan_'               + fpi_mode + '_' + level
	
	dphi_edi = mode eq 'srvy' ? 22.5 : 11.25
	dt_edi   = mode eq 'srvy' ? 2.5  : 1.0
	
;-------------------------------------------
; Get Data /////////////////////////////////
;-------------------------------------------
	if tf_load then begin
	;-------------------------------------------
	; FGM //////////////////////////////////////
	;-------------------------------------------
		MrMMS_FGM_Load_Data, sc, fgm_mode, $
		                     VARFORMAT = '*b_' + coords + '*'
	
	;-------------------------------------------
	; FPI Moments //////////////////////////////
	;-------------------------------------------
		MrMMS_Load_Data, sc, 'fpi', fpi_mode, level, $
		                 OPTDESC   = 'des-moms', $
		                 VARFORMAT = '*temp*'
	
	;-------------------------------------------
	; EDI //////////////////////////////////////
	;-------------------------------------------
		MrMMS_Load_Data, sc, 'edi', mode, level, $
		                 OPTDESC   = 'q0', $
		                 VARFORMAT = '*counts*'
	
	;-------------------------------------------
	; FPI Distribution /////////////////////////
	;-------------------------------------------
		fac_type = mode eq 'brst' ? 'VxB'  : 'ExB'
		
		;Prepare the data
		oFPI     = MrMMS_FPI_Dist( sc, fpi_mode, 'e' )
		oFPI    -> Load_FAC, fac_type
		
		;Create the spectrogram
		oDist    = oFPI -> GetDist4D()
		oDist   -> ConvertUnits, 'DIFF FLUX'
		oESpec   = oDist -> GetESpec( NAME=fpi_espec_vname, THETA_RANGE=[-80,100], /CACHE)
	endif

;-------------------------------------------
; Temperatures /////////////////////////////
;-------------------------------------------
	oTpara = MrVar_Get(fpi_tpara_vname)
	oTperp = MrVar_Get(fpi_tperp_vname)

	oTpara -> AddAttr, 'COLOR', 'Black'
	oTpara -> AddAttr, 'LABEL', 'Tpara'
	
	oTperp -> AddAttr, 'COLOR', 'Blue'
	oTperp -> AddAttr, 'LABEL', 'Tperp'

;-------------------------------------------
; Energy Channels //////////////////////////
;-------------------------------------------
	oESpec = MrVar_Get(fpi_espec_vname)

	;Extract 4 channels
	oEnergy = oESpec['DEPEND_1']
	!Null   = min( abs(oEnergy[0,*] -   66),   i66 )
	!Null   = min( abs(oEnergy[0,*] -  250),  i250 )
	!Null   = min( abs(oEnergy[0,*] -  500),  i500 )
	!Null   = min( abs(oEnergy[0,*] - 1000), i1000 )

	;Create time series objects
	oTS = MrTimeSeries( oEnergy['TIMEVAR'], oESpec[*, [i66, i250, i500, i1000]], /CACHE, $
	                    NAME = fpi_4chan_vname )
	oTS -> AddAttr, 'DIMENSION', 1
	oTS -> AddAttr, 'LABEL',     ['66eV', '250eV', '500eV', '1000eV']
	oTS -> AddAttr, 'LOG',       1B
	oTS -> AddAttr, 'TITLE',     'Flux'
	oTS -> AddAttr, 'UNITS',     oESpec['UNITS']

;-------------------------------------------
; Q0 Difference ////////////////////////////
;-------------------------------------------
	;Get counts from both guns
	q0_gdu1 = MrVar_Get( q0_gdu1_vname )
	q0_gdu2 = MrVar_Get( q0_gdu2_vname )

	;Take the difference
	t_all  = [ q0_gdu1['TIME', 'TT2000'], q0_gdu2['TIME', 'TT2000'] ]
	oT_all = MrTimeVar( t_all[ uniq(t_all, sort(t_all)) ], 'TT2000' )
	q0_temp1 = q0_gdu1 -> Interpol(oT_all)
	q0_temp2 = q0_gdu2 -> Interpol(oT_all)
	q0_diff  = q0_temp1 - q0_temp2
	oQ0diff  = MrTimeSeries( oT_all, abs(q0_diff['DATA']), /NO_COPY )
	
	;Set attributes
	oQ0diff -> SetName, q0_diff_vname
	oQ0diff -> AddAttr, 'COLOR',      'Grey'
	oQ0diff -> AddAttr, 'LABEL',      'GDU1-GDU2'
	oQ0diff -> AddAttr, 'PLOT_TITLE', 'Difference in Quality 0 counts between GDU1 and GDU2'
	oQ0diff -> AddAttr, 'TITLE',      'Q0'
	oQ0diff -> AddAttr, 'UNITS',      'Counts'
	oQ0diff -> Cache
	
	q0_gdu1 -> AddAttr, 'LABEL', 'GDU1'
	
	q0_gdu2 -> AddAttr, 'COLOR', 'Blue'
	q0_gdu2 -> AddAttr, 'LABEL', 'GDU2'

;-------------------------------------------
; Plot /////////////////////////////////////
;-------------------------------------------
	win = MrVar_PlotTS( [fgm_vname, fpi_tpara_vname, fpi_4chan_vname, q0_gdu1_vname], $
	                    XSIZE = 750, $
	                    YSIZE = 850 )
	win -> Refresh, /DISABLE

	p1 = MrVar_Plot( fpi_tperp_vname, $
	                 OVERPLOT = win[fpi_tpara_vname] )
	p2 = MrVar_Plot( q0_gdu2_vname, $
	                 OVERPLOT = win[q0_gdu1_vname] )
	p3 = MrVar_Plot( q0_diff_vname, $
	                 OVERPLOT = win[q0_gdu1_vname] )
	
	win -> Refresh
	return, win
end