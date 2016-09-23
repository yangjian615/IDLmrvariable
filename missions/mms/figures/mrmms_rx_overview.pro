; docformat = 'rst'
;
; NAME:
;       MrMMS_Rx_Overview
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
;       2. E
;       3. ni & ne
;       4. Vi
;       5. E spectra (e)
;       6. E spectra (i)
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
;       2016/09/21  -   Written by Matthew Argall
;-
function MrMMS_Rx_Overview, sc, mode, $
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
; Get Data /////////////////////////////////
;-------------------------------------------
	level    = 'l2'
	edp_mode = mode eq 'srvy' ? 'fast' : mode
	fgm_mode = mode
	fpi_mode = mode eq 'brst' ? mode : 'fast'
	
	coords = 'gse'
	if coords eq 'gse' then begin
		MrPrintF, 'LogWarn', 'FPI does not have data in GSE yet. Changing to DBCS.'
		fpi_coords = 'dbcs'
	endif
	
	;FGM
	if tf_load then begin
		MrMMS_FGM_Load_Data, sc, fgm_mode, $
		                     VARFORMAT = '*b_' + coords + '*'
		
		;EDP E-Field
		MrMMS_Load_Data, sc, 'edp', edp_mode, level, $
		                 OPTDESC   = 'dce'
		                 VARFORMAT = '*dce_' + coords + '*'
	
		;DES
		MrMMS_FPI_Load_Data, sc, fpi_mode, $
		                     OPTDESC   = 'des-moms', $
		                     VARFORMAT = ['*energyspectr*', $
		                                  '*density_' + fpi_coords + '*']
	
		;DIS
		MrMMS_FPI_Load_Data, sc, fpi_mode, $
		                     OPTDESC   = 'dis-moms', $
		                     VARFORMAT = ['*energyspectr*', $
		                                  '*density_' + fpi_coords + '*', $
		                                  '*bulk?_'   + fpi_coords + '*']
	endif

;-------------------------------------------
; Variable Names ///////////////////////////
;-------------------------------------------
	;Source names
	B_vname  = sc + '_fgm_bvec_'          +     coords + '_' + fgm_mode + '_' + level
	E_vname  = sc + '_edp_dce_'           +     coords + '_' + edp_mode + '_' + level
	ni_vname = sc + '_dis_numberdensity_' + fpi_coords + '_' + fpi_mode
	ne_vname = sc + '_des_numberdensity_' + fpi_coords + '_' + fpi_mode
	vi_vname = sc + '_dis_bulkv_'         + fpi_coords + '_' + fpi_mode
	Ee_vname = sc + '_des_energyspectr_'                     + fpi_mode
	Ei_vname = sc + '_dis_energyspectr_'                     + fpi_mode

;-------------------------------------------
; Plot Data ////////////////////////////////
;-------------------------------------------
	win = MrVar_PlotTS( [B_vname, E_vname, ni_vname, vi_vname, Ee_vname, Ei_vname], $
	                    YSIZE = 700)

	return, win
end