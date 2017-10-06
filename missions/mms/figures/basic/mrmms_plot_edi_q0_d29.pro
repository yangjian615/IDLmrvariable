; docformat = 'rst'
;
; NAME:
;       MrMMS_Plot_EDI_q0_d29
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
;   Plot energy dissipation with EDI Data29 counts.
;       1. d29 & q0 flux gdu1
;       2. d29 & q0 flux gdu2
;       3. d29 & q0 traj dbcs gdu1
;       4. d29 & q0 traj dbcs gdu2
;       5. d29 & q0 traj gse gdu1
;       6. d29 & q0 traj gse gdu2
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
;       COORDS:     in, optional, type=string, default='gse'
;                   Coordinate system in which to load the data. Options are: {'dbcs' | 'gse' | 'gsm'}
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
function MrMMS_Plot_EDI_q0_d29, sc, mode, $
NO_LOAD=no_load, $
SUFFIX=suffix, $
TRANGE=trange
	compile_opt idl2
	
	catch, the_error
	if the_error ne 0 then begin
		catch, /CANCEL
		if n_elements(win) gt 0 then obj_destroy, win
		MrPrintF, 'LogErr'
		return, !Null
	endif
	
	instr = 'edi'
	level = 'l1a'
	tf_load = ~keyword_set(no_load)
	IF N_Elements(suffix) EQ 0 THEN suffix = ''
	IF N_Elements(trange) GT 0 THEN MrVar_SetTRange, trange
	
;-------------------------------------------
; Get Data /////////////////////////////////
;-------------------------------------------
	
	IF tf_load THEN BEGIN
		;EDP E-Field
;		MrMMS_EDI_Load_Data, sc, mode, 'q0', $
;		                     VARFORMAT = ['*flux*', '*counts*', '*traj_'+['bcs', 'dbcs', 'gse']+'_gdu?_'+mode+'*']
;		                     SUFFIX = suffix
		
		;DATA29
;		MrMMS_EDI_Load_D29, sc, mode, $
;		                    SUFFIX = suffix

		;Q0
		MrMMS_EDI_Load_EField_Cts, sc, mode, 'q0'
		
		;DATA29
		MrMMS_EDI_Load_EField_Cts, sc, mode, 'data29'
	ENDIF

;-------------------------------------------
; Variable Names ///////////////////////////
;-------------------------------------------
	;Source names
	d29_gdu1_vname           = StrJoin( [sc, instr, 'flux',          'gdu1', 'd29',  mode, 'l1a'], '_' ) + suffix
	d29_gdu2_vname           = StrJoin( [sc, instr, 'flux',          'gdu2', 'd29',  mode, 'l1a'], '_' ) + suffix
	traj_bcs_gdu1_d29_vname  = StrJoin( [sc, instr, 'traj',  'bcs',  'gdu1', 'd29',  mode, 'l1a'], '_' ) + suffix
	traj_bcs_gdu2_d29_vname  = StrJoin( [sc, instr, 'traj',  'bcs',  'gdu2', 'd29',  mode, 'l1a'], '_' ) + suffix
	traj_dbcs_gdu1_d29_vname = StrJoin( [sc, instr, 'traj',  'dbcs', 'gdu1', 'd29',  mode, 'l1a'], '_' ) + suffix
	traj_dbcs_gdu2_d29_vname = StrJoin( [sc, instr, 'traj',  'dbcs', 'gdu2', 'd29',  mode, 'l1a'], '_' ) + suffix
	traj_gse_gdu1_d29_vname  = StrJoin( [sc, instr, 'traj',  'gse',  'gdu1', 'd29',  mode, 'l1a'], '_' ) + suffix
	traj_gse_gdu2_d29_vname  = StrJoin( [sc, instr, 'traj',  'gse',  'gdu2', 'd29',  mode, 'l1a'], '_' ) + suffix
	ang_diff_d29_vname       = StrJoin( [sc, instr, 'dlook',                 'd29',  mode, 'l1a'], '_' ) + suffix
	q_gdu1_vname             = StrJoin( [sc, instr, 'q',                     'gdu1', mode, 'l1a'], '_' ) + suffix
	q_gdu2_vname             = StrJoin( [sc, instr, 'q',                     'gdu2', mode, 'l1a'], '_' ) + suffix
	
	q0_gdu1_vname           = StrJoin( [sc, instr, 'flux',          'gdu1', 'q0', mode, level], '_' ) + suffix
	q0_gdu2_vname           = StrJoin( [sc, instr, 'flux',          'gdu2', 'q0', mode, level], '_' ) + suffix
	traj_bcs_gdu1_q0_vname  = StrJoin( [sc, instr, 'traj',  'bcs',  'gdu1', 'q0', mode, level], '_' ) + suffix
	traj_bcs_gdu2_q0_vname  = StrJoin( [sc, instr, 'traj',  'bcs',  'gdu2', 'q0', mode, level], '_' ) + suffix
	traj_dbcs_gdu1_q0_vname = StrJoin( [sc, instr, 'traj',  'dbcs', 'gdu1', 'q0', mode, level], '_' ) + suffix
	traj_dbcs_gdu2_q0_vname = StrJoin( [sc, instr, 'traj',  'dbcs', 'gdu2', 'q0', mode, level], '_' ) + suffix
	traj_gse_gdu1_q0_vname  = StrJoin( [sc, instr, 'traj',  'gse',  'gdu1', 'q0', mode, level], '_' ) + suffix
	traj_gse_gdu2_q0_vname  = StrJoin( [sc, instr, 'traj',  'gse',  'gdu2', 'q0', mode, level], '_' ) + suffix
	ang_diff_q0_vname       = StrJoin( [sc, instr, 'dlook',                 'q0', mode, level], '_' ) + suffix

;-------------------------------------------
; Set Attributes ///////////////////////////
;-------------------------------------------
	tstart = MrVar_GetTRange()
	tstart = StrMid(tstart[0], 0, 10)
	title  = StrJoin( [StrUpCase(sc), tstart, 'EDI Q0 & D29'], ' ')

	;Flux
	oD29_GDU1 = MrVar_Get(d29_gdu1_vname)
	oD29_GDU2 = MrVar_Get(d29_gdu2_vname)
	oQ0_GDU1  = MrVar_Get(q0_gdu1_vname)
	oQ0_GDU2  = MrVar_Get(q0_gdu2_vname)

	;Data29 GDU1
	oD29_GDU1['AXIS_RANGE'] = [1e4, 1e8]
	oD29_GDU1['LABEL']      = 'D29'
	oD29_GDU1['LOG']        = 1B
	oD29_GDU1['PLOT_TITLE'] = title
	oD29_GDU1['TITLE']      = 'Flux!CGDU1!C(cm^-2 s^-1)'
	oD29_GDU1['UNITS']      = 'cm^-2 s^-1'
	
	;Data29 GDU2
	oD29_GDU2['AXIS_RANGE'] = [1e4, 1e8]
	oD29_GDU2['LABEL']      = 'D29'
	oD29_GDU2['LOG']        = 1B
	oD29_GDU2['TITLE']      = 'Flux!CGDU2!C(cm$\up-2$s$\up-1$)'
	oD29_GDU2['UNITS']      = 'cm^-2 s^-1'
	
	;Q0 GDU1
	oQ0_GDU1['AXIS_RANGE'] = oD29_GDU1['AXIS_RANGE']
	oQ0_GDU1['COLOR']      = 'Blue'
	oQ0_GDU1['LABEL']      = 'Q0'
	oQ0_GDU1['LOG']        = 1B
	oQ0_GDU1['TITLE']      = 'Flux!CGDU1!C(cm^-2 s^-1)'
	oQ0_GDU1['UNITS']      = 'cm^-2 s^-1'
	
	;Q0 GDU2
	oQ0_GDU2['AXIS_RANGE'] = oD29_GDU2['AXIS_RANGE']
	oQ0_GDU2['COLOR']      = 'Blue'
	oQ0_GDU2['LABEL']      = 'Q0'
	oQ0_GDU2['LOG']        = 1B
	oQ0_GDU2['TITLE']      = 'Flux!CGDU2!C(cm$\up-2$s$\up-1$)'
	oQ0_GDU2['UNITS']      = 'cm^-2 s^-1'
	
	;QUALITY GDU1
	oQ_GDU1 = MrVar_Get(q_gdu1_vname)
	oQ_GDU1['AXIS_RANGE'] = [-1,5]
	oQ_GDU1['LABEL']      = 'GDU1'
	oQ_GDU1['TITLE']      = 'Quality'
	
	;QUALITY GDU1
	oQ_GDU2 = MrVar_Get(q_gdu2_vname)
	oQ_GDU1['AXIS_RANGE'] = [-1,5]
	oQ_GDU2['COLOR']      = 'Red'
	oQ_GDU2['LABEL']      = 'GDU2'
	oQ_GDU2['TITLE']      = 'Quality'
	
	;Angular differences
	oDAng_D29 = MrVar_Get(ang_diff_d29_vname)
	oDAng_Q0  = MrVar_Get(ang_diff_q0_vname)
	
	;D29
	oDAng_D29['AXIS_RANGE']   = [0, 180]
	oDAng_D29['LABEL']        = 'D29'
	oDAng_D29['TITLE']        = '$\Delta$look'
	oDAng_D29['TICKINTERVAL'] = 90.0
	
	;Q0
	oDAng_Q0['AXIS_RANGE']   = [0, 180]
	oDAng_Q0['COLOR']        = 'Blue'
	oDAng_Q0['LABEL']        = 'Q0'
	oDAng_Q0['TITLE']        = '$\Delta$look'
	oDAng_Q0['TICKINTERVAL'] = 90.0
	
	
	;Trajectories
	oT1_BCS_Q0   = MrVar_Get(traj_bcs_gdu1_q0_vname)
	oT2_BCS_Q0   = MrVar_Get(traj_bcs_gdu2_q0_vname)
	oT1_DBCS_Q0  = MrVar_Get(traj_dbcs_gdu1_q0_vname)
	oT2_DBCS_Q0  = MrVar_Get(traj_dbcs_gdu2_q0_vname)
	oT1_GSE_Q0   = MrVar_Get(traj_gse_gdu1_q0_vname)
	oT2_GSE_Q0   = MrVar_Get(traj_gse_gdu2_q0_vname)
	oT1_BCS_D29  = MrVar_Get(traj_bcs_gdu1_d29_vname)
	oT2_BCS_D29  = MrVar_Get(traj_bcs_gdu2_d29_vname)
	oT1_DBCS_D29 = MrVar_Get(traj_dbcs_gdu1_d29_vname)
	oT2_DBCS_D29 = MrVar_Get(traj_dbcs_gdu2_d29_vname)
	oT1_GSE_D29  = MrVar_Get(traj_gse_gdu1_d29_vname)
	oT2_GSE_D29  = MrVar_Get(traj_gse_gdu2_d29_vname)

	;D29 GDU1 BCS
	oT1_BCS_D29['AXIS_RANGE']   = [-180, 180]
	oT1_BCS_D29['COLOR']        = ['Blue', 'Red']
	oT1_BCS_D29['LABEL']        = ['$\Phi$$\downD29$', '$\Theta$$\downD29$']
	oT1_BCS_D29['TICKINTERVAL'] = 90
	oT1_BCS_D29['TITLE']        = 'Traj!CBCS!CGDU1!C(deg)'
	
	;D29 GDU2 BCS
	oT2_BCS_D29['AXIS_RANGE']   = [-180, 180]
	oT2_BCS_D29['COLOR']        = ['Blue', 'Red']
	oT2_BCS_D29['LABEL']        = ['$\Phi$$\downD29$', '$\Theta$$\downD29$']
	oT2_BCS_D29['TICKINTERVAL'] = 90
	oT2_BCS_D29['TITLE']        = 'Traj!CBCS!CGDU2!C(deg)'

	;D29 GDU1 DBCS
	oT1_DBCS_D29['AXIS_RANGE']   = [-180, 180]
	oT1_DBCS_D29['COLOR']        = ['Blue', 'Red']
	oT1_DBCS_D29['LABEL']        = ['$\Phi$$\downD29$', '$\Theta$$\downD29$']
	oT1_DBCS_D29['TICKINTERVAL'] = 90
	oT1_DBCS_D29['TITLE']        = 'Traj!CDBCS!CGDU1!C(deg)'
	
	;D29 GDU2 DBCS
	oT2_DBCS_D29['AXIS_RANGE']   = [-180, 180]
	oT2_DBCS_D29['COLOR']        = ['Blue', 'Red']
	oT2_DBCS_D29['LABEL']        = ['$\Phi$$\downD29$', '$\Theta$$\downD29$']
	oT2_DBCS_D29['TICKINTERVAL'] = 90
	oT2_DBCS_D29['TITLE']        = 'Traj!CDBCS!CGDU2!C(deg)'
	
	;D29 GDU1 GSE
	oT1_GSE_D29['AXIS_RANGE']   = [-180, 180]
	oT1_GSE_D29['COLOR']        = ['Blue', 'Red']
	oT1_GSE_D29['LABEL']        = ['$\Phi$$\downD29$', '$\Theta$$\downD29$']
	oT1_GSE_D29['TICKINTERVAL'] = 90
	oT1_GSE_D29['TITLE']        = 'Traj!CGSE!CGDU1!C(deg)'
	
	;D29 GDU2 GSE
	oT2_GSE_D29['AXIS_RANGE']   = [-180, 180]
	oT2_GSE_D29['COLOR']        = ['Blue', 'Red']
	oT2_GSE_D29['LABEL']        = ['$\Phi$$\downD29$', '$\Theta$$\downD29$']
	oT2_GSE_D29['TICKINTERVAL'] = 90
	oT2_GSE_D29['TITLE']        = 'Traj!CGSE!CGDU2!C(deg)'
	
	;Q0 GDU1 BCS
	oT1_BCS_Q0['AXIS_RANGE']   = [0, 180]
	oT1_BCS_Q0['COLOR']        = ['Purple', 'Orange']
	oT1_BCS_Q0['LABEL']        = ['$\Phi$$\downQ0$', '$\Theta$$\downQ0$']
	oT1_BCS_Q0['TICKINTERVAL'] = 90
	oT1_BCS_Q0['TITLE']        = 'Traj!CBCS!CGDU1!C(deg)'
	
	;Q0 GDU2 BCS
	oT2_BCS_Q0['AXIS_RANGE']   = [0, 180]
	oT2_BCS_Q0['COLOR']        = ['Purple', 'Orange']
	oT2_BCS_Q0['LABEL']        = ['$\Phi$$\downQ0$', '$\Theta$$\downQ0$']
	oT2_BCS_Q0['TICKINTERVAL'] = 90
	oT2_BCS_Q0['TITLE']        = 'Traj!CBCS!CGDU2!C(deg)'
	
	;Q0 GDU1 DBCS
	oT1_DBCS_Q0['AXIS_RANGE']   = [0, 180]
	oT1_DBCS_Q0['COLOR']        = ['Purple', 'Orange']
	oT1_DBCS_Q0['LABEL']        = ['$\Phi$$\downQ0$', '$\Theta$$\downQ0$']
	oT1_DBCS_Q0['TICKINTERVAL'] = 90
	oT1_DBCS_Q0['TITLE']        = 'Traj!CDBCS!CGDU1!C(deg)'
	
	;Q0 GDU2 DBCS
	oT2_DBCS_Q0['AXIS_RANGE']   = [0, 180]
	oT2_DBCS_Q0['COLOR']        = ['Purple', 'Orange']
	oT2_DBCS_Q0['LABEL']        = ['$\Phi$$\downQ0$', '$\Theta$$\downQ0$']
	oT2_DBCS_Q0['TICKINTERVAL'] = 90
	oT2_DBCS_Q0['TITLE']        = 'Traj!CDBCS!CGDU2!C(deg)'
	
	;Q0 GDU1 GSE
	oT1_GSE_Q0['AXIS_RANGE']   = [-180, 180]
	oT1_GSE_Q0['COLOR']        = ['Purple', 'Orange']
	oT1_GSE_Q0['LABEL']        = ['$\Phi$$\downQ0$', '$\Theta$$\downQ0$']
	oT1_GSE_Q0['TICKINTERVAL'] = 90
	oT1_GSE_Q0['TITLE']        = 'Traj!CGSE!CGDU1!C(deg)'
	
	;Q0 GDU2 GSE
	oT2_GSE_Q0['AXIS_RANGE']   = [-180, 180]
	oT2_GSE_Q0['COLOR']        = ['Purple', 'Orange']
	oT2_GSE_Q0['LABEL']        = ['$\Phi$$\downQ0$', '$\Theta$$\downQ0$']
	oT2_GSE_Q0['TICKINTERVAL'] = 90
	oT2_GSE_Q0['TITLE']        = 'Traj!CGSE!CGDU2!C(deg)'
	
;-------------------------------------------
; Plot Data ////////////////////////////////
;-------------------------------------------
	;Plot variables
	win = MrVar_PlotTS( [ d29_gdu1_vname, d29_gdu2_vname, q_gdu1_vname, $
	                      ang_diff_d29_vname, $
	                      traj_bcs_gdu1_d29_vname,  traj_bcs_gdu2_d29_vname, $
	                      traj_dbcs_gdu1_d29_vname, traj_dbcs_gdu2_d29_vname, $
	                      traj_gse_gdu1_d29_vname,  traj_gse_gdu2_d29_vname ], $
	                    /NO_REFRESH, $
	                    YSIZE=750 )
	win -> Refresh, /DISABLE
	
	;Overplot
	win = MrVar_OPlotTS( [ d29_gdu1_vname, d29_gdu2_vname, q_gdu1_vname, ang_diff_d29_vname, $
	                       traj_bcs_gdu1_d29_vname,  traj_bcs_gdu2_d29_vname, $
	                       traj_dbcs_gdu1_d29_vname, traj_dbcs_gdu2_d29_vname, $
	                       traj_gse_gdu1_d29_vname,  traj_gse_gdu2_d29_vname ], $
	                     [ q0_gdu1_vname,  q0_gdu2_vname,  q_gdu2_vname, ang_diff_q0_vname, $
	                       traj_bcs_gdu1_q0_vname,  traj_bcs_gdu2_q0_vname, $
	                       traj_dbcs_gdu1_q0_vname, traj_dbcs_gdu2_q0_vname, $
	                       traj_gse_gdu1_q0_vname,  traj_gse_gdu2_q0_vname  ] )
	
	win[0] -> SetLayout, [1,1]
	win    -> TrimLayout
	win.OXMargin = [14,10]
	win -> refresh
	return, win
end