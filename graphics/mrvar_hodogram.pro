; docformat = 'rst'
;
; NAME:
;       MrVar_Hodogram
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
;   Perform a minimum or maximum variance analysis on the electric or magnetic field.
;   Plot both the original and rotated data as well as hodograms in the variance frame.
;
; :Categories:
;   MMS
;
; :Params:
;       SC:         in, required, type=string
;                   MMS spacecraft ID. Options are {'mms1' | 'mms2' | 'mms3' | 'mms4'}
;       MODE:       in, required, type=string
;                   Data rate mode. Options are {'srvy' | 'brst'}
;       T_MVA:      in, optional, type=strarr(2), default=MrVar_GetTRange()
;                   Date-time string, formatted as YYYY-MM-DDThh:mm:ss.fff indicating the
;                       time interval over which to apply MVA.
;
; :Keywords:
;       COORDS:             in, optional, type=string, default='gse'
;                           Coordinate system of the original data.
;       FGM_INSTR:          in, optional, type=string, default='fgm'
;                           Name of the FGM instrument used as the background magnetic
;                               field when calculating the wave normal angle. Options are
;                               {'afg' | 'dfg' | 'fgm'}. If FGM_INSTR is the same as
;                               `INSTR`, it is ignored.
;       INSTR:              in, optional, type=string, default='fgm'
;                           Name of the instrument on which minimum variance analysis is
;                               performed. Options are: {'afg', 'dfg', 'edp', 'fgm', 'scm'}.
;       LEVEL:              in, optional, type=string, default='l2'
;                           Quality level of data to be loaded.
;       MAXIMUM:            in, optional, type=boolean, default=0
;                           If set, a maximum variance analysis is performed.
;       NO_LOAD:            in, optional, type=boolean, default=0
;                           If set, data is not loaded from source files. Instead, it
;                               is taken from the variable cache.
;       TRANGE:             in, optional, type=strarr(2), default=MrVar_GetTRange()
;                           Date-time string, formatted as YYYY-MM-DDThh:mm:ss.fff indicating
;                               time interval for which data is loaded. If given, the
;                               global analysis interval is changed via MrVar_SetTRange.
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
;       2017/05/04  -   Written by Matthew Argall
;       2017/05/15  -   AFG, DFG, and FGM may be used. Added MAXIMUM keyword. - MRA
;-
FUNCTION MrVar_Hodogram, var, trange, ttype
	Compile_Opt idl2
	
	Catch, the_error
	IF the_error NE 0 THEN BEGIN
		Catch, /CANCEL
		IF N_Elements(win) GT 0 THEN Obj_Destroy, win
		MrPrintF, 'LogErr'
		RETURN, !Null
	ENDIF
	
	oVar = MrVar_Get(var)
	IF ~Obj_IsA(oVar, 'MrVectorTS') THEN Message, 'VAR must be a MrVectorTS object.'
	oT = oVar['TIMEVAR']
	
	nPts = oVar -> GetNPts()
	IF N_Elements(trange) GT 0 THEN BEGIN
		IF N_Elements(trange) NE 2 THEN Message, 'TRANGE must have 2 elements.'
		it = oVar['TIMEVAR'] -> Value_Locate(trange, ttype)
		IF it[1] EQ -1 || it[0] EQ nPts-1 THEN Message, 'No data in time interval.'
		it = it > 0
	ENDIF ELSE BEGIN
		it = [0, nPts - 1]
	ENDELSE
	
;-------------------------------------------
; Remove Labels ////////////////////////////
;-------------------------------------------
	tf_labels = oVar -> HasAttr('LABEL')
	IF tf_labels THEN BEGIN
		labels = oVar['LABEL']
		titles = labels
		IF oVar -> HasAttr('UNITS') THEN titles += '!C(' + oVar['UNITS'] + ')'
	ENDIF ELSE BEGIN
		titles = ['X', 'Y', 'Z']
	ENDELSE
	
;-------------------------------------------
; Plot Data ////////////////////////////////
;-------------------------------------------
	cgLoadCT, 13
	
	win = MrWindow( LAYOUT   = [1,1], $
	                OXMARGIN = [10,6], $
	                OYMARGIN = [12,2], $
	                XGAP     = 0.0, $
	                XSIZE    = 740, $
	                YGAP     = 0.5, $
	                YSIZE    = 500, $
	                REFRESH  = 0 )
	
	layout = MrLayout( [3,1], $
	                   ASPECT   = 1.0, $
	                   CHARSIZE = 1.5, $
	                   OXMARGIN = [10,6], $
	                   OYMARGIN = [4,15], $
	                   XGAP     = 5, $
	                   YGAP     = 0 )
	
	;XYZ
	p1 = MrVar_Plot( oVar, $
	                 /CURRENT, $
	                 LAYOUT = [1,1] )
	
	;Remove labels to prevent legend creation
	IF tf_labels THEN oVar -> RemoveAttr, ['LABEL']
	range = [oVar.min, oVar.max]
	
	;X-Z
	p2 = MrVar_Plot( oVar[it[0]:it[1], 0], oVar[it[0]:it[1], 2], $
	                 /CURRENT, $
	                 POSITION = layout[*,0], $
	                 XRANGE   = range, $
	                 YRANGE   = range )
	x2 = MrPlotS( oVar['DATA', it[0]:it[1], 0], oVar['DATA', it[0]:it[1], 2], $
	              COLOR  = BytScl(oT['DATA', it[0]:it[1], 'SSM']), $
	              PSYM   = 3, $
	              TARGET = p2 )
	
	;Y-Z
	p3 = MrVar_Plot( oVar[it[0]:it[1], 1], oVar[it[0]:it[1], 2], $
	                 /CURRENT, $
	                 POSITION = layout[*,1], $
	                 XRANGE   = range, $
	                 YRANGE   = range )
	x3 = MrPlotS( oVar['DATA', it[0]:it[1], 1], oVar['DATA', it[0]:it[1], 2], $
	               COLOR  = BytScl(oT['DATA', it[0]:it[1], 'SSM']), $
	               PSYM   = 3, $
	               TARGET = p3 )
	
	;X-Y
	p4 = MrVar_Plot( oVar[it[0]:it[1], 0], oVar[it[0]:it[1], 1], $
	                 /CURRENT, $
	                 POSITION = layout[*,2], $
	                 XRANGE   = range, $
	                 YRANGE   = range )
	x4 = MrPlotS( oVar['DATA', it[0]:it[1], 0], oVar['DATA', it[0]:it[1], 1], $
	               COLOR  = BytScl(oT['DATA', it[0]:it[1], 'SSM']), $
	               PSYM   = 3, $
	               TARGET = p4 )
	
	;Put labels back
	IF tf_labels THEN oVar['LABEL'] = labels

;-------------------------------------------
; Annotate /////////////////////////////////
;-------------------------------------------
	tt = oT[ 'DATA', [it[0], it[1]], 'SSM' ]
	t0 = tt[0] - Floor(tt[0])
	t1 = tt[1] - Floor(tt[0])

	;Outline the MVA interval
	ps1 = MrPlotS( [t0, t0], p1.yrange, $
	               COLOR     = 'Blue', $
	               LINESTYLE = 2, $
	               NAME      = 'Line: t0', $
	               TARGET    = p1, $
	               THICK     = 2 )
	               
	ps2 = MrPlotS( [t1, t1], p1.yrange, $
	               COLOR     = 'Blue', $
	               LINESTYLE = 2, $
	               NAME      = 'Line: t1', $
	               TARGET    = p1, $
	               THICK     = 2 )

;-------------------------------------------
; Prettify /////////////////////////////////
;-------------------------------------------
	
	p2 -> SetProperty, XTITLE=labels[0], YTITLE=labels[2]
	p3 -> SetProperty, XTITLE=labels[1], YTITLE=labels[2], YTICKFORMAT='(a1)'
	p4 -> SetProperty, XTITLE=labels[0], YTITLE=labels[1], YTICKFORMAT='(a1)'
	
	
	win[0] -> SetLayout, [1,1]
;	win    -> TrimLayout
	win -> SetProperty, OXMARGIN=[10,6]
	win -> refresh
	return, win
end