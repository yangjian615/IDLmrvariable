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
FUNCTION MrMMS_Plot_MVA, var, trange, ttype
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
	
	nPts = oVar -> GetNPts()
	IF N_Elements(trange) GT 0 THEN BEGIN
		IF N_Elements(trange) NE 2 THEN Message, 'TRANGE must have 2 elements.'
		it = oVar -> Value_Locate(var, trange, ttype)
		IF it[1] EQ -1 || it[0] EQ nPts-1 THEN Message, 'No data in time interval.'
		it = it > 0
	ENDIF
	
;-------------------------------------------
; Plot Data ////////////////////////////////
;-------------------------------------------
	cgLoadCT, 13
	
	win = MrWindow( LAYOUT   = [1,5], $
	                OXMARGIN = [10,6], $
	                XGAP     = 0.0, $
	                XSIZE    = 740, $
	                YGAP     = 0.5, $
	                YSIZE    = 775, $
	                REFRESH  = 0 )
	
	layout = MrLayout( [3,5], $
	                   ASPECT   = 1.0, $
	                   OXMARGIN = [10,6], $
	                   OYMARGIN = [4,8], $
	                   XGAP     = 0.0, $
	                   YGAP     = 0.5 )
	
	;XYZ
	p1 = MrVar_Plot( oVec, $
	                 /CURRENT, $
	                 LAYOUT = [1,1] )
	
	;BL-BN
	p3 = MrVar_Plot( oVec_mva[it[0]:it[1], 0], oVec_mva[it[0]:it[1], 2], $
	                 /CURRENT, $
	                 POSITION = layout[*,6] )
	x3a = MrPlotS( oVec_mva['DATA', it[0]:it[1], 0], oVec_mva['DATA', it[0]:it[1], 2], $
	               COLOR  = BytScl(oT['DATA', it[0]:it[1], 'SSM']), $
	               PSYM   = 3, $
	               TARGET = p3 )
	
	;BL-BM
	p4 = MrVar_Plot( oVec_mva[it[0]:it[1], 1], oVec_mva[it[0]:it[1], 2], $
	                 /CURRENT, $
	                 POSITION = layout[*,7] )
	x4 = MrPlotS( oVec_mva['DATA', it[0]:it[1], 1], oVec_mva['DATA', it[0]:it[1], 2], $
	               COLOR  = BytScl(oT['DATA', it[0]:it[1], 'SSM']), $
	               PSYM   = 3, $
	               TARGET = p4 )
	
	;BM-BN
	p5 = MrVar_Plot( oVec_mva[it[0]:it[1], 0], oVec_mva[it[0]:it[1], 1], $
	                 /CURRENT, $
	                 POSITION = layout[*,8] )
	x5 = MrPlotS( oVec_mva['DATA', it[0]:it[1], 0], oVec_mva['DATA', it[0]:it[1], 1], $
	               COLOR  = BytScl(oT['DATA', it[0]:it[1], 'SSM']), $
	               PSYM   = 3, $
	               TARGET = p5 )

;-------------------------------------------
; Annotate /////////////////////////////////
;-------------------------------------------
	tt = oT[ 'DATA', [it[0], it[1]], 'SSM' ]
	t0 = tt[0] - Floor(tt[0])
	t1 = tt[1] - Floor(tt[0])

	;Outline the MVA interval
	ps1 = MrPlotS( [t0, t0], p1.yrange, $
	               COLOR     = 'Blue', $
	               NAME      = 'Line: MVA0', $
	               TARGET    = p1, $
	               LINESTYLE = 2 )
	ps2 = MrPlotS( [t1, t1], p1.yrange, $
	               COLOR     = 'Blue', $
	               NAME      = 'Line: MVA1', $
	               TARGET    = p1, $
	               LINESTYLE = 2 )
	
	;Print eigenvectors and eigenvalues
	eigvec  = String( oEigVec['DATA'], FORMAT='(f7.4)' )
	seigvec = 'N = [' + StrJoin( eigvec[0:2], ', ' ) + ']' + '!C' + $
	          'M = [' + StrJoin( eigvec[3:5], ', ' ) + ']' + '!C' + $
	          'L = [' + StrJoin( eigvec[6:8], ', ' ) + ']'
	
	eigval  = String( oEigVal['DATA'], FORMAT='(f0.3)' )
	seigval = '\lambda = [' + StrJoin( eigval, ', ' ) + ']'
	
	sB     = 'B = [' + StrJoin( String(b_avg, FORMAT='(f0.2)'), ', ' ) + ']'
	sTheta = '\theta_{k} = ' + String(theta, FORMAT='(f0.2)')
	
	txt1 = MrText( 0.50, 0.20, seigval, ALIGNMENT=0.5, NAME='Txt: Eigenvalues',  /NORMAL )
	txt2 = MrText( 0.50, 0.15, seigvec, ALIGNMENT=0.5, NAME='Txt: Eigenvectors', /NORMAL )
	txt3 = MrText( 0.50, 0.25, sB,      ALIGNMENT=0.5, NAME='Txt: B avg', /NORMAL )
	txt3 = MrText( 0.50, 0.05, stheta,  ALIGNMENT=0.5, NAME='Txt: ThetaK', /NORMAL )

;-------------------------------------------
; Prettify /////////////////////////////////
;-------------------------------------------
	p1 -> SetProperty, XTICKFORMAT='(a1)', XTITLE=''
	p3 -> SetProperty, XTITLE=param+'$\downN$', YTITLE=param+'$\downL$'
	p4 -> SetProperty, XTITLE=param+'$\downM$', YTITLE=param+'$\downL$'
	p5 -> SetProperty, XTITLE=param+'$\downN$', YTITLE=param+'$\downM$'
	
	
	win[0] -> SetLayout, [1,1]
;	win    -> TrimLayout
	win -> SetProperty, OXMARGIN=[10,6]
	win -> refresh
	return, win
end