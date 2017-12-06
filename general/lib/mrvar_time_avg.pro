; docformat = 'rst'
;
; NAME:
;       MrVar_Time_Avg
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
;   Time average one variable onto the time stamps of another.
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
;       2017/08/04  -   Written by Matthew Argall
;-
;*****************************************************************************************
;+
;   Test MrVar_Time_Avg using artificial data.
;
; :Returns:
;       WIN:        out, required, type=objref
;                   A MrWindow object reference containing a visualization of the results.
;-
FUNCTION MrVar_Time_Avg_Test
	Compile_Opt idl2
	On_Error, 2
	
	;Reference time
	t_ref  = '2017-08-03T15:18:33'
	length = 10           ; s
	f1     = 1.0          ; Hz
	f2     = 3.0          ; Hz
	
;-------------------------------------------
; Create Two Signals ///////////////////////
;-------------------------------------------
	
	;SIGNAL 1
	fs1     = 5.0           ; S/s
	dt1     = 1.0 / fs1     ; s
	N1      = length * fs1  ; Samples
	t1      = dt1 * FIndGen(N1)
	s1      = Sin( 2 * !pi * f1 * t1 ) + RandomU(5, N1) - 0.5

	oT1 = MrTimeVar( t1, 'SSM', $
	                 T_REF = t_ref )
	oT1['UNITS']       = 's'
	oT1['DELTA_MINUS'] = 0.5 * dt1
	oT1['DELTA_PLUS']  = 0.5 * dt1
	
	oS1 = MrTimeSeries( oT1, s1 )
	
	;SIGNAL 2
	fs2     = 20.0          ; S/s
	dt2     = 1.0 / fs2     ; s
	N2      = length * fs2  ; Samples
	t2      = dt2 * FIndGen(N2)
	s2      = Sin( 2 * !pi * f2 * t2 ) + RandomU(5, N2) - 0.5

	oT2 = MrTimeVar( t2, 'SSM', $
	                 T_REF = t_ref )
	oT2['UNITS']       = 's'
	oT2['DELTA_MINUS'] = 0.5 * dt2
	oT2['DELTA_PLUS']  = 0.5 * dt2
	
	oS2 = MrTimeSeries( oT2, s2 )
	
;-------------------------------------------
; Plot Results /////////////////////////////
;-------------------------------------------
	;Time-Average Signal 2
	oS2_Avg = MrVar_Time_Avg(oS2, oS1)
	
;-------------------------------------------
; Attributes ///////////////////////////////
;-------------------------------------------
	;SIGNAL 1
	oS1['AXIS_RANGE'] = [-1.5, 1.5]
	oS1['PLOT_TITLE'] = 'Targeted Signal'
	oS1['SYMBOL']     = 6
	oS1['TITLE']      = 'S1'
	
	;SIGNAL 2
	oS2['AXIS_RANGE'] = [-1.5, 1.5]
	oS2['PLOT_TITLE'] = 'Source Signal'
	oS2['LABEL'] = 'Orignal'
	oS2['TITLE'] = 'S2'
	
	;RESULTS
	oS2_Avg['COLOR']     = 'Red'
	oS2_Avg['LABEL']     = 'Avg'
	oS2_Avg['LINESTYLE'] = 'None'
	oS2_Avg['SYMBOL']    = 7
	
;-------------------------------------------
; Plot Results /////////////////////////////
;-------------------------------------------
	;Create a window in which to display the graphics
	win = MrWindow( OXMARGIN = [10, 8], $
	                REFRESH=0 )
	
	;Plot the results
	p1 = MrVar_Plot( oS1, /CURRENT )
	p2 = MrVar_Plot( oS2, /CURRENT )
	p3 = MrVar_Plot( oS2_Avg, OVERPLOT=p2 )
	
	;Finish
	win -> SetGlobal, XRANGE=[0,10], XTICKINTERVAL=1
	win -> Refresh
	RETURN, win
END


;+
;   Time average one variable onto the time stamps of another.
;
; :Params:
;       VAR:            in, required, type=string/integer/objref
;                       Name, number, or MrTimeSeries variable to be averaged.
;       TIME:           in, required, type=string/integer/objref
;                       Name, number, MrTimeSeries or MrTimeVar objref of the variable
;                           containing the target time tags.
;
; :Keywords:
;       CACHE:          in, optional, type=boolean, default=0
;                       If set, the output will be added to the variable cache.
;       NAN:            in, optional, type=boolean, default=0
;                       If set, NaNs will be ignored during the averaging process.
;       NAME:           in, optional, type=string, default='Cyclotron_Frequency'
;                       Name to be given to the output variable.
;       NO_CLOBBER:     in, optional, type=boolean, default=0
;                       If set, cached variables with name `NAME` are not over-written.
;                           Instead, the output variable will have "_#" appended, were "#"
;                           represents a unique number.
;       TEST:           in, optional, type=boolean, default=0
;                       If set, run MrVar_Time_Avg with a set of synthetic data and return
;                           a MrGraphics window visualizing the result.
;
; :Returns:
;       OV_AVG:         out, required, type=objref
;                       A MrTimeSeries objref containing the averaged data.
;
FUNCTION MrVar_Time_Avg, var, time, $
CACHE=cache, $
NAN=nan, $
NAME=name, $
NO_CLOBBER=no_clobber, $
TEST=test
	Compile_Opt idl2
	On_Error, 2
	
	IF Keyword_Set(test) THEN BEGIN
		win = MrVar_Time_Avg_Test()
		RETURN, win
	ENDIF
	
	IF N_Elements(t_type) EQ 0 THEN t_type = 'TT2000'
	
	;Grab the variables
	oV = MrVar_Get(var)
	oT = MrVar_Get(time)
	IF ~Obj_IsA(oV, 'MrTimeSeries') THEN Message, 'VAR must be a MrTimeSeries variable.'
	
	;Time
	IF ~Obj_IsA(oT, 'MrTimeVar') THEN BEGIN
		IF Obj_IsA(oT, 'MrTimeSeries') $
			THEN oT = oT['TIMEVAR'] $
			ELSE Message, 'TIME must be a MrTimeVar or MrTimeSeries variable.'
	ENDIF
	
;-------------------------------------------
; Lower-Bound of Time Tag //////////////////
;-------------------------------------------
	t_ref        = oT['DATA', 0]
	t_ssm_target = oT['DATA', 'SSM']
	
	;DELTA_MINUS_VAR
	IF oT -> HasAttr('DELTA_MINUS_VAR') THEN BEGIN
		oDT = oT['DELTA_MINUS_VAR']
		
		;Units
		IF oDT -> HasAttr('UNITS') THEN BEGIN
			oldUnit = IDLunit(oDT['UNITS'])
			newUnit = oldUnit -> To('s')
			dt_si   = newUnit.quantity
		
		;SI-Conversion
		ENDIF ELSE IF oDT -> HasAttr('SI_CONVERSION') THEN BEGIN
			dt_si  = Float( (StrSplit(oDT['SI_CONVERSION'], '>', /EXTRACT))[0] )
		
		;Other
		ENDIF ELSE BEGIN
			MrPrintF, 'LogWarn', 'DELTA_MINUS_VAR has no units. Assuming seconds.'
			dt_si = 1.0
		ENDELSE
		
		dt = dt_si * oDT['DATA']
	
	;DELTA_MINUS
	ENDIF ELSE IF oT -> HasAttr('DELTA_MINUS') THEN BEGIN
		;Units
		IF oT -> HasAttr('UNITS') THEN BEGIN
			oldUnit = IDLunit(oT['UNITS'])
			newUnit = oldUnit -> To('s')
			dt_si   = newUnit.quantity
		
		;SI-Conversion
		ENDIF ELSE IF oT -> HasAttr('SI_CONVERSION') THEN BEGIN
			dt_si  = Float( (StrSplit(oT['SI_CONVERSION'], '>', /EXTRACT))[0] )
		
		;Other
		ENDIF ELSE BEGIN
			MrPrintF, 'LogWarn', 'DELTA_MINUS_VAR has no units. Assuming seconds.'
			dt_si = 1.0
		ENDELSE
		
		dt = dt_si * oT['DELTA_MINUS']
	
	;Center times
	ENDIF ELSE BEGIN
		MrPrintF, 'LogWarn', 'No DELTA_PLUS_VAR or DELTA_PLUS attribute found. Assuming center times.'
		dt = Median(t_ssm_target[1:*] - t_ssm_target) / 2.0
	ENDELSE
	
	t_lo_target = t_ssm_target - dt
	
;-------------------------------------------
; Group Data ///////////////////////////////
;-------------------------------------------
	
	;Locate the variable's time stamps within the target time stamps
	t_ssm = oV['TIMEVAR'] -> GetData('SSM', t_ref)
	it    = Value_Locate(t_lo_target, t_ssm)
	
	;Locate TV_SSM within T_SSM
	iBad = Where(it LT 0 OR t_ssm GT t_lo_target[-1]+dt[-1], nBad, COMPLEMENT=iGood, NCOMPLEMENT=nGood)
	IF nBad GT 0 THEN BEGIN
		IF nGood GT 0 $
			THEN it = it[iGood] $
			ELSE Message, 'Time tags do not overlap.'
	ENDIF
	
	;Allocate memory
	nDims   = Size(oV, /N_DIMENSIONS)
	dims    = Size(oV, /DIMENSIONS)
	dims[0] = oT.N_Elements
	v_out   = Make_Array( dims, TYPE=Size(self, /TYPE) )
	
	;Group like-times
	h = Histogram(it, REVERSE_INDICES=ri)
	FOR i = 0, N_Elements(h) - 1 DO BEGIN
		IF ri[i+1] EQ ri[i] THEN CONTINUE
		
		;Average the data
		CASE nDims OF
			1: v_out[i]               = Mean( oV['DATA', ri[ri[i]:ri[i+1]-1]],               DIMENSION=1, NAN=NaN )
			2: v_out[i,*]             = Mean( oV['DATA', ri[ri[i]:ri[i+1]-1],*],             DIMENSION=1, NAN=NaN )
			3: v_out[i,*,*]           = Mean( oV['DATA', ri[ri[i]:ri[i+1]-1],*,*],           DIMENSION=1, NAN=NaN )
			4: v_out[i,*,*,*]         = Mean( oV['DATA', ri[ri[i]:ri[i+1]-1],*,*,*],         DIMENSION=1, NAN=NaN )
			5: v_out[i,*,*,*,*]       = Mean( oV['DATA', ri[ri[i]:ri[i+1]-1],*,*,*,*],       DIMENSION=1, NAN=NaN )
			6: v_out[i,*,*,*,*,*]     = Mean( oV['DATA', ri[ri[i]:ri[i+1]-1],*,*,*,*,*],     DIMENSION=1, NAN=NaN )
			7: v_out[i,*,*,*,*,*,*]   = Mean( oV['DATA', ri[ri[i]:ri[i+1]-1],*,*,*,*,*,*],   DIMENSION=1, NAN=NaN )
			8: BEGIN
				temp = oV -> GetData()
				v_out[i,*,*,*,*,*,*,*] = Mean( (Temporary(temp))[ri[ri[i]:ri[i+1]-1],*,*,*,*,*,*,*], DIMENSION=1, NAN=NaN )
			ENDCASE
			ELSE: Message, 'Incorrect number of demensions: VAR.'
		ENDCASE
	ENDFOR
	
	;Create the variable
	oV_avg = Obj_New( Obj_Class(oV), oT, v_out, $
	                  CACHE = cache, $
	                  NAME  = name )
	
	;Cleanup variables
	RETURN, oV_avg
END