; docformat = 'rst'
;
; NAME:
;       MrVar_Plot
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
;   Plot MrVariable data
;
;   Calling Sequence:
;       win = MrVar_PlotTS()
;       win = MrVar_PlotTS(variables)
;
; :Params:
;       VARIABLES:  in, optional, type=strarr/intarr
;                   The names or indices of time series variables to be plotted. If
;                       not provided, the variables given in the previous call to
;                       MrVar_PlotTS will be used.
;
; :Returns:
;       WIN:        out, required, type=objref
;                   A MrWindow object reference.
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
;       2016/08/13  -   Written by Matthew Argall
;-
function MrVar_PlotTS, variables, $
XSIZE=xsize, $
YSIZE=ysize
	compile_opt strictarr

	;Catch errors
	catch, the_error
	if the_error ne 0 then begin
		catch, /CANCEL
		if n_elements(win) gt 0 then obj_destroy, win
		MrPrintF, 'LogErr'
		return, !Null
	endif
	
	;Common block to hold previous variables
	common MrVar_PlotTS_Common, vars

;-------------------------------------------
; What to Plot /////////////////////////////
;-------------------------------------------
	;Use what was plotted previously
	if n_elements(variables) eq 0 then begin
		if n_elements(vars) eq 0 $
			then message, 'Specify the variables to be plotted.' $
			else variables = vars
			
	;New set of variables to be plotted
	endif else begin
		;Must be variable names or numbers
		if ~MrIsA(variables, /INTEGER) && ~MrIsA(variables, 'STRING') $
			then message, 'VARIABLES must be variable names or indices.'
	endelse
	
	;Number of variables to plot
	nVars = n_elements(variables)

;-------------------------------------------
; Create the Window ////////////////////////
;-------------------------------------------
	win = MrWindow(LAYOUT=[1,nVars], XSIZE=xsize, YGAP=0.5, YSIZE=ysize, REFRESH=0)

;-------------------------------------------
; Step Through Each Variable ///////////////
;-------------------------------------------
	for i = 0, nVars - 1 do begin
		;Get the variable
		oVar = MrVar_Get(variables[i], COUNT=count)

		;Skip the variable if it is not in the cache
		if count eq 0 then begin
			MrPrintF, 'LogWarn', 'Variable not found: "' + variables[i] + '".'
			continue
		endif

	;-------------------------------------------
	; Unknown Types ////////////////////////////
	;-------------------------------------------
		if oVar -> HasAttr('DEPEND_3') || oVar -> HasAttr('DEPEND_2') then begin
			MrPrintF, 'LogErr', 'Unknown graphic type for "' + oVar.name + '". Maximum of DEPEND_1 expected.'
			
	;-------------------------------------------
	; Image ////////////////////////////////////
	;-------------------------------------------
		endif else if oVar -> HasAttr('DEPEND_1') then begin
			gfx = MrVar_Image(oVar, /CURRENT)
		
	;-------------------------------------------
	; Plot /////////////////////////////////////
	;-------------------------------------------
		endif else if oVar -> HasAttr('DEPEND_0') then begin
			gfx = MrVar_Plot(oVar, /CURRENT)

	;-------------------------------------------
	; Non-Time-Series //////////////////////////
	;-------------------------------------------
		endif else begin
			MrPrintF, 'LogErr', 'Variable must have a DEPEND_0 attribute: "' + oVar.name + '".'
		endelse

	;-------------------------------------------
	; Prettify /////////////////////////////////
	;-------------------------------------------
		if i gt 0       then title       = ''     else void = temporary(title)
		if i lt nVars-1 then xtickformat = '(a1)' else void = temporary(xtickformat)
		if i lt nVars-1 then xtitle      = ''     else void = temporary(xtitle)
		gfx -> SetProperty, TITLE=title, XTICKFORMAT=xtickformat, XTITLE=xtitle
	endfor

;-------------------------------------------
; Finish ///////////////////////////////////
;-------------------------------------------
	;Put the variables into the common block
	vars = variables
	
	;Time range
	trange  = MrVar_GetTRange('SSM')
	win    -> SetGlobal, XRANGE=trange
	
	;Return the plot
	win -> Refresh
	return, win
end