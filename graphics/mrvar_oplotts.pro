; docformat = 'rst'
;
; NAME:
;       MrVar_OPlotTS
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
;   Overplot time-series MrVariable data
;
; :Params:
;       GFXNAMES:   in, optional, type=string/strarr
;                   Names of the graphics objects over which data is plotted.
;       VARNAMES:   in, optional, type=string/strarr
;                   Names of the MrVariable objects to be overplotted.
;
; :Keywords:
;       WIN:        in, optional, type=objref, default=GetMrWindows(/CURRENT)
;                   A MrWindow object reference containin `GFXNAMES`.
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
function MrVar_OPlotTS, gfxNames, varNames, $
WIN=win
	compile_opt strictarr

	;Catch errors
	catch, the_error
	if the_error ne 0 then begin
		catch, /CANCEL
		MrPrintF, 'LogErr'
		return, !Null
	endif

;-------------------------------------------
; Check Inputs /////////////////////////////
;-------------------------------------------
	;Scalar or vector
	nGfx  = n_elements(gfxNames)
	nVars = n_elements(varNames)

	;Ensure same number of elements
	if nGfx eq 1 && nVars gt 1 then begin
		_gfxNames = replicate(gfxNames, nVars)
		_varNames = varNames
	endif else if nVars eq 1 && nGfx gt 1 then begin
		_gfxNames = gfxNames
		_varNames = replicate(varNames, nGfx)
	endif else if nGfx eq nVars then begin
		_gfxNames = gfxNames
		_varNames = varNames
	endif else begin
		message, 'GFXNAMES and VARNAMES must have the same number of elements.'
	endelse
	
	;Graphics window
	if n_elements(win) eq 0 $
		then win = GetMrWindows(/CURRENT) $
		else if ~obj_isa(win, 'MRWINDOW') then message, 'WIN must be a MrWindow object.'

	;Disable refreshing for the moment
	tf_refresh = win -> GetRefresh()
	if tf_refresh then win -> Refresh, /DISABLE

;-------------------------------------------
; Step Through Variables ///////////////////
;-------------------------------------------
	nVars = n_elements(_varNames)
	for i = 0, nVars - 1 do begin
		;Find the graphic & variable to be added to it
		oGfx = win -> FindByName(_gfxNames[i], COUNT=gfxCount)
		oVar = MrVar_Get(_varNames[i], COUNT=varCount)

		;Skip the variable or graphic
		;   - Graphics objects do not necessarily have unique names
		;   - All cached variables have unique names
		if gfxCount ne 1 then begin
			MrPrintF, 'LogWarn', 'Invalid number of graphics found (' + strtrim(gfxCount, 2) + ').'
			continue
		endif
		if varCount eq 0 then begin
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
			gfx = MrVar_Image(oVar, OVERPLOT=oGfx)
		
	;-------------------------------------------
	; Plot /////////////////////////////////////
	;-------------------------------------------
		endif else if oVar -> HasAttr('DEPEND_0') then begin
			gfx = MrVar_Plot(oVar, OVERPLOT=oGfx)

	;-------------------------------------------
	; Non-Time-Series //////////////////////////
	;-------------------------------------------
		endif else begin
			MrPrintF, 'LogErr', 'Variable must have a DEPEND_0 attribute: "' + oVar.name + '".'
		endelse
	endfor

	;Return the plot
	if tf_refresh then win -> Refresh
	return, win
end