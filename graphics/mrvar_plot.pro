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
;       p = MrVar_Plot(x)
;       p = MrVar_Plot(x, y)
;
; :Params:
;       X:          in, required, type=string/integer/objref
;                   A MrVariable name or index. If `Y` is given, then X is
;                       the independent variable, otherwise it is the dependent variable.
;                       In the latter case, X will be searched for a DEPEND_0 attribute.
;                       If present, it will be used as the dependent variable.
;       Y:          in, optional, type=string/integer/objref
;                   A MrVariable name, index, or object representing the dependent data
;                       to be plotted. 
;
; :Keywords:
;       CURRENT:    in, optional, type=boolean, default=0
;                   If set, the plot will be added to the current MrGraphics window.
;       _REF_EXTRA: in, optional, type=any
;                   Any keyword accepted by MrPlot::SetProperty
;
; :Returns:
;       P:          out, required, type=object
;                   A MrPlot object reference.
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
;       2016/06/08  -   Written by Matthew Argall
;-
function MrVar_Plot, x, y, $
CURRENT=current, $
DIMENSION=dimension, $
OVERPLOT=overplot, $
_REF_EXTRA=extra
	compile_opt strictarr

	;Catch errors
	catch, the_error
	if the_error ne 0 then begin
		catch, /CANCEL
		MrPrintF, 'LogErr'
		return, !Null
	endif

;-------------------------------------------
; Get MrVariable Objects ///////////////////
;-------------------------------------------
	;Get the x-variable
	oX = size(x, /TNAME) eq 'OBJREF' ? x : MrVar_Get(x)
	
	;If Y was not given, look for DEPEND_0 in X.
	if n_elements(y) eq 0 then begin
		if oX -> HasAttr('DEPEND_0') then begin
			oY = oX
			xname = oY -> GetAttrValue('DEPEND_0')
			oX = MrVar_Get(xname)
		endif
	endif else begin
		oY = size(y, /TNAME) eq 'OBJREF' ? y : MrVar_Get(y)
	endelse

;-------------------------------------------
; Setup Graphics ///////////////////////////
;-------------------------------------------
	
	;Get the window
	if n_elements(overplot) gt 0 then begin
		win = overplot.window
		tf_refresh = win -> GetRefresh()
	endif else if keyword_set(current) then begin
		win        = GetMrWindows(/CURRENT)
		tf_refresh = win -> GetRefresh()
	endif else begin
		win        = MrWindow(REFRESH=0)
		tf_refresh = 1
	endelse
	win -> Refresh, /DISABLE
	
	;Plot NaNs instead of fill values
	if oY -> HasAttr('FILLVAL') $
		then y_data = replace_fillval(oY['DATA'], oY['FILLVAL']) $
		else y_data = oY['DATA']
	
	;Use seconds since midnight, formatted as HH:MM:SS
	if obj_isa(oX, 'MrTimeVar') then begin
		x_data      = oX -> GetData('SSM')
		xtitle      = 'Time from ' + oX[0, 0:9]
		xtickformat = 'time_labels'
	endif else begin
		x_data = oX['DATA']
	endelse

;-------------------------------------------
; Display Data /////////////////////////////
;-------------------------------------------
	
	;Plot the data
	p1 = MrPlot( temporary(x_data), temporary(y_data), $
	             /CURRENT, $
	             DIMENSION   = oY -> GetAttrValue('DIMENSION',  /NULL), $
	             NAME        = oY.name, $
	             OVERPLOT    = overplot, $
	             COLOR       = oY -> GetAttrValue('COLOR',      /NULL), $
	             FONT        = oY -> GetAttrValue('FONT',       /NULL), $
	             LINESTYLE   = oY -> GetAttrValue('LINESTYLE',  /NULL), $
	             MAX_VALUE   = oY -> GetAttrValue('MAX_VALUE',  /NULL), $
	             MIN_VALUE   = oY -> GetAttrValue('MIN_VALUE',  /NULL), $
	             NSUM        = oY -> GetAttrValue('NSUM',       /NULL), $
	             POLAR       = oY -> GetAttrValue('POLAR',      /NULL), $
	             PSYM        = oY -> GetAttrValue('SYMBOL',     /NULL), $
	             SYMCOLOR    = oY -> GetAttrValue('SYM_COLOR',  /NULL), $
	             SYMSIZE     = oY -> GetAttrValue('SYM_SIZE',   /NULL), $
	             TITLE       = oY -> GetAttrValue('PLOT_TITLE', /NULL) )
	
	;Set graphics properties
	MrVar_SetAxisProperties, p1, oX, /XAXIS
	MrVar_SetAxisProperties, p1, oY, /YAXIS
	
	;Set user-given properties
	p1 -> SetProperty, XTITLE=xtitle, XTICKFORMAT=xtickformat
	if n_elements(extra) gt 0 then p1 -> SetProperty, _STRICT_EXTRA=extra

;-------------------------------------------
; Finish ///////////////////////////////////
;-------------------------------------------
	
	;Return the plot
	if tf_refresh then p1 -> Refresh
	return, p1
end