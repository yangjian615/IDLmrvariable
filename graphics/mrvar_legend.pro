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
;   LEGEND ITEMS
;       Legend items will obtain their properties from variable attributes::
;           LABEL             LABEL | var.name
;           FONT              FONT
;           TEXT_COLOR        COLOR
;           TEXT_SIZE         CHARSIZE
;           TEXT_THICK        THICK
;           SAMPLE_COLOR      COLOR
;           SAMPLE_LINESTYLE  LINESTYLE
;           SYMBOL            SYMBOL
;           SYM_COLOR         SYM_COLOR || COLOR
;           SYM_SIZE          SYM_SIZE
;           SYM_THICK         SYM_THICK
;
; :Params:
;       GFX[1-8]:   in, required, type=string/integer/objref
;                   A MrVariable index, name, or object. Each variable will have an
;                       item in the MrLegend object.
;
; :Keywords:
;       TARGET:     in, optional, type=objref
;                   If the legend position is specified in relative or data coordinates,
;                       then the data space is obtained from this MrGraphics data.
;       _REF_EXTRA: in, optional, type=any
;                   Any keyword accepted by MrLegend::SetProperty
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
function MrVar_Legend, gfx1, gfx2, gfx3, gfx4, gfx5, gfx6, gfx7, gfx8, $
TARGET=target, $
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
; Create Legend ////////////////////////////
;-------------------------------------------
	;Get the first varaible
	theVar = IsA(gfx1, 'MrVariable') ? gfx1 : MrVar_Get(gfx1)
	
	;Legend Label
	case 1 of
		theVar -> HasAttr('LABEL'): label = theVar['LABEL']
		theVar.name ne '':          label = theVar.name
		else:                       label = 'LEGEND_ITEM_1'
	endcase
	
	;Create legend with first variable
	lgd = MrLegend( TARGET           = target, $
	                ;TEXT
	                LABEL            = label, $
	                FONT             = theVar -> GetAttrValue('FONT',      /NULL), $
	                TEXT_COLOR       = theVar -> GetAttrValue('COLOR',     /NULL), $
	                TEXT_SIZE        = theVar -> GetAttrValue('CHARSIZE',  /NULL), $
	                TEXT_THICK       = theVar -> GetAttrValue('THICK',     /NULL), $
	                ;LINE
	                SAMPLE_COLOR     = theVar -> GetAttrValue('COLOR',     /NULL), $
	                SAMPLE_LINESTYLE = theVar -> GetAttrValue('LINESTYLE', /NULL), $
	                SAMPLE_WIDTH     = 3, $
	                ;SYMBOL
	                SYMBOL           = theVar -> GetAttrValue('SYMBOL',    /NULL), $
	                /SYM_CENTER, $
	                SYM_COLOR        = theVar -> GetAttrValue('SYM_COLOR', /NULL), $
	                SYM_SIZE         = theVar -> GetAttrValue('SYM_SIZE',  /NULL), $
	                SYM_THICK        = theVar -> GetAttrValue('SYM_THICK', /NULL) )

;-------------------------------------------
; Add More Items ///////////////////////////
;-------------------------------------------
	
	;Add more items individually
	for i = 2, n_params() do begin
		case i of
			2: theVar = IsA(gfx2, 'MrVariable') ? gfx2 : MrVar_Get(gfx2)
			3: theVar = IsA(gfx3, 'MrVariable') ? gfx3 : MrVar_Get(gfx3)
			4: theVar = IsA(gfx4, 'MrVariable') ? gfx4 : MrVar_Get(gfx4)
			5: theVar = IsA(gfx5, 'MrVariable') ? gfx5 : MrVar_Get(gfx5)
			6: theVar = IsA(gfx6, 'MrVariable') ? gfx6 : MrVar_Get(gfx6)
			7: theVar = IsA(gfx7, 'MrVariable') ? gfx7 : MrVar_Get(gfx7)
			8: theVar = IsA(gfx8, 'MrVariable') ? gfx8 : MrVar_Get(gfx8)
			else: message, 'Incorrect number of parameter.'
		endcase
	
		;Legend Label
		case 1 of
			theVar -> HasAttr('LABEL'): label = theVar['LABEL']
			theVar.name ne '':          label = theVar.name
			else:                       label = 'LEGEND_ITEM_' + strtrim(i, 2)
		endcase
		
		;Add the legend item
		lgd -> Add, TARGET           = target, $
	                ;TEXT
	                LABEL            = label, $
	                FONT             = theVar -> GetAttrValue('FONT',      /NULL), $
	                TEXT_COLOR       = theVar -> GetAttrValue('COLOR',     /NULL), $
	                TEXT_SIZE        = theVar -> GetAttrValue('CHARSIZE',  /NULL), $
	                TEXT_THICK       = theVar -> GetAttrValue('THICK',     /NULL), $
	                ;LINE
	                SAMPLE_COLOR     = sym_color, $
	                SAMPLE_LINESTYLE = theVar -> GetAttrValue('LINESTYLE', /NULL), $
	                SAMPLE_WIDTH     = 3, $
	                ;SYMBOL
	                SYMBOL           = theVar -> GetAttrValue('SYMBOL',    /NULL), $
	                /SYM_CENTER, $
	                SYM_COLOR        = theVar -> GetAttrValue('SYM_COLOR', /NULL), $
	                SYM_SIZE         = theVar -> GetAttrValue('SYM_SIZE',  /NULL), $
	                SYM_THICK        = theVar -> GetAttrValue('SYM_THICK', /NULL)

;		            AUTO_TEXT_COLOR  = auto_text_color, $
;		            FONT             = font, $
;		            LABEL            = label, $
;		            TEXT_COLOR       = text_color, $
;		            TEXT_SIZE        = text_size, $
;		            TEXT_THICK       = text_thick, $
;		            TARGET           = target, $
;		            SAMPLE_ANGLE     = sample_angle, $
;		            SAMPLE_COLOR     = sample_color, $
;		            SAMPLE_LINESTYLE = sample_linestyle, $
;		            SAMPLE_MAGNITUDE = sample_magnitude, $
;		            SAMPLE_THICK     = sample_thick, $
;		            SAMPLE_WIDTH     = sample_width, $
;		            SYMBOL           = psym, $
;		            SYM_CENTER       = sym_center, $
;		            SYM_COLOR        = sym_color, $
;		            SYM_SIZE         = sym_size, $
;		            SYM_THICK        = sym_thick
	endfor

;-------------------------------------------
; Finish ///////////////////////////////////
;-------------------------------------------
	
	;Set user-given properties
	if n_elements(extra) gt 0 then lgd -> SetProperty, _STRICT_EXTRA=extra
	
	;Return the plot
	return, lgd
end