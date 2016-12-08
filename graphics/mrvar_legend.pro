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
;       VAR[1-8]:   in, required, type=string/integer/objref
;                   A MrVariable index, name, or object. Each variable will contribute
;                       items in the MrLegend object.
;
; :Keywords:
;       ADD:        in, optional, type=objref
;                   If the legend position is specified in relative or data coordinates,
;                       then the data space is obtained from this MrGraphics data.
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
function MrVar_Legend, var1, var2, var3, var4, var5, var6, var7, var8, $
ADD=add, $
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
; Add to Existing Legend //////////////////
;-------------------------------------------
	if keyword_set(add) then begin
		;Heap ID of target
		heapID = obj_valid(target, /GET_HEAP_IDENTIFIER)
		if heapID eq 0 then message, 'TARGET must be a valid object if ADD is set.'

		;Loop through all legends
		tf_found = 0B
		iLegend  = 0
		all_lgds = target.window -> Get(/ALL, ISA='MRLEGEND', COUNT=nLegends)
		while ~tf_found && iLegend lt nLegends do begin
			
			;Loop through all items
			iItem     = 0L
			all_items = all_lgds[iLegend] -> GetItem(COUNT=nItems)
			while ~tf_found && iItem lt nItems do begin
				
				;Compare target of legend item to target of new graphic
				if obj_valid((all_items[iItem]).target, /GET_HEAP_IDENTIFIER) eq heapID then begin
					theLegend = all_items[iItem]
					tf_found = 1B
				endif
				
				;Next item
				iItem += 1
			endwhile
			
			;Next legend
			iLegend += 1
		endwhile
		
		;Cannot find
		if ~tf_found then message, 'No legend found. Cannot add items.'
		
		;Start loop at first parameter
		lgd    = all_lgds[iLegend-1]
		iStart = 1

;-------------------------------------------
; Create Legend ////////////////////////////
;-------------------------------------------
	endif else begin
		;Get the first varaible
		theVar = MrVar_Get(var1)
		
		;Legend Label
		case 1 of
			theVar -> HasAttr('LABEL'): label = theVar['LABEL']
			theVar.name ne '':          label = theVar.name
			else:                       label = 'LEGEND_ITEM_1'
		endcase
		
		;Sample Color
		case 1 of
			theVar -> HasAttr('SAMPLE_COLOR'): sample_color = theVar['SAMPLE_COLOR']
			theVar -> HasAttr('COLOR'):        sample_color = theVar['COLOR']
			else: sample_color = !Null
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
		                SAMPLE_COLOR     = sample_color, $
		                SAMPLE_LINESTYLE = theVar -> GetAttrValue('LINESTYLE',    /NULL), $
		                SAMPLE_WIDTH     = theVar -> GetAttrValue('SAMPLE_WIDTH', /NULL), $
		                ;SYMBOL
		                SYMBOL           = theVar -> GetAttrValue('SYMBOL',    /NULL), $
		                /SYM_CENTER, $
		                SYM_COLOR        = theVar -> GetAttrValue('SYM_COLOR', /NULL), $
		                SYM_SIZE         = theVar -> GetAttrValue('SYM_SIZE',  /NULL), $
		                SYM_THICK        = theVar -> GetAttrValue('SYM_THICK', /NULL) )
		
		;Start loop at second parameter
		iStart = 2
	endelse

;-------------------------------------------
; Add More Items ///////////////////////////
;-------------------------------------------
	
	;Add more items individually
	for i = iStart, n_params() do begin
		case i of
			1: theVar = IsA(var1, 'MrVariable') ? var1 : MrVar_Get(var1)
			2: theVar = IsA(var2, 'MrVariable') ? var2 : MrVar_Get(var2)
			3: theVar = IsA(var3, 'MrVariable') ? var3 : MrVar_Get(var3)
			4: theVar = IsA(var4, 'MrVariable') ? var4 : MrVar_Get(var4)
			5: theVar = IsA(var5, 'MrVariable') ? var5 : MrVar_Get(var5)
			6: theVar = IsA(var6, 'MrVariable') ? var6 : MrVar_Get(var6)
			7: theVar = IsA(var7, 'MrVariable') ? var7 : MrVar_Get(var7)
			8: theVar = IsA(var8, 'MrVariable') ? var8 : MrVar_Get(var8)
			else: message, 'Incorrect number of parameter.'
		endcase
	
		;Legend Label
		case 1 of
			theVar -> HasAttr('LABEL'): label = theVar['LABEL']
			theVar.name ne '':          label = theVar.name
			else:                       label = 'LEGEND_ITEM_' + strtrim(i, 2)
		endcase
		
		;Sample Color
		case 1 of
			theVar -> HasAttr('SAMPLE_COLOR'): sample_color = theVar['SAMPLE_COLOR']
			theVar -> HasAttr('COLOR'):        sample_color = theVar['COLOR']
			else: sample_color = !Null
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
	                SAMPLE_COLOR     = sample_color, $
	                SAMPLE_LINESTYLE = theVar -> GetAttrValue('LINESTYLE',    /NULL), $
	                SAMPLE_WIDTH     = theVar -> GetAttrValue('SAMPLE_WIDTH', /NULL), $
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