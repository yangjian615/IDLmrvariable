; docformat = 'rst'
;
; NAME:
;       mms_fig_fields.pro
;
;*****************************************************************************************
;   Copyright (c) 2014, Matthew Argall                                                   ;
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
;   Set keywords for a given graphic using attributes from a MrVariable.
;
; :Categories:
;   MrVariable, Graphics
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
;       2016/02/29  -   Written by Matthew Argall
;-
;*****************************************************************************************
;+
;   Set X-Axis-related graphics keywords for a variety.
;
; :Params:
;       GFX:        in, required, type=objref
;                   The MrGraphics object for which graphics properties are set.
;       VARIABLE:   in, required, type=objref/string
;                   A MrVariable object or the name of a MrVariable object from
;                       which to obtain graphics property values.
;-
pro MrVar_SetAxisProperties_X, gfx, var
	compile_opt strictarr
	on_error, 2

	;Graphics keywords
	gfx -> SetProperty, XCHARSIZE     = var -> GetAttrValue('CHARSIZE',     /NULL), $
	                    XGRIDSTYLE    = var -> GetAttrValue('GRIDSTYLE',    /NULL), $
	                    XLOG          = var -> GetAttrValue('LOG',          /NULL), $
	                    XMINOR        = var -> GetAttrValue('MINOR',        /NULL), $
	                    XRANGE        = var -> GetAttrValue('AXIS_RANGE',   /NULL), $
	                    XSTYLE        = var -> GetAttrValue('STYLE',        /NULL), $
	                    XTHICK        = var -> GetAttrValue('THICK',        /NULL), $
	                    XTICKFORMAT   = var -> GetAttrValue('TICKFORMAT',   /NULL), $
	                    XTICKINTERVAL = var -> GetAttrValue('TICKINTERVAL', /NULL), $
	                    XTICKLAYOUT   = var -> GetAttrValue('TICKLAYOUT',   /NULL), $
	                    XTICKLEN      = var -> GetAttrValue('TICKLEN',      /NULL), $
	                    XTICKNAME     = var -> GetAttrValue('TICKNAME',     /NULL), $
	                    XTICKS        = var -> GetAttrValue('MAJOR',        /NULL), $
	                    XTICKUNITS    = var -> GetAttrValue('TICKUNITS',    /NULL), $
	                    XTICKV        = var -> GetAttrValue('TICKV',        /NULL), $
	                    XTITLE        = var -> GetAttrValue('TITLE',        /NULL)
end


;+
;   Set Y-Axis-related graphics keywords for a variety.
;
; :Params:
;       GFX:        in, required, type=objref
;                   The MrGraphics object for which graphics properties are set.
;       VARIABLE:   in, required, type=objref/string
;                   A MrVariable object or the name of a MrVariable object from
;                       which to obtain graphics property values.
;-
pro MrVar_SetAxisProperties_Y, gfx, var
	compile_opt strictarr
	on_error, 2

	;Graphics keywords
	gfx -> SetProperty, YCHARSIZE     = var -> GetAttrValue('CHARSIZE',     /NULL), $
	                    YGRIDSTYLE    = var -> GetAttrValue('GRIDSTYLE',    /NULL), $
	                    YLOG          = var -> GetAttrValue('LOG',          /NULL), $
	                    YMINOR        = var -> GetAttrValue('MINOR',        /NULL), $
	                    YRANGE        = var -> GetAttrValue('AXIS_RANGE',   /NULL), $
	                    YSTYLE        = var -> GetAttrValue('STYLE',        /NULL), $
	                    YTHICK        = var -> GetAttrValue('THICK',        /NULL), $
	                    YTICKFORMAT   = var -> GetAttrValue('TICKFORMAT',   /NULL), $
	                    YTICKINTERVAL = var -> GetAttrValue('TICKINTERVAL', /NULL), $
	                    YTICKLAYOUT   = var -> GetAttrValue('TICKLAYOUT',   /NULL), $
	                    YTICKLEN      = var -> GetAttrValue('TICKLEN',      /NULL), $
	                    YTICKNAME     = var -> GetAttrValue('TICKNAME',     /NULL), $
	                    YTICKS        = var -> GetAttrValue('MAJOR',        /NULL), $
	                    YTICKUNITS    = var -> GetAttrValue('TICKUNITS',    /NULL), $
	                    YTICKV        = var -> GetAttrValue('TICKV',        /NULL), $
	                    YTITLE        = var -> GetAttrValue('TITLE',        /NULL)
end


;+
;   Set Z-Axis-related graphics keywords for a variety.
;
; :Params:
;       GFX:        in, required, type=objref
;                   The MrGraphics object for which graphics properties are set.
;       VARIABLE:   in, required, type=objref/string
;                   A MrVariable object or the name of a MrVariable object from
;                       which to obtain graphics property values.
;-
pro MrVar_SetAxisProperties_Z, gfx, var
	compile_opt strictarr
	on_error, 2

	;Graphics keywords
	gfx -> SetProperty, ZCHARSIZE     = var -> GetAttrValue('CHARSIZE',     /NULL), $
	                    ZGRIDSTYLE    = var -> GetAttrValue('GRIDSTYLE',    /NULL), $
	                    ZLOG          = var -> GetAttrValue('LOG',          /NULL), $
	                    ZMINOR        = var -> GetAttrValue('MINOR',        /NULL), $
	                    ZRANGE        = var -> GetAttrValue('AXIS_RANGE',   /NULL), $
	                    ZSTYLE        = var -> GetAttrValue('STYLE',        /NULL), $
	                    ZTHICK        = var -> GetAttrValue('THICK',        /NULL), $
	                    ZTICKFORMAT   = var -> GetAttrValue('TICKFORMAT',   /NULL), $
	                    ZTICKINTERVAL = var -> GetAttrValue('TICKINTERVAL', /NULL), $
	                    ZTICKLAYOUT   = var -> GetAttrValue('TICKLAYOUT',   /NULL), $
	                    ZTICKLEN      = var -> GetAttrValue('TICKLEN',      /NULL), $
	                    ZTICKNAME     = var -> GetAttrValue('TICKNAME',     /NULL), $
	                    ZTICKS        = var -> GetAttrValue('MAJOR',        /NULL), $
	                    ZTICKUNITS    = var -> GetAttrValue('TICKUNITS',    /NULL), $
	                    ZTICKV        = var -> GetAttrValue('TICKV',        /NULL), $
	                    ZTITLE        = var -> GetAttrValue('TITLE',        /NULL)
end


;+
;   Set Colorbar-Axis-related graphics keywords.
;
; :Params:
;       GFX:        in, required, type=objref
;                   The MrGraphics object for which graphics properties are set.
;       VARIABLE:   in, required, type=objref/string
;                   A MrVariable object or the name of a MrVariable object from
;                       which to obtain graphics property values.
;-
pro MrVar_SetAxisProperties_C, gfx, var
	compile_opt strictarr
	on_error, 2

	;Graphics keywords
	gfx -> SetProperty, CHARSIZE     = var -> GetAttrValue('CHARSIZE',     /NULL), $
	                    GRIDSTYLE    = var -> GetAttrValue('GRIDSTYLE',    /NULL), $
	                    LOG          = var -> GetAttrValue('LOG',          /NULL), $
	                    MAJOR        = var -> GetAttrValue('MAJOR',        /NULL), $
	                    MINOR        = var -> GetAttrValue('MINOR',        /NULL), $
	                    RANGE        = var -> GetAttrValue('AXIS_RANGE',   /NULL), $
	                    STYLE        = var -> GetAttrValue('STYLE',        /NULL), $
	                    THICK        = var -> GetAttrValue('THICK',        /NULL), $
	                    TICKFORMAT   = var -> GetAttrValue('TICKFORMAT',   /NULL), $
	                    TICKINTERVAL = var -> GetAttrValue('TICKINTERVAL', /NULL), $
	                    TICKLAYOUT   = var -> GetAttrValue('TICKLAYOUT',   /NULL), $
	                    TICKLEN      = var -> GetAttrValue('TICKLEN',      /NULL), $
	                    TICKNAME     = var -> GetAttrValue('TICKNAME',     /NULL), $
	                    TICKUNITS    = var -> GetAttrValue('TICKUNITS',    /NULL), $
	                    TICKV        = var -> GetAttrValue('TICKV',        /NULL), $
	                    TITLE        = var -> GetAttrValue('TITLE',        /NULL)
end


;+
;   Set graphics keywords.
;
; :Params:
;       GFX:        in, required, type=objref
;                   The MrGraphics object for which graphics properties are set.
;       VARIABLE:   in, required, type=objref/string
;                   A MrVariable object or the name of a MrVariable object from
;                       which to obtain graphics property values.
;
; :Keywords:
;       XAXIS:      in, optional, type=boolean, default=0
;                   If set, the X-Axis properties of `GFX` will be set using attributes
;                       from `VARIABLE`.
;       YAXIS:      in, optional, type=boolean, default=0
;                   If set, the Y-Axis properties of `GFX` will be set using attributes
;                       from `VARIABLE`.
;       ZAXIS:      in, optional, type=boolean, default=0
;                   If set, the Z-Axis properties of `GFX` will be set using attributes
;                       from `VARIABLE`.
;       CAXIS:      in, optional, type=boolean, default=0
;                   If set, the Color-Axis properties (e.g. those of a colorbar) of
;                       `GFX` will be set using attributes from `VARIABLE`.
;-
pro MrVar_SetAxisProperties, gfx, variable, $
XAXIS=xaxis, $
YAXIS=yaxis, $
ZAXIS=zaxis, $
CAXIS=caxis
	compile_opt strictarr
	on_error, 2

	;Name or variable?
	if size(var, /TNAME) eq 'STRING' $
		then var = MrVar_Get(variable) $
		else var = variable

	;Set properties
	case 1 of
		keyword_set(xaxis): MrVar_SetAxisProperties_X, gfx, var
		keyword_set(yaxis): MrVar_SetAxisProperties_Y, gfx, var
		keyword_set(zaxis): MrVar_SetAxisProperties_Z, gfx, var
		keyword_set(caxis): MrVar_SetAxisProperties_C, gfx, var
		else: ;Do nothing
	endcase
end