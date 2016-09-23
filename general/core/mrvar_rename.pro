; docformat = 'rst'
;
; NAME:
;       MrVar_Reames
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
;   Rename a MrVariable.
;
; :Params:
;       OLD_NAME:       out, optional, type=string
;                       Name of the MrVariable that is to be renamed.
;       NEW_NAME:       in, required, type=string
;                       New name to be given to the variable.
;
; :Keywords:
;       CHECK_DEPEND:   in, optional, type=boolean, default=0
;                       If set, then all other variables will have their DEPEND_#
;                           variable attributes updated accordingly.
;
; :Author:
;   Matthew Argall::
;       University of New Hampshire
;       Morse Hall, Room 348
;       8 College Rd.
;       Durham, NH, 03824
;       matthew.argall@unh.edu
;
; :Copyright:
;       Matthew Argall 2016
;
; :History:
;   Modification History::
;       2016-06-17  -   Written by Matthew Argall
;-
;*****************************************************************************************
pro MrVar_Rename, old_name, new_name, $
CHECK_DEPEND=check_depend
	compile_opt idl2
	on_error, 2

	;Ensure MrVar has been initialized
	@mrvar_common

;-------------------------------------------
; Rename the Variable //////////////////////
;-------------------------------------------
	
	;Retrieve and rename the variable
	theVar      = MrVarCache[old_name]
	theVar.name = new_name

;-------------------------------------------
; Update DEPEND_# //////////////////////////
;-------------------------------------------
	
	;Change dependent variable names as well
	if keyword_set(check_depend) then begin
		count = MrVarCache -> Count()
		for i = 0, count-1 do begin
			theVar = MrVarCache -> Get(POSITION=i)
			
			;Step over each DEPEND_#
			for j = 0, 3 do begin
				attrname = string('DEPEND_', i, FORMAT='(a0, i1)')
				dep_name = var -> GetVarAttr(attrName, /NULL)
			
				;Change the DEPEND_# attribute value
				if dep_name ne !Null then begin
					if dep0_name eq old_name then var -> SetAttrValue(attrName, new_name)
				endif
			endfor
		endfor
	endif
end