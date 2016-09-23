; docformat = 'rst'
;
; NAME:
;       MrVar_Names
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
;   Retrieve objects from the cache by name, index, or object reference.
;
; :Params:
;       VAR:            in, required, type=name/number/objref
;                       Name, index, or object reference of the variable(s) to be retrieved.
;
; :Keywords:
;       ALL:            in, optional, type=boolean, default=0
;                       If set, all objects in the container matching `ISA` are returned.
;                           In this case `VARS` is ignored.
;       ISA:            in, optional, type=string/strarr
;                       Object class names used to filter the results of `ALL`.
;       COUNT:          out, optional, type=integer
;                       A named variable to return the number of objects returned.
;
; :Returns:
;       VARIABLES:      out, required, type=objref
;                       Objects references to objects in the container.
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
;       2016-02-13  -   Written by Matthew Argall
;       2016-06-23  -   Check if the variable cache has been created. - MRA
;       2016-07-14  -   MrVariable object references may be given. - MRA
;       2016-08-21  -   If object references are given, veryify that they are MrVariable
;                           objects. Added the `ISCACHED` keyword. - MRA
;       2016-08-28  -   Use the (new) MrVariable_Cache::Get method. - MRA
;-
;*****************************************************************************************
function MrVar_Get, var, $
ALL=all, $
COUNT=count, $
ISA=isa
	compile_opt idl2
	on_error, 2

	;Ensure MrVar has been initialized
	@mrvar_common
	
	;Search the cache
	if obj_valid(MrVarCache) then begin
		variables = MrVarCache -> Get( var, $
		                               ALL         = all, $
		                               COUNT       = count, $
		                               ISA         = isa )
	
	;The cache has not been created yet
	endif else begin
		variables = !Null
		count     = 0
		if arg_present(isCached) then begin
			nVars     = n_elements(var)
			isCached  = nVars eq 1 ? 0 : replicate(0, nVars)
		endif
	endelse

	;Get the variable
	return, variables
end