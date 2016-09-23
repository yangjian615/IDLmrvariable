; docformat = 'rst'
;
; NAME:
;       MrVar_Replace
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
;   Replace a cached variable with a non-cached variable.
;
; :Params:
;       OLDVAR:             in, required, type=objref/string/integer
;                           The name, index, or object reference of the MrVariable
;                               to be replaced.
;       NEWVAR:             in, required, type=objref
;                           The name, index, or object reference of replacement MrVariable.
;
; :Keywords:
;       DESTROY:            in, optional, type=boolean, default=0
;                           If set, `OLDVAR` will be destroyed upon replacement. This
;                               is the default unless `OLDVAR` is an object reference.
;       RENAME:             in, optional, type=boolean, default=0
;                           If set, `NEW` will be given the name of `OLD`.
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
;       2016-08-28  -   Written by Matthew Argall
;-
;*****************************************************************************************
pro MrVar_Replace, oldVar, newVar, $
DESTROY=destroy, $
RENAME=rename
	compile_opt idl2
	on_error, 2

	;Ensure everthing has MrVar has been initialized
	@mrvar_common
	
	;No need to do anything if the Cache has not been created
	if ~obj_valid(MrVarCache) then message, 'OLDVAR must be in the cache.'
	
	;Replace the old with the new
	MrVarCache -> Replace, oldVar, newVar, DESTROY=destroy, TAKE_NAME=rename
end