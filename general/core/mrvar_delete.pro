; docformat = 'rst'
;
; NAME:
;       MrVar_Delete
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
;   Remove and destroy variables in the cache.
;
; :Params:
;       VAR[1-10]:          in, optional, type=objref/string/integer
;                           A scalar or array of MrVariable object references, names,
;                               or cache indices.
;
; :Keywords:
;       ALL:                in, optional, type=boolean, default=0
;                           If set all cached variables will be deleted and `VARS[1-10]`
;                               is ignored.
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
;       2016-02-15  -   Written by Matthew Argall
;       2016-06-23  -   Check if the variable cache has been created. - MRA
;-
;*****************************************************************************************
pro MrVar_Delete, var1, var2, var3, var4, var5, var6, var7, var8, var9, var10, $
ALL=all
	compile_opt idl2
	on_error, 2

	;Ensure everthing has MrVar has been initialized
	@mrvar_common
	
	;No need to do anything if the Cache has not been created
	if ~obj_valid(MrVarCache) then return
	
	;Delete all variables
	if keyword_set(all) then begin
		MrVarCache -> Remove, /ALL, /DESTROY
		
	;Delete specific variables
	endif else begin
		switch n_params() of
			10: MrVarCache -> Remove, var10, /DESTROY
			 9: MrVarCache -> Remove,  var9, /DESTROY
			 8: MrVarCache -> Remove,  var8, /DESTROY
			 7: MrVarCache -> Remove,  var7, /DESTROY
			 6: MrVarCache -> Remove,  var6, /DESTROY
			 5: MrVarCache -> Remove,  var5, /DESTROY
			 4: MrVarCache -> Remove,  var4, /DESTROY
			 3: MrVarCache -> Remove,  var3, /DESTROY
			 2: MrVarCache -> Remove,  var2, /DESTROY
			 1: MrVarCache -> Remove,  var1, /DESTROY
		endswitch
	endelse
end