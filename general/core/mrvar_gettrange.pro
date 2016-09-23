; docformat = 'rst'
;
; NAME:
;       MrVar_SetTRange
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
;   Get the global time range for reading and displaying data with MrVariables
;
; :Params:
;       TYPE:               in, optional, type=array, default='ISO-8601'
;                           Specify to output `TIME` in one of the following formats::
;                               'CDF_EPOCH'       - CDF Epoch values (milliseconds)
;                               'CDF_EPOCH16'     - CDF Epoch16 values (picoseconds)
;                               'CDF_EPOCH_LONG'  - CDF Epoch16 values (picoseconds)
;                               'CDF_TIME_TT2000' - CDF TT2000 values (nanoseconds)
;                               'TT2000'          - CDF TT2000 values (nanoseconds)
;                               'JULIAN'          - Julian date (days)
;                               'UNIX'            - Unix time (seconds)
;                               'ISO-8601'        - ISO-8601 formatted string
;                               'CUSTOM'          - Custom time string format
;
; :Keywords:
;       TOKEN_FMT:          in, optional, type=string
;                           The MrTokens pattern that specifies how `TIME` should be
;                               output. If provided and `TYPE` is undefined, automatically
;                               sets `TYPE` = 'CUSTOM'.
;
; :Returns:
;       TRANGE:             in, required, type=2x1 strarr
;                           Array of values to be stored, or a MrVariable object whose
;                               array is to be copied.
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
;       2016-05-27  -   Written by Matthew Argall
;       2016-06-23  -   Check if the time range object has been created. - MRA
;-
function MrVar_GetTRange, type, $
TOKEN_FMT=token_fmt
	compile_opt idl2
	on_error, 2

	;Enable common block
	@mrvar_common
	
	if ~obj_valid(MrVarTRange) then message, 'No time range established. Set with MrVar_SetTRange.'
	
	;Set the time range
	return, MrVarTRange -> GetData(type, TOKEN_FMT=token_fmt)
end