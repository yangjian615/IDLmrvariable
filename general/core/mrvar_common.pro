; docformat = 'rst'
;
; NAME:
;       MrVar_Common
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
;   Define the MrVarCom common block. As the common block may change in the future,
;   it is incorporated as an include file (invoked as @MrVarCom within any procedure
;   or function) so that only the definition need change. Variables within the common
;   block are defined in more appropriate places (see below).
;
;   INCLUDE FILES
;       An include file used to establish the !MR_VARIABLES system variable, which is a
;       container for variable objects.
;   
;       See this help page about include files:
;           http://exelisvis.com/docs/includefiles.html
;           http://exelisvis.com/docs/batchjobs.html
;
;   VARIABLE DEFINITIONS
;       MrVarCache:
;           Defined in:
;               MrVariable::Cache
;               MrVar_Cache
;
;       MrVarTRange:
;           Created in:
;               MrVar_SetTRange
;
;   RECOMMENDED USE
;       If you wish to use a variable in the common block, it is a good idea to
;       check if it exists first [i.e. N_Elements() NE 0], then proceed accordingly.
;
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
;       2016-06-23  -   Common block variables are created elsewhere. - MRA
;       2016-07-14  -   Renamed from MrVar_Setup to MrVar_Common. - MRA
;-
common MrVarCom, MrVarCache, MrVarTRange