; docformat = 'rst'
;
; NAME:
;       MrMMS_EDI_Load_Cals
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
;   Find and read MMS EDI calibration files.
;
; :Categories:
;       MMS, EDI
;
; :Params:
;       SC:                 in, required, type=string
;                           Spacecraft identifier. Options are: {'mms1' | 'mms2' | 'mms3' | 'mms4'}
;
; :Keywords:
;       SUFFIX:             in, optional, type=string, default=''
;                           A suffix to be appended to the variable names.
;       VARFORMAT:          in, optional, type=strarr/strarr
;                           A variable name filter. Those that do not match VARFORMAT
;                               will not be read into the cache.
;       VARNAMES:           out, optional, type=strarr
;                           A named variable to receive the names of each variable read
;                               into the cache.
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
;       2016/01/23  -   Written by Matthew Argall
;-
PRO MrMMS_EDI_Load_Cals, sc, $
SUFFIX=suffix, $
VARFORMAT=varformat, $
VARNAMES=varnames
	Compile_Opt idl2

	Catch, the_error
	IF the_error NE 0 THEN BEGIN
		Catch, /CANCEL
		IF Obj_Valid(oFile) THEN Obj_Destroy, oFile
		MrPrintF, 'LogErr'
		RETURN
	ENDIF
	
	instr   = 'edi'
	level   = 'l2'
	optdesc = 'amb'
	root    = StrJoin( ['file://', 'nfs', instr, 'cals', sc, instr, 'cal', level, optdesc], '/' )
	root    = 'file:///nfs/edi/cals/' + sc + '/edi/cal/' + level + '/amb/'
	ftemp   = StrJoin( [sc, instr, 'cal', level, optdesc, '%Y%M%d', 'v*'], '_' ) + '.cdf'
	
	;URI Object
	oFile  = MrFileURI( root )
	
	;Get the files
	ftemp = oFile -> Path_Append(ftemp, ROOT=root)
	ftemp = oFile -> Get(ftemp)
	oFile -> ParseURI, ftemp, PATH=files
	Obj_Destroy, oFile

	;Load the data
	MrVar_ReadCDF, files, $
	               SUFFIX    = suffix, $
	               VARFORMAT = varformat, $
	               VARNAMES  = varnames
END
