; docformat = 'rst'
;
; NAME:
;       MrBar_Get_Filenames
;
;*****************************************************************************************
;   Copyright (c) 2017, Matthew Argall                                                   ;
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
;+
;   Search for BARREL files on the remote server.
;
; :Keywords:
;       CAMPAIGN:       in, optional, type=string, default='4'
;                       Campaign number.
;       ORDER:          in, optional, type=string, default='A'
;                       Launch order.
;       LEVEL:          in, optional, type=string, default='l2'
;                       Data quality level.
;       TEND:           in, optional, type=string
;                       End time of data interval. Must be formatted as YYYY-MM-DD or
;                           YYYY-MM-DDThh:mm:ss
;       TSTART:         in, optional, type=string/strarr
;                       Start time of data interval. Must be formatted as YYYY-MM-DD or
;                           YYYY-MM-DDThh:mm:ss
;       TYPE:           in, optional, type=string
;                       Data product type.
;       DATE:           in, optional, type=string, default='%y%M%d'
;                       Date of launch.
;       VERSION:        in, optional, type=string
;                       File version number.
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
;       2017-01-21  -   Written by Matthew Argall
;-
FUNCTION MrBar_Get_Filenames, $
CAMPAIGN=campaign, $
COUNT=count, $
LEVEL=level, $
LOCAL_ROOT=local_root, $
ORDER=order, $
TRANGE=trange, $
TYPE=type, $
VARFORMAT=varformat, $
VARNAMES=varnames, $
VERSION=version
	Compile_Opt idl2

	;Catch errors
	Catch, the_error
	IF the_error NE 0 THEN BEGIN
		Catch, /CANCEL
		MrPrintF, 'LogErr'
		count = 0
		RETURN, ''
	ENDIF
	
	;Defaults
	IF N_Elements(trange) GT 0 THEN MrVar_SetTRange, trange
	trange = MrVar_GetTRange()
	
	;Initialize
	MrBar_Init
	
;-----------------------------------------------------
; Find Files \\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\
;-----------------------------------------------------
	
	;Set object properties
	;   - Do not call ::CD because ::CD and ::SetURI DO not like MrTokens
	!MrBar -> SetProperty, CAMPAIGN   = campaign, $
	                       DATE       = date, $
	                       LEVEL      = level, $
	                       ORDER      = order, $
	                       DATE_START = trange[1], $
	                       DATE_END   = trange[0], $
	                       TYPE       = type, $
	                       VERSION    = version
	
	;Get data
	files = oBar -> Search(COUNT=count)
	return, files
END