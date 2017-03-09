; docformat = 'rst'
;
; NAME:
;       MrBar_Load_Data
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
;   Find, download, and load BARREL data into the variable cache.
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
PRO MrBar_Load_Data, $
CAMPAIGN=campaign, $
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
		RETURN
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
	!MrBar -> SetProperty, CAMPAIGN = campaign, $
	                       DATE     = date, $
	                       LEVEL    = level, $
	                       ORDER    = order, $
	                       TEND     = trange[1], $
	                       TSTART   = trange[0], $
	                       TYPE     = type, $
	                       VERSION  = version
	
	;Get data
	files = oBar -> Get(COUNT=nFiles)
	IF nFiles EQ 0 THEN Message, 'No files found.'
	
;-----------------------------------------------------
; Load Into Cache \\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\
;-----------------------------------------------------
	MrBar_Parse_Filename, files, $
	                      CAMPAIGN = campaign, $
	                      ORDER    = order, $
	                      LEVEL    = level, $
	                      TYPE     = type

	;Determine file types
	fType = StrJoin( ['bar', campaign+order, level[k], type[l]], '_' )
	
	;Pick unique file types
	fbase = File_BaseName(files)
	iUniq = Uniq(fType, Sort(fType))
	nUniq = N_Elements(iUniq)

	;Step through each file type
	FOR i = 0, nUniq - 1 DO begin
		;Find similar file types
		iFiles = Where(StrMid( fbase, 0, StrLen(fType[i])) EQ fTYpe[i], nFiles)
		IF nFiles EQ 0 THEN CONTINUE
		
		;Read files
		MrVar_ReadCDF, files[iFiles], $
		               SUFFIX       = suffix, $
		               SUPPORT_DATA = support_data, $
		               TRANGE       = trange, $
		               VARFORMAT    = varformat, $
		               VARNAMES     = temp_names
		
		;Save all of the variables
		IF N_Elements(varnames) GT 0 $
			THEN varnames = [varnames, Temporary(temp_names)] $
			ELSE varnames = Temporary(temp_names)
	ENDFOR
END