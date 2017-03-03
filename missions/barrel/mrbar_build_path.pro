; docformat = 'rst'
;
; NAME:
;       MrBar_Build_Path
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
;   Build a file path in the official BARREL format.
;
; :Categories:
;   BARREL
;
; :Params:
;       CAMPAIGN:       in, required, type=string/strarr
;                       Campaign number.
;       ORDER:          in, required, type=string/strarr
;                       Launch order.
;       LEVEL:          in, required, type=string/strarr
;                       Data quality level.
;       DATE:           in, optional, type=string/strarr, default='%Y%M%d%H%m%S'
;                       Date of launch.
;       VERSION:        in, optional, type=string/strarr, default='*'
;                       File version number.
;
; :Keywords:
;       ROOT:           in, optional, type=string, default=''
;                       A directory that marks the root of the official BARREL directory
;                           tree. The tree is built from the inputs and the path is
;                           pre-pended to the file names.
;
; :Returns:
;       PATHS:          out, required, type=string/strarr
;                       File paths aths built from the inputs.
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
;       2017-01-15  -   Written by Matthew Argall
;-
FUNCTION MrBar_Build_Path, campaign, order, level, date, version
ROOT=root
	Compile_Opt idl2
	On_Error, 2
	
	;Defaults
	IF N_Elements(date)      EQ 0 THEN date      = '%Y%M%d%H%m%S'
	IF N_Elements(root)      EQ 0 THEN root      = ''
	IF N_Elements(version)   EQ 0 THEN version   = '*'

	;Number of elements
	nCampaign = N_Elements(campaign)
	nOrder    = N_Elements(order)
	nLevel    = N_Elements(level)
	nDate     = N_Elements(date)
	nVersion  = N_Elements(version)
	
	;Allocate memory
	paths = StrArr(nCampaign*nOrder*nLevel*nDate*nVersion)
	count  = 0
	
	;Loop over all possibilities
	FOR i = 0, nCampaign DO $
	FOR j = 0, nOrder    DO $
	FOR k = 0, nLevel    DO $
	FOR l = 0, nDate     DO $
	FOR m = 0, nVersion  DO BEGIN
		;Create the path
		paths[count] = FilePath( '', $
		                         ROOT_DIR     = 'v'+version[m], $
		                         SUBDIRECTORY = [level[k], campaign[i] + order[j], date[l]] )
		
		;Append directory
		IF root NE '' THEN paths[count] = FilePath( paths[count], ROOT_DIR=root )
		
		;Next file name
		count += 1
	ENDFOR

	;Return scalar if only one
	IF count EQ 1 THEN paths = paths[0]
	RETURN, paths
END