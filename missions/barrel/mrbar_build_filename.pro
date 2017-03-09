; docformat = 'rst'
;
; NAME:
;       MrBar_Build_Filename
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
;   Build a file name in the official BARREL format.
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
;       TYPE:           in, required, type=string/strarr
;                       Data product type.
;       DATE:           in, optional, type=string/strarr, default='%Y%M%d%H%m%S'
;                       Date of launch.
;       VERSION:        in, optional, type=string/strarr, default='*'
;                       File version number.
;
; :Keywords:
;       DIRECTORY:      in, optional, type=string, default=''
;                       A directory to be pre-pended to the file name. Cannot be
;                           used with `ROOT`.
;       ROOT:           in, optional, type=string, default=''
;                       A directory that marks the root of the official BARREL directory
;                           tree. The tree is built from the inputs and the path is
;                           pre-pended to the file names.
;
; :Returns:
;       FNAMES:         out, required, type=string/strarr
;                       File names built from the inputs.
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
FUNCTION MrBar_Build_Filename, campaign, order, level, type, date, version
DIRECTORY=directory, $
ROOT=root
	Compile_Opt idl2
	On_Error, 2
	
	;Defaults
	IF N_Elements(date)      EQ 0 THEN date      = '%Y%M%d%H%m%S'
	IF N_Elements(directory) EQ 0 THEN directory = ''
	IF N_Elements(root)      EQ 0 THEN root      = ''
	IF N_Elements(version)   EQ 0 THEN version   = '*'
	
	;Conflicts
	IF directory NE '' && root NE '' THEN Message, 'DIRECTORY and ROOT are mutually exclusive.'

	;Number of elements
	nCampaign = N_Elements(campaign)
	nOrder    = N_Elements(order)
	nType     = N_Elements(type)
	nLevel    = N_Elements(level)
	nDate     = N_Elements(date)
	nVersion  = N_Elements(version)
	
	;Allocate memory
	fnames = StrArr(nCampaign*nOrder*nType*nLevel*nDate*nVersion)
	count  = 0
	
	;Loop over all possibilities
	FOR i = 0, nCampaign DO $
	FOR j = 0, nOrder    DO $
	FOR k = 0, nType     DO $
	FOR l = 0, nLevel    DO $
	FOR m = 0, nDate     DO $
	FOR n = 0, nVersion  DO BEGIN
		;Create the file name
		fnames[count] = StrJoin( ['bar', campaign[i]+order[j], level[k], type[l], date[m], 'v'+version[n]+'.cdf'], '_' )
		
		;Append path
		IF directory NE '' THEN fnames[count] = FilePath( fnames[count], ROOT_DIR=directory )
		IF root NE '' THEN BEGIN
			fnames[count] = FilePath( fnames[count], $
			                          ROOT_DIR     = root, $
			                          SUBDIRECTORY = ['v'+version[n], level[l], campaign[i]+order[j], date[m]] )
		ENDIF
		
		;Next file name
		count += 1
	ENDFOR

	;Return scalar if only one
	IF count EQ 1 THEN fnames = fnames[0]
	RETURN, fnames
END
