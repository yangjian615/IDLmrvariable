; docformat = 'rst'
;
; NAME:
;       MrBar_Build_URI
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
;   Build a URI name in the official BARREL format.
;
; :Categories:
;   BARREL
;
; :Params:
;       CAMPAIGN:       in, optional, type=string/strarr, default='*'
;                       Campaign number.
;       ORDER:          in, optional, type=string/strarr, default='*'
;                       Launch order.
;       LEVEL:          in, optional, type=string/strarr, default='*'
;                       Data quality level.
;       TYPE:           in, required, type=string/strarr, default='*'
;                       Data product type.
;       DATE:           in, optional, type=string/strarr, default='*'
;                       Date of launch.
;       VERSION:        in, optional, type=string/strarr, default='*'
;                       File version number.
;
; :Keywords:
;       URI_SCHEME:     out, optional, type=string
;                       The scheme portion of the URI.
;       URI_HOST:       out, optional, type=string
;                       The host portion of the URI.
;       URI_PATH:       out, optional, type=string/strarr
;                       The path portion of the URI.
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
FUNCTION MrBar_Build_URI, campaign, order, level, type, date, version, $
URI_SCHEME=uri_scheme, $
URI_HOST=uri_host, $
URI_PATH=uri_path
	Compile_Opt idl2
	On_Error, 2

;---------------------------------------------------------------------
; URL Scheme & Host //////////////////////////////////////////////////
;---------------------------------------------------------------------
	;SCHEME and HOST are always the same
	uri_scheme = 'http'
	uri_host   = 'barreldata.ucsc.edu'
	uri_root   = 'data_products'

;---------------------------------------------------------------------
; Form the Path //////////////////////////////////////////////////////
;---------------------------------------------------------------------
	;Default to finding everything
	IF N_Elements(campaign) EQ 0 || campaign[0] EQ '' THEN campaign = '*'
	IF N_Elements(order)    EQ 0 || order[0]    EQ '' THEN order    = '*'
	IF N_Elements(level)    EQ 0 || level[0]    EQ '' THEN level    = '*'
	IF N_Elements(type)     EQ 0 || type[0]     EQ '' THEN type     = '*'
	IF N_Elements(date)     EQ 0 || date[0]     EQ '' THEN date     = '%Y%M%d'
	IF N_Elements(version)  EQ 0 || version[0]  EQ '' THEN version  = '*'

	;Number of elements
	nCampaign = N_Elements(campaign)
	nOrder    = N_Elements(order)
	nType     = N_Elements(type)
	nLevel    = N_Elements(level)
	nDate     = N_Elements(date)
	nVersion  = N_Elements(version)
	
	;Allocate memory
	nPaths   = nCampaign*nOrder*nType*nLevel*nDate*nVersion
	uri_path = StrArr(nPaths)
	count    = 0
	
	;Loop over all possibilities
	FOR i = 0, nCampaign - 1 DO BEGIN
	FOR j = 0, nOrder - 1    DO BEGIN
	FOR k = 0, nType - 1     DO BEGIN
	FOR l = 0, nLevel - 1    DO BEGIN
	FOR m = 0, nDate - 1     DO BEGIN
		
		;Ensure DATE is formatted correctly
		IF date[m] EQ '%Y%M%d' THEN BEGIN
			ddate = '%y%M%d'
		ENDIF ELSE IF StRegEx(date[m], '([0-9]{8})', /BOOLEAN) THEN BEGIN
			ddate = StrMid(date, 2)
		ENDIF ELSE BEGIN
			MrPrintF, 'LogWarn', 'DATE must be formatted as YYYYMMDD. Skipping "' + date[m] + '".'
			CONTINUE
		ENDELSE
		
	FOR n = 0, nVersion - 1  DO BEGIN
		
		;Path base and directory names
		uri_basename = StrJoin( ['bar', campaign[i]+order[j], level[l], type[k], date[m], 'v'+version[n]+'.cdf'], '_' )
		uri_dirname  = StrJoin( [uri_root, 'v'+version[n], level[l], campaign[i]+order[j], ddate], '/' )
		
		;Create the path
		uri_path[count] = uri_dirname + '/' + uri_basename
		
		;Next iteration
		count += 1
	ENDFOR ;n
	ENDFOR ;m
	ENDFOR ;l
	ENDFOR ;k
	ENDFOR ;j
	ENDFOR ;i

	;Return scalar if only one
	IF count EQ 0 THEN RETURN, ''
	IF count EQ 1 THEN uri_path = uri_path[0]

;---------------------------------------------------------------------
; Create the URI /////////////////////////////////////////////////////
;---------------------------------------------------------------------
	uri = uri_scheme + '://' + uri_host + '/' + uri_path
	
	return, uri
end
