; docformat = 'rst'
;
; NAME:
;       MrVar_ExportToCDF
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
; PURPOSE:
;+
;   Export MrVariables to a CDF file
;
;   NOTES:
;       Requires the MrCDF distribution.
;           https://github.com/argallmr/IDLcdf
;
;       If there are two variables by the same name, only the first will be written
;       to the CDF file. This is important when it comes to DEPEND_# variables, which
;       are not incorporated into the MrVariable cache when read by MrVar_ReadCDF.
;       Instead, MrVar_ReadCDF sets them as attribute values without ensuring that they
;       have unique names.
;
; :Params:
;       FILENAME:       in, optional, type=string
;                       The name of a tplot save file into which the data will be saved.
;                           The extension ".tplot" will be appended to FILENAME.
;       VARIABLES:      in, required, type=string/integer/objref
;                       Name, number, or objref of a MrTimeSeries object to be exported.
;                           Can be an array. Data is loaded into TPlot with the Store_Data
;                           procedure. Variables that do not subclass MrTimeSeries will
;                           silently be skipped. If not provided, or if it is the empty
;                           string, all variables in the cache are exported.
;       GLOBAL_ATTRS:   in, optional, type=struct/hash
;                       Global attribute name-value pairs. Structure tags or hash keys
;                           are used as attribute names and their values are written as
;                           global attribute values.
;
; :Keywords:
;       CLOBBER:        in, optional, type=boolean, default=0
;                       If set and `FILENAME` already exists, the existing file will be
;                           deleted.
;       COMPRESSION:    in, optional, type=string, default=''
;                       The compression type to be applied to the file.
;       GZIP_LEVEL:     in, optional, type=integer, default=5
;                       The level of gZip compression. This is applied to both `COMPRESSION`
;                           and `VARCOMPRESS` if either are 'GZIP'
;       VARCOMPRESS:    in, optional, type=string, default=''
;                       The compression type to be applied to each variable.
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
;       2017-03-13  -   Written by Matthew Argall
;-
PRO MrVar_ExportToCDF, filename, variables, global_attrs, $
COMPRESSION=compression, $
CLOBBER=clobber, $
GZIP_LEVEL=gzip_level, $
MODIFY=modify, $
VARCOMPRESS=varcompress
	Compile_Opt idl2
	
	Catch, the_error
	IF the_error NE 0 THEN BEGIN
		Catch, /CANCEL
		MrPrintF, 'LogErr'
		IF N_Elements(oCDF) GT 0 THEN Obj_Destroy, oCDF
		RETURN
	ENDIF

	;Get all of the names
	IF N_Elements(variables) EQ 0 || ( MrIsA(variables, /SCALAR, 'STRING') && variables EQ '' ) THEN BEGIN
		MrVar_Names, variables
		IF variables[0] EQ '' THEN Message, 'No variables in cache.'
	ENDIF
	
	;Create or modify
	tf_modify  = Keyword_Set(modify)
	tf_create  = ~tf_modify
	tf_clobber = Keyword_Set(clobber)

;-------------------------------------------
; Create the File //////////////////////////
;-------------------------------------------
	
	;Create CDF object
	oCDF = MrCDF_File( filename, $
	                   CLOBBER     = tf_clobber, $
	                   COMPRESSION = compression, $
	                   CREATE      = tf_create, $
	                   GZIP_LEVEL  = gzip_level, $
	                   MODIFY      = tf_modify )
	IF ~Obj_Valid(oCDF) THEN RETURN
	
	;Compression
	IF N_Elements(compression) GT 0 $
		THEN oCDF -> SetCompression, compress, GZIP_LEVEL=gzip_level

;-------------------------------------------
; Write Global Attributes //////////////////
;-------------------------------------------
	;STRUCTURE
	IF N_Elements(global_attrs) GT 0 THEN BEGIN
		IF Size(global_attrs, /TNAME) EQ 'STRUCT' THEN BEGIN
			;Attribute names
			tags  = Tag_Names(global_attrs)
			nTags = N_Tags(global_attrs)
		
			;Write to CDF
			FOR i = 0, nTags - 1 $
				DO oCDF -> WriteGlobalAttr, tags[i], global_attrs.(i), /CREATE

		;HASH
		ENDIF ELSE IF Obj_IsA(global_attrs, 'HASH') THEN BEGIN
			FOREACH value, global_attrs, attr $
				DO oCDF -> WriteGlobalAttr, attr, value, /CREATE
	
		;OTHER
		ENDIF ELSE BEGIN
			Message, 'GLOBAL_ATTRS must be a structure or hash table.'
		ENDELSE
	ENDIF

;-------------------------------------------
; Write Variables to File //////////////////
;-------------------------------------------
	
	;Write each variable
	FOREACH var, variables DO BEGIN
		oVar = MrVar_Get(var)

		Catch, the_error
		IF the_error EQ 0 THEN BEGIN
			oVar -> ExportToCDF, oCDF, $
			                     /CREATE, $
			                     COMPRESSION = varcompress, $
			                     GZIP_LEVEL  = gzip_level
		ENDIF ELSE BEGIN
			Catch, /CANCEL
			MrPrintF, 'LogWarn', 'Could not export "' + oVar.name + '".'
			MrPrintF, 'LogErr'
		ENDELSE
	ENDFOREACH
	
	;Close the file
	Obj_Destroy, oCDF
END