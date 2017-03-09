; docformat = 'rst'
;
; NAME:
;       MrMMS_Anc_Load
;
;*****************************************************************************************
;   Copyright (c) 2016, University of New Hampshire                                      ;
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
;       * Neither the name of the University of New Hampshire nor the names of its       ;
;         contributors may be used to endorse or promote products derived from this      ;
;         software without specific prior written permission.                            ;
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
;   Load MMS ancillary data into the variable cache.
;
; :Categories:
;   MMS, MrVariable
;
; :Keywords:
;    COUNT:             out, optional, type=long
;                       Set to a named variable to get the number of records read
;    HEADER:            out, optional, type=strarr
;                       Set to a named variable to receive the file header.
;    VARFORMAT:         in, optional, type=string/strarr
;                       Only the variables match this string via IDL's StrMatch will
;                           be loaded into the variable cache.
;    VARNAMES:          out, optional, type=strarr
;                       Names of the variables that were loaded into the cache.
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
;       2016-11-18  -   Written by Matthew Argall
;-
;*****************************************************************************************
;+
;   Set attributes of definitive attitude variables.
;
; :Params:
;    VARNAMES:          in, required, type=string/strarr
;                       Set attributes of definitive attitude variables with these names.
;-
pro MrMMS_Anc_Load_Attrs_DefAtt, varnames
	compile_opt idl2
	on_error, 2
	
	nVars = n_elements(varnames)
	for i = 0, nVars - 1 do begin
		;Skip EPOCH variable (it was not cached)
		if varnames[i] eq 'EPOCH' then continue
		
		;Get the variable
		oVar = MrVar_Get(varnames[i])
		if ~obj_valid(oVar) then MrPrintF, 'LogWarn', 'Variable name not cached: "' + varnames[i] + '".'
	
		;ELAPSEDSEC
		if stregex(varnames[i], 'ELAPSEDSEC', /BOOLEAN) then begin
			oVar -> AddAttr, 'CATDESC', 'Elapsed seconds since reference epoch 1958-001T00:00:00 UTC.'
			oVar -> AddAttr, 'UNITS',   's'
			oVar -> AddAttr, 'TITLE',   'Time'
	
		;Q
		endif else if stregex(varnames[i], 'Q', /BOOLEAN) then begin
			oVar -> AddAttr, 'CATDESC', 'Quaternion ECI to BCS.'
			oVar -> AddAttr, 'TITLE',   'Quaternions'
		
		;W
		endif else if stregex(varnames[i], 'W', /BOOLEAN) then begin
			oVar -> AddAttr, 'CATDESC', 'X,Y,Z rotation rate components (deg/s) (instantaneous spin axis in body frame)'
			oVar -> AddAttr, 'LABEL',   ['X', 'Y', 'Z']
			oVar -> AddAttr, 'UNITS',   'deg/s'
			oVar -> AddAttr, 'TITLE',   'Spin Axis'
		
		;W_PHASE
		endif else if stregex(varnames[i], 'W_PHASE', /BOOLEAN) then begin
			oVar -> AddAttr, 'CATDESC', 'w-phase (Sun-to-body-X dihedral angle about rotation rate vector)'
			oVar -> AddAttr, 'UNITS',   'deg'
			oVar -> AddAttr, 'TITLE',   'Spin phase'
		
		;Z
		endif else if stregex(varnames[i], 'Z', /BOOLEAN) then begin
			oVar -> AddAttr, 'CATDESC', 'right ascension and declination of body Z-axis, ' + $
			                            'Z-phase (Sun-to-body-X dihedral angle about body Z-axis)'
			oVar -> AddAttr, 'LABEL',   ['RA', 'DEC', 'Phase']
			oVar -> AddAttr, 'UNITS',   'deg'
			oVar -> AddAttr, 'TITLE',   'Z-Axis'
	
		;L
		endif else if stregex(varnames[i], 'L', /BOOLEAN) then begin
			oVar -> AddAttr, 'CATDESC', 'right ascension and declination of angular momentum (L), ' + $
			                            'L-phase (Sun-to-body-X dihedral angle about angular momentum vector L)'
			oVar -> AddAttr, 'LABEL',   ['RA', 'DEC', 'Phase']
			oVar -> AddAttr, 'UNITS',   'deg'
			oVar -> AddAttr, 'TITLE',   'L-Axis'
	
		;P
		endif else if stregex(varnames[i], 'P', /BOOLEAN) then begin
			oVar -> AddAttr, 'CATDESC', 'right ascension and declination of major principle axis (P), ' + $
			                            'P-phase (Sun-to-body-X dihedral angle about angular momentum vector P)'
			oVar -> AddAttr, 'LABEL',   ['RA', 'DEC', 'Phase']
			oVar -> AddAttr, 'UNITS',   'deg'
			oVar -> AddAttr, 'TITLE',   'P-Axis'
	
		;Nut
		endif else if stregex(varnames[i], 'NUT', /BOOLEAN) then begin
			oVar -> AddAttr, 'CATDESC', 'Nutation angle'
			oVar -> AddAttr, 'UNITS',   'deg'
			oVar -> AddAttr, 'TITLE',   'Nutation'
	
		;QF
		endif else if stregex(varnames[i], 'QF', /BOOLEAN) then begin
			oVar -> AddAttr, 'CATDESC', 'Quality flag'
			oVar -> AddAttr, 'UNITS',   'deg'
			oVar -> AddAttr, 'TITLE',   'Quality'
	
		endif else begin
			MrPrintF, 'LogWarn', 'Variable name not a DEFATT product: "' + varnames[i] + '".'
		endelse
	endfor
end

;+
;   Set attributes of definitive ephemeris variables.
;
; :Params:
;    VARNAMES:          in, required, type=string/strarr
;                       Set attributes of definitive attitude variables with these names.
;-
pro MrMMS_Anc_Load_Attrs_DefEph, varnames
	compile_opt idl2
	on_error, 2
	
	nVars = n_elements(varnames)
	for i = 0, nVars - 1 do begin
		;Skip EPOCH variable (it was not cached)
		if varnames[i] eq 'EPOCH' then continue
		
		;Get the variable
		oVar = MrVar_Get(varnames[i])
		if ~obj_valid(oVar) then MrPrintF, 'LogWarn', 'Variable name not cached: "' + varnames[i] + '".'
	
		;TAI
		if stregex(varnames[i], 'TAI', /BOOLEAN) then begin
			oVar -> AddAttr, 'CATDESC', 'Elapsed days since MMS TAI Reference Epoch = 1958-001/00:00:00 UTC.'
			oVar -> AddAttr, 'UNITS',   'days'
			oVar -> AddAttr, 'TITLE',   'Time'
	
		;R
		endif else if stregex(varnames[i], 'R', /BOOLEAN) then begin
			oVar -> AddAttr, 'CATDESC', 'Spacecraft position'
			oVar -> AddAttr, 'UNITS',   'km'
			oVar -> AddAttr, 'TITLE',   'R'
	
		;V
		endif else if stregex(varnames[i], 'V', /BOOLEAN) then begin
			oVar -> AddAttr, 'CATDESC', 'Spacecraft velocity'
			oVar -> AddAttr, 'UNITS',   'km/s'
			oVar -> AddAttr, 'TITLE',   'V'
	
		;M
		endif else if stregex(varnames[i], 'M', /BOOLEAN) then begin
			oVar -> AddAttr, 'CATDESC', 'Spacecraft mass'
			oVar -> AddAttr, 'UNITS',   'kg'
			oVar -> AddAttr, 'TITLE',   'Mass'
	
		endif else begin
			MrPrintF, 'LogWarn', 'Variable name not a DEFEPH product: "' + varnames[i] + '".'
		endelse
	endfor
end


;+
;   Provide information required for reading specific ancillary products with
;   MrFile_Read_nAscii.
;
; :Params:
;    INSTR:             in, required, type=string
;                       The ancillary data product for which parameters are output.
;    TFORMAT:           out, required, type=string
;                       Format of the date-time strings.
;    COLUMN_NAMES:      out, required, type=strarr
;                       Names of the columns within the data files.
;    COLUMN_TYPES:      out, required, type=strarr
;                       IDL data type of data within the column.
;    GROUPS:            out, required, type=intarr
;                       Specifies how `COLUMN_NAMES` are grouped together.
;    VARTYPE:           out, required, type=intarr
;                       Type of MrTimeSeries variable to create.
;    NFOOTER:           out, required, type=string
;                       Number of non-data lines at the foot of the file.
;-
pro MrMMS_Anc_Load_Params, instr, $
COLUMN_NAMES=column_names, $
COLUMN_TYPES=column_types, $
GROUPS=groups, $
NFOOTER=nfooter, $
TFORMAT=tformat, $
VARTYPE=vartype
	compile_opt idl2
	on_error, 2
	
	case instr of
		'DEFATT': begin
			column_names = ['EPOCH', 'ElapsedSec', 'Q', 'Q', 'Q', 'Q', 'w', 'w', 'w', 'w_phase', 'z', 'z', 'z', 'L', 'L', 'L', 'P', 'P', 'P', 'Nut', 'QF']
			column_types = ['STRING', 'DOUBLE', 'FLOAT', 'FLOAT', 'FLOAT', 'FLOAT', 'FLOAT', 'FLOAT', 'FLOAT', 'FLOAT', 'FLOAT', 'FLOAT', 'FLOAT', 'FLOAT', 'FLOAT', 'FLOAT', 'FLOAT', 'FLOAT', 'FLOAT', 'FLOAT', 'STRING']
			groups       = [1, 2, 3, 3, 3, 3, 4, 4, 4, 5, 6, 6, 6, 7, 7, 7, 8, 8, 8, 9, 10]
			tformat      = '%Y-%DT%H:%m:%S%f'
			nfooter      = 1
		endcase
		
		;
		; NOTE: The last column is marked "Mass", but there is no data or delimiter
		;       leading to that column. If it is included in COLUMN_NAMES and/or
		;       GROUPS, an error will occur within MrFile_Read_Ascii unless other
		;       keywords are also defined.
		;
		;       It is also possible that earlier versions of DEFEPH files do contain
		;       data in the MASS column.
		;
		'DEFEPH': begin
			tformat      = '%Y-%D/%H:%m:%S%f'
			column_names = ['EPOCH', 'TAI', 'R', 'R', 'R', 'V', 'V', 'V']
			column_types = ['STRING', 'DOUBLE', 'DOUBLE', 'DOUBLE', 'DOUBLE', 'DOUBLE', 'DOUBLE', 'DOUBLE']
			groups       = [1, 2, 3, 3, 3, 4, 4, 4]
			vartype      = ['MrTimeVar', 'MrScalarTS', 'MrVectorTS', 'MrVectorTS']
			nfooter      = 0
		end
		
		else: message, 'Ancillary product not yet implemented: "' + instr + '".'
	endcase
end


;+
;   Load MMS ancillary data into the variable cache.
;
; :Params:
;    FILES:             in, required, type=string/strarr
;                       Names of the ancillary data files to be read.
;
; :Keywords:
;    COUNT:             out, optional, type=long
;                       Set to a named variable to get the number of records read
;    HEADER:            out, optional, type=strarr
;                       Set to a named variable to receive the file header.
;    SUFFIX:            in, optional, type=string, default=''
;                       A suffix to be appended to the end of variable names before
;                           loading them into the cache.
;    VARFORMAT:         in, optional, type=string/strarr
;                       Only the variables match this string via IDL's StrMatch will
;                           be loaded into the variable cache.
;    VARNAMES:          out, optional, type=strarr
;                       Names of the variables that were loaded into the cache.
;-
pro MrMMS_Anc_Load, files, $
COUNT=count, $
HEADER=header, $
SUFFIX=suffix, $
VARFORMAT=varformat, $
VARNAMES=varnames
	compile_opt idl2
	
	catch, the_error
	if the_error ne 0 then begin
		catch, /CANCEL
		MrPrintF, 'LogErr'
		return
	endif
	
	if n_elements(suffix)    eq 0 then suffix    = ''
	
;-----------------------------------------------------
; Separate File Types \\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\
;-----------------------------------------------------
	
	;Find unique file types
	MrMMS_Anc_Parse_Filename, files, SC=sc, INSTR=instr
	prefix = sc + '_' + instr
	iuniq  = uniq(prefix, sort(prefix))
	nuniq  = n_elements(iuniq)
	
;-----------------------------------------------------
; Read Data \\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\
;-----------------------------------------------------
	for i = 0, nuniq - 1 do begin
		idx    = iuniq[i]
		f_temp = files[ where(stregex(files, prefix[idx], /BOOLEAN)) ]

		;Description of how data should be read
		MrMMS_Anc_Load_Params, instr[idx], $
		                       COLUMN_NAMES = column_names, $
		                       COLUMN_TYPES = column_types, $
		                       GROUPS       = groups, $
		                       NFOOTER      = nfooter, $
		                       TFORMAT      = tformat, $
		                       VARTYPE      = vartype
	
		;Get like files
		col_names      = column_names
		col_names[1:*] = prefix[idx] + '_' + col_names[1:*] + suffix

		;Read like files
		MrVar_ReadAscii, f_temp, $
		                 COLUMN_NAMES = col_names, $
		                 COLUMN_TYPES = column_types, $
		                 COUNT        = count, $
		                 GROUPS       = groups, $
		                 HEADER       = header, $
		                 NFOOTER      = nfooter, $
		                 TFORMAT      = tformat, $
		                 VARFORMAT    = varformat, $
		                 VARNAMES     = vnames_temp, $
		                 VARTYPE      = vartype, $
		                 VERBOSE      = !MrMMS.verbose

		;Load Attributes
		case instr[idx] of
			'DEFATT': MrMMS_Anc_Load_Attrs_DefAtt, vnames_temp
			'DEFEPH': MrMMS_Anc_Load_Attrs_DefEph, vnames_temp
			else: message, 'Ancillary product not recognized: "' + instr[idx] + '".'
		endcase
		
		;Combine variable names
		varnames = i eq 0 ? temporary(vnames_temp) : [varnames, temporary(vnames_temp)]
	endfor
end