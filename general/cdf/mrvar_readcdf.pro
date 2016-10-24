; docformat = 'rst'
;
; NAME:
;       MrVar_ReadCDF
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
;   Read data and its dependencies from multiple CDF data files and cache them
;   as MrVariables.
;
;   MRVARIABLE CACHE NOTES:
;       If a variable being read is already in the cache, and NO_CLOBBER=0, then
;       then it AND its DEPEND_# variables will be clobbered. This assumes that
;       all variables using the DEPEND_# support data come from the data files
;       being read. If other variables have been linked to the same DEPEND_# data,
;       then they will have orphaned variable attributes. Such a case is handled
;       internal to the variable, but may suprise the user when attempting to
;       plot the variable.
;
;       Many CDF files use the variable name "Epoch" for all time-dependencies.
;       In the case of NO_CLOBBER=0 described above, new variables will generate
;       new, unique "DEPEND_0" names (by default, "Epoch" would be renamed to
;       "Epoch_1").
;
;   NO_CLOBBER NOTES:
;       - A CDF variable is read, creating a variable and a DEPEND_0 attribute.
;       - The variable is destroyed, but DEPEND_0 gets carried on to child variables
;       - The next time the original variable is read, its DEPEND_0 attribute creates
;         a NEW cached variable (i.e. Epoch_1 instead of Epoch).
;       - This is to prevent clobbering the child variable's DEPEND_0 attribute.
;
;     Take-away lesson
;       - If you plan to read data more than once, keep the original CDF variable in
;         the cache.
;       - The other option is to rename the DEPEND_0 variables after you read them.
;         This way, the uniq variable name will be over-written each time.
;
;
;   MISC NOTES:
;       If DEPEND_# has no record variance, the values are assumed to be
;       uniform across files and data from the last file is returned.
;
;       `FILES` are assumed to be in chronological order. If not, data
;       will also not be in chronological order.
;
; :Categories:
;       CDF Utilities
;
; :Common Blocks:
;   MrVar_ReadCDF_common::
;       CDF_VNAMES   - CDF variable names of all unique variables from all files
;       CACHE_VNAMES - MrVariable names of all variables that have been read
;       CDF_VCOUNT   - Total number of variables that have been read
;       FILE_VNAMES  - CDF variable names of variables that have been read from the current file.
;       FILE_VCOUNT  - Number of variables read from the current file
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
;       2016/04/30  -   Written by Matthew Argall
;       2016/07/04  -   DEPEND_N was being clobbered for each variable. Now clobber
;                           only for first variable. - MRA
;       2016/07/06  -   VARFORMAT can be an array. - MRA
;       2016/07/13  -   Added the VARNAMES keyword. Implemented TRANGE keyword. - MRA
;       2016/07/20  -   Added MrVar_ReadCDF_VarAttr2GfxKwd. - MRA
;       2016/08/20  -   Prevent variables from being read twice - once as an attribute
;                           and once as a variable - when /SUPPORT_DATA is set. - MRA
;       2016/10/06  -   Added the SUFFIX keyword. - MRA
;       2016/10/06  -   Major rewrite to support MrTimeSeries & simplify program flow. - MRA
;-
;*****************************************************************************************
;+
;    Helper function for MrCDF_nRead. Clobber a variable.
;
; :Params:
;       VARNAME:            in, required, type=string/object
;                           Name of the variable to be clobbered.
;-
pro MrVar_ReadCDF_Clobber, varname
	compile_opt idl2
	on_error, 2

	common MrVar_ReadCDF_common, cdf_vnames, cdf_vcount, file_vnames, file_vcount, $
	                             cache_vnames, vsuffix
	
	;Get the variable
	iRemove = where(cdf_vnames eq varname, nRemove, COMPLEMENT=iKeep, NCOMPLEMENT=nKeep)
	if nRemove gt 1 then message, 'Duplicate variable names found'
	
	;Remove the variables
	if nRemove gt 0 then begin
		;Remove and delete variable from cache
		MrVar_Delete, cache_vnames[iRemove]
	
		;Remove variable name from common block variables
		cache_vnames[iRemove] = ''
		cdf_vnames[iRemove]   = ''
		cdf_vcount           -= 1
		
		;Reorder
		iSpace = where(cdf_vnames eq '', nSpace, COMPLEMENT=iName, NCOMPLEMENT=nName)
		if nSpace gt 0 && nName gt 0 then begin
			cache_vnames = [cache_vnames[iName], cache_vnames[iSpace]]
			cdf_vnames   = [cdf_vnames[iName],   cdf_vnames[iSpace]]
		endif
	endif
	
	;Reorganize the variables from the current file as well.
	iRemove = where(file_vnames eq varname or file_vnames eq '', nRemove, COMPLEMENT=iKeep, NCOMPLEMENT=nKeep)
	if nRemove gt 0 then begin
		file_vnames[iRemove]  = ''
		file_vcount          -= 1
		if nKeep gt 0 then file_vnames = [file_vnames[iKeep], file_vnames[iRemove]]
	endif
end


;+
;    Helper function for MrCDF_nRead. Read and append data.
;
; :Params:
;       CDFID:              in, required, type=string/long
;                           CDF identifier of the CDF file being read.
;       VARNAME:            in, required, type=string/object
;                           Name of the variable whose data will be read.
;
; :Keywords:
;       COUNT:              in, optional, type=intarr
;                           Vector containing the counts to be used when reading each
;                               `VALUE`. The default is to read each record, taking
;                               into account `INTERVAL` and `OFFSET`.
;       NO_CLOBBER:         in, optional, type=boolean, default=0
;                           If set, variables with names that already exist in the
;                               MrVariable cache will be renamed by appending '_#' to
;                               the variable name, where "#" represents a number.
;       VARINQ:             out, optional, type=structure
;                           Result of CDF_VarInq() on the variable for which data is
;                               returned.
;-
function MrVar_ReadCDF_GetData, cdfID, varname, $
NO_CLOBBER = no_clobber, $
VARINQ = varinq
	compile_opt idl2
	on_error, 2
	
	common MrVar_ReadCDF_common, cdf_vnames, cdf_vcount, file_vnames, file_vcount, $
	                             cache_vnames, vsuffix
	
;-----------------------------------------------------
; Read Variable Data \\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\
;-----------------------------------------------------
	;Number of records -- read all
	cdf_control, cdfID, GET_VAR_INFO=var_info, VARIABLE=varname
	rec_count = var_info.maxrec + 1
	
	;
	; If REC_COUNT=0, CDF_VarGet will issue an error. However, if we undefine
	; REC_COUNT, it will read a PadValue and issue a warning. Here, we choose
	; to read in zero records
	;
	
	if rec_count gt 0 then begin
		;Do not show annoying cdf_varget warnings
		!Quiet = 1

		;Get its data
		cdf_varget, cdfID, varname, data, $
		            REC_COUNT = rec_count, $
		            /STRING

		;Turn on normal warnings.
		!Quiet = 0
	endif else begin
		MrPrintF, 'logwarn', 'Variable has zero records: "' + varname + '".'
	endelse

;-----------------------------------------------------
; Book-keeping \\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\
;-----------------------------------------------------

	;Store variables from current file
	;   - Collection is necessary once per file when data has been read
	file_vnames[file_vcount]  = varname
	file_vcount              += 1
	
	return, data
end


;+
;   Helper function for MrVar_ReadCDF. Retrieve or create a variable.
;
;   1. Check if the variable has been read
;     a. Get the name from the cache
;     b. Look for pre-existing variables with the same name
;         i. Clobber DEPEND_0 attributes
;        ii. Create new variable -- clobber pre-existing var if necessary
;       iii. Keep a record of the variable so we know it has been created
;   2. Return variable
;     
;
;   NOTES:
;       Uses the MrVar_ReadCDF_common common block
;
; :Params:
;       VARNAME:            in, required, type=string
;                           Name of the CDF variable to retrieve or create.
;       VARTYPE:            in, optional, type=string, type='MrVariable'
;                           Type of varible to be created. Should be a subclass
;                               of MrVariable: {'MrScalarTS' | 'MrVectorTS' | 'MrMatrixTS'}
;
; :Keywords:
;       NO_CLOBBER:         in, required, type=boolean, default=0
;                           If set, variables will be renamed if one with the
;                               same name already exists in the cache.
;-
function MrVar_ReadCDF_GetVar, vname, vtype, $
NO_CLOBBER=no_clobber, $
SUFFIX=suffix
	compile_opt idl2
	on_error, 2
	
	common MrVar_ReadCDF_common, cdf_vnames, cdf_vcount, file_vnames, file_vcount, $
	                             cache_vnames, vsuffix
	
	;Defaults
	;   - Clobber existing variables
	tf_clobber = ~keyword_set(no_clobber)
	
	;Has the variable been read yet?
	tf_has = max(cdf_vnames eq vname, imax)

;-----------------------------------------------------
; Get Existing Variable \\\\\\\\\\\\\\\\\\\\\\\\\\\\\\
;-----------------------------------------------------
	if tf_has then begin
		var = MrVar_Get(cache_vnames[imax])

;-----------------------------------------------------
; Create New Variable \\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\
;-----------------------------------------------------
	endif else begin

	;-----------------------------------------------------
	; Clobber Duplicate Name \\\\\\\\\\\\\\\\\\\\\\\\\\\\\
	;-----------------------------------------------------
		;Check if there is a variable by the same name already in the cache
		old_var = MrVar_Get(vname+vsuffix, COUNT=count)

		;If there is, and we clobber, then clobber its DEPEND_# attributes
		if count gt 0 && tf_clobber then begin
			;Step through each possible DEPEND_N
			for i = 0, 3 do begin
				;Look for dependent variable
				depN    = 'DEPEND_' + string(i, FORMAT='(i0)')
				depName = old_var -> GetAttrValue(depN, /NULL)

				;Remove dependent variable
				;   - If the variable has already been read, then it has
				;     already been sufficiently clobbered.
				if depName ne !Null && ~MrIsMember(cache_vnames, depName) then begin
					MrVar_Delete, depName
					old_var -> RemoveAttr, depN
				endif
			endfor
		endif

	;-----------------------------------------------------
	; Create & Cache Variable \\\\\\\\\\\\\\\\\\\\\\\\\\\\
	;-----------------------------------------------------
		;We do not want to create new variables every time the same
		;file is read, so default to clobbering any pre-existing
		;variable by the same name.
		oVar = obj_new( vtype, $
		                /CACHE, $
		                NAME       = vname + vsuffix, $
		                NO_CLOBBER = ~tf_clobber )
		
		;Create an attribute
		oVar -> AddAttr, 'CDF_NAME', vname
		
		;Keep track of variable information
		;   - Collection is necessary only once per call (i.e. not for every file)
		;   - Unique variable names across all files being read.
		cdf_vnames[cdf_vcount]    = vname
		cache_vnames[cdf_vcount]  = oVar.name
		cdf_vcount               += 1
	endelse

	;Return the variable
	return, oVar
end


;+
;    Helper function for MrCDF_nRead. Read and append data.
;
; :Params:
;       CDFID:              in, required, type=string/long
;                           CDF identifier of the CDF file being read.
;       VARNAME:            in, required, type=string/object
;                           Name of the variable whose data will be read.
;-
pro MrVar_ReadCDF_GetVarAttrs, cdfID, oVar
	compile_opt idl2
	on_error, 2

	common MrVar_ReadCDF_common, cdf_vnames, cdf_vcount, file_vnames, file_vcount, $
	                             cache_vnames, vsuffix

	;Number of attributes
	;   - Variable attributes should be saved after global attributes
	;   - I have seen some files where this is not the case
	;   - So, loop over all attributes
	cdfinq  = cdf_inquire(cdfID)
	varname = oVar['CDF_NAME']

;-----------------------------------------------------
; Loop Over Attributes \\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\
;-----------------------------------------------------
	for i = 0, cdfinq.natts - 1 do begin
		;Check if the variable has the attribute
		;   - Skip the attribute if it does not exist for this variable
		cdf_attget_entry, cdfID, i, varname, cdf_type, attrValue, tf_exists, ATTRIBUTE_NAME=attrName
		if tf_exists eq 0 then continue

	;-----------------------------------------------------
	; DEPEND_[0-3] or LABL_PTR_[1-3] \\\\\\\\\\\\\\\\\\\\\
	;-----------------------------------------------------
		if stregex(attrName, '(DEPEND|LABL_PTR)_[0-3]', /BOOLEAN) then begin
			;
			; The DEPEND_[0-3] attribute value is the name of a CDF
			; variable. We want to mimick this behavior in the
			; MrVariable cache. So, we read the DEPEND_# variable
			; into the cache as if it were a regular variable, then
			; set the DEPEND_# attribute value to its MrVariable name.
			;
			; Like DEPEND_#, LABL_PTR_# is a pointer to another CDF variable.
			; Unlike DEPEND_#, LABL_PTR_# attribute will not become a pointer
			; to a variable in the MrVariable cache. Instead, follow LABL_PTR_#
			; to its data and use that as the attribute value.
			;

			;ATTRVALUE is the name of the variable that serves as DEPEND_[0-3]
			;   - Check if the variable has yet been read.
			iName = where(file_vnames eq attrValue, nName)
			if nName eq 0 then begin
				;Read the data
				;   - Creates and caches the variable
				;   - Do not clobber other DEPEND_[0-3] data. They often
				;     have the same name (e.g. Epoch)
				oAttr = MrVar_ReadCDF_ReadVar( cdfID, attrValue, /NO_CLOBBER )
			endif else begin
				oAttr = MrVar_ReadCDF_GetVar(attrValue)
			endelse

			; Delete the LABL_PTR_# variable from the cache
			;   - But only if it has not already been read into the cache
			;   - If /SUPPORT_DATA is set, it will be read into the cache as a variable
			if nName eq 0 && stregex(attrName, 'LABL_PTR_[1-3]', /BOOLEAN) then begin
				oVar -> SetAttrValue, attrName, oAttr['DATA'], /CREATE
				MrVar_ReadCDF_Clobber, attrValue
			
			;Add DEPEND_0 to the variable attributes
			endif else begin
				oVar -> SetAttrValue, attrName, oAttr.name, /CREATE
			endelse

	;-----------------------------------------------------
	; Other Attributes \\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\
	;-----------------------------------------------------
		endif else begin
			;We have an attribute-value pair (i.e. not a pointer to a variable)
			oVar -> SetAttrValue, attrName, attrValue, /CREATE
		endelse
	endfor
end


;+
;    Determine the type of MrVariable object to create.
;
; :Private:
;
; :Params:
;       CDFID:              in, required, type=string/long
;                           CDF identifier of the CDF file being read.
;       VARNAME:            in, required, type=string/object
;                           Name of the variable whose data will be read.
;
; :Returns:
;       VARTYPE:            out, required, type=string
;                           The type of MrVariable object that best represents
;                               data of `VARNAME`.
;-
function MrVar_ReadCDF_GetVarType, cdfID, varname
	compile_opt idl2
	on_error, 2
	
	;What type of variable is it?
	;   - For zero-dimensional CDFs, DIMVAR will have one element whose value is zero.
	;   - Variables are still read in as 1xN
	varinq = cdf_varinq(cdfID, varname)
	nDims  = n_elements(varinq.dimvar)
	
	;Is there a DEPEND_0 attribute?
	cdf_attget_entry, cdfID, 'DEPEND_0', varname, attr_type, dep0_vname, tf_exists
	if tf_exists eq 0 && MrIsMember(['CDF_EPOCH', 'CDF_EPOCH16', 'CDF_TIME_TT2000'], attr_type) $
		then tf_timeseries = 1B $
		else tf_timeseries = 0B
	
	;Scalar or time
	if nDims eq 1 && varinq.dimvar[0] eq 0 then begin
		;Is the variable an epoch datatype?
		if MrIsMember(['CDF_EPOCH', 'CDF_EPOCH16', 'CDF_TIME_TT2000'], varinq.datatype) $
			then vartype = 'MrTimeVar' $
			else vartype = tf_timeseries ? 'MrScalarTS' : 'MrVariable'
	
	;Vector
	endif else if nDims eq 1 && varinq.dim[0] eq 3 then begin
		vartype = tf_timeseries ? 'MrVectorTS' : 'MrVariable'
	
	;Matrix
	;   - TODO: Should ANY 2D variable be a matrix?
	;           Could check for a "TENSOR_ORDER" attribute.
	endif else if nDims eq 2 then begin
		vartype = tf_timeseries ? 'MrMatrixTS' : 'MrVariable'
	
	;Other
	;   - TODO: Transpose time to leading dimension?
	endif else begin
		vartype = tf_timeseries ? 'MrTimeSeries' : 'MrVariable'
	endelse
	
	return, vartype
end


;+
;   Helper function for MrVar_ReadCDF. Retrieve or create a variable.
;
;   1. Check if the variable has been read
;     a. Get the name from the cache
;     b. Look for pre-existing variables with the same name
;         i. Clobber DEPEND_0 attributes
;        ii. Create new variable -- clobber pre-existing var if necessary
;       iii. Keep a record of the variable so we know it has been created
;   2. Return variable
;     
;
;   NOTES:
;       Uses the MrVar_ReadCDF_common common block
;
; :Params:
;       VARNAME:            in, required, type=string
;                           Name of the CDF variable to retrieve or create.
;       VARTYPE:            in, optional, type=string, type='MrVariable'
;                           Type of varible to be created. Should be a subclass
;                               of MrVariable: {'MrScalarTS' | 'MrVectorTS' | 'MrMatrixTS'}
;
; :Keywords:
;       NO_CLOBBER:         in, required, type=boolean, default=0
;                           If set, variables will be renamed if one with the
;                               same name already exists in the cache.
;-
function MrVar_ReadCDF_ReadVar, cdfID, varname, $
NO_CLOBBER=no_clobber, $
SUFFIX=suffix
	compile_opt idl2
	on_error, 2
	
	common MrVar_ReadCDF_common, cdf_vnames, cdf_vcount, file_vnames, file_vcount, $
	                             cache_vnames, vsuffix

;-----------------------------------------------------
; Read Data & Attributes \\\\\\\\\\\\\\\\\\\\\\\\\\\\\
;-----------------------------------------------------
	;Which MrVariable type?
	vartype = MrVar_ReadCDF_GetVarType(cdfID, varname)
	
	;Get a variable object
	theVar = MrVar_ReadCDF_GetVar( varname, vartype, $
	                               NO_CLOBBER = no_clobber, $
	                               SUFFIX     = suffix )
	
	;Read attributes
	MrVar_ReadCDF_GetVarAttrs, cdfID, theVar
	
	;Create IDL graphics keywords attributes
	MrVar_ReadCDF_VarAttr2GfxKwd, varname
	
	;Read data
	data = MrVar_ReadCDF_GetData(cdfID, varname)

;-----------------------------------------------------
; Add Attributes \\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\
;-----------------------------------------------------

	;Save the CDF datatype as well
	varinq = cdf_varinq(cdfID, varName)
	oVar -> SetAttrValue, 'CDF_TYPE', varinq.datatype, /CREATE

;-----------------------------------------------------
; Append Data \\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\
;-----------------------------------------------------
	;
	; Record variance?
	;   - Append to record-varying dimension (always after the last CDF dimension)
	;   - ::Append can handle undefined data.
	; No record variance
	;   - Overwrite data
	;   - Let variables with no data remain as-is
	;
	
	;Append
	if varinq.recvar eq 'VARY' $
		then data = [theVar['DATA'], data]

	;Set the data
	case vartype of
		'MrVariable':   theVar -> SetData, data
		'MrTimeVar':    theVar -> SetData, data, varinq.datatype, /NO_COPY
		'MrTimeSeries': theVar -> SetData, theVar['DEPEND_0'], data, /NO_COPY
		'MrScalarTS':   theVar -> SetData, theVar['DEPEND_0'], data, /NO_COPY
		'MrVectorTS':   theVar -> SetData, theVar['DEPEND_0'], data, /NO_COPY
		'MrMatrixTS':   theVar -> SetData, theVar['DEPEND_0'], data, /NO_COPY
		else: message, 'Unknown variable type: "' + vartype + '".'
	endcase
	
	return, theVar
end


;+
;    Helper function for MrCDF_nRead. Read and append data.
;
; :Params:
;       CDFID:              in, required, type=string/long
;                           CDF identifier of the CDF file being read.
;       VARNAME:            in, required, type=string/object
;                           Name of the variable whose data will be read.
;-
pro MrVar_ReadCDF_VarAttr2GfxKwd, varname
	compile_opt idl2
	on_error, 2

	;Get the variable
	var  = MrVar_ReadCDF_GetVar(varname)
	
	;PLOT_TITLE
	if ~var -> HasAttr('PLOT_TITLE') then begin
		case 1 of
			var -> HasAttr('FIELDNAM'): var -> AddAttr, 'PLOT_TITLE', var['FIELDNAM']
			var -> HasAttr('CATDESC'):  var -> AddAttr, 'PLOT_TITLE', var['CATDESC']
			else: ;Do nothing
		endcase
	endif

	;TITLE (Axis)
	if ~var -> HasAttr('TITLE') then begin
		case 1 of
			var -> HasAttr('LABLAXIS'): title = var['LABLAXIS']
			var -> HasAttr('FIELDNAM'): title = var['FIELDNAM']
			else: ;Do nothing
		endcase
		if var -> HasAttr('UNITS') then title = (title eq '') ? var['UNITS'] : title + '!C' + var['UNITS']
		var -> AddAttr, 'TITLE', temporary(title)
	endif
	
	;LABEL (Legend)
	if ~var -> HasAttr('LABEL') then begin
		if var -> HasAttr('LABL_PTR_1') then var -> AddAttr, 'LABEL', var['LABL_PTR_1']
	endif
	
	;MIN_VALUE
	if ~var -> HasAttr('MIN_VALUE') then begin
		if var -> HasAttr('VALIDMIN')   then var -> AddAttr, 'MIN_VALUE', var['VALIDMIN']
	endif
	
	;MAX_VALUE
	if ~var -> HasAttr('MAX_VALUE') then begin
		if var -> HasAttr('VALIDMAX')   then var -> AddAttr, 'MAX_VALUE', var['VALIDMAX']
	endif
	
	;AXIS_RANGE
	if ~var -> HasAttr('AXIS_RANGE') then begin
		if var -> HasAttr('SCALEMIN') && var -> HasAttr('SCALEMIN') $
			then VAR -> AddAttr, 'AXIS_RANGE', [var['SCALEMIN'], var['SCALEMAX']]
	endif
	
	;DIMENSION
	if obj_isa(var, 'MrVectorTS') then begin
		var -> AddAttr, 'DIMENSION', 1
		var -> AddAttr, 'COLOR', ['Blue', 'Forest Green', 'Red']
	endif

	;SCALETYP
	;   - 'log' or 'linear'
	if ~var -> HasAttr('LOG') then begin
		if var -> HasAttr('SCALETYP') then begin
			tf_log = var['SCALETYP'] eq 'log' ? 1 : 0
			var -> AddAttr, 'LOG', tf_log
		endif
	endif
end


;+
;    Helper function for MrVar_ReadCDF. Restrict time range.
;
; :Private:
;
; :Params:
;       TRANGE:         in, required, type=strarr(2)
;                       Time interval by which to restrict data, formatted as
;                           ISO-8601 date-time strings.
;-
pro MrVar_ReadCDF_TLimit, trange
	compile_opt idl2
	on_error, 2

	common MrVar_ReadCDF_common, cdf_vnames, cdf_vcount, file_vnames, file_vcount, $
	                             cache_vnames, vsuffix

;-----------------------------------------------------
; Trim in Time \\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\
;-----------------------------------------------------
	;Store the dependent variables
	ndep0      = 0
	dep0_names = strarr(cdf_vcount)

;-----------------------------------------------------
; Data Variables First \\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\
;-----------------------------------------------------
	;Find epoch variables
	nDep0       = 0
	epoch_names = strarr(cdf_vcount)
	for i = 0, cdf_vcount - 1 do begin
		theVar = MrVar_Get(cache_vnames[i])
		if obj_isa(theVar, 'MrTimeVar') then begin
			epoch_names[nDep0]  = theVar.name
			nDep0              += 1
		endif
	endfor
	
	;Step over each epoch variable
	for i = 0, nDep0 - 1 do begin
		;Get the variable
		epochVar = MrVar_Get(epoch_names[i])
		
		;Convert back to an epoch type
		epoch_type = epochVar['CDF_TYPE']
		case epoch_type of
			'CDF_TIME_TT2000': range = epochVar -> iso2tt2000(trange)
			'CDF_EPOCH_LONG':  range = epochVar -> iso2epoch16(trange)
			'CDF_EPOCH':       range = epochVar -> iso2tepoch(trange)
			else: message, 'Unknown datatype (' + epoch_type + ') for epoch variable "' + epochVar.name + '".'
		endcase
		time = epochVar -> GetData(epoch_type)
		
		;Get the index range
		idx = where(MrCDF_Epoch_Compare(temporary(time), range[0], range[1]), nKeep)
		if nKeep eq 0 then message, 'No data found in time interval for variable "' + epochVar.name + '".'
		
		;Step over each variable
		for j = 0, cdf_vcount - 1 do begin
			;Get the variable
			;   - Skip if the variable does not have an DEPEND_0 attribute
			;   - Skip if the DEPEND_0 attribute is not the one we are looking at
			theVar = MrVar_Get(cache_vnames[j])
			if ~theVar -> HasAttr('DEPEND_0')      then continue
			if theVar['DEPEND_0'] ne epochVar.name then continue

			;Trim the variable data
			case 1 of
				obj_isa(theVar, 'MrScalarTS'): theVar -> SetData, theVar[idx]
				obj_isa(theVar, 'MrVectorTS'): theVar -> SetData, theVar[idx,*]
				obj_isa(theVar, 'MrTensorTS'): theVar -> SetData, theVar[idx,*,*]
				theVar -> HasAttr('DEPEND_3'): theVar -> SetData, theVar[*,*,*,idx]
				theVar -> HasAttr('DEPEND_2'): theVar -> SetData, theVar[*,*,idx]
				theVar -> HasAttr('DEPEND_1'): theVar -> SetData, theVar[*,idx]
				else: begin
					case theVar.n_dimensions of
						1: theVar -> SetData, theVar[idx]
						2: theVar -> SetData, theVar[*,idx]
						3: theVar -> SetData, theVar[*,*,idx]
						4: theVar -> SetData, theVar[*,*,*,idx]
						else: message, 'Variable has unexpected number of dimensions: "' + theVar.name + '".'
					endcase
				endcase
			endcase
		endfor
		
		;Trim the time data
		epochVar -> SetData, epochVar[idx]
	endfor
end


;+
;   A more robust method for obtaining variable data, when compared to the GetVarData
;   method.
;
; :Params:
;       FILE:               in, required, type=string/long
;                           The CDF identifier retured by CDF_Open() of the name of a CDF
;                               file from which data is read.
;
; :Keywords:
;       NO_CLOBBER:         in, optional, type=boolean, default=0
;                           If set, variables with names that already exist in the
;                               MrVariable cache will be renamed by appending '_#' to
;                               the variable name, where "#" represents a number.
;       SUFFIX:             in, optional, type=string, default=''
;                           A suffix to be appended to each variable name.
;       SUPPORT_DATA:       in, optional, type=boolean, default=0
;                           If set, support data will be read as well. Regardless of
;                               the status of this keyword, variable data associated
;                               with DEPEND_# variables will be read. This keyword
;                               is ignored if `VARFORMAT` is set.
;       TRANGE:             in, optional, type=strarr(2), default=MrVar_GetTRange()
;                           The time range over which to read data, formatted as ISO-8601
;                               strings: 'YYYY-MM-DDThh:mm:ss'.
;       STRING:             in, optional, type=boolean, default=1
;                           If set, CDF_CHAR and CDF_UCHAR will be read as strings. This
;                               is the default. STRING is ignored for other datatypes.
;       VALIDATE:           in, optional, type=boolean, default=0
;                           If set, the CDF file will be validated when opened.
;       VARFORMAT:          in, optional, type=string/strarr, default='*'
;                           Variable names that match this search pattern will be read.
;                               Any wildcard accepted by IDL's StrMatch function can be
;                               used.
;       VARNAMES:           out, optional, type=string/strarr
;                           Names of the variables that were read and cached. Names are
;                               as they exist in the cache, not necessarily as they are
;                               in the CDF file.
;       VERBOSE:            out, optional, type=boolean, default=0
;                           If set, status messages will be printed to standard output.
;-
pro MrVar_ReadCDF, files, $
NO_CLOBBER=no_clobber, $
SUFFIX=suffix, $
SUPPORT_DATA=support_data, $
TRANGE=trange, $
VALIDATE=validate, $
VARFORMAT=varformat, $
VARNAMES=varnames, $
VERBOSE=verbose
	compile_opt idl2
	
	common MrVar_ReadCDF_common, cdf_vnames, cdf_vcount, file_vnames, file_vcount, $
	                             cache_vnames, vsuffix

	;Get the time range (Will throw error if not set.)
	if n_elements(trange) eq 0 then begin
		catch, the_error
		if the_error eq 0 $
			then trange = MrVar_GetTRange() $
			else trange = ['', '']
		catch, /CANCEL
	endif

	;catch errors
	catch, the_error
	if the_error ne 0 then begin
		catch, /CANCEL
	
		;Check that warnings are turned back on
		!Quiet = quiet_in
		if status eq 0 then status = 1
	
		;Close the file
		if n_elements(cdfID) gt 0 && tf_open then cdf_close, cdfID

		;Turn file validation back on
		if MrCmpVersion('8.0') le 0 then $
			if validate eq 0 then cdf_set_validate, /YES
		
		;Issue error
		if ~arg_present(status) then MrPrintF, 'LogErr'
		return
	endif

	;Defaults
	quiet_in   = !Quiet
	status     = 0
	tstart     = trange[0]
	tend       = trange[1]
	validate   = keyword_set(validate)
	tf_support = keyword_set(support_data)
	tf_verbose = keyword_set(verbose)
	nvarfmt    = n_elements(varformat)
	vsuffix    = n_elements(suffix) eq 0 ? '' : suffix
	
;-----------------------------------------------------
; CDF file names or IDs \\\\\\\\\\\\\\\\\\\\\\\\\\\\\\
;-----------------------------------------------------

	;CDF IDs given?
	if isa(files, 'LONG') then begin
		tf_IDs = 1B
	endif else begin
		tf_IDs = 0B
	
		;Validate the files?
		if MrCmpVersion('8.0') le 0 then begin
			if validate $
				then cdf_set_validate, /YES $
				else cdf_set_validate, /NO
		endif
	endelse
	
	;Allocate memory
	;   - Resets common block variables
	;   - We have to keep track of three sets of variable names:
	;       1) All unique CDF variable names from all files
	;       2) All variable names, as they are in the Cache
	;       3) Variable names from the current file
	;   - #2 is to avoid name conflicts with variable already in the Cache
	;   - #3 is to avoid reading DEPEND_[0-3] variables more than once
	nalloc       = 100
	cache_vnames = strarr(nalloc)   ;All cache names
	cdf_vnames   = strarr(nalloc)   ;All CDF names
	timevar      = bytarr(nalloc)
	cdf_vcount   = 0

;-----------------------------------------------------
; Step Through Each File \\\\\\\\\\\\\\\\\\\\\\\\\\\\\
;-----------------------------------------------------
	for i = 0, n_elements(files) - 1 do begin
		;Open the file
		if tf_IDs then begin
			cdfID   = files[i]
			tf_open = 0B
			if tf_verbose then MrPrintF, 'LogText', files[i], FORMAT='(%"Reading CDF file with ID %i.")'
		endif else begin
			cdfID   = cdf_open(files[i])
			tf_open = 1B
			if tf_verbose then MrPrintF, 'LogText', files[i], FORMAT='(%"Reading file \"%s\".")'
		endelse
		
		;Number of variables in the file
		cdf_info    = cdf_inquire(cdfID)
		nVars       = cdf_info.nvars + cdf_info.nzvars
		file_vnames = strarr(nVars)      ;Names in this CDF file
		file_vcount = 0                  ;Counter for this CDF

	;-----------------------------------------------------
	; Step Through Each Variable \\\\\\\\\\\\\\\\\\\\\\\\\
	;-----------------------------------------------------
		for j = 0, nVars-1 do begin
			;Variable index
			if j ge cdf_info.nvars then begin
				ivar   = j - cdf_info.nvars
				isZVar = 1B
			endif else begin
				ivar   = j
				isZVar = 0B
			endelse

			;Get the variable name and other info
			;   - This is possible if, e.g., the variable is a DEPEND_0
			;     variable attribute.
			varinq = cdf_varinq(cdfID, iVar, ZVARIABLE=isZVar)
			
			;Skip if it has already been read
			if MrIsMember(file_vnames, varinq.name) $
				then continue

			;Skip if it does not match VARFORMAT
			if nvarfmt ge 1 then begin
				tf_strmatch = 0B
				if nvarfmt eq 1 $
					then tf_strmatch = strmatch(varinq.name, varformat) $
					else for k = 0, nvarfmt-1 do tf_strmatch >= strmatch(varinq.name, varformat[k])
				if ~tf_strmatch then CONTINUE
			
			;Skip support data
			endif else if ~tf_support then begin
				cdf_attget_entry, cdfID, 'VAR_TYPE', varinq.name, cdf_type, var_type, tf_exists
				if tf_exists && var_type ne 'data' then CONTINUE
			endif
			
			;Read data and attributes
			!Null = MrVar_ReadCDF_ReadVar( cdfID, varinq.name, $
			                               NO_CLOBBER = no_clobber, $
			                               SUFFIX     = suffix )
		endfor
		
		;Close the data file
		if tf_open then begin
			cdf_close, cdfID
			tf_open = 0B
			cdfID   = 0LL
		endif
	endfor

	;Trim results
	if cdf_vcount eq 0 then begin
		cache_vnames = ''
	endif else begin
		cdf_vnames   = cdf_vnames[0:cdf_vcount-1]
		cache_vnames = cache_vnames[0:cdf_vcount-1]
	endelse

;-----------------------------------------------------
; Restrict Time Range \\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\
;-----------------------------------------------------
	if ~array_equal(trange, '') && cdf_vcount gt 0 $
		then MrVar_ReadCDF_TLimit, trange

;-----------------------------------------------------
; Return \\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\
;-----------------------------------------------------
	;Return variable names
	if arg_present(varnames) then varnames = cache_vnames

	;Turn file validation back on
	if MrCmpVersion('8.0') le 0 then $
		if validate eq 0 then cdf_set_validate, /YES
end
