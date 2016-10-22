; docformat = 'rst'
;
; NAME:
;   MrTimeSeries__Define
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
; PURPOSE
;+
;   The purpose of this class is subclass the IDL_Object such that all _Overload*
;   operations behave as they would for a normal array.
;
;   _OVERLOAD:
;       An operator (such as AND, EQ, ^, *, etc.) calls the overload
;       method for the object on the left side first. So, for two MrTimeSeries objects,
;       A and B::
;
;               print, A [operator] B
;
;       will call A's _Overload method first.
;
;   ARRAY TRUNCATION
;       If two arrays are compared, results will be truncated to have the same number
;       of elements as the shorter array. If an array and a scalar are compared, each
;       elements of the array is compared against the scalar value. This is the same
;       as for normal IDL variables.
;
;   CACHING
;       The ::Cache method will cache the MrTimeSeries object. When the object is destroyed,
;       it will automatically be removed from the cache.
;
; :Categories:
;   MrTimeSeries, Graphics
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
;       2016/07/01  -   Written by Matthew Argall
;-
;*****************************************************************************************
;+
;   The initialization method.
;
; :Params:
;       TIME:           in, required, type=NxM array
;                       Name or reference of a MrTimeVar object, or an array
;                           of time stamps. If a name is provided, the assiciated
;                           variable must exist in the variable cache.
;       DATA:           in, required, type=NxM array
;                       Name or reference of a MrVariable object, or the dependent
;                           variable data. If a name is given, the associated variable
;                           must exist in the variable cache. 
;
; :Keywords:
;       CACHE:          in, optional, type=boolean, default=0
;                       If set, both `TIME` and `DATA` are added to the variable cache.
;       DIMENSION:      in, optional, type=integer
;                       The time-dependent, 1-based dimension of `DATA`. If not provided,
;                           the dimension of `DATA` that is equal in size to `TIME` is
;                           chose as the default.
;       NAME:           in, optional, type=integer
;                       Name to be given to the variable object.
;       NO_CLOBBER:     in, optional, type=boolean, default=0
;                       If set, do not clobber variables of the same name. Instead,
;                           rename this variable by appending "_#" to `NAME`, where
;                           "#" represents a unique number. Ignored unless `CACHE` is set.
;       NO_COPY:        in, optional, type=boolean, default=0
;                       If set `DATA` will be copied directly into the object
;                           and will be left undefined (a MrTimeSeries object will not
;                           be destroyed, but its array will be empty).
;       T_TYPE:         in, optional, type=integer
;                       If `TIME` is an array of time stamps, use this keyword to indicate
;                           the format or time-basis. See MrTimeVar for more details.
;       T_NAME:         in, optional, type=integer
;                       Name to be given to the MrTimeVar object. Ignored unless `TIME`
;                           is an array of time stamps.
;-
function MrTimeSeries::INIT, time, data, $
CACHE=cache, $
DIMENSION=dimension, $
NAME=name, $
NO_CLOBBER=no_clobber, $
NO_COPY=no_copy, $
T_NAME=t_name, $
T_TYPE=t_type
	compile_opt idl2

	;Error handling
	catch, the_error
	if the_error ne 0 then begin
		catch, /CANCEL
		MrPrintF, 'LogErr'
		return, 0
	endif
	
	;Defaults
	if n_elements(name) eq 0 then name = 'MrTimeSeries'
	
	;Initialize superclass
	success = self -> MrVariable::Init()
	if ~success then message, 'Unable to initialize superclass.'
	
	;Set data
	if n_elements(time) gt 0 then begin
		self -> SetData, time, data, $
		                 DIMENSION = dimension, $
		                 NO_COPY   = no_copy, $
		                 T_TYPE    = t_type, $
		                 T_NAME    = t_name
	endif
	
	;Set name
	if n_elements(name) gt 0 then self -> SetName, name
	if keyword_set(cache)    then self -> Cache, NO_CLOBBER=no_clobber

	return, 1
end


;+
;   Clean up after the object is destroyed
;-
pro MrTimeSeries::CLEANUP
	compile_opt idl2
	on_error, 2
	
	;If the time variable is not cached, release it
	if obj_valid(self.oTime) && ~MrVar_IsCached(self.oTime)$
		then obj_destroy, self.oTime
	
	;Superclass
	self -> MrVariable::Cleanup
end


;+
;   Allow square-bracket array indexing from the right side of an operator.
;
;   Calling Sequence
;       oTSvar    = oTSvar[0]
;       t0        = oTSvar[[0]]
;       data      = oTSvar['DATA']
;       pData     = oTSvar['POINTER']
;       pData     = oTSvar['PTR']
;       isoTime   = oTSvar['TIME']
;       time      = oTSvar['TIME', T_TYPE]
;       attrValue = oTSvar[AttrName]
;       data      = oTSvar[i1 [, i2 [, i3 [, i4 [, i5 [, i6 [, i7 [, i8]]]]]]]]
;
; :Params:
;       ISRANGE:            in, required, type=intarr
;                           A vector that has one element for each Subscript argument
;                               supplied by the user; each element contains a zero if the
;                               corresponding input argument was a scalar index value or
;                               array of indices, or a one if the corresponding input
;                               argument was a subscript range.
;       I1:                 in, required, type=integer/intarr(3)
;                           Index subscripts. Either a scalar, an index array, or a 
;                               subscript range in the form [start, stop, step_size]
;       I2:                 in, optional, type=integer/intarr(3)
;                           Index subscripts.
;       I3:                 in, optional, type=integer/intarr(3)
;                           Index subscripts.
;       I4:                 in, optional, type=integer/intarr(3)
;                           Index subscripts.
;       I5:                 in, optional, type=integer/intarr(3)
;                           Index subscripts.
;       I6:                 in, optional, type=integer/intarr(3)
;                           Index subscripts.
;       I7:                 in, optional, type=integer/intarr(3)
;                           Index subscripts.
;       I8:                 in, optional, type=integer/intarr(3)
;                           Index subscripts.
;
; :Returns:
;       RESULT:             in, required, type=numeric array
;                           The subarray accessed by the input parameters.
;-
function MrTimeSeries::_OverloadBracketsRightSide, isRange, i1, i2, i3, i4, i5, i6, i7, i8
	compile_opt idl2
	on_error, 2

	;Number of subscripts given
	nSubscripts = n_elements(isRange)

	;String operations.
	if IsA(i1, /SCALAR, 'STRING') then begin
		case strupcase(i1) of
			'DATA':    return, *self.data
			'TIME':    return,  self.oTime -> GetData(i2)
			'POINTER': return,  self.data
			'PTR':     return,  self.data
			else:      return,  self -> GetAttrValue(i1)
		endcase

	;Scalar operations
	;   - 0   returns the self object
	;   - [0] returns the first data element
	;   - All other cases return data
	endif else if nSubscripts eq 1 && isRange[0] eq 0 && IsA(i1, /SCALAR) && i1 eq 0 then begin
		return, self
	endif

;---------------------------------------------------------------------
;Optimized Subscripting for <= 3D ////////////////////////////////////
;---------------------------------------------------------------------
	if (nSubscripts le 3) then begin
	;---------------------------------------------------------------------
	;3D Subscripts ///////////////////////////////////////////////////////
	;---------------------------------------------------------------------
		if IsA(i3) then begin 
			;Subscript range given: [min:max:interval]?
			if isRange[2] then begin 
				;Subscript range for dimensions 2 and 3?
				if isRange[1] then begin 
					;Range: [1,2,3], Index: --
					if isRange[0] then begin
						return, (*self.data)[i1[0]:i1[1]:i1[2],i2[0]:i2[1]:i2[2],i3[0]:i3[1]:i3[2]]
					;Range: [2,3], Index: 1
					endif else begin
						return, (*self.data)[i1,i2[0]:i2[1]:i2[2],i3[0]:i3[1]:i3[2]] 
					endelse
				;Index value for dimension 2?
				endif else begin
					;Range: [3,1], Index: 2
					if isRange[0] then begin 
						return, (*self.data)[i1[0]:i1[1]:i1[2],i2,i3[0]:i3[1]:i3[2]]
					;Range: 3, Index: [2,1]
					endif else begin 
						return, (*self.data)[i1,i2,i3[0]:i3[1]:i3[2]] 
					endelse 
				endelse
			;Index for dimension 3?
			endif else begin
				;Range for dimension 2?
				if isRange[1] then begin
					;Range: [2,1]
					if isRange[0] then begin 
						return, (*self.data)[i1[0]:i1[1]:i1[2],i2[0]:i2[1]:i2[2],i3] 
					endif else begin 
						return, (*self.data)[i1,i2[0]:i2[1]:i2[2],i3] 
					endelse 
				endif else begin 
					if isRange[0] then begin 
						return, (*self.data)[i1[0]:i1[1]:i1[2],i2,i3] 
					endif else begin 
						return, (*self.data)[i1,i2,i3] 
					endelse 
				endelse 
			endelse 
	;---------------------------------------------------------------------
	;2D Subscripts ///////////////////////////////////////////////////////
	;---------------------------------------------------------------------
		endif else if IsA(i2) then begin
			if isRange[1] then begin
				;[Range, Range]
				if isRange[0] then begin
					return, (*self.data)[i1[0]:i1[1]:i1[2],i2[0]:i2[1]:i2[2]]
				;[Index, Range]
				endif else begin
					return, (*self.data)[i1,i2[0]:i2[1]:i2[2]]
				endelse
			endif else begin
				;[Range, Index]
				if isRange[0] then begin
					return, (*self.data)[i1[0]:i1[1]:i1[2],i2]
				;[Index, Index]
				endif else begin
					return, (*self.data)[i1,i2]
				endelse
			endelse
	;---------------------------------------------------------------------
	;1D Subscripts ///////////////////////////////////////////////////////
	;---------------------------------------------------------------------
		endif else begin
			;Range?
			if isRange[0] then begin
				return, (*self.data)[i1[0]:i1[1]:i1[2]] 
			;Index
			;   - Compensate for passing in [0] instead of 0 for the first element.
			;   - i.e. return a scalar instead of a 1-element array
			endif else begin
				if n_elements(i1) eq 1 && i1 eq 0 $
					then return, (*self.data)[0] $
					else return, (*self.data)[i1]
			endelse
		endelse
	endif

;---------------------------------------------------------------------
; Brute Force Code for 4D or Higher Arrays. //////////////////////////
;---------------------------------------------------------------------
	;Works for any number of dimensions.
	dims = size(*self.data, /DIMENSIONS)
	indices = MrReformIndices(dims, isRange, i1, i2, i3, i4, i5, i6, i7, i8, DIMENSIONS=dimensions);, /IDL_METHOD)

	return, reform((*self.data)[indices], dimensions, /OVERWRITE)
end


;+
;   The purpose of this method is to provide iterations to the FOREACH operator. Iterate
;   over all of the elements in the array.
;
; :Params:
;       VALUE:              out, required, type=vector
;                           Set to the value of the current object element. If `KEY` is
;                               undefined, then set this to the first object element.
;       KEY:                out, required, type=long
;                           Set to the index (or key) associated with the current element.
;                               In the first iteration, KEY is undefined.
;
; :Returns:
;       NEXT:               Returns 1 if there is a current element to retrieve, and 0 if
;                               there are no more elements.
;-
function MrTimeSeries::_OverloadForeach, value, key
	compile_opt idl2
	on_error, 2

	nPts = n_elements( (*self.data)[*,0] )
	if n_elements(key) eq 0 then key = 0

	;Get the array element if the index is in range
	if key lt nPts then begin
		next  = 1
		nDims = size(*self.data, /DIMENSIONS)
		
		case nDims of
			1: value = reform((*self.data)[key]
			2: value = reform((*self.data)[key,*])
			3: value = reform((*self.data)[key,*,*])
			4: value = reform((*self.data)[key,*,*,*])
			5: value = reform((*self.data)[key,*,*,*,*])
			6: value = reform((*self.data)[key,*,*,*,*,*])
			7: value = reform((*self.data)[key,*,*,*,*,*,*])
			8: value = reform((*self.data)[key,*,*,*,*,*,*,*])
			else: message, 'Invalid number of dimensions (' + strtrim(nDims, 2) ').'
		endcase
	
	;Otherwise, stop iterating 
	endif else next = 0

	;Next element to retrieve
	key += 1

	return, next
end


;+
;   Cache the variable for later use
;
; :Keywords:
;       NO_CLOBBER:     in, optional, type=boolean, default=0
;                       If set, and if the variable's name clashes with that of another
;                           variable, the input variable will be renamed by appending
;                           "_#" to its name, where # represents an integer. The default
;                           is to replace the object already in the container.
;       NAME_OUT:       out, optional, type=string
;                       If `NO_CLOBBER` is set, then this is the new name of `VARIABLE`
;                           after it is renamed. If `VARIABLE` is not renamed, the
;                           original name is returned.
;-
pro MrTimeSeries::Cache, $
NO_CLOBBER=no_clobber, $
NAME_OUT=name_out
	compile_opt idl2
	on_error, 2

	;Setup caching store
	@mrvar_common
	
	;Create a cache in which to store MrVariables
	if ~obj_valid(MrVarCache) then MrVarCache = MrVariable_Cache()

;-------------------------------------------------------
; Cache Time ///////////////////////////////////////////
;-------------------------------------------------------
	if ~MrVarCache -> IsContained(self.oTime) then begin
		MrVarCache -> Add, self.oTime, CLOBBER=no_clobber, NAME_OUT=tname_out
		self       -> SetAttrValue, 'DEPEND_0', tname_out
	endif

;-------------------------------------------------------
; Cache Variable ///////////////////////////////////////
;-------------------------------------------------------
	;Add the array to the end of the container
	MrVarCache -> Add, self, NO_CLOBBER=no_clobber, NAME_OUT=name_out
end


;+
;   Create a copy of the variable with a new heap identifier. Time, data,
;   and attributes are copied.
;
; :Params:
;       NAME:           in, optional, type=string, default=<name>+'_copy'
;                       Name of the variable copy.
;
; :Keywords:
;       CACHE:          in, optional, type=boolean, default=0
;                       If set, the copy will be added to the variable cache
;-
function MrTimeSeries::Copy, name, $
CACHE=cache
	compile_opt idl2
	on_error, 2

	;Defaults
	tf_cache = keyword_set(cache)
	if n_elements(name) eq 0 then name = self.name + '_copy'
	
	;Copy the data into a new object
	;   - Use Obj_Class() so subclasses can inherit the method.
	theCopy = obj_new( obj_class(self), self.oTime, *self.data, $
	                   NAME   = name, $
	                   T_NAME = self.oTime.name )

	;Copy the variable attributes
	self -> CopyAttrTo, theCopy
	
	;Cache the variable
	if tf_cache then theCopy -> Cache, /NO_CLOBBER
	return, theCopy
end


;+
;   Perform interpolation on regularly or irregularly vectors.
;
;   Calling Sequence:
;       varOut = MrVar1 -> Interpol(MrVar2)
;       varOut = MrVar1 -> Interpol(X, Xout)
;
; :Params:
;       XOUT:           in, required, type=Numeric array
;                       The new time values.
;       T_TYPE:         in, required, type=string, default='ISO-8601'
;                       The units or time-base of `XOUT`. See MrTimeVar for details.
;
; :Keywords:
;       LSQUADRATIC:    in, optional, type=boolean, default=0
;                       If set, use a least squares quadratic fit to a 4-point neighborhood.
;       NAN:            in, optional, type=boolean, default=0
;                       If set, ignore NaN values.
;       QUADRATIC:      in, optional, type=boolean, default=0
;                       If set, use a quadritic fit to a 3-point neighborhood.
;       SPLINE:         in, optional, type=boolean, default=0
;                       If set, use a cubic spline in a 3-point neighborhood.
;
; :Returns:
;       VAROUT:         out, required, type=object
;                       A MrTimeSeries object containing the interpolated data.
;-
function MrTimeSeries::Interpol, Xout, t_type, $
CACHE=cache, $
NAME=name, $
LSQUADRATIC=lsquadratic, $
NAN=nan, $
QUADRATIC=quadratic, $
SPLINE=spline
	compile_opt idl2
	on_error, 2
	
;-------------------------------------------------------
; MrVariable Object ////////////////////////////////////
;-------------------------------------------------------
	if IsA(Xout, 'MrVariable') then begin
		;Get the time variables
		oT    = self.oTime
		oTout = MrVar_Get(Xout['DEPEND_0'])
		
		;Reference time
		!Null = min( [ oT[0, 'JULDAY'], oTout[0, 'JULDAY'] ], iMin )
		if iMin eq 0 $
			then t_ref = oT[0] $
			else t_ref = oTout[0]
		
		;Convert to seconds since midnight
		t     = oT    -> GetData('SSM', t_ref)
		t_out = oTout -> GetData('SSM', t_ref)

;-------------------------------------------------------
; Normal Arrays ////////////////////////////////////////
;-------------------------------------------------------
	endif else begin
		;Convert time to
		t     = self.oTime -> fromISO(t_type)
		t_out = Xout
	endelse

;-------------------------------------------------------
; Interpolate Scalar //////////////////////////////////////
;-------------------------------------------------------
	nDims = size(self, /N_DIMENSIONS)
	dims  = size(self, /DIMENSIONS)
	
	if nDims eq 1 then begin
		y_out = Interpol( *self.data, temporary(t), temporary(t_out), $
		                  LSQUADRATIC = lsquadratic, $
		                  NAN         = nan, $
		                  QUADRATIC   = quadratic, $
		                  SPLINE      = spline)

;-------------------------------------------------------
; Interpolate Array ////////////////////////////////////
;-------------------------------------------------------
	endif else begin
		;Reform Y so that
		;   [ time, everything else ]
		nOut    = n_elements(t_out)
		allDims = product( dims[1:*] )
		y       = reform( *self.data, [dims[0], allDims] )
		y_out   = make_array( nOut, allDims, TYPE=Xout[0] )
		
		;Interpolate all
		for i = 0, allDims - 1 do begin
			y_out[0,i] = Interpol( y[*,i], t, t_out, $
			                       LSQUADRATIC = lsquadratic, $
			                       NAN         = nan, $
			                       QUADRATIC   = quadratic, $
			                       SPLINE      = spline)
		endfor
		
		;Free up memory
		Y = !Null
		t = !Null
		
		;Reform back
		Yout = reform(Yout, [nOut, dims[1:*], /OVERWRITE)
	endelse

;-------------------------------------------------------
; Create New Variable //////////////////////////////////
;-------------------------------------------------------
	
	;Create a new variable with the same attributes
	varOut = self -> Copy(name, CACHE=cache)
	
	;Set the data
	if obj_valid(oTout) $
		then varOut -> SetData, oTout, temporary(y_out), T_TYPE=t_type $
		else varOut -> SetData, t_out, y_out, T_TYPE=t_type, /NO_COPY
	
	;Return the new variable
	return, varOut
end


;+
;   Set the array.
;
;   CALLING SEQUENCE:
;       oTS -> SetData, data
;       oTS -> SetData, time, data
;
; :Keywords:
;       TIME:           in, required, type=NxM array
;                       Name or reference of a MrTimeVar object, or an array
;                           of time stamps. If a name is provided, the assiciated
;                           variable must exist in the variable cache.
;       DATA:           in, required, type=NxM array
;                       Name or reference of a MrVariable object, or the dependent
;                           variable data. If a name is given, the associated variable
;                           must exist in the variable cache. 
;
; :Keywords:
;       DIMENSION:      in, optional, type=integer
;                       The time-dependent, 1-based dimension of `DATA`. If not provided,
;                           the dimension of `DATA` that is equal in size to `TIME` is
;                           chose as the default.
;       T_TYPE:         in, optional, type=integer
;                       If `TIME` is an array of time stamps, use this keyword to indicate
;                           the format or time-basis. See MrTimeVar for more details.
;       T_NAME:         in, optional, type=integer
;                       Name to be given to the MrTimeVar object. Ignored unless `TIME`
;                           is an array of time stamps.
;       NO_COPY:        in, optional, type=boolean, default=0
;                       If set `DATA` will be copied directly into the object
;                           and will be left undefined (a MrTimeSeries object will not
;                           be destroyed, but its array will be empty).
;-
pro MrTimeSeries::SetData, x1, x2, $
DIMENSION=dimension, $
T_TYPE=t_type, $
T_NAME=t_name, $
NO_COPY=no_copy
	compile_opt idl2
	on_error, 2

	if n_elements(x2) gt 0 then begin
		time   = x1
		ts_var = x2
	endif else begin
		ts_var = x1
	endelse
	
;-----------------------------------------------------
; Check Time \\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\
;-----------------------------------------------------
	;Use existing time
	if n_elements(time) eq 0 then begin
		if obj_valid(self.oTime) $
			then oTime = self.oTime $
			else message, 'No time variable exists. Please provide.'
	
	;Check given time
	endif else begin
		;Object
		if size(time, /TNAME) eq 'OBJREF' then begin
			;MrTimeVar
			if ~obj_isa(time, 'MrTimeVar') $
				then oTime = time $
				else message, 'X1 must be a MrTimeSeries object.' 
	
		;Name
		endif else if size(time, /TNAME) eq 'STRING' then begin
			;Cached?
			if MrVar_IsCached(time) then begin
				oTime = MrVar_Get(time)
				if ~obj_isa(oTime, 'MrTimeVar') $
					then message, 'X1 must be the name of a cached MrTimeVar object.'
			endif else begin
				message, 'X1 must be the name of a cached variable.'
			endelse
	
		;Data
		endif else begin
			oTime = MrTimeVar(time, NAME=t_name, NO_COPY=no_copy, TYPE=t_type)
			if obj_valid(oTime) eq 0 then message, 'Could not create X1 variable.'
		endelse
	endelse
	
;-----------------------------------------------------
; Check Data \\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\
;-----------------------------------------------------
	;Object
	if size(ts_var, /TNAME) eq 'OBJREF' then begin
		;MrVariable
		if obj_class(ts_var) eq 'MrVariable' $
			then data = ts_var -> GetData() $
			message, 'Only "MrVariable" objects can be given.'
	
	;Name
	if size(ts_var, /TNAME) eq 'STRING' then begin
		;Cached?
		if MrVar_IsCached(ts_var) then begin
			oData = MrVar_Get(ts_var)
			if obj_isa(oData, 'MrVariable') $
				then data = oData -> GetData() $
				else message, 'Only names of MrVariable objects may be given.'
		endif else begin
			message, 'Only names of cached variables may be provided.'
		endelse
	
	;Data
	endif else begin
		data = temporary(ts_var)
	endelse
	
;-----------------------------------------------------
; Put Time First \\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\
;-----------------------------------------------------
	;Dimension sizes
	dims  = size(data, /DIMENSIONS)
	nDims = n_elements(dims)
	nPts  = n_elements(oTime)
	
	;Find the time-dependent dimension
	if n_elements(dimension) eq 0 then begin
		dimension = where(dims eq nPts, nt) + 1
		if nt ne 1 then message, 'Cannot determine time-dependent dimension.'
	endif
	
	;Put the time-dependent dimension first
	if dimension ne 1 then begin
		;Shift DIMENSION to first dimension
		p    = bytarr(n_elements(dims))
		p[0] = dimension - 1
		if nDims gt 1 then begin
			if dimension eq 1 then begin
				p[1] = dims[1:*]
			endif else if dimension eq nDims then begin
				p[1] = dims[0:nDims-2]
			endif else if nDims ge 2 then begin
				p[1] = [dims[0:dimension-2], dims[dimension:*]]
			endif
		endif
		
		;Transpose the data
		data = transpose(temporary(data), temporary(p))
	endif
	
;-----------------------------------------------------
; Store Data \\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\
;-----------------------------------------------------
	;Store the data as properties
	self.oTime = oTime
	*self.data = temporary(data)
	
	;Set the DEPEND_0 attribute
	self -> SetAttrValue, 'DEPEND_0', self.oTime.name, /CREATE
	
	;Clear data
	if keyword_set(no_copy) then begin
		x1 = !Null
		x2 = !Null
	endif
end


;+
;   The class definition statement.
;
; :Params:
;       CLASS:          out, optional, type=structure
;-
pro MrTimeSeries__DEFINE
	compile_opt idl2
	
	class = { MrTimeSeries, $
	          inherits MrVariable, $
	          oTime: obj_new() $
	        }
end