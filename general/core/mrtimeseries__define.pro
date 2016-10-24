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
;       OPERATION:
;       An operator (such as AND, EQ, ^, *, etc.) calls the overload
;       method for the object on the left side first. So, for two MrTimeSeries
;       objects, A and B::
;
;               print, A [operator] B
;
;       will call A's _Overload method first.
;
;       INTERPOLATION:
;       If the operator acts between two MrTimeSeries objects and the size of
;       their time-dependent dimensions is different, the larger will be
;       interpolated via IDL's Interpol(/SPLINE) to the smaller.
;
;       ARRAY TRUNCATION:
;       If the operator acts between a MrTimeSeries object and either a MrVariable
;       object or a numerical expression, IDL's array truncation ruls apply. The
;       results will be truncated to have the same number of elements as the shorter
;       array. If an array and a scalar are compared, each elements of the array is
;       compared against the scalar value.
;
;       RESULT:
;       The result of such an operation will be a subclass of MrVariable::
;
;           MrTimeSeries [op] MrTimeSeries = MrTimeSeries
;           MrTimeSeries [op] MrVariable   = MrVariable
;           MrTimeSeries [op] Expression   = MrVariable
;
;   CACHING
;       The ::Cache method will cache the MrScalarTS object. All variables in the
;       cache are forced to have unique names. When the object is destroyed, it
;       will automatically be removed from the cache.
;
; :Categories:
;   MrVariable, MrTimeSeries
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
;       2016/10/24  -   Written by Matthew Argall
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
;   Multiply two expressions together.
;
; :Params:
;       LEFT:               out, required, type=any
;                           The argument that appears on the left side of the operator. 
;                               Possibly the implicit "self".
;       RIGHT:              out, required, type=long
;                           The argument that appears on the left side of the operator. 
;                               Possibly the implicit "self".
;
; :Returns:
;       RESULT:             The result of multiplying `LEFT` by `RIGHT`.
;-
function MrTimeSeries::_OverloadAsterisk, left, right
	compile_opt idl2
	on_error, 2

	;Is SELF on the left or right?
	;   - Are both LEFT and RIGHT GDA data objects?
	side = self -> _LeftOrRight(left, right, ISMRVARIABLE=IsMrVariable)

;-------------------------------------------------------
; Two MrTimeSeries Objects /////////////////////////////
;-------------------------------------------------------
	if isa(left, 'MrTimeSeries') && isa(right, 'MrTimeSeries') then begin
		
		;New name
		name = 'Multiply(' + left.name + ',' + right.name + ')'

		;Perform operation
		if side eq 'LEFT' $
			then temp = *self.data   * right['DATA'] $
			else temp = left['DATA'] * *self.data
		
		;Return object
		result = MrTimeSeries( self.oTime, temp, NAME=name, /NO_COPY)
		
		;Update attributes
		self -> CopyAttrTo, result

;-------------------------------------------------------
; MrTimeSeries with MrVariable /////////////////////////
;-------------------------------------------------------
	endif else if ( side eq 'LEFT'  && isa(right, 'MrVariable') ) || $
	              ( side eq 'RIGHT' && isa(left,  'MrVariable') )    $
	then begin
		;Name of result
		name = 'Multiply(' + left.name + ',' + right.name + ')'
		
		;Multiply
		if side eq 'LEFT' $
			then result = *self.data   * right['DATA'] $
			else result = left['DATA'] * *self.data
		
		;Create a MrVariable object
		result = MrVariable(result, NAME=name, /NO_COPY)
		
		;Update attributes
		self -> CopyAttrTo, result

;-------------------------------------------------------
; MrScalarTS with Expression ///////////////////////////
;-------------------------------------------------------
	endif else begin
		;Multiply the expressions
		;   - Assume the user knows what they are doing.
		;   - All IDL truncation effects apply (shortest in determines size out).
		if side eq 'LEFT' $
			then result = (*self.data) * right $
			else result = left * (*self.data)
		
		;Determine name
		;   - Scalar or TYPE[dims]
		if side eq 'LEFT' then begin
			if nPtsR eq 1 $
				then dims = strtrim(right[0], 2) $
				else dims = '[' + strjoin(strtrim(size(right, /DIMENSIONS), 2), ',') + ']'
			name = 'Multiply(' + self.name + ',' + dims + ')'
		
		endif else begin
			if nPtsL eq 1 $
				then dims = strtrim(left[0], 2) $
				else dims = size(left, /TNAME) + '[' + strjoin(strtrim(size(left, /DIMENSIONS), 2), ',') + ']'
			name = 'Multiply(' + dims + ',' + self.name + ')'
		endelse
		
		;Create a MrVariable object
		result = MrVariable( result, NAME=name, /NO_COPY )
	endelse

;-------------------------------------------------------
; Output Object ////////////////////////////////////////
;-------------------------------------------------------
	return, result
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
;   Raise one expression to the power of another.
;
; :Params:
;       LEFT:               out, required, type=any
;                           The argument that appears on the left side of the operator. 
;                               Possibly the implicit "self".
;       RIGHT:              out, required, type=long
;                           The argument that appears on the left side of the operator. 
;                               Possibly the implicit "self".
;
; :Returns:
;       RESULT:             The result of multiplying `LEFT` by `RIGHT`.
;-
function MrTimeSeries::_OverloadCaret, left, right
	compile_opt idl2
	on_error, 2

	;Is SELF on the left or right?
	;   - Are both LEFT and RIGHT GDA data objects?
	side = self -> _LeftOrRight(left, right, ISMRVARIABLE=IsMrVariable)

;-------------------------------------------------------
; Two MrTimeSeries Objects /////////////////////////////
;-------------------------------------------------------
	if isa(left, 'MrTimeSeries') && isa(right, 'MrTimeSeries') then begin
		
		;New name
		name = 'Caret(' + left.name + ',' + right.name + ')'

		;Perform operation
		if side eq 'LEFT' $
			then temp = *self.data   ^ right['DATA'] $
			else temp = left['DATA'] ^ *self.data
		
		;Return object
		result = MrTimeSeries( self.oTime, temp, NAME=name, /NO_COPY)
		
		;Update attributes
		self -> CopyAttrTo, result

;-------------------------------------------------------
; MrTimeSeries with MrVariable /////////////////////////
;-------------------------------------------------------
	endif else if ( side eq 'LEFT'  && isa(right, 'MrVariable') ) || $
	              ( side eq 'RIGHT' && isa(left,  'MrVariable') )    $
	then begin
		;Name of result
		name = 'Caret(' + left.name + ',' + right.name + ')'
		
		;Multiply
		if side eq 'LEFT' $
			then result = *self.data   ^ right['DATA'] $
			else result = left['DATA'] ^ *self.data
		
		;Create a MrVariable object
		result = MrVariable(result, NAME=name, /NO_COPY)
		
		;Update attributes
		self -> CopyAttrTo, result

;-------------------------------------------------------
; MrScalarTS with Expression ///////////////////////////
;-------------------------------------------------------
	endif else begin
		;Multiply the expressions
		;   - Assume the user knows what they are doing.
		;   - All IDL truncation effects apply (shortest in determines size out).
		if side eq 'LEFT' $
			then result = (*self.data) ^ right $
			else result = left ^ (*self.data)
		
		;Determine name
		;   - Scalar or TYPE[dims]
		if side eq 'LEFT' then begin
			if nPtsR eq 1 $
				then dims = strtrim(right[0], 2) $
				else dims = '[' + strjoin(strtrim(size(right, /DIMENSIONS), 2), ',') + ']'
			name = 'Caret(' + self.name + ',' + dims + ')'
		
		endif else begin
			if nPtsL eq 1 $
				then dims = strtrim(left[0], 2) $
				else dims = size(left, /TNAME) + '[' + strjoin(strtrim(size(left, /DIMENSIONS), 2), ',') + ']'
			name = 'Caret(' + dims + ',' + self.name + ')'
		endelse
		
		;Create a MrVariable object
		result = MrVariable( result, NAME=name, /NO_COPY )
	endelse

;-------------------------------------------------------
; Output Object ////////////////////////////////////////
;-------------------------------------------------------
	return, result
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
			1: value = (*self.data)[key]
			2: value = reform((*self.data)[key,*])
			3: value = reform((*self.data)[key,*,*])
			4: value = reform((*self.data)[key,*,*,*])
			5: value = reform((*self.data)[key,*,*,*,*])
			6: value = reform((*self.data)[key,*,*,*,*,*])
			7: value = reform((*self.data)[key,*,*,*,*,*,*])
			8: value = reform((*self.data)[key,*,*,*,*,*,*,*])
			else: message, 'Invalid number of dimensions (' + strtrim(nDims, 2) + ').'
		endcase
	
	;Otherwise, stop iterating 
	endif else next = 0

	;Next element to retrieve
	key += 1

	return, next
end


;+
;   Raise one expression to the power of another.
;
; :Params:
;       LEFT:               out, required, type=any
;                           The argument that appears on the left side of the operator. 
;                               Possibly the implicit "self".
;       RIGHT:              out, required, type=long
;                           The argument that appears on the left side of the operator. 
;                               Possibly the implicit "self".
;
; :Returns:
;       RESULT:             The result of multiplying `LEFT` by `RIGHT`.
;-
function MrTimeSeries::_OverloadMinus, left, right
	compile_opt idl2
	on_error, 2

	;Is SELF on the left or right?
	;   - Are both LEFT and RIGHT GDA data objects?
	side = self -> _LeftOrRight(left, right, ISMRVARIABLE=IsMrVariable)

;-------------------------------------------------------
; Two MrTimeSeries Objects /////////////////////////////
;-------------------------------------------------------
	if isa(left, 'MrTimeSeries') && isa(right, 'MrTimeSeries') then begin
		
		;New name
		name = 'Minus(' + left.name + ',' + right.name + ')'

		;Perform operation
		if side eq 'LEFT' $
			then temp = *self.data   - right['DATA'] $
			else temp = left['DATA'] - *self.data
		
		;Return object
		result = MrTimeSeries( self.oTime, temp, NAME=name, /NO_COPY)
		
		;Update attributes
		self -> CopyAttrTo, result

;-------------------------------------------------------
; MrTimeSeries with MrVariable /////////////////////////
;-------------------------------------------------------
	endif else if ( side eq 'LEFT'  && isa(right, 'MrVariable') ) || $
	              ( side eq 'RIGHT' && isa(left,  'MrVariable') )    $
	then begin
		;Name of result
		name = 'Minus(' + left.name + ',' + right.name + ')'
		
		;Multiply
		if side eq 'LEFT' $
			then temp = *self.data   - right['DATA'] $
			else temp = left['DATA'] - *self.data
		
		;Create a MrVariable object
		result = MrVariable(temp, NAME=name, /NO_COPY)
		
		;Update attributes
		self -> CopyAttrTo, result

;-------------------------------------------------------
; MrScalarTS with Expression ///////////////////////////
;-------------------------------------------------------
	endif else begin
		;Multiply the expressions
		;   - Assume the user knows what they are doing.
		;   - All IDL truncation effects apply (shortest in determines size out).
		if side eq 'LEFT' $
			then temp = (*self.data) - right $
			else temp = left - (*self.data)
		
		;Determine name
		;   - Scalar or TYPE[dims]
		if side eq 'LEFT' then begin
			if nPtsR eq 1 $
				then dims = strtrim(right[0], 2) $
				else dims = '[' + strjoin(strtrim(size(right, /DIMENSIONS), 2), ',') + ']'
			name = 'Minus(' + self.name + ',' + dims + ')'
		
		endif else begin
			if nPtsL eq 1 $
				then dims = strtrim(left[0], 2) $
				else dims = size(left, /TNAME) + '[' + strjoin(strtrim(size(left, /DIMENSIONS), 2), ',') + ']'
			name = 'Minus(' + dims + ',' + self.name + ')'
		endelse
		
		;Create a MrVariable object
		result = MrVariable( temp, NAME=name, /NO_COPY )
		
		;Copy attributes
		self -> CopyAttrTo, result
	endelse

;-------------------------------------------------------
; Output Object ////////////////////////////////////////
;-------------------------------------------------------
	return, result
end


;+
;   Take the mod of two expressions.
;
; :Params:
;       LEFT:               out, required, type=any
;                           The argument that appears on the left side of the operator. 
;                               Possibly the implicit "self".
;       RIGHT:              out, required, type=long
;                           The argument that appears on the left side of the operator. 
;                               Possibly the implicit "self".
;
; :Returns:
;       RESULT:             The result of multiplying `LEFT` by `RIGHT`.
;-
function MrTimeSeries::_OverloadMOD, left, right
	compile_opt idl2
	on_error, 2

	;Is SELF on the left or right?
	;   - Are both LEFT and RIGHT GDA data objects?
	side = self -> _LeftOrRight(left, right, ISMRVARIABLE=IsMrVariable)

;-------------------------------------------------------
; Two MrTimeSeries Objects /////////////////////////////
;-------------------------------------------------------
	if isa(left, 'MrTimeSeries') && isa(right, 'MrTimeSeries') then begin
		
		;New name
		name = 'Mod(' + left.name + ',' + right.name + ')'

		;Perform operation
		if side eq 'LEFT' $
			then temp = *self.data   mod right['DATA'] $
			else temp = left['DATA'] mod *self.data
		
		;Return object
		result = MrTimeSeries( self.oTime, temp, NAME=name, /NO_COPY)
		
		;Update attributes
		self -> CopyAttrTo, result

;-------------------------------------------------------
; MrTimeSeries with MrVariable /////////////////////////
;-------------------------------------------------------
	endif else if ( side eq 'LEFT'  && isa(right, 'MrVariable') ) || $
	              ( side eq 'RIGHT' && isa(left,  'MrVariable') )    $
	then begin
		;Name of result
		name = 'Mod(' + left.name + ',' + right.name + ')'
		
		;Multiply
		if side eq 'LEFT' $
			then temp = *self.data   mod right['DATA'] $
			else temp = left['DATA'] mod *self.data
		
		;Create a MrVariable object
		result = MrVariable(temp, NAME=name, /NO_COPY)
		
		;Update attributes
		self -> CopyAttrTo, result

;-------------------------------------------------------
; MrScalarTS with Expression ///////////////////////////
;-------------------------------------------------------
	endif else begin
		;Multiply the expressions
		;   - Assume the user knows what they are doing.
		;   - All IDL truncation effects apply (shortest in determines size out).
		if side eq 'LEFT' $
			then temp = (*self.data) mod right $
			else temp = left mod (*self.data)
		
		;Determine name
		;   - Scalar or TYPE[dims]
		if side eq 'LEFT' then begin
			if nPtsR eq 1 $
				then dims = strtrim(right[0], 2) $
				else dims = '[' + strjoin(strtrim(size(right, /DIMENSIONS), 2), ',') + ']'
			name = 'Mod(' + self.name + ',' + dims + ')'
		
		endif else begin
			if nPtsL eq 1 $
				then dims = strtrim(left[0], 2) $
				else dims = size(left, /TNAME) + '[' + strjoin(strtrim(size(left, /DIMENSIONS), 2), ',') + ']'
			name = 'Mod(' + dims + ',' + self.name + ')'
		endelse
		
		;Create a MrVariable object
		result = MrVariable( temp, NAME=name, /NO_COPY )
		
		;Copy attributes
		self -> CopyAttrTo, result
	endelse

;-------------------------------------------------------
; Output Object ////////////////////////////////////////
;-------------------------------------------------------
	return, result
end


;+
;   Add one expression to another.
;
; :Params:
;       LEFT:               out, required, type=any
;                           The argument that appears on the left side of the operator. 
;                               Possibly the implicit "self".
;       RIGHT:              out, required, type=long
;                           The argument that appears on the left side of the operator. 
;                               Possibly the implicit "self".
;
; :Returns:
;       RESULT:             The result of multiplying `LEFT` by `RIGHT`.
;-
function MrTimeSeries::_OverloadPlus, left, right
	compile_opt idl2
	on_error, 2

	;Is SELF on the left or right?
	;   - Are both LEFT and RIGHT GDA data objects?
	side = self -> _LeftOrRight(left, right, ISMRVARIABLE=IsMrVariable)

;-------------------------------------------------------
; Two MrTimeSeries Objects /////////////////////////////
;-------------------------------------------------------
	if isa(left, 'MrTimeSeries') && isa(right, 'MrTimeSeries') then begin
		
		;New name
		name = 'Plus(' + left.name + ',' + right.name + ')'
		outClass = 'MrTimeSeries'

		;Perform operation
		if side eq 'LEFT' $
			then temp = *self.data   + right['DATA'] $
			else temp = left['DATA'] + *self.data
		
		;Return object
		result = MrTimeSeries( self.oTime, temp, NAME=name, /NO_COPY)
		
		;Update attributes
		self -> CopyAttrTo, result

;-------------------------------------------------------
; MrTimeSeries with MrVariable /////////////////////////
;-------------------------------------------------------
	endif else if ( side eq 'LEFT'  && isa(right, 'MrVariable') ) || $
	              ( side eq 'RIGHT' && isa(left,  'MrVariable') )    $
	then begin
		;Name of result
		name = 'Plus(' + left.name + ',' + right.name + ')'
		
		;Multiply
		if side eq 'LEFT' $
			then temp = *self.data   + right['DATA'] $
			else temp = left['DATA'] + *self.data
		
		;Create a MrVariable object
		result = MrVariable(temp, NAME=name, /NO_COPY)
		
		;Update attributes
		self -> CopyAttrTo, result

;-------------------------------------------------------
; MrScalarTS with Expression ///////////////////////////
;-------------------------------------------------------
	endif else begin
		;Multiply the expressions
		;   - Assume the user knows what they are doing.
		;   - All IDL truncation effects apply (shortest in determines size out).
		if side eq 'LEFT' $
			then temp = (*self.data) + right $
			else temp = left + (*self.data)
		
		;Determine name
		;   - Scalar or TYPE[dims]
		if side eq 'LEFT' then begin
			if nPtsR eq 1 $
				then dims = strtrim(right[0], 2) $
				else dims = '[' + strjoin(strtrim(size(right, /DIMENSIONS), 2), ',') + ']'
			name = 'Plus(' + self.name + ',' + dims + ')'
		
		endif else begin
			if nPtsL eq 1 $
				then dims = strtrim(left[0], 2) $
				else dims = size(left, /TNAME) + '[' + strjoin(strtrim(size(left, /DIMENSIONS), 2), ',') + ']'
			name = 'Plus(' + dims + ',' + self.name + ')'
		endelse
		
		;Create a MrVariable object
		result = MrVariable( temp, NAME=name, /NO_COPY )
		
		;Copy attributes
		self -> CopyAttrTo, result
	endelse

;-------------------------------------------------------
; Output Object ////////////////////////////////////////
;-------------------------------------------------------
	return, result
end


;+
;   Divide one expression by another.
;
; :Params:
;       LEFT:               out, required, type=any
;                           The argument that appears on the left side of the operator. 
;                               Possibly the implicit "self".
;       RIGHT:              out, required, type=long
;                           The argument that appears on the left side of the operator. 
;                               Possibly the implicit "self".
;
; :Returns:
;       RESULT:             The result of multiplying `LEFT` by `RIGHT`.
;-
function MrTimeSeries::_OverloadPound, left, right
	compile_opt idl2
	on_error, 2

	;Is SELF on the left or right?
	;   - Are both LEFT and RIGHT GDA data objects?
	side = self -> _LeftOrRight(left, right, ISMRVARIABLE=IsMrVariable)

;-------------------------------------------------------
; Two MrTimeSeries Objects /////////////////////////////
;-------------------------------------------------------
	if isa(left, 'MrTimeSeries') && isa(right, 'MrTimeSeries') then begin
		
		;New name
		name = 'Pound(' + left.name + ',' + right.name + ')'
		outClass = 'MrTimeSeries'

		;Perform operation
		if side eq 'LEFT' $
			then temp = *self.data   # right['DATA'] $
			else temp = left['DATA'] # *self.data
		
		;Return object
		result = MrTimeSeries( self.oTime, temp, NAME=name, /NO_COPY)
		
		;Update attributes
		self -> CopyAttrTo, result

;-------------------------------------------------------
; MrTimeSeries with MrVariable /////////////////////////
;-------------------------------------------------------
	endif else if ( side eq 'LEFT'  && isa(right, 'MrVariable') ) || $
	              ( side eq 'RIGHT' && isa(left,  'MrVariable') )    $
	then begin
		;Name of result
		name = 'Pound(' + left.name + ',' + right.name + ')'
		
		;Multiply
		if side eq 'LEFT' $
			then temp = *self.data   # right['DATA'] $
			else temp = left['DATA'] # *self.data
		
		;Create a MrVariable object
		result = MrVariable(temp, NAME=name, /NO_COPY)
		
		;Update attributes
		self -> CopyAttrTo, result

;-------------------------------------------------------
; MrScalarTS with Expression ///////////////////////////
;-------------------------------------------------------
	endif else begin
		;Multiply the expressions
		;   - Assume the user knows what they are doing.
		;   - All IDL truncation effects apply (shortest in determines size out).
		if side eq 'LEFT' $
			then temp = (*self.data) # right $
			else temp = left # (*self.data)
		
		;Determine name
		;   - Scalar or TYPE[dims]
		if side eq 'LEFT' then begin
			if nPtsR eq 1 $
				then dims = strtrim(right[0], 2) $
				else dims = '[' + strjoin(strtrim(size(right, /DIMENSIONS), 2), ',') + ']'
			name = 'Pound(' + self.name + ',' + dims + ')'
		
		endif else begin
			if nPtsL eq 1 $
				then dims = strtrim(left[0], 2) $
				else dims = size(left, /TNAME) + '[' + strjoin(strtrim(size(left, /DIMENSIONS), 2), ',') + ']'
			name = 'Pound(' + dims + ',' + self.name + ')'
		endelse
		
		;Create a MrVariable object
		result = MrVariable( temp, NAME=name, /NO_COPY )
		
		;Copy attributes
		self -> CopyAttrTo, result
	endelse

;-------------------------------------------------------
; Output Object ////////////////////////////////////////
;-------------------------------------------------------
	return, result
end


;+
;   Divide one expression by another.
;
; :Params:
;       LEFT:               out, required, type=any
;                           The argument that appears on the left side of the operator. 
;                               Possibly the implicit "self".
;       RIGHT:              out, required, type=long
;                           The argument that appears on the left side of the operator. 
;                               Possibly the implicit "self".
;
; :Returns:
;       RESULT:             The result of multiplying `LEFT` by `RIGHT`.
;-
function MrTimeSeries::_OverloadPoundPound, left, right
	compile_opt idl2
	on_error, 2

	;Is SELF on the left or right?
	;   - Are both LEFT and RIGHT GDA data objects?
	side = self -> _LeftOrRight(left, right, ISMRVARIABLE=IsMrVariable)

;-------------------------------------------------------
; Two MrTimeSeries Objects /////////////////////////////
;-------------------------------------------------------
	if isa(left, 'MrTimeSeries') && isa(right, 'MrTimeSeries') then begin
		
		;New name
		name = 'PoundPound(' + left.name + ',' + right.name + ')'
		outClass = 'MrTimeSeries'

		;Perform operation
		if side eq 'LEFT' $
			then temp = *self.data   ## right['DATA'] $
			else temp = left['DATA'] ## *self.data
		
		;Return object
		result = MrTimeSeries( self.oTime, temp, NAME=name, /NO_COPY)
		
		;Update attributes
		self -> CopyAttrTo, result

;-------------------------------------------------------
; MrTimeSeries with MrVariable /////////////////////////
;-------------------------------------------------------
	endif else if ( side eq 'LEFT'  && isa(right, 'MrVariable') ) || $
	              ( side eq 'RIGHT' && isa(left,  'MrVariable') )    $
	then begin
		;Name of result
		name = 'PoundPound(' + left.name + ',' + right.name + ')'
		
		;Multiply
		if side eq 'LEFT' $
			then temp = *self.data   ## right['DATA'] $
			else temp = left['DATA'] ## *self.data
		
		;Create a MrVariable object
		result = MrVariable(temp, NAME=name, /NO_COPY)
		
		;Update attributes
		self -> CopyAttrTo, result

;-------------------------------------------------------
; MrScalarTS with Expression ///////////////////////////
;-------------------------------------------------------
	endif else begin
		;Multiply the expressions
		;   - Assume the user knows what they are doing.
		;   - All IDL truncation effects apply (shortest in determines size out).
		if side eq 'LEFT' $
			then temp = (*self.data) ## right $
			else temp = left ## (*self.data)
		
		;Determine name
		;   - Scalar or TYPE[dims]
		if side eq 'LEFT' then begin
			if nPtsR eq 1 $
				then dims = strtrim(right[0], 2) $
				else dims = '[' + strjoin(strtrim(size(right, /DIMENSIONS), 2), ',') + ']'
			name = 'PoundPound(' + self.name + ',' + dims + ')'
		
		endif else begin
			if nPtsL eq 1 $
				then dims = strtrim(left[0], 2) $
				else dims = size(left, /TNAME) + '[' + strjoin(strtrim(size(left, /DIMENSIONS), 2), ',') + ']'
			name = 'PoundPound(' + dims + ',' + self.name + ')'
		endelse
		
		;Create a MrVariable object
		result = MrVariable( temp, NAME=name, /NO_COPY )
		
		;Copy attributes
		self -> CopyAttrTo, result
	endelse

;-------------------------------------------------------
; Output Object ////////////////////////////////////////
;-------------------------------------------------------
	return, result
end


;+
;   Divide one expression by another.
;
; :Params:
;       LEFT:               out, required, type=any
;                           The argument that appears on the left side of the operator. 
;                               Possibly the implicit "self".
;       RIGHT:              out, required, type=long
;                           The argument that appears on the left side of the operator. 
;                               Possibly the implicit "self".
;
; :Returns:
;       RESULT:             The result of multiplying `LEFT` by `RIGHT`.
;-
function MrTimeSeries::_OverloadSlash, left, right
	compile_opt idl2
	on_error, 2

	;Is SELF on the left or right?
	;   - Are both LEFT and RIGHT GDA data objects?
	side = self -> _LeftOrRight(left, right, ISMRVARIABLE=IsMrVariable)

;-------------------------------------------------------
; Two MrTimeSeries Objects /////////////////////////////
;-------------------------------------------------------
	if isa(left, 'MrTimeSeries') && isa(right, 'MrTimeSeries') then begin
		
		;New name
		name = 'Divide(' + left.name + ',' + right.name + ')'
		outClass = 'MrTimeSeries'

		;Perform operation
		if side eq 'LEFT' $
			then temp = *self.data   / right['DATA'] $
			else temp = left['DATA'] / *self.data
		
		;Return object
		result = MrTimeSeries( self.oTime, temp, NAME=name, /NO_COPY)
		
		;Update attributes
		self -> CopyAttrTo, result

;-------------------------------------------------------
; MrTimeSeries with MrVariable /////////////////////////
;-------------------------------------------------------
	endif else if ( side eq 'LEFT'  && isa(right, 'MrVariable') ) || $
	              ( side eq 'RIGHT' && isa(left,  'MrVariable') )    $
	then begin
		;Name of result
		name = 'Divide(' + left.name + ',' + right.name + ')'
		
		;Multiply
		if side eq 'LEFT' $
			then temp = *self.data   / right['DATA'] $
			else temp = left['DATA'] / *self.data
		
		;Create a MrVariable object
		result = MrVariable(temp, NAME=name, /NO_COPY)
		
		;Update attributes
		self -> CopyAttrTo, result

;-------------------------------------------------------
; MrScalarTS with Expression ///////////////////////////
;-------------------------------------------------------
	endif else begin
		;Multiply the expressions
		;   - Assume the user knows what they are doing.
		;   - All IDL truncation effects apply (shortest in determines size out).
		if side eq 'LEFT' $
			then temp = (*self.data) / right $
			else temp = left / (*self.data)
		
		;Determine name
		;   - Scalar or TYPE[dims]
		if side eq 'LEFT' then begin
			if nPtsR eq 1 $
				then dims = strtrim(right[0], 2) $
				else dims = '[' + strjoin(strtrim(size(right, /DIMENSIONS), 2), ',') + ']'
			name = 'Divide(' + self.name + ',' + dims + ')'
		
		endif else begin
			if nPtsL eq 1 $
				then dims = strtrim(left[0], 2) $
				else dims = size(left, /TNAME) + '[' + strjoin(strtrim(size(left, /DIMENSIONS), 2), ',') + ']'
			name = 'Divide(' + dims + ',' + self.name + ')'
		endelse
		
		;Create a MrVariable object
		result = MrVariable( temp, NAME=name, /NO_COPY )
		
		;Copy attributes
		self -> CopyAttrTo, result
	endelse

;-------------------------------------------------------
; Output Object ////////////////////////////////////////
;-------------------------------------------------------
	return, result
end


;+
;   Concatenate an array, or a series of arrays, to the implicit array. If a series of
;   arrays are to be concatenated, as in a FOR loop, memory allocation is handled
;   internally by doubling the current number of elements or by adding the number of
;   elements present in the input array, whichever is greater. Truncation of unfilled
;   elements is also performed internally. See the `START` and `FINISH` methods, as well
;   as the examples below.
;
;   Notes:
;       - Extra elements are not truncated until ::Append is called with the /FINISH
;           keyword set.
;       - If ::Append was initialized with the /START and /BEFORE keywords, elements are
;           appended to the end of the array. When ::Append is called with the /FINISH
;           keyword set, they are shifted to the front.
;
; :Params:
;       TIME:           in, required, type=Nx1 or 1xN array
;                       Array to be concatenated to the implicit array. If undefined or
;                           !Null, then nothing is appended.
;       DATA:           in, required, type=array
;                       Array to be concatenated to the implicit array. If undefined or
;                           !Null, then nothing is appended.
;       TYPE:           in, optional, type=array, default='ISO-8601'
;                       Units of the input time array. See ::toISO for options.
;
; :Keywords:
;       BEFORE:         in, optional, type=boolean, default=0
;                       If set, `DATA` will be appended to the beginning of the implicit
;                           array. The default is to append to the end.
;       NO_COPY:        in, optional, type=boolean, default=0
;                       If set, `DATA` will be copied directly into the object and
;                           will be left undefined.
;-
pro MrTimeSeries::Append, time, data, type, $
BEFORE=before, $
NO_COPY=no_copy
	compile_opt idl2
	on_error, 2
	
	;Allow input data to be empty. In this case, do nothing.
	if n_elements(data) eq 0 then return

	;Defaults
	before  = keyword_set(before)
	no_copy = keyword_set(no_copy)

	;Restrictions
	nPts = n_elements(time)
	dims = size(data, /DIMENSIONS)
	if dims[0] ne nPts then message, 'Size of TIME and first dimension of DATA must agree.'
	
;-----------------------------------------------------
; Append Data \\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\
;-----------------------------------------------------
	;Is the object empty?
	if n_elements(*self.data) eq 0 then begin
		self -> SetData, time, data, NO_COPY=no_copy
	
	;Append
	endif else begin
		;Append to time array
		self.oTime -> Append, time, type, BEFORE=before, NO_COPY=no_copy
	
		;Append to data array
		if before then begin
			if no_copy $
				then *self.data = [ temporary(data), *self.data ] $
				else *self.data = [ data, *self.data ]
		endif else begin
			if no_copy $
				then *self.data = [ *self.data, temporary(data) ] $
				else *self.data = [ *self.data, data ]
		endelse
	endelse
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
;   Return the variable's data.
;
; :Params:
;       TIME:       out, optional, type=strarr
;                   Time tags, formatted as ISO-8601 strings
;       TYPE:       in, optional, type=string, default='ISO-8601'
;                   The time base in which `TIME` should be returned.
;
; :Keywords:
;       TVAR:       in, optional, type=boolean, default=0
;                   If set, `TIME` will be returned as a MrTimeVar object
;                       instead of the individual time tags.
;-
function MrTimeSeries::GetData, time, type, $
TVAR=tvar
	compile_opt idl2
	on_error, 2
	
	;Return time
	if arg_present(time) then begin
		if keyword_set(tvar) $
			then time = self.oTime $
			else time = self.oTime -> GetData(type)
	endif
	
	;Return data
	return, *self.data
end


;+
;   Return the size of the time-dependent dimension.
;
; :Returns:
;       NTIMES:     in, optional, type=intarr
;                   Number of elements along the time-dependent dimension.
;-
function MrTimeSeries::GetNPts
	compile_opt idl2
	on_error, 2
	
	;Number of records
	sz = size(*self.data, /DIMENSIONS)
	return, sz[0]
end


;+
;   Compute the inner product of two time series tensors of rank > 1.
;   This method generalizes the # operator for tensors of rank > 2.
;
; :Params:
;       B:          in, required, type=MrTimeSeries objref
;                   A tensor with > 1 dimension.
;
; :Returns:
;       RESULT:     out, required, type=MrTimeSeries objref
;                   Result of the inner product.
;-
function MrTimeSeries::InnerProduct, B, $
NAME=name
	compile_opt strictarr
	on_error, 2
	
	;B must be a time series
	if ~isa(B, 'MrTimeSeries') $
		then message, 'B must be a MrTimeSeries object.'

;-------------------------------------------------------
; Dimensions ///////////////////////////////////////////
;-------------------------------------------------------
	
	;Number of points
	nPtsA = self -> GetNPts()
	nPtsB = B    -> GetNPts()
	if nPtsA lt nPtsB then begin
		!Null = A -> GetData( oTime, !Null, /TVAR )
		nPts  = nPtsA
	endif else begin
		!Null = B -> GetData( oTime, !Null, /TVAR )
		nPts  = nPtsB
	endelse

	;Dimensionality of inputs
	dimsA  = size(self, /DIMENSIONS)
	dimsB  = size(B, /DIMENSIONS)
	nDimsA = size(self, /N_DIMENSIONS)
	nDimsB = size(B, /N_DIMENSIONS)
	nDims  = nDimsA < nDimsB
	
	;Output dimensions
	case nDims of
		1:    message, 'SELF and B must have > 1 dimension.'
		2:    outDims = nDimsA < nDimsB ? dimsA : dimsB
		else: outDims = nDimsA < nDimsB ? dimsA[1:-2] : dimsB[2:*]
	endcase

;-------------------------------------------------------
; Reform Inputs ////////////////////////////////////////
;-------------------------------------------------------
	
	;A is currently D1xD2xD3xD4x ... xDN
	;   - Turn A into D1xDixDN matrix
	if nDimsA le 2 $
		then _A = A['DATA'] $
		else _A = reform( A['DATA'], [ dimsA[0], product(dimsA[1:-2]), dimsA[-1] ] )
	
	;B is currently D1xD2xD3xD4x ... xDN
	;   - Turn B into D1xD2xDi matrix
	if nDimsB le 2 $
		then _B = B['DATA'] $
		else _B = reform( B['DATA'], [ dimsB[0], dimsB[1], product(dimsB[2:*]) ] )

;-------------------------------------------------------
; Inner Product ////////////////////////////////////////
;-------------------------------------------------------
	
	;Allocate memory
	temp = make_array( [nPts, outDims], TYPE=size(A[[0]]*B[[0]], /TYPE) )
	
	;Step through each
	for i = 0, nPts - 1 do begin
		temp[i,*,*] = reform(_A[i,*,*]) # reform(_B[i,*,*])
	endfor

;-------------------------------------------------------
; Finish Up ////////////////////////////////////////////
;-------------------------------------------------------

	;Reform back to the original shape, minus the contracted dimension.
	temp = reform(temp, dimsOut, /OVERWRITE)
	
	;Create time series variable
	result = MrTimeSeries( oTime, temp, NAME=name, /NO_COPY )
	return, result
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
		Yout = reform(Yout, [nOut, dims[1:*]], /OVERWRITE)
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
;   Compute the outer product of two time series tensors of rank > 1.
;   This method generalizes the ## operator for tensors of rank > 2.
;
; :Params:
;       B:          in, required, type=MrTimeSeries objref
;                   A tensor with > 1 dimension.
;
; :Returns:
;       RESULT:     out, required, type=MrTimeSeries objref
;                   Result of the outer product.
;-
function MrTimeSeries::OuterProduct, B, $
NAME=name
	compile_opt strictarr
	on_error, 2
	
	;B must be a time series
	if ~isa(B, 'MrTimeSeries') $
		then message, 'B must be a MrTimeSeries object.'

;-------------------------------------------------------
; Dimensions ///////////////////////////////////////////
;-------------------------------------------------------
	
	;Number of points
	nPtsA = self -> GetNPts()
	nPtsB = B    -> GetNPts()
	if nPtsA lt nPtsB then begin
		!Null = A -> GetData( oTime, /TVAR )
		nPts  = nPtsA
	endif else begin
		!Null = B -> GetData( oTime, /TVAR )
		nPts  = nPtsB
	endelse

	;Dimensionality of inputs
	dimsA  = size(self, /DIMENSIONS)
	dimsB  = size(B, /DIMENSIONS)
	nDimsA = size(self, /N_DIMENSIONS)
	nDimsB = size(B, /N_DIMENSIONS)
	nDims  = nDimsA < nDimsB
	
	;Output dimensions
	case nDims of
		1:    message, 'SELF and B must have > 1 dimension.'
		2:    outDims = nDimsA < nDimsB ? dimsA : dimsB
		else: outDims = nDimsA < nDimsB ? dimsA[2:*] : dimsB[1:-2]
	endcase

;-------------------------------------------------------
; Reform Inputs ////////////////////////////////////////
;-------------------------------------------------------
	
	;A is currently D1xD2xD3xD4x ... xDN
	;   - Turn A into D1xD2xDi matrix
	if nDimsA le 2 $
		then _A = self['DATA'] $
		else _A = reform( self['DATA'], [ dimsB[0], dimsB[1], product(dimsB[2:*]) ] )
	
	;B is currently D1xD2xD3xD4x ... xDN
	;   - Turn B into D1xDixDN matrix
	if nDimsB le 2 $
		then _B = B['DATA'] $
		else _B = reform( B['DATA'], [ dimsB[0], product(dimsB[1:-2]), dimsB[-1] ] )

;-------------------------------------------------------
; Inner Product ////////////////////////////////////////
;-------------------------------------------------------
	
	;Allocate memory
	temp = make_array( [nPts, outDims], TYPE=size(A[[0]]*B[[0]], /TYPE) )
	
	;Step through each
	for i = 0, nPts - 1 do begin
		temp[i,*,*] = reform(_A[i,*,*]) ## reform(_B[i,*,*])
	endfor

;-------------------------------------------------------
; Finish Up ////////////////////////////////////////////
;-------------------------------------------------------

	;Reform back to the original shape, minus the contracted dimension.
	temp = reform(temp, dimsOut, /OVERWRITE)
	
	;Create time series variable
	result = MrTimeSeries( oTime, temp, NAME=name, /NO_COPY )
	return, result
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
;                       The time-dependent dimension of `DATA` (1-based). If not
;                           provided, the dimension of `DATA` that is equal in size to
;                           `TIME` is chosen as the default. If zero or multiple
;                           dimensions match in this way, an error will occur.
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
			else message, 'Only "MrVariable" objects can be given.'
	
	;Name
	endif else if size(ts_var, /TNAME) eq 'STRING' then begin
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
	endif else begin
		if dims[dimension-1] ne nPts $
			then message, 'The size of TIME and DATA along DIMENSION must be equal.'
	endelse
	
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
	;   - Let automatic garbage collection take care of the OTIME object
	;     and DATA pointer (IDL v8.0+)
	;   - The OTIME property may be shared by another MrTimeSeries object
	;   - The DATA pointer may be saved by a superclass in case additional
	;     requirements on the dimensionality of the data is required.
	;       * e.g. pData = self.data
	;              self  -> MrTimeSeries::SetData, time, newData
	;              if [dimension check fails] then self.data = pData
	self.oTime = oTime
	self.data  = ptr_new(data, /NO_COPY)
	
	;Set the DEPEND_0 attribute
	self -> SetAttrValue, 'DEPEND_0', self.oTime.name, /CREATE
	
	;Clear data
	if keyword_set(no_copy) then begin
		x1 = !Null
		x2 = !Null
	endif
end


;+
;   Transpose the array.
;
; :Keywords:
;       P:          in, optional, type=intarr
;                   Vector specifying how the dimensions are to be permuted. Dimensions
;                       start at zero and cannot be repeated. The time dimension must
;                       remain first (i.e. zero must be the first element of P). Zero
;                       may be omitted from P so that P specifies how the non-time
;                       dimensions are permuted.
;-
function MrTimeSeries::Transpose, P
	compile_opt idl2
	on_error, 2

;-----------------------------------------------------
; P Not Given \\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\
;-----------------------------------------------------
	
	;If P is not given, the default is to reverse dimensions
	;   - Time dimension is always first
	if n_elements(p) eq 0 then begin
		;Transpose only does something if there are 3+ dimensions
		nDims = size(self, /DIMENSIONS)
		if nDims gt 2 then begin
			permute = [0, reverse( indgen( nDims-1 ) + 1 )]
		
		;Otherwise, keep dimensions the same
		endif else begin
			permute = indgen(nDims)
		endelse
;-----------------------------------------------------
; Permute as Prescribed \\\\\\\\\\\\\\\\\\\\\\\\\\\\\\
;-----------------------------------------------------
	endif else begin
		permute = P
		
		;If 0 is present, it must be first. If not present, add it first.
		if permute[0] ne 1 then begin
			iZero = where(permute eq 0, nZero)
			if nZero eq 0 $
				then permute = [0, P] $
				else message, 'Zero must be the first element of P.'
		endif
	endelse
	
;-----------------------------------------------------
; Transpose \\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\
;-----------------------------------------------------
	;Create a time series object
	result  = self -> Copy(NAME = 'Transpose(' + self.name + ')')
	result -> SetData, self.oTime, transpose(*self.data, permute)
	
	;Return
	return, result
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