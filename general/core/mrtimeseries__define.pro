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
;           MrVariable   [op] MrTimeSeries = MrVariable
;           MrTimeSeries [op] Expression   = MrTimeSeries or MrVariable*
;           Expression   [op] MrTimeSeries = MrTimeSeries or MrVariable*
;
;                                        * MrTimeSeries if expression is
;                                          scalar or the size of its first
;                                          dimension matches that of the
;                                          implicit array. Otherwise,
;                                          MrVariable.
;
;   CACHING
;       The ::Cache method will cache the MrTimeSeries object. All variables in the
;       cache are forced to have unique names. When the object is destroyed, it
;       will be removed automatically from the cache.
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
;       T_NAME:         in, optional, type=integer
;                       Name to be given to the MrTimeVar object. Ignored unless `TIME`
;                           is an array of time stamps.
;       T_REF:              in, optional, type=string, default=''
;                           A reference time from which `TIME` is measured. Applicable
;                               only to certain values of `TYPE`. Must be formatted as
;                               an ISO-8601 string.
;       T_TYPE:         in, optional, type=integer
;                       If `TIME` is an array of time stamps, use this keyword to indicate
;                           the format or time-basis. See MrTimeVar for more details.
;-
function MrTimeSeries::INIT, time, data, $
CACHE=cache, $
DIMENSION=dimension, $
NAME=name, $
NO_CLOBBER=no_clobber, $
NO_COPY=no_copy, $
T_NAME=t_name, $
T_REF=t_ref, $
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
		                 T_NAME    = t_name, $
		                 T_REF     = t_ref, $
		                 T_TYPE    = t_type
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
	
	;
	; Do not destroy the time object.
	;   - It may be used by another MrVariable object
	;   - Automatic garbage collection will destroy it when not used
	;
	
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

;-------------------------------------------------------
; MrTimeSeries with Expression /////////////////////////
;-------------------------------------------------------
	endif else begin
		;Rules for output
		;  Create time series if:
		;     1. Expression is a scalar
		;     2. Expression's first dimension is same size as that of implicit self
		;  Create MrVariable in all other cases
		
		;LEFT
		if side eq 'LEFT' then begin
			;Object class for output
			if MrIsA(right, /SCALAR) || MrDim(right, 1) eq self -> GetNPts() $
				then classOut = obj_class(self) $
				else classOut = 'MrVariable'
			
			;Name of output variable
			if n_elements(right) eq 1 $
				then dims = strtrim(right[0], 2) $
				else dims = '[' + strjoin(strtrim(size(right, /DIMENSIONS), 2), ',') + ']'
			name = 'Multiply(' + self.name + ',' + dims + ')'
			
			;Operation
			result = (*self.data) * right
			
		;RIGHT
		endif else begin
			;Object class for output
			if MrIsA(left, /SCALAR) || MrDim(left, 1) eq self -> GetNPts() $
				then classOut = obj_class(self) $
				else classOut = 'MrVariable'
			
			;Name of output variable
			if n_elements(left) eq 1 $
				then dims = strtrim(left[0], 2) $
				else dims = size(left, /TNAME) + '[' + strjoin(strtrim(size(left, /DIMENSIONS), 2), ',') + ']'
			name = 'Multiply(' + dims + ',' + self.name + ')'
			
			;Operation
			result = left * (*self.data)
		endelse
		
		;Create a MrVariable object
		if classOut eq 'MrVariable' $
			then result = obj_new( classOut, result, NAME=name, /NO_COPY ) $
			else result = obj_new( classOut, self.oTime, result, NAME=name, /NO_COPY)
	endelse

;-------------------------------------------------------
; Output Object ////////////////////////////////////////
;-------------------------------------------------------
	return, result
end


;+
;   Allow square-bracket array indexing from the left side of an operator.
;
; :Examples:
;   Saving a subarray into a 4D array::
;       myArray = MrVariable(4, 2, 5, 3, TYPE='FLOAT')
;       myArray[1:2, 0, 2:4, 1] = RandomU(5, 2,1,3,1)
;
;   Saving a subarray into a 3D array::
;       myArray = MrVariable(fltarr(5,5,5))
;       myArray[*,-3,0:2] = findgen(5,1,3)
;
;
; :Params:
;       OBJREF:             in, required, type=ObjRef
;                           The object reference variable that is being indexed (i.e. "self")
;                               Use when you want to replace "self" with `VALUE`.
;       VALUE:              in, required, type=numeric array
;                           The value specified on the right-hand side of the equal sign.
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
;-
pro MrTimeSeries::_OverloadBracketsLeftSide, objRef, value, isRange, i1, i2, i3, i4, i5, i6, i7, i8
	compile_opt idl2
	on_error, 2

	;Number of subscripts given
	nSubscripts = N_ELEMENTS(isRange)

;---------------------------------------------------------------------
; Attribute Name Given ///////////////////////////////////////////////
;---------------------------------------------------------------------
	if nSubscripts eq 1 && MrIsA(i1, 'STRING', /SCALAR) then begin
		;Restricted names
		;   - They have special uses within ::_OverloadBracketsRightSide
		if MrIsMember(['DATA', 'PTR', 'POINTER', 'TIME', 'TIMEVAR'], i1) $
			then message, '"' + i1 + '" cannot be an attribute name.'
		
		;Set the attribute
		self -> SetAttributeValue, i1, value, /CREATE
		return
	endif

;---------------------------------------------------------------------
; Brute Force Subscripting ///////////////////////////////////////////
;---------------------------------------------------------------------
	;
	; Highly optimized code for three dimensions or lower. 
	; Handle all combinations of subscript ranges or indices.
	;

	;<= 3D subscript range
	if (nSubscripts le 3) then begin
	;---------------------------------------------------------------------
	; 3D Subscripts //////////////////////////////////////////////////////
	;---------------------------------------------------------------------
		if IsA(i3) then begin 
			;[? ,? , min:max:interval]
			if isRange[2] then begin 
				;[? , min:max:interval, min:max:interval]
				if isRange[1] then begin 
					;[min:max:interval , min:max:interval, min:max:interval]
					if isRange[0] then begin
						(*self.data)[i1[0]:i1[1]:i1[2],i2[0]:i2[1]:i2[2],i3[0]:i3[1]:i3[2]] = value
					;[index , min:max:interval, min:max:interval]
					endif else begin
						(*self.data)[i1,i2[0]:i2[1]:i2[2],i3[0]:i3[1]:i3[2]] = value 
					endelse
				;[? , index, min:max:interval]
				endif else begin
					;[min:max:interval , index, min:max:interval]
					if isRange[0] then begin 
						(*self.data)[i1[0]:i1[1]:i1[2],i2,i3[0]:i3[1]:i3[2]] = value
					;[index , index, min:max:interval]
					endif else begin 
						(*self.data)[i1,i2,i3[0]:i3[1]:i3[2]] = value 
					endelse 
				endelse
			;[? , ?, index]
			endif else begin
				;[? , min:max:interval, index]
				if isRange[1] then begin
					;[min:max:interval, min:max:interval, index]
					if isRange[0] then begin 
						(*self.data)[i1[0]:i1[1]:i1[2],i2[0]:i2[1]:i2[2],i3] = value 
					;[index, min:max:interval, index]
					endif else begin 
						(*self.data)[i1,i2[0]:i2[1]:i2[2],i3] = value 
					endelse 
				;[?, index, index]
				endif else begin
					;[min:max:interval, index, index]
					if isRange[0] then begin 
						(*self.data)[i1[0]:i1[1]:i1[2],i2,i3] = value 
					;[index, index, index]
					endif else begin 
						(*self.data)[i1,i2,i3] = value 
					endelse 
				endelse 
			endelse 
	;---------------------------------------------------------------------
	; 2D Subscripts //////////////////////////////////////////////////////
	;---------------------------------------------------------------------
		endif else if IsA(i2) then begin
			;[?, min:max:interval]
			if isRange[1] then begin
				;[min:max:interval, min:max:interval]
				if isRange[0] then begin
					(*self.data)[i1[0]:i1[1]:i1[2],i2[0]:i2[1]:i2[2]] = value
				;[index, min:max:interval]
				endif else begin
					(*self.data)[i1,i2[0]:i2[1]:i2[2]] = value
				endelse
			;[?, index]
			endif else begin
				;[min:max:interval, index]
				if isRange[0] then begin
					(*self.data)[i1[0]:i1[1]:i1[2],i2] = value
				;[index, index]
				endif else begin
					(*self.data)[i1,i2] = value
				endelse
			endelse
	;---------------------------------------------------------------------
	; 1D Subscripts //////////////////////////////////////////////////////
	;---------------------------------------------------------------------
		endif else begin
			;min:max:interval
			if isRange[0] then begin
				(*self.data)[i1[0]:i1[1]:i1[2]] = value 
			;Index
			endif else begin
				(*self.data)[i1] = value 
			endelse
		endelse

		return
	endif

;---------------------------------------------------------------------
; Brute Force Code for 4D or Higher Arrays. //////////////////////////
;---------------------------------------------------------------------
	;Works for any number of dimensions.
	indices = MrReformIndices(Size(*self.data, /DIMENSIONS), isRange, i1, i2, i3, i4, i5, i6, i7, i8);, /IDL_METHOD)
	(*self.data)[indices] = value
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
;       oTime     = oTSvar['TIMEVAR']
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
		case i1 of
			'DATA':    return, *self.data
			'POINTER': return,  self.data
			'PTR':     return,  self.data
			'TIME':    return,  self.oTime -> GetData(i2)
			'TIMEVAR': return,  self.oTime
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

;-------------------------------------------------------
; MrScalarTS with Expression ///////////////////////////
;-------------------------------------------------------
	endif else begin
		;Rules for output
		;  Create time series if:
		;     1. Expression is a scalar
		;     2. Expression's first dimension is same size as that of implicit self
		;  Create MrVariable in all other cases
		
		;LEFT
		if side eq 'LEFT' then begin
			;Object class for output
			if MrIsA(right, /SCALAR) || MrDim(right, 1) eq self -> GetNPts() $
				then classOut = obj_class(self) $
				else classOut = 'MrVariable'
			
			;Name of output variable
			if n_elements(right) eq 1 $
				then dims = strtrim(right[0], 2) $
				else dims = '[' + strjoin(strtrim(size(right, /DIMENSIONS), 2), ',') + ']'
			name = 'Caret(' + self.name + ',' + dims + ')'
			
			;Operation
			result = (*self.data) ^ right
			
		;RIGHT
		endif else begin
			;Object class for output
			if MrIsA(left, /SCALAR) || MrDim(left, 1) eq self -> GetNPts() $
				then classOut = obj_class(self) $
				else classOut = 'MrVariable'
			
			;Name of output variable
			if n_elements(left) eq 1 $
				then dims = strtrim(left[0], 2) $
				else dims = size(left, /TNAME) + '[' + strjoin(strtrim(size(left, /DIMENSIONS), 2), ',') + ']'
			name = 'Caret(' + dims + ',' + self.name + ')'
			
			;Operation
			result = left ^ (*self.data)
		endelse
		
		;Create a MrVariable object
		if classOut eq 'MrVariable' $
			then result = obj_new( classOut, result, NAME=name, /NO_COPY ) $
			else result = obj_new( classOut, self.oTime, result, NAME=name, /NO_COPY)
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
function MrTimeSeries::_OverloadGreaterThan, left, right
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
		name = 'GreaterThan(' + left.name + ',' + right.name + ')'
		outClass = 'MrTimeSeries'

		;Perform operation
		if side eq 'LEFT' $
			then temp = *self.data   > right['DATA'] $
			else temp = left['DATA'] > *self.data
		
		;Return object
		result = MrTimeSeries( self.oTime, temp, NAME=name, /NO_COPY)

;-------------------------------------------------------
; MrTimeSeries with MrVariable /////////////////////////
;-------------------------------------------------------
	endif else if ( side eq 'LEFT'  && isa(right, 'MrVariable') ) || $
	              ( side eq 'RIGHT' && isa(left,  'MrVariable') )    $
	then begin
		;Name of result
		name = 'GreaterThan(' + left.name + ',' + right.name + ')'
		
		;Multiply
		if side eq 'LEFT' $
			then temp = *self.data   > right['DATA'] $
			else temp = left['DATA'] > *self.data
		
		;Create a MrVariable object
		result = MrVariable(temp, NAME=name, /NO_COPY)

;-------------------------------------------------------
; MrScalarTS with Expression ///////////////////////////
;-------------------------------------------------------
	endif else begin
		;Rules for output
		;  Create time series if:
		;     1. Expression is a scalar
		;     2. Expression's first dimension is same size as that of implicit self
		;  Create MrVariable in all other cases
		
		;SELF is LEFT
		if side eq 'LEFT' then begin
			;Object class for output
			if MrIsA(right, /SCALAR) || MrDim(right, 1) eq self -> GetNPts() $
				then classOut = obj_class(self) $
				else classOut = 'MrVariable'
			
			;Name of output variable
			if n_elements(right) eq 1 $
				then dims = strtrim(right[0], 2) $
				else dims = '[' + strjoin(strtrim(size(right, /DIMENSIONS), 2), ',') + ']'
			name = 'GreaterThan(' + self.name + ',' + dims + ')'

			;Operation
			result = *self.data > right
			
		;SELF is RIGHT
		endif else begin
			;Object class for output
			if MrIsA(left, /SCALAR) || MrDim(left, 1) eq self -> GetNPts() $
				then classOut = obj_class(self) $
				else classOut = 'MrVariable'
			
			;Name of output variable
			if n_elements(left) eq 1 $
				then dims = strtrim(left[0], 2) $
				else dims = size(left, /TNAME) + '[' + strjoin(strtrim(size(left, /DIMENSIONS), 2), ',') + ']'
			name = 'GreaterThan(' + dims + ',' + self.name + ')'
			
			;Operation
			result = left > *self.data
		endelse
		
		;Create a MrVariable object
		if classOut eq 'MrVariable' $
			then result = obj_new( classOut, result, NAME=name, /NO_COPY ) $
			else result = obj_new( classOut, self.oTime, result, NAME=name, /NO_COPY)
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
function MrTimeSeries::_OverloadLessThan, left, right
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
		name = 'LessThan(' + left.name + ',' + right.name + ')'
		outClass = 'MrTimeSeries'

		;Perform operation
		if side eq 'LEFT' $
			then temp = *self.data   < right['DATA'] $
			else temp = left['DATA'] < *self.data
		
		;Return object
		result = MrTimeSeries( self.oTime, temp, NAME=name, /NO_COPY)

;-------------------------------------------------------
; MrTimeSeries with MrVariable /////////////////////////
;-------------------------------------------------------
	endif else if ( side eq 'LEFT'  && isa(right, 'MrVariable') ) || $
	              ( side eq 'RIGHT' && isa(left,  'MrVariable') )    $
	then begin
		;Name of result
		name = 'LessThan(' + left.name + ',' + right.name + ')'
		
		;Multiply
		if side eq 'LEFT' $
			then temp = *self.data   < right['DATA'] $
			else temp = left['DATA'] < *self.data
		
		;Create a MrVariable object
		result = MrVariable(temp, NAME=name, /NO_COPY)

;-------------------------------------------------------
; MrScalarTS with Expression ///////////////////////////
;-------------------------------------------------------
	endif else begin
		;Rules for output
		;  Create time series if:
		;     1. Expression is a scalar
		;     2. Expression's first dimension is same size as that of implicit self
		;  Create MrVariable in all other cases
		
		;SELF is LEFT
		if side eq 'LEFT' then begin
			;Object class for output
			if MrIsA(right, /SCALAR) || MrDim(right, 1) eq self -> GetNPts() $
				then classOut = obj_class(self) $
				else classOut = 'MrVariable'
			
			;Name of output variable
			if n_elements(right) eq 1 $
				then dims = strtrim(right[0], 2) $
				else dims = '[' + strjoin(strtrim(size(right, /DIMENSIONS), 2), ',') + ']'
			name = 'LessThan(' + self.name + ',' + dims + ')'

			;Operation
			result = *self.data < right
			
		;SELF is RIGHT
		endif else begin
			;Object class for output
			if MrIsA(left, /SCALAR) || MrDim(left, 1) eq self -> GetNPts() $
				then classOut = obj_class(self) $
				else classOut = 'MrVariable'
			
			;Name of output variable
			if n_elements(left) eq 1 $
				then dims = strtrim(left[0], 2) $
				else dims = size(left, /TNAME) + '[' + strjoin(strtrim(size(left, /DIMENSIONS), 2), ',') + ']'
			name = 'LessThan(' + dims + ',' + self.name + ')'
			
			;Operation
			result = left < *self.data
		endelse
		
		;Create a MrVariable object
		if classOut eq 'MrVariable' $
			then result = obj_new( classOut, result, NAME=name, /NO_COPY ) $
			else result = obj_new( classOut, self.oTime, result, NAME=name, /NO_COPY)
	endelse

;-------------------------------------------------------
; Output Object ////////////////////////////////////////
;-------------------------------------------------------
	return, result
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

;-------------------------------------------------------
; MrScalarTS with Expression ///////////////////////////
;-------------------------------------------------------
	endif else begin
		;Rules for output
		;  Create time series if:
		;     1. Expression is a scalar
		;     2. Expression's first dimension is same size as that of implicit self
		;  Create MrVariable in all other cases
		
		;LEFT
		if side eq 'LEFT' then begin
			;Object class for output
			if MrIsA(right, /SCALAR) || MrDim(right, 1) eq self -> GetNPts() $
				then classOut = obj_class(self) $
				else classOut = 'MrVariable'
			
			;Name of output variable
			if n_elements(right) eq 1 $
				then dims = strtrim(right[0], 2) $
				else dims = '[' + strjoin(strtrim(size(right, /DIMENSIONS), 2), ',') + ']'
			name = 'Minus(' + self.name + ',' + dims + ')'
			
			;Operation
			result = (*self.data) - right
			
		;RIGHT
		endif else begin
			;Object class for output
			if MrIsA(left, /SCALAR) || MrDim(left, 1) eq self -> GetNPts() $
				then classOut = obj_class(self) $
				else classOut = 'MrVariable'
			
			;Name of output variable
			if n_elements(left) eq 1 $
				then dims = strtrim(left[0], 2) $
				else dims = size(left, /TNAME) + '[' + strjoin(strtrim(size(left, /DIMENSIONS), 2), ',') + ']'
			name = 'Minus(' + dims + ',' + self.name + ')'
			
			;Operation
			result = left - (*self.data)
		endelse
		
		;Create a MrVariable object
		if classOut eq 'MrVariable' $
			then result = obj_new( classOut, result, NAME=name, /NO_COPY ) $
			else result = obj_new( classOut, self.oTime, result, NAME=name, /NO_COPY)
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

;-------------------------------------------------------
; MrScalarTS with Expression ///////////////////////////
;-------------------------------------------------------
	endif else begin
		;Rules for output
		;  Create time series if:
		;     1. Expression is a scalar
		;     2. Expression's first dimension is same size as that of implicit self
		;  Create MrVariable in all other cases
		
		;LEFT
		if side eq 'LEFT' then begin
			;Object class for output
			if MrIsA(right, /SCALAR) || MrDim(right, 1) eq self -> GetNPts() $
				then classOut = obj_class(self) $
				else classOut = 'MrVariable'
			
			;Name of output variable
			if n_elements(right) eq 1 $
				then dims = strtrim(right[0], 2) $
				else dims = '[' + strjoin(strtrim(size(right, /DIMENSIONS), 2), ',') + ']'
			name = 'Mod(' + self.name + ',' + dims + ')'
			
			;Operation
			result = (*self.data) mod right
			
		;RIGHT
		endif else begin
			;Object class for output
			if MrIsA(left, /SCALAR) || MrDim(left, 1) eq self -> GetNPts() $
				then classOut = obj_class(self) $
				else classOut = 'MrVariable'
			
			;Name of output variable
			if n_elements(left) eq 1 $
				then dims = strtrim(left[0], 2) $
				else dims = size(left, /TNAME) + '[' + strjoin(strtrim(size(left, /DIMENSIONS), 2), ',') + ']'
			name = 'Mod(' + dims + ',' + self.name + ')'
			
			;Operation
			result = left mod (*self.data)
		endelse
		
		;Create a MrVariable object
		if classOut eq 'MrVariable' $
			then result = obj_new( classOut, result, NAME=name, /NO_COPY ) $
			else result = obj_new( classOut, self.oTime, result, NAME=name, /NO_COPY)
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

;-------------------------------------------------------
; MrScalarTS with Expression ///////////////////////////
;-------------------------------------------------------
	endif else begin
		;Rules for output
		;  Create time series if:
		;     1. Expression is a scalar
		;     2. Expression's first dimension is same size as that of implicit self
		;  Create MrVariable in all other cases
		
		;SELF is LEFT
		if side eq 'LEFT' then begin
			;Object class for output
			if MrIsA(right, /SCALAR) || MrDim(right, 1) eq self -> GetNPts() $
				then classOut = obj_class(self) $
				else classOut = 'MrVariable'
			
			;Name of output variable
			if n_elements(right) eq 1 $
				then dims = strtrim(right[0], 2) $
				else dims = '[' + strjoin(strtrim(size(right, /DIMENSIONS), 2), ',') + ']'
			name = 'Plus(' + self.name + ',' + dims + ')'

			;Operation
			result = *self.data + right
			
		;SELF is RIGHT
		endif else begin
			;Object class for output
			if MrIsA(left, /SCALAR) || MrDim(left, 1) eq self -> GetNPts() $
				then classOut = obj_class(self) $
				else classOut = 'MrVariable'
			
			;Name of output variable
			if n_elements(left) eq 1 $
				then dims = strtrim(left[0], 2) $
				else dims = size(left, /TNAME) + '[' + strjoin(strtrim(size(left, /DIMENSIONS), 2), ',') + ']'
			name = 'Plus(' + dims + ',' + self.name + ')'
			
			;Operation
			result = left + *self.data
		endelse
		
		;Create a MrVariable object
		if classOut eq 'MrVariable' $
			then result = obj_new( classOut, result, NAME=name, /NO_COPY ) $
			else result = obj_new( classOut, self.oTime, result, NAME=name, /NO_COPY)
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

;-------------------------------------------------------
; MrScalarTS with Expression ///////////////////////////
;-------------------------------------------------------
	endif else begin
		;Rules for output
		;  Create time series if:
		;     1. Expression is a scalar
		;     2. Expression's first dimension is same size as that of implicit self
		;  Create MrVariable in all other cases
		
		;LEFT
		if side eq 'LEFT' then begin
			;Object class for output
			if MrIsA(right, /SCALAR) || MrDim(right, 1) eq self -> GetNPts() $
				then classOut = obj_class(self) $
				else classOut = 'MrVariable'
			
			;Name of output variable
			if n_elements(right) eq 1 $
				then dims = strtrim(right[0], 2) $
				else dims = '[' + strjoin(strtrim(size(right, /DIMENSIONS), 2), ',') + ']'
			name = 'Pound(' + self.name + ',' + dims + ')'
			
			;Operation
			result = (*self.data) # right
			
		;RIGHT
		endif else begin
			;Object class for output
			if MrIsA(left, /SCALAR) || MrDim(left, 1) eq self -> GetNPts() $
				then classOut = obj_class(self) $
				else classOut = 'MrVariable'
			
			;Name of output variable
			if n_elements(left) eq 1 $
				then dims = strtrim(left[0], 2) $
				else dims = size(left, /TNAME) + '[' + strjoin(strtrim(size(left, /DIMENSIONS), 2), ',') + ']'
			name = 'Pound(' + dims + ',' + self.name + ')'
			
			;Operation
			result = left # (*self.data)
		endelse
		
		;Create a MrVariable object
		if classOut eq 'MrVariable' $
			then result = obj_new( classOut, result, NAME=name, /NO_COPY ) $
			else result = obj_new( classOut, self.oTime, result, NAME=name, /NO_COPY)
	endelse

;-------------------------------------------------------
; Output Object ////////////////////////////////////////
;-------------------------------------------------------
	return, result
end


;+
;   Take the outer product of two variables.
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
		;Rules for output
		;  Create time series if:
		;     1. Expression is a scalar
		;     2. Expression's first dimension is same size as that of implicit self
		;  Create MrVariable in all other cases
		
		;LEFT
		if side eq 'LEFT' then begin
			;Object class for output
			if MrIsA(right, /SCALAR) || MrDim(right, 1) eq self -> GetNPts() $
				then classOut = obj_class(self) $
				else classOut = 'MrVariable'
			
			;Name of output variable
			if n_elements(right) eq 1 $
				then dims = strtrim(right[0], 2) $
				else dims = '[' + strjoin(strtrim(size(right, /DIMENSIONS), 2), ',') + ']'
			name = 'PoundPound(' + self.name + ',' + dims + ')'
			
			;Operation
			result = (*self.data) ## right
			
		;RIGHT
		endif else begin
			;Object class for output
			if MrIsA(left, /SCALAR) || MrDim(left, 1) eq self -> GetNPts() $
				then classOut = obj_class(self) $
				else classOut = 'MrVariable'
			
			;Name of output variable
			if n_elements(left) eq 1 $
				then dims = strtrim(left[0], 2) $
				else dims = size(left, /TNAME) + '[' + strjoin(strtrim(size(left, /DIMENSIONS), 2), ',') + ']'
			name = 'PoundPound(' + dims + ',' + self.name + ')'
			
			;Operation
			result = left ## (*self.data)
		endelse
		
		;Create a MrVariable object
		if classOut eq 'MrVariable' $
			then result = obj_new( classOut, result, NAME=name, /NO_COPY ) $
			else result = obj_new( classOut, self.oTime, result, NAME=name, /NO_COPY)
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
		;Rules for output
		;  Create time series if:
		;     1. Expression is a scalar
		;     2. Expression's first dimension is same size as that of implicit self
		;  Create MrVariable in all other cases
		
		;LEFT
		if side eq 'LEFT' then begin
			;Object class for output
			if MrIsA(right, /SCALAR) || MrDim(right, 1) eq self -> GetNPts() $
				then classOut = obj_class(self) $
				else classOut = 'MrVariable'
			
			;Name of output variable
			if n_elements(right) eq 1 $
				then dims = strtrim(right[0], 2) $
				else dims = '[' + strjoin(strtrim(size(right, /DIMENSIONS), 2), ',') + ']'
			name = 'Divide(' + self.name + ',' + dims + ')'
			
			;Operation
			result = (*self.data) / right
			
		;RIGHT
		endif else begin
			;Object class for output
			if MrIsA(left, /SCALAR) || MrDim(left, 1) eq self -> GetNPts() $
				then classOut = obj_class(self) $
				else classOut = 'MrVariable'
			
			;Name of output variable
			if n_elements(left) eq 1 $
				then dims = strtrim(left[0], 2) $
				else dims = size(left, /TNAME) + '[' + strjoin(strtrim(size(left, /DIMENSIONS), 2), ',') + ']'
			name = 'Divide(' + dims + ',' + self.name + ')'
			
			;Operation
			result = left / (*self.data)
		endelse
		
		;Create a MrVariable object
		if classOut eq 'MrVariable' $
			then result = obj_new( classOut, result, NAME=name, /NO_COPY ) $
			else result = obj_new( classOut, self.oTime, result, NAME=name, /NO_COPY)
	endelse

;-------------------------------------------------------
; Output Object ////////////////////////////////////////
;-------------------------------------------------------
	return, result
end


;+
;   The purpose of this method is to negate the implicit array.
;
; :Returns:
;       RESULT:             Negation of the implicit array.
;-
function MrTimeSeries::_OverloadMinusUnary
	compile_opt idl2
	on_error, 2

	;Negate the array, making positive values negative, and vice versa
	return, self -> New( self.oTime, -(*self.data), NAME='-'+self.name)
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
	if n_elements(name) eq 0 then name = 'Copy(' + self.name + ')'

	;Copy the data into a new object
	;   - Use Obj_Class() so subclasses can inherit the method.
	theCopy = obj_new( obj_class(self), self.oTime, *self.data, $
	                   DIMENSION = 1, $
	                   NAME      = name, $
	                   T_NAME    = self.oTime.name )

	;Copy the variable attributes
	self -> CopyAttrTo, theCopy
	
	;Cache the variable
	if tf_cache then theCopy -> Cache, /NO_CLOBBER
	return, theCopy
end


;+
;   Detrend data by removing a smoothed version of itself. Acts as a high-pass filter.
;
; :Params:
;       WIDTH:          in, optional, type=integer, default=all
;                       Number of points to use in the boxcar smooth.
;
; :Keywords:
;       CACHE:          in, optional, type=boolean, default=0
;                       If set, the copy will be added to the variable cache
;       NAME:           in, optional, type=string, default=<name>+'_copy'
;                       Name of the variable copy.
;       _REF_EXTRA:     in, optional, type=any
;                       Any keyword accepted by IDL's Smooth function.
;
; :Returns:
;       RESULT:         out, required, type=MrTimeSeries or MrVariable object
;                       The results of the fast fourier transformation. If more than
;                           one FFT is possible, a MrTimeSeries object is returned.
;                           Otherwise, a MrVariable object is returned.
;-
function MrTimeSeries::Detrend, width, $
CACHE=cache, $
NAME=name, $
_REF_EXTRA=extra
	compile_opt idl2
	on_error, 2
	
	;Defaults
	if n_elements(name) eq 0 then name = 'Detrend(' + self.name + ')'

	;Remove a smoothed version
	case size(self, /N_DIMENSIONS) of
		1: data = self - smooth(self['DATA'], width, _STRICT_EXTRA=extra)
		2: data = self - smooth(self['DATA'], [width,1], _STRICT_EXTRA=extra)
		3: data = self - smooth(self['DATA'], [width,1,1], _STRICT_EXTRA=extra)
		4: data = self - smooth(self['DATA'], [width,1,1,1], _STRICT_EXTRA=extra)
		5: data = self - smooth(self['DATA'], [width,1,1,1,1], _STRICT_EXTRA=extra)
		6: data = self - smooth(self['DATA'], [width,1,1,1,1,1], _STRICT_EXTRA=extra)
		7: data = self - smooth(self['DATA'], [width,1,1,1,1,1,1], _STRICT_EXTRA=extra)
		8: data = self - smooth(self['DATA'], [width,1,1,1,1,1,1,1], _STRICT_EXTRA=extra)
		else: message, 'Internal array has invalid number of dimensions.'
	endcase
	
	;Set properties
	self -> CopyAttrTo, data
	data['CATDESC'] = self.name + ' detrended with ' + strtrim(width) + ' point boxcar average.'

	;Cache
	data -> SetName, name
	if keyword_set(cache) then data -> Cache
	return, data
end


;+
;   Store variables as TPlot variables.
;
;   NOTE:
;       Requires the SPEDAS distribution.
;       http://spedas.org/blog/
;
; :Returns:
;       TNAME:              out, required, type=string
;                           Name of the TPlot variable created.
;-
FUNCTION MrTimeSeries::ExportToSPEDAS
	Compile_Opt idl2
	On_Error, 2
	
;-----------------------------------------------------
; Create the Data Structure \\\\\\\\\\\\\\\\\\\\\\\\\\
;-----------------------------------------------------
	CASE 1 OF
		;2D Image
		self -> HasAttr('DEPEND_1'): BEGIN
			data = { x: time_double( self.oTime['DATA'] ), $
			         y: *self.data, $
			         v: (self['DEPEND_1'])['DATA'] }
		ENDCASE
		
		;1D Time Series
		self -> HasAttr('DEPEND_0'): BEGIN
			data = { x: time_double( self.oTime['DATA'] ), $
			         y: *self.data }
		ENDCASE
		
		;Unknown
		ELSE: Message, 'I do not know how to load data with > 3 dimensions into TPlot.'
	ENDCASE
	
;-----------------------------------------------------
; Store the Variable \\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\
;-----------------------------------------------------
	;Store the data
	tname = self.name
	store_data, tname, DATA=temporary(data)
	
;-----------------------------------------------------
; X-Axis Options \\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\
;-----------------------------------------------------
	IF self -> HasAttr('DEPEND_0') THEN BEGIN
		dep0 = MrVar_Get( self['DEPEND_0'] )
		IF dep0 -> HasAttr('CHARSIZE')     THEN options, tname, 'xcharsize',     dep0['CHARSIZE']
		IF dep0 -> HasAttr('GRIDSTYLE')    THEN options, tname, 'xgridstyle',    dep0['GRIDSTYLE']
		IF dep0 -> HasAttr('LOG')          THEN options, tname, 'xlog',          dep0['LOG']
		IF dep0 -> HasAttr('MINOR')        THEN options, tname, 'xminor',        dep0['MINOR']
		IF dep0 -> HasAttr('AXIS_RANGE')   THEN options, tname, 'xrange',        dep0['AXIS_RANGE']
		IF dep0 -> HasAttr('STYLE')        THEN options, tname, 'xstyle',        dep0['STYLE']
		IF dep0 -> HasAttr('THICK')        THEN options, tname, 'xthick',        dep0['THICK']
		IF dep0 -> HasAttr('TICKFORMAT')   THEN options, tname, 'xtickformat',   dep0['TICKFORMAT']
		IF dep0 -> HasAttr('TICKINTERVAL') THEN options, tname, 'xtickinterval', dep0['TICKINTERVAL']
		IF dep0 -> HasAttr('TICKLAYOUT')   THEN options, tname, 'xticklayout',   dep0['TICKLAYOUT']
		IF dep0 -> HasAttr('TICKLEN')      THEN options, tname, 'xticklen',      dep0['TICKLEN']
		IF dep0 -> HasAttr('TICKNAME')     THEN options, tname, 'xtickname',     dep0['TICKNAME']
		IF dep0 -> HasAttr('MAJOR')        THEN options, tname, 'xticks',        dep0['MAJOR']
		IF dep0 -> HasAttr('TICKUNITS')    THEN options, tname, 'xtickunits',    dep0['TICKUNITS']
		IF dep0 -> HasAttr('TICKV')        THEN options, tname, 'xtickv',        dep0['TICKV']
		IF dep0 -> HasAttr('TITLE')        THEN options, tname, 'xtitle',        dep0['TITLE']
	ENDIF
	
;-----------------------------------------------------
; Y-Axis Options \\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\
;-----------------------------------------------------
	;Image or Time series?
	IF self -> HasAttr('DEPEND_1') THEN BEGIN
		yvar = MrVar_Get( self['DEPEND_1'] )
		options, tname, 'spec', 1B
	ENDIF ELSE BEGIN
		yvar = self
	ENDELSE

	IF yvar -> HasAttr('CHARSIZE')     THEN options, tname, 'ycharsize',     yvar['CHARSIZE']
	IF yvar -> HasAttr('GRIDSTYLE')    THEN options, tname, 'ygridstyle',    yvar['GRIDSTYLE']
	IF yvar -> HasAttr('LOG')          THEN options, tname, 'ylog',          yvar['LOG']
	IF yvar -> HasAttr('MINOR')        THEN options, tname, 'yminor',        yvar['MINOR']
	IF yvar -> HasAttr('AXIS_RANGE')   THEN options, tname, 'yrange',        yvar['AXIS_RANGE']
	IF yvar -> HasAttr('STYLE')        THEN options, tname, 'ystyle',        yvar['STYLE']
	IF yvar -> HasAttr('THICK')        THEN options, tname, 'ythick',        yvar['THICK']
	IF yvar -> HasAttr('TICKFORMAT')   THEN options, tname, 'ytickformat',   yvar['TICKFORMAT']
	IF yvar -> HasAttr('TICKINTERVAL') THEN options, tname, 'ytickinterval', yvar['TICKINTERVAL']
	IF yvar -> HasAttr('TICKLAYOUT')   THEN options, tname, 'yticklayout',   yvar['TICKLAYOUT']
	IF yvar -> HasAttr('TICKLEN')      THEN options, tname, 'yticklen',      yvar['TICKLEN']
	IF yvar -> HasAttr('TICKNAME')     THEN options, tname, 'ytickname',     yvar['TICKNAME']
	IF yvar -> HasAttr('MAJOR')        THEN options, tname, 'yticks',        yvar['MAJOR']
	IF yvar -> HasAttr('TICKUNITS')    THEN options, tname, 'ytickunits',    yvar['TICKUNITS']
	IF yvar -> HasAttr('TICKV')        THEN options, tname, 'ytickv',        yvar['TICKV']
	IF yvar -> HasAttr('TITLE')        THEN options, tname, 'ytitle',        yvar['TITLE']
	
;-----------------------------------------------------
; General Options \\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\
;-----------------------------------------------------
	IF self -> HasAttr('COLOR')      THEN options, tname, 'colors', cgColor(self['COLOR'])
	IF self -> HasAttr('PLOT_TITLE') THEN options, tname, 'title', self['PLOT_TITLE']
	IF self -> HasAttr('LABEL') THEN BEGIN
		options, tname, 'labels',  self['LABEL']
		options, tname, 'labflag', 1
	ENDIF
	
;-----------------------------------------------------
; Done \\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\
;-----------------------------------------------------
	RETURN, tname
END


;+
;   Compute the FFT
;
; :Params:
;       NFFT:           in, optional, type=integer, default=all
;                       Number of points per FFT.
;       NSHIFT:         in, optional, type=integer, default=`NFFT`/2.0
;                       Number of points to shift between FFTs.
;
; :Params:
;       AMPLITUDE:      out, optional, type=MrTimeVar/MrVariable object
;                       The amplitude of the signal.
;       PHASE:          out, optional, type=MrTimeVar/MrVariable object
;                       The phase of the signal.
;
; :Returns:
;       RESULT:         out, required, type=MrTimeSeries or MrVariable object
;                       The results of the fast fourier transformation. If more than
;                           one FFT is possible, a MrTimeSeries object is returned.
;                           Otherwise, a MrVariable object is returned.
;-
function MrTimeSeries::FFT, nfft, nshift, $
DOUBLE=double, $
NOISE=noise, $
ONE_SIDED=one_sided, $
POWER=power, $
RMS=rms, $
WINDOW=win
	compile_opt idl2
	on_error, 2
	
	dim          = 1
	tf_double    = keyword_set(double)
	tf_noise     = keyword_set(noise)
	tf_one_sided = keyword_set(one_sided)
	tf_power     = keyword_set(power)
	tf_rms       = keyword_set(rms)
	if n_elements(nfft)   eq 0 then nfft   = self -> GetNPts()
	if n_elements(nshift) eq 0 then nshift = nfft / 2.0
	if n_elements(win)    eq 0 then win    = ''
	
	;Conflicts
	if tf_rms + tf_noise + tf_power gt 1 then message, 'RMS, NOISE, and POWER are mutually exclusive.'

;-------------------------------------------------------
; Tapering Window //////////////////////////////////////
;-------------------------------------------------------
	
	;Name
	if isa(win, 'STRING', /SCALAR) then begin
		case strupcase(win) of
			'BLACKMAN': begin
				xx    = findgen(nfft)/(nfft-1)
				taper = 0.42 - 0.50 * cos(2*!pi*xx) + 0.08 * cos(4*!pi*xx)
			endcase
			'RECTANGULAR': taper = replicate(1.0, nfft)
			'HANNING':     taper = hanning(nfft, ALPHA=0.50)
			'HAMMING':     taper = hanning(nfft, ALPHA=0.54)
			'NONE':        taper = replicate(1.0, nfft)
			'':            taper = replicate(1.0, nfft)
			else: message, 'Window not recognized: "' + win + '".'
		endcase
	
	;User-defined
	endif else begin
		if n_elements(win) ne nfft then message, 'WIN must have NFFT elements.'
		taper = win
	endelse
	
	;Correct dimensions
	dims  = size(self, /DIMENSIONS)
	ndims = size(self, /N_DIMENSIONS)
	if ndims gt 1 then taper = rebin( taper, [nfft, dims[1:*]] )

;-------------------------------------------------------
; Data Intervals ///////////////////////////////////////
;-------------------------------------------------------
	;Convert to seconds
	t  = self.oTime -> GetData('SSM')
	
	;Number of FFTs to perform
	N    = dims[0]
	nMax = floor( (N - nfft) / nshift ) + 1
	
;-----------------------------------------------------
; Step Through Intervals \\\\\\\\\\\\\\\\\\\\\\\\\\\\\
;-----------------------------------------------------
	;Allocate memory
	;   - Only necessary of more than one FFT is being performed
	type = tf_double ? 9 : 6
	if nMax gt 1 then begin
		if nDims gt 7 then message, 'DIMENSIONS must be < 7 for sliding FFTs.'
		outDims   = nDims eq 1 ? [nMax, nfft] : [nMax, nfft, dims[1:*]]
		dft       = make_array( outDims, TYPE=type )
		freqs     = fltarr(nMax,nfft)
		dt_median = fltarr(nMax)
		df_fft    = fltarr(nMax)
		t_fft     = fltarr(nMax)
		dt_fft    = fltarr(nMax)
	endif

	istart = 0
	iend   = nfft-1
	for i = 0, nMax-1 do begin
	;-----------------------------------------------------
	; Define Interval \\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\
	;-----------------------------------------------------
		;
		; Place at beginning of loop so that CONTINUE
		; advances interval as well as loop index.
		;
	
		;First interval
		if i eq 0 then begin
			istart = 0
			iend   = nfft - 1
		
		;Last interval
		;   - Extend backward from the end of the array
		endif else if i eq nMax-1 then begin
			ilast = iend
			iend  = N - 1
			istart = N - nfft
		
		;Intermediate intervals
		;   - Advance forward by NSHIFT
		endif else begin
			istart += nshift
			iend   += nshift
		endelse
	
	;-----------------------------------------------------
	; Compute Sample Interval \\\\\\\\\\\\\\\\\\\\\\\\\\\\
	;-----------------------------------------------------
		;Time and sampling interval
		dtTemp = t[istart+1:iend] - t[istart:iend-1]

		;Is the sampling interval consistent throughout?
		;   - A "bad" sampling interval occurs if it is 10% different
		;     from the first sampling interval
		SI = median(dtTemp)
		if total( abs(temporary(dtTemp) - SI) gt 0.1*SI ) ne 0 then begin
			message, 'Sampling interval changes during FFT. Skipping.', /INFORMATIONAL
			continue
		endif
	
	;-----------------------------------------------------
	; Compute FFT \\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\
	;-----------------------------------------------------
		;Determine frequencies
		sr    = 1.0 / SI
		fN    = sr / 2.0
		df    = 1.0 / (SI * nfft)
		f     = [0:fN:df]
		f     = [ f, -reverse(f[1:-2]) ]
	
		;Compute the FFT
		case ndims of
			1: fftTemp = fft( taper * (*self.data)[istart:iend], DIMENSION=1, DOUBLE=tf_double)
			2: fftTemp = fft( taper * (*self.data)[istart:iend,*], DIMENSION=1, DOUBLE=tf_double)
			3: fftTemp = fft( taper * (*self.data)[istart:iend,*,*], DIMENSION=1, DOUBLE=tf_double)
			4: fftTemp = fft( taper * (*self.data)[istart:iend,*,*,*], DIMENSION=1, DOUBLE=tf_double)
			5: fftTemp = fft( taper * (*self.data)[istart:iend,*,*,*,*], DIMENSION=1, DOUBLE=tf_double)
			6: fftTemp = fft( taper * (*self.data)[istart:iend,*,*,*,*,*], DIMENSION=1, DOUBLE=tf_double)
			7: fftTemp = fft( taper * (*self.data)[istart:iend,*,*,*,*,*,*], DIMENSION=1, DOUBLE=tf_double)
			8: fftTemp = fft( taper * (*self.data)[istart:iend,*,*,*,*,*,*,*], DIMENSION=1, DOUBLE=tf_double)
			else: message, 'Incorrect number of dimensions: SELF.'
		endcase

		;Normalize the FFT to account for the window
;		CG = mean(taper)
;		NG = mean(taper^2)
;		case 1 of
;			tf_rms:   fftTemp /= CG
;			tf_noise: ;Do nothing
;			tf_power: ;Do nothing
;			else:     ;Do nothing
;		endcase

	;-----------------------------------------------------
	; Outputs \\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\
	;-----------------------------------------------------
		;Save data
		if nMax eq 1 then begin
			dft   = temporary(fftTemp)
			freqs = temporary(f)
		endif else begin
			case ndims of
				1: dft[i,*] = temporary(fftTemp)
				2: dft[i,*,*] = temporary(fftTemp)
				3: dft[i,*,*,*] = temporary(fftTemp)
				4: dft[i,*,*,*,*] = temporary(fftTemp)
				5: dft[i,*,*,*,*,*] = temporary(fftTemp)
				6: dft[i,*,*,*,*,*,*] = temporary(fftTemp)
				7: dft[i,*,*,*,*,*,*,*] = temporary(fftTemp)
				else: message, 'Incorrect number of dimensions: SELF.'
			endcase
			
			;Save frequencies and time itnerval
			;   - Necessary only for I=0 if TF_CALC_DT is false
			;   - Keep track of all here to simplify logic
			freqs[i,*]   = f
			dt_median[i] = SI
			dt_fft[i]    = nfft * SI
			df_fft[i]    = 1.0 / (nfft * SI)
		
			;Record time stamp
			t_fft[i]  = t[istart] + (nfft * SI) / 2.0
			dt_fft[i] = nfft * SI / 2.0
		endelse
	endfor

;-----------------------------------------------------
; Finishing Touches \\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\
;-----------------------------------------------------
	
	;Reduce dimensions if sampling interval never changed
	;   - fN = 1 / T = 1 / (nfft*dt)
	;   - NFFT is constant, but DT is permitted to vary
	;   - If DT changes, then we must keep track of FN as a function of time
	;   - If not, then FN and F do not vary with time, and we can reduce to one copy
	if nMax gt 1 && total( (dt_median - dt_median[0]) gt 0.1*dt_median[0] ) eq 0 then begin
		freqs     = reform(freqs[0,*])
		df_fft    = df_fft[0]
		dt_median = dt_median[0]
		dt_fft    = dt_fft[0]
	endif
	
	;Remove negative frequencies
	;   - TODO: What if DIMENSION ~= 2
	if tf_one_sided then begin
		freqs = freqs[0:nfft/2]
		case ndims of
			1: dft = dft[*,0:nfft/2]
			2: dft = dft[*,0:nfft/2,*]
			3: dft = dft[*,0:nfft/2,*,*]
			4: dft = dft[*,0:nfft/2,*,*,*]
			5: dft = dft[*,0:nfft/2,*,*,*,*]
			6: dft = dft[*,0:nfft/2,*,*,*,*,*]
			7: dft = dft[*,0:nfft/2,*,*,*,*,*,*]
			else: message, 'Incorrect number of dimensions: SELF.'
		endcase
	endif

;-----------------------------------------------------
; Output Object \\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\
;-----------------------------------------------------
	;TIME
	if nMax gt 1 then oTime = MrTimeVar( t_fft, 'SSM', /NO_COPY, T_REF=self.oTime[[0]] )

	;FREQUENCY
	if size(freqs, /DIMENSIONS) eq 2 $
		then oFreq = MrTimeSeries( oTime, freqs, /NO_COPY ) $
		else oFreq = MrVariable( freqs, /NO_COPY )

	;DFT
	if nMax gt 1 $
		then oDFT = MrTimeSeries( oTime, dft ) $
		else oDFT = MrVariable( dft, /NO_COPY )

;-----------------------------------------------------
; Attributes \\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\
;-----------------------------------------------------
	;DEPEND_# & DELTA_(MINUS|PLUS)
	if nMax gt 1 then begin
		;dt - Scalar
		if n_elements(dt_fft) eq 1 then begin
			oTime['DELTA_PLUS']  = dt_fft
			oTime['DELTA_MINUS'] = dt_fft
		
		;dt(t)
		endif else begin
			;Create a time-dependent variation
			oDT = MrTimeSeries( oTime, dt_fft, /NO_COPY )
			oDT['TITLE'] = 'Deviation from center time.'
			oDT['UNITS'] = 'seconds'
			
			;Add as attribute to TIME
			oTime['DELTA_PLUS_VAR']  = oDT
			oTime['DELTA_MINUS_VAR'] = oDT
		endelse
		
		;Set DFT dependencies
		oDFT['DEPEND_0']   = oTime
		oDFT['DEPEND_1']   = oFreq
		oDFT['PLOT_TITLE'] = string(nfft, nshift, FORMAT='(%"DFT: NFFT=%i, NSHIFT=%i")')
	
	;Single DFT
	endif else begin
		oDFT['DEPEND_0']   = oFreq
		oDFT['PLOT_TITLE'] = string(nfft, FORMAT='(%"DFT: NFFT=%i")')
	endelse
	
	;TIME
	if nMax gt 1 then begin
		oTime -> SetName, 'fft_tcenter'
		oTime['TITLE'] = 'FFT Center Times'
	endif
	
	;FREQUENCY
	oFreq -> SetName, 'fft_freq'
	oFreq['TITLE'] = 'Freq!C(Hz)'
	oFreq['UNITS'] = 'Hz'
	if tf_one_sided then begin
		oFreq['LOG']        = 1B
		oFreq['AXIS_RANGE'] = [oFreq[1], oFreq[-1]]
	endif
	
	;DFT
	oDFT -> SetName, 'FFT'
	oDFT['TITLE'] = 'FFT'
	oDFT['SCALE'] = 1
	
	return, oDFT
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
	nDims  = (nDimsA > nDimsB) - 1
	if nDimsA eq 1 then message, 'The implicit array must have more than one dimension.'
	if nDimsB eq 1 then message, 'B must have more than one dimension.'
	
	;Output dimensions
	if nDimsA gt nDimsB $
		then outDims = dimsA[0:-2] $
		else outDims = nDimsB eq 2 ? dimsB[0] : [ dimsB[0], dimsB[2:*] ]
	
	;Temporary dimensions
	;   - OUTDIMS, but with non-time-dependent dimensions contracted
	;   - The # operator accepts only two dimensions
	tempDims = nDims eq 1 ? outDims[0] : fix( [outDims[0], product(outDims[1:*])], TYPE=13 )

;-------------------------------------------------------
; Reform Inputs ////////////////////////////////////////
;-------------------------------------------------------
	
	;A is currently D1xD2xD3xD4x ... xDN
	;   - Turn A into D1xDixDN matrix
	if nDimsA le 2 $
		then _A = self['DATA'] $
		else _A = reform( self['DATA'], [ dimsA[0], product(dimsA[1:-2]), dimsA[-1] ] )
	
	;B is currently D1xD2xD3xD4x ... xDN
	;   - Turn B into D1xD2xDi matrix
	if nDimsB le 2 $
		then _B = B['DATA'] $
		else _B = reform( B['DATA'], [ dimsB[0], dimsB[1], product(dimsB[2:*]) ] )

;-------------------------------------------------------
; Inner Product ////////////////////////////////////////
;-------------------------------------------------------
	
	;Allocate memory
	temp = make_array( tempDims, TYPE=size(self[[0]]*B[[0]], /TYPE) )

	;Step through each
	;   - If _A is TxN, we want to keep each time element 1xN so that N is the inner dimension
	;   - If _A is TxNxM, we want each time element to be NxM
	if nDimsA gt 2 $
		then for i = 0, nPts - 1 do temp[i,*] = reform(_A[i,*,*]) # reform(_B[i,*,*]) $
		else for i = 0, nPts - 1 do temp[i,*] = _A[i,*,*]         # reform(_B[i,*,*])
	_A = !Null
	_B = !Null

;-------------------------------------------------------
; Finish Up ////////////////////////////////////////////
;-------------------------------------------------------

	;Reform back to the original shape, minus the contracted dimension.
	temp = reform(temp, outDims, /OVERWRITE)
	
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
	
	if n_elements(name) eq 0 then name = 'Interpol(' + self.name + ')'
	
;-------------------------------------------------------
; MrVariable Object ////////////////////////////////////
;-------------------------------------------------------
	if IsA(Xout, 'MrVariable') then begin
		;Get the time variables
		oT    = self.oTime
		if obj_isa(Xout, 'MrTimeVar') $
			then oTout = Xout $
			else oTout = MrVar_Get(Xout['DEPEND_0'])
		
		;Reference time
		!Null = min( [ oT[0, 'JULDAY'], oTout[0, 'JULDAY'] ], iMin )
		if iMin eq 0 $
			then t_ref = oT[[0]] $
			else t_ref = oTout[[0]]
		
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
; Interpolate Scalar ///////////////////////////////////
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
;   Create a new copy. Properties of the parent object are passed on to the
;   child object.
;-
function MrTimeSeries::New, time, array, $
_REF_EXTRA=extra
	compile_opt idl2
	on_error, 2

	;Return a copy of the array
	;   - Use Obj_Class() so that subclasses can inherit the method.
	return, obj_new(obj_class(self), time, array, _STRICT_EXTRA=extra)
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
		!Null = self -> GetData( oTime, /TVAR )
		nPts  = nPtsA
	endif else begin
		!Null = self -> GetData( oTime, /TVAR )
		nPts  = nPtsB
	endelse

	;Dimensionality of inputs
	dimsA  = size(self, /DIMENSIONS)
	dimsB  = size(B, /DIMENSIONS)
	nDimsA = size(self, /N_DIMENSIONS)
	nDimsB = size(B, /N_DIMENSIONS)
	nDims  = (nDimsA > nDimsB) - 1
	if nDimsA eq 1 then message, 'The implicit array must have more than one dimension.'
	if nDimsB eq 1 then message, 'B must have more than one dimension.'
	
	;Output dimensions
	if nDimsA gt nDimsB $
		then outDims = nDimsA eq 2 ? dimsA[0] : [ dimsA[0], dimsA[2:*] ] $
		else outDims = dimsB[0:-2]
	
	;Temporary dimensions
	;   - OUTDIMS, but with non-time-dependent dimensions contracted
	;   - The ## operator accepts only two dimensions
	tempDims = nDims eq 1 ? outDims[0] : fix( [outDims[0], product(outDims[1:*])], TYPE=13 )

;-------------------------------------------------------
; Reform Inputs ////////////////////////////////////////
;-------------------------------------------------------
	;A is currently D1xD2xD3xD4x ... xDN
	;   - Turn A into D1xD2xDi matrix
	if nDimsA le 2 $
		then _A = self['DATA'] $
		else _A = reform( self['DATA'], [ dimsA[0], dimsA[1], product(dimsA[2:*]) ] )
	
	;B is currently D1xD2xD3xD4x ... xDN
	;   - Turn B into D1xDixDN matrix
	if nDimsB le 2 $
		then _B = B['DATA'] $
		else _B = reform( B['DATA'], [ dimsB[0], product(dimsB[1:-2]), dimsB[-1] ] )

;-------------------------------------------------------
; Outer Product ////////////////////////////////////////
;-------------------------------------------------------
	
	;Allocate memory
	temp = make_array( tempDims, TYPE=size(self[[0]]*B[[0]], /TYPE) )

	;Step through each
	;   - If _B is TxN, we want to keep each time element 1xN so that N is the outer dimension
	;   - If _B is TxNxM, we want B to be NxM
	if nDimsB gt 2 $
		then for i = 0, nPts - 1 do temp[i,*] = reform(_A[i,*,*]) ## reform(_B[i,*,*]) $
		else for i = 0, nPts - 1 do temp[i,*] = reform(_A[i,*,*]) ## _B[i,*,*]
	_A = !Null
	_B = !Null

;-------------------------------------------------------
; Finish Up ////////////////////////////////////////////
;-------------------------------------------------------

	;Reform back to the original shape, minus the contracted dimension.
	temp = reform(temp, outDims, /OVERWRITE)
	
	;Create time series variable
	result = MrTimeSeries( oTime, temp, NAME=name, /NO_COPY )
	return, result
end


;+
;   Compute the PSD
;
; :Params:
;       AMPLITUDE:      out, optional, type=MrTimeVar/MrVariable object
;                       The amplitude of the signal.
;       PHASE:          out, optional, type=MrTimeVar/MrVariable object
;                       The phase of the signal.
;
; :Keywords:
;       CACHE:          in, optional, type=boolean, default=0
;                       If set, `RESULT` will be added to the variable cache.
;       NDETREND:       in, optional, type=integer, default=0
;                       The number of points in a boxcar average. The average is then 
;                           removed from the data to "detrend" it.
;       NAME:           in, optional, type=string, default='PSD('+<name>+')'
;                       Name to be given to `RESULT`.
;       NOISE:          in, optional, type=boolean, default=0
;                       If set, the PSD will be normalized to the noise value. Cannot be
;                           used with `POWER` or `RMS`.
;       POWER:          in, optional, type=boolean, default=0
;                       If set, the PSD will be normalized to the power. Cannot be
;                           used with `NOISE` or `RMS`.
;       RMS:            in, optional, type=boolean, default=0
;                       If set, the PSD will be normalized to the rms value. Cannot be
;                           used with `NOISE` or `POWER`.
;       WINDOW:         in, optional, type=fltarr, default=fltarr()+1
;                       A tapering window to be applied to the data.
;
; :Returns:
;       RESULT:         out, required, type=MrVariable object
;                       The results of the fast fourier transformation. If more than
;                           one FFT is possible, a MrTimeSeries object is returned.
;                           Otherwise, a MrVariable object is returned.
;-
function MrTimeSeries::PSD, $
CACHE=cache, $
NDETREND=nDetrend, $
NAME=name, $
NOISE=noise, $
POWER=power, $
RMS=rms, $
WINDOW=win
	compile_opt idl2
	on_error, 2
	
	tf_rms   = keyword_set(rms)
	tf_noise = keyword_set(noise)
	tf_power = keyword_set(power)
	nfft = self -> GetNPts()
	if n_elements(nDetrend) eq 0 then nDetrend = 0
	if n_elements(name)     eq 0 then name     = 'PSD(' + self.name + ')'
	
	;Gain from window
;	NG = mean(win^2)
;	CG = mean(win)

;-----------------------------------------------------
; Detrend Data \\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\
;-----------------------------------------------------
	if nDetrend gt 0 then begin
		case size(self, /N_DIMENSIONS) of
			1: oVar = self - smooth(self['DATA'], nDetrend, /EDGE_TRUNCATE)
			2: oVar = self - smooth(self['DATA'], [nDetrend,1], /EDGE_TRUNCATE)
			3: oVar = self - smooth(self['DATA'], [nDetrend,1,1], /EDGE_TRUNCATE)
			4: oVar = self - smooth(self['DATA'], [nDetrend,1,1,1], /EDGE_TRUNCATE)
			5: oVar = self - smooth(self['DATA'], [nDetrend,1,1,1,1], /EDGE_TRUNCATE)
			6: oVar = self - smooth(self['DATA'], [nDetrend,1,1,1,1,1], /EDGE_TRUNCATE)
			7: oVar = self - smooth(self['DATA'], [nDetrend,1,1,1,1,1,1], /EDGE_TRUNCATE)
			8: oVar = self - smooth(self['DATA'], [nDetrend,1,1,1,1,1,1,1], /EDGE_TRUNCATE)
			else: message, 'Internal array has invalid number of dimensions.'
		endcase
	endif else begin
		oVar = self
	endelse

;-----------------------------------------------------
; Compute FFT \\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\
;-----------------------------------------------------
	oFFT = oVar -> FFT( nfft, nshift, $
	                    NOISE  = noise, $
	                    POWER  = power, $
	                    RMS    = rms, $
	                    WINDOW = win )
	if ~obj_valid(oFFT) then return, !Null

;-----------------------------------------------------
; One-Sided PSD \\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\
;-----------------------------------------------------

	dims  = size(oFFT, /DIMENSIONS)
	ndims = size(oFFT, /N_DIMENSIONS)
	
	;Positive frequencies
	oFreq = oFFT['DEPEND_0']
	oFreq -> SetData, oFreq[0:nfft/2]
	
	;One-sided
	case ndims of
		1: pwr = 2.0 * oFFT[0:nfft/2]               * conj( oFFT[0:nfft/2] )
		2: pwr = 2.0 * oFFT[0:nfft/2,*]             * conj( oFFT[0:nfft/2,*] )
		3: pwr = 2.0 * oFFT[0:nfft/2,*,*]           * conj( oFFT[0:nfft/2,*,*] )
		4: pwr = 2.0 * oFFT[0:nfft/2,*,*,*]         * conj( oFFT[0:nfft/2,*,*,*] )
		5: pwr = 2.0 * oFFT[0:nfft/2,*,*,*,*]       * conj( oFFT[0:nfft/2,*,*,*,*] )
		6: pwr = 2.0 * oFFT[0:nfft/2,*,*,*,*,*]     * conj( oFFT[0:nfft/2,*,*,*,*,*] )
		7: pwr = 2.0 * oFFT[0:nfft/2,*,*,*,*,*,*]   * conj( oFFT[0:nfft/2,*,*,*,*,*,*] )
		8: pwr = 2.0 * oFFT[0:nfft/2,*,*,*,*,*,*,*] * conj( oFFT[0:nfft/2,*,*,*,*,*,*,*] )
		else: message, 'FFT must have <= 8 dimensions.'
	endcase

;-----------------------------------------------------
; Set Properties |\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\
;-----------------------------------------------------
	
	;PWR attributes
	oPWR  = oFFT -> Copy()
	oPWR -> SetName, name
	oPWR -> SetData, real_part(pwr), /NO_COPY
	oPWR['TITLE'] = 'PSD'
	oPWR['LOG']   = 1
	
	;Frequency attributes
	oFreq['LOG']        = 1B
	oFreq['AXIS_RANGE'] = [oFreq[1], oFreq[-1]]

	;Normalize
;	case 1 of
;		tf_rms:   pwr = NG * pwr / CG^2
;		tf_noise: pwr = pwr / (NG * df)
;		tf_power: pwr = pwr / NG
;		else:     ;Do nothing
;	endcase
	
	;Normalize
;	case 1 of
;		tf_rms:   pwr = NG * pwr / CG^2
;		tf_noise: pwr = NG * df * pwr / CG^2
;		tf_power: pwr = NG * pwr / CG^2
;		else:   ;Nothing
;	endcase
	
	if keyword_set(cache) then oPWR -> Cache
	return, oPWR
end


;+
;   Determine if time stamps are identical. Time stamps are identical if the
;   MrTimeVar objects have the same heap identifier.
;
; :Params:
;       OVAR:           in, required, type=objref
;                       Time stamps to which the internal time series should be identical.
;                           May be a MrTimeVar or MrTimeSeries class or subclass.
;
; :Returns:
;       TF_SAME:        out, required, type=boolean
;                       If true, time stamps are identical. False otherwise.
;-
function MrTimeSeries::IsTimeIdentical, oVar
	compile_opt idl2
	on_error, 2
	
	tf_same = 0B
	if obj_isa(oVar, 'MrTimeVar') then begin
		tf_same = obj_valid(self.oTime, /GET_HEAP_IDENTIFIER) EQ Obj_Valid(oVar, /GET_HEAP_IDENTIFIER)
	endif else if obj_isa(oVar, 'MrTimeSeries') then begin
		tf_same = obj_valid(self.oTime, /GET_HEAP_IDENTIFIER) EQ Obj_Valid(oVar['TIMEVAR'], /GET_HEAP_IDENTIFIER)
	endif else begin
		message, 'OVAR must be a MrTimeVar or MrTimeSeries object.'
	endelse
	
	return, tf_same
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
;       NO_COPY:        in, optional, type=boolean, default=0
;                       If set `DATA` will be copied directly into the object
;                           and will be left undefined (a MrTimeSeries object will not
;                           be destroyed, but its array will be empty).
;       T_NAME:         in, optional, type=integer
;                       Name to be given to the MrTimeVar object. Ignored unless `TIME`
;                           is an array of time stamps.
;       T_REF:              in, optional, type=string, default=''
;                           A reference time from which `TIME` is measured. Applicable
;                               only to certain values of `TYPE`. Must be formatted as
;                               an ISO-8601 string.
;       T_TYPE:         in, optional, type=integer
;                       If `TIME` is an array of time stamps, use this keyword to indicate
;                           the format or time-basis. See MrTimeVar for more details.
;-
PRO MrTimeSeries::SetData, x1, x2, $
DIMENSION=dimension, $
NO_COPY=no_copy, $
T_NAME=t_name, $
T_REF=t_ref, $
T_TYPE=t_type
	Compile_Opt idl2
	On_Error, 2

	IF N_Elements(x2) GT 0 THEN BEGIN
		time   = x1
		ts_var = x2
	ENDIF ELSE BEGIN
		ts_var = x1
	ENDELSE
	
;-----------------------------------------------------
; Check Time \\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\
;-----------------------------------------------------
	;Use existing time
	IF N_Elements(time) EQ 0 THEN BEGIN
		IF Obj_Valid(self.oTime) $
			THEN oTime = self.oTime $
			ELSE Message, 'No time variable exists. Please provide.'
	
	;Check given time
	ENDIF ELSE BEGIN
		;Object
		IF Size(time, /TNAME) EQ 'OBJREF' THEN BEGIN
			;MrTimeVar
			IF Obj_IsA(time, 'MrTimeVar') $
				THEN oTime = time $
				ELSE Message, 'X1 must be a MrTimeVar object.' 
	
		;Name
		ENDIF ELSE IF IsA(time, /SCALAR, 'STRING') THEN BEGIN
			;Cached?
			IF MrVar_IsCached(time) THEN BEGIN
				oTime = MrVar_Get(time)
				IF ~Obj_IsA(oTime, 'MrTimeVar') $
					THEN Message, 'X1 must be the name of a cached MrTimeVar object.'
			ENDIF ELSE BEGIN
				Message, 'X1 must be the name of a cached variable.'
			ENDELSE
	
		;Data
		ENDIF ELSE BEGIN
			oTime = MrTimeVar(time, t_type, NAME=t_name, NO_COPY=no_copy, T_REF=t_ref)
			IF Obj_Valid(oTime) EQ 0 THEN Message, 'Could not create X1 variable.'
		ENDELSE
	ENDELSE
	
;-----------------------------------------------------
; Check Data \\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\
;-----------------------------------------------------
	;Object
	IF Size(ts_var, /TNAME) EQ 'OBJREF' THEN BEGIN
		;MrVariable
		IF Obj_IsA(ts_var, 'MrVariable') $
			THEN data = ts_var -> GetData() $
			ELSE Message, 'Only "MrVariable" objects can be given.'
	
	;Name
	ENDIF ELSE IF MrIsA(ts_var, 'STRING', /SCALAR) THEN BEGIN
		;Cached?
		IF MrVar_IsCached(ts_var) THEN BEGIN
			oData = MrVar_Get(ts_var)
			IF Obj_IsA(oData, 'MrVariable') $
				THEN data = oData -> GetData() $
				ELSE Message, 'Only names of MrVariable objects may be given.'
		ENDIF ELSE BEGIN
			Message, 'Only names of cached variables may be provided.'
		ENDELSE
	
	;Data
	ENDIF ELSE BEGIN
		data = Temporary(ts_var)
	ENDELSE
	
;-----------------------------------------------------
; Put Time First \\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\
;-----------------------------------------------------
	;Dimension sizes
	dims  = size(data, /DIMENSIONS)
	nDims = N_Elements(dims)
	nPts  = N_Elements(oTime)
	
	;Find the time-dependent dimension
	IF N_Elements(dimension) EQ 0 THEN BEGIN
		dimension = Where(dims EQ nPts, nt) + 1
		IF nt GT 1 THEN BEGIN
			IF dims[0] EQ nPts THEN BEGIN
				dimension = 1
				MrPrintF, 'LogWarn', 'Multiple dims of size ' + strtrim(nPts, 2) + '. Assuming DIMENSION=1.'
			ENDIF ELSE BEGIN
				Message, 'Cannot determine time-dependent dimension.'
			ENDELSE
		ENDIF
	ENDIF ELSE BEGIN
		IF dims[dimension-1] NE nPts $
			THEN Message, 'The size of TIME and DATA along DIMENSION must be equal.'
	ENDELSE
	
	;Put the time-dependent dimension first
	IF nDims GT 1 && dimension NE 1 THEN BEGIN
		IF dimension EQ nDims $
			THEN p = [dimension-1, BindGen(nDims-1)] $
			ELSE p = [dimension-1, BindGen(dimension-1), BindGen(nDims-dimension)+dimension]
		
		;Transpose the data
		data = Transpose(Temporary(data), Temporary(p))
	ENDIF
	
;-----------------------------------------------------
; Store Data \\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\
;-----------------------------------------------------
	;Store the data as properties
	;   - Let automatic garbage collection take care OF the OTIME object
	;     and DATA pointer (IDL v8.0+)
	;   - The OTIME property may be shared by another MrTimeSeries object
	;   - The DATA pointer may be saved by a superclass in CASE additional
	;     requirements on the dimensionality OF the data is required.
	;       * e.g. pData = self.data
	;              self  -> MrTimeSeries::SetData, time, newData
	;              IF [dimension check fails] THEN self.data = pData
	self.oTime = oTime
	self.data  = Ptr_New(data, /NO_COPY)
	
	;Copy attributes
	IF IsA(ts_var, 'MrVariable') $
		THEN ts_var -> CopyAttrTo, self
	
	;Set attributes
	self -> SetAttrValue, 'DEPEND_0',  self.oTime, /CREATE
	self -> SetAttrValue, 'DIMENSION', 1,          /CREATE
	
	;Clear data
	;   - Do not erase a MrTimeVar object. It may be referenced
	;     as a DEPEND_0 attribute FOR other objects.
	IF Keyword_Set(no_copy) THEN BEGIN
		IF N_Elements(x2) EQ 0 THEN BEGIN
			x1 = !Null
		ENDIF ELSE BEGIN
			IF Size(x1, /TNAME) NE 'OBJREF' THEN x1 = !Null
			x2 = !Null
		ENDELSE
	ENDIF
END


;+
;   Create a spectrogram
;
; :Params:
;       NFFT:           in, optional, type=integer, default=all
;                       Number of points per FFT.
;       NSHIFT:         in, optional, type=integer, default=`NFFT`/2.0
;                       Number of points to shift between FFTs.
;
; :Params:
;       AMPLITUDE:      out, optional, type=MrTimeVar/MrVariable object
;                       The amplitude of the signal.
;       PHASE:          out, optional, type=MrTimeVar/MrVariable object
;                       The phase of the signal.
;
; :Returns:
;       RESULT:         out, required, type=MrTimeSeries or MrVariable object
;                       The results of the fast fourier transformation. If more than
;                           one FFT is possible, a MrTimeSeries object is returned.
;                           Otherwise, a MrVariable object is returned.
;-
function MrTimeSeries::Spectrogram, nfft, nshift, $
CACHE=cache, $
NAME=name, $
NOISE=noise, $
POWER=power, $
RMS=rms, $
WINDOW=win
	compile_opt idl2
	on_error, 2
	
	tf_rms   = keyword_set(rms)
	tf_noise = keyword_set(noise)
	tf_power = keyword_set(power)
	if n_elements(nfft) eq 0 then nfft = self -> GetNPts()
	if n_elements(name) eq 0 then name = 'Spectrogram(' + self.name + ')'
	
	;Gain from window
;	NG = mean(win^2)
;	CG = mean(win)

;-----------------------------------------------------
; Compute FFT \\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\
;-----------------------------------------------------
	oFFT = self -> FFT( nfft, nshift, $
	                    NOISE  = noise, $
	                    POWER  = power, $
	                    RMS    = rms, $
	                    WINDOW = win )
	if ~obj_valid(oFFT) then return, !Null

;-----------------------------------------------------
; One-Sided Spectrogram \\\\\\\\\\\\\\\\\\\\\\\\\\\\\
;-----------------------------------------------------

	dims  = size(oFFT, /DIMENSIONS)
	ndims = size(oFFT, /N_DIMENSIONS)
	
	;Compute power spectral density
	if obj_isa(oFFT, 'MrTimeSeries') then begin
		;Positive frequencies
		oFreq = oFFT['DEPEND_1']
		if size(oFreq, /N_DIMENSIONS) eq 1 $
			then oFreq -> SetData, oFreq[0:nfft/2] $
			else oFreq -> SetData, oFreq[*,0:nfft/2]
		
		;One-side
		case ndims of
			2: pwr = 2.0 * oFFT[*,0:nfft/2]             * conj( oFFT[*,0:nfft/2] )
			3: pwr = 2.0 * oFFT[*,0:nfft/2,*]           * conj( oFFT[*,0:nfft/2,*] )
			4: pwr = 2.0 * oFFT[*,0:nfft/2,*,*]         * conj( oFFT[*,0:nfft/2,*,*] )
			5: pwr = 2.0 * oFFT[*,0:nfft/2,*,*,*]       * conj( oFFT[*,0:nfft/2,*,*,*] )
			6: pwr = 2.0 * oFFT[*,0:nfft/2,*,*,*,*]     * conj( oFFT[*,0:nfft/2,*,*,*,*] )
			7: pwr = 2.0 * oFFT[*,0:nfft/2,*,*,*,*,*]   * conj( oFFT[*,0:nfft/2,*,*,*,*,*] )
			8: pwr = 2.0 * oFFT[*,0:nfft/2,*,*,*,*,*,*] * conj( oFFT[*,0:nfft/2,*,*,*,*,*,*] )
			else: message, 'FFT must have <= 8 dimensions.'
		endcase
	
	;Single PSD
	endif else begin
		;Positive frequencies
		oFreq = oFFT['DEPEND_0']
		oFreq -> SetData, oFreq[0:nfft/2]
		
		;One-sided
		case ndims of
			1: pwr = 2.0 * oFFT[0:nfft/2]               * conj( oFFT[0:nfft/2] )
			2: pwr = 2.0 * oFFT[0:nfft/2,*]             * conj( oFFT[0:nfft/2,*] )
			3: pwr = 2.0 * oFFT[0:nfft/2,*,*]           * conj( oFFT[0:nfft/2,*,*] )
			4: pwr = 2.0 * oFFT[0:nfft/2,*,*,*]         * conj( oFFT[0:nfft/2,*,*,*] )
			5: pwr = 2.0 * oFFT[0:nfft/2,*,*,*,*]       * conj( oFFT[0:nfft/2,*,*,*,*] )
			6: pwr = 2.0 * oFFT[0:nfft/2,*,*,*,*,*]     * conj( oFFT[0:nfft/2,*,*,*,*,*] )
			7: pwr = 2.0 * oFFT[0:nfft/2,*,*,*,*,*,*]   * conj( oFFT[0:nfft/2,*,*,*,*,*,*] )
			8: pwr = 2.0 * oFFT[0:nfft/2,*,*,*,*,*,*,*] * conj( oFFT[0:nfft/2,*,*,*,*,*,*,*] )
			else: message, 'FFT must have <= 8 dimensions.'
		endcase
	endelse
	
	;PWR attributes
	oPWR  = oFFT -> Copy()
	oPWR -> SetName, name
	oPWR -> SetData, real_part(pwr), /NO_COPY
	oPWR['TITLE'] = 'PSD'
	oPWR['LOG']   = 1
	
	;Frequency attributes
	oFreq['LOG']        = 1B
	oFreq['AXIS_RANGE'] = [oFreq[1], oFreq[-1]]

	;Normalize
;	case 1 of
;		tf_rms:   pwr = NG * pwr / CG^2
;		tf_noise: pwr = pwr / (NG * df)
;		tf_power: pwr = pwr / NG
;		else:     ;Do nothing
;	endcase
	
	;Normalize
;	case 1 of
;		tf_rms:   pwr = NG * pwr / CG^2
;		tf_noise: pwr = NG * df * pwr / CG^2
;		tf_power: pwr = NG * pwr / CG^2
;		else:   ;Nothing
;	endcase
	
	if keyword_set(cache) then oPWR -> Cache
	return, oPWR
end



;+
;   Transpose the array. Attributes are copied to the results.
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
		nDims = size(self, /N_DIMENSIONS)
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
	result  = self -> Copy('Transpose(' + self.name + ')')
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