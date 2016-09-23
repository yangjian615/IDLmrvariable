; docformat = 'rst'
;
; NAME:
;   MrVariable__Define
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
;       method for the object on the left side first. So, for two MrVariable objects,
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
;       The ::Cache method will cache the MrVariable object. When the object is destroyed,
;       it will automatically be removed from the cache.
;
; :Categories:
;   MrVariable, Graphics
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
;       2014/05/09  -   Written by Matthew Argall
;-
;*****************************************************************************************
;+
;   The initialization method.
;
; :Examples:
;   Create a MrVariable object using a pre-existing array::
;       theArray = findgen(24, 36)
;       myArray  = MrVariable(theArray)
;       help, myArray
;           MYARRAY     OBJREF      <ObjHeapVar666(MrVariable)>
;             ARRAY       FLOAT     = Array[24, 36]
;
;   Initialize a MrVariable object via Make_Array::
;       myArray = MrVariable(24, 36, TYPE='ULong')
;       help, myArray
;           MYARRAY     OBJREF      <ObjHeapVar668(MrVariable)>
;             ARRAY       ULONG     = Array[24, 36]
;
;   Initialize a MrVariable object via RandomU::
;       myArray = MrVariable(24, 36, SEED=3)
;       print, myarray[0:3]
;           0.897916     0.558249     0.766930     0.589101
;
;   Initialize a MrVariable with a normal, gaussian distribution::
;       myArray = MrVariable(24, 36, SEED=3, /NORMAL)
;
; :Params:
;       DATA:               in, optional, type=any/integer
;                           If an array, then it is the array to be stored and all other
;                               parameters are ignored. If a scalar value is given,
;                               `MAKE` is set to 1 automatically, unless `RANDOM` or `NORMAL`
;                               are in use.
;       D2:                 in, optional, type=integer
;                           Size of the second dimension. Ignored unless `DATA` is scalar.
;       D3:                 in, optional, type=integer
;                           Size of the third dimension. Ignored unless `DATA` is scalar.
;       D4:                 in, optional, type=integer
;                           Size of the fourth dimension. Ignored unless `DATA` is scalar.
;       D5:                 in, optional, type=integer
;                           Size of the fifth dimension. Ignored unless `DATA` is scalar.
;       D6:                 in, optional, type=integer
;                           Size of the sixth dimension. Ignored unless `DATA` is scalar.
;       D7:                 in, optional, type=integer
;                           Size of the seventh dimension. Ignored unless `DATA` is scalar.
;       D8:                 in, optional, type=integer
;                           Size of the eighth dimension. Ignored unless `DATA` is scalar.
;
; :Keywords:
;       CACHE:              in, optional, type=boolean, default=0
;                           If set, the variable will be placed into the cache.
;       MAKE:               in, optional, type=boolean
;                           If set, all parameters will be passed to the Make_Array
;                               function in order to create the array. This is the default
;                               if `DATA` is a scalar.
;       NORMAL:             in, optional, type=boolean, default=0
;                           If set, then `SEED` will be input to the RandomN function
;                               instead of the RandomU function.
;       NO_CLOBBER:         in, optional, type=boolean, default=0
;                           If set and `NAME` already exists in the cache, then append
;                               '_#' to `NAME` to avoid conflict. "_#" is the smallest
;                               integer that results in a unique name. The default is to
;                               clobber cached variables with the same name.
;       NO_COPY:            in, optional, type=boolean, default=0
;                           If set, `DATA` will be copied directly into the object and
;                               will be left undefined.
;       RANDOM:             in, optional, type=boolean, default=0
;                           If set, the RandomU function will be used to initialize the
;                               array instead of Make_Array. `SEED`, then, is the seed,
;                               and `DATA`, `D2`, etc. specify the dimensions of the
;                               resulting array. See IDL's RandomU function for more
;                               details.
;       SEED:               in, out, optional, type=boolean, default=long
;                           First parameter to the RandomU function. See the `RANDOM`
;                               keyword. If a value is given, `RANDOM` is automatically
;                               set to 1.
;       TO_ARRAY:           in, optional, type=boolean, default=0
;                           If set, `DATA` is a structure, list, or hash whose contents
;                               are to be converted to an array.
;       TYPE:               in, optional, type=int/string, default='FLOAT'
;                           The type of array to be created. Used with `MAKE`.
;       _REF_EXTRA:         in, optional, type=any
;                           Any keyword accepted by IDL's Make_Array, RandomU, or RandomN
;                               function, or by the ToArray method, depending on the
;                               keywords used.
;-
function MrVariable::INIT, data, D2, D3, D4, D5, D6, D7, D8, $
CACHE=cache, $
MAKE=make, $
NAME=name, $
NO_CLOBBER=no_clobber, $
NORMAL=normal, $
NO_COPY=no_copy, $
RANDOM=random, $
SEED=seed, $
TO_ARRAY=to_array, $
TYPE=type, $
_REF_EXTRA=extra
	compile_opt idl2

	;Error handling
	catch, the_error
	if the_error ne 0 then begin
		catch, /CANCEL
		MrPrintF, 'LogErr'
		return, 0
	endif

	;Defaults
	tf_cache = keyword_set(cache)
	if n_elements(name) eq 0 then name = 'MrVariable'

	;Allocate heap to pointers.
	self.attributes       = hash()
	self.data             = ptr_new(/ALLOCATE_HEAP)
	self._append_unReform = ptr_new(/ALLOCATE_HEAP)
	self._append_unTrans  = ptr_new(/ALLOCATE_HEAP)

	;Create the array
	make     = keyword_set(make)
	normal   = keyword_set(normal)
	random   = keyword_set(random)
	to_array = keyword_set(to_array)
	if n_elements(seed) gt 0 then random = ~normal
	if IsA(data, /SCALAR) && random eq 0 then make = 1
	if make + normal + random + to_array gt 1 then message, 'MAKE, NORMAL, RANDOM and TO_ARRAY are mutually exclusive.'

	;Were inputs given?
	;   `DATA` can be undefined if RANDOM or NORMAL are set.
	case 1 of
		make:     self -> Make_Array, data, D2, D3, D4, D5, D6, D7, D8, TYPE=type, _REF_EXTRA=extra
		normal:   self -> RandomN, seed, data, D2, D3, D4, D5, D6, D7, D8, _REF_EXTRA=extra
		random:   self -> RandomU, seed, data, D2, D3, D4, D5, D6, D7, D8, _REF_EXTRA=extra
		to_array: self -> ToArray, data, _REF_EXTRA=extra
		n_elements(data) gt 0: begin
			if n_elements(extra) gt 0 then message, 'EXTRA keywords not allowed when DATA is an array.'
			self -> SetData, data, NO_COPY=no_copy
		endcase
		else: ;Leave the array empty
	endcase

	;Set properties
	if n_elements(name) gt 0 then self -> SetName, name
	
	;Cache the array
	if tf_cache then self -> Cache, NO_CLOBBER=no_clobber

	return, 1
end


;+
;   Clean up after the object is destroyed
;-
pro MrVariable::CLEANUP
	compile_opt idl2
	on_error, 2
	
	;Common block
	@mrvar_common
	
	;Remove from the variable container
	if n_elements(MrVarCache) gt 0 then begin
		if MrVarCache -> IsContained(self) then MrVarCache -> Remove, self
	endif
	
	;Destroy objects
	obj_destroy, self.attributes

	;Free pointers
	ptr_free, self.data
	ptr_free, self._append_unReform
	ptr_free, self._append_unTrans
end


;+
;   Helper method to determine if "self" was given on the left or right side of an
;   operator.
;
; :Private:
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
;       SIDE:               Returns 'RIGHT' if "self" was provided on the right side of
;                               the operator and 'LEFT' if it was provided on the left.
;-
function MrVariable::_LeftOrRight, left, right, $
ISMRVARIABLE=isMrVariable
	compile_opt idl2
	on_error, 2

	;Information about inputs
	lType = size(left, /TNAME)
	rType = size(right, /TNAME)
	heapID = obj_valid(self, /GET_HEAP_IDENTIFIER)

	;Was "self" given on the left of operator?
	side = ''
	if lType eq 'OBJREF' then begin
		lHeapID = obj_valid(left, /GET_HEAP_IDENTIFIER)
		if lHeapID eq heapID then side = 'LEFT'
	endif

	;Was "self" given on the right of the AND operator? Treat the special case
	;where "self" is given on both sides.
	if rType eq 'OBJREF' then begin
		rHeapID = obj_valid(right, /GET_HEAP_IDENTIFIER)
		if rHeapID eq heapID then if side ne 'LEFT' then side = 'RIGHT'
	endif

	;Is the other argument a MrVariable object as well?
	if arg_present(isMrVariable) then begin
		case side of
			'LEFT':  isMrVariable = rtype eq 'OBJREF' && obj_isa(right, 'MrVariable')
			'RIGHT': isMrVariable = ltype eq 'OBJREF' && obj_isa(left,  'MrVariable')
		endcase
	endif

	return, side
end


;+
;   The purpose of this method is to perform a bit-wise comparison between two
;   integer, longword or byte expressions. For other types, `RIGHT` is returned unless
;   `LEFT` is zero or the empty string, in which case 0 (zero) is returned.
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
;       RESULT:             Returns 1 if there is a current element to retrieve, and 0 if
;                               there are no more elements.
;-
function MrVariable::_OverloadAnd, left, right
	compile_opt idl2
	on_error, 2

	;Is SELF on the left or right?
	;   - Are both LEFT and RIGHT GDA data objects?
	side = self -> _LeftOrRight(left, right, ISMrVariable=IsMrVariable)

;-------------------------------------------------------
; Two MrVariable Objects ///////////////////////////////
;-------------------------------------------------------
	;Both LEFT and RIGHT are MrVariable objects
	if IsMrVariable then begin
		;Add
		result = (*self.data) and right['ARRAY']
		
		;Create a new name
		name = self.name + ' AND ' + right.name

;-------------------------------------------------------
; MrVariable with Expression ///////////////////////////
;-------------------------------------------------------
	endif else begin
		;Add the expressions
		;   - Assume the user knows what they are doing.
		;   - All IDL truncation effects apply (shortest in determines size out).
		if side eq 'LEFT' $
			then result = (*self.data) and right['ARRAY'] $
			else result = left['ARRAY'] and (*self.data)
		
		;Determine name
		;   - Scalar or TYPE[dims]
		if side eq 'LEFT' then begin
			if n_elements(right) eq 1 $
				then rname = strtrim(right[0], 2) $
				else rname = size(right, /TNAME) + '[' + strjoin(strtrim(size(right, /DIMENSIONS), 2), ',') + ']'
			name = self.name + ' AND ' + rname
		endif else begin
			if n_elements(left) eq 1 $
				then lname = strtrim(left[0], 2) $
				else lname = size(left, /TNAME) + '[' + strjoin(strtrim(size(left, /DIMENSIONS), 2), ',') + ']'
			name = lname + ' AND ' + self.name
		endelse
	endelse

;-------------------------------------------------------
; Output Object ////////////////////////////////////////
;-------------------------------------------------------
	;Create a new object based on the results
	return, self -> New(result, /NO_COPY, NAME=name)
end


;+
;   The purpose of this method is to multiply two expressions together.
;
;   NOTE:
;     If one of LEFT or RIGHT is an expression, normal IDL operations
;     will take effect. This means the output will be the size of the
;     array with the smallest dimensions.
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
function MrVariable::_OverloadAsterisk, left, right
	compile_opt idl2
	on_error, 2

	;Is SELF on the left or right?
	;   - Are both LEFT and RIGHT GDA data objects?
	side = self -> _LeftOrRight(left, right, ISMrVariable=IsMrVariable)

;-------------------------------------------------------
; Two MrVariable Objects //////////////////////////////////
;-------------------------------------------------------
	;Both LEFT and RIGHT are MrVariable objects
	if IsMrVariable then begin
		;Multiply
		result = (*self.data) * right['ARRAY']
		
		;Create a new name
		name = self.name + '*' + right.name

;-------------------------------------------------------
; MrVariable with Expression //////////////////////////////
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
			if n_elements(right) eq 1 $
				then rname = strtrim(right[0], 2) $
				else rname = size(right, /TNAME) + '[' + strjoin(strtrim(size(right, /DIMENSIONS), 2), ',') + ']'
			name = self.name + '*' + rname
		endif else begin
			if n_elements(left) eq 1 $
				then lname = strtrim(left[0], 2) $
				else lname = size(left, /TNAME) + '[' + strjoin(strtrim(size(left, /DIMENSIONS), 2), ',') + ']'
			name = lname + '*' + self.name
		endelse
	endelse

;-------------------------------------------------------
; Output Object ////////////////////////////////////////
;-------------------------------------------------------
	;Create a new object based on the results
	return, self -> New(result, /NO_COPY, NAME=name)
end


;+
;   The purpose of this method is to apply exponentiation with the caret operator.
;
;   NOTE:
;     If one of LEFT or RIGHT is an expression, normal IDL operations
;     will take effect. This means the output will be the size of the
;     array with the smallest dimensions.
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
;       RESULT:             The result of raising `LEFT` to the power of `RIGHT`.
;-
function MrVariable::_OverloadCaret, left, right
	compile_opt idl2
	on_error, 2

	;Is SELF on the left or right?
	;   - Are both LEFT and RIGHT GDA data objects?
	side = self -> _LeftOrRight(left, right, ISMrVariable=IsMrVariable)

;-------------------------------------------------------
; Two MrVariable Objects //////////////////////////////////
;-------------------------------------------------------
	;Both LEFT and RIGHT are MrVariable objects
	if IsMrVariable then begin
		;Caret
		result = (*self.data) ^ right['DATA']
		
		;Create a new name
		name = self.name + '^' + right.name

;-------------------------------------------------------
; MrVariable with Expression //////////////////////////////
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
			if n_elements(right) eq 1 $
				then rname = strtrim(right[0], 2) $
				else rname = size(right, /TNAME) + '[' + strjoin(strtrim(size(right, /DIMENSIONS), 2), ',') + ']'
			name = self.name + '^' + rname
		endif else begin
			if n_elements(left) eq 1 $
				then lname = strtrim(left[0], 2) $
				else lname = size(left, /TNAME) + '[' + strjoin(strtrim(size(left, /DIMENSIONS), 2), ',') + ']'
			name = lname + '^' + self.name
		endelse
	endelse

;-------------------------------------------------------
; Output Object ////////////////////////////////////////
;-------------------------------------------------------
	;Create a new object based on the results
	return, self -> New(result, /NO_COPY, NAME=name)
end


;+
;   Allow square-bracket array indexing from the left side of an operator.
;
; Notes:
;   This method works, but was replaced by the other _OverloadBracketsLeftSide method.
;   It is my own attempt at bracket overloading.
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
;       SUBSCRIPT1:         in, required, type=integer/intarr(3)
;                           Index subscripts. Either a scalar, an index array, or a 
;                               subscript range in the form [start, stop, step_size]
;       SUBSCRIPT2:         in, optional, type=integer/intarr(3)
;                           Index subscripts.
;       SUBSCRIPT3:         in, optional, type=integer/intarr(3)
;                           Index subscripts.
;       SUBSCRIPT4:         in, optional, type=integer/intarr(3)
;                           Index subscripts.
;       SUBSCRIPT5:         in, optional, type=integer/intarr(3)
;                           Index subscripts.
;       SUBSCRIPT6:         in, optional, type=integer/intarr(3)
;                           Index subscripts.
;       SUBSCRIPT7:         in, optional, type=integer/intarr(3)
;                           Index subscripts.
;       SUBSCRIPT8:         in, optional, type=integer/intarr(3)
;                           Index subscripts.
;-
pro MrVariable::_OverloadBLS, objRef, value, isRange, subscript1, subscript2, $
                                        subscript3, subscript4, subscript5, $
                                        subscript6, subscript7, subscript8
	compile_opt idl2
	on_error, 2

	;Reform VALUE into a 1D array
	nValues = n_elements(value)
	valueOut = reform(value, nValues)

	;Get the implicit array dimensions and reform the subscripts into a 1D array
	;of indices.
	self -> GetProperty, DIMENSIONS=dims
	indices = MrReformIndices(dims, isRange, subscript1, subscript2, subscript3, $
	                                         subscript4, subscript5, subscript6, $
	                                         subscript7, subscript8)

	;Set the values of the implicit array
	(*self.data)[indices] = valueOut
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
pro MrVariable::_OverloadBracketsLeftSide, objRef, value, isRange, i1, i2, i3, i4, i5, i6, i7, i8
	compile_opt idl2
	on_error, 2

	;Number of subscripts given
	nSubscripts = N_ELEMENTS(isRange)

;---------------------------------------------------------------------
;Brute Force Subscripting ////////////////////////////////////////////
;---------------------------------------------------------------------
	;
	; Highly optimized code for three dimensions or lower. 
	; Handle all combinations of subscript ranges or indices.
	;

	;<= 3D subscript range
	if (nSubscripts le 3) then begin
	;---------------------------------------------------------------------
	;3D Subscripts ///////////////////////////////////////////////////////
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
	;2D Subscripts ///////////////////////////////////////////////////////
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
	;1D Subscripts ///////////////////////////////////////////////////////
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
	indices = MrReformIndices(dims, isRange, i1, i2, i3, i4, i5, i6, i7, i8);, /IDL_METHOD)
	(*self.data)[indices] = value
end 


;+
;   Allow square-bracket array indexing from the right side of an operator.
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
function MrVariable::_OverloadBracketsRightSide, isRange, i1, i2, i3, i4, i5, i6, i7, i8
	compile_opt idl2
	on_error, 2

	;Number of subscripts given
	nSubscripts = n_elements(isRange)

	;String operations.
	if IsA(i1, /SCALAR, 'STRING') then begin
		case strupcase(i1) of
			'DATA':    return, *self.data
			'POINTER': return,  self.data
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
;   The purpose of this method is to check for equality. See IDL's online help for
;   `Relational Operators <http://exelisvis.com/docs/Relational_Operators.html>`
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
;       RESULT:             True (1) if `LEFT` equals `RIGHT`. Otherwise false (0)
;-
function MrVariable::_OverloadEQ, left, right
	compile_opt idl2
	on_error, 2

	;Is SELF on the left or right?
	;   - Are both LEFT and RIGHT GDA data objects?
	side = self -> _LeftOrRight(left, right, ISMrVariable=IsMrVariable)

;-------------------------------------------------------
; Two MrVariable Objects ///////////////////////////////
;-------------------------------------------------------
	;Both LEFT and RIGHT are MrVariable objects
	if IsMrVariable then begin
		;Add
		result = (*self.data) eq right['DATA']
		
		;Create a new name
		name = self.name + ' EQ ' + right.name

;-------------------------------------------------------
; MrVariable with Expression ///////////////////////////
;-------------------------------------------------------
	endif else begin
		;Add the expressions
		;   - Assume the user knows what they are doing.
		;   - All IDL truncation effects apply (shortest in determines size out).
		if side eq 'LEFT' $
			then result = (*self.data) eq right['DATA'] $
			else result = left['DATA'] eq (*self.data)
		
		;Determine name
		;   - Scalar or TYPE[dims]
		if side eq 'LEFT' then begin
			if n_elements(right) eq 1 $
				then rname = strtrim(right[0], 2) $
				else rname = size(right, /TNAME) + '[' + strjoin(strtrim(size(right, /DIMENSIONS), 2), ',') + ']'
			name = self.name + ' EQ ' + rname
		endif else begin
			if n_elements(left) eq 1 $
				then lname = strtrim(left[0], 2) $
				else lname = size(left, /TNAME) + '[' + strjoin(strtrim(size(left, /DIMENSIONS), 2), ',') + ']'
			name = lname + ' EQ ' + self.name
		endelse
	endelse

;-------------------------------------------------------
; Output Object ////////////////////////////////////////
;-------------------------------------------------------
	;Create a new object based on the results
	return, self -> New(result, /NO_COPY, NAME=name)
end


;+
;   The purpose of this method is to provide iterations to the FOREACH operator. Iterate
;   over all of the elements in the array.
;
; :Params:
;       VALUE:              out, required, type=scalar
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
function MrVariable::_OverloadForeach, value, key
	compile_opt idl2
	on_error, 2

	nPts = n_elements(*self.data)
	if n_elements(key) eq 0 then key = 0

	;Get the array element if the index is in range
	if key lt nPts then begin
		next = 1
		value = (*self.data)[key]
	
	;Otherwise, stop iterating 
	endif else next = 0

	;Next element to retrieve
	key += 1

	return, next
end


;+
;   The purpose of this method is to check if `LEFT` is greater than or equal to `RIGHT`.
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
;       RESULT:             True (1) if `LEFT` greater than or equal to `RIGHT`.
;                               Otherwise false (0).
;-
function MrVariable::_OverloadGE, left, right
	compile_opt idl2
	on_error, 2

	;Is SELF on the left or right?
	;   - Are both LEFT and RIGHT GDA data objects?
	side = self -> _LeftOrRight(left, right, ISMrVariable=IsMrVariable)

;-------------------------------------------------------
; Two MrVariable Objects ///////////////////////////////
;-------------------------------------------------------
	;Both LEFT and RIGHT are MrVariable objects
	if IsMrVariable then begin
		;Add
		result = (*self.data) ge right['DATA']
		
		;Create a new name
		name = self.name + ' GE ' + right.name

;-------------------------------------------------------
; MrVariable with Expression ///////////////////////////
;-------------------------------------------------------
	endif else begin
		;Add the expressions
		;   - Assume the user knows what they are doing.
		;   - All IDL truncation effects apply (shortest in determines size out).
		if side eq 'LEFT' $
			then result = (*self.data) ge right $
			else result = left ge (*self.data)
		
		;Determine name
		;   - Scalar or TYPE[dims]
		if side eq 'LEFT' then begin
			if n_elements(right) eq 1 $
				then rname = strtrim(right[0], 2) $
				else rname = size(right, /TNAME) + '[' + strjoin(strtrim(size(right, /DIMENSIONS), 2), ',') + ']'
			name = self.name + ' GE ' + rname
		endif else begin
			if n_elements(left) eq 1 $
				then lname = strtrim(left[0], 2) $
				else lname = size(left, /TNAME) + '[' + strjoin(strtrim(size(left, /DIMENSIONS), 2), ',') + ']'
			name = lname + ' GE ' + self.name
		endelse
	endelse

;-------------------------------------------------------
; Output Object ////////////////////////////////////////
;-------------------------------------------------------
	;Create a new object based on the results
	return, self -> New(result, /NO_COPY, NAME=name)
end


;+
;   The purpose of this method is to check if `LEFT` is greater than `RIGHT`. See IDL's
;   online help for `Relational Operators <http://exelisvis.com/docs/Relational_Operators.html>`
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
;       RESULT:             True (1) if `LEFT` greater than or equal to `RIGHT`.
;                               Otherwise false (0).
;-
function MrVariable::_OverloadGT, left, right
	compile_opt idl2
	on_error, 2

	;Is SELF on the left or right?
	;   - Are both LEFT and RIGHT GDA data objects?
	side = self -> _LeftOrRight(left, right, ISMrVariable=IsMrVariable)

;-------------------------------------------------------
; Two MrVariable Objects ///////////////////////////////
;-------------------------------------------------------
	;Both LEFT and RIGHT are MrVariable objects
	if IsMrVariable then begin
		;Add
		result = (*self.data) gt right['DATA']
		
		;Create a new name
		name = self.name + ' GT ' + right.name

;-------------------------------------------------------
; MrVariable with Expression ///////////////////////////
;-------------------------------------------------------
	endif else begin
		;Add the expressions
		;   - Assume the user knows what they are doing.
		;   - All IDL truncation effects apply (shortest in determines size out).
		if side eq 'LEFT' $
			then result = (*self.data) gt right $
			else result = left gt (*self.data)
		
		;Determine name
		;   - Scalar or TYPE[dims]
		if side eq 'LEFT' then begin
			if n_elements(right) eq 1 $
				then rname = strtrim(right[0], 2) $
				else rname = size(right, /TNAME) + '[' + strjoin(strtrim(size(right, /DIMENSIONS), 2), ',') + ']'
			name = self.name + ' GT ' + rname
		endif else begin
			if n_elements(left) eq 1 $
				then lname = strtrim(left[0], 2) $
				else lname = size(left, /TNAME) + '[' + strjoin(strtrim(size(left, /DIMENSIONS), 2), ',') + ']'
			name = lname + ' GT ' + self.name
		endelse
	endelse

;-------------------------------------------------------
; Output Object ////////////////////////////////////////
;-------------------------------------------------------
	;Create a new object based on the results
	return, self -> New(result, /NO_COPY, NAME=name)
end


;+
;   The purpose of this method is to return the element-by-element maximum between
;   `LEFT` and `RIGHT`. See IDL's online help for `Relational Operators <http://exelisvis.com/docs/Relational_Operators.html>`
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
;       RESULT:             Returns the element of `LEFT` if it is greater than `RIGHT`,
;                               and vice versa.
;-
function MrVariable::_OverloadGreaterThan, left, right
	compile_opt idl2
	on_error, 2

	;Is SELF on the left or right?
	;   - Are both LEFT and RIGHT GDA data objects?
	side = self -> _LeftOrRight(left, right, ISMrVariable=IsMrVariable)

;-------------------------------------------------------
; Two MrVariable Objects ///////////////////////////////
;-------------------------------------------------------
	;Both LEFT and RIGHT are MrVariable objects
	if IsMrVariable then begin
		;Add
		result = (*self.data) > right['DATA']
		
		;Create a new name
		name = self.name + ' > ' + right.name

;-------------------------------------------------------
; MrVariable with Expression ///////////////////////////
;-------------------------------------------------------
	endif else begin
		;Add the expressions
		;   - Assume the user knows what they are doing.
		;   - All IDL truncation effects apply (shortest in determines size out).
		if side eq 'LEFT' $
			then result = (*self.data) > right $
			else result = left > (*self.data)
		
		;Determine name
		;   - Scalar or TYPE[dims]
		if side eq 'LEFT' then begin
			if n_elements(right) eq 1 $
				then rname = strtrim(right[0], 2) $
				else rname = size(right, /TNAME) + '[' + strjoin(strtrim(size(right, /DIMENSIONS), 2), ',') + ']'
			name = self.name + ' > ' + rname
		endif else begin
			if n_elements(left) eq 1 $
				then lname = strtrim(left[0], 2) $
				else lname = size(left, /TNAME) + '[' + strjoin(strtrim(size(left, /DIMENSIONS), 2), ',') + ']'
			name = lname + ' > ' + self.name
		endelse
	endelse

;-------------------------------------------------------
; Output Object ////////////////////////////////////////
;-------------------------------------------------------
	;Create a new object based on the results
	return, self -> New(result, /NO_COPY, NAME=name)
end


;+
;   The purpose of this method is to provide information when the HELP procedure
;   is called.
;
; :Params:
;       VARNAME:        in, required, type=string
;                       Name of the variable supplied to the HELP procedure.
;-
function MrVariable::_OverloadHelp, varname
	compile_opt idl2
	on_error, 2

	;Info about the object
	heapnum  = obj_valid(self, /GET_HEAP_IDENTIFIER)
	type     = size(self, /TNAME)
	class    = obj_class(self)
	dims     = size(*self.data, /DIMENSIONS)
	datatype = size(*self.data, /TNAME)

	;Name, variable name, and object reference info of variable.
	fmt_varname = 'a-' + string(strlen(varname)     > 12, FORMAT='(i0)')
	fmt_name    = 'a-' + string(strlen(self.name)+2 > 12, FORMAT='(i0)')
	str = string(varname, '"' + self.name + '"', '<', heapnum, ' (' + class + ')>', $
	             FORMAT='(' + fmt_varname + ', ' + fmt_name + ', 3x, a1, i0, a0)')

	;Description of datatype and dimension sizes
	;   - If one element, give value.
	if n_elements(*self.data) eq 1 $
		then desc = string(datatype, FORMAT='(a-8)') + ' = ' + strtrim(string(*self.data, /PRINT), 2) $
		else desc = string(datatype, FORMAT='(a-8)') + ' = Array[' + strjoin(string(dims, FORMAT='(i0)'), ',') + ']'

	;Combine the two
	str += '  ' + desc
	return, str
end


;+
;   The purpose of this method is to provide information when implied print is used.
;-
function MrVariable::_OverloadImpliedPrint
	compile_opt idl2
	on_error, 2

	;Print the array
	return, *self.data
end


;+
;   The purpose of this method is to evaluate if the array is true or not. Called when
;   logical operators are used (&&, ||, etc.)
;
; :Returns:
;       RESULT:         True (1) if the implicit array is a scalar or 1 element array
;                           not equal to zero and false if a scalar not equal to zero.
;                           Arrays with more than one element result in an error.
;-
function MrVariable::_OverloadIsTrue
	compile_opt idl2
	on_error, 2

	;Make sure the array is a scalar
	if self.n_elements gt 1 then message, 'Array must be a scalar or 1 element array.'

	;Is it true?
	result = (*self.data)[0] ne 0

	;Print the array
	return, result
end


;+
;   The purpose of this method is to return the element-by-element minimum between
;   `LEFT` and `RIGHT`.
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
;       RESULT:             Returns the element of `LEFT` if it is less than `RIGHT`,
;                               and vice versa.
;-
function MrVariable::_OverloadLessThan, left, right
	compile_opt idl2
	on_error, 2

	;Is SELF on the left or right?
	;   - Are both LEFT and RIGHT GDA data objects?
	side = self -> _LeftOrRight(left, right, ISMrVariable=IsMrVariable)

;-------------------------------------------------------
; Two MrVariable Objects ///////////////////////////////
;-------------------------------------------------------
	;Both LEFT and RIGHT are MrVariable objects
	if IsMrVariable then begin
		;Add
		result = (*self.data) < right['DATA']
		
		;Create a new name
		name = self.name + ' < ' + right.name

;-------------------------------------------------------
; MrVariable with Expression ///////////////////////////
;-------------------------------------------------------
	endif else begin
		;Add the expressions
		;   - Assume the user knows what they are doing.
		;   - All IDL truncation effects apply (shortest in determines size out).
		if side eq 'LEFT' $
			then result = (*self.data) < right $
			else result = left < (*self.data)
		
		;Determine name
		;   - Scalar or TYPE[dims]
		if side eq 'LEFT' then begin
			if n_elements(right) eq 1 $
				then rname = strtrim(right[0], 2) $
				else rname = size(right, /TNAME) + '[' + strjoin(strtrim(size(right, /DIMENSIONS), 2), ',') + ']'
			name = self.name + ' < ' + rname
		endif else begin
			if n_elements(left) eq 1 $
				then lname = strtrim(left[0], 2) $
				else lname = size(left, /TNAME) + '[' + strjoin(strtrim(size(left, /DIMENSIONS), 2), ',') + ']'
			name = lname + ' < ' + self.name
		endelse
	endelse

;-------------------------------------------------------
; Output Object ////////////////////////////////////////
;-------------------------------------------------------
	;Create a new object based on the results
	return, self -> New(result, /NO_COPY, NAME=name)
end


;+
;   The purpose of this method is to check if `LEFT` is less than or equal to `RIGHT`.
;   See IDL's online help for `Relational Operators <http://exelisvis.com/docs/Relational_Operators.html>`
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
;       RESULT:             True (1) if `LEFT` less than or equal to `RIGHT`.
;                               Otherwise false (0).
;-
function MrVariable::_OverloadLE, left, right
	compile_opt idl2
	on_error, 2

	;Is SELF on the left or right?
	;   - Are both LEFT and RIGHT GDA data objects?
	side = self -> _LeftOrRight(left, right, ISMrVariable=IsMrVariable)

;-------------------------------------------------------
; Two MrVariable Objects ///////////////////////////////
;-------------------------------------------------------
	;Both LEFT and RIGHT are MrVariable objects
	if IsMrVariable then begin
		;Add
		result = (*self.data) le right['DATA']
		
		;Create a new name
		name = self.name + ' LE ' + right.name

;-------------------------------------------------------
; MrVariable with Expression ///////////////////////////
;-------------------------------------------------------
	endif else begin
		;Add the expressions
		;   - Assume the user knows what they are doing.
		;   - All IDL truncation effects apply (shortest in determines size out).
		if side eq 'LEFT' $
			then result = (*self.data) le right $
			else result = left le (*self.data)
		
		;Determine name
		;   - Scalar or TYPE[dims]
		if side eq 'LEFT' then begin
			if n_elements(right) eq 1 $
				then rname = strtrim(right[0], 2) $
				else rname = size(right, /TNAME) + '[' + strjoin(strtrim(size(right, /DIMENSIONS), 2), ',') + ']'
			name = self.name + ' LE ' + rname
		endif else begin
			if n_elements(left) eq 1 $
				then lname = strtrim(left[0], 2) $
				else lname = size(left, /TNAME) + '[' + strjoin(strtrim(size(left, /DIMENSIONS), 2), ',') + ']'
			name = lname + ' LE ' + self.name
		endelse
	endelse

;-------------------------------------------------------
; Output Object ////////////////////////////////////////
;-------------------------------------------------------
	;Create a new object based on the results
	return, self -> New(result, /NO_COPY, NAME=name)
end


;+
;   The purpose of this method is to check if `LEFT` is less than `RIGHT`. See IDL's
;   online help for `Relational Operators <http://exelisvis.com/docs/Relational_Operators.html>`
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
;       RESULT:             True (1) if `LEFT` less than to `RIGHT`. Otherwise false (0).
;-
function MrVariable::_OverloadLT, left, right
	compile_opt idl2
	on_error, 2

	;Is SELF on the left or right?
	;   - Are both LEFT and RIGHT GDA data objects?
	side = self -> _LeftOrRight(left, right, ISMrVariable=IsMrVariable)

;-------------------------------------------------------
; Two MrVariable Objects ///////////////////////////////
;-------------------------------------------------------
	;Both LEFT and RIGHT are MrVariable objects
	if IsMrVariable then begin
		;Add
		result = (*self.data) lt right['DATA']
		
		;Create a new name
		name = self.name + ' LT ' + right.name

;-------------------------------------------------------
; MrVariable with Expression ///////////////////////////
;-------------------------------------------------------
	endif else begin
		;Add the expressions
		;   - Assume the user knows what they are doing.
		;   - All IDL truncation effects apply (shortest in determines size out).
		if side eq 'LEFT' $
			then result = (*self.data) lt right $
			else result = left lt (*self.data)
		
		;Determine name
		;   - Scalar or TYPE[dims]
		if side eq 'LEFT' then begin
			if n_elements(right) eq 1 $
				then rname = strtrim(right[0], 2) $
				else rname = size(right, /TNAME) + '[' + strjoin(strtrim(size(right, /DIMENSIONS), 2), ',') + ']'
			name = self.name + ' LT ' + rname
		endif else begin
			if n_elements(left) eq 1 $
				then lname = strtrim(left[0], 2) $
				else lname = size(left, /TNAME) + '[' + strjoin(strtrim(size(left, /DIMENSIONS), 2), ',') + ']'
			name = lname + ' LT ' + self.name
		endelse
	endelse

;-------------------------------------------------------
; Output Object ////////////////////////////////////////
;-------------------------------------------------------
	;Create a new object based on the results
	return, self -> New(result, /NO_COPY, NAME=name)
end


;+
;   The purpose of this method is to subract two expressions.
;
;   NOTE:
;     If one of LEFT or RIGHT is an expression, normal IDL operations
;     will take effect. This means the output will be the size of the
;     array with the smallest dimensions.
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
;       RESULT:             The result of subtracting `RIGHT` from `LEFT`.
;-
function MrVariable::_OverloadMinus, left, right
	compile_opt idl2
	on_error, 2

	;Is SELF on the left or right?
	;   - Are both LEFT and RIGHT GDA data objects?
	side = self -> _LeftOrRight(left, right, ISMrVariable=IsMrVariable)

;-------------------------------------------------------
; Two MrVariable Objects //////////////////////////////////
;-------------------------------------------------------
	;Both LEFT and RIGHT are MrVariable objects
	if IsMrVariable then begin
		;Subtract
		result = (*self.data) - right['DATA']
		
		;Create a new name
		name = self.name + '-' + right.name

;-------------------------------------------------------
; MrVariable with Expression //////////////////////////////
;-------------------------------------------------------
	endif else begin
		;Subtract the expressions
		;   - All IDL truncation effects apply (shortest in determines size out).
		if side eq 'LEFT' $
			then result = (*self.data) - right $
			else result = left - (*self.data)
		
		;Determine name
		;   - Scalar or TYPE[dims]
		if side eq 'LEFT' then begin
			if n_elements(right) eq 1 $
				then rname = strtrim(right[0], 2) $
				else rname = size(right, /TNAME) + '[' + strjoin(strtrim(size(right, /DIMENSIONS), 2), ',') + ']'
			name = self.name + '-' + rname
		endif else begin
			if n_elements(left) eq 1 $
				then lname = strtrim(left[0], 2) $
				else lname = size(left, /TNAME) + '[' + strjoin(strtrim(size(left, /DIMENSIONS), 2), ',') + ']'
			name = lname + '-' + self.name
		endelse
	endelse

;-------------------------------------------------------
; Output Object ////////////////////////////////////////
;-------------------------------------------------------
	;Create a new object based on the results
	return, self -> New(result, /NO_COPY, NAME=name)
end


;+
;   The purpose of this method is to negate the implicit array.
;
; :Returns:
;       RESULT:             Negation of the implicit array.
;-
function MrVariable::_OverloadMinusUnary
	compile_opt idl2
	on_error, 2

	;Negate the array, making positive values negative, and vice versa
	return, self -> New(-(*self.data), NAME='-'+self.name)
end


;+
;   The purpose of this method is to take the MOD of `LEFT` and `RIGHT`.
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
;       RESULT:             The remainder of `LEFT` divided by `RIGHT` (i.e. the MOD).
;-
function MrVariable::_OverloadMOD, left, right
	compile_opt idl2
	on_error, 2

	;Is SELF on the left or right?
	;   - Are both LEFT and RIGHT GDA data objects?
	side = self -> _LeftOrRight(left, right, ISMrVariable=IsMrVariable)

;-------------------------------------------------------
; Two MrVariable Objects ///////////////////////////////
;-------------------------------------------------------
	;Both LEFT and RIGHT are MrVariable objects
	if IsMrVariable then begin
		;Add
		result = (*self.data) mod right['DATA']
		
		;Create a new name
		name = self.name + ' MOD ' + right.name

;-------------------------------------------------------
; MrVariable with Expression ///////////////////////////
;-------------------------------------------------------
	endif else begin
		;Add the expressions
		;   - Assume the user knows what they are doing.
		;   - All IDL truncation effects apply (shortest in determines size out).
		if side eq 'LEFT' $
			then result = (*self.data) mod right $
			else result = left mod (*self.data)
		
		;Determine name
		;   - Scalar or TYPE[dims]
		if side eq 'LEFT' then begin
			if n_elements(right) eq 1 $
				then rname = strtrim(right[0], 2) $
				else rname = size(right, /TNAME) + '[' + strjoin(strtrim(size(right, /DIMENSIONS), 2), ',') + ']'
			name = self.name + ' MOD ' + rname
		endif else begin
			if n_elements(left) eq 1 $
				then lname = strtrim(left[0], 2) $
				else lname = size(left, /TNAME) + '[' + strjoin(strtrim(size(left, /DIMENSIONS), 2), ',') + ']'
			name = lname + ' MOD ' + self.name
		endelse
	endelse

;-------------------------------------------------------
; Output Object ////////////////////////////////////////
;-------------------------------------------------------
	;Create a new object based on the results
	return, self -> New(result, /NO_COPY, NAME=name)
end


;+
;   The purpose of this method is to add two expressions.
;
;   NOTE:
;     If one of LEFT or RIGHT is an expression, normal IDL operations
;     will take effect. This means the output will be the size of the
;     array with the smallest dimensions.
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
;       RESULT:             The result of adding `RIGHT` to `LEFT`.
;-
function MrVariable::_OverloadPlus, left, right
	compile_opt idl2
	on_error, 2

	;Is SELF on the left or right?
	;   - Are both LEFT and RIGHT GDA data objects?
	side = self -> _LeftOrRight(left, right, ISMrVariable=IsMrVariable)

;-------------------------------------------------------
; Two MrVariable Objects //////////////////////////////////
;-------------------------------------------------------
	;Both LEFT and RIGHT are MrVariable objects
	if IsMrVariable then begin
		;Add
		result = (*self.data) + right['DATA']
		
		;Create a new name
		name = self.name + '+' + right.name

;-------------------------------------------------------
; MrVariable with Expression //////////////////////////////
;-------------------------------------------------------
	endif else begin
		;Add the expressions
		;   - Assume the user knows what they are doing.
		;   - All IDL truncation effects apply (shortest in determines size out).
		if side eq 'LEFT' $
			then result = (*self.data) + right $
			else result = left + (*self.data)
		
		;Determine name
		;   - Scalar or TYPE[dims]
		if side eq 'LEFT' then begin
			if n_elements(right) eq 1 $
				then rname = strtrim(right[0], 2) $
				else rname = size(right, /TNAME) + '[' + strjoin(strtrim(size(right, /DIMENSIONS), 2), ',') + ']'
			name = self.name + '+' + rname
		endif else begin
			if n_elements(left) eq 1 $
				then lname = strtrim(left[0], 2) $
				else lname = size(left, /TNAME) + '[' + strjoin(strtrim(size(left, /DIMENSIONS), 2), ',') + ']'
			name = lname + '+' + self.name
		endelse
	endelse

;-------------------------------------------------------
; Output Object ////////////////////////////////////////
;-------------------------------------------------------
	;Create a new object based on the results
	return, self -> New(result, /NO_COPY, NAME=name)
end


;+
;   The purpose of this method is to check for inequality. See IDL's online help for
;   `Relational Operators <http://exelisvis.com/docs/Relational_Operators.html>`
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
;       RESULT:             True (1) if `LEFT` is not equal to `RIGHT`. Otherwise false (0).
;-
function MrVariable::_OverloadNE, left, right
	compile_opt idl2
	on_error, 2

	;Is SELF on the left or right?
	;   - Are both LEFT and RIGHT GDA data objects?
	side = self -> _LeftOrRight(left, right, ISMrVariable=IsMrVariable)

;-------------------------------------------------------
; Two MrVariable Objects ///////////////////////////////
;-------------------------------------------------------
	;Both LEFT and RIGHT are MrVariable objects
	if IsMrVariable then begin
		;Add
		result = (*self.data) ne right['DATA']
		
		;Create a new name
		name = self.name + ' NE ' + right.name

;-------------------------------------------------------
; MrVariable with Expression ///////////////////////////
;-------------------------------------------------------
	endif else begin
		;Add the expressions
		;   - Assume the user knows what they are doing.
		;   - All IDL truncation effects apply (shortest in determines size out).
		if side eq 'LEFT' $
			then result = (*self.data) ne right $
			else result = left ne (*self.data)
		
		;Determine name
		;   - Scalar or TYPE[dims]
		if side eq 'LEFT' then begin
			if n_elements(right) eq 1 $
				then rname = strtrim(right[0], 2) $
				else rname = size(right, /TNAME) + '[' + strjoin(strtrim(size(right, /DIMENSIONS), 2), ',') + ']'
			name = self.name + ' NE ' + rname
		endif else begin
			if n_elements(left) eq 1 $
				then lname = strtrim(left[0], 2) $
				else lname = size(left, /TNAME) + '[' + strjoin(strtrim(size(left, /DIMENSIONS), 2), ',') + ']'
			name = lname + ' NE ' + self.name
		endelse
	endelse

;-------------------------------------------------------
; Output Object ////////////////////////////////////////
;-------------------------------------------------------
	;Create a new object based on the results
	return, self -> New(result, /NO_COPY, NAME=name)
end


;+
;   The purpose of this method is to perform a logical NOT of the implicit array. See
;   the IDL Help page for `Bitwise Operators <http://exelisvis.com/docs/Bitwise_Operators.html>`
;   for more information.
;
; :Returns:
;       RESULT:             The logical NOT of the implicit array.
;-
function MrVariable::_OverloadNOT, left, right
	compile_opt idl2
	on_error, 2

	;Negate the array, making positive values negative, and vice versa
	return, self -> GetNew(not (*self.data), NAME='NOT '+self.name)
end


;+
;   The purpose of this method is to perform an inclusive OR between `LEFT` and `RIGHT`.
;   SEE the IDL Help page for `Bitwise Operators <http://exelisvis.com/docs/Bitwise_Operators.html>`
;   for more information.
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
;       RESULT:             The result of `LEFT` OR `RIGHT`.
;-
function MrVariable::_OverloadOR, left, right
	compile_opt idl2
	on_error, 2

	;Is SELF on the left or right?
	;   - Are both LEFT and RIGHT GDA data objects?
	side = self -> _LeftOrRight(left, right, ISMrVariable=IsMrVariable)

;-------------------------------------------------------
; Two MrVariable Objects ///////////////////////////////
;-------------------------------------------------------
	;Both LEFT and RIGHT are MrVariable objects
	if IsMrVariable then begin
		;Add
		result = (*self.data) or right['DATA']
		
		;Create a new name
		name = self.name + ' OR ' + right.name

;-------------------------------------------------------
; MrVariable with Expression ///////////////////////////
;-------------------------------------------------------
	endif else begin
		;Add the expressions
		;   - Assume the user knows what they are doing.
		;   - All IDL truncation effects apply (shortest in determines size out).
		if side eq 'LEFT' $
			then result = (*self.data) or right $
			else result = left or (*self.data)
		
		;Determine name
		;   - Scalar or TYPE[dims]
		if side eq 'LEFT' then begin
			if n_elements(right) eq 1 $
				then rname = strtrim(right[0], 2) $
				else rname = size(right, /TNAME) + '[' + strjoin(strtrim(size(right, /DIMENSIONS), 2), ',') + ']'
			name = self.name + ' OR ' + rname
		endif else begin
			if n_elements(left) eq 1 $
				then lname = strtrim(left[0], 2) $
				else lname = size(left, /TNAME) + '[' + strjoin(strtrim(size(left, /DIMENSIONS), 2), ',') + ']'
			name = lname + ' OR ' + self.name
		endelse
	endelse

;-------------------------------------------------------
; Output Object ////////////////////////////////////////
;-------------------------------------------------------
	;Create a new object based on the results
	return, self -> New(result, /NO_COPY, NAME=name)
end


;+
;   The purpose of this method is to multiply the columns of `LEFT` by the rows of `RIGHT`.
;   (i.e., an IDL matrix multiplication, not a mathematical matrix multiplication). See
;   IDL's page for `Matrix Operators <http://exelisvis.com/docs/Matrix_Operators.html>`
;   for more information.
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
;       RESULT:             The result of matrix multiplying `LEFT` by `RIGHT` in the IDL
;                               sense.
;-
function MrVariable::_OverloadPound, left, right
	compile_opt idl2
	on_error, 2

	;Is SELF on the left or right?
	;   - Are both LEFT and RIGHT GDA data objects?
	side = self -> _LeftOrRight(left, right, ISMrVariable=IsMrVariable)

;-------------------------------------------------------
; Two MrVariable Objects ///////////////////////////////
;-------------------------------------------------------
	;Both LEFT and RIGHT are MrVariable objects
	if IsMrVariable then begin
		;Add
		result = (*self.data) # right['DATA']
		
		;Create a new name
		name = self.name + ' # ' + right.name

;-------------------------------------------------------
; MrVariable with Expression ///////////////////////////
;-------------------------------------------------------
	endif else begin
		;Add the expressions
		;   - Assume the user knows what they are doing.
		;   - All IDL truncation effects apply (shortest in determines size out).
		if side eq 'LEFT' $
			then result = (*self.data) # right $
			else result = left # (*self.data)
		
		;Determine name
		;   - Scalar or TYPE[dims]
		if side eq 'LEFT' then begin
			if n_elements(right) eq 1 $
				then rname = strtrim(right[0], 2) $
				else rname = size(right, /TNAME) + '[' + strjoin(strtrim(size(right, /DIMENSIONS), 2), ',') + ']'
			name = self.name + ' # ' + rname
		endif else begin
			if n_elements(left) eq 1 $
				then lname = strtrim(left[0], 2) $
				else lname = size(left, /TNAME) + '[' + strjoin(strtrim(size(left, /DIMENSIONS), 2), ',') + ']'
			name = lname + ' # ' + self.name
		endelse
	endelse

;-------------------------------------------------------
; Output Object ////////////////////////////////////////
;-------------------------------------------------------
	;Create a new object based on the results
	return, self -> New(result, /NO_COPY, NAME=name)
end


;+
;   The purpose of this method is to multiply the rows of `LEFT` by the columns of `RIGHT`.
;   (i.e., a mathematical matrix multiplication, not an IDL matrix multiplication). See
;   IDL's page for `Matrix Operators <http://exelisvis.com/docs/Matrix_Operators.html>`
;   for more information.
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
;       RESULT:             The result of matrix multiplying `LEFT` by `RIGHT` in the
;                               mathematical sense.
;-
function MrVariable::_OverloadPoundPound, left, right
	compile_opt idl2
	on_error, 2

	;Is SELF on the left or right?
	;   - Are both LEFT and RIGHT GDA data objects?
	side = self -> _LeftOrRight(left, right, ISMrVariable=IsMrVariable)

;-------------------------------------------------------
; Two MrVariable Objects ///////////////////////////////
;-------------------------------------------------------
	;Both LEFT and RIGHT are MrVariable objects
	if IsMrVariable then begin
		;Add
		result = (*self.data) ## right['DATA']
		
		;Create a new name
		name = self.name + ' ## ' + right.name

;-------------------------------------------------------
; MrVariable with Expression ///////////////////////////
;-------------------------------------------------------
	endif else begin
		;Add the expressions
		;   - Assume the user knows what they are doing.
		;   - All IDL truncation effects apply (shortest in determines size out).
		if side eq 'LEFT' $
			then result = (*self.data) ## right $
			else result = left ## (*self.data)
		
		;Determine name
		;   - Scalar or TYPE[dims]
		if side eq 'LEFT' then begin
			if n_elements(right) eq 1 $
				then rname = strtrim(right[0], 2) $
				else rname = size(right, /TNAME) + '[' + strjoin(strtrim(size(right, /DIMENSIONS), 2), ',') + ']'
			name = self.name + ' ## ' + rname
		endif else begin
			if n_elements(left) eq 1 $
				then lname = strtrim(left[0], 2) $
				else lname = size(left, /TNAME) + '[' + strjoin(strtrim(size(left, /DIMENSIONS), 2), ',') + ']'
			name = lname + ' ## ' + self.name
		endelse
	endelse

;-------------------------------------------------------
; Output Object ////////////////////////////////////////
;-------------------------------------------------------
	;Create a new object based on the results
	return, self -> New(result, /NO_COPY, NAME=name)
end


;+
;   The purpose of this method is to provide information when implied print is used.
;   is called.
;-
function MrVariable::_OverloadPrint
	compile_opt idl2
	on_error, 2

	;Print the array
	return, *self.data
end


;+
;   The purpose of this method is to obtain the results of IDL's Size() function when
;   applied to the applicit array.
;
; :Returns:
;       RESULT:             Results of the Size() function on the implicit array
;-
function MrVariable::_OverloadSize
	compile_opt idl2
	on_error, 2

	;NOTE:
	;   There is a bug that incorrectly reports the number of dimesions of an
	;   undefined variable (returns 1 instead of 0). This will be fixed in
	;   version 8.3.1.
	;
	;   https://groups.google.com/forum/#!topic/comp.lang.idl-pvwave/-f8Cxlp5cxQ
	;

	;Return the dimensions of the array. The Size and N_Elements functions
	;will know what to do with them.
	return, size(*self.data, /DIMENSIONS)
end


;+
;   The purpose of this method is to multiply two expressions together.
;
;   NOTE:
;     If one of LEFT or RIGHT is an expression, normal IDL operations
;     will take effect. This means the output will be the size of the
;     array with the smallest dimensions.
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
function MrVariable::_OverloadSlash, left, right
	compile_opt idl2
	on_error, 2

	;Is SELF on the left or right?
	;   - Are both LEFT and RIGHT data objects?
	side = self -> _LeftOrRight(left, right, ISMrVariable=IsMrVariable)

;-------------------------------------------------------
; Two MrVariable Objects //////////////////////////////////
;-------------------------------------------------------
	;Both LEFT and RIGHT are MrVariable objects
	if IsMrVariable then begin
		;Divide
		result = (*self.data) / right['DATA']
		
		;Create a new name
		name = self.name + '/' + right.name

;-------------------------------------------------------
; MrVariable with Expression //////////////////////////////
;-------------------------------------------------------
	endif else begin
		;Divide the expressions
		;   - Assume the user knows what they are doing.
		;   - All IDL truncation effects apply (shortest in determines size out).
		if side eq 'LEFT' $
			then result = (*self.data) / right $
			else result = left / (*self.data)
		
		;Determine name
		;   - Scalar or TYPE[dims]
		if side eq 'LEFT' then begin
			if n_elements(right) eq 1 $
				then rname = strtrim(right[0], 2) $
				else rname = size(right, /TNAME) + '[' + strjoin(strtrim(size(right, /DIMENSIONS), 2), ',') + ']'
			name = self.name + '/' + rname
		endif else begin
			if n_elements(left) eq 1 $
				then lname = strtrim(left[0], 2) $
				else lname = size(left, /TNAME) + '[' + strjoin(strtrim(size(left, /DIMENSIONS), 2), ',') + ']'
			name = lname + '/' + self.name
		endelse
	endelse

;-------------------------------------------------------
; Output Object ////////////////////////////////////////
;-------------------------------------------------------
	;Create a new object based on the results
	return, self -> New(result, /NO_COPY, NAME=name)
end


;+
;   The purpose of this method is to take the logical NOT (~) of the implicit array. See
;   IDL's online help page for `Logical Operators <http://exelisvis.com/docs/Logical_Operators.html>`
;
; :Returns:
;       RESULT:             The logical not of the implicit array.
;-
function MrVariable::_OverloadTilde, left, right
	compile_opt idl2
	on_error, 2

	;Negate the array, making positive values negative, and vice versa
	return, self -> GetNew(~(*self.data), NAME='~'+self.name)
end


;+
;   The purpose of this method is to perform an eXlusive OR between `LEFT` and `RIGHT`.
;   SEE the IDL Help page for `Bitwise Operators <http://exelisvis.com/docs/Bitwise_Operators.html>`
;   for more information.
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
;       RESULT:             The result of `LEFT` XOR `RIGHT`.
;-
function MrVariable::_OverloadXOR, left, right
	compile_opt idl2
	on_error, 2

	;Is SELF on the left or right?
	;   - Are both LEFT and RIGHT GDA data objects?
	side = self -> _LeftOrRight(left, right, ISMrVariable=IsMrVariable)

;-------------------------------------------------------
; Two MrVariable Objects ///////////////////////////////
;-------------------------------------------------------
	;Both LEFT and RIGHT are MrVariable objects
	if IsMrVariable then begin
		;Add
		result = (*self.data) xor right['DATA']
		
		;Create a new name
		name = self.name + ' XOR ' + right.name

;-------------------------------------------------------
; MrVariable with Expression ///////////////////////////
;-------------------------------------------------------
	endif else begin
		;Add the expressions
		;   - Assume the user knows what they are doing.
		;   - All IDL truncation effects apply (shortest in determines size out).
		if side eq 'LEFT' $
			then result = (*self.data) xor right $
			else result = left xor (*self.data)
		
		;Determine name
		;   - Scalar or TYPE[dims]
		if side eq 'LEFT' then begin
			if n_elements(right) eq 1 $
				then rname = strtrim(right[0], 2) $
				else rname = size(right, /TNAME) + '[' + strjoin(strtrim(size(right, /DIMENSIONS), 2), ',') + ']'
			name = self.name + ' XOR ' + rname
		endif else begin
			if n_elements(left) eq 1 $
				then lname = strtrim(left[0], 2) $
				else lname = size(left, /TNAME) + '[' + strjoin(strtrim(size(left, /DIMENSIONS), 2), ',') + ']'
			name = lname + ' XOR ' + self.name
		endelse
	endelse

;-------------------------------------------------------
; Output Object ////////////////////////////////////////
;-------------------------------------------------------
	;Create a new object based on the results
	return, self -> New(result, /NO_COPY, NAME=name)
end


;+
;   Set attribute values.
;
; :Params:
;       ATTRNAME:       in, required, type=string/hash/struct
;                       The name of the attribute for which the value is to be changed,
;                           or a hash or structure whos keys/tags are the attribute names.
;                           If a hash, keys must be strings. Values cannot be complex
;                           datatypes.
;       ATTRVALUE:      in, optional, type=any
;                       The value of the attribute(s) to be added. ATTRVALUE must be
;
; :Keyword:
;       OVERWRITE:      in, optional, type=boolean, default=0
;                       If set, attributes that already exist will be over-written. The
;                           default is to issue a warning.
;-
pro MrVariable::AddAttr, attrName, attrValue, $
OVERWRITE=overwrite
	compile_opt idl2
	on_error, 2
	
	;Defaults
	tf_overwrite = keyword_set(overwrite)

;-------------------------------------------------------
; Hash /////////////////////////////////////////////////
;-------------------------------------------------------
	if isa(attrName, 'HASH') then begin
		;Step through each key
		foreach val, attrName, key do begin
			;KEY must be a string
			if size(key, /TNAME) ne 'STRING' then begin
				MrPrintF, 'LogWarn', 'Hash key must be a string: ' + strtrim(key, 2) + '.'
				continue
			endif
			
			;KEY must be new
			if self -> HasAttr(key) && ~tf_overwrite then begin
				MrPrintF, 'LogWarn', 'Attribute already exists: "' + key + '".'
				continue
			endif
			
			;Validate datatype
			if self -> AttrValue_IsValid(val) then begin
				self.attributes[key] = val
			endif else begin
				MrPrintF, 'LogWarn', size(val, /TNAME), key, $
				          FORMAT='(%"Invalid attribute datatype (%s) for attribute %s")'
				continue
			endelse
		endforeach

;-------------------------------------------------------
; Structure ////////////////////////////////////////////
;-------------------------------------------------------
	endif else if isa(attrName, 'STRUCT') then begin
		;Attributes must already exist
		nTags  = n_tags(attrName)
		tags   = tag_names(attrName)
		tf_has = self -> HasAttr(tags)
		
		;Loop through each value
		for i = 0, nTags - 1 do begin
			tag = tags[i]
		
			;TAG must not exist
			if tf_has[i] && ~tf_overwrite then begin
				MrPrintF, 'LogWarn', 'Attribute already exists: "' + tag + '".'
				continue
			endif
			
			;Validate datatype
			if self -> AttrValue_IsValid(attrName.(i)) then begin
				self.attributes[tag] = attrName.(i)
			endif else begin
				MrPrintF, 'LogWarn', size(attrName.(i), /TNAME), tag, $
				          FORMAT='(%"Invalid attribute datatype (%s) for attribute %s")'
				continue
			endelse
		endfor

;-------------------------------------------------------
; String ///////////////////////////////////////////////
;-------------------------------------------------------
	endif else if isa(attrName, 'STRING') then begin
		;Check if attributes exist
		tf_has = self -> HasAttr(attrName)

	;-------------------------------------------------------
	; Scalar Name //////////////////////////////////////////
	;-------------------------------------------------------
		if isa(attrName, /SCALAR) then begin
			if tf_has && ~tf_overwrite then begin
				MrPrintF, 'LogWarn', 'Attribute already exists: "' + attrName + '".'
			endif else begin
				;Validate datatype
				if self -> AttrValue_IsValid(attrValue) then begin
					self.attributes[attrName] = attrValue
				endif else begin
					MrPrintF, 'LogWarn', size(attrValue, /TNAME), attrName, $
					          FORMAT='(%"Invalid attribute datatype (%s) for attribute %s")'
				endelse
			endelse

	;-------------------------------------------------------
	; Scalar Value /////////////////////////////////////////
	;-------------------------------------------------------
		endif else if isa(attrValue, /SCALAR) then begin
			;An array of names can be given.
			names = attrName
			
			;
			; TODO: Handle case below when NGOOD=0
			;
		
			;Attribute names must not exist
			if max(tf_has) eq 1 && ~tf_overwrite then begin
				ibad = where(~tf_has, nbad, COMPLEMENT=igood, NCOMPLEMENT=ngood)
				for i = 0, nbad do MrPrintF, 'LogWarn', 'Attribute does not exist: "' + attrName[ibad[i]] + '".'
				names = attrNames[igood]
			endif
			
			;Validate datatype
			if self -> AttrValue_IsValid(attrValue) then begin
				self.attributes[names] = attrValue
			endif else begin
				MrPrintF, 'LogWarn', size(attrValue, /TNAME), attrName[0], $
				          FORMAT='(%"Invalid attribute datatype (%s) for attribute %s")'
			endelse

	;-------------------------------------------------------
	; Array or List of Values //////////////////////////////
	;-------------------------------------------------------
		endif else begin
			;Restriction of Hash
			if n_elements(attrName) ne n_elements(attrValue) $
				then message, 'ATTRNAME and ATTRVALUE must have the same number of elements.'
		
			;Loop over each value
			foreach val, attrValue, idx do begin
				;Attribute must exist
				if ~tf_has[idx] && ~tf_create then begin
					MrPrintF, 'LogWarn', 'Attribute does not exist: "' + attrName[idx] + '".'
					continue
				endif
				
				;Validate datatype
				if self -> AttrValue_IsValid(attrValue[idx]) then begin
					self.attributes[attrName[idx]] = attrValue[idx]
				endif else begin
					MrPrintF, 'LogWarn', size(attrValue[idx], /TNAME), attrName[idx], $
					          FORMAT='(%"Invalid attribute datatype (%s) for attribute %s")'
				endelse
			endforeach
		endelse
		
;-------------------------------------------------------
; Other ////////////////////////////////////////////////
;-------------------------------------------------------
	endif else begin
		;
		; Do not accept lists for ATTRNAME because numeric
		; attribute names are not allowed.
		;
	
		message, 'Invalid datatype (' + size(attrName, /TNAME) + ')for ATTRNAME.'
	endelse
end


;+
;   Determine if an attribute value is a valid datatype.
;
; :Private:
;
; :Params:
;       VALUE:          in, required, type=any
;                       Value for which the datatype is to be validated.
;
; :Keywords:
;       TYPE_NAME:      in, optional, type=boolean, default=0
;                       If set, `VALUE` is taken to be a datatype name.
;       TYPE_CODE:      in, optional, type=boolean, default=0
;                       If set, `VALUE` is taken to be a datatype code.
;-
function MrVariable::AttrValue_IsValid, value, $
TYPE_NAME=type_name, $
TYPE_CODE=type_code
	compile_opt idl2
	on_error, 2
	
	;Check keywords
	tf_type_name = keyword_set(type_name)
	tf_type_code = keyword_set(type_code)
	if tf_type_name + tf_type_code gt 1 then message, 'TYPE_NAME and TYPE_CODE are mutually exclusive.'
	
	;Get the type code
	if tf_type_name then begin
		tcode = self -> TypeName2Code(value)
	endif else if tf_type_code then begin
		tcode = value
	endif else begin
		tcode = size(value, /TYPE)
	endelse
	
	;Check valid values
	case tcode of
		 0: tf_valid = 0B ;UNDEFINED
		 1: tf_valid = 1B ;BYTE
		 2: tf_valid = 1B ;INT
		 3: tf_valid = 1B ;LONG
		 4: tf_valid = 1B ;FLOAT
		 5: tf_valid = 1B ;DOUBLE
		 6: tf_valid = 0B ;COMPLEX
		 7: tf_valid = 1B ;STRING
		 8: tf_valid = 0B ;STRUCT
		 9: tf_valid = 0B ;DCOMPLEX
		10: tf_valid = 0B ;POINTER
		11: tf_valid = 0B ;OBJREF
		12: tf_valid = 1B ;UINT
		13: tf_valid = 1B ;ULONG
		14: tf_valid = 1B ;LONG64
		15: tf_valid = 1B ;ULONG64
		else: message, 'Invalid datatype.'
	endcase
	
	return, tf_valid
end


;+
;   Get the names of all attributes
;
; :Keywords:
;       COUNT:          out, optional, type=integer
;                       Named variable to recieve the number of attribute names.
;-
function MrVariable::AttrNames, $
COUNT=count
	compile_opt idl2
	on_error, 2
	
	;Hash returns a list that we want to convert to a string array
	names = self.attributes -> Keys()
	names = names -> ToArray()
	
	;Return the count as well?
	if arg_present(count) then count = self.attributes -> Count()
	
	return, names
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
; :Examples:
;   Append to the 3rd dimension::
;       myArray  = MrVariable(12, 3, 6, 4, TYPE='FLOAT')
;       myArray -> Append, fltarr(12, 3, 4, 4), 3
;       help, myArray
;            MYARRAY     OBJREF      <ObjHeapVar5(MrVariable)>
;             ARRAY       FLOAT     = Array[12, 3, 10, 4]
;
;  Multiple appends to the third dimension::
;       myArray  = MrVariable(12, 3, 6, 4, TYPE='FLOAT')
;       myArray -> Append, fltarr(12, 3, 4, 4), 3
;       myArray -> Append, fltarr(12, 3, 4, 4), 3
;       myArray -> Append, fltarr(12, 3, 4, 4), 3
;       help, myArray
;           MYARRAY     OBJREF      <ObjHeapVar5(MrVariable)>
;             ARRAY       FLOAT     = Array[12, 3, 18, 4]
;
;   Multiple appends to the beginning of an array::
;       myArray -> MrVariable(lonarr(2, 2))
;       myArray -> Append, 1, /BEFORE
;       myArray -> Append, lonarr(2,2) + 1, 1
;       myArray -> Append, lonarr(2,2) + 2, 1
;       myArray -> Append, lonarr(2,2) + 3, 1
;       help, myArray
;           MYARRAY     OBJREF      <ObjHeapVar17(MrVariable)>
;             ARRAY       LONG      = Array[8, 2]
;       print, myArray
;           1      1     2     2     3     3     0      0
;           1      1     2     2     3     3     0      0
;
; :Params:
;       DATA:           in, required, type=array
;                       Array to be concatenated to the implicit array. If undefined or
;                           !Null, then nothing is appended.
;       DIMENSION:      in, optional, type=integer, default=1
;                       The dimension, starting with 1, along which the two arrays are
;                           to be concatenated. For calls between `START` and `FINISH`,
;                           this parameter is ignored.
;
; :Keywords:
;       BEFORE:         in, optional, type=boolean, default=0
;                       If set, `DATA` will be appended to the beginning of the implicit
;                           array. The default is to append to the end.
;       NO_COPY:        in, optional, type=boolean, default=0
;                       If set, `DATA` will be copied directly into the object and
;                           will be left undefined.
;-
pro MrVariable::Append, data, dimension, $
BEFORE=before, $
NO_COPY=no_copy
	compile_opt idl2
	on_error, 2
	
	;Allow input data to be empty. In this case, do nothing.
	if n_elements(data) eq 0 then return

	;Defaults
	before  = keyword_set(before)
	no_copy = keyword_set(no_copy)
	if n_elements(dimension) eq 0 then dimension = 1

	;Is the object empty?
	if n_elements(*self.data) eq 0 then begin
		self -> SetData, data, NO_COPY=no_copy
	
	;Append
	endif else begin
		if before eq 1 $
			then *self.data = MrConcatenate(data, *self.data, dimension) $
			else *self.data = MrConcatenate(*self.data, data, dimension)
	endelse
end


;+
;   A helper method for cleaning up after the ::Append_Multi method.
;
; :Private:
;-
pro MrVariable::Append_Finalize
	compile_opt idl2, hidden

	catch, the_error
	if the_error ne 0 then begin
		catch, /CANCEL
		self._append_cont    = 0B
		self._append_before  = 0B
		self._append_no_copy = 0B
		void = cgErrorMSG()
		return
	endif

	;Was data added?
	if n_elements(*self.data) eq 0 then return

	;Put the desired dimension first
	if self._append_putFirst then begin
		*self.data = MrPutDimensionFirst(*self.data, self._dimAppend, $
		                                 UNREFORM=unReform, UNTRANSPOSE=unTranspose)
	endif else begin
		unReform    = *self._append_unReform
		unTranspose = *self._append_unTrans
	endelse

	;Trim extra elements
	;   MrPutDimensionFirst may have removed shallow dimensions
	nDims = size(*self.data, /N_DIMENSIONS)
	case nDims of
		1: *self.data = (*self.data)[0:self._append_iLast]
		2: *self.data = (*self.data)[0:self._append_iLast,*]
		3: *self.data = (*self.data)[0:self._append_iLast,*,*]
		4: *self.data = (*self.data)[0:self._append_iLast,*,*,*]
		5: *self.data = (*self.data)[0:self._append_iLast,*,*,*,*]
		6: *self.data = (*self.data)[0:self._append_iLast,*,*,*,*,*]
		7: *self.data = (*self.data)[0:self._append_iLast,*,*,*,*,*,*]
		8: *self.data = (*self.data)[0:self._append_iLast,*,*,*,*,*,*,*]
	endcase

	;If BEFORE was set, we need to shift the elements to the front of the array
	if self._append_before then begin
		;Shift only the first dimension
		dimShift = lonarr(nDims)
		dimShift[0] = self._append_iLast + 1

		;Shift
		self -> Shift, dimShift
	endif

	;UnTranspose the data.
	;   Number of elements changed, so update the UNREFORM array.
	;   Be careful of leading shallow dimensions that are being re-introduced.
	iUnReform = min(where(unReform ne 1, count))
	if count eq 0 $
		then unReform[0]            = self._append_iLast + 1L $
		else unReform[iUnReform[0]] = self._append_iLast + 1L
	*self.data = transpose(reform(*self.data, unReform, /OVERWRITE), unTranspose)

	self._append_cont   = 0B
	self._append_before = 0B
end


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
; :Examples:
;  Multiple appends to the third dimension::
;       myArray  = MrVariable(12, 3, 6, 4, TYPE='FLOAT')
;       myArray -> Append_Multi, fltarr(12, 3, 4, 4), 3
;       myArray -> Append_Multi, fltarr(12, 3, 4, 4)
;       myArray -> Append_Multi, fltarr(12, 3, 4, 4)
;       myArray -> Append_Multi, /FINISH
;       help, myArray
;           MYARRAY     OBJREF      <ObjHeapVar5(MrVariable)>
;             ARRAY       FLOAT     = Array[12, 3, 18, 4]
;
;   Multiple appends to the beginning of an array::
;       myArray -> MrVariable(lonarr(2, 2))
;       myArray -> Append_Multi, lonarr(2,2) + 1, 1
;       myArray -> Append_Multi, lonarr(2,2) + 2
;       myArray -> Append_Multi, lonarr(2,2) + 3
;       myArray -> Append_Multi, /FINISH
;       help, myArray
;           MYARRAY     OBJREF      <ObjHeapVar17(MrVariable)>
;             ARRAY       LONG      = Array[8, 2]
;       print, myArray
;           1      1     2     2     3     3     0      0
;           1      1     2     2     3     3     0      0
;
; :Params:
;       DATA:          in, required, type=intarr
;                       Array to be concatenated to the current array. If `START` is set,
;                           then omit this parameter.
;       DIMENSION:      in, optional, type=integer, default=1
;                       The dimension, starting with 1, along which the two arrays are
;                           to be concatenated. For calls other than the first, this
;                           parameter is ignored.
;
; :Keywords:
;       BEFORE:         in, optional, type=boolean, default=0
;                       If set, `DATA` will be appended to the beginning of the implicit
;                           array. The default is to append to the end.
;       FINISH:         in, optional, type=boolean, default=0
;                       Set this keyword after the last call to ::Append_Multi to have
;                           extra, unfilled elements truncated and to reset the internal
;                           appending properties.
;       NO_COPY:        in, optional, type=boolean, default=0
;                       If set, `DATA` will be copied directly into the object and
;                           will be left undefined.
;       NO_DEFORM:      in, optional, type=boolean, default=0
;                       During the appending process, the dimension being appended is
;                           made the first dimension, so that the array is deformed.
;                           To save time, the array is left in this state until the
;                           `FINISH` keyword is used. Set NO_DEFORM to transpose the array
;                           back to its original state after each append.
;-
pro MrVariable::Append_Multi, data, dimension, $
BEFORE=before, $
FINISH=finish, $
NO_COPY=no_copy, $
NO_DEFORM=no_deform
	compile_opt idl2

	catch, the_error
	if the_error ne 0 then begin
		catch, /CANCEL
		self._append_cont    = 0B
		self._append_before  = 0B
		self._append_no_copy = 0B
		if ptr_valid(pArray) then ptr_free, pArray
		void = cgErrorMSG()
		return
	endif

;-----------------------------------------------------
; Finalize Append \\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\
;-----------------------------------------------------
	if keyword_set(finish) then begin
		self -> Append_Finalize
		return
	endif

;-----------------------------------------------------
; SetUp to Append \\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\
;-----------------------------------------------------

	;Dimensions
	if MrCmpVersion('8.3.1') le 0 $
		then nDims = size(self, /N_DIMENSIONS) $
		else nDims = size(*self.data, /N_DIMENSIONS)
	dims   = size(self, /DIMENSIONS)
	dimsIn = size(data, /DIMENSIONS)

	;First time?
	if self._append_cont eq 0 then begin
		;Defaults
		before    = keyword_set(before)
		no_copy   = keyword_set(no_copy)
		no_deform = keyword_set(no_deform)
		if n_elements(dimension) eq 0 then dimension = 1

		;Set up to start appending
		isFirst               = 1B
		self._append_before   = before
		self._append_cont     = 1B
		self._append_dim      = dimension
		self._append_no_copy  = no_copy
		self._append_putFirst = no_deform
		self._append_iLast    = nDims eq 0 ? -1L : (dims[dimension-1]-1 > 0)
	endif else begin
		isFirst               = 0B
	endelse

	;Dimension to which we will append
	dim = self._append_dim
	if isFirst eq 0 && self._append_putFirst eq 0 then dim = 1B

;-----------------------------------------------------
; Append \\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\
;-----------------------------------------------------
	;Prepare the array being appended
	pArray  = ptr_new(data, NO_COPY=self._append_no_copy)
	*pArray = MrPutDimensionFirst(*pArray, self._append_dim, UNREFORM=unReform, $
	                              UNTRANSPOSE=unTranspose)

	;Ensure we know how to return to the original form
	if isFirst then begin
		*self._append_unReform = unReform
		*self._append_unTrans  = unTranspose
	endif

	;No data yet?
	if nDims eq 0 then begin
		*self.data = *pArray

	;Prep internal array for append
	endif else begin
		;Number of empty elements
		nTot   = dims[dim-1]
		nEmpty = nTot - self._append_iLast + 1
	
		;Extend the current array?
		if nEmpty le dimsIn[self._append_dim-1] then begin
			;Extend to fit or double number of elements, whichever is bigger
			nExtend = (2*dims[dim-1]) > dimsIn[dim-1]
			self -> Extend, nExtend, dim
		endif
	
		;Put the append dimension first
		if isFirst || self._append_putFirst $
			then *self.data = MrPutDimensionFirst(*self.data, self._append_dim)
	endelse

	;Append
	sIndex = self._append_iLast + 1L
	eIndex = sIndex + dimsIn[self._append_dim-1] - 1L
	case nDims of
		0: ;Do nothing
		1: (*self.data)[sIndex:eIndex] = temporary(*pArray)
		2: (*self.data)[sIndex:eIndex,*] = temporary(*pArray)
		3: (*self.data)[sIndex:eIndex,*,*] = temporary(*pArray)
		4: (*self.data)[sIndex:eIndex,*,*,*] = temporary(*pArray)
		5: (*self.data)[sIndex:eIndex,*,*,*,*] = temporary(*pArray)
		6: (*self.data)[sIndex:eIndex,*,*,*,*,*] = temporary(*pArray)
		7: (*self.data)[sIndex:eIndex,*,*,*,*,*,*] = temporary(*pArray)
		8: (*self.data)[sIndex:eIndex,*,*,*,*,*,*,*] = temporary(*pArray)
	endcase
	ptr_free, pArray

	;Update the last element filled and the number of empty elements
	self._append_iLast = eIndex

	;Untranspose the array each time?
	if self._append_putFirst $
		then *self.data = transpose(reform(*self.data, unReform, /OVERWRITE), unTranspose)
end


;+
;   Deterime if the input array is equal to the implicit variable array. This is a
;   wrapper method for IDL's Array_Equal function.
;
; :Params:
;       VALUE:      in, required, type=scalar/array
;                   The variable to be compared.
;
; :Keywords:
;       NO_TYPECONV:    in, optional, type=boolean, default=0
;                       If set, variable types will not be converted. The default is
;                           to convert least precise to most precise before comparing.
;
; :Returns:
;       RESULT:         out, required, type=boolean
;                       Returns true (1) if the variables are equal and false (0) otherwise.
;-
function MrVariable::Array_Equal, value, $
INDEX=index
	compile_opt idl2
	on_error, 2
	
	;Determine array equality
	if size(value, /TNAME) eq 'OBJREF' && obj_isa(value, 'MRVARIABLE') $
		then result = array_equal(*self.data, value['DATA'], NO_TYPECONV=no_typeconv) $
		else result = array_equal(*self.data, value,         NO_TYPECONV=no_typeconv)
	
	return, result
end


;+
;   Convert 1-Dimensional indices to their multi-dimensional counterparts.
;
; :Params:
;       INDEX:          in, required, type=intarr
;                       Array of 1D indices.
;
; :Returns:
;       IMULTI:         Multi-dimensional indices.
;-
function MrVariable::Array_Indices, index
	compile_opt idl2
	on_error, 2

	;Get the dimensions of the implicit array
	self -> GetProperty, DIMENSIONS=dims

	;Convert to multi-dimensional indices
	iMulti = array_indices(dims, index, /DIMENSIONS)

	return, iMulti
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
pro MrVariable::Cache, $
NO_CLOBBER=no_clobber, $
NAME_OUT=name_out
	compile_opt idl2
	on_error, 2

	;Setup caching store
	@mrvar_common
	
	;Create a cache in which to store MrVariables
	if ~obj_valid(MrVarCache) then MrVarCache = MrVariable_Cache()
	
	;Add the array to the end of the container
	MrVarCache -> Add, self, NO_CLOBBER=no_clobber, NAME_OUT=name_out
end


;+
;   The prupose of this method is to erase the data from the array. Alternatively, try
;       myArray.array = !Null
;       myArray[*] = !Null
;-
pro MrVariable::Clear
	compile_opt idl2
	on_error, 2

	*self.data = !Null
end


;+
;   Create a copy of the variable with a new heap identifier. Both data and
;   attributes are copied.
;
; :Params:
;       NAME:           in, optional, type=string, default=<name>+'_copy'
;                       Name of the variable copy.
;
; :Keywords:
;       CACHE:          in, optional, type=boolean, default=0
;                       If set, the copy will be added to the variable cache
;-
function MrVariable::Copy, name, $
CACHE=cache
	compile_opt idl2
	on_error, 2

	;Defaults
	cache = keyword_set(cache)
	if n_elements(name) eq 0 then name = self.name + '_copy'
	
	;Copy the data into a new object
	;   - Use Obj_Class() so subclasses can inherit the method.
	theCopy = obj_new(obj_class(self), *self.data)

	;Copy the variable attributes
	self -> CopyAttrTo, theCopy
	
	;Cache the variable
	if cache then theCopy -> Cache, /NO_CLOBBER
	return, theCopy
end


;+
;   Copy variable attributes to another MrVariable object.
;
; :Params:
;       VAR:            in, required, type=objref
;                       The destination MrVariable object.
;       ATTRNAMES:      in, optional, type=string/strarr, default=all attrs
;                       Names of the attributes to be copied.
;
; :Keywords:
;       OVERWRITE:      in, optional, type=boolean, default=0
;                       If set, then existing attributes with names appearing in
;                           `ATTRNAMES` will have their values overwritten. The
;                           default is to issue a warning and skip the attribute.
;-
pro MrVariable::CopyAttrTo, var, attrNames, $
OVERWRITE=overwrite
	compile_opt idl2
	on_error, 2
	
	;VAR must be a MrVariable
	if ~obj_isa(var, 'MRVARIABLE') then message, 'VAR must be a MrVariable object.'
	
	;Default to copying all attributes
	if n_elements(attrName) eq 0 $
		then attrNames = self -> GetAttrNames(COUNT=nAttrs) $
		else nAttrs    = n_elements(attrNames)

	;Copy the variable attributes
	for i = 0, nAttrs-1 do var -> AddAttr, attrNames[i], self[attrNames[i]], OVERWRITE=overwrite
end


;+
;   Extend a dimension of the implicit array by appending the desired number of
;   elements.
;
; :Params:
;       NELEMENTS:      in, optional, type=intarr, default=2xCurrent number of elements
;                       Number of elements to extend the `DIMENSION` of the implicit
;                           array.
;       DIMENSION:      in, optional, type=integer, default=1
;                       The dimension, starting with 1, to be extended.
;
; :Keywords:
;       BEFORE:         in, optional, type=boolean, default=0
;                       If set, `NELEMENTS` will be appended to the beginning of the
;                           implicit array. The default is to append to the end.
;       VALUE:          in, optional, type=any
;                       The value that the extended elements will take on.
;-
pro MrVariable::Extend, nElements, dimension, $
BEFORE=before, $
VALUE=value
	compile_opt idl2
	on_error, 2

	;Defaults
	before = keyword_set(before)
	if n_elements(nElements) eq 0 then nElements = n_elements(self)
	if n_elements(dimension) eq 0 then dimension = 1

	;Create the 
	self -> GetProperty, DIMENSIONS=ExtendDims, TYPE=type
	ExtendDims[dimension-1] = nElements
	ExtendArray = Make_Array(DIMENSION=ExtendDims, TYPE=type, VALUE=value)

	;Concatenate
	if before eq 1 $
		then *self.data = MrConcatenate(ExtendArray, *self.data, dimension) $
		else *self.data = MrConcatenate(*self.data, ExtendArray, dimension)
end


;+
;   Get attribute names.
;
; :Keywords:
;       COUNT:          in, optional, type=integer
;                       Number of attribute names returned.
;
; :Returns:
;       ATTRNAMES:      Names of all variable attributes.
;-
function MrVariable::GetAttrNames, $
COUNT=count
	compile_opt idl2
	on_error, 2

	;Return null for non-existant attributes
	attrList  = self.attributes -> Keys()
	attrNames = attrList -> ToArray(/NO_COPY)
	count     = n_elements(attrNames)

	return, attrNames
end


;+
;   Get an attribute value.
;
; :Params:
;       ATTRNAME:       in, required, type=string
;                       Name of the attribute for which the value is retreived.
;
; :Keywords:
;       NULL:           in, optional, type=boolean
;                       If set, and `ATTRNAME` is not a valid attribute name,
;                           then silently return !Null. The default is to throw
;                           an error.
;
; :Returns:
;       ATTRVALUE:      The attribute value.
;-
function MrVariable::GetAttrValue, attrName, $
NULL=null
	compile_opt idl2
	on_error, 2

	;Check if the attribute exists
	tf_exists = self.attributes -> HasKey(attrName)
	
	;If it exists
	if tf_exists then begin
		;Return the value
		value = self.attributes[attrName]
		
	;Otherwise
	endif else begin
		if keyword_set(null) $
			then value = !Null $
			else message, 'Attribute does not exist: "' + attrName + '".'
	endelse

	return, value
end


;+
;   Get class properties.
;
; :Examples:
;   Get the array::
;       myArray  = MrVariable(11, 91, 6)
;       theArray = myArray -> GetArray()
;       help, theArray
;           THEARRAY        FLOAT     = Array[11, 91, 6]
;
; :Params:
;       DATA:              out, optional, type=any
;                           Array to be stored internally.
;
; :Keywords:
;       DESTROY:            in, optional, type=boolean, default=0
;                           If set, the object will be destroyed after returning the array.
;       NO_COPY:            in, optional, type=boolean, default=0
;                           If set, the array will be removed frome the object and return.
;                               This prevents two copies of the array from being in
;                               memory.
;-
function MrVariable::GetData, $
DESTROY=destroy, $
NO_COPY=no_copy
	compile_opt idl2
	on_error, 2

	;Destroy the data and the object?
	if keyword_set(destroy) then begin
		;Get the data
		;   - The pointer will be cleaned upon destroy
		data = temporary(*self.data)
		
		;Destroy the object
		obj_destroy, self
	
	;Free memory
	endif else if keyword_set(no_copy) then begin
		data = temporary(*self.data)
		ptr_free, self.data
		self.data = ptr_new(/ALLOCATE_HEAP)
	
	;Return a copy
	endif else begin
		return, *self.data
	endelse
	
	;Return the array
	return, data
end


;+
;   Return the name of the array.
;
; :Returns:
;       NAME:       Name of the object.
;-
function MrVariable::GetName
	return, self.name
end


;+
;   Get class properties.
;
; :Keywords:
;       DIMENSIONS:         out, optional, type=lonarr
;                           Sizes of each dimension of `ARRAY`.
;       MAXIMUM:            out, optional, type=any
;                           Maximum value of `ARRAY`.
;       MINIMUM:            out, optional, type=any
;                           Minimum value of `ARRAY`.
;       N_DIMENSIONS:       out, optional, type=int
;                           Number of dimensions of `ARRAY`.
;       N_ELEMENTS:         out, optional, type=long
;                           Number of elements in `ARRAY`.
;       TNAME:              out, optional, type=string
;                           Type-name of `ARRAY`.
;       TYPE:               out, optional, type=int
;                           Type-code of `ARRAY`.
;-
pro MrVariable::GetProperty, $
DATA=data, $
DIMENSIONS=dimensions, $
N_DIMENSIONS=n_dimensions, $
N_ELEMENTS=n_elements, $
MINIMUM=minimum, $
MAXIMUM=maximum, $
NAME=name, $
TNAME=tname, $
TYPE=type
	compile_opt idl2
	on_error, 2

	if arg_present(dimensions)   then dimensions   = size(*self.data, /DIMENSIONS)
	if arg_present(n_dimensions) then n_dimensions = size(*self.data, /N_DIMENSIONS)
	if arg_present(n_elements)   then n_elements   = size(*self.data, /N_ELEMENTS)
	if arg_present(name)         then name         = self.name
	if arg_present(tname)        then tname        = size(*self.data, /TNAME)
	if arg_present(type)         then type         = size(*self.data, /TYPE)
	if arg_present(maximum)      then maximum      = max(*self.data)
	if arg_present(minimum)      then minimum      = min(*self.data)
end


;+
;   Determine if the variable has an attribute.
;
; :Params:
;       ATTRNAME:       in, required, type=string,hash,struct
;                       The name of the attribute to be check
;-
function MrVariable::HasAttr, attrName
	compile_opt idl2
	on_error, 2

	;Number of names given
	nNames = n_elements(attrName)
	
	;Scalar name
	if nNames eq 1 then begin
		tf_has = self.attributes -> HasKey(attrName)
	
	;Loop over each name
	;   - Hash::HasKey accepts only scalars
	endif else begin
		tf_has = bytarr(nNames)
		foreach key, attrName, idx do tf_has[idx] = self.attributes -> HasKey(key)
	endelse
	
	return, tf_has
end


;+
;   Print information about the variable.
;-
pro MrVariable::Help
	compile_opt idl2
	on_error, 2

	;Get general help
	help, self, OUTPUT=help_txt
	parts = stregex(help_txt, '[A-Z]+[ ]+(.*)', /SUBEXP, /EXTRACT)
	help_txt = parts[1]
	
	;Get the attribute names
	attrNames = self -> GetAttrNames(COUNT=count)
	if count eq 0 then begin
		print, help_txt
		print, '  Variable does not contain attributes.'
		return
	endif
	
	;Sort attribute names
	attrNames = attrNames[sort(attrNames)]
	
	;Allocate memory
	attr_txt = strarr(count)
	name_fmt = 'a-' + string(max(strlen(attrNames)), FORMAT='(i0)')

	;Step through each variable
	for i = 0, count-1 do begin
		;Get the value
		value = self -> GetAttrValue(attrNames[i])

		;Turn the value into a string
		;   - Show maximum of 10 values
		;   - Added benefit of creating row vector for strjoin
		;   - Add ellipsis if there are more than 10 values
		nValues = n_elements(value)
		if nValues eq 1 then begin
			str_value = strtrim(value, 2)
		endif else begin
			nPts = (nValues-1) < 10
			str_value  = '[' + strjoin(strtrim(value[0:nPts], 2), ',')
			str_value += (nPts lt nValues-1) ? ',...]' : ']'
		endelse
		
		;Store the value
		attr_txt[i] = string(attrNames[i], str_value, FORMAT='(2x, ' + name_fmt + ', 4x, a0)')
	endfor

	;Print info
	print, help_txt
	print, transpose(attr_txt)
end


;+
;   Perform interpolation on regularly or irregularly vectors.
;
;   Calling Sequence:
;       varOut = MrVar1 -> Interpol(MrVar2)
;       varOut = MrVar1 -> Interpol(X, Xout)
;
; :Params:
;       X:              in, required, type=numeric array
;                       Current abcissa values or a MrVariable object.
;       Xout:           in, required, type=Numeric array
;                       The new abcissa values.
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
;                       A MrVariable object containing the interpolated data.
;-
function MrVariable::Interpol, X, Xout, $
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
		;Get the current abscissa values
		xx = self['DEPEND_0'] -> GetData()
		
		;Get the new abscissa values
		dep0_var = MrVar_Get(x['DEPEND_0'])
		Xout     = dep0_var -> GetData()
		
		;Interpolate
		Y = Interpol( *self.data, temporary(xx), temporary(Xout), $
		              LSQUADRATIC = lsquadratic, $
		              NAN         = nan, $
		              QUADRATIC   = quadratic, $
		              SPLINE      = spline )
		
		;Create output variable
		;   - Copy this variable
		;   - Set its data to the interpolated values
		;   - Set its DEPEND_0 attribute to the new values
		varOut = self -> Copy(NAME=name, CACHE=cache)
		varOut -> SetData, Y, /NO_COPY
		varOut -> SetAttrValue, 'DEPEND_0', dep0_var.name

;-------------------------------------------------------
; Normal Arrays ////////////////////////////////////////
;-------------------------------------------------------
	endif else begin
		;Interpolate
		Y = Interpol( *self.data, X, Xout, $
		              LSQUADRATIC = lsquadratic, $
		              NAN         = nan, $
		              QUADRATIC   = quadratic, $
		              SPLINE      = spline)
		
		;Create a new variable with the same attributes
		varOut = self -> Copy(name, CACHE=cache)
		varOut -> SetData, Y, /NO_COPY
		
		;Delete the DEPEND_0 attribute, if there is one
		;   - TODO: Also create a DEPEND_0 variable ?????
		if varOut -> HasAttr('DEPEND_0') then varOut -> RemoveAttr, 'DEPEND_0'
	endelse
	
	;Return the new variable
	return, varOut
end


;+
;   Check if a variable is defined, of a specific type/class, etc.
;
; :Params:
;       TYPE_NAME:      in, optional, type=string
;                       IDL data-type name, object class name, or structure name.
;
; :Keywords:
;       ARRAY:          in, optional, type=boolean, default=0
;                       If set, return true if the array is an array, list, or
;                           structure and (optionally) matches `TYPENAME`.
;       NULL:           in, optional, type=boolean, default=0
;                       If set, return true if the array is equal to !NULL and
;                           (optionally) matches `TYPENAME`.
;       NUMBER:         in, optional, type=boolean, default=0
;                       If set, return true if the array a number and (optionally)
;                           matches `TYPENAME`.
;       SCALAR:         in, optional, type=boolean, default=0
;                       If set, return true if the array is a scalar and (optionally)
;                           matches `TYPENAME`.
;-
function MrVariable::IsA, type_name, $
ARRAY=array, $
NULL=null, $
NUMBER=number, $
SCALAR=scalar
	compile_opt idl2
	on_error, 2

	;Return the results.
	if n_elements(type_name) gt 0 $
		then return, IsA(*self.data, ARRAY=array, NULL=null, NUMBER=number, SCALAR=scalar) $
		else return, IsA(*self.data, type_name, ARRAY=array, NULL=null, NUMBER=number, SCALAR=scalar)
end


;+
;   Make an array and store it internally.
;
; :Params:
;       D1:                 in, optional, type=any/integer
;                           Size of the second dimension.
;       D2:                 in, optional, type=integer
;                           Size of the second dimension.
;       D3:                 in, optional, type=integer
;                           Size of the third dimension.
;       D4:                 in, optional, type=integer
;                           Size of the fourth dimension.
;       D5:                 in, optional, type=integer
;                           Size of the fifth dimension.
;       D6:                 in, optional, type=integer
;                           Size of the sixth dimension.
;       D7:                 in, optional, type=integer
;                           Size of the seventh dimension.
;       D8:                 in, optional, type=integer
;                           Size of the eight dimension.
;
; :Keywords:
;       TYPE:               in, optional, type=int/float, default='FLOAT'
;                           The type-name or type-code of the array to be created.
;       _REF_EXTRA:         in, optional, type=any
;                           Any keyword accepted by IDL's Make_Array procedure.
;-
pro MrVariable::Make_Array, D1, D2, D3, D4, D5, D6, D7, D8, $
TYPE=type, $
_REF_EXTRA=extra
	compile_opt idl2

	;Error handling
	catch, the_error
	if the_error ne 0 then begin
		catch, /cancel
		void = cgErrorMsg()
		return
	endif

	;Defaults
	if n_elements(type) eq 0 then type='FLOAT'

	;Get a type-code if a name was given.
	tcode = size(type, /TNAME) ne 'STRING' ? type : self -> TypeName2Code(type) 

	;Make the array
	case 1 of
		n_elements(D8) gt 0: *self.data = make_array(D1, D2, D3, D4, D5, D6, D7, D8, $
		                                              TYPE=tcode, _STRICT_EXTRA=extra)
		n_elements(D7) gt 0: *self.data = make_array(D1, D2, D3, D4, D5, D6, D7, $
		                                              TYPE=tcode, _STRICT_EXTRA=extra)
		n_elements(D6) gt 0: *self.data = make_array(D1, D2, D3, D4, D5, D6, $
		                                              TYPE=tcode, _STRICT_EXTRA=extra)
		n_elements(D5) gt 0: *self.data = make_array(D1, D2, D3, D4, D5, $
		                                              TYPE=tcode, _STRICT_EXTRA=extra)
		n_elements(D4) gt 0: *self.data = make_array(D1, D2, D3, D4, $
		                                              TYPE=tcode, _STRICT_EXTRA=extra)
		n_elements(D3) gt 0: *self.data = make_array(D1, D2, D3, $
		                                              TYPE=tcode, _STRICT_EXTRA=extra)
		n_elements(D2) gt 0: *self.data = make_array(D1, D2, $
		                                              TYPE=tcode, _STRICT_EXTRA=extra)
		n_elements(D1) gt 0: *self.data = make_array(D1, $
		                                              TYPE=tcode, _STRICT_EXTRA=extra)
		else: message, 'No dimensions specified for result.' 
	endcase
end


;+
;   Find the maximum value of the implicit array.
;
; :Params:
;       SUBSCRIPT_MAX:      in, optional, type=integer
;                           Index of the maximum value.
;
; :Keywords:
;       ABSOLUTE:           in, optional, type=boolean, default=0
;                           Find the minimum of the absolute value of the implicit array.
;       DIMENSION:          in, optional, type=integer
;                           Find the minimum along a particular dimension (begins with 1).
;       MIN:                out, optional, type=any
;                           Name of a variable to recieve the minimum array element.
;       NAN:                in, optional, type=boolean, default=0
;                           If set, NaN's will be excluded from the result.
;       SUBSCRIPT_MIN:      out, optional, type=integer
;                           Name of a variable to recieve the index of the `MIN`.
;-
function MrVariable::Max, subscript_max, $
ABSOLUTE=absolute, $
DIMENSION=dimension, $
MIN=minimum, $
NAN=nan, $
SUBSCRIPT_MIN=iMin
	compile_opt idl2

	;Error handling
	catch, the_error
	if the_error ne 0 then begin
		catch, /cancel
		void = cgErrorMsg()
		return, !Null
	endif

	;Find only the minimum
	if arg_present(minimum) || arg_present(iMin) then begin
		maximum = max(*self.data, subscript_max, ABSOLUTE=absolute, NAN=nan, DIMENSION=dimension, $
		                                          MIN=minimum, SUSBSCRIPT_MIN=iMin)

	;Find the maximum?
	endif else begin
		maximum = max(*self.data, subscript_max, ABSOLUTE=absolute, NAN=nan, DIMENSION=dimension)
	endelse

	return, maximum
end


;+
;   Find the minimum value of the implicit array.
;
; :Params:
;       SUBSCRIPT_MIN:      in, optional, type=integer
;                           Index of the minimum value.
;
; :Keywords:
;       ABSOLUTE:           in, optional, type=boolean, default=0
;                           Find the minimum of the absolute value of the implicit array.
;       DIMENSION:          in, optional, type=integer
;                           Find the minimum along a particular dimension (begins with 1).
;       MAX:                out, optional, type=any
;                           Name of a variable to recieve the maximum array element.
;       NAN:                in, optional, type=boolean, default=0
;                           If set, NaN's will be excluded from the result.
;       SUBSCRIPT_MAX:      out, optional, type=integer
;                           Index of the maximum value.
;-
function MrVariable::Min, iMin, $
ABSOLUTE=absolute, $
DIMENSION=dimension, $
MAX=maximum, $
NAN=nan, $
SUBSCRIPT_MAX=iMax
	compile_opt idl2

	;Error handling
	catch, the_error
	if the_error ne 0 then begin
		catch, /cancel
		void = cgErrorMsg()
		return, !Null
	endif

	;Find the minimum.
	if arg_present(maximum) || arg_present(iMax) then begin
		minimum = min(*self.data, subscript_min, ABSOLUTE=absolute, NAN=nan, DIMENSION=dimension, $
		                                          MAX=maximum, SUBSCRIPT_MAX=iMax)
	endif else begin
		minimum = min(*self.data, subscript_min, ABSOLUTE=absolute, NAN=nan, DIMENSION=dimension)
	endelse

	return, minimum
end


;+
;   Create a new copy. Properties of the parent object are passed on to the
;   child object.
;-
function MrVariable::New, array, $
NAME=name, $
NO_COPY=no_copy
	compile_opt idl2
	on_error, 2

	;Return a copy of the array
	;   - Use Obj_Class() so that subclasses can inherit the method.
	if n_elements(array) gt 0 $
		then return, obj_new(obj_class(self), array, NAME=name, NO_COPY=no_copy) $
		else return, obj_new(obj_class(self), *self.data, NAME=self.name)
end 


;+
;   Make an array and store it internally.
;
; :Params:
;       SEED:               in, out, required
;                           The seed to use. If undefined, the generic state is used
;                               and returned.
;       D1:                 in, optional, type=any/integer
;                           Size of the first dimension. If an array, then its elements
;                               specified the size of each dimension. Other keywords
;                               are ignored.
;       D2:                 in, optional, type=integer
;                           Size of the second dimension.
;       D3:                 in, optional, type=integer
;                           Size of the third dimension.
;       D4:                 in, optional, type=integer
;                           Size of the fourth dimension.
;       D5:                 in, optional, type=integer
;                           Size of the fifth dimension.
;       D6:                 in, optional, type=integer
;                           Size of the sixth dimension.
;       D7:                 in, optional, type=integer
;                           Size of the seventh dimension.
;       D8:                 in, optional, type=integer
;                           Size of the eight dimension.
;
; :Keywords:
;       _REF_EXTRA:         in, optional, type=any
;                           Any keyword accepted by IDL's RandomU function.
;-
pro MrVariable::RandomN, seed, D1, D2, D3, D4, D5, D6, D7, D8, $
_REF_EXTRA=extra
	compile_opt idl2
	on_error, 2

	;Make the array
	case 1 of
		n_elements(D1) gt 1: *self.data = randomn(seed, D1, _STRICT_EXTRA=extra)
		n_elements(D8) gt 0: *self.data = randomn(seed, D1, D2, D3, D4, D5, D6, D7, D8, _STRICT_EXTRA=extra)
		n_elements(D7) gt 0: *self.data = randomn(seed, D1, D2, D3, D4, D5, D6, D7, _STRICT_EXTRA=extra)
		n_elements(D6) gt 0: *self.data = randomn(seed, D1, D2, D3, D4, D5, D6, _STRICT_EXTRA=extra)
		n_elements(D5) gt 0: *self.data = randomn(seed, D1, D2, D3, D4, D5, _STRICT_EXTRA=extra)
		n_elements(D4) gt 0: *self.data = randomn(seed, D1, D2, D3, D4, _STRICT_EXTRA=extra)
		n_elements(D3) gt 0: *self.data = randomn(seed, D1, D2, D3, _STRICT_EXTRA=extra)
		n_elements(D2) gt 0: *self.data = randomn(seed, D1, D2, _STRICT_EXTRA=extra)
		n_elements(D1) gt 0: *self.data = randomn(seed, D1, _STRICT_EXTRA=extra)
		arg_present(seed):   *self.data = [randomn(seed)]
		else: message, 'No dimensions specified for result.' 
	endcase
end


;+
;   Make an array and store it internally.
;
; :Params:
;       SEED:               in, out, required
;                           The seed to use. If undefined, the generic state is used
;                               and returned.
;       D1:                 in, optional, type=any/integer
;                           Size of the first dimension. If an array, then its elements
;                               specified the size of each dimension. Other keywords
;                               are ignored.
;       D2:                 in, optional, type=integer
;                           Size of the second dimension.
;       D3:                 in, optional, type=integer
;                           Size of the third dimension.
;       D4:                 in, optional, type=integer
;                           Size of the fourth dimension.
;       D5:                 in, optional, type=integer
;                           Size of the fifth dimension.
;       D6:                 in, optional, type=integer
;                           Size of the sixth dimension.
;       D7:                 in, optional, type=integer
;                           Size of the seventh dimension.
;       D8:                 in, optional, type=integer
;                           Size of the eight dimension.
;
; :Keywords:
;       _REF_EXTRA:         in, optional, type=any
;                           Any keyword accepted by IDL's RandomU function.
;-
pro MrVariable::RandomU, seed, D1, D2, D3, D4, D5, D6, D7, D8, $
_REF_EXTRA=extra
	compile_opt idl2

	;Error handling
	catch, the_error
	if the_error ne 0 then begin
		catch, /cancel
		void = cgErrorMsg()
		return
	endif

	;Make the array
	case 1 of
		n_elements(D1) gt 1: *self.data = randomu(seed, D1, _STRICT_EXTRA=extra)
		n_elements(D8) gt 0: *self.data = randomu(seed, D1, D2, D3, D4, D5, D6, D7, D8, _STRICT_EXTRA=extra)
		n_elements(D7) gt 0: *self.data = randomu(seed, D1, D2, D3, D4, D5, D6, D7, _STRICT_EXTRA=extra)
		n_elements(D6) gt 0: *self.data = randomu(seed, D1, D2, D3, D4, D5, D6, _STRICT_EXTRA=extra)
		n_elements(D5) gt 0: *self.data = randomu(seed, D1, D2, D3, D4, D5, _STRICT_EXTRA=extra)
		n_elements(D4) gt 0: *self.data = randomu(seed, D1, D2, D3, D4, _STRICT_EXTRA=extra)
		n_elements(D3) gt 0: *self.data = randomu(seed, D1, D2, D3, _STRICT_EXTRA=extra)
		n_elements(D2) gt 0: *self.data = randomu(seed, D1, D2, _STRICT_EXTRA=extra)
		n_elements(D1) gt 0: *self.data = randomu(seed, D1, _STRICT_EXTRA=extra)
		arg_present(seed):   *self.data = [randomu(seed)]
		else: message, 'No dimensions specified for result.' 
	endcase
end


;+
;   Resize the array to the given dimension sizes. Dimensions must be an integer
;   multiple or factor of the original dimensions.
;
; :Params:
;       D1:         in, required, type=integer/intarr
;                   A scalar or array of up to eight elements denoting the size of
;                       the new array.
;       D2-8:       in, optional, type=integer
;                   The size of dimensions 2-8 of the new array.
;
; :Returns:
;       OVAR:       out, required, type=objref (MrVariable)
;                   A MrVariable with the reformed data name "*_rebin".
;-
function MrVariable::Rebin, d1, d2, d3, d4, d5, d6, d7, d8, $
SAMPLE=sample
	compile_opt idl2
	on_error, 2
	
	;Rebin the data
	case 1 of
		n_elements(d8) gt 0: result = rebin(*self.data, d1, d2, d3, d4, d5, d6, d7, d8, SAMPLE=sample)
		n_elements(d7) gt 0: result = rebin(*self.data, d1, d2, d3, d4, d5, d6, d7, SAMPLE=sample)
		n_elements(d6) gt 0: result = rebin(*self.data, d1, d2, d3, d4, d5, d6, SAMPLE=sample)
		n_elements(d5) gt 0: result = rebin(*self.data, d1, d2, d3, d4, d5, SAMPLE=sample)
		n_elements(d4) gt 0: result = rebin(*self.data, d1, d2, d3, d4, SAMPLE=sample)
		n_elements(d3) gt 0: result = rebin(*self.data, d1, d2, d3, SAMPLE=sample)
		n_elements(d2) gt 0: result = rebin(*self.data, d1, d2, SAMPLE=sample)
		n_elements(d1) gt 0: result = rebin(*self.data, d1, SAMPLE=sample)
		else:                message, 'Usage: oVar -> Rebin, d1[, d2, ..., d8]'
	endcase
	
	;Create result
	oVar = MrVariable(result, NAME=self.name + '_rebin', /NO_COPY)
	self -> CopyAttrTo, oVar
	
	return, oVar
end


;+
;   Resize the array to the given dimension sizes. Dimensions must be an integer
;   multiple or factor of the original dimensions.
;
; :Params:
;       D1:         in, required, type=integer/intarr
;                   A scalar or array of up to eight elements denoting the size of
;                       the new array.
;       D2-8:       in, optional, type=integer
;                   The size of dimensions 2-8 of the new array.
;
; :Returns:
;       OVAR:       out, required, type=objref (MrVariable)
;                   A MrVariable with the reformed data name "*_reform".
;-
function MrVariable::Reform, d1, d2, d3, d4, d5, d6, d7, d8
	compile_opt idl2
	on_error, 2
	
	;Reform the data
	case 1 of
		n_elements(d8) gt 0: result = reform(*self.data, d1, d2, d3, d4, d5, d6, d7, d8)
		n_elements(d7) gt 0: result = reform(*self.data, d1, d2, d3, d4, d5, d6, d7)
		n_elements(d6) gt 0: result = reform(*self.data, d1, d2, d3, d4, d5, d6)
		n_elements(d5) gt 0: result = reform(*self.data, d1, d2, d3, d4, d5)
		n_elements(d4) gt 0: result = reform(*self.data, d1, d2, d3, d4)
		n_elements(d3) gt 0: result = reform(*self.data, d1, d2, d3)
		n_elements(d2) gt 0: result = reform(*self.data, d1, d2)
		n_elements(d1) gt 0: result = reform(*self.data, d1)
		else:                result = reform(*self.data)
	endcase
	
	;Create result
	oVar = MrVariable(result, NAME=self.name + '_reform', /NO_COPY)
	self -> CopyAttrTo, oVar
	
	return, oVar
end


;+
;   Remove attributes.
;
; :Params:
;       ATTRNAMES:      in, required, type=string/hash/struct
;                       Attribute names to be removed. A warning is issued for names
;                           that do not already exist.
;-
pro MrVariable::RemoveAttr, attrNames
	compile_opt idl2
	on_error, 2
	
	;Which attributes exist
	tf_has = self -> HasAttr(attrNames)
	
	;Separate existent from non-existent
	iBad = where(tf_has eq 0, nBad, COMPLEMENT=iGood, NCOMPLEMENT=nGood)
	
	;Issue warning for non-existent
	if nBad gt 0 then begin
		for i = 0, nBad - 1 do MrPrintF, 'LogWarn', 'No such attribute: "' + attrNames[iBad[i]] + '".'
	endif
	
	;Remove the attributes
	if nGood gt 0 then self.attributes -> Remove, attrNames[iGood]
end


;+
;   Set attribute values.
;
;   Calling Sequence:
;       var -> SetAttrValue, struct
;       var -> SetAttrValue, hash
;       var -> SetAttrValue, string, value
;       var -> SetAttrValue, strarr, array
;       var -> SetAttrValue, strarr, list
;
; :Params:
;       ATTRNAME:       in, required, type=string/strarr/hash/struct
;                       The name of the attribute for which the value is to be changed.
;                           Or a hash or structure whos keys/tags are the attribute names.
;                           If an array of strings, then each element is an attribute name
;                           and `ATTRVALUE` may be an array or list of values with the same
;                           number of elements as ATTRNAME. If ATTRNAME is a hash, keys
;                           must be strings. Values cannot be complex datatypes.
;       ATTRVALUE:      in, optional, type=any accepted by ::AttrValue_IsValid
;                       The value of the attribute(s) to be added. ATTRVALUE must be
;
; :Keyword:
;       CREATE:         in, optional, type=boolean, default=0
;                       If set, then attributes will be created if they do not exist.
;-
pro MrVariable::SetAttrValue, attrName, attrValue, $
CREATE=create
	compile_opt idl2
	on_error, 2
	
;-------------------------------------------------------
; Hash /////////////////////////////////////////////////
;-------------------------------------------------------
	if isa(attrName, 'HASH') then begin
		
		;Step through each key
		foreach val, attrName, key do begin
			;Skip invalid attributes, but issue warning
			catch, the_error
			if the_error eq 0 $
				then self -> SetAttributeValue, key, val, CREATE=create $
				else MrPrintF, 'LogWarn', !error_state.msg
		endforeach

;-------------------------------------------------------
; Structure ////////////////////////////////////////////
;-------------------------------------------------------
	endif else if isa(attrName, 'STRUCT') then begin
		
		;Number of attributes and their names
		nTags = n_tags(attrName)
		tags  = tag_names(attrName)
		
		;Loop through each value
		for i = 0, nTags - 1 do begin
			
			;Skip invalid attributes, but issue warning
			catch, the_error
			if the_error eq 0 $
				then self -> SetAttributeValue, tags[i], attrName.(i), CREATE=create $
				else MrPrintF, 'LogWarn', !error_state.msg
		endfor

;-------------------------------------------------------
; String ///////////////////////////////////////////////
;-------------------------------------------------------
	endif else if isa(attrName, 'STRING', /SCALAR) then begin
		
		;Set the attribute
		self -> SetAttributeValue, attrName, attrValue, CREATE=create

;-------------------------------------------------------
; String Array with Array or List of Values ////////////
;-------------------------------------------------------
	endif else if isa(attrName, 'STRING', /ARRAY) then begin
			
		;Restriction of Hash
		if n_elements(attrName) ne n_elements(attrValue) $
			then message, 'ATTRNAME and ATTRVALUE must have the same number of elements.'
		
		;Loop over each value
		foreach val, attrValue, idx do begin
		
			;Skip invalid attributes, but issue warning
			catch, the_error
			if the_error eq 0 $
				then self -> SetAttributeValue, attrName[i], val, CREATE=create $
				else MrPrintF, 'LogWarn', !error_state.msg
		endforeach
		
;-------------------------------------------------------
; Other ////////////////////////////////////////////////
;-------------------------------------------------------
	endif else begin
		;
		; Do not accept lists for ATTRNAME because numeric
		; attribute names are not allowed.
		;
	
		message, 'Invalid value for ATTRNAME.'
	endelse
end


;+
;   Set the value of a single attribute.
;
; :Params:
;       ATTRNAME:       in, required, type=string
;                       The name of the attribute for which the value is to be changed.
;       ATTRVALUE:      in, optional, type=any accepted by ::AttrValue_IsValid
;                       The value of the attribute.
;
; :Keyword:
;       CREATE:         in, optional, type=boolean, default=0
;                       If set, then the attribute will be created if it does not
;                           already exist.
;-
pro MrVariable::SetAttributeValue, attrName, attrValue, $
CREATE=create
	compile_opt idl2
	on_error, 2
	
	;Check inputs
	if ~IsA(attrName, 'STRING', /SCALAR) $
		then message, 'ATTRNAME must be a scalar string.'
	
	if self -> HasAttr(attrName) eq 0 && ~keyword_set(create) $
		then message, 'Attribute name does not exist "' + attrName

	if self -> AttrValue_IsValid(attrValue) eq 0 then begin
		message, string(size(attrValue, /TNAME), attrName, $
		                FORMAT='(%"Invalid attribute datatype (%s) for attribute %s")')
	endif
	
	;Set attribute value
	;   - Will simultaneously create the attribute
	self.attributes[attrName] = attrValue
end


;+
;   Set the array.
;
; :Examples:
;   Set the array::
;       myArray  = MrVariable(11, 91, 6, TYPE='FLOAT')
;       myArray -> SetArray, indgen(23)
;       help, myArray
;           MYARRAY     OBJREF      <ObjHeapVar680(MrVariable)>
;             ARRAY       INT       = Array[23]
;
; :Keywords:
;       DATA:               in, required, type=array
;                           Array of values to be stored, or a MrVariable object whose
;                               array is to be copied. 
;
; :Keywords:
;       NO_COPY:            in, optional, type=boolean, default=0
;                           If set `DATA` will be copied directly into the object
;                               and will be left undefined (a MrVariable object will not
;                               be destroyed, but its array will be empty).
;-
pro MrVariable::SetData, data, $
NO_COPY=no_copy
	compile_opt idl2
	on_error, 2
	
	;Defaults
	no_copy = keyword_set(no_copy)

	;Take from a MrVariable object?
	if IsA(data, 'OBJREF') then begin
		if obj_isa(data, 'MrVariable') $
			then *self.data = data -> GetData(NO_COPY=no_copy) $
			else message, 'Only "MrVariable" objects can be given.'
	
	;Turn a scalar into an array.
	endif else if IsA(data, /SCALAR) then begin
		if no_copy $
			then *self.data = [temporary(data)] $
			else *self.data = [data]
		
	;Treat arrays normally.
	endif else begin
		if no_copy $
			then *self.data = temporary(data) $
			else *self.data = data
	endelse
end


;+
;   Set class properties.
;
; :Params:
;       NAME:               in, required, type=string
;                           Name to be given to the variable. If the variable is cached,
;                               then NAME must be unique.
;
; :Keywords:
;       NAME_OUT:           out, optional, type=string
;                           In cases where `NAME` must be unique and `NO_CLOBBER` is
;                               set, then this is the modified, unique name. Otherwise,
;                               NAME_OUT=`NAME`.
;       NO_CLOBBER:         in, optional, type=boolean, default=0
;                           If set and `NAME` matches a variable within the cache, then
;                               append "_#" to `NAME`, where "#" represents the smallest
;                               available unique number.
;-
pro MrVariable::SetName, name, $
NAME_OUT=name_out, $
NO_CLOBBER=clobber
	compile_opt idl2
	on_error, 2
	
;-----------------------------------------------------
; Cached Variables \\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\
;-----------------------------------------------------
	;Are the current and new names already in the cache?
	;   - Must use SELF.NAME, not SELF because the ::Init method calls
	;     ::SetName before the object is fully defined.
	if MrVar_IsCached(self.name) && MrVar_IsCached(name) then begin
		;Get all of the names
		MrVar_Names, allNames
		
		;Do not include the current name
		iKeep = where(allNames ne self.name, nKeep)
		if nKeep gt 0 then allNames = allNames[iKeep]
		
		;Is the new name already taken?
		tf_taken = ~array_equal(stregex(allNames, '^'+name+'$', /BOOLEAN), 0)
		if tf_taken then begin
			;Append '_#' to the name
			if keyword_set(no_clobber) then begin
				parts    = stregex(allNames, '^'+name+'(_[0-9]+)?$', /SUBEXP, /EXTRACT)
				nMax     = max(fix(parts[1,*]))
				name_out = name + '_' + string(nMax+1, FORMAT='(i0)')
			
			;Delete the other variable
			endif else begin
				MrVar_Delete, name
				name_out = name
			endelse
		endif
	
;-----------------------------------------------------
; Not in Cache \\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\
;-----------------------------------------------------
	endif else begin
		name_out = name
	endelse
	
;-----------------------------------------------------
; Set Name \\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\
;-----------------------------------------------------
	self.name = name_out
end


;+
;   Set class properties.
;-
pro MrVariable::SetProperty
	;Nothing to set
end


;+
;   Convert the implicit array to the desired type.
;
; :Keywords:
;       TYPE:               in, optional, type=string/integer, default='INT'
;                           The or type-code type-name to which the implicit array will
;                               be converted.
;-
pro MrVariable::SetType, type
	compile_opt idl2
	on_error, 2

	;Default to an integer
	if n_elements(type) eq 0 then type = 'INT'

	;Type-Name?
	tt = size(type, /TNAME)
	typeCode = tt ne 'STRING' ? type : self -> TypeName2Code(tt)

	;Fix the array type
	*self.data = fix(*self.data, TYPE=typeCode)
end


;+
;   Shift elements of a an array along any dimension. Positive (negative) shifts move
;   elements right (left).
;
; :Examples:
;   Create a vector and shift its elements::
;       myArray = MrVariable(findgen(10))
;       myArray -> Shift, 5
;       print, myArray
;           5.000  6.000  7.000  8.000  9.000  0.000  1.000  2.000  3.000  4.000
;
;  Create a 2D array and shift one dimension to the right and one to the left.
;       myArray = MrVariable(indgen(10,5))
;       myArray -> Shift, 5, -3
;
; :Params:
;       S1:         in, required, type=integer/intarr, default=0
;                   Either a scalar indicating the number of elements in the first
;                       dimension to shift or an array containing the shift parameters
;                       for each dimension.
;       S2:         in, required, type=integer/intarr, default=0
;                   Number of elements in the second dimension to shift.
;       S3:         in, required, type=integer, default=0
;                   Number of elements in the third dimension to shift.
;       S4:         in, required, type=integer, default=0
;                   Number of elements in the fourth dimension to shift.
;       S5:         in, required, type=integer, default=0
;                   Number of elements in the fifth dimension to shift.
;       S6:         in, required, type=integer, default=0
;                   Number of elements in the sixth dimension to shift.
;       S7:         in, required, type=integer, default=0
;                   Number of elements in the seventh dimension to shift.
;       S8:         in, required, type=integer, default=0
;                   Number of elements in the eighth dimension to shift.
;-
pro MrVariable::Shift, s1, s2, s3, s4, s5, s6, s7, s8
	compile_opt idl2

	;Error handling
	catch, the_error
	if the_error ne 0 then begin
		catch, /cancel
		void = cgErrorMsg()
		return
	endif

	;Shift *self.data
	if IsA(s1, /ARRAY) then begin
		*self.data = shift(*self.data, s1)

	;Scalars
	endif else begin
		case 1 of
			n_elements(s8) gt 0: *self.data = shift(*self.data, s1, s2, s3, s4, s5, s6, s7, s8)
			n_elements(s7) gt 0: *self.data = shift(*self.data, s1, s2, s3, s4, s5, s6, s7)
			n_elements(s6) gt 0: *self.data = shift(*self.data, s1, s2, s3, s4, s5, s6)
			n_elements(s5) gt 0: *self.data = shift(*self.data, s1, s2, s3, s4, s5)
			n_elements(s4) gt 0: *self.data = shift(*self.data, s1, s2, s3, s4)
			n_elements(s3) gt 0: *self.data = shift(*self.data, s1, s2, s3)
			n_elements(s2) gt 0: *self.data = shift(*self.data, s1, s2)
			n_elements(s1) gt 0: *self.data = shift(*self.data, s1)
			else: message, 'Incorrect number of parameters.'
		endcase
	endelse
end


;+
;   Sort the implicit array.
;
; :Keywords:
;       INDEX:      out, optional, type=intarr
;                   The indices into the original array, in the order needed to sort.
;-
pro MrVariable::Sort, $
INDEX=index
	compile_opt idl2
	on_error, 2
	
	;Return the indices
	if arg_present(index) then begin
		index      = sort(*self.data)
		*self.data = (*self.data)[index]
	
	;Sort in one step
	endif else begin
		*self.data = (*self.data)[sort(*self.data)]
	endelse
end


;+
;   Convert a list, hash, or structure to an array. All elements/keys/tags must be
;   of the same type. See MrConcatenate.pro for details.
;
; :Examples:
;   Convert a structure to an array::
;       struct = {A: indgen(13, 4,  8), $
;                 B: indgen(13, 4, 12)}
;       myArray = MrVariable()
;       myArray -> ToArray, struct, DIMENSION=3
;       help, myArray
;           MYARRAY     OBJREF      <ObjHeapVar682(MrVariable)>
;             ARRAY       INT       = Array[13, 4, 20]
;
; :Params:
;       DATA:              in, required, type=any
;                           Structure, List, or Hash whose contents are to be converted
;                               to a single array. All elements must be able to be
;                               concatenated along `DIMENSION`.
;
; :Keywords:
;       DIMENSION:          in, optional, type=integer, default=1
;                           Dimension, starting with 1, to be concatenated.
;       NO_COPY:            in, optional, type=boolean, default=0
;                           If set `DATA` will be copied directly into the object
;                               and will be left undefined.
;-
pro MrVariable::ToArray, data, $
DIMENSION=dimension, $
NO_COPY=no_copy
	compile_opt idl2
	on_error, 2

	no_copy = keyword_set(no_copy)
	if n_elements(dimension) eq 0 then dimension = 0

	type = size(data, /TNAME)
	if type ne 'OBJREF' and type ne 'STRUCT' then $
		message, 'Only array, lists, hashes, and structures may be given.'
	
;---------------------------------------------------------------------
; Structure //////////////////////////////////////////////////////////
;---------------------------------------------------------------------
	if type eq 'STRUCT' then begin
		nTags = n_tags(data)
		for i = 1, nTags-1 do begin
			if i eq 1 $
				then cat_array = MrConcatenate(data.(0), data.(i), dimension) $
				else cat_array = MrConcatenate(temporary(cat_array), data.(i), dimension)
		endfor
	
		*self.data = temporary(cat_array)
	  
;---------------------------------------------------------------------
; List ///////////////////////////////////////////////////////////////
;---------------------------------------------------------------------
	endif else if obj_isa(data, 'LIST') then begin
		*self.data = MrConcatenate(data, dimension)
	
;---------------------------------------------------------------------
; Hash ///////////////////////////////////////////////////////////////
;---------------------------------------------------------------------
	endif else if obj_isa(data, 'HASH') then begin
		keys  = data -> Keys()
		nKeys = data -> Count()

		;Step through the keys
		for i = 1, nTags-1 do begin
			if i eq 1 $
				then cat_array = MrConcatenate(data[keys[0]], data[keys[i]], dimension) $
				else cat_array = MrConcatenate(temporary(cat_array), data[keys[i]], dimension)
		endfor
	
		*self.data = temporary(cat_array)

	endif else begin
		message, 'Only Structures, Lists, and Hash objects can be converted to arrays.'
	endelse

	if no_copy then data = !Null
end


;+
;   Transpose the array.
;
; :Keywords:
;       P:          in, optional, type=intarr
;                   Vector specifying how the dimensions are to be permuted. Dimensions
;                       start at zero and cannot be repeated.
;-
function MrVariable::Transpose, P
	compile_opt idl2
	on_error, 2

	;Get the array and transpose it.
	case n_params() of
		0: result = transpose(*self.data)
		1: result = transpose(*self.data, P)
		else: message, 'Usage: myArray -> Transpose([P])'
	endcase
	
	;Create a variable object
	result = MrVariable(result, /NO_COPY, NAME='Transpose(' + self.name + ')')
	self  -> CopyAttrTo, result
	
	;Return
	return, result
end


;+
;   Convert the implicit array to the desired type.
;
; :Keywords:
;       TYPENAME:           in, required, type=string
;                           The type-name to be converted to a type-code
;
; :Returns:
;       TYPECODE:           The type-code belonging to `TYPENAME`
;-
function MrVariable::TypeName2Code, type
	compile_opt idl2
	on_error, 2

	;Type-Name?
	case strupcase(type) of
		'UNDEFINED': typeCode = 0
		'BYTE':      typeCode = 1
		'INT':       typeCode = 2
		'LONG':      typeCode = 3
		'FLOAT':     typeCode = 4
		'DOUBLE':    typeCode = 5
		'COMPLEX':   typeCode = 6
		'STRING':    typeCode = 7
		'STRUCT':    typeCode = 8
		'DCOMPLEX':  typeCode = 9
		'POINTER':   typeCode = 10
		'OBJREF':    typeCode = 11
		'UINT':      typeCode = 12
		'ULONG':     typeCode = 13
		'LONG64':    typeCode = 14
		'ULONG64':   typeCode = 15
		else: message, 'Type name does not exist: "' + type + '".'
	endcase

	return, typeCode
end


;+
;   Indices of the unique elements within the implicit array.
;
; :Keywords:
;       COMPLEMENT:         out, optional, type=intarr
;                           The index values of the non-unique elements of the implicit array.
;       COUNT:              out, optional, type=integer
;                           The number of unique elements in the implicit array.
;       NAN                 in, optional, type=boolean, default=0
;                           If set, all NaN values will be excluded from `IUNIQ`. Instead,
;                               they will be included in `COMPLEMENT` and `NCOMPLEMENT`.
;       NCOMPLEMENT:        out, optional, type=intarr
;                           The number of non-unique values in the implicit array.
;       VALUES:             out, optional, type=any
;                           Unique values of the imipicit array.
;
; :Returns:
;       IUNIQ:              Indices of the unique elements of the implicit array.
;-
function MrVariable::Uniq, $
COMPLEMENT=complement, $
COUNT=count, $
NAN=nan, $
NCOMPLEMENT=nComplement, $
VALUES=values
	compile_opt idl2
	on_error, 2

	;Find uniq indices
	iSort = sort(*self.data)
	iUniq = uniq(*self.data, iSort)

	;Remove NaNs
	if keyword_set(nan) then begin
		iFinite = where(finite((*self.data)[iUniq]) eq 1, nFinite)
		if nFinite gt 0 then begin
			iUniq = iUniq[iFinite]
		endif else begin
			iUniq = !Null
			count = 0
		endelse
	endif

	;Count the number of uniq values
	if arg_present(count) then count = n_elements(unique_inds)

	;Get the index values of the non-unique elements
	if arg_present(complement) or arg_present(ncomplement) then begin
		complement = where(histogram(iUniq, MIN=0, MAX=n_elements(*self.data), BINSIZE=1) eq 0, nComplement, /NULL)
		if nComplement gt 0 then complement = iSort[complement]
	endif

	;Return the uniq values?
	if arg_present(values) then values = (*self.data)[iSort[iUniq]]

	;Return the uniq indices in the correct order.
	return, iSort[iUniq]
end


;+
;   Finds the intervals within a given monotonic vector that brackets a given set of
;   one or more search values. See IDL's `Value_Locate <http://exelisvis.com/docs/VALUE_LOCATE.html>`
;
; :Params:
;       VALUE:              in, required, type=any
;                           Values to be located in the implicit array.
;
; :Keywords:
;       L64:                in, optional, type=boolean, default=0
;                           If set, indices will be returned as type Long64.
;       SORT:               in, optional, type=boolean, default=0
;                           If set, the implicit array will first be sorted into
;                               ascending order, as required by the Value_Locate function.
;
; :Returns:
;       RESULT:             Indices into the implicit array.
;-
function MrVariable::Value_Locate, value, $
L64=l64, $
SORT=sort
	compile_opt idl2
	on_error, 2

	;Sort first?
	if keyword_set(sort) then begin
		iSort = sort(*self.data)
		result = value_locate((*self.data)[iSort], value, L64=l64)
		result = result[iSort]
	endif else begin
		result = value_locate(*self.data, value, L64=l64)
	endelse

	return, result
end


;+
;   Compare the implicit array with a particular value and returns the indices that
;   the make the comparison true.
;
; :Params:
;       VALUE:              in, required, type=any
;                           Value to be the subject of the Where search.
;
; :Keywords:
;       COMPLEMENT:         out, optional, type=intarr
;                           Indices where the comparison failed.
;       COUNT:              out, optional, type=integer
;                           Number of true comparisons.
;       L64:                in, optional, type=boolean, default=0
;                           If set, indices will be returned as type Long64.
;       NCOMPLEMENT:        out, optional, type=integer
;                           Number of false comparisons.
;       EQUAL:              in, optional, type=boolean
;                           If set, use the EQ operator for the comparison. If no other
;                               input keyords are given, this is assumed.
;       GREATER:            in, optional, type=boolean, default=0
;                           If set, use the GT operator for the comparison.
;       GEQ:                in, optional, type=boolean, default=0
;                           If set, use the GE operator for the comparison.
;       NOTEQ:              in, optional, type=boolean, default=0
;                           If set, use the NE operator for the comparison.
;       LESS:               in, optional, type=boolean, default=0
;                           If set, use the LT operator for the comparison.
;       LEQ:                in, optional, type=boolean, default=0
;                           If set, use the LE operator for the comparison.
;       MATCHES:            in, optional, type=boolean
;                           If set, then the elements of the implicit array will be
;                               returned instead of their index locations. Cannot
;                               be used with `MULTID`.
;       MULTID:             in, optional, type=boolean, default=0
;                           If set, `RESULTS` will contain the multi-dimensional array
;                               indices of each match. Normally, 1D array indices are
;                               returned. Cannot be used with `MATCHES`.
;
; :Returns:
;       RESULT:             Indices into the implicit array where the comparison returned
;                               true. If `COUNT`=0, !Null is returned. If `MATCHES` is
;                               set, then the matching elements are returned.
;-
function MrVariable::Where, value, $
COMPLEMENT=complement, $
COUNT=count, $
L64=l64, $
NCOMPLEMENT=n_complement, $
MATCHES=matches, $
MULTID=multiD, $
;Relational Operators
EQUAL=equal, $
GREATER=greater, $
GEQ=GEQ, $
NOTEQ=notEQ, $
LESS=lessThan, $
LEQ=LEQ
	compile_opt idl2
	on_error, 2

	;Defaults
	equal   = keyword_set(equal)
	greater = keyword_set(greater)
	geq     = keyword_set(geq)
	noteq   = keyword_set(notEQ)
	less    = keyword_set(less)
	leq     = keyword_set(leq)

	;Resolve conflicts
	nKeys = equal + less + leq + greater + geq
	if nKeys gt 1 then message, 'Conflicting keywords. Only one comparison keyword is allowed.'
	if nKeys eq 0 then equal = 1

	;Check where
	case 1 of
		equal:   result = where(*self.data eq value, count, COMPLEMENT=complement, NCOMPLEMENT=ncomplement, L64=l64, /NULL)
		greater: result = where(*self.data ge value, count, COMPLEMENT=complement, NCOMPLEMENT=ncomplement, L64=l64, /NULL)
		geq:     result = where(*self.data gt value, count, COMPLEMENT=complement, NCOMPLEMENT=ncomplement, L64=l64, /NULL)
		noteq:   result = where(*self.data ne value, count, COMPLEMENT=complement, NCOMPLEMENT=ncomplement, L64=l64, /NULL)
		less:    result = where(*self.data le value, count, COMPLEMENT=complement, NCOMPLEMENT=ncomplement, L64=l64, /NULL)
		leq:     result = where(*self.data lt value, count, COMPLEMENT=complement, NCOMPLEMENT=ncomplement, L64=l64, /NULL)
	endcase

	;Return the matches as well.
	if arg_present(matches) then begin
		result = (*self.data)[result]
	
	;Return the multi-dimensional array indices
	endif else if keyword_set(multiD) then begin
		dims   = size(*self.data, /DIMENSIONS)
		result = array_indices(dims, result, /DIMENSIONS)
	endif

	return, result
end


;+
;   The class definition statement.
;
; :Params:
;       CLASS:          out, optional, type=structure
;
; :Fields:
;       DATA:       Data to be accessed via bracket overloading.
;-
pro MrVariable__DEFINE
	compile_opt idl2
	
	class = { MrVariable, $
	          inherits IDL_Object, $
	          data:             Ptr_New(), $
	          name:             '', $
	          
	          attributes:       obj_new(), $
	          
	          ;Append
	          _append_before:   0B, $
	          _append_cont:     0B, $
	          _append_putFirst: 0B, $
	          _append_dim:      0B, $
	          _append_iLast:    0L, $
	          _append_no_copy:  0B, $
	          _append_unReform: ptr_new(), $
	          _append_unTrans:  ptr_new() $
	        }
end