; docformat = 'rst'
;
; NAME:
;   MrVar_Overload__Define
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
;       method for the object on the left side first. So, for two MrVar_Overload objects,
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
; :Categories:
;   MrVar_Overload, Graphics
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
;       2016/10/26  -   Written by Matthew Argall
;-
;*****************************************************************************************
;+
;   The initialization method.
;-
function MrVar_Overload::INIT
	compile_opt idl2
	on_error, 2

	;Initialize superclass
	success = self -> IDL_Object::Init()
	if success eq 0 then return, 0
	
	;Check if we have the required fields
	catch, the_error
	if the_error eq 0 then begin
		help, self.data, OUTPUT=void
		help, self.name, OUTPUT=void
		success = 1
	
	;FAIL
	endif else begin
		catch, /CANCEL
		MrPrintF, 'MrVar_Overload requires a DATA and NAME property.'
		success = 0
	endelse
	
	;Success?
	return, success
end


;+
;   Clean up after the object is destroyed
;-
pro MrVar_Overload::CLEANUP
	compile_opt idl2
	on_error, 2
	
	self -> IDL_Object::Cleanup
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
function MrVar_Overload::_LeftOrRight, left, right, $
ISMrVar_Overload=isMrVar_Overload
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

	;Is the other argument a MrVar_Overload object as well?
	if arg_present(isMrVar_Overload) then begin
		case side of
			'LEFT':  isMrVar_Overload = rtype eq 'OBJREF' && obj_isa(right, 'MrVar_Overload')
			'RIGHT': isMrVar_Overload = ltype eq 'OBJREF' && obj_isa(left,  'MrVar_Overload')
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
function MrVar_Overload::_OverloadAnd, left, right
	compile_opt idl2
	on_error, 2

	;Is SELF on the left or right?
	;   - Are both LEFT and RIGHT GDA data objects?
	side = self -> _LeftOrRight(left, right, ISMrVar_Overload=IsMrVar_Overload)

;-------------------------------------------------------
; Two MrVar_Overload Objects ///////////////////////////////
;-------------------------------------------------------
	;Both LEFT and RIGHT are MrVar_Overload objects
	if IsMrVar_Overload then begin
		;Add
		result = (*self.data) and right['ARRAY']
		
		;Create a new name
		name = self.name + ' AND ' + right.name

;-------------------------------------------------------
; MrVar_Overload with Expression ///////////////////////////
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
function MrVar_Overload::_OverloadAsterisk, left, right
	compile_opt idl2
	on_error, 2

	;Is SELF on the left or right?
	;   - Are both LEFT and RIGHT GDA data objects?
	side = self -> _LeftOrRight(left, right, ISMrVar_Overload=IsMrVar_Overload)

;-------------------------------------------------------
; Two MrVar_Overload Objects //////////////////////////////////
;-------------------------------------------------------
	;Both LEFT and RIGHT are MrVar_Overload objects
	if IsMrVar_Overload then begin
		;Multiply
		result = (*self.data) * right['ARRAY']
		
		;Create a new name
		name = self.name + '*' + right.name

;-------------------------------------------------------
; MrVar_Overload with Expression //////////////////////////////
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
function MrVar_Overload::_OverloadCaret, left, right
	compile_opt idl2
	on_error, 2

	;Is SELF on the left or right?
	;   - Are both LEFT and RIGHT GDA data objects?
	side = self -> _LeftOrRight(left, right, ISMrVar_Overload=IsMrVar_Overload)

;-------------------------------------------------------
; Two MrVar_Overload Objects //////////////////////////////////
;-------------------------------------------------------
	;Both LEFT and RIGHT are MrVar_Overload objects
	if IsMrVar_Overload then begin
		;Caret
		result = (*self.data) ^ right['DATA']
		
		;Create a new name
		name = self.name + '^' + right.name

;-------------------------------------------------------
; MrVar_Overload with Expression //////////////////////////////
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
pro MrVar_Overload::_OverloadBLS, objRef, value, isRange, subscript1, subscript2, $
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
;       myArray = MrVar_Overload(4, 2, 5, 3, TYPE='FLOAT')
;       myArray[1:2, 0, 2:4, 1] = RandomU(5, 2,1,3,1)
;
;   Saving a subarray into a 3D array::
;       myArray = MrVar_Overload(fltarr(5,5,5))
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
pro MrVar_Overload::_OverloadBracketsLeftSide, objRef, value, isRange, i1, i2, i3, i4, i5, i6, i7, i8
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
function MrVar_Overload::_OverloadBracketsRightSide, isRange, i1, i2, i3, i4, i5, i6, i7, i8
	compile_opt idl2
	on_error, 2

	;Number of subscripts given
	nSubscripts = n_elements(isRange)

	;String operations.
	if IsA(i1, /SCALAR, 'STRING') then begin
		case strupcase(i1) of
			'DATA':    return, *self.data
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
function MrVar_Overload::_OverloadEQ, left, right
	compile_opt idl2
	on_error, 2

	;Is SELF on the left or right?
	;   - Are both LEFT and RIGHT GDA data objects?
	side = self -> _LeftOrRight(left, right, ISMrVar_Overload=IsMrVar_Overload)

;-------------------------------------------------------
; Two MrVar_Overload Objects ///////////////////////////////
;-------------------------------------------------------
	;Both LEFT and RIGHT are MrVar_Overload objects
	if IsMrVar_Overload then begin
		;Add
		result = (*self.data) eq right['DATA']
		
		;Create a new name
		name = self.name + ' EQ ' + right.name

;-------------------------------------------------------
; MrVar_Overload with Expression ///////////////////////////
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
function MrVar_Overload::_OverloadForeach, value, key
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
function MrVar_Overload::_OverloadGE, left, right
	compile_opt idl2
	on_error, 2

	;Is SELF on the left or right?
	;   - Are both LEFT and RIGHT GDA data objects?
	side = self -> _LeftOrRight(left, right, ISMrVar_Overload=IsMrVar_Overload)

;-------------------------------------------------------
; Two MrVar_Overload Objects ///////////////////////////////
;-------------------------------------------------------
	;Both LEFT and RIGHT are MrVar_Overload objects
	if IsMrVar_Overload then begin
		;Add
		result = (*self.data) ge right['DATA']
		
		;Create a new name
		name = self.name + ' GE ' + right.name

;-------------------------------------------------------
; MrVar_Overload with Expression ///////////////////////////
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
function MrVar_Overload::_OverloadGT, left, right
	compile_opt idl2
	on_error, 2

	;Is SELF on the left or right?
	;   - Are both LEFT and RIGHT GDA data objects?
	side = self -> _LeftOrRight(left, right, ISMrVar_Overload=IsMrVar_Overload)

;-------------------------------------------------------
; Two MrVar_Overload Objects ///////////////////////////////
;-------------------------------------------------------
	;Both LEFT and RIGHT are MrVar_Overload objects
	if IsMrVar_Overload then begin
		;Add
		result = (*self.data) gt right['DATA']
		
		;Create a new name
		name = self.name + ' GT ' + right.name

;-------------------------------------------------------
; MrVar_Overload with Expression ///////////////////////////
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
function MrVar_Overload::_OverloadGreaterThan, left, right
	compile_opt idl2
	on_error, 2

	;Is SELF on the left or right?
	;   - Are both LEFT and RIGHT GDA data objects?
	side = self -> _LeftOrRight(left, right, ISMrVar_Overload=IsMrVar_Overload)

;-------------------------------------------------------
; Two MrVar_Overload Objects ///////////////////////////////
;-------------------------------------------------------
	;Both LEFT and RIGHT are MrVar_Overload objects
	if IsMrVar_Overload then begin
		;Add
		result = (*self.data) > right['DATA']
		
		;Create a new name
		name = self.name + ' > ' + right.name

;-------------------------------------------------------
; MrVar_Overload with Expression ///////////////////////////
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
function MrVar_Overload::_OverloadHelp, varname
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
function MrVar_Overload::_OverloadImpliedPrint
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
function MrVar_Overload::_OverloadIsTrue
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
function MrVar_Overload::_OverloadLessThan, left, right
	compile_opt idl2
	on_error, 2

	;Is SELF on the left or right?
	;   - Are both LEFT and RIGHT GDA data objects?
	side = self -> _LeftOrRight(left, right, ISMrVar_Overload=IsMrVar_Overload)

;-------------------------------------------------------
; Two MrVar_Overload Objects ///////////////////////////////
;-------------------------------------------------------
	;Both LEFT and RIGHT are MrVar_Overload objects
	if IsMrVar_Overload then begin
		;Add
		result = (*self.data) < right['DATA']
		
		;Create a new name
		name = self.name + ' < ' + right.name

;-------------------------------------------------------
; MrVar_Overload with Expression ///////////////////////////
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
function MrVar_Overload::_OverloadLE, left, right
	compile_opt idl2
	on_error, 2

	;Is SELF on the left or right?
	;   - Are both LEFT and RIGHT GDA data objects?
	side = self -> _LeftOrRight(left, right, ISMrVar_Overload=IsMrVar_Overload)

;-------------------------------------------------------
; Two MrVar_Overload Objects ///////////////////////////////
;-------------------------------------------------------
	;Both LEFT and RIGHT are MrVar_Overload objects
	if IsMrVar_Overload then begin
		;Add
		result = (*self.data) le right['DATA']
		
		;Create a new name
		name = self.name + ' LE ' + right.name

;-------------------------------------------------------
; MrVar_Overload with Expression ///////////////////////////
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
function MrVar_Overload::_OverloadLT, left, right
	compile_opt idl2
	on_error, 2

	;Is SELF on the left or right?
	;   - Are both LEFT and RIGHT GDA data objects?
	side = self -> _LeftOrRight(left, right, ISMrVar_Overload=IsMrVar_Overload)

;-------------------------------------------------------
; Two MrVar_Overload Objects ///////////////////////////////
;-------------------------------------------------------
	;Both LEFT and RIGHT are MrVar_Overload objects
	if IsMrVar_Overload then begin
		;Add
		result = (*self.data) lt right['DATA']
		
		;Create a new name
		name = self.name + ' LT ' + right.name

;-------------------------------------------------------
; MrVar_Overload with Expression ///////////////////////////
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
function MrVar_Overload::_OverloadMinus, left, right
	compile_opt idl2
	on_error, 2

	;Is SELF on the left or right?
	;   - Are both LEFT and RIGHT GDA data objects?
	side = self -> _LeftOrRight(left, right, ISMrVar_Overload=IsMrVar_Overload)

;-------------------------------------------------------
; Two MrVar_Overload Objects //////////////////////////////////
;-------------------------------------------------------
	;Both LEFT and RIGHT are MrVar_Overload objects
	if IsMrVar_Overload then begin
		;Subtract
		result = (*self.data) - right['DATA']
		
		;Create a new name
		name = self.name + '-' + right.name

;-------------------------------------------------------
; MrVar_Overload with Expression //////////////////////////////
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
function MrVar_Overload::_OverloadMinusUnary
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
function MrVar_Overload::_OverloadMOD, left, right
	compile_opt idl2
	on_error, 2

	;Is SELF on the left or right?
	;   - Are both LEFT and RIGHT GDA data objects?
	side = self -> _LeftOrRight(left, right, ISMrVar_Overload=IsMrVar_Overload)

;-------------------------------------------------------
; Two MrVar_Overload Objects ///////////////////////////////
;-------------------------------------------------------
	;Both LEFT and RIGHT are MrVar_Overload objects
	if IsMrVar_Overload then begin
		;Add
		result = (*self.data) mod right['DATA']
		
		;Create a new name
		name = self.name + ' MOD ' + right.name

;-------------------------------------------------------
; MrVar_Overload with Expression ///////////////////////////
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
function MrVar_Overload::_OverloadPlus, left, right
	compile_opt idl2
	on_error, 2

	;Is SELF on the left or right?
	;   - Are both LEFT and RIGHT GDA data objects?
	side = self -> _LeftOrRight(left, right, ISMrVar_Overload=IsMrVar_Overload)

;-------------------------------------------------------
; Two MrVar_Overload Objects //////////////////////////////////
;-------------------------------------------------------
	;Both LEFT and RIGHT are MrVar_Overload objects
	if IsMrVar_Overload then begin
		;Add
		result = (*self.data) + right['DATA']
		
		;Create a new name
		name = self.name + '+' + right.name

;-------------------------------------------------------
; MrVar_Overload with Expression //////////////////////////////
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
function MrVar_Overload::_OverloadNE, left, right
	compile_opt idl2
	on_error, 2

	;Is SELF on the left or right?
	;   - Are both LEFT and RIGHT GDA data objects?
	side = self -> _LeftOrRight(left, right, ISMrVar_Overload=IsMrVar_Overload)

;-------------------------------------------------------
; Two MrVar_Overload Objects ///////////////////////////////
;-------------------------------------------------------
	;Both LEFT and RIGHT are MrVar_Overload objects
	if IsMrVar_Overload then begin
		;Add
		result = (*self.data) ne right['DATA']
		
		;Create a new name
		name = self.name + ' NE ' + right.name

;-------------------------------------------------------
; MrVar_Overload with Expression ///////////////////////////
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
function MrVar_Overload::_OverloadNOT, left, right
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
function MrVar_Overload::_OverloadOR, left, right
	compile_opt idl2
	on_error, 2

	;Is SELF on the left or right?
	;   - Are both LEFT and RIGHT GDA data objects?
	side = self -> _LeftOrRight(left, right, ISMrVar_Overload=IsMrVar_Overload)

;-------------------------------------------------------
; Two MrVar_Overload Objects ///////////////////////////////
;-------------------------------------------------------
	;Both LEFT and RIGHT are MrVar_Overload objects
	if IsMrVar_Overload then begin
		;Add
		result = (*self.data) or right['DATA']
		
		;Create a new name
		name = self.name + ' OR ' + right.name

;-------------------------------------------------------
; MrVar_Overload with Expression ///////////////////////////
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
function MrVar_Overload::_OverloadPound, left, right
	compile_opt idl2
	on_error, 2

	;Is SELF on the left or right?
	;   - Are both LEFT and RIGHT GDA data objects?
	side = self -> _LeftOrRight(left, right, ISMrVar_Overload=IsMrVar_Overload)

;-------------------------------------------------------
; Two MrVar_Overload Objects ///////////////////////////////
;-------------------------------------------------------
	;Both LEFT and RIGHT are MrVar_Overload objects
	if IsMrVar_Overload then begin
		;Add
		result = (*self.data) # right['DATA']
		
		;Create a new name
		name = self.name + ' # ' + right.name

;-------------------------------------------------------
; MrVar_Overload with Expression ///////////////////////////
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
function MrVar_Overload::_OverloadPoundPound, left, right
	compile_opt idl2
	on_error, 2

	;Is SELF on the left or right?
	;   - Are both LEFT and RIGHT GDA data objects?
	side = self -> _LeftOrRight(left, right, ISMrVar_Overload=IsMrVar_Overload)

;-------------------------------------------------------
; Two MrVar_Overload Objects ///////////////////////////////
;-------------------------------------------------------
	;Both LEFT and RIGHT are MrVar_Overload objects
	if IsMrVar_Overload then begin
		;Add
		result = (*self.data) ## right['DATA']
		
		;Create a new name
		name = self.name + ' ## ' + right.name

;-------------------------------------------------------
; MrVar_Overload with Expression ///////////////////////////
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
function MrVar_Overload::_OverloadPrint
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
function MrVar_Overload::_OverloadSize
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
function MrVar_Overload::_OverloadSlash, left, right
	compile_opt idl2
	on_error, 2

	;Is SELF on the left or right?
	;   - Are both LEFT and RIGHT data objects?
	side = self -> _LeftOrRight(left, right, ISMrVar_Overload=IsMrVar_Overload)

;-------------------------------------------------------
; Two MrVar_Overload Objects //////////////////////////////////
;-------------------------------------------------------
	;Both LEFT and RIGHT are MrVar_Overload objects
	if IsMrVar_Overload then begin
		;Divide
		result = (*self.data) / right['DATA']
		
		;Create a new name
		name = self.name + '/' + right.name

;-------------------------------------------------------
; MrVar_Overload with Expression //////////////////////////////
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
function MrVar_Overload::_OverloadTilde, left, right
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
function MrVar_Overload::_OverloadXOR, left, right
	compile_opt idl2
	on_error, 2

	;Is SELF on the left or right?
	;   - Are both LEFT and RIGHT GDA data objects?
	side = self -> _LeftOrRight(left, right, ISMrVar_Overload=IsMrVar_Overload)

;-------------------------------------------------------
; Two MrVar_Overload Objects ///////////////////////////////
;-------------------------------------------------------
	;Both LEFT and RIGHT are MrVar_Overload objects
	if IsMrVar_Overload then begin
		;Add
		result = (*self.data) xor right['DATA']
		
		;Create a new name
		name = self.name + ' XOR ' + right.name

;-------------------------------------------------------
; MrVar_Overload with Expression ///////////////////////////
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
;   The class definition statement.
;
; :Params:
;       CLASS:          out, optional, type=structure
;
; :Fields:
;       DATA:       Data to be accessed via bracket overloading.
;-
pro MrVar_Overload__DEFINE
	compile_opt idl2
	
	class = { MrVar_Overload, $
	          inherits IDL_Object $
	        }
end