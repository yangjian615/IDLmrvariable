; docformat = 'rst'
;
; NAME:
;   MrScalarTS__Define
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
;       method for the object on the left side first. So, for two MrScalarTS objects,
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
;       The ::Cache method will cache the MrScalarTS object. When the object is destroyed,
;       it will automatically be removed from the cache.
;
; :Categories:
;   MrScalarTS, Graphics
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
; :Examples:
;   Create a MrScalarTS object using a pre-existing array::
;       theArray = findgen(24, 36)
;       myArray  = MrScalarTS(theArray)
;       help, myArray
;           MYARRAY     OBJREF      <ObjHeapVar666(MrScalarTS)>
;             ARRAY       FLOAT     = Array[24, 36]
;
;   Initialize a MrScalarTS object via Make_Array::
;       myArray = MrScalarTS(24, 36, TYPE='ULong')
;       help, myArray
;           MYARRAY     OBJREF      <ObjHeapVar668(MrScalarTS)>
;             ARRAY       ULONG     = Array[24, 36]
;
;   Initialize a MrScalarTS object via RandomU::
;       myArray = MrScalarTS(24, 36, SEED=3)
;       print, myarray[0:3]
;           0.897916     0.558249     0.766930     0.589101
;
;   Initialize a MrScalarTS with a normal, gaussian distribution::
;       myArray = MrScalarTS(24, 36, SEED=3, /NORMAL)
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
;       MAKE:               in, optional, type=boolean
;                           If set, all parameters will be passed to the Make_Array
;                               function in order to create the array. This is the default
;                               if `DATA` is a scalar.
;       NORMAL:             in, optional, type=boolean, default=0
;                           If set, then `SEED` will be input to the RandomN function
;                               instead of the RandomU function.
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
function MrScalarTS::INIT, data, D2, $
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
	if n_elements(name) eq 0 then name = 'MrScalarTS'

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
pro MrScalarTS::CLEANUP
	compile_opt idl2
	on_error, 2
	
	;Superclass
	self -> MrVariable::Cleanup
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
function MrScalarTS::_OverloadAsterisk, left, right
	compile_opt idl2
	on_error, 2

	;Is SELF on the left or right?
	;   - Are both LEFT and RIGHT GDA data objects?
	side = self -> _LeftOrRight(left, right, ISMRVARIABLE=IsMrVariable)

;-------------------------------------------------------
; Two MrScalarTS Objects //////////////////////////////////
;-------------------------------------------------------
	;Both LEFT and RIGHT are MrScalarTS objects
	if IsMrVariable then begin
		;Name of result
		name = self.name + '*' + right.name
		
		;SELF is LEFT
		if side eq 'LEFT' then begin
			case obj_class(right) of
				'MRVARIABLE': result = *self.data * right['DATA']
				'MRSCALARTS': result = *self.data * right['DATA']
				
				'MRVECTORTS': begin
					type      = size( (*self.data)[0] * right[[0]], /TYPE )
					temp      = make_array( n_elements(*self.data), 3, TYPE=type )
					temp[*,0] = *self.data * right[*,0]
					temp[*,1] = *self.data * right[*,1]
					temp[*,2] = *self.data * right[*,2]
					result    = MrVectorTS(temp, NAME=name, /NO_COPY)
				endcase

				'MRMATRIXTS': begin
					;Allocate memory
					type = size( (*self.data)[0] * right[[0]], /TYPE )
					dims = size(right, /DIMENSIONS)
					temp = make_array( dims, TYPE=type )
					
					;Operate
					for i = 0, dims[1] - 1 do begin
					for j = 0, dims[2] - 1 do begin
						temp[*,i,j] = *self.data * right[*,i,j]
					endfor
					endfor
					
					;Create matrix
					result = MrMatrixTS(temp, NAME=name, /NO_COPY)
				endcase
				else: message, 'Unrecognized object class: "' + obj_class(right) + '".'
			endcase
		
		;SELF is RIGHT
		endif else begin
			case obj_class(right) of
				'MRVARIABLE': result = left['DATA'] * *self.data
				'MRSCALARTS': result = left['DATA'] * *self.data
				
				'MRVECTORTS':  begin
					type      = size( left[[0]] * (*self.data)[0], /TYPE )
					temp      = make_array( n_elements(*self.data), 3, TYPE=type )
					temp[*,0] = left[*,0] * *self.data
					temp[*,1] = left[*,1] * *self.data
					temp[*,2] = left[*,2] * *self.data
					result    = MrVectorTS(temp, NAME=name, /NO_COPY)
				endcase
				
				'MRMATRIXTS': begin
					;Allocate memory
					type = size( left[[0]] * (*self.data)[0], /TYPE )
					dims = size(left, /DIMENSIONS)
					temp = make_array( dims, TYPE=type )
					
					;Operate
					for i = 0, dims[1] - 1 do begin
					for j = 0, dims[2] - 1 do begin
						temp[*,i,j] = left[*,i,j] * *self.data
					endfor
					endfor
					
					;Create matrix
					result = MrMatrixTS(temp, NAME=name, /NO_COPY)
				endcase
				else: message, 'Unrecognized object class: "' + obj_class(left) + '".'
			endcase
		endelse

;-------------------------------------------------------
; MrScalarTS with Expression //////////////////////////////
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
	if size(result, /TNAME) eq 'OBJREF' $
		then return, result $
		else return, MrVariable(result, /NO_COPY, NAME=name)
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
function MrScalarTS::_OverloadCaret, left, right
	compile_opt idl2
	on_error, 2

	;Is SELF on the left or right?
	;   - Are both LEFT and RIGHT GDA data objects?
	side = self -> _LeftOrRight(left, right, ISMRVARIABLE=IsMrVariable)

;-------------------------------------------------------
; Two MrScalarTS Objects //////////////////////////////////
;-------------------------------------------------------
	;Both LEFT and RIGHT are MrScalarTS objects
	if IsMrVariable then begin
		;Name of result
		name = self.name + '^' + right.name
		
		;SELF is LEFT
		if side eq 'LEFT' then begin
			case obj_class(right) of
				'MRVARIABLE': result = *self.data ^ right['DATA']
				'MRSCALARTS': result = *self.data ^ right['DATA']
				
				'MRVECTORTS': begin
					type      = size( (*self.data)[0] ^ right[[0]], /TYPE )
					temp      = make_array( n_elements(*self.data), 3, TYPE=type )
					temp[*,0] = *self.data ^ right[*,0]
					temp[*,1] = *self.data ^ right[*,1]
					temp[*,2] = *self.data ^ right[*,2]
					result    = MrVectorTS(temp, NAME=name, /NO_COPY)
				endcase

				'MRMATRIXTS': begin
					;Allocate memory
					type = size( (*self.data)[0] ^ right[[0]], /TYPE )
					dims = size(right, /DIMENSIONS)
					temp = make_array( dims, TYPE=type )
					
					;Operate
					for i = 0, dims[1] - 1 do begin
					for j = 0, dims[2] - 1 do begin
						temp[*,i,j] = *self.data ^ right[*,i,j]
					endfor
					endfor
					
					;Create matrix
					result = MrMatrixTS(temp, NAME=name, /NO_COPY)
				endcase
				else: message, 'Unrecognized object class: "' + obj_class(right) + '".'
			endcase
		
		;SELF is RIGHT
		endif else begin
			case obj_class(right) of
				'MRVARIABLE': result = left['DATA'] ^ *self.data
				'MRSCALARTS': result = left['DATA'] ^ *self.data
				
				'MRVECTORTS':  begin
					type      = size( left[[0]] ^ (*self.data)[0], /TYPE )
					temp      = make_array( n_elements(*self.data), 3, TYPE=type )
					temp[*,0] = left[*,0] ^ *self.data
					temp[*,1] = left[*,1] ^ *self.data
					temp[*,2] = left[*,2] ^ *self.data
					result    = MrVectorTS(temp, NAME=name, /NO_COPY)
				endcase
				
				'MRMATRIXTS': begin
					;Allocate memory
					type = size( left[[0]] ^ (*self.data)[0], /TYPE )
					dims = size(left, /DIMENSIONS)
					temp = make_array( dims, TYPE=type )
					
					;Operate
					for i = 0, dims[1] - 1 do begin
					for j = 0, dims[2] - 1 do begin
						temp[*,i,j] = left[*,i,j] ^ *self.data
					endfor
					endfor
					
					;Create matrix
					result = MrMatrixTS(temp, NAME=name, /NO_COPY)
				endcase
				else: message, 'Unrecognized object class: "' + obj_class(right) + '".'
			endcase
		endelse

;-------------------------------------------------------
; MrScalarTS with Expression //////////////////////////////
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
	if size(result, /TNAME) eq 'OBJREF' $
		then return, result $
		else return, MrVariable(result, /NO_COPY, NAME=name)
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
function MrScalarTS::_OverloadForeach, value, key
	compile_opt idl2
	on_error, 2

	nPts = n_elements( (*self.data)[*,0,0] )
	if n_elements(key) eq 0 then key = 0

	;Get the array element if the index is in range
	if key lt nPts then begin
		next = 1
		value = (*self.data)[key,*,*]
	
	;Otherwise, stop iterating 
	endif else next = 0

	;Next element to retrieve
	key += 1

	return, next
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
function MrScalarTS::_OverloadMinus, left, right
	compile_opt idl2
	on_error, 2

	;Is SELF on the left or right?
	;   - Are both LEFT and RIGHT GDA data objects?
	side = self -> _LeftOrRight(left, right, ISMRVARIABLE=IsMrVariable)

;-------------------------------------------------------
; Two MrScalarTS Objects //////////////////////////////////
;-------------------------------------------------------
	;Both LEFT and RIGHT are MrScalarTS objects
	if IsMrVariable then begin
		;Name of result
		name = self.name + '-' + right.name
		
		;SELF is LEFT
		if side eq 'LEFT' then begin
			case obj_class(right) of
				'MRVARIABLE': result = *self.data - right['DATA']
				'MRSCALARTS': result = *self.data - right['DATA']
				
				'MRVECTORTS': begin
					type      = size( (*self.data)[0] - right[[0]], /TYPE )
					temp      = make_array( n_elements(*self.data), 3, TYPE=type )
					temp[*,0] = *self.data - right[*,0]
					temp[*,1] = *self.data - right[*,1]
					temp[*,2] = *self.data - right[*,2]
					result    = MrVectorTS(temp, NAME=name, /NO_COPY)
				endcase

				'MRMATRIXTS': begin
					;Allocate memory
					type = size( (*self.data)[0] - right[[0]], /TYPE )
					dims = size(right, /DIMENSIONS)
					temp = make_array( dims, TYPE=type )
					
					;Operate
					for i = 0, dims[1] - 1 do begin
					for j = 0, dims[2] - 1 do begin
						temp[*,i,j] = *self.data - right[*,i,j]
					endfor
					endfor
					
					;Create matrix
					result = MrMatrixTS(temp, NAME=name, /NO_COPY)
				endcase
				else: message, 'Unrecognized object class: "' + obj_class(right) + '".'
			endcase
		
		;SELF is RIGHT
		endif else begin
			case obj_class(right) of
				'MRVARIABLE': result = left['DATA'] - *self.data
				'MRSCALARTS': result = left['DATA'] - *self.data
				
				'MRVECTORTS':  begin
					type      = size( left[[0]] - (*self.data)[0], /TYPE )
					temp      = make_array( n_elements(*self.data), 3, TYPE=type )
					temp[*,0] = left[*,0] - *self.data
					temp[*,1] = left[*,1] - *self.data
					temp[*,2] = left[*,2] - *self.data
					result    = MrVectorTS(temp, NAME=name, /NO_COPY)
				endcase
				
				'MRMATRIXTS': begin
					;Allocate memory
					type = size( left[[0]] - (*self.data)[0], /TYPE )
					dims = size(left, /DIMENSIONS)
					temp = make_array( dims, TYPE=type )
					
					;Operate
					for i = 0, dims[1] - 1 do begin
					for j = 0, dims[2] - 1 do begin
						temp[*,i,j] = left[*,i,j] - *self.data
					endfor
					endfor
					
					;Create matrix
					result = MrMatrixTS(temp, NAME=name, /NO_COPY)
				endcase
				else: message, 'Unrecognized object class: "' + obj_class(right) + '".'
			endcase
		endelse

;-------------------------------------------------------
; MrScalarTS with Expression //////////////////////////////
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
	if size(result, /TNAME) eq 'OBJREF' $
		then return, result $
		else return, MrVariable(result, /NO_COPY, NAME=name)
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
function MrScalarTS::_OverloadMOD, left, right
	compile_opt idl2
	on_error, 2

	;Is SELF on the left or right?
	;   - Are both LEFT and RIGHT GDA data objects?
	side = self -> _LeftOrRight(left, right, ISMRVARIABLE=IsMrVariable)

;-------------------------------------------------------
; Two MrScalarTS Objects ///////////////////////////////
;-------------------------------------------------------
	;Both LEFT and RIGHT are MrScalarTS objects
	if IsMrVariable then begin
		;Name of result
		name = 'Mod(' + self.name + ',' + right.name + ')'
		
		;SELF is LEFT
		if side eq 'LEFT' then begin
			case obj_class(right) of
				'MRVARIABLE': result = *self.data mod right['DATA']
				'MRSCALARTS': result = *self.data mod right['DATA']
				
				'MRVECTORTS': begin
					type      = size( (*self.data)[0] mod right[[0]], /TYPE )
					temp      = make_array( n_elements(*self.data), 3, TYPE=type )
					temp[*,0] = *self.data mod right[*,0]
					temp[*,1] = *self.data mod right[*,1]
					temp[*,2] = *self.data mod right[*,2]
					result    = MrVectorTS(temp, NAME=name, /NO_COPY)
				endcase

				'MRMATRIXTS': begin
					;Allocate memory
					type = size( (*self.data)[0] mod right[[0]], /TYPE )
					dims = size(right, /DIMENSIONS)
					temp = make_array( dims, TYPE=type )
					
					;Operate
					for i = 0, dims[1] - 1 do begin
					for j = 0, dims[2] - 1 do begin
						temp[*,i,j] = *self.data mod right[*,i,j]
					endfor
					endfor
					
					;Create matrix
					result = MrMatrixTS(temp, NAME=name, /NO_COPY)
				endcase
				else: message, 'Unrecognized object class: "' + obj_class(right) + '".'
			endcase
		
		;SELF is RIGHT
		endif else begin
			case obj_class(right) of
				'MRVARIABLE': result = left['DATA'] mod *self.data
				'MRSCALARTS': result = left['DATA'] mod *self.data
				
				'MRVECTORTS':  begin
					type      = size( left[[0]] mod (*self.data)[0], /TYPE )
					temp      = make_array( n_elements(*self.data), 3, TYPE=type )
					temp[*,0] = left[*,0] mod *self.data
					temp[*,1] = left[*,1] mod *self.data
					temp[*,2] = left[*,2] mod *self.data
					result    = MrVectorTS(temp, NAME=name, /NO_COPY)
				endcase
				
				'MRMATRIXTS': begin
					;Allocate memory
					type = size( left[[0]] mod (*self.data)[0], /TYPE )
					dims = size(left, /DIMENSIONS)
					temp = make_array( dims, TYPE=type )
					
					;Operate
					for i = 0, dims[1] - 1 do begin
					for j = 0, dims[2] - 1 do begin
						temp[*,i,j] = left[*,i,j] mod *self.data
					endfor
					endfor
					
					;Create matrix
					result = MrMatrixTS(temp, NAME=name, /NO_COPY)
				endcase
				else: message, 'Unrecognized object class: "' + obj_class(right) + '".'
			endcase
		endelse

;-------------------------------------------------------
; MrScalarTS with Expression ///////////////////////////
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
	if size(result, /TNAME) eq 'OBJREF' $
		then return, result $
		else return, MrVariable(result, /NO_COPY, NAME=name)
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
function MrScalarTS::_OverloadPlus, left, right
	compile_opt idl2
	on_error, 2

	;Is SELF on the left or right?
	;   - Are both LEFT and RIGHT GDA data objects?
	side = self -> _LeftOrRight(left, right, ISMRVARIABLE=IsMrVariable)

;-------------------------------------------------------
; Two MrScalarTS Objects //////////////////////////////////
;-------------------------------------------------------
	;Both LEFT and RIGHT are MrVariableTS objects
	if IsMrVariable then begin
		;Name of result
		name = self.name + '+' + right.name
		
		;SELF is LEFT
		if side eq 'LEFT' then begin
			case obj_class(right) of
				'MRVARIABLE': result = *self.data + right['DATA']
				'MRSCALARTS': result = *self.data + right['DATA']
				
				'MRVECTORTS': begin
					type      = size( (*self.data)[0] + right[[0]], /TYPE )
					temp      = make_array( n_elements(*self.data), 3, TYPE=type )
					temp[*,0] = *self.data + right[*,0]
					temp[*,1] = *self.data + right[*,1]
					temp[*,2] = *self.data + right[*,2]
					result    = MrVectorTS(temp, NAME=name, /NO_COPY)
				endcase

				'MRMATRIXTS': begin
					;Allocate memory
					type = size( (*self.data)[0] + right[[0]], /TYPE )
					dims = size(right, /DIMENSIONS)
					temp = make_array( dims, TYPE=type )
					
					;Operate
					for i = 0, dims[1] - 1 do begin
					for j = 0, dims[2] - 1 do begin
						temp[*,i,j] = *self.data + right[*,i,j]
					endfor
					endfor
					
					;Create matrix
					result = MrMatrixTS(temp, NAME=name, /NO_COPY)
				endcase
				else: message, 'Unrecognized object class: "' + obj_class(right) + '".'
			endcase
		
		;SELF is RIGHT
		endif else begin
			case obj_class(right) of
				'MRVARIABLE': result = left['DATA'] + *self.data
				'MRSCALARTS': result = left['DATA'] + *self.data
				
				'MRVECTORTS':  begin
					type      = size( left[[0]] + (*self.data)[0], /TYPE )
					temp      = make_array( n_elements(*self.data), 3, TYPE=type )
					temp[*,0] = left[*,0] + *self.data
					temp[*,1] = left[*,1] + *self.data
					temp[*,2] = left[*,2] + *self.data
					result    = MrVectorTS(temp, NAME=name, /NO_COPY)
				endcase
				
				'MRMATRIXTS': begin
					;Allocate memory
					type = size( left[[0]] + (*self.data)[0], /TYPE )
					dims = size(left, /DIMENSIONS)
					temp = make_array( dims, TYPE=type )
					
					;Operate
					for i = 0, dims[1] - 1 do begin
					for j = 0, dims[2] - 1 do begin
						temp[*,i,j] = left[*,i,j] + *self.data
					endfor
					endfor
					
					;Create matrix
					result = MrMatrixTS(temp, NAME=name, /NO_COPY)
				endcase
				else: message, 'Unrecognized object class: "' + obj_class(right) + '".'
			endcase
		endelse

;-------------------------------------------------------
; MrScalarTS with Expression //////////////////////////////
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
	if size(result, /TNAME) eq 'OBJREF' $
		then return, result $
		else return, MrVariable(result, /NO_COPY, NAME=name)
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
function MrScalarTS::_OverloadPound, left, right
	compile_opt idl2
	on_error, 2

	;Is SELF on the left or right?
	;   - Are both LEFT and RIGHT GDA data objects?
	side = self -> _LeftOrRight(left, right, ISMRVARIABLE=IsMrVariable)

;-------------------------------------------------------
; Two MrScalarTS Objects ///////////////////////////////
;-------------------------------------------------------
	;Both LEFT and RIGHT are MrScalarTS objects
	if IsMrVariable then begin
		message, 'The # operator cannot act on MrScalarTS and other MrVariable objects.'

;-------------------------------------------------------
; MrScalarTS with Expression ///////////////////////////
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
function MrScalarTS::_OverloadPoundPound, left, right
	compile_opt idl2
	on_error, 2

	;Is SELF on the left or right?
	;   - Are both LEFT and RIGHT GDA data objects?
	side = self -> _LeftOrRight(left, right, ISMRVARIABLE=IsMrVariable)

;-------------------------------------------------------
; Two MrScalarTS Objects ///////////////////////////////
;-------------------------------------------------------
	;Both LEFT and RIGHT are MrScalarTS objects
	if IsMrVariable then begin
		message, 'The ## operator cannot act on MrScalarTS and other MrVariable objects.'

;-------------------------------------------------------
; MrScalarTS with Expression ///////////////////////////
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
function MrScalarTS::_OverloadPrint
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
function MrScalarTS::_OverloadSize
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
function MrScalarTS::_OverloadSlash, left, right
	compile_opt idl2
	on_error, 2

	;Is SELF on the left or right?
	;   - Are both LEFT and RIGHT data objects?
	side = self -> _LeftOrRight(left, right, ISMRVARIABLE=IsMrVariable)

;-------------------------------------------------------
; Two MrScalarTS Objects //////////////////////////////////
;-------------------------------------------------------
	;Both LEFT and RIGHT are MrScalarTS objects
	if IsMrVariable then begin
		;Name of result
		name = self.name + '/' + right.name
		
		;SELF is LEFT
		if side eq 'LEFT' then begin
			case obj_class(right) of
				'MRVARIABLE': result = *self.data / right['DATA']
				'MRSCALARTS': result = *self.data / right['DATA']
				
				'MRVECTORTS': begin
					type      = size( (*self.data)[0] / right[[0]], /TYPE )
					temp      = make_array( n_elements(*self.data), 3, TYPE=type )
					temp[*,0] = *self.data / right[*,0]
					temp[*,1] = *self.data / right[*,1]
					temp[*,2] = *self.data / right[*,2]
					result    = MrVectorTS(temp, NAME=name, /NO_COPY)
				endcase

				'MRMATRIXTS': begin
					;Allocate memory
					type = size( (*self.data)[0] / right[[0]], /TYPE )
					dims = size(right, /DIMENSIONS)
					temp = make_array( dims, TYPE=type )
					
					;Operate
					for i = 0, dims[1] - 1 do begin
					for j = 0, dims[2] - 1 do begin
						temp[*,i,j] = *self.data / right[*,i,j]
					endfor
					endfor
					
					;Create matrix
					result = MrMatrixTS(temp, NAME=name, /NO_COPY)
				endcase
				else: message, 'Unrecognized object class: "' + obj_class(right) + '".'
			endcase
		
		;SELF is RIGHT
		endif else begin
			case obj_class(right) of
				'MRVARIABLE': result = left['DATA'] / *self.data
				'MRSCALARTS': result = left['DATA'] / *self.data
				
				'MRVECTORTS':  begin
					type      = size( left[[0]] / (*self.data)[0], /TYPE )
					temp      = make_array( n_elements(*self.data), 3, TYPE=type )
					temp[*,0] = left[*,0] / *self.data
					temp[*,1] = left[*,1] / *self.data
					temp[*,2] = left[*,2] / *self.data
					result    = MrVectorTS(temp, NAME=name, /NO_COPY)
				endcase
				
				'MRMATRIXTS': begin
					;Allocate memory
					type = size( left[[0]] * (*self.data)[0], /TYPE )
					dims = size(left, /DIMENSIONS)
					temp = make_array( dims, TYPE=type )
					
					;Operate
					for i = 0, dims[1] - 1 do begin
					for j = 0, dims[2] - 1 do begin
						temp[*,i,j] = left[*,i,j] * *self.data
					endfor
					endfor
					
					;Create matrix
					result = MrMatrixTS(temp, NAME=name, /NO_COPY)
				endcase
				else: message, 'Unrecognized object class: "' + obj_class(right) + '".'
			endcase
		endelse

;-------------------------------------------------------
; MrScalarTS with Expression //////////////////////////////
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
	if size(result, /TNAME) eq 'OBJREF' $
		then return, result $
		else return, MrVariable(result, /NO_COPY, NAME=name)
end


;+
;   Concatenate an array, or a series of arrays, to the implicit array.
;
; :Params:
;       DATA:           in, required, type=Nx1 or 1xN array
;                       Array to be concatenated to the implicit array. If undefined or
;                           !Null, then nothing is appended.
;
; :Keywords:
;       BEFORE:         in, optional, type=boolean, default=0
;                       If set, `DATA` will be appended to the beginning of the implicit
;                           array. The default is to append to the end.
;       NO_COPY:        in, optional, type=boolean, default=0
;                       If set, `DATA` will be copied directly into the object and
;                           will be left undefined.
;-
pro MrScalarTS::Append, data, $
BEFORE=before, $
NO_COPY=no_copy
	compile_opt idl2
	on_error, 2
	
	;Allow input data to be empty. In this case, do nothing.
	if n_elements(data) eq 0 then return

	;Defaults
	before  = keyword_set(before)
	no_copy = keyword_set(no_copy)

	;Is the object empty?
	if n_elements(*self.data) eq 0 then begin
		self -> SetData, data, NO_COPY=no_copy
	
	;Append
	;   - Use Reform() to turn 1xN into Nx1.
	;   - Data of any other size will case error.
	endif else begin
		;Append before
		if before then begin
			if no_copy $
				then self -> SetData, [reform(temporary(data)), *self.data] $
				else self -> SetData, [reform(data), *self.data]
		
		;Append after
		endif else begin
			if no_copy $
				then self -> SetData, [*self.data, reform(temporary(data))] $
				else self -> SetData, [*self.data, data]
		endelse
	endelse
end


;+
;   Set the array.
;
; :Keywords:
;       DATA:              in, required, type=NxM array
;                           Array of values to be stored, or a MrScalarTS object whose
;                               array is to be copied. 
;
; :Keywords:
;       NO_COPY:            in, optional, type=boolean, default=0
;                           If set `DATA` will be copied directly into the object
;                               and will be left undefined (a MrScalarTS object will not
;                               be destroyed, but its array will be empty).
;-
pro MrScalarTS::SetData, data, $
NO_COPY=no_copy
	compile_opt idl2
	on_error, 2

	no_copy = keyword_set(no_copy)
	
;-----------------------------------------------------
; MrVariable Object \\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\
;-----------------------------------------------------
	if size(data, /TNAME) eq 'OBJREF' then begin
		;MrScalarTS
		if obj_isa(data, 'MrScalarTS') then begin
			*self.data = data -> GetData()
		
		;MrVariable
		endif else if obj_class(data) eq 'MRVARIABLE' then begin
			;Check sizes
			sz = size(data)
			
			;Scalar or Nx1
			if sz[0] eq 2 && sz[1] eq 1 then begin
				*self.data = reform( data -> GetData() )
			
			;1xN
			endif else if sz[0] le 1 then begin
				*self.data = data -> GetData()
			
			;Other
			endif else begin
				message, 'DATA must be 1xN or Nx1.'
			endelse
		;Other
		endif else begin
			message, 'Only "MrScalarTS" or "MrVariable" objects can be given.'
		endelse

;-----------------------------------------------------
; Normal Array \\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\
;-----------------------------------------------------
	endif else begin
		;Check sizes
		sz = size(data)

		;Scalar or Nx1
		if sz[0] le 1 then begin
			if no_copy $
				then *self.data = temporary(data) $
				else *self.data = data
		;1xN
		endif else if sz[0] eq 2 && sz[1] eq 1 then begin
			if no_copy $
				then *self.data = reform(temporary(data)) $
				else *self.data = reform(data)
		;Other
		endif else begin
			message, 'DATA must be 1xN or Nx1.'
		endelse
	endelse
end


;+
;   The class definition statement.
;
; :Params:
;       CLASS:          out, optional, type=structure
;-
pro MrScalarTS__DEFINE
	compile_opt idl2
	
	class = { MrScalarTS, $
	          inherits MrVariable $
	        }
end