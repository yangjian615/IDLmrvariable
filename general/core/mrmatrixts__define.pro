; docformat = 'rst'
;
; NAME:
;   MrMatrixTS__Define
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
;       method for the object on the left side first. So, for two MrMatrixTS objects,
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
;       The ::Cache method will cache the MrMatrixTS object. When the object is destroyed,
;       it will automatically be removed from the cache.
;
; :Categories:
;   MrMatrixTS, Graphics
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
;   Create a MrMatrixTS object using a pre-existing array::
;       theArray = findgen(24, 36)
;       myArray  = MrMatrixTS(theArray)
;       help, myArray
;           MYARRAY     OBJREF      <ObjHeapVar666(MrMatrixTS)>
;             ARRAY       FLOAT     = Array[24, 36]
;
;   Initialize a MrMatrixTS object via Make_Array::
;       myArray = MrMatrixTS(24, 36, TYPE='ULong')
;       help, myArray
;           MYARRAY     OBJREF      <ObjHeapVar668(MrMatrixTS)>
;             ARRAY       ULONG     = Array[24, 36]
;
;   Initialize a MrMatrixTS object via RandomU::
;       myArray = MrMatrixTS(24, 36, SEED=3)
;       print, myarray[0:3]
;           0.897916     0.558249     0.766930     0.589101
;
;   Initialize a MrMatrixTS with a normal, gaussian distribution::
;       myArray = MrMatrixTS(24, 36, SEED=3, /NORMAL)
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
function MrMatrixTS::INIT, data, D2, $
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
	if n_elements(name) eq 0 then name = 'MrMatrixTS'

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
pro MrMatrixTS::CLEANUP
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
function MrMatrixTS::_OverloadAsterisk, left, right
	compile_opt idl2
	on_error, 2

	;Is SELF on the left or right?
	;   - Are both LEFT and RIGHT GDA data objects?
	side = self -> _LeftOrRight(left, right, ISMRVARIABLE=IsMrVariable)

;-------------------------------------------------------
; Two MrMatrixTS Objects //////////////////////////////////
;-------------------------------------------------------
	;Both LEFT and RIGHT are MrMatrixTS objects
	if IsMrVariable then begin
		;Name of result
		name = self.name + '*' + right.name
		
		;SELF is LEFT
		if side eq 'LEFT' then begin
			case obj_class(right) of
				'MRVARIABLE': result = *self.data * right['DATA']
				'MRSCALARTS': begin
					;Allocate memory
					type   = size( (*self.data)[0] * right[[0]], /TYPE )
					dims   = size(*self.data, /DIMENSIONS)
					result = make_array( dims, TYPE=type )
					
					;Multiply
					for i = 0, dims[1]-1 do begin
					for j = 0, dims[2]-1 do begin
						result[*,i,j] = (*self.data)[*,i,j] * right['DATA']
					endfor
					endfor
				end
				'MRVECTORTS': message, 'MrMatrixTS * MrVectorTS not allowed.'
				'MRMATRIXTS': result = *self.data * right['DATA']
				else: message, 'Unrecognized object class: "' + obj_class(right) + '".'
			endcase
		
		;SELF is RIGHT
		endif else begin
			case obj_class(left) of
				'MRVARIABLE': result = left['DATA'] * *self.data
				'MRSCALARTS': begin
					;Allocate memory
					type   = size( left[[0]] * (*self.data)[0], /TYPE )
					dims   = size(*self.data, /DIMENSIONS)
					result = make_array( dims, TYPE=type )
					
					;Multiply
					for i = 0, dims[1]-1 do begin
					for j = 0, dims[2]-1 do begin
						result[*,i,j] = left['DATA'] * (*self.data)[*,i,j]
					endfor
					endfor
				end
				'MRVECTORTS': message, 'MrVectorTS * MrMatrixTS not allowed.'
				'MRMATRIXTS': result = left['DATA'] * *self.data
				else: message, 'Unrecognized object class: "' + obj_class(left) + '".'
			endcase
		endelse

;-------------------------------------------------------
; MrMatrixTS with Expression ///////////////////////////
;-------------------------------------------------------
	endif else begin
		;Multiply the expressions
		;   - Assume the user knows what they are doing.
		;   - All IDL truncation effects apply (shortest in determines size out).
		if side eq 'LEFT' $
			then result = (*self.data) * right['ARRAY'] $
			else result = left['ARRAY'] * (*self.data)
		
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
function MrMatrixTS::_OverloadCaret, left, right
	compile_opt idl2
	on_error, 2

	;Is SELF on the left or right?
	;   - Are both LEFT and RIGHT GDA data objects?
	side = self -> _LeftOrRight(left, right, ISMRVARIABLE=IsMrVariable)

;-------------------------------------------------------
; Two MrMatrixTS Objects //////////////////////////////////
;-------------------------------------------------------
	;Both LEFT and RIGHT are MrMatrixTS objects
	if IsMrVariable then begin
		;Name of result
		name = self.name + '^' + right.name
		
		;SELF is LEFT
		if side eq 'LEFT' then begin
			case obj_class(right) of
				'MRVARIABLE': result = *self.data ^ right['DATA']
				'MRSCALARTS': begin
					;Allocate memory
					type   = size( (*self.data)[0] ^ right[[0]], /TYPE )
					dims   = size(*self.data, /DIMENSIONS)
					result = make_array( dims, TYPE=type )
					
					;Multiply
					for i = 0, dims[1]-1 do begin
					for j = 0, dims[2]-1 do begin
						result[*,i,j] = (*self.data)[*,i,j] ^ right['DATA']
					endfor
					endfor
				end
				'MRVECTORTS': message, 'MrMatrixTS ^ MrVectorTS not allowed.'
				'MRMATRIXTS': result = *self.data ^ right['DATA']
				else: message, 'Unrecognized object class: "' + obj_class(right) + '".'
			endcase
		
		;SELF is RIGHT
		endif else begin
			case obj_class(left) of
				'MRVARIABLE': result = left['DATA'] ^ *self.data
				'MRSCALARTS': begin
					;Allocate memory
					type   = size( left[[0]] ^ (*self.data)[0], /TYPE )
					dims   = size(*self.data, /DIMENSIONS)
					result = make_array( dims, TYPE=type )
					
					;Multiply
					for i = 0, dims[1]-1 do begin
					for j = 0, dims[2]-1 do begin
						result[*,i,j] = left['DATA'] ^ (*self.data)[*,i,j]
					endfor
					endfor
				end
				'MRVECTORTS': message, 'MrVectorTS ^ MrMatrixTS not allowed.'
				'MRMATRIXTS': result = left['DATA'] ^ *self.data
				else: message, 'Unrecognized object class: "' + obj_class(left) + '".'
			endcase
		endelse

;-------------------------------------------------------
; MrMatrixTS with Expression //////////////////////////////
;-------------------------------------------------------
	endif else begin
		;Multiply the expressions
		;   - Assume the user knows what they are doing.
		;   - All IDL truncation effects apply (shortest in determines size out).
		if side eq 'LEFT' $
			then result = (*self.data) ^ right['ARRAY'] $
			else result = left['ARRAY'] ^ (*self.data)
		
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
function MrMatrixTS::_OverloadForeach, value, key
	compile_opt idl2
	on_error, 2

	nPts = n_elements( (*self.data)[*,0,0] )
	if n_elements(key) eq 0 then key = 0 else key += 1

	;Get the array element if the index is in range
	if key lt nPts then begin
		next = 1
		value = reform((*self.data)[key,*,*])
	
	;Otherwise, stop iterating 
	endif else next = 0

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
function MrMatrixTS::_OverloadMinus, left, right
	compile_opt idl2
	on_error, 2

	;Is SELF on the left or right?
	;   - Are both LEFT and RIGHT GDA data objects?
	side = self -> _LeftOrRight(left, right, ISMRVARIABLE=IsMrVariable)

;-------------------------------------------------------
; Two MrMatrixTS Objects //////////////////////////////////
;-------------------------------------------------------
	;Both LEFT and RIGHT are MrMatrixTS objects
	if IsMrVariable then begin
		;Name of result
		name = self.name + '-' + right.name
		
		;SELF is LEFT
		if side eq 'LEFT' then begin
			case obj_class(right) of
				'MRVARIABLE': result = *self.data - right['DATA']
				'MRSCALARTS': begin
					;Allocate memory
					type   = size( (*self.data)[0] - right[[0]], /TYPE )
					dims   = size(*self.data, /DIMENSIONS)
					result = make_array( dims, TYPE=type )
					
					;Multiply
					for i = 0, dims[1]-1 do begin
					for j = 0, dims[2]-1 do begin
						result[*,i,j] = (*self.data)[*,i,j] - right['DATA']
					endfor
					endfor
				end
				'MRVECTORTS': message, 'MrMatrixTS - MrVectorTS not allowed.'
				'MRMATRIXTS': result = *self.data - right['DATA']
				else: message, 'Unrecognized object class: "' + obj_class(right) + '".'
			endcase
		
		;SELF is RIGHT
		endif else begin
			case obj_class(left) of
				'MRVARIABLE': result = left['DATA'] - *self.data
				'MRSCALARTS': begin
					;Allocate memory
					type   = size( left[[0]] - (*self.data)[0], /TYPE )
					dims   = size(*self.data, /DIMENSIONS)
					result = make_array( dims, TYPE=type )
					
					;Multiply
					for i = 0, dims[1]-1 do begin
					for j = 0, dims[2]-1 do begin
						result[*,i,j] = left['DATA'] - (*self.data)[*,i,j]
					endfor
					endfor
				end
				'MRVECTORTS': message, 'MrVectorTS - MrMatrixTS not allowed.'
				'MRMATRIXTS': result = left['DATA'] - *self.data
				else: message, 'Unrecognized object class: "' + obj_class(left) + '".'
			endcase
		endelse

;-------------------------------------------------------
; MrMatrixTS with Expression //////////////////////////////
;-------------------------------------------------------
	endif else begin
		;Subtract the expressions
		;   - All IDL truncation effects apply (shortest in determines size out).
		if side eq 'LEFT' $
			then result = (*self.data) - right['ARRAY'] $
			else result = left['ARRAY'] - (*self.data)
		
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
function MrMatrixTS::_OverloadMOD, left, right
	compile_opt idl2
	on_error, 2

	;Is SELF on the left or right?
	;   - Are both LEFT and RIGHT GDA data objects?
	side = self -> _LeftOrRight(left, right, ISMRVARIABLE=IsMrVariable)

;-------------------------------------------------------
; Two MrMatrixTS Objects ///////////////////////////////
;-------------------------------------------------------
	;Both LEFT and RIGHT are MrMatrixTS objects
	if IsMrVariable then begin
		;Name of result
		name = 'MOD(' + self.name + ',' + right.name + ')'
		
		;SELF is LEFT
		if side eq 'LEFT' then begin
			case obj_class(right) of
				'MRVARIABLE': result = *self.data mod right['DATA']
				'MRSCALARTS': begin
					;Allocate memory
					type   = size( (*self.data)[0] mod right[[0]], /TYPE )
					dims   = size(*self.data, /DIMENSIONS)
					result = make_array( dims, TYPE=type )
					
					;Multiply
					for i = 0, dims[1]-1 do begin
					for j = 0, dims[2]-1 do begin
						result[*,i,j] = (*self.data)[*,i,j] mod right['DATA']
					endfor
					endfor
				end
				'MRVECTORTS': message, 'MrMatrixTS MOD MrVectorTS not allowed.'
				'MRMATRIXTS': result = *self.data mod right['DATA']
				else: message, 'Unrecognized object class: "' + obj_class(right) + '".'
			endcase
		
		;SELF is RIGHT
		endif else begin
			case obj_class(left) of
				'MRVARIABLE': result = left['DATA'] mod *self.data
				'MRSCALARTS': begin
					;Allocate memory
					type   = size( left[[0]] mod (*self.data)[0], /TYPE )
					dims   = size(*self.data, /DIMENSIONS)
					result = make_array( dims, TYPE=type )
					
					;Multiply
					for i = 0, dims[1]-1 do begin
					for j = 0, dims[2]-1 do begin
						result[*,i,j] = left['DATA'] mod (*self.data)[*,i,j]
					endfor
					endfor
				end
				'MRVECTORTS': message, 'MrVectorTS MOD MrMatrixTS not allowed.'
				'MRMATRIXTS': result = left['DATA'] mod *self.data
				else: message, 'Unrecognized object class: "' + obj_class(left) + '".'
			endcase
		endelse

;-------------------------------------------------------
; MrMatrixTS with Expression ///////////////////////////
;-------------------------------------------------------
	endif else begin
		;Add the expressions
		;   - Assume the user knows what they are doing.
		;   - All IDL truncation effects apply (shortest in determines size out).
		if side eq 'LEFT' $
			then result = (*self.data) mod right['ARRAY'] $
			else result = left['ARRAY'] mod (*self.data)
		
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
function MrMatrixTS::_OverloadPlus, left, right
	compile_opt idl2
	on_error, 2

	;Is SELF on the left or right?
	;   - Are both LEFT and RIGHT GDA data objects?
	side = self -> _LeftOrRight(left, right, ISMRVARIABLE=IsMrVariable)

;-------------------------------------------------------
; Two MrMatrixTS Objects //////////////////////////////////
;-------------------------------------------------------
	;Both LEFT and RIGHT are MrMatrixTS objects
	if IsMrVariable then begin
		;Name of result
		name = self.name + '+' + right.name
		
		;SELF is LEFT
		if side eq 'LEFT' then begin
			case obj_class(right) of
				'MRVARIABLE': result = *self.data + right['DATA']
				'MRSCALARTS': begin
					;Allocate memory
					type   = size( (*self.data)[0] + right[[0]], /TYPE )
					dims   = size(*self.data, /DIMENSIONS)
					result = make_array( dims, TYPE=type )
					
					;Multiply
					for i = 0, dims[1]-1 do begin
					for j = 0, dims[2]-1 do begin
						result[*,i,j] = (*self.data)[*,i,j] + right['DATA']
					endfor
					endfor
				end
				'MRVECTORTS': message, 'MrMatrixTS + MrVectorTS not allowed.'
				'MRMATRIXTS': result = *self.data + right['DATA']
				else: message, 'Unrecognized object class: "' + obj_class(right) + '".'
			endcase
		
		;SELF is RIGHT
		endif else begin
			case obj_class(left) of
				'MRVARIABLE': result = left['DATA'] + *self.data
				'MRSCALARTS': begin
					;Allocate memory
					type   = size( left[[0]] + (*self.data)[0], /TYPE )
					dims   = size(*self.data, /DIMENSIONS)
					result = make_array( dims, TYPE=type )
					
					;Multiply
					for i = 0, dims[1]-1 do begin
					for j = 0, dims[2]-1 do begin
						result[*,i,j] = left['DATA'] + (*self.data)[*,i,j]
					endfor
					endfor
				end
				'MRVECTORTS': message, 'MrVectorTS + MrMatrixTS not allowed.'
				'MRMATRIXTS': result = left['DATA'] + *self.data
				else: message, 'Unrecognized object class: "' + obj_class(left) + '".'
			endcase
		endelse

;-------------------------------------------------------
; MrMatrixTS with Expression //////////////////////////////
;-------------------------------------------------------
	endif else begin
		;Add the expressions
		;   - Assume the user knows what they are doing.
		;   - All IDL truncation effects apply (shortest in determines size out).
		if side eq 'LEFT' $
			then result = (*self.data) + right['ARRAY'] $
			else result = left['ARRAY'] + (*self.data)
		
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
function MrMatrixTS::_OverloadPound, left, right
	compile_opt idl2
	on_error, 2

	;Is SELF on the left or right?
	;   - Are both LEFT and RIGHT GDA data objects?
	side = self -> _LeftOrRight(left, right, ISMRVARIABLE=IsMrVariable)

;-------------------------------------------
; Object ///////////////////////////////////
;-------------------------------------------
	
	if IsMrVariable then begin
		Lsize  = size(left)
		Rsize  = size(right)
		Rclass = obj_class(right)
		Lclass = obj_class(left)
		if n_elements(name) eq 0 then name = left.name + '#' + right.name

	;-------------------------------------------
	; Matrix # Matrix //////////////////////////
	;-------------------------------------------
		if Lclass eq 'MRMATRIXTS' && Rclass eq 'MRMATRIXTS' then begin
			;LEFT=Nx3x3, RIGHT=Nx3x3
			if array_equal(Lsize[2:3], 3) && array_equal(Rsize[2:3], 3) then begin
				result = [ [ [left[*,0,0]*right[*,0,0] + left[*,0,1]*right[*,1,0] + left[*,0,2]*right[*,2,0]], $
				             [left[*,1,0]*right[*,0,0] + left[*,1,1]*right[*,1,0] + left[*,1,2]*right[*,2,0]], $
				             [left[*,2,0]*right[*,0,0] + left[*,2,1]*right[*,1,0] + left[*,2,2]*right[*,2,0]] ], $
				            
				           [ [left[*,0,0]*right[*,0,1] + left[*,0,1]*right[*,1,1] + left[*,0,2]*right[*,2,1]], $
				             [left[*,1,0]*right[*,0,1] + left[*,1,1]*right[*,1,1] + left[*,1,2]*right[*,2,1]], $
				             [left[*,2,0]*right[*,0,1] + left[*,2,1]*right[*,1,1] + left[*,2,2]*right[*,2,1]] ], $
				             
				           [ [left[*,0,0]*right[*,0,2] + left[*,0,1]*right[*,1,2] + left[*,0,2]*right[*,2,2]], $
				             [left[*,1,0]*right[*,0,2] + left[*,1,1]*right[*,1,2] + left[*,1,2]*right[*,2,2]], $
				             [left[*,2,0]*right[*,0,2] + left[*,2,1]*right[*,1,2] + left[*,2,2]*right[*,2,2]] ] ]
			
			;LEFT=anything else; RIGHT=anything else
			endif else begin
				;Pick the length with fewest elements
				nPts = Lsize[1] < Rsize[1]
				
				;Allocate memory to results
				result = make_array(nPts, Lsize[2], Rsize[3], $
				                    TYPE=size( reform(left[0,*,*])#reform(right[0,*,*]), /TYPE))
				
				;Compute result
				;   - Increment to the least number of elements
				foreach mat, left, idx do begin
					result[idx,*,*] = mat # reform(right[idx,*,*])
					if idx eq nPts-1 then break
				endforeach
			endelse

	;-------------------------------------------
	; Matrix # Vector //////////////////////////
	;-------------------------------------------
		endif else if Lclass eq 'MRMATRIXTS' && Rclass eq 'MRVECTORTS' then begin
			;LEFT=Nx3x3, RIGHT=Nx3
			if array_equal(Lsize[1:2], 3) then begin
				result = [ [left[*,0,0]*right[*,0] + left[*,0,1]*right[*,1] + left[*,0,2]*right[*,2]], $
				           [left[*,1,0]*right[*,0] + left[*,1,1]*right[*,1] + left[*,1,2]*right[*,2]], $
				           [left[*,2,0]*right[*,0] + left[*,2,1]*right[*,1] + left[*,2,2]*right[*,2]] ]
			
			;LEFT=anything else, RIGHT=Nx3
			endif else begin
				;Pick the length with fewest elements
				nPts = Lsize[1] < Rsize[1]
				
				;Allocate memory to results
				result = make_array(nPts, Lsize[2], $
				                    TYPE=size( reform(left[0,*,*]) # reform(right[0,*]), /TYPE))
				
				;Compute result
				;   - Increment to the least number of elements
				foreach mat, left, idx do begin
					result[idx,*,*] = mat # reform(right[idx,*])
					if idx eq nPts-1 then break
				endforeach
			endelse

	;-------------------------------------------
	; Vector # Matrix //////////////////////////
	;-------------------------------------------
		endif else if Lclass eq 'MRVECTORTS' && Rclass eq 'MRMATRIXTS' then begin
			;LEFT=Nx3, RIGHT=Nx3x3
			if array_equal(Lsize[1:2], 3) then begin
				result = [ [left[*,0]*right[*,0,0] + left[*,1]*right[*,0,1] + left[*,2]*right[*,0,2]], $
				           [left[*,0]*right[*,1,0] + left[*,1]*right[*,1,1] + left[*,2]*right[*,1,2]], $
				           [left[*,0]*right[*,2,0] + left[*,1]*right[*,2,1] + left[*,2]*right[*,2,2]] ]
				
				;The result is a vector
				result = MrVector(result, /NO_COPY, NAME=name)
			
			;LEFT=Nx3, RIGHT=anything else
			endif else begin
				;Pick the length with fewest elements
				nPts = Lsize[1] < Rsize[1]
				
				;Allocate memory to results
				result = make_array(nPts, Lsize[2], Rsize[2], $
				                    TYPE=size( reform(left[0,*]) # reform(right[0,*,*]), /TYPE))
				
				;Compute result
				;   - Increment to the least number of elements
				foreach vec, left, idx do begin
					result[idx,*,*] = vec # reform(right[idx,*,*])
					if idx eq nPts-1 then break
				endforeach
			endelse
			
		endif else begin
			message, 'Inputs are of incorrect type.'
		endelse
	endif else begin
		message, 'Both values in # must be MrVariable objects or subclasses.'
	endelse

;-------------------------------------------------------
; Output Object ////////////////////////////////////////
;-------------------------------------------------------
	;Is the result a vector
	sz = size(results)
	
	;Return a vector
	if (Lclass eq 'MRVECTOR' || Rclass eq 'MRVECTOR') && $
	   (sz[0] eq 1 && sz[1] eq 3) || $
	   (sz[0] eq 2 && (sz[1] eq 3 || sz[2] eq 3)) $
	then begin
		result = MrVector(result, /NO_COPY, NAME=name)
	
	;Return a matrix
	endif else if sz[0] eq 3 then begin
		result = MrMatrixTS(result, /NO_COPY, NAME=name)
	
	;Return a variable
	endif else begin
		result = MrVariable(result, /NO_COPY, NAME=name)
	endelse

	;Create a new object based on the results
	return, result
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
function MrMatrixTS::_OverloadPoundPound, left, right
	compile_opt idl2
	on_error, 2

	;Is SELF on the left or right?
	;   - Are both LEFT and RIGHT GDA data objects?
	side = self -> _LeftOrRight(left, right, ISMRVARIABLE=IsMrVariable)

;-------------------------------------------
; Object ///////////////////////////////////
;-------------------------------------------
	
	if IsMrVariable then begin
		Lsize  = size(left)
		Rsize  = size(right)
		Rclass = obj_class(right)
		Lclass = obj_class(left)
		name = left.name + '#' + right.name

	;-------------------------------------------
	; Matrix ## Matrix /////////////////////////
	;-------------------------------------------
		if Lclass eq 'MRMATRIXTS' && Rclass eq 'MRMATRIXTS' then begin
			;LEFT=Nx3x3, RIGHT=Nx3x3
			if array_equal(Lsize[2:3], 3) && array_equal(Rsize[2:3], 3) then begin
				result = [ [ [left[*,0,0]*right[*,0,0] + left[*,1,0]*right[*,0,1] + left[*,2,0]*right[*,0,2]], $
				             [left[*,0,0]*right[*,1,0] + left[*,1,0]*right[*,1,1] + left[*,2,0]*right[*,1,2]], $
				             [left[*,0,0]*right[*,2,0] + left[*,1,0]*right[*,2,1] + left[*,2,0]*right[*,2,2]] ], $
				           
				           [ [left[*,0,1]*right[*,0,0] + left[*,1,1]*right[*,0,1] + left[*,2,1]*right[*,0,2]], $
				             [left[*,0,1]*right[*,1,0] + left[*,1,1]*right[*,1,1] + left[*,2,1]*right[*,1,2]], $
				             [left[*,0,1]*right[*,2,0] + left[*,1,1]*right[*,2,1] + left[*,2,1]*right[*,2,2]] ], $
				           
				           [ [left[*,0,2]*right[*,0,0] + left[*,1,2]*right[*,0,1] + left[*,2,2]*right[*,0,2]], $
				             [left[*,0,2]*right[*,1,0] + left[*,1,2]*right[*,1,1] + left[*,2,2]*right[*,1,2]], $
				             [left[*,0,2]*right[*,2,0] + left[*,1,2]*right[*,2,1] + left[*,2,2]*right[*,2,2]] ] ]
			
			;LEFT=anything else; RIGHT=anything else
			endif else begin
				;Pick the length with fewest elements
				nPts = Lsize[1] < Rsize[1]
				
				;Allocate memory to results
				result = make_array(nPts, Lsize[3], Rsize[2], $
				                    TYPE=size( reform(left[0,*,*]) ## reform(right[0,*,*]), /TYPE))
				
				;Compute result
				;   - Increment to the least number of elements
				foreach mat, left, idx do begin
					result[idx,*,*] = mat ## reform(right[idx,*,*])
					if idx eq nPts-1 then break
				endforeach
			endelse

	;-------------------------------------------
	; Matrix ## Vector /////////////////////////
	;-------------------------------------------
		endif else if Lclass eq 'MRMATRIXTS' && Rclass eq 'MRVECTORTS' then begin
			;LEFT=Nx3x3, RIGHT=Nx3
			if array_equal(Lsize[2:3], 3) then begin
				result = [ [left[*,0,0]*right[*,0] + left[*,1,0]*right[*,1] + left[*,2,0]*right[*,2]], $
				           [left[*,0,1]*right[*,0] + left[*,1,1]*right[*,1] + left[*,2,1]*right[*,2]], $
				           [left[*,0,2]*right[*,0] + left[*,1,2]*right[*,1] + left[*,2,2]*right[*,2]] ]

			;LEFT=anything else, RIGHT=Nx3
			endif else begin
				;Pick the length with fewest elements
				nPts = Lsize[1] < Rsize[1]
				
				;Allocate memory to results
				result = make_array(nPts, Lsize[3], $
				                    TYPE=size( reform(left[0,*,*]) ## reform(right[0,*]), /TYPE))

				;Compute result
				;   - Increment to the least number of elements
				foreach mat, left, idx do begin
					result[idx,*] = mat ## reform(right[idx,*])
					if idx eq nPts - 1 then break
				endforeach
			endelse

	;-------------------------------------------
	; Vector # Matrix //////////////////////////
	;-------------------------------------------
		endif else if Lclass eq 'MRVECTORTS' && Rclass eq 'MRMATRIXTS' then begin
			;LEFT=Nx3, RIGHT=Nx3x3
			if array_equal(Lsize[1:2], 3) then begin
				result = [ [left[*,0]*right[*,0,0] + left[*,1]*right[*,1,0] + left[*,2]*right[*,2,0]], $
				           [left[*,0]*right[*,0,1] + left[*,1]*right[*,1,1] + left[*,2]*right[*,2,1]], $
				           [left[*,0]*right[*,0,2] + left[*,1]*right[*,1,2] + left[*,2]*right[*,2,2]] ]
				
				;The result is a vector
				result = MrVector(result, /NO_COPY, NAME=name)
			
			;LEFT=Nx3, RIGHT=anything else
			endif else begin
				;Pick the length with fewest elements
				nPts = Lsize[1] < Rsize[1]
				
				;Allocate memory to results
				result = make_array(nPts, Rsize[1], $
				                    TYPE=size( reform(left[0,*]) ## reform(right[0,*,*]), /TYPE))
				
				;Compute result
				;   - Increment to the least number of elements
				foreach vec, left, idx do begin
					result[idx,*] = vec ## reform(right[idx,*,*])
					if idx eq nPts-1 then break
				endforeach
			endelse
			
		endif else begin
			message, 'Inputs are of incorrect type.'
		endelse
	endif else begin
		message, 'Both values in ## must be MrVariable objects or subclasses.'
	endelse

;-------------------------------------------------------
; Output Object ////////////////////////////////////////
;-------------------------------------------------------
	;Is the result a vector
	sz = size(results)
	
	;Return a vector
	if (Lclass eq 'MRVECTOR' || Rclass eq 'MRVECTOR') && $
	   (sz[0] eq 1 && sz[1] eq 3) || $
	   (sz[0] eq 2 && (sz[1] eq 3 || sz[2] eq 3)) $
	then begin
		result = MrVectorTS(result, /NO_COPY, NAME=name)
	
	;Return a matrix
	endif else if sz[0] eq 3 then begin
		result = MrMatrixTS(result, /NO_COPY, NAME=name)
	
	;Return a variable
	endif else begin
		result = MrVariable(result, /NO_COPY, NAME=name)
	endelse

	;Create a new object based on the results
	return, result
end


;+
;   The purpose of this method is to provide information when implied print is used.
;   is called.
;-
function MrMatrixTS::_OverloadPrint
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
function MrMatrixTS::_OverloadSize
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
function MrMatrixTS::_OverloadSlash, left, right
	compile_opt idl2
	on_error, 2

	;Is SELF on the left or right?
	;   - Are both LEFT and RIGHT data objects?
	side = self -> _LeftOrRight(left, right, ISMRVARIABLE=IsMrVariable)

;-------------------------------------------------------
; Two MrMatrixTS Objects //////////////////////////////////
;-------------------------------------------------------
	;Both LEFT and RIGHT are MrMatrixTS objects
	if IsMrVariable then begin
		;Name of result
		name = self.name + '/' + right.name
		
		;SELF is LEFT
		if side eq 'LEFT' then begin
			case obj_class(right) of
				'MRVARIABLE': result = *self.data / right['DATA']
				'MRSCALARTS': begin
					;Allocate memory
					type   = size( (*self.data)[0] / right[[0]], /TYPE )
					dims   = size(*self.data, /DIMENSIONS)
					result = make_array( dims, TYPE=type )
					
					;Multiply
					for i = 0, dims[1]-1 do begin
					for j = 0, dims[2]-1 do begin
						result[*,i,j] = (*self.data)[*,i,j] / right['DATA']
					endfor
					endfor
				end
				'MRVECTORTS': message, 'MrMatrixTS / MrVectorTS not allowed.'
				'MRMATRIXTS': result = *self.data / right['DATA']
				else: message, 'Unrecognized object class: "' + obj_class(right) + '".'
			endcase
		
		;SELF is RIGHT
		endif else begin
			case obj_class(left) of
				'MRVARIABLE': result = left['DATA'] / *self.data
				'MRSCALARTS': begin
					;Allocate memory
					type   = size( left[[0]] / (*self.data)[0], /TYPE )
					dims   = size(*self.data, /DIMENSIONS)
					result = make_array( dims, TYPE=type )
					
					;Multiply
					for i = 0, dims[1]-1 do begin
					for j = 0, dims[2]-1 do begin
						result[*,i,j] = left['DATA'] / (*self.data)[*,i,j]
					endfor
					endfor
				end
				'MRVECTORTS': message, 'MrVectorTS / MrMatrixTS not allowed.'
				'MRMATRIXTS': result = left['DATA'] / *self.data
				else: message, 'Unrecognized object class: "' + obj_class(left) + '".'
			endcase
		endelse

;-------------------------------------------------------
; MrMatrixTS with Expression //////////////////////////////
;-------------------------------------------------------
	endif else begin
		;Divide the expressions
		;   - Assume the user knows what they are doing.
		;   - All IDL truncation effects apply (shortest in determines size out).
		if side eq 'LEFT' $
			then result = (*self.data) / right['ARRAY'] $
			else result = left['ARRAY'] / (*self.data)
		
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
;   A wrapper for the # operator or the Matrix_Multiply function, generalized to rank N
;   tensors. IDL's Matrix_Multiply truncate dimensions beyond rank 2.
;
;   For two IDL tensors, A and B, the last dimension of A will be contracted with the
;   first dimension of B.
;
; :Examples:
;   See the example program at the end of this document::
;       IDL> .r MrInnerProduct
;
; :Params:
;       A:          in, required, type=tensor rank n
;                   Rank N tensor whose last dimension will be contracted with the
;                       first dimension of `B`.
;       B:          in, required, type=tensor rank n
;                   Rank N tensor whose first dimension will be contracted with the
;                       last dimension of `A`.
;
; :Returns:
;       RESULT:     Result of the inner product.
;-
function MrMatrixTS::InnerProduct, A, B
	compile_opt strictarr
	on_error, 2

	;Dimensionality of inputs
	nDimsA = size(A, /N_DIMENSIONS)
	nDimsB = size(B, /N_DIMENSIONS)
	dimsA = size(A, /DIMENSIONS)
	dimsB = size(B, /DIMENSIONS)

	;Reform _A and B so that they have two dimensions
	_A = (nDimsA le 2) ? A : reform(A, product(dimsA[0:nDimsA-2]), dimsA[nDimsA-1])
	_B = (nDimsB le 2) ? B : reform(B, dimsB[0], product(dimsB[1:*]))

	;Compute the inner product
	result = temporary(_A) # temporary(_B)

	;Reform back to the original shape, minus the contracted dimension.
	if nDimsA gt 2 then dimsOut = dimsA[0:nDimsA-2]
	if nDimsB gt 2 then begin
		if n_elements(dimsOut) gt 0 $
			then dimsOut = [dimsOut, dimsB[1:*]] $
			else dimsOut = dimsB[1:*]
	endif
	if n_elements(dimsOut) gt 0 $
		then result = reform(result, dimsOut, /OVERWRITE)

	return, result
end


;+
;   A wrapper for the ## operator, generalized to rank N tensors. IDL's ## operator
;   truncate dimensions beyond rank 2.
;
;   For two IDL tensors, A and B, the first dimension of A will be contracted with the
;   last dimension of B.
;
; :Examples:
;   See the example program at the end of this document::
;       IDL> .r MrOuterProduct
;
; :Params:
;       A:          in, required, type=tensor rank n
;                   Rank N tensor whose first dimension will be contracted with the
;                       last dimension of `B`.
;       B:          in, required, type=tensor rank n
;                   Rank N tensor whose last dimension will be contracted with the
;                       first dimension of `A`.
;
; :Returns:
;       RESULT:     Result of the outer product.
;-
function MrMatrixTS::OuterProduct, A, B
	compile_opt strictarr
	on_error, 2

	;Dimensionality of inputs
	nDimsA = size(A, /N_DIMENSIONS)
	nDimsB = size(B, /N_DIMENSIONS)
	dimsA = size(A, /DIMENSIONS)
	dimsB = size(B, /DIMENSIONS)

	;Reform _A and B so that they have two dimensions
	_A = (nDimsA le 2) ? A : reform(A, dimsA[0], product(dimsA[1:*]))
	_B = (nDimsB le 2) ? B : reform(B, product(dimsB[0:nDimsB-2]), dimsB[nDimsB-1])

	;Compute the inner product
	result = temporary(_A) ## temporary(_B)

	;Reform back to the original shape, minus the contracted dimension.
	if nDimsA gt 2 then dimsOut = dimsA[1:*]
	if nDimsB gt 2 then begin
		if n_elements(dimsOut) gt 0 $
			then dimsOut = [dimsOut, dimsB[0:nDimsB-2]] $
			else dimsOut = dimsB[0:nDimsB-2]
	endif
	if n_elements(dimsOut) gt 0 $
		then result = reform(result, dimsOut, /OVERWRITE)
	
	return, result
end

;+
;   The purpose of this program is to rotate a 3x3 or 3x3xN matrix via a standard
;   rotation::
;
;           A' = R A R^T
;
;       Where R^T is the transpose of the rotation matrix R.
;
;   The tensor product of two fields is::
;                | HxKx  HxKy  HxKz |
;       P = HK = | HyKx  HyKy  HyKz | = HiKj
;                | HzKx  HzKy  HzKz |
;
;   In a different coordinate system, H and K are given by::
;       H'i = Aij Hj     K'l = Alm Km
;
;   The tensor H'K', then, is::
;       P' = H'i K'j = (Aij Hj) (Alm Km)
;                    = Aij Alm (HK)jm
;                    = Aij (Aml)^T (HK)jm
;                    = Aij (HK)jm (Aml)^T
;                    = A HK A^T
;
; :Keywords:
;       MAT:                in, required, type=NxM array
;                           Matrix to be rotated. Can be a MrVariable or MrMatrixTS object.
;
; :Keywords:
;       CACHE:              in, optional, type=boolean, default=0
;                           If set, the returned MrVectorTS will be cached.
;       NAME:               in, optional, type=string, default=<name>_rot
;                           Name to be given to the resulting MrScalar object. 
;-
function MrMatrixTS::Rotate_Matrix, mat, $
CACHE=cache, $
NAME=name
	compile_opt idl2
	on_error, 2

	if n_elements(name) eq 0 then name = self.name + '_rot'

;-----------------------------------------------------
; MrVariable Object \\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\
;-----------------------------------------------------
	if isa(vec, 'MrMatrixTS') then begin
		;A and B must have the same dimensions.
		Adims = size(self, /DIMENSIONS)
		Bdims = size(mat, /DIMENSIONS)
		if ~array_equal(Adims[1:2], Bdims[1:2]) then message, 'MAT is in correct size.'
		
		;Rotate
		out = self # mat # self -> Transpose()

;-----------------------------------------------------
; Other Array \\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\
;-----------------------------------------------------
	endif else if isa(vec, /NUMBER) || isa(vec, 'MrVariable') then begin
		;Check sizes
		sz = size(data)

		;VEC is 3xN (Treat 3x3 as Nx3)
		if (sz[0] eq 1 && sz[1] eq 3) || (sz[0] eq 2 && sz[1] eq 3 && sz[2] ne 3) then begin
			outVec = [ [(*self.data)[*,0,0]*vec[0,*] + (*self.data)[*,1,0]*vec[1,*] + (*self.data)[*,2,0]*vec[2,*]], $
			           [(*self.data)[*,0,1]*vec[0,*] + (*self.data)[*,1,1]*vec[1,*] + (*self.data)[*,2,1]*vec[2,*]], $
			           [(*self.data)[*,0,2]*vec[0,*] + (*self.data)[*,1,2]*vec[1,*] + (*self.data)[*,2,2]*vec[2,*]] ]
		
		;VEC is Nx3
		endif else if (sz[0] eq 2 && sz[2] eq 3) then begin
			outVec = [ [(*self.data)[*,0,0]*vec[*,0] + (*self.data)[*,1,0]*vec[*,1] + (*self.data)[*,2,0]*vec[*,1]], $
			           [(*self.data)[*,0,1]*vec[*,0] + (*self.data)[*,1,1]*vec[*,1] + (*self.data)[*,2,1]*vec[*,1]], $
			           [(*self.data)[*,0,2]*vec[*,0] + (*self.data)[*,1,2]*vec[*,1] + (*self.data)[*,2,2]*vec[*,1]] ]
		endif else begin
			message, 'VEC must be 3xN or Nx3.'
		endelse
		
;-----------------------------------------------------
; Incorrect Type \\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\
;-----------------------------------------------------
	endif else begin
		message, 'VEC is the incorrect datatype.'
	endelse
	
;-----------------------------------------------------
; Output \\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\
;-----------------------------------------------------
	oVec = self -> Copy(name, CACHE=cache)
	oVec -> SetData, outVec, /NO_COPY

	return, oVec
end


;+
;   Rotate a vector.
;
; :Keywords:
;       VEC:                in, required, type=Nx3 or 3xN array
;                           Array to be rotated. Can be a MrVariable or MrVectorTS object.
;                               If 3x3, it will be treated as Nx3.
;
; :Keywords:
;       CACHE:              in, optional, type=boolean, default=0
;                           If set, the returned MrVectorTS will be cached.
;       NAME:               in, optional, type=string, default=<name>_rot
;                           Name to be given to the resulting MrScalar object. 
;-
function MrMatrixTS::Rotate_Vector, vec, $
CACHE=cache, $
NAME=name
	compile_opt idl2
	on_error, 2

	dims = size(*self.data, /DIMENSIONS)
	if ~array_equal(dims[1:2], 3) then message, 'MrMatrix must be 3x3.'
	if n_elements(name) eq 0 then name = self.name + '_rot'

;-----------------------------------------------------
; MrVariable Object \\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\
;-----------------------------------------------------
	if isa(vec, 'MrVectorTS') then begin
		;MrVectorTS
		outVec = [ [(*self.data)[*,0,0]*vec[*,0] + (*self.data)[*,1,0]*vec[*,1] + (*self.data)[*,2,0]*vec[*,2]], $
		           [(*self.data)[*,0,1]*vec[*,0] + (*self.data)[*,1,1]*vec[*,1] + (*self.data)[*,2,1]*vec[*,2]], $
		           [(*self.data)[*,0,2]*vec[*,0] + (*self.data)[*,1,2]*vec[*,1] + (*self.data)[*,2,2]*vec[*,2]] ]

;-----------------------------------------------------
; Normal Array \\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\
;-----------------------------------------------------
	endif else if isa(vec, /NUMBER) || isa(vec, 'MrVariable') then begin
		;Check sizes
		sz = size(data)

		;VEC is 3xN (Treat 3x3 as Nx3)
		if (sz[0] eq 1 && sz[1] eq 3) || (sz[0] eq 2 && sz[1] eq 3 && sz[2] ne 3) then begin
			outVec = [ [(*self.data)[*,0,0]*vec[0,*] + (*self.data)[*,1,0]*vec[1,*] + (*self.data)[*,2,0]*vec[2,*]], $
			           [(*self.data)[*,0,1]*vec[0,*] + (*self.data)[*,1,1]*vec[1,*] + (*self.data)[*,2,1]*vec[2,*]], $
			           [(*self.data)[*,0,2]*vec[0,*] + (*self.data)[*,1,2]*vec[1,*] + (*self.data)[*,2,2]*vec[2,*]] ]
		
		;VEC is Nx3
		endif else if (sz[0] eq 2 && sz[2] eq 3) then begin
			outVec = [ [(*self.data)[*,0,0]*vec[*,0] + (*self.data)[*,1,0]*vec[*,1] + (*self.data)[*,2,0]*vec[*,1]], $
			           [(*self.data)[*,0,1]*vec[*,0] + (*self.data)[*,1,1]*vec[*,1] + (*self.data)[*,2,1]*vec[*,1]], $
			           [(*self.data)[*,0,2]*vec[*,0] + (*self.data)[*,1,2]*vec[*,1] + (*self.data)[*,2,2]*vec[*,1]] ]
		endif else begin
			message, 'VEC must be 3xN or Nx3.'
		endelse
		
;-----------------------------------------------------
; Incorrect Type \\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\
;-----------------------------------------------------
	endif else begin
		message, 'VEC is the incorrect datatype.'
	endelse
	
;-----------------------------------------------------
; Output \\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\
;-----------------------------------------------------
	oVec = MrVectorTS(outVec, NAME=name, CACHE=cache, /NO_COPY)

	return, oVec
end


;+
;   Transpose the matrix. An TxMxN matrix becomes TxNxM.
;
; :Keywords:
;       CACHE:              in, optional, type=boolean, default=0
;                           If set, the returned MrMatrixTS will be cached.
;       NAME:               in, optional, type=string, default=<name>_transpose
;                           Name to be given to the resulting MrScalar object. 
;-
function MrMatrixTS::Transpose, $
CACHE=cache, $
NAME=name
	compile_opt idl2
	on_error, 2

	;Default name
	if n_elements(name) eq 0 then name = self.name + '_transpose'
	
	;Copy the array contents
	outMat = self -> Copy(name, CACHE=cache)

	;Transpose and set data
	outMat -> SetData, transpose(*self.data, [0,2,1])
	
	return, outMat
end


;+
;   Set the array.
;
; :Keywords:
;       DATA:              in, required, type=NxM array
;                           Array of values to be stored, or a MrMatrixTS object whose
;                               array is to be copied. 
;
; :Keywords:
;       NO_COPY:            in, optional, type=boolean, default=0
;                           If set `DATA` will be copied directly into the object
;                               and will be left undefined (a MrMatrixTS object will not
;                               be destroyed, but its array will be empty).
;-
pro MrMatrixTS::SetData, data, $
NO_COPY=no_copy
	compile_opt idl2
	on_error, 2

	no_copy = keyword_set(no_copy)

;-----------------------------------------------------
; MrVariable Object \\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\
;-----------------------------------------------------
	if size(data, /TNAME) eq 'OBJREF' then begin
		;MrMatrixTS
		if obj_isa(data, 'MrMatrixTS') then begin
			*self.data = data -> GetData()
		
		;MrVariable
		endif else if obj_isa(data, 'MrVariable') then begin
			;Check sizes
			sz = size(data)
			if sz[0] lt 2 || sz[0] gt 3 then message, 'DATA must be 2D or 3D.'
			
			;Set data
			;   - Save as NxMxL
			if nDims eq 2 $
				then *self.data = reform(data -> GetData(), 1, sz[1], sz[2]) $
				else *self.data = data -> GetData()
		
		;Other
		endif else begin
			message, 'Only "MrMatrixTS" or "MrVariable" objects can be given.'
		endelse

;-----------------------------------------------------
; Normal Array \\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\
;-----------------------------------------------------
	endif else begin
		;Check sizes
		sz = size(data)
		if sz[0] lt 2 || sz[0] gt 3 then message, 'DATA must be 2D or 3D.'

		;Set data
		if sz[0] eq 2 then begin
			if no_copy $
				then *self.data = reform(temporary(data), 1 , sz[1], sz[2]) $
				else *self.data = reform(data, 1, sz[1], sz[2])
		endif else begin
			if no_copy $
				then *self.data = temporary(data) $
				else *self.data = data
		endelse
	endelse
end


;+
;   The class definition statement.
;
; :Params:
;       CLASS:          out, optional, type=structure
;-
pro MrMatrixTS__DEFINE
	compile_opt idl2
	
	class = { MrMatrixTS, $
	          inherits MrVariable $
	        }
end