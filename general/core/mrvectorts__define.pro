; docformat = 'rst'
;
; NAME:
;   MrVectorTS__Define
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
;   A class for time series vector products.
;
; :Categories:
;   MrVariable
;
; :See Also:
;   MrVariable__Define.pro
;   MrScalarTS__Define.pro
;   MrMatrixTS__Define.pro
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
;   Create a MrVectorTS object using a pre-existing array::
;       theArray = findgen(24, 36)
;       myArray  = MrVectorTS(theArray)
;       help, myArray
;           MYARRAY     OBJREF      <ObjHeapVar666(MrVectorTS)>
;             ARRAY       FLOAT     = Array[24, 36]
;
;   Initialize a MrVectorTS object via Make_Array::
;       myArray = MrVectorTS(24, 36, TYPE='ULong')
;       help, myArray
;           MYARRAY     OBJREF      <ObjHeapVar668(MrVectorTS)>
;             ARRAY       ULONG     = Array[24, 36]
;
;   Initialize a MrVectorTS object via RandomU::
;       myArray = MrVectorTS(24, 36, SEED=3)
;       print, myarray[0:3]
;           0.897916     0.558249     0.766930     0.589101
;
;   Initialize a MrVectorTS with a normal, gaussian distribution::
;       myArray = MrVectorTS(24, 36, SEED=3, /NORMAL)
;
; :Params:
;       DATA:               in, optional, type=any/integer
;                           If an array, then it is the array to be stored. If a scalar
;                               value is given, it signifies the number of vectors to
;                               allocate and `MAKE` is set to 1 automatically, unless
;                              `RANDOM` or `NORMAL` are in use.
;
; :Keywords:
;       CACHE:              in, optional, type=boolean, default=0
;                           If set, the variable will be placed into the cache.
;       MAKE:               in, optional, type=boolean
;                           If set, all parameters will be passed to the Make_Array
;                               function in order to create the array. This is the default
;                               if `DATA` is a scalar.
;       NO_CLOBBER:         in, optional, type=boolean, default=0
;                           If set and `NAME` already exists in the cache, then append
;                               '_#' to `NAME` to avoid conflict. "_#" is the smallest
;                               integer that results in a unique name. The default is to
;                               clobber cached variables with the same name.
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
function MrVectorTS::INIT, data, $
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
	if n_elements(name) eq 0 then name = 'MrVectorTS'

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
		make:     self -> Make_Array, data, 3, TYPE=type, _REF_EXTRA=extra
		normal:   self -> RandomN, seed, data, 3, _REF_EXTRA=extra
		random:   self -> RandomU, seed, data, 3, _REF_EXTRA=extra
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
pro MrVectorTS::CLEANUP
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
function MrVectorTS::_OverloadAsterisk, left, right
	compile_opt idl2
	on_error, 2

	;Is SELF on the left or right?
	;   - Are both LEFT and RIGHT GDA data objects?
	side = self -> _LeftOrRight(left, right, ISMRVARIABLE=IsMrVariable)

;-------------------------------------------------------
; Two MrVectorTS Objects //////////////////////////////////
;-------------------------------------------------------
	;Both LEFT and RIGHT are MrVectorTS objects
	if IsMrVariable then begin
		;Name of result
		name = self.name + '*' + right.name
		
		;SELF is LEFT
		if side eq 'LEFT' then begin
			case obj_class(right) of
				'MRVARIABLE': result = *self.data * right['DATA']
				'MRSCALARTS': begin
					type        = size( (*self.data)[0] * right[[0]], /TYPE )
					result      = make_array( size(*self.data, /DIMENSIONS), TYPE=type )
					result[*,0] = (*self.data)[*,0] * right['DATA']
					result[*,1] = (*self.data)[*,1] * right['DATA']
					result[*,2] = (*self.data)[*,2] * right['DATA']
				end
				'MRVECTORTS': result = *self.data * right['DATA']
				'MRMATRIXTS': message, 'MrVectorTS * MrMatrixTS not allowed.'
				else: message, 'Unrecognized object class: "' + obj_class(right) + '".'
			endcase
		
		;SELF is RIGHT
		endif else begin
			case obj_class(left) of
				'MRVARIABLE': result = left['DATA'] * *self.data
				'MRSCALARTS': begin
					type        = size( left[[0]] * (*self.data)[0], /TYPE )
					result      = make_array( size(*self.data, /DIMENSIONS), TYPE=type )
					result[*,0] = left['DATA'] * (*self.data)[*,0]
					result[*,1] = left['DATA'] * (*self.data)[*,1]
					result[*,2] = left['DATA'] * (*self.data)[*,2]
				endcase
				'MRVECTORTS': result = left['DATA'] * *self.data
				'MRMATRIXTS': message, 'MrMatrixTS * MrVectorTS not allowed.'
				else: message, 'Unrecognized object class: "' + obj_class(left) + '".'
			endcase
		endelse

;-------------------------------------------------------
; MrVectorTS with Expression //////////////////////////////
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
function MrVectorTS::_OverloadCaret, left, right
	compile_opt idl2
	on_error, 2

	;Is SELF on the left or right?
	;   - Are both LEFT and RIGHT GDA data objects?
	side = self -> _LeftOrRight(left, right, ISMRVARIABLE=IsMrVariable)

;-------------------------------------------------------
; Two MrVectorTS Objects //////////////////////////////////
;-------------------------------------------------------
	;Both LEFT and RIGHT are MrVectorTS objects
	if IsMrVariable then begin
		;Name of result
		name = self.name + '^' + right.name
		
		;SELF is LEFT
		if side eq 'LEFT' then begin
			case obj_class(right) of
				'MRVARIABLE': result = *self.data ^ right['DATA']
				'MRSCALARTS': begin
					type        = size( (*self.data)[0] ^ right[[0]], /TYPE )
					result      = make_array( size(*self.data, /DIMENSIONS), TYPE=type )
					result[*,0] = (*self.data)[*,0] ^ right['DATA']
					result[*,1] = (*self.data)[*,1] ^ right['DATA']
					result[*,2] = (*self.data)[*,2] ^ right['DATA']
				end
				'MRVECTORTS': result = *self.data ^ right['DATA']
				'MRMATRIXTS': message, 'MrVectorTS ^ MrMatrixTS not allowed.'
				else: message, 'Unrecognized object class: "' + obj_class(right) + '".'
			endcase
		
		;SELF is RIGHT
		endif else begin
			case obj_class(left) of
				'MRVARIABLE': result = left['DATA'] ^ *self.data
				'MRSCALARTS': begin
					type        = size( left[[0]] ^ (*self.data)[0], /TYPE )
					result      = make_array( size(*self.data, /DIMENSIONS), TYPE=type )
					result[*,0] = left['DATA'] ^ (*self.data)[*,0]
					result[*,1] = left['DATA'] ^ (*self.data)[*,1]
					result[*,2] = left['DATA'] ^ (*self.data)[*,2]
				endcase
				'MRVECTORTS': result = left['DATA'] ^ *self.data
				'MRMATRIXTS': message, 'MrMatrixTS ^ MrVectorTS not allowed.'
				else: message, 'Unrecognized object class: "' + obj_class(left) + '".'
			endcase
		endelse

;-------------------------------------------------------
; MrVectorTS with Expression //////////////////////////////
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
function MrVectorTS::_OverloadForeach, value, key
	compile_opt idl2
	on_error, 2

	nPts = n_elements( (*self.data)[*,0] )
	if n_elements(key) eq 0 then key = 0

	;Get the array element if the index is in range
	if key lt nPts then begin
		next = 1
		value = reform((*self.data)[key,*])
	
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
function MrVectorTS::_OverloadMinus, left, right
	compile_opt idl2
	on_error, 2

	;Is SELF on the left or right?
	;   - Are both LEFT and RIGHT GDA data objects?
	side = self -> _LeftOrRight(left, right, ISMRVARIABLE=IsMrVariable)

;-------------------------------------------------------
; Two MrVectorTS Objects //////////////////////////////////
;-------------------------------------------------------
	;Both LEFT and RIGHT are MrVectorTS objects
	if IsMrVariable then begin
		;Name of result
		name = self.name + '-' + right.name
		
		;SELF is LEFT
		if side eq 'LEFT' then begin
			case obj_class(right) of
				'MRVARIABLE': result = *self.data - right['DATA']
				'MRSCALARTS': begin
					type        = size( (*self.data)[0] - right[[0]], /TYPE )
					result      = make_array( size(*self.data, /DIMENSIONS), TYPE=type )
					result[*,0] = (*self.data)[*,0] - right['DATA']
					result[*,1] = (*self.data)[*,1] - right['DATA']
					result[*,2] = (*self.data)[*,2] - right['DATA']
				end
				'MRVECTORTS': result = *self.data - right['DATA']
				'MRMATRIXTS': message, 'MrVectorTS - MrMatrixTS not allowed.'
				else: message, 'Unrecognized object class: "' + obj_class(right) + '".'
			endcase
		
		;SELF is RIGHT
		endif else begin
			case obj_class(left) of
				'MRVARIABLE': result = left['DATA'] - *self.data
				'MRSCALARTS': begin
					type        = size( left[[0]] - (*self.data)[0], /TYPE )
					result      = make_array( size(*self.data, /DIMENSIONS), TYPE=type )
					result[*,0] = left['DATA'] - (*self.data)[*,0]
					result[*,1] = left['DATA'] - (*self.data)[*,1]
					result[*,2] = left['DATA'] - (*self.data)[*,2]
				endcase
				'MRVECTORTS': result = left['DATA'] - *self.data
				'MRMATRIXTS': message, 'MrMatrixTS - MrVectorTS not allowed.'
				else: message, 'Unrecognized object class: "' + obj_class(left) + '".'
			endcase
		endelse

;-------------------------------------------------------
; MrVectorTS with Expression //////////////////////////////
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
function MrVectorTS::_OverloadMOD, left, right
	compile_opt idl2
	on_error, 2

	;Is SELF on the left or right?
	;   - Are both LEFT and RIGHT GDA data objects?
	side = self -> _LeftOrRight(left, right, ISMRVARIABLE=IsMrVariable)

;-------------------------------------------------------
; Two MrVectorTS Objects ///////////////////////////////
;-------------------------------------------------------
	;Both LEFT and RIGHT are MrVectorTS objects
	if IsMrVariable then begin
		;Name of result
		name = 'MOD(' + self.name + ',' + right.name + ')'
		
		;SELF is LEFT
		if side eq 'LEFT' then begin
			case obj_class(right) of
				'MRVARIABLE': result = *self.data mod right['DATA']
				'MRSCALARTS': begin
					type        = size( (*self.data)[0] mod right[[0]], /TYPE )
					result      = make_array( size(*self.data, /DIMENSIONS), TYPE=type )
					result[*,0] = (*self.data)[*,0] mod right['DATA']
					result[*,1] = (*self.data)[*,1] mod right['DATA']
					result[*,2] = (*self.data)[*,2] mod right['DATA']
				end
				'MRVECTORTS': result = *self.data mod right['DATA']
				'MRMATRIXTS': message, 'MrVectorTS mod MrMatrixTS not allowed.'
				else: message, 'Unrecognized object class: "' + obj_class(right) + '".'
			endcase
		
		;SELF is RIGHT
		endif else begin
			case obj_class(left) of
				'MRVARIABLE': result = left['DATA'] mod *self.data
				'MRSCALARTS': begin
					type        = size( left[[0]] mod (*self.data)[0], /TYPE )
					result      = make_array( size(*self.data, /DIMENSIONS), TYPE=type )
					result[*,0] = left['DATA'] mod (*self.data)[*,0]
					result[*,1] = left['DATA'] mod (*self.data)[*,1]
					result[*,2] = left['DATA'] mod (*self.data)[*,2]
				endcase
				'MRVECTORTS': result = left['DATA'] mod *self.data
				'MRMATRIXTS': message, 'MrMatrixTS mod MrVectorTS not allowed.'
				else: message, 'Unrecognized object class: "' + obj_class(left) + '".'
			endcase
		endelse

;-------------------------------------------------------
; MrVectorTS with Expression ///////////////////////////
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
function MrVectorTS::_OverloadPlus, left, right
	compile_opt idl2
	on_error, 2

	;Is SELF on the left or right?
	;   - Are both LEFT and RIGHT GDA data objects?
	side = self -> _LeftOrRight(left, right, ISMRVARIABLE=IsMrVariable)

;-------------------------------------------------------
; Two MrVectorTS Objects ///////////////////////////////
;-------------------------------------------------------
	;Both LEFT and RIGHT are MrVectorTS objects
	if IsMrVariable then begin
		;Name of result
		name = self.name + '+' + right.name
		
		;SELF is LEFT
		if side eq 'LEFT' then begin
			case obj_class(right) of
				'MRVARIABLE': result = *self.data + right['DATA']
				'MRSCALARTS': begin
					type        = size( (*self.data)[0] + right[[0]], /TYPE )
					result      = make_array( size(*self.data, /DIMENSIONS), TYPE=type )
					result[*,0] = (*self.data)[*,0] + right['DATA']
					result[*,1] = (*self.data)[*,1] + right['DATA']
					result[*,2] = (*self.data)[*,2] + right['DATA']
				end
				'MRVECTORTS': result = *self.data + right['DATA']
				'MRMATRIXTS': message, 'MrVectorTS + MrMatrixTS not allowed.'
				else: message, 'Unrecognized object class: "' + obj_class(right) + '".'
			endcase
		
		;SELF is RIGHT
		endif else begin
			case obj_class(left) of
				'MRVARIABLE': result = left['DATA'] + *self.data
				'MRSCALARTS': begin
					type        = size( left[[0]] + (*self.data)[0], /TYPE )
					result      = make_array( size(*self.data, /DIMENSIONS), TYPE=type )
					result[*,0] = left['DATA'] + (*self.data)[*,0]
					result[*,1] = left['DATA'] + (*self.data)[*,1]
					result[*,2] = left['DATA'] + (*self.data)[*,2]
				endcase
				'MRVECTORTS': result = left['DATA'] + *self.data
				'MRMATRIXTS': message, 'MrMatrixTS + MrVectorTS not allowed.'
				else: message, 'Unrecognized object class: "' + obj_class(left) + '".'
			endcase
		endelse

;-------------------------------------------------------
; MrVectorTS with Expression ///////////////////////////
;-------------------------------------------------------
	endif else begin
		;Add the expressions
		;   - Assume the user knows what they are doing.
		;   - All IDL truncation effects apply (shortest in determines size out).
		if side eq 'LEFT' $
			then result = *self.data + right $
			else result = left       + *self.data
		
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
function MrVectorTS::_OverloadPound, left, right
	compile_opt idl2
	on_error, 2

	;Is SELF on the left or right?
	;   - Are both LEFT and RIGHT GDA data objects?
	side = self -> _LeftOrRight(left, right, ISMRVARIABLE=IsMrVariable)

;-------------------------------------------------------
; Two MrVectorTS Objects ///////////////////////////////
;-------------------------------------------------------
	;Both LEFT and RIGHT are MrVectorTS objects
	if IsMrVariable then begin
		;Name of result
		name = self.name + '#' + right.name
		
		;SELF is LEFT
		if side eq 'LEFT' then begin
			case obj_class(right) of
				'MRVARIABLE': result = *self.data # right['DATA']
				'MRSCALARTS': message, 'MrVectorTS # MrScalarTS operation not allowed.'
				'MRVECTORTS': begin
					;Compute the inner product.
					;   - This could be simplified as:
					;       total( *self.data * right['DATA'], 2, /PRESERVE_TYPE)
					;   - However, the Total function converts integer data to FLOAT
					temp   = (*self.data)[*,0] * right[*,0] + $
					         (*self.data)[*,1] * right[*,1] + $
					         (*self.data)[*,2] * right[*,2]
					result = MrScalarTS(temp, NAME=name, /NO_COPY)
				endcase
				'MRMATRIXTS': begin
					;Make sure the matrix is the correct size
					dims = size(right, /DIMENSIONS)
					if dims[1] ne 3 then message, 'MrMatrixTS dimensions must be Nx3xM.'
					
					;Allocate memory
					type = size( (*self.data)[0] * right[[0]], /TYPE )
					temp = make_array( dims[0], dims[2], TYPE=type)
					
					;IDL's inner product
					for i = 0, dims[1] - 1 do begin
						temp[*,i] = (*self.data)[*,0] * right[*,0,i] + $
						            (*self.data)[*,1] * right[*,1,i] + $
						            (*self.data)[*,2] * right[*,2,i]
					endfor
						
					;Create matrix
					if dims[2] eq 3 $
						then result = MrVectorTS(temp, NAME=name, /NO_COPY) $
						else result = MrVariable(temp, NAME=name, /NO_COPY)
				endcase
				else: message, 'Unrecognized object class: "' + obj_class(right) + '".'
			endcase
		
		;SELF is RIGHT
		endif else begin
			case obj_class(left) of
				'MRVARIABLE': result = left['DATA'] # *self.data
				'MRSCALARTS': message, 'MrScalarTS # MrVectorTS operation not allowed.'
				'MRVECTORTS': begin
					;IDL's inner product
					temp   = total(left['DATA'] * *self.data, 2)
					result = MrScalarTS(temp, NAME=name, /NO_COPY)
				endcase
				'MRMATRIXTS': begin
					;Make sure the matrix is the correct size
					dims = size(left, /DIMENSIONS)
					if dims[1] ne 3 then message, 'MrMatrixTS dimensions must be NxMx3.'
					
					;Allocate memory
					type = size( (*self.data)[0] * right[[0]], /TYPE )
					temp = make_array( dims[0], dims[2], TYPE=type)
					
					;IDL's inner product
					for i = 0, dims[1] - 1 do begin
						temp[*,i] = (*self.data)[*,0] * right[*,0,i] + $
						            (*self.data)[*,1] * right[*,1,i] + $
						            (*self.data)[*,2] * right[*,2,i]
					endfor
						
					;Create matrix
					if dims[2] eq 3 $
						then result = MrVectorTS(temp, NAME=name, /NO_COPY) $
						else result = MrVariable(temp, NAME=name, /NO_COPY)
				endcase
				else: message, 'Unrecognized object class: "' + obj_class(left) + '".'
			endcase
		endelse

;-------------------------------------------------------
; MrVectorTS with Expression ///////////////////////////
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
	if size(result, /TNAME) eq 'OBJREF' $
		then return, result $
		else return, MrVariable(result, /NO_COPY, NAME=name)
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
function MrVectorTS::_OverloadPoundPound, left, right
	compile_opt idl2
	on_error, 2

	;Is SELF on the left or right?
	;   - Are both LEFT and RIGHT GDA data objects?
	side = self -> _LeftOrRight(left, right, ISMRVARIABLE=IsMrVariable)

;-------------------------------------------------------
; Two MrVectorTS Objects ///////////////////////////////
;-------------------------------------------------------
	;Both LEFT and RIGHT are MrVectorTS objects
	if IsMrVariable then begin
		;Name of result
		name = self.name + '##' + right.name
		
		;SELF is LEFT
		if side eq 'LEFT' then begin
			case obj_class(right) of
				'MRVARIABLE': result = *self.data ## right['DATA']
				'MRSCALARTS': message, 'MrVectorTS ## MrScalarTS operation not allowed.'
				'MRVECTORTS': begin
					;Allocate memory
					nPts = size(*self.data, /DIMENSIONS)
					nPts = nPts[0]
					type = size( (*self.data)[0] * right[[0]], /TYPE )
					temp = make_array( nPts, 3, 3, TYPE=type)
					
					;IDL's outer product
					temp[*,0,0] = (*self.data)[*,0] * right[*,0]
					temp[*,0,1] = (*self.data)[*,1] * right[*,0]
					temp[*,0,2] = (*self.data)[*,2] * right[*,0]
					temp[*,1,0] = (*self.data)[*,0] * right[*,1]
					temp[*,1,1] = (*self.data)[*,1] * right[*,1]
					temp[*,1,2] = (*self.data)[*,2] * right[*,1]
					temp[*,2,0] = (*self.data)[*,0] * right[*,2]
					temp[*,2,1] = (*self.data)[*,1] * right[*,2]
					temp[*,2,2] = (*self.data)[*,2] * right[*,2]
					
					;Create matrix
					result = MrMatrixTS(temp, NAME=name, /NO_COPY)
				endcase
				'MRMATRIXTS': begin
					;Make sure the matrix is the correct size
					dims = size(right, /DIMENSIONS)
					if dims[2] ne 3 then message, 'MrMatrixTS dimensions must be NxMx3.'
					
					;Allocate memory
					type = size( (*self.data)[0] * right[[0]], /TYPE )
					temp = make_array( dims[0], dims[1], TYPE=type)
					
					;IDL's inner product
					for i = 0, dims[1] - 1 do begin
						temp[*,i] = (*self.data)[*,0] * right[*,i,0] + $
						            (*self.data)[*,1] * right[*,i,1] + $
						            (*self.data)[*,2] * right[*,i,2]
					endfor
						
					;Create matrix
					if dims[2] eq 3 $
						then result = MrVectorTS(temp, NAME=name, /NO_COPY) $
						else result = MrVariable(temp, NAME=name, /NO_COPY)
				endcase
				else: message, 'Unrecognized object class: "' + obj_class(right) + '".'
			endcase
		
		;SELF is RIGHT
		endif else begin
			case obj_class(left) of
				'MRVARIABLE': result = left['DATA'] ## *self.data
				'MRSCALARTS': message, 'MrScalarTS ## MrVectorTS operation not allowed.'
				'MRVECTORTS': begin
					;Allocate memory
					nPts = size(*self.data, /DIMENSIONS)
					nPts = nPts[0]
					type = size( left[[0]] * (*self.data)[0], /TNAME )
					temp = make_array( nPts, 3, 3, TYPE=type)
					
					;IDL's inner product
					temp[*,0,0] = left[*,0] * (*self.data)[*,0]
					temp[*,0,1] = left[*,1] * (*self.data)[*,0]
					temp[*,0,2] = left[*,2] * (*self.data)[*,0]
					temp[*,1,0] = left[*,0] * (*self.data)[*,1]
					temp[*,1,1] = left[*,1] * (*self.data)[*,1]
					temp[*,1,2] = left[*,2] * (*self.data)[*,1]
					temp[*,2,0] = left[*,0] * (*self.data)[*,2]
					temp[*,2,1] = left[*,1] * (*self.data)[*,2]
					temp[*,2,2] = left[*,2] * (*self.data)[*,2]
					
					;Create matrix
					result = MrScalarTS(temp, NAME=name, /NO_COPY)
				endcase
				'MRMATRIXTS': begin
					;Make sure the matrix is the correct size
					dims = size(left, /DIMENSIONS)
					if dims[1] ne 3 then message, 'MrMatrixTS dimensions must be NxMx3.'
					
					;Allocate memory
					type = size( (*self.data)[0] * right[[0]], /TNAME )
					temp = make_array( dims[0], dims[2], TYPE=type)
					
					;IDL's inner product
					for i = 0, dims[1] - 1 do begin
						temp[*,i] = (*self.data)[*,0] * right[*,0,i] + $
						            (*self.data)[*,1] * right[*,1,i] + $
						            (*self.data)[*,2] * right[*,2,i]
					endfor
						
					;Create matrix
					if dims[2] eq 3 $
						then result = MrVectorTS(temp, NAME=name, /NO_COPY) $
						else result = MrVariable(temp, NAME=name, /NO_COPY)
				endcase
				else: message, 'Unrecognized object class: "' + obj_class(left) + '".'
			endcase
		endelse

;-------------------------------------------------------
; MrVectorTS with Expression ///////////////////////////
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
	if size(result, /TNAME) eq 'OBJREF' $
		then return, result $
		else return, MrVariable(result, /NO_COPY, NAME=name)
end


;+
;   The purpose of this method is to provide information when implied print is used.
;   is called.
;-
function MrVectorTS::_OverloadPrint
	compile_opt idl2
	on_error, 2

	;Print the array
	return, *self.data
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
function MrVectorTS::_OverloadSlash, left, right
	compile_opt idl2
	on_error, 2

	;Is SELF on the left or right?
	;   - Are both LEFT and RIGHT data objects?
	side = self -> _LeftOrRight(left, right, ISMRVARIABLE=IsMrVariable)

;-------------------------------------------------------
; Two MrVectorTS Objects //////////////////////////////////
;-------------------------------------------------------
	;Both LEFT and RIGHT are MrVectorTS objects
	if IsMrVariable then begin
		;Name of result
		name = self.name + '/' + right.name
		
		;SELF is LEFT
		if side eq 'LEFT' then begin
			case obj_class(right) of
				'MRVARIABLE': result = *self.data / right['DATA']
				'MRSCALARTS': begin
					type        = size( (*self.data)[0] / right[[0]], /TYPE )
					result      = make_array( size(*self.data, /DIMENSIONS), TYPE=type )
					result[*,0] = (*self.data)[*,0] / right['DATA']
					result[*,1] = (*self.data)[*,1] / right['DATA']
					result[*,2] = (*self.data)[*,2] / right['DATA']
				end
				'MRVECTORTS': result = *self.data / right['DATA']
				'MRMATRIXTS': message, 'MrVectorTS / MrMatrixTS not allowed.'
				else: message, 'Unrecognized object class: "' + obj_class(right) + '".'
			endcase
		
		;SELF is RIGHT
		endif else begin
			case obj_class(left) of
				'MRVARIABLE': result = left['DATA'] / *self.data
				'MRSCALARTS': begin
					type        = size( left[[0]] / (*self.data)[0], /TYPE )
					result      = make_array( size(*self.data, /DIMENSIONS), TYPE=type )
					result[*,0] = left['DATA'] / (*self.data)[*,0]
					result[*,1] = left['DATA'] / (*self.data)[*,1]
					result[*,2] = left['DATA'] / (*self.data)[*,2]
				endcase
				'MRVECTORTS': result = left['DATA'] / *self.data
				'MRMATRIXTS': message, 'MrMatrixTS / MrVectorTS not allowed.'
				else: message, 'Unrecognized object class: "' + obj_class(left) + '".'
			endcase
		endelse

;-------------------------------------------------------
; MrVectorTS with Expression //////////////////////////////
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
pro MrVectorTS::Append, data, $
BEFORE=before, $
NO_COPY=no_copy
	compile_opt idl2
	on_error, 2
	
	;Defaults
	before  = keyword_set(before)
	no_copy = keyword_set(no_copy)

;-------------------------------------------
; Implicit Array Empty /////////////////////
;-------------------------------------------
	if n_elements(*self.data) eq 0 then begin
		self -> SetData, data, NO_COPY=no_copy

;-------------------------------------------
; Append ///////////////////////////////////
;-------------------------------------------
	endif else begin
		sz = size(data)

	;-------------------------------------------
	; DATA Is Empty ////////////////////////////
	;-------------------------------------------
		if sz[0] eq 0 && sz[sz[0]+2] eq 0 then begin
			;Allow input data to be empty. In this case, do nothing.

	;-------------------------------------------
	; 3xN //////////////////////////////////////
	;-------------------------------------------
		;3xN (but not 3x3)
		;   - Must transpose the data
		endif else if sz[0] eq 2 && sz[1] eq 3 && sz[2] ne 3 then begin
			;Append before
			if before then begin
				if no_copy $
					then self -> SetData, [transpose(temporary(data)), *self.data] $
					else self -> SetData, [transpose(data), *self.data]
		
			;Append after
			endif else begin
				if no_copy $
					then self -> SetData, [*self.data, transpose(temporary(data))] $
					else self -> SetData, [*self.data, transpose(data)]
			endelse

	;-------------------------------------------
	; Nx3 //////////////////////////////////////
	;-------------------------------------------
		endif else if sz[0] eq 2 && sz[2] eq 3 then begin
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

	;-------------------------------------------
	; Incompatible Size ////////////////////////
	;-------------------------------------------
		endif else begin
			message, 'DATA must be Nx3 or 3xN.'
		endelse
	endelse
end


;+
;   Take the cross product of the MrVectorTS data with another vector.
;
; :Params:
;       VAR:                in, required, type=array/objref
;                           Another MrVectorTS object or a 3xN or Nx3 array or MrVariable. 
;
; :Keywords:
;       CACHE:              in, optional, type=boolean, default=0
;                           If set, the returned MrVectorTS will be cached.
;       NAME:               in, optional, type=string, default=(<name>)x(<name>) or (<name>)x(<size>)
;                           Name to be given to the resulting MrVectorTS object. 
;
; :Returns:
;       AXB:                out, required, type=objref
;                           A MrVectorTS object containing the cross product results.
;-
function MrVectorTS::Cross, var, $
CACHE=cache, $
NAME=name
	compile_opt idl2
	on_error, 2

;-------------------------------------------------------
; Object ///////////////////////////////////////////////
;-------------------------------------------------------
	if size(var, /TNAME) eq 'OBJREF' then begin
		;Default name
		if n_elements(name) eq 0 then name = '(' + self.name + ')x(' + var.name + ')'

	;-------------------------------------------------------
	; MrVectorTS /////////////////////////////////////////////
	;-------------------------------------------------------
		if obj_isa(var, 'MrVectorTS') then begin

			;Cross
			;   - Data will be transposed to Nx3 by MrVectorTS() below
			AxB = [ [self[*,1]*var[*,2] - self[*,2]*var[*,1]], $
			        [self[*,2]*var[*,0] - self[*,0]*var[*,2]], $
			        [self[*,0]*var[*,1] - self[*,1]*var[*,0]] ]

	;-------------------------------------------------------
	; MrVariable ///////////////////////////////////////////
	;-------------------------------------------------------
		endif else if obj_isa(var, 'MrVariable') then begin
			
			;Nx3 cross 3xN (treat 3x3 as Nx3
			sz = size(var)
			if (sz[0] eq 1 && sz[1] eq 3) || (sz[0] eq 2 && sz[1] eq 3 && sz[2] ne 3) then begin
				
				;Cross
				;   - Data will be transposed to Nx3 by MrVectorTS() below
				AxB = [ [self[*,1]*var[2,*] - self[*,2]*var[1,*]], $
				        [self[*,2]*var[0,*] - self[*,0]*var[2,*]], $
				        [self[*,0]*var[1,*] - self[*,1]*var[0,*]] ]
			
			;Nx3 cross Nx3
			endif else if (sz[0] eq 2 && sz[2] eq 3) then begin
				
				;Cross
				;   - Data will be transposed to Nx3 by MrVectorTS() below
				AxB = [ [self[*,1]*var[*,2] - self[*,2]*var[*,1]], $
				        [self[*,2]*var[*,0] - self[*,0]*var[*,2]], $
				        [self[*,0]*var[*,1] - self[*,1]*var[*,0]] ]
			
			;Incompatible dimensions
			endif else begin
				message, 'VAR must be 3xN or Nx3'
			endelse

	;-------------------------------------------------------
	; Unrecognized /////////////////////////////////////////
	;-------------------------------------------------------
		endif else begin
			message, 'VAR object class "' + obj_class(var) + '" not accepted.'
		endelse

;-------------------------------------------------------
; Array ////////////////////////////////////////////////
;-------------------------------------------------------
	endif else begin
		;Default name
		if n_elements(name) eq 0 then name = '(' + self.name + ')x[' + strjoin(string(size(var, /DIMENSIONS), FORMAT='(i0)'), ',') + ']'
		
		;Cross
		AxB = MrVector_Cross(*self.data, var)
	endelse
	
	;Save as a new variable
	AxB = MrVectorTS(AxB, /NO_COPY, NAME=name, CACHE=cache)
	return, AxB
end


;+
;   Take the dot product of the MrVectorTS data with another vector.
;
; :Params:
;       VAR:                in, required, type=array/objref
;                           Another MrVectorTS object or a 3xN or Nx3 array or MrVariable.
;
; :Keywords:
;       CACHE:              in, optional, type=boolean, default=0
;                           If set, the returned MrScalar will be cached.
;       NAME:               in, optional, type=string, default=(<name>).(<name>) or (<name>).(<size>)
;                           Name to be given to the resulting MrScalar object. 
;
; :Returns:
;       ADOTB:              out, required, type=objref
;                           A MrScalar object containing the dot product results.
;-
function MrVectorTS::Dot, var, $
CACHE=cache, $
NAME=name
	compile_opt idl2
	on_error, 2

;-------------------------------------------------------
; Object ///////////////////////////////////////////////
;-------------------------------------------------------
	if size(var, /TNAME) eq 'OBJREF' then begin
		;Default name
		if n_elements(name) eq 0 then name = '(' + self.name + ').(' + var.name + ')'

	;-------------------------------------------------------
	; MrVectorTS /////////////////////////////////////////////
	;-------------------------------------------------------
		if obj_isa(var, 'MrVectorTS') then begin

			;Dot
			AdotB = self[*,0]*var[*,0] + self[*,1]*var[*,1] + self[*,2]*var[*,2]

	;-------------------------------------------------------
	; MrVariable ///////////////////////////////////////////
	;-------------------------------------------------------
		endif else if obj_isa(var, 'MrVariable') then begin
			
			;Nx3 cross 3xN
			sz = size(var)
			if (sz[0] eq 1 && sz[1] eq 3) || (sz[0] eq 2 && sz[1] eq 3) then begin
				
				;Dot
				AdotB = self[*,0]*var[0,*] + self[*,1]*var[1,*] + self[*,2]*var[2,*]
			
			;Nx3 cross Nx3
			endif else if (sz[0] eq 2 && sz[2] eq 3) then begin
				
				;Dot
				AdotB = self[*,0]*var[*,0] + self[*,1]*var[*,1] + self[*,2]*var[*,2]
			
			;Incompatible dimensions
			endif else begin
				message, 'VAR must be 3xN or Nx3'
			endelse

	;-------------------------------------------------------
	; Unrecognized /////////////////////////////////////////
	;-------------------------------------------------------
		endif else begin
			message, 'VAR object class "' + obj_class(var) + '" not accepted.'
		endelse

;-------------------------------------------------------
; Array ////////////////////////////////////////////////
;-------------------------------------------------------
	endif else begin
		;Default name
		if n_elements(name) eq 0 then name = '(' + self.name + ').[' + strjoin(string(size(var, /DIMENSIONS), FORMAT='(i0)'), ',') + ']'
		
		;Cross
		AdotB = MrVectorTS_Dot(*self.data, var)
	endelse
	
	;Save as a new variable
	AdotB = MrScalarTS(AdotB, /NO_COPY, NAME=name, CACHE=cache)
	return, AdotB
end


;+
;   Perform interpolation on regularly or irregularly gridded vectors.
;
;   All variable attributes are copied to the resultant vector, except in the
;   case that `XOUT` is not a MrVariable object. In this case, the 'DEPEND_0'
;   variable attribute is removed from the output vector.
;
;   Calling Sequence:
;       varOut = MrVar1 -> Interpol(MrVar2)
;       varOut = MrVar1 -> Interpol(X, Xout)
;
; :Params:
;       X:              in, required, type=numeric array
;                       Current abcissa values or a MrVectorTS object.
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
;                       A MrVectorTS object containing the interpolated data.
;-
function MrVectorTS::Interpol, X, Xout, $
CACHE=cache, $
NAME=name, $
LSQUADRATIC=lsquadratic, $
NAN=nan, $
QUADRATIC=quadratic, $
SPLINE=spline
	compile_opt idl2
	on_error, 2

;-------------------------------------------------------
; MrVectorTS Object //////////////////////////////////////
;-------------------------------------------------------
	if IsA(X, 'MrVariable') then begin
		;Get the current abscissa values
		xx_var = MrVar_Get(self['DEPEND_0'])
		if obj_isa(xx_var, 'MrTimeVar') $
			then xx = xx_var -> GetData('SSM') $
			else xx = xx_var -> GetData()
		
		;Get the new abscissa values
		dep0_var = MrVar_Get(X['DEPEND_0'])
		if obj_isa(dep0_var, 'MrTimeVar') $
			then Xout = dep0_var -> GetData('SSM') $
			else Xout = dep0_var -> GetData()

		;Interpolate
		vx = Interpol( (*self.data)[*,0], xx, Xout, $
		               LSQUADRATIC = lsquadratic, $
		               NAN         = nan, $
		               QUADRATIC   = quadratic, $
		               SPLINE      = spline )
		vy = Interpol( (*self.data)[*,1], xx, Xout, $
		               LSQUADRATIC = lsquadratic, $
		               NAN         = nan, $
		               QUADRATIC   = quadratic, $
		               SPLINE      = spline )
		vz = Interpol( (*self.data)[*,2], temporary(xx), temporary(Xout), $
		               LSQUADRATIC = lsquadratic, $
		               NAN         = nan, $
		               QUADRATIC   = quadratic, $
		               SPLINE      = spline )

		;Create output variable
		;   - Copy this variable
		;   - Set its data to the interpolated values
		;   - Set its DEPEND_0 attribute to the new values
		varOut = self -> Copy(name, CACHE=cache)
		varOut -> SetData, [[reform(temporary(vx))], [reform(temporary(vy))], [reform(temporary(vz))]]
		varOut -> SetAttrValue, 'DEPEND_0', dep0_var.name

;-------------------------------------------------------
; Normal Arrays ////////////////////////////////////////
;-------------------------------------------------------
	endif else begin
		;Interpolate
		vx = Interpol( (*self.data)[*,0], X, Xout, $
		               LSQUADRATIC = lsquadratic, $
		               NAN         = nan, $
		               QUADRATIC   = quadratic, $
		               SPLINE      = spline)
		vy = Interpol( (*self.data)[*,1], X, Xout, $
		               LSQUADRATIC = lsquadratic, $
		               NAN         = nan, $
		               QUADRATIC   = quadratic, $
		               SPLINE      = spline)
		vz = Interpol( (*self.data)[*,2], X, Xout, $
		               LSQUADRATIC = lsquadratic, $
		               NAN         = nan, $
		               QUADRATIC   = quadratic, $
		               SPLINE      = spline)
		
		;Create a new variable with the same attributes
		varOut = self -> Copy(name, CACHE=cache)
		varOut -> SetData, [[temporary(vx)], [temporary(vy)], [temporary(vz)]]
		
		;Delete the DEPEND_0 attribute, if there is one
		;   - TODO: Also create a DEPEND_0 variable ?????
		if varOut -> HasAttr('DEPEND_0') then varOut -> RemoveAttr, 'DEPEND_0'
	endelse
	
	;Return the new variable
	return, varOut
end


;+
;   Compute the magnitude of the implicit vector.
;
; :Keywords:
;       CACHE:              in, optional, type=boolean, default=0
;                           If set, the returned MrScalar will be cached.
;       NAME:               in, optional, type=string, default=|<name>|
;                           Name to be given to the resulting MrScalar object. 
;
; :Returns:
;       VMAG:               out, required, type=objref
;                           A MrScalar object containing the magnitude of the vector.
;-
function MrVectorTS::Magnitude, $
CACHE=cache, $
NAME=name
	compile_opt idl2
	on_error, 2
	
	;Default name
	if n_elements(name) eq 0 then name = '|' + self.name + '|'

	;Compute the magnitude
	Vmag = sqrt(total(*self.data^2, 2))
	
	;Save as a new variable
	Vmag = MrScalar(Vmag, /NO_COPY, NAME=name, CACHE=cache)
	return, Vmag
end


;+
;   Normalize the implicit vector. If the magnitude is zero, then the result will
;   be NaN. (x/0.0 should result in infinity, but a normalized vector can have values
;   only between 0.0 and 1.0, so NaN is returned as a result.)
;
; :Keywords:
;       CACHE:              in, optional, type=boolean, default=0
;                           If set, the returned MrScalar will be cached.
;       NAME:               in, optional, type=string, default=<name>_hat
;                           Name to be given to the resulting MrScalar object. 
;
; :Returns:
;       VMAG:               out, required, type=objref
;                           A MrScalar object containing the magnitude of the vector.
;-
function MrVectorTS::Normalize, $
CACHE=cache, $
NAME=name
	compile_opt idl2
	on_error, 2
	
	;Default name
	if n_elements(name) eq 0 then name = self.name + '_hat'

	;Compute the magnitude
	mag  = sqrt(total(*self.data^2, 2))
	
	;Allocate memory
	nPts = n_elements(mag)
	hat  = make_array(nPts, 3, TYPE=size(mag[0]*self[[0]], /TYPE))
	
	;Avoid math errors -- divide by zero
	iBad = where(mag eq 0.0, nBad, COMPLEMENT=iGood, NCOMPLEMENT=nGood)
	if nBad  gt 0 then begin
		MrPrintF, 'LogWarn', nBad, FORMAT='(%"%i values of zero magnitude found. Setting to NaN.")'
		hat[iBad,*]  = !values.f_nan
	endif
	if nGood gt 0 then hat[iGood,*] = (*self.data)[iGood,*] / rebin( mag[iGood], nGood, 3)
	
	;Save as a new variable
	v_hat = MrVectorTS(hat, NAME=name, CACHE=cache, /NO_COPY)
	self -> CopyAttrTo, v_hat
	return, v_hat
end


;+
;   Set the array.
;
; :Keywords:
;       DATA:              in, required, type=3xN or Nx3 array
;                           Array of values to be stored, or a MrVectorTS object whose
;                               array is to be copied. 3x3 arrays are treated as Nx3.
;
; :Keywords:
;       NO_COPY:            in, optional, type=boolean, default=0
;                           If set `DATA` will be copied directly into the object
;                               and will be left undefined (a MrVectorTS object will not
;                               be destroyed, but its array will be empty).
;-
pro MrVectorTS::SetData, data, $
NO_COPY=no_copy
	compile_opt idl2
	on_error, 2

	no_copy = keyword_set(no_copy)

;-----------------------------------------------------
; MrArray Object \\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\
;-----------------------------------------------------
	if size(data, /TNAME) eq 'OBJREF' then begin
		;MrVectorTS
		if obj_isa(data, 'MrVectorTS') then begin
			*self.data = data -> GetData()
		
		;MrVariable
		endif else if obj_isa(data, 'MrVariable') then begin
			;Check sizes
			sz = size(data)
			
			;3xN (But not 3x3)
			if (sz[0] eq 1 && sz[1] eq 3) || (sz[0] eq 2 && sz[1] eq 3 && sz[2] ne 3) then begin 
				*self.data = transpose( data -> GetData() )
			endif else if sz[0] eq 2 && sz[2] eq 3 then begin
				*self.data = data -> GetData()
			endif else begin
				message, 'DATA must be 3xN or Nx3.'
			endelse
		
		;Other
		endif else begin
			message, 'Only "MrVectorTS" or "MrArray" objects can be given.'
		endelse

;-----------------------------------------------------
; Normal Array \\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\
;-----------------------------------------------------
	endif else begin
		;Check sizes
		sz = size(data)

		;Change 3xN to Nx3 (Do not transpose 3x3)
		if (sz[0] eq 1 && sz[1] eq 3) || (sz[0] eq 2 && sz[1] eq 3 && sz[2] ne 3) then begin
			if no_copy $
				then *self.data = transpose(temporary(data)) $
				else *self.data = transpose(data)
		
		;Save Nx3
		endif else if sz[0] eq 2 && sz[2] eq 3 then begin
			if no_copy $
				then *self.data = temporary(data) $
				else *self.data = data
		endif else begin
			message, 'DATA must be 3xN or Nx3.'
		endelse
	endelse
end


;+
;   Split the time-series vector into three time-series scalars, on per component.
;   All attributes from the vector will be copied to the scalars.
;
; :Params:
;       OX:                 out, optional, type=objref (MrScalarTS)
;                           A MrScalarTS variable containing the X-component.
;       OY:                 out, optional, type=objref (MrScalarTS)
;                           A MrScalarTS variable containing the Y-component.
;       OZ:                 out, optional, type=objref (MrScalarTS)
;                           A MrScalarTS variable containing the Z-component.
;
; :Keywords:
;       CACHE:              in, optional, type=boolean, default=0
;                           If set, `OX`, `OY`, and `OZ` will be cached.
;       NAMES:              in, optional, type=strarr(3)
;                           New names for each component. The default is to append
;                               {'_x' | '_y' | '_z'} to the original name.
;-
pro MrVectorTS::Split, oX, oY, oZ, $
CACHE=cache, $
NAMES=names
	compile_opt idl2
	on_error, 2

	;Default names
	if n_elements(names) eq 0 then names = self.name + ['_x', '_y', '_z']

	;Create the scalar variables
	oX = MrScalarTS(self[*,0], NAME=names[0])
	oY = MrScalarTS(self[*,1], NAME=names[1])
	oZ = MrScalarTS(self[*,2], NAME=names[2])
	
	;Copy Attributes
	self -> CopyAttrTo, oX
	self -> CopyAttrTo, oY
	self -> CopyAttrTo, oZ
	
	;Cache the variables
	if keyword_set(cache) then begin
		oX -> Cache
		oY -> Cache
		oZ -> Cache
	endif
end


;+
;   The class definition statement.
;
; :Params:
;       CLASS:          out, optional, type=structure
;-
pro MrVectorTS__DEFINE
	compile_opt idl2
	
	class = { MrVectorTS, $
	          inherits MrVariable $
	        }
end