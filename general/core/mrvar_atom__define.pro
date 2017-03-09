; docformat = 'rst'
;
; NAME:
;   MrVar_Atom__Define
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
;       method for the object on the left side first. So, for two MrVar_Atom objects,
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
;       The ::Cache method will cache the MrVar_Atom object. When the object is destroyed,
;       it will automatically be removed from the cache.
;
; :Categories:
;   MrVar_Atom, Graphics
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
;       2016/10/24  -   Added the InnerProduct and OuterProduct methods. - MRA
;-
;*****************************************************************************************
;+
;   The initialization method.
;
; :Examples:
;   Create a MrVar_Atom object using a pre-existing array::
;       theArray = findgen(24, 36)
;       myArray  = MrVar_Atom(theArray)
;       help, myArray
;           MYARRAY     OBJREF      <ObjHeapVar666(MrVar_Atom)>
;             ARRAY       FLOAT     = Array[24, 36]
;
;   Initialize a MrVar_Atom object via Make_Array::
;       myArray = MrVar_Atom(24, 36, TYPE='ULong')
;       help, myArray
;           MYARRAY     OBJREF      <ObjHeapVar668(MrVar_Atom)>
;             ARRAY       ULONG     = Array[24, 36]
;
;   Initialize a MrVar_Atom object via RandomU::
;       myArray = MrVar_Atom(24, 36, SEED=3)
;       print, myarray[0:3]
;           0.897916     0.558249     0.766930     0.589101
;
;   Initialize a MrVar_Atom with a normal, gaussian distribution::
;       myArray = MrVar_Atom(24, 36, SEED=3, /NORMAL)
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
function MrVar_Atom::INIT, data, D2, D3, D4, D5, D6, D7, D8, $
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
	if n_elements(name) eq 0 then name = 'MrVar_Atom'

	;Allocate heap to pointers.
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
pro MrVar_Atom::CLEANUP
	compile_opt idl2
	on_error, 2
	
	;Common block
	@mrvar_common
	
	;Remove from the variable container
	if n_elements(MrVarCache) gt 0 then begin
		if MrVarCache -> IsContained(self) then MrVarCache -> Remove, self
	endif

	;Free pointers
	ptr_free, self.data
	ptr_free, self._append_unReform
	ptr_free, self._append_unTrans
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
;       myArray  = MrVar_Atom(12, 3, 6, 4, TYPE='FLOAT')
;       myArray -> Append, fltarr(12, 3, 4, 4), 3
;       help, myArray
;            MYARRAY     OBJREF      <ObjHeapVar5(MrVar_Atom)>
;             ARRAY       FLOAT     = Array[12, 3, 10, 4]
;
;  Multiple appends to the third dimension::
;       myArray  = MrVar_Atom(12, 3, 6, 4, TYPE='FLOAT')
;       myArray -> Append, fltarr(12, 3, 4, 4), 3
;       myArray -> Append, fltarr(12, 3, 4, 4), 3
;       myArray -> Append, fltarr(12, 3, 4, 4), 3
;       help, myArray
;           MYARRAY     OBJREF      <ObjHeapVar5(MrVar_Atom)>
;             ARRAY       FLOAT     = Array[12, 3, 18, 4]
;
;   Multiple appends to the beginning of an array::
;       myArray -> MrVar_Atom(lonarr(2, 2))
;       myArray -> Append, 1, /BEFORE
;       myArray -> Append, lonarr(2,2) + 1, 1
;       myArray -> Append, lonarr(2,2) + 2, 1
;       myArray -> Append, lonarr(2,2) + 3, 1
;       help, myArray
;           MYARRAY     OBJREF      <ObjHeapVar17(MrVar_Atom)>
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
pro MrVar_Atom::Append, data, dimension, $
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
pro MrVar_Atom::Append_Finalize
	compile_opt idl2, hidden

	catch, the_error
	if the_error ne 0 then begin
		catch, /CANCEL
		self._append_cont    = 0B
		self._append_before  = 0B
		self._append_no_copy = 0B
		MrPrintF, 'LogErr'
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
;       myArray  = MrVar_Atom(12, 3, 6, 4, TYPE='FLOAT')
;       myArray -> Append_Multi, fltarr(12, 3, 4, 4), 3
;       myArray -> Append_Multi, fltarr(12, 3, 4, 4)
;       myArray -> Append_Multi, fltarr(12, 3, 4, 4)
;       myArray -> Append_Multi, /FINISH
;       help, myArray
;           MYARRAY     OBJREF      <ObjHeapVar5(MrVar_Atom)>
;             ARRAY       FLOAT     = Array[12, 3, 18, 4]
;
;   Multiple appends to the beginning of an array::
;       myArray -> MrVar_Atom(lonarr(2, 2))
;       myArray -> Append_Multi, lonarr(2,2) + 1, 1
;       myArray -> Append_Multi, lonarr(2,2) + 2
;       myArray -> Append_Multi, lonarr(2,2) + 3
;       myArray -> Append_Multi, /FINISH
;       help, myArray
;           MYARRAY     OBJREF      <ObjHeapVar17(MrVar_Atom)>
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
pro MrVar_Atom::Append_Multi, data, dimension, $
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
		MrPrintF, 'LogErr'
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
function MrVar_Atom::Array_Equal, value, $
INDEX=index
	compile_opt idl2
	on_error, 2
	
	;Determine array equality
	if size(value, /TNAME) eq 'OBJREF' && obj_isa(value, 'MrVar_Atom') $
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
function MrVar_Atom::Array_Indices, index
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
pro MrVar_Atom::Cache, $
NO_CLOBBER=no_clobber, $
NAME_OUT=name_out
	compile_opt idl2
	on_error, 2

	;Setup caching store
	@mrvar_common
	
	;Create a cache in which to store MrVar_Atoms
	if ~obj_valid(MrVarCache) then MrVarCache = MrVar_Atom_Cache()
	
	;Add the array to the end of the container
	MrVarCache -> Add, self, NO_CLOBBER=no_clobber, NAME_OUT=name_out
end


;+
;   The prupose of this method is to erase the data from the array. Alternatively, try
;       myArray.array = !Null
;       myArray[*] = !Null
;-
pro MrVar_Atom::Clear
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
function MrVar_Atom::Copy, name, $
CACHE=cache
	compile_opt idl2
	on_error, 2

	;Defaults
	cache = keyword_set(cache)
	if n_elements(name) eq 0 then name = self.name + '_copy'
	
	;Copy the data into a new object
	;   - Use Obj_Class() so subclasses can inherit the method.
	theCopy = obj_new(obj_class(self), *self.data, $
	                  NAME = name)

	;Copy the variable attributes
	self -> CopyAttrTo, theCopy
	
	;Cache the variable
	if cache then theCopy -> Cache, /NO_CLOBBER
	return, theCopy
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
pro MrVar_Atom::Extend, nElements, dimension, $
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
;   Get class properties.
;
; :Examples:
;   Get the array::
;       myArray  = MrVar_Atom(11, 91, 6)
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
function MrVar_Atom::GetData, $
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
function MrVar_Atom::GetName
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
pro MrVar_Atom::GetProperty, $
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
;   Print information about the variable.
;-
pro MrVar_Atom::Help
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
;   Compute the inner product of two tensors of rank > 1.
;   This method generalizes the # operator for tensors of rank > 2.
;
; :Params:
;       B:          in, required, type=numeric or MrVar_Atom objref
;                   A tensor with > 1 dimension.
;
; :Returns:
;       RESULT:     out, required, type=MrVar_Atom objref
;                   Result of the inner product.
;-
function MrVar_Atom::InnerProduct, B, $
NAME=name
	compile_opt strictarr
	on_error, 2
	
	;Extract the data
	if isa(B, 'MrVar_Atom') $
		then _B = B['DATA'] $
		else _B = B
	
	;Default name
	if n_elements(name) eq 0 then name = 'InnerProduct(' + self.name + ',B)'

;-------------------------------------------------------
; Dimensions ///////////////////////////////////////////
;-------------------------------------------------------
	
	;Dimensionality of inputs
	dimsA  = size(self, /DIMENSIONS)
	dimsB  = size(B, /DIMENSIONS)
	nDimsA = size(self, /N_DIMENSIONS)
	nDimsB = size(B, /N_DIMENSIONS)
	nDims  = nDimsA < nDimsB
	
	;Output dimensions
	if nDims eq 1 $
		then outDims = nDimsA < nDimsB ? dimsA : dimsB $
		else outDims = nDimsA < nDimsB ? dimsA[0:-2] : dimsB[1:*]

;-------------------------------------------------------
; Reform Inputs ////////////////////////////////////////
;-------------------------------------------------------
	
	;A is currently D1xD2xD3xD4x ... xDN
	;   - Turn A into DixDN matrix
	if nDimsA eq 1 $
		then _A = A['DATA'] $
		else _A = reform( A['DATA'], [ product(dimsA[0:-2]), dimsA[-1] ] )
	
	;B is currently D1xD2xD3xD4x ... xDN
	;   - Turn B into D1xDi matrix
	if nDimsB ge 2 $
		then _B = reform( _B, [ dimsB[0], product(dimsB[1:*]) ] )

;-------------------------------------------------------
; Inner Product ////////////////////////////////////////
;-------------------------------------------------------
	
	temp = temporary(_A) # temporary(_B)

;-------------------------------------------------------
; Finish Up ////////////////////////////////////////////
;-------------------------------------------------------

	;Reform back to the original shape, minus the contracted dimension.
	temp = reform(temp, dimsOut, /OVERWRITE)
	
	;Create time series variable
	result = MrVar_Atom( temp, NAME=name, /NO_COPY )
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
;       X:              in, required, type=numeric array
;                       Current abcissa values or a MrVar_Atom object.
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
;                       A MrVar_Atom object containing the interpolated data.
;-
function MrVar_Atom::Interpol, X, Xout, $
CACHE=cache, $
NAME=name, $
LSQUADRATIC=lsquadratic, $
NAN=nan, $
QUADRATIC=quadratic, $
SPLINE=spline
	compile_opt idl2
	on_error, 2

;-------------------------------------------------------
; MrVar_Atom Object ////////////////////////////////////
;-------------------------------------------------------
	if IsA(Xout, 'MrVar_Atom') then begin
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
function MrVar_Atom::IsA, type_name, $
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
pro MrVar_Atom::Make_Array, D1, D2, D3, D4, D5, D6, D7, D8, $
TYPE=type, $
_REF_EXTRA=extra
	compile_opt idl2
	on_error, 2

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
function MrVar_Atom::Max, subscript_max, $
ABSOLUTE=absolute, $
DIMENSION=dimension, $
MIN=minimum, $
NAN=nan, $
SUBSCRIPT_MIN=iMin
	compile_opt idl2
	on_error, 2

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
function MrVar_Atom::Min, iMin, $
ABSOLUTE=absolute, $
DIMENSION=dimension, $
MAX=maximum, $
NAN=nan, $
SUBSCRIPT_MAX=iMax
	compile_opt idl2
	on_error, 2

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
function MrVar_Atom::New, array, $
NAME=name, $
NO_COPY=no_copy
	compile_opt idl2
	on_error, 2

	;Return a copy of the array
	;   - Use Obj_Class() so that subclasses can inherit the method.
	return, obj_new(obj_class(self), array, NAME=name, NO_COPY=no_copy)
end 


;+
;   Compute the outer product of two tensors of rank > 1.
;   This method generalizes the ## operator for tensors of rank > 2.
;
; :Params:
;       B:          in, required, type=numeric or MrVar_Atom objref
;                   A tensor with > 1 dimension.
;
; :Returns:
;       RESULT:     out, required, type=MrVar_Atom objref
;                   Result of the inner product.
;-
function MrVar_Atom::OuterProduct, B, $
NAME=name
	compile_opt strictarr
	on_error, 2
	
	;Extract the data
	if isa(B, 'MrVar_Atom') $
		then _B = B['DATA'] $
		else _B = B
	
	;Default name
	if n_elements(name) eq 0 then name = 'OuterProduct(' + self.name + ',B)'

;-------------------------------------------------------
; Dimensions ///////////////////////////////////////////
;-------------------------------------------------------
	
	;Dimensionality of inputs
	dimsA  = size(self, /DIMENSIONS)
	dimsB  = size(B, /DIMENSIONS)
	nDimsA = size(self, /N_DIMENSIONS)
	nDimsB = size(B, /N_DIMENSIONS)
	nDims  = nDimsA < nDimsB
	
	;Output dimensions
	if nDims eq 1 $
		then outDims = nDimsA < nDimsB ? dimsA : dimsB $
		else outDims = nDimsA < nDimsB ? dimsA[1:*] : dimsB[0:-2]

;-------------------------------------------------------
; Reform Inputs ////////////////////////////////////////
;-------------------------------------------------------
	
	;A is currently D1xD2xD3xD4x ... xDN
	;   - Turn A into DixDN matrix
	if nDimsA eq 1 $
		then _A = A['DATA'] $
		else _A = reform( A['DATA'], [ dimsA[0], product(dimsA[1:*]) ] )
	
	;B is currently D1xD2xD3xD4x ... xDN
	;   - Turn B into D1xDi matrix
	if nDimsB ge 2 $
		then _B = reform( _B, [ product(dimsB[0:-2]), dimsB[-1] ] )

;-------------------------------------------------------
; Inner Product ////////////////////////////////////////
;-------------------------------------------------------
	
	temp = temporary(_A) ## temporary(_B)

;-------------------------------------------------------
; Finish Up ////////////////////////////////////////////
;-------------------------------------------------------

	;Reform back to the original shape, minus the contracted dimension.
	temp = reform(temp, dimsOut, /OVERWRITE)
	
	;Create time series variable
	result = MrVar_Atom( temp, NAME=name, /NO_COPY )
	return, result
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
pro MrVar_Atom::RandomN, seed, D1, D2, D3, D4, D5, D6, D7, D8, $
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
pro MrVar_Atom::RandomU, seed, D1, D2, D3, D4, D5, D6, D7, D8, $
_REF_EXTRA=extra
	compile_opt idl2
	on_error, 2

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
;       OVAR:       out, required, type=objref (MrVar_Atom)
;                   A MrVar_Atom with the reformed data name "*_rebin".
;-
function MrVar_Atom::Rebin, d1, d2, d3, d4, d5, d6, d7, d8, $
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
	oVar = MrVar_Atom(result, NAME=self.name + '_rebin', /NO_COPY)
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
;       OVAR:       out, required, type=objref (MrVar_Atom)
;                   A MrVar_Atom with the reformed data name "*_reform".
;-
function MrVar_Atom::Reform, d1, d2, d3, d4, d5, d6, d7, d8
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
	oVar = MrVar_Atom(result, NAME=self.name + '_reform', /NO_COPY)
	self -> CopyAttrTo, oVar
	
	return, oVar
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
pro MrVar_Atom::SetAttrValue, attrName, attrValue, $
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
pro MrVar_Atom::SetAttributeValue, attrName, attrValue, $
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
;       myArray  = MrVar_Atom(11, 91, 6, TYPE='FLOAT')
;       myArray -> SetArray, indgen(23)
;       help, myArray
;           MYARRAY     OBJREF      <ObjHeapVar680(MrVar_Atom)>
;             ARRAY       INT       = Array[23]
;
; :Keywords:
;       DATA:               in, required, type=array
;                           Array of values to be stored, or a MrVar_Atom object whose
;                               array is to be copied. 
;
; :Keywords:
;       NO_COPY:            in, optional, type=boolean, default=0
;                           If set `DATA` will be copied directly into the object
;                               and will be left undefined (a MrVar_Atom object will not
;                               be destroyed, but its array will be empty).
;-
pro MrVar_Atom::SetData, data, $
NO_COPY=no_copy
	compile_opt idl2
	on_error, 2
	
	;Defaults
	no_copy = keyword_set(no_copy)

	;Take from a MrVar_Atom object?
	if IsA(data, 'OBJREF') then begin
		if obj_isa(data, 'MrVar_Atom') $
			then *self.data = data -> GetData(NO_COPY=no_copy) $
			else message, 'Only "MrVar_Atom" objects can be given.'
	
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
pro MrVar_Atom::SetName, name, $
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
pro MrVar_Atom::SetProperty
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
pro MrVar_Atom::SetType, type
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
;       myArray = MrVar_Atom(findgen(10))
;       myArray -> Shift, 5
;       print, myArray
;           5.000  6.000  7.000  8.000  9.000  0.000  1.000  2.000  3.000  4.000
;
;  Create a 2D array and shift one dimension to the right and one to the left.
;       myArray = MrVar_Atom(indgen(10,5))
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
pro MrVar_Atom::Shift, s1, s2, s3, s4, s5, s6, s7, s8
	compile_opt idl2
	on_error, 2

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
pro MrVar_Atom::Sort, $
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
;       myArray = MrVar_Atom()
;       myArray -> ToArray, struct, DIMENSION=3
;       help, myArray
;           MYARRAY     OBJREF      <ObjHeapVar682(MrVar_Atom)>
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
pro MrVar_Atom::ToArray, data, $
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
function MrVar_Atom::Transpose, P
	compile_opt idl2
	on_error, 2

	;Get the array and transpose it.
	case n_params() of
		0: result = transpose(*self.data)
		1: result = transpose(*self.data, P)
		else: message, 'Usage: myArray -> Transpose([P])'
	endcase
	
	;Create a variable object
	result = MrVar_Atom(result, /NO_COPY, NAME='Transpose(' + self.name + ')')
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
function MrVar_Atom::TypeName2Code, type
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
function MrVar_Atom::Uniq, $
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
function MrVar_Atom::Value_Locate, value, $
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
function MrVar_Atom::Where, value, $
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
pro MrVar_Atom__DEFINE
	compile_opt idl2
	
	class = { MrVar_Atom, $
	          inherits IDL_Object, $
	          data:             Ptr_New(), $
	          name:             '', $
	          
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