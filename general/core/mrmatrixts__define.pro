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
;       If the operation is between two time-series objects, the following
;       applies:
;
;           MrMatrixTS [op] MrScalarTS = MrMatrixTS
;           MrMatrixTS [op] MrVectorTS = Error
;           MrMatrixTS [op] MrMatrixTS = MrMatrixTS
;
;   CACHING
;       The ::Cache method will cache the MrMatrixTS object. All variables in the
;       cache are forced to have unique names. When the object is destroyed, it
;       will automatically be removed from the cache.
;
; :Categories:
;   MrVariable, MrTimeSeries, MrMatrixTS
;
; :See Also:
;   MrVariable__Define.pro
;   MrTimeSeries__Define.pro
;   MrScalarTS__Define.pro
;   MrVectorTS__Define.pro
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
;       2016/10/24  -   Major rewrite to inherit MrTimeSeries object. - MRA
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
;       NAME:           in, optional, type=integer
;                       Name to be given to the variable object.
;       _REF_EXTRA:     in, optional, type=any
;                       Any keyword accepted by MrTimeSeries::Init is accepted here via
;                           keyword inheritance.
;-
function MrMatrixTS::INIT, time, data, $
NAME=name, $
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
	if n_elements(name) eq 0 then name = 'MrMatrixTS'
	
	;Initialize superclass
	success = self -> MrTimeSeries::Init( time, data, $
	                                      NAME          = name, $
	                                      _STRICT_EXTRA = extra )
	if ~success then message, 'Unable to initialize superclass.'

	return, 1
end


;+
;   Clean up after the object is destroyed
;-
pro MrMatrixTS::CLEANUP
	compile_opt idl2
	on_error, 2
	
	;Superclass
	self -> MrTimeSeries::Cleanup
end


;+
;   Multiply two expressions together.
;
;   NOTE:
;     If one of LEFT or RIGHT is an expression or a MrVariable object,
;     normal IDL operations will take effect. This means the output will
;     be the size of the array with the smallest dimensions.
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

;-------------------------------------------------------
; Superclass ///////////////////////////////////////////
;-------------------------------------------------------
	if ~isa(left, 'MrTimeSeries') || ~isa(right, 'MrTimeSeries') then begin
		result = self -> MrTimeSeries::_OverloadAsterisk(left, right)

;-------------------------------------------------------
; MrTimeSeries with MrTimeSeries ///////////////////////
;-------------------------------------------------------
	endif else begin

		;Is SELF on the left or right?
		;   - Are both LEFT and RIGHT GDA data objects?
		side = self -> _LeftOrRight(left, right, ISMRVARIABLE=IsMrVariable)

		;New name
		name = 'Multiply(' + left.name + ',' + right.name + ')'

	;-------------------------------------------------------
	; SELF is LEFT /////////////////////////////////////////
	;-------------------------------------------------------
		if side eq 'LEFT' then begin
			case 1 of
				obj_isa(right, 'MRSCALARTS'): begin
					;Output dimensions
					nPts     = right -> GetNPts()
					dims     = size(*self.data, /DIMENSIONS)
					outDims  = [nPts, dims[1:2]]
					outClass = 'MrMatrixTS'
					
					;Operate
					temp = *self.data * rebin(right['DATA'], outDims)
					
					;NOT VECTORIZED (saves memory)
					;Allocate memory
;					type   = size( (*self.data)[0] * right[[0]], /TYPE )
;					dims   = size(*self.data, /DIMENSIONS)
;					result = make_array( dims, TYPE=type )
					
					;Multiply
;					for i = 0, dims[1]-1 do begin
;					for j = 0, dims[2]-1 do begin
;						result[*,i,j] = (*self.data)[*,i,j] * right['DATA']
;					endfor
;					endfor
				endcase
			
				obj_isa(right, 'MRVECTORTS'): message, 'Operation not valid: MrMatrixTS * MrVectorTS.'

				obj_isa(right, 'MRMATRIXTS'): begin
					outClass = 'MrMatrixTS'
					temp     = *self.data * right['DATA']
				endcase
				
				else: message, 'Unrecognized object class: "' + obj_class(right) + '".'
			endcase

	;-------------------------------------------------------
	; SELF is RIGHT ////////////////////////////////////////
	;-------------------------------------------------------
		endif else begin
			case 1 of
				obj_isa(left, 'MRSCALARTS'): begin
					;Output dimensions
					nPts     = left -> GetNPts()
					dims     = size(*self.data, /DIMENSIONS)
					outDims  = [nPts, dims[1:2]]
					outClass = 'MrMatrixTS'
					
					;Operate
					temp = rebin(left['DATA'], outDims) * *self.data
				endcase
			
				obj_isa(left, 'MRVECTORTS'): message, 'Operation not valid: MrVectorTS * MrMatrixTS.'

				obj_isa(left, 'MRMATRIXTS'): begin
					outClass = 'MrMatrixTS'
					temp     = left['DATA'] * *self.data
				endcase
				
				else: message, 'Unrecognized object class: "' + obj_class(left) + '".'
			endcase
		endelse

	;-------------------------------------------------------
	; Create Result ////////////////////////////////////////
	;-------------------------------------------------------
		result = obj_new(outClass, self.oTime, temp, NAME=name, /NO_COPY)
	endelse

;-------------------------------------------------------
; Output Object ////////////////////////////////////////
;-------------------------------------------------------
	return, result
end


;+
;   Raise one expression to the power of another.
;
;   NOTE:
;     If one of LEFT or RIGHT is an expression or a MrVariable object,
;     normal IDL operations will take effect. This means the output will
;     be the size of the array with the smallest dimensions.
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
function MrMatrixTS::_OverloadCaret, left, right
	compile_opt idl2
	on_error, 2

;-------------------------------------------------------
; Superclass ///////////////////////////////////////////
;-------------------------------------------------------
	if ~isa(left, 'MrTimeSeries') || ~isa(right, 'MrTimeSeries') then begin
		result = self -> MrTimeSeries::_OverloadCaret(left, right)

;-------------------------------------------------------
; MrTimeSeries with MrTimeSeries ///////////////////////
;-------------------------------------------------------
	endif else begin

		;Is SELF on the left or right?
		;   - Are both LEFT and RIGHT GDA data objects?
		side = self -> _LeftOrRight(left, right, ISMRVARIABLE=IsMrVariable)

		;New name
		name = 'Caret(' + left.name + ',' + right.name + ')'

	;-------------------------------------------------------
	; SELF is LEFT /////////////////////////////////////////
	;-------------------------------------------------------
		if side eq 'LEFT' then begin
			case 1 of
				obj_isa(right, 'MRSCALARTS'): begin
					;Output dimensions
					nPts     = right -> GetNPts()
					dims     = size(*self.data, /DIMENSIONS)
					outDims  = [nPts, dims[1:2]]
					outClass = 'MrMatrixTS'
					
					;Operate
					temp = *self.data ^ rebin(right['DATA'], outDims)
				endcase
			
				obj_isa(right, 'MRVECTORTS'): message, 'Operation not valid: MrMatrixTS ^ MrVectorTS.'

				obj_isa(right, 'MRMATRIXTS'): begin
					outClass = 'MrMatrixTS'
					temp     = *self.data ^ right['DATA']
				endcase
				
				else: message, 'Unrecognized object class: "' + obj_class(right) + '".'
			endcase

	;-------------------------------------------------------
	; SELF is RIGHT ////////////////////////////////////////
	;-------------------------------------------------------
		endif else begin
			case 1 of
				obj_isa(left, 'MRSCALARTS'): begin
					;Output dimensions
					nPts     = left -> GetNPts()
					dims     = size(*self.data, /DIMENSIONS)
					outDims  = [nPts, dims[1:2]]
					outClass = 'MrMatrixTS'
					
					;Operate
					temp = rebin(left['DATA'], outDims) ^ *self.data
				endcase
			
				obj_isa(left, 'MRVECTORTS'): message, 'Operation not valid: MrVectorTS ^ MrMatrixTS.'

				obj_isa(left, 'MRMATRIXTS'): begin
					outClass = 'MrMatrixTS'
					temp     = left['DATA'] ^ *self.data
				endcase
				
				else: message, 'Unrecognized object class: "' + obj_class(left) + '".'
			endcase
		endelse

	;-------------------------------------------------------
	; Create Result ////////////////////////////////////////
	;-------------------------------------------------------
		result = obj_new(outClass, self.oTime, temp, NAME=name, /NO_COPY)
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
;   Subtract one expression from another.
;
;   NOTE:
;     If one of LEFT or RIGHT is an expression or a MrVariable object,
;     normal IDL operations will take effect. This means the output will
;     be the size of the array with the smallest dimensions.
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
function MrMatrixTS::_OverloadMinus, left, right
	compile_opt idl2
	on_error, 2

;-------------------------------------------------------
; Superclass ///////////////////////////////////////////
;-------------------------------------------------------
	if ~isa(left, 'MrTimeSeries') || ~isa(right, 'MrTimeSeries') then begin
		result = self -> MrTimeSeries::_OverloadMinus(left, right)

;-------------------------------------------------------
; MrTimeSeries with MrTimeSeries ///////////////////////
;-------------------------------------------------------
	endif else begin

		;Is SELF on the left or right?
		;   - Are both LEFT and RIGHT GDA data objects?
		side = self -> _LeftOrRight(left, right, ISMRVARIABLE=IsMrVariable)

		;New name
		name = 'Minus(' + left.name + ',' + right.name + ')'

	;-------------------------------------------------------
	; SELF is LEFT /////////////////////////////////////////
	;-------------------------------------------------------
		if side eq 'LEFT' then begin
			case 1 of
				obj_isa(right, 'MRSCALARTS'): begin
					;Output dimensions
					nPts     = right -> GetNPts()
					dims     = size(*self.data, /DIMENSIONS)
					outDims  = [nPts, dims[1:2]]
					outClass = 'MrMatrixTS'
					
					;Operate
					temp = *self.data - rebin(right['DATA'], outDims)
				endcase
			
				obj_isa(right, 'MRVECTORTS'): message, 'Operation not valid: MrMatrixTS - MrVectorTS.'

				obj_isa(right, 'MRMATRIXTS'): begin
					outClass = 'MrMatrixTS'
					temp     = *self.data - right['DATA']
				endcase
				
				else: message, 'Unrecognized object class: "' + obj_class(right) + '".'
			endcase

	;-------------------------------------------------------
	; SELF is RIGHT ////////////////////////////////////////
	;-------------------------------------------------------
		endif else begin
			case 1 of
				obj_isa(left, 'MRSCALARTS'): begin
					;Output dimensions
					nPts     = left -> GetNPts()
					dims     = size(*self.data, /DIMENSIONS)
					outDims  = [nPts, dims[1:2]]
					outClass = 'MrMatrixTS'
					
					;Operate
					temp = rebin(left['DATA'], outDims) - *self.data
				endcase
			
				obj_isa(left, 'MRVECTORTS'): message, 'Operation not valid: MrVectorTS - MrMatrixTS.'

				obj_isa(left, 'MRMATRIXTS'): begin
					outClass = 'MrMatrixTS'
					temp     = left['DATA'] - *self.data
				endcase
				
				else: message, 'Unrecognized object class: "' + obj_class(left) + '".'
			endcase
		endelse

	;-------------------------------------------------------
	; Create Result ////////////////////////////////////////
	;-------------------------------------------------------
		result = obj_new(outClass, self.oTime, temp, NAME=name, /NO_COPY)
	endelse

;-------------------------------------------------------
; Output Object ////////////////////////////////////////
;-------------------------------------------------------
	return, result
end


;+
;   Take the modulus of one expression with respect to another.
;
;   NOTE:
;     If one of LEFT or RIGHT is an expression or a MrVariable object,
;     normal IDL operations will take effect. This means the output will
;     be the size of the array with the smallest dimensions.
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
function MrMatrixTS::_OverloadMOD, left, right
	compile_opt idl2
	on_error, 2

;-------------------------------------------------------
; Superclass ///////////////////////////////////////////
;-------------------------------------------------------
	if ~isa(left, 'MrTimeSeries') || ~isa(right, 'MrTimeSeries') then begin
		result = self -> MrTimeSeries::_OverloadMOD(left, right)

;-------------------------------------------------------
; MrTimeSeries with MrTimeSeries ///////////////////////
;-------------------------------------------------------
	endif else begin

		;Is SELF on the left or right?
		;   - Are both LEFT and RIGHT GDA data objects?
		side = self -> _LeftOrRight(left, right, ISMRVARIABLE=IsMrVariable)

		;New name
		name = 'Mod(' + left.name + ',' + right.name + ')'

	;-------------------------------------------------------
	; SELF is LEFT /////////////////////////////////////////
	;-------------------------------------------------------
		if side eq 'LEFT' then begin
			case 1 of
				obj_isa(right, 'MRSCALARTS'): begin
					;Output dimensions
					nPts     = right -> GetNPts()
					dims     = size(*self.data, /DIMENSIONS)
					outDims  = [nPts, dims[1:2]]
					outClass = 'MrMatrixTS'
					
					;Operate
					temp = *self.data mod rebin(right['DATA'], outDims)
				endcase
			
				obj_isa(right, 'MRVECTORTS'): message, 'Operation not valid: MrMatrixTS mod MrVectorTS.'

				obj_isa(right, 'MRMATRIXTS'): begin
					outClass = 'MrMatrixTS'
					temp     = *self.data mod right['DATA']
				endcase
				
				else: message, 'Unrecognized object class: "' + obj_class(right) + '".'
			endcase

	;-------------------------------------------------------
	; SELF is RIGHT ////////////////////////////////////////
	;-------------------------------------------------------
		endif else begin
			case 1 of
				obj_isa(left, 'MRSCALARTS'): begin
					;Output dimensions
					nPts     = left -> GetNPts()
					dims     = size(*self.data, /DIMENSIONS)
					outDims  = [nPts, dims[1:2]]
					outClass = 'MrMatrixTS'
					
					;Operate
					temp = rebin(left['DATA'], outDims) mod *self.data
				endcase
			
				obj_isa(left, 'MRVECTORTS'): message, 'Operation not valid: MrVectorTS mod MrMatrixTS.'

				obj_isa(left, 'MRMATRIXTS'): begin
					outClass = 'MrMatrixTS'
					temp     = left['DATA'] mod *self.data
				endcase
				
				else: message, 'Unrecognized object class: "' + obj_class(left) + '".'
			endcase
		endelse

	;-------------------------------------------------------
	; Create Result ////////////////////////////////////////
	;-------------------------------------------------------
		result = obj_new(outClass, self.oTime, temp, NAME=name, /NO_COPY)
	endelse

;-------------------------------------------------------
; Output Object ////////////////////////////////////////
;-------------------------------------------------------
	return, result
end


;+
;   Add one expression to another.
;
;   NOTE:
;     If one of LEFT or RIGHT is an expression or a MrVariable object,
;     normal IDL operations will take effect. This means the output will
;     be the size of the array with the smallest dimensions.
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
function MrMatrixTS::_OverloadPlus, left, right
	compile_opt idl2
	on_error, 2

;-------------------------------------------------------
; Superclass ///////////////////////////////////////////
;-------------------------------------------------------
	if ~isa(left, 'MrTimeSeries') || ~isa(right, 'MrTimeSeries') then begin
		result = self -> MrTimeSeries::_OverloadPlus(left, right)

;-------------------------------------------------------
; MrTimeSeries with MrTimeSeries ///////////////////////
;-------------------------------------------------------
	endif else begin

		;Is SELF on the left or right?
		;   - Are both LEFT and RIGHT GDA data objects?
		side = self -> _LeftOrRight(left, right, ISMRVARIABLE=IsMrVariable)

		;New name
		name = 'Plus(' + left.name + ',' + right.name + ')'

	;-------------------------------------------------------
	; SELF is LEFT /////////////////////////////////////////
	;-------------------------------------------------------
		if side eq 'LEFT' then begin
			case 1 of
				obj_isa(right, 'MRSCALARTS'): begin
					;Output dimensions
					nPts     = right -> GetNPts()
					dims     = size(*self.data, /DIMENSIONS)
					outDims  = [nPts, dims[1:2]]
					outClass = 'MrMatrixTS'
					
					;Operate
					temp = *self.data + rebin(right['DATA'], outDims)
				endcase
			
				obj_isa(right, 'MRVECTORTS'): message, 'Operation not valid: MrMatrixTS + MrVectorTS.'

				obj_isa(right, 'MRMATRIXTS'): begin
					outClass = 'MrMatrixTS'
					temp     = *self.data + right['DATA']
				endcase
				
				else: message, 'Unrecognized object class: "' + obj_class(right) + '".'
			endcase

	;-------------------------------------------------------
	; SELF is RIGHT ////////////////////////////////////////
	;-------------------------------------------------------
		endif else begin
			case 1 of
				obj_isa(left, 'MRSCALARTS'): begin
					;Output dimensions
					nPts     = left -> GetNPts()
					dims     = size(*self.data, /DIMENSIONS)
					outDims  = [nPts, dims[1:2]]
					outClass = 'MrMatrixTS'
					
					;Operate
					temp = rebin(left['DATA'], outDims) + *self.data
				endcase
			
				obj_isa(left, 'MRVECTORTS'): message, 'Operation not valid: MrVectorTS + MrMatrixTS.'

				obj_isa(left, 'MRMATRIXTS'): begin
					outClass = 'MrMatrixTS'
					temp     = left['DATA'] + *self.data
				endcase
				
				else: message, 'Unrecognized object class: "' + obj_class(left) + '".'
			endcase
		endelse

	;-------------------------------------------------------
	; Create Result ////////////////////////////////////////
	;-------------------------------------------------------
		result = obj_new(outClass, self.oTime, temp, NAME=name, /NO_COPY)
	endelse

;-------------------------------------------------------
; Output Object ////////////////////////////////////////
;-------------------------------------------------------
	return, result
end


;+
;   Compute the inner product of two expressions.
;
;   NOTE:
;     If one of LEFT or RIGHT is an expression or a MrVariable object,
;     normal IDL operations will take effect. This means the output will
;     be the size of the array with the smallest dimensions.
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
function MrMatrixTS::_OverloadPound, left, right
	compile_opt idl2
	on_error, 2

;-------------------------------------------------------
; Superclass ///////////////////////////////////////////
;-------------------------------------------------------
	if ~isa(left, 'MrTimeSeries') || ~isa(right, 'MrTimeSeries') then begin
		result = self -> MrTimeSeries::_OverloadPound(left, right)

;-------------------------------------------------------
; MrTimeSeries with MrTimeSeries ///////////////////////
;-------------------------------------------------------
	endif else begin

		;Is SELF on the left or right?
		;   - Are both LEFT and RIGHT GDA data objects?
		side = self -> _LeftOrRight(left, right, ISMRVARIABLE=IsMrVariable)

		;New name
		name = 'Pound(' + left.name + ',' + right.name + ')'

	;-------------------------------------------------------
	; Two MrMatrixTS ///////////////////////////////////////
	;-------------------------------------------------------
		if obj_isa(left, 'MrMatrixTS') && obj_isa(right, 'MrMatrixTS') then begin
			;Matrix dimensions
			lDims    = size(left,  /DIMENSIONS)
			rDims    = size(right, /DIMENSIONS)
			outClass = 'MrMatrixTS'

			;LEFT=Nx3x3, RIGHT=Nx3x3
			if array_equal(lDims[1:2], 3) && array_equal(rDims[1:2], 3) then begin
				temp = [ [ [left[*,0,0]*right[*,0,0] + left[*,0,1]*right[*,1,0] + left[*,0,2]*right[*,2,0]], $
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
				nPts = lDims[0] < rDims[0]
				
				;Allocate memory to results
				temp = make_array(nPts, lDims[1], rDims[2], $
				                  TYPE=size( reform(left[0,*,*])#reform(right[0,*,*]), /TYPE))
				
				;Compute result
				;   - Increment to the least number of elements
				foreach mat, left, idx do begin
					temp[idx,*,*] = mat # reform(right[idx,*,*])
					if idx eq nPts-1 then break
				endforeach
			endelse

	;-------------------------------------------------------
	; SELF is LEFT /////////////////////////////////////////
	;-------------------------------------------------------
		endif else if side eq 'LEFT' then begin
			case 1 of
				obj_isa(right, 'MRSCALARTS'): message, 'Operation now allowed: MrMatrixTS # MrScalarTS.'
			
				obj_isa(right, 'MRVECTORTS'): begin
					;Make sure the matrix is the correct size
					mDims    = size(self, /DIMENSIONS)
					outClass = mDims[1] eq 3 ? 'MrVectorTS' : 'MrTimeSeries'
					if mDims[2] ne 3 then message, 'MrMatrixTS dimensions must be NxMx3.'
					
					;Inner product
					;   - Reform vector from Nx3 to NxMx3
					vDims = size( right, /DIMENSIONS )
					temp = total( *self.data * $
					              rebin( reform(right['DATA'], [vDims[0], 1, vDims[1]]), [vDims[0], mDims[1], vDims[1]] ), $
					              3, /PRESERVE_TYPE )
				endcase

				else: message, 'Unrecognized object class: "' + obj_class(right) + '".'
			endcase

	;-------------------------------------------------------
	; SELF is RIGHT ////////////////////////////////////////
	;-------------------------------------------------------
		endif else begin
			case 1 of
				obj_isa(left, 'MRSCALARTS'): message, 'Operation not valid: MrScalarTS # MrVectorTS'
			
				obj_isa(left, 'MRVECTORTS'): begin
					;Make sure the matrix is the correct size
					mDims    = size(self, /DIMENSIONS)
					outClass = mDims[1] eq 3 ? 'MrVectorTS' : 'MrTimeSeries'
					if mDims[1] ne 3 then message, 'MrMatrixTS dimensions must be Nx3xM.'
					
					;Inner product
					;   - Reform vector from Nx3 to Nx3xM
					vDims = size( left, /DIMENSIONS )
					temp  = total( rebin( left['DATA'], [vDims[0], vDims[1], mDims[2]] ) * $
					               *self.data, 2, /PRESERVE_TYPE )
				endcase

				else: message, 'Unrecognized object class: "' + obj_class(left) + '".'
			endcase
		endelse

	;-------------------------------------------------------
	; Create Result ////////////////////////////////////////
	;-------------------------------------------------------
		result = obj_new(outClass, self.oTime, temp, $
		                 DIMENSION = 1, $
		                 NAME      = name, $
		                 /NO_COPY)
	endelse

;-------------------------------------------------------
; Output Object ////////////////////////////////////////
;-------------------------------------------------------
	return, result
end


;+
;   Compute the outer product of two expressions.
;
;   NOTE:
;     If one of LEFT or RIGHT is an expression or a MrVariable object,
;     normal IDL operations will take effect. This means the output will
;     be the size of the array with the smallest dimensions.
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
function MrMatrixTS::_OverloadPoundPound, left, right
	compile_opt idl2
	on_error, 2

;-------------------------------------------------------
; Superclass ///////////////////////////////////////////
;-------------------------------------------------------
	if ~isa(left, 'MrTimeSeries') || ~isa(right, 'MrTimeSeries') then begin
		result = self -> MrTimeSeries::_OverloadPoundPound(left, right)

;-------------------------------------------------------
; MrTimeSeries with MrTimeSeries ///////////////////////
;-------------------------------------------------------
	endif else begin

		;Is SELF on the left or right?
		;   - Are both LEFT and RIGHT GDA data objects?
		side = self -> _LeftOrRight(left, right, ISMRVARIABLE=IsMrVariable)

		;New name
		name = 'PoundPound(' + left.name + ',' + right.name + ')'

	;-------------------------------------------------------
	; Two MrMatrixTS ///////////////////////////////////////
	;-------------------------------------------------------
		if obj_isa(left, 'MrMatrixTS') && obj_isa(right, 'MrMatrixTS') then begin
			;Matrix dimensions
			lDims    = size(left,  /DIMENSIONS)
			rDims    = size(right, /DIMENSIONS)
			outClass = 'MrMatrixTS'
			
			;LEFT=Nx3x3, RIGHT=Nx3x3
			if array_equal(lDims[1:2], 3) && array_equal(rDims[1:2], 3) then begin
				temp = [ [ [left[*,0,0]*right[*,0,0] + left[*,1,0]*right[*,0,1] + left[*,2,0]*right[*,0,2]], $
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
				nPts = lDims[0] < rDims[0]
				
				;Allocate memory to results
				temp = make_array(nPts, lDims[2], rDims[1], $
				                  TYPE=size( reform(left[0,*,*]) ## reform(right[0,*,*]), /TYPE))
				
				;Compute result
				;   - Increment to the least number of elements
				foreach mat, left, idx do begin
					temp[idx,*,*] = mat ## reform(right[idx,*,*])
					if idx eq nPts-1 then break
				endforeach
			endelse

	;-------------------------------------------------------
	; SELF is LEFT /////////////////////////////////////////
	;-------------------------------------------------------
		endif else if side eq 'LEFT' then begin
			case 1 of
				obj_isa(right, 'MRSCALARTS'): message, 'Operation now allowed: MrMatrixTS ## MrScalarTS.'
			
				obj_isa(right, 'MRVECTORTS'): begin
					;Make sure the matrix is the correct size
					mDims    = size(self, /DIMENSIONS)
					outClass = mDims[2] eq 3 ? 'MrVectorTS' : 'MrTimeSeries'
					if mDims[1] ne 3 then message, 'MrMatrixTS dimensions must be Nx3xM.'
					
					;Outer product
					;   - Reform vector from Nx3 to Nx3xM
					vDims = size( right, /DIMENSIONS )
					temp  = total( *self.data * $
					               rebin( right['DATA'], [vDims[0], vDims[1], mDims[2]] ), $
					               2, /PRESERVE_TYPE )
				endcase

				else: message, 'Unrecognized object class: "' + obj_class(right) + '".'
			endcase

	;-------------------------------------------------------
	; SELF is RIGHT ////////////////////////////////////////
	;-------------------------------------------------------
		endif else begin
			case 1 of
				obj_isa(left, 'MRSCALARTS'): message, 'Operation not valid: MrScalarTS ## MrVectorTS'
			
				obj_isa(left, 'MRVECTORTS'): begin
					;Make sure the matrix is the correct size
					mDims    = size(self, /DIMENSIONS)
					outClass = mDims[1] eq 3 ? 'MrVectorTS' : 'MrTimeSeries'
					if mDims[2] ne 3 then message, 'MrMatrixTS dimensions must be NxMx3.'
					
					;Outer product
					;   - Reform vector from Nx3 to NxMx3
					vDims = size( right, /DIMENSIONS )
					temp = total( rebin( reform(left['DATA'], [vDims[0], 1, vDims[1]]), [vDims[0], mDims[1], vDims[1]] ) * $
					              *self.data, 3, /PRESERVE_TYPE )
				endcase

				else: message, 'Unrecognized object class: "' + obj_class(left) + '".'
			endcase
		endelse

	;-------------------------------------------------------
	; Create Result ////////////////////////////////////////
	;-------------------------------------------------------
		result = obj_new(outClass, self.oTime, temp, NAME=name, /NO_COPY)
	endelse

;-------------------------------------------------------
; Output Object ////////////////////////////////////////
;-------------------------------------------------------
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
;   Divide one expression by another.
;
;   NOTE:
;     If one of LEFT or RIGHT is an expression or a MrVariable object,
;     normal IDL operations will take effect. This means the output will
;     be the size of the array with the smallest dimensions.
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

;-------------------------------------------------------
; Superclass ///////////////////////////////////////////
;-------------------------------------------------------
	if ~isa(left, 'MrTimeSeries') || ~isa(right, 'MrTimeSeries') then begin
		result = self -> MrTimeSeries::_OverloadSlash(left, right)

;-------------------------------------------------------
; MrTimeSeries with MrTimeSeries ///////////////////////
;-------------------------------------------------------
	endif else begin

		;Is SELF on the left or right?
		;   - Are both LEFT and RIGHT GDA data objects?
		side = self -> _LeftOrRight(left, right, ISMRVARIABLE=IsMrVariable)

		;New name
		name = 'Slash(' + left.name + ',' + right.name + ')'

	;-------------------------------------------------------
	; SELF is LEFT /////////////////////////////////////////
	;-------------------------------------------------------
		if side eq 'LEFT' then begin
			case 1 of
				obj_isa(right, 'MRSCALARTS'): begin
					;Output dimensions
					nPts     = right -> GetNPts()
					dims     = size(*self.data, /DIMENSIONS)
					outDims  = [nPts, dims[1:2]]
					outClass = 'MrMatrixTS'
					
					;Operate
					temp = *self.data / rebin(right['DATA'], outDims)
				endcase
			
				obj_isa(right, 'MRVECTORTS'): message, 'Operation not valid: MrMatrixTS / MrVectorTS.'

				obj_isa(right, 'MRMATRIXTS'): begin
					outClass = 'MrMatrixTS'
					temp     = *self.data / right['DATA']
				endcase
				
				else: message, 'Unrecognized object class: "' + obj_class(right) + '".'
			endcase

	;-------------------------------------------------------
	; SELF is RIGHT ////////////////////////////////////////
	;-------------------------------------------------------
		endif else begin
			case 1 of
				obj_isa(left, 'MRSCALARTS'): begin
					;Output dimensions
					nPts     = left -> GetNPts()
					dims     = size(*self.data, /DIMENSIONS)
					outDims  = [nPts, dims[1:2]]
					outClass = 'MrMatrixTS'
					
					;Operate
					temp = rebin(left['DATA'], outDims) / *self.data
				endcase
			
				obj_isa(left, 'MRVECTORTS'): message, 'Operation not valid: MrVectorTS / MrMatrixTS.'

				obj_isa(left, 'MRMATRIXTS'): begin
					outClass = 'MrMatrixTS'
					temp     = left['DATA'] / *self.data
				endcase
				
				else: message, 'Unrecognized object class: "' + obj_class(left) + '".'
			endcase
		endelse

	;-------------------------------------------------------
	; Create Result ////////////////////////////////////////
	;-------------------------------------------------------
		result = obj_new(outClass, self.oTime, temp, NAME=name, /NO_COPY)
	endelse

;-------------------------------------------------------
; Output Object ////////////////////////////////////////
;-------------------------------------------------------
	return, result
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
	if isa(mat, 'MrMatrixTS') then begin
		;A and B must have the same dimensions.
		Adims = size(self, /DIMENSIONS)
		Bdims = size(mat, /DIMENSIONS)
		if ~array_equal(Adims[1:2], Bdims[1:2]) then message, 'MAT is in correct size.'
		
		;Rotate
		;   - self # mat # transpose(self)
		out = self # mat # self -> Transpose()

;-----------------------------------------------------
; Other Array \\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\
;-----------------------------------------------------
	endif else begin
		;self # mat
		temp = self -> InnerProduct(mat)
		
		;(self # mat) # transpose(self)
		out  = temp -> InnerProduct( self -> Transpose() )
	endelse
	
;-----------------------------------------------------
; Output \\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\
;-----------------------------------------------------
	return, out
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
	oVec = MrVectorTS(self.oTime, outVec, NAME=name, CACHE=cache, /NO_COPY)

	return, oVec
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
;       _REF_EXTRA:     in, optional, type=integer
;                       Any keyword accepted by MrTimeSeries::SetData is accepted here
;                           via keyword inheritance.
;-
pro MrMatrixTS::SetData, time, data, $
_REF_EXTRA=extra
	compile_opt idl2
	on_error, 2
	
	;Keep the old data
	oTime = self.oTime
	pDAta = self.data

	;Use the superclass
	self -> MrTimeSeries::SetData, time, data, _STRICT_EXTRA=extra

	;Check the results
	;   - Make sure it is NxMxL (3 dimensions of any size)
	sz = size(self)
	if sz[0] ne 3 then begin
		ptr_free, self.data
		self.data  = pData
		self.oTime = oTime
		message, 'Invalid dimensions: Data must be an NxMxL matrix time series.'
	endif else begin
		ptr_free, pData
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
	          inherits MrTimeSeries $
	        }
end