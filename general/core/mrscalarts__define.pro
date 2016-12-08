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
;           MrScalarTS [op] MrScalarTS = MrScalarTS
;           MrScalarTS [op] MrVectorTS = MrVectorTS
;           MrScalarTS [op] MrMatrixTS = MrMatrixTS
;
;   CACHING
;       The ::Cache method will cache the MrScalarTS object. All variables in the
;       cache are forced to have unique names. When the object is destroyed, it
;       will automatically be removed from the cache.
;
; :Categories:
;   MrVariable, MrTimeSeries, MrScalarTS
;
; :See Also:
;   MrVariable__Define.pro
;   MrTimeSeries__Define.pro
;   MrVectorTS__Define.pro
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
;       2016/10/23  -   Major rewrite to inherit MrTimeSeries object. - MRA
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
function MrScalarTS::INIT, time, data, $
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
	if n_elements(name) eq 0 then name = 'MrScalarTS'
	
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
pro MrScalarTS::CLEANUP
	compile_opt idl2
	on_error, 2
	
	;Superclass
	self -> MrTimeSeries::Cleanup
end


;+
;   The purpose of this method is to multiply two expressions together.
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
function MrScalarTS::_OverloadAsterisk, left, right
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
					temp     = *self.data * right['DATA']
					outClass = 'MrScalarTS'
				endcase
			
				obj_isa(right, 'MRVECTORTS'): begin
					;Output dimensions
					nPts     = self -> GetNPts()
					outDims  = [nPts, 3]
					outClass = 'MrVectorTS'
				
					;Operate
					temp = rebin(*self.data, outDims) * right['DATA']
				
					;As opposed to
;					temp[*,0] = *self.data * right[*,0]
;					temp[*,1] = *self.data * right[*,1]
;					temp[*,2] = *self.data * right[*,2]
				endcase

				obj_isa(right, 'MRMATRIXTS'): begin
					;Output dimensions
					nPts     = self -> GetNPts()
					dims     = size( right, /DIMENSIONS )
					outDims  = [nPts, dims[1], dims[2]]
					outClass = 'MrMatrixTS'
				
					;Operate
					temp = rebin(*self.data, outDims) * right['DATA']
					
					;As opposed to
;					for i = 0, dims[1]-1 do begin
;					for j = 0, dims[2]-1 do begin
;						temp[*,i,j] = *self.data * right[*,i,j]
;					endfor
;					endfor
				endcase
				else: message, 'Unrecognized object class: "' + obj_class(right) + '".'
			endcase

	;-------------------------------------------------------
	; SELF is RIGHT ////////////////////////////////////////
	;-------------------------------------------------------
		endif else begin
			case 1 of
				obj_isa(left, 'MRSCALARTS'): begin
					temp     = left['DATA'] * *self.data
					outClass = 'MrScalarTS'
				endcase
			
				obj_isa(left, 'MRVECTORTS'): begin
					;Output dimensions
					nPts     = self -> GetNPts()
					outDims  = [nPts, 3]
					outClass = 'MrVectorTS'
				
					;Operate
					temp = left['DATA'] * rebin(*self.data, outDims)
				endcase

				obj_isa(left, 'MRMATRIXTS'): begin
					;Output dimensions
					nPts     = self -> GetNPts()
					dims     = size( right, /DIMENSIONS )
					outDims  = [nPts, dims[1], dims[2]]
					outClass = 'MrMatrixTS'
				
					;Operate
					temp = left['DATA'] * rebin(*self.data, outDims)
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
function MrScalarTS::_OverloadCaret, left, right
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
	; SELF is on LEFT //////////////////////////////////////
	;-------------------------------------------------------
		if side eq 'LEFT' then begin
			case 1 of
				obj_isa(right, 'MRSCALARTS'): begin
					temp     = *self.data ^ right['DATA']
					outClass = 'MrScalarTS'
				endcase
			
				obj_isa(right, 'MRVECTORTS'): begin
					;Output dimensions
					nPts     = self -> GetNPts()
					outDims  = [nPts, 3]
					outClass = 'MrVectorTS'
				
					;Operate
					temp = rebin(*self.data, outDims) ^ right['DATA']
				endcase

				obj_isa(right, 'MRMATRIXTS'): begin
					;Output dimensions
					nPts     = self -> GetNPts()
					dims     = size( right, /DIMENSIONS )
					outDims  = [nPts, dims[1], dims[2]]
					outClass = 'MrMatrixTS'
				
					;Operate
					temp = rebin(*self.data, outDims) ^ right['DATA']
				endcase
				else: message, 'Unrecognized object class: "' + obj_class(right) + '".'
			endcase

	;-------------------------------------------------------
	; SELF is on RIGHT /////////////////////////////////////
	;-------------------------------------------------------
		endif else begin
			case 1 of
				obj_isa(left, 'MRSCALARTS'): begin
					temp     = left['DATA'] ^ *self.data
					outClass = 'MrScalarTS'
				endcase
			
				obj_isa(left, 'MRVECTORTS'): begin
					;Output dimensions
					nPts     = self -> GetNPts()
					outDims  = [nPts, 3]
					outClass = 'MrVectorTS'
				
					;Operate
					temp = left['DATA'] ^ rebin(*self.data, outDims)
				endcase

				obj_isa(left, 'MRMATRIXTS'): begin
					;Output dimensions
					nPts     = self -> GetNPts()
					dims     = size( right, /DIMENSIONS )
					outDims  = [nPts, dims[1], dims[2]]
					outClass = 'MrMatrixTS'
				
					;Operate
					temp = left['DATA'] ^ rebin(*self.data, outDims)
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
function MrScalarTS::_OverloadMinus, left, right
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
	; SELF is on LEFT //////////////////////////////////////
	;-------------------------------------------------------
		if side eq 'LEFT' then begin
			case 1 of
				obj_isa(right, 'MRSCALARTS'): begin
					temp     = *self.data - right['DATA']
					outClass = 'MrScalarTS'
				endcase
				
				obj_isa(right, 'MRVECTORTS'): begin
					;Output dimensions
					nPts     = self -> GetNPts()
					outDims  = [nPts, 3]
					outClass = 'MrVectorTS'
				
					;Operate
					temp = rebin(*self.data, outDims) - right['DATA']
				endcase
	
				obj_isa(right, 'MRMATRIXTS'): begin
					;Output dimensions
					nPts     = self -> GetNPts()
					dims     = size( right, /DIMENSIONS )
					outDims  = [nPts, dims[1], dims[2]]
					outClass = 'MrMatrixTS'
				
					;Operate
					temp = rebin(*self.data, outDims) - right['DATA']
				endcase
				else: message, 'Unrecognized object class: "' + obj_class(right) + '".'
			endcase

	;-------------------------------------------------------
	; SELF is on RIGHT /////////////////////////////////////
	;-------------------------------------------------------
		endif else begin
			case 1 of
				obj_isa(left, 'MRSCALARTS'): begin
					temp     = left['DATA'] - *self.data
					outClass = 'MrScalarTS'
				endcase
				
				obj_isa(left, 'MRVECTORTS'): begin
					;Output dimensions
					nPts     = self -> GetNPts()
					outDims  = [nPts, 3]
					outClass = 'MrVectorTS'
				
					;Operate
					temp = left['DATA'] - rebin(*self.data, outDims)
				endcase
	
				obj_isa(left, 'MRMATRIXTS'): begin
					;Output dimensions
					nPts     = self -> GetNPts()
					dims     = size( right, /DIMENSIONS )
					outDims  = [nPts, dims[1], dims[2]]
					outClass = 'MrMatrixTS'
				
					;Operate
					temp = left['DATA'] - rebin(*self.data, outDims)
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
;   Take the MOD of with expression with another.
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
function MrScalarTS::_OverloadMOD, left, right
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
	; SELF is on the LEFT //////////////////////////////////
	;-------------------------------------------------------
		if side eq 'LEFT' then begin
			case 1 of
				obj_isa(right, 'MRSCALARTS'): begin
					temp     = *self.data mod right['DATA']
					outClass = 'MrScalarTS'
				endcase
				
				obj_isa(right, 'MRVECTORTS'): begin
					;Output dimensions
					nPts     = self -> GetNPts()
					outDims  = [nPts, 3]
					outClass = 'MrVectorTS'
				
					;Operate
					temp = rebin(*self.data, outDims) mod right['DATA']
				endcase
	
				obj_isa(right, 'MRMATRIXTS'): begin
					;Output dimensions
					nPts     = self -> GetNPts()
					dims     = size( right, /DIMENSIONS )
					outDims  = [nPts, dims[1], dims[2]]
					outClass = 'MrMatrixTS'
				
					;Operate
					temp = rebin(*self.data, outDims) mod right['DATA']
				endcase
				else: message, 'Unrecognized object class: "' + obj_class(right) + '".'
			endcase

	;-------------------------------------------------------
	; SELF is on the RIGHT //////////////////////////////////
	;-------------------------------------------------------
		endif else begin
			case 1 of
				obj_isa(left, 'MRSCALARTS'): begin
					temp     = left['DATA'] mod *self.data
					outClass = 'MrScalarTS'
				endcase
				
				obj_isa(left, 'MRVECTORTS'): begin
					;Output dimensions
					nPts     = self -> GetNPts()
					outDims  = [nPts, 3]
					outClass = 'MrVectorTS'
				
					;Operate
					temp = left['DATA'] mod rebin(*self.data, outDims)
				endcase
	
				obj_isa(left, 'MRMATRIXTS'): begin
					;Output dimensions
					nPts     = self -> GetNPts()
					dims     = size( right, /DIMENSIONS )
					outDims  = [nPts, dims[1], dims[2]]
					outClass = 'MrMatrixTS'
				
					;Operate
					temp = left['DATA'] mod rebin(*self.data, outDims)
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
;   Add two expressions together.
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
function MrScalarTS::_OverloadPlus, left, right
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
					temp     = *self.data + right['DATA']
					outClass = 'MrScalarTS'
				endcase
			
				obj_isa(right, 'MRVECTORTS'): begin
					;Output dimensions
					nPts     = self -> GetNPts()
					outDims  = [nPts, 3]
					outClass = 'MrVectorTS'
				
					;Operate
					temp = rebin(*self.data, outDims) + right['DATA']
				endcase

				obj_isa(right, 'MRMATRIXTS'): begin
					;Output dimensions
					nPts     = self -> GetNPts()
					dims     = size( right, /DIMENSIONS )
					outDims  = [nPts, dims[1], dims[2]]
					outClass = 'MrMatrixTS'
				
					;Operate
					temp = rebin(*self.data, outDims) + right['DATA']
				endcase
				else: message, 'Unrecognized object class: "' + obj_class(right) + '".'
			endcase

	;-------------------------------------------------------
	; SELF is RIGHT ////////////////////////////////////////
	;-------------------------------------------------------
		endif else begin
			case 1 of
				obj_isa(left, 'MRSCALARTS'): begin
					temp     = left['DATA'] + *self.data
					outClass = 'MrScalarTS'
				endcase
			
				obj_isa(left, 'MRVECTORTS'): begin
					;Output dimensions
					nPts     = self -> GetNPts()
					outDims  = [nPts, 3]
					outClass = 'MrVectorTS'
				
					;Operate
					temp = left['DATA'] + rebin(*self.data, outDims)
				endcase

				obj_isa(left, 'MRMATRIXTS'): begin
					;Output dimensions
					nPts     = self -> GetNPts()
					dims     = size( right, /DIMENSIONS )
					outDims  = [nPts, dims[1], dims[2]]
					outClass = 'MrMatrixTS'
				
					;Operate
					temp = left['DATA'] + rebin(*self.data, outDims)
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
function MrScalarTS::_OverloadPound, left, right
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
		message, 'The # operator cannot act on MrScalarTS and other MrTimeSeries objects.'
	endelse

;-------------------------------------------------------
; Output Object ////////////////////////////////////////
;-------------------------------------------------------
	return, result
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
function MrScalarTS::_OverloadPoundPound, left, right
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
		message, 'The ## operator cannot act on MrScalarTS and other MrTimeSeries objects.'
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
function MrScalarTS::_OverloadSlash, left, right
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

	;-------------------------------------------------------
	; SELF is on LEFT //////////////////////////////////////
	;-------------------------------------------------------
		if side eq 'LEFT' then begin
			case 1 of
				obj_isa(right, 'MRSCALARTS'): begin
					temp     = *self.data / right['DATA']
					outClass = 'MrScalarTS'
				endcase
			
				obj_isa(right, 'MRVECTORTS'): begin
					;Output dimensions
					nPts     = self -> GetNPts()
					outDims  = [nPts, 3]
					outClass = 'MrVectorTS'
				
					;Operate
					temp = rebin(*self.data, outDims) / right['DATA']
				endcase

				obj_isa(right, 'MRMATRIXTS'): begin
					;Output dimensions
					nPts     = self -> GetNPts()
					dims     = size( right, /DIMENSIONS )
					outDims  = [nPts, dims[1], dims[2]]
					outClass = 'MrMatrixTS'
				
					;Operate
					temp = rebin(*self.data, outDims) / right['DATA']
				endcase
				else: message, 'Unrecognized object class: "' + obj_class(right) + '".'
			endcase
		
			;New name
			name = 'Divide(' + self.name + ',' + right.name + ')'

	;-------------------------------------------------------
	; SELF is on RIGHT /////////////////////////////////////
	;-------------------------------------------------------
		endif else begin
			case 1 of
				obj_isa(left, 'MRSCALARTS'): begin
					temp     = left['DATA'] / *self.data
					outClass = 'MrScalarTS'
				endcase
			
				obj_isa(left, 'MRVECTORTS'): begin
					;Output dimensions
					nPts     = self -> GetNPts()
					outDims  = [nPts, 3]
					outClass = 'MrVectorTS'
				
					;Operate
					temp = left['DATA'] / rebin(*self.data, outDims)
				endcase

				obj_isa(left, 'MRMATRIXTS'): begin
					;Output dimensions
					nPts     = self -> GetNPts()
					dims     = size( right, /DIMENSIONS )
					outDims  = [nPts, dims[1], dims[2]]
					outClass = 'MrMatrixTS'
				
					;Operate
					temp = left['DATA'] / rebin(*self.data, outDims)
				endcase
				else: message, 'Unrecognized object class: "' + obj_class(left) + '".'
			endcase
		
			;New name
			name = 'Divide(' + left.name + ',' + self.name + ')'
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
;   Concatenate an array, or a series of arrays, to the implicit array.
;
; :Params:
;       TIME:           in, required, type=Nx1 or 1xN array
;                       Array to be concatenated to the implicit array. If undefined or
;                           !Null, then nothing is appended.
;       DATA:           in, required, type=Nx1 or 1xN array
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
pro MrScalarTS::Append, time, data, type, $
BEFORE=before, $
NO_COPY=no_copy
	compile_opt idl2
	on_error, 2
	
	;Both must be column or row vectors
	if n_elements(time) ne n_elements(data) $
		then message, 'TIME and DATA must have the same number of elements.'

	;Let superclass do the rest
	self -> MrTimeSeries::Append, time, data, type, $
	                              BEFORE  = before, $
	                              NO_COPY = no_copy
end


;+
;   High-pass, low-pass, band-pass, or band-stop filter data. The filter is created
;   using IDL's DIGITAL_FILTER function and applied using IDL's CONVOL function.
;
; :Params:
;       FLOW:           in, required, type=Numeric array
;                       Lower frequency of the filter as a fraction of the nyquist.
;       FHIGH:          in, required, type=Numeric array
;                       Upper frequency of the filter as a fraction of the nyquist.
;       A:              in, required, type=Numeric array
;                       The filter power relative to the Gibbs phenomenon wiggles in
;                           decibels. 50 is a good choice.
;       NTERMS:         in, required, type=Numeric array
;                       The number of terms used to construct the filter.
;
; :Keywords:
;       CACHE:          in, optional, type=boolean, default=0
;                       If set, the result will be added to the cache.
;       DOUBLE:         in, optional, type=boolean, default=0
;                       If set, computations are performed in double-precision.
;       NAME:           in, optional, type=string, default='Digital_Filter(<name>)'
;                       A name to be given to the variable.
;
; :Returns:
;       VAROUT:         out, required, type=object
;                       A MrScalarTS object containing the filtered data.
;-
function MrScalarTS::Digital_Filter, fLow, fHigh, A, nTerms, $
CACHE=cache, $
DOUBLE=double, $
NAME=name
	compile_opt idl2
	on_error, 2
	
	if n_elements(name) eq 0 then name = 'Digital_Filter(' + self.name + ')'
	
	;Create the filter
	coeff = digital_filter(fLow, fHigh, A, nTerms, DOUBLE=double)
	
	;Apply the filter
	dTemp = convol(self['DATA'], coeff)

	;Create the output variable
	varOut = MrScalarTS( self.oTime, dTemp, $
	                     CACHE     = cache, $
	                     DIMENSION = 1, $
	                     NAME      = name, $
	                     /NO_COPY )
	
	;Return the new variable
	return, varOut
end


;+
;   Perform interpolation on regularly or irregularly vectors.
;
;   Calling Sequence:
;       varOut = MrVar1 -> Interpol(Xout)
;       varOut = MrVar1 -> Interpol(Xout, t_type)
;
; :Params:
;       XOUT:           in, required, type=Numeric array
;                       The new time values or the name or object reference of a
;                           MrTimeVar object.
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
function MrScalarTS::Interpol, Xout, t_type, $
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
; Interpolate //////////////////////////////////////////
;-------------------------------------------------------
	y_out = Interpol( *self.data, temporary(t), temporary(t_out), $
	                  LSQUADRATIC = lsquadratic, $
	                  NAN         = nan, $
	                  QUADRATIC   = quadratic, $
	                  SPLINE      = spline)

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
;       _REF_EXTRA:     in, optional, type=integer
;                       Any keyword accepted by MrTimeSeries::SetData is accepted here
;                           via keyword inheritance.
;-
pro MrScalarTS::SetData, time, data, $
_REF_EXTRA=extra
	compile_opt idl2
	on_error, 2
	
	;Keep the old data
	oTime = self.oTime
	pDAta = self.data

	;Use the superclass
	self -> MrTimeSeries::SetData, time, data, _STRICT_EXTRA=extra

	;Check the results
	if size(self, /N_DIMENSIONS) ne 1 then begin
		ptr_free, self.data
		self.data  = pData
		self.oTime = oTime
		message, 'Invalid dimensions: Data must be an Nx1 scalar time series.'
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
pro MrScalarTS__DEFINE
	compile_opt idl2
	
	class = { MrScalarTS, $
	          inherits MrTimeSeries $
	        }
end