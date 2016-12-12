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
;           MrVectorTS [op] MrScalarTS = MrVectorTS
;           MrVectorTS [op] MrVectorTS = MrVectorTS
;           MrVectorTS [op] MrMatrixTS = Error
;
;   CACHING
;       The ::Cache method will cache the MrVectorTS object. All variables in the
;       cache are forced to have unique names. When the object is destroyed, it
;       will automatically be removed from the cache.
;
; :Categories:
;   MrVariable
;
; :See Also:
;   MrVariable__Define.pro
;   MrTimeSeries__Define.pro
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
function MrVectorTS::INIT, time, data, $
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
	if n_elements(name) eq 0 then name = 'MrVectorTS'
	
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
pro MrVectorTS::CLEANUP
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
function MrVectorTS::_OverloadAsterisk, left, right
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
					nPts     = self -> GetNPts()
					outDims  = [nPts, 3]
					outClass = 'MrVectorTS'
					
					;Operate
					temp = *self.data * rebin(right['DATA'], outDims)
				endcase
			
				obj_isa(right, 'MRVECTORTS'): begin
					outClass = 'MrVectorTS'
					temp     = *self.data * right['DATA']
				endcase

				obj_isa(right, 'MRMATRIXTS'): message, 'MrVectorTS * MrMatrixTS is not allowed.'
				else: message, 'Unrecognized object class: "' + obj_class(right) + '".'
			endcase

	;-------------------------------------------------------
	; SELF is RIGHT ////////////////////////////////////////
	;-------------------------------------------------------
		endif else begin
			case 1 of
				obj_isa(left, 'MRSCALARTS'): begin
					;Output dimensions
					nPts     = self -> GetNPts()
					outDims  = [nPts, 3]
					outClass = 'MrVectorTS'
					
					;Operate
					temp = rebin(left['DATA'], outDims) * *self.data
				endcase
			
				obj_isa(left, 'MRVECTORTS'): begin
					outClass = 'MrVectorTS'
					temp     = left['DATA'] * *self.data
				endcase

				obj_isa(left, 'MRMATRIXTS'): message, 'MrMatrixTS * MrVectorTS is not allowed.'
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
function MrVectorTS::_OverloadCaret, left, right
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
					nPts     = self -> GetNPts()
					outDims  = [nPts, 3]
					outClass = 'MrVectorTS'
					
					;Operate
					temp = *self.data ^ rebin(right['DATA'], outDims)
				endcase
			
				obj_isa(right, 'MRVECTORTS'): begin
					outClass = 'MrVectorTS'
					temp     = *self.data ^ right['DATA']
				endcase

				obj_isa(right, 'MRMATRIXTS'): message, 'MrVectorTS ^ MrMatrixTS is not allowed.'
				else: message, 'Unrecognized object class: "' + obj_class(right) + '".'
			endcase

	;-------------------------------------------------------
	; SELF is RIGHT ////////////////////////////////////////
	;-------------------------------------------------------
		endif else begin
			case 1 of
				obj_isa(left, 'MRSCALARTS'): begin
					;Output dimensions
					nPts     = self -> GetNPts()
					outDims  = [nPts, 3]
					outClass = 'MrVectorTS'
					
					;Operate
					temp = rebin(left['DATA'], outDims) ^ *self.data
				endcase
			
				obj_isa(left, 'MRVECTORTS'): begin
					outClass = 'MrVectorTS'
					temp     = left['DATA'] ^ *self.data
				endcase

				obj_isa(left, 'MRMATRIXTS'): message, 'MrMatrixTS ^ MrVectorTS is not allowed.'
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
function MrVectorTS::_OverloadMinus, left, right
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
				obj_isa(right, 'MrScalarTS'): begin
					;Output dimensions
					nPts     = self -> GetNPts()
					outDims  = [nPts, 3]
					outClass = 'MrVectorTS'
					
					;Operate
					temp = *self.data - rebin(right['DATA'], outDims)
				endcase
			
				obj_isa(right, 'MrVectorTS'): begin
					outClass = 'MrVectorTS'
					temp     = *self.data - right['DATA']
				endcase

				obj_isa(right, 'MrMatrixTS'): message, 'MrVectorTS - MrMatrixTS is not allowed.'
				else: message, 'Unrecognized object class: "' + obj_class(right) + '".'
			endcase

	;-------------------------------------------------------
	; SELF is RIGHT ////////////////////////////////////////
	;-------------------------------------------------------
		endif else begin
			case 1 of
				obj_isa(left, 'MrScalarTS'): begin
					;Output dimensions
					nPts     = self -> GetNPts()
					outDims  = [nPts, 3]
					outClass = 'MrVectorTS'
					
					;Operate
					temp = rebin(left['DATA'], outDims) - *self.data
				endcase
			
				obj_isa(left, 'MrVectorTS'): begin
					outClass = 'MrVectorTS'
					temp     = left['DATA'] - *self.data
				endcase

				obj_isa(left, 'MrMatrixTS'): message, 'MrMatrixTS - MrVectorTS is not allowed.'
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
;   Take the modulus between two expressions.
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
function MrVectorTS::_OverloadMOD, left, right
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
				obj_isa(right, 'MrScalarTS'): begin
					;Output dimensions
					nPts     = self -> GetNPts()
					outDims  = [nPts, 3]
					outClass = 'MrVectorTS'
					
					;Operate
					temp = *self.data mod rebin(right['DATA'], outDims)
				endcase
			
				obj_isa(right, 'MrVectorTS'): begin
					outClass = 'MrVectorTS'
					temp     = *self.data mod right['DATA']
				endcase

				obj_isa(right, 'MrMatrixTS'): message, 'MrVectorTS mod MrMatrixTS is not allowed.'
				else: message, 'Unrecognized object class: "' + obj_class(right) + '".'
			endcase

	;-------------------------------------------------------
	; SELF is RIGHT ////////////////////////////////////////
	;-------------------------------------------------------
		endif else begin
			case 1 of
				obj_isa(left, 'MRSCALARTS'): begin
					;Output dimensions
					nPts     = self -> GetNPts()
					outDims  = [nPts, 3]
					outClass = 'MrVectorTS'
					
					;Operate
					temp = rebin(left['DATA'], outDims) mod *self.data
				endcase
			
				obj_isa(left, 'MRVECTORTS'): begin
					outClass = 'MrVectorTS'
					temp     = left['DATA'] mod *self.data
				endcase

				obj_isa(left, 'MRMATRIXTS'): message, 'MrMatrixTS mod MrVectorTS is not allowed.'
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
;   Add two expressions.
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
function MrVectorTS::_OverloadPlus, left, right
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
					nPts     = self -> GetNPts()
					outDims  = [nPts, 3]
					outClass = 'MrVectorTS'
					
					;Operate
					temp = *self.data + rebin(right['DATA'], outDims)
				endcase
			
				obj_isa(right, 'MRVECTORTS'): begin
					outClass = 'MrVectorTS'
					temp     = *self.data + right['DATA']
				endcase

				obj_isa(right, 'MRMATRIXTS'): message, 'MrVectorTS + MrMatrixTS is not allowed.'
				else: message, 'Unrecognized object class: "' + obj_class(right) + '".'
			endcase

	;-------------------------------------------------------
	; SELF is RIGHT ////////////////////////////////////////
	;-------------------------------------------------------
		endif else begin
			case 1 of
				obj_isa(left, 'MRSCALARTS'): begin
					;Output dimensions
					nPts     = self -> GetNPts()
					outDims  = [nPts, 3]
					outClass = 'MrVectorTS'
					
					;Operate
					temp = rebin(left['DATA'], outDims) + *self.data
				endcase
			
				obj_isa(left, 'MRVECTORTS'): begin
					outClass = 'MrVectorTS'
					temp     = left['DATA'] + *self.data
				endcase

				obj_isa(left, 'MRMATRIXTS'): message, 'MrMatrixTS + MrVectorTS is not allowed.'
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
function MrVectorTS::_OverloadPound, left, right
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
	; SELF is LEFT /////////////////////////////////////////
	;-------------------------------------------------------
		if side eq 'LEFT' then begin
			case 1 of
				obj_isa(right, 'MRSCALARTS'): message, 'Operation now allowed: MrVectorTS # MrScalarTS.'
			
				obj_isa(right, 'MRVECTORTS'): begin
					;Compute the inner product.
					temp     = total( *self.data * right['DATA'], 2, /PRESERVE_TYPE )
					outClass = 'MrScalarTS'
				endcase

				obj_isa(right, 'MRMATRIXTS'): begin
					;Make sure the matrix is the correct size
					mDims = size(right, /DIMENSIONS)
					if mDims[1] ne 3 then message, 'MrMatrixTS dimensions must be Nx3xM.'
					
					;Output size
					dims     = size( *self.data, /DIMENSIONS )
					outClass = mDims[2] eq 3 ? 'MrVectorTS' : 'MrTimeSeries'
					
					;Inner product
					;   - Reform Nx3 to Nx3xM
					temp = total( rebin( *self.data, [dims[0], dims[1], mDims[2]] ) * $
					              right['DATA'], 2, /PRESERVE_TYPE )
					
					;NOT VECTORIZED (Saves memory)
;					type = size( (*self.data)[0] * right[[0]], /TYPE )
;					temp = make_array( dims[0], dims[2], TYPE=type)
;					for i = 0, dims[1] - 1 do begin
;						temp[*,i] = (*self.data)[*,0] * right[*,0,i] + $
;						            (*self.data)[*,1] * right[*,1,i] + $
;						            (*self.data)[*,2] * right[*,2,i]
;					endfor
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
					;Compute the inner product.
					temp     = total( *self.data * right['DATA'], 2, /PRESERVE_TYPE )
					outClass = 'MrScalarTS'
				endcase

				obj_isa(left, 'MRMATRIXTS'): begin
					;Make sure the matrix is the correct size
					mDims = size(left, /DIMENSIONS)
					if mDims[2] ne 3 then message, 'MrMatrixTS dimensions must be NxMx3.'
					
					;Output size
					dims     = size( *self.data, /DIMENSIONS )
					outClass = mDims[1] eq 3 ? 'MrVectorTS' : 'MrTimeSeries'
					
					;Inner product
					;   - Reform Nx3 to NxMx3
					temp = total( left['DATA'] * $
					              rebin( reform(*self.data, [dims[0], 1, dims[1]]), [dims[0], mDims[1], dims[1]] ), $
					              3, /PRESERVE_TYPE )
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
function MrVectorTS::_OverloadPoundPound, left, right
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
	; SELF is LEFT /////////////////////////////////////////
	;-------------------------------------------------------
		if side eq 'LEFT' then begin
			case 1 of
				obj_isa(right, 'MRSCALARTS'): message, 'Operation now allowed: MrVectorTS # MrScalarTS.'
			
				obj_isa(right, 'MRVECTORTS'): begin
					;Output type
					outClass = 'MrMatrixTS'
					
					;Operate
					temp = [ [ [(*self.data)[*,0] * right[*,0]], [(*self.data)[*,0] * right[*,1]], [(*self.data)[*,0] * right[*,2]] ], $
					         [ [(*self.data)[*,1] * right[*,0]], [(*self.data)[*,1] * right[*,1]], [(*self.data)[*,1] * right[*,2]] ], $
					         [ [(*self.data)[*,2] * right[*,0]], [(*self.data)[*,2] * right[*,1]], [(*self.data)[*,2] * right[*,2]] ] ]
				endcase

				obj_isa(right, 'MRMATRIXTS'): begin
					;Make sure the matrix is the correct size
					mDims = size(right, /DIMENSIONS)
					if mDims[2] ne 3 then message, 'MrMatrixTS dimensions must be NxMx3.'
					
					;Output size
					dims     = size( *self.data, /DIMENSIONS )
					outClass = mDims[1] eq 3 ? 'MrVectorTS' : 'MrTimeSeries'
					
					;Outer product
					;   - Reform vector from Nx3 to NxMx3
					temp = total( rebin( reform(*self.data, [dims[0], 1, dims[1]]), [dims[0], mDims[1], dims[1]] ) * $
					              right['DATA'], 3, /PRESERVE_TYPE )
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
					;Output type
					outClass = 'MrMatrixTS'
					
					;Operate
					temp = [ [ [left[*,0] * (*self.data)[*,0]], [left[*,0] * (*self.data)[*,1]], [left[*,0] * (*self.data)[*,2]] ], $
					         [ [left[*,1] * (*self.data)[*,0]], [left[*,1] * (*self.data)[*,1]], [left[*,1] * (*self.data)[*,2]] ], $
					         [ [left[*,2] * (*self.data)[*,0]], [left[*,2] * (*self.data)[*,1]], [left[*,2] * (*self.data)[*,2]] ] ]
				endcase

				obj_isa(left, 'MRMATRIXTS'): begin
					;Make sure the matrix is the correct size
					mDims = size(left, /DIMENSIONS)
					if mDims[1] ne 3 then message, 'MrMatrixTS dimensions must be Nx3xM.'
					
					;Output size
					dims     = size( *self.data, /DIMENSIONS )
					outClass = mDims[2] eq 3 ? 'MrVectorTS' : 'MrTimeSeries'
					
					;Outer product
					;   - Reform vector from Nx3 to Nx3xM
					temp = total( left['DATA'] * $
					              rebin( *self.data, [dims[0], dims[1], mDims[2]] ), $
					              2, /PRESERVE_TYPE )
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
function MrVectorTS::_OverloadSlash, left, right
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
		name = 'Divide(' + left.name + ',' + right.name + ')'

	;-------------------------------------------------------
	; SELF is LEFT /////////////////////////////////////////
	;-------------------------------------------------------
		if side eq 'LEFT' then begin
			case 1 of
				obj_isa(right, 'MRSCALARTS'): begin
					;Output dimensions
					nPts     = self -> GetNPts()
					outDims  = [nPts, 3]
					outClass = 'MrVectorTS'
					
					;Operate
					temp = *self.data / rebin(right['DATA'], outDims)
				endcase
			
				obj_isa(right, 'MRVECTORTS'): begin
					outClass = 'MrVectorTS'
					temp     = *self.data / right['DATA']
				endcase

				obj_isa(right, 'MRMATRIXTS'): message, 'MrVectorTS / MrMatrixTS is not allowed.'
				else: message, 'Unrecognized object class: "' + obj_class(right) + '".'
			endcase

	;-------------------------------------------------------
	; SELF is RIGHT ////////////////////////////////////////
	;-------------------------------------------------------
		endif else begin
			case 1 of
				obj_isa(left, 'MRSCALARTS'): begin
					;Output dimensions
					nPts     = self -> GetNPts()
					outDims  = [nPts, 3]
					outClass = 'MrVectorTS'
					
					;Operate
					temp = rebin(left['DATA'], outDims) / *self.data
				endcase
			
				obj_isa(left, 'MRVECTORTS'): begin
					outClass = 'MrVectorTS'
					temp     = left['DATA'] / *self.data
				endcase

				obj_isa(left, 'MRMATRIXTS'): message, 'MrMatrixTS / MrVectorTS is not allowed.'
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
pro MrVectorTS::Append, time, data, type, $
BEFORE=before, $
NO_COPY=no_copy
	compile_opt idl2
	on_error, 2
	
	;Both must be column or row vectors
	nPts = n_elements(time)
	dims = size(data, /DIMENSIONS)
	if nPts ne dims[0] || dims[1] ne 3 || n_elements(dims) ne 2 $
		then message, 'DATA must be Nx3 where N is the number of elements in TIME.'

	;Let superclass do the rest
	self -> MrTimeSeries::Append, time, data, type, $
	                              BEFORE  = before, $
	                              NO_COPY = no_copy
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
	; MrVectorTS ///////////////////////////////////////////
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
		if n_elements(name) eq 0 then name = 'Cross(' + self.name + ',[' + strjoin(string(size(var, /DIMENSIONS), FORMAT='(i0)'), ',') + '])'
		
		;Cross
		AxB = MrVector_Cross(*self.data, var)
	endelse
	
	;Save as a new variable
	AxB = MrVectorTS(self.oTime, AxB, /NO_COPY, NAME=name, CACHE=cache)
	return, AxB
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
;                       A MrVectorTS object containing the filtered data.
;-
function MrVectorTS::Digital_Filter, fLow, fHigh, A, nTerms, $
CACHE=cache, $
DOUBLE=double, $
NAME=name
	compile_opt idl2
	on_error, 2
	
	if n_elements(name) eq 0 then name = 'Digital_Filter(' + self.name + ')'
	
	;Create the filter
	coeff = digital_filter(fLow, fHigh, A, nTerms, DOUBLE=double)
	
	;Apply the filter
	xTemp = convol(self[*,0], coeff)
	yTemp = convol(self[*,1], coeff)
	zTemp = convol(self[*,2], coeff)
	dTemp = [ [temporary(xTemp)], [temporary(yTemp)], [temporary(zTemp)] ]

	;Create the output variable
	varOut = MrVectorTS( self.oTime, dTemp, $
	                     CACHE     = cache, $
	                     DIMENSION = 1, $
	                     NAME      = name, $
	                     /NO_COPY )
	
	;Return the new variable
	return, varOut
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
		AdotB = MrVector_Dot(*self.data, var)
	endelse
	
	;Save as a new variable
	AdotB = MrScalarTS(self.oTime, AdotB, /NO_COPY, NAME=name, CACHE=cache)
	return, AdotB
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
function MrVectorTS::Interpol, Xout, t_type, $
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
	;Allocate memory
	y_out = make_array( [n_elements(t_out), 3], TYPE=size((*self.data)[0], /TYPE) )

	;Interpolate
	y_out[0,0] = Interpol( (*self.data)[*,0], t, t_out, $
	                       LSQUADRATIC = lsquadratic, $
	                       NAN         = nan, $
	                       QUADRATIC   = quadratic, $
	                       SPLINE      = spline )
	y_out[0,1] = Interpol( (*self.data)[*,1], t, t_out, $
	                       LSQUADRATIC = lsquadratic, $
	                       NAN         = nan, $
	                       QUADRATIC   = quadratic, $
	                       SPLINE      = spline )
	y_out[0,2] = Interpol( (*self.data)[*,2], temporary(t), temporary(t_out), $
	                       LSQUADRATIC = lsquadratic, $
	                       NAN         = nan, $
	                       QUADRATIC   = quadratic, $
	                       SPLINE      = spline )

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
	if n_elements(name) eq 0 then name = 'Magnitude(' + self.name + ')'

	;Compute the magnitude
	Vmag = sqrt(total(*self.data^2, 2))
	
	;Save as a new variable
	Vmag = MrScalarTS(self.oTime, Vmag, /NO_COPY, NAME=name, CACHE=cache)
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
	if n_elements(name) eq 0 then name = 'Normalize(' + self.name + ')'

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
	v_hat = MrVectorTS(self.oTime, hat, NAME=name, CACHE=cache, /NO_COPY)
	self -> CopyAttrTo, v_hat
	return, v_hat
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
pro MrVectorTS::SetData, time, data, $
_REF_EXTRA=extra
	compile_opt idl2
	on_error, 2
	
	;Keep the old data
	oTime = self.oTime
	pDAta = self.data

	;Use the superclass
	self -> MrTimeSeries::SetData, time, data, _STRICT_EXTRA=extra

	;Check the results
	;   - Make sure it is Nx3 (2 dimensions with second dimension of size 3)
	sz = size(self)
	if sz[0] ne 2 || sz[2] ne 3 then begin
		ptr_free, self.data
		self.data  = pData
		self.oTime = oTime
		message, 'Invalid dimensions: Data must be an Nx3 vector time series.'
	endif else begin
		ptr_free, pData
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
	oX = MrScalarTS(self.oTime, self[*,0], NAME=names[0])
	oY = MrScalarTS(self.oTime, self[*,1], NAME=names[1])
	oZ = MrScalarTS(self.oTime, self[*,2], NAME=names[2])
	
	;Copy Attributes
	self -> CopyAttrTo, oX
	self -> CopyAttrTo, oY
	self -> CopyAttrTo, oZ
	
	;Set color to scalar value
	oX['COLOR'] = 'Black'
	oY['COLOR'] = 'Black'
	oZ['COLOR'] = 'Black'
	
	;Remove (multi-elements) labels
	if oX -> HasAttr('LABEL') then oX -> RemoveAttr, 'LABEL'
	if oY -> HasAttr('LABEL') then oY -> RemoveAttr, 'LABEL'
	if oZ -> HasAttr('LABEL') then oZ -> RemoveAttr, 'LABEL'
	
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
	          inherits MrTimeSeries $
	        }
end