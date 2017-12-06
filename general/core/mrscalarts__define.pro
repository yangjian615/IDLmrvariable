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
;       2017/03/31  -   Testing revealed VAR->GetData() is faster than VAR['DATA'],
;                           so the change was made in all ::_Overload methods. - MRA
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
FUNCTION MrScalarTS::_OverloadAsterisk, left, right
	Compile_Opt idl2
	On_Error, 2
	
;-------------------------------------------------------
; Two MrTimeSeries Objects /////////////////////////////
;-------------------------------------------------------
	IF IsA(left, 'MrTimeSeries') && IsA(right, 'MrTimeSeries') THEN BEGIN
		;Pick a name and a side
		name = 'Multiply(' + left.name + ',' + right.name + ')'
		side = self -> _LeftOrRight(left, right)
		
		;Distinguish between the SELF object and the OTHER object
		oOther = side EQ 'LEFT' ? right : left
		dims   = Size(oOther, /DIMENSIONS)
		type   = Obj_Class(oOther)
		
		;Number of points
		;   - If times are different, output will be truncated to fewer number of points
		;   - TODO: Check if time stamps are equal?
		;           If different: Interpolate? Error?
		nSelf  = self   -> GetNPts()
		nOther = oOther -> GetNPts()
		oTime  = nSelf LT nOther ? self['TIMEVAR'] : oOther['TIMEVAR']
		nPts   = nSelf < nOther
		
		;SCALAR
		IF Obj_IsA(type, 'MrScalarTS') THEN BEGIN
			temp     = (left -> GetData()) * (right -> GetData())
			outClass = 'MrScalarTS'
			
		;VECTOR, MATRIX, OTHER
		ENDIF ELSE BEGIN
			;Output dimensions
			dims[0]  = nSelf
			outClass = type
		
			;Operate
			IF side EQ 'LEFT' $
				THEN temp = Rebin(*self.data, dims) * (right -> GetData()) $
				ELSE temp = (left -> GetData()) * Rebin(*self.data, dims)
		ENDELSE
		
		;Create a new variable
		result = Obj_New( outClass, oTime, temp, $
		                  NAME = name, $
		                  /NO_COPY )

;-------------------------------------------------------
; Expression or MrVariable /////////////////////////////
;-------------------------------------------------------
	ENDIF ELSE BEGIN
		result   = self -> MrTimeSeries::_OverloadAsterisk(left, right)
	ENDELSE

;-------------------------------------------------------
; Done /////////////////////////////////////////////////
;-------------------------------------------------------
	RETURN, result
END


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
;       time      = oTSvar['TIME', i2]
;       time      = oTSvar['TIME', i2, <StrMid>]
;       time      = oTSvar['TIME', i2, T_TYPE]
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
FUNCTION MrScalarTS::_OverloadBracketsRightSide, isRange, i1, i2, i3, i4
	Compile_Opt idl2
	On_Error, 2
	
	;Number of subscripts given
	nSubs = N_Elements(isRange)
	
	;String operations
	IF IsA(i1, /SCALAR, 'STRING') THEN BEGIN
		RETURN, self -> MrTimeSeries::_OverloadBracketsRightSide(isRange, i1, i2, i3, i4, i5, i6, i7, i8)
	
	;Scalar operations
	;   - 0   returns the self object
	;   - [0] returns the first data element
	;   - All other cases return data
	ENDIF ELSE IF nSubs EQ 1 && isRange[0] EQ 0 && IsA(i1, /SCALAR) && i1 EQ 0 THEN BEGIN
		RETURN, self
	ENDIF

;---------------------------------------------------------------------
; Extract the Subarray ///////////////////////////////////////////////
;---------------------------------------------------------------------
	IF nSubs GT 1 THEN Message, 'Too many subscripts given.'
	
	;Data
	data = self -> _OverloadBracketsRightSide_Data(isRange, i1)

	;Attributes
	outDims = Size(data, /DIMENSIONS)
	attrs   = self -> _OverloadBracketsRightSide_Attrs(outDims, isRange, i1)
	
;---------------------------------------------------------------------
; Create Output //////////////////////////////////////////////////////
;---------------------------------------------------------------------
	;MrScalarTS
	;   - Scalars have a dimension size of 0 with 1 element.
	nDep0 = N_Elements( attrs['DEPEND_0'] )
	IF (outDims[0] EQ nDep0) || (N_Elements(data) EQ 1 && nDep0 EQ 1) THEN BEGIN
		vOut = MrScalarTS( attrs['DEPEND_0'], data, $
		                   NAME='OverloadBRS(' + self.name + ')', $
		                   /NO_COPY )
	
	;MrVariable
	ENDIF ELSE BEGIN
		;Issue warning
		MrPrintF, 'LogWarn', 'Unable to create MrScalarTS object. Converting to MrVariable.'
		
		;Create variable
		vOut = MrVariable( data, $
		                   NAME='OverloadBRS(' + self.name + ')', $
		                   /NO_COPY )
	ENDELSE
	
	;Copy all attributes
	self -> CopyAttrTo, vOut
	
	;Update attributes
	vOut -> SetAttrValue, attrs, /OVERWRITE
;-------------------------------------------
; Done! ////////////////////////////////////
;-------------------------------------------

	RETURN, vOut
END


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
FUNCTION MrScalarTS::_OverloadCaret, left, right
	Compile_Opt idl2
	On_Error, 2
	
;-------------------------------------------------------
; Two MrTimeSeries Objects /////////////////////////////
;-------------------------------------------------------
	IF IsA(left, 'MrTimeSeries') && IsA(right, 'MrTimeSeries') THEN BEGIN
		;Pick a name and a side
		name = 'Caret(' + left.name + ',' + right.name + ')'
		side = self -> _LeftOrRight(left, right)
		
		;Distinguish between the SELF object and the OTHER object
		oOther = side EQ 'LEFT' ? right : left
		dims   = Size(oOther, /DIMENSIONS)
		type   = Obj_Class(oOther)
		
		;Number of points
		;   - If times are different, output will be truncated to fewer number of points
		;   - TODO: Check if time stamps are equal?
		;           If different: Interpolate? Error?
		nSelf  = self   -> GetNPts()
		nOther = oOther -> GetNPts()
		oTime  = nSelf LT nOther ? self['TIMEVAR'] : oOther['TIMEVAR']
		nPts   = nSelf < nOther
		
		;SCALAR
		IF Obj_IsA(type, 'MrScalarTS') THEN BEGIN
			temp     = (left -> GetData()) ^ (right -> GetData())
			outClass = 'MrScalarTS'
			
		;VECTOR, MATRIX, OTHER
		ENDIF ELSE BEGIN
			;Output dimensions
			dims[0]  = nSelf
			outClass = type
		
			;Operate
			IF side EQ 'LEFT' $
				THEN temp = Rebin(*self.data, dims) ^ (right -> GetData()) $
				ELSE temp = (left -> GetData()) ^ Rebin(*self.data, dims)
		ENDELSE
		
		;Create a new variable
		result = Obj_New( outClass, oTime, temp, $
		                  NAME = name, $
		                  /NO_COPY )

;-------------------------------------------------------
; Expression or MrVariable /////////////////////////////
;-------------------------------------------------------
	ENDIF ELSE BEGIN
		result   = self -> MrTimeSeries::_OverloadCaret(left, right)
	ENDELSE

;-------------------------------------------------------
; Done /////////////////////////////////////////////////
;-------------------------------------------------------
	RETURN, result
END


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
FUNCTION MrScalarTS::_OverloadMinus, left, right
	Compile_Opt idl2
	On_Error, 2
	
;-------------------------------------------------------
; Two MrTimeSeries Objects /////////////////////////////
;-------------------------------------------------------
	IF IsA(left, 'MrTimeSeries') && IsA(right, 'MrTimeSeries') THEN BEGIN
		;Pick a name and a side
		name = 'Minus(' + left.name + ',' + right.name + ')'
		side = self -> _LeftOrRight(left, right)
		
		;Distinguish between the SELF object and the OTHER object
		oOther = side EQ 'LEFT' ? right : left
		dims   = Size(oOther, /DIMENSIONS)
		type   = Obj_Class(oOther)
		
		;Number of points
		;   - If times are different, output will be truncated to fewer number of points
		;   - TODO: Check if time stamps are equal?
		;           If different: Interpolate? Error?
		nSelf  = self   -> GetNPts()
		nOther = oOther -> GetNPts()
		oTime  = nSelf LT nOther ? self['TIMEVAR'] : oOther['TIMEVAR']
		nPts   = nSelf < nOther
		
		;SCALAR
		IF Obj_IsA(type, 'MrScalarTS') THEN BEGIN
			temp     = (left -> GetData()) - (right -> GetData())
			outClass = 'MrScalarTS'
			
		;VECTOR, MATRIX, OTHER
		ENDIF ELSE BEGIN
			;Output dimensions
			dims[0]  = nSelf
			outClass = type
		
			;Operate
			IF side EQ 'LEFT' $
				THEN temp = Rebin(*self.data, dims) - (right -> GetData()) $
				ELSE temp = (left -> GetData()) - Rebin(*self.data, dims)
		ENDELSE
		
		;Create a new variable
		result = Obj_New( outClass, oTime, temp, $
		                  NAME = name, $
		                  /NO_COPY )

;-------------------------------------------------------
; Expression or MrVariable /////////////////////////////
;-------------------------------------------------------
	ENDIF ELSE BEGIN
		result = self -> MrTimeSeries::_OverloadMinus(left, right)
	ENDELSE

;-------------------------------------------------------
; Done /////////////////////////////////////////////////
;-------------------------------------------------------
	RETURN, result
END


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
FUNCTION MrScalarTS::_OverloadMOD, left, right
	Compile_Opt idl2
	On_Error, 2
	
;-------------------------------------------------------
; Two MrTimeSeries Objects /////////////////////////////
;-------------------------------------------------------
	IF IsA(left, 'MrTimeSeries') && IsA(right, 'MrTimeSeries') THEN BEGIN
		;Pick a name and a side
		name = 'Mod(' + left.name + ',' + right.name + ')'
		side = self -> _LeftOrRight(left, right)
		
		;Distinguish between the SELF object and the OTHER object
		oOther = side EQ 'LEFT' ? right : left
		dims   = Size(oOther, /DIMENSIONS)
		type   = Obj_Class(oOther)
		
		;Number of points
		;   - If times are different, output will be truncated to fewer number of points
		;   - TODO: Check if time stamps are equal?
		;           If different: Interpolate? Error?
		nSelf  = self   -> GetNPts()
		nOther = oOther -> GetNPts()
		oTime  = nSelf LT nOther ? self['TIMEVAR'] : oOther['TIMEVAR']
		nPts   = nSelf < nOther
		
		;SCALAR
		IF Obj_IsA(type, 'MrScalarTS') THEN BEGIN
			temp     = (left -> GetData()) MOD (right -> GetData())
			outClass = 'MrScalarTS'
			
		;VECTOR, MATRIX, OTHER
		ENDIF ELSE BEGIN
			;Output dimensions
			dims[0]  = nSelf
			outClass = type
		
			;Operate
			IF side EQ 'LEFT' $
				THEN temp = Rebin(*self.data, dims) MOD (right -> GetData()) $
				ELSE temp = (left -> GetData()) MOD Rebin(*self.data, dims)
		ENDELSE
		
		;Create a new variable
		result = Obj_New( outClass, oTime, temp, $
		                  NAME = name, $
		                  /NO_COPY )

;-------------------------------------------------------
; Expression or MrVariable /////////////////////////////
;-------------------------------------------------------
	ENDIF ELSE BEGIN
		result = self -> MrTimeSeries::_OverloadMOD(left, right)
	ENDELSE

;-------------------------------------------------------
; Done /////////////////////////////////////////////////
;-------------------------------------------------------
	RETURN, result
END


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
FUNCTION MrScalarTS::_OverloadPlus, left, right
	Compile_Opt idl2
	On_Error, 2
	
;-------------------------------------------------------
; Two MrTimeSeries Objects /////////////////////////////
;-------------------------------------------------------
	IF IsA(left, 'MrTimeSeries') && IsA(right, 'MrTimeSeries') THEN BEGIN
		;Pick a name and a side
		name = 'Plus(' + left.name + ',' + right.name + ')'
		side = self -> _LeftOrRight(left, right)
		
		;Distinguish between the SELF object and the OTHER object
		oOther = side EQ 'LEFT' ? right : left
		dims   = Size(oOther, /DIMENSIONS)
		type   = Obj_Class(oOther)
		
		;Number of points
		;   - If times are different, output will be truncated to fewer number of points
		;   - TODO: Check if time stamps are equal?
		;           If different: Interpolate? Error?
		nSelf  = self   -> GetNPts()
		nOther = oOther -> GetNPts()
		oTime  = nSelf LT nOther ? self['TIMEVAR'] : oOther['TIMEVAR']
		nPts   = nSelf < nOther
		
		;SCALAR
		IF Obj_IsA(type, 'MrScalarTS') THEN BEGIN
			temp     = (left -> GetData()) + (right -> GetData())
			outClass = 'MrScalarTS'
			
		;VECTOR, MATRIX, OTHER
		ENDIF ELSE BEGIN
			;Output dimensions
			dims[0]  = nSelf
			outClass = type
		
			;Operate
			IF side EQ 'LEFT' $
				THEN temp = Rebin(*self.data, dims) + (right -> GetData()) $
				ELSE temp = (left -> GetData()) + Rebin(*self.data, dims)
		ENDELSE
		
		;Create a new variable
		result = Obj_New( outClass, oTime, temp, $
		                  NAME = name, $
		                  /NO_COPY )

;-------------------------------------------------------
; Expression or MrVariable /////////////////////////////
;-------------------------------------------------------
	ENDIF ELSE BEGIN
		result = self -> MrTimeSeries::_OverloadPlus(left, right)
	ENDELSE

;-------------------------------------------------------
; Done /////////////////////////////////////////////////
;-------------------------------------------------------
	RETURN, result
END


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
FUNCTION MrScalarTS::_OverloadSlash, left, right
	Compile_Opt idl2
	On_Error, 2
	
;-------------------------------------------------------
; Two MrTimeSeries Objects /////////////////////////////
;-------------------------------------------------------
	IF IsA(left, 'MrTimeSeries') && IsA(right, 'MrTimeSeries') THEN BEGIN
		;Pick a name and a side
		name = 'Divide(' + left.name + ',' + right.name + ')'
		side = self -> _LeftOrRight(left, right)
		
		;Distinguish between the SELF object and the OTHER object
		oOther = side EQ 'LEFT' ? right : left
		dims   = Size(oOther, /DIMENSIONS)
		type   = Obj_Class(oOther)
		
		;Number of points
		;   - If times are different, output will be truncated to fewer number of points
		;   - TODO: Check if time stamps are equal?
		;           If different: Interpolate? Error?
		nSelf  = self   -> GetNPts()
		nOther = oOther -> GetNPts()
		oTime  = nSelf LT nOther ? self['TIMEVAR'] : oOther['TIMEVAR']
		nPts   = nSelf < nOther
		
		;SCALAR
		IF Obj_IsA(type, 'MrScalarTS') THEN BEGIN
			temp     = (left -> GetData()) / (right -> GetData())
			outClass = 'MrScalarTS'
			
		;VECTOR, MATRIX, OTHER
		ENDIF ELSE BEGIN
			;Output dimensions
			dims[0]  = nSelf
			outClass = type
		
			;Operate
			IF side EQ 'LEFT' $
				THEN temp = Rebin(*self.data, dims) / (right -> GetData()) $
				ELSE temp = (left -> GetData()) / Rebin(*self.data, dims)
		ENDELSE
		
		;Create a new variable
		result = Obj_New( outClass, oTime, temp, $
		                  NAME = name, $
		                  /NO_COPY )

;-------------------------------------------------------
; Expression or MrVariable /////////////////////////////
;-------------------------------------------------------
	ENDIF ELSE BEGIN
		result = self -> MrTimeSeries::_OverloadSlash(left, right)
	ENDELSE

;-------------------------------------------------------
; Done /////////////////////////////////////////////////
;-------------------------------------------------------
	RETURN, result
END


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
		!Null = min( [ oT['DATA', 0, 'JULDAY'], oTout['DATA', 0, 'JULDAY'] ], iMin )
		if iMin eq 0 $
			then t_ref = oT['DATA', 0] $
			else t_ref = oTout['DATA', 0]
		
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
	;   - Scalar values have dimension size of 0
	if size(self, /N_DIMENSIONS) eq 1 || n_elements(*self.data) eq 1 then begin
		ptr_free, pData
	endif else begin
		ptr_free, self.data
		self.data  = pData
		self.oTime = oTime
		message, 'Invalid dimensions: Data must be an Nx1 scalar time series.'
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