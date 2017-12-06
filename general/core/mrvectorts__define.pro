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
FUNCTION MrVectorTS::_OverloadAsterisk, left, right
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
			dims     = Size(*self.data, /DIMENSIONS)
			dims[0]  = nOther
			outClass = 'MrVectorTS'
		
			;Operate
			IF side EQ 'LEFT' $
				THEN temp = (*self.data) * Rebin( (right -> GetData()), dims ) $
				ELSE temp = Rebin( (left -> GetData()), dims ) * (*self.data)
			
		;VECTOR
		ENDIF ELSE IF Obj_IsA(type, 'MrVectorTS') THEN BEGIN
			temp     = (left -> GetData()) * (right -> GetData())
			outClass = 'MrVectorTS'
		
		;MATRIX
		ENDIF ELSE IF Obj_IsA(type, 'MrMatrixTS') THEN BEGIN
			Message, 'MrVectorTS * MrMatrixTS is not allowed.'
		
		;OTHER
		ENDIF ELSE BEGIN
			result   = self -> MrTimeSeries::_OverloadAsterisk(left, right)
			outClass = ''
		ENDELSE
		
		;Create a new variable
		IF outClass NE '' THEN BEGIN
			result = Obj_New( outClass, oTime, temp, $
			                  NAME = name, $
			                  /NO_COPY )
		ENDIF

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
FUNCTION MrVectorTS::_OverloadBracketsRightSide, isRange, i1, i2, i3, i4
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
	IF nSubs GT 2 THEN Message, 'Too many subscripts given.'
	
	;Data
	data = self -> _OverloadBracketsRightSide_Data(isRange, i1, i2)

	;Attributes
	outDims  = Size(data, /DIMENSIONS)
	nOutDims = Size(data, /N_DIMENSIONS)
	attrs    = self -> _OverloadBracketsRightSide_Attrs(outDims, isRange, i1)
	
;---------------------------------------------------------------------
; Create Output //////////////////////////////////////////////////////
;---------------------------------------------------------------------
	;Which MrTimeSeries object
	;   - The implicit array should start as Nx3
	;   - Scalars have a dimension size of 0 with 1 element.
	nDep0 = N_Elements( attrs['DEPEND_0'] )
	IF (outDims[0] EQ nDep0) || (N_Elements(data) EQ 1 && nDep0 EQ 1) THEN BEGIN
		;Scalar
		IF nOutDims LE 1 THEN BEGIN
			vOut = MrScalarTS( attrs['DEPEND_0'], data, $
			                   NAME='OverloadBRS(' + self.name + ')', $
			                   /NO_COPY )
		
		;Vector
		ENDIF ELSE IF outDims[1] EQ 3 THEN BEGIN
			vOut = MrVectorTS( attrs['DEPEND_0'], data, $
			                   NAME='OverloadBRS(' + self.name + ')', $
			                   /NO_COPY )
		
		;TimeSeries
		ENDIF ELSE BEGIN
			vOut = MrTimeSeries( attrs['DEPEND_0'], data, $
			                     NAME='OverloadBRS(' + self.name + ')', $
			                     /NO_COPY )
		ENDELSE
	
	;MrVariable
	ENDIF ELSE BEGIN
		;Issue warning
		MrPrintF, 'LogWarn', 'Unable to create MrTimeSeries object. Converting to MrVariable.'
		
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
FUNCTION MrVectorTS::_OverloadCaret, left, right
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
			dims     = Size(*self.data, /DIMENSIONS)
			dims[0]  = nOther
			outClass = 'MrVectorTS'
		
			;Operate
			IF side EQ 'LEFT' $
				THEN temp = (*self.data) ^ Rebin( (right -> GetData()), dims ) $
				ELSE temp = Rebin( (left -> GetData()), dims ) ^ (*self.data)
			
		;VECTOR
		ENDIF ELSE IF Obj_IsA(type, 'MrVectorTS') THEN BEGIN
			temp     = (left -> GetData()) ^ (right -> GetData())
			outClass = 'MrVectorTS'
		
		;MATRIX
		ENDIF ELSE IF Obj_IsA(type, 'MrMatrixTS') THEN BEGIN
			Message, 'MrVectorTS ^ MrMatrixTS is not allowed.'
		
		;OTHER
		ENDIF ELSE BEGIN
			result   = self -> MrTimeSeries::_OverloadCaret(left, right)
			outClass = ''
		ENDELSE
		
		;Create a new variable
		IF outClass NE '' THEN BEGIN
			result = Obj_New( outClass, oTime, temp, $
			                  NAME = name, $
			                  /NO_COPY )
		ENDIF

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
FUNCTION MrVectorTS::_OverloadMinus, left, right
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
			dims     = Size(*self.data, /DIMENSIONS)
			dims[0]  = nOther
			outClass = 'MrVectorTS'
		
			;Operate
			IF side EQ 'LEFT' $
				THEN temp = (*self.data) - Rebin( (right -> GetData()), dims ) $
				ELSE temp = Rebin( (left -> GetData()), dims ) - (*self.data)
			
		;VECTOR
		ENDIF ELSE IF Obj_IsA(type, 'MrVectorTS') THEN BEGIN
			temp     = (left -> GetData()) - (right -> GetData())
			outClass = 'MrVectorTS'
		
		;MATRIX
		ENDIF ELSE IF Obj_IsA(type, 'MrMatrixTS') THEN BEGIN
			Message, 'MrVectorTS - MrMatrixTS is not allowed.'
		
		;OTHER
		ENDIF ELSE BEGIN
			result   = self -> MrTimeSeries::_OverloadMinus(left, right)
			outClass = ''
		ENDELSE
		
		;Create a new variable
		IF outClass NE '' THEN BEGIN
			result = Obj_New( outClass, oTime, temp, $
			                  NAME = name, $
			                  /NO_COPY )
		ENDIF

;-------------------------------------------------------
; Expression or MrVariable /////////////////////////////
;-------------------------------------------------------
	ENDIF ELSE BEGIN
		result   = self -> MrTimeSeries::_OverloadMinus(left, right)
	ENDELSE

;-------------------------------------------------------
; Done /////////////////////////////////////////////////
;-------------------------------------------------------
	RETURN, result
END


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
FUNCTION MrVectorTS::_OverloadMOD, left, right
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
			dims     = Size(*self.data, /DIMENSIONS)
			dims[0]  = nOther
			outClass = 'MrVectorTS'
		
			;Operate
			IF side EQ 'LEFT' $
				THEN temp = (*self.data) MOD Rebin( (right -> GetData()), dims ) $
				ELSE temp = Rebin( (left -> GetData()), dims ) MOD (*self.data)
			
		;VECTOR
		ENDIF ELSE IF Obj_IsA(type, 'MrVectorTS') THEN BEGIN
			temp     = (left -> GetData()) MOD (right -> GetData())
			outClass = 'MrVectorTS'
		
		;MATRIX
		ENDIF ELSE IF Obj_IsA(type, 'MrMatrixTS') THEN BEGIN
			Message, 'MrVectorTS MOD MrMatrixTS is not allowed.'
		
		;OTHER
		ENDIF ELSE BEGIN
			result   = self -> MrTimeSeries::_OverloadMOD(left, right)
			outClass = ''
		ENDELSE
		
		;Create a new variable
		IF outClass NE '' THEN BEGIN
			result = Obj_New( outClass, oTime, temp, $
			                  NAME = name, $
			                  /NO_COPY )
		ENDIF

;-------------------------------------------------------
; Expression or MrVariable /////////////////////////////
;-------------------------------------------------------
	ENDIF ELSE BEGIN
		result   = self -> MrTimeSeries::_OverloadMOD(left, right)
	ENDELSE

;-------------------------------------------------------
; Done /////////////////////////////////////////////////
;-------------------------------------------------------
	RETURN, result
END


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
FUNCTION MrVectorTS::_OverloadPlus, left, right
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
			dims     = Size(*self.data, /DIMENSIONS)
			dims[0]  = nOther
			outClass = 'MrVectorTS'
		
			;Operate
			IF side EQ 'LEFT' $
				THEN temp = (*self.data) + Rebin( (right -> GetData()), dims ) $
				ELSE temp = Rebin( (left -> GetData()), dims ) + (*self.data)
			
		;VECTOR
		ENDIF ELSE IF Obj_IsA(type, 'MrVectorTS') THEN BEGIN
			temp     = (left -> GetData()) + (right -> GetData())
			outClass = 'MrVectorTS'
		
		;MATRIX
		ENDIF ELSE IF Obj_IsA(type, 'MrMatrixTS') THEN BEGIN
			Message, 'MrVectorTS + MrMatrixTS is not allowed.'
		
		;OTHER
		ENDIF ELSE BEGIN
			result   = self -> MrTimeSeries::_OverloadPlus(left, right)
			outClass = ''
		ENDELSE
		
		;Create a new variable
		IF outClass NE '' THEN BEGIN
			result = Obj_New( outClass, oTime, temp, $
			                  NAME = name, $
			                  /NO_COPY )
		ENDIF

;-------------------------------------------------------
; Expression or MrVariable /////////////////////////////
;-------------------------------------------------------
	ENDIF ELSE BEGIN
		result   = self -> MrTimeSeries::_OverloadPlus(left, right)
	ENDELSE

;-------------------------------------------------------
; Done /////////////////////////////////////////////////
;-------------------------------------------------------
	RETURN, result
END


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
FUNCTION MrVectorTS::_OverloadPound, left, right
	Compile_Opt idl2
	On_Error, 2

;-------------------------------------------------------
; Two MrTimeSeries Objects /////////////////////////////
;-------------------------------------------------------
	IF IsA(left, 'MrTimeSeries') && IsA(right, 'MrTimeSeries') THEN BEGIN
		;Pick a name and a side
		name = 'Pound(' + left.name + ',' + right.name + ')'
		side = self -> _LeftOrRight(left, right)
		
		;Distinguish between the SELF object and the OTHER object
		oOther = side EQ 'LEFT' ? right : left
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
			Message, 'The # operation not allowed between MrVectorTS & MrScalarTS.'
			
		;VECTOR
		ENDIF ELSE IF Obj_IsA(type, 'MrVectorTS') THEN BEGIN
			;Compute the inner product.
			temp     = Total( (left -> GetData()) * (right -> GetData()), 2, /PRESERVE_TYPE )
			outClass = 'MrScalarTS'
		
		;MATRIX
		ENDIF ELSE IF Obj_IsA(type, 'MrMatrixTS') THEN BEGIN
			;Make sure the matrix is the correct size
			mDims = Size(oOther, /DIMENSIONS)
			vDims = Size( *self.data, /DIMENSIONS )
			
			;Inner product
			IF side EQ 'LEFT' THEN BEGIN
				;Nx3 # Nx3xM
				IF mDims[1] NE 3 THEN Message, 'MrMatrixTS dimensions must be Nx3xM.'
				outClass = mDims[2] EQ 3 ? 'MrVectorTS' : 'MrTimeSeries'
				temp = Total( Rebin( *self.data, [vDims[0], vDims[1], mDims[2]] ) * $
				              (right -> GetData()), 2, /PRESERVE_TYPE )
			ENDIF ELSE BEGIN
				;NxMx3 # Nx3
				IF mDims[2] NE 3 THEN Message, 'MrMatrixTS dimensions must be NxMx3.'
				outClass = mDims[1] EQ 3 ? 'MrVectorTS' : 'MrTimeSeries'
				temp = Total( (left -> GetData()) * $
				              Rebin( Reform( *self.data, [vDims[0], 1, vDims[1]] ), [vDims[0], vDims[1], mDims[2]] ), $
				              3, /PRESERVE_TYPE )
			ENDELSE
		
		;OTHER
		ENDIF ELSE BEGIN
			result   = self -> MrTimeSeries::_OverloadPound(left, right)
			outClass = ''
		ENDELSE
		
		;Create a new variable
		IF outClass NE '' THEN BEGIN
			result = Obj_New( outClass, oTime, temp, $
			                  NAME = name, $
			                  /NO_COPY )
		ENDIF

;-------------------------------------------------------
; Expression or MrVariable /////////////////////////////
;-------------------------------------------------------
	ENDIF ELSE BEGIN
		result = self -> MrTimeSeries::_OverloadPound(left, right)
	ENDELSE

;-------------------------------------------------------
; Done /////////////////////////////////////////////////
;-------------------------------------------------------
	RETURN, result
END


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
FUNCTION MrVectorTS::_OverloadPoundPound, left, right
	Compile_Opt idl2
	On_Error, 2

;-------------------------------------------------------
; Two MrTimeSeries Objects /////////////////////////////
;-------------------------------------------------------
	IF IsA(left, 'MrTimeSeries') && IsA(right, 'MrTimeSeries') THEN BEGIN
		;Pick a name and a side
		name = 'PoundPound(' + left.name + ',' + right.name + ')'
		side = self -> _LeftOrRight(left, right)
		
		;Distinguish between the SELF object and the OTHER object
		oOther = side EQ 'LEFT' ? right : left
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
			Message, 'The ## operation not allowed between MrVectorTS & MrScalarTS.'
			
		;VECTOR
		ENDIF ELSE IF Obj_IsA(type, 'MrVectorTS') THEN BEGIN
			;Output type
			outClass = 'MrMatrixTS'
			temp     = [ [ [left['DATA',*,0] * right['DATA',*,0]], [left['DATA',*,0] * right['DATA',*,1]], [left['DATA',*,0] * right['DATA',*,2]] ], $
			             [ [left['DATA',*,1] * right['DATA',*,0]], [left['DATA',*,1] * right['DATA',*,1]], [left['DATA',*,1] * right['DATA',*,2]] ], $
			             [ [left['DATA',*,2] * right['DATA',*,0]], [left['DATA',*,2] * right['DATA',*,1]], [left['DATA',*,2] * right['DATA',*,2]] ] ]
		
		;MATRIX
		ENDIF ELSE IF Obj_IsA(type, 'MrMatrixTS') THEN BEGIN
			;Make sure the matrix is the correct size
			mDims = Size(oOther, /DIMENSIONS)
			vDims = Size( *self.data, /DIMENSIONS )
			
			;Inner product
			IF side EQ 'LEFT' THEN BEGIN
				;Nx3 ## NxMx3
				IF mDims[2] NE 3 THEN Message, 'MrMatrixTS dimensions must be NxMx3.'
				outClass = mDims[1] EQ 3 ? 'MrVectorTS' : 'MrTimeSeries'
				temp = Total( Rebin( Reform( *self.data, [vDims[0], 1, vDims[1]] ), [vDims[0], vDims[1], mDims[2]] ) * $
				              (right -> GetData()), $
				              3, /PRESERVE_TYPE )
			ENDIF ELSE BEGIN
				;Nx3xM ## Nx3
				IF mDims[1] NE 3 THEN Message, 'MrMatrixTS dimensions must be Nx3xM.'
				outClass = mDims[2] EQ 3 ? 'MrVectorTS' : 'MrTimeSeries'
				temp = Total( (left -> GetData()) * $
				              Rebin( *self.data, [vDims[0], vDims[1], mDims[2]] ), $
				              2, /PRESERVE_TYPE )
			ENDELSE
		
		;OTHER
		ENDIF ELSE BEGIN
			result   = self -> MrTimeSeries::_OverloadPoundPound(left, right)
			outClass = ''
		ENDELSE
		
		;Create a new variable
		IF outClass NE '' THEN BEGIN
			result = Obj_New( outClass, oTime, temp, $
			                  NAME = name, $
			                  /NO_COPY )
		ENDIF

;-------------------------------------------------------
; Expression or MrVariable /////////////////////////////
;-------------------------------------------------------
	ENDIF ELSE BEGIN
		result = self -> MrTimeSeries::_OverloadPoundPound(left, right)
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
FUNCTION MrVectorTS::_OverloadSlash, left, right
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
			dims     = Size(*self.data, /DIMENSIONS)
			dims[0]  = nOther
			outClass = 'MrVectorTS'
		
			;Operate
			IF side EQ 'LEFT' $
				THEN temp = (*self.data) / Rebin( (right -> GetData()), dims ) $
				ELSE temp = Rebin( (left -> GetData()), dims ) / (*self.data)
			
		;VECTOR
		ENDIF ELSE IF Obj_IsA(type, 'MrVectorTS') THEN BEGIN
			temp     = (left -> GetData()) / (right -> GetData())
			outClass = 'MrVectorTS'
		
		;MATRIX
		ENDIF ELSE IF Obj_IsA(type, 'MrMatrixTS') THEN BEGIN
			Message, 'MrVectorTS / MrMatrixTS is not allowed.'
		
		;OTHER
		ENDIF ELSE BEGIN
			result   = self -> MrTimeSeries::_OverloadSlash(left, right)
			outClass = ''
		ENDELSE
		
		;Create a new variable
		IF outClass NE '' THEN BEGIN
			result = Obj_New( outClass, oTime, temp, $
			                  NAME = name, $
			                  /NO_COPY )
		ENDIF

;-------------------------------------------------------
; Expression or MrVariable /////////////////////////////
;-------------------------------------------------------
	ENDIF ELSE BEGIN
		result   = self -> MrTimeSeries::_OverloadSlash(left, right)
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
	key  = n_elements(key) eq 0 ? 0 : key+1

	;Get the array element if the index is in range
	if key lt nPts then begin
		next = 1
		value = reform((*self.data)[key,*])
	
	;Otherwise, stop iterating 
	endif else next = 0

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
			AxB = [ [self['DATA',*,1]*var['DATA',*,2] - self['DATA',*,2]*var['DATA',*,1]], $
			        [self['DATA',*,2]*var['DATA',*,0] - self['DATA',*,0]*var['DATA',*,2]], $
			        [self['DATA',*,0]*var['DATA',*,1] - self['DATA',*,1]*var['DATA',*,0]] ]

	;-------------------------------------------------------
	; MrVariable ///////////////////////////////////////////
	;-------------------------------------------------------
		endif else if obj_isa(var, 'MrVariable') then begin
			
			;Nx3 cross 3xN (treat 3x3 as Nx3
			sz = size(var)
			if (sz[0] eq 1 && sz[1] eq 3) || (sz[0] eq 2 && sz[1] eq 3 && sz[2] ne 3) then begin
				
				;Cross
				;   - Data will be transposed to Nx3 by MrVectorTS() below
				AxB = [ [self['DATA',*,1]*var['DATA',2,*] - self['DATA',*,2]*var['DATA',1,*]], $
				        [self['DATA',*,2]*var['DATA',0,*] - self['DATA',*,0]*var['DATA',2,*]], $
				        [self['DATA',*,0]*var['DATA',1,*] - self['DATA',*,1]*var['DATA',0,*]] ]
			
			;Nx3 cross Nx3
			endif else if (sz[0] eq 2 && sz[2] eq 3) then begin
				
				;Cross
				;   - Data will be transposed to Nx3 by MrVectorTS() below
				AxB = [ [self['DATA',*,1]*var['DATA',*,2] - self['DATA',*,2]*var['DATA',*,1]], $
				        [self['DATA',*,2]*var['DATA',*,0] - self['DATA',*,0]*var['DATA',*,2]], $
				        [self['DATA',*,0]*var['DATA',*,1] - self['DATA',*,1]*var['DATA',*,0]] ]
			
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
	xTemp = convol(self['DATA',*,0], coeff)
	yTemp = convol(self['DATA',*,1], coeff)
	zTemp = convol(self['DATA',*,2], coeff)
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
			AdotB = (*self.data)[*,0] * var['DATA',*,0] + $
			        (*self.data)[*,1] * var['DATA',*,1] + $
			        (*self.data)[*,2] * var['DATA',*,2]

	;-------------------------------------------------------
	; MrVariable ///////////////////////////////////////////
	;-------------------------------------------------------
		endif else if obj_isa(var, 'MrVariable') then begin
			
			;Nx3 cross 3xN
			sz = size(var)
			if (sz[0] eq 1 && sz[1] eq 3) || (sz[0] eq 2 && sz[1] eq 3) then begin
				
				;Dot
				AdotB = (*self.data)[*,0] * var['DATA',0,*] + $
				        (*self.data)[*,1] * var['DATA',1,*] + $
				        (*self.data)[*,2] * var['DATA',2,*]
			
			;Nx3 cross Nx3
			endif else if (sz[0] eq 2 && sz[2] eq 3) then begin
				
				;Dot
				AdotB = (*self.data)[*,0] * var['DATA',*,0] + $
				        (*self.data)[*,1] * var['DATA',*,1] + $
				        (*self.data)[*,2] * var['DATA',*,2]
			
			;Incompatible dimensions
			endif else begin
				message, 'VAR must be 3xN or Nx3'
			endelse

	;-------------------------------------------------------
	; Unrecognized /////////////////////////////////////////
	;-------------------------------------------------------
		endif else begin
			message, 'MrVectorTS cannot be dotted with object class "' + obj_class(var) + '".'
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
	
	oXout = MrVar_Get(Xout)
	if n_elements(name) eq 0 then name = 'Interpol(' + self.name + ')'
	
;-------------------------------------------------------
; MrVariable Object ////////////////////////////////////
;-------------------------------------------------------
	if IsA(oXout, 'MrVariable') then begin
		;Get the time variables
		oT = self.oTime
		if obj_isa(oXout, 'MrTimeVar') $
			then oTout = Xout $
			else oTout = MrVar_Get(oXout['DEPEND_0'])
		
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
		t_out = oXout
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
	hat  = make_array(nPts, 3, TYPE=size(mag[0]*(*self.data)[0], /TYPE))
	
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
	oX = self[*,0]
	oY = self[*,1]
	oZ = self[*,2]
	
	;Name
	oX -> SetName, names[0]
	oY -> SetName, names[1]
	oZ -> SetName, names[2]
	
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