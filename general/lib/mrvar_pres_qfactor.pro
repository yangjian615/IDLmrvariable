; docformat = 'rst'
;
; NAME:
;       MrVar_Pres_QFactor
;
;*****************************************************************************************
;   Copyright (c) 2017, Matthew Argall                                                   ;
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
; PURPOSE:
;+
;   Compute the non-gyrotropy factor.
;
;   References:
;       Swisdak, M. (2016), Quantifying gyrotropy in magnetic reconnection,
;           Geophys. Res. Lett., 43(1), 43–49, doi:10.1002/2015GL066980.
;
; :Categories:
;   MrVariable
;
; :Params:
;       B:              in, required, type=string/integer/objref
;                       Name, number, or MrVectorTS object of the vector magnetic field.
;                           If B and `P` do not have identical time tags, B is interpolated
;                           to 'P`.
;       P:              in, required, type=string/integer/objref
;                       Name, number, or MrMatrixTS object of the pressure tensor.
;
; :Keywords:
;       CACHE:          in, optional, type=boolean, default=0
;                       If set, the output will be added to the variable cache.
;       NAME:           in, optional, type=string, default='Cyclotron_Frequency'
;                       Name to be given to the output variable.
;       NO_CLOBBER:     in, optional, type=boolean, default=0
;                       If set, cached variables with name `NAME` are not over-written.
;                           Instead, the output variable will have "_#" appended, were "#"
;                           represents a unique number.
;
; :Returns:
;       SQRTQ:          out, required, type=MrScalarTS
;                       The square-root of the Q-Factor, a quantifying measure of
;                           agyrotropy. Note that the Sqrt(Q) is typically shown in the
;                           literature when comparing with Nongyrotropy and Agyrotropy,
;                           as published by Aunai and Scudder.
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
;       2017/03/01  -   Written by Matthew Argall
;-
FUNCTION MrVar_Pres_QFactor, b, p, $
CACHE=cache, $
NAME=name, $
NO_CLOBBER=no_clobber
	Compile_Opt idl2
	On_Error, 2
	
	oB = MrVar_Get(b)
	oP = MrVar_Get(p)
	IF ~oP -> IsTimeIdentical(oB) THEN oB = oB -> Interpol(oP)
	
	;Tensor invariants
	;   - Trace and sum of principle minors
	I1 = oP[*,0,0] + oP[*,1,1] + oP[*,2,2]
	I2 = oP[*,0,0]*oP[*,1,1] + oP[*,0,0]*oP[*,2,2] + oP[*,1,1]*oP[*,2,2] - $
	     oP[*,0,1]*oP[*,1,0] - oP[*,0,2]*oP[*,2,0] - oP[*,1,2]*oP[*,2,1] 

	;Get the parallel component of P
	;   - P-par = b.P.b
	;   - IDL's dimensions are [col,row], so the dot product is the outer product
	oB_hat = oB -> Normalize()
	oP_par = oB_hat -> OuterProduct(oP)
	oP_Par = oP_par -> OuterProduct(oB_hat)
	
	;Q-Factor
	Q  = 1 - 4*I2 / ( (I1 - oP_par) * (I1 + 3*oP_par) )
	I1 = !Null
	I2 = !Null
	
	;Create a time series
	oQ = MrScalarTS( oP['TIMEVAR'], Sqrt(Q['DATA']), $
	                 CACHE      = cache, $
	                 NAME       = name, $
	                 NO_CLOBBER = no_clobber, $
	                 /NO_COPY )
	
	;Set attributes
	oQ['PLOT_TITLE']    = 'Measure of Agyrotropy'
	oQ['TITLE']         = 'Sqrt(Q)'
	oQ['UNITS']         = ''
	oQ['SI_CONVERSION'] = ''
	oQ['VAR_NOTES']     = 'Computation is outlined in the appendix of: ' + $
	                      'Swisdak, M. (2016), Quantifying gyrotropy in magnetic ' + $
	                      'reconnection, Geophys. Res. Lett., 43(1), 43–49, ' + $
	                      'doi:10.1002/2015GL066980.'
	
	;Cleanup variables
	RETURN, oQ
END