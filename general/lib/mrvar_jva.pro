; docformat = 'rst'
;
; NAME:
;       MrVar_JVA
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
;   Determine the joint variance frame. The normal direction is given by the maximum
;   variance of the electric field. The coordinate system is then rotated about this axis
;   until the Z direction is closest to the Z axis found from the minimum variance of the
;   magnetic field.
;
; REFERENCES::
;   Mozer, F. S., and A. Retinò (2007), Quantitative estimates of magnetic field
;       reconnection properties from electric and magnetic field measurements,
;       J. Geophys. Res., 112(A10), A10206, doi:10.1029/2007JA012406.
;
; :Params:
;       B:              in, required, type=int/string/objref
;                       The name, number, or objref of a MrVectorTS variable that contains
;                           the 3-component magnetic field to be analyzed.
;       E:              in, required, type=int/string/objref
;                       The name, number, or objref of a MrVectorTS variable that contains
;                           the 3-component electric field to be analyzed.
;
; :Keywords:
;       DOUBLE:         in, optional, type=boolean, default=0
;                       If set, computation will be performed in double precision.
;       EIGENVALS_MAX:  out, optional, type=objref
;                       The eigenvalues associated with the maximum variance analysis.
;       EIGENVALS_MIN:  out, optional, type=objref
;                       The eigenvalues associated with the minimum variance analysis.
;       EIGENVEC_MAX:   out, optional, type=objref
;                       The eigenvectors associated with the maximum variance analysis.
;       EIGENVEC_MIN:   out, optional, type=objref
;                       The eigenvectors associated with the minimum variance analysis.
;       THETA:          out, optional, type=float
;                       The angle by which MVAE was rotated to arrive at JVA system.
;
; :Returns:
;       OXYZ2JOINT:     out, required, type=3x3 objref
;                       The minimum, intermediate, and maximum normalized eigenvectors
;                           of the covariant matrix, as described above. Eigvec[*,0]
;                           corresponds to eigval[0], etc.
;
; :History:
;   Modification History::
;       2017-05-15  -   Written by Matthew Argall
;-
FUNCTION MrVar_JVA, b, e, $
EIGENVALS_MAX=oEigenVals_max, $
EIGENVALS_MIN=oEigenVals_min, $
EIGENVEC_MAX=oEigenVec_max, $
EIGENVEC_MIN=oEigenVec_min, $
THETA=theta, $
DOUBLE=double, $
EIGENVALUES=oEigVals
	Compile_Opt idl2
	On_Error, 2
	
	;MVAB
	IF Arg_Present(oEigenVals_min) $
		THEN oxyz2min = MrVar_MVA(b, EIGENVALUES=oEigenVals_min) $
		ELSE oxyz2min = MrVar_MVA(b)
		
	;MVAE
	IF Arg_Present(oEigenVals_min) $
		THEN oxyz2max = MrVar_MVA(e, /MAXIMUM, EIGENVALUES=oEigenVals_max) $
		ELSE oxyz2max = MrVar_MVA(e, /MAXIMUM)
	
	;Determine rotation angle
	;   - Rotate MVAB frame into MVAE system
	;   - Project L-vectors onto YZ-plane
	;   - Determine angle to Z-axis
	;   - Build rotation matrix about N-axis
	;   - Rotate N-axis rotation matrix into GSE
	;   - Rotate maximum variance frame
	omax2min = oxyz2max ## oxyz2min ## oxyz2max -> Transpose()
	theta    = ATan(omax2min['DATA',2,1], omax2min['DATA',2,2])
	
	;Rotation matrix about N
	omax2joint = MrVariable( [ [  1,       0,           0     ], $
	                           [  0,  cos(theta),  sin(theta) ], $
	                           [  0, -sin(theta),  cos(theta) ] ] )
	
	;Joint variance
	omax2xyz   = oxyz2max -> Transpose()
	oxyz2joint = omax2xyz ## omax2joint ## omax2xyz -> Transpose()
	
	;Attributes
	oxyz2joint['CATDESC'] = 'Transformation matrix to the joint variance frame. The normal ' + $
	                        'direction is given by the maximum variance of the electric ' + $
	                        'field. The coordinate system is then rotated about this axis ' + $
	                        'until the Z direction is closest to the Z axis found from the ' + $
	                        'minimum variance of the magnetic field.'
	oxyz2joint['LABL_PTR_1'] = ['X', 'Y', 'Z']
	oxyz2joint['LABL_PTR_2'] = ['N', 'M', 'L']
	
	;Return variables
	IF Arg_Present(oEigenVec_max) THEN oEigenVec_max = oxyz2max
	IF Arg_Present(oEigenVec_min) THEN oEigenVec_min = oxyz2min
	IF Arg_Present(theta)         THEN theta        *= !radeg
	
	RETURN, oxyz2joint
END