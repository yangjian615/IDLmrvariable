; docformat = 'rst'
;
; NAME:
;       MrVar_PoyntingFlux
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
;   Calculate the Poynting Vector, in milli-Watts per square meter (mW/m^2)::
;
;       \vec{S} = \frac{1}{\mu} \left( \vec{E} \times \vec{B} \right)
;
; :Categories:
;   MrVariable
;
; :Params:
;       E:              in, required, type=string/integer/objref
;                       Name, number, or objref of a MrVectorTS variable containing the
;                           vector electric field (mV/m). If E and `B` do not have
;                           identical time stamps, `E` will be inteprolated to `B`.
;       B:              in, required, type=string/integer/objref
;                       Name, number, or objref of a MrVectorTS variable containing the
;                           vector magnetic field (nT).
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
;       SPHERE:         in, optional, type=boolean, default=0
;                       If set, poynting flux is returned in spherical coordinates.
;                       
; :Returns:
;       S:              out, type=objarr(3)
;                       The poynting vector in the spectral domain. Resulting units
;                           are in micro-Watts per square meter (uW / m^2). Three
;                           MrTimeSeries objects are returned, one for each component.
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
;       2017/02/01  -   Written by Matthew Argall
;-
FUNCTION MrVar_PoyntingFlux, E, B, $
CACHE=cache, $
NAME=name, $
NO_CLOBBER=no_clobber, $
SPHERE=sphere
	Compile_Opt idl2
	
	Catch, the_error
	IF the_error NE 0 THEN BEGIN
		Catch, /CANCEL
		MrPrintF, 'LogErr'
		RETURN, !Null
	ENDIF
	
	tf_sphere = Keyword_Set(sphere)
	IF N_Elements(name) EQ 0 THEN name = 'Poynting_Flux'
	
	;Get variables
	oB = MrVar_Get(B)
	oE = MrVar_Get(E)
	
	;Interpolate
	IF ~oE -> IsTimeIdentical(oB) $
		THEN oE = oE -> Interpol(oB)

;-----------------------------------------------------
; Calculate Poynting Flux \\\\\\\\\\\\\\\\\\\\\\\\\\\\
;-----------------------------------------------------
	;Poynting Flux
	;   - 1e-6 converts to uW/m^2
	mu0 = MrConstants('mu_0')
	oS  = 1e-6/mu0 * oE -> Cross(oB)
	
	;Spherical coordinates?
	IF Keyword_Set(sphere) THEN BEGIN
		;Compute spherical components
		oSr = oS -> Magnitude()
		oSu = oS -> Normalize()
		Sp  = atan( oSu[*,1], oSu[*,0] ) * !radeg
		St  = acos( oSu[*,2] ) * !radeg
		
		;Create the vector
		oS          = MrVectorTS( oS['TIMEVAR'], [ [oSr['DATA']], [Sp], [St] ] )
		oS['LABEL'] = ['S', 'Phi', 'Theta']
		oS['UNITS'] = ['$\mu$W/m^2', 'degrees', 'degrees']
	
	;Cartesian components
	ENDIF ELSE BEGIN
		oS['LABEL'] = ['Sx', 'Sy', 'Sz']
		oS['UNITS'] = '\mu W/m^2'
	ENDELSE
	
	;Attributes
	oS['CATDESC']    = 'Poynting Flux as a function of time'
	oS['PLOT_TITLE'] = 'Poynting Flux'
	oS['TITLE']      = 'S!C$\mu$W/m$\up2$'
	oS['VAR_NOTES']  = 'S = 1/\mu_0 (\vec{E} \times \vec{B})'
	
	;Name and store in cache
	oS -> SetName, name
	IF Keyword_Set(cache) THEN oS -> Cache, NO_CLOBBER=no_clobber

	RETURN, oS
END