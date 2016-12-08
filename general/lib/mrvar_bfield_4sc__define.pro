; docformat = 'rst'
;
; NAME:
;   MrVar_BField_4sc__Define
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
;   A class for applying multi-spacecraft analysis methods to magnetic field data.
;
;   References:
;       Daly, P. W., and G. Paschmann (1998), Analysis Methods for Multi-spacecraft Data,
;           ISSI scientific report SR, book, International Space Science Institute.
;
;       Dunlop, W. M., Southwood, D. J., Glassmeier, K.-H., and Neubauer, F. M.: 
;           Analysis of multipoint magnetometer data, Adv. Space Res., 8, 9–10, 1988.
;           http://ac.els-cdn.com/027311778890141X/1-s2.0-027311778890141X-main.pdf?_tid=b19d7876-fa0c-11e1-91d7-00000aacb360&acdnat=1347147002_b1b1630516e6b3576e8e43063c6bf475
;
;       Maszl, C., The Curlometer Method and Cluster II, 2004
;           http://www.space.irfu.se/exjobb/2004_christian_maszl/Documentation/projectwork_maszl.pdf
;
;       Paschmann, G., and P. W. Daly (Eds.) (2008), Multi-Spacecraft Analysis
;           Methods Revisited.
;
; :Categories:
;   MMS, MrVariable
;
; :See Also:
;   MrVarRecipVec__Define.pro
;   MrMMS_FGM_BField__Define.pro
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
;       2016/11/24  -   Written by Matthew Argall
;-
;*****************************************************************************************
;+
;   The initialization method.
;
;   Calling Sequence:
;       o4sc = MrVar_BField_4sc()
;       o4sc = MrVar_BField_4sc(B1, B2, B3, B4)
;       o4sc = MrVar_BField_4sc(B1, B2, B3, B4, R1, R2, R3, R4)
;
; :Params:
;       B1:         in, required, type=integer/string/objref
;                   Name, number, or objref of the vector magnetic field from spacecraft 1.
;       B2:         in, optional, type=integer/string/objref
;                   Name, number, or objref of the vector magnetic field from spacecraft 2.
;       B3:         in, optional, type=integer/string/objref
;                   Name, number, or objref of the vector magnetic field from spacecraft 3.
;       B4:         in, optional, type=integer/string/objref
;                   Name, number, or objref of the vector magnetic field from spacecraft 4.
;       R1:         in, optional, type=integer/string/objref
;                   Name, number, or objref of the position vector from spacecraft 1.
;       R2:         in, optional, type=integer/string/objref
;                   Name, number, or objref of the position vector from spacecraft 2.
;       R3:         in, optional, type=integer/string/objref
;                   Name, number, or objref of the position vector from spacecraft 3.
;       R4:         in, optional, type=integer/string/objref
;                   Name, number, or objref of the position vector from spacecraft 4.
;-
function MrVar_BField_4sc::INIT, b1, b2, b3, b4, r1, r2, r3, r4
	compile_opt idl2

	;Error handling
	catch, the_error
	if the_error ne 0 then begin
		catch, /CANCEL
		MrPrintF, 'LogErr'
		return, 0
	endif
	
	;Initialize reciprocal vectors
	self.oRecipVec = MrVar_RecipVec()
	if ~obj_valid(self.oRecipVec) then message, 'Unable to initialize reciprocal vector object.'

;-----------------------------------------------------
; Load Data \\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\
;-----------------------------------------------------
	case n_params() of
		;USAGE:
		;   - oFGM = MrVar_BField_4sc()
		0: ;Do nothing
		
		;USAGE:
		;   - oFGM = MrVar_BField_4sc(b1, b2, b3, b4)
		4: self -> SetData, b1, b2, b3, b4
		
		;USAGE:
		;   - oFGM = MrVar_BField_4sc(b1, b2, b3, b4, r1, r2, r3, r4)
		8: begin
			self -> SetData, b1, b2, b3, b4
			self -> SetPosition, r1, r2, r3, r4
		endcase
		
		else: message, 'Incorrect number of parameters.'
	endcase
	
	return, 1
end


;+
;   Clean up after the object is destroyed
;-
pro MrVar_BField_4sc::CLEANUP
	compile_opt idl2
	on_error, 2

	obj_destroy, self.oB1
	obj_destroy, self.oB2
	obj_destroy, self.oB3
	obj_destroy, self.oB4
	obj_destroy, self.oRecipVec
end


;+
;   Compute the barycentric average of the magnetic field.
;
; :Keywords:
;       CACHE:          in, optional, type=boolean, default=0
;                       If set, the reciprocal vectors will be added to the cache.
;       NAME:           in, optional, type=string, default='RecipCurl'
;                       A name to be given to the variable.
;
; :Returns:
;       B_BARY:         out, required, type=MrVectorTS objref
;                       Barycentric average of the magnetic field.
;-
function MrVar_BField_4sc::Barycenter, $
CACHE=cache, $
NAME=name
	compile_opt idl2
	on_error, 2
	
	if n_elements(name) eq 0 then name = 'b_barycenter'
	
	;Average all of the fields
	b_bary = (self.oB1 + self.oB2 + self.oB3 + self.oB4) / 4.0
	
	;Name and cache
	b_bary -> SetName, name
	if keyword_set(cache) then b_bary -> Cache
	
	;Set attributes
	b_bary -> AddAttr, 'CATDESC',    'Barycentric average of the magnetic field.'
	b_bary -> AddAttr, 'PLOT_TITLE', 'Barycentric Magnetic Field'
	b_bary -> AddAttr, 'UNITS',      'nT'
	b_bary -> AddAttr, 'TITLE',      'B_{avg}!c(nT)'

	return, b_bary
end


;+
;   Compute the current desnity using the reciprocal vector method.
;
; :Keywords:
;       CACHE:          in, optional, type=boolean, default=0
;                       If set, the reciprocal vectors will be added to the cache.
;       NAME:           in, optional, type=string, default='RecipCurl'
;                       A name to be given to the variable.
;
; :Returns:
;       DIVB:           out, required, type=MrScalarTS objref
;                       Divergence of the magnetic field, in units of current density.
;-
function MrVar_BField_4sc::DivB, $
NAME=name, $
CACHE=cache
	compile_opt idl2
	on_error, 2
	
	;Defaults
	if n_elements(name) eq 0 then name = 'DivB'

	;Compute curl
	;   - Compute (del x B)
	div = self.oRecipVec -> Divergence( self.oB1, self.oB2, self.oB3, self.oB4, $
	                                    CACHE = cache )

	;Convert to current density
	;   - J = 1/mu0 * del . B
	;   - A factor of 1e-3/mu0 converts nT/km to nA/m^2
	div *= ( 1e-3 / MrConstants('mu_0') )
	
	;Add attributes
	div -> SetName, name
	div -> AddAttr, 'CATDESC',    'Divergence of B from the reciprocal vector technique.'
	div -> AddAttr, 'PLOT_TITLE', 'Divergence of B'
	div -> AddAttr, 'UNITS',      'nA/m^2'
	div -> AddAttr, 'TITLE',      'mu0*Div(B)!C(nA/m^2)'

	return, div
end


;+
;   Compute the current density via the curlometer method.
;
; :Keywords:
;       CACHE:          in, optional, type=boolean, default=0
;                       If set, the reciprocal vectors will be added to the cache.
;       NAME:           in, optional, type=string, default='RecipCurl'
;                       A name to be given to the variable.
;
; :Returns:
;       J:              out, required, type=MrVectorTS objref
;                       The current density.
;-
function MrVar_BField_4sc::Curlometer, $
REFSC=refsc, $
CACHE=cache, $
NAME=name
	compile_opt idl2
	
	catch, the_error
	if the_error ne 0 then begin
		catch, /CANCEL
		MrPrintF, 'LogErr'
		return, !Null
	end

	;Constants & conversion factors
	mu_0 = MrConstants('mu_0')        ;m kg s-2 A-2
	iSC  = n_elements(refsc) eq 0 ? 0 : refsc - 1
	if n_elements(name) eq 0 then name = 'j_curlometer'
	
	;Number of spacecraft and number of data points
	nSC  = 4
	nPts = self.oB1 -> GetNPts()
	self.oRecipVec -> GetData, oR1, oR2, oR3, oR4
	oBB  = [self.oB1, self.oB2, self.oB3, self.oB4]
	oRR  = [oR1, oR2, oR3, oR4]
	
	;For the surfaces opposite to each spacecraft in the tetrahedron,
	lhs = objarr(nSC-1)
	rhs = objarr(nSC-1)
	j   = (isc + 1) mod 4
	k   = (isc + 2) mod 4
	for i = 0, nSC - 2 do begin
		;The difference in the fields of REF_SC and spacecraft K (M).
		B_ij = (oBB[J] - oBB[iSC])
		B_ik = (oBB[K] - oBB[iSC])

		;The distance between REF_SC and spacecraft K (M).
		R_ij = (oRR[J] - oRR[iSC]) * 1e3   ;km -> m
		R_ik = (oRR[K] - oRR[iSC]) * 1e3   ;km -> m

		;mu_0 * J . (R_ij x R_ik) = (B_ij . R_ik) - (B_ik . R_ij)
		lhs[i] = mu_0 * R_ij -> Cross(R_ik)
		rhs[i] = ( B_ij -> Dot(R_ik) ) - ( B_ik -> Dot(R_ij) )
		
		;Cycle through
		j = (j + 1) mod 4
		k = (k + 1) mod 4
		if j eq iSC then j = (j + 1) mod 4
		if k eq iSC then k = (k + 1) mod 4
	endfor
	
	;Permute the results so they equations (surfaces) are along the rows
	;   - [component, surface, time]
	J = fltarr(nPts, 3)
	for i = 0, nPts-1 do begin
		;Extract the set of equations
		ltemp = transpose( [ (lhs[0])[i,*], (lhs[1])[i,*], (lhs[2])[i,*] ] )
		rtemp = transpose( [ (rhs[0])[i,*], (rhs[1])[i,*], (rhs[2])[i,*] ] )
		
		;LU Decompose
		la_ludc, ltemp, index
		
		;Diagonalize
		J[i,*] = la_lusol(ltemp, index, rtemp)
	endfor
	
	;Create a vector
	J = MrVectorTS( self.oB1['TIMEVAR'], J, $
	                CACHE = cache, $
	                NAME  = name, $
	                /NO_COPY )
	
	;Set attributes
	J -> AddAttr, 'CATDESC',    'Current denstiy from the curlometer technique.'
	J -> AddAttr, 'PLOT_TITLE', 'Current Density'
	J -> AddAttr, 'UNITS',      'nA/m^2'
	J -> AddAttr, 'TITLE',      'J!C(nA/m^2)'

	return, J
end


;+
;   Error in the measure of curvature.
;
;   References
;       Shen, C., X. Li, M. Dunlop, Z. X. Liu, A. Balogh, D. N. Baker, M. Hapgood,
;           and X. Wang (2003), Analyses on the geometrical structure of magnetic field
;           in the current sheet based on cluster measurements, J. Geophys. Res. Sp.
;           Phys., 108(A5), doi:10.1029/2002JA009612.
;
; :Keywords:
;       CACHE:          in, optional, type=boolean, default=0
;                       If set, the reciprocal vectors will be added to the cache.
;       NAME:           in, optional, type=string, default='RecipCurl'
;                       A name to be given to the variable.
;
; :Returns:
;       OCERR:          out, required, type=MrVectorTS objref
;                       The curvature radius error.
;-
function MrVar_BField_4sc::CurvErr, $
NAME=name, $
CACHE=cache
	compile_opt idl2
	
	catch, the_error
	if the_error ne 0 then begin
		catch, /CANCEL
		MrPrintF, 'LogErr'
		return, !Null
	end
	
	;Defaults
	if n_elements(name) eq 0 then name = 'r_curv_err'
	
	;Mean separation
	oSep  = self.oRecipVec -> Separation()
	
	;Curvature radius
	oRCurv = self -> R_Curvature()
	
	;Error
	oErr = oSep / (2 * oRCurv)

	;Add attributes
	oErr -> SetName, name
	oErr -> AddAttr, 'CATDESC',    'Curvature radius error estimate.'
	oErr -> AddAttr, 'PLOT_TITLE', 'Curvature error'
	oErr -> AddAttr, 'UNITS',      ''
	oErr -> AddAttr, 'TITLE',      '\rho_{c,err}'
	if keyword_set(cache) then oErr -> Cache

	return, oErr
end


;+
;   Compute the current desnity using the reciprocal vector method.
;
;   References
;       Shen, C., X. Li, M. Dunlop, Z. X. Liu, A. Balogh, D. N. Baker, M. Hapgood,
;           and X. Wang (2003), Analyses on the geometrical structure of magnetic field
;           in the current sheet based on cluster measurements, J. Geophys. Res. Sp.
;           Phys., 108(A5), doi:10.1029/2002JA009612.
;
; :Keywords:
;       CACHE:          in, optional, type=boolean, default=0
;                       If set, the reciprocal vectors will be added to the cache.
;       NAME:           in, optional, type=string, default='RecipCurl'
;                       A name to be given to the variable.
;
; :Returns:
;       OCURV:          out, required, type=MrVectorTS objref
;                       The curvature radius.
;-
function MrVar_BField_4sc::Curvature, $
NAME=name, $
CACHE=cache
	compile_opt idl2
	
	catch, the_error
	if the_error ne 0 then begin
		catch, /CANCEL
		MrPrintF, 'LogErr'
		return, !Null
	end
	
	;Defaults
	if n_elements(name) eq 0 then name = 'curvature'

	;Compute curl
	;   - Compute (del x B)
	ob1 = self.oB1 -> Normalize()
	ob2 = self.oB2 -> Normalize()
	ob3 = self.oB3 -> Normalize()
	ob4 = self.oB4 -> Normalize()
	grad_b = self.oRecipVec -> Gradient( ob1, ob2, ob3, ob4 )

	;Barycentric average
	b_bary = self   -> Barycenter()
	b_bary = b_bary -> Normalize()

	;Dot product
	;   c = bi * Tij (math notation)
	;     = | bx by bz | | Txx Txy Txz |
	;                    | Tyx Tyy Tyz |
	;                    | Tzx Tzy Tzz |
	;     = bx Txx + by Tyx + bz Tzx
	;       bx Txy + by Tyy + bz Tyy
	;       bx Txz + by Tyz + bz Tzz
	oCurv = b_bary ## grad_b
	
	;Add attributes
	oCurv -> SetName, name
	oCurv -> AddAttr, 'CATDESC',    'Curvature radius of the magnetic field'
	oCurv -> AddAttr, 'PLOT_TITLE', 'Magnetic Curvature'
	oCurv -> AddAttr, 'UNITS',      '1/km'
	oCurv -> AddAttr, 'TITLE',      '\rho_{c}'
	if keyword_set(cache) then oCurv -> Cache

	return, oCurv
end


;+
;   Compute the current desnity using the reciprocal vector method.
;
; :Keywords:
;       CACHE:          in, optional, type=boolean, default=0
;                       If set, the reciprocal vectors will be added to the cache.
;       NAME:           in, optional, type=string, default='RecipCurl'
;                       A name to be given to the variable.
;
; :Returns:
;       J:              out, required, type=MrVectorTS objref
;                       The current density.
;-
function MrVar_BField_4sc::J, $
NAME=name, $
CACHE=cache
	compile_opt idl2
	
	catch, the_error
	if the_error ne 0 then begin
		catch, /CANCEL
		MrPrintF, 'LogErr'
		return, !Null
	end
	
	;Defaults
	if n_elements(name) eq 0 then name = 'j_recipvec'

	;Compute curl
	;   - Compute (del x B)
	curl = self.oRecipVec -> Curl( self.oB1, self.oB2, self.oB3, self.oB4, $
	                               CACHE = cache )

	;Convert to current density
	;   - J = 1/mu0 * del x B
	;   - A factor of 1e-3/mu0 converts nT/km to nA/m^2
	J = curl * ( 1e-3 / MrConstants('mu_0') )
	obj_destroy, curl
	
	;Add attributes
	J -> SetName, name
	J -> AddAttr, 'CATDESC',    'Current denstiy from the reciprocal vector technique.'
	J -> AddAttr, 'PLOT_TITLE', 'Current Density'
	J -> AddAttr, 'UNITS',      'nA/m^2'
	J -> AddAttr, 'TITLE',      'J!C(nA/m^2)'

	return, J
end


;+
;   Adiabatic scattering parameter.
;
;   References
;       Shen, C., X. Li, M. Dunlop, Z. X. Liu, A. Balogh, D. N. Baker, M. Hapgood,
;           and X. Wang (2003), Analyses on the geometrical structure of magnetic field
;           in the current sheet based on cluster measurements, J. Geophys. Res. Sp.
;           Phys., 108(A5), doi:10.1029/2002JA009612.
;
; :Params:
;       ENERGY:         in, required, type=float
;                       Energy (eV) of the particle.
;       MASS:           in, required, type=float/string
;                       Either the mass (kg) of the particle, or string that can be
;                           used with MrConstants to return the particle mass.
;
; :Keywords:
;       CACHE:          in, optional, type=boolean, default=0
;                       If set, the reciprocal vectors will be added to the cache.
;       NAME:           in, optional, type=string, default='RecipCurl'
;                       A name to be given to the variable.
;
; :Returns:
;       OKAPPA:         out, required, type=MrVectorTS objref
;                       The square of the adiabatic scattering parameter.
;-
function MrVar_BField_4sc::Kappa, energy, mass, $
NAME=name, $
CACHE=cache
	compile_opt idl2
	
	catch, the_error
	if the_error ne 0 then begin
		catch, /CANCEL
		MrPrintF, 'LogErr'
		return, !Null
	end
	
	;Defaults
	if n_elements(name) eq 0 then name = 'kappa'
	if size(mass, /TNAME) eq 'STRING' $
		then m = MrConstants(mass) $
		else m = mass
	
	;Larmor radius
	;   - r = m v / ( q B )
	;   - 1e9 to convert nT to T.
	b    = self -> Barycenter()
	bmag = b -> Magnitude()
	v    = MrRel_Velocity(energy, m)
	r    = ( 1e9 * m / MrConstants('q') ) * v / bmag
	
	
	;Curvature radius
	oRCurv  = self -> R_Curvature()
	
	;Curvature radius
	oKappa = oRCurv / r
	
	;Add attributes
	oKappa -> SetName, name
	oKappa -> AddAttr, 'CATDESC',    'Adiabatic scattering parameter'
	oKappa -> AddAttr, 'PLOT_TITLE', 'Scattering parameter'
	oKappa -> AddAttr, 'UNITS',      ''
	oKappa -> AddAttr, 'TITLE',      '\kappa^2'
	oKappa -> AddAttr, 'VARNOTES',   'Larmor radius / curvature radius'
	if keyword_set(cache) then oKappa -> Cache

	return, oKappa
end


;+
;   Load data from source files.
;-
pro MrVar_BField_4sc::Load
	compile_opt idl2
	on_error, 2
	
	message, 'MrVar_BField_4sc::Load must be over-ridden by a subclass.'
end


;+
;   Load position data from source files.
;-
pro MrVar_BField_4sc::LoadPosition
	compile_opt idl2
	on_error, 2
	
	message, 'MrVar_BField_4sc::LoadPosition must be over-ridden by a subclass.'
end


;+
;   Compute the curvature radius.
;
;   References
;       Shen, C., X. Li, M. Dunlop, Z. X. Liu, A. Balogh, D. N. Baker, M. Hapgood,
;           and X. Wang (2003), Analyses on the geometrical structure of magnetic field
;           in the current sheet based on cluster measurements, J. Geophys. Res. Sp.
;           Phys., 108(A5), doi:10.1029/2002JA009612.
;
; :Keywords:
;       CACHE:          in, optional, type=boolean, default=0
;                       If set, the result will be added to the variable cache.
;       NAME:           in, optional, type=string, default='r_curvature'
;                       A name to be given to the return variable.
;
; :Returns:
;       ORCURV:         out, required, type=MrScalarTS objref
;                       The curvature radius.
;-
function MrVar_BField_4sc::R_Curvature, $
NAME=name, $
CACHE=cache
	compile_opt idl2
	
	catch, the_error
	if the_error ne 0 then begin
		catch, /CANCEL
		MrPrintF, 'LogErr'
		return, !Null
	end
	
	if n_elements(name) eq 0 then name = 'r_curvature'
	
	;Curvature radius is the inverse of the curvature
	oCurv  = self -> Curvature()
	oRCurv = 1.0 / oCurv -> Magnitude()
	
	;Name and cache
	oRCurv -> SetName, name
	oRCurv -> AddAttr, 'CATDESC',    'Radius of curvature'
	oRCurv -> AddAttr, 'PLOT_TITLE', 'Radius of curvature'
	oRCurv -> AddAttr, 'UNITS',      'km'
	oRCurv -> AddAttr, 'TITLE',      '\rho_{c}'
	if keyword_set(cache) then oRCurv -> Cache

	return, oRCurv
end


;+
;   Quantities for which the spatial gradients are computed. All variables will be
;   interpolated to P1.
;
; :Params:
;       B1:         in, required, type=integer/string/objref
;                   Name, number, or objref of the vector magnetic field from spacecraft 1.
;       B2:         in, required, type=integer/string/objref
;                   Name, number, or objref of the vector magnetic field from spacecraft 2.
;       B3:         in, required, type=integer/string/objref
;                   Name, number, or objref of the vector magnetic field from spacecraft 3.
;       B4:         in, required, type=integer/string/objref
;                   Name, number, or objref of the vector magnetic field from spacecraft 4.
;-
pro MrVar_BField_4sc::SetData, b1, b2, b3, b4
	compile_opt idl2
	on_error, 2
	
	;Check object class
	oB       = MrVar_Get(b1)
	objClass = obj_class(oB)
	if ~obj_isa(oB, 'MrVectorTS') then message, 'B1-4 must be MrVectorTS objects.'
	
	;Get the time variable
	;   - All variables will be interpolated to P1
	oTime = oB['TIMEVAR']
	
	;Loop over all variables
	oBB    = [b1, b2, b3, b4]
	oBBout = objarr(4)
	for i = 0, 3 do begin
		;Get the variable
		oB = MrVar_Get(oBB[i])
		
		;Class restriction
		if obj_class(oB) ne objClass $
			then message, 'All B1-4 parameters must be the same object class.'
		
		;Interpolate if we need to
		if oB['TIMEVAR'] -> IsIdentical(oTime) $
			then oBBout[i] = oB -> Copy() $
			else oBBout[i] = oB -> Interpol(oTime)
	endfor
	
	;Set data
	self.oB1 = oBBout[0]
	self.oB2 = oBBout[1]
	self.oB3 = oBBout[2]
	self.oB4 = oBBout[3]
end


;+
;   Quantities for which the spatial gradients are computed. All variables will be
;   interpolated to P1.
;
; :Params:
;       R1:         in, required, type=integer/string/objref
;                   Name, number, or objref of the position vector from spacecraft 1.
;       R2:         in, required, type=integer/string/objref
;                   Name, number, or objref of the position vector from spacecraft 2.
;       R3:         in, required, type=integer/string/objref
;                   Name, number, or objref of the position vector from spacecraft 3.
;       R4:         in, required, type=integer/string/objref
;                   Name, number, or objref of the position vector from spacecraft 4.
;
; :Keywords:
;       TIME:       in, required, type=integer/string/objref
;                   Name, number, or objref of a time or time-series variable. R1-4 will
;                       be interpolated to these time tags. The default is to interpolate
;                       to the time tags of the magnetic field from SC 1. This requires
;                       data to be set via the ::SetData method.
;-
pro MrVar_BField_4sc::SetPosition, r1, r2, r3, r4, $
TIME=time
	compile_opt idl2
	on_error, 2
	
	;Interpolate to the magnetic field from SC 1
	if n_elements(time) eq 0 then begin
		if obj_valid(self.oB1) $
			then time = self.oB1['TIMEVAR'] $
			else message, 'B-Field has not been set. Cannot inteprolate R1-4.'
	endif
	
	;Set data
	self.oRecipVec -> SetData, r1, r2, r3, r4, TIME=time
end


;+
;   The class definition statement.
;
; :Params:
;       CLASS:          out, optional, type=structure
;-
pro MrVar_BField_4sc__DEFINE
	compile_opt idl2
	
	class = { MrVar_BField_4sc, $
	          oRecipVec: obj_new(), $
	          oB1:       obj_new(), $
	          oB2:       obj_new(), $
	          oB3:       obj_new(), $
	          oB4:       obj_new() $
	        }
end