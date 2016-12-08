; docformat = 'rst'
;
; NAME:
;   MrDist3D__Define
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
;   Calculate moments and reduced distributions from a 3D distribution function.
;
; :Categories:
;   MrDist
;
; :See Also:
;   MrDist4D__Define.pro
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
;       2016/10/24  -   Written by Matthew Argall
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
;       CACHE:          in, optional, type=boolean, default=0
;                       If set, both `TIME` and `DATA` are added to the variable cache.
;       DIMENSION:      in, optional, type=integer
;                       The time-dependent, 1-based dimension of `DATA`. If not provided,
;                           the dimension of `DATA` that is equal in size to `TIME` is
;                           chose as the default.
;       NAME:           in, optional, type=integer
;                       Name to be given to the variable object.
;       NO_CLOBBER:     in, optional, type=boolean, default=0
;                       If set, do not clobber variables of the same name. Instead,
;                           rename this variable by appending "_#" to `NAME`, where
;                           "#" represents a unique number. Ignored unless `CACHE` is set.
;       NO_COPY:        in, optional, type=boolean, default=0
;                       If set `DATA` will be copied directly into the object
;                           and will be left undefined (a MrTimeSeries object will not
;                           be destroyed, but its array will be empty).
;       T_TYPE:         in, optional, type=integer
;                       If `TIME` is an array of time stamps, use this keyword to indicate
;                           the format or time-basis. See MrTimeVar for more details.
;       T_NAME:         in, optional, type=integer
;                       Name to be given to the MrTimeVar object. Ignored unless `TIME`
;                           is an array of time stamps.
;-
function MrDist3D::INIT, dist3D, phi, theta, energy, $
DEGREES=degrees, $
MASS=mass, $
SPECIES=species, $
UNITS=units
	compile_opt idl2

	;Error handling
	catch, the_error
	if the_error ne 0 then begin
		catch, /CANCEL
		MrPrintF, 'LogErr'
		return, 0
	endif
	
	;Allocate heap
	self.distfn = ptr_new(/ALLOCATE_HEAP)
	self.phi    = ptr_new(/ALLOCATE_HEAP)
	self.theta  = ptr_new(/ALLOCATE_HEAP)
	self.energy = ptr_new(/ALLOCATE_HEAP)
	
	;Store data
	if n_elements(dist3d) gt 0 then begin
		self -> SetData, dist3D, phi, theta, energy, $
		                 DEGREES = degrees, $
		                 UNITS   = units, $
		                 RADIANS = radians
	endif
	
	;Set object properties
	self -> SetProperty, MASS    = mass, $
	                     SPECIES = species

	return, 1
end


;+
;   Clean up after the object is destroyed
;-
pro MrDist3D::CLEANUP
	compile_opt idl2
	on_error, 2
	
	ptr_free, self.distfn
	ptr_free, self.theta
	ptr_free, self.phi
	ptr_free, self.energy
end


;+
;   Find the spacing of energy bins assuming constant deltaE/E
;
; :Returns:
;       DELTAE:             Spacing of the energy bins.
;-
function MrDist3D::DeltaE
	compile_opt idl2
	on_error, 2

	;
	; dLogE is the spacing of the energy bins in log space
	;
	;   dLogE = log(E1) - log(E0)                      (1)
	;
	; dE/E is just the derivative of a natural log
	;
	;   d( ln(E) ) = dE / E                            (2)
	;
	; If we change bases by log_a(B) = log_x(B) / log_x(A), where x=10, A=exp, B=E
	;
	;   d( ln(E) ) = d [ log(E) / log(exp) ]
	;              = dLogE / log(exp)                  (3)
	;
	; Combining Equations 1 and 2
	;
	;   dE/E = dLogE / log(exp)
	;
	;     dE = E * dLogE / log(exp)
	;
	
	dLogE  = alog10( (*self.energy)[1] ) - alog10( (*self.energy)[0] )
	deltaE = *self.energy * dLogE / alog10( exp(1) )
	
	;Return results
	return, deltaE
end


;+
;   Reduce a single 3D distribution function to 1D by averaging over
;   energy and pitch angle.
;
; :Params:
;       ENERGY:         out, optional, type=fltarr
;                       Energy bin centers of the distribution.
;       DE:             out, optional, type=fltarr
;                       Half-width of the energy bins.
;
; :Keywords:
;       NE_BINS:        in, optional, type=integer
;                       Number of energy bins in the reduced distribution. The default
;                           is to use the same bins and the original distribution.
;       PHI_RANGE:      in, optional, type=fltarr(2), default=[0.0\, 360.0]
;                       The range in azimuthal angle (degrees) over which to average.
;       THETA_RANGE:    in, optional, type=fltarr(2), default=[0.0\, 180.0]
;                       The range in polar angle (degrees) over which to average.
;
; :Returns:
;       DIST1D:         out, required, type=fltarr(L)
;                       The 1D distribution with size nEnergy.
;-
function MrDist3D::ESpec, energy, dE, $
PHI_RANGE=phi_range, $
NE_BINS=nE_bins, $
THETA_RANGE=pa_range
	compile_opt idl2
	on_error, 2

;-----------------------------------------------------
; Defaults \\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\
;-----------------------------------------------------

	;Dimension sizes
	dims    = size(*self.distFn, /DIMENSIONS)
	nPhi    = dims[0]
	nTheta  = dims[1]
	nEnergy = dims[2]

	;PA range over which to average
	if n_elements(phi_range)   eq 0 then phi_range   = [0.0, 360.0]
	if n_elements(theta_range) eq 0 then theta_range = [0.0, 180.0]
	if n_elements(nE_bins)     gt 0 then MrPrintF, 'LogWarn', 'TODO: Allow NE_BINS to vary.'

	;Use original number of energy bins
	;   - Do not use NE_BINS to prevent cyclic messages there ---^
	nBins = nEnergy

;-----------------------------------------------------
; Coordinate Space \\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\
;-----------------------------------------------------
	
	;Combine data into 3-vectors of [phi, theta, energy]
	coords = transpose( [ [reform( *self.phi,   nPhi*nTheta)], $
	                      [reform( *self.theta, nPhi*nTheta)] ] )

;-----------------------------------------------------
; Weight \\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\
;-----------------------------------------------------
	;
	;   - w = v^2 * sin(theta) * dv * dTheta * dPhi
	;   - Applied as total(w*psd)/total(w)
	;   - dTheta and dPhi are constants, so will factor out
	;   - v^2 and dv are constants, because we are not avaraging over different energies.
	;   - Elements from two different old theta bins can fall into the same
	;     new theta bin, so the sin(theta) term does not cancel.
	;   => w = sin(theta)
	;
	
	;Weight
	weight = sin( *self.theta * !dtor )

;-----------------------------------------------------
; Re-bin Data \\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\
;-----------------------------------------------------
	
	;Locate data within new bins
	;   - Keep the number of Theta bins the same
	;   - Average all relevant PHI and ENERGY values.
	cHist  = hist_nd( coords, $
	                  MIN             = [phi_range[0], theta_range[0]], $
	                  MAX             = [phi_range[1], theta_range[1]], $
	                  NBINS           = [           1,              1], $
	                  REVERSE_INDICES = ri )

;-----------------------------------------------------
; Reduce to 1D \\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\
;-----------------------------------------------------
	
	;Allocate memory to reduced 2D distribution
	dist1D = fltarr(nBins)

	;Loop over bins
	k = 0
	for i = 0, nEnergy - 1 do begin
		;Skip empty bins
		if ri[k] eq ri[k+1] then continue

		;Source indices
		inds = ri[ri[k]:ri[k+1]-1]
;		isrc = array_indices([nPhi,nTheta], inds, /DIMENSIONS)
	
		;Weight
;		w = weight[isrc[0,*], isrc[1,*]]

		;Re-bin
		temp      = (*self.distFn)[*, *, i]
;		dist1D[i] = total( temp[isrc[0,*], isrc[1,*]] * w ) / total(w)
		dist1D[i] = total( temp[inds] * weight[inds]) / total( weight[inds] )
	endfor

;-----------------------------------------------------
; Output Bins \\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\
;-----------------------------------------------------
	
	;Create bins
	energy = *self.energy

	;Return the 1D distribution
	return, dist1D
end


;+
;   Reduce the 3D distribution function to a 1D distribution in polar angle.
;
; :Params:
;       THETA:          out, optional, type=fltarr
;                       Polar angle (degrees) bins of the distribution.
;       ENERGY:         out, optional, type=fltarr
;                       Energy (eV) bins of the distribution.
;
; :Keywords:
;       NE_BINS:        in, optional, type=integer
;                       Number of energy bins in the reduced distribution. The default
;                           is to use the same bins and the original distribution.
;       NTHETA_BINS:    in, optional, type=integer
;                       Number of polar angle bins in the reduced distribution. The
;                           default is to use the same bins and the original distribution.
;       PHI_RANGE:      in, optional, type=fltarr(2), default=[0.0\, 360.0]
;                       The range in azimuthal angle (degrees) over which to average.
;-
function MrDist3D::ThetaE, theta, energy, dTheta, dE, $
NE_BINS=nE_bins, $
NTHETA_BINS=nTheta_bins, $
PHI_RANGE=phi_range
	compile_opt idl2
	on_error, 2

;-----------------------------------------------------
; Defaults \\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\
;-----------------------------------------------------

	;Dimension sizes
	dims    = size(*self.distFn, /DIMENSIONS)
	nPhi    = dims[0]
	nTheta  = dims[1]
	nEnergy = dims[2]

	;Amount to average over PHI
	theta_range = [0, 180]
	if n_elements(phi_range)   eq 0 then phi_range   = [0.0, 360.0]
	if n_elements(nTheta_bins) eq 0 then nTheta_bins = nTheta
	if n_elements(nE_bins)     gt 0 then MrPrintF, 'logwarn', 'TODO: Allow nE_bins to change.'
	
	;Keep the number of energy bins the same.
	;   - Do not use NE_BINS to prevent cyclic messages there ---^
	nBins = nEnergy

;-----------------------------------------------------
; Coordinate Space \\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\
;-----------------------------------------------------

	;Combine data into 3-vectors of [phi, theta, energy]
	coords = transpose( [ [reform(*self.phi,   nPhi*nTheta)], $
	                      [reform(*self.theta, nPhi*nTheta)] ] )

;-----------------------------------------------------
; Weight \\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\
;-----------------------------------------------------
	
	;Weight
	;   - Weight by size of old element
	;   - phi element: v*sin(theta)*dphi.
	;   - Weight is applied as total(w*psd)/total(w).
	;   - v is constant, so cancels out of w.
	;   - dphi is constant, so concels out of w
	;
	
	;Weight function
	weight = sin( *self.theta * !dtor )

;-----------------------------------------------------
; Re-bin Data \\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\
;-----------------------------------------------------
	;Locate data within new bins
	;   - Keep the number of Theta bins the same
	;   - Average all PHI values within [-DELTA, DELTA] in each theta bin
	cHist  = hist_nd(coords, $
	                 MIN             = [phi_range[0], theta_range[0]], $
	                 MAX             = [phi_range[1], theta_range[1]], $
	                 NBINS           = [           1,    nTheta_bins], $
	                 REVERSE_INDICES = ri)

;-----------------------------------------------------
; Reduce to 2D \\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\
;-----------------------------------------------------

	;Allocate memory to reduced 2D distribution
	dist2D = fltarr(nTheta_bins, nE_bins)
	
	;Loop over bins
	for j = 0, nEnergy - 1 do begin
		for k = 0, n_elements(chist) - 1 do begin
			;Skip empty bins
			if ri[k] eq ri[k+1] then continue

			;Source indices
			inds = ri[ri[k]:ri[k+1]-1]
			isrc = array_indices([nPhi,nTheta], inds, /DIMENSIONS)
			
			;Weight
			w = weight[isrc[0,*], isrc[1,*]]

			;Re-bin
			dist2D[k, j] = total( (*self.distFn)[isrc[0,*], isrc[1,*], j] * w ) / total(w)
		endfor
	endfor

;-----------------------------------------------------
; Output Bins \\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\
;-----------------------------------------------------
	
	;Create bins
	dTheta  = (theta_range[1] - theta_range[0]) / nTheta_bins
	theta   = theta_range[0] + dTheta*findgen(nTheta_bins) + dTheta/2.0
	dTheta /= 2.0
	
	energy = *self.energy

	;Return the 2D distribution
	return, dist2D
end


;+
;   Reduce a single 3D distribution function to 1D by averaging over
;   energy and azimuth.
;
; :Params:
;       THETA:          out, optional, type=fltarr
;                       Angles (degrees) of the polar bin centers of the distribution.
;       DTHETA:         out, optional, type=float
;                       Half-width of the `THETA` bins.
;
; :Keywords:
;       E_RANGE:        in, optional, type=fltarr(2), default=[min, max]
;                       The range in energy, in electron volts (eV) over which to average.
;       NTHETA_BINS:    in, optional, type=integer
;                       Number of polar angle bins in the reduced distribution. The
;                           default is to use the same bins and the original distribution.
;       PHI_RANGE:      in, optional, type=fltarr(2), default=[0.0\, 360.0]
;                       The range in azimuthal angle (degrees) over which to average.
;
; :Returns:
;       DIST1D:         out, required, type=fltarr(L)
;                       The 1D distribution in polar angle.
;-
function MrDist3D::ThetaSpec, theta, dTheta, $
E_RANGE=e_range, $
NTHETA_BINS=nTheta_bins, $
PHI_RANGE=phi_range
	compile_opt idl2
	on_error, 2

;-----------------------------------------------------
; Defaults \\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\
;-----------------------------------------------------

	;Constants
	eV_to_J = 1.602e-19
	m_to_km = 1e-3

	;Dimension sizes
	dims    = size(*self.distFn, /DIMENSIONS)
	nPhi    = dims[0]
	nTheta  = dims[1]
	nEnergy = dims[2]

	;PA range over which to average
	theta_range = [0, 180]
	if n_elements(phi_range)   eq 0 then phi_range   = [0.0, 360.0]
	if n_elements(nTheta_bins) eq 0 then nTheta_bins = nTheta

	;Energy range over which to average
	dE = self -> DeltaE()
	if n_elements(e_range) eq 0 then begin
		e_range = [ (*self.energy)[0]  - (*self.energy)[0]  * dE[0]  / exp(1), $
		            (*self.energy)[-1] + (*self.energy)[-1] * dE[-1] / exp(1) ]
	endif

;-----------------------------------------------------
; Coordinate Space \\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\
;-----------------------------------------------------
	
	;Combine data into 3-vectors of [phi, theta, energy]
	coords = transpose( [ [reform( rebin( *self.phi,   nPhi, nTheta, nEnergy ), nPhi*nTheta*nEnergy ) ], $
	                      [reform( rebin( *self.theta, nPhi, nTheta, nEnergy ), nPhi*nTheta*nEnergy ) ], $
	                      [reform( rebin( reform( *self.energy, 1, 1, nEnergy ), nPhi, nTheta, nEnergy ), nPhi*nTheta*nEnergy ) ] ] )

;-----------------------------------------------------
; Weight \\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\
;-----------------------------------------------------
	;
	;   - w = v^2 * sin(theta) * dv * dTheta * dPhi
	;   - Applied as total(w*psd)/total(w)
	;   - Units of w cancel, so does not need same units as psd.
	;   - dTheta and dPhi are constants, so will factor out
	;   - Elements from two different old theta bins can fall into the same
	;     new theta bin, so the sin(theta) term does not cancel.
	;   => w = v^2 * sin(theta) * dv 
	;
	
	;Velocity element
	v_sqr    = *self.energy * ( (2*eV_to_J*m_to_km^2)/self.mass )
	dv       = sqrt(1.0 / (2.0 * self.mass * *self.energy) ) * dE * sqrt(eV_to_J)*m_to_km
	sinTheta = sin(*self.theta * !dtor)
	
	;Weight function
	weight = rebin( reform(v_sqr, 1, 1, nEnergy), nPhi, nTheta, nEnergy ) * $
	         rebin( sinTheta,                     nPhi, nTheta, nEnergy ) * $
	         rebin( reform(dv, 1, 1, nEnergy),    nPhi, nTheta, nEnergy )
;	weight = replicate(1.0, nPhi, nTheta, nEnergy)

;-----------------------------------------------------
; Re-bin Data \\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\
;-----------------------------------------------------
	
	;Locate data within new bins
	;   - Keep the number of Theta bins the same
	;   - Average all relevant PHI and ENERGY values.
	cHist  = hist_nd(coords, $
	                 MIN             = [phi_range[0], theta_range[0], e_range[0]], $
	                 MAX             = [phi_range[1], theta_range[1], e_range[1]], $
	                 NBINS           = [           1,    nTheta_bins,          1], $
	                 REVERSE_INDICES = ri)

;-----------------------------------------------------
; Reduce to 1D \\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\
;-----------------------------------------------------
	
	;Allocate memory to reduced 2D distribution
	dist1D = fltarr(nTheta_bins)

	;Loop over bins
	for k = 0, n_elements(chist) - 1 do begin
		;Skip empty bins
		if ri[k] eq ri[k+1] then continue

		;Source indices
		inds = ri[ri[k]:ri[k+1]-1]

		;Re-bin
		dist1D[k] = total( (*self.distFn)[inds] * weight[inds] ) / total(weight[inds])
	endfor

;-----------------------------------------------------
; Output Bins \\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\
;-----------------------------------------------------
	
	;Create bins
	dTheta  = (theta_range[1] - theta_range[0]) / nTheta_bins
	theta   = theta_range[0] + dTheta*findgen(nTheta_bins) + dTheta/2.0
	dTheta /= 2.0

	;Return the 1D distribution
	return, dist1D
end


;+
;   Set object properties.
;
; :Params:
;       DISTFN:             out, optional, type=NxMxL float
;                           Three-dimensional distribution function.
;       THETA:              out, optional, type=NxM float
;                           Polar location of each pixel in `DISTFN`.
;       PHI:                out, optional, type=NxM float
;                           Azimuthal location of each pixel in `DISTFN`.
;       ENERGY:             out, optional, type=Lx1 float
;                           Energy location of each pixel in `DISTFN`.
;-
pro MrDist3D::GetData, distfn, phi, theta, energy
	compile_opt idl2
	on_error, 2
	
	;Get the data
	if arg_present(distfn) then distfn = *self.distfn
	if arg_present(theta)  then theta  = *self.theta
	if arg_present(phi)    then phi    = *self.phi
	if arg_present(energy) then energy = *self.energy
end


;+
;   Get object properties.
;
; :Keywords:
;       DEGREES:            out, optional, type=boolean
;                           If set, the THETA and PHI properties have units of degrees.
;       RADIANS:            out, optional, type=boolean
;                           If set, the THETA and PHI properties have units of radians.
;       UNITS:              out, optional, type=string
;                           Units of the distribution function.
;-
pro MrDist3D::GetProperty, $
DEGREES=degrees, $
MASS=mass, $
RADIANS=radians, $
UNITS=units
	compile_opt idl2
	on_error, 2
	
	if arg_present(degrees) then degrees = self.degrees
	if arg_present(mass)    then mass    = self.mass
	if arg_present(radians) then radians = ~self.degrees
	if arg_present(units)   then units   = self.units
end


;+
;   Reduce a single 3D distribution function to 2D by averaging over polar angle.
;
; :Params:
;       PHI:            out, optional, type=fltarr
;                       Azimuthal angle (degrees) bins of the distribution.
;       ENERGY:         out, optional, type=fltarr
;                       Energy (eV) bins of the distribution.
;
; :Keywords:
;       NE_BINS:        in, optional, type=integer
;                       Number of energy bins in the reduced distribution. The default
;                           is to use the same bins and the original distribution.
;       NPHI_BINS:      in, optional, type=integer
;                       Number of phi bins in the reduced distribution. The default
;                           is to use the same bins and the original distribution.
;       THETA_RANGE:    in, optional, type=fltarr(2), default=[0.0\, 180.0]
;                       The range in polar angle (degrees) over which to average.
;
; :Returns:
;       DIST2D:         out, required, type=fltarr(L)
;                       The 2D distribution with size nEnergy.
;-
function MrDist3D::PhiE, phi, energy, dPhi, dE, $
NE_BINS=nE_bins, $
NPHI_BINS=nphi_bins, $
THETA_RANGE=theta_range
	compile_opt idl2
	on_error, 2

;-----------------------------------------------------
; Defaults \\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\
;-----------------------------------------------------

	;Dimension sizes
	dims    = size(*self.distFn, /DIMENSIONS)
	nPhi    = dims[0]
	nTheta  = dims[1]
	nEnergy = dims[2]

	;Amount to average over THETA
	phi_range = [-180, 180]
	if n_elements(theta_range) eq 0 then theta_range = [0.0, 180.0]
	if n_elements(nPhi_bins)   eq 0 then nPhi_bins   = nPhi
	if n_elements(nE_bins)     gt 0 then MrPrintF, 'logwarn', 'TODO: Allow nE_bins to change.'
	
	;Keep the number of energy bins the same.
	;   - Do not use NE_BINS to prevent cyclic messages there ---^
	nBins = nEnergy

;-----------------------------------------------------
; Coordinate Space \\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\
;-----------------------------------------------------

	;Locate data within new bins
	;   - Keep the number of Theta bins the same
	;   - Average all PHI values within [-DELTA, DELTA] in each theta bin
	coords = transpose( [ [reform(*self.phi,   nPhi*nTheta)], $
	                      [reform(*self.theta, nPhi*nTheta)] ] )

;-----------------------------------------------------
; Weight \\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\
;-----------------------------------------------------
	;
	;Weight
	;   - Weight by size of old element
	;   - theta element: v*dtheta.
	;   - Weight is applied as total(w*psd)/total(w).
	;   - v and dtheta are constant, so the weight is 1.
	;

;-----------------------------------------------------
; Re-bin Data \\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\
;-----------------------------------------------------
	
	cHist  = hist_nd(coords, $
	                 MIN             = [ phi_range[0], theta_range[0]], $
	                 MAX             = [ phi_range[1], theta_range[1]], $
	                 NBINS           = [    nPhi_bins,              1], $
	                 REVERSE_INDICES = ri)

;-----------------------------------------------------
; Reduce to 2D \\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\
;-----------------------------------------------------

	;Allocate memory to reduced 2D distribution
	dist2D = fltarr(nPhi_bins, nBins)

	;Loop over each PHI bin
	for j = 0, nEnergy - 1 do begin
		for k = 0, n_elements(chist) - 1 do begin
			;Skip empty bins
			if ri[k] eq ri[k+1] then continue
		
			;Source indices
			inds  = ri[ri[k]:ri[k+1]-1]
			isrc  = array_indices([nPhi,nTheta], inds, /DIMENSIONS)
	
			;Re-bin
			dist2D[k,j] = mean( (*self.distFn)[isrc[0,*], isrc[1,*], j] )
		endfor
	endfor

;-----------------------------------------------------
; Output Bins \\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\
;-----------------------------------------------------
	
	;Create bins
	dPhi  = (phi_range[1] - phi_range[0]) / nPhi_bins
	phi   = phi_range[0] + dPhi*findgen(nPhi_bins) + dPhi/2.0
	dPhi /= 2.0
	
	energy = *self.energy

	;Return the 2D distribution
	return, dist2D
end


;+
;   Reduce a single 3D distribution function to 1D by averaging over
;   energy and polar angle.
;
; :Params:
;       PHI:            out, optional, type=fltarr
;                       Azimuth angles (degrees) of the bin centers of the distribution.
;       DPHI:           out, optional, type=float
;                       Half-width of the `PHI` bins.
;
; :Keywords:
;       E_RANGE:        in, optional, type=fltarr(2), default=[min, max]
;                       The range in energy, in electron volts (eV) over which to average.
;       NPHI_BINS:      in, optional, type=integer
;                       Number of polar angle bins in the reduced distribution. The
;                           default is to use the same bins and the original distribution.
;       THETA_RANGE:    in, optional, type=fltarr(2), default=[0.0\, 180.0]
;                       The range in polar angle (degrees) over which to average.
;
; :Returns:
;       DIST1D:         out, required, type=fltarr(L)
;                       The 1D distribution with size nEnergy.
;-
function MrDist3D::PhiSpec, phi, dPhi, $
E_RANGE=e_range, $
NPHI_BINS=nPhi_bins, $
THETA_RANGE=theta_range
	compile_opt idl2
	on_error, 2

;-----------------------------------------------------
; Defaults \\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\
;-----------------------------------------------------

	;Dimension sizes
	dims    = size(*self.distFn, /DIMENSIONS)
	nPhi    = dims[0]
	nTheta  = dims[1]
	nEnergy = dims[2]

	;PA range over which to average
	phi_range = [-180, 180]
	if n_elements(theta_range) eq 0 then theta_range = [0.0, 180.0]
	if n_elements(nPhi_bins)   eq 0 then nPhi_bins   = nPhi

	;Energy range over which to average
	dE = self -> DeltaE()
	if n_elements(e_range) eq 0 then begin
		e_range = [ (*self.energy)[0]  - (*self.energy)[0]  * dE[0]  / exp(1), $
		            (*self.energy)[-1] + (*self.energy)[-1] * dE[-1] / exp(1) ]
	endif


;-----------------------------------------------------
; Coordinate Space \\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\
;-----------------------------------------------------
	
	;Combine data into 3-vectors of [phi, theta, energy]
	coords = transpose( [ [reform( rebin( *self.phi,   nPhi, nTheta, nEnergy ), nPhi*nTheta*nEnergy ) ], $
	                      [reform( rebin( *self.theta, nPhi, nTheta, nEnergy ), nPhi*nTheta*nEnergy ) ], $
	                      [reform( rebin( reform( alog10( *self.energy ), 1, 1, nEnergy ), nPhi, nTheta, nEnergy ), nPhi*nTheta*nEnergy ) ] ] )

;-----------------------------------------------------
; Weight \\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\
;-----------------------------------------------------
	
	;
	;   - w = v^2 * sin(theta) * dv * dTheta * dPhi
	;   - Applied as total(w*psd)/total(w)
	;   - Units of w cancel, so does not need same units as psd.
	;   - dTheta and dPhi are constants, so will factor out
	;   - Elements from two different old theta bins can fall into the same
	;     new theta bin, so the sin(theta) term does not cancel.
	;   => w = v^2 * sin(theta) * dv 
	;
	
	;Velocity element
	vsqr     = 2.0 * *self.energy / self.mass
	dv       = sqrt( 1.0 / (2.0 * self.mass * *self.energy ) ) * dE
	sinTheta = sin(*self.theta * !dtor)
	
	;Weight function
	weight = rebin( reform(vsqr, 1, 1, nEnergy), nPhi, nTheta, nEnergy ) * $
	         rebin( sin( *self.theta * !dtor ),  nPhi, nTheta, nEnergy ) * $
	         rebin( reform(dv, 1, 1, nEnergy),   nPhi, nTheta, nEnergy )

;-----------------------------------------------------
; Re-bin Data \\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\
;-----------------------------------------------------
	
	;Locate data within new bins
	;   - Keep the number of Theta bins the same
	;   - Average all PHI values within [-DELTA, DELTA] in each theta bin
	cHist  = hist_nd(coords, $
	                 MIN             = [ phi_range[0], theta_range[0], e_range[0]], $
	                 MAX             = [ phi_range[1], theta_range[1], e_range[1]], $
	                 NBINS           = [    nPhi_bins,              1,          1], $
	                 REVERSE_INDICES = ri)

;-----------------------------------------------------
; Reduce to 1D \\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\
;-----------------------------------------------------
	
	;Allocate memory to reduced 2D distribution
	dist1D = fltarr(nPhi)

	;Loop over bins
	for k = 0, n_elements(chist) - 1 do begin
		;Skip empty bins
		if ri[k] eq ri[k+1] then continue

		;Source indices
		inds = ri[ri[k]:ri[k+1]-1]
;		isrc = array_indices([nPhi,nTheta,nEnergy], inds, /DIMENSIONS)
		
		;Weight
;		w = weight[isrc[0,*], isrc[1,*], isrc[2,*]]

		;Re-bin
;		dist1D[k] = total( reform(distFn[isrc[0,*], isrc[1,*], isrc[2,*]]) * w ) / total(w)
		dist1D[k] = total( (*self.distFn)[inds] * weight[inds] ) / total(weight[inds])
	endfor

;-----------------------------------------------------
; Output Bins \\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\
;-----------------------------------------------------
	
	;Create bins
	dPhi  = (phi_range[1] - phi_range[0]) / nPhi_bins
	phi   = phi_range[0] + dPhi*findgen(nPhi_bins) + dPhi/2.0
	dPhi /= 2.0

	;Return the 1D distribution
	return, dist1D
end


;+
;   Set object properties.
;
; :Params:
;       DISTFN:             in, required, type=NxMxL float
;                           Three-dimensional distribution function.
;       PHI:                in, required, type=NxM float
;                           Azimuthal location of each pixel in `DISTFN`.
;       THETA:              in, required, type=NxM float
;                           Polar location of each pixel in `DISTFN`.
;       ENERGY:             in, required, type=Lx1 float
;                           Energy location of each pixel in `DISTFN`.
;-
pro MrDist3D::SetData, distfn, phi, theta, energy, $
DEGREES=degrees, $
RADIANS=radians, $
UNITS=units
	compile_opt idl2
	on_error, 2
	
	;Unis
	if n_elements(units) eq 0 then units = 'E FLUX'
	if n_elements(degrees) eq 0 && n_elements(radians) eq 0 then degrees = 1B
	tf_degrees = n_elements(radians) gt 0 ? ~keyword_set(radians) : keyword_set(degrees)
	
	nDims   = size(distfn, /N_DIMENSIONS)
	dims    = size(distfn, /DIMENSIONS)
	nPhi    = dims[0]
	nTheta  = dims[1]
	nEnergy = dims[2]

;-----------------------------------------------------
; Distribution Function \\\\\\\\\\\\\\\\\\\\\\\\\\\\\\
;-----------------------------------------------------

	;Distribution function must be 3D: [nPhi, nTheta, nEnergy]
	if nDims ne 3 then message, 'DISTFN must have 3 dimensions.'

;-----------------------------------------------------
; Phi \\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\
;-----------------------------------------------------

	;Phi must be 2D: [nPhi, nTheta]
	nDims = size(phi, /N_DIMENSIONS)
	dims  = size(phi, /DIMENSIONS)
	
	;1D
	if nDims eq 1 then begin
		;Reform from [nPhi] to [nPhi, nTheta]
		if dims[0] ne nPhi $
			then message, 'PHI must be the size of the first dimension of DISTFN.' $
			else phOut = rebin( phi, nPhi, nTheta )
		
	;2D
	endif else if nDims eq 2 then begin
		if (dims[0] ne nPhi) || (dims[1] ne nTheta) $
			then message, 'PHI must have dimensions of [nPhi, nTheta].' $
			else phOut = phi
	
	;3+D
	endif else begin
		message, 'PHI must have one or two dimensions.'
	endelse

;-----------------------------------------------------
; Theta \\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\
;-----------------------------------------------------
	;Theta must be 2D: [nPhi, nTheta]
	nDims = size(theta, /N_DIMENSIONS)
	dims  = size(theta, /DIMENSIONS)
	
	;1D
	if nDims eq 1 then begin
		;Reform from [nTheta] to [nPhi, nTheta]
		if dims[0] ne nTheta $
			then message, 'THETA must be the size of the second dimension of DISTFN.' $
			else thOut = rebin( reform(theta, 1, nTheta), nPhi, nTheta )
		
	;2D
	endif else if nDims eq 2 then begin
		if (dims[0] ne nPhi) || (dims[1] ne nTheta) $
			then message, 'THETA must have dimensions of [nPhi, nTheta].' $
			else thOut = theta
	
	;3+D
	endif else begin
		message, 'THETA must have one or two dimensions.'
	endelse

;-----------------------------------------------------
; Energy \\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\
;-----------------------------------------------------

	;Energy must be a row or column vector
	nDims = size(energy, /N_DIMENSIONS)
	dims  = size(energy, /DIMENSIONS)
	if (nDims eq 2 && dims[0] ne 1) || nDims ge 3 then message, 'ENERGY must be a column or row vector.'
	if n_elements(energy) ne nEnergy $
		then message, 'ENERGY must be the size of third dimension of DISTFN.'

;-----------------------------------------------------
; Set Properties \\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\
;-----------------------------------------------------
	
	;Set data
	*self.distfn = distfn
	*self.phi    = tf_degrees ? temporary(phOut) : temporary(phOut) * !radeg
	*self.theta  = tf_degrees ? temporary(thOut) : temporary(thOut) * !radeg
	*self.energy = energy
	self.degrees = 1B
	self.units   = units
end


;+
;   Convert the distribution function from one set of units to another.
;
; :Params:
;       FLUX:           in, required, type=fltarr
;                       Distribution function for which to convert units
;       TO_UNITS:       in, required, type=string
;                       Convert to these units.  Options are::
;                           'ENERGY'      - eV
;                           'EFLUX'       - eV / cm^2 / s / sr / eV    or    keV / cm^2 / s / sr / keV
;                           'DIFF FLUX'   - # / cm^2 / s / sr / keV
;                           'PSD'         - s^2 / km^6
;                           'DF'          - s^2 / km^6
;       FROM_UNITS:     in, optional, type=string, default='PSD'
;                       Name of the units of `FLUX`.
;
; :Keywords:
;       SPECIES:        in, optional, type=string, default='e'
;                       Particle species measured in the distribution. Particle mass
;                           is taken into account when converting 'PSD' <-> {'EFLUX' | 'DIFF FLUX'}
;       ENERGY:         in, optional, type=float
;                       Energy bins at which the distribution is taken.
;
; :Returns:
;       NEW_FLUX:       Distribution with new units.
;-
function MrDist3D::SetUnits, flux, to_units, from_units, $
SPECIES = species, $
ENERGY  = energy
	compile_opt idl2
	on_error, 2
	
	to   = strupcase(to_units)
	from = n_elements(from_units) eq 0 ? 'PSD' : strupcase(from_units)

	;Proton mass, kg
	m = 1.6726219e-27

	;Mass relative to the proton mass.
	case species of
	   'i':   A = 1        ; H+
	   'H':   A = 1        ; H+
	   'He':  A = 4        ; He+
	   'He2': A = 4        ; He++
	   'O':   A = 16       ; O+
	   'e':   A = 1./1836  ; e-
	endcase

	;Conversion factor from energy flux to phase space density
	;   - mass^2 * (sr / eV^2)
	eflux_to_psd = A^2 * 0.5447 * 1.0e6
	
	;Energy Flux
	if from eq 'EFLUX' then begin
		
		;Convert to:
		case to of
			'DIFF FLUX': new_flux = flux / energy*1e3
			'PSD':       new_flux = eflux_to_psd * flux / energy^2
			'DF':        new_flux = eflux_to_psd * flux / energy^2
			'EFLUX':     new_flux = flux
			else: message, 'Cannot convert from "' + from + '" to "' + to + '".'
		endcase
	
	;Differential flux
	endif else if from eq 'DIFF FLUX' then begin
		eflux = flux * energy * 1e-3
		
		;Convert to:
		case to of
			'EFLUX':     new_flux = eflux
			'PSD':       new_flux = eflux_to_psd * eflux / energy^2
			'DF':        new_flux = eflux_to_psd * eflux / energy^2
			'DIFF FLUX': new_flux = flux
			else: message, 'Cannot convert from "' + from + '" to "' + to + '".'
		endcase
	
	;Phase space density
	endif else if from eq 'PSD' || from eq 'DF' then begin
		eflux = flux * energy * 1.e-3
		
		;Convert to:
		case to of
			'EFLUX':     new_flux = flux / eflux_to_psd * energy^2
			'DIFF FLUX': new_flux = flux / eflux_to_psd * energy*1e3
			'PSD':       new_flux = flux
			'DF':        new_flux = flux
			else: message, 'Cannot convert from "' + from + '" to "' + to + '".'
		endcase
	
	;Invalid
	endif else begin
		message, 'Invalid FROM_UNIT: "' + from_unit + '".'
	endelse
	
	;Return results
	return, new_flux
end


;+
;   Set object properties.
;
; :Keywords:
;       MASS:               in, optional, type=float
;                           Mass (kg) of the species represented in the distribution.
;       SPECIES:            in, optional, type=string
;                           Species of particle represented in the distribution. Options
;                               are: { 'e' | 'p' | 'H' | 'He' | 'O' }. This keyword
;                               sets the MASS property and takes precendence over the
;                               `MASS` keyword.
;-
pro MrDist3D::SetProperty, $
MASS=mass, $
SPECIES=species
	compile_opt idl2
	on_error, 2
	
	if n_elements(mass)    gt 0 then self.mass = mass
	if n_elements(species) gt 0 then self.mass = MrConstants('m_' + species)
end


;+
;   Reduce a single 3D distribution function to 2D by averaging over energy.
;
; :Categories:
;   Distribution Function
;
;
; :Params:
;       THETA:          out, optional, type=fltarr
;                       Polar angle bins (degrees) of the distribution.
;       PHI:            out, optional, type=fltarr
;                       Azimuthal angle bins (degrees) of the distribution.
;
; :Keywords:
;       E_RANGE:        in, optional, type=fltarr(2), default=[min, max]
;                       The range in energy, in electron volts (eV) over which to average.
;       NPHI_BINS:      in, optional, type=integer
;                       Number of phi bins in the reduced distribution. The default
;                           is to use the same bins and the original distribution.
;       NTHETA_BINS:    in, optional, type=integer
;                       Number of polar angle bins in the reduced distribution. The
;                           default is to use the same bins and the original distribution.
;
; :Returns:
;       DIST1D:         out, required, type=fltarr(L)
;                       The 1D distribution with size nEnergy.
;-
function MrDist3D::ThetaPhi, theta, phi, $
E_RANGE=E_Range, $
NPHI_BINS=nPhi_bins, $
NTHETA_BINS=nTheta_bins
	compile_opt idl2
	on_error, 2

;-----------------------------------------------------
; Defaults \\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\
;-----------------------------------------------------

	;Dimension sizes
	dims    = size(*self.distFn, /DIMENSIONS)
	nPhi    = dims[0]
	nTheta  = dims[1]
	nEnergy = dims[2]

	;Energy range over which to average
	dE = self -> DeltaE()
	if n_elements(ERange) eq 0 then begin
		ERange = [ (*self.energy)[0]  - (*self.energy)[0]  * dE[0]  / exp(1), $
		           (*self.energy)[-1] + (*self.energy)[-1] * dE[-1] / exp(1) ]
	endif
	
	;Bins & mass
	phi_range   = [-180, 180]
	theta_range = [0, 180]
	if n_elements(nTheta_bins) eq 0 then nTheta_bins = nTheta_bins
	if n_elements(nPhi_bins)   eq 0 then nPhi_bins   = nPhi_bins

;-----------------------------------------------------
; Coordinate Space \\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\
;-----------------------------------------------------
	
	;Combine data into 3-vectors of [phi, theta, energy]
	coords = transpose( [ [reform( rebin( *self.phi,   nPhi, nTheta, nEnergy ), nPhi*nTheta*nEnergy ) ], $
	                      [reform( rebin( *self.theta, nPhi, nTheta, nEnergy ), nPhi*nTheta*nEnergy ) ], $
	                      [reform( rebin( reform( alog10( *self.energy ), 1, 1, nEnergy ), nPhi, nTheta, nEnergy ), nPhi*nTheta*nEnergy ) ] ] )

;-----------------------------------------------------
; Weight \\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\
;-----------------------------------------------------
	;
	;Weight
	;   - w = v^2 * sin(theta) * dv * dTheta * dPhi
	;   - Applied as total(w*psd)/total(w)
	;   - Units of w cancel, so does not need same units as psd.
	;   - dTheta and dPhi are constants, so will factor out
	;   - Elements from two different old theta bins can fall into a new theta bin,
	;     so the sin(theta) term does not cancel.
	;   => w = v^2 * sin(theta) * dv 
	;
	
	;Velocity element
	vsqr     = 2.0 * *self.energy / self.mass
	dv       = sqrt( 1.0 / (2.0 * self.mass * *self.energy ) ) * dE
	sinTheta = sin( *self.theta * !dtor)
	
	;Weight function
	weight = rebin( reform(vsqr, 1, 1, nEnergy), nPhi, nTheta, nEnergy ) * $
	         rebin( sin( *self.theta * !dtor ),  nPhi, nTheta, nEnergy ) * $
	         rebin( reform(dv, 1, 1, nEnergy),   nPhi, nTheta, nEnergy )

;-----------------------------------------------------
; Re-bin Data \\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\
;-----------------------------------------------------
	
	;Locate data within new bins
	;   - Keep the number of Theta bins the same
	;   - Average all PHI values within [-DELTA, DELTA] in each theta bin
	cHist  = hist_nd(coords, $
	                 MIN             = [ phi_range[0], theta_range[0], E_Range[0]], $
	                 MAX             = [ phi_range[1], theta_range[1], E_Range[1]], $
	                 NBINS           = [   nPhi_bins,     nTheta_bins,          1], $
	                 REVERSE_INDICES = ri)

;-----------------------------------------------------
; Reduce to 2D \\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\
;-----------------------------------------------------
	;Allocate memory to reduced 2D distribution
	dist2D = fltarr(nPhi_bins, nTheta_bins)

	;Loop over bins
	for k = 0, n_elements(chist) - 1 do begin
		;Skip empty bins
		if ri[k] eq ri[k+1] then continue

		;Source indices
		inds = ri[ri[k]:ri[k+1]-1]
		isrc = array_indices([nPhi,nTheta,nEnergy], inds, /DIMENSIONS)
		
		;Destination indices
		idest = array_indices([nPhi,nTheta], k, /DIMENSIONS)
		
		;Weight
		w = weight[isrc[0,*], isrc[1,*], isrc[2,*]]

		;Re-bin
		dist2D[idest[0], idest[1]] = total( reform((*self.distFn)[isrc[0,*], isrc[1,*], isrc[2,*]]) * w ) / total(w)
	endfor

;-----------------------------------------------------
; Output Bins \\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\
;-----------------------------------------------------
	
	;Phi bins
	dPhi  = (phi_range[1] - phi_range[0]) / nPhi_bins
	phi   = phi_range[0] + dPhi*findgen(nPhi_bins) + dPhi/2.0
	dPhi /= 2.0
	
	;Theta bins
	dTheta  = (theta_range[1] - theta_range[0]) / nTheta_bins
	theta   = theta_range[0] + dTheta*findgen(nTheta_bins) + dTheta/2.0
	dTheta /= 2.0

	;Return the 2D distribution
	return, dist2D
end


;+
;   The class definition statement.
;
; :Params:
;       CLASS:          out, optional, type=structure
;-
pro MrDist3D__DEFINE
	compile_opt idl2
	
	class = { MrDist3D, $
	          distfn:      ptr_new(), $
	          theta:       ptr_new(), $
	          phi:         ptr_new(), $
	          energy:      ptr_new(), $
	          degrees:     0B, $
	          units:       '', $
	          mass:        0.0 $
	        }
end