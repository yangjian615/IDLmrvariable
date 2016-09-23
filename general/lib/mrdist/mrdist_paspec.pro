; docformat = 'rst'
;
; NAME:
;       MrDist_PASpec
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
; PURPOSE:
;+
;   Reduce a single 3D distribution function to 1D by averaging over
;   energy and azimuth.
;
; :Categories:
;   Distribution Function
;
;
; :Params:
;       DISTFN:         in, required, type=NxMxL float
;                       The 3D distribution function to be reduced. Its dimensions must
;                           be [nPhi, nTheta, nEnergy].
;       PHI:            in, required, type=fltarr(N)
;                       The azimuthal coordinates (degrees) on a spherical grid where
;                           each point in `DISTFN` is defined.
;       THETA:          in, required, type=fltarr(M)
;                       The polar coordinates (degrees) on a spherical grid where
;                           each point in `DISTFN` is defined.
;       ENERGY:         in, required, type=fltarr(L)
;                       The energy bins, in electron volts (eV), at which each point in
;                           `DISTFN` is defined.
;
; :Keywords:
;       E_RANGE:        in, optional, type=fltarr(2), default=[min, max]
;                       The range in energy, in electron volts (eV) over which to average.
;       MASS:           in, optional, type=float, default=1.6726219e-27 (kg, H+)
;                       The mass, in kilograms, of the particle species measured in
;                           the distribution function.
;       NTHETA_BINS:    in, optional, type=integer, default=nTheta
;                       The number of evently spaced polar angle bins for the reduced
;                           distribution.
;       PHI_RANGE:      in, optional, type=fltarr(2), default=[0.0\, 360.0]
;                       The range in azimuth (degrees) over which to average.
;
; :Returns:
;       DIST1D:         out, required, type=fltarr(L)
;                       The 1D distribution with size nEnergy.
;
; :Author:
;    Matthew Argall::
;    University of New Hampshire
;    Morse Hall Room 348
;    8 College Road
;    Durham, NH 03824
;    matthew.argall@unh.edu
;
; :History:
;    Modification History::
;       2016/08/26  -   Written by Matthew Argall
;-
function MrDist_PASpec, distFn, phi, theta, energy, $
E_RANGE=e_range, $
MASS=mass, $
NTHETA_BINS=nBins, $
PHI_RANGE=gyro_range
	compile_opt idl2
	on_error, 2

;-----------------------------------------------------
; Defaults \\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\
;-----------------------------------------------------

	;Constants
	eV_to_J = 1.602e-19
	m_to_km = 1e-3

	;Dimension sizes
	dims    = size(distFn, /DIMENSIONS)
	nPhi    = dims[0]
	nTheta  = dims[1]
	nEnergy = dims[2]

	;PA range over which to average
	if n_elements(mass)       eq 0 then mass       = 1.6726219e-27  ;Proton mass (kg)
	if n_elements(gyro_range) eq 0 then gyro_range = [0.0, 360.0]
	if n_elements(nBins)      eq 0 then nBins      = nTheta

	;Energy range over which to average
	dE = MrDist_DeltaE(energy)
	if n_elements(e_range) eq 0 then begin
		e_range = [ energy[0]  - energy[0]  * dE[0]  / exp(1), $
		            energy[-1] + energy[-1] * dE[-1] / exp(1) ]
	endif

;-----------------------------------------------------
; Coordinate Space \\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\
;-----------------------------------------------------
	
	;Combine data into 3-vectors of [phi, theta, energy]
	coords = transpose( [ [reform( rebin(phi,   nPhi, nTheta, nEnergy), nPhi*nTheta*nEnergy)], $
	                      [reform( rebin(theta, nPhi, nTheta, nEnergy), nPhi*nTheta*nEnergy)], $
	                      [reform( rebin( reform( energy, 1, 1, nEnergy), nPhi, nTheta, nEnergy ), nPhi*nTheta*nEnergy)] ] )

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
	v_sqr    = energy * ( (2*eV_to_J*m_to_km^2)/mass )
	dv       = sqrt(1.0 / (2.0 * mass * energy) ) * dE * sqrt(eV_to_J)*m_to_km
	sinTheta = sin(theta*!dtor)
	
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
	                 MIN             = [gyro_range[0],    0.0, e_range[0]], $
	                 MAX             = [gyro_range[1],  180.0, e_range[1]], $
	                 NBINS           = [            1,  nBins,          1], $
	                 REVERSE_INDICES = ri)

;-----------------------------------------------------
; Reduce to 1D \\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\
;-----------------------------------------------------
	
	;Allocate memory to reduced 2D distribution
	dist1D = fltarr(nBins)

	;Loop over bins
	for k = 0, n_elements(chist) - 1 do begin
		;Skip empty bins
		if ri[k] eq ri[k+1] then continue

		;Source indices
		inds = ri[ri[k]:ri[k+1]-1]

		;Re-bin
		dist1D[k] = total( distFn[inds] * weight[inds] ) / total(weight[inds])
	endfor

	;Return the 1D distribution
	return, dist1D
end
