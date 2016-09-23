; docformat = 'rst'
;
; NAME:
;       MrDist_GyroPA
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
;   Reduce a single 3D distribution function to 2D by averaging over energy.
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
;                       The azimuthal coordinates on a spherical grid where each point
;                           in `DISTFN` is defined.
;       THETA:          in, required, type=fltarr(M)
;                       The polar coordinates on a spherical grid where each point
;                           in `DISTFN` is defined.
;       ENERGY:         in, required, type=fltarr(L)
;                       The energy bins, in electron volts, at which each point in
;                           `DISTFN` is defined.
;
; :Keywords:
;       E_RANGE:        in, optional, type=fltarr(2), default=[min, max]
;                       The range in energy, in electron volts (eV) over which to average.
;       MASS:           in, optional, type=float, default=1.6726219e-27 (kg, H+)
;                       The mass, in kilograms, of the particle species measured in
;                           the distribution function.
;       NPHI_BINS:      in, optional, type=integer, default=nTheta
;                       The number of evently spaced azimuth angle bins for the reduced
;                           distribution.
;       NTHETA_BINS:    in, optional, type=integer, default=nTheta
;                       The number of evently spaced polar angle bins for the reduced
;                           distribution.
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
function MrDist_GyroPA, distFn, phi, theta, energy, $
E_RANGE=E_Range, $
MASS=mass, $
NPHI_BINS=nPhi_bins, $
NTHETA_BINS=nTheta_bins
	compile_opt idl2
	on_error, 2

;-----------------------------------------------------
; Defaults \\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\
;-----------------------------------------------------

	;Dimension sizes
	dims    = size(distFn, /DIMENSIONS)
	nPhi    = dims[0]
	nTheta  = dims[1]
	nEnergy = dims[2]

	;Energy range over which to average
	dE = MrDist_DeltaE(energy)
	if n_elements(ERange) eq 0 then begin
		ERange = [ energy[0]  - energy[0]  * dE[0]  / exp(1), $
		           energy[-1] + energy[-1] * dE[-1] / exp(1) ]
	endif
	
	;Bins & mass
	if n_elements(mass)        eq 0 then mass        = 1.6726219e-27  ;Proton mass (kg)
	if n_elements(nTheta_bins) eq 0 then nTheta_bins = nTheta
	if n_elements(nPhi_bins)   eq 0 then nPhi_bins   = nPhi

;-----------------------------------------------------
; Coordinate Space \\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\
;-----------------------------------------------------
	
	;Combine data into 3-vectors of [phi, theta, energy]
	coords = transpose( [ [reform( rebin(phi,   nPhi, nTheta, nEnergy), nPhi*nTheta*nEnergy)], $
	                      [reform( rebin(theta, nPhi, nTheta, nEnergy), nPhi*nTheta*nEnergy)], $
	                      [reform( rebin( reform( alog10(energy), 1, 1, nEnergy), nPhi, nTheta, nEnergy ), nPhi*nTheta*nEnergy)] ] )

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
	vsqr     = 2.0 * energy / mass
	dv       = sqrt( 1.0 / (2.0 * mass * energy ) ) * dE
	sinTheta = sin(theta*!dtor)
	
	;Weight function
	weight = rebin( reform(vsqr, 1, 1, nEnergy),   nPhi, nTheta, nEnergy ) * $
	         rebin( sin( temporary(theta)*!dtor ), nPhi, nTheta, nEnergy ) * $
	         rebin( reform(dv, 1, 1, nEnergy),     nPhi, nTheta, nEnergy )

;-----------------------------------------------------
; Re-bin Data \\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\
;-----------------------------------------------------
	
	;Locate data within new bins
	;   - Keep the number of Theta bins the same
	;   - Average all PHI values within [-DELTA, DELTA] in each theta bin
	cHist  = hist_nd(coords, $
	                 MIN             = [   0.0,    0.0, E_Range[0]], $
	                 MAX             = [ 360.0,  180.0, E_Range[1]], $
	                 NBINS           = [  nPhi, nTheta,          1], $
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
		dist2D[idest[0], idest[1]] = total( reform(distFn[isrc[0,*], isrc[1,*], isrc[2,*]]) * w ) / total(w)
	endfor

	;Return the 2D distribution
	return, dist2D
end
