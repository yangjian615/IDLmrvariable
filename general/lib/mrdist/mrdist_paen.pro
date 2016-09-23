; docformat = 'rst'
;
; NAME:
;       MrDist_PAEn
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
;   Reduce a single 3D distribution function to 2D by averaging over azimuth angle.
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
;
; :Keywords:
;       NTHETA_BINS:    in, optional, type=integer, default=nTheta
;                       The number of evently spaced polar angle bins for the reduced
;                           distribution.
;       PHI_RANGE:      in, optional, type=fltarr(2), default=[0.0\, 360.0]
;                       The range in azimuthal angle over which to average.
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
function MrDist_PAEn, distFn, phi, theta, $
NE_BINS=ne_bins, $
NTHETA_BINS=npa_bins, $
PHI_RANGE=gyro_range
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

	;Amount to average over PHI
	if n_elements(gyro_range) eq 0 then gyro_range = [0.0, 360.0]
	if n_elements(ne_bins)    eq 0 then ne_bins    = nEnergy
	if n_elements(npa_bins)   eq 0 then npa_bins   = nTheta

;-----------------------------------------------------
; Coordinate Space \\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\
;-----------------------------------------------------

	;Combine data into 3-vectors of [phi, theta, energy]
	coords = transpose( [ [reform(phi,   nPhi*nTheta)], $
	                      [reform(theta, nPhi*nTheta)] ] )

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
	weight = sin( theta*!dtor )

;-----------------------------------------------------
; Re-bin Data \\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\
;-----------------------------------------------------
	;Locate data within new bins
	;   - Keep the number of Theta bins the same
	;   - Average all PHI values within [-DELTA, DELTA] in each theta bin
	cHist  = hist_nd(coords, $
	                 MIN             = [gyro_range[0],      0.0], $
	                 MAX             = [gyro_range[1],    180.0], $
	                 NBINS           = [            1, npa_bins], $
	                 REVERSE_INDICES = ri)

;-----------------------------------------------------
; Reduce to 2D \\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\
;-----------------------------------------------------

	;Allocate memory to reduced 2D distribution
	dist2D = fltarr(npa_bins, ne_bins)
	
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
			dist2D[k, j] = total( distFn[isrc[0,*], isrc[1,*], j] * w ) / total(w)
		endfor
	endfor

	;Return the 2D distribution
	return, dist2D
end