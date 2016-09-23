; docformat = 'rst'
;
; NAME:
;       MrDist_Reduce2D
;
; PURPOSE:
;+
;   Reduce a 3D distribution function to a 2D distribution function.
;
;   For a 2D slice to be taken in or perpendicular to the plane containing tangent
;   vector v = v(r,theta), the distribution function must first be rotated into 
;   a coordinate system in which v lies along the z-axis.
;
; :Categories:
;   Distribution functions
;
; :Params:
;       DISTFN:         in, required, type=fltarr
;                       The distribution function to be rebinned.
;                           Dimensions [nPhi, nTheta, nEnergy]
;       PHI_BINS:       in, optional, type=fltarr
;                       Azimuthal coordinates to which data is rebinned. Dimension [nPhi].
;                           If not provided, the number of bins will be taken from the
;                           dimensions of `DISTFN`, and the bin locations will be spaced
;                           evenly from [0, 360).
;       THETA_BINS:     in, optional, type=fltarr
;                       Polar coordinates to which data is rebinned. Dimension [nTheta].
;                           If not provided, the number of bins will be taken from the
;                           dimensions of `DISTFN`, and the bin locations will be spaced
;                           evenly from [0, 180).
;       ENERGY_BINS:    in, optional, type=fltarr
;                       Energies at which the distribution function was accumulated.
;                          Dimensions [nEnergy]. If not provided, the number of bins will
;                          be taken from the dimensions of `DISTFN` and the bins will be
;                          spaced logarithmically from 10eV to 10keV.
;
; :Keywords:
;       DEGREES:        in, optional, type=float, default=1
;                       If set, all angular information is taken to be in degrees.
;       DELTA:          in, optional, type=float, default=20.0
;                       The angular range in the third dimension over which to average.
;       ORDER:          in, optional, type=integer, default=1
;                       Flag indicating how the dimensions of `DISTFN` are ordered. Options::
;                           1: [nPhi, nTheta, nEnergy]
;       PHI_ANGLE:      in, optional, type=float
;                       An angle in the azimuth plane. If given, the reduced distribution
;                           will be formed from the plane containing the parallel direction
;                           (z) and a vector v = (r,PHI_ANGLE) in the azimuthal plane. The
;                           default is to reduce the distribution to the perp1-perp2 (xy)
;                           plane.
;       V_PHI:          out, optional, type=fltarr
;                       Azimuthal coordinates in velocity space in the reduced 2D plane
;                          of each bin.
;       V_MAG:          out, optional, type=fltarr
;                       Radial coordinates in velocity space in the reduced 2D plane
;                          of each bin.
;
; :Returns:
;       DISTFN_2D:      The resulting 2D distribution function
;
; :Author:
;       Matthew Argall::
;       University of New Hampshire
;       Morse Hall, Room 348
;       8 College Rd.
;       Durham, NH, 03824
;       matthew.argall@unh.edu
;
; :History:
;   Modification History::
;       2016/03/09  -   Written by Matthew Argall
;-
function MrDist_Image, dist2D, v_mag, v_phi, $
CURRENT=current, $
LAYOUT=layout, $
NAME=name, $
_REF_EXTRA=extra
	compile_opt idl2
	on_error, 2

;-----------------------------------------------------
; Create Window \\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\
;-----------------------------------------------------

	;Createa window
	if keyword_set(current) then begin
		win = GetMrWindows(/CURRENT)
		tf_refresh = win -> GetRefresh()
		if tf_refresh then win -> Refresh, /DISABLE
	endif else begin
		win = MrWindow(OXMARGIN=[11,15], ASPECT=1.0, XSIZE=600, YSIZE=500, REFRESH=0)
		tf_refresh = 1
	endelse

;-----------------------------------------------------
; Create Image \\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\
;-----------------------------------------------------

	;Titles
	if n_elements(phi_angle) gt 0 then begin
		xtitle = 'v$\down||$ x10$\up-4$ (km/s)'
		ytitle = 'v-perp x10$\up-4$!c(km/s)'
	endif else begin
		xtitle = 'v-perp1 x10$\up-4$ (km/s)'
		ytitle = 'v-perp2 x10$\up-4$!c(km/s)'
	endelse
	
	;Name
	if n_elements(name) eq 0 then name = 'PSD 2D'
	
	;Plot the results
	;   - Put the energy dimension first
	;   - Scale the magnitude by 1e-4
	im = MrImage( dist2D, v_mag, v_phi, $
	             /AXES, $
	             /CENTER, $
	             /CURRENT, $
	             /LOG, $
	             /NAN, $
	             /POLAR, $
	             /SCALE, $
	             LAYOUT        = layout, $
;	             BOTTOM        = 5, $
	             RGB_TABLE     = 22, $
;	             MISSING_VALUE = 0, $
;	             MISSING_COLOR = 'Antique White', $
	             NAME          = name, $
	             NOCLIP        = 0, $
	             XTITLE        = xtitle, $
	             YTITLE        = ytitle, $
	             TITLE         = title)
	
;-----------------------------------------------------
; Create Contour \\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\
;-----------------------------------------------------
	;Cartesian space
;	vx = v_mag * cos(v_phi)
;	vy = v_mag * sin(v_phi)

	;Contour levels
;	levels = alog10(v_mag) / 32

	;Create contour
;	c = MrContour( alog10(dist2D), vx, vy, $
;	               C_LABELS = 0, $
;	               CLOSED   = 0, $
;	               /IRREGULAR, $
;                  NAME     = 'C: ' + name, $
;	               OVERPLOT = im, $
;	               NLEVEL   = 32 )
	
	
;-----------------------------------------------------
; Annotations \\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\
;-----------------------------------------------------
	
	;Plot a colorbar
	cb = MrColorbar( LOCATION    = 'Right', $
	                 NAME        = 'CB: ' + name, $
	                 ORIENTATION = 1, $
	                 TARGET      = im, $
	                 TITLE       = 'PSD!c(s$\up3$/cm$\up6)$', $
	                 WIDTH       = 1.0)
	
	;Change any default properties
	if n_elements(extra) gt 0 then im -> SetProperty, _STRICT_EXTRA=extra

	;Return
	if tf_refresh then win -> Refresh
	return, im
end