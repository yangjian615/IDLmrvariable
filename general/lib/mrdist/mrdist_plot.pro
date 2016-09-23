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
function MrDist_Plot, dist2D, v_mag, v_phi, $
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
; Pick Data \\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\
;-----------------------------------------------------

	;Pick out the 0-degree data
	!Null   = min(v_phi[0,*], iv_pos)
	!Null   = max(v_phi[0,*], iv_neg)
	v1      = [ reverse(-v_mag[*,iv_neg]), v_mag[*,iv_pos] ]
	dist1Da = [ reverse(dist2D[*,iv_neg]), dist2D[*,iv_pos] ]
	
	;Pick out the 90-degree data
	!Null   = min( abs(!pi/2 - v_phi[0,*]), iv_pos )
	!Null   = min( abs(3*!pi/2 - v_phi[0,*]), iv_neg )
	v2      = [ reverse(-v_mag[*,iv_neg]), v_mag[*,iv_pos] ]
	dist1Db = [ reverse(dist2D[*,iv_neg]), dist2D[*,iv_pos] ]
	
	;Remove 0s
	i0 = where(dist1Da eq 0, n0)
	if n0 gt 0 then dist1Da[i0] = !values.f_nan
	
	i0 = where(dist1Db eq 0, n0)
	if n0 gt 0 then dist1Db[i0] = !values.f_nan

;-----------------------------------------------------
; Plot \\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\
;-----------------------------------------------------

	;Legend labels
	if n_elements(phi_angle) eq 0 $
		then labels = ['v-perp1', 'v-perp2'] $
		else labels = ['v$\down||$', 'v-perp']

	;Titles
	xtitle = 'v (km/s)'
	ytitle = 'PSD (s$\up3$/cm$\up6$)'
	
	;Name
	if n_elements(name) eq 0 then name = 'PSD'

	;Plot the results
	p1 = MrPlot(v1, dist1Da, $
	            CURRENT = current, $
	            LAYOUT  = layout, $
	            NAME    = 'X: ' + name, $
	            XTITLE  = xtitle, $
	            /YLOG, $
	            YTITLE  = ytitle, $
	            TITLE   = title)
	
	;Plot the results
	p2 = MrPlot(v2, dist1Db, $
	            COLOR    = 'Blue', $
	            NAME     = 'Y: ' + name, $
	            OVERPLOT = p1, $
	            XTITLE   = xtitle, $
	            YTITLE   = ytitle, $
	            TITLE    = title)
	
	;Legend
	lnd = MrLegend(ALIGNMENT = 'SE', $
	               /AUTO_TEXT_COLOR, $
	               FILL_COLOR   = '', $
	               LABEL        = labels, $
	               LINESTYLE    = 6, $
	               NAME         = 'LEG: ' + name, $
	               POSITION     = [1.0, 0.0], $
	               /RELATIVE, $
	               SAMPLE_WIDTH = 0.0, $
	               TARGET       = [p1, p2])
	
;-----------------------------------------------------
; Annotations \\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\
;-----------------------------------------------------
	
	;Change any default properties
	if n_elements(extra) gt 0 then p1 -> SetProperty, _STRICT_EXTRA=extra

	;Return
	if tf_refresh then win -> Refresh
	return, p1
end