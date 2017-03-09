; docformat = 'rst'
;
; NAME:
;       MrDist_Sphere2Cart
;
; PURPOSE:
;+
;   Transform a spherical coordinate grid into a cartesian coordinate grid.
;
;   X, Y, and Z are assumed to have been expanded from the original phi and theta
;   coordinates (see mrdist_sphere2cart) such that they describe all grid points
;   on the unit sphere.
;
; :Categories:
;   Distribution functions
;
; :Params:
;       X:              out, optional, type=fltarr
;                       The X cartesian coordinates or points on a unit sphere.
;       Y:              in, required, type=fltarr
;                       The X cartesian coordinates or points on a unit sphere.
;       Z:              in, required, typefltarr
;                       The X cartesian coordinates or points on a unit sphere.
;       PHI:            in, required, type=N or NxM float
;                       The azimuth angle on a unit sphere. (-180, 180]
;       THETA:          in, required, type=N or NxM float
;                       The elevation angle on a unit sphere. [-90, 90]
;
; :Keywords:
;       DEGREES:        in, optional, type=boolean, default=0
;                       Set it `PHI` and `THETA` are returned in degrees, otherwise radians.
;       ORIENTATION:    in, optional, type=boolean, default=1
;                       Orientation of `THETA` and `PHI`. Options are::
;                         1: PHI   - Positive from x-axis
;                            THETA - Polar angle from z-axis
;                         2: PHI   - Positive from y-axis
;                            THETA - Polar angle from z-axis
;                         3: PHI   - Positive from x-axis
;                            THETA - Elevation angle from xy-plane
;                         4: PHI   - Positive from y-axis
;                            THETA - Elevation angle from xy-plane
;                         5: PHI   - Positive from z-axis
;                            THETA - Polar angle from y-axis
;                         6: PHI   - Positive from x-axis
;                            THETA - Polar angle from y-axis
;                         7: PHI   - Positive from z-axis
;                            THETA - Elevation angle from zx-plane
;                         8: PHI   - Positive from x-axis
;                            THETA - Elevation angle from zx-plane
;                         9: PHI   - Positive from y-axis
;                            THETA - Polar angle from x-axis
;                        10: PHI   - Positive from z-axis
;                            THETA - Polar angle from x-axis
;                        11: PHI   - Positive from y-axis
;                            THETA - Elevation angle from yz-plane
;                        12: PHI   - Positive from z-axis
;                            THETA - Elevation angle from yz-plane
;
; :Returns:
;       FILENAME:       The MMS file name.
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
pro MrDist_Cart2Sphere, x, y, z, phi, theta, $
DEGREES=degrees, $
ORIENTATION=orientation
	compile_opt idl2
	on_error, 2
	
	;Defaults
	tf_degrees = keyword_set(degrees)
	if n_elements(orientation) eq 0 then orientation = 3

;-----------------------------------------------------
; Compute Spherical Coordinates \\\\\\\\\\\\\\\\\\\\\\
;-----------------------------------------------------
	case orientation of
		;PHI   - Positive from x-axis             (-180, 180]
		;THETA - Polar angle from z-axis          (-180, 180]
		1: begin
			phi   = atan(y, x)
			theta = acos(z / sqrt(x^2 + y^2 + z^2))        ;OR atan( sqrt(x^2 + y^2), z )
		endcase
		
		;PHI   - Positive from y-axis             (-180, 180]
		;THETA - Polar angle from z-axis          (-180, 180]
		2: begin
			phi   = atan(-x, y)
			theta = acos(z / sqrt(x^2 + y^2 + z^2))        ;OR atan( sqrt(x^2 + y^2), z )
		endcase
		
		;PHI   - Positive from x-axis             (-180, 180]
		;THETA - Elevation angle from xy-plane    [ -90,  90]
		3: begin
			phi   = atan(y, x)
			theta = asin(z / sqrt(x^2 + y^2 + z^2))        ;OR atan( z / sqrt(x^2 + y^2) )
		endcase
		
		;PHI   - Positive from y-axis             (-180, 180]
		;THETA - Elevation angle from xy-plane    [ -90,  90]
		4: begin
			phi   = atan(-x, y)
			theta = asin(z / sqrt(x^2 + y^2 + z^2))        ;OR atan( z / sqrt(x^2 + y^2) )
		endcase
		
		;PHI   - Positive from z-axis             (-180, 180]
		;THETA - Polar angle from y-axis          (-180, 180]
		5: begin
			phi   = atan(x, z)
			theta = acos(y / sqrt(x^2 + y^2 + z^2))        ;OR atan( sqrt(x^2 + z^2), y )
		endcase
		
		;PHI   - Positive from x-axis             (-180, 180]
		;THETA - Polar angle from y-axis          (-180, 180]
		6: begin
			phi   = atan(-z, x)
			theta = acos(y / sqrt(x^2 + y^2 + z^2))        ;OR atan( sqrt(x^2 + z^2), y )
		endcase
		
		;PHI   - Positive from z-axis             (-180, 180]
		;THETA - Elevation angle from zx-plane    [ -90,  90]
		7: begin
			phi   = atan(x, z)
			theta = asin(y / sqrt(x^2 + y^2 + z^2))        ;OR atan( y / sqrt(x^2 + z^2) )
		endcase
		
		;PHI   - Positive from x-axis             (-180, 180]
		;THETA - Elevation angle from zx-plane    [ -90,  90]
		8: begin
			phi   = atan(-z, x)
			theta = asin(y / sqrt(x^2 + y^2 + z^2))        ;OR atan( y / sqrt(x^2 + z^2) )
		endcase
		
		;PHI   - Positive from y-axis             (-180, 180]
		;THETA - Polar angle from x-axis          (-180, 180]
		9: begin
			phi   = atan(z, y)
			theta = acos(x / sqrt(x^2 + y^2 + z^2))        ;OR atan( sqrt(y^2 + z^2), x )
		endcase
		
		;PHI   - Positive from z-axis             (-180, 180]
		;THETA - Polar angle from x-axis          (-180, 180]
		10: begin
			phi   = atan(-y, z)
			theta = acos(x / sqrt(x^2 + y^2 + z^2))        ;OR atan( sqrt(y^2 + z^2), x )
		endcase
		
		;PHI   - Positive from y-axis             (-180, 180]
		;THETA - Elevation angle from yz-plane    [ -90,  90]
		11: begin
			phi   = atan(z, y)
			theta = asin(x / sqrt(x^2 + y^2 + z^2))        ;OR atan( x / sqrt(y^2 + z^2) )
		endcase
		
		;PHI   - Positive from z-axis             (-180, 180]
		;THETA - Elevation angle from yz-plane    [ -90,  90]
		12: begin
			phi   = atan(-y, z)
			theta = asin(x / sqrt(x^2 + y^2 + z^2))        ;OR atan( x / sqrt(y^2 + z^2) )
		endcase
		
		else: message, 'Invalid value for ORIENTATION.'
	endcase

;-----------------------------------------------------
; Make Adjustments \\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\
;-----------------------------------------------------

	;Phi: Move range from (-!pi,!pi] to [0, 2*!pi)
;	iPhi       = where(phi lt 0, nPhi)
;	phi[iPhi] += 2.0*!pi
	
	;Convert to degrees
	if tf_degrees then begin
		rad2deg  = size(x, /TNAME) eq 'DOUBLE' ? 180.0D / !dpi : 180.0 /!pi
		phi     *= rad2deg
		theta   *= rad2deg
	endif
end