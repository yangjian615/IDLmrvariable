; docformat = 'rst'
;
; NAME:
;    MrVar_Grid_sphere2fac
;
; PURPOSE:
;+
;   Rotate unit spherical coordinates to a field-aligned coordinate system (FAC).
;
;   Calling Sequences::
;       MrVar_Grid_sphere2fac, B, phi, theta, oPhi, oTheta, /SPHERE
;       MrVar_Grid_sphere2fac, B, phi, theta, x, y, z
;
; :Categories:
;    MrVariable, Grid
;
; :Params:
;       B:              in, required, type=3xN or Nx3 float
;                       Time series of vector magnetic field.
;       PHI:            in, required, type=N or NxM float
;                       The azimuth coordinates (may be time-dependent) of the spherical grid.
;       THETA:          in, required, type=N or NxM float
;                       The polar coordinates (may be time-dependent) of the spherical grid.
;       OX1:            out, optional, type=NxM float
;                       The cartesian x-coordinates in FAC.
;       OX2:            out, optional, type=NxM float
;                       The cartesian y-coordinates in FAC.
;       OX3:            out, optional, type=NxM float
;                       The cartesian z-coordinates in FAC.
;
; :Keywords:
;       ORIENTATION:    in, optional, type=integer, default=3
;                       Orientation of the sphereical grid when `SPHERE` is set. See
;                           MrVar_Grid_Cart2Sphere.pro for options.
;       SPHERE:         in, optional, type=boolean, default=0
;                       If set, the output grid will be defined in spherical coordinate
;                           system. `OX1` and `OX2` will be the azimuthal and polar
;                           coordinates, respectively.
;       RADIANS:        in, optional, type=boolean, default=0
;                       Set if `PHI` and `THETA` are in radians. Degrees are assumed.
;       TYPE:           in, optional, type=string, default=ExB
;                       Type of FAC to . Options are::
;                           ''       - `VEC` = <undefined>      Z = B  X = YxB      Y = [1,0,0] x B
;                           'ExB'    - `VEC` = electric field   Z = B  X = ExB      Y = Bx(ExB)
;                           'VxB'    - `VEC` = velocity         Z = B  X = (BxV)xB  Y = BxV
;                           'RadAz'  - `VEC` = radial position  Z = B  X = BxR      Y = Bx(BxR)
;       VEC:            in, optional, type=3xN or Nx3 float
;                       Vector field that defines the plane perpendicular to `B`.
;
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
;       2016/08/21  -   Written by Matthew Argall
;       2016/09/05  -   Added the ORIENTATION keyword. - MRA
;-
;*****************************************************************************************
pro MrVar_Grid_sphere2fac, b, phi, theta, oX1, oX2, oX3, $
ORIENTATION=orientation, $
RADIANS=radians, $
SPHERE=sphere, $
TYPE=type, $
VEC=vec
	compile_opt idl2
	on_error, 2
	
	tf_sphere  =  keyword_set(sphere)
	tf_degrees = ~keyword_set(radians)
	if n_elements(order) eq 0 then order = 2
	if n_elements(vec)   eq 0 then type  = ''
	
	;Time dimension
	case order of
		0: dimension = 0
		1: dimension = 3
		2: dimension = 1
		else: message, 'Invalid value for ORDER (' + string(order, FORMAT='(i0)') + ').'
	endcase

;-----------------------------------------------------
; Field-Aligned Coordinates \\\\\\\\\\\\\\\\\\\\\\\\\\
;-----------------------------------------------------

	;Create a cartesian grid
	MrVar_Grid_Sphere2Cart, phi, theta, oX, oY, oZ, DEGREES=tf_degrees

	;Field-Aligned Coordinate System
	oT_fac = MrVar_FAC(b, vec, TYPE=type)

	;Rotate the cartesian grid to field-aligned coordinates
	MrVar_Grid_Cart2FAC, oT_fac, -temporary(oX), -temporary(oY), -temporary(oZ), oX1, oX2, oX3
	
	;Convert to spherical coordinates
	if tf_sphere then begin
		MrVar_Grid_cart2sphere, temporary(oX1), temporary(oX2), temporary(oX3), oX1, oX2, $
		                        DEGREES     = tf_degrees, $
		                        ORIENTATION = orientation
	endif
end