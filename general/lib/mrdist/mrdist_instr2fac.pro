; docformat = 'rst'
;
; NAME:
;    unh_fpi_pad_crib
;
; PURPOSE:
;+
;   Rotate unit spherical coordinates to a field-aligned coordinate system (FAC).
;
;   Calling Sequences::
;       MrDist_Instr2FAC, B, phi, theta, phi_new, theta_new
;       MrDist_Instr2FAC, B, phi, theta, x, y, z, /CART
;
; :Categories:
;    MMS, SPEDAS
;
; :Examples:
;   To use::
;       IDL> .r unh_fpi_pad_crib
;
; :Params:
;       B:              in, required, type=3xN or Nx3 float
;                       Time series of vector magnetic field.
;       PHI:            in, required, type=N or NxM float
;                       The azimuth coordinates (may be time-dependent) of the spherical grid.
;       THETA:          in, required, type=N or NxM float
;                       The polar coordinates (may be time-dependent) of the spherical grid.
;       X:              out, optional, type=NxM float
;                       The cartesian x-coordinates in FAC.
;       Y:              out, optional, type=NxM float
;                       The cartesian y-coordinates in FAC.
;       Z:              out, optional, type=NxM float
;                       The cartesian z-coordinates in FAC.
;
; :Keywords:
;       ORDER:          in, optional, type=integer, default=2
;                       Indicate how the dimensions are ordered. By default, 0 is
;                           chosen for 2D arrays, and 1 is chosen for 3D arrays.
;                           Options are:
;                               0: [phi, theta]
;                               1: [phi, theta, time]
;                               2: [time, phi, theta]
;       RADIANS:        in, optional, type=boolean, default=0
;                       Set if `PHI` and `THETA` are in radians. Degrees are assumed.
;       VEC:            in, optional, type=3xN or Nx3 float
;                       Vector field that defines the plane perpendicular to `B`.
;       TYPE:           in, optional, type=string, default=ExB
;                       Type of FAC to . Options are::
;                           ''       - `VEC` = <undefined>      Z = B  X = YxB      Y = [1,0,0] x B
;                           'ExB'    - `VEC` = electric field   Z = B  X = ExB      Y = Bx(ExB)
;                           'VxB'    - `VEC` = velocity         Z = B  X = (BxV)xB  Y = BxV
;                           'RadAz'  - `VEC` = radial position  Z = B  X = BxR      Y = Bx(BxR)
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
;       2016/02/10  -   Written by Matthew Argall
;-
;*****************************************************************************************
pro mrdist_instr2fac, b, phi, theta, x, y, z, $
ORDER=order, $
RADIANS=radians, $
CART=cart, $
VEC=vec, $
TYPE=type
	compile_opt idl2
	on_error, 2
	
	tf_sphere  = ~keyword_set(cart)
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
	MrDist_Sphere2Cart, phi, theta, x_grid, y_grid, z_grid, $
	                    DEGREES    = tf_degrees, $
	                    DIMENSION  = dimension

	;Field-Aligned Coordinate System
	T_fac = MrDist_FAC(b, vec, $
	                   TYPE      = type, $
	                   DIMENSION = dimension)

	;Rotate the cartesian grid to field-aligned coordinates
	MrDist_Cart2FAC, T_fac, -temporary(x_grid), -temporary(y_grid), -temporary(z_grid), $
	                 x, y, z, ORDER=order
end