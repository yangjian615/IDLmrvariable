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
;       PAR:            in, required, type=3xN or Nx3 float
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
;       FAC:            in, optional, type=string, default='CrossX'
;                       Type of FAC to . Options are::
;                           ''        - `VEC` = <user defined>   Z = B  X = (BxVEC)xB  Y = BxVEC
;                           'ExB'     - `VEC` = electric field   Z = B  X = ExB        Y = Bx(ExB)
;                           'VxB'     - `VEC` = velocity         Z = B  X = (BxV)xB    Y = BxV
;                           'RadAz'   - `VEC` = radial position  Z = B  X = BxR        Y = Bx(BxR)
;                           'CrossX'  - `VEC` = [1, 0, 0]        Z = B  X = (BxVEC)xB  Y = BxVEC
;       ORIENTATION:    in, optional, type=integer, default=3
;                       Orientation of the sphereical grid when `SPHERE` is set. See
;                           MrVar_Grid_Cart2Sphere.pro for options.
;       PERP:           in, optional, type=3xN or Nx3 float
;                       Vector field that defines the plane perpendicular to `PAR`.
;       SPHERE:         in, optional, type=boolean, default=0
;                       If set, the output grid will be defined in spherical coordinate
;                           system. `OX1` and `OX2` will be the azimuthal and polar
;                           coordinates, respectively.
;       RADIANS:        in, optional, type=boolean, default=0
;                       Set if `PHI` and `THETA` are in radians. Degrees are assumed.
;       TIME:           in, optional, type=MrTimeVar
;                       A variable that defines the time. If given, the output will be
;                           interpolated to these time tags. If not, everything is
;                           interpolated to `PAR`.
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
;       2017/01/30  -   Renamed parameter B to PAR and keyword VEC to PERP. - MRA
;       2017/02/28  -   Added the TIME keyword. - MRA
;-
;*****************************************************************************************
pro MrVar_Grid_sphere2fac, par, phi, theta, oX1, oX2, oX3, $
FAC=fac, $
ORIENTATION=orientation, $
PERP=perp, $
RADIANS=radians, $
SPHERE=sphere, $
TIME=time
	compile_opt idl2
	on_error, 2
	
	tf_sphere  =  keyword_set(sphere)
	tf_degrees = ~keyword_set(radians)
	if n_elements(perp) eq 0 then fac = 'CROSSX'

;-----------------------------------------------------
; Field-Aligned Coordinates \\\\\\\\\\\\\\\\\\\\\\\\\\
;-----------------------------------------------------

	;Create a cartesian grid
	MrVar_Grid_MakeCart, phi, theta, oX, oY, oZ, DEGREES=tf_degrees

	;Field-Aligned Coordinate System
	oT_fac = MrVar_FAC(par, perp, fac, TIME=time)

	;Rotate the cartesian grid to field-aligned coordinates
	MrVar_Grid_Cart2FAC, oT_fac, -temporary(oX), -temporary(oY), -temporary(oZ), oX1, oX2, oX3
	
	;Convert to spherical coordinates
	if tf_sphere then begin
		MrVar_Grid_cart2sphere, temporary(oX1), temporary(oX2), temporary(oX3), oX1, oX2, $
		                        DEGREES     = tf_degrees, $
		                        ORIENTATION = orientation
	endif
end