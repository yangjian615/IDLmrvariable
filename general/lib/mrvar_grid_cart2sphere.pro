; docformat = 'rst'
;
; NAME:
;       MrVar_Grid_Cart2Sphere
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
;                       Set to return `PHI` and `THETA` in degrees, otherwise radians.
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
PRO MrVar_Grid_Cart2Sphere, x, y, z, oPhi, oTheta, $
RADIANS=radians, $
DEGREES=degrees, $
ORIENTATION=orientation
	Compile_Opt idl2
	On_Error, 2
	
	;Defaults
	tf_degrees = Keyword_Set(degrees)
	tf_radians = Keyword_Set(radians)
	IF N_Elements(orientation) EQ 0 THEN orientation = 1
	
	;Get/verify variables
	oX = MrVar_Get(x)
	oY = MrVar_Get(y)
	oZ = MrVar_Get(z)

;-----------------------------------------------------
; Compute Spherical Coordinates \\\\\\\\\\\\\\\\\\\\\\
;-----------------------------------------------------
	CASE orientation OF
		;PHI   - Positive from x-axis             (-180, 180]
		;THETA - Polar angle from z-axis          [   0, 180]
		1: BEGIN
			phi   = ATan(oY['DATA'], oX['DATA'])
			theta = ACos(oZ['DATA'] / Sqrt(oX['DATA']^2 + oY['DATA']^2 + oZ['DATA']^2))     ;OR ATan( Sqrt(x^2 + y^2), z )
		ENDCASE
		
		;PHI   - Positive from y-axis             (-180, 180]
		;THETA - Polar angle from z-axis          [   0, 180]
		2: BEGIN
			phi   = ATan(-oX['DATA'], oY['DATA'])
			theta = ACos(oZ['DATA'] / Sqrt(oX['DATA']^2 + oY['DATA']^2 + oZ['DATA']^2))     ;OR ATan( Sqrt(x^2 + y^2), z )
		ENDCASE
		
		;PHI   - Positive from x-axis             (-180, 180]
		;THETA - Elevation angle from xy-plane    [ -90,  90]
		3: BEGIN
			phi   = ATan(oY['DATA'], oX['DATA'])
			theta = ASin(oZ['DATA'] / Sqrt(oX['DATA']^2 + oY['DATA']^2 + oZ['DATA']^2))     ;OR ATan( z / Sqrt(x^2 + y^2) )
		ENDCASE
		
		;PHI   - Positive from y-axis             (-180, 180]
		;THETA - Elevation angle from xy-plane    [ -90,  90]
		4: BEGIN
			phi   = ATan(-oX['DATA'], oY['DATA'])
			theta = ASin(oZ['DATA'] / Sqrt(oX['DATA']^2 + oY['DATA']^2 + oZ['DATA']^2))     ;OR ATan( z / Sqrt(x^2 + y^2) )
		ENDCASE
		
		;PHI   - Positive from z-axis             (-180, 180]
		;THETA - Polar angle from y-axis          [   0, 180]
		5: BEGIN
			phi   = ATan(oX['DATA'], oZ['DATA'])
			theta = ACos(oY['DATA'] / Sqrt(oX['DATA']^2 + oY['DATA']^2 + oZ['DATA']^2))     ;OR ATan( Sqrt(x^2 + z^2), y )
		ENDCASE
		
		;PHI   - Positive from x-axis             (-180, 180]
		;THETA - Polar angle from y-axis          [   0, 180]
		6: BEGIN
			phi   = ATan(-oZ['DATA'], oX['DATA'])
			theta = ACos(oY['DATA'] / Sqrt(oX['DATA']^2 + oY['DATA']^2 + oZ['DATA']^2))     ;OR ATan( Sqrt(x^2 + z^2), y )
		ENDCASE
		
		;PHI   - Positive from z-axis             (-180, 180]
		;THETA - Elevation angle from zx-plane    [ -90,  90]
		7: BEGIN
			phi   = ATan(oX['DATA'], oZ['DATA'])
			theta = ASin(oY['DATA'] / Sqrt(oX['DATA']^2 + oY['DATA']^2 + oZ['DATA']^2))     ;OR ATan( y / Sqrt(x^2 + z^2) )
		ENDCASE
		
		;PHI   - Positive from x-axis             (-180, 180]
		;THETA - Elevation angle from zx-plane    [ -90,  90]
		8: BEGIN
			phi   = ATan(-oZ['DATA'], oX['DATA'])
			theta = ASin(oY['DATA'] / Sqrt(oX['DATA']^2 + oY['DATA']^2 + oZ['DATA']^2))     ;OR ATan( y / Sqrt(x^2 + z^2) )
		ENDCASE
		
		;PHI   - Positive from y-axis             (-180, 180]
		;THETA - Polar angle from x-axis          [   0, 180]
		9: BEGIN
			phi   = ATan(oZ['DATA'], oY['DATA'])
			theta = ACos(oX['DATA'] / Sqrt(oX['DATA']^2 + oY['DATA']^2 + oZ['DATA']^2))     ;OR ATan( Sqrt(y^2 + z^2), x )
		ENDCASE
		
		;PHI   - Positive from z-axis             (-180, 180]
		;THETA - Polar angle from x-axis          [   0, 180]
		10: BEGIN
			phi   = ATan(-oY['DATA'], oZ['DATA'])
			theta = ACos(oX['DATA'] / Sqrt(oX['DATA']^2 + oY['DATA']^2 + oZ['DATA']^2))     ;OR ATan( Sqrt(y^2 + z^2), x )
		ENDCASE
		
		;PHI   - Positive from y-axis             (-180, 180]
		;THETA - Elevation angle from yz-plane    [ -90,  90]
		11: BEGIN
			phi   = ATan(oZ['DATA'], oY['DATA'])
			theta = ASin(oX['DATA'] / Sqrt(oX['DATA']^2 + oY['DATA']^2 + oZ['DATA']^2))     ;OR ATan( x / Sqrt(y^2 + z^2) )
		ENDCASE
		
		;PHI   - Positive from z-axis             (-180, 180]
		;THETA - Elevation angle from yz-plane    [ -90,  90]
		12: BEGIN
			phi   = ATan(-oY['DATA'], oZ['DATA'])
			theta = ASin(oX['DATA'] / Sqrt(oX['DATA']^2 + oY['DATA']^2 + oZ['DATA']^2))     ;OR ATan( x / Sqrt(y^2 + z^2) )
		ENDCASE
		
		ELSE: Message, 'Invalid value for ORIENTATION.'
	ENDCASE

;-----------------------------------------------------
; Make Adjustments \\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\
;-----------------------------------------------------

	;Phi: Move range from (-!pi,!pi] to [0, 2*!pi)
;	iPhi       = where(phi LT 0, nPhi)
;	phi[iPhi] += 2.0*!pi
	
	;Convert to degrees
	IF tf_degrees THEN BEGIN
		rad2deg  = Size(oX, /TNAME) EQ 'DOUBLE' ? 180.0D / !dpi : 180.0 /!pi
		phi     *= rad2deg
		theta   *= rad2deg
	ENDIF

	;Create variables
	IF Size(phi, /N_DIMENSIONS) EQ 3 THEN BEGIN
		oPhi   = MrTimeSeries( oX['TIMEVAR'], phi, /NO_COPY)
		oTheta = MrTimeSeries( oX['TIMEVAR'], theta, /NO_COPY)
	ENDIF ELSE BEGIN
		oPhi   = MrVariable(phi, /NO_COPY)
		oTheta = MrVariable(theta, /NO_COPY)
	ENDELSE
	
	;Add attributes
	oPhi['UNITS']   = tf_degrees ? 'Degrees' : 'Radians'
	oTheta['UNITS'] = tf_degrees ? 'Degrees' : 'Radians'
end