; docformat = 'rst'
;
; NAME:
;       MrDist_Sphere2Cart
;
; PURPOSE:
;+
;   Transform a spherical coordinate grid into a cartesian coordinate grid.
;
;   These are intended to come directly from the CDF file, as the particle instrument
;   team has defined their look directions in (phi,theta) space. As such, they are
;   often 1D arrays; i.e., one set of phi coordinates that describes the azimuth location
;   for all sensors, and one set of theta coordinates that describes the polar location of
;   all sensors.
;
;   In the case of MMS, phi is time-dependent due to how the data is despun.
;
; :Categories:
;   Distribution functions
;
; :Params:
;       PHI:            in, required, type=N or NxM float
;                       The azimuth coordinates (may be time-dependent) of the spherical grid.
;       THETA:          in, required, type=N or NxM float
;                       The polar coordinates (may be time-dependent) of the spherical grid.
;       X_GRID:         out, optional, type=NxM or NxMxL
;                       The X cartesian coordinate of each point in the original grid.
;                           If time is the first (last) dimension of `THETA` or `PHI`,
;                           then it will also be the first (last) dimension of X_GRID.
;                           Will have the dimensions of [phi, theta] (and possibly time).
;       Y_GRID:         in, required, type=N or NxM float
;                       The Y cartesian coordinate of each point in the original grid.
;                           If time is the first (last) dimension of `THETA` or `PHI`,
;                           then it will also be the first (last) dimension of X_GRID.
;                           Will have the dimensions of [phi, theta] (and possibly time).
;       Z_GRID:         The Z cartesian coordinate of each point in the original grid.
;                           If time is the first (last) dimension of `THETA` or `PHI`,
;                           then it will also be the first (last) dimension of X_GRID.
;                           Will have the dimensions of [phi, theta] (and possibly time).
;
; :Keywords:
;       DIMENSION:      in, optional, type=integer
;                       The time-dependent dimension. If zero or undefined, the grid
;                           is assumed to be time indepenent.
;       DEGREES:        in, optional, type=boolean, default=0
;                       Set it `PHI` and `THETA` are in degrees. Radians are assumed.
;       PHI_GRID:       out, optional, type=NxM or NxMxL float, default=0
;                       The azimuthal coordinate grid. Has dimensions [phi, theta]
;                           (and possibly time).
;       THETA_GRID:     out, optional, type=NxM or NxMxL float, default=0
;                       The polar or zenith coordinate grid. Has dimensions [phi, theta]
;                           (and possibly time).
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
pro MrDist_Sphere2Cart, phi, theta, x_grid, y_grid, z_grid, $
DEGREES=degrees, $
DIMENSION=dimension, $
THETA_GRID=theta_grid, $
PHI_GRID=phi_grid
	compile_opt idl2
	on_error, 2
	
	;Number of sectors in each dimension
	tf_degrees = keyword_set(degrees)
	t_dim      = n_elements(dimension) eq 0 ? 0 : dimension
	
	;Get dimension sizes
	szPhi   = size(phi)
	szTheta = size(theta)
	if szPhi[0] eq 1 && szTheta[0] eq 1 && t_dim ne 0 then t_dim = 0
	if szPhi[0] gt 1 && szTheta[0] gt 1 && t_dim eq 0 then begin
		if szPhi[0] ne 1 || szTheta[0] ne 1 $
			then message, 'DIMENSION must be >= 1 if PHI or THETA is time dependent.'
	endif
	
	;Number phi bins
	if szPhi[0] eq 2 then begin
		nPhi  = dimension eq 1 ? szPhi[2] : szPhi[1]
		nTime = szPhi[dimension]
	endif else nPhi = szPhi[szPhi[0]+2]
	
	;Number of theta bins
	if szTheta[0] eq 2 then begin
		nTheta = dimension eq 1 ? szTheta[2] : szTheta[1]
		nTime  = szTheta[dimension]
	endif else nTheta = szTheta[szTheta[0]+2]

;-----------------------------------------------------
; Compute Grid \\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\
;-----------------------------------------------------
	
	;Rebin into time-dependent coordinate grid
	case t_dim of
		;GRID: [phi, theta]
		0: begin
			;Rebin into coordinate grid
			phi_grid   = rebin(phi, nPhi, nTheta)
			theta_grid = rebin( reform(theta, 1, nTheta), nPhi, nTheta )
		endcase
	
		;GRID: [time, phi, theta]
		1: begin
			;Is phi time dependent?
			;   1) [time, phi] --> [time, phi, theta]
			;   2) [phi]       --> [time, phi, theta]
			if szPhi[0] eq 2 $
				then phi_grid = rebin(phi, nTime, nPhi, nTheta) $
				else phi_grid = rebin( reform(phi, 1, nPhi), nTime, nPhi, nTheta )
				
			;Is theta time dependent?
			;   1) [time, theta] --> [time, phi, theta]
			;   2) [theta]       --> [time, phi, theta]
			if szTheta[0] eq 2 $
				then theta_grid = rebin( reform(theta, nTime, 1, nTheta), nTime, nPhi, nTheta) $
				else theta_grid = rebin( reform(theta,     1, 1, nTheta), nTime, nPhi, nTheta )
		endcase
		
		;GRID: [phi, theta, time]
		2: begin
			;Is phi time dependent?
			;   1) [phi, time] --> [phi, theta, time]
			;   2) [phi]       --> [phi, theta, time]
			if szPhi[0] eq 2 $
				then phi_grid = rebin( reform(phi, nPhi, 1, nTime), nPhi, nTheta, nTime) $
				else phi_grid = rebin(phi, nPhi, nTheta, nTime)
				
			;Is theta time dependent?
			;   1) [theta, time] --> [phi, theta, time]
			;   2) [theta]       --> [phi, theta, time]
			if szTheta[0] eq 2 $
				then theta_grid = rebin( reform(theta, 1, nTheta, nTime), nPhi, nTheta, nTime) $
				else theta_grid = rebin( reform(theta, 1, nTheta), nPhi, nTheta, nTime )
		endcase
		
		;Invalid dimension
		else: message, 'DIMENSION must be 0, 1, or 2.'
	endcase

;-----------------------------------------------------
; Convert to Cartesian Coordinates \\\\\\\\\\\\\\\\\\\
;-----------------------------------------------------

	;Convert degrees to radians?
	if tf_degrees then begin
		deg2rad     = !dpi / 180.0
		phi_grid   *= deg2rad
		theta_grid *= deg2rad
	endif

	;Compute cartesian coordinates
	x_grid = sin(theta_grid) * cos(phi_grid)
	y_grid = sin(theta_grid) * sin(phi_grid)
	z_grid = cos(theta_grid)
end