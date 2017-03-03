; docformat = 'rst'
;
; NAME:
;       MrVar_Grid_MakeCart
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
;       PHI:            in, required, type=depends
;                       Name, number, or objref of the variable containing the azimuth
;                           coordinates of the spherical grid. If time-dependent, the
;                           time-varying dimension must come first.
;       THETA:          in, required, type=depends
;                       Name, number, or objref of the variable containing the polar
;                           coordinates of the spherical grid. If time-dependent, the
;                           time-varying dimension must come first.
;       OXGRID:         out, optional, type=MrVariable
;                       The X cartesian coordinate of each point in the original grid.
;                           Will have the dimensions of [time(?), phi, theta].
;       OYGRID:         out, optional, type=MrVariable
;                       The Y cartesian coordinate of each point in the original grid.
;                           Will have the dimensions of [time(?), phi, theta].
;       OZGRID:         out, optional, type=objref (MrVariable)
;                       The Z cartesian coordinate of each point in the original grid.
;                           Will have the dimensions of [time(?), phi, theta].
;
; :Keywords:
;       DEGREES:        in, optional, type=boolean, default=0
;                       Set it `PHI` and `THETA` are in degrees. Radians are assumed.
;       PHI_GRID:       out, optional, type=MrVariable
;                       The azimuthal coordinate grid. Has dimensions [phi, theta]
;                           (and possibly time) and units indicated by the `DEGREES`
;                           keyword.
;       THETA_GRID:     out, optional, type=NxM or NxMxL float, default=0
;                       The polar grid coordinates. Has dimensions [phi, theta]
;                           (and possibly time) and units indicated by the `DEGREES`
;                           keyword.
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
;       2016/08/21  -   Written by Matthew Argall
;       2016/09/29  -   PHI and THETA can be time-independent. - MRA
;       2016/12/13  -   Renamed from MrVar_Grid_Sphere2Cart to MrVar_Grid_MakeCart. - MRA
;-
pro MrVar_Grid_MakeCart, phi, theta, oXGrid, oYGrid, oZGrid, $
DEGREES=degrees, $
THETA_GRID=oThetaGrid, $
PHI_GRID=oPhiGrid
	compile_opt idl2
	on_error, 2

;-----------------------------------------------------
; Get Variables \\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\
;-----------------------------------------------------
	;Get the object references / verify object class.
	oPhi   = MrVar_Get(phi)
	oTheta = MrVar_Get(theta)
	
	;Check units
	if n_elements(degrees) eq 0 then begin
		case 1 of
			oPhi   -> HasAttr('UNITS'): tf_degrees = stregex(oPhi['UNITS'],   'deg', /BOOLEAN, /FOLD_CASE)
			oTheta -> HasAttr('UNITS'): tf_degrees = stregex(oTheta['UNITS'], 'deg', /BOOLEAN, /FOLD_CASE)
			else:                       tf_degrees = 0B
		endcase
	endif else begin
		tf_degrees = keyword_set(degrees)
	endelse
	
	;Time series
	if obj_isa(oPhi,   'MrTimeSeries') then oTime = oPhi['TIMEVAR']
	if obj_isa(oTheta, 'MrTimeSeries') then oTime = oTheta['TIMEVAR']
	tf_timeseries = obj_valid(oTime)
	
;-----------------------------------------------------
; Size of Grid \\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\
;-----------------------------------------------------
	
	;Number phi bins
	szPhi = size(oPhi)
	if szPhi[0] eq 2 then begin
		nPhi  = szPhi[2]
		nTime = szPhi[1]
	endif else nPhi = szPhi[szPhi[0]+2]
	
	;Number of theta bins
	szTheta = size(oTheta)
	if szTheta[0] eq 2 then begin
		nTheta = szTheta[2]
		nTime  = szTheta[1]
	endif else nTheta = szTheta[szTheta[0]+2]
	
	;Number of data points
	if n_elements(nTime) eq 0 then nTime = 1

;-----------------------------------------------------
; Compute Grid \\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\
;-----------------------------------------------------
	
	;Is phi time dependent?
	;   1) [time, phi] --> [time, phi, theta]
	;   2) [phi]       --> [time, phi, theta]
	if szPhi[0] eq 2 $
		then phi_grid = rebin( oPhi['DATA'], nTime, nPhi, nTheta ) $
		else phi_grid = rebin( reform( oPhi['DATA'], 1, nPhi), nTime, nPhi, nTheta )
		
	;Is theta time dependent?
	;   1) [time, theta] --> [time, phi, theta]
	;   2) [theta]       --> [time, phi, theta]
	if szTheta[0] eq 2 $
		then theta_grid = rebin( reform( oTheta['DATA'], nTime, 1, nTheta), nTime, nPhi, nTheta ) $
		else theta_grid = rebin( reform( oTheta['DATA'],     1, 1, nTheta), nTime, nPhi, nTheta )

	;Not time-dependent
	if nTime eq 1 then begin
		phi_grid   = reform(phi_grid)
		theta_grid = reform(theta_grid)
	endif
	
;-----------------------------------------------------
; Convert to Cartesian Coordinates \\\\\\\\\\\\\\\\\\\
;-----------------------------------------------------

	;Convert degrees to radians?
	deg2rad = !dpi / 180.0
	rad2deg = 180.0 / !dpi
	if tf_degrees then begin
		phi_grid   *= deg2rad
		theta_grid *= deg2rad
	endif

	;Compute cartesian coordinates
	if tf_timeseries then begin
		oXGrid = MrTimeSeries( oTime, sin(theta_grid) * cos(phi_grid) )
		oYGrid = MrTimeSeries( oTime, sin(theta_grid) * sin(phi_grid) )
		oZGrid = MrTimeSeries( oTime, cos(theta_grid) )
		
	endif else begin
		oXGrid = MrVariable( sin(theta_grid) * cos(phi_grid) )
		oYGrid = MrVariable( sin(theta_grid) * sin(phi_grid) )
		oZGrid = MrVariable( cos(theta_grid) )
	endelse
	
;-----------------------------------------------------
; Spherical Grid \\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\
;-----------------------------------------------------
	
	;PHI
	if arg_present(oPhiGrid) then begin
		;Convert to degrees
		if tf_degrees then phi_grid = phi_grid * rad2deg
		
		;Variable type
		if tf_time_series $
			then oPhiGrid = MrTimeSeries( oTime, float( temporary(phi_grid) ) ) $
			else oPhiGrid = MrVariable( float( temporary(phi_grid) ) )
		
		;Set units
		oPhiGrid['UNITS'] = 'degrees'
	endif
	
	;THETA
	if arg_present(oThetaGrid) then begin
		;Convert to degrees
		if tf_degrees then phi_grid = phi_grid * rad2deg
		
		;Variable type
		if tf_time_series $
			then oThetaGrid = MrTimeSeries( oTime, float( temporary(theta_grid) ) ) $
			else oThetaGrid = MrVariable( float( temporary(theta_grid) ) )
		
		;Set units
		oThetaGrid['UNITS'] = 'degrees'
	endif
end