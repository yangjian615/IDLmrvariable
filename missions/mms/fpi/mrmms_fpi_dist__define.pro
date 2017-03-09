; docformat = 'rst'
;
; NAME:
;   MrMMS_FPI_Dist4D__Define
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
; PURPOSE
;+
;   Calculate moments and reduced distributions from a 3D distribution function.
;
; :Categories:
;   MrVariable, MrTimeSeries, MrMMS_FPI_Dist4D
;
; :See Also:
;   MrVariable__Define.pro
;   MrTimeSeries__Define.pro
;
; :Author:
;   Matthew Argall::
;       University of New Hampshire
;       Morse Hall, Room 348
;       8 College Rd.
;       Durham, NH, 03824
;       matthew.argall@unh.edu
;
; :History:
;   Modification History::
;       2016/10/24  -   Written by Matthew Argall
;-
;*****************************************************************************************
;+
;   The initialization method.
;
; :Params:
;       TIME:           in, required, type=NxM array
;                       Name or reference of a MrTimeVar object, or an array
;                           of time stamps. If a name is provided, the assiciated
;                           variable must exist in the variable cache.
;       DATA:           in, required, type=NxM array
;                       Name or reference of a MrVariable object, or the dependent
;                           variable data. If a name is given, the associated variable
;                           must exist in the variable cache. 
;
; :Keywords:
;       CACHE:          in, optional, type=boolean, default=0
;                       If set, both `TIME` and `DATA` are added to the variable cache.
;       NAME:           in, optional, type=integer
;                       Name to be given to the variable object.
;       NO_CLOBBER:     in, optional, type=boolean, default=0
;                       If set, do not clobber variables of the same name. Instead,
;                           rename this variable by appending "_#" to `NAME`, where
;                           "#" represents a unique number. Ignored unless `CACHE` is set.
;       NO_COPY:        in, optional, type=boolean, default=0
;                       If set `DATA` will be copied directly into the object
;                           and will be left undefined (a MrTimeSeries object will not
;                           be destroyed, but its array will be empty).
;-
function MrMMS_FPI_Dist::INIT, sc, mode, species, $
_REF_EXTRA=extra
	compile_opt idl2

	;Error handling
	catch, the_error
	if the_error ne 0 then begin
		catch, /CANCEL
		MrPrintF, 'LogErr'
		return, 0
	endif

;-----------------------------------------------------
; Load Data \\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\
;-----------------------------------------------------
	;USAGE:
	;   - oFPI = MrMMS_FPI_Dist(variable)
	if n_params() eq 1 then begin
		if n_elements(extra) gt 0 then message, 'Keywords not allowed when variable is given.'
		
		;Set the data
		variable = sc
		self -> SetData, variable
	
	;USAGE:
	;   - oFPI = MrMMS_FPI_Dist(sc, instr, species)
	endif else if n_params() gt 1 then begin
		self -> Load, sc, mode, species, $
		              _STRICT_EXTRA = extra
	
	;USAGE:
	;   - oFPI = MrMMS_FPI_Dist()
	endif else begin
		if n_elements(extra) gt 0 then message, 'Keywords not allowed when no parameters are given.'
	endelse

	return, 1
end


;+
;   Clean up after the object is destroyed
;-
pro MrMMS_FPI_Dist::CLEANUP
	compile_opt idl2
	on_error, 2
	
	;
	; Do not destroy the distribution object, as it may
	; still be in the variable cache. Automatic garbage
	; collection will take care of it.
	;

	obj_destroy, self.oFAC
end


;+
;   Manipulate the data loaded from source via ::Load so that it is compatible
;   with the ::SetData method.
;-
function MrMMS_FPI_Dist::GetDist4D, $
CACHE=cache, $
NAME=name
	compile_opt idl2
	
	catch, the_error
	if the_error ne 0 then begin
		catch, /CANCEL
		MrPrintF, 'LogErr'
		return, !Null
	endif
	
	if n_elements(name) eq 0 then name = self.oDist.name + '_dist4d'
	
	;Get the data objects
	oPhiTemp   = MrVar_Get( self.oDist['DEPEND_1'] )
	oThetaTemp = MrVar_Get( self.oDist['DEPEND_2'] )
	oEnergy    = MrVar_Get( self.oDist['DEPEND_3'] )
	species    = strmid(self.optdesc, 1, 1) eq 'i' ? 'H' : 'e'

	;Start by gridding the data
	self -> Grid, oPhiTemp, oThetaTemp, oPhi, oTheta

	;Create a 4D Distribution
	oDist = MrDist4D( self.oDist['TIMEVAR'], self.oDist, oPhi, oTheta, oEnergy, $
	                  CACHE   = cache, $
	                  NAME    = name, $
	                  SPECIES = species, $
	                  UNITS   = 'PSD' )
	
	;Return the distribution
	return, oDist
end


;+
;   Transform a spherical coordinate grid into a cartesian coordinate grid.
;
; :Keywords:
;       SC:             out, optional, type=string
;                       MMS spacecraft identifier.
;       INSTR:          out, optional, type=string
;                       Instrument identifier.
;       LEVEL:          out, optional, type=string
;                       Data quality level.
;       MODE:           out, optional, type=string
;                       Telemetry mode.
;       OPTDESC:        out, optional, type=string
;                       EDI-specific optional descriptor.
;       COORD_SYS:      out, optional, type=string
;                       Coordinate system in which the data resides.
;-
pro MrMMS_FPI_Dist::GetProperty, $
SC=sc, $
INSTR=instr, $
MODE=mode, $
LEVEL=level, $
OPTDESC=optdesc, $
COORD_SYS=coord_sys, $
TYPE_FAC=type_fac
	compile_opt idl2
	on_error, 2
	
	if arg_present(sc)        then sc        = self.sc
	if arg_present(instr)     then instr     = self.instr
	if arg_present(mode)      then mode      = self.mode
	if arg_present(level)     then level     = self.level
	if arg_present(optdesc)   then optdesc   = self.optdesc
	if arg_present(coord_sys) then coord_sys = self.coord_sys
end


;+
;   Create a spherical coordinate grid.
;
; :Private:
;
; :Params:
;       ORIENTATION:        in, optional, type=integer, default=1
;                           Orientation of the spherical coordinate system. The default
;                               has polar angle measured down from +Z and azimuth
;                               counter clock-wise from +X.
;-
pro MrMMS_FPI_Dist::Grid, oTheta, oPhi, oX1, oX2
	compile_opt idl2
	on_error, 2
	
	;Default to standard spherical orientation
	if n_elements(orientation) eq 0 then orientation = 1
	
	;Create a cartesian grid
	;   - Will ensure THETA and PHI are NxM arrays
	self -> Grid_MakeCart, oTheta, oPhi, oX1, oX2, oX3

;-----------------------------------------------------
; Rotate to FAC \\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\
;-----------------------------------------------------
	if obj_valid(self.oFAC) && self.oFAC.type_fac ne 'NONE' then begin
		;Create a cartesian grid
		self -> Grid_MakeCart, oTheta, oPhi, oX, oY, oZ
	
		;Field-Aligned Coordinate System
		oT_FAC = self.oFAC -> GetTFAC()
	
		;Rotate the cartesian grid to field-aligned coordinates
		self -> Grid_Cart2FAC, temporary(oT_FAC), -temporary(oX), -temporary(oY), -temporary(oZ), $
		                       oX1, oX2, oX3
	
		;Convert to spherical coordinates
		self -> Grid_cart2sphere, temporary(oX1), temporary(oX2), temporary(oX3), oX1, oX2, $
		                          ORIENTATION = orientation

;-----------------------------------------------------
; Keep Original Coordinates \\\\\\\\\\\\\\\\\\\\\\\\\\
;-----------------------------------------------------
	endif else begin
		;Convert to spherical coordinates
		self -> Grid_cart2sphere, temporary(oX1), temporary(oX2), temporary(oX3), oX1, oX2, $
		                          ORIENTATION = orientation
	endelse
end


;+
;   Transform a cartesian coordinate grid into field-aligned coordinates.
;
; :Params:
;       OT:             in, required, type=MrTensorTS object
;                       Transformation matrix from cartesian to field-aligned coordinates.
;       OX:             in, required, type=MrVariable object
;                       The x-coordinates to be rotated.
;       OY:             in, required, type=MrVariable object
;                       The y-coordinates to be rotated.
;       OZ:             in, required, type=MrVariable object
;                       The z-coordinates to be rotated.
;       ON:             out, required, type=MrTimeSeries object
;                       The resulting perp-1 component.
;       OP:             out, required, type=MrTimeSeries object
;                       The resulting perp-2 component.
;       OQ:             out, required, type=MrTimeSeries object
;                       The resulting field-aligned component.
;-
pro MrMMS_FPI_Dist::Grid_Cart2FAC, oT, oX, oY, oZ, oN, oP, oQ
	compile_opt idl2
	on_error, 2
	
	;Size of the variables
	dimsT = size(oT, /DIMENSIONS)
	nDims = size(oX, /N_DIMENSIONS)
	dims  = size(oX, /DIMENSIONS)
	nTime  = dimsT[0]

;-----------------------------------------------------
; Rotate to FAC \\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\
;-----------------------------------------------------
	if nDims eq 2 then begin
		nPhi   = dims[0]
		nTheta = dims[1]
		
		;Temporarily reform data to vectorize multiplication
		x_temp = rebin(reform(oX['DATA'], 1, nPhi, nTheta), nTime, nPhi, nTheta)
		y_temp = rebin(reform(oY['DATA'], 1, nPhi, nTheta), nTime, nPhi, nTheta)
		z_temp = rebin(reform(oZ['DATA'], 1, nPhi, nTheta), nTime, nPhi, nTheta)
		
		;Perp-1
		n = rebin(oT[*,0,0], nTime, nPhi, nTheta) * x_temp + $
		    rebin(oT[*,1,0], nTime, nPhi, nTheta) * y_temp + $
		    rebin(oT[*,2,0], nTime, nPhi, nTheta) * z_temp
		
		;Perp-2
		p = rebin(oT[*,0,1], nTime, nPhi, nTheta) * x_temp + $
		    rebin(oT[*,1,1], nTime, nPhi, nTheta) * y_temp + $
		    rebin(oT[*,2,1], nTime, nPhi, nTheta) * z_temp
		
		;Par
		q = rebin(oT[*,0,2], nTime, nPhi, nTheta) * temporary(x_temp) + $
		    rebin(oT[*,1,2], nTime, nPhi, nTheta) * temporary(y_temp) + $
		    rebin(oT[*,2,2], nTime, nPhi, nTheta) * temporary(z_temp)

		;Turn into time-series variables
		oN = MrTimeSeries( oT['TIMEVAR'], n, /NO_COPY )
		oP = MrTimeSeries( oT['TIMEVAR'], p, /NO_COPY )
		oQ = MrTimeSeries( oT['TIMEVAR'], q, /NO_COPY )

	;Time is first
	endif else begin
		nPhi   = dims[1]
		nTheta = dims[2]
		
		;Perp-1
		oN = rebin( oT[*,0,0], nTime, nPhi, nTheta) * oX + $
		     rebin( oT[*,1,0], nTime, nPhi, nTheta) * oY + $
		     rebin( oT[*,2,0], nTime, nPhi, nTheta) * oZ
		
		;Perp-2
		oP = rebin( oT[*,0,1], nTime, nPhi, nTheta) * oX + $
		     rebin( oT[*,1,1], nTime, nPhi, nTheta) * oY + $
		     rebin( oT[*,2,1], nTime, nPhi, nTheta) * oZ
		
		;Field-aligned
		oQ = rebin( oT[*,0,2], nTime, nPhi, nTheta) * oX + $
		     rebin( oT[*,1,2], nTime, nPhi, nTheta) * oY + $
		     rebin( oT[*,2,2], nTime, nPhi, nTheta) * oZ
	endelse
	
	;Renormalize
	mag = MrTimeSeries( oT['TIMEVAR'], sqrt( oN['DATA']^2 + oP['DATA']^2 + oQ['DATA']^2 ) )
	oN /= mag
	oP /= mag
	oQ /= temporary(mag)
end


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
;       OX:             out, optional, type=MrVariable object
;                       The X cartesian coordinates or points on a unit sphere.
;       OY:             in, required, type=fltarr
;                       The Y cartesian coordinates or points on a unit sphere.
;       OZ:             in, required, typefltarr
;                       The Z cartesian coordinates or points on a unit sphere.
;       OPHI:           in, required, type=N or NxM float
;                       The azimuth angle (degrees) on a unit sphere.
;       OTHETA:         in, required, type=N or NxM float
;                       The polar or elevation angle (degrees) on a unit sphere.
;
; :Keywords:
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
;-
pro MrMMS_FPI_Dist::Grid_Cart2Sphere, oX, oY, oZ, oPhi, oTheta, $
ORIENTATION=orientation
	compile_opt idl2
	on_error, 2
	
	;Defaults
	if n_elements(orientation) eq 0 then orientation = 3

;-----------------------------------------------------
; Compute Spherical Coordinates \\\\\\\\\\\\\\\\\\\\\\
;-----------------------------------------------------
	case orientation of
		;PHI   - Positive from x-axis             (-180, 180]
		;THETA - Polar angle from z-axis          [   0, 180]
		1: begin
			phi   = atan(oY['DATA'], oX['DATA'])
			theta = acos(oZ['DATA'] / sqrt(oX['DATA']^2 + oY['DATA']^2 + oZ['DATA']^2))     ;OR atan( sqrt(x^2 + y^2), z )
		endcase
		
		;PHI   - Positive from y-axis             (-180, 180]
		;THETA - Polar angle from z-axis          [   0, 180]
		2: begin
			phi   = atan(-oX['DATA'], oY['DATA'])
			theta = acos(oZ['DATA'] / sqrt(oX['DATA']^2 + oY['DATA']^2 + oZ['DATA']^2))     ;OR atan( sqrt(x^2 + y^2), z )
		endcase
		
		;PHI   - Positive from x-axis             (-180, 180]
		;THETA - Elevation angle from xy-plane    [ -90,  90]
		3: begin
			phi   = atan(oY['DATA'], oX['DATA'])
			theta = asin(oZ['DATA'] / sqrt(oX['DATA']^2 + oY['DATA']^2 + oZ['DATA']^2))     ;OR atan( z / sqrt(x^2 + y^2) )
		endcase
		
		;PHI   - Positive from y-axis             (-180, 180]
		;THETA - Elevation angle from xy-plane    [ -90,  90]
		4: begin
			phi   = atan(-oX['DATA'], oY['DATA'])
			theta = asin(oZ['DATA'] / sqrt(oX['DATA']^2 + oY['DATA']^2 + oZ['DATA']^2))     ;OR atan( z / sqrt(x^2 + y^2) )
		endcase
		
		;PHI   - Positive from z-axis             (-180, 180]
		;THETA - Polar angle from y-axis          [   0, 180]
		5: begin
			phi   = atan(oX['DATA'], oZ['DATA'])
			theta = acos(oY['DATA'] / sqrt(oX['DATA']^2 + oY['DATA']^2 + oZ['DATA']^2))     ;OR atan( sqrt(x^2 + z^2), y )
		endcase
		
		;PHI   - Positive from x-axis             (-180, 180]
		;THETA - Polar angle from y-axis          [   0, 180]
		6: begin
			phi   = atan(-oZ['DATA'], oX['DATA'])
			theta = acos(oY['DATA'] / sqrt(oX['DATA']^2 + oY['DATA']^2 + oZ['DATA']^2))     ;OR atan( sqrt(x^2 + z^2), y )
		endcase
		
		;PHI   - Positive from z-axis             (-180, 180]
		;THETA - Elevation angle from zx-plane    [ -90,  90]
		7: begin
			phi   = atan(oX['DATA'], oZ['DATA'])
			theta = asin(oY['DATA'] / sqrt(oX['DATA']^2 + oY['DATA']^2 + oZ['DATA']^2))     ;OR atan( y / sqrt(x^2 + z^2) )
		endcase
		
		;PHI   - Positive from x-axis             (-180, 180]
		;THETA - Elevation angle from zx-plane    [ -90,  90]
		8: begin
			phi   = atan(-oZ['DATA'], oX['DATA'])
			theta = asin(oY['DATA'] / sqrt(oX['DATA']^2 + oY['DATA']^2 + oZ['DATA']^2))     ;OR atan( y / sqrt(x^2 + z^2) )
		endcase
		
		;PHI   - Positive from y-axis             (-180, 180]
		;THETA - Polar angle from x-axis          [   0, 180]
		9: begin
			phi   = atan(oZ['DATA'], oY['DATA'])
			theta = acos(oX['DATA'] / sqrt(oX['DATA']^2 + oY['DATA']^2 + oZ['DATA']^2))     ;OR atan( sqrt(y^2 + z^2), x )
		endcase
		
		;PHI   - Positive from z-axis             (-180, 180]
		;THETA - Polar angle from x-axis          [   0, 180]
		10: begin
			phi   = atan(-oY['DATA'], oZ['DATA'])
			theta = acos(oX['DATA'] / sqrt(oX['DATA']^2 + oY['DATA']^2 + oZ['DATA']^2))     ;OR atan( sqrt(y^2 + z^2), x )
		endcase
		
		;PHI   - Positive from y-axis             (-180, 180]
		;THETA - Elevation angle from yz-plane    [ -90,  90]
		11: begin
			phi   = atan(oZ['DATA'], oY['DATA'])
			theta = asin(oX['DATA'] / sqrt(oX['DATA']^2 + oY['DATA']^2 + oZ['DATA']^2))     ;OR atan( x / sqrt(y^2 + z^2) )
		endcase
		
		;PHI   - Positive from z-axis             (-180, 180]
		;THETA - Elevation angle from yz-plane    [ -90,  90]
		12: begin
			phi   = atan(-oY['DATA'], oZ['DATA'])
			theta = asin(oX['DATA'] / sqrt(oX['DATA']^2 + oY['DATA']^2 + oZ['DATA']^2))     ;OR atan( x / sqrt(y^2 + z^2) )
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
	rad2deg  = size(oX, /TNAME) eq 'DOUBLE' ? 180.0D / !dpi : 180.0 /!pi
	phi     *= rad2deg
	theta   *= rad2deg

	;Create variables
	if size(phi, /N_DIMENSIONS) eq 3 then begin
		oPhi   = MrTimeSeries( oX['TIMEVAR'], phi, /NO_COPY)
		oTheta = MrTimeSeries( oX['TIMEVAR'], theta, /NO_COPY)
	endif else begin
		oPhi   = MrVariable(phi, /NO_COPY)
		oTheta = MrVariable(theta, /NO_COPY)
	endelse
	
	;Add attributes
	oPhi   -> AddAttr, 'UNITS', 'Degrees'
	oTheta -> AddAttr, 'UNITS', 'Degrees'
end

;+
;   Transform a spherical coordinate grid into a cartesian coordinate grid.
;
;   These are intended to come directly from the CDF file, as the particle instrument
;   team has defined their look directions in (phi,theta) space. As such, they are
;   often 1D arrays; i.e., one set of phi coordinates that describes the azimuth location
;   for all sensors, and one set of theta coordinates that describes the polar location of
;   all sensors.
;
; :Categories:
;   Distribution functions
;
; :Params:
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
;       PHI_GRID:       out, optional, type=MrVariable
;                       The azimuthal coordinate grid. Has dimensions [phi, theta]
;                           (and possibly time) and units indicated by the `DEGREES`
;                           keyword.
;       THETA_GRID:     out, optional, type=NxM or NxMxL float, default=0
;                       The polar grid coordinates. Has dimensions [phi, theta]
;                           (and possibly time) and units indicated by the `DEGREES`
;                           keyword.
;-
pro MrMMS_FPI_Dist::Grid_MakeCart, oPhi, oTheta, oXGrid, oYGrid, oZGrid, $
THETA_GRID=oThetaGrid, $
PHI_GRID=oPhiGrid
	compile_opt idl2
	on_error, 2

;-----------------------------------------------------
; Get Variables \\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\
;-----------------------------------------------------
	;Get the object references / verify object class.
	;   - These are assumed to be in units of DEGREES
	;   - See the ::SetData method
	
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
; Cartesian Grid \\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\
;-----------------------------------------------------

	;Convert degrees to radians
	deg2rad     = !dpi / 180.0
	rad2deg     = 180.0 / !dpi
	phi_grid   *= deg2rad
	theta_grid *= deg2rad

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
		;Create variable object
		if tf_timeseries $
			then oPhiGrid = MrTimeSeries( oTime, float( temporary(phi_grid) * rad2deg ) ) $
			else oPhiGrid = MrVariable( float( temporary(phi_grid) * rad2deg ) )
		
		;Add units
		oPhiGrid -> AddAttr, 'UNITS', 'degrees'
	endif
	
	;THETA
	if arg_present(oThetaGrid) then begin
		;Create variable object
		if tf_timeseries $
			then oThetaGrid = MrTimeSeries( oTime, float( temporary(theta_grid) * rad2deg ) ) $
			else oThetaGrid = MrVariable( float( temporary(theta_grid) * rad2deg ) )
		
		;Add units
		oThetaGrid -> AddAttr, 'UNITS', 'degrees'
	endif
end


;+
;   Find and read MMS FPI data.
;
; :Params:
;       SC:                 in, required, type=string/strarr
;                           The MMS spacecraft identifier. Options are:
;                               {'mms1' | 'mms2' | 'mms3' | 'mms4'}
;       MODE:               in, required, type=string/strarr
;                           Telemetry mode of the data. Options include: {'fast' | 'brst'}
;       SPECIES:            in, optional, type=string, default='e'
;                           Particle species for which the distribution function is
;                               to be loaded. Options are: {'e' | 'i'} for electrons
;                               and ions, respectively.
;
; :Keywords:
;       COORD_SYS:          in, optional, type=string, default='gse'
;                           Coordinate system of the original distribution function.
;                               Options are: {'dbcs' | 'gse'}.
;       LEVEL:              in, optional, type=string/strarr, default='l2'
;                           Data quality level. Options include:
;                               {'sitl' | 'l1b' | 'l2'}
;       TEAM_SITE:          in, optional, type=boolean, default=0
;                           If set, requests will be sent to the team site (password
;                               required, L1A and above). Automatically set if `LEVEL`
;                               is below level 2. This option is sticky.
;       TRANGE:             out, optional, type=string, default=MrVar_GetTRange()
;                           Start and end times over which to read data.
;       VARNAMES:           out, optional, type=strarr
;                           Names of all variables that have been loaded.
;-
pro MrMMS_FPI_Dist::Load, sc, mode, species, $
COORD_SYS=coord_sys, $
LEVEL=level, $
SUFFIX=suffix, $
TEAM_SITE=team_site, $
TRANGE=trange
	compile_opt idl2

	catch, the_error
	if the_error ne 0 then begin
		catch, /CANCEL
		MrPrintF, 'LogErr'
		return
	endif
	
	;Check inputs
	cs = n_elements(coord_sys) eq 0 ? 'gse' : strlowcase(coord_sys)
	if n_elements(level)   eq 0 then level   = 'l2'
	if n_elements(species) eq 0 then species = 'e'
	if n_elements(suffix)  eq 0 then suffix  = ''
	
	;Conflicts
	if n_elements(sc)    ne 1 then message, 'SC must be scalar.'
	if n_elements(mode)  ne 1 then message, 'MODE must be scalar.'
	if ~MrIsMember(['fast', 'brst'], mode) then message, 'MODE must be {"fast" | "slow"}'
	
	;Fast and Brst are organized differently
	if mode eq 'fast' $
		then varformat = ['*_dist_*', '*_theta_*', '*_energy_*', '*_phi_*'] $
		else varformat = ['*dist_*', '*energy?*', '*phi*', '*theta*', '*steptable*']

;-----------------------------------------------------
; Load the FPI \\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\
;-----------------------------------------------------
	;Get the FPI distribution function
	MrMMS_Load_Data, sc, 'fpi', mode, level, $
	                 OPTDESC   = 'd' + species + 's-dist', $
	                 SUFFIX    = suffix, $
	                 TEAM_SITE = team_site, $
	                 VARFORMAT = varformat, $
	                 VARNAMES  = varnames
	
	;Associate variable attributes with DEPEND_[0-3]
	dist_vname = sc + '_d' + species + 's_dist_' + mode + suffix
	self -> Load_Meta, dist_vname

;-----------------------------------------------------
; Finish up \\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\
;-----------------------------------------------------
	self.oDist = MrVar_Get(dist_vname)
	
	self.sc        = sc
	self.instr     = 'fpi'
	self.mode      = mode
	self.level     = level
	self.optdesc   = 'd' + species + 's-dist'
	self.coord_sys = cs
end


;+
;   Load the distribution from source.
;-
pro MrMMS_FPI_Dist::Load_FAC, type_fac, $
FGM_INSTR=fgm_instr
	compile_opt idl2
	on_error, 2
	
	if n_elements(type_fac) eq 0 then type_fac = ''
	
	;FGM has 'srvy' and 'brst'
	;FPI has 'fast' and 'brst'
	fgm_mode = self.mode eq 'fast' ? 'srvy' : 'brst'
	
	;EDP 'fast' data (32 S/s) is roughly the same cadence as FPI 'brst' (33.33 S/s)
	case strupcase(type_fac) of
		'VXB':  perp_mode = self.mode
		'EXB':  perp_mode = 'fast'
		'':     ;Not applicable
		'NONE': ;Do nothing
		else: message, 'TYPE_FAC unknown: "' + type_fac + '".'
	endcase

	;Create the object
	;   - AFG srvy data is ok for FPI fast, so level 2 FGM works.
	if ~obj_valid(self.oFAC) then begin
		self.oFAC = MrMMS_FGM( self.sc, fgm_mode, $
		                       COORD_SYS = self.coord_sys, $
		                       INSTR     = fgm_instr, $
		                       LEVEL     = self.level  )
	endif

	;Load the data
	self.oFAC -> Load_FAC, type_fac, self.oDist['TIMEVAR'], $
	                       PERP_MODE = perp_mode
end


;+
;   Create new energy tables and set the distribution function's DEPEND_1-3 variable
;   attributes to be variables with physical units.
;
; :Private:
;
; :Params:
;       NAMES:          in, required, type=string
;                       Name of the distribution function variable.
;-
pro MrMMS_FPI_Dist::Load_Meta, name
	compile_opt idl2
	on_error, 2
	
	;Variable name parts
	;   - sc_instr_energy_mode[_suffix]
	;      0   1     2     3      4+
	;    | prefix |      |   suffix   |
	parts  = strsplit(name, '_', /EXTRACT)
	prefix = strjoin(parts[0:1], '_') + '_'
	suffix = '_' + strjoin(parts[3:*], '_')

	;BRST mode
	;   - Two energy tables alternate based on STEPTABLE_PARITY
	;   - Before v3.0.0, two scalar energy tables were provided and they had to
	;     be combined into a time-varying energy matrix via the "steptable_parity"
	;     variable.
	;   - As of v3.0.0, this process has been done by the FPI team.
	if MrVar_IsCached( prefix + 'energy0' + suffix ) then begin
		;Get the sector, pixel, and energy tables
		oParity  = MrVar_Get( prefix + 'steptable_parity' + suffix )
		oEnergy0 = MrVar_Get( prefix + 'energy0'          + suffix )
		oEnergy1 = MrVar_Get( prefix + 'energy1'          + suffix )
		oDist    = MrVar_get( prefix + 'dist'             + suffix )
	
		;Create new energy table
		;   - One time-dependent energy table
		;   - One combined (average) energy table
		energy      = transpose( [ [oEnergy0['DATA']], [oEnergy1['DATA']] ] )
		energy_full = MrVariable( energy[oParity['DATA'], *],                NAME=eTable_name, /CACHE)
		energy_mean = MrVariable( reform( sqrt(energy[0,*] * energy[1,*]) ), NAME=eMean_name,  /CACHE)
	
		;Names of new energy tables
		eTable_name = prefix + 'energy_table'   + suffix
		eMean_name  = prefix + 'energy_geomean' + suffix
	
	;SRVY & BRST v3.0.0+
	endif else begin
		;There is only one energy table
		eTable_name = prefix + 'energy' + suffix
	endelse
	
	;Names of phi, theta, and new energy tables
	dist_name   = prefix + 'dist'  + suffix
	phi_name    = prefix + 'phi'   + suffix
	theta_name  = prefix + 'theta' + suffix
		
	;Set the distribution function dependencies
	oDist  = MrVar_Get(dist_name)
	oDist -> SetAttrValue, 'DEPEND_1', phi_name
	oDist -> SetAttrValue, 'DEPEND_2', theta_name
	oDist -> SetAttrValue, 'DEPEND_3', eTable_name
end


;+
;   Set the distribution function object (also, see the ::Load method). The distribution
;   should be 4D with dimensions of [time, phi, theta, energy], with the following
;   variable attributes:
;       DEPEND_0  --  Time
;       DEPEND_1  --  Phi
;       DEPEND_2  --  Theta
;       DEPEND_3  --  Energy
;
; :Params:
;       VARIABLE:       in, required, type=string/integer/objref
;                       Name, number, of MrTimeSeries objref of the distribution function
;                           variable.
;-
pro MrMMS_FPI_Dist::SetData, variable
	compile_opt idl2
	on_error, 2

;-------------------------------------------
; Get the Data Objects /////////////////////
;-------------------------------------------
	
	;Get the variable object
	oDist = MrVar_Get(variable)
	
	;Make sure it has all of the dependent variable attributes
	if ~obj_isa(oDist, 'MrTimeSeries') then message, 'VARIABLE must be a MrTimeSeries object.'
	oTime   = oDist['TIMEVAR']
	oPhi    = MrVar_Get( oDist['DEPEND_1'] )
	oTheta  = MrVar_Get( oDist['DEPEND_2'] )
	oEnergy = MrVar_Get( oDist['DEPEND_3'] )
	
	;Check sizes
	nDims   = size(oDist, /N_DIMENSIONS)
	dims    = size(oDist, /DIMENSIONS)
	nTime   = dims[0]
	nPhi    = dims[1]
	nTheta  = dims[2]
	nEnergy = dims[3]

;-------------------------------------------
; Check Name ///////////////////////////////
;-------------------------------------------
	parts   = stregex(oDist.name, '(mms[1-4])_(des|dis)_.+_(fast|brst)', /SUBEXP, /EXTRACT)
	if parts[0] ne '' then begin
		sc      = parts[1]
		species = strmid(parts[2], 1, 1)
		mode    = parts[3]
	endif

;-------------------------------------------
; Phi //////////////////////////////////////
;-------------------------------------------
	;Dimensions:
	;   - [nPhi]
	;   - [nTime, nPhi]
	;   - [nTime, nPhi, nTheta]
	nDims = size(oPhi, /N_DIMENSIONS)
	dims  = size(oPhi, /DIMENSIONS)
	case nDims of
		3: if dims[0] ne nTime && dims[1] ne nPhi && dims[2] ne nTheta then message, 'DEPEND_1 is incorrect size.'
		2: if dims[0] ne nTime && dims[1] ne nPhi then message, 'DEPEND_1 is incorrect size.'
		1: if dims[0] ne nPhi then message, 'DEPEND_1 is incorrect size.'
		else: message, 'DEPEND_1 is incorrect size.'
	endcase

;-------------------------------------------
; Theta ////////////////////////////////////
;-------------------------------------------
	;Dimensions:
	;   - [nTheta]
	;   - [nTime, nTheta]
	;   - [nTime, nPhi, nTheta]
	nDims = size(oTheta, /N_DIMENSIONS)
	dims  = size(oTheta, /DIMENSIONS)
	case nDims of
		3: if dims[0] ne nTime && dims[1] ne nPhi && dims[2] ne nTheta then message, 'DEPEND_2 is incorrect size.'
		2: if dims[0] ne nTime && dims[1] ne nTheta then message, 'DEPEND_2 is incorrect size.'
		1: if dims[0] ne nTheta then message, 'DEPEND_2 is incorrect size.'
		else: message, 'DEPEND_2 is incorrect size.'
	endcase

;-------------------------------------------
; Energy ///////////////////////////////////
;-------------------------------------------
	;Dimensions:
	;   - [nEnergy]
	;   - [nTime, nEnergy]
	nDims = size(oEnergy, /N_DIMENSIONS)
	dims  = size(oEnergy, /DIMENSIONS)
	case nDims of
		2: if dims[0] ne nTime && dims[1] ne nEnergy then message, 'DEPEND_3 is incorrect size.'
		1: if dims[0] ne nEnergy then message, 'DEPEND_3 is incorrect size.'
		else: message, 'DEPEND_3 is incorrect size.'
	endcase
	
	;Set Data
	self.oDist = oDist
	if n_elements(sc) gt 0 then begin
		self.sc        = sc
		self.instr     = 'fpi'
		self.mode      = mode
		self.level     = 'l2'
		self.optdesc   = 'd' + species + 's-dist'
		self.coord_sys = 'dbcs'
	endif
end


;+
;   The class definition statement.
;
; :Params:
;       CLASS:          out, optional, type=structure
;-
pro MrMMS_FPI_Dist__DEFINE
	compile_opt idl2
	
	class = { MrMMS_FPI_Dist, $
	          sc:        '', $
	          instr:     '', $
	          mode:      '', $
	          level:     '', $
	          optdesc:   '', $
	          coord_sys: '', $
	          oFAC:      obj_new(), $
	          oDist:     obj_new() $
	        }
end