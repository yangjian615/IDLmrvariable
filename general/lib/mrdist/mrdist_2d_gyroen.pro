; docformat = 'rst'
;
; NAME:
;    MrDist_2D_GyroEn
;
; PURPOSE:
;+
;   Create a pitch angle distribution from the time-dependent 3D distribution function.
;
; :Categories:
;    Distribution Function, PAD
;
; :Params:
;       DIST3D:     in, required, type=NxMxLxP float
;                   3D distribution function as a function of time (psd).
;       B:          in, required, type=Nx3 float
;                   Vector magnetic field (nT) that defines the axis from which
;                       pitch is measured.
;       PHI:        in, required, type=MxL float
;                   Azimuth locations on a spherical grid that define instrument
;                       look directions.
;       THETA:      in, required, type=MxL float
;                   Polar locations on a spherical grid that define instrument
;                       look directions.
;       ENERGY:     in, required, type=P float
;                   Polar locations on a spherical grid that define instrument
;                       look directions.
;
; :Keywords:
;       GYRO_BINS:      out, optional, type=fltarr
;                       A named variable to receive the centers of the new gyrophase bins.
;                           Bins range from -180 to 180 degrees.
;       NGYRO_BINS:     in, optional, type=integer
;                       Number of gyrophase angle bins in output vector. By default, the
;                           original number of bins is chosen, based on the dimensions
;                           of `DIST3D`.
;       ORDER:          in, optional, type=integer
;                       Indicate how the dimensions are ordered. By default, 0 is
;                           chosen for 2D arrays, and 1 is chosen for 3D arrays.
;                           Options are:
;                               0: [phi, theta]
;                               1: [phi, theta, time]
;                               2: [time, phi, theta]
;       PA_RANGE:       in, optional, type=boolean, default=[0,180]
;                       Pitch angle range over which to average.
;       RADIANS:        in, optional, type=boolean, default=0
;                       Set if `PHI` and `THETA` are given in radians. Degrees is the default
;       SPECIES:        in, optional, type=string, default='i'
;                       Species of particle represented by the distribution function. Choices::
;                           'e'    - Electron
;                           'i'    - H+   Proton
;                           'p'    - H+   Proton
;                           'H+'   - H+   Proton
;                           'He'   - He+  Helium
;                           'He2'  - He++ Doubly charged helium
;                           'O'    - O+   Oxygen
;       TYPE:           in, optional, type=string, default=''
;                       The type of field-aligned coordinate system to use. Options are::
;                           ''       - `VEC` = <undefined>      Z = B  X = YxB      Y = [1,0,0] x B
;                           'ExB'    - `VEC` = electric field   Z = B  X = ExB      Y = Bx(ExB)
;                           'VxB'    - `VEC` = velocity         Z = B  X = (BxV)xB  Y = BxV
;                           'RadAz'  - `VEC` = radial position  Z = B  X = BxR      Y = Bx(BxR)
;       UNITS:          in, optional, type=string, default='psd'
;                       Output units of the distribution. Options are::
;                           'ENERGY'      - eV
;                           'EFLUX'       - eV / cm^2 / s / sr / eV    or    keV / cm^2 / s / sr / keV
;                           'DIFF FLUX'   - # / cm^2 / s / sr / keV
;                           'PSD'         - s^2 / km^6
;                           'DF'          - s^2 / km^6
;       VEC:            in, optional, type=Nx3 float
;                       A time dependent 3-component vector that defines the plane
;                           perpendicular to `B`.
;
; :Returns:
;       GYROEN:         2D distribution as a function of time, gyrophase and energy.
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
;       2016/02/26  -   Written by Matthew Argall
;-
;*****************************************************************************************
function MrDist_2D_GyroEn, dist3D, b, phi, theta, energy, $
GYRO_BINS=gyro_bins, $
NGYRO_BINS=nBins, $
ORDER=order, $
PA_RANGE=pa_range, $
RADIANS=radians, $
SPECIES=species, $
TYPE=type, $
UNITS=units, $
VEC=vec
	compile_opt idl2

	;Rotate to field-aligned coordinates
	MrDist_Instr2FAC, b, phi, theta, x, y, z, $
	                  ORDER       = order, $
	                  RADIANS     = radians, $
	                  TYPE        = type, $
	                  VEC         = vec

	;Convert them back to spherical coordinates
	;   - Return angles in degrees
	MrDist_Cart2Sphere, temporary(x), temporary(y), temporary(z), phi, theta, $
	                    /DEGREES, $
	                    ORIENTATION = 1

	;Reduce to 2D
	;   - [time, PA, Energy]
	MrDist_GyroPAEn, dist3D, phi, theta, energy, $
	                 PHIE         = GyroEn, $
	                 GYRO_BINS    = gyro_bins, $
	                 NENERGY_BINS = nE_bins, $
	                 NGYRO_BINS   = nBins, $
	                 PA_RANGE     = pa_range, $
	                 UNITS        = units

	;Move gyrophase form [0,180,360) --> [0,180,-180,0) -> (-180,180]
	i180             = where(gyro_bins gt 180, n180)
	gyro_bins[i180] -= 360.0
	gyro_bins        = shift(gyro_bins, i180[0])
	GyroEn           = shift(GyroEn, 0, i180[0], 0)
	
	return, GyroEn
end