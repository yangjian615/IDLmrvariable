; docformat = 'rst'
;
; NAME:
;       MrDist_GyroPAEn
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
;       DISTFN:         in, required, type=NxMxL float
;                       The 3D distribution function to be reduced. Its dimensions must
;                           be [nPhi, nTheta, nEnergy].
;       PHI:            in, required, type=fltarr(N)
;                       The azimuthal coordinates (degrees) on a spherical grid where
;                           each point in `DISTFN` is defined.
;       THETA:          in, required, type=fltarr(M)
;                       The polar coordinates (degrees) on a spherical grid where
;                           each point in `DISTFN` is defined.
;       ENERGY:         in, required, type=fltarr(L)
;                       The energy bins, in electron volts (eV), at which each point in
;                           `DISTFN` is defined.
;
; :Keywords:
;       ORDER:          in, optional, type=integer, default=1
;                       Flag indicating how the dimensions of `DISTFN` are ordered. Options::
;                           1: [nTime, nPhi, nTheta, nEnergy]
;       SPECIES:        in, optional, type=string, default='i'
;                       Species of particle represented by the distribution function. Choices::
;                           'e'    - Electron
;                           'i'    - H+   Proton
;                           'p'    - H+   Proton
;                           'H+'   - H+   Proton
;                           'He'   - He+  Helium
;                           'He2'  - He++ Doubly charged helium
;                           'O'    - O+   Oxygen
;       UNITS:          in, optional, type=string, default='psd'
;                       Output units of the distribution. Options are::
;                           'ENERGY'      - eV
;                           'EFLUX'       - eV / cm^2 / s / sr / eV    or    keV / cm^2 / s / sr / keV
;                           'DIFF FLUX'   - # / cm^2 / s / sr / keV
;                           'PSD'         - s^2 / km^6
;                           'DF'          - s^2 / km^6
;
;       E_RANGE:        in, optional, type=fltarr(2), default=[min, max]
;                       The range in energy, in electron volts (eV) over which to average.
;       GYRO_RANGE:     in, optional, type=fltarr(2), default=[0.0\, 360.0]
;                       The range in azimuth (degrees) over which to average.
;       PA_RANGE:       in, optional, type=fltarr(2), default=[0.0\, 180.0]
;                       The range in polar angle (degrees) over which to average.
;
;       NE_BINS:        in, optional, type=integer, default=nEnergy
;                       The number of energy angle bins for the reduced distribution.
;       NGYRO_BINS:     in, optional, type=integer, default=nTheta
;                       The number of evently spaced azimuth angle bins for the reduced
;                           distribution.
;       NPA_BINS:       in, optional, type=integer, default=nTheta
;                       The number of evently spaced polar angle bins for the reduced
;                           distribution.
;
;       E_BINS:         out, optional, type=integer
;                       Energy bins (eV) at which the output distribution is defined.
;       GYRO_BINS:      out, optional, type=integer
;                       Gyrophase bins (degrees) at which the output distribution
;                           is defined.
;       PA_BINS:        out, optional, type=integer
;                       Pitch angle bins (degrees) at which the output distribution
;                           is defined.
;
;       ENSPEC:         out, optional, type=TxL float
;                       1D energy distribution.
;       PHIE:           out, optional, type=TxL float
;                       2D distribution in azimuth and energy.
;       PHITHETA:       out, optional, type=TxL float
;                       2D distribution in azimuth and polar sectors.
;       PAD:            out, optional, type=TxL float
;                       1D pitch angle distribution.
;       GPD:            out, optional, type=TxL float
;                       1D gyrophase distribution.
;       THETAE:         out, optional, type=TxL float
;                       2D pitch angle and energy distirbution.
;
; :Returns:
;       DIST2D:         The resulting 2D distribution function
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
pro MrDist_GyroPAEn, distFn, phi, theta, energy, $
ORDER=order, $
SPECIES=species, $
UNITS=units, $
;Range to average
PA_RANGE=pa_range, $
GYRO_RANGE=gyro_range, $
E_RANGE=e_range, $
;Number of output bins
NPA_BINS=nPA_bins, $
NGYRO_BINS=nGyro_bins, $
NENERGY_BINS=nEnergy_bins, $
;Output bins
PA_BINS=pa_bins, $
GYRO_BINS=gyro_bins, $
ENERGY_BINS=energy_bins, $
;Output distributions
ENSPEC=EnSpec, $
PHIE=PhiE, $
PHITHETA=PhiTheta, $
PAD=pad, $
GPD=gpd, $
THETAE=ThetaE
	compile_opt idl2
	on_error, 2
	
	;Defaults
	tf_degrees = 1 ;keyword_set(degrees)
	dist_units = n_elements(units) eq 0 ? 'PSD' : strupcase(units)
	if n_elements(order)   eq 0 then order   = 1
	if n_elements(species) eq 0 then species = 'e'
	
	;Mass
	mass = 1.6726219e-27   ;Proton mass (kg)
	case species of
		'e':   A = 1.0/1836.0  ; e-
		'i':   A = 1           ; H+
		'p':   A = 1           ; H+
		'H':   A = 1           ; H+
		'He':  A = 4           ; He+
		'He2': A = 4           ; He++
		'O':   A = 16          ; O+
		else: message, 'Species not recognized: "' + species + '".'
	endcase
	mass *= A

	;Which dimension is which?
	dims = size(distFn, /DIMENSIONS)
	case order of
		;[time, phi, theta, energy]
		1: begin
			nTime   = dims[0]
			nPhi    = dims[1]
			nTheta  = dims[2]
			nEnergy = dims[3]
		endcase
		else: message, 'Invalid value for ORDER.'
	endcase
	
	;Number of output bins
	if n_elements(nPA_bins)     eq 0 then nPA_bins     = nTheta
	if n_elements(nGyro_bins)   eq 0 then nGyro_bins   = nPhi
	if n_elements(nEnergy_bins) eq 0 then nEnergy_bins = nEnergy

;-----------------------------------------------------
; Which Products to Return \\\\\\\\\\\\\\\\\\\\\\\\\\\
;-----------------------------------------------------
	;Which data products to produce?
	tf_paEn   = arg_present(ThetaE)
	tf_gyroEn = arg_present(PhiE)
	tf_gyroPA = arg_present(PhiTheta)
	tf_pa     = arg_present(pad)
	tf_en     = arg_present(enSpec)
	tf_gyro   = arg_present(gpd)
	
	;Allocate memory
	if tf_paEn   then ThetaE   = fltarr(nTime, nPA_bins, nEnergy_bins)
	if tf_gyroEn then PhiE     = fltarr(nTime, nGyro_bins, nEnergy_bins)
	if tf_gyroPA then PhiTheta = fltarr(nTime, nGyro_bins, nPA_bins)
	if tf_gyro   then gpd      = fltarr(nTime, nGyro_bins)
	if tf_pa     then pad      = fltarr(nTime, nPA_bins)
	if tf_en     then enSpec   = fltarr(nTime, nEnergy_bins)

;-----------------------------------------------------
; Step through Time \\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\
;-----------------------------------------------------
	
	;Loop over time
	for i = 0, nTime - 1 do begin
		;Extract information for time i
		oneDist  = reform(distFn[i,*,*,*])
		onePhi   = reform(phi[i,*,*])
		oneTheta = reform(theta[i,*,*])
		
		;Convert units
		if dist_units ne 'PSD' then begin
			oneDist = MrDist_ConvertUnits(oneDist * 1e30, units, 'PSD', $
			                              ENERGY    = rebin( reform(energy, 1, 1, nEnergy), nPhi, nTheta, nEnergy ), $
			                              SPECIES   = species )
		endif
	
		if tf_paEn   then ThetaE[i,*,*]   = MrDist_PAEn(   oneDist, onePhi, oneTheta, GYRO_RANGE=gyro_range, NPA_BINS=npa_bins, NE_BINS=nEnergy_bins)
		if tf_gyroEn then PhiE[i,*,*]     = MrDist_GyroEn( oneDist, onePhi, oneTheta, PA_RANGE=pa_range)
		if tf_gyroPA then PhiTheta[i,*,*] = MrDist_GyroPA( oneDist, onePhi, oneTheta)
		if tf_gyro   then gpd[i,*]        = MrDist_Gyro(   oneDist, onePhi, oneTheta, energy, PA_RANGE=pa_range, E_RANGE=e_range)
		if tf_pa     then pad[i,*]        = MrDist_PA(     oneDist, onePhi, oneTheta, energy, GYRO_RANGE=gyro_range, E_RANGE=e_range, MASS=mass, NBINS=nPA_Bins)
		if tf_en     then enSpec[i,*]     = MrDist_En(     oneDist, onePhi, oneTheta, GYRO_RANGE=gyro_range, PA_RANGE=pa_range)
	endfor

;-----------------------------------------------------
; Create Bins \\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\
;-----------------------------------------------------
	
	;Size of each angular bin
	dPhi   = 360.0 / nGyro_bins
	dTheta = 180.0 / nPA_bins
	
	;Create bins
	pa_bins   = findgen(nPA_bins) * dTheta + dTheta/2.0
	gyro_bins = findgen(nGyro_bins) * dPhi + dPhi/2.0
end