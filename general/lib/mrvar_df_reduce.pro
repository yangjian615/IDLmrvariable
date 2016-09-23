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
;       2016/08/28  -   Written by Matthew Argall
;-
pro MrVar_DF_Reduce, distFn, phi, theta, energy, $
SPECIES=species, $
UNITS=units, $
;Range to average
ENERGY_RANGE=energy_range, $
PHI_RANGE=phi_range, $
THETA_RANGE=theta_range, $
;Number of output bins
NENERGY_BINS=nEnergy_bins, $
NPHI_BINS=nPhi_bins, $
NTHETA_BINS=nTheta_bins, $
;Output bins
ENERGY_BINS=energy_bins, $
PHI_BINS=phi_bins, $
THETA_BINS=theta_bins, $
;Output distributions
EGYRO=EGyro, $
ESPEC=ESpec, $
EPA=EPA, $
PAGYRO=PAGyro, $
PAD=pad, $
GPD=gpd
	compile_opt idl2
	on_error, 2
	
	;Get/Verify variables
	oDist   = MrVar_Get(distFN)
	oPhi    = MrVar_Get(phi)
	oTheta  = MrVar_Get(theta)
	oEnergy = MrVar_Get(energy)
	
	;Defaults
	tf_degrees = keyword_set(degrees)
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

	;Dimension sizes
	;   - [time, phi, theta, energy]
	dims    = size(oDist, /DIMENSIONS)
	nTime   = dims[0]
	nPhi    = dims[1]
	nTheta  = dims[2]
	nEnergy = dims[3]
	
	;Number of output bins
	if n_elements(nPhi_bins)    eq 0 then nPhi_bins    = nPhi
	if n_elements(nTheta_bins)  eq 0 then nTheta_bins  = nTheta
	if n_elements(nEnergy_bins) eq 0 then nEnergy_bins = nEnergy

;-----------------------------------------------------
; Which Products to Return \\\\\\\\\\\\\\\\\\\\\\\\\\\
;-----------------------------------------------------
	;Which data products to produce?
	tf_EPA    = arg_present(EPA)
	tf_EGyro  = arg_present(EGyro)
	tf_Espec  = arg_present(Espec)
	tf_PAGyro = arg_present(PAGyro)
	tf_pad    = arg_present(pad)
	tf_gpd    = arg_present(gpd)
	
	;Allocate memory
	if tf_EPA    then EPA    = fltarr(nTime, nTheta_bins, nEnergy_bins)
	if tf_EGyro  then EGyro  = fltarr(nTime, nPhi_bins, nEnergy_bins)
	if tf_PAGyro then PAGyro = fltarr(nTime, nPhi_bins, nTheta_bins)
	if tf_gpd    then gpd    = fltarr(nTime, nPhi_bins)
	if tf_pad    then pad    = fltarr(nTime, nTheta_bins)
	if tf_Espec  then Espec  = fltarr(nTime, nEnergy_bins)

;-----------------------------------------------------
; Create Bins \\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\
;-----------------------------------------------------
	;PHI
	if nPhi_bins ne nPhi then begin
		;Create the bins
		dPhi     = 360.0 / nPhi_bins
		phi_bins = findgen(nPhi_bins) * dPhi + dPhi/2.0
		
		;Create a MrVariable object
		name     = oPhi.name + '_' + string(nPhi_bins, FORMAT='(i0)') + 'bins'
		phi_bins = MrVariable(phi_bins, NAME=name, /NO_COPY)
		phi_bins -> CreateAttr, 'UNITS', 'degrees'
		phi_bins -> CreateAttr, 'TITLE', 'Phi Bins'
	endif else begin
		phi_bins = oPhi
	endelse
	
	;THETA
	if nTheta_bins ne nTheta then begin
		;Create the bins
		dTheta = 180.0 / nTheta_bins
		theta_bins = findgen(nTheta_bins) * dTheta + dTheta/2.0
		
		;Create a MrVariable object
		name        = oTheta.name + '_' + string(nTheta_bins, FORMAT='(i0)') + 'bins'
		theta_bins  = MrVariable(theta_bins, NAME=name, /NO_COPY)
		theta_bins -> CreateAttr, 'UNITS', 'degrees'
		theta_bins -> CreateAttr, 'TITLE', 'Theta Bins'
	endif else begin
		theta_bins = oTheta
	endelse
	
	;ENERGY
	if nEnergy_bins ne nEnergy $
		then message, 'Changing the number of energy bins has not been thought out yet.' $
		else energy_bins = oEnergy

;-----------------------------------------------------
; Step through Time \\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\
;-----------------------------------------------------
	
	;Loop over time
	for i = 0, nTime - 1 do begin
		;Extract information for time i
		oneDist  = reform(oDist[i,*,*,*])
		onePhi   = reform(oPhi[i,*,*])
		oneTheta = reform(oTheta[i,*,*])
		oneE     = reform(oEnergy[i,*])
		
		;Convert units
		if dist_units ne 'PSD' then begin
			oneDist = MrDist_ConvertUnits( oneDist * 1e30, $
			                               ENERGY    = rebin( reform(energy, 1, 1, nEnergy), nPhi, nTheta, nEnergy ), $
			                               FROM_UNIT = 'PSD', $
			                               SPECIES   = species, $
			                               TO_UNIT   = units )
		endif

	;-----------------------------------------------------
	; 2D: Energy - Pitch Angle \\\\\\\\\\\\\\\\\\\\\\\\\\\
	;-----------------------------------------------------
		if tf_EPA then begin
			;Reduce the distribution
			EPA[i,*,*] = MrDist_PAEn( oneDist, onePhi, oneTheta, $
			                          PHI_RANGE    = phi_range, $
			                          NTHETA_BINS  = nTheta_bins, $
			                          NE_BINS      = nEnergy_bins )
			
			;Create variable
			EPA  = MrVariable(EPA, /NO_COPY)
			EPA -> AddAttr, 'DEPEND_0', oDist['DEPEND_0']
			EPA -> AddAttr, 'DEPEND_1', theta_bins.name
			EPA -> AddAttr, 'DEPEND_2', energy_bins.name
			EPA -> AddAttr, 'UNITS',    dist_units
			EPA -> AddAttr, 'TITLE',    '2D Pitch Angle & Energy Distribution'
		endif

	;-----------------------------------------------------
	; 2D: Energy - Gyrophase \\\\\\\\\\\\\\\\\\\\\\\\\\\\\
	;-----------------------------------------------------
		if tf_EGyro then begin
			;Reduce the distribution
			EGyro[i,*,*] = MrDist_GyroEn( oneDist, onePhi, oneTheta, $
			                              NE_BINS     = nEnergy_bins, $
			                              NPHI_BINS   = nPhi_bins, $
			                              THETA_RANGE = theta_range )
			
			;Create variable
			EGyro  = MrVariable(EGyro, /NO_COPY)
			EGyro -> AddAttr, 'DEPEND_0', oDist['DEPEND_0']
			EGyro -> AddAttr, 'DEPEND_1', phi_bins.name
			EGyro -> AddAttr, 'DEPEND_2', energy_bins.name
			EGyro -> AddAttr, 'UNITS',    dist_units
			EGyro -> AddAttr, 'TITLE',    '2D Energy & Gyrophase Distribution'
		endif

	;-----------------------------------------------------
	; 2D: Pitch Angle - Gyrophase \\\\\\\\\\\\\\\\\\\\\\\\
	;-----------------------------------------------------
		if tf_PAGyro then begin
			;Reduce the distribution
			PAGyro[i,*,*] = MrDist_GyroPA( oneDist, onePhi, oneTheta, $
			                               E_RANGE     = energy_range, $
			                               MASS        = mass, $
			                               NTHETA_BINS = nTheta_bins, $
			                               NPHI_BINS   = nPhi_bins )
			
			;Create variable
			PAGyro  = MrVariable(PAGyro, /NO_COPY)
			PAGyro -> AddAttr, 'DEPEND_0', oDist['DEPEND_0']
			PAGyro -> AddAttr, 'DEPEND_1', phi_bins.name
			PAGyro -> AddAttr, 'DEPEND_2', theta_bins.name
			PAGyro -> AddAttr, 'UNITS',    dist_units
			PAGyro -> AddAttr, 'TITLE',    '2D Pitch Angle & Gyrophase Distribution'
		endif

	;-----------------------------------------------------
	; 1D: Gyrophase \\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\
	;-----------------------------------------------------
		if tf_gpd then begin
			;Reduce the distribution
			gpd[i,*] = MrDist_GyroSpec( oneDist, onePhi, oneTheta, oneE, $
			                            E_RANGE     = energy_range, $
			                            THETA_RANGE = theta_range, $
			                            MASS        = mass, $
			                            NPHI_BINS   = nPhi_bins )
			
			;Create variable
			gpd  = MrVariable(gpd, /NO_COPY)
			gpd -> AddAttr, 'DEPEND_0', oDist['DEPEND_0']
			gpd -> AddAttr, 'DEPEND_1', phi_bins.name
			gpd -> AddAttr, 'UNITS',    dist_units
			gpd -> AddAttr, 'TITLE',    'Gyrophase Distribution'
		endif

	;-----------------------------------------------------
	; 1D: Pitch Angle \\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\
	;-----------------------------------------------------
		if tf_pad then begin
			;Reduce the distribution
			pad[i,*] = MrDist_PASpec( oneDist, onePhi, oneTheta, oneE, $
			                          PHI_RANGE   = phi_range, $
			                          E_RANGE     = energy_range, $
			                          MASS        = mass, $
			                          NTHETA_BINS = nTheta_Bins )
			
			;Create variable
			pad  = MrVariable(pad, /NO_COPY)
			pad -> AddAttr, 'DEPEND_0', oDist['DEPEND_0']
			pad -> AddAttr, 'DEPEND_1', theta_bins.name
			pad -> AddAttr, 'UNITS',    dist_units
			pad -> AddAttr, 'TITLE',    'Pitch Angle Distribution'
		endif

	;-----------------------------------------------------
	; 1D: Energy \\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\
	;-----------------------------------------------------
		if tf_Espec then begin
			;Reduce the distribution
			Espec[i,*] = MrDist_ESpec( oneDist, onePhi, oneTheta, $
			                           PHI_RANGE   = phi_range, $
			                           NE_BINS     = nEnergy_bins, $
			                           THETA_RANGE = theta_range )
			
			;Create variable
			espec  = MrVariable(Espec, /NO_COPY)
			espec -> AddAttr, 'DEPEND_0', oDist['DEPEND_0']
			espec -> AddAttr, 'DEPEND_1', energy_bins.name
			espec -> AddAttr, 'UNITS',    dist_units
			espec -> AddAttr, 'TITLE',    'Energy Distribution'
		endif
	endfor
end