; docformat = 'rst'
;
; NAME:
;       MrMMS_FPI_Dist_GPD
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
; PURPOSE:
;+
;   Reduce the 3D distribution function to a 1D distribution in gyrophase.
;   One distribution will be created for each range of energy channels given. The range
;   of polar angle can be constrained.
;
;   It is expected that the 3D distribution function and already been loaded and its
;   coordinates have been rotated to field-aligned coordinates (e.g. via
;   MrDist_FPI_Load_Dist3D.pro).
;
; :Categories:
;       MrVariable, MMS, FPI, Distribution Functions
;
; :Params:
;       SC:                 in, required, type=string/strarr
;                           The MMS spacecraft identifier. Options are:
;                               {'mms1' | 'mms2' | 'mms3' | 'mms4'}
;       MODE:               in, required, type=string/strarr
;                           Telemetry mode of the data. Options include: {'fast' | 'brst'}
;       SPECIES:            in, optional, type=string, default='e'
;                           Particle species. Options include {'e' | 'i'} for electrons
;                               and ions, respectively.
;       CHANNELS:           in, required, type=2xN intarr, default=[0\,31]
;                           Energy channels to be averaged. Channels range from 0 to 31.
;
; :Keywords:
;       SHOW_EBINS:         in, optional, type=boolean, default=0
;                           If set, the energy bins and their respective channels will
;                               be printed. All other keywords are ignored and no data
;                               is produced.
;       THETA_RANGE:        in, optional, type=fltarr(2), default=[0.0\, 180.0]
;                           Range in polar angle over which to average the data.
;       UNITS:              in, optional, type=string, default='E FLUX'
;                           Units of the output distribution. Options are::
;                               'ENERGY'      - eV
;                               'EFLUX'       - eV / cm^2 / s / sr / eV    or    keV / cm^2 / s / sr / keV
;                               'DIFF FLUX'   - # / cm^2 / s / sr / keV
;                               'PSD'         - s^2 / km^6
;                               'DF'          - s^2 / km^6
;       VARNAMES:           out, optional, type=string/strarr
;                           Names of the variables created and cached.
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
;       2016/09/01  -   Written by Matthew Argall
;-
pro MrMMS_FPI_Dist_GPD, sc, mode, species, channels, $
SHOW_EBINS=show_ebins, $
THETA_RANGE=theta_range, $
UNITS=units, $
VARNAMES=varnames
	compile_opt idl2

	catch, the_error
	if the_error ne 0 then begin
		catch, /CANCEL
		MrPrintF, 'LogErr'
		return
	endif
	
	;Check inputs
	tf_show_ebins = keyword_set(show_ebins)
	if n_elements(species)     eq 0 then species     = 'e'
	if n_elements(channels)    eq 0 then channels    = [0, 31]
	if n_elements(theta_range) eq 0 then theta_range = [0.0, 180.0]
	
	;Restrictions
	if n_elements(sc)   ne 1 then message, 'SC must be scalar.'
	if n_elements(mode) ne 1 then message, 'MODE must be scalar.'
	if ~array_equal(channels ge 0 and channels le 31, 1) $
		then message, 'Invalid channel. 0 <= CHANNELS <= 31.'
	
	;Constants for later
	eV2J = constants('eV2J')
	m2km = constants('m2km')
	mass = species eq 'e' ? constants('m_e') : constants('m_H')
	
;-----------------------------------------------------
; Create Variable Names \\\\\\\\\\\\\\\\\\\\\\\\\\\\\\
;-----------------------------------------------------
	
	;Variable Names
	theta_vname     = sc + '_d' + species + 's_theta_'            + mode
	theta_fac_vname = sc + '_d' + species + 's_theta_fac_'        + mode
	phi_fac_vname   = sc + '_d' + species + 's_phi_fac_'          + mode
	phi_vname       = sc + '_d' + species + 's_phi_'              + mode
	e0_vname        = sc + '_d' + species + 's_energy0_'          + mode
	e1_vname        = sc + '_d' + species + 's_energy1_'          + mode
	etable_vname    = sc + '_d' + species + 's_energy_table_'     + mode
	emean_vname     = sc + '_d' + species + 's_energy_geomean_'   + mode
	dist_fac_vname  = sc + '_d' + species + 's_dist_fac_'         + mode
	
	;New variable names
	pad_vname = sc + '_d' + species + 's_pad_'   + mode
	gpd_vname = sc + '_d' + species + 's_gpd_'   + mode
	ed_vname  = sc + '_d' + species + 's_espec_' + mode

;-----------------------------------------------------
; Show the Energies? \\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\
;-----------------------------------------------------
	if tf_show_ebins then begin
		;Grab the two energy tables
		oE0 = MrVar_Get(e0_vname)
		oE1 = MrVar_Get(e1_vname)
		
		;Create an index vector
		nEnergy = n_elements(oE0)
		idx     = indgen(nEnergy)

		;Print a header
		print, 'Index', 'E0', 'E1', FORMAT='(a5, 5x, a2, 9x, a2)'
		
		;Print the energy table
		;   - Scalar integer 0 returns the object itself.
		;   - To return the value at index zero, the index must be an array: index = [0]
		for i = 0, nEnergy-1 do print, idx[i], oE0[[i]], oE1[[i]], FORMAT='(2x, i2, 2x, f9.2, 2x, f9.2)'
		return
	endif

;-----------------------------------------------------
; Reduce to Energy-Phi \\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\
;-----------------------------------------------------

	;Average along THETA
	MrVar_DF_Reduce, dist_fac_vname, phi_fac_vname, theta_fac_vname, etable_vname, $
	                 UNITS       = units, $
	                 SPECIES     = species, $
	                 THETA_RANGE = theta_range, $
	                 EGYRO       = oEGyro

;-----------------------------------------------------
; Weight Function \\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\
;-----------------------------------------------------
	;Number of channels into which the distribution function is split
	;   - If one channel was given, the second dimension will be 0
	nChannels = MrDim(channels, 2) > 1
	
	;Allocate memory to results
	dims    = size(oEGyro, /DIMENSIONS)
	nTime   = dims[0]
	nPhi    = dims[1]
	nEnergy = dims[2]
	
	;Extract the energy bins
	oEBins = MrVar_Get(etable_vname)
	
	;Determine velocity-space bin size
	dE    = MrDist_DeltaE(oEBins['DATA'])
	v_sqr = oEBins * ( (2 * eV2J * m2km^2) / mass )
	dv    = sqrt(1.0 / (2.0 * mass * oEBins['DATA']) ) * dE * sqrt(eV2J)*m2km
	
	;Weight
	;   - w = v^2 * sin(theta) * dv * dTheta * dPhi
	;   - applied as total(w*dist)/total(w)
	;   - theta, dTheta, and dPhi are constant, so cancel out
	;   => w = v^2 * dv
	w = temporary(v_sqr) * temporary(dv)
	w = transpose( rebin( w['DATA'], nTime, nEnergy, nPhi ), [0, 2, 1] )

;-----------------------------------------------------
; Reduce to Phi \\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\
;-----------------------------------------------------

	;Loop through each channel
	for i = 0, nChannels - 1 do begin
		inds = indgen(channels[1,i] - channels[0,i] + 1) + channels[0,i]
		
		;Average energy bins
		gpd = total(oEGyro[*,*,inds] * w[*,*,inds], 3) / total(w[*,*,inds], 3)
		
		;Create a name
		name = string( sc, species, channels[0,i], channels[1,i], mode, $
		               FORMAT='(%"%s_d%ss_gpd_ch%02i_ch%02i_%s")' )
		
		;Create a MrVariable
		oESpec = MrVariable(gpd, /NO_COPY, NAME=name, /CACHE)
		oESpec -> AddAttr, 'DEPEND_0', oEGyro['DEPEND_0']
		oESpec -> AddAttr, 'DEPEND_1', phi_vname
		oESpec -> AddAttr, 'LOG',      1
		oESpec -> AddAttr, 'TITLE',    units
		oESpec -> AddAttr, 'SCALE',    1
	endfor
end