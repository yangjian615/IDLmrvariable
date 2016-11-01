; docformat = 'rst'
;
; NAME:
;   MrDist4D__Define
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
;   REFERENCES:
;       [1] CIS Interface Control Document
;               http://caa.estec.esa.int/documents/ICD/CAA_CIS_ICD_V3.4.2.pdf
;               http://www.cosmos.esa.int/web/csa/documentation
;       [2] FPI Dataset
;               https://lasp.colorado.edu/mms/sdc/public/datasets/fpi/
;
; :Categories:
;   MrVariable, MrTimeSeries, MrDist
;
; :See Also:
;   MrDist3D__Define.pro
;   MrTimeSeries__Define.pro
;   MrVariable__Define.pro
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
;       DIMENSION:      in, optional, type=integer
;                       The time-dependent, 1-based dimension of `DATA`. If not provided,
;                           the dimension of `DATA` that is equal in size to `TIME` is
;                           chose as the default.
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
;       T_TYPE:         in, optional, type=integer
;                       If `TIME` is an array of time stamps, use this keyword to indicate
;                           the format or time-basis. See MrTimeVar for more details.
;       T_NAME:         in, optional, type=integer
;                       Name to be given to the MrTimeVar object. Ignored unless `TIME`
;                           is an array of time stamps.
;-
function MrDist4D::INIT, time, dist3D, phi, theta, energy, $
CACHE=cache, $
DEGREES=degrees, $
DIMENSION=dimension, $
MASS=mass, $
NAME=name, $
NO_CLOBBER=no_clobber, $
NO_COPY=no_copy, $
RADIANS=radians, $
SPECIES=species, $
T_NAME=t_name, $
T_TYPE=t_type, $
UNITS=units
	compile_opt idl2

	;Error handling
	catch, the_error
	if the_error ne 0 then begin
		catch, /CANCEL
		MrPrintF, 'LogErr'
		return, 0
	endif
	
	;Defaults
	if n_elements(name) eq 0 then name = 'MrDist4D'

;-----------------------------------------------------
; Initialize \\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\
;-----------------------------------------------------
	success = self -> MrTimeSeries::Init( CACHE      = cache, $
		                                  DIMENSION  = dimension, $
		                                  NAME       = name, $
		                                  NO_CLOBBER = no_clobber, $
		                                  NO_COPY    = no_copy, $
		                                  T_NAME     = t_name, $
		                                  T_TYPE     = t_type )
	if ~success then message, 'Unable to initialize superclass.'

;-----------------------------------------------------
; Set Data \\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\
;-----------------------------------------------------
	if n_elements(dist3D) gt 0 then begin
		;Store the data
		self -> SetData, time, dist3D, phi, theta, energy, $
		                 UNITS   = units, $
		                 DEGREES = degrees, $
		                 RADIANS = radians

		;Create a coordinate grid
		self -> Grid
	endif

;-----------------------------------------------------
; Set Properties \\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\
;-----------------------------------------------------
	self -> SetProperty, MASS       = mass, $
	                     SPECIES    = species

	return, 1
end


;+
;   Clean up after the object is destroyed
;-
pro MrDist4D::CLEANUP
	compile_opt idl2
	on_error, 2

	;
	; Do not destroy other object properties
	;   - They could be DEPEND_# attributes of other variables
	;   - Automatic garbage collection will destroy them
	;
	
	obj_destroy, oTheta_FAC
	obj_destroy, oTheta_Grid
	obj_destroy, oPhi_FAC
	obj_destroy, oPhi_Grid
	
	;Superclass
	self -> MrTimeSeries::Cleanup
end


;+
;   Convert the distribution function from one set of units to another.
;
; :Params:
;       FLUX:           in, required, type=fltarr
;                       Distribution function for which to convert units
;       TO_UNITS:       in, required, type=string
;                       Convert to these units.  Options are::
;                           'ENERGY'      - eV
;                           'EFLUX'       - eV / cm^2 / s / sr / eV    or    keV / cm^2 / s / sr / keV
;                           'DIFF FLUX'   - # / cm^2 / s / sr / keV
;                           'PSD'         - s^2 / m^6
;                           'DF'          - s^2 / m^6
;       FROM_UNITS:     in, optional, type=string, default='PSD'
;                       Name of the units of `FLUX`.
;
; :Keywords:
;       SPECIES:        in, optional, type=string, default='e'
;                       Particle species measured in the distribution. Particle mass
;                           is taken into account when converting 'PSD' <-> {'EFLUX' | 'DIFF FLUX'}
;       ENERGY:         in, optional, type=float
;                       Energy bins at which the distribution is taken.
;
; :Returns:
;       NEW_FLUX:       Distribution with new units.
;-
pro MrDist4D::ConvertUnits, to_units
	compile_opt idl2
	on_error, 2
	
	toUnits = strupcase(to_units)

	;Conversion factor from energy flux to phase space density
	;   - mass^2 * (sr / eV^2)
	eflux_to_psd = self.mass^2 * 0.5447e6
	
	;Vectorize multiplication
	dims  = size(*self.data, /DIMENSIONS)
	if obj_isa(self.oEnergy, 'MrTimeSeries') $
		then tempE = rebin( reform( self.oEnergy['DATA'], dims[0], 1, 1, dims[3] ), dims ) $
		else tempE = rebin( reform( self.oEnergy['DATA'],       1, 1, 1, dims[3] ), dims )
	
	;Energy Flux
	if self.units eq 'EFLUX' then begin
		
		;Convert to:
		case toUnits of
			'DIFF FLUX': new_flux = flux / energy*1e3
			'PSD':       new_flux = eflux_to_psd * flux / temporary(energy)^2
			'DF':        new_flux = eflux_to_psd * flux / temporary(energy)^2
			'EFLUX':     new_flux = flux
			else: message, 'Cannot convert from "' + self.units + '" to "' + to_units + '".'
		endcase
	
	;Differential flux
	endif else if self.units eq 'DIFF FLUX' then begin
		eflux = flux * energy * 1e-3
		
		;Convert to:
		case toUnits of
			'EFLUX':     new_flux = eflux
			'PSD':       new_flux = eflux_to_psd * eflux / temporary(energy)^2
			'DF':        new_flux = eflux_to_psd * eflux / temporary(energy)^2
			'DIFF FLUX': new_flux = flux
			else: message, 'Cannot convert from "' + self.units + '" to "' + to_units + '".'
		endcase
	
	;Phase space density
	endif else if self.units eq 'PSD' || self.units eq 'DF' then begin
		eflux = flux * energy * 1.e-3
		
		;Convert to:
		case toUnits of
			'EFLUX':     new_flux = flux / eflux_to_psd * temporary(energy)^2
			'DIFF FLUX': new_flux = flux / eflux_to_psd * temporary(energy)*1e3
			'PSD':       new_flux = flux
			'DF':        new_flux = flux
			else: message, 'Cannot convert from "' + self.units + '" to "' + to_units + '".'
		endcase
	
	;Invalid
	endif else begin
		message, 'Invalid FROM_UNIT: "' + self.units + '".'
	endelse
	
	;Set properties
	self.units = toUnits
	case toUnits of
		'EFLUX':     self -> SetAttrValue, 'UNITS', 'keV / cm^2 / s / sr / keV'
		'ENERGY':    self -> SetAttrValue, 'UNITS', 'eV'
		'DIFF FLUX': self -> SetAttrValue, 'UNITS', '# / cm^2 / s / sr / keV'
		'PSD':       self -> SetAttrValue, 'UNITS', 's^2 / km^6'
		else: message, 'Invalid units: "' + to_units + '".'
	endcase
	*self.data = temporary(new_flux)
end


;+
;   Reduce the 3D distribution function to a 1D distribution in polar angle.
;
; :Keywords:
;       E_RANGE:        in, optional, type=fltarr(2), default=[min, max]
;                       The range in energy, in electron volts (eV) over which to average.
;       NTHETA_BINS:    in, optional, type=integer
;                       Number of polar angle bins in the reduced distribution. The
;                           default is to use the same bins and the original distribution.
;       PHI_RANGE:      in, optional, type=fltarr(2), default=[0.0\, 360.0]
;                       The range in azimuthal angle (degrees) over which to average.
;
; :Returns:
;       OTHETASPEC:     out, required, type=MrTimeSeries
;                       A 1D distribution in time, averaged over energy and azimuth.
;-
function MrDist4D::Spec_Theta, $
CACHE=cache, $
E_RANGE=e_range, $
NAME=name, $
NTHETA_BINS=nTheta_bins, $
PHI_RANGE=phi_range
	compile_opt idl2
	
	catch, the_error
	if the_error ne 0 then begin
		catch, /CANCEL
		MrPrintF, 'LogErr'
		if n_elements(oDist3D)    gt 0 then obj_destroy, oDist3D
		if n_elements(oThetaSpec) gt 0 then obj_destroy, oThetaSpec
		if n_elements(oThetaBins) gt 0 then obj_destroy, oThetaBins
		return, !Null
	endif
	
	;Defaults
	tf_cache = keyword_set(cache)
	if n_elements(name) eq 0 then name = self.name + '_ThetaSpec'

	;Allocate memory
	dims      = size(self, /DIMENSIONS)
	nTime     = dims[0]
	nPhi      = dims[1]
	nTheta    = dims[2]
	nEnergy   = dims[3]
	ThetaSpec = fltarr( nTime, nTheta )
	
	;Make theta the polar angle
	if self.orientation ne 1 then begin
		self -> Grid, 1
		if n_elements(self.oPar_FAC) gt 0 then self -> Grid_FAC, 1
	endif
	
	;Step over each time
	for i = 0, nTime - 1 do begin
		oDist3D = self -> GetDist3D(i)
		
		;Reduce the distribution
		ThetaSpec[i,*] = oDist3D -> ThetaSpec(theta_bins, $
		                                      E_RANGE     = e_range, $
		                                      NTHETA_BINS = nTheta_bins, $
		                                      PHI_RANGE   = phi_range )

		;Destroy the object
		obj_destroy, oDist3D
	endfor
	
	;Theta-time spectrogram
	oThetaSpec = MrTimeSeries( self.oTime, thetaSpec, $
	                           CACHE = tf_cache, $
	                           NAME  = name, $
	                           /NO_COPY )
	
	;Abscissa
	binName    = name + '_ThetaBins'
	oThetaBins = size(theta_bins, /N_DIMENSIONS) eq 2 $
	                 ? MrTimeSeries( self.oTime, theta_bins, NAME=binName, /NO_COPY ) $
	                 : MrVariable( theta_bins, NAME=binName, /NO_COPY )
	
	;Theta attributes
	oThetaBins -> AddAttr, 'UNITS', 'degrees'
	oThetaBins -> AddAttr, 'TITLE', 'Polar Angle'

	;Sepctrogram attributes
	oThetaSpec -> AddAttr, 'DEPEND_1', oThetaBins
	oThetaSpec -> AddAttr, 'SCALE',    1B
	oThetaSpec -> AddAttr, 'LOG',      1B
	oThetaSpec -> AddAttr, 'UNITS',    self['UNITS']
	
	return, oThetaSpec
end


;+
;   Reduce the 3D distribution function to a 1D distribution in azimuth angle.
;
; :Keywords:
;       E_RANGE:        in, optional, type=fltarr(2), default=[min, max]
;                       The range in energy, in electron volts (eV) over which to average.
;       NPHI_BINS:      in, optional, type=integer
;                       Number of polar angle bins in the reduced distribution. The
;                           default is to use the same bins and the original distribution.
;       THETA_RANGE:    in, optional, type=fltarr(2), default=[0.0\, 180.0]
;                       The range in polar angle (degrees) over which to average.
;
; :Returns:
;       OPHISPEC:       out, required, type=MrTimeSeries
;                       A 1D distribution in time, averaged over energy and polar angle.
;-
function MrDist4D::Spec_Phi, $
CACHE=cache, $
E_RANGE=E_range, $
NAME=name, $
NPHI_BINS=nPhi_bins, $
THETA_RANGE=theta_range
	compile_opt idl2
	
	catch, the_error
	if the_error ne 0 then begin
		catch, /CANCEL
		MrPrintF, 'LogErr'
		if n_elements(oDist3D)  gt 0 then obj_destroy, oDist3D
		if n_elements(oPhiSpec) gt 0 then obj_destroy, oPhiSpec
		if n_elements(oPhiBins) gt 0 then obj_destroy, oPhiBins
		return, !Null
	endif
	
	;Defaults
	tf_cache = keyword_set(cache)
	if n_elements(name) eq 0 then name = self.name + '_PhiSpec'

	;Allocate memory
	dims      = size(self, /DIMENSIONS)
	nTime     = dims[0]
	nPhi      = dims[1]
	nTheta    = dims[2]
	nEnergy   = dims[3]
	phiSpec = fltarr( nTime, nPhi )
	
	;Make phi the polar angle
	if self.orientation ne 1 then begin
		self -> Grid, 1
		if n_elements(self.oPar_FAC) gt 0 then self -> Grid_FAC, 1
	endif
	
	;Step over each time
	for i = 0, nTime - 1 do begin
		oDist3D = self -> GetDist3D(i)
		
		;Reduce the distribution
		phiSpec[i,*] = oDist3D -> PhiSpec( phi_bins, $
		                                   E_RANGE     = e_range, $
		                                   NPHI_BINS   = nPhi_bins, $
		                                   THETA_RANGE = theta_range )

		;Destroy the object
		obj_destroy, oDist3D
	endfor
	
	;Phi-time spectrogram
	oPhiSpec = MrTimeSeries( self.oTime, phiSpec, $
	                         CACHE = tf_cache, $
	                         NAME  = name, $
	                         /NO_COPY )
	
	;Abscissa
	binName  = name + '_PhiBins'
	oPhiBins = size(phi_bins, /N_DIMENSIONS) eq 2 $
	                 ? MrTimeSeries( self.oTime, phi_bins, NAME=binName, /NO_COPY ) $
	                 : MrVariable( phi_bins, NAME=binName, /NO_COPY )
	
	;Phi attributes
	oPhiBins -> AddAttr, 'UNITS', 'degrees'
	oPhiBins -> AddAttr, 'TITLE', 'Azimuth Angle'

	;Sepctrogram attributes
	oPhiSpec -> AddAttr, 'DEPEND_1', oPhiBins
	oPhiSpec -> AddAttr, 'SCALE',    1B
	oPhiSpec -> AddAttr, 'LOG',      1B
	oPhiSpec -> AddAttr, 'UNITS',    self['UNITS']
	
	return, oPhiSpec
end


;+
;   Reduce the 3D distribution function to a 1D distribution in azimuth angle.
;
; :Keywords:
;       NE_BINS:        in, optional, type=integer
;                       Number of energy bins in the reduced distribution. The default
;                           is to use the same bins and the original distribution.
;       PHI_RANGE:      in, optional, type=fltarr(2), default=[0.0\, 360.0]
;                       The range in azimuthal angle (degrees) over which to average.
;       THETA_RANGE:    in, optional, type=fltarr(2), default=[0.0\, 180.0]
;                       The range in polar angle (degrees) over which to average.
;
; :Returns:
;       OESPEC:         out, required, type=MrTimeSeries
;                       A 1D distribution in time, averaged over polar and azimuth angle.
;-
function MrDist4D::Spec_E, $
CACHE=cache, $
PHI_RANGE=phi_range, $
NAME=name, $
NE_BINS=nE_bins, $
THETA_RANGE=theta_range
	compile_opt idl2
	
	catch, the_error
	if the_error ne 0 then begin
		catch, /CANCEL
		MrPrintF, 'LogErr'
		if n_elements(oDist3D) gt 0 then obj_destroy, oDist3D
		if n_elements(oESpec)  gt 0 then obj_destroy, oESpec
		if n_elements(oEBins)  gt 0 then obj_destroy, oEBins
		return, !Null
	endif
	
	;Defaults
	tf_cache = keyword_set(cache)
	if n_elements(name) eq 0 then name = self.name + '_ESpec'

	;Allocate memory
	dims      = size(self, /DIMENSIONS)
	nTime     = dims[0]
	nPhi      = dims[1]
	nTheta    = dims[2]
	nEnergy   = dims[3]
	ESpec = fltarr( nTime, nEnergy )
	
	;Make phi the polar angle
	if self.orientation ne 1 then begin
		self -> Grid, 1
		if n_elements(self.oPar_FAC) gt 0 then self -> Grid_FAC, 1
	endif

	;Step over each time
	for i = 0, nTime - 1 do begin
		oDist3D = self -> GetDist3D(i)
		
		;Reduce the distribution
		ESpec[i,*] = oDist3D -> ESpec( e_bins, $
		                               NE_BINS     = nE_bins, $
		                               PHI_RANGE   = phi_range, $
		                               THETA_RANGE = theta_range )

		;Destroy the object
		obj_destroy, oDist3D
	endfor
	
	;Energy-time spectrogram
	oESpec = MrTimeSeries( self.oTime, ESpec, $
	                           CACHE = tf_cache, $
	                           NAME  = name, $
	                           /NO_COPY )
	
	;Ordinate
	binName = name + '_EBins'
	oEBins  = size(e_bins, /N_DIMENSIONS) eq 2 $
	               ? MrTimeSeries( self.oTime, e_bins, NAME=binName, /NO_COPY ) $
	               : MrVariable( e_bins, NAME=binName, /NO_COPY )
	
	;Phi attributes
	oEBins -> AddAttr, 'UNITS', self.oEnergy['UNITS']
	oEBins -> AddAttr, 'TITLE', 'Energy'

	;Sepctrogram attributes
	oESpec -> AddAttr, 'DEPEND_1', oEBins
	oESpec -> AddAttr, 'SCALE',    1B
	oESpec -> AddAttr, 'LOG',      1B
	oESpec -> AddAttr, 'UNITS',    self['UNITS']
	
	return, oESpec
end


;+
;   Reduce the 3D distribution function to a 1D distribution in pitch angle.
;   One distribution will be created for each range of energy channels given. The range
;   of polar angle can be constrained.
;
;   It is expected that the 3D distribution function and already been loaded and its
;   coordinates have been rotated to field-aligned coordinates (e.g. via
;   MrDist_FPI_Load_Dist3D.pro).
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
;       NO_LOAD:            in, optional, type=boolean, default=0
;                           If set, data will not be (re-)read from its CDF file and
;                               loaded into the variable cache.
;       PHI_RANGE:          in, optional, type=fltarr(2), default=[0.0\, 360.0]
;                           Range in azimuth angle over which to average the data.
;       UNITS:              in, optional, type=string, default='E FLUX'
;                           Units of the output distribution. Options are::
;                               'ENERGY'      - eV
;                               'EFLUX'       - eV / cm^2 / s / sr / eV    or    keV / cm^2 / s / sr / keV
;                               'DIFF FLUX'   - # / cm^2 / s / sr / keV
;                               'PSD'         - s^2 / km^6
;                               'DF'          - s^2 / km^6
;       SHOW_EBINS:         in, optional, type=boolean, default=0
;                           If set, the energy bins and their respective channels will
;                               be printed. All other keywords are ignored and no data
;                               is produced.
;       VARNAMES:           out, optional, type=string/strarr
;                           Names of the variables created and cached.
;-
function MrDist4D::Dist2D_ThetaE, distFn, phi, theta, $
NE_BINS=ne_bins, $
NTHETA_BINS=npa_bins, $
PHI_RANGE=gyro_range
	compile_opt idl2
	on_error, 2

	;Allocate memory
	dims    = size(self, /DIMENSIONS)
	nTime   = dims[0]
	nTheta  = dims[1]
	nPhi    = dims[2]
	nEnergy = dims[3]
	ThetaE  = fltarr( nTime, nTheta, nEnergy )
	
	;Proper orientation
	if self.orientation ne 2 then begin
		self -> Grid, 2
		if n_elements(self.oPar_FAC) gt 0 then self -> Grid_FAC, 2
	endif

	;Create an empty distribution
	dist3D = MrDist3D()
	
	;Step over each time
	for i = 0, n_elements(nTime) - 1 do begin
		;Update the distribution
		dist3D -> SetData, reform( (*self.data)[i,*,*,*] ), $
		                   reform( self.oPhi[i,*,*] ), $
		                   reform( self.oTheta[i,*,*] ), $
		                   reform( self.oEnergy[i,*,*] )
		
		;Reduce the distribution
		ThetaE[i,*,*] = dist3D -> ThetaE()
	endfor
	
	;Destroy the 3D distribution
	obj_destroy, dist3D
	
	;Return the 2D distribution
	return, ThetaE
end


;+
;   Display the energy bins
;-
pro MrDist4D::EBins
	compile_opt idl2
	on_error, 2

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
end


;+
;   Transform a spherical coordinate grid into a cartesian coordinate grid.
;
; :Keywords:
;       MASS:               out, optional, type=float
;                           Mass (kg) of the species represented in the distribution.
;       UNITS:              out, optional, type=string
;                           Units of the distribution function.
;       _REF_EXTRA:         out, optional, type=any
;                           Any keyword accepted by MrTimeSeries::GetProperty
;-
function MrDist4D::GetDist3D, idx
	compile_opt idl2
	on_error, 2

;-----------------------------------------------------
; Type of Distribution \\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\
;-----------------------------------------------------
	
	;FAC?
	if obj_valid(self.oPhi_FAC) && n_elements(self.oPhi_FAC) gt 0 then begin
		oPhi   = self.oPhi_FAC
		oTheta = self.oTheta_FAC
	endif else begin
		oPhi   = self.oPhi_Grid
		oTheta = self.oTheta_Grid
	endelse
	
	;Are the dependent variables time-dependent
	tf_angleTS  = obj_isa(oPhi,         'MrTimeSeries')
	tf_energyTS = obj_isa(self.oEnergy, 'MrTimeSeries')

;-----------------------------------------------------
; Create Distribution \\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\
;-----------------------------------------------------
	
	case 1 of
		;BOTH
		tf_angleTS && tf_energyTS: begin
			dist3D = MrDist3D( reform( (*self.data)[idx,*,*,*] ), $
			                   reform( oPhi[idx,*,*] ), $
			                   reform( oTheta[idx,*,*] ), $
			                   reform( self.oEnergy[idx,*,*] ), $
			                   /DEGREES, $
			                   MASS  = self.mass, $
			                   UNITS = self.units )
		endcase
		
		;ANGLES
		tf_angleTS: begin
			dist3D = MrDist3D( reform( (*self.data)[idx,*,*,*] ), $
			                   reform( oPhi[idx,*,*] ), $
			                   reform( oTheta[idx,*,*] ), $
			                   self.oEnergy['DATA'], $
			                   /DEGREES, $
			                   MASS  = self.mass, $
			                   UNITS = self.units )
		endcase
		
		;ENERGY
		tf_angleTS: begin
			dist3D = MrDist3D( reform( (*self.data)[idx,*,*,*] ), oPhi['DATA'], oTheta['DATA'], $
			                   reform( self.oEnergy[idx,*] ), $
			                   /DEGREES, $
			                   MASS  = self.mass, $
			                   UNITS = self.units )
		endcase
		
		;NEITHER
		else: begin
			dist3D = MrDist3D( reform( (*self.data)[idx,*,*,*] ), oPhi['DATA'], oTheta['DATA'], self.oEnergy['DATA'], $
			                   /DEGREES, $
			                   MASS  = self.mass, $
			                   UNITS = self.units )
		endcase
	endcase

;-----------------------------------------------------
; Done \\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\
;-----------------------------------------------------
	return, Dist3D
end


;+
;   Transform a spherical coordinate grid into a cartesian coordinate grid.
;
; :Keywords:
;       MASS:               out, optional, type=float
;                           Mass (kg) of the species represented in the distribution.
;       UNITS:              out, optional, type=string
;                           Units of the distribution function.
;       _REF_EXTRA:         out, optional, type=any
;                           Any keyword accepted by MrTimeSeries::GetProperty
;-
pro MrDist4D::GetProperty, $
MASS=mass, $
UNITS=units, $
_REF_EXTRA=extra
	compile_opt idl2
	on_error, 2
	
	if arg_present(mass)  gt 0 then mass  = self.mass
	if arg_present(units) gt 0 then units = self.units
	
	;Superclasses
	if n_elements(extra) gt 0 then self -> MrTimeSeries::GetProperty, _STRICT_EXTRA=extra
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
pro MrDist4D::Grid, orientation
	compile_opt idl2
	on_error, 2
	
	;Default to standard spherical orientation
	if n_elements(orientation) eq 0 then orientation = 1
	
	;Create a cartesian grid
	self -> Grid_MakeCart, oX1, oX2, oX3
	
	;Convert to spherical coordinates
	self -> Grid_cart2sphere, temporary(oX1), temporary(oX2), temporary(oX3), oX1, oX2, $
	                          ORIENTATION = orientation

	;Save object properties
	self.oTheta_Grid = oX2
	self.oPhi_Grid   = oX1
	self.orientation = orientation
end


;+
;   Create a spherical coordinate grid in field-aligned coordinates.
;
; :Private:
;-
pro MrDist4D::Grid_FAC, orientation
	compile_opt idl2
	on_error, 2
	
	;Default to standard spherical orientation
	if n_elements(orientation) eq 0 then orientation = 1

	;Create a cartesian grid
	self -> Grid_MakeCart, oX, oY, oZ

	;Field-Aligned Coordinate System
	oT_FAC = self -> T_FAC()

	;Rotate the cartesian grid to field-aligned coordinates
	self -> Grid_Cart2FAC, temporary(oT_FAC), -temporary(oX), -temporary(oY), -temporary(oZ), $
	                       oX1, oX2, oX3

	;Convert to spherical coordinates
	self -> Grid_cart2sphere, temporary(oX1), temporary(oX2), temporary(oX3), oX1, oX2, $
	                          ORIENTATION = orientation

	;Set properties
	self.oTheta_FAC  = oX2
	self.oPhi_FAC    = oX1
	self.orientation = orientation
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
pro MrDist4D::Grid_Cart2FAC, oT, oX, oY, oZ, oN, oP, oQ
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
		oN = MrTimeSeries( self.oTime, n, /NO_COPY )
		oP = MrTimeSeries( self.oTime, p, /NO_COPY )
		oQ = MrTimeSeries( self.oTime, q, /NO_COPY )

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
pro MrDist4D::Grid_Cart2Sphere, oX, oY, oZ, oPhi, oTheta, $
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
	iPhi       = where(phi lt 0, nPhi)
	phi[iPhi] += 2.0*!pi
	
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
pro MrDist4D::Grid_MakeCart, oXGrid, oYGrid, oZGrid, $
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
	oPhi   = self.oPhi
	oTheta = self.oTheta
	
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
;   Load the distribution from source.
;-
pro MrDist4D::Load
	compile_opt idl2
	on_error, 2
	
	message, 'MrDist4D::Load must be over-ridden by a subclass.'
end


;+
;   Load the distribution from source.
;-
pro MrDist4D::Load_FAC
	compile_opt idl2
	on_error, 2
	
	message, 'MrDist4D::Load_FAC must be over-ridden by a subclass.'
end


;+
;   Transform a spherical coordinate grid into a cartesian coordinate grid.
;
; :Returns:
;       T:              The transformation matrix to the field-aligned coordinate system.
;-
function MrDist4D::T_FAC
	compile_opt idl2
	on_error, 2

;-----------------------------------------------------
; Electric Field \\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\
;-----------------------------------------------------
	if self.type_fac eq 'EXB' then begin
		;Parallel: B
		;   - z_hat = self.oPar_FAC
		
		;Perp-1: ExB
		oX_hat = self.oPerp_FAC -> Cross(self.oPar_FAC)
		oX_hat = oX_hat -> Normalize()
		
		;Perp-2: Bx(ExB)
		oY_hat = self.oPar_FAC -> Cross(oX_hat)

;-----------------------------------------------------
; Velocity \\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\
;-----------------------------------------------------
	endif else if fac_type eq 'VXB' then begin
		;Parallel: B
		;   - oZ_hat = self.oPar_FAC
		
		;Perp-2: bxv = -vxb = e
		oY_hat = self.oPar_FAC -> Cross(self.oPerp_FAC)
		oY_hat = oY_hat -> Normalize()
		
		;Perp-1: (bxv)xb = exb
		oX_hat = oY_hat -> Cross(oPar_FAC)

;-----------------------------------------------------
; Radial/Azimuthal \\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\
;-----------------------------------------------------
	endif else if fac_type eq 'RADAZ' then begin
		;Parallel: B
		;   - oZ_hat = self.oPar_FAC
		
		;Perp2: bxr (azimuth)
		oY_hat = self.oPar_FAC -> Cross(self.oPerp_FAC)
		oY_hat = oY_hat -> Normalize()
		
		;Perp1: bx(bxr) (radial)
		oX_hat = oY_hat -> Cross(self.oPar_FAC)

;-----------------------------------------------------
; X-Axis \\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\
;-----------------------------------------------------
	endif else if fac_type eq '' gt 0 then  begin
		;Parallel to B
		;   - oZ_hat = oB_hat
		
		;Perp2: XxB
		oY_hat = self.oPar_FAC -> Cross([1,0,0])
		oY_hat = oY_hat -> Normalize()
		
		;Perp1: Yx(XxB)
		oX_hat = oY_hat -> Cross(self.oPar_FAC)

;-----------------------------------------------------
; Unknown \\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\
;-----------------------------------------------------
	endif else begin
		message, 'FAC TYPE not recognized: "' + type + '".'
	endelse

;-----------------------------------------------------
; Form Rotation Matrix \\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\
;-----------------------------------------------------

	;Time dimension first
	;   - [time, component, axis]
	T = MrMatrixTS( self.oTime, [ [[ oX_hat['DATA'] ]], $
	                              [[ oY_hat['DATA'] ]], $
	                              [[ self.oPar_FAC['DATA'] ]] ])

	;Return the matrix
	return, T
end


;+
;   Transform a spherical coordinate grid into a cartesian coordinate grid.
;
; :Keywords:
;       MASS:               in, optional, type=float
;                           Mass (kg) of the species represented in the distribution.
;       SPECIES:            in, optional, type=string
;                           Species of particle represented in the distribution. Options
;                               are: { 'e' | 'p' | 'H' | 'He' | 'O' }
;       _REF_EXTRA:         in, optional, type=any
;                           Any keyword accepted by MrTimeSeries::SetProperty
;-
pro MrDist4D::SetProperty, $
MASS=mass, $
SPECIES=species
	compile_opt idl2
	on_error, 2
	
	if n_elements(mass)    gt 0 then self.mass = mass
	if n_elements(species) gt 0 then self.mass = MrConstants('m_' + species)
end


;+
;   Set the array.
;
;   CALLING SEQUENCE:
;       oTS -> SetData, data
;       oTS -> SetData, time, data
;
; :Keywords:
;       TIME:           in, required, type=NxM array
;                       Name or reference of a MrTimeVar object, or an array
;                           of time stamps. If a name is provided, the assiciated
;                           variable must exist in the variable cache.
;       DATA:           in, required, type=NxM array
;                       Name or reference of a MrVariable object, or the dependent
;                           variable data. If a name is given, the associated variable
;                           must exist in the variable cache.
;       THETA:          in, optional, type=Nx1 or NxM array
;                       Polar coordinates of the distribution pixels. Can be the name or
;                           reference of a MrVariable object, or the variable data.
;                           If the variable is a MrTimeSeries object, its time property
;                           must be the same as that of the implicit distribution.
;                           If data has two dimensions, one must be time and the other
;                           must be the same size as the second dimension of the
;                           distribution.
;       PHI:            in, optional, type=Nx1 or NxM array
;                       Azimuthal coordinates of the distribution pixels. Can be the name
;                           or reference of a MrVariable object, or the variable data.
;                           If the variable is a MrTimeSeries object, its time property
;                           must be the same as that of the implicit distribution.
;                           If data has two dimensions, one must be time and the other
;                           must be the same size as the second dimension of the
;                           distribution.
;       ENERGY:         in, optional, type=Nx1 or NxM array
;                       Energy coordinates of the distribution pixels. Can be the name
;                           or reference of a MrVariable object, or the variable data.
;                           If the variable is a MrTimeSeries object, its time property
;                           must be the same as that of the implicit distribution.
;                           If data has two dimensions, one must be time and the other
;                           must be the same size as the fourth dimension of the
;                           distribution.
;
; :Keywords:
;       DIMENSION:      in, optional, type=integer
;                       The time-dependent dimension of `DATA` (1-based). If not
;                           provided, the dimension of `DATA` that is equal in size to
;                           `TIME` is chosen as the default. If zero or multiple
;                           dimensions match in this way, an error will occur.
;       T_TYPE:         in, optional, type=integer
;                       If `TIME` is an array of time stamps, use this keyword to indicate
;                           the format or time-basis. See MrTimeVar for more details.
;       T_NAME:         in, optional, type=integer
;                       Name to be given to the MrTimeVar object. Ignored unless `TIME`
;                           is an array of time stamps.
;       NO_COPY:        in, optional, type=boolean, default=0
;                       If set `DATA` will be copied directly into the object
;                           and will be left undefined (a MrTimeSeries object will not
;                           be destroyed, but its array will be empty).
;-
pro MrDist4D::SetData, time, data, phi, theta, energy, $
DEGREES=degrees, $
DIMENSION=dimension, $
NO_COPY=no_copy, $
T_NAME=t_name, $
T_TYPE=t_type, $
RADIANS=radians, $
UNITS=units
	compile_opt idl2
	
	catch, the_error
	if the_error ne 0 then begin
		catch, /CANCEL
		MrPrintF, 'LogErr'
		
		;Reset data
		ptr_free, self.data
		self.data    = pData
		self.oTime   = oTime
		self.oTheta  = oTheta
		self.oPhi    = oPhi
		self.oEnergy = oEnergy
		
		return
	endif
	
	;Keep the old data
	oTime   = self.oTime
	pDAta   = self.data
	oPhi    = self.oPhi
	oTheta  = self.oTheta
	oEnergy = self.oEnergy

;-----------------------------------------------------
; Check Distribution \\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\
;-----------------------------------------------------

	;Use the superclass
	self -> MrTimeSeries::SetData, time, data, $
	                               DIMENSION = dimension, $
	                               NO_COPY   = no_copy, $
	                               T_TYPE    = t_type, $
	                               T_NAME    = t_name

	;Check the results
	;   - Make sure it is NxMxLxK (4 dimensions of any size)
	sz = size(self)
	if sz[0] ne 4 $
		then message, 'Invalid dimensions: Data must be an 4D.' $
		else ptr_free, pData
	
	;Units
	if n_elements(units) eq 0 then begin
		if self -> HasAttr('UNITS') then begin
			units = self['UNITS']
			if stregex(units, '(psd|phase|space|density)', /BOOLEAN, /FOLD_CASE) then begin
				units = 'PSD'
			endif else if stregex(units, '(eflux|energy flux)', /BOOLEAN, /FOLD_CASE) then begin
				units = 'EFLUX'
			endif else if stregex(units, '(diff flux)', /BOOLEAN, /FOLD_CASE) then begin
				units = 'DIFF FLUX'
			endif else begin
				units = 'EFLUX'
			endelse
		endif else begin
			units = 'PSD'
		endelse
	endif
	
;-----------------------------------------------------
; Set Dependents \\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\
;-----------------------------------------------------
	;Set remaining data
	self.units = units
	if n_elements(theta)  gt 0 then self -> SetTheta, theta, DEGREES=degrees, RADIANS=radians
	if n_elements(phi)    gt 0 then self -> SetPhi, phi, DEGREES=degrees, RADIANS=radians
	if n_elements(energy) gt 0 then self -> SetEnergy, energy
	
	;Update coordinate grid
	self -> Grid
end


;+
;   Set the field-aligned coordinate system.
;
;   CALLING SEQUENCE:
;       oDist3D -> SetFAC, par
;       oDist3D -> SetFAC, par, perp, fac_type
;
; :Keywords:
;       PAR:            in, required, type=Nx3 array
;                       Variable name or MrVectorTS object associated with a vector that
;                           defines the parallel direction. If a name is given, the
;                           variable must be in the variable cache.
;       PERP:           in, optional, type=Nx3 array
;                       Variable name or MrVectorTS object associated with a vector that
;                           defines one perpendicular direction. If a name is given, the
;                           variable must be in the variable cache.
;       TYPE_FAC:       in, optional, type=string
;                       Type of field-aligned coordinates to make. Options include
;                           {'ExB' | 'VxB' | 'RadAz' | ''}. Ignored if `PERP` is not given.
;-
pro MrDist4D::SetFAC, par, perp, type_fac
	compile_opt idl2
	
	catch, the_error
	if the_error ne 0 then begin
		catch, /CANCEL
		MrPrintF, 'LogErr'
		
		;Reset data
		self.oPar_FAC  = oPar_FAC
		self.oPerp_FAC = oPerp_FAC
		self.type_fac  = old_fac
		
		return
	endif
	
	;Keep the old data
	oPar_FAC  = self.oPar_FAC
	oPerp_FAC = self.oPerp_FAC
	old_fac   = self.type_fac
	
	;Set FAC type
	self.type_fac = n_elements(type_fac) eq 0 ? '' : strupcase(type_fac)
	
	;
	; Steps:
	;   1. Obtain par object
	;   2. If par time tags differ from implicit time, interpolate
	;   3. Obtain perp object, if given
	;   4. If perp time tags differ from implicit time, interpolate
	;   5. Create FAC Grid
	;

;-----------------------------------------------------
; PAR \\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\
;-----------------------------------------------------

	;OBJECT
	if size(par, /TNAME) eq 'OBJREF' then begin
		if obj_isa(par, 'MrVectorTS') $
			then oPar = par $
			else message, 'PAR must be a MrVectorTS object.'
	
	;NAME
	endif else if size(par, /TNAME) eq 'STRING' then begin
		;Cached?
		if MrVar_IsCached(par) $
			then oPar = MrVar_Get(par) $
			else message, 'PAR must be the name of a cached variable.'
	
	;OTHER
	endif else begin
		message, 'PAR must be a variable name or a MrVectorTS object.'
	endelse
	
	;Interpolate to same time tags
	!Null = oPar -> GetData(oT_par, /TVAR)
	if self.oTime -> IsIdentical(oT_par) $
		then oPar_FAC = oPar $
		else oPar_FAC = oPar -> Interpol(self.oTime)
	
	;Unit vector (direction, not magnitude)
	self.oPar_FAC = oPar_FAC -> Normalize()

;-----------------------------------------------------
; PERP \\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\
;-----------------------------------------------------
	if n_elements(perp) gt 0 then begin

		;OBJECT
		if size(perp, /TNAME) eq 'OBJREF' then begin
			if obj_isa(perp, 'MrVectorTS') $
				then oPerp = perp $
				else message, 'PERP must be a MrVectorTS object.'
	
		;NAME
		endif else if size(perp, /TNAME) eq 'STRING' then begin
			;Cached?
			if MrVar_IsCached(perp) $
				then oPerp = MrVar_Get(perp) $
				else message, 'PERP must be the name of a cached variable.'
	
		;OTHER
		endif else begin
			message, 'PERP must be a variable name or a MrVectorTS object.'
		endelse
		
		;Interpolate to same time tags
		!Null = oPerp -> GetData(oT_perp, /TVAR)
		if self.oTime -> IsIdentical(oT_perp) $
			then oPerp_FAC = oPerp $
			else oPerp_FAC = oPerp -> Interpol(self.oTime)
	endif
	
	;Unit vector (direction, not magnitude)
	self.oPerp_FAC = oPerp_FAC -> Normalize()

;-----------------------------------------------------
; Coordinate Grid \\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\
;-----------------------------------------------------
	self -> Grid_FAC
end


;+
;   Set the theta array.
;
; :Params:
;       THETA:          in, required, type=Nx1 or NxM array
;                       Polar coordinates of the distribution pixels. Can be the name or
;                           reference of a MrVariable object, or the variable data.
;                           If the variable is a MrTimeSeries object, its time property
;                           must be the same as that of the implicit distribution.
;                           If data has two dimensions, one must be time and the other
;                           must be the same size as the second dimension of the
;                           distribution.
;
; :Keywords:
;       DEGREES:        in, optional, type=boolean, default=1
;                       If set to zero, `THETA` has units of radians. Degrees are
;                           assumed. Cannot be used with `RADIANS`.
;       NO_COPY:        in, optional, type=boolean, default=0
;                       If set `THETA` will be copied directly into the object
;                           and will be left undefined (a MrTimeSeries object will not
;                           be destroyed, but its array will be empty).
;       RADIANS:        in, optional, type=boolean, default=1
;                       If set, `THETA` has units of radians. Otherwise, degrees are
;                           assumed. Cannot be used with `DEGREES`.
;-
pro MrDist4D::SetTheta, theta, $
DEGREES=degrees, $
NO_COPY=no_copy, $
RADIANS=radians
	compile_opt idl2
	on_error, 2
	
	;Dimensions
	theDims = size(*self.data, /DIMENSIONS)
	nTimes  = theDims[0]
	nPhi    = theDims[1]
	nTheta  = theDims[2]
	nEnergy = theDims[3]
	
	if n_elements(degrees) gt 0 && n_elements(radians) gt 0 $
		then message, 'DEGREES and RADIANS are mutually exclusive.'
	
	;
	; Steps:
	;   1. Obtain variable object
	;   2. Compare size to distribution function
	;   3. Units
	;   4. Set object property
	;

;-----------------------------------------------------
; Obtain Variable Object \\\\\\\\\\\\\\\\\\\\\\\\\\\\\
;-----------------------------------------------------

	;Use existing THETA
	if n_elements(theta) eq 0 then begin
		if obj_valid(self.oTheta) $
			then oTheta = self.oTheta $
			else message, 'No theta variable exists. Please provide.'
	
	;Check given THETA
	endif else begin
		;OBJECT
		if size(theta, /TNAME) eq 'OBJREF' then begin
			if obj_isa(theta, 'MrVariable') $
				then oTheta = theta $
				else message, 'THETA must be a MrVariable object or subclass.'
		
		;NAME
		endif else if size(theta, /TNAME) eq 'STRING' then begin
			;Cached?
			if MrVar_IsCached(theta) $
				then oTheta = MrVar_Get(theta) $
				else message, 'THETA must be the name of a cached variable.'
		
		;DATA
		endif else begin
			dims  = size(theta, /DIMENSIONS)
			nDims = n_elements(dims)
			
			;Check dimensions
			if nDims eq 1 then begin
				oTheta = MrVariable( theta, NAME=self.name + '_theta'  )
			
			;Time-dependent
			endif else if nDims eq 2 then begin
				;One must be TIME
				if dims[0] eq nTimes && dims[1] eq nTheta $
					then oTheta = MrTimeSeries( self.oTime, theta, NAME=self.name + '_theta' ) $
					else message, 'Invalid dimensions: THETA.'
			
			;Invalid
			endif else begin
				message, 'THETA must have 1 or 2 dimensions.'
			endelse
		endelse
	endelse

;-----------------------------------------------------
; Compare With Distribution \\\\\\\\\\\\\\\\\\\\\\\\\\
;-----------------------------------------------------
	
	;Time objects
	if obj_isa(oTheta, 'MrTimeSeries') then begin
		;Make sure DIST and THETA use the same time object
		!Null = theta -> GetData( thetaTime, /TVAR )
		if thetaTime ne self.oTime $
			then message, 'THETA and DIST have different time objects.'
	endif
	
	;Data Size
	dims  = size(oTheta, /DIMENSIONS)
	nDims = n_elements(dims)
	if nDims eq 2 then begin
		if dims[0] ne nTimes then message, 'Invalid dimensions: THETA.'
		if dims[1] ne nTheta then message, 'Invalid dimensions: THETA.'
	endif else if nDims eq 1 then begin
		if dims[0] ne nTheta then message, 'Invalid dimensions: THETA.'
	endif else begin
		message, 'THETA must have <= 2 dimensions.'
	endelse

;-----------------------------------------------------
; Units \\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\
;-----------------------------------------------------
	if n_elements(tf_degrees) eq 0 then begin
		if oTheta -> HasAttr('UNITS') then begin
			units = oTheta['UNITS']
			case 1 of
				stregex(units, 'deg', /BOOLEAN, /FOLD_CASE): tf_degrees = 1B
				stregex(units, 'rad', /BOOLEAN, /FOLD_CASE): tf_degrees = 0B
				else: begin
					MrPrintF, 'LogWarn', 'Units not recognized "' + units + '". Assuming degrees.'
					tf_degrees = 1B
				endelse
			endcase
		endif else begin
			tf_degrees = n_elements(radians) eq 0 ? keyword_set(degrees) : ~keyword_set(radians)
		endelse
	endif
	
	;Convert to degrees
	if ~tf_degrees then begin
		oTheta -> SetData, oTheta['DATA']*!radeg
		oTheta['UNITS'] = 'degrees'
	endif

;-----------------------------------------------------
; SetProperties \\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\
;-----------------------------------------------------
	self.oTheta = oTheta
end


;+
;   Set the phi array.
;
; :Params:
;       PHI:            in, required, type=Nx1 or NxM array
;                       Azimuthal coordinates of the distribution pixels. Can be the name
;                           or reference of a MrVariable object, or the variable data.
;                           If the variable is a MrTimeSeries object, its time property
;                           must be the same as that of the implicit distribution.
;                           If data has two dimensions, one must be time and the other
;                           must be the same size as the third dimension of the
;                           distribution.
;
; :Keywords:
;       DEGREES:        in, optional, type=boolean, default=1
;                       If set to zero, `THETA` has units of radians. Degrees are
;                           assumed. Cannot be used with `RADIANS`.
;       NO_COPY:        in, optional, type=boolean, default=0
;                       If set `THETA` will be copied directly into the object
;                           and will be left undefined (a MrTimeSeries object will not
;                           be destroyed, but its array will be empty).
;       RADIANS:        in, optional, type=boolean, default=1
;                       If set, `THETA` has units of radians. Otherwise, degrees are
;                           assumed. Cannot be used with `DEGREES`.
;-
pro MrDist4D::SetPhi, phi, $
DEGREES=degrees, $
NO_COPY=no_copy, $
RADIANS=radians
	compile_opt idl2
	on_error, 2
	
	;Dimensions
	theDims = size(*self.data, /DIMENSIONS)
	nTimes  = theDims[0]
	nPhi    = theDims[1]
	nTheta  = theDims[2]
	nEnergy = theDims[3]
	
	if n_elements(degrees) gt 0 && n_elements(radians) gt 0 $
		then message, 'DEGREES and RADIANS are mutually exclusive.'
	if n_elements(degrees) gt 0 then tf_degrees = keyword_set(degrees)
	if n_elements(radians) gt 0 then tf_degrees = ~keyword_set(radians)
	
	;
	; Steps:
	;   1. Obtain variable object
	;   2. Compare size to distribution function
	;   3. Set object property
	;
	
;-----------------------------------------------------
; Obtain Variable Object \\\\\\\\\\\\\\\\\\\\\\\\\\\\\
;-----------------------------------------------------
	
	;Use existing PHI
	if n_elements(phi) eq 0 then begin
		if obj_valid(self.oPhi) $
			then oPhi = self.oPhi $
			else message, 'No phi variable exists. Please provide.'
	
	;Check given PHI
	endif else begin
		;OBJECT
		if size(phi, /TNAME) eq 'OBJREF' then begin
			if obj_isa(phi, 'MrVariable') $
				then oPhi = phi $
				else message, 'PHI must be a MrVariable object or subclass.'
		
		;NAME
		endif else if size(phi, /TNAME) eq 'STRING' then begin
			;Cached?
			if MrVar_IsCached(phi) $
				then oPhi = MrVar_Get(phi) $
				else message, 'PHI must be the name of a cached variable.'
		
		;DATA
		endif else begin
			dims  = size(phi, /DIMENSIONS)
			nDims = n_elements(dims)
			
			;Check dimensions
			if nDims eq 1 then begin
				oPhi = MrVariable( phi, NAME=self.name + '_phi'  )
			
			;Time-dependent
			endif else if nDims eq 2 then begin
				;One must be TIME
				if dims[0] eq nTimes && dims[1] eq nTimes $
					then oPhi = MrTimeSeries( self.oTime, phi, NAME=self.name + '_phi' ) $
					else message, 'Invalid dimensions: PHI.'
			
			;Invalid
			endif else begin
				message, 'PHI must have 1 or 2 dimensions.'
			endelse
		endelse
	endelse

;-----------------------------------------------------
; Compare With Distribution \\\\\\\\\\\\\\\\\\\\\\\\\\
;-----------------------------------------------------
	
	;Time objects
	if obj_isa(oPhi, 'MrTimeSeries') then begin
		;Make sure DIST and PHI use the same time object
		!Null = phi -> GetData( phiTime, /TVAR )
		if phiTime ne self.oTime $
			then message, 'PHI and DIST have different time objects.'
	endif
	
	;Data Size
	dims  = size(oPhi, /DIMENSIONS)
	nDims = n_elements(dims)
	if nDims eq 2 then begin
		if dims[0] ne nTimes then message, 'Invalid dimensions: PHI.'
		if dims[1] ne nPhi then message, 'Invalid dimensions: PHI.'
	endif else if nDims eq 1 then begin
		if dims[0] ne nPhi then message, 'Invalid dimensions: PHI.'
	endif else begin
		message, 'PHI must have <= 2 dimensions.'
	endelse

;-----------------------------------------------------
; Units \\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\
;-----------------------------------------------------
	if n_elements(tf_degrees) eq 0 then begin
		if oPhi -> HasAttr('UNITS') then begin
			units = oPhi['UNITS']
			case 1 of
				stregex(units, 'deg', /BOOLEAN, /FOLD_CASE): tf_degrees = 1B
				stregex(units, 'rad', /BOOLEAN, /FOLD_CASE): tf_degrees = 0B
				else: begin
					MrPrintF, 'LogWarn', 'Units not recognized "' + units + '". Assuming degrees.'
					tf_degrees = 1B
				endelse
			endcase
		endif else begin
			tf_degrees = n_elements(radians) eq 0 ? keyword_set(degrees) : ~keyword_set(radians)
		endelse
	endif
	
	;Convert to degrees
	if ~tf_degrees then begin
		oPhi -> SetData, oPhi['DATA']*!radeg
		oPhi['UNITS'] = 'degrees'
	endif

;-----------------------------------------------------
; SetProperties \\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\
;-----------------------------------------------------
	self.oPhi = oPhi
end


;+
;   Set the energy array.
;
; :Params:
;       ENERGY:         in, required, type=Nx1 or NxM array
;                       Energy coordinates of the distribution pixels. Can be the name
;                           or reference of a MrVariable object, or the variable data.
;                           If the variable is a MrTimeSeries object, its time property
;                           must be the same as that of the implicit distribution.
;                           If data has two dimensions, one must be time and the other
;                           must be the same size as the fourth dimension of the
;                           distribution.
;
; :Keywords:
;       NO_COPY:        in, optional, type=boolean, default=0
;                       If set `PHI` will be copied directly into the object
;                           and will be left undefined (a MrTimeSeries object will not
;                           be destroyed, but its array will be empty).
;-
pro MrDist4D::SetEnergy, energy, $
NO_COPY=no_copy
	compile_opt idl2
	on_error, 2
	
	;Dimensions
	theDims = size(*self.data, /DIMENSIONS)
	nTimes  = theDims[0]
	nPhi    = theDims[1]
	nTheta  = theDims[2]
	nEnergy = theDims[3]
	
	;
	; Steps:
	;   1. Obtain variable object
	;   2. Compare size to distribution function
	;   3. Set object property
	;
	

;-----------------------------------------------------
; Energy \\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\
;-----------------------------------------------------
	;
	; I. Obtain variable object
	;
	
	;Use existing ENERGY
	if n_elements(energy) eq 0 then begin
		if obj_valid(self.oEnergy) $
			then oEnergy = self.oEnergy $
			else message, 'No energy variable exists. Please provide.'
	
	;Check given ENERGY
	endif else begin
		;OBJECT
		if size(energy, /TNAME) eq 'OBJREF' then begin
			if obj_isa(energy, 'MrVariable') $
				then oEnergy = energy $
				else message, 'ENERGY must be a MrVariable object or subclass.'
		
		;NAME
		endif else if size(energy, /TNAME) eq 'STRING' then begin
			;Cached?
			if MrVar_IsCached(energy) $
				then oEnergy = MrVar_Get(energy) $
				else message, 'ENERGY must be the name of a cached variable.'
		
		;DATA
		endif else begin
			szTheta = size(energy, /DIMENSIONS)
			nDims   = n_elements(szTheta)
			
			;Check dimensions
			if nDims eq 1 then begin
				oEnergy = MrVariable( energy, NAME=self.name + '_energy'  )
			
			;Time-dependent
			endif else if nDims eq 2 then begin
				;One must be TIME
				if szTheta[0] eq nTimes || szTheta[1] eq nTimes $
					then oEnergy = MrTimeSeries( self.oTime, energy, NAME=self.name + '_energy' ) $
					else message, 'Invalid dimensions: ENERGY.'
			
			;Invalid
			endif else begin
				message, 'ENERGY must have 1 or 2 dimensions.'
			endelse
		endelse
	endelse
		
	;
	; II. Compare size with distribution function
	;
	
	;Time objects
	if obj_isa(oEnergy, 'MrTimeSeries') then begin
		;Make sure DIST and ENERGY use the same time object
		!Null = energy -> GetData( energyTime, /TVAR )
		if energyTime ne self.oTime $
			then message, 'ENERGY and DIST have different time objects.'
	endif
	
	;Data Size
	dims  = size(oEnergy, /DIMENSIONS)
	nDims = n_elements(dims)
	if nDims eq 2 then begin
		if dims[0] ne nTimes then message, 'Invalid dimensions: ENERGY.'
		if dims[1] ne nEnergy then message, 'Invalid dimensions: ENERGY.'
	endif else if nDims eq 1 then begin
		if dims[0] ne nEnergy then message, 'Invalid dimensions: ENERGY.'
	endif else begin
		message, 'ENERGY must have <= 2 dimensions.'
	endelse
		
	;
	; II. Set object property
	;
	self.oEnergy = oEnergy
end


;+
;   The class definition statement.
;
; :Params:
;       CLASS:          out, optional, type=structure
;-
pro MrDist4D__DEFINE
	compile_opt idl2
	
	class = { MrDist4D, $
	          inherits MrTimeSeries, $
	          oTheta:      obj_new(), $
	          oTheta_FAC:  obj_new(), $
	          oTheta_Grid: obj_new(), $
	          oPhi:        obj_new(), $
	          oPhi_FAC:    obj_new(), $
	          oPhi_Grid:   obj_new(), $
	          oEnergy:     obj_new(), $
	          oPar_FAC:    obj_new(), $
	          oPerp_FAC:   obj_new(), $
	          type_FAC:    '', $
	          mass:        0.0, $
	          orientation: 0B, $
	          units:       '' $
	        }
end