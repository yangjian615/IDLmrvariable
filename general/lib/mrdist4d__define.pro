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
DEGREES=degrees, $
MASS=mass, $
NAME=name, $
RADIANS=radians, $
SPECIES=species, $
UNITS=units, $
_REF_EXTRA=extra
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
	success = self -> MrTimeSeries::Init( NAME          = name, $
	                                      _STRICT_EXTRA = extra )
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
	
	;Superclass
	self -> MrTimeSeries::Cleanup
end


;+
;   Convert the distribution function from one set of units to another. The conversion
;   factors are:
;                                        PARTICLE
;           |  PARTICLE FLUX (F)  |  ENERGY FLUX (FE)  |  PHASE SPACE DENSITY (f)  |
;      -----------------------------------------------------------------------------
;       F   |         1           |      1 / E         |          2*E/m^2          |
;       FE  |         E           |         1          |         2*(E/m)^2         |
;       f   |     m^2/(2*E)       |    m^2/(2*E^2)     |             1             |
;      -----------------------------------------------------------------------------
;
;   Such that f = [ m^2 / (2 * E^2) ] * FE, etc.
;
; :Params:
;       FLUX:           in, required, type=fltarr
;                       Distribution function for which to convert units
;       TO_UNITS:       in, required, type=string
;                       Convert to these units.  Options are::
;                           Energy:                'ENERGY'      - eV
;                           Particle Energy Flux:  'EFLUX'       - eV / cm^2 / s / sr / eV    or    keV / cm^2 / s / sr / keV
;                           Particle Flux:         'DIFF FLUX'   - # / cm^2 / s / sr / keV
;                           Phase Space Density:   'PSD'         - s^2 / cm^6
;                                                  'DF'          - s^2 / cm^6
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

	;
	; Conversion factor from particle energy flux to phase space density
	;   - (m^2 / 2 * E^2 ) * FE = [ kg^2 / (2 * eV^2) ]                         * (eV / cm^2 / s / sr / eV)
	;                           = ( kg^2 / eV^2 )                               / ( cm^2 * s * sr ) * 0.5
	;                           = [ kg^2 / ( eV * (1.602e-19 J/eV) ) ]          / ( cm^2 * s * sr ) * 0.5
	;                           = [ kg^2 / ( kg m^2 / s^2 )^2 ]                 / ( cm^2 * s * sr ) * 0.5 * 1.602e-19^(-2)
	;                           = [ (N * 1.672e-27 kg)^2 / ( kg^2 m^4 / s^4 ) ] / ( cm^2 * s * sr ) * 0.5 * 1.602e-19^(-2)
	;                           = [ kg^2 / ( kg^2 m^4 / s^4 ) ]                 / ( cm^2 * s * sr ) * 0.5 * 1.602e-19^(-2) * N^2 * 1.672e-27^2
	;                           = [ 1 / (m^4 * 1e8 /cm^4/m^4 / s^4) ]           / ( cm^2 * s * sr ) * 0.5 * 1.602e-19^(-2) * N^2 * 1.672e-27^2
	;                           = [ 1 / (cm^4 / s^4) ]                          / ( cm^2 * s * sr ) * 0.5 * 1.602e-19^(-2) * N^2 * 1.672e-27^2 * 1e-8
	;                           = ( s^4 / cm^4 )                                / ( cm^2 * s * sr ) * 0.5 * 1.602e-19^(-2) * N^2 * 1.672e-27^2 * 1e-8
	;                           = s^3 / cm^6 * 0.5 * 1.602e-19^(-2) * N^2 * 1.672e-27^2 * 1e-8
	;                           = s^3 / cm^6 * N^2 * 5.44933e-25
	;                           = s^3 / m^6  * N^2 * 5.44933e-13
	;                           = s^3 / km^6 * N^2 * 5.44933e+5
	;
	
	;Mass number
	case self.species of
		'H':  N = 1
		'He': N = 2
		'O':  N = 16
		'e':  N = MrConstants('m_e') / MrConstants('m_p')
		else: message, 'Species not recognized.'
	endcase
	
	;Must still divide by the values of E^2!!
	eflux_to_psd = N^2 * 5.44933e-25
	
	;Vectorize multiplication
	oEnergy = self['DEPEND_3']
	dims    = size(*self.data, /DIMENSIONS)
	if obj_isa(oEnergy, 'MrTimeSeries') $
		then tempE = rebin( reform( oEnergy['DATA'], dims[0], 1, 1, dims[3] ), dims ) $
		else tempE = rebin( reform( oEnergy['DATA'],       1, 1, 1, dims[3] ), dims )
	
	;Energy Flux
	if self.units eq 'EFLUX' then begin
		
		;Convert to:
		case toUnits of
			'DIFF FLUX': new_flux = self['DATA'] / temporary(tempE) * 1e3     ; 1/eV * (1e3 eV/keV) = 1e3/keV
			'PSD':       new_flux = eflux_to_psd * self['DATA'] / temporary(tempE)^2
			'DF':        new_flux = eflux_to_psd * self['DATA'] / temporary(tempE)^2
			'EFLUX':     new_flux = self['DATA']
			else: message, 'Cannot convert from "' + self.units + '" to "' + to_units + '".'
		endcase
	
	;Differential flux
	endif else if self.units eq 'DIFF FLUX' then begin
		;Convert from PF to PEF
		eflux = self['DATA'] * tempE * 1e-3     ; 1/keV * (1e-3 keV/eV) = 1e-3/eV
		
		;Convert to:
		case toUnits of
			'EFLUX':     new_flux = temporary(eflux)
			'PSD':       new_flux = eflux_to_psd * temporary(eflux) / temporary(tempE)^2
			'DF':        new_flux = eflux_to_psd * temporary(eflux) / temporary(tempE)^2
			'DIFF FLUX': new_flux = self['DATA']
			else: message, 'Cannot convert from "' + self.units + '" to "' + to_units + '".'
		endcase
	
	;Phase space density
	endif else if self.units eq 'PSD' || self.units eq 'DF' then begin
		;Convert to:
		case toUnits of
			'EFLUX':     new_flux = self['DATA'] / eflux_to_psd * temporary(tempE)^2
			'DIFF FLUX': new_flux = self['DATA'] / eflux_to_psd * temporary(tempE) * 1e3   ; 1/eV * (1e3 eV/keV) = 1e3/keV
			'PSD':       new_flux = self['DATA']
			'DF':        new_flux = self['DATA']
			else: message, 'Cannot convert from "' + self.units + '" to "' + to_units + '".'
		endcase
	
	;Invalid
	endif else begin
		message, 'Invalid FROM_UNIT: "' + self.units + '".'
	endelse

;-------------------------------------------
; Set Properties ///////////////////////////
;-------------------------------------------
	
	;Set properties
	self.units = toUnits
	case toUnits of
		'EFLUX':     self -> SetAttrValue, 'UNITS', 'keV / cm^2 / s / sr / keV'
		'ENERGY':    self -> SetAttrValue, 'UNITS', 'eV'
		'DIFF FLUX': self -> SetAttrValue, 'UNITS', '# / cm^2 / s / sr / keV'
		'PSD':       self -> SetAttrValue, 'UNITS', 's^2 / cm^6'
		else: message, 'Invalid units: "' + to_units + '".'
	endcase
	
	;Set the object properties
	*self.data = temporary(new_flux)
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
;   Extract a single distribution function.
;
; :Params:
;       IDX:                in, required, type=integer
;                           Time index for which the 3D distribution is returned.
;
; :Returns:
;       DIST3D:             out, required, type=MrVariable object
;                           A 3D distribution function.
;-
function MrDist4D::GetDist3D, idx
	compile_opt idl2
	on_error, 2

;-----------------------------------------------------
; Type of Distribution \\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\
;-----------------------------------------------------
	oPhi    = self['DEPEND_1']
	oTheta  = self['DEPEND_2']
	oEnergy = self['DEPEND_3']
	
	;Are the dependent variables time-dependent
	tf_angleTS  = obj_isa(oPhi,    'MrTimeSeries')
	tf_energyTS = obj_isa(oEnergy, 'MrTimeSeries')

;-----------------------------------------------------
; Create Distribution \\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\
;-----------------------------------------------------
	
	case 1 of
		;BOTH
		tf_angleTS && tf_energyTS: begin
			dist3D = MrDist3D( reform( (*self.data)[idx,*,*,*] ), $
			                   reform( oPhi[idx,*,*] ), $
			                   reform( oTheta[idx,*,*] ), $
			                   reform( oEnergy[idx,*,*] ), $
			                   /DEGREES, $
			                   MASS  = self.mass, $
			                   UNITS = self.units )
		endcase
		
		;ANGLES
		tf_angleTS: begin
			dist3D = MrDist3D( reform( (*self.data)[idx,*,*,*] ), $
			                   reform( oPhi[idx,*,*] ), $
			                   reform( oTheta[idx,*,*] ), $
			                   oEnergy['DATA'], $
			                   /DEGREES, $
			                   MASS  = self.mass, $
			                   UNITS = self.units )
		endcase
		
		;ENERGY
		tf_energyTS: begin
			dist3D = MrDist3D( reform( (*self.data)[idx,*,*,*] ), oPhi['DATA'], oTheta['DATA'], $
			                   reform( oEnergy[idx,*] ), $
			                   /DEGREES, $
			                   MASS  = self.mass, $
			                   UNITS = self.units )
		endcase
		
		;NEITHER
		else: begin
			dist3D = MrDist3D( reform( (*self.data)[idx,*,*,*] ), oPhi['DATA'], oTheta['DATA'], oEnergy['DATA'], $
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
function MrDist4D::GetESpec, $
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

	;Step over each time
	for i = 0, nTime - 1 do begin
		oDist3D = self -> GetDist3D(i)
		
		;Reduce the distribution
		ESpec[i,*] = oDist3D -> ESpec( e_bins, dE, $
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
;	binName = name + '_EBins'
;	oEBins  = size(e_bins, /N_DIMENSIONS) eq 2 $
;	               ? MrTimeSeries( self.oTime, e_bins, NAME=binName, /NO_COPY ) $
;	               : MrVariable( e_bins, NAME=binName, /NO_COPY )
	
	;Energy attributes
;	oEBins -> AddAttr, 'UNITS', self.oEnergy['UNITS']
;	oEBins -> AddAttr, 'TITLE', 'Energy'

	;Energy bins have not changed
	;   - MUST ALSO UPDATE MRDIST3D::SPECE
	oEBins = self['DEPEND_3']

	;Sepctrogram attributes
	oESpec -> AddAttr, 'DEPEND_1', oEBins
	oESpec -> AddAttr, 'SCALE',    1B
	oESpec -> AddAttr, 'LOG',      1B
	oESpec -> AddAttr, 'UNITS',    self['UNITS']
	
	return, oESpec
end


;+
;   Reduce the 3D distribution function to a 2D distribution in polar angle and energy,
;   averaging over azimuth angle.
;
; :Keywords:
;       CACHE:          in, optional, type=boolean, default=0
;                       If set, the output variable will be added to the variable cache.
;       NAME:           in, optional, type=string, default=self.name + '_ThetaE'
;                       Name to be given to the output variable.
;       NE_BINS:        in, optional, type=integer
;                       Number of energy bins in the reduced distribution. The default
;                           is to use the same bins and the original distribution.
;       NPHI_BINS:      in, optional, type=integer
;                       Number of polar angle bins in the reduced distribution. The
;                           default is to use the same bins and the original distribution.
;       THETA_RANGE:    in, optional, type=fltarr(2), default=[0.0\, 180.0]
;                       The range in polar angle (degrees) over which to average.
;
; :Returns:
;       DIST2D:         out, required, type=MrTimeSeries object
;                       A time-varying 2D distribution in polar angle and energy.
;-
function MrDist4D::GetPhiE, $
CACHE=cache, $
NAME=name, $
NE_BINS=ne_bins, $
NPHI_BINS=nPhi_bins, $
THETA_RANGE=theta_range
	compile_opt idl2
	
	catch, the_error
	if the_error ne 0 then begin
		catch, /CANCEL
		MrPrintF, 'LogErr'
		if n_elements(oDist3D)  gt 0 then obj_destroy, oDist3D
		if n_elements(oPhiE)    gt 0 then obj_destroy, oPhiE
		if n_elements(oPhiBins) gt 0 then obj_destroy, oPhiBins
		if n_elements(oEBins)   gt 0 then obj_destroy, oEBins
		return, !Null
	endif
	
	;Defaults
	if n_elements(name) eq 0 then name = self.name + '_PhiE'

;-------------------------------------------
; Reduce the 4D Distribution ///////////////
;-------------------------------------------

	;Allocate memory
	dims    = size(self, /DIMENSIONS)
	nTimes  = dims[0]
	nPhi    = dims[1]
	nTheta  = dims[2]
	nEnergy = dims[3]
	PhiE  = fltarr( nTimes, nPhi, nEnergy )
	
	;Step over each time
	for i = 0, n_elements(nTimes) - 1 do begin
		oDist3D = self -> GetDist3D(i)
		
		;Reduce the distribution
		PhiE[i,*,*] = oDist3D -> PhiE( phi, energy, dPhi, dE, $
		                               NE_BINS     = ne_bins, $
		                               NPHI_BINS   = nPhi_bins, $
		                               THETA_RANGE = theta_range )

		;Destroy the 3D distribution
		obj_destroy, oDist3D
	endfor

;-------------------------------------------
; Datasets /////////////////////////////////
;-------------------------------------------
	
	;Theta-Energy distribution
	oPhiE = MrTimeSeries( self.oTime, PhiE, $
	                      CACHE = cache, $
	                      NAME  = name, $
	                      /NO_COPY )
	
	;Phi
	binName  = name + '_PhiBins'
	oPhiBins = size(phi, /N_DIMENSIONS) eq 2 $
	                 ? MrTimeSeries( self.oTime, phi, NAME=binName, /NO_COPY ) $
	                 : MrVariable( phi, NAME=binName, /NO_COPY )
	
	;Energy
	binName     = name + '_EnergyBins'
	oEnergyBins = size(energy, /N_DIMENSIONS) eq 2 $
	                  ? MrTimeSeries( self.oTime, energy, NAME=binName, /NO_COPY ) $
	                  : MrVariable( energy, NAME=binName, /NO_COPY )

;-------------------------------------------
; Attributes ///////////////////////////////
;-------------------------------------------
	
	;Phi attributes
	oPhiBins -> AddAttr, 'DELTA_MINUS', dPhi
	oPhiBins -> AddAttr, 'DELTA_PLUS',  dPhi
	oPhiBins -> AddAttr, 'UNITS',       'degrees'
	oPhiBins -> AddAttr, 'TITLE',       'Azimuth'
	oPhiBins -> AddAttr, 'PLOT_TITLE',  'Azimuthal Bin Centers'
	
	;Energy attributes
;	oEBins -> AddAttr, 'UNITS', self.oEnergy['UNITS']
;	oEBins -> AddAttr, 'TITLE', 'Energy'

	;Energy bins have not changed
	;   - MUST ALSO UPDATE MRDIST3D::SPECE
	oEBins = self['DEPEND_3']

	;Distribution attributes
	oPhiE -> AddAttr, 'DEPEND_1', oPhiBins
	oPhiE -> AddAttr, 'DEPEND_2', oEBins
	oPhiE -> AddAttr, 'SCALE',    1B
	oPhiE -> AddAttr, 'LOG',      1B
	oPhiE -> AddAttr, 'UNITS',    self['UNITS']
	
	;Return the 2D distribution
	return, oPhiE
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
function MrDist4D::GetPhiSpec, $
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
	if n_elements(name) eq 0 then name = self.name + '_PhiSpec'

	;Allocate memory
	dims    = size(self, /DIMENSIONS)
	nTime   = dims[0]
	nPhi    = dims[1]
	nTheta  = dims[2]
	nEnergy = dims[3]
	phiSpec = fltarr( nTime, nPhi )
	
	;Step over each time
	for i = 0, nTime - 1 do begin
		oDist3D = self -> GetDist3D(i)
		
		;Reduce the distribution
		phiSpec[i,*] = oDist3D -> PhiSpec( phi_bins, dPhi, $
		                                   E_RANGE     = e_range, $
		                                   NPHI_BINS   = nPhi_bins, $
		                                   THETA_RANGE = theta_range )

		;Destroy the object
		obj_destroy, oDist3D
	endfor
	
	;Phi-time spectrogram
	oPhiSpec = MrTimeSeries( self.oTime, phiSpec, $
	                         CACHE = cache, $
	                         NAME  = name, $
	                         /NO_COPY )
	
	;Abscissa
	binName  = name + '_PhiBins'
	oPhiBins = size(phi_bins, /N_DIMENSIONS) eq 2 $
	                 ? MrTimeSeries( self.oTime, phi_bins, NAME=binName, /NO_COPY ) $
	                 : MrVariable( phi_bins, NAME=binName, /NO_COPY )
	
	;Phi attributes
	oPhiBins -> AddAttr, 'DELTA_MINUS', dPhi
	oPhiBins -> AddAttr, 'DELTA_PLUS',  dPhi
	oPhiBins -> AddAttr, 'UNITS',       'degrees'
	oPhiBins -> AddAttr, 'TITLE',       'Azimuth'
	oPhiBins -> AddAttr, 'PLOT_TITLE',  'Azimuthal Bin Centers'

	;Sepctrogram attributes
	oPhiSpec -> AddAttr, 'DEPEND_1',   oPhiBins
	oPhiSpec -> AddAttr, 'SCALE',      1B
	oPhiSpec -> AddAttr, 'LOG',        1B
	oPhiSpec -> AddAttr, 'UNITS',      self['UNITS']
	oPhiSpec -> AddAttr, 'TITLE',      'Phi Dist'
	oPhiSpec -> AddAttr, 'PLOT_TITLE', 'Distribution in Phi'
	
	return, oPhiSpec
end


;+
;   Reduce the 3D distribution function to a 2D distribution in polar angle and energy,
;   averaging over azimuth angle.
;
; :Keywords:
;       CACHE:          in, optional, type=boolean, default=0
;                       If set, the output variable will be added to the variable cache.
;       NAME:           in, optional, type=string, default=self.name + '_ThetaE'
;                       Name to be given to the output variable.
;       NE_BINS:        in, optional, type=integer
;                       Number of energy bins in the reduced distribution. The default
;                           is to use the same bins and the original distribution.
;       NTHETA_BINS:    in, optional, type=integer
;                       Number of polar angle bins in the reduced distribution. The
;                           default is to use the same bins and the original distribution.
;       PHI_RANGE:      in, optional, type=fltarr(2), default=[0.0\, 360.0]
;                       The range in azimuthal angle (degrees) over which to average.
;
; :Returns:
;       DIST2D:         out, required, type=MrTimeSeries object
;                       A time-varying 2D distribution in polar angle and energy.
;-
function MrDist4D::GetThetaE, $
CACHE=cache, $
NAME=name, $
NE_BINS=ne_bins, $
NTHETA_BINS=nTheta_bins, $
PHI_RANGE=phi_range
	compile_opt idl2
	on_error, 2
	
	;Defaults
	if n_elements(name) eq 0 then name = self.name + '_ThetaE'

;-------------------------------------------
; Reduce the 4D Distribution ///////////////
;-------------------------------------------

	;Allocate memory
	dims    = size(self, /DIMENSIONS)
	nTimes  = dims[0]
	nTheta  = dims[1]
	nPhi    = dims[2]
	nEnergy = dims[3]
	ThetaE  = fltarr( nTimes, nTheta, nEnergy )
	
	;Step over each time
	for i = 0, n_elements(nTimes) - 1 do begin
		oDist3D = self -> GetDist3D(i)
		
		;Reduce the distribution
		ThetaE[i,*,*] = oDist3D -> ThetaE( theta, energy, dTheta, dE, $
		                                   NE_BINS     = ne_bins, $
		                                   NTHETA_BINS = nTheta_bins, $
		                                   PHI_RANGE   = phi_range )
		                                   
		;Destroy the 3D distribution
		obj_destroy, oDist3D
	endfor

;-------------------------------------------
; Datasets /////////////////////////////////
;-------------------------------------------
	
	;Theta-Energy distribution
	oThetaE = MrTimeSeries( self.oTime, ThetaE, $
	                         CACHE = cache, $
	                         NAME  = name, $
	                         /NO_COPY )
	
	;Theta
	binName    = name + '_ThetaBins'
	oThetaBins = size(theta, /N_DIMENSIONS) eq 2 $
	                 ? MrTimeSeries( self.oTime, theta_bins, NAME=binName, /NO_COPY ) $
	                 : MrVariable( theta_bins, NAME=binName, /NO_COPY )
	
	;Energy
	binName     = name + '_EnergyBins'
	oEnergyBins = size(energy, /N_DIMENSIONS) eq 2 $
	                  ? MrTimeSeries( self.oTime, energy, NAME=binName, /NO_COPY ) $
	                  : MrVariable( energy, NAME=binName, /NO_COPY )

;-------------------------------------------
; Attributes ///////////////////////////////
;-------------------------------------------
	
	;Theta attributes
	oThetaBins -> AddAttr, 'DELTA_MINUS', dTheta
	oThetaBins -> AddAttr, 'DELTA_PLUS',  dTheta
	oThetaBins -> AddAttr, 'UNITS',      'degrees'
	oThetaBins -> AddAttr, 'TITLE',      'Polar Angle'
	oThetaBins -> AddAttr, 'PLOT_TITLE', 'Polar Bin Centers'
	
	;Energy attributes
;	oEBins -> AddAttr, 'UNITS', self.oEnergy['UNITS']
;	oEBins -> AddAttr, 'TITLE', 'Energy'

	;Energy bins have not changed
	;   - MUST ALSO UPDATE MRDIST3D::SPECE
	oEBins = self['DEPEND_3']

	;Distribution attributes
	oESpec -> AddAttr, 'DEPEND_1', oThetaBins
	oESpec -> AddAttr, 'DEPEND_2', oEBins
	oESpec -> AddAttr, 'SCALE',    1B
	oESpec -> AddAttr, 'LOG',      1B
	oESpec -> AddAttr, 'UNITS',    self['UNITS']
	
	;Return the 2D distribution
	return, oThetaE
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
function MrDist4D::GetThetaSpec, $
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
	
	;Step over each time
	for i = 0, nTime - 1 do begin
		oDist3D = self -> GetDist3D(i)
		
		;Reduce the distribution
		ThetaSpec[i,*] = oDist3D -> ThetaSpec(theta_bins, dTheta, $
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
	oThetaBins -> AddAttr, 'DELTA_MINUS', dTheta
	oThetaBins -> AddAttr, 'DELTA_PLUS',  dTheta
	oThetaBins -> AddAttr, 'UNITS',      'degrees'
	oThetaBins -> AddAttr, 'TITLE',      'Polar Angle'
	oThetaBins -> AddAttr, 'PLOT_TITLE', 'Polar Bin Centers'

	;Sepctrogram attributes
	oThetaSpec -> AddAttr, 'DEPEND_1',   oThetaBins
	oThetaSpec -> AddAttr, 'SCALE',      1B
	oThetaSpec -> AddAttr, 'LOG',        1B
	oThetaSpec -> AddAttr, 'UNITS',      self['UNITS']
	oThetaSpec -> AddAttr, 'TITLE',      'Theta Dist'
	oThetaSpec -> AddAttr, 'PLOT_TITLE', 'Distribution in Theta'
	
	return, oThetaSpec
end


;+
;   Transform a spherical coordinate grid into a cartesian coordinate grid.
;
; :Keywords:
;       MASS:               out, optional, type=float
;                           Mass (kg) of the species represented in the distribution.
;       SPECIES:            out, optional, type=string
;                           The particle species represented in the distribution.
;       UNITS:              out, optional, type=string
;                           Units of the distribution function.
;       _REF_EXTRA:         out, optional, type=any
;                           Any keyword accepted by MrTimeSeries::GetProperty
;-
pro MrDist4D::GetProperty, $
MASS=mass, $
SPECIES=species, $
UNITS=units, $
_REF_EXTRA=extra
	compile_opt idl2
	on_error, 2
	
	if arg_present(mass)    gt 0 then mass    = self.mass
	if arg_present(species) gt 0 then species = self.species
	if arg_present(units)   gt 0 then units   = self.units
	
	;Superclasses
	if n_elements(extra) gt 0 then self -> MrTimeSeries::GetProperty, _STRICT_EXTRA=extra
end


;+
;   Load the distribution from source. Loaded data should be saved as properties unique
;   to the subclass (and optionally removed from the variable cache).
;-
pro MrDist4D::Load
	compile_opt idl2
	on_error, 2
	
	message, 'MrDist4D::Load must be over-ridden by a subclass.'
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
	
	if n_elements(mass) gt 0 then begin
		N = round(mass / MrConstants('m_p'))
		case N of
			0:    species = 'e'
			1:    species = 'H'
			2:    species = 'He'
			16:   species = 'O'
			else: message, 'Unable to determine particle species given MASS.'
		endcase
		self.mass    = mass
		self.species = species
	endif
	
	if n_elements(species) gt 0 then begin
		if ~MrIsMember(['e', 'H', 'He', 'O'], species) $
			then message, 'SPECIES must be {"e" | "H" | "He" | "O"}'
		self.mass    = MrConstants('m_' + species)
		self.species = species
	endif
end


;+
;   Set the array.
;
;   CALLING SEQUENCE:
;       oTS -> SetData, data
;       oTS -> SetData, time, data
;       oTS -> SetData, time, data, phi, theta, energy
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
;       PHI:            in, optional, type=Nx1 or NxM array
;                       Azimuthal coordinates of the distribution pixels. Can be the name
;                           or reference of a MrVariable object, or the variable data.
;                           If the variable is a MrTimeSeries object, its time property
;                           must be the same as that of the implicit distribution.
;                           It must have dimensions of [phi, theta] or [time, phi, theta]
;       THETA:          in, optional, type=Nx1 or NxM array
;                       Polar coordinates of the distribution pixels. Can be the name or
;                           reference of a MrVariable object, or the variable data.
;                           If the variable is a MrTimeSeries object, its time property
;                           must be the same as that of the implicit distribution.
;                           It must have dimensions of [phi, theta] or [time, phi, theta]
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
	oPhi    = self -> GetAttrValue('DEPEND_1', /NULL)
	oTheta  = self -> GetAttrValue('DEPEND_2', /NULL)
	oEnergy = self -> GetAttrValue('DEPEND_3', /NULL)

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
	nEnergy = theDims[3]
	
	;
	; Steps:
	;   1. Obtain variable object
	;   2. Compare size to distribution function
	;   3. Set DEPEND_3 attribute
	;

;-----------------------------------------------------
; Obtain Variable Object \\\\\\\\\\\\\\\\\\\\\\\\\\\\\
;-----------------------------------------------------
	
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
				if szTheta[0] eq nTime || szTheta[1] eq nTimes $
					then oEnergy = MrTimeSeries( self.oTime, energy, NAME=self.name + '_energy' ) $
					else message, 'Invalid dimensions: ENERGY.'
			
			;Invalid
			endif else begin
				message, 'ENERGY must have 1 or 2 dimensions.'
			endelse
		endelse
	endelse

;-----------------------------------------------------
; Compare With Distribution \\\\\\\\\\\\\\\\\\\\\\\\\\
;-----------------------------------------------------
	
	;Time objects
	if obj_isa(oEnergy, 'MrTimeSeries') then begin
		;Make sure DIST and ENERGY use the same time object
		energyTime = energy['TIMEVAR']
		if ~energyTime -> IsIdentical( self.oTime ) $
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

;-----------------------------------------------------
; Set Attribute \\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\
;-----------------------------------------------------
	self -> SetAttrValue, 'DEPEND_3', oEnergy, /CREATE
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
;                           It must have dimensions of [phi, theta] or [time, phi, theta]
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
	nTimes   = theDims[0]
	nPhi    = theDims[1]
	nTheta  = theDims[2]
	
	if n_elements(degrees) gt 0 && n_elements(radians) gt 0 $
		then message, 'DEGREES and RADIANS are mutually exclusive.'
	if n_elements(degrees) gt 0 then tf_degrees = keyword_set(degrees)
	if n_elements(radians) gt 0 then tf_degrees = ~keyword_set(radians)
	
	;
	; Steps:
	;   1. Obtain variable object
	;   2. Compare size to distribution function
	;   3. Set DEPEND_2 attribute
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
				if dims[0] eq nTimes && dims[1] eq nPhi $
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
		phiTime = phi['TIMEVAR']
		if ~phiTime -> IsIdentical( self.oTime ) $
			then message, 'PHI and DIST have different time objects.'
	endif
	
	;Data Size
	dims  = size(oPhi, /DIMENSIONS)
	nDims = n_elements(dims)
	if nDims eq 2 then begin
		if dims[0] ne nPhi then message, 'Invalid dimensions: PHI.'
		if dims[1] ne nTheta then message, 'Invalid dimensions: PHI.'
	endif else if nDims eq 3 then begin
		if dims[0] ne nTimes then message, 'Invalid dimensions: PHI.'
		if dims[1] ne nPhi then message, 'Invalid dimensions: PHI.'
		if dims[2] ne nTheta then message, 'Invalid dimensions: PHI.'
	endif else begin
		message, 'PHI must have 2 or 3 dimensions.'
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
	self -> SetAttrValue, 'DEPEND_1', oPhi, /CREATE
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
;                           It must have dimensions of [phi, theta] or [time, phi, theta]
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
	nTimes   = theDims[0]
	nPhi    = theDims[1]
	nTheta  = theDims[2]
	
	if n_elements(degrees) gt 0 && n_elements(radians) gt 0 $
		then message, 'DEGREES and RADIANS are mutually exclusive.'
	
	;
	; Steps:
	;   1. Obtain variable object
	;   2. Compare size to distribution function
	;   3. Units
	;   4. Set DEPEND_2 attribute
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
		thetaTime = theta['TIMEVAR']
		if ~thetaTime -> IsIdentical(self.oTime) $
			then message, 'THETA and DIST have different time objects.'
	endif
	
	;Data Size
	dims  = size(oTheta, /DIMENSIONS)
	nDims = n_elements(dims)
	if nDims eq 2 then begin
		if dims[0] ne nPhi   then message, 'Invalid dimensions: THETA.'
		if dims[1] ne nTheta then message, 'Invalid dimensions: THETA.'
	endif else if nDims eq 3 then begin
		if dims[0] ne nTimes  then message, 'Invalid dimensions: THETA.'
		if dims[1] ne nPhi   then message, 'Invalid dimensions: THETA.'
		if dims[2] ne nTheta then message, 'Invalid dimensions: THETA.'
	endif else begin
		message, 'THETA must have 2 or 3 dimensions.'
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
	self -> SetAttrValue, 'DEPEND_2', oTheta, /CREATE
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
	          mass:    0.0, $
	          species: '', $
	          units:   '' $
	        }
end