; docformat = 'rst'
;
; NAME:
;   MrVar_EField__Define
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
;   A class for computing common properties/quantities from the magnetic field. These do
;   not include properties that can be generated directly from the MrVectorTS class
;   (e.g. a Spectrogram).
;
;   NOTES:
;       Data is expected to be saved as object properties via the ::SetData method. If
;       a data product is required by a particular method but has not been saved, a
;       ::Load method will be called to try and load the data from source files. It is
;       up to the subclass to define the ::Load_* methods.
;
; :Categories:
;   MrVariable
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
;       2016/11/29  -   Written by Matthew Argall
;-
;*****************************************************************************************
;+
;   The initialization method.
;
; :Keywords:
;       _REF_EXTRA:     in, optional, type=any
;                       Any keyword accepted by the ::SetData method is accepted here via
;                           keyword inheritance.
;-
function MrVar_EField::INIT, $
_REF_EXTRA=extra
	compile_opt idl2

	;Error handling
	catch, the_error
	if the_error ne 0 then begin
		catch, /CANCEL
		MrPrintF, 'LogErr'
		return, 0
	endif
	
	;Initialize superclass
	success = self -> IDL_Object::Init()
	if ~success then message, 'Unable to initialize IDL_Object superclass.'
	
	;Set BField object
	self.oBField = MrVar_BField()
	if ~obj_valid(self.oBField) then message, 'Unable to initialize oBField property.'
	
	;Set data
	if n_elements(extra) gt 0 then self -> SetData, _STRICT_EXTRA=extra

	return, 1
end


;+
;   Clean up after the object is destroyed
;-
pro MrVar_EField::CLEANUP
	compile_opt idl2
	on_error, 2
	
	obj_destroy, self.oB
	obj_destroy, self.oBField
	obj_destroy, self.oB0
	obj_destroy, self.oE
	obj_destroy, self.oV
	
	;Superclass
	self -> IDL_Object::Cleanup
end


;+
;   Check if a variable exists, is of a particular class, and has the same time tags
;   as the magnetic field variable.
;
; :Params:
;       VAR:            in, required, type=integer/string/objref
;                       Name, number, or MrVectorTS objref of a MrVariable. If a name
;                           or number, the corresponding variable must exist in the cache.
;       OVAR:           out, required, type=string, default='MrTimeSeries'
;                       The object reference corresponding to `OVAR`. If `OVAR` is
;                           already an objref, then OVAR is a copy with a different heap
;                           identifier. If `VAR` does not have the same time tags as the
;                           magnetic field vector, it is interpoted and a new variable is
;                           made.
;
; :Keywords:
;       CLASS:          in, optional, type=string, default='MrTimeSeries'
;                       An object class or subclass that `VAR` must be.
;       TIME:           in, optional, type=boolean, default=1
;                       If set, `VAR` will be interpolated to match the times of the
;                           magnetic field vector. This is the default.
;
; :Returns:
;       STATUS:         out, required, type=byte
;                       A flag to indicate if `VAR` is a valid variable. If valid, then
;                           `VAR` passes all tests and `OVAR` is a valid objref.
;                           If invalid, then at least one test failed. Values for SUCCESS:
;                               0  -  success
;                               1  -  `VAR` is not a valid name/number/objref
;                               2  -  `VAR` is not the correct (sub)class
;-
function MrVar_EField::CheckVar, var, oVar, $
CLASS=class, $
MSG=msg, $
TIME=time
	compile_opt idl2
	on_error, 2
	
	;Defaults
	status  = 0B
	msg     = ''
	tf_time = n_elements(time) eq 0 ? 1B : keyword_set(time)
	if n_elements(class) eq 0 then class = 'MrTimeSeries'
	
	;Get the variable
	oTemp = MrVar_Get(var)
	
	;Check if it is valid and is the right class
	if ~obj_valid(oTemp) then begin
		status = 1B
		msg    = 'Variable must be a variable name, number, or object reference'
	endif
	if ~obj_isa(oTemp, class) then begin
		status = 2B
		msg    = 'Variable must be a "' + class + '" object.'
	endif
	
	;Valid object
	if status eq 0B then begin
		;Check time
		if tf_time then begin
			
			;Valid E-Field
			if obj_valid(self.oE) then begin
				;Check time tags
				;   - Store an independent copy, not a reference to what is in the cache
				if oTemp['TIMEVAR'] -> IsIdentical(self.oE['TIMEVAR']) $
					then oVar = oTemp -> Copy() $
					else oVar = oTemp -> Interpol(self.oE['TIMEVAR'])
			
			;Invalid B-Field
			endif else begin
				status = 3B
				msg    = 'Electric field data not available.'
				oVar   = obj_new()
			endelse
		
		;Do not check time
		endif else begin
			oVar = oTemp -> Copy()
		endelse
	
	;Invalid object
	endif else begin
		oVar = obj_new()
	endelse
	
	;Set property
	return, status
end


;+
;   Compute the variations of the electric field about the background values. If a FAC
;   has been defined, DE is rotated into field-aligned coordinates.
;
;       dE = E - E0
;
; :Returns:
;       DE:         out, required, type=MrVectorTS object
;                   Perturbations of the electric field.
;-
function MrVar_EField::dE, nSmooth, $
CACHE=cache, $
NAME=name
	compile_opt idl2
	on_error, 2
	
	;Defaults
	if n_elements(name) eq 0 then name = 'dE(' + self.oB.name + ')'
	if n_elements(nSmooth) eq 0 then begin
		nPts    = self.oE -> GetNPts()
		nSmooth = 256 < nPts/8
	endif
	
	;Get relevant data
	if ~obj_valid(self.oE)  then message, 'No electric field data has been loaded.'
	
	;Subtrack the background field
	dE = self.oE - smooth(self.oE['DATA'], [nSmooth,1])
	
	;Rotate to FAC
	if self.oBField.fac ne 'NONE' then begin
		oT     = self -> T_FAC()
		dE     = oT ## dE
		labels = ['||', 'perp1', 'perp2']
	endif else begin
		labels = ['x', 'y', 'z']
	endelse
	
	;Set attributes
	dE -> AddAttr, 'CATDESC',    'Perturbations from the background electric field.'
	dE -> AddAttr, 'LABEL',      labels
	dE -> AddAttr, 'PLOT_TITLE', 'AC electric field'
	dE -> AddAttr, 'UNITS',      self.oE['UNITS']
	dE -> AddAttr, 'TITLE',      'dE'
	
	return, dE
end


;+
;   Get object properties.
;
; :Keywords:
;       FAC:            out, optional, type=string
;                       Type of field-aligned coordinates to make. Options include
;                           {'ExB' | 'VxB' | 'RadAz' | ''}. Ignored if `PERP` is not given.
;-
pro MrVar_EField::GetProperty, $
FAC=fac
	compile_opt idl2
	
	catch, the_error
	if the_error ne 0 then begin
		catch, /CANCEL
		MrPrintF, 'LogErr'
		return
	endif
	
	if arg_present(fac) then fac = self.oBField.fac
end


;+
;   Load electric field data from source files.
;-
pro MrVar_EField::Load_B
	on_error, 2
	message, 'MrVar_EField::Load_B must be over-ridden by a subclass.'
end


;+
;   Load the background magnetic field. The default action is to detrend the
;   implicit magnetic field (assumes data contains the DC component). Subclasses
;   should load data from source files and interpolate to the implicit array.
;
; :Private:
;
; :Params:
;       NSMOOTH:        in, optional, type=integer
;                       Number of points to smooth the implicit array. This effectively
;                           low-pass filters the data. The default is to use the smaller
;                           of 256 and 1/4 of the total number of points.
;-
function MrVar_BField::Load_B0, nSmooth
	compile_opt idl2
	on_error, 2
	
	self.oBField -> Load_B0, nSmooth
end


;+
;   Load bulk velocity data from source files.
;-
pro MrVar_EField::Load_V
	on_error, 2
	message, 'MrVar_EField::Load_V must be over-ridden by a subclass.'
end


;+
;   Load magnetic field data from source files.
;-
pro MrVar_EField::Load
	on_error, 2
	message, 'MrVar_EField::Load must be over-ridden by a subclass.'
end


;+
;   The purpose of this program is to calculate the Poynting Vector, in micro-Watts per
;   square meter (W/m^2 * 1e-6), in the time domain::
;       \vec{S} = \frac{1}{\mu_{0}} \left( \vec{E} \times \vec{B} \right)
;
;   References::
;       Griffiths, D. J., Introduction to Electrodynamics, 3rd. Ed., Prentice Hall,
;           1999, pg. 347
;       Loto'aniu, T, M., et. al., Propagation of electromagnetic ion cyclotron wave
;           energy in the magnetosphere, J. Geophys. Res., 110, 2005.
;           doi:10.1029/2004JA010816
;
; :Params:
;       E:                  in, required, type=3xN numeric
;                           Electric field waveform data in millivolts per meter (mV/m)
;       B:                  in, required, type=3xN numeric
;                           Magnetic field waveform data in nanoTelsa (nT). Must have the
;                               same number of elements as `E`.
;
; :Returns:
;       S_poynting:         The poynting vector in the spectral domain. Resulting units
;                           are in Watts per square meter (W / m^2).
;-
function MrVar_EField::PoyntingFlux
	compile_opt idl2
	on_error, 2

;-----------------------------------------------------
;Calculate Poynting Vector \\\\\\\\\\\\\\\\\\\\\\\\\\\
;-----------------------------------------------------

	;The coefficient 1 / (4*mu_0)
	;   Poynting Flux strengths reported in Loto'aniu ranged from 1-150 uW/m^2, so
	;   convert to microWeber per square meter.
	;
	;   In MKS units, to convert to micro-W/m^2, multiply by 1e-6
	;
	;   If not in MKS units, mV/m * nT results an answer 1e+12 too big. Therefore, multiply
	;   by 1e-12 to convert to W/m^2 and another 1e+6 to convert to micro-W/m^2
	if keyword_set(mks) $
		then coeff = 1.0e-6 / MrConstants('mu_0') $
		else coeff = 1.0e-6 / MrConstants('mu_0') 

	;Allocate Memory
	S_poynting = coeff * self.oE -> Cross(self.oB)

	return, S_poynting
end


;+
;   The purpose of this program is to calculate the Poynting Vector, in micro-Watts per
;   square meter (W/m^2 * 1e-6), in the spectral domain::
;       \vec{S}_{avg} = \frac{1}{4 \mu} \left(\delta \vec{E}^{*} \times \delta \vec{B} +
;                       \delta \vec{E} \times \delta \vec{B}^{*} \right)
;
;   References::
;       Loto'aniu, T, M., et. al., Propagation of electromagnetic ion cyclotron wave
;           energy in the magnetosphere, J. Geophys. Res., 110, 2005.
;           doi:10.1029/2004JA010816
;
; :Params:
;       NFFT:               in, optional, type=int, default=N_ELEMENTS(DATA)
;                           The number of points to use per FFT
;       DT:                 in, optional, type=float. default=1
;                           The time between data samples. If not present, unit spacing
;                               is assumed.
;       NSHIFT:             in, optional, type=int. default=NFFT/2
;                           The number of points to shift ahead after each FFT.
;
; :Keywords:
;       S_MIN:              in, optional, type=float
;                           Values of `S_poynting` below this value will be set to NaN
;       _EXTRA:             in, type=structure
;                           Any keyword accepted by MrFFT.pro
;                           
; :Returns:
;       S_poynting:         The poynting vector in the spectral domain. Resulting units
;                           are in Watts per square meter (W / m^2).
;-
function MrVar_EField::PoyntingSpectra, nfft, nshift, $
CACHE = cache, $
NAME  = name, $
S_MIN = s_min
	compile_opt idl2
	
	catch, the_error
	if the_error ne 0 then begin
		catch, /CANCEL
		MrPrintF, 'LogErr'
		return, !Null
	endif
	
	if n_elements(name)   eq 0 then name   = 'poynting_spectra'
	if n_elements(nfft)   eq 0 then nfft   = 256
	if n_elements(nshift) eq 0 then nshift = nfft / 4
	
	;Load E-Field
	if ~obj_valid(self.oB) then message, 'No B-Field available.'
	if ~obj_valid(self.oE) then self -> Load_E

;-----------------------------------------------------
; Rotate to FAC \\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\
;-----------------------------------------------------

	if obj_valid(self.oB0) then begin
		oT = self -> T_FAC()
		oB = oT ## self.oB
		oE = oT ## self.oE
	endif else begin
		oB = self.oB
		oE = self.oE
	endelse

;-----------------------------------------------------
;FFT \\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\
;-----------------------------------------------------

	;Take the FFT
	E_fft = oE -> FFT( nfft, nshift, /ONE_SIDED )
	B_fft = oB -> FFT( nfft, nshift, /ONE_SIDED )

;-----------------------------------------------------
;Calculate Poynting Vector \\\\\\\\\\\\\\\\\\\\\\\\\\\
;-----------------------------------------------------

	;The coefficient 1 / (4*mu_0)
	;   Poynting Flux strengths reported in Loto'aniu ranged from 1-150 uW/m^2, so
	;   convert to microWeber per square meter.
	;
	;   In MKS units, to convert to micro-W/m^2, multiply by 1e-6
	;
	;   If not in MKS units, mV/m * nT results an answer 1e+12 too big. Therefore, multiply
	;   by 1e-12 to convert to W/m^2 and another 1e+6 to convert to micro-W/m^2. In total,
	;   that means we have to multiply by 1e-6.
	coeff = 1.0 / (4.0 * MrConstants('mu_0') * 1.0e6)

	;Sx = 1/(4u) * [(Ey*Bz - Ez*By) + (EyBz* - EzBy*)]
	Sx = conj(E_fft[*,*,1])*B_fft[*,*,2] - conj(E_fft[*,*,2])*B_fft[*,*,1] + $
	     E_fft[*,*,1]*conj(B_fft[*,*,2]) - E_fft[*,*,2]*conj(B_fft[*,*,1])
	
	;Sy = 1/(4u) * [(Ez*Bx - Ex*Bz) + (EzBx* - ExBz*)]
	Sy = conj(E_fft[*,*,2])*B_fft[*,*,0] - conj(E_fft[*,*,0])*B_fft[*,*,2] + $
	     E_fft[*,*,2]*conj(B_fft[*,*,0]) - E_fft[*,*,0]*conj(B_fft[*,*,2])

	;Sz = 1/(4u) * [(Ex*By - Ey*Bx) + (ExBy* - EyBx*)]
	Sz = conj(E_fft[*,*,0])*B_fft[*,*,1] - conj(E_fft[*,*,1])*B_fft[*,*,0] + $
	     E_fft[*,*,0]*conj(B_fft[*,*,1]) - E_fft[*,*,1]*conj(B_fft[*,*,0])

	;Multiply by the coefficient
	Sx *= coeff
	Sy *= coeff
	Sz *= coeff

;-----------------------------------------------------
; Real, Positive \\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\
;-----------------------------------------------------
	;Set values |S_poynting| < S_MIN to !values.f_nan
	if n_elements(s_min) gt 0 then begin
		iBad = where(abs(Sx) lt s_min, nBad)
		if nBad gt 0 then Sx[iBad] = !values.f_nan
		
		iBad = where(abs(Sy) lt s_min, nBad)
		if nBad gt 0 then Sy[iBad] = !values.f_nan
		
		iBad = where(abs(Sz) lt s_min, nBad)
		if nBad gt 0 then Sz[iBad] = !values.f_nan
	endif

	;Create variable and set attributes.
	oSx = MrTimeSeries( B_fft['TIMEVAR'], real_part(Sx), NAME=name + '_x', CACHE=cache )
	oSx -> AddAttr, 'AXIS_RANGE',     [-1,1] * max(abs(Sx[*,2:*]), /NAN)
	oSx -> AddAttr, 'DEPEND_1',       B_fft['DEPEND_1']
	oSx -> AddAttr, 'CATDESC',       'Poynting Flux as a function of frequency'
	oSx -> AddAttr, 'MISSING_VALUE', !values.f_nan
	oSx -> AddAttr, 'PLOT_TITLE',    'Poynting Flux Spectrogram'
	oSx -> AddAttr, 'RGB_TABLE',     72
	oSx -> AddAttr, 'UNITS',         '\mu W/m^2'
	oSx -> AddAttr, 'TITLE',         'Sx'

	;Create variable and set attributes.
	oSy = MrTimeSeries( B_fft['TIMEVAR'], real_part(Sy), NAME=name + '_y', CACHE=cache )
	oSy -> AddAttr, 'AXIS_RANGE',     [-1,1] * max(abs(Sy[*,2:*]), /NAN)
	oSy -> AddAttr, 'DEPEND_1',       B_fft['DEPEND_1']
	oSy -> AddAttr, 'CATDESC',       'Poynting Flux as a function of frequency'
	oSy -> AddAttr, 'MISSING_VALUE', !values.f_nan
	oSy -> AddAttr, 'PLOT_TITLE',    'Poynting Flux Spectrogram'
	oSy -> AddAttr, 'RGB_TABLE',     72
	oSy -> AddAttr, 'UNITS',         '\mu W/m^2'
	oSy -> AddAttr, 'TITLE',         'Sy'

	;Create variable and set attributes.
	oSz = MrTimeSeries( B_fft['TIMEVAR'], real_part(Sz), NAME=name + '_z', CACHE=cache )
	oSz -> AddAttr, 'AXIS_RANGE',    [-1,1] * max(abs(Sz[*,2:*]), /NAN)
	oSz -> AddAttr, 'DEPEND_1',      B_fft['DEPEND_1']
	oSz -> AddAttr, 'CATDESC',       'Poynting Flux as a function of frequency'
	oSx -> AddAttr, 'MISSING_VALUE', !values.f_nan
	oSz -> AddAttr, 'PLOT_TITLE',    'Poynting Flux Spectrogram'
	oSz -> AddAttr, 'RGB_TABLE',     72
	oSz -> AddAttr, 'UNITS',         '\mu W/m^2'
	oSz -> AddAttr, 'TITLE',         'Sz'

	return, [oSx, oSy, oSz]
end


;+
;   Compute the convective electric field:
;
; :Keywords:
;       CACHE:          in, optional, type=boolean, default=0
;                       If set, the result will be added to the cache.
;       NAME:           in, optional, type=string, default='RecipCurl'
;                       A name to be given to the return variable.
;
; :Returns:
;       E_VXB:          out, required, type=MrVectorTS objref
;                       Convective electric field.
;-
function MrVar_EField::VxB, $
CACHE=cache, $
NAME=name
	compile_opt idl2
	on_error, 2

	;Load required data
	if ~obj_valid(self.oB) then self -> Load_B
	if ~obj_valid(self.oV) then self -> Load_V
	if n_elements(name) eq 0 then name = 'E_VxB'
	
	;Compute convective electric field
	e_vxb = 1e-3 * self.oV -> Cross(self.oB)
	
	;Attributes
	e_vxb -> SetName, name
	e_vxb['CATDESC']    = 'VxB Convective electric field.'
	e_vxb['PLOT_TITLE'] = 'Convective E-Field'
	e_vxb['TITLE']      = 'Ec!C(mV/m)'
	e_vxb['UNITS']      = 'mV/m'
	if keyword_set(cache) then e_vxb -> Cache
	
	return, e_vxb
end


;+
;   Set data properties.
;
;   All data, is interpolated to the time stamps of `BFIELD`. If `BFIELD` is set,
;   it will clear data from all other variables unless they have identical time tags.
;
; :Params:
;       B0:             in, optional, type=integer/string/objref
;                       Name, number, or MrVectorTS objref of the background magnetic
;                           field variable. If a name or number, the variable must reside
;                           in the cache.
;       BFIELD:         in, optional, type=integer/string/objref
;                       Name, number, or MrVectorTS objref of the magnetic field variable.
;                           If a name or number, the variable must reside in the cache.
;       EFIELD:         in, optional, type=integer/string/objref
;                       Name, number, or MrVectorTS objref of the electric field variable.
;                           If a name or number, the variable must reside in the cache.
;       VELOCITY:       in, optional, type=integer/string/objref
;                       Name, number, or MrVectorTS objref of the bulk velocity variable
;                           If a name or number, the variable must reside in the cache.
;-
pro MrVar_EField::SetData, $
B0=B0, $
BFIELD=bfield, $
EFIELD=eField, $
VELOCITY=velocity
	compile_opt idl2
	
	catch, the_error
	if the_error ne 0 then begin
		catch, /CANCEL
		MrPrintF, 'LogErr'
		return
	endif

;-------------------------------------------
; EFIELD ///////////////////////////////////
;-------------------------------------------
	if n_elements(efield) gt 0 then begin
		status = self -> CheckVar(efield, oE, CLASS='MrVectorTS', MSG=msg, TIME=0)
		
		;Success
		if status eq 0B then begin
			self.oE = oE

			;Clear all other variables
			if obj_valid(self.oB0) && ~oE['TIMEVAR'] -> IsIdentical(self.oB0['TIMEVAR']) then obj_destroy, self.oB0
			if obj_valid(self.oB)  && ~oE['TIMEVAR'] -> IsIdentical(self.oB['TIMEVAR'])  then obj_destroy, self.oB
			if obj_valid(self.oV)  && ~oE['TIMEVAR'] -> IsIdentical(self.oV['TIMEVAR'])  then obj_destroy, self.oV
		
		;Failure
		endif else begin
			MrPrintF, 'LogText', 'Cannot set EFIELD.'
			MrPrintF, 'LogErr', msg
		endelse
	endif

;-------------------------------------------
; B0 ///////////////////////////////////////
;-------------------------------------------
	if n_elements(B0) gt 0 then begin
		status = self -> CheckVar(B0, oB0, CLASS='MrVectorTS', MSG=msg)

		if status eq 0B then begin
			self.oB0 = oB0 
		endif else begin
			MrPrintF, 'LogText', 'Cannot set B0.'
			MrPrintF, 'LogErr', msg
		endelse
	endif

;-------------------------------------------
; BFIELD ///////////////////////////////////
;-------------------------------------------
	if n_elements(bfield) gt 0 then begin
		status = self -> CheckVar(bfield, oB, CLASS='MrVectorTS', MSG=msg)

		if status eq 0B then begin
			self.oB = oB
		endif else begin
			MrPrintF, 'LogText', 'Cannot set BFIELD.'
			MrPrintF, 'LogErr', msg
		endelse
	endif

;-------------------------------------------
; VELOCITY /////////////////////////////////
;-------------------------------------------
	if n_elements(velocity) gt 0 then begin
		status = self -> CheckVar(velocity, oV, CLASS='MrVectorTS', MSG=msg)

		if status eq 0B then begin
			self.oV = oV 
		endif else begin
			MrPrintF, 'LogText', 'Cannot set VELOCITY.'
			MrPrintF, 'LogErr', msg
		endelse
	endif
end


;+
;   Set the field-aligned coordinate system. The background field B0 is used as the
;   parallel (or z'-direction). The perpendicular directions are determined by either
;   `TYPE` or `PERP`.
;
;   CALLING SEQUENCE:
;       oB -> SetFAC, fac
;       oB -> SetFAC, fac, perp
;
; :Keywords:
;       TYPE:           in, optional, type=string
;                       Type of field-aligned coordinates to make. Options include
;                           {'ExB' | 'VxB' | 'RadAz' | 'CROSSX' | 'NONE'}. Ignored if `PERP` is not given.
;       PERP:           in, optional, type=string/integer/objref
;                       Name, number, or MrVectorTS object associated with a vector that
;                           defines one perpendicular direction. If a name is given, the
;                           variable must be in the variable cache.
;-
pro MrVar_EField::SetFAC, fac, perp
	compile_opt idl2
	
	;Load magnetic field
	if ~obj_valid(self.oB) then self -> Load_B
	oB = self.oB
	
	;Load perpendicular vector
	case strupcase(fac) of
		'EXB': begin
			if ~obj_valid(self.oE) then self -> Load
			oE = self.oE
		endcase
		
		'VXB': begin
			if ~obj_valid(self.oV) then self -> Load_V
			oV = self.oV
		endcase
		
		'CROSSX': ;Do nothing
		'NONE':   ;Do nothing
		else: message, 'FAC not recognized: "' + fac + '".'
	endcase
	
	;Create B-Field object
	if ~obj_valid(self.oBField) then self.oBField = MrVar_BField()
	self.oBField -> SetData, BFIELD   = oB, $
	                         EFIELD   = oE, $
	                         VELOCITY = oV
	self.oBField -> SetFAC, fac
end


;+
;   The class definition statement.
;
; :Params:
;       CLASS:          out, optional, type=structure
;-
pro MrVar_EField__DEFINE
	compile_opt idl2
	
	class = { MrVar_EField, $
	          inherits IDL_Object, $
	          oB:      obj_new(), $
	          oB0:     obj_new(), $
	          oE:      obj_new(), $
	          oBField: obj_new(), $
	          oV:      obj_new() $
	        }
end