; docformat = 'rst'
;
; NAME:
;   MrVar_BField__Define
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
function MrVar_BField::INIT, $
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

	;Default property values
	self.fac = 'NONE'
	
	;Set data
	if n_elements(extra) gt 0 then self -> SetData, _STRICT_EXTRA=extra

	return, 1
end


;+
;   Clean up after the object is destroyed
;-
pro MrVar_BField::CLEANUP
	compile_opt idl2
	on_error, 2
	
	obj_destroy, self.oB
	obj_destroy, self.oB0
	obj_destroy, self.oE
	obj_destroy, self.oPar_FAC
	obj_destroy, self.oPerp_FAC
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
function MrVar_BField::CheckVar, var, oVar, $
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
			
			;Valic B-Field
			if obj_valid(self.oB) then begin
				;Check time tags
				;   - Store an independent copy, not a reference to what is in the cache
				if oTemp['TIMEVAR'] -> IsIdentical(self.oB['TIMEVAR']) $
					then oVar = oTemp -> Copy() $
					else oVar = oTemp -> Interpol(self.oB['TIMEVAR'])
			
			;Invalid B-Field
			endif else begin
				status = 3B
				msg    = 'Magnetic field data not found.'
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
;   Compute the variations of the magnetic field about the background values. If a FAC
;   has been defined, DB is rotated into field-aligned coordinates.
;
;       dB = B - B0
;
; :Returns:
;       DB:         out, required, type=MrVectorTS object
;                   Perturbations of the magnetic field.
;-
function MrVar_BField::dB, $
CACHE=cache, $
NAME=name
	compile_opt idl2
	on_error, 2
	
	if n_elements(name) eq 0 then name = 'dB(' + self.oB.name + ')'
	
	;Get relevant data
	if ~obj_valid(self.oB)  then message, 'No magnetic field data has been loaded.'
	if ~obj_valid(self.oB0) then self -> Load_B0
	
	;Subtrack the background field
	dB = self.oB - self.oB0
	
	;Rotate to FAC
	if self.fac ne 'NONE' then begin
		oT     = self -> T_FAC()
		dB     = oT ## dB
		labels = ['||', 'perp1', 'perp2']
	endif else begin
		labels = ['x', 'y', 'z']
	endelse
	
	;Set attributes
	dB -> AddAttr, 'CATDESC',    'Perturbations from the background magnetic field.'
	dB -> AddAttr, 'LABEL',      labels
	dB -> AddAttr, 'PLOT_TITLE', 'AC magnetic field'
	dB -> AddAttr, 'UNITS',      self.oB['UNITS']
	dB -> AddAttr, 'TITLE',      'dB'
	
	return, dB
end


;+
;   Get object properties.
;
; :Keywords:
;       FAC:            out, optional, type=string
;                       Type of field-aligned coordinates to make. Options include
;                           {'ExB' | 'VxB' | 'RadAz' | ''}. Ignored if `PERP` is not given.
;-
pro MrVar_BField::GetProperty, $
FAC=fac
	compile_opt idl2
	
	catch, the_error
	if the_error ne 0 then begin
		catch, /CANCEL
		MrPrintF, 'LogErr'
		return
	endif
	
	if arg_present(fac) then fac = self.fac
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
	
	if n_elements(nSmooth) then begin
		nPts    = self -> GetNPts()
		nSmooth = 256 < (nPts/4)
	endif
	
	;Smooth the field (low-pass filter)
	data = smooth(self.oB['DATA'], [nSmooth,1], /EDGE_TRUNCATE)
	
	;Create a variable
	oB0 = MrVar_BField( self.oTime, data, $
	                    NAME = 'Detrend(' + self.name + ')', $
	                    /NO_COPY )
	
	;Set the background field
	self -> SetB0, oB0
end


;+
;   Load electric field data from source files.
;-
pro MrVar_BField::Load_E
	on_error, 2
	message, 'MrVar_BField::Load_E must be over-ridden by a subclass.'
end


;+
;   Load bulk velocity data from source files.
;-
pro MrVar_BField::Load_V
	on_error, 2
	message, 'MrVar_BField::Load_V must be over-ridden by a subclass.'
end


;+
;   Load magnetic field data from source files.
;-
pro MrVar_BField::Load
	on_error, 2
	message, 'MrVar_BField::Load must be over-ridden by a subclass.'
end


;+
;   Compute the pitch angle at which particles mirror.
;
;   References::
;       Chen, F. F. (1974), Introduction to plasma physics, book, Plenum Press. Pg. 34.
;
; :Params:
;       BM:         in, required, type=float
;                   Magnetic field strength at the mirror point.
;
; :Returns:
;       OANGLE:     out, required, type=MrScalarTS object
;                   Angle that represents the trapped-passing boundary, or loss cone angle.
;-
function MrVar_BField::MirrorAngle, Bm, $
NAME=name, $
CACHE=cache
	compile_opt idl2
	on_error, 2
	
	if n_elements(name) eq 0 then name = 'Mirror(' + self.name + ')'

	;Loss-cone angle
	Bmag  = self.oB -> Magnitude()
	sinA2 = Bmag / Bm
	
	;Set all values of |B| > Bm = NaN
	iNaN        = sinA2 -> where(1.0, /GREATER)
	sinA2[iNaN] = !values.f_nan
	angle       = asin( sqrt(sinA2['DATA']) )
	
	;Create output
	oAngle = MrScalarTS( self.oB['TIMEVAR'], angle*!radeg, $
	                     CACHE   = cache, $
	                     NAME    = name, $
	                     NO_COPY = no_copy )
	
	;Attributes
	oAngle -> AddAttr, 'CATDESC',    'Loss-cone angle'
	oAngle -> AddAttr, 'FILLVAL',    !values.f_nan
	oAngle -> AddAttr, 'PLOT_TITLE', 'Loss-cone'
	oAngle -> AddAttr, 'UNITS',      'degrees'
	oAngle -> AddAttr, 'TITLE',      'Theta'
	
	return, oAngle
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
function MrVar_BField::PoyntingFlux
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
	S_poynting = coeff * self.oE -> Cross(self)

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
function MrVar_BField::PoyntingSpectra, nfft, nshift, $
CACHE  = cache, $
NAME   = name, $
S_MIN  = s_min, $
WINDOW = win
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
	E_fft = oE -> FFT( nfft, nshift, /ONE_SIDED, WINDOW=win )
	B_fft = oB -> FFT( nfft, nshift, /ONE_SIDED, WINDOW=win )

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
	oSx['AXIS_RANGE']    = [-1,1] * max(abs(Sx[*,2:*]), /NAN)
	oSx['DEPEND_1']      = B_fft['DEPEND_1']
	oSx['CATDESC']       = 'Poynting Flux as a function of frequency'
	oSx['MISSING_VALUE'] = !values.f_nan
	oSx['PLOT_TITLE']    = 'Poynting Flux Spectrogram'
	oSx['RGB_TABLE']     = 67
	oSx['UNITS']         = '\mu W/m^2'
	oSx['TITLE']         = 'Sx'

	;Create variable and set attributes.
	oSy = MrTimeSeries( B_fft['TIMEVAR'], real_part(Sy), NAME=name + '_y', CACHE=cache )
	oSy['AXIS_RANGE']    = [-1,1] * max(abs(Sy[*,2:*]), /NAN)
	oSy['DEPEND_1']      = B_fft['DEPEND_1']
	oSy['CATDESC']       = 'Poynting Flux as a function of frequency'
	oSy['MISSING_VALUE'] = !values.f_nan
	oSy['PLOT_TITLE']    = 'Poynting Flux Spectrogram'
	oSy['RGB_TABLE']     = 67
	oSy['UNITS']         = '\mu W/m^2'
	oSy['TITLE']         = 'Sy'

	;Create variable and set attributes.
	oSz = MrTimeSeries( B_fft['TIMEVAR'], real_part(Sz), NAME=name + '_z', CACHE=cache )
	oSz['AXIS_RANGE']    = [-1,1] * max(abs(Sz[*,2:*]), /NAN)
	oSz['DEPEND_1']      = B_fft['DEPEND_1']
	oSz['CATDESC']       = 'Poynting Flux as a function of frequency'
	oSx['MISSING_VALUE'] = !values.f_nan
	oSz['PLOT_TITLE']    = 'Poynting Flux Spectrogram'
	oSz['RGB_TABLE']     = 67
	oSz['UNITS']         = '\mu W/m^2'
	oSz['TITLE']         = 'Sz'

	return, [oSx, oSy, oSz]
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
pro MrVar_BField::SetData, $
B0=b0, $
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
; BFIELD ///////////////////////////////////
;-------------------------------------------
	if n_elements(bfield) gt 0 then begin
		status = self -> CheckVar(bfield, oB, CLASS='MrVectorTS', MSG=msg, TIME=0)
		
		;Success
		if status eq 0B then begin
			self.oB = oB

			;Clear all other variables
			if obj_valid(self.oB0)       && ~oB['TIMEVAR'] -> IsIdentical(self.oB0['TIMEVAR'])       then obj_destroy, self.oB0
			if obj_valid(self.oE)        && ~oB['TIMEVAR'] -> IsIdentical(self.oE['TIMEVAR'])        then obj_destroy, self.oE
			if obj_valid(self.oV)        && ~oB['TIMEVAR'] -> IsIdentical(self.oV['TIMEVAR'])        then obj_destroy, self.oV
			if obj_valid(self.oPar_FAC)  && ~oB['TIMEVAR'] -> IsIdentical(self.oPar_FAC['TIMEVAR'])  then obj_destroy, self.oPar_FAC
			if obj_valid(self.oPerp_FAC) && ~oB['TIMEVAR'] -> IsIdentical(self.oPerp_FAC['TIMEVAR']) then obj_destroy, self.oPerp_FAC
		
		;Failure
		endif else begin
			MrPrintF, 'LogText', 'Cannot set BFIELD.'
			MrPrintF, 'LogErr', msg
		endelse
	endif

;-------------------------------------------
; B0 ///////////////////////////////////////
;-------------------------------------------
	if n_elements(b0) gt 0 then begin
		status = self -> CheckVar(b0, oB0, CLASS='MrVectorTS', MSG=msg)

		if status eq 0B then begin
			self.oB0 = oB0 
		endif else begin
			MrPrintF, 'LogText', 'Cannot set B0.'
			MrPrintF, 'LogErr', msg
		endelse
	endif

;-------------------------------------------
; EFIELD ///////////////////////////////////
;-------------------------------------------
	if n_elements(efield) gt 0 then begin
		status = self -> CheckVar(efield, oE, CLASS='MrVectorTS', MSG=msg)

		if status eq 0B then begin
			self.oE = oE
		endif else begin
			MrPrintF, 'LogText', 'Cannot set EFIELD.'
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
pro MrVar_BField::SetFAC, fac, perp
	compile_opt idl2
	
	catch, the_error
	if the_error ne 0 then begin
		catch, /CANCEL
		MrPrintF, 'LogErr'
		
		;Reset data
		self.oPar_FAC  = oPar_FAC
		self.oPerp_FAC = oPerp_FAC
		self.fac  = old_fac
		
		return
	endif
	
	;Set FAC type
	self.fac = n_elements(type) eq 0 ? 'CROSSX' : strupcase(type)
	if self.fac eq 'NONE' then return
	
	;Keep the old data
	oPar_FAC  = self.oPar_FAC
	oPerp_FAC = self.oPerp_FAC
	old_fac   = self.fac

;-----------------------------------------------------
; PERP \\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\
;-----------------------------------------------------
	if n_elements(perp) eq 0 then begin
		case self.fac of
			'EXB': begin
				if ~obj_valid(self.oE) then self -> Load_E
				perp = self.oE
			endcase
			
			'VXB': begin
				if ~obj_valid(self.oV) then self -> Load_V
				perp = self.oV
			endcase
			
			'CROSSX': ;Do nothing
			'NONE':   ;Do nothing
			else: message, 'FAC not recognized: "' + self.fac + '".'
		endcase
	endif
	
;-----------------------------------------------------
; PAR \\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\
;-----------------------------------------------------
	;Check its time tags
	if ~obj_valid(self.oB0) then self -> Load_B0
	oPar = self.oB0 -> Normalize()
	
	;Unit vector (direction, not magnitude)
	obj_destroy, self.oPar_FAC
	self.oPar_FAC = oPar

;-----------------------------------------------------
; PERP \\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\
;-----------------------------------------------------
	if n_elements(perp) gt 0 then begin
		status = self -> CheckVar(perp, oPerp, MSG=msg, CLASS='MrVectorTS')
		if status eq 0B then begin
			obj_destroy, self.oPerp_FAC
			self.oPerp_FAC = oPerp -> Normalize()
		endif else begin
			MrPrintF, 'LogText', 'Unable to set perpendicular direction.'
			MrPrintF, 'LogErr',  msg
		endelse
	endif
end


;+
;   Create a matrix that will transform a vector to field-aligned coordinate system.
;
; :Returns:
;       T:              The transformation matrix to the field-aligned coordinate system.
;-
function MrVar_BField::T_FAC
	compile_opt idl2
	on_error, 2

;-----------------------------------------------------
; Electric Field \\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\
;-----------------------------------------------------
	if self.fac eq 'EXB' then begin
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
	endif else if self.fac eq 'VXB' then begin
		;Parallel: B
		;   - oZ_hat = self.oPar_FAC
		
		;Perp-2: bxv = -vxb = e
		oY_hat = self.oPar_FAC -> Cross(self.oPerp_FAC)
		oY_hat = oY_hat -> Normalize()
		
		;Perp-1: (bxv)xb = exb
		oX_hat = oY_hat -> Cross(self.oPar_FAC)

;-----------------------------------------------------
; Radial/Azimuthal \\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\
;-----------------------------------------------------
	endif else if self.fac eq 'RADAZ' then begin
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
	endif else if self.fac eq 'CROSSX' gt 0 then  begin
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
		message, 'FAC TYPE not recognized: "' + self.fac + '".'
	endelse

;-----------------------------------------------------
; Form Rotation Matrix \\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\
;-----------------------------------------------------

	;Time dimension first
	;   - [time, component, axis]
	T = MrMatrixTS( self.oPar_FAC['TIMEVAR'], [ [[ oX_hat['DATA'] ]], $
	                                            [[ oY_hat['DATA'] ]], $
	                                            [[ self.oPar_FAC['DATA'] ]] ])

	;Return the matrix
	return, T
end


;+
;   The class definition statement.
;
; :Params:
;       CLASS:          out, optional, type=structure
;-
pro MrVar_BField__DEFINE
	compile_opt idl2
	
	class = { MrVar_BField, $
	          inherits IDL_Object, $
	          fac:       '', $
	          oB:        obj_new(), $
	          oB0:       obj_new(), $
	          oE:        obj_new(), $
	          oPar_FAC:  obj_new(), $
	          oPerp_FAC: obj_new(), $
	          oV:        obj_new() $
	        }
end