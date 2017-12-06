; docformat = 'rst'
;
; NAME:
;
;       MrVar_Polarization
;
;*****************************************************************************************
;   Copyright (c) 2017, Matthew Argall                                                   ;
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
;       The purpose of this program is to determine the polarization and other polarization
;       related parameters of a wave.
;
;       IMPORTANT:
;           Matrices are stored in the mathematical sense, not in the IDL sense. As an
;           example, the power spectral matrix, J_prime, is::
;
;               J_prime = [[Hxx, Hxy, Hyz], $
;                          [Hyx, Hyy, Hyz], $
;                          [Hzx, Hzy, Hzz]]
;
;           Thus, to access the Hxy component, one must select matrix index J_prime[1,0]
;
;   Physical Notes::
;       1) The magnetic field is assumed to be detrended and in a field-aligned coordinate
;           system wtih the mean magnetic field pointing along the z-axis. If this is not
;           the case, specify the NDETREND and/or NFAR keywords.
;       2) Samson is ultimately the reference you want, followed by Means and Rankin.
;           Fowler gives pretty much the same theoretical analysis as Rankin, but is
;           slightly more detailed when it comes to averaging over time and frequency.
;       3) Without smoothing over frequencies (i.e. NFAVG=0), the polarization at each
;           frequency will be 1. Consider a monochromatic wave whose k-vector makes an
;           angle of 40 degrees with respect to the mean magnetic field direction. 100%
;           of the monochromatic wave will be polarized at this angle. That is to say,
;           without mixing frequencies, the polarization is pure at all frequencies.
;       4) References to Percival and Barakat give the same analysis as Fowler, Rankin,
;           Means, and Samson, but use an enseble average as opposed to a time average.
;           The steps are qualitatively similar and Percival especially is enlightening,
;           but to generate an enseble, one would need to create a model and run it many
;           many times.
;       5) There are two places where NaNs are explicitly used to fill array elements::
;           a) If |B| is too small, the rotation matrix into the wave normal frame cannot
;               be properly determined.
;           b) If the trace of the covariance matrix is not sufficiently large, the wave-
;               normal direction cannot be determined properly (Applicable to the `MEANS`
;               method only).
;
;   Programming Notes::
;       * If -1 < k_dot_b < 1, then 0 < kdotb_angle < 180 and 0 < ellipticity < 1. k_dot_b
;         must be positive in order for ellipticity to be calculated properly.
;       * The same problem of 0 < ellipiticy < 1 arose when the rotation was not performed
;         properly.
;
;   References::
;
;       Barakat, R., Theory of Coherency Matrix for Light of Arbitrary Spectral Bandwidth,
;           Journal of the Optical Society of America, 53, 3, 1963.
;       Fowler, R. A., et. al, Polarization Analysis of Natural and Artificially Induced
;           Geomagnetic Micropulsations, Journal of Geophysical Research, 72, 11, 1967.
;       Means, J., Use of Three-Dimensional Covariance Matrix in Analyzing the
;           Polarization Properties of Plane Waves, Journal of Geophysical Research,
;           77, 28, 1972.
;       Percival, D. and Walden, A., Spectral Analysis for Physical Applications: Multitaper
;           and Conventional Univariate Techniques, Cambridge University Press, 1993.
;       Rankin, D., and Kurtz, R., Statistical Study of Micropulsation Polarization,
;           Journal of Geophysical Research, 75, 28, 1970.
;       Samson, J. C., and Olson, J. V., Some comments on the description of polarization
;           states of waves, Geophys J. R. Astr. Soc., 115-119, 61, 1980.
;
; :Categories:
;
;       Math Utility, Polarization
;
; :Examples:
;   See the test program "test_mrpolarization.pro"
;
; :Uses:
;   Uses the following external programs::
;       nfft_intervals.pro
;       fft_freqs.pro
;       logspace.pro
;       linspace.pro
;       rotate_matrix.pro
;       MrDetrendRotate.pro
;       undefine.pro
;
; :Author:
;   Matthew Argall::
;       University of New Hampshire
;       Morse Hall, Room 348
;       8 College Rd.
;       Durham, NH, 03824
;       matthew.argall@unh.edu
;
; :History::
;   Modification History::
;       2017/05/06  -   Written by Matthew Argall
;-
;***************************************************************************************
;+
;   The purpose of this function is to calculate the wave-normal direction by using
;   the imaginary, anti-symmetric part of the spectral matrix, as done in Means.
;
; :Private:
;
; :Params:
;
;       J_PRIME:                in, required, type=3x3xN numeric
;                               Spectral matrix in the frame of the observer.
;       IPOL:                   out, required, type=long
;                               Indices of the frequencies that are polarized
;       ILIN:                   out, required, type=long
;                               Indices of the frequencies that are linear (unpolarized)
;
; :Keywords:
;       K_VEC:                  out, optional, type=3xN numeric
;                               The 3 component k vector.
;
; :Returns:
;
;       K_HAT:                  The wave normal direction at each frequency.
;-
FUNCTION MrVar_Polarization_Means, J_prime, iPol, iLin, nPol, nLin, $
K_VEC = k_vec
	Compile_Opt idl2, hidden
	On_Error, 2

	;Allocate memory
	J_prime_dims = Size(J_prime, /DIMENSIONS)
	npts         = J_prime_dims[0]
	nfreqs       = J_prime_dims[1]
	ab           = FltArr(npts, nfreqs)
	k_hat        = FltArr(npts*nfreqs, 3)
	IF Arg_Present(k_vec) THEN k_vec = FltArr(npts*nfreqs, 3)

;-----------------------------------------------------
; Wave Normal Direction \\\\\\\\\\\\\\\\\\\\\\\\\\\\\\
;-----------------------------------------------------
	;The wave normal direction can now be determined by examining the imaginary part of
	;the spectral density matrix (Means pg. 5554, Bakarat EQ 3.19).

	;Using the imaginary components.
	;a*b = HxHy^2 + HzHx^2 + HyHz^2
	ab = Sqrt( Imaginary(J_prime[*,*,1,0])^2 + $     ;kz^2    = HxHy^2
	           Imaginary(J_prime[*,*,2,0])^2 + $     ;(-ky)^2 = HxHz^2
	           Imaginary(J_prime[*,*,2,1])^2 )       ;kx^2    = HyHz^2

	;The imaginary part of the power spectral matrix for linearly polarized waves
	;is the 0-matrix. Thus, the analysis that follows cannot be undertaken. (Means
	;pg. 5555-6). Treat them separately later.
	;   - The minimum value of AB depends on the sampling interval, dt
	;   - A tolerance of 1e-6 can be too large...
	iLin  = Where(ab LE 0, nLin, COMPLEMENT=iPol, NCOMPLEMENT=nPol)

	;ILIN and IPOL are 1D subscripts into a 2D vector. Reform J_prime so that subscript-
	;rounding does not occur (e.g. a = findgen(10) ... print, a[[20]]).
	J_prime = Reform(J_prime, npts*nfreqs, 3, 3, /OVERWRITE)

;-----------------------------------------------------
; Linear Polarization \\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\
;-----------------------------------------------------
	IF nLin GT 0 THEN BEGIN
		;The power of the linearly polarized wave
		;Tr[J'] = a^2 = Jxx + Jyy + Jzz   --   (J is purely real)
		a2 = Real_Part(J_prime[iLin,0,0] + J_prime[iLin,1,1] + J_prime[iLin,2,2])
	
		;If there is no power at the frequency
		i_NoPower = Where(a2 LT 1e-5, n_NoPower, COMPLEMENT=iPower, NCOMPLEMENT=nPower)
		IF n_NoPower GT 0 THEN BEGIN
			;There is no direction of propagation.
			k_hat[iLin[i_NoPower],*] = Replicate(!values.f_nan, 3, n_NoPower)
		ENDIF
		
		;Otherwise
		IF nPower GT 0 THEN BEGIN
			;Calculate the direction of the linearized wave
			Lx = Sqrt(Real_Part(J_prime[iLin[iPower],0,0]) / a2[iPower])    ;(Jxx / a^2)^(1/2)
			Ly = Real_Part(J_prime[iLin[iPower],1,0]) / (a2[iPower] * Lx)   ;Jxy / (a^2 * Lx)
			Lz = Real_Part(J_prime[iLin[iPower],2,0]) / (a2[iPower] * Lx)   ;Jxz / (a^2 * Lx)
		
			;Store the direction of propagation
			k_hat[iLin[iPower],*] = [[Lx], [Ly], [Lz]]
		ENDIF
	ENDIF

;-----------------------------------------------------
; Polarized Waves \\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\
;-----------------------------------------------------
	IF nPol GT 0 THEN BEGIN
		;Return the k-vector?
		IF Arg_Present(k_vec) THEN BEGIN
			k_vec[iPol,2] =  Imaginary(J_prime[iPol,1,0])
			k_vec[iPol,1] = -Imaginary(J_prime[iPol,2,0])
			k_vec[iPol,0] =  Imaginary(J_prime[iPol,2,1])
		ENDIF
	
		;Note that dividing by "ab" normalizes the components.
		k_hat[iPol,2] =  Imaginary(J_prime[iPol,1,0]) / ab[iPol]   ;kz =  HxHy / (a*b)
		k_hat[iPol,1] = -Imaginary(J_prime[iPol,2,0]) / ab[iPol]   ;ky = -HxHz / (a*b)
		k_hat[iPol,0] =  Imaginary(J_prime[iPol,2,1]) / ab[iPol]   ;kx =  HyHz / (a*b)
	ENDIF

;-----------------------------------------------------
; Finish Up \\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\
;-----------------------------------------------------
	;
	;This anaylsis has been done under the assumptions of a counter-clockwise,
	;elliptically polarized plane wave. Thus, a clockwise polarized wave has k_hat < 0
	;
	
	J_prime = Reform(J_prime, npts, nfreqs, 3, 3, /OVERWRITE)
	
	RETURN, k_hat
END


;+
;   The purpose of this function is to calculate the wave-normal direction by using
;   the real, symmetric part of the covariance matrix (the cross-spectral matrix). This
;   involves diagonalizing the real part of the covariance matrix and using the
;   vector associated with the minimum eigenvalue as the wave normal directin (i.e. we
;   obtain the wave-normal vector via a minimum variance approach, as is often stated in
;   the literature).
;
;   :Private:
;
;   :Params:
;
;       J_PRIME:                in, required, type=3x3xN numeric
;                               The real, symmetric part of the Spectral matrix in the
;                                   frame of the observer.
;
;   :Returns:
;
;       K_HAT:                  The wave normal direction at each frequency.
;-
FUNCTION MrVar_Polarization_Fowler, J_prime
	Compile_Opt idl2, hidden
	On_Error, 2

	;The wave normal direction can now be determined by examining either the real
	;part OF the spectral density matrix (Fowler, pg. 2873-4, Means pg. 5554, 
	;Bakarat EQ 3.19).

	J_prime_dims = Size(J_prime, /DIMENSIONS)
	npts = J_prime_dims[2]
	nfreqs = J_prime_dims[3]
	k_hat = FltArr(3, npts, nfreqs)

	;Diagonalizing the covariance matrix
	FOR i = 0, npts - 1 DO BEGIN
		FOR j = 0, nfreqs-1 DO BEGIN
			;Calculate the eigenvectors OF the covariance matrix. The wave-normal direction
			;points along the direction OF minimum variance, i.e. the direction associated
			;with the smallest eigenvalue
			eigvals = EigenQL(J_prime[*,*,i,j], /ABSOLUTE, /ASCENDING, EIGENVECTORS=eigvecs)
			k_hat[*,i,j] = eigvecs[*,0]
		ENDFOR
	ENDFOR

	RETURN, k_hat
end


;+
;
;   The purpose of this program is to determine the polarization and other polarization
;   related parameters of a wave.
;
;   IMPORTANT:
;       Matrices are stored in the mathematical sense, not in the IDL sense. As an
;       example, the power spectral matrix, J_prime, is::
;
;           J_prime = [[Hxx, Hxy, Hyz], $
;                      [Hyx, Hyy, Hyz], $
;                      [Hzx, Hzy, Hzz]]
;
;       Thus, to access the Hxy component, one must select matrix index J_prime[1,0]
;
; :Params:
;       VAR:                in, required, type=integer/string/objref
;                           The name, number, of object reference of a MrVectorTS variable
;                               for which polarization parameters are computed.
;       NFFT:               in, optional, type=int, default=N_ELEMENTS(DATA)
;                           The number of points to use per FFT
;       DT:                 in, optional, type=float. default=1
;                           The time between data samples. If not present, unit spacing
;                               is assumed.
;       NSHIFT:             in, optional, type=int. default=NFFT/2
;                           The number of points to shift ahead after each FFT.
;
; :Keywords:
;       COHERENCY:          out, optional, type=NxM float
;                           Coherency of the wave in the wave-normal frame. 0 indicates
;                               that the waves are incoherent while a value of 1 indicates
;                               complete coherence.
;       ELLIPTICITY:        out, optional, type=NxM float
;                           Ellipticity of the wave in the wave-normal frame. Counter-
;                               clockwise if arg(Jxy) > 0.
;       INTENSITY:          out, optional, type=NxM float
;                           Intensity of the wave in the wave-normal system.
;       K_VEC:              out, optional, type=3xM float
;                           The 3 component k-vector.
;       K_HAT:              out, optional, type=3xM float
;                           A unit vector pointing in the wave-normal direction.
;       KDOTB_HAT:          out, optional, type=NxM float
;                           The angle between the wave normal direction and the mean
;                               magnetic field.
;       METHOD:             in, optional, type=int, default=1
;                           Two methods have been incorporated (with a third on the way).
;                               Method::
;                                   1 - Means, uses complex part of the covariance matrix
;                                        to determine the wave normal direction directly.
;                                   2 - Fowler diagonalizes the real part of the wave
;                                        normal direction and uses the minimum eigenvalue
;                                        as the wave-normal direction
;                                  [3]- Samson expands the covariance matrix in an ortho-
;                                        normal basis and computes the polarization
;                                        directly, without need of the wave-normal frame.
;       POLANGLE:           out, optional, type=NxM float
;                           Angle of polarization, i.e. the angle between the wave-normal
;                               frame's x-axis and the major axis of the polarization
;                               ellipse.
;       POLARIZATION:       out, optional, type=3x3xNxM float
;                           The polarization state of the wave.
;       SPECTRAL_MATRIX:    out, optional, type=3x3xNxM float
;                           The power spectral matrix of the waveform in the reference
;                               frame of the wave.
;       WAVE_NORMAL_MATRIX: out, optional, type=2x2xNxM float
;                           The power spectral matrix in the wave-normal frame.
;       _REF_EXTRA:         in, optional, type=any
;                           Any keyword accepted by MrVectorTS::FFT is also accepted here
;                               via keyword inheritance.
;
; :Returns:
;-
PRO MrVar_Polarization, var, nfft, nshift, $
;Options
CACHE = cache, $
METHOD = method, $
NFAVG = nfavg, $
VARNAMES = varnames, $
;Data Outputs
COHERENCY = oCoherency, $
ELLIPTICITY = oEllipticity, $
INTENSITY = oIntensity, $
K_HAT = oK_hat, $
K_VEC = oK_vec, $
KDOTB_ANGLE = oK_dot_B, $
POLANGLE = oPolAngle, $
POLARIZATION = oPolarization, $
SPECTRAL_MATRIX = oSM, $
WAVE_NORMAL_MATRIX = oWaveNormalMatrix, $
;Extras
_REF_EXTRA = extra
	Compile_Opt idl2

	;Error handling
	Catch, the_error
	IF the_error NE 0 THEN BEGIN
		Catch, /CANCEL
		MrPrintF, 'LogErr'
		RETURN
	ENDIF

;-----------------------------------------------------
;Check Inputs \\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\
;-----------------------------------------------------
	;Create defaults.
	IF N_Elements(nfft)    EQ 0 THEN nfft    = Floor(N_Elements(data) / 4)
	IF N_Elements(n_shift) EQ 0 THEN n_shift = Floor(nfft/2)
	IF N_Elements(nfavg)   EQ 0 THEN nfavg   = 5
	IF N_Elements(method)  EQ 0 THEN method  = 1

	;Must be method 1 OR 2
	IF method LT 1 || method GT 2 THEN Message, 'Invalid method. METHOD = {1 | 2}.'

	;Make sure NFAVG is odd AND >= 3
	IF (nfavg MOD 2) EQ 0 || nfavg LT 3 $
		THEN Message, 'NFAVG must be an odd integer >= 3.'
	
	;Keep certain parameters?
	keep_polarization    = Arg_Present(oPolarization)
	keep_coherency       = Arg_Present(oCoherency)
	keep_intensity       = Arg_Present(oIntensity)
	keep_ellipticity     = Arg_Present(oEllipticity)
	keep_spectral_matrix = Arg_Present(oSM)
	keep_wave_normal     = Arg_Present(oWaveNormalAngle)
	keep_angle           = Arg_Present(oPolAngle)
	keep_kvec            = Arg_Present(oK_vec)
	keep_khat            = Arg_Present(oK_hat)
	keep_kdotb_angle     = Arg_Present(oK_dot_B)

;-----------------------------------------------------
; Verify the data \\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\
;-----------------------------------------------------
	;Grab the variable
	oVar = MrVar_Get(var)
	IF ~Obj_IsA(oVar, 'MrVectorTS') THEN Message, 'VAR must be the name, number, or objref of MrVectorTS variable.'
	oTempVar = oVar -> Copy()

	;Must hanlde the fill value before detrending AND taking the FFT
	;   - MrDetrendRotate will skip over NaN values when computing the background field
	;       x Replace the fill value with NaN
	IF oVar -> HasAttr('FILLVAL') && oVar['FILLVAL'] NE !Values.F_NaN THEN BEGIN
		iBad = oTempVar -> Where(oTempVar['FILLVAL'], /EQUAL, COUNT=nBad)
		IF nBad GT 0 THEN oTempVar[iBad] = !Values.F_NaN
	ENDIF

;-----------------------------------------------------
; I. Take the FFT \\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\
;-----------------------------------------------------
	;Compute the FFT OF the data
	oFFT = oTempVar -> FFT( nfft, nshift, $
	                        /ONE_SIDED, $
	                        _STRICT_EXTRA = extra )
	
	;The DC component cannot be polarized
	oFFT = oFFT[*,1:*,*]
	
	;Dimension sizes
	data_size = Size(oFFT, /DIMENSIONS)
	npts      = data_size[0]
	nfreqs    = data_size[1]
	ncomps    = data_size[2]
	
	;Free memory
	Obj_Destroy, oTempVar

;-----------------------------------------------------
; Allocate Memory \\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\
;-----------------------------------------------------

	;Allocate memory to the polarization parameters' arrays
	IF keep_coherency       THEN coherency          = FltArr(npts*nfreqs)
	IF keep_intensity       THEN intensity          = FltArr(npts,nfreqs)
	IF keep_ellipticity     THEN ellipticity        = FltArr(npts*nfreqs)
	IF keep_angle           THEN polarization_angle = FltArr(npts*nfreqs)
	IF keep_kvec            THEN k_vec              = FltArr(npts*nfreqs, 3)
	IF keep_spectral_matrix THEN spectral_matrix    = ComplexArr(npts, nfreqs, 3, 3)
	IF keep_wave_normal     THEN wave_normal_matrix = ComplexArr(npts, nfreqs, 2, 2)
	IF keep_kdotb_angle     THEN kdotb_angle        = FltArr(npts, nfreqs)
	pzation = FltArr(npts*nfreqs)

	;Variables
	J_prime = ComplexArr(npts, nfreqs, 3, 3)
	J       = ComplexArr(npts, nfreqs, 3, 3)
	Js      = ComplexArr(npts, nfreqs, 3, 3)
	k_hat   = FltArr(npts, nfreqs, 3)
	k_dot_b = FltArr(npts, nfreqs)
	R       = FltArr(npts, nfreqs, 3, 3)

;-----------------------------------------------------
; II. Covariance Spectral Matrix \\\\\\\\\\\\\\\\\\\\\
;-----------------------------------------------------

	;Due to the Hermetian nature of the spectral denstiy matrix (EQ 3.18 Bakarat), 
	;Means says,
	;
	;   The analytic representation of the real signal is easily obtained by 
	;   multiplying the  Fourier transform of the real signal by the Heaviside step
	;   FUNCTION 
	;
	;       S(f) = 0        f <  0
	;       S(f) = 1        f >= 0
	;
	;   AND doubling the result.
	;
	; For a purely real time-series signal, the backwards frequencies will have the
	; same phase AND amplitude as the forward frequencies. Thus the total power
	; can be computed as twice the power of the forward frequencies.

	;Using Means' notation, calculate the spectral density matrix. The "prime"
	;indicates that the spectral density matrix is NOT in the principle axis system.
	;Dividing by the duration of the interval converts from Energy to Power.
	J_prime[0,0,0,0] = 2.0 * oFFT['DATA',*,*,0] * Conj(oFFT['DATA',*,*,0])      ;HxHx
	J_prime[0,0,1,0] = 2.0 * oFFT['DATA',*,*,0] * Conj(oFFT['DATA',*,*,1])      ;HxHy
	J_prime[0,0,2,0] = 2.0 * oFFT['DATA',*,*,0] * Conj(oFFT['DATA',*,*,2])      ;HxHz
	J_prime[0,0,0,1] = 2.0 * oFFT['DATA',*,*,1] * Conj(oFFT['DATA',*,*,0])      ;HyHx
	J_prime[0,0,1,1] = 2.0 * oFFT['DATA',*,*,1] * Conj(oFFT['DATA',*,*,1])      ;HyHy
	J_prime[0,0,2,1] = 2.0 * oFFT['DATA',*,*,1] * Conj(oFFT['DATA',*,*,2])      ;HyHz
	J_prime[0,0,0,2] = 2.0 * oFFT['DATA',*,*,2] * Conj(oFFT['DATA',*,*,0])      ;HzHx
	J_prime[0,0,1,2] = 2.0 * oFFT['DATA',*,*,2] * Conj(oFFT['DATA',*,*,1])      ;HzHy
	J_prime[0,0,2,2] = 2.0 * oFFT['DATA',*,*,2] * Conj(oFFT['DATA',*,*,2])      ;HzHz

	;This is the spectral density matrix [covariance matrix in the frequency domain]
	;(c.f. "Technique AND Theory" AND EQ 1 in Means OR EQ 3.18 in Bakarat). IDL notation
	;is used ... J_prime[col, row, time, frequency]

;-----------------------------------------------------
; III. Wave Normal Direction \\\\\\\\\\\\\\\\\\\\\\\\\
;-----------------------------------------------------

	;Means?
	IF method EQ 1 THEN BEGIN
		;Keep the non-normalized wave vector?
		IF keep_kvec THEN BEGIN
			k_hat = MrVar_Polarization_Means(J_prime, ipol, ilin, npol, nlin, K_VEC=k_vec)
		
		ENDIF ELSE k_hat = MrVar_Polarization_Means(J_prime, ipol, ilin, npol, nlin)

	;Fowler?
	ENDIF ELSE IF method EQ 2 THEN BEGIN
		k_hat = MrVar_Polarization_Fowler(Real_Part(J_prime))
		ipol = LIndGen(nfreqs)
		npol = nfreqs
		nlin = 0

	;Samson?
	ENDIF ELSE IF method EQ 3 THEN BEGIN
		Message, 'The Samson method has not yet been implemented.'
	ENDIF

;-----------------------------------------------------
; IV. Wave-Normal System \\\\\\\\\\\\\\\\\\\\\\\\\\\\\
;-----------------------------------------------------

	;Polarization is always referenced to the magnetic field direction. A wave is
	;right-hand polarized if it rotates in a counter-clockwise direction when looking
	;down B_hat, and left-handed if it rotates clockwise. 

	;Now, the covariance spectral matrix must be rotated into a frame in which the
	;wave normal direction is along one of the principal axes (the z-axis). Then,
	;choosing x-direction to be in the k_hat dot B_hat plane, for elliptiicty
	;measurement purposes, and the y-axis to complete the system. (Means, pg. 5555)

	;
	;Means says:
	;
	;  It is important to note that as a result of the original assumptions this is the wave
	;  normal direction for a counterclockwise elliptically polarized plane wave. Since the
	;  sense of polarization of the wave changes if we let k' = k, this fact poses no real
	;  restriction on the analysis. if a wave with a counterclockwise rotation as a wave
	;  normal vector k, a wave with the same wave normal vector but with clockwise rotation
	;  will, in the above analysis, result in a wave normal vector -k.
	;
	;  ...For right-handed waves, (k dot B) > 0, whereas, for left-handed waves,
	;  (k dot B) < 0.
	;
	;Therefore, in the new coordinate system, (x', y', z'), the angle that the z'-axis
	;makes with B must lie between 0 and 90 degrees so that left-hand polarized waves,
	;which have a -k_hat normal vector, DO not inadvertantly get turned into right-hand
	;polarized waves by allowing k_hat to lie always along the z'-axis.
	;
	;As such we will create the z'-axis by taking k-hat and, wherever (k dot B) < 0,
	;let z' = -z'.
	;
	;Now, because (k dot B) > 0 = right-hand polarized and (k dot B) < 0 is left-hand
	;polarized, we really only know the direction of k with respect to the mangetic field
	;to within +/- 90 degrees. As such, the kdotb_angle will be the angle between the
	;z'-axis and B.
	;

	;The mean field is along the z-axis, so dotting k with [0,0,1] gives kz
	;   - B is suppose to be rotated so that the average field is along the z-axis.
	;   - Therefore, k \cdot B_hat = kz
	k_dot_b = k_hat[*,2]

	;Make sure that the new z-hat direction is always parallel to B
	z_prime_hat = k_hat
	iAntiPar    = Where(k_dot_b LT 0, nAntiPar)
	IF nAntiPar GT 0 THEN BEGIN
		z_prime_hat[iAntiPar,*] = -z_prime_hat[iAntiPar,*]
		k_dot_b                 = z_prime_hat[*,2]
	ENDIF

	;Reform the vectors
	k_hat       = Reform(k_hat,       npts, nfreqs, 3)
	k_dot_b     = Reform(k_dot_b,     npts, nfreqs)
	z_prime_hat = Reform(z_prime_hat, npts, nfreqs, 3)

	;Put the wave normal vector along the z-hat direction. Make "y" be in the plane
	;perpendicular to k_hat AND B_hat, THEN complete the system to make "x" such that
	;"x" lies in the k-B plane.
	;
	;Loop over either time OR frequency. Functions used below work FOR 3xN arrays,
	;not 3xNxM.
	FOR i = 0, npts - 1 DO BEGIN
		R[i,*,*,2] = z_prime_hat[i,*,*]

		;If k // B, THEN y_hat = k_hat x [0,0,1] = 0. Check FOR this by finding where the
		;magnitude OF y_hat = k_hat x [0,0,1] < 1e-6. In these CASE, use y_hat = [0,1,0].
		y_component = MrVector_Cross(Reform(k_hat[i,*,*]), [0.0, 0.0, 1.0])
		iNegK = Where(MrVector_Magnitude(y_component) LT 1e-6 AND k_hat[i,*,2] LT 0, nNegK)
		iPosK = Where(MrVector_Magnitude(y_component) LT 1e-6 AND k_hat[i,*,2] GT 0, nPosK)
		IF nNegK GT 0 THEN y_component[iNegK,*] = Rebin([[0.0],[-1.0], [0.0]], nNegK, 3)
		IF nPosK GT 0 THEN y_component[iPosK,*] = Rebin([[0.0], [1.0], [0.0]], nPosK, 3)

		;x- AND y- components OF the transformation matrix.
		R[i,*,*,1] = MrVector_Normalize(y_component)
		R[i,*,*,0] = MrVector_Normalize(MrVector_Cross(Reform(R[i,*,*,1]), Reform(R[i,*,*,2])))

		;The wave-normal matrix (R) just created is the transformation matrix required
		;to transform the current spectral density matrix (J') into the wave-normal
		;system (J) via the rotation J = R J' R^T.

		;Calculating the real AND imaginary parts separately,
		;NOTE: It might be pertinent to set the diagonal components OF J_prime equal to
		;      zero explicily (c.f. Means pg. 5555).
		J[i,*,*,*] = Complex(MrMatrix_Rotate(Reform(R[i,*,*,*]), Reform(Real_Part(J_prime[i,*,*,*]))), $
		                     MrMatrix_Rotate(Reform(R[i,*,*,*]), Reform(Imaginary(J_prime[i,*,*,*]))))
	ENDFOR
	
	R = !Null

;-----------------------------------------------------
; V. Average Over Frequency Bins \\\\\\\\\\\\\\\\\\\\\
;-----------------------------------------------------
	;Without smoothing over frequencies (i.e. NSMOOTH=0), the polarization at each
	;frequency will be 1. Consider a monochromatic wave whose k-vector makes an
	;angle OF 40 degrees with respect to the mean magnetic field direction. 100%
	;OF the monochromatic wave will be polarized at this angle. That is to say,
	;without mixing frequencies, the polarization is pure at all frequencies.

	;The center OF the averaging interval
	nfavg_half = (nfavg-1)/2

	;step through all OF the averaging intervals.
	IDLversion8 = MrCmpVersion('8.0')
	FOR k = 0, nfreqs - nfavg DO BEGIN
		;Calculate index FOR the center, start, AND end frequencies
		icenter_bin = k + nfavg_half
		istart      = icenter_bin - nfavg_half
		iend        = icenter_bin + nfavg_half

		;Average of the frequency inteval (Js == J smoothed over frequency)
		;   The DIMENSION keyword was added to MEAN in version 8.0.
		;   - Without the /NaN flag, NaNs introduced in _Means when Power=0 will smear
		;       to all frequencies.
		IF IDLversion8 LE 0 THEN BEGIN
			Js[*,icenter_bin,*,*] = Mean(J[*,istart:iend,*,*], DIMENSION=2)
		
			;k dot B
			IF keep_kdotb_angle THEN BEGIN
				iNaN = Where(Total(Finite(k_dot_b[*,istart:iend]), 2) EQ 0, nNaN, COMPLEMENT=iGood, NCOMPLEMENT=nGood)
				IF nGood GT 0 THEN k_dot_b[iGood,icenter_bin] = Mean(k_dot_b[iGood,istart:iend], DIMENSION=2, /NAN)
				IF nNaN  GT 0 THEN k_dot_b[iNaN,icenter_bin] = !values.f_nan
			ENDIF
		
			;k-hat
			IF keep_khat THEN BEGIN
				iNaN = Where(Total(Finite(k_hat[*,istart:iend,*]), 2) EQ 0, nNaN, COMPLEMENT=iGood, NCOMPLEMENT=nGood)
				IF nGood GT 0 THEN k_hat[iGood,icenter_bin,*] = Mean(k_hat[iGood,istart:iend,*], DIMENSION=2, /NAN)
				IF nNaN  GT 0 THEN k_hat[iNaN,icenter_bin,*] = !values.f_nan
			ENDIF
		ENDIF ELSE BEGIN
			Js[*,icenter_bin,*,*] = cmapply('USER:MEAN', J[*,istart:iend,*,*], 2)
			IF keep_kdotb_angle THEN k_dot_b[*,icenter_bin] = cmapply('USER:MEAN', k_dot_b[*,istart:iend], 2)
			IF keep_khat        THEN k_hat[*,icenter_bin,*] = cmapply('USER:MEAN', k_hat[*,istart:iend,*], 2)
		ENDELSE
	ENDFOR
	undefine, J

	;Truncate the end points that were not included in the average over frequencies.
	;ipol may not contain all of the indices, so must search for them.
	if0 = nfavg_half
	if1 = nfreqs-nfavg_half-1
	
	temp_arr = lindgen(npts, nfreqs)
	temp_arr = temp_arr[*, if0:if1]
	void     = MrIsMember(temp_arr, iPol, these_iPol)
	iPol     = iPol[these_iPol]
	undefine, temp_arr

;-----------------------------------------------------
; VI. What to Keep? \\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\
;-----------------------------------------------------
	;Time
	oTime = oFFT['TIMEVAR']
	
	;Frequencies
	oFreqs = oFFT['DEPEND_1']
	oFreqs = oFreqs[if0:if1]
	oFreqs['AXIS_RANGE'] = [oFreqs['DATA',0], oFreqs['DATA',-1]]
	
	;SPECTRAL MATRIX
	IF keep_spectral_matrix THEN BEGIN
		;Variable
		oSM = MrTimeSeries( oTime, J_prime[*,if0:if1,*,*], $
		                    CACHE = cache, $
		                    NAME  = 'SpectralMatrix(' + oVar.name + ')' )
		
		;Attributes
		oSM['DEPEND_1']   = oFreqs
		oSM['LOG']        = 1B
		oSM['PLOT_TITLE'] = 'Spectral Matrix'
		oSM['TITLE']      = 'Spectral Matrix'
		IF oVar -> HasAttr('UNITS') THEN oSM['UNITS'] = '(' + oVar['UNITS'] + ')^2/Hz'
	ENDIF
	J_prime = !Null

	;K-HAT
	IF keep_khat THEN BEGIN
		;Variable
		oK_hat = MrTimeSeries( oTime, k_hat[*,if0:if1,*], $
		                       CACHE = cache, $
		                       NAME  = 'K_Hat(' + oVar.name + ')' )
		
		;Attributes
		oK_hat['AXIS_RANGE']   = [-1.0, 1.0]
		oK_hat['DEPEND_1']   = oFreqs
		oK_hat['PLOT_TITLE'] = 'Wave Normal Vector'
		oK_hat['TICKINTERVAL'] = 0.5
		oK_hat['TITLE']      = 'k'
		oK_hat['UNITS']      = ''
	ENDIF
	k_hat = !Null

	;WAVE-NORMAL ANGLE
	IF keep_kdotb_angle THEN BEGIN
		;Variable
		oK_dot_B = MrTimeSeries( oTime, ACos(k_dot_b[*,if0:if1])*!radeg, $
		                         CACHE = cache, $
		                         NAME  = 'WaveNormalAngle(' + oVar.name + ')' )
		
		;Attributes
		oK_dot_B['AXIS_RANGE']   = [0, 90]
		oK_dot_B['DEPEND_1']     = oFreqs
		oK_dot_B['PLOT_TITLE']   = 'Wave Normal Angle'
		oK_dot_B['RGB_TABLE']    = 64
		oK_dot_B['SCALE']        = 1B
		oK_dot_B['TICKINTERVAL'] = 45
		oK_dot_B['TITLE']        = '$\theta$$\downk$!C(deg)'
		oK_dot_B['UNITS']        = 'degrees'
	ENDIF

;-----------------------------------------------------
; VI. Polarization Parameters \\\\\\\\\\\\\\\\\\\\\\\\
;-----------------------------------------------------
	;Now we switch to the paper by Rankin and Kurtz (1970) to get the polarization
	;parameters.

	;Start with the "General Case" and note that "J" in Rankin and Kurts
	;corresponds to the 2x2 x-y submatrix in our J.

	;Recall that linearly polarized waves do not work for this analysis. Their
	;spectral power lies only in the Jzz component, leaving Jxx, Jxy, Jyx, Jyy = 0.
	;This causes floating point errors.

	;Js must be reformed to a [3,3,npts*nfreqs] array in order for IPOL and ILIN to index
	;properly. They are 1D indieces into a 2D array.
	Js = Reform(Js, npts*nfreqs, 3, 3)

	;For a wave traveling along the z-axis with perturbations in the xy-plane,
	;   Hx = A cos(theta) + i A sin(theta)
	;   Hy = B cos(theta) + i B sin(theta)
	;
	;In this wave normal frame, the spectral matrix is
	;        | HxHx*  HxHy*  0 |
	;   Js = | HyHx*  HyHy*  0 |
	;        |   0      0    0 |
	;
	;Not that the diagonal elements are real
	;   HxHx* = A^2
	;   HyHy* = B^2
	;
	;Because of this, we set the imaginary component explicitly to zero. Without doing so,
	;floating point underflow errors occur in the calculation of det(Js).
	Js[*,0,0] = Complex(Real_Part(Js[*,0,0]))
	Js[*,1,1] = Complex(Real_Part(Js[*,1,1]))
	
	;Determinant
	det_Js = Real_Part(Js[*,0,0]*Js[*,1,1] - Js[*,1,0]*Js[*,0,1])

	;A) DEGREE OF POLARIZATION
	IF keep_polarization THEN BEGIN
		;   |J| = (Jxx * Jyy) - (Jxy * Jyx)
		; => plrz = { 1 - [ (4*|J|) / (Jxx + Jyy)^2 ] }^(1/2)
		IF nPol GT 0 THEN pzation[iPol] = Sqrt( 1 - ((4*det_Js[iPol]) / Real_Part(Js[iPol,0,0] + Js[iPol,1,1])^2) )
		IF nLin GT 0 THEN pzation[iLin] = !values.f_nan
		
		;Variable
		pzation = Reform(pzation, npts, nfreqs)
		oPolarization = MrTimeSeries( oTime, pzation[*, if0:if1], $
		                              CACHE = cache, $
		                              NAME  = 'Polarization(' + oVar.name + ')' )
		
		;Attributes
		oPolarization['AXIS_RANGE']   = [0, 1]
		oPolarization['DEPEND_1']     = oFreqs
		oPolarization['PLOT_TITLE']   = 'Percent Polarization'
		oPolarization['RGB_TABLE']    = 54
		oPolarization['TICKINTERVAL'] = 0.5
		oPolarization['TITLE']        = '% Polarization'
		oPolarization['UNITS']        = ''
	ENDIF
	pzation = !Null

	;B) COHERENCY
	IF keep_coherency THEN BEGIN
		;C = |Jxy| / sqrt( Jxx*Jyy )
		;  = sqrt( Jxy*Jxy / (Jxx*Jyy) )
		IF nPol GT 0 THEN coherency[ipol] = Sqrt( Real_Part(Js[iPol,0,1]*Js[iPol,1,0]) / $
		                                          Real_Part(Js[iPol,0,0]*Js[iPol,1,1]) )
		IF nLin GT 0 THEN coherency[iLin] = !values.f_nan
		coherency = Reform(coherency, npts, nfreqs)
		
		;Variable
		oCoherency = MrTimeSeries( oTime, coherency[*, if0:if1], $
		                           CACHE = cache, $
		                           NAME  = 'Coherency(' + oVar.name + ')' )
		
		;Attributes
		oCoherency['AXIS_RANGE']   = [0, 1]
		oCoherency['DEPEND_1']     = oFreqs
		oCoherency['PLOT_TITLE']   = 'Coherency'
		oCoherency['RGB_TABLE']    = 54
		oCoherency['TICKINTERVAL'] = 0.5
		oCoherency['TITLE']        = 'Coherency'
		oCoherency['UNITS']        = ''
	ENDIF
	coherency = !Null

	;C) ANGLE OF POLARIZATION
	;       This is the angle between the x-axis of frame R and the major axis of the
	;       polarization ellipse. It is NOT the angle between k and B.
	IF keep_angle THEN BEGIN
		;tan(2*Theta') = 2*Re[Jxx] / (Jxx - Jyy)
		; => Theta' = 0.5 * atan( 2*Re[Jxx] / (Jxx - Jyy) )
		IF nPol GT 0 THEN BEGIN
			tan2Theta = 2*Real_Part(Js[iPol,0,0]) / $
		                Real_Part(Js[iPol,0,0] - Js[iPol,1,1])
			polarization_angle[iPol] = 0.5 * ATan(tan2Theta) * !radeg
		ENDIF

		IF nLin GT 0 THEN polarization_angle[iLin] = !values.f_nan
		polarization_angle = Reform(polarization_angle, npts, nfreqs)
		
		;Variable
		oPAngle = MrTimeSeries( oTime, polarization_angle[*, if0:if1], $
		                        CACHE = cache, $
		                        NAME  = 'PolarizationAngle(' + oVar.name + ')' )
		
		;Attributes
		oPolAngle['AXIS_RANGE']   = [-180.0, 180.0]
		oPolAngle['CATDESC']      = 'The angle between the x-axis of frame R and the major axis ' + $
		                          'of the polarization ellipse.'
		oPolAngle['DEPEND_1']     = oFreqs
		oPolAngle['PLOT_TITLE']   = 'Angle of Polarization'
		oPolAngle['RGB_TABLE']    = 70
		oPolAngle['TICKINTERVAL'] = 90.0
		oPolAngle['TITLE']        = '$\theta$$\downP$'
		oPolAngle['UNITS']        = 'degrees'
	ENDIF
	polarization_angle = !Null

	;D) ELLIPTICITY
	;       +1 is left-hand polarized
	;       -1 is right-hand polarized
	IF keep_ellipticity THEN BEGIN
		;e = tan(B)
		;sin(2B) = 2*Im[Jxy] / sqrt( (Jxx + Jyy)^2 - 4*|J| )
		;  =>   e = tan(0.5 * asin[sin(2B)])
		;Note that sin(2B) < 0 FOR clockwise rotation OF the vector.
		IF nPol GT 0 THEN BEGIN
			sin2B = 2.0*Imaginary(Js[iPol,1,0]) / $
			        Sqrt( Real_Part(Js[iPol,0,0] + Js[iPol,1,1])^2 - 4.0*det_Js[iPol] )
			
			;Occasionally, numerical error produces |sin2B| > 1 by less than 1e-7.
			;Check AND set them explicitly to +/- 1. Otherwise, produces error:
			;   % Program caused arithmetic error: Floating illegal operand
			iGT1 = Where(Abs(sin2B) GT 1, nGT1)
			IF nGT1 GT 0 THEN sin2B[iGT1] = -1.0 > sin2B[iGT1] < 1.0

			;Compute ellipticity
			ellipticity[iPol] = Tan(0.5 * ASin(sin2B))
		ENDIF
		IF nLin GT 0 THEN ellipticity[iLin] = !values.f_nan
		ellipticity = Reform(ellipticity, npts, nfreqs)
		
		;Variable
		oEllipticity = MrTimeSeries( oTime, ellipticity[*, if0:if1], $
		                             CACHE = cache, $
		                             NAME  = 'Ellipticity(' + oVar.name + ')' )
		
		;Attributes
		oEllipticity['AXIS_RANGE']   = [-1.0, 1.0]
		oEllipticity['CATDESC']      = 'Wave ellipticity. +1 is right-hand polarized, 0 is ' + $
		                               'linearly polarized, and -1 is left-hand polarized.'
		oEllipticity['DEPEND_1']     = oFreqs
		oEllipticity['PLOT_TITLE']   = 'Ellipticity'
		oEllipticity['RGB_TABLE']    = 70
		oEllipticity['TICKINTERVAL'] = 0.5
		oEllipticity['TITLE']        = '$\epsilon$'
		oEllipticity['UNITS']        = ''
	ENDIF

;-----------------------------------------------------
; VII. Other Useful Parameters \\\\\\\\\\\\\\\\\\\\\\\
;-----------------------------------------------------

	;INTENSITY in the wave-normal frame
	IF keep_intensity THEN BEGIN
		;I = Jxx + Jyy
		;   Because the diagonal terms are purely real (within ~1e-8), I explicitly take
		;   the real part to prevent INTENSITY from being converted to a complex array.
		intensity = Real_Part(Reform(Js[*,0,0] + Js[*,1,1], npts, nfreqs))
		
		;Variable
		oIntensity = MrTimeSeries( oTime, intensity[*, if0:if1], $
		                           CACHE = cache, $
		                           NAME  = 'Intensity(' + oVar.name + ')' )
		
		;Attributes
		oIntensity['CATDESC']      = 'Power spectral matrix in the wave-normal frame'
		oIntensity['DEPEND_1']     = oFreqs
		oIntensity['LOG']          = 1B
		oIntensity['PLOT_TITLE']   = 'Intensity'
		oIntensity['SCALE']        = 1B
		oIntensity['TITLE']        = 'Intensity'
		IF oVar -> HasAttr('UNITS') THEN oIntensity['UNITS'] = '(' + oVar['UNITS'] + ')^2/Hz'
	ENDIF

	;POWER SPECTRAL MATRIX in the Wave Normal Frame.
	IF keep_wave_normal THEN BEGIN
;		wave_normal_matrix = reform(Js[0:1,0:1,*], 2, 2, npts, nfreqs)
		wave_normal_matrix = Reform(Js, npts, nfreqs, 3, 3)
		
		;Variable
		oWaveNormalMatrix = MrTimeSeries( oTime, wave_normal_matrix[*, if0:if1, *, *], $
		                                  CACHE = cache, $
		                                  NAME  = 'WaveNormalMatrix(' + oVar.name + ')' )
		
		;Attributes
		oWaveNormalMatrix['CATDESC']      = 'Power spectral matrix in the wave-normal frame'
		oWaveNormalMatrix['DEPEND_1']     = oFreqs
		oWaveNormalMatrix['LABL_PTR_2']   = ['X', 'Y', 'Z']
		oWaveNormalMatrix['LABL_PTR_3']   = ['X', 'Y', 'Z']
		oWaveNormalMatrix['LOG']          = 1B
		oWaveNormalMatrix['PLOT_TITLE']   = 'Power Spectral Matrix'
		oWaveNormalMatrix['TITLE']        = 'PSD'
		IF oVar -> HasAttr('UNITS') THEN oWaveNormalMatrix['UNITS'] = '(' + oVar['UNITS'] + ')^2/Hz'
	ENDIF
END