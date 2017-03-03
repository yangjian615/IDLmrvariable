; docformat = 'rst'
;
; NAME:
;       MrVar_Freq_LowerHybrid
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
;   Compute the lower-hybrid frequency.
;
;       f_lh = 1/(2*!pi) * sqrt( 1/(f_ci * f_ce) + 1/f_pi^2 )
;       f_ci = proton cyclotron frequency
;       f_ce = electron cyclotron frequency
;       f_pi = proton plasma frequency
;
; :Categories:
;   MrVariable
;
; :Params:
;       BMAG:           in, required, type=string/integer/objref
;                       Name, number, or MrTimeSeries object of the magnetic field magnitude (nT).
;                           If BMAG and `N` do not have identical time tags, BMAG will be
;                           interpolated onto `N`.
;       N:              in, required, type=string/integer/objref
;                       Name, number, or MrVariable object of the number density (cm^-3).
;
; :Keywords:
;       CACHE:          in, optional, type=boolean, default=0
;                       If set, the output will be added to the variable cache.
;       NAME:           in, optional, type=string, default='Cyclotron_Frequency'
;                       Name to be given to the output variable.
;       NO_CLOBBER:     in, optional, type=boolean, default=0
;                       If set, cached variables with name `NAME` are not over-written.
;                           Instead, the output variable will have "_#" appended, were "#"
;                           represents a unique number.
;
; :Returns:
;       FC:             out, required, type=objref
;                       Plasma frequency (Hz). Variable is MrScalarTS object.
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
;       2016/12/08  -   Written by Matthew Argall
;-
function MrVar_Freq_LowerHybrid, Bmag, N, $
CACHE=cache, $
NAME=name, $
NO_CLOBBER=no_clobber
	compile_opt idl2
	on_error, 2
	
	
	;Constants
	q        = MrConstants('q', /DOUBLE)
	mu0      = MrConstants('mu_0', /DOUBLE)
	epsilon0 = MrConstants('epsilon_0', /DOUBLE)
	m_i      = MrConstants('m_p')
	m_e      = MrConstants('m_e')
	IF N_Elements(name) EQ 0 THEN name = 'Cyclotron_Frequency'
	
	;Get the variables
	oBmag = MrVar_Get(Bmag)
	oN = MrVar_Get(N)
	IF ~oBmag -> IsTimeIdentical(oN) $
		THEN oBmag = oBmag -> Interpol(oN)
	
;-----------------------------------------------------
; Cyclotron Frequency \\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\
;-----------------------------------------------------

	;Cyclotron frequency
	;   - 1e-9 comes from converting nT to T
	f_ci = (1e-9*q/(m_i*2*!pi)) * oBmag ;Hz
	f_ce = (1e-9*q/(m_e*2*!pi)) * oBmag ;Hz
	
;-----------------------------------------------------
; Plasma Frequency \\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\
;-----------------------------------------------------
	;units: sqrt(1e6 * m^-3) * sqrt(q/(m*epsilon_0)) --> 1e3*rad/s => Multiply result by 1e3
	omega_pi = Sqrt(oN['DATA']) * Sqrt(q^2 / (m_i * epsilon0))
	f_pi     = Float(Temporary(omega_pi)) * 1e3 / (2*!pi)
	
;-----------------------------------------------------
; Lower Hybrid Frequency \\\\\\\\\\\\\\\\\\\\\\\\\\\\
;-----------------------------------------------------
	omega_lh = ( 1.0 / ( f_ci['DATA'] * f_ce['DATA'] ) + 1.0 / f_pi^2 )^(-1.0/2.0)
	f_lh     = Temporary(omega_lh) / (2*!pi)

	;Create variable
	f_lh = MrScalarTS( oN['TIMEVAR'], f_lh, $
	                   CACHE      = cache, $
	                   NAME       = name, $
	                   NO_CLOBBER = no_clobber )
	
;-----------------------------------------------------
; Finish Up \\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\
;-----------------------------------------------------
	
	;Attributes
	f_lh['CATDESC']    = 'Lower hybrid frequency: f_lh = sqrt( 1/(f_ci * f_ce) + 1/f_pi^2 ) .'
	f_lh['PLOT_TITLE'] = 'Lower hybrid frequency'
	f_lh['TITLE']      = 'f_lh!C(Hz)'
	f_lh['UNITS']      = 'Hz'
	
	;Cleanup variables
	return, f_lh
end