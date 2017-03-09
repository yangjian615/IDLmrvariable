; docformat = 'rst'
;
; NAME:
;       MrVar_Freq_Plasma
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
;   Compute the plasma frequency.
;
;       fp = 1/(2*!pi) * sqrt(q * N / (m * epsilon0))
;
; :Categories:
;   MrVariable
;
; :Params:
;       N:              in, required, type=string/integer/objref
;                       Name, number, or MrVariable object of the number density (cm^-3).
;       MASS:           in, required, type=string/float
;                       Mass (kg) of particle species. If a string, any mass recognized by
;                           MrConstants is acceptable.
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
function MrVar_Freq_Plasma, N, mass, $
CACHE=cache, $
NAME=name, $
NO_CLOBBER=no_clobber
	compile_opt idl2
	on_error, 2
	
	
	;Constants
	q        = MrConstants('q', /DOUBLE)
	epsilon0 = MrConstants('epsilon_0', /DOUBLE)
	m        = Size(mass, /TNAME) EQ 'STRING' ? MrConstants(mass) : mass
	IF N_Elements(name) EQ 0 THEN name = 'Cyclotron_Frequency'
	
;-----------------------------------------------------
; Plasma Frequency \\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\
;-----------------------------------------------------
	;Get the variables
	oN = MrVar_Get(N)

	;compute the plasma beta
	;units: sqrt(1e6 * m^-3) * sqrt(q/(m*epsilon_0)) --> 1e3*rad/s => Multiply result by 1e3
	omega_p = Sqrt(oN['DATA']) * Sqrt(q^2 / (m * epsilon0))
	f_p     = Float(Temporary(omega_p)) * 1e3 / (2*!pi)
	

	f_p = MrScalarTS( oN['TIMEVAR'], f_p, $
	                  CACHE      = cache, $
	                  NAME       = name, $
	                  NO_CLOBBER = no_clobber )
	
;-----------------------------------------------------
; Finish Up \\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\
;-----------------------------------------------------
	
	;Attributes
	f_p['CATDESC']    = 'Plasma frequency: fp = sqrt(q * N / (m * epsilon0)) .'
	f_p['PLOT_TITLE'] = 'Plasma frequency'
	f_p['TITLE']      = 'fp!C(Hz)'
	f_p['UNITS']      = 'Hz'
	
	;Cleanup variables
	return, f_p
end