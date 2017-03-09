; docformat = 'rst'
;
; NAME:
;       MrVar_Whistler_ERes
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
;   Compute the resonant energy of whistler waves assuming parallel wave propagation.
;
;   Refrences:
;       Smith, E. J., and B. T. Tsurutani (1976), Magnetosheath lion roars, J. Geophys.
;           Res., 81(13), 2261–2266, doi:10.1029/JA081i013p02261.
;
; :Categories:
;   MrVariable, Waves
;
; :Params:
;       BMAG:       in, required, type=string/integer/objref
;                   Name, number, or MrTimeSeries object of the magnetic field magnitude (nT).
;                       If BMAG and `N` do not have identical time tags, BMAG will be
;                       interpolated onto `N`.
;       N:          in, required, type=string/integer/objref
;                   Name, number, or MrVariable object of the number density (cm^-3).
;       MASS:       in, required, type=string/float
;                   Mass (kg) of particle species. If a string, any mass recognized by
;                       MrConstants is acceptable.
;       FW:         in, required, type=float
;                   Observed wave frequency.
;
; :Keywords:
;       CACHE:      in, optional, type=boolean, default=0
;                   If set, the output will be added to the variable cache.
;       NAME:       in, optional, type=string, default='Whistler_Resonant_Energy'
;                   Name to be given to the output variable.
;       NO_CLOBBER: in, optional, type=boolean, default=0
;                   If set, cached variables with name `NAME` are not over-written.
;                       Instead, the output variable will have "_#" appended, were "#"
;                       represents a unique number.
;
; :Returns:
;       ER:         out, required, type=objref
;                   Resonant energy (eV) of the same variable class as `BMAG`.
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
function MrVar_Whistler_ERes, Bmag, N, mass, fw, $
CACHE=cache, $
NAME=name, $
NO_CLOBBER=no_clobber
	compile_opt idl2
	on_error, 2
	
	
	;Constants
	q   = MrConstants('q')
	mu0 = MrConstants('mu_0')
	m   = Size(mass, /TNAME) EQ 'STRING' ? MrConstants(mass) : mass
	if n_elements(name) eq 0 then name = 'Whistler_Resonant_Energy'
	
;-----------------------------------------------------
; Resonant Energy \\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\
;-----------------------------------------------------
	;Get the variables
	oBmag = MrVar_Get(Bmag)
	oN    = MrVar_Get(N)

	;Interpolate
	IF ~oBmag -> IsTimeIdentical(oN) $
		THEN oBmag = oBmag -> Interpol(oN)

	;Cyclotron frequency
	;   - 1e-9 comes from converting nT to T
	fc = (1e-9*q/(m*2*!pi)) * oBmag ;Hz
	
	;Normalized frequency
	f = fw / Temporary(fc)
	
	;Energy density per particle (nano-Weber)
	;   - 1e-6 comes from converting cm^3 to m^3
	oEn = 1e-24/(2*mu0) * oBmag^2 / oN

	;Resonant energy
	Er = (1-f)^3 * oEn / f
	Er /= 1.602e-19
	
;-----------------------------------------------------
; Finish Up \\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\
;-----------------------------------------------------
	
	;Name & cache
	Er -> SetName, name
	Er -> Cache, NO_CLOBBER=no_clobber
	
	;Attributes
	Er['CATDESC']    = 'Energy of electrons resonant with whistler waves at observed frequency ' + $
	                   'f=' + string(fw, FORMAT='(e0.2)') + '. Assumes parallel wave propagation.'
	Er['PLOT_TITLE'] = 'Resonant Energy'
	Er['TITLE']      = 'E!C(eV)'
	Er['UNITS']      = 'eV'
	
	;Cleanup variables
	return, Er
end