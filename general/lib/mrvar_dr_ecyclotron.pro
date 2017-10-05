; docformat = 'rst'
;
; NAME:
;       MrVar_DR_eCyclotron
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
;   Compute the index of refraction of an electron cyclotron wave using the dispersion
;   relation:
;
;       n^2 = (ck/w)^2 = 1 - wpe / (w * (w + wce))
;
;   References:
;       Kennel, C. F., and H. E. Petschek (1966), Limit on stably trapped particle fluxes,
;           J. Geophys. Res., 71(1), 1–28, doi:10.1029/JZ071i001p00001.
;
; :Categories:
;   MrVariable
;
; :Params:
;       F:              in, required, type=float
;                       The observed wave frequency.
;       BMAG:           in, required, type=string/integer/objref
;                       Name, number, or MrScalarTS object of the magnetic field magnitude.
;       N:              in, required, type=string/integer/objref
;                       Name, number, or MrScalarTS object of the plasma density.
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
;                       Cyclotron frequency (Hz) of the same variable class as `BMAG`.
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
;       2016/06/21  -   Written by Matthew Argall
;-
FUNCTION MrVar_DR_eCyclotron, f, Bmag, N, mass, $
CACHE=cache, $
NAME=name, $
NO_CLOBBER=no_clobber, $
WAVE_NUMBER=oK
	Compile_Opt idl2
	On_Error, 2
	
	IF N_Elements(name) EQ 0 THEN name = 'index_of_refraction'
	k_vname = 'wave_number'
	
	;Check that Bmag and N have the same time tags
	oB = MrVar_Get(Bmag)
	oN = MrVar_Get(N)
	IF ~oN -> IsTimeIdentical(oB) $
		THEN oN = oN -> Interpol(oB)
	
	;Calculate frequencies
	oFc = MrVar_Freq_Cyclotron(oB, mass)
	oFp = MrVar_Freq_Plasma(oN, mass)
	
	;Calculate index of refraction
	oIdx = 1.0 - oFp^2 / ( f * (f - oFc) )
	oIdx  = MrScalarTS( oB['TIMEVAR'], Sqrt(oIdx['DATA']), $
	                    CACHE      = cache, $
	                    NAME       = name, $
	                    NO_CLOBBER = no_clobber )
	
	;Attributes
	oIdx['CATDESC']       = 'Index of refraction for an electron cyclotron wave.'
	oIdx['FIELDNAM']      = 'n'
	oIdx['FILLVAL']       = !values.f_nan
	oIdx['FORMAT']        = 'E11.4'
	oIdx['SCALETYP']      = 'linear'
	oIdx['SI_CONVERSION'] = '>'
	oIdx['UNITS']         = ' '
	oIdx['VAR_NOTES']     = 'The index of refraction is computed using the dispersion relation of ' + $
	                        'electron whistler waves given by equation 2.16 and neglecting ' + $
	                        'the ion term of: ' + $
	                        'Kennel, C. F., and H. E. Petschek (1966), Limit on stably ' + $
	                        'trapped particle fluxes, J. Geophys. Res., 71(1), 1–28, ' + $
	                        'doi:10.1029/JZ071i001p00001.'
	
	;Calculate the wave number
	IF Arg_Present(oK) THEN BEGIN
		oK  = 1e3 * oIdx * 2.0*!pi*f / MrConstants('c')
		oK -> SetName, k_vname
		IF Keyword_Set(cache) THEN oK -> Cache, NO_CLOBBER=no_clobber
		
		;Attributes
		oK['CATDESC']       = 'Wave number for an electron cyclotron wave.'
		oK['FIELDNAM']      = 'k'
		oK['FILLVAL']       = !values.f_nan
		oK['FORMAT']        = 'E11.4'
		oK['SCALETYP']      = 'linear'
		oK['SI_CONVERSION'] = '1e-3>1/m^3'
		oK['UNITS']         = 'km'
		oK['VAR_NOTES']     = 'The wave number is computed using the dispersion relation of ' + $
		                      'electron whistler waves given by equation 2.16 and neglecting ' + $
		                      'the ion term of: ' + $
		                      'Kennel, C. F., and H. E. Petschek (1966), Limit on stably ' + $
		                      'trapped particle fluxes, J. Geophys. Res., 71(1), 1–28, ' + $
		                      'doi:10.1029/JZ071i001p00001.'
	ENDIF
		
	;Done
	RETURN, oIdx
END