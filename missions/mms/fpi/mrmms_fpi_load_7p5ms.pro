; docformat = 'rst'
;
; NAME:
;       MrMMS_FPI_Load_7p5ms
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
;   Find and read MMS FPI reduced moments data.
;
; :Categories:
;       MMS, FPI
;
; :Params:
;       SC:                 in, required, type=string/strarr
;                           The MMS spacecraft identifier. Options are:
;                               {'mms1' | 'mms2' | 'mms3' | 'mms4'}
;       SPECIES:            in, required, type=string
;                           Species of particle for which to read data. Options are: {'e' | 'i'}.
;
; :Keywords:
;       SUFFIX:             in, optional, type=string, default=''
;                           A suffix to be appended to the variable names.
;       VARNAMES:           out, optional, type=strarr
;                           A named variable to receive the names of each variable read
;                               into the cache.
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
;       2016/01/18  -   Written by Matthew Argall
;-
PRO MrMMS_FPI_Load_7p5ms, sc, species, $
SUFFIX=suffix, $
VARNAMES=varnames
	Compile_Opt idl2

	Catch, the_error
	IF the_error NE 0 THEN BEGIN
		Catch, /CANCEL
		MrPrintF, 'LogErr'
		RETURN
	ENDIF
	
	;Defaults & Constants
	dir = '/home/argall/moments/2015-10-16/'
	IF N_Elements(species) EQ 0 THEN species = 'e'
	IF N_Elements(suffix)  EQ 0 THEN suffix  = ''
	
	instr = 'd' + species + 's'
	CASE species OF
		'e': fname = StrJoin( [sc, 'fpi', 'moments', '7p5ms', instr], '_' ) + '.txt'
		'i': fname = StrJoin( [sc, 'fpi', 'moments', '37p5ms', instr], '_' ) + '.txt'
		ELSE: Message, 'Invalid value for SPECIES: "' + species + '".'
	ENDCASE
	
	;Variable names
	n_vname = StrJoin( [sc, instr, 'numberdensity', 'brst'], '_' ) + suffix
	v_vname = StrJoin( [sc, instr, 'bulkv',         'brst'], '_' ) + suffix
	p_vname = StrJoin( [sc, instr, 'pres',          'brst'], '_' ) + suffix
	q_vname = StrJoin( [sc, instr, 'heatflux',      'brst'], '_' ) + suffix
	
	;Load the data into the variable cache
	MrVar_ReadASCII, FilePath(fname, ROOT_DIR=dir), $
	                 COLUMN_TYPES = [ 'LONG64', Replicate('FLOAT', 13) ], $
	                 COLUMN_NAMES = [ 'Epoch', n_vname, Replicate(v_vname, 3), $
	                                  Replicate(p_vname, 6), Replicate(q_vname, 3) ], $
	                 GROUPS       = [1, 2, Replicate(3, 3), Replicate(4, 6), Replicate(5, 3)], $
	                 NHEADER      = 2, $
	                 TFORMAT      = 'TT2000', $
	                 VARNAMES     = varnames
	
	;Add variable attributes
	n  = MrVar_Get( StrUpCase(n_vname) )
	n -> SetName, n_vname
	n['PLOT_TITLE'] = 'Number density'
	n['TITLE']      = 'N!Ccm^-3'
	n['UNITS']      = 'cm^-3'
	
	v = MrVar_Get( StrUpCase(v_vname) )
	v -> SetName, v_vname
	v['PLOT_TITLE'] = 'Velocity'
	v['LABEL']      = ['Vx', 'Vy', 'Vz']
	v['TITLE']      = 'V!C(km/s)'
	v['UNITS']      = 'km/s'
	
	P = MrVar_Get( StrUpCase(p_vname) )
	P -> SetName, p_vname
	P['PLOT_TITLE'] = 'Pressure'
	P['LABEL']      = ['Pxx', 'Pyy', 'Pzz', 'Pxy', 'Pxz', 'Pyz']
	P['TITLE']      = 'P!C(nPa)'
	P['UNITS']      = 'nPa'
	
	Q = MrVar_Get( StrUpCase(q_vname) )
	Q -> SetName, q_vname
	Q['PLOT_TITLE'] = 'Heat Flux'
	Q['LABEL']      = ['Qxx', 'Qyy', 'Qzz']
	Q['TITLE']      = 'Q!C(mW/m^2)'
	Q['UNITS']      = 'mW/m^2'
END
