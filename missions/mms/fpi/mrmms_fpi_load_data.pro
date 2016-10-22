; docformat = 'rst'
;
; NAME:
;       MrMMS_Get_FPI_Data
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
;   Read data from the Fast Plasma Instrument on MMS.
;
; TIME TAGS:
;   FPI time tags are shifted to the center of the sampling interval to match
;   the rest of the MMS data.
;
; MOMENTS FILES:
;   The following quantities are combined. When using VARFORMAT, the original
;   names must be provided.
;       bulkx                   heatx
;       bulky  -->  bulkv       heaty  -->  heat
;       bulkz                   heatz
;
;       presxx                  tempxx
;       presxy                  tempxy
;       presxz  --> pres        tempxz  -->  temp
;       presyy                  tempyy
;       presyz                  tempyz
;       preszz                  tempzz
;
;       energyspectr_px                       pitchangdist_lowen
;       energyspectr_mx                       pitchangdist_miden   --> pitchangdist
;       energyspectr_py --> energyspectr      pitchangdist_highen
;       energyspectr_my
;       energyspectr_pz
;       energyspectr_mz
;
; DIST FILES:
;   Energy tables are converted to an Nx32 time-series array that combines the
;   information in the two energy tables and the parity table. Also, an energy
;   table that is the geometric mean of the to original tables is created. The
;   variable attributes of the distribution are updated to be physical arrays
;   instead of indices. When reading distributions, either use the /SUPPORT_DATA
;   keyword or 
;       VARFORMAT=['*dist_*', '*energy0*', '*energy1*', '*phi*', '*theta*', '*steptable*']
;
; :Categories:
;       MMS, FPI
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
;       2016/08/20  -   Written by Matthew Argall
;       2016/10/07  -   Added the SUFFIX keyword. - MRA
;-
;*****************************************************************************************
;+
;   Create an omni-directional energy spectrogram.
;
; :Params:
;       NAMES:          in, required, type=strarr
;                       Names of the energy spectra variables.
;-
pro MrMMS_FPI_Load_Data_ESpectr, names
	compile_opt idl2
	on_error, 2
	
	;Expect 6 names, +/- [x, y, z]
	nESpectr  = n_elements(names)
	if nESpectr ne 6 then message, 'Unexpected number of energy spectra.'
	
	;Variable names
	;   - sc_instr_energyspectr_px_mode[_suffix]
	;      0   1        2        3  4      5+
	;    | prefix |                  |   suffix   |
	parts    = strsplit(names[0], '_', /EXTRACT)
	prefix   = parts[0] + '_' + parts[1] + '_'
	suffix   = '_' + strjoin(parts[4:*], '_')
	new_name = prefix + 'energyspectr' + suffix

	;Average the distributions
	;   - A new variable is created via addition and all
	;     of the variable attributes are lost.
	tempESpectr = MrVar_Get( names[0] )
	oESpectr    = tempESpectr -> Copy()
	for i = 1, nESpectr - 1 do begin
		tempSpectr = MrVar_Get( names[i] )
		oESpectr  += tempSpectr
	endfor
	oESpectr /= nESpectr

	;Save the variable
	oESpectr    = oESpectr -> Transpose()
	oESpectr   -> SetName, new_name
	oESpectr   -> Cache
	tempSpectr -> CopyAttrTo, oESpectr
	
	;Set Attributes
	oESPectr -> SetAttrValue, 'CATDESC',    'Omni-directional energy spectrum.'
	oESPectr -> SetAttrValue, 'FIELDNAM',   'Energy Spectrum'
	oESPectr -> SetAttrValue, 'LABLAXIS',   'E Specturm'
	oESPectr -> SetAttrValue, 'PLOT_TITLE', 'Omni-directional energy spectrum.'
	oESPectr -> SetAttrValue, 'LOG',        1
	oESPectr -> AddAttr,      'SCALE',      1
	oESPectr -> SetAttrValue, 'TITLE',      'E Flux'
	oESPectr -> AddAttr,      'UNITS',      'eV / (cm^2 s sr eV)'
	
	;DEPEND_1
	eIndex  = MrVar_Get( oESPectr['DEPEND_1'] )
	eIndex -> AddAttr, 'TITLE', 'E Index'
end


;+
;   Create new energy tables and set the distribution function's DEPEND_1-3 variable
;   attributes to be variables with physical units.
;
; :Params:
;       NAMES:          in, required, type=string
;                       Name of the distribution function variable.
;-
pro MrMMS_FPI_Load_Data_Dist, name
	compile_opt idl2
	on_error, 2
	
	;Variable name parts
	;   - sc_instr_energy_mode[_suffix]
	;      0   1     2     3      4+
	;    | prefix |      |   suffix   |
	parts  = strsplit(name, '_', /EXTRACT)
	prefix = strjoin(parts[0:1], '_') + '_'
	suffix = '_' + strjoin(parts[3:*], '_')

	;BRST mode
	;   - Two energy tables alternate based on STEPTABLE_PARITY
	if stregex(parts[3], '^brst', /BOOLEAN) then begin
		;Get the sector, pixel, and energy tables
		oParity  = MrVar_Get( prefix + 'steptable_parity' + suffix )
		oEnergy0 = MrVar_Get( prefix + 'energy0'          + suffix )
		oEnergy1 = MrVar_Get( prefix + 'energy1'          + suffix )
		oDist    = MrVar_get( prefix + 'dist'             + suffix )
	
		;Create new energy table
		;   - One time-dependent energy table
		;   - One combined (average) energy table
		energy      = transpose( [ [oEnergy0['DATA']], [oEnergy1['DATA']] ] )
		energy_full = MrVariable( energy[oParity['DATA'], *],                NAME=eTable_name, /CACHE)
		energy_mean = MrVariable( reform( sqrt(energy[0,*] * energy[1,*]) ), NAME=eMean_name,  /CACHE)
		
		;Names of new energy tables
		eTable_name = prefix + 'energy_table'   + suffix
		eMean_name  = prefix + 'energy_geomean' + suffix
	
	;SRVY mode
	endif else begin
		;There is only one energy table
		eTable_name = prefix + 'energy' + suffix
	endelse
	
	;Names of phi, theta, and new energy tables
	dist_name   = prefix + 'dist'  + suffix
	phi_name    = prefix + 'phi'   + suffix
	theta_name  = prefix + 'theta' + suffix
		
	;Set the distribution function dependencies
	oDist  = MrVar_Get(dist_name)
	oDist -> SetAttrValue, 'DEPEND_1', phi_name
	oDist -> SetAttrValue, 'DEPEND_2', theta_name
	oDist -> SetAttrValue, 'DEPEND_3', eTable_name
end


;+
;   Create an pitch angle distribution.
;
; :Params:
;       NAMES:          in, required, type=strarr
;                       Names of the pitch angle distribution variables.
;-
pro MrMMS_FPI_Load_Data_PAD, names
	compile_opt idl2
	on_error, 2
	
	;Expect 3 names: [low, mid, high] energy
	nPAD = n_elements(names)
	if nPAD ne 3 then message, 'Unexpected number of energy spectra.'
	
	;Variable names
	;   - sc_instr_pitchangdist_miden_mode[_suffix]
	;      0   1        2         3    4      5+
	;    | prefix |                  |   suffix   |
	parts    = strsplit(names[0], '_', /EXTRACT)
	prefix   = parts[0] + '_' + parts[1] + '_'
	suffix   = '_' + strjoin(parts[4:*], '_')
	new_name = prefix + 'pitchangdist' + suffix
	
	;Average the distributions
	;   - A new variable is created via addition and all
	;     of the variable attributes are lost.
	tempPAD = MrVar_Get( names[0] )
	oPAD    = tempPAD -> Copy()
	for i = 1, nPad - 1 do begin
		tempPAD  = MrVar_Get( names[i] )
		oPAD    += tempPAD[i]
	endfor
	oPAD /= nPAD
	
	;Save the variable
	oPAD     = oPAD -> Transpose()
	oPAD    -> SetName, new_name
	oPAD    -> Cache
	tempPAD -> CopyAttrTo, oPAD
	
	;Set Attributes
	oPAD -> SetAttrValue, 'CATDESC',    'Pitch angle ditribution'
	oPAD -> SetAttrValue, 'FIELDNAM',   'Pitch angle ditribution'
	oPAD -> SetAttrValue, 'LABLAXIS',   'PAD'
	oPAD -> SetAttrValue, 'PLOT_TITLE', 'Pitch angle ditribution'
	oPAD -> SetAttrValue, 'LOG',        1
	oPAD -> AddAttr,      'SCALE',      1
	oPAD -> SetAttrValue, 'TITLE',      'E Flux'
	oPAD -> AddAttr,      'UNITS',      'eV / (cm^2 s sr eV)'
	
	;DEPEND_1
	paIndex = MrVar_Get( oPAD['DEPEND_1'] )
	paIndex -> AddAttr, 'TITLE', 'PA Index'
end


;+
;   Combine tensor components into a single matrix
;
; :Params:
;       NAMES:          in, required, type=strarr(3)
;                       Variable names of the six symmetric tensor components, ordered
;                           as ['xx', 'xy', 'xz', 'yy', 'yz', 'zz'].
;-
pro MrMMS_FPI_Load_Data_Tensor, names
	compile_opt idl2
	on_error, 2

	;Get components
	oXX = MrVar_Get(names[0])
	oXY = MrVar_Get(names[1])
	oXZ = MrVar_Get(names[2])
	oYY = MrVar_Get(names[3])
	oYZ = MrVar_Get(names[4])
	oZZ = MrVar_Get(names[5])
	
	;New name
	old_name = oXX.name
	pos      = stregex(old_name, '(pres|temp)([xyz][xyz])', /SUBEXP, LEN=len)
	type     = strmid(old_name, pos[1], len[1])
	new_name = strmid(old_name, 0, pos[2]) + strmid(old_name, pos[2]+2)
	
	;Concatenate the data
	oT = MrMatrixTS( [ [ [oXX['DATA']], [oXY['DATA']], [oXZ['DATA']] ], $
	                   [ [oXY['DATA']], [oYY['DATA']], [oYZ['DATA']] ], $
	                   [ [oXZ['DATA']], [oYZ['DATA']], [oZZ['DATA']] ] ], $
	                 NAME=new_name, /CACHE )
	
	;Set Attributes
	oXX -> CopyAttrTo, oT
	oT  -> AddAttr, 'COLOR',     ['Purple', 'Blue', 'Forest Green', 'Org4', 'Red', 'Magenta']
	oT  -> AddAttr, 'DIMENSION', 1
	oT  -> AddAttr, 'LABEL',     ['XX', 'XY', 'XZ', 'YX', 'YY', 'YZ', 'ZX', 'ZY', 'ZZ']
	
	;Delete old variables
	MrVar_Delete, [oXX, oXY, oXZ, oYY, oYZ, oZZ]
end


;+
;   Combine vector components into a single matrix
;
; :Params:
;       NAMES:          in, required, type=strarr(3)
;                       Variable names of the three vector components, ordered as
;                           ['x', 'y', 'z'].
;-
pro MrMMS_FPI_Load_Data_Vector, names
	compile_opt idl2
	on_error, 2

	;Get components
	oX = MrVar_Get(names[0])
	oY = MrVar_Get(names[1])
	oZ = MrVar_Get(names[2])
	
	;New name
	old_name = oX.name
	pos      = stregex(old_name, '(bulk|heat)([xyz])', /SUBEXP, LEN=len)
	type     = strmid(old_name, pos[1], len[1])
	if type eq 'bulk' $
		then new_name = strmid(old_name, 0, pos[2]) + 'v' + strmid(old_name, pos[2]+1) $
		else new_name = strmid(old_name, 0, pos[2])       + strmid(old_name, pos[2]+1)
	
	;Concatenate the data
	oV = MrVectorTS( [ [oX['DATA']], [oY['DATA']], [oZ['DATA']] ], NAME=new_name, /CACHE )
	
	;Set Attributes
	oX -> CopyAttrTo, oV
	oV -> AddAttr, 'COLOR',     ['Blue', 'Forest Green', 'Red']
	oV -> AddAttr, 'DIMENSION', 1
	oV -> AddAttr, 'LABEL',     ['X', 'Y', 'Z']
	
	;Delete the old variables
	MrVar_Delete, [oX, oY, oZ]
end


;+
;   Find and read MMS FPI data.
;
; :Params:
;       SC:                 in, required, type=string/strarr
;                           The MMS spacecraft identifier. Options are:
;                               {'mms1' | 'mms2' | 'mms3' | 'mms4'}
;       MODE:               in, required, type=string/strarr
;                           Telemetry mode of the data. Options include:
;                               {'slow' | 'fast' | 'srvy' | 'brst'}
;
; :Keywords:
;       LEVEL:              in, optional, type=string/strarr, default='l2'
;                           Data quality level. Options include:
;                               {'l1a' | 'l1b' | 'l2pre' | 'l2'}
;       OPTDESC:            in, optional, type=string, default=''
;                           Optional descriptor of the data.
;       SUPPORT_DATA:       in, optional, type=boolean, default=0
;                           If set, support data will be read as well. Regardless of
;                               the status of this keyword, variable data associated
;                               with DEPEND_# variables will be read. This keyword
;                               is ignored if `VARFORMAT` is set.
;       TEAM_SITE:          in, optional, type=boolean, default=0
;                           If set, requests will be sent to the team site (password
;                               required, L1A and above). Automatically set if `LEVEL`
;                               is below level 2. This option is sticky.
;       TRANGE:             out, optional, type=string, default=MrVar_GetTRange()
;                           Start and end times over which to read data.
;       VARFORMAT:          out, optional, type=string, default='*'
;                           Variables that match this search pattern will be read,
;                               others are ignored.
;-
pro MrMMS_FPI_Load_Data, sc, mode, $
LEVEL=level, $
OPTDESC=optdesc, $
SUFFIX=suffix, $
SUPPORT_DATA=support_data, $
TEAM_SITE=team_site, $
TRANGE=trange, $
VARFORMAT=varformat, $
VARNAMES=varnames
	compile_opt idl2

	catch, the_error
	if the_error ne 0 then begin
		catch, /CANCEL
		MrPrintF, 'LogErr'
		return
	endif
	
	;Defaults
	instr = 'fpi'
	if n_elements(level)   eq 0 then level   = 'l2'
	if n_elements(optdesc) eq 0 then optdesc = 'des-moms'
	
	;Get the data
	MrMMS_Load_Data, sc, instr, mode, level, $
	                 OPTDESC      = optdesc, $
	                 SUFFIX       = suffix, $
	                 SUPPORT_DATA = support_data, $
	                 TEAM_SITE    = team_site, $
	                 TRANGE       = trange, $
	                 VARFORMAT    = varformat, $
	                 VARNAMES     = varnames

;-----------------------------------------------------
; Center the Time Tags \\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\
;-----------------------------------------------------
	;
	; FPI time tags are at the beginning of the sampling interval
	; while the rest of the instruments tag the middle of the
	; sampling interval.
	;
	
	;Search for the epoch variable
	iEpoch = where( stregex(varnames, 'epoch', /BOOLEAN), nEpoch )
	if nEpoch gt 0 then begin
		;Extract the time as both TT2000 (original) and SSM
		oEpoch   = MrVar_Get(varnames[iEpoch])
		t_tt2000 = oEpoch -> GetData('TT2000')
		t_ssm    = oEpoch -> GetData('SSM')
		
		;Nanoseconds between samples
		dt = long64( (t_ssm[1:*] - temporary(t_ssm) / 2D ) * 1d9)
		dt = [dt, dt[-1]]
		
		;Add this to the TT2000 times and reset the data
		t_tt2000 += dt
		oEpoch   -> SetData, t_tt2000, 'TT2000', /NO_COPY
	endif

;-----------------------------------------------------
; Load Distributions \\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\
;-----------------------------------------------------
	iDist = where( stregex(varnames, '_dist_', /BOOLEAN), nDist )
	for i = 0, nDist - 1 do begin
		;Update the distribution
		MrMMS_FPI_Load_Data_Dist, varnames[i]

		;Delete variables
		iDel = where( stregex(varnames, '(sector|pixel|energy)(_index|_label)', /BOOLEAN), nDel, $
		              COMPLEMENT=iKeep, NCOMPLEMENT=nKeep)
		MrVar_Delete, varnames[iDel]
	endfor
	
;-----------------------------------------------------
; Combine Components \\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\
;-----------------------------------------------------
	cs = ['dbcs', 'gse', 'err']
	for i = 0, n_elements(cs) - 1 do begin
		;Velocity
		ibulk = where( stregex(varnames, 'bulk(x|y|z)_' + cs[i], /BOOLEAN), nBulk)
		if nBulk gt 0 then MrMMS_FPI_Load_Data_Vector, varnames[iBulk]
		
		;Pressure
		iPres = where( stregex(varnames, 'pres[xyz][xyz]_' + cs[i], /BOOLEAN), nPres)
		if nPres gt 0 then MrMMS_FPI_Load_Data_Tensor, varnames[iPres]
		
		;Temperature
		iTemp = where( stregex(varnames, 'temp[xyz][xyz]_' + cs[i], /BOOLEAN), nTemp)
		if nTemp gt 0 then MrMMS_FPI_Load_Data_Tensor, varnames[iTemp]
		
		;Heat Flux
		iHeat = where( stregex(varnames, 'heat(x|y|z)_' + cs[i], /BOOLEAN), nHeat)
		if nHeat gt 0 then MrMMS_FPI_Load_Data_Vector, varnames[iHeat]
	endfor
	
;-----------------------------------------------------
; Energy Sectra \\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\
;-----------------------------------------------------
	iESpectr = where( stregex(varnames, 'energyspectr_(m|p)(x|y|z)', /BOOLEAN), nESpectr )
	if nESpectr gt 0 then MrMMS_FPI_Load_Data_ESpectr, varnames[iESpectr]
	
;-----------------------------------------------------
; Pitch Angle Distribution \\\\\\\\\\\\\\\\\\\\\\\\\\\
;-----------------------------------------------------
	iPAD = where( stregex(varnames, 'pitchangdist_(low|mid|high)en', /BOOLEAN), nPAD )
	if nPAD gt 0 then MrMMS_FPI_Load_Data_PAD, varnames[iPAD]
end
