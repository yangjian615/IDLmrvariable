; docformat = 'rst'
;
; NAME:
;   MrMMS_FPI_Dist4D__Define
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
; :Categories:
;   MrVariable, MrTimeSeries, MrMMS_FPI_Dist4D
;
; :See Also:
;   MrVariable__Define.pro
;   MrTimeSeries__Define.pro
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
function MrMMS_FPI_Dist4D::INIT, sc, mode, species, $
CACHE=cache, $
DIMENSION=dimension, $
NAME=name, $
NO_CLOBBER=no_clobber, $
NO_COPY=no_copy, $
T_NAME=t_name, $
T_TYPE=t_type
	compile_opt idl2

	;Error handling
	catch, the_error
	if the_error ne 0 then begin
		catch, /CANCEL
		MrPrintF, 'LogErr'
		return, 0
	endif
	
	;Defaults
	if n_elements(name) eq 0 then name = 'MrMMS_FPI_Dist4D'
	particle = species eq 'e' ? species : 'H'


;-----------------------------------------------------
; Initialize Superclass \\\\\\\\\\\\\\\\\\\\\\\\\\\\\\
;-----------------------------------------------------
	success = self -> MrDist4D::Init( CACHE      = cache, $
		                              DIMENSION  = dimension, $
		                              NAME       = name, $
		                              NO_CLOBBER = no_clobber, $
		                              NO_COPY    = no_copy, $
		                              SPECIES    = particle, $
		                              T_NAME     = t_name, $
		                              T_TYPE     = t_type )
	if ~success then message, 'Unable to initialize superclass.'

;-----------------------------------------------------
; Load Data \\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\
;-----------------------------------------------------
	if n_params() gt 0 then begin
		vSuffix = n_elements(suffix) eq 0 ? '_Dist3D' : suffix + '_Dist3D'
		vnames = self -> Load( sc, mode, species, $
		                       COORD_SYS = coord_sys, $
		                       LEVEL     = level, $
		                       SUFFIX    = vSuffix, $
		                       TEAM_SITE = team_site, $
		                       TRANGE    = trange )
		
		;RETURN on error
		if vnames eq !Null then return, 0
		
		;Set data
		oDist = MrVar_Get(vnames[0])
		self -> SetData, oDist['DEPEND_0'], oDist, oDist['DEPEND_1'], $
		                 oDist['DEPEND_2'], oDist['DEPEND_3']
		
		;Remove everything from the cache
		MrVar_Remove, vnames

		;Create coordinate grid
		self -> Grid
	endif

	return, 1
end


;+
;   Clean up after the object is destroyed
;-
pro MrMMS_FPI_Dist4D::CLEANUP
	compile_opt idl2
	on_error, 2

	;Superclass
	self -> MrTimeSeries::Cleanup
end


;+
;   Find and read MMS FPI data.
;
; :Params:
;       SC:                 in, required, type=string/strarr
;                           The MMS spacecraft identifier. Options are:
;                               {'mms1' | 'mms2' | 'mms3' | 'mms4'}
;       MODE:               in, required, type=string/strarr
;                           Telemetry mode of the data. Options include: {'fast' | 'brst'}
;       SPECIES:            in, optional, type=string, default='e'
;                           Particle species for which the distribution function is
;                               to be loaded. Options are: {'e' | 'i'} for electrons
;                               and ions, respectively.
;       FAC:                in, optional, type=string
;                           The field-aligned coordinate system into which the instrument
;                               look directions should be rotated.
;
; :Keywords:
;       COORD_SYS:          in, optional, type=string, default='gse'
;                           Coordinate system of the original distribution function.
;                               Options are: {'dbcs' | 'gse'}.
;       LEVEL:              in, optional, type=string/strarr, default='l2'
;                           Data quality level. Options include:
;                               {'sitl' | 'l1b' | 'l2'}
;       TEAM_SITE:          in, optional, type=boolean, default=0
;                           If set, requests will be sent to the team site (password
;                               required, L1A and above). Automatically set if `LEVEL`
;                               is below level 2. This option is sticky.
;       TRANGE:             out, optional, type=string, default=MrVar_GetTRange()
;                           Start and end times over which to read data.
;       VARNAMES:           out, optional, type=strarr
;                           Names of all variables that have been loaded.
;-
function MrMMS_FPI_Dist4D::Load, sc, mode, species, $
COORD_SYS=coord_sys, $
LEVEL=level, $
SUFFIX=suffix, $
TEAM_SITE=team_site, $
TRANGE=trange
	compile_opt idl2

	catch, the_error
	if the_error ne 0 then begin
		catch, /CANCEL
		MrPrintF, 'LogErr'
		return, !Null
	endif
	
	;Check inputs
	cs = n_elements(coord_sys) eq 0 ? 'gse' : strlowcase(coord_sys)
	if n_elements(level)   eq 0 then level   = 'l2'
	if n_elements(species) eq 0 then species = 'e'
	if n_elements(suffix)  eq 0 then suffix  = ''
	
	;Conflicts
	if n_elements(sc)    ne 1 then message, 'SC must be scalar.'
	if n_elements(mode)  ne 1 then message, 'MODE must be scalar.'
	if ~MrIsMember(['fast', 'brst'], mode) then message, 'MODE must be {"fast" | "slow"}'
	
	;Fast and Brst are organized differently
	if mode eq 'fast' $
		then varformat = ['*_dist_*', '*_theta_*', '*_energy_*', '*_phi_*'] $
		else varformat = ['*dist_*', '*energy?*', '*phi*', '*theta*', '*steptable*']

;-----------------------------------------------------
; Load the FPI \\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\
;-----------------------------------------------------
	;Get the FPI distribution function
	MrMMS_Load_Data, sc, 'fpi', mode, level, $
	                 OPTDESC   = 'd' + species + 's-dist', $
	                 SUFFIX    = suffix, $
	                 TEAM_SITE = team_site, $
	                 VARFORMAT = varformat, $
	                 VARNAMES  = varnames
	
	;Associate variable attributes with DEPEND_[0-3]
	dist_vname = sc + '_d' + species + 's_dist_' + mode + suffix
	self -> Load_Meta, dist_vname

;-----------------------------------------------------
; Finish up \\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\
;-----------------------------------------------------
	return, varnames
end


;+
;   Find and read MMS FPI data.
;
; :Params:
;       SC:                 in, required, type=string/strarr
;                           The MMS spacecraft identifier. Options are:
;                               {'mms1' | 'mms2' | 'mms3' | 'mms4'}
;       MODE:               in, required, type=string/strarr
;                           Telemetry mode of the data. Options include: {'fast' | 'brst'}
;       SPECIES:            in, optional, type=string, default='e'
;                           Particle species for which the distribution function is
;                               to be loaded. Options are: {'e' | 'i'} for electrons
;                               and ions, respectively.
;       FAC:                in, optional, type=string
;                           The field-aligned coordinate system into which the instrument
;                               look directions should be rotated.
;
; :Keywords:
;       COORD_SYS:          in, optional, type=string, default='gse'
;                           Coordinate system of the original distribution function.
;                               Options are: {'dbcs' | 'gse'}.
;       LEVEL:              in, optional, type=string/strarr, default='l2'
;                           Data quality level. Options include:
;                               {'sitl' | 'l1b' | 'l2'}
;       TEAM_SITE:          in, optional, type=boolean, default=0
;                           If set, requests will be sent to the team site (password
;                               required, L1A and above). Automatically set if `LEVEL`
;                               is below level 2. This option is sticky.
;       TRANGE:             out, optional, type=string, default=MrVar_GetTRange()
;                           Start and end times over which to read data.
;       VARNAMES:           out, optional, type=strarr
;                           Names of all variables that have been loaded.
;-
pro MrMMS_FPI_Dist4D::Load_FAC, sc, mode, fac_type, $
COORD_SYS = coord_sys, $
INSTR     = instr, $
LEVEL     = level, $
NO_LOAD   = no_load, $
SUFFIX    = suffix, $
TRANGE    = trange
	compile_opt idl2

	catch, the_error
	if the_error ne 0 then begin
		catch, /CANCEL
		MrPrintF, 'LogErr'
		
		;Delete any variables that were loaded
		MrVar_Delete, varnames
		
		return
	endif
	
	if n_elements(suffix) eq 0 then suffix = ''
	vsuffix = suffix + '_Dist3D_FAC'
	
	;Load FAC data
	MrMMS_FGM_Load_FAC_Data, sc, mode, fac_type, self.oTime, $
	                         COORD_SYS = coord_sys, $
	                         INSTR     = instr, $
	                         LEVEL     = level, $
	                         NO_LOAD   = no_load, $
	                         SUFFIX    = vsuffix, $
	                         TRANGE    = trange, $
	                         VARNAMES  = varnames

	;Get variable objects
	oPar = MrVar_Get(varnames[0])
	if n_elements(varnames) gt 1 then oPerp = MrVar_Get(varnames[1])
	
	;Remove variables from caceh
	MrVar_Remove, varnames
	
	;Set FAC grid
	self -> SetFAC, oPar, oPerp, fac_type
end


;+
;   Create new energy tables and set the distribution function's DEPEND_1-3 variable
;   attributes to be variables with physical units.
;
; :Private:
;
; :Params:
;       NAMES:          in, required, type=string
;                       Name of the distribution function variable.
;-
pro MrMMS_FPI_Dist4D::Load_Meta, name
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
;   The class definition statement.
;
; :Params:
;       CLASS:          out, optional, type=structure
;-
pro MrMMS_FPI_Dist4D__DEFINE
	compile_opt idl2
	
	class = { MrMMS_FPI_Dist4D, $
	          inherits MrDist4D $
	        }
end