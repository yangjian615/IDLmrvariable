; docformat = 'rst'
;
; NAME:
;       MrMMS_FPI_Load_Dist3D
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
;   Load FPI distribution function data and rotate the instrument look directions
;   into a field-aligned coordinate system.
;
; :Categories:
;       CDF Utilities
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
;-
;*****************************************************************************************
;+
;   Load data associated with a particular field-aligned coordinate system.
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
;       LEVEL:              in, optional, type=string/strarr, default='l2'
;                           Data quality level. Options include:
;                               {'sitl' | 'l1b' | 'l2'}
;       VARNAMES:           out, optional, type=strarr
;                           Names of all variables that have been loaded.
;-
pro MrMMS_FPI_Load_Dist3D_FACData, sc, mode, fac, species, cs, $
LEVEL    = level, $
VARNAMES = varnames
	compile_opt idl2
	on_error, 2
	
	;Load data for coordinate system
	if fac eq 'VXB' then begin
		;Load the data
		MrMMS_FPI_Load_Data, sc, mode, $
		                     LEVEL     = level, $
		                     OPTDESC   = 'd' + species + 's-moms', $
		                     VARFORMAT = '*bulk?_' + cs + '*', $
		                     VARNAMES  = vec_names
		
	endif else if fac eq 'EXB' then begin
		MrMMS_Load_Data, sc, 'edp', 'fast', level, $
		                 OPTDESC   = 'dce', $
		                 VARFORMAT = '*dce_gse*', $
		                 VARNAMES  = vec_names
	endif else if fac ne '' then begin
		message, 'FAC value not recognized: "' + fac + '".'
	endif
	
	;Get B-field data
	MrMMS_FGM_Load_Data, sc, mode, $
	                     INSTR     = instr, $
	                     LEVEL     = level, $
	                     VARFORMAT = '*_b_gse_*', $
	                     VARNAMES  = varnames
	
	;Combine variable names
	if n_elements(vec_names) gt 0 then varnames = [varnames, vec_names]
end


;+
;   Rotate the instrument look-direction grid into field-aligned coordinates.
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
;       ORIENTATION:    in, optional, type=integer, default=3
;                       Orientation of the sphereical grid. See
;                           MrVar_Grid_Cart2Sphere.pro for options.
;-
pro MrMMS_FPI_Load_Dist3D_Rotate, sc, mode, fac, species, cs, $
VARNAMES=varnames
	compile_opt idl2
	on_error, 2
	
;-----------------------------------------------------
; Create Variable Names \\\\\\\\\\\\\\\\\\\\\\\\\\\\\\
;-----------------------------------------------------
	
	;Variable Names
	theta_vname = sc + '_d' + species + 's_theta_'            + mode
	phi_vname   = sc + '_d' + species + 's_phi_'              + mode
	e_vname     = sc + '_d' + species + 's_energy_table_'     + mode
	dist_vname  = sc + '_d' + species + 's_dist_'             + mode
	
	;New variable names
	phi_fac_vname   = sc + '_d' + species + 's_phi_fac_'   + mode
	theta_fac_vname = sc + '_d' + species + 's_theta_fac_' + mode
	dist_fac_vname  = sc + '_d' + species + 's_dist_fac_'  + mode

;-----------------------------------------------------
; Interpolate Data \\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\
;-----------------------------------------------------
	;Get the variables
	oDist = MrVar_Get(dist_vname)
	oB    = MrVar_Get(b_vname)
	oV    = MrVar_Get(vec_vname)

	;Interpolate B and V to the distribution function time tags
	oB = oB -> Interpol(oDist)
	oV = oV -> Interpol(oDist)

;-----------------------------------------------------
; Put time first \\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\
;-----------------------------------------------------
	;Azimuth
	oPhi    = MrVar_Get(phi_vname)
	oPhiTS  = oPhi -> Transpose([1,0])
	
	;Distribution
	oDist    = MrVar_Get(dist_vname)
	oDistTS  = oDist -> Transpose([3,0,1,2])

;-----------------------------------------------------
; Rotate Coordinate Systems \\\\\\\\\\\\\\\\\\\\\\\\\\
;-----------------------------------------------------

	;Convert from instrument coordinates to field-aligned coordinates
	MrVar_Grid_sphere2fac, oB, oPhiTS, theta_vname, oPhi_FAC, oTheta_FAC, $
	                       ORIENTATION = orientation, $
	                       /SPHERE, $
	                       TYPE        = fac, $
	                       VEC         = oV

;-----------------------------------------------------
; Store Variables \\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\
;-----------------------------------------------------
	;Phi
	oPhi_FAC -> Cache
	oPhi_FAC -> SetName, phi_fac_vname
	oPhi_FAC -> AddAttr, 'DEPEND_0', oDistTS['DEPEND_0']
	
	;Theta
	oTheta_FAC -> Cache
	oTheta_FAC -> SetName, theta_fac_vname
	oTheta_FAC -> AddAttr, 'DEPEND_0', oDistTS['DEPEND_0']

	;Distribution
	oDistTS -> SetName, dist_fac_vname
	MrVar_Replace, oDist, oDistTS
	oDistTS -> SetAttrValue, 'DEPEND_1', phi_fac_vname
	oDistTS -> SetAttrValue, 'DEPEND_2', theta_fac_vname

	;Variable names
	varnames = [phi_fac_vname, theta_fac_vname, dist_fac_vname]
end


;+
;   Create new energy tables and set the distribution function's DEPEND_1-3 variable
;   attributes to be variables with physical units.
;
; :Params:
;       NAMES:          in, required, type=string
;                       Name of the distribution function variable.
;-
pro MrMMS_FPI_Load_Dist3D_Meta, name
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
;       ORIENTATION:        in, optional, type=integer, default=3
;                           Orientation of the sphereical grid. See
;                               MrVar_Grid_Cart2Sphere.pro for options.
;       TEAM_SITE:          in, optional, type=boolean, default=0
;                           If set, requests will be sent to the team site (password
;                               required, L1A and above). Automatically set if `LEVEL`
;                               is below level 2. This option is sticky.
;       TRANGE:             out, optional, type=string, default=MrVar_GetTRange()
;                           Start and end times over which to read data.
;       VARNAMES:           out, optional, type=strarr
;                           Names of all variables that have been loaded.
;-
pro MrMMS_FPI_Load_Dist3D, sc, mode, species, fac, $
COORD_SYS=coord_sys, $
LEVEL=level, $
ORIENTATION=orientation, $
SUFFIX=suffix, $
TEAM_SITE=team_site, $
TRANGE=trange, $
VARNAMES=varnames
	compile_opt idl2

	catch, the_error
	if the_error ne 0 then begin
		catch, /CANCEL
		MrPrintF, 'LogErr'
		return
	endif
	
	;Check inputs
	cs = n_elements(coord_sys) eq 0 ? 'gse' : strlowcase(coord_sys)
	if n_elements(level)   eq 0 then level   = 'l2'
	if n_elements(species) eq 0 then species = 'e'
	if n_elements(suffix)  eq 0 then suffix  = ''
	
	;Conflicts
	if n_elements(sc)    ne 1 then message, 'SC must be scalar.'
	if n_elements(mode)  ne 1 then message, 'MODE must be scalar.'
	
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
	dist_vname = sc + '_d' + species + 's_dist_' + mode
	MrMMS_FPI_Load_Dist3D_Meta, dist_vname

	;Put time first
	oDist   = MrVar_Get(dist_vname)
	oDistTS = oDist -> Transpose([3,0,1,2])

;-----------------------------------------------------
; FAC \\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\
;-----------------------------------------------------
	if n_elements(fac) gt 0 then begin

	;-----------------------------------------------------
	; Rotate to FAC \\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\
	;-----------------------------------------------------
		;Load FAC data and interpolate onto FPI-DIST data
		MrMMS_FGM_Load_FAC_Data, sc, mode, fac, dist_vname, $
		                         SUFFIX   = suffix, $
		                         VARNAMES = fac_vnames
		
		;FAC variables
		b_vname = fac_vnames[0]
		if n_elements(fac_vnames) gt 1 then perp_vname = fac_vnames[1]

		;Convert from instrument coordinates to field-aligned coordinates
		MrVar_Grid_sphere2fac, b_vname, oDist['DEPEND_1'], oDist['DEPEND_2'], oPhi_FAC, oTheta_FAC, $
		                       ORIENTATION = orientation, $
		                       /SPHERE, $
		                       TYPE        = fac, $
		                       VEC         = perp_vname
	
	;-----------------------------------------------------
	; Store Variables \\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\
	;-----------------------------------------------------
		;Output Names
		dist_fac_vname  = sc + '_d' + species + 's_dist_fac_'  + mode + suffix
		phi_fac_vname   = sc + '_d' + species + 's_phi_fac_'   + mode + suffix
		theta_fac_vname = sc + '_d' + species + 's_theta_fac_' + mode + suffix
		
		;Set names
		oPhi_FAC   -> SetName, phi_fac_vname
		oTheta_FAC -> SetName, theta_fac_vname
		
		;Add to variable cache
		oPhi_FAC   -> Cache
		oTheta_FAC -> Cache
		
		;Set time dependence
		if mode eq 'brst' then begin
			oPhi_FAC   -> AddAttr, 'DEPEND_0', oDistTS['DEPEND_0']
			oTheta_FAC -> AddAttr, 'DEPEND_0', oDistTS['DEPEND_0']
		endif
	
		;Distribution
		oDistTS -> SetName, dist_fac_vname
		MrVar_Replace, oDist, oDistTS
		oDistTS -> SetAttrValue, 'DEPEND_1', phi_fac_vname
		oDistTS -> SetAttrValue, 'DEPEND_2', theta_fac_vname
	
		;Variable names
		varnames = [dist_vname, phi_fac_vname, theta_fac_vname, oDist['DEPEND_3'], fac_vnames]

;-----------------------------------------------------
; Create Grid \\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\
;-----------------------------------------------------
	endif else begin
		;Data variable names
		phi_vname   = string(sc, species, level, FORMAT='(%"%s_d%ss_phi_%s")')
		theta_vname = string(sc, species, level, FORMAT='(%"%s_d%ss_theta_%s")')
		dist_vname  = string(sc, species, level, FORMAT='(%"%s_d%ss_dist_%s")')
		
		;Output variable names
		phi_grid_vname   = string(sc, species, level, FORMAT='(%"%s_d%ss_phi_grid_%s")')
		theta_grid_vname = string(sc, species, level, FORMAT='(%"%s_d%ss_theta_grid_%s")')
		
		;Create the cartesian grid
		MrVar_Grid_Sphere2Cart, phi_vname, theta_vname, oX, oY, oZ, /DEGREES
		
		;Convert back to spherical grid
		MrVar_Grid_Cart2Sphere, oX, oY, oZ, oPhiGrid, oThetaGrid, $
		                        /DEGREES, $
		                        ORIENTATION = orientation
		
		;Save PHI
		oPhiGrid -> SetName, phi_grid_vname
		oPhiGrid -> Cache
		oPhiGrid -> AddAttr, 'DEPEND_0', oDistTS['DEPEND_0']
		oPhiGrid -> AddAttr, 'DEPEND_1', phi_vname
		oPhiGrid -> AddAttr, 'DEPEND_2', theta_vname
		
		;Save THETA
		oThetaGrid -> SetName, theta_grid_vname
		oThetaGrid -> Cache
		oThetaGrid -> AddAttr, 'DEPEND_0', oDist['DEPEND_0']
		oThetaGrid -> AddAttr, 'DEPEND_1', phi_vname
		oThetaGrid -> AddAttr, 'DEPEND_2', theta_vname
		
		;Update varnames
		varnames = [varnames, phi_grid_vname, theta_grid_vname]
	endelse
end