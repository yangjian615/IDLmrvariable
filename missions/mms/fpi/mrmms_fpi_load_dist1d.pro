; docformat = 'rst'
;
; NAME:
;       MrMMS_FPI_Load_Dist1D
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
;   Read 3D distribution function data and reduce it to three 1D distribution
;   functions in Energy, pitch angle, and gyrophase.
;
; :Categories:
;       CDF Utilities
;
; :Params:
;       SC:                 in, required, type=string/strarr
;                           The MMS spacecraft identifier. Options are:
;                               {'mms1' | 'mms2' | 'mms3' | 'mms4'}
;       MODE:               in, required, type=string/strarr
;                           Telemetry mode of the data. Options include:
;                               {'slow' | 'fast' | 'srvy' | 'brst'}
;       SPECIES:            in, required, type=string
;                           Particle species for the distribution. Options include:
;                               { 'e' | 'i' }
;       FAC:                in, optional, type=string, default='VXB'
;                           Field-aligned coordinate system into which to rotate the
;                               distribution. Options include: { 'VxB' | 'ExB' | '' }
;
; :Keywords:
;       COORD_SYS:          in, optional, type=string, default='GSE'
;                           Original data coordinate system. Options include:
;                               { 'dbcs' | 'gse' }
;       LEVEL:              in, optional, type=string/strarr, default='l2'
;                           Data quality level. Options include:
;                               {'l1b' | 'sitl' | 'l2'}
;       NO_LOAD:            in, optional, type=boolean, default=0
;                           If set, data will not be read from CDF and loaded into the cache.
;       TEAM_SITE:          in, optional, type=boolean, default=0
;                           If set, requests will be sent to the team site (password
;                               required, L1A and above). Automatically set if `LEVEL`
;                               is below level 2. This option is sticky.
;       TRANGE:             out, optional, type=string, default=MrVar_GetTRange()
;                           Start and end times over which to read data.
;       VARNAMES:           out, optional, type=string,
;                           Variable names that were loaded into the variable cache as
;                               a result of this program.
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
pro MrMMS_FPI_Load_Dist1D, sc, mode, species, fac, $
COORD_SYS=coord_sys, $
LEVEL=level, $
NO_LOAD=no_load, $
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
	tf_load = ~keyword_set(no_load)
	cs      = n_elements(coord_sys) eq 0 ? 'gse' : strlowcase(coord_sys)
	if n_elements(fac)  eq 0 then fac = 'VXB'
	if n_elements(sc)   ne 1 then message, 'SC must be scalar.'
	if n_elements(mode) ne 1 then message, 'MODE must be scalar.'

;-----------------------------------------------------
; Load the Data \\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\
;-----------------------------------------------------
	;Get the FPI distribution function
	if tf_load then begin
		MrMMS_FPI_Load_Dist3D, sc, mode, species, fac, $
		                       COORD_SYS = coord_sys, $
		                       LEVEL     = level, $
		                       TEAM_SITE = team_site, $
		                       TRANGE    = trange, $
		                       VARNAMES  = varnames
	endif
	
;-----------------------------------------------------
; Create Variable Names \\\\\\\\\\\\\\\\\\\\\\\\\\\\\\
;-----------------------------------------------------
	fpi_prefix = sc + '_d' + species + 's_'
	fpi_suffix = '_' + mode
	
	;Variable Names
	theta_vname     = fpi_prefix + 'theta'        + fpi_suffix
	theta_fac_vname = fpi_prefix + 'theta_fac'    + fpi_suffix
	phi_fac_vname   = fpi_prefix + 'phi_fac'      + fpi_suffix
	phi_vname       = fpi_prefix + 'phi'          + fpi_suffix
	e_vname         = fpi_prefix + 'energy_table' + fpi_suffix
	dist_fac_vname  = fpi_prefix + 'dist_fa'      + fpi_suffix
	vec_vname       = fpi_prefix + 'bulkv_' + cs  + fpi_suffix
	b_vname         = sc + '_fgm_bvec_gse_'       + mode + '_l2'
	
	;New variable names
	pad_vname = sc + '_d' + species + 's_pad_'   + mode
	gpd_vname = sc + '_d' + species + 's_gpd_'   + mode
	ed_vname  = sc + '_d' + species + 's_espec_' + mode

;-----------------------------------------------------
; Create 1D Distributions \\\\\\\\\\\\\\\\\\\\\\\\\\\\
;-----------------------------------------------------

	;Create the energy spectrogram
	MrVar_DF_Reduce, dist_fac_vname, phi_fac_vname, theta_fac_vname, e_vname, $
	                 ESPEC = oESpec, $
	                 PAD   = oPAD, $
	                 GPD   = oGPD

;-----------------------------------------------------
; Store Variables \\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\
;-----------------------------------------------------
	
	;Names
	oESpec -> SetName, ed_vname
	oPAD   -> SetName, pad_vname
	oGPD   -> SetName, gpd_vname
	
	;
	;Attributes
	;
	
	;Energy Spectrogram
	oESpec -> AddAttr, 'SCALE',         1
	oESpec -> AddAttr, 'LOG',           1
	oESpec -> AddAttr, 'MISSING_VALUE', 0
	
	;Energy table
	oETable = MrVar_Get(oESpec['DEPEND_1'])
	oETable -> AddAttr, 'LOG', 1
	
	;Pitch angle distribution
	oPAD -> SetAttrValue, 'DEPEND_1', theta_vname
	oPAD -> AddAttr,      'SCALE',         1
	oPAD -> AddAttr,      'LOG',           1
	oPAD -> AddAttr,      'MISSING_VALUE', 0
	oPAD -> AddAttr,      'YLOG',          0
	
	;Gyrophase distribution
	oGPD -> SetAttrValue, 'DEPEND_1', phi_vname
	oGPD -> AddAttr,      'SCALE',         1
	oGPD -> AddAttr,      'LOG',           1
	oGPD -> AddAttr,      'MISSING_VALUE', 0
	oGPD -> AddAttr,      'YLOG',          0
	
	;Cache
	oESpec -> Cache
	oPAD   -> Cache
	oGPD   -> Cache
end