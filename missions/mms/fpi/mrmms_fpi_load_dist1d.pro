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
SUPPORT_DATA=support_data, $
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
	if n_elements(fac)  eq 0 then fac = 'VXB'
	if n_elements(sc)   ne 1 then message, 'SC must be scalar.'
	if n_elements(mode) ne 1 then message, 'MODE must be scalar.'

;-----------------------------------------------------
; Load the Data \\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\
;-----------------------------------------------------
	;Get the FPI distribution function
	MrMMS_FPI_Load_Dist3D, sc, mode, species, fac, $
	                       COORD_SYS = coord_sys, $
	                       LEVEL     = level, $
	                       VARNAMES  = varnames
	
;-----------------------------------------------------
; Create Variable Names \\\\\\\\\\\\\\\\\\\\\\\\\\\\\\
;-----------------------------------------------------
	
	;Variable Names
	theta_vname     = sc + '_d' + species + 's_theta_'            + mode
	theta_fac_vname = sc + '_d' + species + 's_theta_fac_'        + mode
	phi_fac_vname   = sc + '_d' + species + 's_phi_fac_'          + mode
	phi_vname       = sc + '_d' + species + 's_phi_'              + mode
	e_vname         = sc + '_d' + species + 's_energy_table_'     + mode
	dist_fac_vname  = sc + '_d' + species + 's_dist_fac_'         + mode
	vec_vname       = sc + '_d' + species + 's_bulkv_' + cs + '_' + mode
	b_vname         = sc + '_fgm_bvec_gse_'                       + mode + '_l2'
	
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