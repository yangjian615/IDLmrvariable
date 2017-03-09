; docformat = 'rst'
;
; NAME:
;       MrMMS_EDI_Load_PGA
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
;   Load EDI Q0 data and compute pitch angle and gyrophase angles. 
;
;   FGM data uses AFG for srvy and DFG for brst. Onboard, EDI determines its look
;   direction using data from DFG, and in srvy mode, DFG's sampling rate is closer
;   than AFG's to that of EDI. For this reason, srvy data is processed using DFG
;   L2PRE data (requires access to team site).
;
;   The perpendicular plane using either the 'VexB' or 'ExB' direction. The default is
;   to use use 'EXB' in srvy mode (assumes plasma is frozen-in), because FPI does not
;   operate in slow survey, and 'VxB' in brst mode to better represent electron data at
;   times when the plasma is not frozen-in.
;
;   EDP fast data is used when processing brst mode data because its sampling rate
;   (32 S/s) is the same as that of DFG and ~FPI.
;
; :Categories:
;       MMS, EDI, MrVariable
;
; :Params:
;       SC:                 in, required, type=string/strarr
;                           The MMS spacecraft identifier. Options are:
;                               {'mms1' | 'mms2' | 'mms3' | 'mms4'}
;       MODE:               in, required, type=string/strarr
;                           Telemetry mode of the data. Options include:
;                               {'slow' | 'fast' | 'srvy' | 'brst'}
;       OPTDESC:            in, optional, type=string, default=''
;                           Optional descriptor of the data.
;
; :Keywords:
;       DT:                 in, optional, type=string/objref, default=2.5
;                           Duration, in seconds, of each time bin.
;       DPA:                in, optional, type=float, default=1.0
;                           Width, in degrees, of each pitch angle bin.
;       DGA:                in, optional, type=float, default=11.25
;                           Width, in degrees, of each gyrophase bin.
;       FAC:                in, optional, type=string, default='EXB' for srvy and 'VXB' for brst
;                           Name of the field-aligned coordinate system used
;                               to define the directions perpendicular to B.
;                               Options include 'VXB' and 'EXB'
;       LEVEL:              in, required, type=string/strarr
;                           Data quality level. Options include:
;                               {'l1a' | 'l1b' | 'l2pre' | 'l2'}
;       PARANGE:            out, optional, type=string, default=[80\, 100]
;                           Range of pitch angles over which to bin.
;       GARANGE:            out, optional, type=string, default=[-180\, 180]
;                           Range of gyrophase angles over which to bin.
;       SEPARATE:           in, optional, type=boolean, default=0
;                           If set, the GDU1 and GDU2 data will be binned separately.
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
;       2014/06/28  -   Written by Matthew Argall
;-
;*****************************************************************************************
;+
;
;-
pro MrMMS_EDI_Load_PGA, sc, mode, optdesc, $
DT=dt, $
DPA=dPA, $
DGA=dGA, $
FAC=fac, $
PARANGE=paRange, $
GARANGE=gaRange, $
SEPARATE=separate
	compile_opt idl2

	catch, the_error
	if the_error ne 0 then begin
		catch, /CANCEL
		if n_elements(win) gt 0 then obj_destroy, win
		MrPrintF, 'LogErr'
		return
	endif
	
	;Defaults
	cs          = 'gse'
	tf_separate = keyword_set(separate)
	if n_elements(level)   eq 0 then level   = 'l2'
	if n_elements(mode)    eq 0 then mode    = 'srvy'
	if n_elements(optdesc) eq 0 then optdesc = 'amb'
	if n_elements(fac)     eq 0 then fac     = mode eq 'srvy' ? 'EXB' : 'VXB'

;-------------------------------------------
; EDI Data /////////////////////////////////
;-------------------------------------------
	prefix   = sc + '_'
	suffix   = '_' + mode + '_' + level

	;Variable names
	c_gdu1_vname    = prefix + 'edi_counts_gdu1'           + suffix
	c_gdu2_vname    = prefix + 'edi_counts_gdu2'           + suffix
	traj_gdu1_vname = prefix + 'edi_traj_'  + cs + '_gdu1' + suffix
	traj_gdu2_vname = prefix + 'edi_traj_'  + cs + '_gdu2' + suffix

	;Get EDI data
	MrMMS_Load_Data, sc, 'edi', mode, level, $
	                 OPTDESC   = 'q0', $
	                 VARFORMAT = ['*counts*', '*traj_' + cs + '_gdu?' + suffix]
	
;-------------------------------------------
; FGM Data /////////////////////////////////
;-------------------------------------------
	;Variable name
	;   - FGM uses DFG for burst data
	if mode eq 'brst' then begin
		fgm_level = 'l2'
		fgm_instr = 'fgm'
		b_vname   = prefix + fgm_instr + '_b_' + cs + '_' + mode + '_' + fgm_level
	endif else begin
		fgm_level = 'l2pre'
		fgm_instr = 'dfg'
		b_vname   = prefix + fgm_instr + '_' + mode + '_' + fgm_level + '_' + cs
	endelse 

	;Get FGM data
	MrMMS_Get_Data, sc, fgm_instr, mode, fgm_level, $
	                VARFORMAT = b_vname
	
	;Retrieve the data
	oB_temp = MrVar_Get(b_vname)
	b_data  = oB_temp -> GetData()
	
	;Create a Vector and transfer attributes
	oB_gse = MrVectorTS(b_data[0:2,*])
	oB_mag = MrScalarTS(b_data[3,*])
	b_data = !Null
	
	;Copy attributes
	oB_temp -> CopyAttrTo, oB_gse
	oB_temp -> CopyAttrTo, oB_mag

;-------------------------------------------
; FPI Data /////////////////////////////////
;-------------------------------------------
	;Get FPI data
	if strupcase(fac) eq 'VXB' then begin
		;Reminder warning
		;   - GSE is not available yet
		;   - FPI does not operate in slow survey, so there is no "srvy" product.
		MrPrintF, 'LogWarn', 'FPI data is not available in GSE. Using DBCS.'
		fpi_mode = mode eq 'brst' ? mode : 'fast'
		fpi_cs = 'dbcs'
		
		;Variable names
		vx_vname = prefix + 'des_bulkx_' + fpi_cs + '_' + fpi_mode
		vy_vname = prefix + 'des_bulky_' + fpi_cs + '_' + fpi_mode
		vz_vname = prefix + 'des_bulkz_' + fpi_cs + '_' + fpi_mode
		
		;Load the data
		MrMMS_Get_Data, sc, 'fpi', fpi_mode, level, $
		                OPTDESC   = 'des-moms', $
		                VARFORMAT = '*bulk?_' + fpi_cs + '*'
		
		;Extract the data
		oVx_gse = MrVar_Get(vx_vname)
		oVy_gse = MrVar_Get(vy_vname)
		oVz_gse = MrVar_Get(vz_vname)

		;Combine Vx, Vy, and Vz
		oPerp_Vec = MrVectorTS( [ [oVx_gse['DATA']], [oVy_gse['DATA']], [oVz_gse['DATA']] ] )
		oVx_gse  -> CopyAttrTo, oPerp_Vec

;-------------------------------------------
; EDP Data /////////////////////////////////
;-------------------------------------------
	endif else if strupcase(fac) eq 'EXB' then begin
		;EDP does not make a srvy product; slow and fast are kept separate.
		if mode eq 'srvy' then MrPrintF, 'LogWarn', 'TODO: Combine slow and fast survey data.'
		edp_mode = 'fast'
		
		;Variable names
		E_vname = prefix + 'edp_dce_' + cs + '_' + edp_mode + '_' + level
		
		;Load the data
		MrMMS_Get_Data, sc, 'edp', edp_mode, level, $
		                OPTDESC   = 'dce', $
		                VARFORMAT = E_vname
		
		;Retrieve the data
		oPerp_Vec = MrVar_Get(E_vname)
	endif else begin
		message, 'FAC must be {"VxB" | "ExB"}'
	endelse

;-------------------------------------------
; Compute PA & GA //////////////////////////
;-------------------------------------------
	;Retrieve data
	oC_GDU1    = MrVar_Get(c_gdu1_vname)
	oC_GDU2    = MrVar_Get(c_gdu2_vname)
	oTraj_GDU1 = MrVar_Get(traj_gdu1_vname)
	oTraj_GDU2 = MrVar_Get(traj_gdu2_vname)

	;Output Variable Names
	pa_gdu1_name = prefix + 'edi_pa_gdu1' + suffix
	pa_gdu2_name = prefix + 'edi_pa_gdu2' + suffix
	ga_gdu1_name = prefix + 'edi_ga_gdu1' + suffix
	ga_gdu2_name = prefix + 'edi_ga_gdu2' + suffix

	;Compute angles
	MrMMS_EDI_PGA_Compute, oTraj_GDU1, oB_gse, oPerp_Vec, fac, $
	                       NAME_PA = pa_gdu1_name, $
	                       NAME_GA = ga_gdu1_name

	MrMMS_EDI_PGA_Compute, oTraj_GDU2, oB_gse, oPerp_Vec, fac, $
	                       NAME_PA = pa_gdu2_name, $
	                       NAME_GA = ga_gdu2_name

;-------------------------------------------
; GDU1 and GDU2 Separately /////////////////
;-------------------------------------------
	if tf_separate then begin
	;-------------------------------------------
	; GDU1 /////////////////////////////////////
	;-------------------------------------------
		;Names
		t_bins_gdu1_name  = prefix + 'edi_epoch_bins_gdu1' + suffix
		pa_bins_gdu1_name = prefix + 'edi_pa_bins_gdu1'    + suffix
		ga_bins_gdu1_name = prefix + 'edi_ga_bins_gdu1'    + suffix
		pad_gdu1_name     = prefix + 'edi_pad_gdu1'        + suffix
		gpd_gdu1_name     = prefix + 'edi_gpd_gdu1'        + suffix

		;PAD
		MrMMS_EDI_PGA_Bin, oC_GDU1, pa_gdu1_name, dt, dPA, $
		                   ARANGE    = paRange, $
		                   DEP0_NAME = t_bins_gdu1_name, $
		                   DEP1_NAME = pa_bins_gdu1_name, $
		                   NAME      = pad_gdu1_name
	
		;GPD
		MrMMS_EDI_PGA_Bin, oC_GDU1, ga_gdu1_name, dt, dGA, $
		                   /GA, $
		                   ARANGE    = gaRange, $
		                   DEP0_NAME = t_bins_gdu1_name, $
		                   DEP1_NAME = ga_bins_gdu1_name, $
		                   NAME      = gpd_gdu1_name
		
		
	;-------------------------------------------
	; GDU2 /////////////////////////////////////
	;-------------------------------------------
		;Names
		t_bins_gdu2_name  = prefix + 'edi_epoch_bins_gdu2' + suffix
		pa_bins_gdu2_name = prefix + 'edi_pa_bins_gdu2'    + suffix
		ga_bins_gdu2_name = prefix + 'edi_ga_bins_gdu2'    + suffix
		pad_gdu2_name     = prefix + 'edi_pad_gdu2'        + suffix
		gpd_gdu2_name     = prefix + 'edi_gpd_gdu2'        + suffix
	
		;PAD
		MrMMS_EDI_PGA_Bin, oC_GDU2, pa_gdu2_name, dt, dPA, $
		                   ARANGE    = paRange, $
		                   DEP0_NAME = t_bins_gdu2_name, $
		                   DEP1_NAME = pa_bins_gdu2_name, $
		                   NAME      = pad_gdu2_name
	
		;GPD
		MrMMS_EDI_PGA_Bin, oC_GDU2, ga_gdu2_name, dt, dGA, $
		                   /GA, $
		                   ARANGE    = gaRange, $
		                   DEP0_NAME = t_bins_gdu2_name, $
		                   DEP1_NAME = ga_bins_gdu2_name, $
		                   NAME      = gpd_gdu2_name
	
;-------------------------------------------
; GDU1 and GDU2 Together ///////////////////
;-------------------------------------------
	endif else begin
	;-------------------------------------------
	; Combine Data /////////////////////////////
	;-------------------------------------------
		;Names for joined variables
		t_name  = prefix + 'edi_epoch'  + suffix
		c_name  = prefix + 'edi_counts' + suffix
		pa_name = prefix + 'edi_pa'     + suffix
		ga_name = prefix + 'edi_ga'     + suffix
		
		;Get variables
		oDep0_GDU1 = MrVar_Get(oC_GDU1['DEPEND_0'])
		oDep0_GDU2 = MrVar_Get(oC_GDU2['DEPEND_0'])
		oPA_GDU1   = MrVar_Get(pa_gdu1_name)
		oPA_GDU2   = MrVar_Get(pa_gdu2_name)
		oGA_GDU1   = MrVar_Get(ga_gdu1_name)
		oGA_GDU2   = MrVar_Get(ga_gdu2_name)
		
		;Concatenate data from each GDU
		if oDep0_GDU1[0, 0:10] ne oDep0_GDU2[0, 0:10] then message, 'Data starts on two different days.'
		oTime   = MrTimeVar(  [ oDep0_GDU1['DATA'],      oDep0_GDU2['DATA'] ],      NAME=t_name,  /CACHE )
		oPA     = MrScalarTS( [ oPA_GDU1['DATA'],        oPA_GDU2['DATA'] ],        NAME=pa_name, /CACHE )
		oGA     = MrScalarTS( [ oGA_GDU1['DATA'],        oGA_GDU2['DATA'] ],        NAME=ga_name, /CACHE )
		oCounts = MrScalarTS( [ reform(oC_GDU1['DATA']), reform(oC_GDU2['DATA']) ], NAME=c_name,  /CACHE )

		;Sort data
		oTime   -> Sort, INDEX=index
		oPA     -> SetData, oPA[index]
		oGA     -> SetData, oGA[index]
		oCounts -> SetData, oCounts[temporary(index)]
		
		;Set Attributes
		oPA     -> AddAttr, 'DEPEND_0', t_name
		oGA     -> AddAttr, 'DEPEND_0', t_name
		oCounts -> AddAttr, 'DEPEND_0', t_name

	;-------------------------------------------
	; Bin Data /////////////////////////////////
	;-------------------------------------------
		;Names
		t_bins_name  = prefix + 'edi_epoch_bins' + suffix
		pa_bins_name = prefix + 'edi_pa_bins'    + suffix
		ga_bins_name = prefix + 'edi_ga_bins'    + suffix
		pad_name     = prefix + 'edi_pad'        + suffix
		gpd_name     = prefix + 'edi_gpd'        + suffix
	
		;PAD
		MrMMS_EDI_PGA_Bin, oCounts, oPA, dt, dPA, $
		                   ARANGE    = paRange, $
		                   DEP0_NAME = t_bins_name, $
		                   DEP1_NAME = pa_bins_name, $
		                   NAME      = pad_name
	
		;GPD
		MrMMS_EDI_PGA_Bin, oCounts, oGA, dt, dGA, $
		                   /GA, $
		                   ARANGE    = gaRange, $
		                   DEP0_NAME = t_bins_name, $
		                   DEP1_NAME = ga_bins_name, $
		                   NAME      = gpd_name

	;-------------------------------------------
	; Free Data ////////////////////////////////
	;-------------------------------------------
		MrVar_Delete, [t_name, c_name, pa_name, ga_name]
	endelse
end