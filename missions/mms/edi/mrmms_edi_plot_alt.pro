; docformat = 'rst'
;
; NAME:
;       MrMMS_EDI_Plot_ALT
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
;   Load EDI alternating-mode data (as opposed to field-aligned and perpendicular). Rotate
;   trajectories into a field-aligned coordinate system and plot.
;       A) B-field                     A) B-field
;       B) 0-Degree Fluxes             B) 0-Degree Trajectories
;       C) 90-Degree GDU1 Fluxes       C) 90-Degree GDU1 Trajectories
;       D) 90-Degree GDU2 Fluxes       D) 90-Degree GDU2 Trajectories
;       E) 180-Degree Fluxes           E) 180-Degree Trajectories
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
;                           Optional descriptor of the data. Options are:
;                               {'amb-alt-cc' | 'amb-alt-oc' | 'amb-alt-oom' | 'amb-alt-oob'}
;
; :Keywords:
;       COORDS:             in, optional, type=string, default='dbcs'
;                           Original data coordinate system before transforming to FAC.
;                               Options are {'dbcs', 'gse'}.
;       FAC:                in, optional, type=string, default=''
;                           Name of the field-aligned coordinate system used
;                               to define the directions perpendicular to B.
;                               Options include {'' | 'VXB' | 'EXB'}
;       LEVEL:              in, required, type=string/strarr
;                           Data quality level. Options include: {'ql' | 'l2'}
;       NO_LOAD:            in, optional, type=boolean, default=0
;                           If set, data is not re-read and loaded into the variable cache.
;       TRANGE:             in, optional, type=strarr(2), default=MrVar_GetTRange
;                           The start and end times of the data interval to be loaded.
;                               Formatting is: 'YYYY-MM-DDThh:mm:ss'.
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
;       2014/09/27  -   Written by Matthew Argall
;-
function MrMMS_EDI_Plot_ALT, sc, mode, optdesc, $
COORDS=coords, $
FAC=fac, $
LEVEL=level, $
NO_LOAD=no_load, $
TRANGE=trange
	compile_opt idl2

	catch, the_error
	if the_error ne 0 then begin
		catch, /CANCEL
		if n_elements(win) gt 0 then obj_destroy, win
		MrPrintF, 'LogErr'
		return, obj_new()
	endif
	
	;Defaults
	tf_load = ~keyword_set(no_load)
	if n_elements(trange) gt 0 then MrVar_SetTRange, trange
	if n_elements(coords) eq 0 then coords = 'dbcs'
	if n_elements(level)  eq 0 then level  = 'l2'
	if n_elements(fac)    eq 0 then fac    = ''
	
	;Variable type
	type = level eq 'l2' ? 'flux' : 'counts'

;-------------------------------------------
; EDI Data /////////////////////////////////
;-------------------------------------------
	;Get EDI data
	if tf_load then begin
		;Load EDI data
		MrMMS_Load_Data, sc, 'edi', mode, level, $
		                 OPTDESC   = optdesc, $
		                 VARFORMAT = ['*' + type + '*', '*traj?_' + coords + '*']
	endif
	
;-------------------------------------------
; Variable Names ///////////////////////////
;-------------------------------------------
	;Parts
	prefix = sc + '_edi_'
	suffix = '_' + mode + '_' + level
	ch     = mode eq 'brst' ? ['1', '2', '3', '4'] : '1'
	
	;Names
	flux_vname = [ prefix + type   + ch +                '_0'       + suffix, $
	               prefix + type   + ch +                '_90_gdu1' + suffix, $
	               prefix + type   + ch +                '_90_gdu2' + suffix, $
	               prefix + type   + ch +                '_180'     + suffix ]
	traj_vname = [ prefix + 'traj' + ch + '_' + coords + '_0'       + suffix, $
	               prefix + 'traj' + ch + '_' + coords + '_90_gdu1' + suffix, $
	               prefix + 'traj' + ch + '_' + coords + '_90_gdu2' + suffix, $
	               prefix + 'traj' + ch + '_' + coords + '_180'     + suffix ]
	
	;Output names
	traj_fac_vname = [ prefix + 'traj' + ch + '_fac_0'       + suffix, $
	                   prefix + 'traj' + ch + '_fac_90_gdu1' + suffix, $
	                   prefix + 'traj' + ch + '_fac_90_gdu2' + suffix, $
	                   prefix + 'traj' + ch + '_fac_180'     + suffix ]

;-------------------------------------------
; Transform Trajectories to FAC ////////////
;-------------------------------------------
	;Create the transformation matrix
	oT_fa = MrMMS_FGM_Get_TFAC( sc, mode, fac, traj_vname[0], $
	                            COORDS  = coords, $
	                            LEVEL   = level, $
	                            NO_LOAD = no_load, $
	                            TRANGE  = trange )
	
	;Create the transformation matrix
	oT_perp = MrMMS_FGM_Get_TFAC( sc, mode, fac, traj_vname[1], $
	                              COORDS  = coords, $
	                              LEVEL   = level, $
	                              /NO_LOAD, $
	                              TRANGE  = trange )
	
	;Step through each trajectory variable
	for i = 0, n_elements(traj_vname) - 1 do begin
		;Get the trajectories
		oTemp = MrVar_Get(traj_vname[i])
		oTraj = oTemp * !dtor
		oTraj -> AddAttr, 'DEPEND_0', oTemp['DEPEND_0']

		;Convert to a vector
		x    = sin(oTraj[1,*]) * cos(oTraj[0,*])
		y    = sin(oTraj[1,*]) * sin(oTraj[0,*])
		z    = cos(oTraj[1,*])
		oVec = MrVectorTS( [temporary(x), temporary(y), temporary(z)] )
		oVec -> AddAttr, 'DEPEND_0', oTraj['DEPEND_0']

		;Rotate to field-aligned coordinates
		if stregex(traj_vname[i], '_90_gdu(1|2)_', /BOOLEAN) $
			then oVec_fac = oT_perp -> Rotate_Vector(oVec) $
			else oVec_fac = oT_fa   -> Rotate_Vector(oVec)
		obj_destroy, oVec
		
		;Convert back to spherical coordinates
		phi        = atan( oVec_fac[*,1], oVec_fac[*,0] ) * !radeg
		theta      = acos( oVec_fac[*,2] )                * !radeg
		oTraj_sphr = MrVariable( [[temporary(phi)], [temporary(theta)]] )
		
		;Set properties
		oTraj_sphr -> SetName, traj_fac_vname[i]
		oTraj_sphr -> AddAttr, 'AXIS_RANGE',   [-200, 200]
		oTraj_sphr -> AddAttr, 'CATDESC',      'Electron incident trajectories in field-aligned coordinates.'
		oTraj_sphr -> AddAttr, 'COLOR',        ['Red', 'Blue']
		oTraj_sphr -> AddAttr, 'DEPEND_0',     oTraj['DEPEND_0']
		oTraj_sphr -> AddAttr, 'DIMENSION',    1
		oTraj_sphr -> AddAttr, 'LABEL',        ['Phi', 'Theta']
		oTraj_sphr -> AddAttr, 'TICKINTERVAL', 90.0
		oTraj_sphr -> AddAttr, 'TITLE',        'Traj!C(deg)'
		oTraj_sphr -> AddAttr, 'UNITS',        'degrees'
		oTraj_sphr -> Cache
	endfor

;-------------------------------------------
; Set Properties ///////////////////////////
;-------------------------------------------
	if tf_load then begin
		foreach vname, traj_vname, idx do begin
			oVar = MrVar_Get(vname)
			oVar -> AddAttr,      'AXIS_RANGE',   [-200,200]
			oVar -> AddAttr,      'DIMENSION',    2
			oVar -> AddAttr,      'COLOR',        ['Red', 'Blue']
			oVar -> AddAttr,      'TICKINTERVAL', 90.0
			oVar -> SetAttrValue, 'TITLE',        '-Look!C(DBCS)'
			oVar -> RemoveAttr,   ['MIN_VALUE', 'MAX_VALUE']
		endforeach
	
;		foreach vname, flux_vname, idx do begin
;			oVar = MrVar_Get(vname)
;			oVar -> AddAttr, 'AXIS_RANGE', [1e2, oVar.max*1.3]
;		endforeach
	endif
	
	;Magnetic Field
	;   - L2Pre and L2 are different from lower quality names
	MrVar_Names, b_vname, '(afg|dfg|fgm).*vec', /REGEX
	

;-------------------------------------------
; FGM Data /////////////////////////////////
;-------------------------------------------
	;Plot the data
	win = MrVar_PlotTS( [b_vname, traj_fac_vname], XSIZE=600, YSIZE=800 )
	win -> Refresh, /DISABLE
	win -> SetProperty, OXMARGIN=[10,10]
	
	lgd = MrLegend( ALIGNMENT    = 'NW', $
	                LABEL        = ['Gyro', 'Pitch'], $
	                LINESTYLE    = 6, $
	                POSITION     = [1.0, 1.0], $
	                /RELATIVE, $
	                SAMPLE_WIDTH = 0.0, $
	                TARGET       = win[traj_fac_vname[0]], $
	                TEXT_COLOR   = ['Red', 'Blue'] )
	
	win -> Refresh
	return, win
end