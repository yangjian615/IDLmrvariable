; docformat = 'rst'
;
; NAME:
;       MrMMS_EDI_Plot_AMB
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
;   Load EDI field-aligned-mode data (as opposed to alternating and perpendicular).
;   Rotate trajectories into a field-aligned coordinate system and plot.
;       A) B-field
;       B) 0-Degree Fluxes
;       C) 180-Degree Fluxes
;       D) 0-Degree Trajectories
;       E) 180-Degree Trajectories
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
;                           Optional descriptor of the data. Options are: {'amb', 'amb-pm2'}
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
function MrMMS_EDI_Plot_AMB, sc, mode, optdesc, $
COORDS=coords, $
FAC=fac, $
LEVEL=level, $
NO_LOAD=no_load, $
TRANGE=trange
	compile_opt idl2

	Catch, the_error
	IF the_error NE 0 THEN BEGIN
		Catch, /CANCEL
		IF N_Elements(win) GT 0 THEN Obj_Destroy, win
		MrPrintF, 'LogErr'
		RETURN, Obj_New()
	ENDIF
	
	;Defaults
	tf_load = ~Keyword_Set(no_load)
	IF N_Elements(trange) GT 0 THEN MrVar_SetTRange, trange
	IF N_Elements(coords) EQ 0 THEN coords = 'dbcs'
	IF N_Elements(level)  EQ 0 THEN level  = 'l2'
	IF N_Elements(fac)    EQ 0 THEN fac    = ''
	
	;Variable type
	type = level EQ 'l2' ? 'flux' : 'counts'

;-------------------------------------------
; EDI Data /////////////////////////////////
;-------------------------------------------
	IF N_Elements(fgm_instr) EQ 0 THEN fgm_instr = 'fgm'
	fgm_coords = coords EQ 'dbcs' ? 'dmpa' : coords

	;Get EDI data
	IF tf_load THEN BEGIN
		;Load EDI data
		MrMMS_Load_Data, sc, 'edi', mode, level, $
		                 OPTDESC   = optdesc, $
		                 VARFORMAT = '*traj?_' + coords + '*'
;		                 VARFORMAT = ['*' + type + '*', '*traj?_' + coords + '*']
		
		;FGM
		MrMMS_FGM_Load_Data, sc, mode, $
		                     INSTR     = fgm_instr, $
		                     VARFORMAT = '*_b_'+fgm_coords+'_*'
	ENDIF
	
;-------------------------------------------
; Variable Names ///////////////////////////
;-------------------------------------------
	;Parts
	prefix = sc + '_edi_'
	suffix = '_' + mode + '_' + level
	ch     = mode EQ 'brst' ? ['1', '2', '3', '4'] : '1'
	
	;Names
	b_vname    = StrJoin( [sc, fgm_instr, 'b',    fgm_coords, mode, level], '_' )
	bmag_vname = StrJoin( [sc, fgm_instr, 'bmag', fgm_coords, mode, level], '_' )
	bvec_vname = StrJoin( [sc, fgm_instr, 'bvec', fgm_coords, mode, level], '_' )
	flux_vname = [ prefix + type   + ch +                '_0'   + suffix, $
	               prefix + type   + ch +                '_180' + suffix ]
	traj_vname = [ prefix + 'traj' + ch + '_' + coords + '_0'   + suffix, $
	               prefix + 'traj' + ch + '_' + coords + '_180' + suffix ]
	
	;Output names
	traj_fac_vname = [ prefix + 'traj' + ch + '_fac_0'   + suffix, $
	                   prefix + 'traj' + ch + '_fac_180' + suffix ]

;-------------------------------------------
; Transform Trajectories to FAC ////////////
;-------------------------------------------
	;Create the transformation matrix
	oT = MrVar_Fac(bvec_vname, TIME=traj_vname[0])
	
	;Step through each trajectory variable
	FOR i = 0, N_Elements(traj_vname) - 1 DO BEGIN
		;Get the trajectories
		oTemp = MrVar_Get(traj_vname[i])
		oTraj = oTemp * !dtor

		;Convert to a vector
		x    = Sin(oTraj['DATA',*,1]) * Cos(oTraj['DATA',*,0])
		y    = Sin(oTraj['DATA',*,1]) * Sin(oTraj['DATA',*,0])
		z    = Cos(oTraj['DATA',*,1])
		oVec = MrVectorTS( oTraj['TIMEVAR'], [ [Temporary(x)], [Temporary(y)], [Temporary(z)] ] )
		
		;Rotate to field-aligned coordinates
		oVec_fac = oT -> Rotate_Vector(oVec) ;oT # oVec
		Obj_Destroy, oVec
		
		;Convert back to spherical coordinates
		phi        = ATan( oVec_fac['DATA',*,1], oVec_fac['DATA',*,0] ) * !radeg
		theta      = ACos( oVec_fac['DATA',*,2] )                       * !radeg
		oTraj_sphr = MrTimeSeries( oTraj['TIMEVAR'], [[Temporary(phi)], [Temporary(theta)]] )
		
		;Set properties
		oTraj_sphr -> SetName, traj_fac_vname[i]
		oTraj_sphr ['AXIS_RANGE']   = [-200, 200]
		oTraj_sphr ['CATDESC']      = 'Electron incident trajectories in field-aligned coordinates.'
		oTraj_sphr ['COLOR']        = ['Red', 'Blue']
		oTraj_sphr ['DEPEND_0']     = oTraj['DEPEND_0']
		oTraj_sphr ['DIMENSION']    = 1
		oTraj_sphr ['LABEL']        = ['Phi', 'Theta']
		oTraj_sphr ['TICKINTERVAL'] = 90.0
		oTraj_sphr ['TITLE']        = 'Traj!C(deg)'
		oTraj_sphr ['UNITS']        = 'degrees'
		oTraj_sphr -> Cache
	ENDFOR

;-------------------------------------------
; Set Properties ///////////////////////////
;-------------------------------------------
	oVar = MrVar_Get(vname)
	oVar['AXIS_RANGE']   = [-200,200]
	oVar['DIMENSION']    = 2
	oVar['COLOR']        = ['Red', 'Blue']
	oVar['TICKINTERVAL'] = 90.0
	oVar['TITLE']        = '-Look!C(DBCS)'
	oVar -> RemoveAttr, ['MIN_VALUE', 'MAX_VALUE']
	
	FOREACH vname, flux_vname, idx DO BEGIN
		oVar = MrVar_Get(vname)
		oVar['AXIS_RANGE'] = [1e2, oVar.max*1.3]
	ENDFOREACH
	
	;Magnetic Field
	b_vname = StrJoin([sc, 'fgm', 'bvec', 'dmpa', mode, level], '_')
	

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
	RETURN, win
END