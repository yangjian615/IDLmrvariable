; docformat = 'rst'
;
; NAME:
;       MrMMS_EDI_Load_EField_Cts
;
;*****************************************************************************************
;   Copyright (c) 2017, Matthew Argall                                                   ;
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
;   Load EDI Q0 or Data29 data from source l1a files. Counts are calibrated and
;   trajectories are rotated into geostationary coordinate systems. This program
;   requires access to the MMS team side of the SDC.
;
; :Categories:
;   MrVariable, MMS, EDI
;
; :Params:
;       SC:         in, required, type=string
;                   Spacecraft ID. Options are {'mms1' | 'mms2' | 'mms3' | 'mms4' }
;       MODE:       in, required, type=string, default='srvy'
;                   Data telemetry rate of the data. Options are: { 'slow' | 'fast' | 'srvy' | 'brst' }
;
; :Keywords:
;       ABSCAL:     in, optional, type=boolean, default=1
;                   When set, absolute calibration factors are applied to convert counts
;                       to flux. This is the default.
;       SUFFIX:     in, optional, type=string, default=''
;                   A suffix to be appendend to the end of EDI variable names.
;       TRANGE:     in, optional, type=string/strarr(2), default=MrVar_GetTRange()
;                   The start and end times of the data interval to be plotted, formatted
;                       as 'YYYY-MM-DDThh:mm:ss'
;       VARNAMES:   in, optional, type=strarr
;                   Names of the variables that have been loaded into the cache
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
;       2017/04/25  -   Written by Matthew Argall
;-
PRO MrMMS_EDI_Load_EField_Cts, sc, mode, dataset, $
ABSCAL=abscal, $
SUFFIX=suffix, $
TRANGE=trange, $
VARNAMES=varnames
	Compile_Opt idl2
	
	Catch, the_error
	IF the_error NE 0 THEN BEGIN
		Catch, /CANCEL
		MrPrintF, 'LogErr'
		RETURN
	ENDIF
	
	;Defaults
	tf_abscal = N_Elements(abscal) EQ 0 ? 1B : Keyword_Set(abscal)
	IF N_Elements(dataset) EQ 0 THEN dataset = 'q0'
	IF N_Elements(mode)    EQ 0 THEN mode    = 'fast'
	IF N_Elements(suffix)  EQ 0 THEN suffix  = ''
	IF N_Elements(trange)  GT 0 THEN MrVar_SetTRange, trange
	
	IF Array_Equal(dataset EQ ['q0', 'data29'],         0) THEN Message, 'DATASET must be {"q0" | "data29"}'
	IF Array_Equal(mode    EQ ['slow', 'fast', 'brst'], 0) THEN Message, 'MODE must be {"slow" | "fast" | "brst"}'
	
	;Constants
	nPhi   = 32
	nTheta = 512
	dPhi   = 360.0 / nPhi
	dTheta = 360.0 / nTheta
	
;-------------------------------------------
; Get Data /////////////////////////////////
;-------------------------------------------
	
	;Constants
	instr    = 'edi'
	level    = 'l1a'
	optdesc  = 'efield'
	inparam  = dataset EQ 'q0' ? 'word15' : 'data29'
	outparam = dataset EQ 'q0' ? 'q0'     : 'd29'
	
	;Variable names
	q_gd12_vname     = StrJoin( [sc, instr, 'sq',    'gd12'], '_')
	q_gd21_vname     = StrJoin( [sc, instr, 'sq',    'gd21'], '_')
	phi_gd12_vname   = StrJoin( [sc, instr, 'phi',   'gd12'], '_')
	phi_gd21_vname   = StrJoin( [sc, instr, 'phi',   'gd21'], '_')
	theta_gd12_vname = StrJoin( [sc, instr, 'theta', 'gd12'], '_')
	theta_gd21_vname = StrJoin( [sc, instr, 'theta', 'gd21'], '_')
	cts_gd12_vname   = StrJoin( [sc, instr, inparam, 'gd12'], '_')
	cts_gd21_vname   = StrJoin( [sc, instr, inparam, 'gd21'], '_')
	
	;Derived names
	epoch_dang_vname     = 'epoch_angdiff'
	traj_labl_ptr_vname  = StrJoin( [sc, instr, 'traj', 'labl', 'ptr', '1', outparam, mode, level], '_' ) + suffix
	q_gdu1_vname         = StrJoin( [sc, instr, 'q',              'gdu1',           mode, level], '_' ) + suffix
	q_gdu2_vname         = StrJoin( [sc, instr, 'q',              'gdu2',           mode, level], '_' ) + suffix
	flux_gdu1_vname      = StrJoin( [sc, instr, 'flux',           'gdu1', outparam, mode, level], '_' ) + suffix
	flux_gdu2_vname      = StrJoin( [sc, instr, 'flux',           'gdu2', outparam, mode, level], '_' ) + suffix
	traj_gdu1_vname      = StrJoin( [sc, instr, 'traj',           'gdu1', outparam, mode, level], '_' ) + suffix
	traj_bcs_gdu1_vname  = StrJoin( [sc, instr, 'traj',   'bcs',  'gdu1', outparam, mode, level], '_' ) + suffix
	traj_dbcs_gdu1_vname = StrJoin( [sc, instr, 'traj',   'dbcs', 'gdu1', outparam, mode, level], '_' ) + suffix
	traj_gse_gdu1_vname  = StrJoin( [sc, instr, 'traj',   'gse',  'gdu1', outparam, mode, level], '_' ) + suffix
	traj_gsm_gdu1_vname  = StrJoin( [sc, instr, 'traj',   'gsm',  'gdu1', outparam, mode, level], '_' ) + suffix
	traj_gdu2_vname      = StrJoin( [sc, instr, 'traj',           'gdu2', outparam, mode, level], '_' ) + suffix
	traj_bcs_gdu2_vname  = StrJoin( [sc, instr, 'traj',   'bcs',  'gdu2', outparam, mode, level], '_' ) + suffix
	traj_dbcs_gdu2_vname = StrJoin( [sc, instr, 'traj',   'dbcs', 'gdu2', outparam, mode, level], '_' ) + suffix
	traj_gse_gdu2_vname  = StrJoin( [sc, instr, 'traj',   'gse',  'gdu2', outparam, mode, level], '_' ) + suffix
	traj_gsm_gdu2_vname  = StrJoin( [sc, instr, 'traj',   'gsm',  'gdu2', outparam, mode, level], '_' ) + suffix
	ang_diff_vname       = StrJoin( [sc, instr, 'dlook',                  outparam, mode, level], '_' ) + suffix
	
	;EDI
	MrMMS_Load_Data, sc, instr, mode, level, $
	                 OPTDESC   = optdesc, $
	                 VARFORMAT = ['*sq*', '*phi_gd*', '*theta_gd*', '*'+inparam+'_gd*']
	
;-------------------------------------------
; Q0 Data //////////////////////////////////
;-------------------------------------------
	IF dataset EQ 'q0' THEN BEGIN
	;-------------------------------------------
	; Keep Only Quality 0 Data /////////////////
	;-------------------------------------------
		oQ_gd12     = MrVar_Get(q_gd12_vname)
		oQ_gd21     = MrVar_Get(q_gd21_vname)
		oPhi_gd12   = MrVar_Get(phi_gd12_vname)
		oPhi_gd21   = MrVar_Get(phi_gd21_vname)
		oTheta_gd12 = MrVar_Get(theta_gd12_vname)
		oTheta_gd21 = MrVar_Get(theta_gd21_vname)
		oCts_gd12   = MrVar_Get(cts_gd12_vname)
		oCts_gd21   = MrVar_Get(cts_gd21_vname)
	
		;Find quality 0
		iq0_gd12 = oQ_gd12 -> Where(0, /EQUAL, COUNT=nQ0_gd12)
		iq0_gd21 = oQ_gd21 -> Where(0, /EQUAL, COUNT=nQ0_gd21)
	
		;Extract quality 0
		oTraj_gdu1 = MrTimeSeries(oPhi_gd21['TIME', iq0_gd21], [ [oPhi_gd21['DATA', iq0_gd21]], [oTheta_gd21['DATA', iq0_gd21]] ], /CACHE, NAME=traj_gdu1_vname )
		oTraj_gdu2 = MrTimeSeries(oPhi_gd12['TIME', iq0_gd12], [ [oPhi_gd12['DATA', iq0_gd12]], [oTheta_gd12['DATA', iq0_gd12]] ], /CACHE, NAME=traj_gdu2_vname )
		oCts_gdu1  = oCts_gd21[iq0_gd21]
		oCts_gdu2  = oCts_gd12[iq0_gd12]
		
		;Remove quality variables
		MrVar_Delete, [q_gd12_vname, q_gd21_vname]
		iq0_gd12 = !Null
		iq0_gd21 = !Null
	
;-------------------------------------------
; Data29 ///////////////////////////////////
;-------------------------------------------
	ENDIF ELSE BEGIN
	
		;Phi
		oPhi_GD12 = MrVar_Get( phi_gd12_vname )
		oPhi_GD21 = MrVar_Get( phi_gd21_vname )
	
		;Adjust angle
		;   - (-180,180] -> [0,360)
		phi_gdu1 = ( oPhi_GD21['DATA'] + (oPhi_GD21['DATA'] LT 0) * 360.0 ) MOD 360.0
		phi_gdu2 = ( oPhi_GD12['DATA'] + (oPhi_GD12['DATA'] LT 0) * 360.0 ) MOD 360.0
		
		;Shift by half an anode
;		phi_gdu1 = (phi_gdu1 - width/2.0 + 360.0) MOD 360.0
;		phi_gdu2 = (phi_gdu2 + width/2.0)         MOD 360.0
		
		;Create variable
		oPhi_GDU1 = MrTimeSeries(oPhi_GD21['TIMEVAR'], phi_gdu1, /NO_COPY)
		oPhi_GDU2 = MrTimeSeries(oPhi_GD12['TIMEVAR'], phi_gdu2, /NO_COPY)
	
	;-------------------------------------------
	; Interpolate Angles to D29 ////////////////
	;-------------------------------------------
		;Look-directions in instrument coordinates
		oTheta_GDU1 = MrVar_Get( theta_gd21_vname )
		oTheta_GDU2 = MrVar_Get( theta_gd12_vname )
		oCts_GDU1   = MrVar_Get( cts_gd21_vname   )
		oCts_GDU2   = MrVar_Get( cts_gd12_vname   )

		;Interpolate angles to DATA29 cadence
		;   - Data29 GD12 & GD21 have the same time tags
		;   - Angles for GD12 & GD21 do not have the same time tags
		oT_GD12 = oTheta_GDU2['TIMEVAR']
		oT_GD21 = oTheta_GDU1['TIMEVAR']
		iGDU2   = oT_GD12 -> Value_Locate( oCts_GDU2['TIME', 'TT2000'], 'TT2000' )
		iGDU1   = oT_GD21 -> Value_Locate( oCts_GDU1['TIME', 'TT2000'], 'TT2000' )
		
		;Convert to cartesian coordinates
;		phi        = ophi_gdu1['DATA'] * !dtor
;		theta      = oTheta_GDU1['DATA'] * !dtor
;		x1         = Sin(theta) * Cos(phi)
;		y1         = Sin(theta) * Sin(phi)
;		z1         = Cos(theta)
;		oX1        = MrVectorTS(oT_GD21, [ [x1], [y1], [z1] ])
;		oX1        = oX1 -> Interpol( oCts_GDU1 )
;		oX1        = oX1 -> Normalize()
;		phi1       = ATan(oX1['DATA',*,1], oX1['DATA',*,0]) * !radeg
;		theta1     = ACos(oX1['DATA',*,2] ) * !radeg
;		oTraj_GDU1 = MrTimeSeries( oCts_GDU1['TIMEVAR'], [ [phi1], [theta1] ], /CACHE, NAME=traj_gdu1_vname )
		
;		phi        = ophi_gdu2['DATA'] * !dtor
;		theta      = oTheta_GDU2['DATA'] * !dtor
;		x2         = Sin(theta) * Cos(phi)
;		y2         = Sin(theta) * Sin(phi)
;		z2         = Cos(theta)
;		oX2        = MrVectorTS(oT_GD12, [ [x2], [y2], [z2] ])
;		oX2        = oX2 -> Interpol( oCts_GDU2 )
;		oX2        = oX2 -> Normalize()
;		phi2       = ATan(oX2['DATA',*,1], oX2['DATA',*,0]) * !radeg
;		theta2     = ACos(oX2['DATA',*,2] ) * !radeg
;		oTraj_GDU2 = MrTimeSeries( oCts_GDU2['TIMEVAR'], [ [phi2], [theta2] ], /CACHE, NAME=traj_gdu2_vname )
	
		;Create variables
		oT_GDU1    = oCts_GDU1['TIMEVAR']
		oT_GDU2    = oCts_GDU2['TIMEVAR']
		oTraj_GDU1 = MrTimeSeries(oT_GDU1, [ [oPhi_GDU1['DATA',iGDU1]], [oTheta_GDU1['DATA', Temporary(iGDU1)]] ], /CACHE, NAME=traj_gdu1_vname )
		oTraj_GDU2 = MrTimeSeries(oT_GDU2, [ [oPhi_GDU2['DATA',iGDU2]], [oTheta_GDU2['DATA', Temporary(iGDU2)]] ], /CACHE, NAME=traj_gdu2_vname )
	
		;GDU1: Do not extrapolate!
		iLT = oT_GDU1 -> Where( oT_GD21['DATA',  0, 'TT2000'], 'TT2000', /LESS,    COUNT=nLT )
		iGT = oT_GDU1 -> Where( oT_GD21['DATA', -1, 'TT2000'], 'TT2000', /GREATER, COUNT=nGT )
		IF nLT GT 0 THEN oTraj_GDU1[iLT,*] = !Values.F_NaN
		IF nGT GT 0 THEN oTraj_GDU1[iGT,*] = !Values.F_NaN
	
		;GDU2: Do not extrapolate!
		iLT = oT_GDU2 -> Where( oT_GD12['DATA',  0, 'TT2000'], 'TT2000', /LESS,    COUNT=nLT )
		iGT = oT_GDU2 -> Where( oT_GD12['DATA', -1, 'TT2000'], 'TT2000', /GREATER, COUNT=nGT )
		IF nLT GT 0 THEN oTraj_GDU2[iLT,*] = !Values.F_NaN
		IF nGT GT 0 THEN oTraj_GDU2[iGT,*] = !Values.F_NaN
	
	;-------------------------------------------
	; Quality Flags ////////////////////////////
	;-------------------------------------------
		;Q GDU1
		oQ_GDU1 = MrVar_Get(q_gd21_vname)
		oQ_GDU1 -> SetName, q_gdu1_vname
	
		;Q GDU2
		oQ_GDU2 = MrVar_Get(q_gd12_vname)
		oQ_GDU2 -> SetName, q_gdu2_vname
	ENDELSE
	
;-------------------------------------------
; Calibrate Data ///////////////////////////
;-------------------------------------------
	;GDU1
	oFlux_GDU1 = MrMMS_EDI_Cal_Cts( sc, mode, dataset, 'gdu1', oCts_GDU1, oTraj_GDU1, $
	                                ABSCAL    = tf_abscal, $
	                                /CACHE, $
	                                NAME      = flux_gdu1_vname, $
	                                ONE_SIDED = (dataset EQ 'data29') )
	
	;GDU2
	oFlux_GDU2 = MrMMS_EDI_Cal_Cts( sc, mode, dataset, 'gdu2', oCts_GDU2, oTraj_GDU2, $
	                                ABSCAL    = tf_abscal, $
	                                /CACHE, $
	                                NAME      = flux_gdu2_vname, $
	                                ONE_SIDED = (dataset EQ 'data29') )

;-------------------------------------------
; Adjust Angles ////////////////////////////
;-------------------------------------------
	;
	; In E-Field mode, two anodes are active and
	; contribute to Q0 counts. The Phi value given
	; in the L1A files points to the center of the
	; two anodes. Data29 is taken from the anode
	; closer to lower pitch angles, so its phi value
	; has to be shifted by half an anode width, or
	; 5.625 degrees.
	;
	
	;
	; Electron incident vectors are opposite the detector look direction.
	; The firing direction of the guns (given by phi and theta) are also
	; opposte the detector look direction. Thus, incident vectors are
	; are equivalent to gun firing directions. (Otherwise, (180 - phi)
	; would transform into the detector frame, then the look directions
	; would have to be negated.)
	;
	
	IF dataset EQ 'data29' THEN BEGIN
		;Shift by half an anode
		phi_gdu1 = oTraj_GDU1['DATA',*,0] - dPhi/2.0
		phi_gdu2 = oTraj_GDU2['DATA',*,0] + dPhi/2.0
		
		;Put back into range [0,360)
		oTraj_GDU1[*,0] = ( phi_gdu1 + (phi_gdu1 LT 0.0)*360.0 ) MOD 360.0
		oTraj_GDU2[*,0] = ( phi_gdu2 + (phi_gdu2 LT 0.0)*360.0 ) MOD 360.0
	ENDIF
	
;-------------------------------------------
; Incident Vectors /////////////////////////
;-------------------------------------------

	oTraj_GDU1 *= !dtor
	oTraj_GDU2 *= !dtor

	;Convert to cartesian coords
	x1 = Sin( oTraj_GDU1['DATA',*,1] ) * Cos( oTraj_GDU1['DATA',*,0] )
	y1 = Sin( oTraj_GDU1['DATA',*,1] ) * Sin( oTraj_GDU1['DATA',*,0] )
	z1 = Cos( oTraj_GDU1['DATA',*,1] )
	
	;Angles are in GDU coordinates, so one vector must be negated
	;   - X-Axes are aligned. Others are inverted
	x2 = Sin( oTraj_GDU2['DATA',*,1] ) * Cos( oTraj_GDU2['DATA',*,0] )
	y2 = Sin( oTraj_GDU2['DATA',*,1] ) * Sin( oTraj_GDU2['DATA',*,0] )
	z2 = Cos( oTraj_GDU2['DATA',*,1] )
	
	;Create vectors
	oX_GDU1 = MrVectorTS( oTraj_GDU1['TIMEVAR'], [ [ x1 ], [ y1 ], [ z1 ] ] )
	oX_GDU2 = MrVectorTS( oTraj_GDU2['TIMEVAR'], [ [ x2 ], [ y2 ], [ z2 ] ] )
	
	;Rotation from EDI1 to BCS
	alpha1 = -90 * !dpi/180.0
	alpha2 = 221 * !dpi/180.0
	r1 = [ [  cos(alpha1),       0,      -sin(alpha1) ], $
	       [        0,           1,             0     ], $
	       [  sin(alpha1),       0,       cos(alpha1) ] ]
	r2 = [ [  cos(alpha2),  sin(alpha2),        0 ], $
	       [ -sin(alpha2),  cos(alpha2),        0 ], $
	       [        0,           0,             1 ] ]
	edi1_to_bcs = Temporary(r2) ## Temporary(r1)
	
	;Rotation from EDI2 to BCS
	alpha1 = -90 * !dpi/180.0
	alpha2 =  41 * !dpi/180.0
	r1 = [ [  cos(alpha1),       0,      -sin(alpha1) ], $
	       [        0,           1,             0     ], $
	       [  sin(alpha1),       0,       cos(alpha1) ] ]
	r2 = [ [  cos(alpha2),  sin(alpha2),        0 ], $
	       [ -sin(alpha2),  cos(alpha2),        0 ], $
	       [        0,           0,             1 ] ]
	edi2_to_bcs = Temporary(r2) ## Temporary(r1)

	;Rotate to BCS
	;   - Detector 1 (here called GDU1) is on EDI #2
	;   - Detector 2 (here called GDU2) is on EDI #1
	nPts1 = oX_GDU1 -> GetNPts()
	nPts2 = oX_GDU2 -> GetNPts()
	x_bcs_gdu1 = FltArr( 3, nPts1 )
	x_bcs_gdu2 = FltArr( 3, nPts2 )
	FOREACH vec, oX_GDU1, i DO x_bcs_gdu1[0,i] = Reform(edi2_to_bcs ## vec)
	FOREACH vec, oX_GDU2, i DO x_bcs_gdu2[0,i] = Reform(edi1_to_bcs ## vec)
	
	;Create vectors
	oX_BCS_GDU1 = MrVectorTS(oTraj_GDU1['TIMEVAR'], x_bcs_gdu1, /NO_COPY)
	oX_BCS_GDU2 = MrVectorTS(oTraj_GDU2['TIMEVAR'], x_bcs_gdu2, /NO_COPY)

	;Delete variables
	r      = !Null
	alpha1 = !Null
	alpha2 = !Null
	Obj_Destroy, [oX_GDU1, oX_GDU2]
	
;-------------------------------------------
; Angular Difference ///////////////////////
;-------------------------------------------
	IF dataset EQ 'q0' THEN BEGIN
		;
		; GDUs 1 & 2 do not necessarily have the same time tags.
		; To find the angular difference, take all unique times
		; between the two GDUs and interpolate each set of
		; trajectories onto those times.
		;
		
		
		;Time stamps
		t1 = oTraj_GDU1['TIME', 'TT2000']
		t2 = oTraj_GDU2['TIME', 'TT2000']
	
		;Find all unique time stamps
		t = [ t1, t2 ]
		t = t[Uniq(t, Sort(t))]
		
		;Interpolate onto unique time stamps
		x1 = Interpol(x1, t1, t)
		y1 = Interpol(y1, t1, t)
		z1 = Interpol(z1, t1, t)
		x2 = Interpol(x2, t2, t)
		y2 = Interpol(y2, t2, t)
		z2 = Interpol(z2, t2, t)
		
		;Clear memory
		t1 = !Null
		t2 = !Null
	ENDIF ELSE BEGIN
		t = oTraj_GDU1['TIME', 'TT2000']
	ENDELSE

	;Angular difference between GDU1 & GDU2
	;   - XYZ1 and XYZ2 are in EDI1 and EDI2 coordinates
	;   - They share an x-axis, but y and z are inverted with respect to one another
	;   - Bring XYZ2 into XYZ1 coordinates before taking the dot product
	angdiff = ACos( x1*x2 + y1*(-y2) + z1*(-z2) ) * !radeg
	oT_dAng = MrTimeVar(t, 'TT2000', NAME=epoch_dang_vname)
	oDAng   = MrScalarTS( oT_dAng, angdiff, $
	                      /CACHE, $
	                      NAME   = ang_diff_vname, $
	                      T_TYPE = 'TT2000' )
	
	;Clear memory
	x1 = !Null
	y1 = !Null
	z1 = !Null
	x2 = !Null
	y2 = !Null
	z2 = !Null

;-------------------------------------------
; Rotate to GSE ////////////////////////////
;-------------------------------------------

	;GDU1
	MrMMS_Bromund_Rotate, oX_BCS_GDU1, sc, $
	                      DEFATT     = defatt, $
	                      DEFEPH     = defeph, $
	                      /DESPIN, $
	                      DESPUN     = oX_DBCS_GDU1, $
	                      GSE        = oX_GSE_GDU1, $
	                      GSM        = oX_GSM_GDU1, $
	                      TYPE       = 'Z'

	;GDU2
	MrMMS_Bromund_Rotate, oX_BCS_GDU2, $
	                      DEFATT     = defatt, $
	                      DEFEPH     = defeph, $
	                      /DESPIN, $
	                      DESPUN     = oX_DBCS_GDU2, $
	                      GSE        = oX_GSE_GDU2, $
	                      GSM        = oX_GSM_GDU2, $
	                      TYPE       = 'Z'
	
	;Free memory
	defatt = !Null
	defeph = !Null

;-------------------------------------------
; Split into Theta & Phi ///////////////////
;-------------------------------------------
	phi_bcs_gdu1   = !radeg * ATan( oX_BCS_GDU1['DATA', *, 1],  oX_BCS_GDU1['DATA', *, 0] )
	phi_dbcs_gdu1  = !radeg * ATan( oX_DBCS_GDU1['DATA', *, 1], oX_DBCS_GDU1['DATA', *, 0] )
	phi_gse_gdu1   = !radeg * ATan( oX_GSE_GDU1['DATA', *, 1],  oX_GSE_GDU1['DATA', *, 0] )
	phi_gsm_gdu1   = !radeg * ATan( oX_GSM_GDU1['DATA', *, 1],  oX_GSM_GDU1['DATA', *, 0] )
	
	theta_bcs_gdu1  = !radeg * ACos( oX_BCS_GDU1['DATA', *, 2]  )
	theta_dbcs_gdu1 = !radeg * ACos( oX_DBCS_GDU1['DATA', *, 2] )
	theta_gse_gdu1  = !radeg * ACos( oX_GSE_GDU1['DATA', *, 2]  )
	theta_gsm_gdu1  = !radeg * ACos( oX_GSM_GDU1['DATA', *, 2]  )
	
	phi_bcs_gdu2  = !radeg * ATan( oX_BCS_GDU2['DATA', *, 1],  oX_BCS_GDU2['DATA', *, 0]  )
	phi_dbcs_gdu2 = !radeg * ATan( oX_DBCS_GDU2['DATA', *, 1], oX_DBCS_GDU2['DATA', *, 0] )
	phi_gse_gdu2  = !radeg * ATan( oX_GSE_GDU2['DATA', *, 1],  oX_GSE_GDU2['DATA', *, 0]  )
	phi_gsm_gdu2  = !radeg * ATan( oX_GSM_GDU2['DATA', *, 1],  oX_GSM_GDU2['DATA', *, 0]  )
	
	theta_bcs_gdu2  = !radeg * ACos( oX_BCS_GDU2['DATA', *, 2]  )
	theta_dbcs_gdu2 = !radeg * ACos( oX_DBCS_GDU2['DATA', *, 2] )
	theta_gse_gdu2  = !radeg * ACos( oX_GSE_GDU2['DATA', *, 2]  )
	theta_gsm_gdu2  = !radeg * ACos( oX_GSM_GDU2['DATA', *, 2]  )
	
	;Create trajectories
	traj_bcs_gdu1  = [ [ Temporary(phi_bcs_gdu1) ],  [ Temporary(theta_bcs_gdu1)  ] ]
	traj_dbcs_gdu1 = [ [ Temporary(phi_dbcs_gdu1) ], [ Temporary(theta_dbcs_gdu1) ] ]
	traj_gse_gdu1  = [ [ Temporary(phi_gse_gdu1) ],  [ Temporary(theta_gse_gdu1)  ] ]
	traj_gsm_gdu1  = [ [ Temporary(phi_gsm_gdu1) ],  [ Temporary(theta_gsm_gdu1)  ] ]
	traj_bcs_gdu2  = [ [ Temporary(phi_bcs_gdu2) ],  [ Temporary(theta_bcs_gdu2)  ] ]
	traj_dbcs_gdu2 = [ [ Temporary(phi_dbcs_gdu2) ], [ Temporary(theta_dbcs_gdu2) ] ]
	traj_gse_gdu2  = [ [ Temporary(phi_gse_gdu2) ],  [ Temporary(theta_gse_gdu2)  ] ]
	traj_gsm_gdu2  = [ [ Temporary(phi_gsm_gdu2) ],  [ Temporary(theta_gsm_gdu2)  ] ]
	
	;Create variables
	oTraj_BCS_GDU1  = MrTimeSeries( oX_DBCS_GDU1['TIMEVAR'], traj_bcs_gdu1,  NAME=traj_bcs_gdu1_vname,  /NO_COPY, /CACHE)
	oTraj_DBCS_GDU1 = MrTimeSeries( oX_DBCS_GDU1['TIMEVAR'], traj_dbcs_gdu1, NAME=traj_dbcs_gdu1_vname, /NO_COPY, /CACHE)
	oTraj_GSE_GDU1  = MrTimeSeries( oX_GSE_GDU1['TIMEVAR'],  traj_gse_gdu1,  NAME=traj_gse_gdu1_vname,  /NO_COPY, /CACHE)
	oTraj_GSM_GDU1  = MrTimeSeries( oX_GSM_GDU1['TIMEVAR'],  traj_gsm_gdu1,  NAME=traj_gsm_gdu1_vname,  /NO_COPY, /CACHE)
	oTraj_BCS_GDU2  = MrTimeSeries( oX_DBCS_GDU2['TIMEVAR'], traj_bcs_gdu2,  NAME=traj_bcs_gdu2_vname,  /NO_COPY, /CACHE)
	oTraj_DBCS_GDU2 = MrTimeSeries( oX_DBCS_GDU2['TIMEVAR'], traj_dbcs_gdu2, NAME=traj_dbcs_gdu2_vname, /NO_COPY, /CACHE)
	oTraj_GSE_GDU2  = MrTimeSeries( oX_GSE_GDU2['TIMEVAR'],  traj_gse_gdu2,  NAME=traj_gse_gdu2_vname,  /NO_COPY, /CACHE)
	oTraj_GSM_GDU2  = MrTimeSeries( oX_GSM_GDU2['TIMEVAR'],  traj_gsm_gdu2,  NAME=traj_gsm_gdu2_vname,  /NO_COPY, /CACHE)
	
	;Clean up
	Obj_Destroy, [ oX_BCS_GDU1, oX_DBCS_GDU1, oX_GSE_GDU1, oX_GSM_GDU1, $
	               oX_BCS_GDU2, oX_DBCS_GDU2, oX_GSE_GDU2, oX_GSM_GDU2 ]

;-------------------------------------------
; Attributes ///////////////////////////////
;-------------------------------------------
	;FLUX GDU1
	oFlux_GDU1['TITLE'] = 'Flux!CGDU1!C(cm$\up-2$s$\up-1$)'
	oFlux_GDU1['LOG']   = 1B
	
	;FLUX GDU2
	oFlux_GDU2['TITLE'] = 'Flux!CGDU2!C(cm$\up-2$s$\up-1$)'
	oFlux_GDU2['LOG']   = 1B
	
	;Angular Difference
	oDAng['AXIS_RANGE']   = [0, 180]
	oDAng['TICKINTERVAL'] = 90
	oDAng['TITLE']        = '$\Delta$$\theta$!C(deg)'
	
	traj_delta = mode eq 'brst' ? 9.0 : 13.0
	traj_notes = 'Trajectories are given as unit vectors in spherical coordinates, with phi ' + $
	             '(theta) representing the azimuthal (polar) directions, in the ' + $
	             'indicated coordinate system. They are opposite to the nominal look-direction ' + $
	             "of the instrument. Errors represent an omni-directional error. For more " + $
	             'details about errors, contact the EDI instrument team.'
	             
	;TRAJ LABL PTR
	oTraj_Labl_Ptr = MrVariable( ['Phi', 'Theta'], NAME=traj_labl_ptr_vname )
	oTraj_Labl_Ptr['CATDESC']       = 'Labels for trajectory components.'
	oTraj_Labl_Ptr['FIELDNAM']      = 'Trajectory Labels'
	oTraj_Labl_Ptr['FORMAT']        = 'A5'
	oTraj_Labl_Ptr['VAR_TYPE']      = 'metadata'
	
	;TRAJ GDU1
	oTraj_GDU1['AXIS_RANGE']    = [-180.0, 180.0]
	oTraj_GDU1['CATDESC']       = 'Incident trajectories of EDI electrons in instrument coordinates measured by GDU1.'
	oTraj_GDU1['COLOR']         = ['Blue', 'Red']
	oTraj_GDU1['DELTA_PLUS']    = traj_delta
	oTraj_GDU1['DELTA_MINUS']   = traj_delta
	oTraj_GDU1['DISPLAY_TYPE']  = 'time_series'
	oTraj_GDU1['FIELDNAM']      = 'Incident Traj'
	oTraj_GDU1['FILLVAL']       = -1e31
	oTraj_GDU1['FORMAT']        = 'F9.4'
	oTraj_GDU1['LABEL']         = ['Phi', 'Theta']
	oTraj_GDU1['LABL_PTR_1']    = oTraj_Labl_Ptr
	oTraj_GDU1['SCALETYP']      = 'linear'
	oTraj_GDU1['SI_CONVERSION'] = '0.01745>rad'
	oTraj_GDU1['UNITS']         = 'degrees'
	oTraj_GDU1['VALIDMAX']      = -180.0
	oTraj_GDU1['VALIDMIN']      = 180.0
	oTraj_GDU1['VAR_TYPE']      = 'data'
	oTraj_GDU1['VAR_NOTES']     = traj_notes
	
	;TRAJ GDU1
	oTraj_BCS_GDU1['AXIS_RANGE']    = [-180.0, 180.0]
	oTraj_BCS_GDU1['CATDESC']       = 'Incident trajectories of EDI electrons in BCS coordinates measured by GDU1.'
	oTraj_BCS_GDU1['COLOR']         = ['Blue', 'Red']
	oTraj_BCS_GDU1['DELTA_PLUS']    = traj_delta
	oTraj_BCS_GDU1['DELTA_MINUS']   = traj_delta
	oTraj_BCS_GDU1['DISPLAY_TYPE']  = 'time_series'
	oTraj_BCS_GDU1['FIELDNAM']      = 'Incident Traj'
	oTraj_BCS_GDU1['FILLVAL']       = -1e31
	oTraj_BCS_GDU1['FORMAT']        = 'F9.4'
	oTraj_BCS_GDU1['LABEL']         = ['Phi', 'Theta']
	oTraj_BCS_GDU1['LABL_PTR_1']    = oTraj_Labl_Ptr
	oTraj_BCS_GDU1['SCALETYP']      = 'linear'
	oTraj_BCS_GDU1['SI_CONVERSION'] = '0.01745>rad'
	oTraj_BCS_GDU1['UNITS']         = 'degrees'
	oTraj_BCS_GDU1['VALIDMAX']      = -180.0
	oTraj_BCS_GDU1['VALIDMIN']      = 180.0
	oTraj_BCS_GDU1['VAR_TYPE']      = 'data'
	oTraj_BCS_GDU1['VAR_NOTES']     = traj_notes
	
	;TRAJ GDU1
	oTraj_DBCS_GDU1['AXIS_RANGE']    = [-180.0, 180.0]
	oTraj_DBCS_GDU1['CATDESC']       = 'Incident trajectories of EDI electrons in DBCS coordinates measured by GDU1.'
	oTraj_DBCS_GDU1['COLOR']         = ['Blue', 'Red']
	oTraj_DBCS_GDU1['DELTA_PLUS']    = traj_delta
	oTraj_DBCS_GDU1['DELTA_MINUS']   = traj_delta
	oTraj_DBCS_GDU1['DISPLAY_TYPE']  = 'time_series'
	oTraj_DBCS_GDU1['FIELDNAM']      = 'Incident Traj'
	oTraj_DBCS_GDU1['FILLVAL']       = -1e31
	oTraj_DBCS_GDU1['FORMAT']        = 'F9.4'
	oTraj_DBCS_GDU1['LABEL']         = ['Phi', 'Theta']
	oTraj_DBCS_GDU1['LABL_PTR_1']    = oTraj_Labl_Ptr
	oTraj_DBCS_GDU1['SCALETYP']      = 'linear'
	oTraj_DBCS_GDU1['SI_CONVERSION'] = '0.01745>rad'
	oTraj_DBCS_GDU1['UNITS']         = 'degrees'
	oTraj_DBCS_GDU1['VALIDMAX']      = -180.0
	oTraj_DBCS_GDU1['VALIDMIN']      = 180.0
	oTraj_DBCS_GDU1['VAR_TYPE']      = 'data'
	oTraj_DBCS_GDU1['VAR_NOTES']     = traj_notes
	
	;TRAJ GDU1
	oTraj_GSE_GDU1['AXIS_RANGE']    = [-180.0, 180.0]
	oTraj_GSE_GDU1['CATDESC']       = 'Incident trajectories of EDI electrons in GSE coordinates measured by GDU1.'
	oTraj_GSE_GDU1['COLOR']         = ['Blue', 'Red']
	oTraj_GSE_GDU1['DELTA_PLUS']    = traj_delta
	oTraj_GSE_GDU1['DELTA_MINUS']   = traj_delta
	oTraj_GSE_GDU1['DISPLAY_TYPE']  = 'time_series'
	oTraj_GSE_GDU1['FIELDNAM']      = 'Incident Traj'
	oTraj_GSE_GDU1['FILLVAL']       = -1e31
	oTraj_GSE_GDU1['FORMAT']        = 'F9.4'
	oTraj_GSE_GDU1['LABEL']         = ['Phi', 'Theta']
	oTraj_GSE_GDU1['LABL_PTR_1']    = oTraj_Labl_Ptr
	oTraj_GSE_GDU1['SCALETYP']      = 'linear'
	oTraj_GSE_GDU1['SI_CONVERSION'] = '0.01745>rad'
	oTraj_GSE_GDU1['UNITS']         = 'degrees'
	oTraj_GSE_GDU1['VALIDMAX']      = -180.0
	oTraj_GSE_GDU1['VALIDMIN']      = 180.0
	oTraj_GSE_GDU1['VAR_TYPE']      = 'data'
	oTraj_GSE_GDU1['VAR_NOTES']     = traj_notes
	
	;TRAJ GDU1
	oTraj_GSM_GDU1['AXIS_RANGE']    = [-180.0, 180.0]
	oTraj_GSM_GDU1['CATDESC']       = 'Incident trajectories of EDI electrons in GSM coordinates measured by GDU1.'
	oTraj_GSM_GDU1['COLOR']         = ['Blue', 'Red']
	oTraj_GSM_GDU1['DELTA_PLUS']    = traj_delta
	oTraj_GSM_GDU1['DELTA_MINUS']   = traj_delta
	oTraj_GSM_GDU1['DISPLAY_TYPE']  = 'time_series'
	oTraj_GSM_GDU1['FIELDNAM']      = 'Incident Traj'
	oTraj_GSM_GDU1['FILLVAL']       = -1e31
	oTraj_GSM_GDU1['FORMAT']        = 'F9.4'
	oTraj_GSM_GDU1['LABEL']         = ['Phi', 'Theta']
	oTraj_GSM_GDU1['LABL_PTR_1']    = oTraj_Labl_Ptr
	oTraj_GSM_GDU1['SCALETYP']      = 'linear'
	oTraj_GSM_GDU1['SI_CONVERSION'] = '0.01745>rad'
	oTraj_GSM_GDU1['UNITS']         = 'degrees'
	oTraj_GSM_GDU1['VALIDMAX']      = -180.0
	oTraj_GSM_GDU1['VALIDMIN']      = 180.0
	oTraj_GSM_GDU1['VAR_TYPE']      = 'data'
	oTraj_GSM_GDU1['VAR_NOTES']     = traj_notes
	
	;TRAJ GDU1
	oTraj_BCS_GDU2['AXIS_RANGE']    = [-180.0, 180.0]
	oTraj_BCS_GDU2['CATDESC']       = 'Incident trajectories of EDI electrons in BCS coordinates measured by GDU2.'
	oTraj_BCS_GDU2['COLOR']         = ['Blue', 'Red']
	oTraj_BCS_GDU2['DELTA_PLUS']    = traj_delta
	oTraj_BCS_GDU2['DELTA_MINUS']   = traj_delta
	oTraj_BCS_GDU2['DISPLAY_TYPE']  = 'time_series'
	oTraj_BCS_GDU2['FIELDNAM']      = 'Incident Traj'
	oTraj_BCS_GDU2['FILLVAL']       = -1e31
	oTraj_BCS_GDU2['FORMAT']        = 'F9.4'
	oTraj_BCS_GDU2['LABEL']         = ['Phi', 'Theta']
	oTraj_BCS_GDU2['LABL_PTR_1']    = oTraj_Labl_Ptr
	oTraj_BCS_GDU2['SCALETYP']      = 'linear'
	oTraj_BCS_GDU2['SI_CONVERSION'] = '0.01745>rad'
	oTraj_BCS_GDU2['UNITS']         = 'degrees'
	oTraj_BCS_GDU2['VALIDMAX']      = -180.0
	oTraj_BCS_GDU2['VALIDMIN']      = 180.0
	oTraj_BCS_GDU2['VAR_TYPE']      = 'data'
	oTraj_BCS_GDU2['VAR_NOTES']     = traj_notes
	
	;TRAJ GDU2
	oTraj_GDU2['AXIS_RANGE']    = [-180.0, 180.0]
	oTraj_GDU2['CATDESC']       = 'Incident trajectories of EDI electrons in instrument coordinates measured by GDU2.'
	oTraj_GDU2['COLOR']         = ['Blue', 'Red']
	oTraj_GDU2['DELTA_PLUS']    = traj_delta
	oTraj_GDU2['DELTA_MINUS']   = traj_delta
	oTraj_GDU2['DISPLAY_TYPE']  = 'time_series'
	oTraj_GDU2['FIELDNAM']      = 'Incident Traj'
	oTraj_GDU2['FILLVAL']       = -1e31
	oTraj_GDU2['FORMAT']        = 'F9.4'
	oTraj_GDU2['LABEL']         = ['Phi', 'Theta']
	oTraj_GDU2['LABL_PTR_1']    = oTraj_Labl_Ptr
	oTraj_GDU2['SCALETYP']      = 'linear'
	oTraj_GDU2['SI_CONVERSION'] = '0.01745>rad'
	oTraj_GDU2['UNITS']         = 'degrees'
	oTraj_GDU2['VALIDMAX']      = -180.0
	oTraj_GDU2['VALIDMIN']      = 180.0
	oTraj_GDU2['VAR_TYPE']      = 'data'
	oTraj_GDU2['VAR_NOTES']     = traj_notes
	
	;TRAJ GDU2
	oTraj_DBCS_GDU2['AXIS_RANGE']    = [-180.0, 180.0]
	oTraj_DBCS_GDU2['CATDESC']       = 'Incident trajectories of EDI electrons in DBCS coordinates measured by GDU2.'
	oTraj_DBCS_GDU2['COLOR']         = ['Blue', 'Red']
	oTraj_DBCS_GDU2['DELTA_PLUS']    = traj_delta
	oTraj_DBCS_GDU2['DELTA_MINUS']   = traj_delta
	oTraj_DBCS_GDU2['DISPLAY_TYPE']  = 'time_series'
	oTraj_DBCS_GDU2['FIELDNAM']      = 'Incident Traj'
	oTraj_DBCS_GDU2['FILLVAL']       = -1e31
	oTraj_DBCS_GDU2['FORMAT']        = 'F9.4'
	oTraj_DBCS_GDU2['LABEL']         = ['Phi', 'Theta']
	oTraj_DBCS_GDU2['LABL_PTR_1']    = oTraj_Labl_Ptr
	oTraj_DBCS_GDU2['SCALETYP']      = 'linear'
	oTraj_DBCS_GDU2['SI_CONVERSION'] = '0.01745>rad'
	oTraj_DBCS_GDU2['UNITS']         = 'degrees'
	oTraj_DBCS_GDU2['VALIDMAX']      = -180.0
	oTraj_DBCS_GDU2['VALIDMIN']      = 180.0
	oTraj_DBCS_GDU2['VAR_TYPE']      = 'data'
	oTraj_DBCS_GDU2['VAR_NOTES']     = traj_notes
	
	;TRAJ GDU2
	oTraj_GSE_GDU2['AXIS_RANGE']    = [-180.0, 180.0]
	oTraj_GSE_GDU2['CATDESC']       = 'Incident trajectories of EDI electrons in GSE coordinates measured by GDU2.'
	oTraj_GSE_GDU2['COLOR']         = ['Blue', 'Red']
	oTraj_GSE_GDU2['DELTA_PLUS']    = traj_delta
	oTraj_GSE_GDU2['DELTA_MINUS']   = traj_delta
	oTraj_GSE_GDU2['DISPLAY_TYPE']  = 'time_series'
	oTraj_GSE_GDU2['FIELDNAM']      = 'Incident Traj'
	oTraj_GSE_GDU2['FILLVAL']       = -1e31
	oTraj_GSE_GDU2['FORMAT']        = 'F9.4'
	oTraj_GSE_GDU2['LABEL']         = ['Phi', 'Theta']
	oTraj_GSE_GDU2['LABL_PTR_1']    = oTraj_Labl_Ptr
	oTraj_GSE_GDU2['SCALETYP']      = 'linear'
	oTraj_GSE_GDU2['SI_CONVERSION'] = '0.01745>rad'
	oTraj_GSE_GDU2['UNITS']         = 'degrees'
	oTraj_GSE_GDU2['VALIDMAX']      = -180.0
	oTraj_GSE_GDU2['VALIDMIN']      = 180.0
	oTraj_GSE_GDU2['VAR_TYPE']      = 'data'
	oTraj_GSE_GDU2['VAR_NOTES']     = traj_notes
	
	;TRAJ GDU2
	oTraj_GSM_GDU2['AXIS_RANGE']    = [-180.0, 180.0]
	oTraj_GSM_GDU2['CATDESC']       = 'Incident trajectories of EDI electrons in GSM coordinates measured by GDU2.'
	oTraj_GSM_GDU2['COLOR']         = ['Blue', 'Red']
	oTraj_GSM_GDU2['DELTA_PLUS']    = traj_delta
	oTraj_GSM_GDU2['DELTA_MINUS']   = traj_delta
	oTraj_GSM_GDU2['DISPLAY_TYPE']  = 'time_series'
	oTraj_GSM_GDU2['FIELDNAM']      = 'Incident Traj'
	oTraj_GSM_GDU2['FILLVAL']       = -1e31
	oTraj_GSM_GDU2['FORMAT']        = 'F9.4'
	oTraj_GSM_GDU2['LABEL']         = ['Phi', 'Theta']
	oTraj_GSM_GDU2['LABL_PTR_1']    = oTraj_Labl_Ptr
	oTraj_GSM_GDU2['SCALETYP']      = 'linear'
	oTraj_GSM_GDU2['SI_CONVERSION'] = '0.01745>rad'
	oTraj_GSM_GDU2['UNITS']         = 'degrees'
	oTraj_GSM_GDU2['VALIDMAX']      = -180.0
	oTraj_GSM_GDU2['VALIDMIN']      = 180.0
	oTraj_GSM_GDU2['VAR_TYPE']      = 'data'
	oTraj_GSM_GDU2['VAR_NOTES']     = traj_notes

;-------------------------------------------
; Finish ///////////////////////////////////
;-------------------------------------------
	
	;Remove L1A variables
	MrVar_Delete, [ phi_gd12_vname, phi_gd21_vname, theta_gd12_vname,  theta_gd21_vname ]
;	MrVar_Delete, [ phi_gd12_vname, phi_gd21_vname, theta_gd12_vname,  theta_gd21_vname, $
;	                cts_gd12_vname, cts_gd21_vname ]
	
	;Output variable names
	varnames = [ traj_dbcs_gdu1_vname, traj_gse_gdu1_vname, traj_gsm_gdu1_vname, $
	             traj_dbcs_gdu2_vname, traj_gse_gdu2_vname, traj_gsm_gdu2_vname, $
	             ang_diff_vname, flux_gdu1_vname, flux_gdu2_vname ]
END