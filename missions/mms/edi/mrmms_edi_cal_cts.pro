; docformat = 'rst'
;
; NAME:
;    mms_edi_amb_srvy_calibrate
;
; PURPOSE:
;+
;   Apply calibration parameters to EDI data.
;
;==========================================
;
; AMBIENT MODES:
;
;   Looking up specs in EDI design documents, the EDI sensor deadtime is nominally 200ns,
;   although with a fairly generous error (+/- 50 ns)
;
;   The accumulation times to be used in the deadtime corrections before 
;   applying the relative calibration are:
;
;     Burst: 1/1024 sec (0.9765625 ms)
;     Survey: 16/1024 sec (15.625 ms)
;
;   The accumulation time for survey counts is 16/1024 seconds (and 1/1024 
;   for burst). For the cal factor adjustment for survey another factor of
;   two comes from the fact that we are summing up two adjacent anodes.
;   [this comes into play when applying the absolute calibration].
;   
;   These are the right settings for "amb" survey:
;   
;      accumulation time = 16./1024
;      abscal_counts     = relcal_counts * (abscal_factor / 32)
;   
;   And for "amb-pm2" survey uses a single pad instead of two, so there we 
;   would use:
;   
;      accumulation time = 16./1024
;      abscal_counts     = relcal_counts * (abscal_factor / 16)
;
;
;   Dead-time correction formula:
;     Ct = Cm / ( 1 -  Cm * dt / Ta)
;     Cm = measured counts
;     Ct = true counts
;     dt = dead time (250 ns)
;     Ta = accumulation time ( 2^(-10) seconds for EDI ambient mode burst telemetry )
;
;   Dead-time error formula:
;     dCm = sqrt(Cm)                          (Laplacian counting error)
;     dCt = Ct * [ 1/Cm + 1/(Ta/dt + Cm) ]    (derivative of Ct with respect to dt)
;     dCt = Error in true counts
;     dCm = Error in measured counts
;
;   Error Propagation:
;
;
;   Assuming the fluxes are expressed as
;   
;           F = C * R * A
;   
;   where
;   
;           C = deadtime corrected counts
;           R = relative calibration factor (flat fielding, angle-dependent)
;           A = absolute calibration factor
;   
;   Then
;   
;           errF = sqrt[ (R*A)^2 * errC^2  +  (R*C)^2 * errA^2 ]
;   
;   where
;   
;           errC = error in deadtime corrected counts (see below)
;           errA = 0.15 * A  (15 percent error)
;   
;   Later on, there will be a third term "(C*A)^2 * errR^2" under the square 
;   root. We are skipping that for now.
;   
;   The formula for the error of the deadtime corrected counts that Roy 
;   derived is:
;   
;           errC = sqrt(X) * [ tA/(tA - X*tD) ]^2
;   
;   where
;           X = raw counts
;           tA = accumulation time (1/1024 sec for burst, 16/1024 sec for survey)
;           tD = deadtime (200ns)
;
;==========================================
;
; EFIELD MODES (Q0 and DATA29):
;
;   1) accumulation time: for E_Field mode data (q0 survey/burst and data29
;   burst) the accumulation time is 1/1024.
;   
;   2) dead time: the sensor dead time is 200 ns (2e-7 sec)
;   
;   3) cal factor correction: the ambient mode cal factor correction
;   addresses two things, I think: two vs one anode (survey one-sided vs
;   survey centered), and burst vs survey accumulation times. Translating
;   this to the E_Field Mode situation, we have: two anodes (q0) vs one
;   anode (data 29), but the accumulation times for q0 and data29 are
;   identical.
;   
;   Note that there is an additional effect when trying to compare q0 data
;   to data 29. q0 is data coming out of the correlator and combines events
;   from the two inner anodes. The two event pulse streams coming from the
;   two sensor anodes (and have already seen a 200ns dead time imposed on
;   them individually) are merged in hardware before presenting the result
;   as a single event pulse stream to the correlator. The merging circuit
;   removes pulses that are too close to each other, resulting in effects
;   similar to the dead time: the higher the count rate the more likely
;   there are two events close to each other on the two event pulse lines,
;   in which case one will be eliminated. I need to look up the specs of
;   this merging circuit. Even with that in hand, there is no exact math in
;   that one does not know (after the fact) what the input count rates from
;   the two individual anodes were. I will think about what can be done.
;
;
; :Params:
;       EDI:        in, required, type=struct
;                   Structure of edi raw counts to be calibrated.
;       CALS:       in, required, type=string
;                   Structure of edi calibration parameters.
;
; :Keywords:
;       ABSCAL:     in, optional, type=boolean, default=0
;                   If set, absolute calibration factors will be applied to the data.
;
; :Returns:
;       OCALCTS:    out, required, type=objref
;                   A MrScalarTS variable containing the calibrated counts.
;-
;*****************************************************************************************
;+
;
;-
function MrMMS_EDI_Cal_Cts, sc, mode, optdesc, gdu, cts, traj, $
ABSCAL=abscal, $
CACHE=cache, $
NAME=name, $
ONE_SIDED=one_sided
	compile_opt idl2
	on_error, 2
	
	;Defaults
	abs_err_pct  = 0.2  ;20% error on absolute calibration factor
	tf_abscal    = Keyword_Set(abscal)
	tf_cache     = Keyword_Set(cache)
	tf_one_sided = Keyword_Set(one_sided)
	tf_relcal    = 1B
	IF N_Elements(one_sided) EQ 0 THEN one_sided = 0
	IF Array_Equal(sc   EQ 'mms'+['1', '2', '3', '4'], 0) THEN Message, 'SC must be {"mms1" | "mms2" | "mms3" | "mms4"}.'
	IF Array_Equal(mode EQ ['srvy', 'brst'],           0) THEN Message, 'MODE must be {"srvy" | "brst"}.'
	IF Array_Equal(gdu  EQ ['gdu1', 'gdu2'],           0) THEN Message, 'GDU must be {"gdu1" | "gdu2"}.'
	
	nPts = N_Elements(cts)
	tf_one_sided = N_Elements(one_sided) EQ 1 ? Replicate(one_sided, nPts) : one_sided
	
	instr    = 'edi'
	datatype = tf_abscal ? 'flux' : 'counts'
	
	;Angular widths
	dPhi      = 360.0 / 32
	dTheta    = 360.0 / 512
	type      = tf_abscal ? 4 : 13  ; float | ulong
	fillval   = tf_abscal ? -1e31 : 4294967295UL
	
;-------------------------------------------
; Load Data ////////////////////////////////
;-------------------------------------------
	
	;EDI Calibrations
	MrMMS_EDI_Load_Cals, sc, $
	                     VARFORMAT='*'+['rel', 'abs']+'*'+gdu+'*cal*'
	
;-------------------------------------------
; Input & Output Data //////////////////////
;-------------------------------------------
	
	;Variable names
	relcal_vname = StrJoin( [sc, instr, 'rel', gdu, 'cal', 'l2'], '_')
	abscal_vname = StrJoin( [sc, instr, 'abs', gdu, 'cal', 'l2'], '_')
	
	;Output variable names
	delta_vname = StrJoin( [sc, instr, datatype, 'delta', gdu, optdesc, mode, 'l2'], '_')
	IF N_Elements(name) EQ 0 $
		THEN name  = StrJoin( [sc, instr, datatype, gdu, optdesc, mode, 'l2'], '_')
	
	;Variables
	oRel  = MrVar_Get(relcal_vname)
	oAbs  = MrVar_Get(abscal_vname)
	oCts  = MrVar_Get(cts)
	oTraj = MrVar_Get(Traj)
	
	;Insert an artificial data point
	IF 1 THEN BEGIN
		;Read calibration table
		IF gdu EQ 'gdu1' $
			THEN Restore, FILENAME='/home/argall/data/mms2_gdu1_relcal.sav' $
			ELSE Restore, FILENAME='/home/argall/data/mms2_gdu2_relcal.sav'
		
		;Append data point
		oT    = oRel['TIMEVAR']
		oT   -> SetData, [ oT['DATA',0], '2015-10-16T00:00:00.000000000Z', oT['DATA',1] ]
		oRel -> SetData, [ oRel['DATA',0,*,*], Transpose( Reform( data.relcal, 1, 32, 129 ), [0,2,1] ), oRel['DATA',1,*,*] ]
		
		;GDU2 scale factor
		scl_gdu2 = 0.73
	ENDIF

;-------------------------------------------
; Dead-Time Correction /////////////////////
;-------------------------------------------

	;Correct for dead-time
	dt = 200.0e-9  ; in seconds
	IF mode EQ 'brst' $
		THEN ta = 2.0^(-10) $ ; 1/1024 seconds
		ELSE ta = 2.0^(-6)    ; 1/64   seconds
	
	;Dead-Time Correction
	oCts_DT = oCts / (1.0 - oCts * (dt/ta) )
	
;-------------------------------------------
; Relative Calibrations ////////////////////
;-------------------------------------------
	;Find nearest calibrations
	iRel = oRel['TIMEVAR'] -> Value_Locate(oCts['TIME', 'TT2000'], 'TT2000')
	IF Min(iRel) EQ -1 THEN BEGIN
		MrPrintF, 'LogErr', 'No relative calibrations for "' + gdu + '".'
		tf_relcal = 0
	ENDIF
	
	;Polar index
	CASE optdesc OF
		'q0':     iTheta = Fix( Round(oTraj['DATA',*,1] / dTheta), TYPE=12 )
		'data29': iTheta = Fix( Round(oTraj['DATA',*,1] / dTheta), TYPE=12 )
		'amb':    iTheta = oTraj['DATA',*,1]
		ELSE: Message, 'OptDesc not recognized: "' + optdesc + '".'
	ENDCASE
	
	;Azimuthal index
	CASE optdesc OF
;		'q0':     iPhi = Fix( Round( oTraj['DATA',*,0] / dPhi ), TYPE=12 )
;		'data29': iPhi = Fix( Round( oTraj['DATA',*,0] / dPhi ), TYPE=12 )
		'q0': BEGIN
			temp = (180.0 - oTraj['DATA',*,0])
			temp = (temp + (temp LT 0) * 360) MOD 360.0
			iPhi = Fix( Round( Temporary(temp) / dPhi), TYPE=12 ) - 1
		ENDCASE
		'data29': BEGIN
			temp = (180.0 - oTraj['DATA',*,0])
			temp = (temp + (temp LT 0) * 360) MOD 360.0
			iPhi = Fix( Round( Temporary(temp) / dPhi), TYPE=12 ) - 1
		ENDCASE
		'amb': iPhi = oTraj['DATA',*,0]
		ELSE: Message, 'OptDesc not recognized: "' + optdesc + '".'
	ENDCASE
	
	;Extract and apply relative calibrations
	oRelCts = oCts_DT * oRel[iRel, iTheta, iPhi]
	
;-----------------------------------------------------
; Absolute Calibrations \\\\\\\\\\\\\\\\\\\\\\\\\\\\\\
;-----------------------------------------------------
	;Find nearest calibration
	iAbs = oAbs['TIMEVAR'] -> Value_Locate(oCts['TIME', 'TT2000'], 'TT2000')
	IF Min(iAbs) EQ -1 THEN Message, 'No absolute calibrations for "' + gdu + '".'
	
	;Absolute calibrations first require relative calibrations
	IF tf_abscal && ~tf_relcal THEN Message, 'No relative cal data. Cannot apply absolute cals.'
	
	;Absolute calibrations
	IF tf_abscal THEN BEGIN
		;BRST
		IF mode EQ 'brst' THEN BEGIN
			IF optdesc EQ 'q0' $
				THEN oCalCts = MrScalarTS( oCts['TIMEVAR'], oRelCts * (oAbs[iAbs] / 2.0) ) $
				ELSE oCalCts = MrScalarTS( oCts['TIMEVAR'], oRelCts * oAbs[iAbs] )
		
		;SRVY
		ENDIF ELSE BEGIN
			iOneSide = Where(tf_one_sided, nOneSide, COMPLEMENT=iCenter, NCOMPLEMENT=nCenter)
			oCalCts  = MrScalarTS( oCts['TIMEVAR'], FltArr(nOneSide + nCenter) )
			IF nOneSide GT 0 THEN oCalCts[iOneSide] = oRelCts[iOneSide] * (oAbs[iAbs[iOneSide]] / 16.0)
			IF nCenter  GT 0 THEN oCalCts[iCenter]  = oRelCts[iCenter]  * (oAbs[iAbs[iCenter]]  / 32.0)
		ENDELSE
	
	;Do not apply absolute calibrations
	;   - Fix to integer ULong
	ENDIF ELSE BEGIN
		oCalCts = MrScalarTS( oCts['TIMEVAR'], Fix(Round(oRelCts['DATA']), TYPE=13) )
	ENDELSE
	
;-----------------------------------------------------
; Inter-GDU Calibrations \\\\\\\\\\\\\\\\\\\\\\\\\\\\\
;-----------------------------------------------------
	IF gdu EQ 'gdu2' && N_Elements(scl_gdu2) GT 0 $
		THEN oCalCts *= scl_gdu2
	
;-----------------------------------------------------
; Errors \\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\
;-----------------------------------------------------
	;
	; TODO: Incorporate relative calibration error
	;

	;Raw counts error
	err_raw = Sqrt(oCts['DATA'])

	;Error from dead-time correction formula
	;   - Careful of C = R = 0 case. Should be 1.
	iZero = oCts -> Where(0, /EQUAL, COUNT=nZero, COMPLEMENT=iGood, NCOMPLEMENT=nGood)
	dC_dR = FltArr(nZero+nGood)
	if nGood gt 0 then dC_dR[iGood] = sqrt(oCts['DATA',igood]) * ( ta / (ta - oCts['DATA',igood]*dt) )^2.0
;	if nGood gt 0 then dC_dR[iGood] = oCts_DT['DATA',igood] * ( 1.0/oCts_DT['DATA',igood] + 1.0/(oCts_DT['DATA',igood] + ta/dt) )
	if nZero gt 0 then dC_dR[iZero] = 1.0
	
	;Error in dead-time corrected counts
	err_DT = Temporary(dC_dR)
;	err_DT = err_raw * Temporary(dC_dR)
	
	;ABSCAL Error
	IF tf_abscal THEN BEGIN
		;Independent errors
		err_abs = abs_err_pct * oAbs['DATA', iAbs]
		
		;Total error
;		err_tot = Sqrt( (oRel['DATA', iTheta, iPhi, iRel] * oAbs['DATA',iAbs] )^2 * err_DT + $
;		                (oRel['DATA', iTheta, iPhi, iRel] * oCts_DT['DATA']   )^2 * err_abs )
		err_tot = oCalCts['DATA'] * Sqrt( (err_DT/oCts_DT['DATA'])^2.0 + abs_err_pct^2.0 )
	
	;RELCAL Error
	ENDIF ELSE BEGIN
		;Total Error
		err_tot  = Temporary(err_raw) * Temporary(err_DT)
		
		;Integer count error
		;   - Fix to integer ULong
		err_tot = Fix(Round(err_tot), TYPE=13)
	ENDELSE

;-----------------------------------------------------
; Attributes \\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\
;-----------------------------------------------------
	;Error
	oErrTot = MrTimeSeries(oCts['TIMEVAR'], err_tot, NAME=delta_vname, /NO_COPY)
	oErrTot['CATDESC']       = datatype + ' error'
	oErrTot['DISPLAY_TYPE']  = 'time_series'
	oErrTot['FIELDNAM']      = tf_abscal ? 'Flux' : 'Counts'
	oErrTot['FILLVAL']       = -1e31
	oErrTot['FORMAT']        = 'E11.4'
	oErrTot['LABLAXIS']      = tf_abscal ? 'Flux' : 'Counts'
	oErrTot['SCALETYP']      = tf_abscal ? 'log' : 'linear'
	oErrTot['SI_CONVERSION'] = tf_abscal ? '1e4>m^-2 s^-1' : '>'
	oErrTot['UNITS']         = tf_abscal ? 'cm^-2 s^-1' : 'counts'
	oErrTot['VALIDMAX']      = 1e20
	oErrTot['VALIDMIN']      = 1e0
	oErrTot['VAR_TYPE']      = 'data'
	
	;Calibrated counts
	oCalCts -> SetName, name
	oCalCts['CATDESC']         = datatype + ' error'
	oCalCts['DELTA_MINUS_VAR'] = oErrTot
	oCalCts['DELTA_PLUS_VAR']  = oErrTot
	oCalCts['DISPLAY_TYPE']    = 'time_series'
	oCalCts['FIELDNAM']        = tf_abscal ? 'Flux' : 'Counts'
	oCalCts['FILLVAL']         = -1e31
	oCalCts['FORMAT']          = 'E11.4'
	oCalCts['LABLAXIS']        = tf_abscal ? 'Flux' : 'Counts'
	oCalCts['SCALETYP']        = tf_abscal ? 'log' : 'linear'
	oCalCts['SI_CONVERSION']   = tf_abscal ? '1e4>m^-2 s^-1' : '>'
	oCalCts['UNITS']           = tf_abscal ? 'cm^-2 s^-1' : 'counts'
	oCalCts['VALIDMAX']        = 1e20
	oCalCts['VALIDMIN']        = 1e0
	oCalCts['VAR_TYPE']        = 'data'
	
;-----------------------------------------------------
; Finish Up \\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\
;-----------------------------------------------------
	;Remove calibration data from cache
	MrVar_Delete, [relcal_vname, abscal_vname]
	
	;Cache
	IF tf_cache THEN oCalCts -> Cache
	
	RETURN, oCalCts
	
;-------------------------------------------
; SetUp ////////////////////////////////////
;-------------------------------------------
	
	;Get the variables
	oCts  = MrVar_Get(cts)
	oTraj = MrVar_Get(traj)

	;Check for fill values
	ifill = oCts -> Where(65535US, /EQUAL, COUNT=nfill)
END