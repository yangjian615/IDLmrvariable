; docformat = 'rst'
;
; NAME:
;       MrMMS_Bromund_Rotate
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
;   Despin and/or rotate a vector into GSE or GSM coordinates from a spacecraft
;   coordinate system.
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
;       2017-04-19  -   Written by Matthew Argall
;-
;*****************************************************************************************
;+
;   Despin a vector.
;
; :Params:
;       ATT_TIME:       in, required, type=int64 (cdf_time_tt2000)
;                       Time tags of definitive attitude data.
;       DEF_ATT:        in, required, type=struct
;                       Definitive attitude data.
;       EPH_TIME:       in, required, type=int64 (cdf_time_tt2000)
;                       Time tags of definitive ephemeris data.
;       DEF_EPH:        in, required, type=struct
;                       Definitive ephemeris data.
;       TIME:           in, required, type=int64 (cdf_time_tt2000)
;                       Time to which attitude and ephemeris data is interpolated.
;       INPUT:          in, required, type=3xN float
;                       The vector to be despun.
;
; :Keywords:
;       REVERSE:    in, optional, type=boolean, default=0
;                   If set, `INPUT` will be spun up instead of despun.
;       M:          out, optional, type=3x3xN
;                   The transformation matrix from despun to GSE coordinates.
;       TYPE:       in, optional, type=string, default='P'
;                   The coordinate system in which `VEC` is measured. Options are 'P' for
;                       SMPA, 'Z' for BCS, and 'L' for SSL.
;
; :Returns:
;       OUTPUT:         out, required, type=3xN float
;                       The despun vector.

;-
FUNCTION MrMMS_Bromund_Rotate_Despun2GSE, att_time, def_att, eph_time, def_eph, time, input, output, $
M=m, $
REVERSE=reverse, $
TYPE=type
	Compile_Opt idl2
	On_Error, 2
	
	CASE StrUpCase(type) OF
		'P': BEGIN
			ra  = def_att.pra
			dec = def_att.pdec
		ENDCASE
		
		'L': BEGIN
			ra  = def_att.lra
			dec = def_att.ldec
		ENDCASE
		
		'Z': BEGIN
			ra  = def_att.zra
			dec = def_att.zdec
		ENDCASE
		
		ELSE: Message, 'TYPE must be {"P" | "L" | "Z"}'
	ENDCASE
	
	; Spacecraft attitude (angular momentum vector). WARNING: this should be smoothed
	; these are in J2000
	; Interpolate: this is how THEMIS did it, but could run into 360 degree trouble,
	; makes me wonder if they really use dsl2gse.pre.
	; also, L will be changing noticibly in definitive attitude at 20 sec period, 
	; and this interpolation method is not appropriate
	lra  = Interpol(phunwrap(*ra, MAXVAL=360), att_time, time) 
	ldec = Interpol(*dec, att_time, time)

	; Spacecraft position
	; Interpolate: use the MMS-approved hermite interpolation
	R = DblArr(3, N_Elements(eph_time))
	V = R

	R[0,*] = *def_eph.xeci
	R[1,*] = *def_eph.yeci
	R[2,*] = *def_eph.zeci

	V[0,*] = *def_eph.vxeci
	V[1,*] = *def_eph.vyeci
	V[2,*] = *def_eph.vzeci

	r = mms_fg_xinterp_ephem(eph_time, r, v, time)

	mms_fg_xj2k2gse, time, r, rGSE

	;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
	; We find unit vectors for the X, Y, Z axes of the DSL system in GSE coordinates
	;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
	;
	; Z: make a unit vector that points along the angular momentum vector (L)
	L = angunitvec(lra, ldec, /dec) ; dim [3, N], still in J2000  

	; take average of L over the given attitude input range.
	; TODO: use polynomial fit, constrained to match fit to previous orbits...(this will 
	;       require pre-processing of FDOA data, or at least some method of saving 
	;       previous fits)
	zdsl = total(L,2)/n_elements(time)  ; average each of the components
	zdsl /= sqrt(total(zdsl*zdsl))      ; renormalize
	zdsl = zdsl#replicate(1,n_elements(time))  ;expand into a [3,N] array

	; we might want to check the residuals. If there is a trend in L,
	; we may need a linear or polynomial fit. 
	; HOWEVER, L SHOULD BE 
	; SMOOTHED AT A HIGHER LEVEL. OTHERWISE, WE WILL GET DIFFERENT 
	; RESULTS DEPENDING ON THE TIMESPAN OF THE ATTITUDE DATA THAT IS PASSED INTO
	; THIS ROUTINE.  THE TIMESPAN MAY NOT BE OPTIMIZED FOR FITTING L, AND
	; WILL LEAD TO DISCONTINUITIES IN L.

	mms_fg_xj2k2gse, time, zdsl, zdslGSE ; convert it to GSE coordinates

	; Y: Z x Vector to from spacecraft to sun 
	; Vector from spacecraft to sun = position of sun in GSE (km) - position of SC in GSE (km)
	; correcting for parallax errors in this manner removes error possibly as large as
	; 0.06 degree.  NOTE: Distance to sun as a function of time is not needed 
	; (it change the parallax correction by at most 1.7%: no more than 0.001 degrees).
	sun = -rGSE
	sun[0, *] += !const.au/1000  
	sun = unitize(sun) 
	ydsl = crosspn(zdslGSE, sun)
	ydsl = unitize(ydsl)

	xdsl = crosspn(ydsl, zdslGSE)

	m = dblarr(3,3,n_elements(time))
	; fill out the gse2dsl xform matrix, row by row
	m[0,*,*]=xdsl
	m[1,*,*]=ydsl
	m[2,*,*]=zdslGSE
	; to get dsl2gse, we take the transpose.
	if not keyword_set(reverse) then m = transpose(m, size(/n_dim, m) gt 2 ? [1,0,2]:[1,0])
	output = mms_fg_matrix_mult_data(m, input)
	
	RETURN, output
END


;+
;   Despin and/or rotate a vector into GSE or GSM coordinates from a spacecraft
;   coordinate system.
;
; :Params:
;       VEC:        in, required, type=string/int/objref
;                   The name, number, or object reference of a MrVectorTS variable.
;       SC:         in, optional, type=string
;                   The spacecraft identifier. Options are {'mms1' | 'mms2' | 'mms3' | 'mms4' }
;                       This parameter is required if `DEFATT` and `DEFEPH` are not given.
;
; :Keywords:
;       DEFEPH:     in, out, required, type=struct
;                   Definitive ephemeris data obtained from mms_fg_defeph. If not provided,
;                       then it is read from the source file and returned into the named
;                       variable DEFEPH. Files are found automatically.
;       DEFATT:     in, out, optional, type=struct
;                   Definitive attitude data obtained from mms_fg_defatt. If not provided,
;                       then it is read from the source file and returned into the named
;                       variable DEFATT. Files are found automatically.
;       TYPE:       in, optional, type=string, default='P'
;                   The coordinate system in which `VEC` is measured. Options are 'P' for
;                       SMPA or DMPA, 'Z' for BCS or DBCS, and 'L' for SSL or DSL.
;       _REF_EXTRA: in, optional, type=any
;                   Any keyword accepted by mms_fg_config is also accepted here.
;
; :Returns:
;       VOUT:       out, required, type=objref
;                   A MrVectorTS variable with the rotated results.
;-
PRO MrMMS_Bromund_Rotate, vec, sc, $
DEFATT=def_att, $
DEFEPH=def_eph, $
DESPIN=despin, $
DESPUN=oDespun, $
GSE=oGSE, $
GSM=oGSM, $
TYPE=type, $
_REF_EXTRA=extra
	Compile_Opt idl2
;	On_Error, 2

	Catch, the_error
	IF the_error NE 0 THEN BEGIN
		Catch, /CANCEL
		MrPrintF, 'LogErr'
		RETURN
	ENDIF
	
	;Define location of data
	DefSysV, '!MMS_FG', EXIST=tf_exist
	IF N_Elements(extra) GT 0 || ~tf_exist THEN MrMMS_Bromund_Config, _STRICT_EXTRA=extra
	
	;Defaults
	tf_despin = Keyword_Set(despin)
	IF N_Elements(type) EQ 0 THEN type = 'P'
	
	;Extract data from object
	oV   = MrVar_Get(vec)
	oT   = oV['TIMEVAR']
	time = oV['TIME', 'TT2000']
	data = oV['DATA']
	
;-------------------------------------------
; DEFATT ///////////////////////////////////
;-------------------------------------------

	IF N_Elements(def_att) EQ 0 THEN BEGIN
		;Read attitude data
		;   - Finds attitude files automatically
		mms_fg_read_defatt, sc, time, $
		                    DEF_ATT   = def_att, $
		                    FILENAMES = attfiles, $
		                    MPA       = mpa, $
		                    Q         = Q, $
		                    TT2000    = att_time
		
		;No data found
		IF Size(att_time, /TYPE) EQ 0 $
			THEN MrPrintF, 'LogErr', 'No definitive attitude found.'
		
		;Allowing 30 sec extrapolation, to let cluster test proceed...
		IF att_time[0] GT time[0] || att_time[-1] + uLong64(30.d9) LT time[-1] $
			THEN Message, 'Definitive attitude loaded does not cover entire timespan'
	ENDIF ELSE BEGIN
		att_time = MrCDF_Epoch_Parse( *def_att.utc, $
		                              PATTERN='%Y-%DT%H:%m:%S%f', $
		                              /TO_TT2000 )
	ENDELSE

;-------------------------------------------
; DEFEPH ///////////////////////////////////
;-------------------------------------------
	IF N_Elements(def_eph) EQ 0 THEN BEGIN
		;Read ephemeris data
		;   - Finds ephemeris files automatically
		mms_fg_read_eph, sc, time, 'defeph', $
		                 eph       = def_eph, $
		                 tt2000    = eph_time, $
		                 filenames = ephfiles
		
		;No data found
		IF Size(eph_time, /TYPE) EQ 0 $
			THEN Message, 'No definitive ephemeris found.'
		
		; subset the data -- adding 15 seconds tolerance to start/stop of 
		;                    the ephemeris data, so that we always find 
		;                    at least one ephemeris point for bursts
		;                    shorter than 30 sec.  (assumes ephemeris 
		;                    will always have 30 second cadence.)
		keepstate = Where( eph_time+15*Long64(1d9) GE time[0] AND $
		                   eph_time-15*Long64(1d9) LE time[-1], nstate)
		
		IF nstate EQ 0 $
			THEN Message, 'Definitive ephemeris loaded does not cover entire timespan'
		
		; add extra points so we don't have to extrapolate ephemeris later on
		eb = keepstate[0] -1
		ee = keepstate[-1] +1
		IF eb GE 0 THEN keepstate = [eb, keepstate]
		IF ee LT N_Elements(eph_tt2000) THEN keepstate = [keepstate, ee]
	ENDIF ELSE BEGIN
		eph_time = MrCDF_Epoch_Parse( *def_eph.epoch, $
		                              PATTERN='%Y-%D/%H:%m:%S%f', $
		                              /TO_TT2000 )
	ENDELSE

;-------------------------------------------
; Despin ///////////////////////////////////
;-------------------------------------------
	IF tf_despin THEN BEGIN
		;Smooth and interpolate the phase
		phase = mms_fg_xinterp_phase( def_att, att_time, time, $
		                              /SMOOTH, $
		                              TYPE = type )
		
		;Despin
		mms_fg_xdespin, phase, Transpose(data), v_despun;, $
;		                M       = m, $
;		                REVERSE = reverse
	ENDIF ELSE BEGIN
		v_despun = Transpose(Temporary(data))
	ENDELSE

;-------------------------------------------
; GSE //////////////////////////////////////
;-------------------------------------------
	v_gse = MrMMS_Bromund_Rotate_despun2GSE( att_time, def_att, eph_time, def_eph, time, v_despun, $
;	                                         M       = m, $
;	                                         REVERSE = reverse, $
	                                         TYPE    = type )

;-------------------------------------------
; GSM //////////////////////////////////////
;-------------------------------------------
	
	;GSE to GSM
	IF Arg_Present(oGSM) THEN mms_fg_xgse2gsm, time, v_gse, v_gsm

;-------------------------------------------
; Create Vectors ///////////////////////////
;-------------------------------------------
	;DESPUN
	IF Arg_Present(oDespun) THEN BEGIN
		oDespun = MrVectorTS( oT, Temporary(v_despun), $
		                      NAME   = oV.name + '_despun', $
		                      T_TYPE = 'TT2000' )
	ENDIF

	;GSE
	oGSE = MrVectorTS( oT, Temporary(v_gse), $
	                   NAME   = oV.name + '_gse', $
	                   T_TYPE = 'TT2000' )
	
	;GSM
	IF Arg_Present(oGSM) THEN BEGIN
		oGSM = MrVectorTS( oT, v_gsm, $
		                   NAME   = oV.name + '_gsm', $
		                   /NO_COPY, $
		                   T_TYPE = 'TT2000' )
	ENDIF
END