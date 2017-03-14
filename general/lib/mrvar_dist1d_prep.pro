; docformat = 'rst'
;
; NAME:
;       MrVar_Dist1D_Prep
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
;   Extract 1D cuts along the principle axes of a 2D distribution function. Data in the
;   two bins on either size of +X, -X, +Y, and -Y are averaged together to make the ouput
;   data. The following attributes are used when extracting data.
;
;       Time-Dependent Distribtions:
;           'DEPEND_0' - Time variable
;           'DEPEND_1' - Azimuth angle bins variable
;           'DEPEND_2' - Energy bins variable
;
;       Time-Inependent Distribtions:
;           'DEPEND_0' - Azimuth angle bins variable
;           'DEPEND_1' - Energy bins variable
;
;       Energy:
;           'SI_CONVERSION'   - Conversion factor to SI units
;           'UNITS'           - Energy units. Used when SI_CONVERSION is not present
;           'DELTA_PLUS_VAR'  - Upper-bound of each energy bin. Converted to velocity delta.
;           'DELTA_PLUS'      - Uniform upper-bound of the energy bin (when DELTA_PLUS_VAR not present)
;           'DELTA_MINUS_VAR' - Lower-bound of each energy bin
;           'DELTA_MINUS'     - Uniform lower-bound of the energy bin (when DELTA_MINUS_VAR not present)
;
;       Phi:
;           'UNITS'           - Angle units. Used to convert to radians
;           'DELTA_PLUS_VAR'  - Upper-bound of each energy bin. Used to determin bin centers.
;           'DELTA_PLUS'      - Uniform upper-bound of the energy bin (when DELTA_PLUS_VAR not present)
;           'DELTA_MINUS_VAR' - Lower-bound of each energy bin
;           'DELTA_MINUS'     - Uniform lower-bound of the energy bin (when DELTA_MINUS_VAR not present)
;
;   Calling Sequence:
;       MrVar_Dist_Image(oX, oY, theDist, mass)
;       MrVar_Dist_Image(oX, oY, theDist, mass, idx)
;
; :Params:
;       oX:         out, required, type=objref
;                   A MrVariable containing the 1D cut along the X-axis (PHI=0,180).
;       oY:         out, required, type=objref
;                   A MrVariable containing the 1D cut along the X-axis (PHI=90,270).
;       THEDIST:    in, required, type=string/integer/objref
;                   A MrVariable object, name or index of distribution function
;                       to be displayed. Dimensions should be ordered as
;                       [time, azimuth, energy], with the time dimension being optional.
;                       Also must have corresponding DEPEND_[0-2] attributes.
;       MASS:       in, required, type=string/float
;                   Mass (kg) of the particles represented in the distribution. If a
;                       string, MrConstants('m_'+mass) will be used to obtain the mass.
;       IDX:        in, optional, type=integer, default=0
;                   If `THEDIST` is time-dependent, then this is the index along the
;                       time dimension of the distribution to be displayed.
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
;       2017/02/12  -   Written by Matthew Argall
;-
;*****************************************************************************************
;+
;
;-
PRO MrVar_Dist1D_CenterBins, oPhi
	Compile_Opt idl2
	On_Error, 2

;-------------------------------------------
; PHI Bin Centers //////////////////////////
;-------------------------------------------
	;
	; Assumes:
	;    - DELTA_(PLUS|MINUS)_VAR are arrays of the same size
	;    - DELTA_(PLUS|MINUS) are scalars
	;    - DELTA_MINUS(_VAR) is positive
	;    - All deltas have the same units as PHI
	;    - If no deltas are present, bin centers are assumed
	;

	;DELTA_PLUS
	IF oPhi -> HasAttr('DELTA_PLUS_VAR') THEN BEGIN
		dPlus = MrVar_Get(oPhi['DELTA_PLUS_VAR'])
	ENDIF ELSE IF oPhi -> HasAttr('DELTA_PLUS') THEN BEGIN
		dPlus = oPhi['DELTA_PLUS']
	ENDIF ELSE BEGIN
		dPlus = 0
	ENDELSE
	
	;DELTA_MINUS
	IF oPhi -> HasAttr('DELTA_MINUS_VAR') THEN BEGIN
		dMinus = MrVar_Get(oPhi['DELTA_MINUS_VAR'])
	ENDIF ELSE IF oPhi -> HasAttr('DELTA_MINUS') THEN BEGIN
		dMinus = oPhi['DELTA_MINUS']
	ENDIF ELSE BEGIN
		dMinus = 0
	ENDELSE

	;Center bins
	delta = (dPlus - dMinus) / 2.0
	oPhi -> SetData, oPhi['DATA'] + delta
	
	;Save deltas
	;   - If DELTA is a MrVariable, then it is a DELTA_(PLUS|MINUS)_VAR
	;   - Otherwise, it can be either a scalar or array
	;   - Deltas are always absolute deviations. Sign is implied with (PLUS|MINUS)
	IF Size(delta, /TNAME) NE 'OBJREF' THEN BEGIN
		IF MrIsA(delta, /SCALAR) THEN BEGIN
			oPhi['DELTA_PLUS']  = Abs(delta)
			oPhi['DELTA_MINUS'] = Abs(delta)
		ENDIF ELSE BEGIN
			delta = MrVariable( Abs(delta) )
			delta['UNITS'] = oPhi['UNITS']
			delta['TITLE'] = 'Half-width of angle bins.'
			oPhi['DELTA_PLUS_VAR']  = delta
			oPhi['DELTA_MINUS_VAR'] = delta
		ENDELSE
	ENDIF ELSE BEGIN
		oPhi['DELTA_PLUS_VAR']  = delta
		oPhi['DELTA_MINUS_VAR'] = delta
	ENDELSE 
END


;+
;
;-
PRO MrVar_Dist1D_Azimuth, oDist, oX, oY, $
CACHE=cache
	Compile_Opt idl2
	On_Error, 2

;-------------------------------------------
; Translate Angles /////////////////////////
;-------------------------------------------
	
	;Move from (-!pi, pi] to [0, 2*!pi)
	oPhi = oDist['DEPEND_1']
	iNeg = oPhi -> Where(0, /LESS, COUNT=nNeg)
	IF nNeg GT 0 THEN oPhi[iNeg] = 2*!pi + oPhi[iNeg]

;-------------------------------------------
; Find +/- X, Y ////////////////////////////
;-------------------------------------------
	;Get 0 +/- Delta
	;   - 0-Delta wraps to 360-Delta
	!Null = oPhi.Min(i0p, SUBSCRIPT_MAX=i0m)
	
	;Get 90 +/- Delta
	;   - Shift values by 270 to get the [0,360] wrapping effect
	!Null = Min( (oPhi['DATA'] + 3.0*!pi/2.0) MOD (2.0*!pi), i90p, SUBSCRIPT_MAX=i90m )
	
	;Get 180 +/- Delta
	;   - Shift values by 180 to get the [0,360] wrapping effect
	!Null = Min( (oPhi['DATA'] + !pi) MOD (2.0*!pi), i180m, SUBSCRIPT_MAX=i180p )
	
	;Get 270 +/- Delta
	;   - Shift values by 90 to get the [0,360] wrapping effect
	!Null = Min( (oPhi['DATA'] + !pi/2.0) MOD (2.0*!pi), i270p, SUBSCRIPT_MAX=i270m )
	
	;
	; Assume dPhi is uniform
	;

	;Average the two channels together
	xp = Mean( oDist[ *, [  i0p,   i0m] ], DIMENSION=2 )
	xm = Mean( oDist[ *, [i180p, i180m] ], DIMENSION=2 )
	yp = Mean( oDist[ *, [ i90p,  i90m] ], DIMENSION=2 )
	ym = Mean( oDist[ *, [i270p, i270m] ], DIMENSION=2 )

;-------------------------------------------
; Create Variables /////////////////////////
;-------------------------------------------
	
	;Reverse the order of the minus so the velcity maxima are at the edges
	oX = MrVariable( [Reverse(xm), !Values.F_NaN, xp], NAME = oDist.name + '_x' )
	oY = MrVariable( [Reverse(ym), !Values.F_NaN, yp], NAME = oDist.name + '_y' )
	
	;Vx
	;   - Set mirror velocities to -X
	oV_temp             = oDist['DEPEND_0']
	oV                  = MrVariable( [-Reverse(oV_temp['DATA']), 0, oV_temp['DATA']] )
	oV['TITLE']         = 'Velocity (km/s)'
	oV['UNITS']         = 'km/s'
	oV['SI_CONVERSION'] = '1e3>m/s'
	
	;X-Distribution
	oX['LOG']      = 1B
	oX['DEPEND_0'] = oV
	
	;Y-Distribution
	oY['LOG']      = 1B
	oY['DEPEND_0'] = oV

;-------------------------------------------
; Set Attributes ///////////////////////////
;-------------------------------------------
	
	;Distribution range
	iGT0 = oDist -> Where(0, /GREATER, COUNT=nGT0)
	IF nGT0 GT 0 THEN BEGIN
		range = [Min( oDist[iGT0], MAX=dmax ), dmax]
		oX['AXIS_RANGE'] = range
		oY['AXIS_RANGE'] = range
	ENDIF
	
	;Other attributes
	IF oDist -> HasAttr('UNITS') THEN BEGIN
		oDist -> CopyAttrTo, oX, 'UNITS'
		oDist -> CopyAttrTo, oY, 'UNITS'
	ENDIF
	IF oDist -> HasAttr('SI_CONVERSION') THEN BEGIN
		oDist -> CopyAttrTo, oX, 'SI_CONVERSION'
		oDist -> CopyAttrTo, oY, 'SI_CONVERSION'
	ENDIF

;-------------------------------------------
; Store Variables //////////////////////////
;-------------------------------------------
	IF Keyword_Set(cache) THEN BEGIN
		oX -> Cache
		oY -> Cache
	ENDIF
END


;+
;
;-
PRO MrVar_Dist1D_Polar, oDist, oX0, oX90, oX180, $
CACHE=cache
	Compile_Opt idl2
	On_Error, 2

;-------------------------------------------
; Translate Angles /////////////////////////
;-------------------------------------------
	
	;Move from (-!pi/2, pi/2] to [0, !pi)
	oPhi = oDist['DEPEND_1']
	iNeg = oPhi -> Where(0, /LESS, COUNT=nNeg)
	IF nNeg GT 0 THEN oPhi[iNeg] = !pi + oPhi[iNeg]

;-------------------------------------------
; Find +/- X, Y ////////////////////////////
;-------------------------------------------
	;Get 0 & 180 Bins
	!Null = oPhi.Min(i0, SUBSCRIPT_MAX=i180)
	
	;Get 90 +/- Delta
	;   - Shift values by 90 to get the [0,360] wrapping effect
	!Null = Min( (oPhi['DATA'] + !pi/2.0) MOD !pi, i90m, SUBSCRIPT_MAX=i90p )
	
	;
	; Assume dPhi is uniform
	;

	;Average the two perpendicular channels together
	x0   = oDist[ *, i0 ]
	x180 = oDist[ *, i180 ]
	x90  = Mean( oDist[ *, [i90m, i90p] ], DIMENSION=2 )

;-------------------------------------------
; Create Variables /////////////////////////
;-------------------------------------------
	
	;Reverse the order of the minus so the velcity maxima are at the edges
	oX0   = MrVariable( x0,   /NO_COPY, NAME = oDist.name + '_0' )
	oX90  = MrVariable( x90,  /NO_COPY, NAME = oDist.name + '_90' )
	oX180 = MrVariable( x180, /NO_COPY, NAME = oDist.name + '_180' )

;-------------------------------------------
; Set Attributes ///////////////////////////
;-------------------------------------------
	
	;DEPEND_0
	oX0['DEPEND_0']   = oDist['DEPEND_0']
	oX90['DEPEND_0']  = oDist['DEPEND_0']
	oX180['DEPEND_0'] = oDist['DEPEND_0']
	
	;LOG
	oX0['LOG']   = 1B
	oX90['LOG']  = 1B
	oX180['LOG'] = 1B
	
	;AXIS_RANGE
	iGT0 = oDist -> Where(0, /GREATER, COUNT=nGT0)
	IF nGT0 GT 0 THEN BEGIN
		range = [Min( oDist[iGT0], MAX=dmax ), dmax]
		oX0['AXIS_RANGE']   = range
		oX90['AXIS_RANGE']  = range
		oX180['AXIS_RANGE'] = range
	ENDIF
	
	;UNITS
	IF oDist -> HasAttr('UNITS') THEN BEGIN
		oDist -> CopyAttrTo, oX0, 'UNITS'
		oDist -> CopyAttrTo, oX90, 'UNITS'
		oDist -> CopyAttrTo, oX180, 'UNITS'
	ENDIF
	
	;SI_CONVERSION
	IF oDist -> HasAttr('SI_CONVERSION') THEN BEGIN
		oDist -> CopyAttrTo, oX0, 'SI_CONVERSION'
		oDist -> CopyAttrTo, oX90, 'SI_CONVERSION'
		oDist -> CopyAttrTo, oX180, 'SI_CONVERSION'
	ENDIF

;-------------------------------------------
; Store Variables //////////////////////////
;-------------------------------------------
	IF Keyword_Set(cache) THEN BEGIN
		oX0   -> Cache
		oX90  -> Cache
		oX180 -> Cache
	ENDIF
END


;+
;   Extract 1D cuts along the principle axes of a 2D distribution function. Data in the
;   two bins on either size of +X, -X, +Y, and -Y are averaged together to make the ouput
;   data. The following attributes are used when extracting data.
;
;   Calling Sequence:
;       MrVar_Dist1D_Prep, oX1, oX2, theDist, mass
;       MrVar_Dist1D_Prep, oX1, oX2, theDist, mass, idx
;       MrVar_Dist1D_Prep, oX1, oX2, oX3, theDist, mass, /POLAR
;       MrVar_Dist1D_Prep, oX1, oX2, oX3, theDist, mass, idx, /POLAR
;
; :Params:
;       oX1:        out, required, type=MrVariable objref
;                   1D cut of the distribution along PHI=0,360 (`POLAR`=0) or PHI=0 (`POLAR`=1)
;       oX2:        out, required, type=MrVariable objref
;                   1D cut of the distribution along PHI=90,270 (`POLAR`=0) or PHI=90 (`POLAR`=1)
;       oX3:        out, optional, type=MrVariable objref
;                   1D cut of the distribution along PHI=180 (`POLAR`=1)
;       DISTFN:     in, required, type=string/integer/objref
;                   A MrVariable object, name or index of distribution function
;                       to be displayed. Dimensions should be ordered as
;                       [time, azimuth, energy], with the time dimension being optional.
;                       Also must have corresponding DEPEND_[0-2] attributes.
;       MASS:       in, required, type=string/float
;                   Mass (kg) of the particles represented in the distribution. If a
;                       string, MrConstants(MASS) will be used to obtain the mass.
;       IDX:        in, optional, type=integer, default=0
;                   If `THEDIST` is time-dependent, then this is the index along the
;                       time dimension of the distribution to be displayed.
;-
PRO MrVar_Dist1D_Prep, oX1, oX2, oX3, distfn, mass, idx, $
CACHE=cache, $
POLAR=polar
	Compile_Opt idl2

	;Catch errors
	Catch, the_error
	IF the_error NE 0 THEN BEGIN
		Catch, /CANCEL
		MrPrintF, 'LogErr'
		RETURN
	ENDIF
	
	;Grab the distribution
	tf_cache = Keyword_Set(cache)
	tf_polar = Keyword_Set(polar)
	
	;Check order of inputs
	IF tf_polar THEN BEGIN
		theDist = distfn
		theMass = mass
		theIdx  = idx
	ENDIF ELSE BEGIN
		theDist = oX3
		theMass = distfn
		IF N_Elements(mass) GT 0 THEN theIdx = mass
	ENDELSE
	
	;Perform the necessary conversions
	;   - Pick a single distribution
	;   - Energy  --> Velocity
	;   - Degrees --> Radians
	oDist2D = MrVar_Dist2D_Prep(theDist, theMass, theIdx, ENERGY=tf_polar)

	;Center angular bins
	MrVar_Dist1D_CenterBins, oDist2D['DEPEND_1']
	
	;Extract data
	IF tf_polar $
		THEN MrVar_Dist1D_Polar, oDist2D, oX1, oX2, oX3, CACHE=cache $
		ELSE MrVar_Dist1D_Azimuth, oDist2D, oX1, oX2, CACHE=cache
END