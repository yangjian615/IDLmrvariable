; docformat = 'rst'
;
; NAME:
;       MrVar_FAC
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
;   Create a matrix that transforms vectors into a field-aligned coordinate system.
;
;   Calling Sequence
;       oT = MrVar_FAC(par)
;       oT = MrVar_FAC(par, perp, fac)
;
; :Categories:
;   Coordinate Systems
;
; :Params:
;       PAR:            in, required, type=string/integer/object
;                       Name, index, or objref of a MrVectorTS variable containing the
;                           three-component magnetic field.
;       PERP:           in, optional, type=object (MrVectorTS)
;                       Name, index or objref of a MrVectorTS variable that defines the
;                           perpendicular direction. If PERP and `PAR` do not have identical
;                           time tags, PERP is interpolated onto `PAR`.
;       FAC:            in, optional, type=boolean, default='CROSSX'
;                       The type of FAC coordinates to use. Choices are::
;                               'EXB' - `PAR` = B-Field, `PERP` = E-Field
;                                         z' = B                Magnetic field direction
;                                         x' = E x B            Drift velocity direction
;                                         y' = B x (E x B)      Electric field direction
;                               'VXB' - `PAR` = B-Field, `PERP` = Plasma Bulk Velocity
;                                         z' = B                Magnetic field direction
;                                         y' = B x V            Electric field direction
;                                         x' = (B x V) x B      Bulk velocity direction
;                               'RADAZ' - `PAR` = B-Field, `PERP` = Position Vector in Geocentric CS
;                                         z' = B                Magnetic field direction
;                                         x' = B x R            Azimuthal (counter clockwise)
;                                         y' = B x (B x R)      Radial outward
;                               'CROSSX' - `PAR` = Anything, `PERP` is ignored.
;                                         z' = `PAR`           Parallel direction
;                                         y' = z' x [1,0,0]    Perp2 direction
;                                         x' = y' x z'         Perp1 direction
;                               '' - `PAR` = Anything, `VEC` = Anything.
;                                         z' = PAR             Parallel direction
;                                         y' = PARxPERP        Per2 direction
;                                         x' = (PARxPERP)xPAR  Perp1 direction
;
; :Keywords:
;       AXLABELS:       out, optional, type=strarr(3)
;                       Strings indicating how the X, Y, and Z unit vectors in the output
;                           coordinate system were calculated.
;       TIME:           in, required, type=string/integer/object
;                       Name, index, or objref of a MrTimeVar or MrTimeSeries variable.
;                           `PAR` and `PERP` are interpolated to the time tags in TIME.
;                           If not given, `PERP` is interpolated to `PAR`.
;
; :Returns:
;       T:               out, required, type=object (MrMatrixTS)
;                        The transformation matrix to the field-aligned coordinate system.
;
; :Author:
;       Matthew Argall::
;       University of New Hampshire
;       Morse Hall, Room 348
;       8 College Rd.
;       Durham, NH, 03824
;       matthew.argall@unh.edu
;
; :History:
;   Modification History::
;       2016/06/30  -   Written by Matthew Argall
;       2016/08/26  -   MrVariable names or indices can be given. - MRA
;       2016/12/09  -   Interplate V onto B if they do not have the same time tags.
;                           Removed TYPE keyword, added FAC parameter. - MRA
;       2017/01/22  -   Switched order of `PERP` and `FAC` params. Added `TIME` keyword. - MRA
;       2017/03/09  -   'RADAZ' system now correct. Added AXLABEL keyword. - MRA
;-
FUNCTION MrVar_FAC, par, perp, fac, $
AXLABELS=axlabels, $
TIME=time
	Compile_Opt idl2
	On_Error, 2
	
	;Defaults
	theFAC = N_Elements(fac) EQ 0 ? 'CROSSX' : StrUpCase(fac)
	
	;PAR
	oPar = MrVar_Get(par)
	IF ~IsA(oPar, 'MrVectorTS') THEN Message, 'PAR must be a MrVectorTS object.'
	
	;PERP
	IF N_Elements(perp) EQ 0 THEN BEGIN
		theFAC = 'CROSSX'
	ENDIF ELSE BEGIN
		oPerp = MrVar_Get(perp)
		IF ~IsA(oPerp, 'MrVectorTS') THEN Message, 'PERP must be a MrVectorTS object.'
		theFAC = N_Elements(fac) EQ 0 ? '' : StrUpCase(fac)
	ENDELSE

;-----------------------------------------------------
; Base Vectors \\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\
;-----------------------------------------------------
	
	;TIME
	IF N_Elements(time) EQ 0 THEN time = oPar['TIMEVAR']
	oTime = MrVar_Get(time)
	
	;Interpolate parallel vector
	IF ~oPar -> IsTimeIdentical(oTime) THEN oPar = oPar -> Interpol(oTime)
	
	;Interpolate perpendicular vector
	IF theFAC NE 'CROSSX' THEN BEGIN
		IF ~oPerp -> IsTimeIdentical(oTime) THEN oPerp = oPerp -> Interpol(oTime)
		oPerp_hat = oPerp -> Normalize()
	ENDIF
	
	;Parallel unit vector
	oZ_hat = oPar -> Normalize()

;-----------------------------------------------------
; Electric Field \\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\
;-----------------------------------------------------
	IF theFAC EQ 'EXB' THEN BEGIN
		;Parallel: B
		;   - z_hat = b_hat
		
		;Perp-1: ExB
		oX_hat = oPerp_hat -> Cross(oZ_hat)
		oX_hat = oX_hat -> Normalize()
		
		;Perp-2: Bx(ExB)
		oY_hat = oZ_hat -> Cross(oX_hat)
		
		;Axis labels
		axlabels = ['ExB', 'Bx(ExB)', 'B']

;-----------------------------------------------------
; Velocity \\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\
;-----------------------------------------------------
	ENDIF ELSE IF theFAC EQ 'VXB' THEN BEGIN
		;Parallel: B
		;   - oZ_hat = oB_hat
		
		;Perp-2: bxv = -vxb = e
		oY_hat = oZ_hat -> Cross(oPerp_hat)
		oY_hat = oY_hat -> Normalize()
		
		;Perp-1: (bxv)xb = exb
		oX_hat = oY_hat -> Cross(oZ_hat)
		
		;Axis labels
		axlabels = ['(BxV)xB', 'BxV', 'B']

;-----------------------------------------------------
; Radial/Azimuthal \\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\
;-----------------------------------------------------
	ENDIF ELSE IF theFAC EQ 'RADAZ' THEN BEGIN
		;Parallel: B
		;   - oZ_hat = oB_hat
		
		;Perp2: bxr (azimuth)
		oX_hat = oZ_hat -> Cross(oPerp_hat)
		oX_hat = oY_hat -> Normalize()
		
		;Perp1: bx(bxr) (radial)
		oY_hat = oZ_hat -> Cross(oX_hat)
		
		;Axis labels
		axlabels = ['BxR', 'Bx(BxR)', 'B']

;-----------------------------------------------------
; X-Axis \\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\
;-----------------------------------------------------
	ENDIF ELSE IF theFAC EQ 'CROSSX' GT 0 THEN BEGIN
		;Parallel to B
		;   - oZ_hat = oB_hat
		
		;Perp2: XxB
		oY_hat = oZ_hat -> Cross([1,0,0])
		oY_hat = oY_hat -> Normalize()
		
		;Perp1: Yx(XxB)
		oX_hat = oY_hat -> Cross(oZ_hat)
		
		;Axis labels
		axlabels = ['(BxX)xB', 'BxX', 'B']

;-----------------------------------------------------
; Custom \\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\
;-----------------------------------------------------
	ENDIF ELSE IF theFAC EQ '' THEN BEGIN
		;Perp2 = Par x Perp
		oY_hat = oZ_hat -> Cross(oPerp_hat)
		oY_hat = oY_hat -> Normalize()
		
		;Perp1 = Yx(PARxPERP)
		oX_hat = oY_hat -> Cross(oPar_hat)
		
		;Axis labels
		axlabels = ['(PARxPERP)xPAR', 'PARxPERP', 'PAR']
		
;-----------------------------------------------------
; Unknown \\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\
;-----------------------------------------------------
	ENDIF ELSE BEGIN
		Message, 'FAC not recognized: "' + theFAC + '".'
	ENDELSE

;-----------------------------------------------------
; Form Rotation Matrix \\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\
;-----------------------------------------------------

	;Time dimension first
	;   - [time, component, axis]
	T = MrMatrixTS( oPar['TIMEVAR'], [ [[ oX_hat['DATA'] ]], [[ oY_hat['DATA'] ]], [[ oZ_hat['DATA'] ]] ])

	;RETURN the matrix
	RETURN, T
END