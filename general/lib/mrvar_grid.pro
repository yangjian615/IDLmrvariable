; docformat = 'rst'
;
; NAME:
;       MrVar_Grid
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
;   Expand 2D polar axes locations into a full 2D polar grid.
;
; :Categories:
;   MrVariable
;
; :Params:
;       THETA:              in, required, type=string/integer/objref
;                           Name, number, or MrVariable/MrTimeSeries objref of a variable
;                               containing angles (degrees) of the polar axis.
;       PHI:                in, required, type=string/integer/objref
;                           Name, number, or MrVariable/MrTimeSeries objref of a variable
;                               containing angles (degrees) of the azimuthal axis.
;       OX1:                in, required, type=string/integer/objref
;                           Name, number, or MrVariable/MrTimeSeries objref of a variable
;                               containing polar angle (degrees) of each grid point.
;       OX2:                in, required, type=string/integer/objref
;                           Name, number, or MrVariable/MrTimeSeries objref of a variable
;                               containing azimuthal angle (degrees) of each grid point.
;
; :Keywords:
;       FAC:                in, optional, type=string, default='CROSSX'
;                           Name of the Field-Aligned Coordinate system to which the
;                               input coordinates are rotated. See MrVar_FAC for details.
;       ORIENTATION:        in, optional, type=integer, default=1
;                           Orientation of the spherical coordinate system. The default
;                               has polar angle measured down from +Z and azimuth
;                               counter clock-wise from +X.
;       PAR:                in, optional, type=string/integer/objref
;                           Name, number, for MrVector objref of the vector that defines
;                               the parallel direction. If the inputs are time dependent
;                               and PAR does not have identical time tags, it will be
;                               interpolated to the inputs.
;       PERP:               in, optional, type=string, default='CROSSX'
;                           Name, number, for MrVector objref of the vector that defines
;                               the perpendicular direction. If the inputs are time
;                               dependent and PAR does not have identical time tags, it
;                               will be interpolated to the inputs.
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
;       2016/12/13  -   Written by Matthew Argall
;-
PRO MrVar_Grid, theta, phi, oX1, oX2, $
FAC=fac, $
ORIENTATION=orientation, $
PAR=par, $
PERP=perp
	Compile_Opt idl2
	On_Error, 2
	
	;Default to standard spherical orientation
	IF N_Elements(orientation) EQ 0 THEN orientation = 1

	;Create a cartesian grid
	;   - Will ensure THETA and PHI are NxM arrays
	MrVar_Grid_MakeCart, phi, theta, oX1, oX2, oX3

;-----------------------------------------------------
; Rotate to FAC \\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\
;-----------------------------------------------------
	IF N_Elements(par) GT 0 THEN BEGIN
		;Get the parallel vector
		oPar = MrVar_Get(par)
		
		;Interpolate the parallel vector onto the MMS time tags
		IF Obj_IsA(oX1, 'MrTimeSeries') $
			THEN oPar_dist = oPar -> Interpol(oX1) $
			ELSE oPar_dist = oPar

		;Field-Aligned Coordinate System
		oT_FAC = MrVar_FAC(oPar_dist, fac, perp)

		;Rotate the cartesian grid to field-aligned coordinates
		MrVar_Grid_Cart2FAC, temporary(oT_FAC), -temporary(oX1), -temporary(oX2), -temporary(oX3), $
		                     oX1, oX2, oX3

		;Convert to spherical coordinates
		MrVar_Grid_cart2sphere, temporary(oX1), temporary(oX2), temporary(oX3), oX1, oX2, $
		                        ORIENTATION = orientation

;-----------------------------------------------------
; Keep Original Coordinates \\\\\\\\\\\\\\\\\\\\\\\\\\
;-----------------------------------------------------
	endif else begin
		;Convert to spherical coordinates
		MrVar_Grid_cart2sphere, temporary(oX1), temporary(oX2), temporary(oX3), oX1, oX2, $
		                        ORIENTATION = orientation
	endelse
end
