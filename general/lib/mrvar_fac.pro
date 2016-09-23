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
;   Transform a spherical coordinate grid into a cartesian coordinate grid.
;
; :Categories:
;   Coordinate Systems
;
; :Params:
;       B:              in, required, type=string/integer/object
;                       Name, index, or objref of a MrVectorTS variable containing the
;                           three-component magnetic field.
;       VEC:            in, optional, type=object (MrVectorTS)
;                       Name, index or objref of a MrVectorTS variable that defines the
;                           perpendicular direction. If not provided, `TYPE` is set to ''.
;
; :Keywords:
;       TYPE:           in, optional, type=boolean
;                       The type of FAC coordinates to use. If `VEC` is not defined,
;                           then TYPE is set to the empty string automatically. Otherwise,
;                           'EXB' is the default. Choices are::
;                               'EXB' - `VEC` is assumed to be the electric field
;                                         z' = B                Magnetic field direction
;                                         x' = E x B            Drift velocity direction
;                                         y' = B x (E x B)      Electric field direction
;                               'VXB' - `VEC` is assumed to be the plasma velocity
;                                         z' = B                Magnetic field direction
;                                         x' = E x B            Bulk velocity direction
;                                         y' = B x V            Electric field direction
;                               'RADAZ' - `VEC` is assumed to be the radial position vector
;                                         z' = B                Magnetic field direction
;                                         x' = B x R            Azimuthal (counter clockwise)
;                                         y' = B x (B x R)      Radial outward
;                               'XAXIS' or '' - `VEC` is ignored.
;                                         z' = B               Magnetic field direction
;                                         y' = z' x [1,0,0]    Azimuthal direction
;                                         x' = y' x z'         Radial direction
;
; :Returns:
;       T:              The transformation matrix to the field-aligned coordinate system.
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
;-
function MrVar_FAC, b, v, $
TYPE=type
	compile_opt idl2
	on_error, 2
	
	;Defaults
	if n_elements(type) eq 0 then type = ''
	fac_type = n_elements(type) eq 0 ? '' : strupcase(type)
	
	;Verify and normalize vector1
	oB = MrVar_Get(b)
	if ~isa(oB, 'MrVectorTS') then message, 'B must be a MrVectorTS object.'
	oB_hat = oB -> Normalize()
	
	;Do the same for vector2 if required
	if type ne '' then begin
		oV = MrVar_Get(v)
		if ~isa(oV, 'MrVectorTS') then message, 'VEC must be a MrVectorTS object.'
		oV_hat = oV -> Normalize()
	endif

;-----------------------------------------------------
; Electric Field \\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\
;-----------------------------------------------------
	if fac_type eq 'EXB' then begin
		;Parallel: B
		;   - z_hat = b_hat
		
		;Perp-1: ExB
		oX_hat = oV_hat -> Cross(oB_hat)
		oX_hat = oX_hat -> Normalize()
		
		;Perp-2: Bx(ExB)
		oY_hat = oB_hat -> Cross(oX_hat)

;-----------------------------------------------------
; Velocity \\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\
;-----------------------------------------------------
	endif else if fac_type eq 'VXB' then begin
		;Parallel: B
		;   - oZ_hat = oB_hat
		
		;Perp-2: bxv = -vxb = e
		oY_hat = oB_hat -> Cross(oV_hat)
		oY_hat = oY_hat -> Normalize()
		
		;Perp-1: (bxv)xb = exb
		oX_hat = oY_hat -> Cross(oB_hat)

;-----------------------------------------------------
; Radial/Azimuthal \\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\
;-----------------------------------------------------
	endif else if fac_type eq 'RADAZ' then begin
		;Parallel: B
		;   - oZ_hat = oB_hat
		
		;Perp2: bxr (azimuth)
		oY_hat = oB_hat -> Cross(oV_hat)
		oY_hat = oY_hat -> Normalize()
		
		;Perp1: bx(bxr) (radial)
		oX_hat = oY_hat -> Cross(oB_hat)

;-----------------------------------------------------
; X-Axis \\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\
;-----------------------------------------------------
	endif else if fac_type eq '' gt 0 then  begin
		;Parallel to B
		;   - oZ_hat = oB_hat
		
		;Perp2: XxB
		oY_hat = oZ_hat -> Cross([1,0,0])
		oY_hat = oY_hat -> Normalize()
		
		;Perp1: Yx(XxB)
		oX_hat = oY_hat -> Cross(oB_hat)

;-----------------------------------------------------
; Unknown \\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\
;-----------------------------------------------------
	endif else begin
		message, 'FAC TYPE not recognized: "' + type + '".'
	endelse

;-----------------------------------------------------
; Form Rotation Matrix \\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\
;-----------------------------------------------------

	;Time dimension first
	;   - [time, component, axis]
	T = MrMatrixTS([ [[ oX_hat['DATA'] ]], [[ oY_hat['DATA'] ]], [[ oB_hat['DATA'] ]] ])

	;Return the matrix
	return, T
end