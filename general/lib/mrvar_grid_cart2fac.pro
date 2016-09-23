; docformat = 'rst'
;
; NAME:
;       MrVar_Grid_Cart2FAC
;
; PURPOSE:
;+
;   Transform a cartesian coordinate grid into field-aligned coordinates.
;
; :Categories:
;   Distribution functions
;
; :Params:
;       T:              in, required, type=name/index/MrTensorTS
;                       Transformation matrix from cartesian to field-aligned coordinates.
;       X:              in, required, type=name/index/MrVariable
;                       The x-coordinates to be rotated.
;       Y:              in, required, type=name/index/MrVariable
;                       The y-coordinates to be rotated.
;       Z:              in, required, type=name/index/MrVariable
;                       The z-coordinates to be rotated.
;       N:              out, required, type=MrVariable
;                       The resulting perp-1 component.
;       P:              out, required, type=MrVariable
;                       The resulting perp-2 component.
;       Q:              out, required, type=MrVariable
;                       The resulting field-aligned component.
;
; :Keywords:
;       ORDER:          in, optional, type=integer
;                       Indicate how the dimensions are ordered. Options are:
;                           0: [phi, theta]
;                           1: [phi, theta, time]
;                           2: [time, phi, theta]
;                       By default, 0 is chosen for 2D arrays, and 1 is  chosen
;                           for 3D arrays.
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
;       2016/08/21  -   Written by Matthew Argall
;-
pro MrVar_Grid_Cart2FAC, T, x, y, z, n, p, q
	compile_opt idl2
	on_error, 2
	
	;Get/Verify Variables
	oT = MrVar_Get(T)
	oX = MrVar_Get(x)
	oY = MrVar_Get(y)
	oZ = MrVar_Get(z)
	if ~obj_isa(oT, 'MrMatrixTS') then message, 'T must be a MrTensorTS object.'
	
	;Size of the variables
	nDims = size(oX, /N_DIMENSIONS)
	
;-----------------------------------------------------
; Rotate to FAC \\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\
;-----------------------------------------------------
	if nDims eq 2 then begin
		n = oT[0,0] * oX + oT[1,0] * oY + oT[2,0] * oZ    ;Perp-1
		p = oT[0,1] * oX + oT[1,1] * oY + oT[2,1] * oZ    ;Perp-2
		q = oT[0,2] * oX + oT[1,2] * oY + oT[2,2] * oZ    ;Field-aligned

	;Time is first
	endif else begin
		dims   = size(oX, /DIMENSIONS)
		nTime  = dims[0]
		nPhi   = dims[1]
		nTheta = dims[2]
		
		;Perp-1
		n = rebin(reform(oT[*,0,0]), nTime, nPhi, nTheta) * oX + $
		    rebin(reform(oT[*,1,0]), nTime, nPhi, nTheta) * oY + $
		    rebin(reform(oT[*,2,0]), nTime, nPhi, nTheta) * oZ
		
		;Perp-2
		p = rebin(reform(oT[*,0,1]), nTime, nPhi, nTheta) * oX + $
		    rebin(reform(oT[*,1,1]), nTime, nPhi, nTheta) * oY + $
		    rebin(reform(oT[*,2,1]), nTime, nPhi, nTheta) * oZ
		
		;Field-aligned
		q = rebin(reform(oT[*,0,2]), nTime, nPhi, nTheta) * oX + $
		    rebin(reform(oT[*,1,2]), nTime, nPhi, nTheta) * oY + $
		    rebin(reform(oT[*,2,2]), nTime, nPhi, nTheta) * oZ
	endelse
	
	;Renormalize
	mag = sqrt( n['DATA']^2 + p['DATA']^2 + q['DATA']^2 )
	n /= mag
	p /= mag
	q /= temporary(mag)
end