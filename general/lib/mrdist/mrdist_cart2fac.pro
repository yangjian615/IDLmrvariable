; docformat = 'rst'
;
; NAME:
;       MrDist_Cart2FAC
;
; PURPOSE:
;+
;   Transform a spherical coordinate grid into a cartesian coordinate grid.
;
; :Categories:
;   Distribution functions
;
; :Params:
;       T:              in, required, type=3x3xN or Nx3x3 float
;                       Transformation matrix from cartesian to field-aligned coordinates.
;       X:              in, required, type=PxT or PxTxt or txPxT float
;                       The x-coordinates to be rotated.
;       Y:              in, required, type=PxT or PxTxt or txPxT float
;                       The y-coordinates to be rotated.
;       Z:              in, required, type=PxT or PxTxt or txPxT float
;                       The z-coordinates to be rotated.
;       N:              out, required, type=PxT or PxTxt or txPxT float
;                       The resulting field-aligned component.
;       P:              out, required, type=PxT or PxTxt or txPxT float
;                       The resulting perp-1 component.
;       Q:              out, required, type=PxT or PxTxt or txPxT float
;                       The resulting perp-2 component.
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
;       2016/03/09  -   Written by Matthew Argall
;-
pro MrDist_Cart2FAC, T, x, y, z, n, p, q, $
ORDER=order
	compile_opt idl2
	on_error, 2
	
	;Defaults
	if n_elements(order) eq 0 then begin
		if size(x_grid, /N_DIMENSIONS) eq 2 $
			then order = 0 $
			else order = 1
	endif
	
	;Dimensions
	dims = size(x, /DIMENSIONS)

;-----------------------------------------------------
; Rotate to FAC \\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\
;-----------------------------------------------------
	if order eq 0 then begin
		n = T[0,0] * x + T[1,0] * y + T[2,0] * z
		p = T[0,1] * x + T[1,1] * y + T[2,1] * z
		q = T[0,2] * x + T[1,2] * y + T[2,2] * z

	;Time is first
	endif else if order eq 1 then begin
		nPhi   = dims[0]
		nTheta = dims[1]
		nTime  = dims[2]
		
		;Perp-1
		n = rebin(reform(T[0,0,*]), nPhi, nTheta, nTime) * x + $
		    rebin(reform(T[1,0,*]), nPhi, nTheta, nTime) * y + $
		    rebin(reform(T[2,0,*]), nPhi, nTheta, nTime) * z
		
		;Perp-2
		p = rebin(reform(T[0,1,*]), nPhi, nTheta, nTime) * x + $
		    rebin(reform(T[1,1,*]), nPhi, nTheta, nTime) * y + $
		    rebin(reform(T[2,1,*]), nPhi, nTheta, nTime) * z
		
		;Field-aligned
		q = rebin(reform(T[0,2,*]), nPhi, nTheta, nTime) * x + $
		    rebin(reform(T[1,2,*]), nPhi, nTheta, nTime) * y + $
		    rebin(reform(T[2,2,*]), nPhi, nTheta, nTime) * z
	
	;Time is last
	endif else if order eq 2 then begin
		nTime  = dims[0]
		nPhi   = dims[1]
		nTheta = dims[2]

		;Perp-1
		n = rebin(T[*,0,0], nTime, nPhi, nTheta) * x + $
		    rebin(T[*,1,0], nTime, nPhi, nTheta) * y + $
		    rebin(T[*,2,0], nTime, nPhi, nTheta) * z
		
		;Perp-2
		p = rebin(T[*,0,1], nTime, nPhi, nTheta) * x + $
		    rebin(T[*,1,1], nTime, nPhi, nTheta) * y + $
		    rebin(T[*,2,1], nTime, nPhi, nTheta) * z
		
		;Parallel
		q = rebin(T[*,0,2], nTime, nPhi, nTheta) * x + $
		    rebin(T[*,1,2], nTime, nPhi, nTheta) * y + $
		    rebin(T[*,2,2], nTime, nPhi, nTheta) * z
	endif else begin
		message, 'Invalid value for ORDER.'
	endelse

	;Renormalize
	mag = sqrt(n^2 + p^2 + q^2)
	n /= mag
	p /= mag
	q /= mag
end