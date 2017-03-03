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
;       ON:             out, required, type=MrVariable
;                       The resulting perp-1 component.
;       OP:             out, required, type=MrVariable
;                       The resulting perp-2 component.
;       OQ:             out, required, type=MrVariable
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
pro MrVar_Grid_Cart2FAC, T, x, y, z, oN, oP, oQ
	compile_opt idl2
	on_error, 2
	
	;Get/Verify Variables
	oT = MrVar_Get(T)
	oX = MrVar_Get(x)
	oY = MrVar_Get(y)
	oZ = MrVar_Get(z)
	if ~obj_isa(oT, 'MrMatrixTS') then message, 'T must be a MrTensorTS object.'
	
	;Size of the variables
	dimsT = size(oT, /DIMENSIONS)
	nDims = size(oX, /N_DIMENSIONS)
	dims  = size(oX, /DIMENSIONS)
	nTime  = dimsT[0]

;-----------------------------------------------------
; Rotate to FAC \\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\
;-----------------------------------------------------
	if nDims eq 2 then begin
		nPhi   = dims[0]
		nTheta = dims[1]
		
		;Temporarily reform data to vectorize multiplication
		x_temp = rebin(reform(oX['DATA'], 1, nPhi, nTheta), nTime, nPhi, nTheta)
		y_temp = rebin(reform(oY['DATA'], 1, nPhi, nTheta), nTime, nPhi, nTheta)
		z_temp = rebin(reform(oZ['DATA'], 1, nPhi, nTheta), nTime, nPhi, nTheta)
		
		;Perp-1
		n = rebin(oT[*,0,0], nTime, nPhi, nTheta) * x_temp + $
		    rebin(oT[*,1,0], nTime, nPhi, nTheta) * y_temp + $
		    rebin(oT[*,2,0], nTime, nPhi, nTheta) * z_temp
		
		;Perp-2
		p = rebin(oT[*,0,1], nTime, nPhi, nTheta) * x_temp + $
		    rebin(oT[*,1,1], nTime, nPhi, nTheta) * y_temp + $
		    rebin(oT[*,2,1], nTime, nPhi, nTheta) * z_temp
		
		;Par
		q = rebin(oT[*,0,2], nTime, nPhi, nTheta) * temporary(x_temp) + $
		    rebin(oT[*,1,2], nTime, nPhi, nTheta) * temporary(y_temp) + $
		    rebin(oT[*,2,2], nTime, nPhi, nTheta) * temporary(z_temp)

		;Turn into time-series variables
		oN = MrTimeSeries( oT['TIMEVAR'], n, /NO_COPY )
		oP = MrTimeSeries( oT['TIMEVAR'], p, /NO_COPY )
		oQ = MrTimeSeries( oT['TIMEVAR'], q, /NO_COPY )

	;Time is first
	endif else begin
		nPhi   = dims[1]
		nTheta = dims[2]
		
		;Perp-1
		oN = rebin( oT[*,0,0], nTime, nPhi, nTheta) * oX + $
		     rebin( oT[*,1,0], nTime, nPhi, nTheta) * oY + $
		     rebin( oT[*,2,0], nTime, nPhi, nTheta) * oZ
		
		;Perp-2
		oP = rebin( oT[*,0,1], nTime, nPhi, nTheta) * oX + $
		     rebin( oT[*,1,1], nTime, nPhi, nTheta) * oY + $
		     rebin( oT[*,2,1], nTime, nPhi, nTheta) * oZ
		
		;Field-aligned
		oQ = rebin( oT[*,0,2], nTime, nPhi, nTheta) * oX + $
		     rebin( oT[*,1,2], nTime, nPhi, nTheta) * oY + $
		     rebin( oT[*,2,2], nTime, nPhi, nTheta) * oZ
	endelse

	;Renormalize
	mag = MrTimeSeries( oT['TIMEVAR'], sqrt( oN['DATA']^2 + oP['DATA']^2 + oQ['DATA']^2 ) )
	oN /= mag
	oP /= mag
	oQ /= temporary(mag)
end