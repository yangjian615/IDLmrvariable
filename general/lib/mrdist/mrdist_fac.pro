; docformat = 'rst'
;
; NAME:
;       MrDist_FAC
;
; PURPOSE:
;+
;   Transform a spherical coordinate grid into a cartesian coordinate grid.
;
; :Categories:
;   Distribution functions
;
; :Params:
;       B:              in, required, type=3xN or Nx3
;                       Three-component magnetic field
;       VEC:            in, optional, type=3xN or Nx3
;                       A vector that defines the perpendicular direction. If not provided,
;                           the resulting transformation will have
;                               z' = B               North
;                               y' = z' x [1,0,0]    Usually duskward
;                               x' = y' x z'         Usually sunward
;
; :Keywords:
;       DIMENSION:      in, optional, type=integer
;                       The time-dependent dimension. If zero the grid vectors are
;                           assumed to be time indepenent. If undefined, the dimension
;                           of size other than 3 will be chosen.
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
;       2016/03/09  -   Written by Matthew Argall
;-
function MrDist_FAC, b, vec, $
DIMENSION=dimension, $
TYPE=type
	compile_opt idl2
	on_error, 2
	
	;Defaults
	if n_elements(type) eq 0 then type = 'ExB'
	fac_type = n_elements(vec) eq 0 ? '' : strupcase(type)

	;Normalize the vectors
	b_hat = MrVector_Normalize(b)
	v_hat = MrVector_Normalize(vec)

;-----------------------------------------------------
; Electric Field \\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\
;-----------------------------------------------------
	if fac_type eq 'EXB' then begin
		;Parallel: B
		z_hat = temporary(b_hat)
		
		;Perp-1: ExB
		x_hat = MrVector_Cross(temporary(v_hat), z_hat)
		x_hat = MrVector_Normalize(x_hat)
		
		;Perp-2: Bx(ExB)
		y_hat = MrVector_Cross(z_hat, x_hat)

;-----------------------------------------------------
; Velocity \\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\
;-----------------------------------------------------
	endif else if fac_type eq 'VXB' then begin
		;Parallel: B
		z_hat = temporary(b_hat)
		
		;Perp-2: bxv = -vxb = e
		y_hat = MrVector_Cross(z_hat, temporary(v_hat))
		y_hat = MrVector_Normalize(y_hat)
		
		;Perp-1: (bxv)xb = exb
		x_hat = MrVector_Cross(y_hat, z_hat)

;-----------------------------------------------------
; Radial/Azimuthal \\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\
;-----------------------------------------------------
	endif else if fac_type eq 'RADAZ' then begin
		;Parallel: B
		z_hat = temporary(b_hat)
		
		;Perp2: bxr (azimuth)
		y_hat = MrVector_Cross(z_hat, temporary(v_hat))
		y_hat = MrVector_Normalize(y_hat)
		
		;Perp1: bx(bxr) (radial)
		x_hat = MrVector_Cross(y_hat, z_hat)

;-----------------------------------------------------
; X-Axis \\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\
;-----------------------------------------------------
	endif else if fac_type eq '' gt 0 then  begin
		;Parallel to B
		z_hat = temporary(b_hat)
		
		;Perp2: XxB
		y_hat = MrVector_Cross(z_hat, [1,0,0])
		y_hat = MrVector_Normalize(y_hat)
		
		;Perp1: Yx(XxB)
		x_hat = MrVector_Cross(y_hat, z_hat)

;-----------------------------------------------------
; Unknown \\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\
;-----------------------------------------------------
	endif else begin
		message, 'FAC TYPE not recognized: "' + type + '".'
	endelse

;-----------------------------------------------------
; Form Rotation Matrix \\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\
;-----------------------------------------------------
	;Where is the time dependence?
	;   - The not-3 dimension
	if n_elements(dimension) eq 0 then begin
		szB = size(b)
		if szB[0] eq 1 then begin
			dimension = 0
		endif else begin
			dimension = where(dims ne 3, count) + 1
			if n_elements(dims) eq 1 then dimension = 1
			if count eq 0 then begin
				MrPrintF, 'LogWarn', '3x3 array given. Assuming DIMENSION=2.'
				dimension = 2
			endif else begin
				dimension = dimension[0]
			endelse
		endelse
	endif

	;Time dimension first
	;   - [component, axis]
	;   - [time, component, axis]
	;   - [component, axis, time]
	if dimension eq 0 then begin
		T = [ [temporary(x_hat)], [temporary(y_hat)], [temporary(z_hat)] ]
	endif else if dimension eq 1 then begin
		T = [ [[temporary(x_hat)]], [[temporary(y_hat)]], [[temporary(z_hat)]] ]
	endif else begin
		T = [ [[temporary(x_hat)]], [[temporary(y_hat)]], [[temporary(z_hat)]] ]
		T = transpose( T, [0,2,1] )
	endelse

	;Return the matrix
	return, T
end