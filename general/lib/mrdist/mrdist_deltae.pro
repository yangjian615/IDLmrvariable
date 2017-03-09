; docformat = 'rst'
;
; NAME:
;    MrDist_DeltaE
;
; PURPOSE:
;+
;   Find the spacing of energy bins assuming constant deltaE/E
;
; :Categories:
;    distribution functions
;
; :Params:
;       ENERGY_BINS:        in, required, type=Mx1 or NxM fltarr
;                           Energies spaced at constant dE/E. If time-dependent, the
;                               time-varying dimension is assumed to be first.
;
; :Returns:
;       DELTAE:             Spacing of the energy bins.
;
; :Author:
;    Matthew Argall::
;    University of New Hampshire
;    Morse Hall Room 348
;    8 College Road
;    Durham, NH 03824
;    matthew.argall@unh.edu
;
; :History:
;    Modification History::
;       2016/02/10  -   Written by Matthew Argall
;       2016/09/03  -   ENERGY_BINS can be time-dependent. - MRA
;-
;*****************************************************************************************
function MrDist_DeltaE, energy_bins
	compile_opt idl2
	on_error, 2

	;
	; dLogE is the spacing of the energy bins in log space
	;
	;   dLogE = log(E1) - log(E0)                      (1)
	;
	; dE/E is just the derivative of a natural log
	;
	;   d( ln(E) ) = dE / E                            (2)
	;
	; If we change bases by log_a(B) = log_x(B) / log_x(A), where x=10, A=exp, B=E
	;
	;   d( ln(E) ) = d [ log(E) / log(exp) ]
	;              = dLogE / log(exp)                  (3)
	;
	; Combining Equations 1 and 2
	;
	;   dE/E = dLogE / log(exp)
	;
	;     dE = E * dLogE / log(exp)
	;
	
	;Do the energy bins change in time?
	;   - Assume time is first dimension
	if size(energy_bins, /N_DIMENSIONS) eq 2 then begin
		dims   = size(energy_bins, /DIMENSIONS)
		dLogE  = alog10(energy_bins[*,1]) - alog10(energy_bins[*,0])
		deltaE = energy_bins * rebin(dLogE, dims[0], dims[1]) / alog10( exp(1) )
	
	;Time-independent energy bins
	endif else begin
		dLogE  = alog10(energy_bins[1]) - alog10(energy_bins[0])
		deltaE = energy_bins * dLogE / alog10( exp(1) )
	endelse
	
	;Return results
	return, deltaE
end