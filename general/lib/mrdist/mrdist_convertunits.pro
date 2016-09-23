; docformat = 'rst'
;
; NAME:
;       MrDist_ConvertUnits
;
; PURPOSE:
;+
;   Convert the distribution function from one set of units to another.
;
; :Categories:
;   Distribution functions
;
; :Params:
;       FLUX:           in, required, type=fltarr
;                       Distribution function for which to convert units
;       TO_UNITS:       in, required, type=string
;                       Convert to these units.  Options are::
;                           'ENERGY'      - eV
;                           'EFLUX'       - eV / cm^2 / s / sr / eV    or    keV / cm^2 / s / sr / keV
;                           'DIFF FLUX'   - # / cm^2 / s / sr / keV
;                           'PSD'         - s^2 / km^6
;                           'DF'          - s^2 / km^6
;       FROM_UNITS:     in, optional, type=string, default='PSD'
;                       Name of the units of `FLUX`.
;
; :Keywords:
;       SPECIES:        in, optional, type=string, default='e'
;                       Particle species measured in the distribution. Particle mass
;                           is taken into account when converting 'PSD' <-> {'EFLUX' | 'DIFF FLUX'}
;       ENERGY:         in, optional, type=float
;                       Energy bins at which the distribution is taken.
;
; :Returns:
;       NEW_FLUX:       Distribution with new units.
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
function MrDist_ConvertUnits, flux, to_units, from_units, $
SPECIES = species, $
ENERGY  = energy
	compile_opt idl2
	on_error, 2
	
	to   = strupcase(to_units)
	from = n_elements(from_units) eq 0 ? 'PSD' : strupcase(from_units)

	;Proton mass, kg
	m = 1.6726219e-27

	;Mass relative to the proton mass.
	case species of
	   'i':   A = 1        ; H+
	   'H':   A = 1        ; H+
	   'He':  A = 4        ; He+
	   'He2': A = 4        ; He++
	   'O':   A = 16       ; O+
	   'e':   A = 1./1836  ; e-
	endcase

	;Conversion factor from energy flux to phase space density
	;   - mass^2 * (sr / eV^2)
	eflux_to_psd = A^2 * 0.5447 * 1.0e6
	
	;Energy Flux
	if from eq 'EFLUX' then begin
		
		;Convert to:
		case to of
			'DIFF FLUX': new_flux = flux / energy*1e3
			'PSD':       new_flux = eflux_to_psd * flux / energy^2
			'DF':        new_flux = eflux_to_psd * flux / energy^2
			'EFLUX':     new_flux = flux
			else: message, 'Cannot convert from "' + from + '" to "' + to + '".'
		endcase
	
	;Differential flux
	endif else if from eq 'DIFF FLUX' then begin
		eflux = flux * energy * 1e-3
		
		;Convert to:
		case to of
			'EFLUX':     new_flux = eflux
			'PSD':       new_flux = eflux_to_psd * eflux / energy^2
			'DF':        new_flux = eflux_to_psd * eflux / energy^2
			'DIFF FLUX': new_flux = flux
			else: message, 'Cannot convert from "' + from + '" to "' + to + '".'
		endcase
	
	;Phase space density
	endif else if from eq 'PSD' || from eq 'DF' then begin
		eflux = flux * energy * 1.e-3
		
		;Convert to:
		case to of
			'EFLUX':     new_flux = flux / eflux_to_psd * energy^2
			'DIFF FLUX': new_flux = flux / eflux_to_psd * energy*1e3
			'PSD':       new_flux = flux
			'DF':        new_flux = flux
			else: message, 'Cannot convert from "' + from + '" to "' + to + '".'
		endcase
	
	;Invalid
	endif else begin
		message, 'Invalid FROM_UNIT: "' + from_unit + '".'
	endelse
	
	;Return results
	return, new_flux
end
