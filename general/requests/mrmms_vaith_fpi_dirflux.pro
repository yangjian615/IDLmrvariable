; docformat = 'rst'
;
; NAME:
;       MrMMS_Vaith_FPI_DirFlux
;
; PURPOSE:
;+
;   Compute directional fluxes.
;
; :Categories:
;       CDF Utilities
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
;       2016/08/22  -   Written by Matthew Argall
;-
FUNCTION MrMMS_Vaith_FPI_DirFlux, sc, mode, theta, species, $
ENERGY=energy, $
FAC=fac, $
NO_LOAD=no_load
	Compile_Opt idl2
	On_Error, 2
	
	Catch, the_error
	IF the_error NE 0 THEN BEGIN
		Catch, /CANCEL
		MrPrintF, 'LogErr'
		RETURN
	ENDIF
	
	;Inputs
	sc      = 'mms4'
	mode    = 'brst'
	theta   = [0, 20]
	species = 'e'
	energy  = 500
	fac     = 'CROSSX'     ;Field-aligned coordinate system: 'CROSSX', 'VXB', 'EXB'
	MrVar_SetTRange, ['2015-09-19T10:05:00', '2015-09-19T10:07:00']

;-------------------------------------------
; Information //////////////////////////////
;-------------------------------------------
	
	;Defaults
	tf_load = ~Keyword_Set(no_load)
	IF N_Elements(energy)  EQ 0 THEN energy = 500
	IF N_Elements(fac)     EQ 0 THEN fac    = 'CROSSX'
	IF N_Elements(energy)  NE 1 THEN Message, 'ENERGY must have 1 element.'
	IF N_Elements(species) EQ 0 THEN speces = 'e'
	IF N_Elements(theta)   EQ 0 THEN theta  = [80, 110]
	IF N_Elements(theta)   NE 2 THEN Message, 'THETA must have 2 elements: [min, max].'
	
	;Constants
	instr = 'd' + species + 's'
	level = 'l2'
	
	;Names of source data
	dist_vname      = StrJoin( [sc, instr, 'dist', 'fac', mode], '_' )
	flux1_0_vname   = StrJoin( [sc, 'edi', 'flux1',   '0', mode, level], '_' )
	flux2_0_vname   = StrJoin( [sc, 'edi', 'flux2',   '0', mode, level], '_' )
	flux3_0_vname   = StrJoin( [sc, 'edi', 'flux3',   '0', mode, level], '_' )
	flux4_0_vname   = StrJoin( [sc, 'edi', 'flux4',   '0', mode, level], '_' )
	flux1_180_vname = StrJoin( [sc, 'edi', 'flux1', '180', mode, level], '_' )
	flux2_180_vname = StrJoin( [sc, 'edi', 'flux2', '180', mode, level], '_' )
	flux3_180_vname = StrJoin( [sc, 'edi', 'flux3', '180', mode, level], '_' )
	flux4_180_vname = StrJoin( [sc, 'edi', 'flux4', '180', mode, level], '_' )
	
	;Names of derived data
	espec_vname    = StrJoin( [sc, instr, String(theta, FORMAT='(%"eflux_pa%0i-%i")'), mode], '_' )
	fpi_flux_vname = StrJoin( [sc, instr, String(energy, theta, FORMAT='(%"eflux_e%i_pa%i-%i")'), mode], '_' )

;-------------------------------------------
; Load Data ////////////////////////////////
;-------------------------------------------
	IF tf_load THEN BEGIN
		;FPI Distribution
		MrMMS_FPI_Load_Dist3D, sc, mode, species, fac
		
		;EDI Data
		MrMMS_Load_Data, sc, 'edi', mode, level, $
		                 OPTDESC   = 'amb' + ['', '-pm2', '-alt-cc', '-alt-oc', '-alt-oom', '-alt-ooc']
		                 VARFORMAT = ['*flux*', '*counts*']
	ENDIF
	
;-------------------------------------------
; Energy Spectra at Given Pitch Angle //////
;-------------------------------------------
	;Create a distribution object and convert units to number flux
	oDist4D  = MrDist4D( dist_vname )
	oDist4D -> ConvertUnits, 'EFLUX'
	
	;Create an energy spectra with specific pitch angles
	oESpec = oDist4D -> ESpec(/CACHE, NAME=flux_name, THETA_RANGE=theta)
	
;-------------------------------------------
; Extract Flux at Energy ///////////////////
;-------------------------------------------
	;Get the energy table
	e_table = oESpec['DEPEND_1']
	
	;Extract the desired energy or energies
	;   - Find nearest neighbor
	iE = Value_Locate(e_table[0,*], energy)
	iEnew = Where( Abs(e_table[0,iE+1] - energy) LT Abs(e_table[0,iE] - energy), nEnew )
	IF nEnew GT 0 THEN iE[iEnew] += 1
	
	;Flux variable
	oFlux = MrScalarTS( oESpec['TIMEVAR'], oESpec[*,iE], $
	                    /CACHE, $
	                    NAME = fpi_flux_vname )
	
	;Attributes
	oFlux['LOG']        = 1B
	oFlux['PLOT_TITLE'] = String(StrUpCase(instr), e_table[0,iE], theta, FORMAT='(%"%s Number Flux E=%ieV PA=%i-%i")')
	oFlux['TITLE']      = 'Flux!C(' + oESpec['UNITS'] + ')'
	oFlux['UNITS']      = oESpec['UNITS']
	
;-------------------------------------------
; EDI Attributes ///////////////////////////
;-------------------------------------------
	edi_vnames = [flux1_0_vname, flux2_0_vname, flux3_0_vname, flux4_0_vname, $
	              flux1_180_vname, flux2_180_vname, flux3_180_vname, flux4_180_vname]
	
	FOR i = 0, N_Elements(edi_vnames) - 1 DO BEGIN
		ediFlux = MrVar_Get(edi_vnames[i])
		ediFlux['COLOR'] = 'Blue'
	ENDFOR
	
;-------------------------------------------
; Plot /////////////////////////////////////
;-------------------------------------------
	win = MrVar_PlotTS( fpi_flux_vname, /NO_REFRESH )
	win = MrVar_OPlotTS( fpi_flux_vname, flux2_0_vname )
	
	win[0] -> SetLayout, [1,1]
	win -> TrimLayout
	win -> Refresh
	
	RETURN, win
END