; docformat = 'rst'
;
; NAME:
;    test_MrDist_1D
;
; PURPOSE:
;+
;   Compare UNH-made gyrophase and pitch angle distributions and energy spectrograms
;   with those of FPI.
;
; :Categories:
;    MMS, SPEDAS
;
; :Examples:
;   To use::
;       IDL> .r test_MrDist_1D
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
;-
function MrMMS_FPI_Test_Dist, sc, mode, species, $
NO_LOAD=no_load
	compile_opt idl2
	
	catch, the_error
	if the_error ne 0 then begin
		catch, /CANCEL
		MrPrintF, 'LogErr'
		return, obj_new()
	endif
	
;	MrMMS_SetTRange, ['2015-10-16T13:06:40', '2015-10-16T13:07:20']
;	sc   = 'mms2'
;	mode = 'brst'
	
	MrVar_SetTRange, ['2015-10-16T08:00:00', '2015-10-16T14:00:00']
	sc      = 'mms2'
	mode    = 'fast'
	species = 'e'
	instr   = 'd' + species + 's'
	tf_load = ~keyword_set(no_load)

;-------------------------------------------
; Load Data ////////////////////////////////
;-------------------------------------------
	if tf_load then begin
		;Load FPI data
		;   - Omni-directional Energy spectrogram
		;   - [0, 90, 180] degree spectrograms
		;   - Pitch angle distribution
		MrMMS_FPI_Load_Data, sc, mode, $
		                     OPTDESC   = 'd' + species + 's-moms', $
		                     TEAM_SITE = team_site, $
		                     VARFORMAT = ['*energyspectr*', 'pitchangdist']
		
		;Set names and cache
		theDist  = MrMMS_FPI_Dist4D(sc, mode, species, /CACHE)
		theDist -> Load_FAC, sc, 'srvy', species
	endif

;-------------------------------------------
; Load Data ////////////////////////////////
;-------------------------------------------
	;Get data
	distName   = strjoin([sc, instr, 'dist4d',   'l2', mode], '_')
	eSpecName  = strjoin([sc, instr, 'espectra', 'l2', mode], '_')
	paSpecName = strjoin([sc, instr, 'pad',      'l2', mode], '_')
stop
	
	
	;Create the energy and pitch angle spectra
	oDist    = MrVar_Get(distName)
	eSpectra = oDist -> ESpec()
	pad      = oDist -> ThetaSpec()
stop



	win -> Refresh
end