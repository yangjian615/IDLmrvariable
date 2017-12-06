; docformat = 'rst'
;
; NAME:
;       MrMMS_FEEPS_Load_Data
;
;*****************************************************************************************
;   Copyright (c) 2017, Matthew Argall                                                   ;
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
;   Generate a plot of EPD-EIS ElectronEnergy quantities:
;       1. FGM Bxyz
;       2. Integraged flux for Telescopes 0-5
;       3. Energy-time spectrogram for T0
;       4. Energy-time spectrogram for T1
;       5. Energy-time spectrogram for T2
;       6. Energy-time spectrogram for T3
;       7. Energy-time spectrogram for T4
;       8. Energy-time spectrogram for T5
;
; :Categories:
;   MMS
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
;       2017/03/25  -   Written by Matthew Argall
;-
;*****************************************************************************************
;+
;       Apply flat field correction factors to FEEPS ion/electron data;
;       correct factors are from the gain factor found in:
;       
;           FlatFieldResults_V3.xlsx
;           
;       from Drew Turner, 1/19/2017
;
; NOTES:
; 
;   From Drew Turner, 1/18/17:
;       Here are the correction factors that we need to apply to the current 
;       ION counts/rates/fluxes in the CDF files.  
;       NOTE, THIS IS A DIFFERENT TYPE OF CORRECTION THAN THAT FOR THE ELECTRONS!  
;       These shifts should be applied to the counts/rates/fluxes data EYE-BY-EYE on each spacecraft.  
;       These are multiplication factors (i.e., Jnew = Jold * Gcorr). 
;       For those equations, Jold is the original count/rate/flux array and
;       Jnew is the corrected version of the arrays using the factors listed below.
;
; NOTES:
;     BAD EYES are replaced by NaNs
;
;     See the spedas distribution
;         idl/projects/mms/feeps/mms_feeps_flat_field_corrections
;-
PRO MrMMS_FEEPS_Load_Data_Bad_Eyes, theVar
	Compile_Opt idl2
	On_Error, 2
	
	;Which spacecraft and eye?
	parts   = StRegEx(theVar.name, '^(mms[1-4]).*(electron|ion).*(top|bottom).*sensorid_([1-9]|1[0-2])', /SUBEXP, /EXTRACT)
	sc      = parts[1]
	species = parts[2]
	eye     = parts[3]
	sensor  = Fix(parts[4])
	
	;
	; Eyes that are 100% bad
	;
	
	ebad                = hash()
;	ebad['mms1-top']    = []
	ebad['mms1-bottom'] = [1]
	ebad['mms2-top']    = [5, 12]
;	ebad['mms2-bottom'] = []
	ebad['mms3-top']    = [2, 12]
	ebad['mms3-bottom'] = [2, 5, 11]
	ebad['mms4-top']    = [1, 2]
	ebad['mms4-bottom'] = [2, 4, 5, 10, 11]
	
	ibad                = hash()
;	ibad['mms1-top']    = []
;	ibad['mms1-bottom'] = []
	ibad['mms2-top']    = [7]
	ibad['mms2-bottom'] = [7]
;	ibad['mms3-top']    = []
;	ibad['mms3-bottom'] = []
	ibad['mms4-top']    = [7]
;	ibad['mms4-bottom'] = []
	
	;Get the energy table
	key = sc + '-' + eye
	
	;Ions
	IF species EQ 'ion' THEN BEGIN
		IF ibad -> HasKey(key) THEN BEGIN
			IF ~Array_Equal(ibad[key] EQ sensor, 0) $
				THEN theVar[*] = !Values.F_NaN
		ENDIF
	
	;Electrons
	ENDIF ELSE BEGIN
		IF ebad -> HasKey(key) THEN BEGIN
			IF ~Array_Equal(ebad[key] EQ sensor, 0) $
				THEN theVar[*] = !Values.F_NaN
		ENDIF
	ENDELSE
	
	
	;
	; Eyes for which the first energy channel is bad
	;
	
	ebad             = hash()
;	ebad['mms1-top'] = []
	ebad['mms1-bot'] = [2, 3, 4, 5, 9, 11, 12] ;11
	ebad['mms2-top'] = [2, 3, 4, 10, 12]
	ebad['mms2-bot'] = [1, 2, 3, 4, 5, 9, 10, 11, 12]
	ebad['mms3-top'] = [4, 5, 9, 10]
	ebad['mms3-bot'] = [1, 4, 9, 10, 11, 12]
	ebad['mms4-top'] = [3, 5, 9, 12]
	ebad['mms4-bot'] = [1, 3, 9, 12 ]
	
	ibad             = hash()
	ibad['mms1-top'] = [6]
	ibad['mms1-bot'] = [7, 8]
	ibad['mms2-top'] = [8] ;8
	ibad['mms2-bot'] = [6, 8, 12] ;12
	ibad['mms3-top'] = [2, 6, 7] ;2
	ibad['mms3-bot'] = [6, 7]
;	ibad['mms4-top'] = []
	ibad['mms4-bot'] = [6, 7] ;6
	
	;Ions
	IF species EQ 'ion' THEN BEGIN
		IF ibad -> HasKey(key) THEN BEGIN
			IF ~Array_Equal(ibad[key] EQ sensor, 0) $
				THEN theVar[*,0] = !Values.F_NaN
		ENDIF
	
	;Electrons
	ENDIF ELSE BEGIN
		IF ebad -> HasKey(key) THEN BEGIN
			IF ~Array_Equal(ebad[key] EQ sensor, 0) $
				THEN theVar[*,0] = !Values.F_NaN
		ENDIF
	ENDELSE
	
	
	;
	; Eyes for which the second energy channel is bad
	;
	
	ebad             = hash()
;	ebad['mms1-top'] = []
	ebad['mms1-bot'] = [11]
;	ebad['mms2-top'] = []
;	ebad['mms2-bot'] = []
;	ebad['mms3-top'] = []
;	ebad['mms3-bot'] = []
;	ebad['mms4-top'] = []
;	ebad['mms4-bot'] = []
	
	ibad             = hash()
;	ibad['mms1-top'] = []
;	ibad['mms1-bot'] = []
	ibad['mms2-top'] = [8]
	ibad['mms2-bot'] = [12]
	ibad['mms3-top'] = [2]
;	ibad['mms3-bot'] = []
;	ibad['mms4-top'] = []
	ibad['mms4-bot'] = [6]
	
	;Get the energy table
	theVar[*,1]   = !values.f_nan
	
	;Ions
	IF species EQ 'ion' THEN BEGIN
		IF ibad -> HasKey(key) THEN BEGIN
			IF ~Array_Equal(ibad[key] EQ sensor, 0) $
				THEN theVar[*,1] = !Values.F_NaN
		ENDIF
	
	;Electrons
	ENDIF ELSE BEGIN
		IF ebad -> HasKey(key) THEN BEGIN
			IF ~Array_Equal(ebad[key] EQ sensor, 0) $
				THEN theVar[*,1] = !Values.F_NaN
		ENDIF
	ENDELSE
END


;+
;       Apply flat field correction factors to FEEPS ion/electron data;
;       correct factors are from the gain factor found in:
;       
;           FlatFieldResults_V3.xlsx
;           
;       from Drew Turner, 1/19/2017
;
; NOTES:
; 
;   From Drew Turner, 1/18/17:
;       Here are the correction factors that we need to apply to the current 
;       ION counts/rates/fluxes in the CDF files.  
;       NOTE, THIS IS A DIFFERENT TYPE OF CORRECTION THAN THAT FOR THE ELECTRONS!  
;       These shifts should be applied to the counts/rates/fluxes data EYE-BY-EYE on each spacecraft.  
;       These are multiplication factors (i.e., Jnew = Jold * Gcorr). 
;       For those equations, Jold is the original count/rate/flux array and
;       Jnew is the corrected version of the arrays using the factors listed below.
;
; NOTES:
;
;     See the spedas distribution
;         idl/projects/mms/feeps/mms_feeps_flat_field_corrections
;-
FUNCTION MrMMS_FEEPS_Load_Data_Flat_Field, theVar
	Compile_Opt idl2
	On_Error, 2
	
	G_corr                  = hash()
	G_corr['mms1-top-6']    = 0.7
	G_corr['mms1-top-7']    = 2.5
	G_corr['mms1-top-8']    = 1.5
	G_corr['mms1-bottom-5'] = 1.2    ; updated 1/24
	G_corr['mms1-bottom-6'] = 0.9
	G_corr['mms1-bottom-7'] = 2.2    ; updated 1/24
	G_corr['mms1-bottom-8'] = 1.0

	G_corr['mms2-top-4']    = 1.2    ; added 1/24
	G_corr['mms2-top-6']    = 1.3
	G_corr['mms2-top-7']    = 0      ; bad eye
	G_corr['mms2-top-8']    = 0.8
	G_corr['mms2-bottom-6'] = 1.4
	G_corr['mms2-bottom-7'] = 0      ; bad eye
	G_corr['mms2-bottom-8'] = 1.5

	G_corr['mms3-top-6']    = 0.7
	G_corr['mms3-top-7']    = 0.8
	G_corr['mms3-top-8']    = 1.0
	G_corr['mms3-bottom-6'] = 0.9
	G_corr['mms3-bottom-7'] = 0.9
	G_corr['mms3-bottom-8'] = 1.3

	G_corr['mms4-top-6']    = 0.8
	G_corr['mms4-top-7']    = 0      ; bad eye
	G_corr['mms4-top-8']    = 1.0
	G_corr['mms4-bottom-6'] = 0.8
	G_corr['mms4-bottom-7'] = 0.6
	G_corr['mms4-bottom-8'] = 0.9
	G_corr['mms4-bottom-9'] = 1.5    ; added 1/24
	
	;Which spacecraft and eye?
	parts  = StRegEx(theVar.name, '^(mms[1-4]).*(top|bottom).*sensorid_([1-9]|1[0-2])', /SUBEXP, /EXTRACT)
	sc     = parts[1]
	eye    = parts[2]
	sensor = parts[3]
	
	;Correct the counts
	tag = sc + '-' + eye + '-' + sensor
	IF G_corr -> HasTag(tag) $
		THEN theVar -> SetData, theVar['DATA'] * G_corr[tag]
END


;+
;
;       This function returns the energy table based on
;       each spacecraft and eye; based on the table from:
;       
;               FlatFieldResults_V3.xlsx
;               
;       from Drew Turner, 1/19/2017
;
; NOTES:
;     BAD EYES are replaced by NaNs
;
;     See the spedas distribution
;         idl/projects/mms/feeps/mms_feeps_correct_energies.pro
;         idl/projects/mms/feeps/mms_feeps_energy_table.pro
;-
PRO MrMMS_FEEPS_Load_Data_Fix_Energies, theVar
	Compile_Opt idl2
	On_Error, 2
	
	;For easy referencing
	NaN = !values.f_nan
	
	;Create the correction table
	table                = hash()
	table['mms1-top']    = [14.0,  7.0, 16.0, 14.0, 14.0,  0.0, 0.0,  0.0, 14.0, 14.0,  17.0, 15.0]
	table['mms1-bottom'] = [ NaN, 14.0, 14.0, 13.0, 14.0,  0.0, 0.0,  0.0, 14.0, 14.0, -25.0, 14.0]
	table['mms2-top']    = [-1.0,  6.0, -2.0, -1.0,  NaN,  0.0, NaN,  0.0,  4.0, -1.0,  -1.0,  0.0]
	table['mms2-bottom'] = [-2.0, -1.0, -2.0,  0.0, -2.0, 15.0, NaN, 15.0, -1.0, -2.0,  -1.0, -3.0]
	table['mms3-top']    = [-3.0,  NaN,  2.0, -1.0, -5.0,  0.0, 0.0,  0.0, -3.0, -1.0,  -3.0,  NaN]
	table['mms3-bottom'] = [-7.0,  NaN, -5.0, -6.0,  NaN,  0.0, 0.0, 12.0,  0.0, -2.0,  -3.0, -3.0]
	table['mms4-top']    = [ NaN,  NaN, -2.0, -5.0, -5.0,  0.0, NaN,  0.0, -1.0, -3.0,  -6.0, -6.0]
	table['mms4-bottom'] = [-8.0,  NaN, -2.0,  NaN,  NaN, -8.0, 0.0,  0.0, -2.0,  NaN,   NaN, -4.0]
	
	;Which spacecraft and eye?
	parts  = StRegEx(theVar.name, '^(mms[1-4]).*(top|bottom).*sensorid_([1-9]|1[0-2])', /SUBEXP, /EXTRACT)
	sc     = parts[1]
	eye    = parts[2]
	sensor = Fix(parts[3])
	
	;Correct the energy table
	;   - Sensors start at 1, IDL index starts at 0
	oE_old = theVar['DEPEND_1']
	oE_new = oE_old + table[sc+'-'+eye, sensor-1]
	oE_old -> CopyAttrTo, oE_new
	theVar['DEPEND_1'] = oE_new
END


;+
;    this procedure splits the last integral channel from the FEEPS spectra, 
;    creating 2 new tplot variables:
;    
;       [original variable]_clean - spectra with the integral channel removed
;       [original variable]_500keV_int - the integral channel that was removed
;-
FUNCTION MrMMS_FEEPS_Load_Data_Integral_Channel, theVar
	Compile_Opt idl2
	On_Error, 2
	
	;Name of integral channel
	;   - Put "500keV" after eye and before data product
	parts     = StrSplit(theVar.name, '_', /EXTRACT)
	int_vname = StrJoin( [parts[0:7], '500keV', parts[8:*]], '_' )
	
	;Create variables
	o500keV  = theVar[*, -1]
	
	;Remove teh 500keV channel
	newVar  = theVar[*,0:-2]
	newVar -> SetName, theVar.name
	MrVar_Replace, theVar, newVar
	
	;Add 500keV to the cache
	o500keV -> SetName, int_vname
	o500keV -> Cache
	
	;Attributes
	o500keV -> RemoveAttr, 'DEPEND_1'
	
	;Return its variable name
	RETURN, o500keV
END


;+
;    this procedure splits the last integral channel from the FEEPS spectra, 
;    creating 2 new tplot variables:
;    
;       [original variable]_clean - spectra with the integral channel removed
;       [original variable]_500keV_int - the integral channel that was removed
;-
FUNCTION MrMMS_FEEPS_Load_Data_Omni, varname
	Compile_Opt idl2
	On_Error, 2
	
	;Energy and gain correction factors
	;   - inter-spacecraft calibrations
	;   - The element corresponds to the spacecraft
	eEcorr = [14.0,  -1.0, -3.0, -3.0]
	iEcorr = [ 0.0,   0.0,  0.0,  0.0]
	eGfact = [ 1.0,   1.0,  1.0,  1.0]
	iGfact = [ 0.84,  1.0,  1.0,  1.0]
	
;-------------------------------------------
; Variable Names ///////////////////////////
;-------------------------------------------
	
	;Dissect the variable names
	;   - The eye and sensor have been replaced with the "#" character
	;   - The sc, instr, mode, and level occur before the first "#"
	segs    = StrSplit(varname[0], '*', /EXTRACT, COUNT=nSegs)
	parts   = StrSplit(segs[0], '_', /EXTRACT)
	sc      = parts[0]
	instr   = parts[1] + '_' + parts[2]
	mode    = parts[3]
	level   = parts[4]
	species = parts[5]
	
	;
	; Available sensors
	;
	
	;Electrons
	IF species EQ 'electron' THEN BEGIN
		;Sensor ID
		IF level EQ 'sitl' $
			THEN sensors = ['5','11','12'] $
			ELSE sensors = mode EQ 'brst' ? ['1','2','3','4','5','9','10','11','12'] : ['3', '4', '5', '11', '12']
	
	;Ions
	ENDIF ELSE BEGIN
		;Sensor ID
		sensors = ['6', '7', '8']
	ENDELSE
	
	;Variable names of each sensor
	top_vnames = segs[0] + 'top'    + segs[1] + sensors
	bot_vnames = segs[0] + 'bottom' + segs[1] + sensors
	IF nSegs GT 2 THEN BEGIN
		top_vnames += segs[2:*]
		bot_vnames += segs[2:*]
	ENDIF
	
	;Ouput name
	omni_vname = segs[0] + 'omni' + segs[1:*]
	omni_vname = StrJoin( (StRegEx(omni_vname, '(.*)_sensorid_(.*)', /SUBEXP, /EXTRACT))[[1,2]] )
	
	;Are the names present?
	IF ~Array_Equal( MrVar_IsCached( [top_vnames, bot_vnames] ), 1) THEN BEGIN
		MrPrintF, 'LogText', 'Not all sensors found: "' + varname + '".'
		RETURN, !Null
	ENDIF

;-------------------------------------------
; Energy Bins //////////////////////////////
;-------------------------------------------
	
	;Energy table
	IF species EQ 'electron' THEN BEGIN
		energies = [ 33.200000D,  51.900000D,  70.600000d,  89.400000d, 107.10000d, $
		            125.20000D,  146.50000D,  171.30000d,  200.20000d,  234.00000d, $
		            273.40000D,  319.40000D,  373.20000d,  436.00000d,  509.20000d ]
		
		CASE sc OF
			'mms1': energies += eECorr[0]
			'mms2': energies += eECorr[1]
			'mms3': energies += eECorr[2]
			'mms4': energies += eECorr[3]
			ELSE: Message, 'Invalid spacecraft: "' + sc + '".'
		ENDCASE
		
	;Ions
	ENDIF ELSE BEGIN
		energies = [ 57.900000d,  76.800000d,  95.400000d, 114.10000d, 133.00000d, $
		            153.70000d,  177.60000d,  205.10000d,  236.70000d, 273.20000d, $
		            315.40000d,  363.80000d,  419.70000d,  484.20000d, 558.60000d]
		
		CASE sc OF
			'mms1': energies += iECorr[0]
			'mms2': energies += iECorr[1]
			'mms3': energies += iECorr[2]
			'mms4': energies += iECorr[3]
			ELSE: Message, 'Invalid spacecraft: "' + sc + '".'
		ENDCASE
	ENDELSE

;-------------------------------------------
; Omni-Directional Flux ////////////////////
;-------------------------------------------
	
	;SITL
	IF level EQ 'sitl' THEN BEGIN
		MrPrintF, 'LogErr', 'SITL data products are not yet supported.'
		RETURN, !Null
	ENDIF
	
	;Allocate memory
	oVar     = MrVar_Get(top_vnames[0])
	dims     = oVar.dimensions
	nTime    = dims[0]
	nEnergy  = dims[1]
	nSensors = N_Elements(sensors)
	omni     = FltArr(nTime, nEnergy, 2*nSensors)
	
	FOR i = 0, 2*nSensors-1, 2 DO BEGIN
		iSensor = Fix(i/2)
		
		;Bottom
		IF MrVar_IsCached(bot_vnames[iSensor]) THEN BEGIN
			;Collect the data
			oVar        = MrVar_Get(bot_vnames[iSensor])
			omni[0,0,i] = oVar['DATA']
			
			;Energies
			oE   = oVar['DEPEND_1']
			iBad = Where( Abs( oE['DATA'] - Energies ) GT 0.1*Energies, nBad)
			IF nBad GT 0 THEN oE[*,iBad] = !Values.F_NaN
		ENDIF ELSE BEGIN
			MrPrintF, 'LogWarn', 'Sensor not found: "' + bot_vnames[iSensor] + '".'
		ENDELSE
		
		;TOP
		IF MrVar_IsCached(top_vnames[iSensor]) THEN BEGIN
			;Collect the data
			oVar          = MrVar_Get(top_vnames[iSensor])
			omni[0,0,i+1] = oVar['DATA']
			
			;Energies
			oE   = oVar['DEPEND_1']
			iBad = Where( Abs( oE['DATA'] - Energies ) GT 0.1*Energies, nBad)
			IF nBad GT 0 THEN oE[*,iBad] = !Values.F_NaN
		ENDIF ELSE BEGIN
			MrPrintF, 'LogWarn', 'Sensor not found: "' + top_vnames[iSensor] + '".'
		ENDELSE
	ENDFOR

;-------------------------------------------
; Gain Correction //////////////////////////
;-------------------------------------------
	
	IF StRegEx(species, 'electron', /BOOLEAN) THEN BEGIN
		CASE sc OF
			'mms1': omni *= eGfact[0]
			'mms2': omni *= eGfact[1]
			'mms3': omni *= eGfact[2]
			'mms4': omni *= eGfact[3]
			ELSE: Message, 'Invalid spacecraft: "' + sc + '".'
		ENDCASE
	ENDIF ELSE BEGIN
		CASE sc OF
			'mms1': omni *= iGfact[0]
			'mms2': omni *= iGfact[1]
			'mms3': omni *= iGfact[2]
			'mms4': omni *= iGfact[3]
			ELSE: Message, 'Invalid spacecraft: "' + sc + '".'
		ENDCASE
	ENDELSE

;-------------------------------------------
; Output Variable //////////////////////////
;-------------------------------------------
	
	;Energy variable
	oEnergy = MrVariable(energies, NAME=e_vname, /NO_COPY)
	oEnergy['LOG']   = 1B
	oEnergy['TITLE'] = 'Energy!C(eV)'
	oEnergy['UNITS'] = 'eV'
	
	;Create a variable
	oOmni = MrTimeSeries( oVar['TIMEVAR'], Mean( omni, DIMENSION=3, /NAN ), $
	                      /CACHE, $
	                      NAME = omni_vname )
	
	;Attributes
	oVar -> CopyAttrTo, oOmni, [ 'AXIS_RANGE', 'FILLVAL', 'FORMAT', 'LOG', 'SCALETYP', 'SCALEMAX', $
	                             'SCALEMIN', 'SI_CONVERSION', 'UNITS', 'VALIDMIN', 'VALIDMAX']
	oOmni['DEPEND_1']   = oEnergy
	oOmni['NAN']        = 1B
	oOmni['TITLE']      = 'Omin!C' + oOmni['UNITS']
	oOmni['PLOT_TITLE'] = 'FEEPS Omni-Directional Flux'
END


;+
;    this procedure splits the last integral channel from the FEEPS spectra, 
;    creating 2 new tplot variables:
;    
;       [original variable]_clean - spectra with the integral channel removed
;       [original variable]_500keV_int - the integral channel that was removed
;-
FUNCTION MrMMS_FEEPS_Load_Data_Read_CSV
	Compile_Opt idl2
	On_Error, 2
	
	;File to read
	path = File_DirName( File_Which('mrmms_feeps_load_data.pro') )
	
	;Convert the structure to a hash
	mask = hash()
	
	;Each spacecraft
	FOR i = 0, 3 DO BEGIN
		sc   = 'mms' + String(i+1, FORMAT='(i1)')
		file = FilePath( StrJoin( [StrUpCase(sc), 'FEEPS', 'ContaminatedSectors', '20160709.csv'], '_' ), $
		                 ROOT_DIR = path )
	
		;Read the file
		data = Read_CSV(file)
		
		;Each Eye
		FOR j = 0, 23 DO BEGIN
			eye = j LE 11 ? 'top' : 'bottom'
			iBad = Where(data.(j) EQ 1, nBad)
			IF nBad GT 0 THEN mask[sc + '-' + eye + '-' + String(j+1, FORMAT='(i0)')] = iBad
		ENDFOR
	ENDFOR
	
	RETURN, mask
END


;+
;    this procedure splits the last integral channel from the FEEPS spectra, 
;    creating 2 new tplot variables:
;    
;       [original variable]_clean - spectra with the integral channel removed
;       [original variable]_500keV_int - the integral channel that was removed
;-
PRO MrMMS_FEEPS_Load_Data_Remove_Sun, theVar, sunSector, mask
	Compile_Opt idl2
	On_Error, 2
	
	;Which spacecraft, eye, and sensor?
	parts  = StRegEx(theVar.name, '^(mms[1-4]).*(top|bottom).*sensorid_([1-9]|1[0-2])', /SUBEXP, /EXTRACT)
	sc     = parts[1]
	eye    = parts[2]
	sensor = parts[3]
	
	;Remove bad sectors
	key = sc + '-' + eye + '-' + sensor
	IF mask -> HasKey(key) THEN BEGIN
		tf_bad = MrIsMember( mask[key], sunSector['DATA'], iBad, COUNT=nBad )
		IF nBad GT 0 THEN theVar[iBad,*] = !Values.F_NaN
	ENDIF
END


;+
;   Find FEEPS data then load it into the MrVariable cache.
;
; :Params:
;       SC:         in, required, type=string
;                   Spacecraft ID. Options are {'mms1' | 'mms2' | 'mms3' | 'mms4' }
;       MODE:       in, required, type=string, default='srvy'
;                   Data telemetry rate of the data. Options are: { 'slow' | 'fast' | 'srvy' | 'brst' }
;
; :Keywords:
;       FGM_INSTR:  in, optional, type=string, default='fgm'
;                   FGM instrument to use. Options are: { 'afg' | 'dfg' | 'fgm' }
;       LEVEL:      in, optional, type=string, default='l2'
;                   Data quality level. Options are: {'l1a' | 'l1b' | 'ql' | 'l2pre' | 'l2'}
;       NO_LOAD:    in, optional, type=boolean, default=0
;                   If set, data will not be loaded from source CDF files.
;       TRANGE:     in, optional, type=string/strarr(2), default=MrVar_GetTRange()
;                   The start and end times of the data interval to be plotted, formatted
;                       as 'YYYY-MM-DDThh:mm:ss'
;-
PRO MrMMS_FEEPS_Load_Data, sc, mode, $
LEVEL=level, $
OPTDESC=optdesc, $
SUFFIX=suffix, $
TRANGE=trange, $
VARFORMAT=varformat, $
VARNAMES=varnames
	Compile_Opt idl2
	
	Catch, the_error
	IF the_error NE 0 THEN BEGIN
		Catch, /CANCEL
		MrPrintF, 'LogErr'
		RETURN
	ENDIF
	
	instr = 'epd_feeps'
	IF N_Elements(varformat) EQ 0 THEN varformat = '*intensity*'
	IF N_Elements(level)     EQ 0 THEN level     = 'l2'
	IF N_Elements(mode)      EQ 0 THEN mode      = 'srvy'
	IF N_Elements(optdesc)   EQ 0 THEN optdesc   = 'electron'
	IF N_Elements(suffix)    EQ 0 THEN suffix    = ''
	IF N_Elements(trange)    GT 0 THEN MrVar_SetTRange, trange
	
	;Also get the spin sector number to make the sun correction
	varformat = [varformat, '*spinsectnum*']

;-------------------------------------------
; Get Data /////////////////////////////////
;-------------------------------------------
	;FEEPS
	MrMMS_Load_Data, sc, 'feeps', mode, level, $
	                 OPTDESC   = optdesc, $
	                 SUFFIX    = suffiX, $
	                 VARFORMAT = varformat, $
	                 VARNAMES  = varnames
	
	;Get the sun-sensor mask
	mask = MrMMS_FEEPS_Load_Data_Read_CSV()
	
	;Get the sun-sector variable
	iSun = Where( StrMatch(varnames, '*spinsectnum*'), nSun )
	oSunSector = MrVar_Get(varnames[iSun])

;-------------------------------------------
; Fix Data /////////////////////////////////
;-------------------------------------------
	;Fix each variable
	FOR i = 0, N_Elements(varnames) - 1 DO BEGIN
		tf_ion  = StRegEx(varnames[i], 'ion', /BOOLEAN)
		tf_top  = StRegEx(varnames[i], 'top', /BOOLEAN)
		tf_data = StRegEx(varnames[i], '(count_rate|intensity)', /BOOLEAN)
		
		;SKIP
		IF ~tf_data THEN CONTINUE
		
		;Extract the variable
		oVar = MrVar_Get(varnames[i])

		;Energy table corrections
		MrMMS_FEEPS_Load_Data_Fix_Energies, oVar
	
		;Flat-field correction
		;   - Only for ions
		IF tf_ion THEN MrMMS_FEEPS_Load_Data_Flat_Field, oVar
	
		;Remove data from bad eyes
		MrMMS_FEEPS_Load_Data_Bad_Eyes, oVar
	
		;Separate 500keV channel
		;   - It integrates from 500keV and above
		;   - Energy bin is disproportionate
		o500keV = MrMMS_FEEPS_Load_Data_Integral_Channel(oVar)
		varnames = [varnames, o500keV.name]
	
		;Separate 500keV channel
		;   - It integrates from 500keV and above
		;   - Energy bin is disproportionate
		MrMMS_FEEPS_Load_Data_Remove_Sun, oVar, oSunSector, mask
	ENDFOR

;-------------------------------------------
; Omni-Directional Flux ////////////////////
;-------------------------------------------
	dataset = ['count_rate', 'intensity']

	;Create the omin-directional flux
	FOR i = 0, N_Elements(sc)      - 1 DO $
	FOR j = 0, N_Elements(mode)    - 1 DO $
	FOR k = 0, N_Elements(level)   - 1 DO $
	FOR l = 0, N_Elements(optdesc) - 1 DO $
	FOR m = 0, N_Elements(dataset) - 1 DO BEGIN

		;Look for relevant data
		vtest = StrJoin( [sc[i], instr, mode[j], level[k], optdesc[l], '*', dataset[m], 'sensorid', '*'], '_') + suffix
		MrVar_Names, varnames, vtest
		IF varnames[0] NE '' THEN oOmni = MrMMS_FEEPS_Load_Data_Omni(vtest)
		
		;Add name
		If Obj_Valid(oOmni) THEN varnames = [varnames, oOmni.name]
	ENDFOR

;-------------------------------------------
; Clean Up /////////////////////////////////
;-------------------------------------------
	;Get the names of the individual sensors
	MrVar_Names, names, '.*_(electron|ion)_(bottom|top)_(count_rate|intensity)_sensorid_.*'+suffix, /REGEX
	
	;Delete them
	MrVar_Delete, names
END