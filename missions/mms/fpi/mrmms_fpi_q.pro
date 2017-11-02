; docformat = 'rst'
;
; NAME:
;       MrMMS_FPI_Q
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
;+
;   Calculate Swisdak's Q parameter. Can be caculated for specific energies. Data will
;   be loaded automatically.
;
; :Categories:
;    MMS
;
; :Params:
;       SC:         in, required, type=string
;                   MMS spacecraft ID. Options are {'mms1' | 'mms2' | 'mms3' | 'mms4'}
;       MODE:       in, required, type=string
;                   Data rate mode. Options are {'srvy' | 'brst'}
;       SPECIES:    in, required, type=string
;                   Particle species. Options are {'e' | 'i'}
;       ENERGIES:   in, optional, type=2xN intarr
;                   Energies ranges in which to compute the agyrotropy measure.
;
; :Keywords:
;       COORDS:     in, optional, type=string, default='gse'
;                   Coordinate system in which Q is computed.
;       FGM_INSTR:  in, optional, type=string, default='fgm'
;                   FGM instrument to use for the DC magnetic field measurement.
;       LEVEL:      in, optional, type=string, default='l2'
;                   Data quality level to load.
;       NO_LOAD:    in, optional, type=boolean, default=0
;                   If set, data will not be loaded from source files.
;       OUTPUT_DIR: in, optional, type=string, default=pwd
;                   A directory in which to save the figure. If neither `OUTPUT_DIR`
;                       nor `OUTPUT_EXT` are defined, no file is generated.
;       OUTPUT_EXT: in, optional, type=string, default=pwd
;                   File extensions for the output figure. Options include: 'eps', 'gif',
;                       'jpg', 'ps', 'pdf', 'png', 'tiff'. If neither `OUTPUT_DIR` nor
;                       `OUTPUT_EXT` are defined, no file is generated.
;       SUFFIX:     in, optional, type=string, defauld=''
;                   Suffix to be appended to each variable name.
;       TAIL:       in, optional, type=boolean, default=0
;                   If set, the default energies are chosen to highlight magnetotail
;                       populations. The default is to highlight magnetopause populations.
;       VARNAMES:   out, optional, type=strarr
;                   Names of the variables that were loaded into the cache. Names, in
;                       this order are::
;                           0. Q within each energy range specified by `ENERGIES`
;                           1. Q from the total pressure tensor computed from the 3D distribution function
;                           2. Q from the total pressure tensor given in the moments files
;                           3. P scalar from the moments files
;                           4. P scalar from the computed from the distribution files
;                           5. Spacecraft potential
;                           6. Pressure tensor from the moments files
;                           7. FGM magnetic field
;                           8. FGM magnetic field vector
;                           9. FGM magnetic field magnitude
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
;       2017/02/07  -   Written by Matthew Argall
;-
PRO MrMMS_FPI_Q, sc, mode, species, energies, $
COORDS=coords, $
FGM_INSTR=fgm_instr, $
LEVEL=level, $
NO_LOAD=no_load, $
SUFFIX=suffix, $
TAIL=tail, $
VARNAMES=varnames
	Compile_Opt idl2
	
	Catch, the_error
	IF the_error NE 0 THEN BEGIN
		Catch, /CANCEL
		MrPrintF, 'LogErr'
		RETURN
	ENDIF
	
	coords  = 'gse'
	tf_load = ~Keyword_Set(no_load)
	tf_tail = Keyword_Set(tail)
	IF N_Elements(coords)    EQ 0 THEN coords    = 'gse'
	IF N_Elements(fgm_instr) EQ 0 THEN fgm_instr = 'fgm'
	IF N_Elements(level)     EQ 0 THEN level     = 'l2'
	IF N_Elements(species)   EQ 0 THEN species   = 'e'
	IF N_Elements(suffix)    EQ 0 THEN suffix    = ''
	instr = 'd' + species + 's'
	
	IF tf_tail THEN BEGIN
		energies = [ [ 0, 10], $     ;< 100eV
		             [11, 14], $     ;250 eV
		             [15, 18], $     ;500 eV
		             [19, 25], $     ;2.5 keV
		             [26, 31] ]      ;> 7 keV
	ENDIF ELSE BEGIN
		energies = [ [ 0,  4], $    ;20 eV
		             [ 5,  9], $    ;60 eV
		             [10, 13], $    ;250 eV
		             [14, 17], $    ;500 eV
		             [18, 31] ]     ;> 1 keV
	ENDELSE

;-------------------------------------------
; Variable Names ///////////////////////////
;-------------------------------------------
	fpi_mode  = mode EQ 'brst' ? mode : 'fast'
	fpi_instr = 'd' + species + 's'

	;Source
	b_vname     = StrJoin( [sc, fgm_instr, 'b',     coords, mode, level], '_' )  + suffix
	bvec_vname  = StrJoin( [sc, fgm_instr, 'bvec',  coords, mode, level], '_' )  + suffix
	bmag_vname  = StrJoin( [sc, fgm_instr, 'bmag',  coords, mode, level], '_' )  + suffix
	f_vname     = StrJoin( [sc, fpi_instr, 'dist',               fpi_mode], '_') + suffix
	p_vname     = StrJoin( [sc, fpi_instr, 'prestensor', coords, fpi_mode], '_') + suffix
	scpot_vname = StrJoin( [sc, 'edp', 'scpot', 'fast', level], '_' ) + suffix
	
	;Derived Moments
	pscl_vname      = StrJoin( [sc, fpi_instr, 'p',          fpi_mode], '_') + suffix
	pscl_calc_vname = StrJoin( [sc, fpi_instr, 'p', 'calc',  fpi_mode], '_') + suffix
	q_steps_vname   = StrJoin( [sc, fpi_instr, 'q', 'steps', fpi_mode], '_') + suffix
	q_all_vname     = StrJoin( [sc, fpi_instr, 'q', 'all',   fpi_mode], '_') + suffix
	q_moms_vname    = StrJoin( [sc, fpi_instr, 'q', 'moms',  fpi_mode], '_') + suffix
	
	;Output names
	varnames = [q_steps_vname, q_all_vname, q_moms_vname, pscl_vname, pscl_calc_vname, $
	            scpot_vname, p_vname, b_vname, bmag_vname, bvec_vname]

;-------------------------------------------
; Load Data ////////////////////////////////
;-------------------------------------------

	IF tf_load THEN BEGIN
		;FGM
		MrMMS_FGM_Load_Data, sc, mode, $
		                     INSTR  = fgm_instr, $
		                     LEVEL  = level, $
		                     VARFORMAT = '*b_gse_'+mode+'*', $
		                     SUFFIX = suffix
		
		;FPI-DIST
		MrMMS_FPI_Load_Dist3D, sc, fpi_mode, species, $
		                       /APPLY_MODEL, $
		                       COORD_SYS   = coord_sys, $
		                       LEVEL       = level, $
		                       ORIENTATION = orientation, $
		                       SUFFIX      = suffix
		
		;FPI-MOMS
		MrMMS_FPI_Load_Data, sc, fpi_mode, $
		                     OPTDESC   = fpi_instr + '-moms', $
		                     LEVEL     = level, $
		                     SUFFIX    = suffix, $
		                     VARFORMAT = '*pres*'+coords+'*'
		
		;Spacecraft potential
		MrMMS_Load_Data, sc, 'edp', 'fast', 'l2', $
		                 OPTDESC   = 'scpot', $
		                 SUFFIX    = suffix, $
		                 VARFORMAT = '*scpot*'
	ENDIF ELSE BEGIN
		IF Array_Equal(MrVar_IsCached(varnames[0:4]), 1) THEN RETURN
	ENDELSE

;-------------------------------------------
; Compute Moments //////////////////////////
;-------------------------------------------
	
	;Distribution function
	;   - Calculate moments in original coordinate system
	theSpecies = species EQ 'i' ? 'H' : species
	oDist4D    = MrDist4D(f_vname, VSC=scpot_vname, SPECIES=theSpecies)
	
	;Allocate memory to output
	oDist = MrVar_Get(f_vname)
	nPts  = oDist -> GetNPts()
	nQ    = N_Elements(energies[0,*])
	oQ    = MrTimeSeries( oDist['DEPEND_0'], FltArr(nPts, nQ), $
	                      /CACHE, $
	                      NAME = q_steps_vname )
	
	;Setup metadata
	labels   = StrArr(nQ)
	titles   = StrArr(nQ)
	colors   = MrDefaultColor(NCOLORS=nQ)
	oE       = oDist['DEPEND_3']
	
	;Calculate Q at each step
	FOR i = 0, nQ - 1 DO BEGIN
		;Energy ranges
		E0 = oE['DATA',0,energies[0,i]]
		E1 = oE['DATA',0,energies[1,i]]
		E0 = E0 LT 1000 ? String(E0, FORMAT='(i0)')+'eV' : String(E0/1000.0, FORMAT='(i0)')+'keV'
		E1 = E1 LT 1000 ? String(E1, FORMAT='(i0)')+'eV' : String(E1/1000.0, FORMAT='(i0)')+'keV'
		labels[i]   = E0 + '-' + E1
		
		;Compute Q-factor
		oDist4D -> Moments, PRESSURE = oPres, ENERGY_RANGE = energies[*,i]
		oTemp    = MrVar_Pres_QFactor( bvec_vname, oPres )
		oQ[*,i]  = oTemp['DATA']
	ENDFOR
		
	;Attributes
	oQ['CATDESC']    = 'The agryotropy measure calculated in different energy ranges within ' + $
	                   'the distribution function. The pressure tensor was computed ' + $
	                   'by integrated the distribution function, taking into account the ' + $
	                   'spacecraft potential and the FPI interally generated photoelectron model.'
	oQ['COLOR']      = colors
	oQ['LABEL']      = labels
	oQ['PLOT_TITLE'] = 'Agyrotropy Measure'
	oQ['TITLE']      = 'Sqrt(Q)'

;-------------------------------------------
; Total Agyrotropy /////////////////////////
;-------------------------------------------

	;Calculate for the complete distribution as well as for the FPI derived pressure tensor
	oDist4D -> Moments, PRESSURE = oPres_All, ENERGY_RANGE = [0, 31]
	oQ_all   = MrVar_Pres_QFactor( bvec_vname, oPres_All, /CACHE, NAME=q_all_vname )
	
	;Energy range
	E0 = oE['DATA', 0,  0]
	E1 = oE['DATA', 0, 31]
	E0 = E0 LT 1000 ? String(E0, FORMAT='(i0)')+'eV' : String(E0/1000.0, FORMAT='(i0)')+'keV'
	E1 = E1 LT 1000 ? String(E1, FORMAT='(i0)')+'eV' : String(E1/1000.0, FORMAT='(i0)')+'keV'
	
	;Derived Moments
	oQ_all['CATDESC'] = 'The agryotropy measure calculated between the energy ranges ' + $
	                    'of ' + E0 + ' and ' + E1 + '. The pressure tensor was computed ' + $
	                    'by integrated the distribution function, taking into account the ' + $
	                    'spacecraft potential and the FPI interally generated photoelectron model.'
	oQ_all['COLOR']   = 'Black'
	oQ_all['LABEL']   = 'UNH'
	oQ_all['TITLE']   = 'Sqrt(Q)!C' + E0 + '!C' + E1

;-------------------------------------------
; FPI Agyrotropy ///////////////////////////
;-------------------------------------------
	oQ_moms = MrVar_Pres_QFactor( bvec_vname, p_vname, $
	                              /CACHE, $
	                              NAME = q_moms_vname )
	
	;FPI-Moments
	oQ_moms['CATDESC'] = 'The agryotropy measure calculated between the energy ranges ' + $
	                    'of ' + E0 + ' and ' + E1 + '. The pressure tensor available in the ' + $
	                    'FPI moments files was used.'
	oQ_moms['COLOR']   = 'Blue'
	oQ_moms['LABEL']   = 'FPI'
	oQ_moms['TITLE']   = 'Sqrt(Q)!C' + E0 + '!C' + E1
	
	Obj_Destroy, oDist4D

;-------------------------------------------
; FPI and Custom Pressures /////////////////
;-------------------------------------------
	;FPI Pressure
	oP    = MrVar_Get(p_vname)
	oPscl = MrScalarTS( oP['TIMEVAR'], (oP[*,0,0] + oP[*,1,1] + oP[*,2,2]) / 3.0, $
	                    /CACHE, $
	                    NAME = pscl_vname )
	
	oP['COLOR'] = 'Black'
	oP['LABEL'] = 'FPI'
	oP['TITLE'] = 'P!C(nPa)'
	
	;FPI Pressure
	oP    = oPres_All
	oPscl = MrScalarTS( oP['TIMEVAR'], (oP[*,0,0] + oP[*,1,1] + oP[*,2,2]) / 3.0, $
	                    /CACHE, $
	                    NAME = pscl_calc_vname )
	
	oP['COLOR'] = 'Blue'
	oP['LABEL'] = 'UNH'
	oP['TITLE'] = 'P!C(nPa)'
END