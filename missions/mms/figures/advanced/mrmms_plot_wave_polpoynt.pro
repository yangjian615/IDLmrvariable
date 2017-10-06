; docformat = 'rst'
;
; NAME:
;       MrMMS_Plot_Wave_PolPoynt
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
;   Plot the parallel and perpendicular components of the electric field.
;        1. Bx, By, Bz, |B|
;        2. dB FAC (choice of magnetometers)
;        3. dE FAC
;        4. B Intensity
;        5. B Ellipticity
;        6. B Wave Normal Angle
;        7. E Intensity
;        8. E Ellipticity
;        9. E Wave Normal Angle
;       10. Poynting Flux Vector
;       11. Poynting Flux Magnitude
;       12. Poynting Flux Polar Angle
;       13. Poynting Flux Azimuthal Angle
;
; :Categories:
;   MMS, EDI, MrVariable
;
; :Params:
;       SC:         in, required, type=string
;                   MMS spacecraft ID. Options are {'mms1' | 'mms2' | 'mms3' | 'mms4'}
;       MODE:       in, required, type=string
;                   Data rate mode. Options are {'srvy' | 'brst'}
;       TSTART:     in, optional, type=string/strarr
;                   Date-time string, formatted as YYYY-MM-DDThh:mm:ss.fff indicating the
;                       time of the first distribution. If an array, `TEND` and `TSTRIDE`
;                       are ignored. If not provided, the first time stamp will be used.
;       TEND:       in, optional, type=string/integer, default=1
;                   Date-time string, formatted as YYYY-MM-DDThh:mm:ss[.fff] indicating
;                       the time of the last distribution. If an integer, the total
;                       number of distributions to be plotted.
;       TSTRIDE:    in, optional, type=integer, defualt=1
;                   The number of distributions to skip between successive plots.
;
; :Keywords:
;       DT:                 in, optional, type=string/objref, default=2.5
;                           Duration, in seconds, of each time bin.
;       DGA:                in, optional, type=float, default=11.25
;                           Width, in degrees, of each gyrophase bin.
;       FAC:                in, optional, type=string, default='EXB' for srvy and 'VXB' for brst
;                           Name of the field-aligned coordinate system used
;                               to define the directions perpendicular to B.
;                               Options include 'VXB' and 'EXB'
;       GARANGE:            out, optional, type=string, default=[-180\, 180]
;                           Range of gyrophase angles over which to bin.
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
;       2017/05/04  -   Written by Matthew Argall
;-
FUNCTION MrMMS_Plot_Wave_PolPoynt, sc, mode, nfft, nshift, $
COORDS=coords, $
MAG_INSTR=mag_instr, $
LEVEL=level, $
NDETREND=nDetrend, $
NO_LOAD=no_load, $
TRANGE=trange
	Compile_Opt idl2
	
	Catch, the_error
	IF the_error NE 0 THEN BEGIN
		Catch, /CANCEL
		IF N_Elements(win) GT 0 THEN Obj_Destroy, win
		MrPrintF, 'LogErr'
		RETURN, !Null
	ENDIF
	
	;Defaults
	tf_load = ~Keyword_Set(no_load)
	IF N_Elements(nfft)      EQ 0 THEN nfft      = 512
	IF N_Elements(nshift)    EQ 0 THEN nshift    = nfft / 2.0
	IF N_Elements(nDetrend)  EQ 0 THEN nDetrend  = nfft
	IF N_Elements(mag_instr) EQ 0 THEN mag_instr = 'scm'
	IF N_Elements(coords)    EQ 0 THEN coords    = 'gse'
	IF N_Elements(level)     EQ 0 THEN level     = 'l2'
	IF N_Elements(trange)    GT 0 THEN MrVar_SetTRange, trange
	
	;Conflicts
	IF Array_Equal(mag_instr EQ ['afg', 'dfg', 'fgm', 'scm'], 0) $
		THEN Message, 'Invalid value for MAG_INSTR: "' + mag_instr + '".'
	
;-------------------------------------------
; Get Data /////////////////////////////////
;-------------------------------------------
	;FGM parameters
	IF N_Elements(fgm_instr) EQ 0 THEN fgm_instr = (mag_instr NE 'scm') ? mag_instr : 'fgm'
	fgm_coords = (coords EQ 'dsl'  || coords EQ 'dbcs') ? 'dmpa' : coords
	fgm_mode   = (mode   EQ 'fast' || mode   EQ 'slow') ? 'srvy' : mode
	
	;EDP parameters
	edp_coords = (coords EQ 'dmpa' || coords EQ 'dbcs') ? 'dsl'  : coords
	IF mode EQ 'srvy' THEN BEGIN
		MrPrintF, 'LogWarn', 'EDP does not have SRVY data. Using FAST.'
		edp_mode = 'fast'
	ENDIF ELSE BEGIN
		edp_mode = mode
	ENDELSE
	
	;FPI parameters
	fpi_mode   = mode EQ 'brst' ? mode : 'fast'
	fpi_coords = (coords EQ 'dsl' || coords EQ 'dmpa') ? 'dbcs' : coords
	
	;SCM parameters
	IF N_Elements(scm_optdesc) EQ 0 THEN BEGIN
		CASE mode OF
			'slow': scm_optdesc = 'scf'
			'fast': scm_optdesc = 'scf'
			'srvy': scm_optdesc = 'scsrvy'
			'brst': scm_optdesc = 'scb'  ; or 'sch'
			ELSE: Message, 'Invalid MODE: "' + mode + '".'
		ENDCASE
	ENDIF
	scm_coords = coords
	IF scm_coords NE 'gse' THEN Message, 'SCM data is available only in GSE.'
	
	
	;Load the data
	IF tf_load THEN BEGIN
		;EDP
			MrMMS_Load_Data, sc, instr, edp_mode, level, $
			                 OPTDESC   = 'dce', $
			                 VARFORMAT = '*_dce_'+edp_coords+'_*'
		
		;SCM
		IF mag_instr EQ 'scm' THEN BEGIN
			MrMMS_Load_Data, sc, instr, 'brst', 'l2', $
			                 OPTDESC = scm_optdesc
		
		;FGM
		ENDIF ELSE IF mag_instr NE mag_instr THEN BEGIN
			MrMMS_FGM_Load_Data, sc, fgm_mode, $
			                     INSTR     = mag_instr, $
			                     LEVEL     = level, $
			                     VARFORMAT = '*b_'+fgm_coords+'_'+fgm_mode+'*'
		ENDIF
		
		;FGM
		MrMMS_FGM_Load_Data, sc, fgm_mode, $
		                     INSTR     = fgm_instr, $
		                     LEVEL     = level, $
		                     VARFORMAT = '*b_'+fgm_coords+'_'+fgm_mode+'*'
		
		;FPI
		MrMMS_FPI_Load_Data, sc, fpi_mode, $
		                     OPTDESC   = 'des-moms', $
		                     VARFORMAT = '*numberdensity_'+fpi_mode+'*'
	ENDIF

;-------------------------------------------
; Variable Names ///////////////////////////
;-------------------------------------------
	;Source names
	b_vname    = StrJoin( [sc, fgm_instr, 'b',    fgm_coords,              mode, level], '_' )
	bmag_vname = StrJoin( [sc, fgm_instr, 'bmag', fgm_coords,              mode, level], '_' )
	bvec_vname = StrJoin( [sc, fgm_instr, 'bvec', fgm_coords,              mode, level], '_' )
	edp_vname  = StrJoin( [sc, 'edp',     'dce',  edp_coords,              mode, level], '_' )
	scm_vname  = StrJoin( [sc, 'scm',     'acb',  scm_coords, scm_optdesc, mode, level], '_' )
	fgm_vname  = StrJoin( [sc, mag_instr, 'bvec', fgm_coords,              mode, level], '_' )
	n_vname    = StrJoin( [sc, 'des',     'numberdensity',             fpi_mode       ], '_' )
	mag_vname  = mag_instr EQ 'scm' ? scm_vname : fgm_vname
	
	;Output names
	de_fac_vname = StrJoin( [sc, 'edp', 'dce',  'dfac', edp_mode, level], '_' )
	IF mag_instr EQ 'scm' $
		THEN db_fac_vname = StrJoin( [sc, 'scm',     'acb',  'dfac',  scm_optdesc, mode,     level], '_' ) $
		ELSE db_fac_vname = StrJoin( [sc, mag_instr, 'bvec', 'dfac',               fgm_mode, level], '_' )

	b_intensity_vname    = 'Intensity('       + db_fac_vname + ')'
	b_wavenormal_vname   = 'WaveNormalAngle(' + db_fac_vname + ')'
	b_ellipticity_vname  = 'Ellipticity('     + db_fac_vname + ')'
	e_intensity_vname    = 'Intensity('       + de_fac_vname + ')'
	e_wavenormal_vname   = 'WaveNormalAngle(' + de_fac_vname + ')'
	e_ellipticity_vname  = 'Ellipticity('     + de_fac_vname + ')'
	fce_vname            = StrJoin( [sc, fgm_instr, 'fce',                mode, level], '_' )
	fce_half_vname       = StrJoin( [sc, fgm_instr, 'fce', 'half',        mode, level], '_' )
	flh_vname            = StrJoin( [sc, fgm_instr, 'flh',                mode, level], '_' )
	s_vec_vname          = StrJoin( [sc, 'fields', 's', 'fac',            mode, level], '_' )
	s_spec_vname         = StrJoin( [sc, 'fields', 's', 'fac', 'spectra', mode, level], '_' )
	s_r_vname            = StrJoin( [sc, 'fields', 's', 'fac', 'spectra', mode, level, 'r'],     '_' )
	s_theta_vname        = StrJoin( [sc, 'fields', 's', 'fac', 'spectra', mode, level, 'theta'], '_' )
	s_phi_vname          = StrJoin( [sc, 'fields', 's', 'fac', 'spectra', mode, level, 'phi'],   '_' )

;-------------------------------------------
; Detrend & FAC ////////////////////////////
;-------------------------------------------
	oMag = MrVar_Get(bvec_vname)
	OE   = MrVar_Get(edp_vname)
	oB   = MrVar_Get(mag_vname)
	
	;Remove DC field
	oDB = oB -> Detrend(nDetrend, /EDGE_TRUNCATE)
	oDE = oE -> Detrend(nDetrend, /EDGE_TRUNCATE)
	
	;Background field
	IF StRegEx(mag_instr, '(afg|dfg|fgm)', /BOOLEAN) $
		THEN oB0 = oMag - oDB $
		ELSE oB0 = oMag -> Interpol(oB)
	
	;Interpolate to magnetic field
	oDE_mag = oE -> Interpol(oB)
	
	;Field-Aligned Coordinates
	oT      = MrVar_FAC(oB0)
	odB_FAC = oT ## oDB
	odE_FAC = oT ## oDE_mag
	
	;Name and cache
	odB_FAC -> SetName, db_fac_vname
	odE_FAC -> SetName, de_fac_vname
	odB_FAC -> Cache
	odE_FAC -> Cache
	
	;Free memory
	Obj_Destroy, [oB0, oDB, oDE, oT, oDE_mag]

;-------------------------------------------
; Polarization Analysis ////////////////////
;-------------------------------------------
	;Polarization
	MrVar_Polarization, odB_FAC, nfft, nshift, $
	                    /CACHE, $
	                    ELLIPTICITY  = oB_Ellip, $
	                    INTENSITY    = oB_Intensity, $
	                    KDOTB_ANGLE  = oB_WaveNormal
	
	;Polarization
	MrVar_Polarization, odE_FAC, nfft, nshift, $
	                    /CACHE, $
	                    ELLIPTICITY  = oE_Ellip, $
	                    INTENSITY    = oE_Intensity, $
	                    KDOTB_ANGLE  = oE_WaveNormal
	
	;Poynting Flux Vector
	oSvec = MrVar_PoyntingFlux( odB_FAC, odE_FAC, $
	                            /CACHE, $
	                            NAME = s_vec_vname)
	
	;Poynting Flux Spectra
	oSspec = MrVar_PoyntingSpectra( odB_FAC, odE_FAC, nfft, nshift, $
	                                /CACHE, $
	                                NAME   = s_spec_vname, $
	                                /SPHERE, $
	                                WINDOW = 'hanning')

;-------------------------------------------
; Gyrofrequency Lines //////////////////////
;-------------------------------------------
	oBmag = MrVar_Get(bmag_vname)
	
	oFce      = MrVar_Freq_Cyclotron(oBmag, 'm_e', /CACHE, NAME=fce_vname)
	
	oFce_half = oFce / 2.0
	oFce_half -> SetName, fce_half_vname
	oFce_half -> Cache
	
	oFlh = MrVar_Freq_LowerHybrid(oBmag, n_vname, /CACHE, NAME=flh_vname)

;-------------------------------------------
; Attributes ///////////////////////////////
;-------------------------------------------
	oB = MrVar_Get(b_vname)
	oB['PLOT_TITLE'] = StrUpCase(sc)
	
	odB_FAC['LABEL'] = ['dB$\down||$', 'dB$\downPerp1$', 'dB$\downPerp2$']
	odB_FAC['TITLE'] = 'dB!C(nT)'
	
	odE_FAC['LABEL'] = ['dE$\down||$', 'dE$\downPerp1$', 'dE$\downPerp2$']
	odE_FAC['TITLE'] = 'dE!C(mV/m^2)'
	
	oB_intensity = MrVar_Get(b_intensity_vname)
	oB_intensity['TITLE'] = 'dB!C(nT^2/Hz)'
	
	oE_intensity = MrVar_Get(e_intensity_vname)
	oE_intensity['TITLE'] = 'dE!C(mV/m)^2/Hz'
	
	oFce['COLOR']     = 'Black'
	oFce['LABEL']     = 'fce'
	
	oFce_half['COLOR']     = 'Black'
	oFce_half['LABEL']     = 'fce/2'
	oFce_half['LINESTYLE'] = '--'
	
	oFlh['COLOR'] = 'White'
	oFlh['LABEL'] = 'flh'

;	IF instr EQ 'scm' THEN BEGIN
;		oVec['LABEL'] = ['dB$\downX$', 'dB$\downY$', 'dB$\downZ$']
;		oVec['TITLE'] = 'dB!C(nT)'
;		
;		oVec_FAC['LABEL'] = ['dB$\down||$', 'dB$\downPerp1$', 'dB$\downPerp2$']
;		oVec_FAC['TITLE'] = 'dB!C(nT)'
;	ENDIF

;-------------------------------------------
; Plot Data ////////////////////////////////
;-------------------------------------------
	win = MrVar_PlotTS( [b_vname, db_fac_vname, de_fac_vname, $
	                     b_intensity_vname, b_ellipticity_vname, b_wavenormal_vname, $
	                     e_intensity_vname, e_ellipticity_vname, e_wavenormal_vname, $
	                     s_vec_vname, s_r_vname, s_theta_vname, s_phi_vname], $
	                    /NO_REFRESH, $
	                    XSIZE = 650, $
	                    YSIZE = 800 )
	
	win = MrVar_OPlotTS( b_intensity_vname, [fce_vname, fce_half_vname, flh_vname] )
	
	win[0] -> SetLayout, [1,1]
	win    -> TrimLayout
	win -> refresh
	return, win
end