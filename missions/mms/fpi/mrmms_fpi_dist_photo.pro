; docformat = 'rst'
;
; NAME:
;       MrMMS_FPI_Dist_Photo
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
;   Load FPI distribution function data and rotate the instrument look directions
;   into a field-aligned coordinate system.
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
;       2017/01/25  -   Written by Matthew Argall
;-
;*****************************************************************************************
;+
;   Find and read MMS FPI data.
;
;   Calling Sequence:
;       ofcorr = MrMMS_FPI_Dist_Photo( f, dphi, ph_scl, ph_dphi, ph_f0, ph_e0 )
;       ofcorr = MrMMS_FPI_Dist_Photo( f, dphi, ph_scl, ph_dphi, ph_f0, ph_e0, ph_f1, ph_e1, parity )
;
; :Params:
;       F:                  in, required, type=string/integer/objref
;                           Name, number, or MrTimeSeries object of the distribution to
;                               be corrected
;       DPHI:               in, required, type=string/integer/objref
;                           Name, number, or MrTimeSeries object spin-phase of the angular
;                               offset of the sensor +X direction.
;       PH_SCL:             in, required, type=string/integer/objref
;                           Name, number, or MrTimeSeries object of the photo-electron
;                               scaling factor.
;       PH_DPHI:            in, required, type=string/integer/objref
;                           Name, number, or MrTimeSeries object of the spin-phase angular
;                               offset at which the photo electron correction is to be applied.
;       PH_F0:              in, required, type=string/integer/objref
;                           Name, number, or MrTimeSeries object of the photo electron
;                               model. If in burst mode, this should apply to parity zero.
;       PH_E0:              in, required, type=string/integer/objref
;                           Name, number, or MrTimeSeries object of the energy bins
;                               associated with `PH_F0`.
;       PH_F1:              in, required, type=string/integer/objref
;                           Name, number, or MrTimeSeries object of the photo electron
;                               model. If in burst mode, this should apply to parity zero.
;       PH_E1:              in, required, type=string/integer/objref
;                           Name, number, or MrTimeSeries object of the energy bins
;                               associated with `PH_F1`.
;       PARITY:             in, optional, type=string
;                           Name, number, or MrTimeSeries object of the step-table parity
;                               in use at each time.
;
; :Keywords:
;       CACHE:              in, optional, type=boolean, default=0
;                           If set, the return variable will be added to the cache.
;       NAME:               in, optional, type=string, default='gse'
;                           Name to be given to the return variable. 
;-
FUNCTION MrMMS_FPI_Dist_Photo, f, dphi, ph_scl, ph_dphi, ph_f0, ph_e0, ph_f1, ph_e1, parity, $
CACHE=cache, $
NAME=name
	Compile_Opt idl2

	Catch, the_error
	IF the_error NE 0 THEN BEGIN
		Catch, /CANCEL
		MrPrintF, 'LogErr'
		RETURN, Obj_New()
	ENDIF
	
	oDist   = MrVar_Get(f)
	dims    = Size(oDist, /DIMENSIONS)
	nTime   = dims[0]
	nPhi    = dims[1]
	nTheta  = dims[2]
	nEnergy = dims[3]
	IF N_Elements(name) EQ 0 THEN name = 'mms_photo_electron_model_distribution'

;-------------------------------------------
; Spin-Phase Location //////////////////////
;-------------------------------------------
	;Get spin-phase variables
	oDelPhi       = MrVar_Get(dphi)
	oDelPhi_Photo = MrVar_Get(ph_dphi)
	
	;Locate the photoelectron model spin phase angles within the distribution spin phase angles
	idx       = Floor(oDelPhi['DATA']/16.0)
	photo_idx = Floor(oDelPhi_Photo['DATA']/16.0)
	iPhoto    = Value_Locate(photo_idx, idx)

;-------------------------------------------
; Build fphoto /////////////////////////////
;-------------------------------------------
	;Brst mode
	IF N_Params() EQ 9 THEN BEGIN
		of0_ph   = MrVar_Get(ph_f0)
		of1_ph   = MrVar_Get(ph_f1)
		oParity  = MrVar_Get(parity)
		oE0      = MrVar_Get(ph_e0)
		oE1      = MrVar_Get(ph_e1)
	
		;Separate parity 0 from parity 1
		i0 = oParity -> Where(0, /EQUAL, COUNT=n0, COMPLEMENT=i1, NCOMPLEMENT=n1)
	
		;Create the photoelectron distribution
		of_ph = MrTimeSeries( oDist['TIMEVAR'], FltArr(n0+n1, nPhi, nTheta, nEnergy) )
		IF n0 GT 0 THEN of_ph[i0,*,*,*] = of0_ph[iPhoto[i0],*,*,*]
		IF n1 GT 0 THEN of_ph[i1,*,*,*] = of1_ph[iPhoto[i1],*,*,*]
		
		;Create the energy tables
		oE  = MrTimeSeries( oDist['TIMEVAR'], FltArr(n0+n1, nEnergy), NAME=e_vname )
		IF n0 GT 0 THEN oE[i0,*] = Rebin( Reform(oE0['DATA'], 1, nEnergy), n0, nEnergy )
		IF n1 GT 0 THEN oE[i1,*] = Rebin( Reform(oE1['DATA'], 1, nEnergy), n1, nEnergy )
	
	;Fast mode
	ENDIF ELSE BEGIN
		oE0    = MrVar_Get(ph_e0)
		of0_ph = MrVar_Get(ph_f0)
		of_ph  = of0_ph[iPhoto,*,*,*]
		oE     = oE0
	ENDELSE
	
	;Scale the photo electron distribution and subtract it from
	;the skymap
	nPhoto = MrVar_Get(ph_scl)
	of_ph *= nPhoto['DATA',0]

;-------------------------------------------
; Attributes ///////////////////////////////
;-------------------------------------------
	;Energy
	oE['LOG']   = 1B
	oE['LABEL'] = Size(of0_ph['LABL_PTR_3'], /TNAME) EQ 'OBJREF' ? of0_ph['LABL_PTR_3'] -> GetData() : of0_ph['LABL_PTR_3']
	oE0 -> CopyAttrTo, oE, [ 'FILLVAL', 'FORMAT', 'LABLAXIS', 'MAX_VALUE', 'MIN_VALUE', $
	                         'PLOT_TITLE', 'TITLE', 'UNITS', 'VAR_NOTES']

	;Distribution
	of_ph['DEPEND_3'] = oE
	of0_ph -> CopyAttrTo, of_ph, [ 'CDF_TYPE', 'DEPEND_1', 'DEPEND_2', 'FIELDNAM', 'FILLVAL', $
	                               'FORMAT', 'LOG', 'UNITS', 'MAX_VALUE', 'MIN_VALUE', 'VAR_NOTES' ]
	
	;Done
	of_ph -> SetName, name
	IF Keyword_Set(cache) THEN of_ph -> Cache
	RETURN, of_ph
END