; docformat = 'rst'
;
; NAME:
;       MrMMS_EDI_Load_Amb_L1A
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
;   Load EDI Q0 or Data29 data from source l1a files. Counts are calibrated and
;   trajectories are rotated into geostationary coordinate systems. This program
;   requires access to the MMS team side of the SDC.
;
; :Categories:
;   MrVariable, MMS, EDI
;
; :Params:
;       SC:         in, required, type=string
;                   Spacecraft ID. Options are {'mms1' | 'mms2' | 'mms3' | 'mms4' }
;       MODE:       in, required, type=string, default='srvy'
;                   Data telemetry rate of the data. Options are: { 'slow' | 'fast' | 'srvy' | 'brst' }
;
; :Keywords:
;       SUFFIX:     in, optional, type=string, default=''
;                   A suffix to be appendend to the end of EDI variable names.
;       TRANGE:     in, optional, type=string/strarr(2), default=MrVar_GetTRange()
;                   The start and end times of the data interval to be plotted, formatted
;                       as 'YYYY-MM-DDThh:mm:ss'
;       VARNAMES:   in, optional, type=strarr
;                   Names of the variables that have been loaded into the cache
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
;       2017/05/19  -   Written by Matthew Argall
;-
;*****************************************************************************************
;+
;
;-
PRO MrMMS_EDI_Load_Amb_L1A_Anode, phi, theta, bitmask, pitch_gdu1, pitch_gdu2, $
BRST=brst, $
GDU1=oIdx_GDU1, $
GDU2=oIdx_GDU2
	Compile_Opt idl2
	On_Error, 2
	
	tf_brst = Keyword_Set(brst)

	;Get variables
	oBitmask    = MrVar_Get(bitmask)
	oPhi        = MrVar_Get(phi)
	oTheta      = MrVar_Get(theta)
	oPitch_GDU1 = MrVar_Get(pitch_gdu1)
	oPitch_GDU2 = MrVar_Get(pitch_gdu2)
	
	;Constants
	dPhi   = 360.0 / 32.0
	dTheta = 360.0 / 512.0
	nChan  = tf_brst ? 4 : 1
	
;-------------------------------------------
; Theta Index //////////////////////////////
;-------------------------------------------
	
	;Theta index
	itheta_gdu1 = Round( oTheta['DATA'] / dTheta )
	itheta_gdu2 = itheta_gdu1
	
;-------------------------------------------
; Phi Anode ////////////////////////////////
;-------------------------------------------
	
	;Reference anode number
	N = Round( oPhi['DATA'] / dPhi )
	
	;Find unique modes
	nPts  = oBitmask -> GetNPts()
	iUniq = oBitmask -> Uniq(COUNT=nUniq)
	
	;Allocate memory
	anode_gdu1 = BytArr(nPts, nChan)
	anode_gdu2 = BytArr(nPts, nChan)
	
	;Step through each mode
	FOR i = 0, nUniq - 1 DO BEGIN
		theBit = bitmask[iUniq[i]]
		idx    = Where(bitmask EQ theBit, nIdx)

	;-----------------------------------------------------
	; AMB \\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\
	;-----------------------------------------------------
		;AMB
		;   - pitch   = field-aligned = bit 1
		;   - [0,180] = centered      = bit 4
		IF Array_Equal( MrBitGet( theBit, [1,4] ), 1) THEN BEGIN
			IF tf_brst THEN BEGIN
				ch_fa_gdu1 = IndGen(1,nChan) -  2
				ch_fa_gdu2 = IndGen(1,nChan) + 14
			ENDIF ELSE BEGIN
				ch_fa_gdu1 =  0
				ch_fa_gdu2 = 16
			ENDELSE

	;-----------------------------------------------------
	; AMB-PM2 \\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\
	;-----------------------------------------------------
		;AMB-PM2
		;   - pitch   = field-aligned = bit 1
		;   - [0,180] = one-sided     = bit 5
		ENDIF ELSE IF Array_Equal( MrBitGet( theBit, [1,5] ), 1) THEN BEGIN
			IF tf_brst THEN BEGIN
				ch_fa_gdu1 = Indgen(1,nChan)
				ch_fa_gdu2 = Indgen(1,nChan) + 15
			ENDIF ELSE BEGIN
				ch_fa_gdu1 =  0
				ch_fa_gdu2 = 15
			ENDELSE
		
	;-----------------------------------------------------
	; AMB-ALT-CC \\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\
	;-----------------------------------------------------
		;AMB-ALT-CC
		;   - pitch    = alternating = bit 2
		;   - [0,180]  = centered    = bit 4
		;   - 90       = centered    = bit -
		ENDIF ELSE IF Array_Equal( MrBitGet( theBit, [2,4] ), 1) THEN BEGIN
			IF tf_brst THEN BEGIN
				iChan      = Rebin(Indgen(1,nChan), nPts, nChan)
				ch_fa_gdu1 = Indgen(1,nChan) -  2
				ch_fa_gdu2 = Indgen(1,nChan) + 14
				ch_90_gdu1 = Indgen(1,nChan) -  2
				ch_90_gdu2 = Indgen(1,nChan) + 14
			ENDIF ELSE BEGIN
				ch_fa_gdu1 =  0
				ch_fa_gdu2 = 16
				ch_90_gdu1 =  0
				ch_90_gdu2 = 16
			ENDELSE

	;-----------------------------------------------------
	; AMB-ALT-OC \\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\
	;-----------------------------------------------------
		;AMB-ALT-OC
		;   - pitch    = alternating = bit 2
		;   - [0,180]  = one-sided   = bit 5
		;   - 90       = centered    = bit -
		ENDIF ELSE IF Array_Equal( MrBitGet( theBit, [2,5] ), 1) THEN BEGIN
			IF tf_brst THEN BEGIN
				iChan      = Rebin(Indgen(1,nChan), nPts, nChan)
				ch_fa_gdu1 = Indgen(1,nChan)
				ch_fa_gdu2 = Indgen(1,nChan) + 15
				ch_90_gdu1 = Indgen(1,nChan) -  2
				ch_90_gdu2 = Indgen(1,nChan) + 14
			ENDIF ELSE BEGIN
				ch_fa_gdu1 =  0
				ch_fa_gdu2 = 15
				ch_90_gdu1 =  0
				ch_90_gdu2 = 16
			ENDELSE

	;-----------------------------------------------------
	; AMB-ALT-OOM \\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\
	;-----------------------------------------------------
		;AMB-ALT-OOM
		;   - pitch    = alternating       = bit 2
		;   - [0,180]  = one-sided         = bit 5
		;   - 90       = one-sided         = bit 6
		;   - 90       = moni-directional  = bit -
		ENDIF ELSE IF Array_Equal( MrBitGet( theBit, [2,5,6] ), 1) THEN BEGIN
			IF tf_brst THEN BEGIN
				iChan      = Rebin(Indgen(1,nChan), nPts, nChan)
				ch_fa_gdu1 = Indgen(1,nChan)
				ch_fa_gdu2 = Indgen(1,nChan) + 15
				ch_90_gdu1 = Indgen(1,nChan)
				ch_90_gdu2 = Indgen(1,nChan) + 15
			ENDIF ELSE BEGIN
				ch_fa_gdu1 =  0
				ch_fa_gdu2 = 15
				ch_90_gdu1 =  0
				ch_90_gdu2 = 15
			ENDELSE

	;-----------------------------------------------------
	; AMB-ALT-OOB \\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\
	;-----------------------------------------------------
		;AMB-ALT-OOB
		;   - pitch    = alternating     = bit 2
		;   - [0,180]  = one-sided       = bit 5
		;   - 90       = one-sided       = bit 6
		;   - 90       = bi-directional  = bit 7
		ENDIF ELSE IF Array_Equal( MrBitGet( theBit, [2,5,6,7] ), 1) THEN BEGIN
			IF tf_brst THEN BEGIN
				iChan      = Reverse(Indgen(1,nChan), 2)
				ch_fa_gdu1 = Reverse(Indgen(1,nChan), 2)
				ch_fa_gdu2 = Reverse(Indgen(1,nChan), 2) + 12
				ch_90_gdu1 = Reverse(Indgen(1,nChan), 2)
				ch_90_gdu2 = Reverse(Indgen(1,nChan), 2) + 12
			ENDIF ELSE BEGIN
				ch_fa_gdu1 =  0
				ch_fa_gdu2 = 15
				ch_90_gdu1 =  0
				ch_90_gdu2 = 15
			ENDELSE

	;-----------------------------------------------------
	; Other \\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\
	;-----------------------------------------------------
		ENDIF ELSE BEGIN
			Message, 'Invalid operating mode. Bit=' + String(theBit, FORMAT='(i0)') + '.'
		ENDELSE

	;-----------------------------------------------------
	; Field-Aligned Channels \\\\\\\\\\\\\\\\\\\\\\\\\\\\\
	;-----------------------------------------------------
		IF N_Elements(ch_fa_gdu1) GT 0 THEN BEGIN
			iGDU1 = Where( oPitch_GDU1['DATA'] EQ 0 OR oPitch_GDU1['DATA'] EQ 180, nGDU1 )
			iGDU2 = Where( oPitch_GDU2['DATA'] EQ 0 OR oPitch_GDU2['DATA'] EQ 180, nGDU2 )
			
			IF nGDU1 GT 0 THEN anode_gdu1[idx[iGDU1],*] = Rebin( Temporary(ch_fa_gdu1), nGDU1, nChan ) + $
			                                              Rebin( N[idx[iGDU1]], nGDU1, nChan )
			IF nGDU2 GT 0 THEN anode_gdu2[idx[iGDU2],*] = Rebin( Temporary(ch_fa_gdu2), nGDU2, nChan ) - $
			                                              Rebin( N[idx[iGDU2]], nGDU2, nChan )
		ENDIF

	;-----------------------------------------------------
	; Perpendicular Channels \\\\\\\\\\\\\\\\\\\\\\\\\\\\\
	;-----------------------------------------------------
		IF N_Elements(ch_90_gdu1) GT 0 THEN BEGIN
			iGDU1 = Where( oPitch_GDU1['DATA'] EQ 90, nGDU1 )
			iGDU2 = Where( oPitch_GDU2['DATA'] EQ 90, nGDU2 )
			
			IF nGDU1 GT 0 THEN anode_gdu1[idx[iGDU1],*] = Rebin( Temporary(ch_90_gdu1), nGDU1, nChan ) + $
			                                              Rebin( N[idx[iGDU1]], nGDU1, nChan )
			IF nGDU2 GT 0 THEN anode_gdu2[idx[iGDU2],*] = Rebin( Temporary(ch_90_gdu2), nGDU2, nChan ) - $
			                                              Rebin( N[idx[iGDU2]], nGDU2, nChan )
		ENDIF
	ENDFOR
	
	;Force into range [0,31]
	anode_gdu1 = (anode_gdu1 + (anode_gdu1 lt 0)*32) mod 32
	anode_gdu2 = (anode_gdu2 + (anode_gdu2 lt 0)*32) mod 32

;-----------------------------------------------------
; Create Variables \\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\
;-----------------------------------------------------
	oIdx_GDU1 = MrTimeSeries( oPhi['TIMEVAR'], [ [Rebin(anode_gdu1, nPts, 1, nChan)], [Rebin(itheta_gdu1, nPts, 1, nChan)] ] )
	oIdx_GDU2 = MrTimeSeries( oPhi['TIMEVAR'], [ [Rebin(anode_gdu2, nPts, 1, nChan)], [Rebin(itheta_gdu2, nPts, 1, nChan)] ] )
	
	;IDX GDU1
	oIdx_GDU1['AXIS_RANGE']    = [0, 128]
	oIdx_GDU1['CATDESC']       = 'Anode number and polar index associated with the look direction of each channel of GDU1.'
	oIdx_GDU1['COLOR']         = ['Blue', 'Red']
	oIdx_GDU1['DISPLAY_TYPE']  = 'time_series'
	oIdx_GDU1['FIELDNAM']      = 'Anode number and polar index'
	oIdx_GDU1['FILLVAL']       = -1e31
	oIdx_GDU1['FORMAT']        = 'I4'
	oIdx_GDU1['LABEL']         = ['Phi', 'Theta']
	oIdx_GDU1['LABL_PTR_1']    = ['Phi', 'Theta']
	oIdx_GDU1['LABL_PTR_2']    = ['Ch1', 'Ch2', 'Ch3', 'Ch4']
	oIdx_GDU1['SCALETYP']      = 'linear'
	oIdx_GDU1['SI_CONVERSION'] = '>'
	oIdx_GDU1['UNITS']         = ''
	oIdx_GDU1['VALIDMAX']      = 0
	oIdx_GDU1['VALIDMIN']      = 128
	oIdx_GDU1['VAR_TYPE']      = 'support_data'
	
	;IDX GDU2
	oIdx_GDU2['AXIS_RANGE']    = [0, 128]
	oIdx_GDU2['CATDESC']       = 'Anode number and polar index associated with the look direction of each channel of GDU2.'
	oIdx_GDU2['COLOR']         = ['Blue', 'Red']
	oIdx_GDU2['DISPLAY_TYPE']  = 'time_series'
	oIdx_GDU2['FIELDNAM']      = 'Anode number and polar index'
	oIdx_GDU2['FILLVAL']       = -1e31
	oIdx_GDU2['FORMAT']        = 'I4'
	oIdx_GDU2['LABEL']         = ['Phi', 'Theta']
	oIdx_GDU2['LABL_PTR_1']    = ['Phi', 'Theta']
	oIdx_GDU2['LABL_PTR_2']    = ['Ch1', 'Ch2', 'Ch3', 'Ch4']
	oIdx_GDU2['SCALETYP']      = 'linear'
	oIdx_GDU2['SI_CONVERSION'] = '>'
	oIdx_GDU2['UNITS']         = ''
	oIdx_GDU2['VALIDMAX']      = 0
	oIdx_GDU2['VALIDMIN']      = 128
	oIdx_GDU2['VAR_TYPE']      = 'support_data'
END


;+
;
;       |-------------------------------------------------------|
;       |             |  pitch  |  pack  |  perp-     |  perp-  |
;       |             |  mode   |  mode  |  onesided  |  bidir  |
;       |-------------------------------------------------------|
;       | amb         |    0    |   0,1  |     --     |   --    |
;       | amb-pm2     |    0    |    2   |     --     |   --    |
;       | amb-alt-cc  |   1,3   |   0,1  |      0     |    0    |
;       | amb-alt-oc  |   1,3   |    2   |      0     |    0    |
;       | amb-alt-oom |   1,3   |    2   |      1     |    0    |
;       | amb-alt-oob |   1,3   |    2   |      1     |    1    |
;       | amb-perp    |    2    |        |            |         |
;       |-------------------------------------------------------------|
;       |                             Bit                             |
;       |-------------------------------------------------------------|
;       | amb         |   2^0   |   2^3  |      0     |    0    |   9 |
;       | amb-pm2     |   2^0   |   2^4  |      0     |    0    |  17 |
;       | amb-alt-cc  |   2^1   |   2^3  |      0     |    0    |  10 |
;       | amb-alt-oc  |   2^1   |   2^4  |      0     |    0    |  18 |
;       | amb-alt-oom |   2^1   |   2^4  |     2^5    |    0    |  50 |
;       | amb-alt-oob |   2^1   |   2^4  |     2^5    |   2^6   | 114 |
;       | amb-perp    |   2^2   |        |            |         |     |
;       |-------------------------------------------------------------|
;-
FUNCTION MrMMS_EDI_Load_Amb_L1A_Bitmask, t, pitch_mode, pack_mode, perp_oneside, perp_bidir, $
CACHE=cache, $
NAME=name
	Compile_Opt idl2
	On_Error, 2
	
	;Default name
	IF N_Elements(name) EQ 0 THEN name = 'mms_edi_ops_bitmask'
	
	;Extract Data
	oTime        = MrVar_Get(t)
	oPackMode    = MrVar_Get(pack_mode)
	oPitchMode   = MrVar_Get(pitch_mode)
	oPerpOneSide = MrVar_Get(perp_oneside)
	oPerpBiDir   = MrVar_Get(perp_bidir)
	
	;Check if time-series variable was given
	IF Obj_IsA(oTime, 'MrTimeSeries') THEN oTime = oTime['TIMEVAR']
	
	;Create bitmask
	nPackets = oPackMode -> GetNPts()
	bitmask  = BytArr(nPackets)
	
	;Do not set identical modes separately. One would unset the other.
	bitmask = MrBitSet( bitmask, 1, (oPitchMode['DATA']   EQ 0) )
	bitmask = MrBitSet( bitmask, 2, (oPitchMode['DATA']   EQ 1) or (oPitchMode['DATA'] EQ 3) ) ;PITCH_MODE 1,3 identical
	bitmask = MrBitSet( bitmask, 3, (oPitchMode['DATA']   EQ 2) )
	bitmask = MrBitSet( bitmask, 4, (oPackMode['DATA']    EQ 0) or (oPackMode['DATA']  EQ 1) ) ;PACMO 0,1 identical
	bitmask = MrBitSet( bitmask, 5, (oPackMode['DATA']    EQ 2) )
	bitmask = MrBitSet( bitmask, 6, (oPerpOneSide['DATA'] EQ 1) )
	bitmask = MrBitSet( bitmask, 7, (oPerpBiDir['DATA']   EQ 1) )

	;Look for counts outside of the packet time range
	iLT = oTime -> Where( oPitchMode['TIME', 0, 'TT2000'], 'TT2000', /LESS,    COUNT=nLT)
	iGT = oTime -> Where( oPitchMode['TIME', 0, 'TT2000'], 'TT2000', /GREATER, COUNT=nGT)
	IF nLT GT 0 THEN MrPrintF, 'LogWarn', nLT, FORMAT='(%"%i points before first packet time.")'
	IF nGT GT 0 THEN MrPrintF, 'LogWarn', nGT, FORMAT='(%"%i points after last packet time.")'
	
	;Expand the bits
	iBits   = oPitchMode['TIMEVAR'] -> Value_Locate(oTime['DATA', 'TT2000'], 'TT2000')
	bitmask = bitmask[iBits]

;-----------------------------------------------------
; Create Variables \\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\
;-----------------------------------------------------
	oBitMask = MrTimeSeries( oTime, bitmask, $
	                         CACHE = cache, $
	                         NAME  = name, $
	                         /NO_COPY )
	
	;Attributes
	newline                   = String(10B)
	oBitMask['AXIS_RANGE']    = [0, 255]
	oBitMask['CATDESC']       = 'Anode number and polar index associated with the look direction of GDU1.'
	oBitMask['COLOR']         = ['Blue', 'Red']
	oBitMask['DISPLAY_TYPE']  = 'time_series'
	oBitMask['FIELDNAM']      = 'Anode number and polar index'
	oBitMask['FILLVAL']       = -1e31
	oBitMask['FORMAT']        = 'I4'
	oBitMask['LABEL']         = ['Phi', 'Theta']
	oBitMask['SCALETYP']      = 'linear'
	oBitMask['SI_CONVERSION'] = '>'
	oBitMask['UNITS']         = ''
	oBitMask['VALIDMAX']      = 0
	oBitMask['VALIDMIN']      = 128
	oBitMask['VAR_TYPE']      = 'support_data'
	oBitMask['VAR_NOTES']     = 'Bitmask indicating EDI operating mode. Bit table:'               + newline + $
	                            ';-------------------------------------------------------|-----|' + newline + $
	                            ';             |               Bit                       |     |' + newline + $
	                            ';-------------------------------------------------------------|' + newline + $
	                            ';             |  pitch  |  pack  |  perp-     |  perp-  |     |' + newline + $
	                            '; OptDesc     |  mode   |  mode  |  onesided  |  bidir  | Val |' + newline + $
	                            ';-------------------------------------------------------|-----|' + newline + $
	                            '; amb         |   2^0   |   2^3  |      0     |    0    |   9 |' + newline + $
	                            '; amb-pm2     |   2^0   |   2^4  |      0     |    0    |  17 |' + newline + $
	                            '; amb-alt-cc  |   2^1   |   2^3  |      0     |    0    |  10 |' + newline + $
	                            '; amb-alt-oc  |   2^1   |   2^4  |      0     |    0    |  18 |' + newline + $
	                            '; amb-alt-oom |   2^1   |   2^4  |     2^5    |    0    |  50 |' + newline + $
	                            '; amb-alt-oob |   2^1   |   2^4  |     2^5    |   2^6   | 114 |' + newline + $
	                            '; amb-perp-om |   2^2   |    0   |     2^5    |    0    |  36 |' + newline + $
	                            '; amb-perp-ob |   2^2   |    0   |     2^5    |   2^6   | 100 |' + newline + $
	                            ';-------------------------------------------------------------|'
	
	RETURN, oBitmask
END



;+
;   Convert instrument look directions (unit vectors) from cartesian to spherical
;   coordinates.
;-
FUNCTION MrMMS_EDI_Load_Amb_L1A_cart2sphr, x, $
CACHE=cache, $
NAME=name
	Compile_Opt idl2
	On_Error, 2
	
	oX = MrVar_Get(x)
	
	;Azimuth
	phi = !radeg * ATan( oX['DATA', *, 1, *], oX['DATA', *, 0, *] )
	
	;Polar
	theta = !radeg * ACos( oX['DATA', *, 2, *] )
	
	;Trajectory
	traj = [ [ Temporary(phi) ], [ Temporary(theta) ] ]
	
	;Variable
	oTraj = MrTimeSeries( oX['TIMEVAR'], traj, $
	                      CACHE = cache, $
	                      NAME  = name, $
	                      /NO_COPY )
	
	RETURN, oTraj
END


;+
;   Convert instrument look directions (unit vectors) from cartesian to spherical
;   coordinates.
;-
FUNCTION MrMMS_EDI_Load_Amb_L1A_Sort, gdu1, gdu2, pa, pitch_gdu1, pitch_gdu2, $
EDI=edi, $
CACHE=cache, $
NAME=name
	Compile_Opt idl2
	On_Error, 2
	
	oGDU1       = MrVar_Get(gdu1)
	oGDU2       = MrVar_Get(gdu2)
	oPitch_GDU1 = MrVar_Get(pitch_gdu1)
	oPitch_GDU2 = MrVar_Get(pitch_gdu2)
	
	;Find correct pitch angle
	iGDU1 = (oPitch_GDU1 EQ pa ) -> Where(1, /EQUAL, COUNT=nGDU1)
	iGDU2 = (oPitch_GDU2 EQ pa ) -> Where(1, /EQUAL, COUNT=nGDU2)
	
	;BOTH GDUs
	IF nGDU1 GT 0 && nGDU2 GT 0 THEN BEGIN
		;Combine arrays
		oGDU_PA = oGDU1[iGDU1,*,*] -> Append( oGDU2[iGDU1,*,*], 1 )
		
		;Sort arrays
		iSort   = oGDU_PA['TIMEVAR'] -> Sort('TT2000')
		oGDU_PA = oGDU_PA[iSort,*,*]
		
		;Keep track of which GDU
		oEDI[iGDU1] = 1B
		oEDI[iGDU2] = 2B
	
	;GDU1
	ENDIF ELSE IF n0_GDU1 GT 0 THEN BEGIN
		oGDU_PA = oGDU_PA[iGDU1,*,*]
	
	;GDU2
	ENDIF ELSE IF n0_GDU2 GT 0 THEN BEGIN
		oGDU_PA = oGDU_PA[iGDU2,*,*]
	ENDIF
	
	RETURN, oGDU_PA
END


;+
;
;-
PRO MrMMS_EDI_Load_Amb_L1A, sc, mode, $
COORDS=coords, $
OPTDESC=optdesc, $
SUFFIX=suffix, $
TRANGE=trange, $
VARNAMES=varnames
	Compile_Opt idl2
	
	Catch, the_error
	IF the_error NE 0 THEN BEGIN
		Catch, /CANCEL
		MrPrintF, 'LogErr'
		RETURN
	ENDIF
	
	;Defaults
	IF N_Elements(coords) EQ 0 THEN coords  = ['edi', 'bcs', 'dbcs', 'gse', 'gsm']
	IF N_Elements(mode)   EQ 0 THEN mode    = 'fast'
	IF N_Elements(suffix) EQ 0 THEN suffix  = ''
	IF N_Elements(trange) GT 0 THEN MrVar_SetTRange, trange
	
	tf_coords = MrIsMember(['edi', 'bcs', 'dbcs', 'gse', 'gsm'], coords)
	IF ~Array_Equal(tf_coords, 1) THEN Message, 'Invalid coordinates specified.'
	IF Array_Equal(sc EQ ['mms1', 'mms2', 'mms3', 'mms4'], 0) THEN Message, 'SC must be {"mms1" | "mms2" | "mms3" | "mms4"}.'
	IF Array_Equal(mode EQ ['slow', 'fast', 'brst'], 0) THEN Message, 'MODE must be {"slow" | "fast" | "brst"}.'
	
	tf_edi  = tf_coords[0]
	tf_bcs  = tf_coords[1]
	tf_dbcs = tf_coords[2]
	tf_gse  = tf_coords[3]
	tf_gsm  = tf_coords[4]
	tf_coords = !Null
	
	;Constants
	dPhi   = 360.0 / 32.0
	dTheta = 360.0 / 512.0
	
;-------------------------------------------
; Which Dataset is Available? //////////////
;-------------------------------------------
	instr = 'edi'
	level = 'l1a'
	IF N_Elements(optdesc) EQ 0 THEN BEGIN
		;Find available datatypes
		files = MrMMS_Get_Filenames(sc, 'edi', mode, level)
		MrMMS_Parse_Filename, files, OPTDESC=optdesc
		
		;Pick one
		iuniq = Uniq(optdesc, Sort(optdesc))
		IF N_Elements(iuniq) GT 1 THEN BEGIN
			MrPrintF, 'LogWarn', 'More than one OPTDESC found. Choosing: "' + optdesc[0] + '".'
			optdesc = optdesc[0]
		ENDIF
	ENDIF

;-------------------------------------------
; Get Data /////////////////////////////////
;-------------------------------------------
	channel = mode EQ 'brst' ? ['1', '2', '3', '4'] : '1'
	nChan   = N_Elements(channel)
	
	;Variable names
	counts_gdu1_vname  = StrJoin( [sc, instr, 'amb', 'gdu1',     'raw', 'counts' ], '_' ) + channel
	counts_gdu2_vname  = StrJoin( [sc, instr, 'amb', 'gdu2',     'raw', 'counts' ], '_' ) + channel
	phi_vname          = StrJoin( [sc, instr, 'amb', 'phi'                       ], '_' )
	theta_vname        = StrJoin( [sc, instr, 'amb', 'theta'                     ], '_' )
	pack_mode_vname    = StrJoin( [sc, instr, 'amb', 'pacmo'                     ], '_' )
	pitch_mode_vname   = StrJoin( [sc, instr, 'amb', 'pitchmode'                 ], '_' )
	flip_vname         = StrJoin( [sc, instr,        'flip'                      ], '_' )
	pitch_gdu1_vname   = StrJoin( [sc, instr,        'pitch',    'gdu1'          ], '_' )
	pitch_gdu2_vname   = StrJoin( [sc, instr,        'pitch',    'gdu2'          ], '_' )
	perp_oneside_vname = StrJoin( [sc, instr, 'amb', 'perp',     'onesided'      ], '_' )
	perp_bidir_vname   = StrJoin( [sc, instr, 'amb', 'perp',     'bidirectional' ], '_' )
	
	;Derived names
	flux_0_vname        = StrJoin( [sc, instr, 'flux',          '0', mode, level], '_' )
	flux_90_vname       = StrJoin( [sc, instr, 'flux',         '90', mode, level], '_' )
	flux_180_vname      = StrJoin( [sc, instr, 'flux',        '180', mode, level], '_' )
	traj_edi_0_vname    = StrJoin( [sc, instr, 'traj', 'edi',   '0', mode, level], '_' )
	traj_edi_90_vname   = StrJoin( [sc, instr, 'traj', 'edi',  '90', mode, level], '_' )
	traj_edi_180_vname  = StrJoin( [sc, instr, 'traj', 'edi', '180', mode, level], '_' )
	traj_edi_0_vname    = StrJoin( [sc, instr, 'traj', 'edi',   '0', mode, level], '_' )
	traj_edi_90_vname   = StrJoin( [sc, instr, 'traj', 'edi',  '90', mode, level], '_' )
	traj_edi_180_vname  = StrJoin( [sc, instr, 'traj', 'edi', '180', mode, level], '_' )
	traj_bcs_0_vname    = StrJoin( [sc, instr, 'traj', 'bcs',   '0', mode, level], '_' )
	traj_bcs_90_vname   = StrJoin( [sc, instr, 'traj', 'bcs',  '90', mode, level], '_' )
	traj_bcs_180_vname  = StrJoin( [sc, instr, 'traj', 'bcs', '180', mode, level], '_' )
	traj_dbcs_0_vname   = StrJoin( [sc, instr, 'traj', 'dbcs',  '0', mode, level], '_' )
	traj_dbcs_90_vname  = StrJoin( [sc, instr, 'traj', 'dbcs', '90', mode, level], '_' )
	traj_dbcs_180_vname = StrJoin( [sc, instr, 'traj', 'dbcs','180', mode, level], '_' )
	traj_gse_0_vname    = StrJoin( [sc, instr, 'traj', 'gse',   '0', mode, level], '_' )
	traj_gse_90_vname   = StrJoin( [sc, instr, 'traj', 'gse',  '90', mode, level], '_' )
	traj_gse_180_vname  = StrJoin( [sc, instr, 'traj', 'gse', '180', mode, level], '_' )
	traj_gsm_0_vname    = StrJoin( [sc, instr, 'traj', 'gsm',   '0', mode, level], '_' )
	traj_gsm_90_vname   = StrJoin( [sc, instr, 'traj', 'gsm',  '90', mode, level], '_' )
	traj_gsm_180_vname  = StrJoin( [sc, instr, 'traj', 'gsm', '180', mode, level], '_' )
	
	;EDI
	MrMMS_Load_Data, sc, instr, mode, level, $
	                 OPTDESC   = optdesc, $
	                 VARFORMAT = ['*raw_counts*', '*phi', '*theta', '*pacmo', '*pitch*', $
	                              '*perp*', '*flip']
	
;-------------------------------------------
; Expand Variables /////////////////////////
;-------------------------------------------
	oCts1_GDU1 = MrVar_Get(counts_gdu1_vname[0])
	oTime      = oCts1_GDU1['TIMEVAR']
	oPhi       = MrVar_Get(phi_vname)
	oTheta     = MrVar_Get(theta_vname)
	nPts       = oCts1_GDU1 -> GetNPts()
	
	t_cts   = oCts1_GDU1['TIME', 'TT2000']
	t_angle = oPhi['TIME', 'TT2000']
	iAngle  = Value_Locate(t_angle, t_cts)
	
	;Report Extrapolation
	iLT     = Where(t_cts LT t_angle[0],  nLT)
	iGT     = Where(t_cts GT t_angle[-1], nGT)
	IF nLT GT 0 THEN MrPrintF, 'LogWarn', nLT, FORMAT='(%"%i angles extrapolated before interval.")'
	IF nGT GT 0 THEN MrPrintF, 'LogWarn', nGT, FORMAT='(%"%i angles extrapolated after interval.")'
	
	;Set Data
	oPhi   = oPhi[iAngle]
	oTheta = oTheta[iAngle]
	
;-------------------------------------------
; Which Mode ///////////////////////////////
;-------------------------------------------
	
	oBitmask = MrMMS_EDI_Load_Amb_L1A_Bitmask(counts_gdu1_vname[0], pitch_mode_vname, pack_mode_vname, perp_oneside_vname, perp_bidir_vname)
	
;-------------------------------------------
; Anode & Theta Index //////////////////////
;-------------------------------------------
	MrMMS_EDI_Load_Amb_L1A_Anode, oPhi, oTheta, oBitmask, pitch_gdu1_vname, pitch_gdu2_vname, $
	                              BRST = (mode EQ 'brst'), $
	                              GDU1 = oAnode_GDU1, $
	                              GDU2 = oAnode_GDU2

;-----------------------------------------------------
; Calibrate Counts \\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\
;-----------------------------------------------------
	oPitch_GDU1 = MrVar_Get(pitch_gdu1_vname)
	oPitch_GDU2 = MrVar_Get(pitch_gdu2_vname)
	oFlux_GDU1 = MrTimeSeries( oTime, FltArr(nPts, nChan) )
	oFlux_GDU2 = MrTimeSeries( oTime, FltArr(nPts, nChan) )

	;One-sided modes
	tf_onesided_gdu1 = ( ((oPitch_GDU1 EQ 0) OR (oPitch_GDU1 EQ 180)) AND MrBitGet(oBitmask['DATA'], 5) ) OR $
	                   ( (oPitch_GDU1 EQ 90) AND MrBitGet(oBitmask['DATA'], 6) )
	tf_onesided_gdu2 = ( ((oPitch_GDU2 EQ 0) OR (oPitch_GDU2 EQ 180)) AND MrBitGet(oBitmask['DATA'], 5) ) OR $
	                   ( (oPitch_GDU2 EQ 90) AND MrBitGet(oBitmask['DATA'], 6) )
	
	;Calibrate each channel
	FOR i = 0, nChan - 1 DO BEGIN
		;GDU1
		oFlux_GDU1[*,i] = MrMMS_EDI_Cal_Cts( sc, mode, optdesc, 'gdu1', counts_gdu1_vname[i], oAnode_GDU1[*,*,i], $
		                                     /ABSCAL, $
		                                     ONE_SIDED = tf_onesided_gdu1['DATA'] )
		
		;GDU2
		oFlux_GDU2[*,i] = MrMMS_EDI_Cal_Cts( sc, mode, optdesc, 'gdu2', counts_gdu2_vname[i], oAnode_GDU2[*,*,i], $
		                                     /ABSCAL, $
		                                     ONE_SIDED = tf_onesided_gdu2['DATA'] )
	ENDFOR

;-----------------------------------------------------
; Phi Values \\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\
;-----------------------------------------------------

	;BRST
	IF mode EQ 'brst' THEN BEGIN
		phi_gdu1 = ( Reform(oAnode_GDU1['DATA',*,0,*]) + 0.5) * dPhi
		phi_gdu2 = ( Reform(oAnode_GDU2['DATA',*,0,*]) + 0.5) * dPhi
	
	;SRVY
	ENDIF ELSE BEGIN
		phi_gdu1 = FltArr(nPts)
		phi_gdu2 = FltArr(nPts)
		
		;Separate one-sided and centered
		iCenter = Where( MrBitGet(oBitmask['DATA'], 5) OR MrBitGet(oBitmask['DATA'], 6), nCenter, $
		                 COMPLEMENT  = iOneSide, $
		                 NCOMPLEMENT = nOneSide )
		
		;Center
		IF nCenter GT 0 THEN BEGIN
			phi_gdu1[iCenter] = N[iCenter] * dPhi
			phi_gdu2[iCenter] = (16 - N[iCenter]) * dPhi
		ENDIF
		
		;One-Sided
		IF nOneSide GT 0 THEN BEGIN
			phi_gdu1[iOneSide] = (N[iOneSide] + 0.5) * dPhi
			phi_gdu2[iOneSide] = (15.5 - N[iOneSide]) * dPhi
		ENDIF
		
;		FOR i = 0, nModes - 1 DO BEGIN
;			theBit = modeBit[iUniq[i]]
;			idx    = Where(modeBit EQ theBit, nIdx)
;			
;			;AMB
;			;   - pitch   = field-aligned = bit 1
;			;   - [0,180] = centered      = bit 4
;			IF Array_Equal( MrBitGet( theBit, [1,4] ), 1)
;				phi_gdu1[idx] = N[idx] * 11.25
;				phi_gdu2[idx] = (16 - N[idx]) * 11.25
;			
;			;AMB-PM2
;			;   - pitch   = field-aligned = bit 1
;			;   - [0,180] = one-sided     = bit 5
;			ENDIF ELSE IF Array_Equal( MrBitGet( theBit, [1,5] ), 1) THEN BEGIN
;				phi_gdu1[idx] = (N[idx] + 0.5) * 11.25
;				phi_gdu2[idx] = (15.5 - N[idx]) * 11.25
;			
;			;AMB-ALT-CC
;			;   - pitch    = alternating = bit 2
;			;   - [0,180]  = centered    = bit 4
;			;   - 90       = centered    = bit -
;			ENDIF ELSE IF Array_Equal( MrBitGet( theBit, [2,4] ), 1) THEN BEGIN
;				phi_gdu1[idx] = N[idx] * 11.25
;				phi_gdu2[idx] = (16 - N[idx]) * 11.25
;			
;			;AMB-ALT-OC
;			;   - pitch    = alternating = bit 2
;			;   - [0,180]  = one-sided   = bit 5
;			;   - 90       = centered    = bit -
;			ENDIF ELSE IF Array_Equal( MrBitGet( theBit, [2,5] ), 1) THEN BEGIN
;				iFA = Where( oPitch_GDU1['DATA',idx] EQ 0 OR oPitch_GDU1['DATA',idx] EQ 180, nFA, $
;				             COMPLEMENT=i90, NCOMPLEMENT=n90 )
;				
;				;One-Sided
;				IF nFA GT 0 THEN BEGIN
;					phi_gdu1[idx[iFA]] = (N[idx[iFA]] + 0.5) * 11.25
;					phi_gdu2[idx[iFA]] = (15.5 - N[idx[iFA]]) * 11.25
;				ENDIF
;				
;				;Centered
;				IF n90 GT 0 THEN BEGIN
;					phi_gdu1[idx[i90]] = N[idx[i90]] * 11.25
;					phi_gdu2[idx[i90]] = (16 - N[idx[i90]]) * 11.25
;				ENDIF
;			
;			;AMB-ALT-OOM
;			;   - pitch    = alternating       = bit 2
;			;   - [0,180]  = one-sided         = bit 5
;			;   - 90       = one-sided         = bit 6
;			;   - 90       = moni-directional  = bit -
;			ENDIF ELSE IF Array_Equal( MrBitGet( theBit, [2,5,6] ), 1) THEN BEGIN
;				phi_gdu1[idx] = (N[idx] + 0.5) * 11.25
;				phi_gdu2[idx] = (15.5 - N[idx]) * 11.25
;				
;			;AMB-ALT-OOB
;			;   - pitch    = alternating     = bit 2
;			;   - [0,180]  = one-sided       = bit 5
;			;   - 90       = one-sided       = bit 6
;			;   - 90       = bi-directional  = bit 7
;			ENDIF ELSE IF Array_Equal( MrBitGet( theBit, [2,5,6,7] ), 1) THEN BEGIN
;				phi_gdu1[idx] = (N[idx] + 0.5) * 11.25
;				phi_gdu2[idx] = (15.5 - N[idx]) * 11.25
;			
;			;OTHER
;			ENDIF ELSE BEGIN
;				Message, 'Invalid operating mode. Bit=' + String(theBit, FORMAT='(i0)') + '.'
;			ENDELSE
;		ENDFOR
	ENDELSE

	oTraj_GDU1 = MrTimeSeries( oTime, Transpose( [ [ [phi_gdu1] ], [ [Rebin(oTheta['DATA'], nPts, nChan) ] ] ], [0,2,1] ) )
	oTraj_GDU2 = MrTimeSeries( oTime, Transpose( [ [ [phi_gdu2] ], [ [Rebin(oTheta['DATA'], nPts, nChan) ] ] ], [0,2,1] ) )

;-----------------------------------------------------
; EDI -> BCS \\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\
;-----------------------------------------------------
	deg2rad  = !dpi / 180D
	phi_gdu1 = deg2rad * phi_gdu1
	phi_gdu2 = deg2rad * phi_gdu2
	theta    = deg2rad * Rebin(oTheta['DATA'], nPts, nChan)

	x1 = Sin( theta ) * Cos( phi_gdu1 )
	y1 = Sin( theta ) * Sin( phi_gdu1 )
	z1 = Cos( theta )
	
	x2 = Sin( theta ) * Cos( phi_gdu2 )
	y2 = Sin( theta ) * Sin( phi_gdu2 )
	z2 = Cos( theta )
	
	phi   = !Null
	theta = !Null
	
	;Create vectors
	;   - Negate to change look directions to incident vectors
	oX_GDU1 = MrTimeSeries( oTime, Transpose( [ [ [ x1 ] ], [ [ y1 ] ], [ [ z1 ] ] ], [0,2,1] ) )
	oX_GDU2 = MrTimeSeries( oTime, Transpose( [ [ [ x2 ] ], [ [ y2 ] ], [ [ z2 ] ] ], [0,2,1] ) )
	
	;Rotation from EDI1 to BCS
	alpha1 = -90 * !dpi/180.0
	alpha2 = 221 * !dpi/180.0
	r1 = [ [  Cos(alpha1),       0,      -Sin(alpha1) ], $
	       [        0,           1,             0     ], $
	       [  Sin(alpha1),       0,       Cos(alpha1) ] ]
	r2 = [ [  Cos(alpha2),  Sin(alpha2),        0 ], $
	       [ -Sin(alpha2),  Cos(alpha2),        0 ], $
	       [        0,           0,             1 ] ]
	edi1_to_bcs = Temporary(r2) ## Temporary(r1)
	
	;Rotation from EDI2 to BCS
	alpha1 = -90 * !dpi/180.0
	alpha2 =  41 * !dpi/180.0
	r1 = [ [  Cos(alpha1),       0,      -Sin(alpha1) ], $
	       [        0,           1,             0     ], $
	       [  Sin(alpha1),       0,       Cos(alpha1) ] ]
	r2 = [ [  Cos(alpha2),  Sin(alpha2),        0 ], $
	       [ -Sin(alpha2),  Cos(alpha2),        0 ], $
	       [        0,           0,             1 ] ]
	edi2_to_bcs = Temporary(r2) ## Temporary(r1)

	;Rotate to BCS
	;   - Detector 1 (here called GDU1) is on EDI #2
	;   - Detector 2 (here called GDU2) is on EDI #1
	x_bcs_gdu1 = FltArr( nPts, 3, nChan )
	x_bcs_gdu2 = FltArr( nPts, 3, nChan )
	FOR i = 0, nChan - 1 DO BEGIN
		x_bcs_gdu1[0,0,i] = edi2_to_bcs ## Reform(oX_GDU1['DATA',*,*,i])
		x_bcs_gdu2[0,0,i] = edi1_to_bcs ## Reform(oX_GDU2['DATA',*,*,i])
	ENDFOR


;	nPts1 = oX_GDU1 -> GetNPts()
;	nPts2 = oX_GDU2 -> GetNPts()
;	x_bcs_gdu1 = FltArr( 3, nPts1 )
;	x_bcs_gdu2 = FltArr( 3, nPts2 )
;	FOREACH vec, oX_GDU1, i DO x_bcs_gdu1[0,i] = Reform(edi2_to_bcs ## vec)
;	FOREACH vec, oX_GDU2, i DO x_bcs_gdu2[0,i] = Reform(edi1_to_bcs ## vec)
	
	;Create vectors
	oX_BCS_GDU1 = MrTimeSeries(oTime, x_bcs_gdu1, /NO_COPY)
	oX_BCS_GDU2 = MrTimeSeries(oTime, x_bcs_gdu2, /NO_COPY)

	;Delete variables
	r1     = !Null
	r2     = !Null
	alpha1 = !Null
	alpha2 = !Null
	Obj_Destroy, [oX_GDU1, oX_GDU2]

;-------------------------------------------
; Separate by PA ///////////////////////////
;-------------------------------------------
	;Fluxes
	oFlux_0    = MrMMS_EDI_Load_Amb_L1A_Sort( oFlux_GDU1,  oFlux_GDU2,    0, oPitch_GDU1, oPitch_GDU2 )
	oFlux_90   = MrMMS_EDI_Load_Amb_L1A_Sort( oFlux_GDU1,  oFlux_GDU2,   90, oPitch_GDU1, oPitch_GDU2 )
	oFlux_180  = MrMMS_EDI_Load_Amb_L1A_Sort( oFlux_GDU1,  oFlux_GDU2,  180, oPitch_GDU1, oPitch_GDU2 )
	
	;Look directions
	oX_0_BCS   = MrMMS_EDI_Load_Amb_L1A_Sort( oX_BCS_GDU1, oX_BCS_GDU2,   0, oPitch_GDU1, oPitch_GDU2 )
	oX_90_BCS  = MrMMS_EDI_Load_Amb_L1A_Sort( oX_BCS_GDU1, oX_BCS_GDU2,  90, oPitch_GDU1, oPitch_GDU2 )
	oX_180_BCS = MrMMS_EDI_Load_Amb_L1A_Sort( oX_BCS_GDU1, oX_BCS_GDU2, 180, oPitch_GDU1, oPitch_GDU2 )

;-------------------------------------------
; Rotate to GSE ////////////////////////////
;-------------------------------------------
	oX_0_DBCS   = MrTimeSeries( oTime, FltArr(nPts, 3, nChan) )
	oX_0_GSE    = MrTimeSeries( oTime, FltArr(nPts, 3, nChan) )
	oX_0_GSM    = MrTimeSeries( oTime, FltArr(nPts, 3, nChan) )
	oX_90_DBCS  = MrTimeSeries( oTime, FltArr(nPts, 3, nChan) )
	oX_90_GSE   = MrTimeSeries( oTime, FltArr(nPts, 3, nChan) )
	oX_90_GSM   = MrTimeSeries( oTime, FltArr(nPts, 3, nChan) )
	oX_180_DBCS = MrTimeSeries( oTime, FltArr(nPts, 3, nChan) )
	oX_180_GSE  = MrTimeSeries( oTime, FltArr(nPts, 3, nChan) )
	oX_180_GSM  = MrTimeSeries( oTime, FltArr(nPts, 3, nChan) )
	
	FOR i = 0, nChan - 1 DO BEGIN
		;PA=0
		MrMMS_Bromund_Rotate, oX_0_BCS[*,*,i], sc, $
		                      DEFATT     = defatt, $
		                      DEFEPH     = defeph, $
		                      /DESPIN, $
		                      DESPUN     = oTemp_0_DBCS, $
		                      GSE        = oTemp_0_GSE, $
		                      GSM        = oTemp_0_GSM, $
		                      TYPE       = 'Z'
	
		;PA=90
		MrMMS_Bromund_Rotate, oX_90_BCS[*,*,i], $
		                      DEFATT     = defatt, $
		                      DEFEPH     = defeph, $
		                      /DESPIN, $
		                      DESPUN     = oTemp_90_DBCS, $
		                      GSE        = oTemp_90_GSE, $
		                      GSM        = oTemp_90_GSM, $
		                      TYPE       = 'Z'
	
		;PA=180
		MrMMS_Bromund_Rotate, oX_180_BCS[*,*,i], $
		                      DEFATT     = defatt, $
		                      DEFEPH     = defeph, $
		                      /DESPIN, $
		                      DESPUN     = oTemp_180_DBCS, $
		                      GSE        = oTemp_180_GSE, $
		                      GSM        = oTemp_180_GSM, $
		                      TYPE       = 'Z'
		
		;Store data
		oX_0_DBCS[0,0,i] = Temporary( oTemp_0_DBCS )
		oX_0_GSE[0,0,i]  = Temporary( oTemp_0_GSE  )
		oX_0_GSM[0,0,i]  = Temporary( oTemp_0_GSM  )
		
		oX_90_DBCS[0,0,i] = Temporary( oTemp_0_DBCS )
		oX_90_GSE[0,0,i]  = Temporary( oTemp_0_GSE  )
		oX_90_GSM[0,0,i]  = Temporary( oTemp_0_GSM  )
		
		oX_180_DBCS[0,0,i] = Temporary( oTemp_0_DBCS )
		oX_180_GSE[0,0,i]  = Temporary( oTemp_0_GSE  )
		oX_180_GSM[0,0,i]  = Temporary( oTemp_0_GSM  )
	ENDFOR
	
	;Free memory
	defatt = !Null
	defeph = !Null

;-------------------------------------------
; Split into Theta & Phi ///////////////////
;-------------------------------------------
	;BCS
	IF tf_bcs THEN BEGIN
		oTraj_BCS_GDU1 = MrMMS_EDI_Load_Amb_L1A_cart2sphr( oX_DBCS_GDU1 )
		oTraj_BCS_GDU2 = MrMMS_EDI_Load_Amb_L1A_cart2sphr( oX_DBCS_GDU2 )
	ENDIF
	Obj_Destroy, [ oX_BCS_GDU1, oX_BCS_GDU2 ]
	
	;DBCS
	IF tf_dbcs THEN BEGIN
		oTraj_DBCS_GDU1 = MrMMS_EDI_Load_Amb_L1A_cart2sphr( oX_DDBCS_GDU1 )
		oTraj_DBCS_GDU2 = MrMMS_EDI_Load_Amb_L1A_cart2sphr( oX_DDBCS_GDU2 )
	ENDIF
	Obj_Destroy, [ oX_DBCS_GDU1, oX_DBCS_GDU2 ]
	
	;GSE
	IF tf_gse THEN BEGIN
		oTraj_GSE_GDU1 = MrMMS_EDI_Load_Amb_L1A_cart2sphr( oX_GSE_GDU1 )
		oTraj_GSE_GDU2 = MrMMS_EDI_Load_Amb_L1A_cart2sphr( oX_GSE_GDU2 )
	ENDIF
	Obj_Destroy, [ oX_GSE_GDU1, oX_GSE_GDU2 ]
	
	;GSM
	IF tf_gsm THEN BEGIN
		oTraj_GSM_GDU1 = MrMMS_EDI_Load_Amb_L1A_cart2sphr( oX_GSM_GDU1 )
		oTraj_GSM_GDU2 = MrMMS_EDI_Load_Amb_L1A_cart2sphr( oX_GSM_GDU2 )
	ENDIF
	Obj_Destroy, [ oX_GSM_GDU1, oX_GSM_GDU2 ]

;-------------------------------------------
; Split by Pitch Angle /////////////////////
;-------------------------------------------
	;
	; Do not worry about which L2 data product the result is. Combine
	; all 0, 90, 180 degree pitch angle data into their respective arrays.
	;

	i0   = (oPitch_GDU1 EQ   0 AND oPitch_GDU2 EQ   0) -> Where(1, /EQUAL, COUNT=n0)
	i90  = (oPitch_GDU1 EQ  90 AND oPitch_GDU2 EQ  90) -> Where(1, /EQUAL, COUNT=n90)
	i180 = (oPitch_GDU1 EQ 180 AND oPitch_GDU2 EQ 180) -> Where(1, /EQUAL, COUNT=n180)

	;PA = 0
	IF n0 GT 0 THEN BEGIN
		;GDU1
		oFlux_0_GDU1      = oFlux_GDU1[i0,*]
		oTraj_0_GDU1      = oTraj_GDU1[i0,*,*]
		oTraj_0_BCS_GDU1  = oTraj_BCS_GDU1[i0,*,*]
		oTraj_0_DBCS_GDU1 = oTraj_DBCS_GDU1[i0,*,*]
		oTraj_0_GSE_GDU1  = oTraj_GSE_GDU1[i0,*,*]
		oTraj_0_GSM_GDU1  = oTraj_GSM_GDU1[i0,*,*]
		
		oFlux_0_GDU1      -> SetName, flux_0_vname
		oTraj_0_GDU1      -> SetName, traj_edi_0_vname
		oTraj_0_BCS_GDU1  -> SetName, traj_bcs_0_vname
		oTraj_0_DBCS_GDU1 -> SetName, traj_dbcs_0_vname
		oTraj_0_GSE_GDU1  -> SetName, traj_gse_0_vname
		oTraj_0_GSM_GDU1  -> SetName, traj_gsm_0_vname
		
		oFlux_0_GDU1      -> Cache
		oTraj_0_GDU1      -> Cache
		oTraj_0_BCS_GDU1  -> Cache
		oTraj_0_DBCS_GDU1 -> Cache
		oTraj_0_GSE_GDU1  -> Cache
		oTraj_0_GSM_GDU1  -> Cache
		
		;GDU2
		oFlux_0_GDU2      = oFlux_GDU2[i0,*]
		oTraj_0_GDU2      = oTraj_GDU2[i0,*,*]
		oTraj_0_BCS_GDU2  = oTraj_BCS_GDU2[i0,*,*]
		oTraj_0_DBCS_GDU2 = oTraj_DBCS_GDU2[i0,*,*]
		oTraj_0_GSE_GDU2  = oTraj_GSE_GDU2[i0,*,*]
		oTraj_0_GSM_GDU2  = oTraj_GSM_GDU2[i0,*,*]
		
		oFlux_0_GDU2      -> SetName, flux_0_vname
		oTraj_0_GDU2      -> SetName, traj_edi_0_vname
		oTraj_0_BCS_GDU2  -> SetName, traj_bcs_0_vname
		oTraj_0_DBCS_GDU2 -> SetName, traj_dbcs_0_vname
		oTraj_0_GSE_GDU2  -> SetName, traj_gse_0_vname
		oTraj_0_GSM_GDU2  -> SetName, traj_gsm_0_vname
		
		oFlux_0_GDU2      -> Cache
		oTraj_0_GDU2      -> Cache
		oTraj_0_BCS_GDU2  -> Cache
		oTraj_0_DBCS_GDU2 -> Cache
		oTraj_0_GSE_GDU2  -> Cache
		oTraj_0_GSM_GDU2  -> Cache
	ENDIF
	
	;PA = 90
	IF n90 GT 0 THEN BEGIN
		;GDU1
		oFlux_90_GDU1      = oFlux_GDU1[i90,*]
		oTraj_90_GDU1      = oTraj_GDU1[i90,*,*]
		oTraj_90_BCS_GDU1  = oTraj_BCS_GDU1[i90,*,*]
		oTraj_90_DBCS_GDU1 = oTraj_DBCS_GDU1[i90,*,*]
		oTraj_90_GSE_GDU1  = oTraj_GSE_GDU1[i90,*,*]
		oTraj_90_GSM_GDU1  = oTraj_GSM_GDU1[i90,*,*]
		
		oFlux_90_GDU1      -> SetName, flux_90_vname
		oTraj_90_GDU1      -> SetName, traj_edi_90_vname
		oTraj_90_BCS_GDU1  -> SetName, traj_bcs_90_vname
		oTraj_90_DBCS_GDU1 -> SetName, traj_dbcs_90_vname
		oTraj_90_GSE_GDU1  -> SetName, traj_gse_90_vname
		oTraj_90_GSM_GDU1  -> SetName, traj_gsm_90_vname
		
		oFlux_90_GDU1      -> Cache
		oTraj_90_GDU1      -> Cache
		oTraj_90_BCS_GDU1  -> Cache
		oTraj_90_DBCS_GDU1 -> Cache
		oTraj_90_GSE_GDU1  -> Cache
		oTraj_90_GSM_GDU1  -> Cache
		
		;GDU2
		oFlux_90_GDU2      = oFlux_GDU2[i90,*]
		oTraj_90_GDU2      = oTraj_GDU2[i90,*,*]
		oTraj_90_BCS_GDU2  = Traj_BCS_GDU2[i90,*,*]
		oTraj_90_DBCS_GDU2 = Traj_DBCS_GDU2[i90,*,*]
		oTraj_90_GSE_GDU2  = Traj_GSE_GDU2[i90,*,*]
		oTraj_90_GSM_GDU2  = Traj_GSM_GDU2[i90,*,*]
		
		oFlux_90_GDU2      -> SetName, flux_90_vname
		oTraj_90_GDU2      -> SetName, traj_edi_90_vname
		oTraj_90_BCS_GDU2  -> SetName, traj_bcs_90_vname
		oTraj_90_DBCS_GDU2 -> SetName, traj_dbcs_90_vname
		oTraj_90_GSE_GDU2  -> SetName, traj_gse_90_vname
		oTraj_90_GSM_GDU2  -> SetName, traj_gsm_90_vname
		
		oFlux_90_GDU2      -> Cache
		oTraj_90_GDU2      -> Cache
		oTraj_90_BCS_GDU2  -> Cache
		oTraj_90_DBCS_GDU2 -> Cache
		oTraj_90_GSE_GDU2  -> Cache
		oTraj_90_GSM_GDU2  -> Cache
	ENDIF
	
	;PA = 180
	IF n0 GT 0 THEN BEGIN
		;GDU1
		oFlux_180_GDU1      = oFlux_GDU1[i180,*]
		oTraj_180_GDU1      = oTraj_GDU1[i180,*,*]
		oTraj_180_BCS_GDU1  = oTraj_BCS_GDU1[i180,*,*]
		oTraj_180_DBCS_GDU1 = oTraj_DBCS_GDU1[i180,*,*]
		oTraj_180_GSE_GDU1  = oTraj_GSE_GDU1[i180,*,*]
		oTraj_180_GSM_GDU1  = oTraj_GSM_GDU1[i180,*,*]
		
		oFlux_180_GDU1      -> SetName, flux_180_vname
		oTraj_180_GDU1      -> SetName, traj_edi_180_vname
		oTraj_180_BCS_GDU1  -> SetName, traj_bcs_180_vname
		oTraj_180_DBCS_GDU1 -> SetName, traj_dbcs_180_vname
		oTraj_180_GSE_GDU1  -> SetName, traj_gse_180_vname
		oTraj_180_GSM_GDU1  -> SetName, traj_gsm_180_vname
		
		oFlux_180_GDU1      -> Cache
		oTraj_180_GDU1      -> Cache
		oTraj_180_BCS_GDU1  -> Cache
		oTraj_180_DBCS_GDU1 -> Cache
		oTraj_180_GSE_GDU1  -> Cache
		oTraj_180_GSM_GDU1  -> Cache
		
		;GDU2
		oFlux_180_GDU2      = oFlux_GDU2[i180,*]
		oTraj_180_GDU2      = oTraj_GDU2[i180,*,*]
		oTraj_180_BCS_GDU2  = oTraj_BCS_GDU2[i180,*,*]
		oTraj_180_DBCS_GDU2 = oTraj_DBCS_GDU2[i180,*,*]
		oTraj_180_GSE_GDU2  = oTraj_GSE_GDU2[i180,*,*]
		oTraj_180_GSM_GDU2  = oTraj_GSM_GDU2[i180,*,*]
		
		oFlux_180_GDU2      -> SetName, flux_180_vname
		oTraj_180_GDU2      -> SetName, traj_edi_180_vname
		oTraj_180_BCS_GDU2  -> SetName, traj_bcs_180_vname
		oTraj_180_DBCS_GDU2 -> SetName, traj_dbcs_180_vname
		oTraj_180_GSE_GDU2  -> SetName, traj_gse_180_vname
		oTraj_180_GSM_GDU2  -> SetName, traj_gsm_180_vname
		
		oFlux_180_GDU2      -> Cache
		oTraj_180_GDU2      -> Cache
		oTraj_180_BCS_GDU2  -> Cache
		oTraj_180_DBCS_GDU2 -> Cache
		oTraj_180_GSE_GDU2  -> Cache
		oTraj_180_GSM_GDU2  -> Cache
	ENDIF

;-------------------------------------------
; Attributes ///////////////////////////////
;-------------------------------------------
	traj_delta = mode eq 'brst' ? 9.0 : 13.0
	traj_notes = 'Trajectories are given as unit vectors in spherical coordinates, with phi ' + $
	             '(theta) representing the azimuthal (polar) directions, in the ' + $
	             'indicated coordinate system. They are opposite to the nominal look-direction ' + $
	             "of the instrument. Errors represent an omni-directional error. For more " + $
	             'details about errors, contact the EDI instrument team.'
	
	;TRAJ GDU1
	oTraj_GDU1['AXIS_RANGE']    = [-180.0, 180.0]
	oTraj_GDU1['CATDESC']       = 'Incident trajectories of EDI electrons in instrument coordinates measured by GDU1.'
	oTraj_GDU1['COLOR']         = ['Blue', 'Red']
	oTraj_GDU1['DELTA_PLUS']    = traj_delta
	oTraj_GDU1['DELTA_MINUS']   = traj_delta
	oTraj_GDU1['DISPLAY_TYPE']  = 'time_series'
	oTraj_GDU1['FIELDNAM']      = 'Incident Traj'
	oTraj_GDU1['FILLVAL']       = -1e31
	oTraj_GDU1['FORMAT']        = 'F9.4'
	oTraj_GDU1['LABEL']         = ['Phi', 'Theta']
	oTraj_GDU1['SCALETYP']      = 'linear'
	oTraj_GDU1['SI_CONVERSION'] = '0.01745>rad'
	oTraj_GDU1['UNITS']         = 'degrees'
	oTraj_GDU1['VALIDMAX']      = -180.0
	oTraj_GDU1['VALIDMIN']      = 180.0
	oTraj_GDU1['VAR_TYPE']      = 'data'
	oTraj_GDU1['VAR_NOTES']     = traj_notes
	
	;TRAJ GDU1
	oTraj_BCS_GDU1['AXIS_RANGE']    = [-180.0, 180.0]
	oTraj_BCS_GDU1['CATDESC']       = 'Incident trajectories of EDI electrons in BCS coordinates measured by GDU1.'
	oTraj_BCS_GDU1['COLOR']         = ['Blue', 'Red']
	oTraj_BCS_GDU1['DELTA_PLUS']    = traj_delta
	oTraj_BCS_GDU1['DELTA_MINUS']   = traj_delta
	oTraj_BCS_GDU1['DISPLAY_TYPE']  = 'time_series'
	oTraj_BCS_GDU1['FIELDNAM']      = 'Incident Traj'
	oTraj_BCS_GDU1['FILLVAL']       = -1e31
	oTraj_BCS_GDU1['FORMAT']        = 'F9.4'
	oTraj_BCS_GDU1['LABEL']         = ['Phi', 'Theta']
	oTraj_BCS_GDU1['SCALETYP']      = 'linear'
	oTraj_BCS_GDU1['SI_CONVERSION'] = '0.01745>rad'
	oTraj_BCS_GDU1['UNITS']         = 'degrees'
	oTraj_BCS_GDU1['VALIDMAX']      = -180.0
	oTraj_BCS_GDU1['VALIDMIN']      = 180.0
	oTraj_BCS_GDU1['VAR_TYPE']      = 'data'
	oTraj_BCS_GDU1['VAR_NOTES']     = traj_notes
	
	;TRAJ GDU1
	oTraj_DBCS_GDU1['AXIS_RANGE']    = [-180.0, 180.0]
	oTraj_DBCS_GDU1['CATDESC']       = 'Incident trajectories of EDI electrons in DBCS coordinates measured by GDU1.'
	oTraj_DBCS_GDU1['COLOR']         = ['Blue', 'Red']
	oTraj_DBCS_GDU1['DELTA_PLUS']    = traj_delta
	oTraj_DBCS_GDU1['DELTA_MINUS']   = traj_delta
	oTraj_DBCS_GDU1['DISPLAY_TYPE']  = 'time_series'
	oTraj_DBCS_GDU1['FIELDNAM']      = 'Incident Traj'
	oTraj_DBCS_GDU1['FILLVAL']       = -1e31
	oTraj_DBCS_GDU1['FORMAT']        = 'F9.4'
	oTraj_DBCS_GDU1['LABEL']         = ['Phi', 'Theta']
	oTraj_DBCS_GDU1['SCALETYP']      = 'linear'
	oTraj_DBCS_GDU1['SI_CONVERSION'] = '0.01745>rad'
	oTraj_DBCS_GDU1['UNITS']         = 'degrees'
	oTraj_DBCS_GDU1['VALIDMAX']      = -180.0
	oTraj_DBCS_GDU1['VALIDMIN']      = 180.0
	oTraj_DBCS_GDU1['VAR_TYPE']      = 'data'
	oTraj_DBCS_GDU1['VAR_NOTES']     = traj_notes
	
	;TRAJ GDU1
	oTraj_GSE_GDU1['AXIS_RANGE']    = [-180.0, 180.0]
	oTraj_GSE_GDU1['CATDESC']       = 'Incident trajectories of EDI electrons in GSE coordinates measured by GDU1.'
	oTraj_GSE_GDU1['COLOR']         = ['Blue', 'Red']
	oTraj_GSE_GDU1['DELTA_PLUS']    = traj_delta
	oTraj_GSE_GDU1['DELTA_MINUS']   = traj_delta
	oTraj_GSE_GDU1['DISPLAY_TYPE']  = 'time_series'
	oTraj_GSE_GDU1['FIELDNAM']      = 'Incident Traj'
	oTraj_GSE_GDU1['FILLVAL']       = -1e31
	oTraj_GSE_GDU1['FORMAT']        = 'F9.4'
	oTraj_GSE_GDU1['LABEL']         = ['Phi', 'Theta']
	oTraj_GSE_GDU1['SCALETYP']      = 'linear'
	oTraj_GSE_GDU1['SI_CONVERSION'] = '0.01745>rad'
	oTraj_GSE_GDU1['UNITS']         = 'degrees'
	oTraj_GSE_GDU1['VALIDMAX']      = -180.0
	oTraj_GSE_GDU1['VALIDMIN']      = 180.0
	oTraj_GSE_GDU1['VAR_TYPE']      = 'data'
	oTraj_GSE_GDU1['VAR_NOTES']     = traj_notes
	
	;TRAJ GDU1
	oTraj_GSM_GDU1['AXIS_RANGE']    = [-180.0, 180.0]
	oTraj_GSM_GDU1['CATDESC']       = 'Incident trajectories of EDI electrons in GSM coordinates measured by GDU1.'
	oTraj_GSM_GDU1['COLOR']         = ['Blue', 'Red']
	oTraj_GSM_GDU1['DELTA_PLUS']    = traj_delta
	oTraj_GSM_GDU1['DELTA_MINUS']   = traj_delta
	oTraj_GSM_GDU1['DISPLAY_TYPE']  = 'time_series'
	oTraj_GSM_GDU1['FIELDNAM']      = 'Incident Traj'
	oTraj_GSM_GDU1['FILLVAL']       = -1e31
	oTraj_GSM_GDU1['FORMAT']        = 'F9.4'
	oTraj_GSM_GDU1['LABEL']         = ['Phi', 'Theta']
	oTraj_GSM_GDU1['SCALETYP']      = 'linear'
	oTraj_GSM_GDU1['SI_CONVERSION'] = '0.01745>rad'
	oTraj_GSM_GDU1['UNITS']         = 'degrees'
	oTraj_GSM_GDU1['VALIDMAX']      = -180.0
	oTraj_GSM_GDU1['VALIDMIN']      = 180.0
	oTraj_GSM_GDU1['VAR_TYPE']      = 'data'
	oTraj_GSM_GDU1['VAR_NOTES']     = traj_notes
	
	;TRAJ GDU1
	oTraj_BCS_GDU2['AXIS_RANGE']    = [-180.0, 180.0]
	oTraj_BCS_GDU2['CATDESC']       = 'Incident trajectories of EDI electrons in BCS coordinates measured by GDU2.'
	oTraj_BCS_GDU2['COLOR']         = ['Blue', 'Red']
	oTraj_BCS_GDU2['DELTA_PLUS']    = traj_delta
	oTraj_BCS_GDU2['DELTA_MINUS']   = traj_delta
	oTraj_BCS_GDU2['DISPLAY_TYPE']  = 'time_series'
	oTraj_BCS_GDU2['FIELDNAM']      = 'Incident Traj'
	oTraj_BCS_GDU2['FILLVAL']       = -1e31
	oTraj_BCS_GDU2['FORMAT']        = 'F9.4'
	oTraj_BCS_GDU2['LABEL']         = ['Phi', 'Theta']
	oTraj_BCS_GDU2['SCALETYP']      = 'linear'
	oTraj_BCS_GDU2['SI_CONVERSION'] = '0.01745>rad'
	oTraj_BCS_GDU2['UNITS']         = 'degrees'
	oTraj_BCS_GDU2['VALIDMAX']      = -180.0
	oTraj_BCS_GDU2['VALIDMIN']      = 180.0
	oTraj_BCS_GDU2['VAR_TYPE']      = 'data'
	oTraj_BCS_GDU2['VAR_NOTES']     = traj_notes
	
	;TRAJ GDU2
	oTraj_GDU2['AXIS_RANGE']    = [-180.0, 180.0]
	oTraj_GDU2['CATDESC']       = 'Incident trajectories of EDI electrons in instrument coordinates measured by GDU2.'
	oTraj_GDU2['COLOR']         = ['Blue', 'Red']
	oTraj_GDU2['DELTA_PLUS']    = traj_delta
	oTraj_GDU2['DELTA_MINUS']   = traj_delta
	oTraj_GDU2['DISPLAY_TYPE']  = 'time_series'
	oTraj_GDU2['FIELDNAM']      = 'Incident Traj'
	oTraj_GDU2['FILLVAL']       = -1e31
	oTraj_GDU2['FORMAT']        = 'F9.4'
	oTraj_GDU2['LABEL']         = ['Phi', 'Theta']
	oTraj_GDU2['SCALETYP']      = 'linear'
	oTraj_GDU2['SI_CONVERSION'] = '0.01745>rad'
	oTraj_GDU2['UNITS']         = 'degrees'
	oTraj_GDU2['VALIDMAX']      = -180.0
	oTraj_GDU2['VALIDMIN']      = 180.0
	oTraj_GDU2['VAR_TYPE']      = 'data'
	oTraj_GDU2['VAR_NOTES']     = traj_notes
	
	;TRAJ GDU2
	oTraj_DBCS_GDU2['AXIS_RANGE']    = [-180.0, 180.0]
	oTraj_DBCS_GDU2['CATDESC']       = 'Incident trajectories of EDI electrons in DBCS coordinates measured by GDU2.'
	oTraj_DBCS_GDU2['COLOR']         = ['Blue', 'Red']
	oTraj_DBCS_GDU2['DELTA_PLUS']    = traj_delta
	oTraj_DBCS_GDU2['DELTA_MINUS']   = traj_delta
	oTraj_DBCS_GDU2['DISPLAY_TYPE']  = 'time_series'
	oTraj_DBCS_GDU2['FIELDNAM']      = 'Incident Traj'
	oTraj_DBCS_GDU2['FILLVAL']       = -1e31
	oTraj_DBCS_GDU2['FORMAT']        = 'F9.4'
	oTraj_DBCS_GDU2['LABEL']         = ['Phi', 'Theta']
	oTraj_DBCS_GDU2['SCALETYP']      = 'linear'
	oTraj_DBCS_GDU2['SI_CONVERSION'] = '0.01745>rad'
	oTraj_DBCS_GDU2['UNITS']         = 'degrees'
	oTraj_DBCS_GDU2['VALIDMAX']      = -180.0
	oTraj_DBCS_GDU2['VALIDMIN']      = 180.0
	oTraj_DBCS_GDU2['VAR_TYPE']      = 'data'
	oTraj_DBCS_GDU2['VAR_NOTES']     = traj_notes
	
	;TRAJ GDU2
	oTraj_GSE_GDU2['AXIS_RANGE']    = [-180.0, 180.0]
	oTraj_GSE_GDU2['CATDESC']       = 'Incident trajectories of EDI electrons in GSE coordinates measured by GDU2.'
	oTraj_GSE_GDU2['COLOR']         = ['Blue', 'Red']
	oTraj_GSE_GDU2['DELTA_PLUS']    = traj_delta
	oTraj_GSE_GDU2['DELTA_MINUS']   = traj_delta
	oTraj_GSE_GDU2['DISPLAY_TYPE']  = 'time_series'
	oTraj_GSE_GDU2['FIELDNAM']      = 'Incident Traj'
	oTraj_GSE_GDU2['FILLVAL']       = -1e31
	oTraj_GSE_GDU2['FORMAT']        = 'F9.4'
	oTraj_GSE_GDU2['LABEL']         = ['Phi', 'Theta']
	oTraj_GSE_GDU2['SCALETYP']      = 'linear'
	oTraj_GSE_GDU2['SI_CONVERSION'] = '0.01745>rad'
	oTraj_GSE_GDU2['UNITS']         = 'degrees'
	oTraj_GSE_GDU2['VALIDMAX']      = -180.0
	oTraj_GSE_GDU2['VALIDMIN']      = 180.0
	oTraj_GSE_GDU2['VAR_TYPE']      = 'data'
	oTraj_GSE_GDU2['VAR_NOTES']     = traj_notes
	
	;TRAJ GDU2
	oTraj_GSM_GDU2['AXIS_RANGE']    = [-180.0, 180.0]
	oTraj_GSM_GDU2['CATDESC']       = 'Incident trajectories of EDI electrons in GSM coordinates measured by GDU2.'
	oTraj_GSM_GDU2['COLOR']         = ['Blue', 'Red']
	oTraj_GSM_GDU2['DELTA_PLUS']    = traj_delta
	oTraj_GSM_GDU2['DELTA_MINUS']   = traj_delta
	oTraj_GSM_GDU2['DISPLAY_TYPE']  = 'time_series'
	oTraj_GSM_GDU2['FIELDNAM']      = 'Incident Traj'
	oTraj_GSM_GDU2['FILLVAL']       = -1e31
	oTraj_GSM_GDU2['FORMAT']        = 'F9.4'
	oTraj_GSM_GDU2['LABEL']         = ['Phi', 'Theta']
	oTraj_GSM_GDU2['SCALETYP']      = 'linear'
	oTraj_GSM_GDU2['SI_CONVERSION'] = '0.01745>rad'
	oTraj_GSM_GDU2['UNITS']         = 'degrees'
	oTraj_GSM_GDU2['VALIDMAX']      = -180.0
	oTraj_GSM_GDU2['VALIDMIN']      = 180.0
	oTraj_GSM_GDU2['VAR_TYPE']      = 'data'
	oTraj_GSM_GDU2['VAR_NOTES']     = traj_notes

;-------------------------------------------
; Finish ///////////////////////////////////
;-------------------------------------------
	
	;Remove L1A variables
	MrVar_Delete, [ counts_gdu1_vname, counts_gdu2_vname, phi_vname,  theta_vname, $
	                pack_mode_vname, pitch_mode_vname, flip_vname, pitch_gdu1_vname, $
	                pitch_gdu2_vname, perp_oneside_vname, perp_bidir_vname ]
	
	IF ~tf_edi THEN BEGIN
		traj_edi_0_vname   = !Null
		traj_edi_90_vname  = !Null
		traj_edi_180_vname = !Null
		Obj_Destroy, [oTraj_GDU1, oTraj_GDU1]
	ENDIF
	
	IF ~tf_bcs THEN BEGIN
		traj_bcs_0_vname   = !Null
		traj_bcs_90_vname  = !Null
		traj_bcs_180_vname = !Null
		Obj_Destroy, [oTraj_BCS_GDU1, oTraj_BCS_GDU1]
	ENDIF
	
	IF ~tf_dbcs THEN BEGIN
		traj_dbcs_0_vname   = !Null
		traj_dbcs_90_vname  = !Null
		traj_dbcs_180_vname = !Null
		Obj_Destroy, [oTraj_DBCS_GDU1, oTraj_DBCS_GDU1]
	ENDIF
	
	IF ~tf_gse THEN BEGIN
		traj_gse_0_vname   = !Null
		traj_gse_90_vname  = !Null
		traj_gse_180_vname = !Null
		Obj_Destroy, [oTraj_GSE_GDU1, oTraj_GSE_GDU1]
	ENDIF
	
	IF ~tf_gsm THEN BEGIN
		traj_gsm_0_vname   = !Null
		traj_gsm_90_vname  = !Null
		traj_gsm_180_vname = !Null
		Obj_Destroy, [oTraj_GSM_GDU1, oTraj_GSM_GDU1]
	ENDIF
	
	;Output variable names
	varnames = []
	IF n0 GT 0 THEN BEGIN
		varnames = [ flux_0_vname, traj_edi_0_vname, traj_bcs_0_vname, traj_dbcs_0_vname, $
		             traj_gse_0_vname, traj_gsm_0_vname ]
	ENDIF
	IF n90 GT 0 THEN BEGIN
		varnames = [ flux_90_vname, traj_edi_90_vname, traj_bcs_90_vname, traj_dbcs_90_vname, $
		             traj_gse_90_vname, traj_gsm_90_vname ]
	ENDIF
	IF n180 GT 0 THEN BEGIN
		varnames = [ flux_180_vname, traj_edi_180_vname, traj_bcs_180_vname, traj_dbcs_180_vname, $
		             traj_gse_180_vname, traj_gsm_180_vname ]
	ENDIF
END