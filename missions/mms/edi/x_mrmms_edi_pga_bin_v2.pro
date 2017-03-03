; docformat = 'rst'
;
; NAME:
;       MrMMS_EDI_PGA_BinGDU
;
;*****************************************************************************************
;   Copyright (c) 2016, Matthew Argall                                                   ;
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
;   Bin EDI data by pitch or gyrophase angle. Resulting data is added to the
;   MrVariable cache.
;
; :Categories:
;       MMS, EDI, MrVariable
;
; :Params:
;       COUNTS:     in, required, type=string/integer/objref
;                   The MrScalarTS object, name, or index corresponding to EDI particle
;                       flux or counts data.
;       ANGLE:      in, required, type=string/integer/objref
;                   The MrScalarTS object, index, or name corresponding to EDI pitch angle
;                       or gyrophase angle data.
;       DT:         in, optional, type=string/objref, default=''
;                   Duration, in seconds, of each time bin.
;       DA:         in, optional, type=string, default=''
;                   Width, in degrees, of each angular bin.
;
; :Keywords:
;       DEP0_NAME:  in, optional, type=string, default='edi_epoch_bins'
;                   Name to be given to the MrTimeVar variable containing the time bins.
;       DEP1_NAME:  in, optional, type=string, default='edi_(pa|ga)_bins'
;                   Name to be given to the MrScalarTS variable containing angle bins.
;       GA:         in, optional, type=boolean, default=0
;                   If set, `ANGLE` represents the gyrophase angle. Pitch angle is assumed.
;       NAME:       in, optional, type=string, default='edi_(pad|gpd)'
;                   Name to be given to the MrVariable object containing the binned results.
;       TRANGE:     in, optional, type=dblarr(2), default=[min(t)\, max(t)] rounded to `DT`
;                   Time range, in seconds since midnight, over which to bin data.
;       ARANGE:     in, optional, type=dblarr(2), default=[-180\,180] for GPD or [80\,100] for PAD
;                   Angular range, in degrees, over which to bin data.
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
;       2016/06/07  -   Written by Matthew Argall
;-
pro MrMMS_EDI_PGA_Bin, counts, angle, dt, dA, $
DEP0_NAME=dep0_name, $
DEP1_NAME=dep1_name, $
GA=ga, $
NAME=name, $
TRANGE=trange, $
ARANGE=arange
	compile_opt idl2
	on_error, 2

;-------------------------------------------
; Retrieve Data Objects ////////////////////
;-------------------------------------------

	;Data
	oCounts = size(counts, /TNAME) eq 'STRING' ? MrVar_Get(counts) : counts
	oAngle  = size(angle,  /TNAME) eq 'STRING' ? MrVar_Get(angle)  : angle

;-------------------------------------------
; Default Values ///////////////////////////
;-------------------------------------------

	;Defaults
	tf_ga = keyword_set(ga)
	if n_elements(dA)        eq 0 then dA        = tf_ga ? 11.25 : 1.0
	if n_elements(dt)        eq 0 then dt        = 2.5
	if n_elements(arange)    eq 0 then arange    = tf_ga ? [-180.0, 180.0] : [80.0, 100.0]
	if n_elements(dep0_name) eq 0 then dep0_name = 'edi_epoch_binned'
	if n_elements(dep1_name) eq 0 then dep1_name = 'edi_' + (tf_ga ? 'ga' : 'pa') + '_binned'
	if n_elements(name)      eq 0 then name      = 'edi_' + (tf_ga ? 'gpd' : 'pad')
	
	;Get the time array
	oCts  = MrVar_Get(counts)
	oDep0 = MrVar_Get(oCts['DEPEND_0'])
	nPts  = n_elements(oDep0)

;-------------------------------------------
; Bin Information //////////////////////////
;-------------------------------------------
	
	;Time bins
	;   - BINSIZE = (MAX – MIN) / (NBINS – 1)
	if n_elements(trange) eq 0 then begin
		trange        = [oDep0[0, 'TT2000'], oDep0[-1, 'TT2000']]
		trange_ssm    = MrCDF_epoch2ssm(trange)
		trange_ssm[0] = trange_ssm[0] - (trange_ssm[0] mod dt)
		trange_ssm[1] = trange_ssm[1] - (trange_ssm[1] mod dt) + dt
	endif else begin
		trange_ssm = MrCDF_epoch2ssm(trange)
	endelse
	t_ssm   = oDep0 -> GetData('SSM')
	nt_bins = long( (trange_ssm[1] - trange_ssm[0]) / dt + 1 )
	t_bins  = linspace(trange_ssm[0], trange_ssm[1], nt_bins)
	
	;Min and max PA range
	if n_elements(arange) eq 0 then begin
		arange    = [oAngle.min, oAngle.max]
		arange[0] = arange[0] - (arange[0] mod dA)
		arange[1] = arange[1] - (arange[1] mod dA) + dA
	endif
	nA_bins = long( (arange[1] - arange[0]) / dA + 1 )
	a_bins  = linspace(arange[0], arange[1], nA_bins)
	
	;
	; TODO: Report number of points that fall outside of ARANGE
	;

;-------------------------------------------
; Bin by Time //////////////////////////////
;-------------------------------------------

	;Histogram the data
	tHist  = histogram( temporary(t_ssm), MIN=trange_ssm[0], MAX=trange_ssm[1], NBINS=nt_bins, REVERSE_INDICES=ri )
	outArr = fltarr(nt_bins, nA_bins)
	for i = 0, n_elements(tHist) - 1 do begin
		if ri[i] eq ri[i+1] then continue
		
		;Indices within the source
		isrc = ri[ri[i]:ri[i+1]-1]

	;-------------------------------------------
	; Bin by Angle /////////////////////////////
	;-------------------------------------------
		
		;Bin PA data
		aHist = histogram(oAngle[isrc], MIN=arange[0], MAX=arange[1], NBINS=nA_bins, REVERSE_INDICES=rj)
		for j = 0, n_elements(aHist) - 1 do begin
			if rj[j] eq rj[j+1] then continue

			;Indices within the source
			jsrc = rj[rj[j]:rj[j+1]-1]

			;Save the data
			;   - Sum all counts
			;   - Normalize by time spent in each bin
			outArr[i,j] = total(oCounts[isrc[jsrc]]) / n_elements(jsrc)
		endfor
	endfor
	outArr = round(outArr)

;-------------------------------------------
; Output Data //////////////////////////////
;-------------------------------------------

	;Time Bins
	t_tt2000  = MrCDF_ssm2epoch(t_bins, trange[0], EPOCH_TYPE='CDF_TIME_TT2000')
	oBinnedT  = MrTimeVar(t_tt2000, 'TT2000', /CACHE, /NO_COPY, NAME=dep0_name)
	oBinnedT -> AddAttr, 'DELTA_PLUS',  0LL
	oBinnedT -> AddAttr, 'DELTA_MINUS', long64(dt * 1e9)
	oBinnedT -> AddAttr, 'UNITS',       'ns'
	oBinnedT -> AddAttr, 'TIME_BASE',   'J2000'
	oBinnedT -> AddAttr, 'TITLE',       'TT2000'

	;Angle Bins
	oBinnedPA  = MrScalarTS(a_bins, /CACHE, /NO_COPY, NAME=dep1_name)
	oBinnedPA -> AddAttr, 'DELTA_PLUS',  0.0
	oBinnedPA -> AddAttr, 'DELTA_MINUS', dA
	oBinnedPA -> AddAttr, 'UNITS',       'degrees'
	oBinnedPA -> AddAttr, 'TITLE',       (tf_ga ? 'Gyrophase' : 'Pitch Angle')
	oBinnedPA -> AddAttr, 'PLOT_TITLE',  'EDI ' + (tf_ga ? 'Gyrophase' : 'Pitch Angle')

	;Binned Counts
	oBinnedCts  = MrVariable(outArr, /CACHE, /NO_COPY, NAME=name)
	oBinnedCts -> AddAttr, 'DEPEND_0',   dep0_name
	oBinnedCts -> AddAttr, 'DEPEND_1',   dep1_name
	oBinnedCts -> AddAttr, 'TITLE',      'Normalized Counts'
	oBinnedCts -> AddAttr, 'UNITS',      'Counts'
	oBinnedCts -> AddAttr, 'PLOT_TITLE', 'EDI Binned Electron Counts'
end