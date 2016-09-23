; docformat = 'rst'
;
; NAME:
;       MrMMS_EDI_Q0_GPDist
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
;   Compute the pitch and gyrophase angle of EDI particle trajectory information.
;   Resulting data is added to the MrVariable cache.
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
;       2014/06/28  -   Written by Matthew Argall
;-
function MrMMS_EDI_Q0_GPDist, sc, mode, tstart, tend, tstride, $
DT      = dt, $
DGA     = dGA, $
FAC     = fac, $
GARANGE = gaRange, $
LAYOUT  = layout
	compile_opt idl2
	
	catch, the_error
	if the_error ne 0 then begin
		catch, /CANCEL
		if n_elements(win) gt 0 then obj_destroy, win
		MrPrintF, 'LogErr'
		return, !Null
	endif
	
	;Defaults
	if n_elements(tstart)  eq 0 then tstart  = ''
	if n_elements(tend)    eq 0 then tend    = 1
	if n_elements(tstride) eq 0 then tstride = 1
	
;-------------------------------------------
; Get Data /////////////////////////////////
;-------------------------------------------
	
	;EDI, FGM, FPI moments
	if 0 then $
	MrMMS_EDI_Load_PGA, sc, mode, 'q0', $
	                    DT      = dt, $
	                    DGA     = dGA, $
	                    FAC     = fac, $
	                    GARANGE = gaRange

;-------------------------------------------
; Variables to Plot ////////////////////////
;-------------------------------------------
	
	;Create variable names
	prefix   = sc + '_edi_'
	suffix   = '_' + mode + '_l2'
	gpd_name = prefix + 'gpd'     + suffix
	bin_name = prefix + 'ga_bins' + suffix
	
;-------------------------------------------
; Identify Distributions ///////////////////
;-------------------------------------------
	;Get the GPD
	oGPD  = MrVar_Get(gpd_name)
	oGA   = MrVar_Get(bin_name)
	oTime = MrVar_Get(oGPD['DEPEND_0'])
	
	;Get the start index
	if tstart eq '' then begin
		istart = 0
	endif else begin
		ts_tt2000 = oTime -> iso2tt2000(tstart)
		istart    = oTime -> Value_Locate(ts_tt2000, 'TT2000') > 0
	endelse
	
	;Get the end index
	if size(tend, /TNAME) eq 'STRING' then begin
		te_tt2000 = oTime -> iso2tt2000(tend)
		iend      = oTime -> Value_Locate(te_tt2000, 'TT2000')
	endif else begin
		iend = istart + tstride*(tend-1)
	endelse

;-------------------------------------------
; Prepare to Plot //////////////////////////
;-------------------------------------------
	;Plot layout
	nTotal = (iend - istart) / tstride + 1 > 1
	if n_elements(layout) eq 0 then begin
		nRows  = ceil(sqrt(nTotal))
		nCols  = ceil( float(nTotal) / float(nRows) )
		layout = [temporary(nCols), temporary(nRows)]
	endif
	
	;Create the graphics window
	win = MrWindow(ASPECT=1.0, LAYOUT=layout, XGAP=0.5, OYMARGIN=[6,2], YGAP=0.5, YSIZE=800, REFRESH=0)

;-------------------------------------------
; Plot /////////////////////////////////////
;-------------------------------------------
	;Step through each distribution
	yrange = [!values.f_infinity, -!values.f_infinity]
	while istart le iend do begin
		;Plot Range
		i          = where(oGPD[iStart,*] gt 0, n)
		yrange[0] <= min(oGPD[iStart,i])
		yrange[1] >= max(oGPD[iStart,i])
	
		;Plot the distribution without annotations
		p = MrPlot( oGA['DATA'], oGPD[istart, *], $
		            /CURRENT, $
		            NAME        = 'Dist ' + string(istart, FORMAT='(i0)'), $
		            TITLE       = '', $
		            XTICKFORMAT = '(a1)', $
		            XTITLE      = '', $
		            /YLOG, $
		            YTICKFORMAT = '(a1)', $
		            YTITLE      = '' )
		
		;Add the distribution time to the plot
		t = MrText( 0.05, 0.05, oTime[istart, 11:22], $
		            NAME   = 't Dist ' + string(istart, FORMAT='(i0)'), $
		            /RELATIVE, $
		            TARGET = p)
		
		;Next iteration
		istart += tstride
	endwhile

;-------------------------------------------
; Make Pretty //////////////////////////////
;-------------------------------------------
	;Plot title
	title  = strupcase(sc) + ' ' + strupcase(mode) + ' ' + oTime[0, 0:9]
	oTitle = MrText(0.55, 0.975, title, NAME='Title', ALIGNMENT=0.5)

	;Set global properties
	win -> SetGlobal, XRANGE=[-180.0, 180.0], XTICKINTERVAL=90.0, YRANGE=yrange

	;Loop over all plots in the bottom row
	theLay = win.layout
	for i = 1, theLay[0] do begin
		theGfx = win -> FindByColRow( [i, theLay[1]] )
		theGfx -> SetProperty, XTICKFORMAT='', XTITLE='Gyrophase!C0=(BxV)xB!C90=BxV'
	endfor
	
	;Loop over all plots in the first column
	for i = 1, theLay[1] do begin
		theGfx = win -> FindByColRow( [1, i] )
		theGfx -> SetProperty, YTICKFORMAT='', YTITLE='Counts'
	endfor

;-------------------------------------------
; Return ///////////////////////////////////
;-------------------------------------------
	
	;Refresh the window and return
	win -> Refresh
	return, win
end