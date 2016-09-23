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
function argall_q0_grl_2016a, sc, mode, tstart, tend, tstride, $
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
	
	;Inputs
	MrVar_SetTRange, ['2015-10-16T13:06:40', '2015-10-16T13:07:20']
	sc     = 'mms2'
	mode   = 'brst'
	dt     = 1.4
	dGA    = 8.0
	yrange = [10,300]
	
	;Distributions to plot
	theDist = ['2015-10-16T13:06:56.5', $
	           '2015-10-16T13:06:59.5', $
	           '2015-10-16T13:07:00.0', $
	           '2015-10-16T13:07:02.0']
	
;-------------------------------------------
; Get Data /////////////////////////////////
;-------------------------------------------
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
	
	;Get the GPD
	oGPD  = MrVar_Get(gpd_name)
	oGA   = MrVar_Get(bin_name)
	oTime = MrVar_Get(oGPD['DEPEND_0'])

;-------------------------------------------
; Identify the Distributions ///////////////
;-------------------------------------------
	
	;Get the start index
	dist_tt2000 = oTime -> iso2tt2000(theDist)
	iDist       = oTime -> Value_Locate(dist_tt2000, 'TT2000') > 0

;-------------------------------------------
; Prepare to Plot //////////////////////////
;-------------------------------------------
	
	;Create the graphics window
	win = MrWindow(ASPECT=1.0, XGAP=0.5, OYMARGIN=[6,2], YGAP=0.5, YSIZE=800, REFRESH=0)
	
	;Capture a YRANGE to be applied to all graphics
	yr_temp = [!values.f_infinity, -!values.f_infinity]

;-------------------------------------------
; Plot /////////////////////////////////////
;-------------------------------------------
	nDist   = n_elements(theDist)
	plotarr = objarr(nDist)

	;Step through each distribution
	for i = 1, nDist - 1 do begin
		;Plot Range
		idx        = where(oGPD[iDist[i],*] gt 0, n)
		yr_temp[0] <= min(oGPD[iDist[i],idx])
		yr_temp[1] >= max(oGPD[iDist[i],idx])
		
		;Turn the index into a string
		distNum = string(i, FORMAT='(i1)')
	
		;Plot the distribution without annotations
		plotarr[i] = MrPlot( oGA['DATA'], oGPD[iDist[i], *], $
		                     /CURRENT, $
		                     NAME        = 'Dist ' + distNum, $
		                     TITLE       = '', $
		                     XTICKFORMAT = '(a1)', $
		                     XTITLE      = '', $
		                     /YLOG, $
		                     YTICKFORMAT = '(a1)', $
		                     YTITLE      = '' )
		
		;Add the distribution time to the plot
		t = MrText( 0.05, 0.05, oTime[iDist[i], 11:20], $
		            NAME   = 't Dist ' + distNum, $
		            /RELATIVE, $
		            TARGET = plotarr[i] )
	endfor

	;Overplot the background distribution
	plotarr[0] = MrPlot( oGA['DATA'], oGPD[iDist[0], *], $
	                     COLOR    = 'Red', $
	                     NAME     = 'Dist Bkgd', $
	                     OVERPLOT = plotarr[1] )
	
	;Time of background distribution
	win['t Dist 1'] -> SetProperty, YLOC=0.12
	t = MrText( 0.05, 0.045, oTime[iDist[0], 11:20], $
	            /RELATIVE, $
	            COLOR  = 'Red', $
	            NAME   = 't Bkgd', $
	            TARGET = plotarr[0] )

;-------------------------------------------
; Make Pretty //////////////////////////////
;-------------------------------------------
	;Plot title
	title  = strupcase(sc) + ' ' + strupcase(mode) + ' ' + oTime[0, 0:9]
	oTitle = MrText(0.55, 0.975, title, NAME='Title', ALIGNMENT=0.5)

	;Set global properties
	if n_elements(yrange) eq 0 then yrange = temporary(yr_temp)
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