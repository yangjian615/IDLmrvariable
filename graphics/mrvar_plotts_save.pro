; docformat = 'rst'
;
; NAME:
;       MrVar_PlotTS_Save
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
;   Plot MrVariable data
;
;   Calling Sequence:
;       fnames = MrVar_PlotTS(win, fbase)
;       fnames = MrVar_PlotTS(win, fbase, ext)
;
; :Params:
;       VARIABLES:  in, optional, type=strarr/intarr
;                   The names or indices of time series variables to be plotted. If
;                       not provided, the variables given in the previous call to
;                       MrVar_PlotTS will be used.
;
; :Params:
;       WIN:        in, required, type=objref
;                   A MrWindow object reference containing the time-series plot to be saved.
;       FBASE:      in, optional, type=string, default=`WIN`.NAME
;                   The filename base without extension. The time interval defined in
;                       in MrVar_GetTRange and `EXT` will be appended to FBASE to create
;                       the final output file name. If FBASE does not contain a directory
;                       path, the output file is saved in the present working directory.
;       EXT:        in, optional, type=string/strarr, default='png'
;                   The image formats in which to save the plot. Options include:
;                      'eps', 'gif', 'jpeg', 'pdf', 'png', 'ps', and 'tiff'.
;
; :Returns:
;       FNAMES:     out, required, type=string/strarr
;                   Names of the files produced.
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
;       2017/05/10  -   Written by Matthew Argall
;       2017/06/18  -   Replacement of decimal point by "p" fixed. - MRA
;       2017/06/20  -   Leading zeros after decimal point preserved. - MRA
;-
FUNCTION MrVar_PlotTS_Save, win, fbase, ext
	Compile_Opt idl2
	On_Error, 2
	
	;Defaults
	IF N_Elements(ext)   EQ 0 THEN ext   = 'png'
	IF N_Elements(fbase) EQ 0 THEN fbase = win.name
	
	;Check if only a file name was given
	ftemp = fbase
	IF File_BaseName(ftemp) EQ ftemp THEN BEGIN
		CD, CURRENT=pwd
		ftemp = FilePath(ftemp, ROOT_DIR=pwd)
	ENDIF
	
	;Date and time range of plot
	ftime  = MrVar_GetTRange()
	dstart = StrJoin( StrSplit( StrMid(ftime[0],  0, 10), '-', /EXTRACT ) )
	dend   = StrJoin( StrSplit( StrMid(ftime[1],  0, 10), '-', /EXTRACT ) )
	tstart = StrJoin( StrSplit( StrMid(ftime[0], 11,  8), ':', /EXTRACT ) )
	tend   = StrJoin( StrSplit( StrMid(ftime[1], 11,  8), ':', /EXTRACT ) )
	
	;Sub-seconds time interval?
	ftime  = MrVar_GetTRange('SSM')
	dtime  = ftime[1] - ftime[0]
	pow    = ALog10(dtime)
	IF pow LT 0 THEN BEGIN
		thePow = Ceil(Abs(pow))
		fmt    = '(i' + String( thePow, FORMAT='(i0' + String(thePow, FORMAT='(i0)') + ')' ) + ')'
		tstart = tstart + 'p' + String( Round( (ftime[0] MOD 1)*10^thePow ), FORMAT=fmt)
		tend   = tend   + 'p' + String( Round( (ftime[1] MOD 1)*10^thePow ), FORMAT=fmt)
	ENDIF

	;Time of file
	ftime = dstart + '_' + tstart
	IF dend NE dstart THEN ftime += '_' + dend
	ftime += '_' + tend
	
	;File name
	fname  = ftemp + '_' + ftime + '.' + ext
	
	;Save the figure
	FOR i = 0, N_Elements(fname) - 1 DO win -> Save, fname[i]
	
	RETURN, fname
END