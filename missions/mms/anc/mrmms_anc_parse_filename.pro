; docformat = 'rst'
;
; NAME:
;       MrMMS_Anc_Parse_Filename
;
; PURPOSE:
;+
;       The purpose of this program is to dissect ancillary data filenames from the
;       MMS mission.
;
; :Categories:
;       MMS
;
; :Params:
;       FILENAME:       in, required, type=string
;                       The filename of the MMS file to be dissected.
;
; :Keywords:
;       INSTR:          out, optional, type=string
;                       Instrument ID. Possible values are::
;                           'DEFATT'           'DEFOVER'
;                           'DEFEPH'           'NAVPERF'
;                           'DEFERR'           'TIMEBIAS'
;                           'ATTVAL'           
;       SC:             out, optional, type=string
;                       The spacecraft ID. Possible values are::
;                           'mms1'
;                           'mms2'
;                           'mms3'
;                           'mms4'
;       TSTART:         out, optional, type=string
;                       Start time of the data interval formatted as YYYY-DDD.
;       TEND:           out, optional, type=string
;                       End time of the data interval formatted as YYYY-DDD.
;       TT2000_START:   out, optional, type=int64 (cdf_time_tt2000)
;                       Start time of the data interval.
;       TT2000_END:     out, optional, type=int64 (cdf_time_tt2000)
;                       End time of the data interval.
;       VERSION:        out, optional, type=string
;                       Version of the data file, formatted as 'X'
;
; :Author:
;   Matthew Argall::
;       University of New Hampshire
;       Morse Hall, Room 348
;       8 College Rd.
;       Durham, NH, 03824
;       matthew.argall@unh.edu
;
; :History::
;   Modification History::
;       2016/11/18  -   Written by Matthew Argall.
;-
pro MrMMS_Anc_Parse_Filename, filename, $
DIRECTORY=directory, $
INSTR=instr, $
SC=sc, $
TSTART=tstart, $
TEND=tend, $
VERSION=version, $
TT2000_START=tt2000_start, $
TT2000_END=tt2000_end
	compile_opt strictarr
	on_error, 2

	;Check that a filename was provided.
	if n_elements(filename) eq 0 then message, 'A file name must be given.'
	
	;Remove the directory, if it is present
	directory = file_dirname(filename)
	fname     = file_basename(filename)
	
;-----------------------------------------------------
;DISSECT FILENAMES \\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\
;-----------------------------------------------------
	str = stregex(fname, '(MMS[1-4])_'          + $     ;Spacecraft ID
	                     '([A-Z-]+)_'           + $     ;Instrument ID
	                     '([0-9]{4}[0-9]{3})_'  + $     ;Start Time
	                     '([0-9]{4}[0-9]{3})\.' + $     ;End Time
	                     'V([0-9]+)', $                 ;Version
	                     /EXTRACT, /SUBEXP)

	;Find non-matches
	iFail = where(str[0,*] eq '', nFail, COMPLEMENT=iPass, NCOMPLEMENT=nPass)
	if nFail gt 0 then begin
		message, 'Cannot dissect filenames: ', /INFORMATIONAL
		print, '    "' + transpose(filename[iFail]) + '"'
	endif
	if nPass eq 0 then return

	;Extract the subexpressions
	sc      = reform(str[1,iPass])
	instr   = reform(str[2,iPass])
	tstart  = reform(str[3,iPass])
	tend    = reform(str[4,iPass])
	version = reform(str[5,iPass])
	
	;Dissect the start time
	if arg_present(tt2000_start) then begin
		year         = fix(strmid(tstart, 0, 4))
		doy          = fix(strmid(tstart, 4, 3))
		date         = MrDOY2Date(doy)
		tt2000_start = MrCDF_Epoch_Compute(year, date[0], date[1], 0, 0, 0, 0, 0, 0)
	endif
		
	if arg_present(tt2000_end) then begin
		year       = fix(strmid(tend, 0, 4))
		doy        = fix(strmid(tend, 4, 3))
		date       = MrDOY2Date(doy)
		tt2000_end = MrCDF_Epoch_Compute(year, date[0], date[1], 0, 0, 0, 0, 0, 0)
	endif
	
	;Return scalars?
	if nPass eq 1 then begin
		sc      = sc[0]
		instr   = instr[0]
		tstart  = tstart[0]
		tend    = tend[0]
		version = version[0]
		
		if n_elements(tt2000_start) eq 1 then tt2000_start = tt2000_start[0]
		if n_elements(tt2000_end)   eq 1 then tt2000_end   = tt2000_end[0]
	endif
end

;-----------------------------------------------------
;Main Level Example Program \\\\\\\\\\\\\\\\\\\\\\\\\\
;-----------------------------------------------------

;pick a few filenames
filenames = ['MMS2_DEFATT_2015289_2015289.V00', $
             'MMS2_DEFATT_2015289_2015289.V00']

;print, the filenames
print, format='(%"The filenames used are: %s")', filenames[0]
print, format='(%"                        %s")', filenames[1]

;Dissect all of the names
MrMMS_Anc_Parse_Filename, filenames, $
                          INSTR   = instrument, $
                          TSTART  = start_time, $
                          TEND    = end_time, $
                          SC      = spacecraft, $
                          VERSION = version

;breakdown the filenames and display the results.
for i = 0, n_elements(filenames) - 1 do begin
    print, '----------------------------------------------------------------------'
    print, format='(%"Results for: %s")', filenames[i]
    print, FORMAT='(%"  Spacecraft:    %s")', spacecraft[i]
    print, FORMAT='(%"  Instrument:    %s")', instrument[i]
    print, FORMAT='(%"  Start Time:    %s")', start_time[i]
    print, FORMAT='(%"  End Time:      %s")', end_time[i]
    print, FORMAT='(%"  Version:       %s")', version[i]
    print, ''
endfor

end