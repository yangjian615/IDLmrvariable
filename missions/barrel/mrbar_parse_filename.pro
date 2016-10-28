; docformat = 'rst'
;
; NAME:
;       MrBar_Parse_Filename
;
; PURPOSE:
;+
;   Parse BARREL file names. File names take the form::
;
;       barCLL_PP_S_l1_TTTT_YYYYMMDD_v++.cdf
;
; :Categories:
;       MrVariable, BARREL
;
; :Params:
;       FILENAME:       in, required, type=string
;                       The filename of the MMS file to be dissected.
;
; :Keywords:
;       CAMPAIGN:       out, optional, type=string
;                       Optional data product descriptor.
;       ORDER:          out, optional, type=string
;                       Optional data product descriptor.
;       PAYLOAD:        out, optional, type=string
;                       Optional data product descriptor.
;       SITE:           out, optional, type=string
;                       Optional data product descriptor.
;       LEVEL:          out, optional, type=string
;                       Optional data product descriptor.
;       TYPE:           out, optional, type=string
;                       Optional data product descriptor.
;       DATE:           out, optional, type=string
;                       Optional data product descriptor.
;       VERSION:        out, optional, type=string
;                       Optional data product descriptor.
;       
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
;       2016/10/24  -   Written by Matthew Argall.
;-
pro MrBar_Parse_Filename, filename, $
DIRECTORY=directory, $
CAMPAIGN=campaign, $
ORDER=order, $
LEVEL=level, $
TYPE=type, $
DATE=date, $
VERSION=version, $
TT2000=tt2000
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
	str = stregex(fname, 'bar_([1-4])'       + $                  ;Campaign
	                     '([A-Z])_'          + $                  ;Order
	                     '(l[0-3])_'         + $                  ;Level
	                     '([a-z]{4})_'       + $                  ;Type
	                     '([0-9]{8})_'       + $                  ;Date
	                     'v([0-9]+)'         + $                  ;Version
	                     '.cdf', /EXTRACT, /SUBEXP)

	;Find non-matches
	iFail = where(str[0,*] eq '', nFail, COMPLEMENT=iPass, NCOMPLEMENT=nPass)
	if nFail gt 0 then begin
		message, 'Cannot dissect filenames: ', /INFORMATIONAL
		print, '    "' + transpose(filename[iFail]) + '"'
	endif
	if nPass eq 0 then return

	;Extract the subexpressions
	campaign = reform(str[1,iPass])
	order    = reform(str[2,iPass])
	level    = reform(str[3,iPass])
	type     = reform(str[4,iPass])
	date     = reform(str[5,iPass])
	version  = reform(str[6,iPass])
	
	;Dissect the start time
	if arg_present(tt2000) then begin
		;Reform
		year   = fix(strmid(tstart,  0, 4))
		month  = fix(strmid(tstart,  4, 2))
		day    = fix(strmid(tstart,  6, 2))
		tt2000 = MrCDF_Epoch_Compute(year, month, day, 0, 0, 0, 0, 0, 0)
	endif
	
	;Return scalars?
	if nPass eq 1 then begin
		campaign = campaign[0]
		order    = order[0]
		level    = level[0]
		type     = type[0]
		date     = date[0]
		version  = version[0]
		if n_elements(tt2000) eq 1 then tt2000 = tt2000[0]
	endif
end

;-----------------------------------------------------
;Main Level Example Program \\\\\\\\\\\\\\\\\\\\\\\\\\
;-----------------------------------------------------

;pick a few filenames
filenames = ['bar_4H_l2_ephm_20160830_v06.cdf', $
             'bar_4H_l2_ephm_20160830_v06.cdf', $
             'bar_4H_l2_ephm_20160830_v06.cdf', $
             'bar_4H_l2_ephm_20160830_v06.cdf']

;print, the filenames
print, format='(%"The filenames used are: %s")', filenames[0]
print, format='(%"                        %s")', filenames[1]
print, format='(%"                        %s")', filenames[2]
print, format='(%"                        %s")', filenames[3]

;Dissect all of the names
MrBar_Parse_Filename, filenames, $
                      CAMPAIGN = campaign, $
                      ORDER    = order, $
                      LEVEL    = level, $
                      TYPE     = type, $
                      DATE     = date, $
                      VERSION  = version
                    
;breakdown the filenames and display the results.
for i = 0, n_elements(filenames) - 1 do begin
    print, '----------------------------------------------------------------------'
    print, format='(%"Results for: %s")', filenames[i]
    print, FORMAT='(%"  Campaign:    %s")', campaign[i]
    print, FORMAT='(%"  Order:       %s")', order[i]
    print, FORMAT='(%"  Level:       %s")', level[i]
    print, FORMAT='(%"  Type:        %s")', type[i]
    print, FORMAT='(%"  Date:        %s")', date[i]
    print, FORMAT='(%"  Version:     %s")', version[i]
    print, ''
endfor

end