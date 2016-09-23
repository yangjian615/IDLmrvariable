; docformat = 'rst'
;
; NAME:
;       MrMMS_Farrugia_ASCII_Data
;
; PURPOSE:
;+
;   Get and convert MMS CDF files to ASCII files.
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
;       2016/08/22  -   Written by Matthew Argall
;-
pro MrMMS_Farrugia_ASCII_Data
	compile_opt idl2
	
	catch, the_error
	if the_error ne 0 then begin
		catch, /CANCEL
		MrPrintF, 'LogErr'
		return
	endif
	
	;Directory where files are saved
	ascii_dir = '/Users/argall/Desktop/data_dump/mms_data'
	if ~file_test(ascii_dir, /DIRECTORY) then file_mkdir, ascii_dir

	;Set the date-time range
	MrVar_SetTRange, ['2015-12-31T18:00:00', '2016-01-01T06:00:00']

	;Get FGM & FPI data
	fgm_files = MrMMS_Get_Data(['mms1', 'mms2', 'mms3', 'mms4'], 'fgm', 'srvy', 'l2', COUNT=nFGM)
	fpi_files = MrMMS_Get_Data(['mms1', 'mms2', 'mms3', 'mms4'], 'fpi', 'fast', 'l2', OPTDESC=['des-moms', 'dis-moms'], COUNT=nFPI)
	
	;Variables to read
	fgm_fmt = '*_b_gse_*'
	fpi_fmt = ['*bulk?_dbcs*', '*density_dbcs*', '*temp' + ['xx_dbcs', 'yy_dbcs', 'zz_dbcs', 'para', 'perp'] + '_*']

	;Convert files to ASCII
	fout = strarr(nFGM + nFPI)
	for i = 0, nFGM - 1 do fout[i]      = MrCDF_ToASCII(fgm_files[i], VARFORMAT=fgm_fmt, DIRECTORY=ascii_dir)
	for i = 0, nFPI - 1 do fout[nFGM+i] = MrCDF_ToASCII(fpi_files[i], VARFORMAT=fpi_fmt, DIRECTORY=ascii_dir)
end