; docformat = 'rst'
;
; NAME:
;       MrMMS_Parse_Filename
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
;   Parse MMS file names.
;
; :Categories:
;       MMS
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
;       2015/03/01  -   Written by Matthew Argall
;       2017/07/12  -   Parse both data and ancillary files. - MRA
;-
;*****************************************************************************************
;+
;   Parse MMS data file names.
;
; :Params:
;       FILENAME:       in, required, type=string
;                       The filename of the MMS file to be parsed.
;
; :Keywords:
;       INSTR:          out, optional, type=string
;                       Instrument ID.
;       LEVEL:          out, optional, type=string
;                       Level of data production.
;       MODE:           out, optional, type=string
;                       Data capture mode.
;       OPTDESC:        out, optional, type=string
;                       Optional data product descriptor.
;       SC:             out, optional, type=string
;                       The spacecraft ID.
;       TSTART:         out, optional, type=string
;                       Start time of the data interval.
;       TEND:           out, optional, type=string
;                       End time of the data interval.
;       VERSION:        out, optional, type=string
;                       Version of the data file.
;-
PRO MrMMS_Parse_Filename_Data, filename, $
INSTR=instr, $
LEVEL=level, $
MODE=mode, $
OPTDESC=optdesc, $
SC=sc, $
TSTART=tstart, $
VERSION=version
	Compile_Opt idl2
	On_Error, 2
	
	nFiles  = N_Elements(filename)
	sc      = StrArr(nFiles)
	instr   = StrArr(nFiles)
	mode    = StrArr(nFiles)
	level   = StrArr(nFiles)
	optdesc = StrArr(nFiles)
	tstart  = StrArr(nFiles)
	version = StrArr(nFiles)
	
	;Parse the file name
	str = StRegEx(filename, '(mms[1-4])_'       + $                  ;Spacecraft ID
	                        '([a-z-]+)_'        + $                  ;Instrument ID
	                        '([a-z0-9]+)_'      + $                  ;Mode
	                        '([a-z0-4]+)_'      + $                  ;Data Level
	                        '(([a-zA-Z0-9-]*)_)?' + $                ;Optional Descriptor
	                        '([0-9]{4}[0-9]{2}[0-9]{2}[0-9]*)_' + $  ;Start Time
	                        'v([0-9]+\.[0-9]+\.[0-9]+)\.cdf', $      ;Version
	                        /EXTRACT, /SUBEXP)

	;Find non-matches
	iFail = Where(str[0,*] EQ '', nFail, COMPLEMENT=iPass, NCOMPLEMENT=nPass)
	IF nFail GT 0 THEN BEGIN
		MrPrintF, 'LogWarn', 'Cannot dissect filenames: '
		MrPrintF, 'LogText', '    "' + filename[iFail] + '"'
	ENDIF
	
	;Extract the subexpressions
	IF nPass GT 0 THEN BEGIN
		sc[iPass]      = Reform(str[1,iPass])
		instr[iPass]   = Reform(str[2,iPass])
		mode[iPass]    = Reform(str[3,iPass])
		level[iPass]   = Reform(str[4,iPass])
		optdesc[iPass] = Reform(str[6,iPass])
		tstart[iPass]  = Reform(str[7,iPass])
		version[iPass] = Reform(str[8,iPass])
	ENDIF
END


;+
;   Parse MMS ancillary file names.
;
; :Params:
;       FILENAME:       in, required, type=string
;                       The filename of the MMS file to be parsed.
;
; :Keywords:
;       INSTR:          out, optional, type=string
;                       Instrument ID.
;       SC:             out, optional, type=string
;                       The spacecraft ID.
;       TSTART:         out, optional, type=string
;                       Start time of the data interval.
;       TEND:           out, optional, type=string
;                       End time of the data interval.
;                       Version of the data file
;-
PRO MrMMS_Parse_Filename_Anc, filename, $
INSTR=instr, $
SC=sc, $
TEND=tend, $
TSTART=tstart, $
VERSION=version
	Compile_Opt idl2
	On_Error, 2
	
	nFiles  = N_Elements(filename)
	sc      = StrArr(nFiles)
	instr   = StrArr(nFiles)
	tstart  = StrArr(nFiles)
	tend    = StrArr(nFiles)
	version = StrArr(nFiles)
	
	;Parse the file name
	str = StRegEx(filename, '(MMS[1-4])_'          + $     ;Spacecraft ID
	                        '([A-Z]+)_'            + $     ;Ancillary Product
	                        '([0-9]{4}[0-9]{3})_'  + $     ;Start Time
	                        '([0-9]{4}[0-9]{3})\.' + $     ;End Time
	                        'V([0-9]+)', $                 ;Version
	                        /EXTRACT, /SUBEXP)

	;Find non-matches
	iFail = Where(str[0,*] EQ '', nFail, COMPLEMENT=iPass, NCOMPLEMENT=nPass)
	IF nFail GT 0 THEN BEGIN
		MrPrintF, 'LogWarn', 'Cannot dissect filenames: '
		MrPrintF, 'LogText', '    "' + filename[iFail] + '"'
	ENDIF
	
	;Extract the subexpressions
	IF nPass GT 0 THEN BEGIN
		sc[iPass]      = Reform(str[1,iPass])
		instr[iPass]   = Reform(str[2,iPass])
		tstart[iPass]  = Reform(str[3,iPass])
		tend[iPass]    = Reform(str[4,iPass])
		version[iPass] = Reform(str[5,iPass])
	ENDIF
END

;+
;   Parse MMS file names.
;
; :Params:
;       FILENAME:       in, required, type=string
;                       The filename of the MMS file to be parsed.
;
; :Keywords:
;       INSTR:          out, optional, type=string
;                       Instrument ID.
;       LEVEL:          out, optional, type=string
;                       Level of data production.
;       MODE:           out, optional, type=string
;                       Data capture mode.
;       OPTDESC:        out, optional, type=string
;                       Optional data product descriptor.
;       SC:             out, optional, type=string
;                       The spacecraft ID.
;       TSTART:         out, optional, type=string
;                       Start time of the data interval formatted as yyyymmddhhmmss,
;                           with irrelevant, least significant, fields dropped
;                           when files start on regular hourly or minute boundaries.
;       TEND:           out, optional, type=string
;                       End time of the data interval formatted as yyyymmddhhmmss,
;                           with irrelevant, least significant, fields dropped
;                           when files start on regular hourly or minute boundaries.
;       TT2000:         out, optional, type=long64
;                       Values of `TSTART` converted to TT2000 times.
;       VERSION:        out, optional, type=string
;                       Version of the data file, formatted as 'X.Y.Z'
;                           X - Interface number.  Increments in this number represent a
;                                   significant change to the processing software and/or to the contents of the 
;                                   file. These changes will likely break existing code that expects a specific 
;                                   file format (e.g. file reading software).  Additionally, increments in this 
;                                   number may require code changes to analysis software that expects the 
;                                   data to have been created using specific processing algorithms. The user 
;                                   should consult the appropriate meta-data for or changelogs.
;                           Y - Quality number. This number represents a change in the quality of
;                                   the data in the file, such as change in calibration or increase in fidelity. 
;                                   Changes should not impact software, but may require consideration when 
;                                   processing data.
;                           Z - Bug fix/revision number. This number changes to indicate minor
;                                   changes to the contents of the file due to reprocessing of missing data.  
;                                   Any dependent data products should generally be reprocessed if this value 
;                                   changes.
;       VX:             out, optional, type=string
;                       X-Version numbers
;       VY:             out, optional, type=string
;                       Y-Version numbers
;       VZ:             out, optional, type=string
;                       Z-Version numbers
;-
PRO MrMMS_Parse_Filename, filename, $
DIRECTORY=directory, $
INSTR=instr, $
LEVEL=level, $
MODE=mode, $
OPTDESC=optdesc, $
SC=sc, $
TSTART=tstart, $
TEND=tend, $
VERSION=version, $
VX=vx, $
VY=vy, $
VZ=vz, $
TT2000=tt2000
	Compile_Opt idl2
	On_Error, 2

	;Check that a filename was provided.
	tf_tt2000  = Arg_Present(tt2000)
	tf_version = Arg_Present(vx) || Arg_Present(vy) || Arg_Present(vz)
	if n_elements(filename) eq 0 then message, 'A file name must be given.'
	
	;Remove the directory, if it is present
	directory = file_dirname(filename)
	fname     = file_basename(filename)
	
;-----------------------------------------------------
; DISSECT FILENAMES \\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\
;-----------------------------------------------------
	;Separate ancillary from data files
	iAnc = Where( StRegEx(fname, '(DEF|PRED|ATTVAL|NAV|TIMEBIAS)', /BOOLEAN), nAnc, $
	              COMPLEMENT=iData, $
	              NCOMPLEMENT=nData )
	
	;Allocate memory
	nFiles  = nData + nAnc
	sc      = StrArr(nFiles)
	instr   = StrArr(nFiles)
	mode    = StrArr(nFiles)
	level   = StrArr(nFiles)
	optdesc = StrArr(nFiles)
	tstart  = StrArr(nFiles)
	tend    = StrArr(nFiles)
	version = StrArr(nFiles)
	IF tf_version THEN BEGIN
		vx = StrArr(nFiles)
		vy = StrArr(nFiles)
		vz = StrArr(nFiles)
	ENDIF
	IF tf_tt2000 THEN tt2000 = L64IndGen(nFiles)
	
;-----------------------------------------------------
; Parse Ancillary Files \\\\\\\\\\\\\\\\\\\\\\\\\\\\\\
;-----------------------------------------------------
	IF nAnc GT 0 THEN BEGIN
		;Parse files
		MrMMS_Parse_Filename_Anc, fname[iAnc], $
		                          SC      = sc_anc, $
		                          INSTR   = instr_anc, $
		                          TSTART  = tstart_anc, $
		                          TEND    = tend_anc, $
		                          VERSION = version_anc
		
		;Store data
		sc[iAnc]      = Temporary(sc_anc)
		instr[iAnc]   = Temporary(instr_anc)
		tstart[iAnc]  = Temporary(tstart_anc)
		tend[iAnc]    = Temporary(tend_anc)
		version[iAnc] = Temporary(version_anc)
		
		;TT2000 time stamps
		IF tf_tt2000 THEN BEGIN
			syear  = Fix(StrMid(tstart_anc, 0, 4))
			smonth = Fix(StrMid(tstart_anc, 4, 2))
			sday   = Fix(StrMid(tstart_anc, 6, 2))
			eyear  = Fix(StrMid(tend_anc, 0, 4))
			emonth = Fix(StrMid(tend_anc, 4, 2))
			eday   = Fix(StrMid(tend_anc, 6, 2))
			tt2000[iAnc] = MrCDF_Epoch_Compute(syear, smonth, sday, 0, 0, 0, 0, 0, 0)
		ENDIF
	ENDIF
	
;-----------------------------------------------------
; Parse Data Files \\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\
;-----------------------------------------------------
	IF nData GT 0 THEN BEGIN
		MrMMS_Parse_Filename_Data, fname[iData], $
		                           SC      = sc_data, $
		                           INSTR   = instr_data, $
		                           MODE    = mode_data, $
		                           LEVEL   = level_data, $
		                           OPTDESC = optdesc_data, $
		                           TSTART  = tstart_data, $
		                           VERSION = version_data
		
		;Store data
		sc[iData]        = Temporary(sc_data)
		instr[iData]     = Temporary(instr_data)
		mode[iData]      = Temporary(mode_data)
		level[iData]     = Temporary(level_data)
		optdesc[iData]   = Temporary(optdesc_data)
		tstart[iData]    = tstart_data
		version[iData]   = version_data
		
		;Dissect the version numbers
		IF tf_version THEN BEGIN
			parts     = StRegEx(version_data, '([0-9]+)\.([0-9]+)\.([0-9]+)', /SUBEXP, /EXTRACT)
			vx[iData] = Reform(parts[1,*])
			vy[iData] = Reform(parts[2,*])
			vz[iData] = Reform(parts[3,*])
		ENDIF
		
		;TT2000 time stamps
		IF tf_tt2000 THEN BEGIN
			;Reform
			year   = fix(strmid(tstart_data,  0, 4))
			month  = fix(strmid(tstart_data,  4, 2))
			day    = fix(strmid(tstart_data,  6, 2))
			hour   = fix(strmid(tstart_data,  8, 2))
			minute = fix(strmid(tstart_data, 10, 2))
			second = fix(strmid(tstart_data, 12, 2))
			tt2000[iData] = MrCDF_Epoch_Compute(year, month, day, hour, minute, second, 0, 0, 0)
		ENDIF
	ENDIF
	
;-----------------------------------------------------
; Finish \\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\
;-----------------------------------------------------
	;Return scalars
	IF nFiles EQ 1 THEN BEGIN
		sc      = sc[0]
		instr   = instr[0]
		mode    = mode[0]
		level   = level[0]
		optdesc = optdesc[0]
		tstart  = tstart[0]
		tend    = tend[0]
		version = version[0]
		
		IF tf_version THEN BEGIN
			vx = vx[0]
			vy = vy[0]
			vz = vz[0]
		ENDIF
		
		IF tf_tt2000 THEN tt2000 = tt2000[0]
	ENDIF
END

;-----------------------------------------------------
;Main Level Example Program \\\\\\\\\\\\\\\\\\\\\\\\\\
;-----------------------------------------------------

;pick a few filenames
filenames = ['mms3_dfg_hr_l2p_duration-1h1m_20010704_v1.0.0.cdf', $
             'mms3_dfg_hr_l2p_duration-1h1m_20010704_v1.0.0.cdf', $
             'mms3_edi_fast_l1a_beam_20010704_v0.2.0.cdf', $
             'mms3_edi_fast_l1a_beam_20010704_v0.2.0.cdf']

;print, the filenames
print, format='(%"The filenames used are: %s")', filenames[0]
print, format='(%"                        %s")', filenames[1]
print, format='(%"                        %s")', filenames[2]
print, format='(%"                        %s")', filenames[3]

;Dissect all of the names
mrmms_parse_filename, filenames, $
                      OPTDESC = descriptor, $
                      INSTR   = instrument, $
                      LEVEL   = level, $
                      MODE    = mode, $
                      TSTART  = start_time, $
                      SC      = spacecraft, $
                      VERSION = version

;breakdown the filenames and display the results.
for i = 0, n_elements(filenames) - 1 do begin
    print, '----------------------------------------------------------------------'
    print, format='(%"Results for: %s")', filenames[i]
    print, FORMAT='(%"  Spacecraft:    %s")', spacecraft[i]
    print, FORMAT='(%"  Instrument:    %s")', instrument[i]
    print, FORMAT='(%"  Mode:          %s")', mode[i]
    print, FORMAT='(%"  Level:         %s")', level[i]
    print, FORMAT='(%"  Descriptor:    %s")', descriptor[i]
    print, FORMAT='(%"  Start Time:    %s")', start_time[i]
    print, FORMAT='(%"  Version:       %s")', version[i]
    print, ''
endfor

end