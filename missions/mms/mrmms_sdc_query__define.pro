; docformat = 'rst'
;
; NAME:
;       MrMMS_SDC_Query__DEFINE
;
;*****************************************************************************************
;   Copyright (c) 2016, University of New Hampshire                                      ;
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
;       * Neither the name of the University of New Hampshire nor the names of its       ;
;         contributors may be used to endorse or promote products derived from this      ;
;         software without specific prior written permission.                            ;
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
;   An interface into the MMS Science Data Center
;
;   Class Structure:
;       MrMMS_SDC_Query
;           MrWebURI
;               IDLnetURL
;               MrURI
;                   IDL_Object
;
;   OFFLINE MODE:
;      ::SetURI  -- Parses the URI and sets object properties. Does not check if the URI
;                   is valid. This is because ::CD will pass it a URI with a query string,
;                   which means there is not a 1-to-1 correspondence between the remote
;                   URI and the local URI.
;      ::Get     -- Will call ::GetLocalFiles instead of ::GetFileNames
;      ::uri2dir -- Since ::GetLocalFiles already returns local file paths, this method
;                   will return the URI path instead of trying to convert a remote path to
;                   a local path.
;
;   NO_DOWNLOAD:
;      ::Get     -- Search for files remotely, then find their local counter-parts. If files
;                   are not found locally, they are ignored.
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
; :History:
;   Modification History::
;       2016/06/30  -   Written by Matthew Argall
;       2016/08/30  -   Bump "End_Date" field up to next day if time is not 00:00:00. - MRA
;-
;*****************************************************************************************
;+
;   The purpose of this method is to provide information when implied print is used.
;   is called.
;-
function MrMMS_SDC_Query::_OverloadPrint
	compile_opt idl2
	on_error, 2
	
	;Get info from the superclass
	web_print = self -> MrWebURI::_OverloadPrint()
	
	;Create strings
	tstart = string('  TStart', '=', self.tstart, FORMAT='(a-26, a-2, a0)')
	tend   = string('  TEnd',   '=', self.tend,   FORMAT='(a-26, a-2, a0)')

	;Output array
	outStr = [ [ web_print ], $
	           [ tstart    ], $
	           [ tend      ] $
	         ]
	
	;Sort alphabetically
	outStr = outStr[0,sort(outStr)]

	;Print the array
	return, outStr
end

;+
;   Build a URI
;
; :Keywords:
;       RESET_PATH:     in, optional, type=boolean, default=0
;                       If set, the path will be returned to the default path. This is
;                           equivalent to setting `PUBLIC`, `DOWNLOAD`, and `SCIENCE`.
;       RESET_QUERY:    in, optional, type=boolean, default=0
;                       If set, no information from previous queries will be retained.
;                           The default is to use values from the previous query unless
;                           otherwise provided via the `SC`, `INSTR`, `MODE`, `LEVEL`,
;                           `OPTDESC`, `TSTART`, `TEND`, or `ANC_PRODUCT` keywords.
;                           This keyword takes precedence over the aforementioned
;                           keywords.
;
;       ANCILLARY:      in, optional, type=boolean, default=0
;                       If set, the request will be for anciallary data products.
;                           Cannot be used with the `HK`, `SCIENCE`, or `VERSION`.
;       HK:             in, optional, type=boolean, default=0
;                       If set, the request will be for house keeping data products.
;                           Cannot be used with the `ANCILLARY`, `SCIENCE`, or `VERSION`.
;       SCIENCE:        in, optional, type=boolean, default=0
;                       If set, the request will be for science data products.
;                           Cannot be used with the `ANCILLARY`, or `HK`. If
;                           None of the three keywords are set, the current path will
;                           be checked for one of those options. If still nothing is found
;                           then SCIENCE will be the default.
;
;       DOWNLOAD:       in, optional, type=boolean, default=0
;                       If set, the URL will request that files be downloaded. Cannot
;                           be used with `INFO`, `NAMES`, or `V_INFO`. If none of the
;                           four keywords are set, the current URI will be examined for
;                           a choice. If still nothing has been selected, DOWNLOAD will
;                           be the default.
;       FILE_INFO:      in, optional, type=boolean, default=0
;                       If set, the URL will request file information. Cannot be used
;                           with `DOWNLOAD`, `NAMES`, or `V_INFO`
;       FILE_NAMES:     in, optional, type=boolean, default=0
;                       If set, the URL will requets file names. Cannot be used with
;                           `DOWNLOAD`, `INFO`, or `V_INFO`
;       V_INFO:         in, optional, type=boolean, default=0
;                       If set, the request will be for file version information.
;                           Cannot be used with the `ANCILLARY` or `HK`, or with
;                           `DOWNLOAD`, `INFO`, or `NAMES`.
;
;       PUBLIC:         in, optional, type=boolean, default=0
;                       If set, the public site will be queried (no password required,
;                           contains data of quality L2 and above). Cannot be used
;                           with `TEAM`. If neither keywords are set, the current URL
;                           is examined. If still neither are chosen, PUBLIC is the default.
;       TEAM:           in, optional, type=string/strarr
;                       If set, the public site will be queried (password required,
;                           contains data of quality L1A and above). Cannot be used
;                           with `TEAM`.
;
;       ANC_PRODUCT:    in, optional, type=string/strarr
;                       Ancillary data product names. Should only be used with `ANCILLARY`.
;       FILES:          in, optional, type=string/strarr, default=''
;                       Names of the files to be downloaded. If set, then `RESET_QUERY`, 
;                           will be set.
;       INSTR:          in, optional, type=string/strarr
;                       Instrument ID.
;       LEVEL:          in, optional, type=string/strarr
;                       Data quality level.
;       MODE:           in, optional, type=string/strarr
;                       Data rate mode.
;       OPTDESC:        in, optional, type=string/strarr
;                       Optional file name descriptor.
;       SC_ID:          in, optional, type=string/strarr
;                       Spacecraft ID. Options are: {'mms1' | 'mms2' | 'mms3' | 'mms4'}
;       TEND:           in, optional, type=string
;                       End time of data interval. Must be formatted as YYYY-MM-DD or
;                           YYYY-MM-DDThh:mm:ss
;       TSTART:         in, optional, type=string/strarr
;                       Start time of data interval. Must be formatted as YYYY-MM-DD or
;                           YYYY-MM-DDThh:mm:ss
;       VERSION:        in, optional, type=string/strarr
;                       File name version number, formatted as X.Y.Z, where X, Y, and Z
;                           are integers.
;
; :Returns:
;       URL:            in, optional, type=string
;                       URL build from the keyword parameters.
;-
function MrMMS_SDC_Query::BuildURI, $
ANC_PRODUCT=ancprod, $
ANCILLARY=ancillary, $
DOWNLOAD=download, $
HK=hk, $
FILES=files, $
FILE_INFO=info, $
FILE_NAMES=names, $
INSTR=instr, $
LEVEL=level, $
MODE=mode, $
OPTDESC=optdesc, $
PUBLIC_SITE=public, $
RESET_PATH=reset_path, $
RESET_QUERY=reset_query, $
SCIENCE=science, $
SC_ID=sc_id, $
TEAM_SITE=team, $
TEND=tend, $
TSTART=tstart, $
V_INFO=v_info, $
VERSION=version
	compile_opt idl2
	on_error, 2
	
;---------------------------------------------------------------------
; Check Keywords /////////////////////////////////////////////////////
;---------------------------------------------------------------------

	;Type of information to get
	tf_reset_path  = keyword_set(reset_path)
	tf_reset_query = keyword_set(reset_query)
	
	tf_science     = keyword_set(science)
	tf_anc         = keyword_set(ancillary)
	tf_hk          = keyword_set(hk)
	
	tf_info        = keyword_set(info)
	tf_names       = keyword_set(names)
	tf_v_info      = keyword_set(v_info)
	tf_download    = keyword_set(download)
	
	tf_team        = keyword_set(team)
	tf_public      = keyword_set(public)
	
	;If FILES is provided, other query parameters are ignored.
	if n_elements(files) gt 0 then tf_reset_query = 1B

	;Download files by their file name
;	if n_elements(files) gt 0 then begin
;		tf_download    = 1B
;		tf_names       = 0B
;		tf_info        = 0B
;		tf_v_info      = 0B
;		tf_science     = 1B
;		tf_anc         = 0B
;		tf_hk          = 0B
;		tf_reset_query = 1B
;	endif else begin
;		files = ''
;	endelse
	
	;Restrictions
	if tf_v_info && (tf_anc || tf_hk) then message, 'V_INFO cannot be used with ANCILLARY or HK.'
	if tf_anc && tf_hk                then message, 'ANCILLARY and HK are mutually exclusive.'
	if tf_team && tf_public           then message, 'TEAM_SITE and PUBLIC_SITE are mutually exclusive.'
	if tf_names + tf_info + tf_v_info + tf_download gt 1 $
		then message, 'Only one of FILE_NAMES, FILE_INFO, V_INFO, and DOWNLOAD may be selected.'

;---------------------------------------------------------------------
; URL Scheme & Host //////////////////////////////////////////////////
;---------------------------------------------------------------------
	;SCHEME and HOST are always the same
	uri_scheme = 'https'
	uri_host   = 'lasp.colorado.edu'
	
	;Get the old path
	if tf_reset_path then begin
		oldPath = ''
	endif else begin
		uri     = self -> GetURI()
		oldPath = self.path
	endelse

;---------------------------------------------------------------------
; Team or Public /////////////////////////////////////////////////////
;---------------------------------------------------------------------
	;Team site
	case 1 of
		tf_team:   site = 'sitl'
		tf_public: site = 'public'
		
		;Use the old path, if possible. Othewise, default to public site
		else: begin
			parts = stregex(oldPath, '(sitl|public)', /SUBEXP, /EXTRACT)
			site = parts[1] eq '' ? 'public' : parts[1]
		endcase
	endcase

;---------------------------------------------------------------------
; Names | Info | Version | Download //////////////////////////////////
;---------------------------------------------------------------------
	case 1 of
		tf_names:    info_type = 'file_names'
		tf_info:     info_type = 'file_info'
		tf_v_info:   info_type = 'version_info'
		tf_download: info_type = 'download'
		else: begin
			parts = stregex(oldPath, '(file_names|file_info|version_info|download)', /SUBEXP, /EXTRACT)
			info_type = parts[1] eq '' ? 'download' : parts[1]
		endcase
	endcase

;---------------------------------------------------------------------
; Ancillary | HK | Science ///////////////////////////////////////////
;---------------------------------------------------------------------
	
	;Type of data
	case 1 of
		tf_anc:     data_type = 'ancillary'
		tf_hk:      data_type = 'hk'
		tf_science: data_type = 'science'
		else: begin
			parts = stregex(oldPath, '(ancillary|hk|science)', /SUBEXP, /EXTRACT)
			data_type = parts[1] eq '' ? 'science' : parts[1]
		endcase
	endcase

;---------------------------------------------------------------------
; Form the Path //////////////////////////////////////////////////////
;---------------------------------------------------------------------
	uri_path = '/mms/sdc/' + site + '/files/api/v1/' + info_type + '/' + data_type

;---------------------------------------------------------------------
; Construct the Query ////////////////////////////////////////////////
;---------------------------------------------------------------------
	;Default values
	if n_elements(sc_id)   eq 0 then sc_id   = tf_reset_query ? '' : (*self.field_values).sc_id
	if n_elements(instr)   eq 0 then instr   = tf_reset_query ? '' : (*self.field_values).instrument_id
	if n_elements(mode)    eq 0 then mode    = tf_reset_query ? '' : (*self.field_values).data_rate_mode
	if n_elements(level)   eq 0 then level   = tf_reset_query ? '' : (*self.field_values).data_level
	if n_elements(optdesc) eq 0 then optdesc = tf_reset_query ? '' : (*self.field_values).descriptor
	if n_elements(version) eq 0 then version = tf_reset_query ? '' : (*self.field_values).version
	if n_elements(ancprod) eq 0 then ancprod = tf_reset_query ? '' : (*self.field_values).product
	if n_elements(files)   eq 0 then files   = tf_reset_query ? '' : (*self.field_values).file
	if n_elements(tstart)  eq 0 then tstart  = tf_reset_query ? '' : self.tstart
	if n_elements(tend)    eq 0 then tend    = tf_reset_query ? '' : self.tend
	
	;Query string
	;   - Does not like empty fields
	uri_query = ''
	if sc_id[0]   ne '' then uri_query += ( n_elements(sc_id)    eq 1 ? 'sc_id='          + sc_id   : 'sc_id='          + strjoin(sc_id,   ',') ) + '&'
	if instr[0]   ne '' then uri_query += ( n_elements(instr)    eq 1 ? 'instrument_id='  + instr   : 'instrument_id='  + strjoin(instr,   ',') ) + '&'
	if mode[0]    ne '' then uri_query += ( n_elements(mode)     eq 1 ? 'data_rate_mode=' + mode    : 'data_rate_mode=' + strjoin(mode,    ',') ) + '&'
	if level[0]   ne '' then uri_query += ( n_elements(level)    eq 1 ? 'data_level='     + level   : 'data_level='     + strjoin(level,   ',') ) + '&'
	if optdesc[0] ne '' then uri_query += ( n_elements(optdesc)  eq 1 ? 'descriptor='     + optdesc : 'descriptor='     + strjoin(optdesc, ',') ) + '&'
	if version[0] ne '' then uri_query += ( n_elements(version)  eq 1 ? 'version='        + version : 'version='        + strjoin(version, ',') ) + '&'
	if ancprod[0] ne '' then uri_query += ( n_elements(ancprod)  eq 1 ? 'product='        + ancprod : 'ancprod='        + strjoin(ancprod, ',') ) + '&'
	if files[0]   ne '' then uri_query += ( n_elements(files)    eq 1 ? 'file='           + files   : 'file='           + strjoin(files,   ',') ) + '&'
	
	
	;The query requires two dates from two different days
	;   - If we want data from a single day, then some fudging needs to be done
	date_start = tstart
	date_end   = tend
	if date_start ne '' && date_end ne '' then begin
		;Extract the date part
		date_start = strmid(tstart, 0, 10)
		date_end   = strmid(tend,   0, 10)
		time_end   = strmid(tend,  11,  8)
		if time_end eq '' then time_end = '00:00:00'

		;Are they the same?
		if date_start eq date_end || time_end ne '00:00:00' then begin
			;Parse the date
			month = fix(strmid(date_end, 5, 2))
			day   = fix(strmid(date_end, 8, 2))
			year  = fix(strmid(date_end, 0, 4))
			
			;Bump the end date up by one day
			cdf_tt2000, tt_end, year, month, day+1, /COMPUTE_EPOCH
			date_end = strmid(cdf_encode_tt2000(tt_end), 0, 10)
		endif
	endif

	;TSTART
	if tstart ne '' then begin
		;2015-05-14T12:00:00
		if MrTokens_IsMatch(tstart, '%Y-%M-%dT%H:%m:%S') then begin
			uri_query  += 'start_date=' + date_start + '&'
			self.tstart = tstart
		;2015-05-14
		endif else if MrTokens_IsMatch(tstart, '%Y-%M-%d') then begin
			uri_query  += 'start_date=' + strmid(date_start, 0, 10) + '&'
			self.tstart = tstart + 'T00:00:00'
		;Unrecognized format
		endif else begin
			message, 'Invalid format for TSTART: "' + tstart + '".'
		endelse
	endif else begin
		if tf_reset_query && files[0] eq '' then self.tstart = ''
	endelse

	;TEND
	if tend ne '' then begin
		;2015-05-14T12:00:00
		if MrTokens_IsMatch(tend, '%Y-%M-%dT%H:%m:%S') then begin
			uri_query += 'end_date=' + strmid(date_end, 0, 10) + '&'
			self.tend  = tend
		;2015-05-14
		endif else if MrTokens_IsMatch(tend, '%Y-%M-%d') then begin
			uri_query += 'end_date=' + date_end + '&'
			self.tend  = tend + 'T24:00:00'
		;Unrecognized format
		endif else begin
			message, 'Invalid format for TEND: "' + tend + '".'
		endelse
	endif else begin
		if tf_reset_query && files[0] eq '' then self.tend = ''
	endelse
	
	;Trim the trailing "&"
	uri_query = strmid(uri_query, 0, strlen(uri_query)-1)

;---------------------------------------------------------------------
; Create the URI /////////////////////////////////////////////////////
;---------------------------------------------------------------------

	uri = self -> MrURI::BuildURI( HOST     = uri_host, $
	                               PATH     = uri_path, $
	                               QUERY    = uri_query, $
	                               SCHEME   = uri_scheme )
	
	return, uri
end


;+
;   Create a link that will allow you to download data from the MMS SDC.
;
; :Params:
;       URI:                in, optional, type=string
;                           The destination URI. If not provided, `_REF_EXTRA` keywords
;                               must be given.
;
; :Keywords:
;       SUCCESS:            out, required, type=boolean
;                           A flag indicating whether the operaion was successful.
;       _REF_EXTRA:         in, optional, type=any
;                           Any keyword accepted by the BuildURI method is accepted via
;                               via keyword inheritance.
;-
pro MrMMS_SDC_Query::CD, uri, $
SUCCESS=success, $
_REF_EXTRA=extra
	compile_opt idl2
	on_error, 2
	
	;URI or Keywords must be given
	if (n_elements(uri) gt 0) + (n_elements(extra) gt 0) ne 1 $
		then message, 'Either a URI or URI keywords must be given.'

	;Create the URI
	if n_elements(uri) eq 0 then uri = self -> BuildURI( _STRICT_EXTRA = extra )

	;Set the URI
	self -> SetURI, uri, success
	
	;Success status
	if ~success && ~arg_present(success) then message, 'Cannot change directories.'
end


;+
;   Download a single file from the SDC.
;
; :Params:
;       FILENAME:       in, optional, type=string
;                       Name given to the file being downloaded. By default, the
;                           server-provided name will be used.
;
; :Returns:
;       FILEOUT:         Names of the downloaded files.
;-
function MrMMS_SDC_Query::Download, filename
	compile_opt idl2

	;Catch errors
	catch, the_error
	if the_error ne 0 then begin
		catch, /CANCEL
		if n_elements(fileOut) gt 0 && file_test(fileOut) then file_delete, fileOut
		MrPrintF, 'LogErr'
		return, ''
	endif

;---------------------------------------------------------------------
; Download ///////////////////////////////////////////////////////////
;---------------------------------------------------------------------

	;Get data.
	;   - Cookies (credentials) are stored in HEADER, which is reset by GET.
	fileOut = self -> IDLnetURL::Get(FILENAME=filename)
	
;---------------------------------------------------------------------
; Clean Up ///////////////////////////////////////////////////////////
;---------------------------------------------------------------------
	
	;Return the file
	return, fileOut
end


;+
;   Filter files by time and version.
;
; :Private:
;
; :Params:
;       FILENAMES:      in, required, type=string/strarr
;                       File names to be filtered.
;
; :Keywords:
;       COUNT:          out, optional, type=integer
;                       Number of files that pass the filter.
;
; :Returns:
;       FILES:          Files that pass the filter.
;-
function MrMMS_SDC_Query::FilterFiles, filenames, $
COUNT=count
	compile_opt idl2
	on_error, 2
	
;---------------------------------------------------------------------
; Filter Time ////////////////////////////////////////////////////////
;---------------------------------------------------------------------
	
	;Time filter
	;   - Required because ::GetFileNames ignores the hour, minute, second
	;   - Must do survey and burst separately because their times overlap
	iBrst = where( stregex(filenames, 'brst', /BOOLEAN), nBrst, COMPLEMENT=iSrvy, NCOMPLEMENT=nSrvy )
	if nSrvy gt 0 then srvy_files = self -> FilterTime(filenames[iSrvy], self.tstart, self.tend, COUNT=nSrvy)
	if nBrst gt 0 then brst_files = self -> FilterTime(filenames[iBrst], self.tstart, self.tend, COUNT=nBrst)

	;Check results
	count = nSrvy + nBrst
	if count eq 0 then return, ''
	
;---------------------------------------------------------------------
; Filter Version /////////////////////////////////////////////////////
;---------------------------------------------------------------------
	
	;Version filter
	;   - If a specific file is downloaded (as is done below), only the most recent
	;     version of a file will be downloaded. If an older version is specified, no
	;     file is returned.
	files = strarr(count)
	if nSrvy gt 0 then srvy_files = self -> FilterVersion(srvy_files, COUNT=nSrvy)
	if nBrst gt 0 then brst_files = self -> FilterVersion(brst_files, COUNT=nBrst)
	
;---------------------------------------------------------------------
; Combine Results ////////////////////////////////////////////////////
;---------------------------------------------------------------------
	case 1 of
		nSrvy gt 0 && nBrst gt 0: files = [temporary(srvy_files), temporary(brst_files)]
		nSrvy gt 0: files = temporary(srvy_files)
		nBrst gt 0: files = temporary(brst_files)
		else:       files = ''
	endcase
	count = nSrvy + nBrst
	
	;Return
	return, files
end


;+
;   Filter files by time. Burst and Srvy files should be filtered separately.
;
; :Private:
;
; :Params:
;       FILENAMES:      in, required, type=string/strarr
;                       File names to be filtered.
;       TSTART:         in, required, type=string
;                       Start time of filter.
;       TEND:           in, required, type=string
;                       End time of filter.
;
; :Keywords:
;       COUNT:          out, optional, type=integer
;                       Number of files that pass the filter.
;
; :Returns:
;       FILES_OUT:      Files that pass the filter.
;-
function MrMMS_SDC_Query::FilterTime, filenames, tstart, tend, $
COUNT=count
	compile_opt idl2
	on_error, 2

;------------------------------------;
; Sort Time                          ;
;------------------------------------;

	;Parse the file names
	MrMMS_Parse_Filename, filenames, TSTART=fstart

	;Parse the start times
	;   - Convert to TT2000
	MrMMS_Parse_Time, fstart, year, month, day, hour, minute, second, /INTEGER
	cdf_tt2000, tt2000, year, month, day, hour, minute, second, /COMPUTE_EPOCH

	;Sort by time
	nfiles    = n_elements(filenames)
	isort     = sort(tt2000)
	files_out = filenames[isort]
	tt2000    = tt2000[isort]

;------------------------------------;
; Filter Time                        ;
;------------------------------------;

	;TT2000 values for the start and end times
	MrMMS_Parse_Time, strjoin(strsplit(tstart, '-T:', /EXTRACT)), syr, smo, sday, shr, smnt, ssec, /INTEGER
	MrMMS_Parse_Time, strjoin(strsplit(tend, '-T:', /EXTRACT)),   eyr, emo, eday, ehr, emnt, esec, /INTEGER
	cdf_tt2000, tt2000_start, syr, smo, sday, shr, smnt, ssec, /COMPUTE_EPOCH
	cdf_tt2000, tt2000_end,   eyr, emo, eday, ehr, emnt, esec, /COMPUTE_EPOCH

	;Filter files by end time
	;   - Any files that start after TEND can be discarded
	iend = where( tt2000 le tt2000_end[0], nend )
	if nend gt 0 then begin
		files_out = files_out[iend]
		tt2000    = tt2000[iend]
	endif

	;Filter files by begin time
	;   - Any file with TSTART < TRANGE[0] can potentially have data
	;     in our time interval of interest.
	;   - Assume the start time of one file marks the end time of the previous file.
	;   - With this, we look for the file that begins just prior to TRANGE[0] and
	;     throw away any files that start before it.
	istart = where( tt2000 le tt2000_start[0], nstart )
	if nstart gt 0 then begin
		;Select the file time that starts closest to the given time without
		;going over.
		istart = istart[nstart-1]
	
		;Find all files with start time on or after the selected time
		ifiles = where(tt2000 ge tt2000[istart], count)
		if count gt 0 then begin
			files_out = files_out[ifiles]
			tt2000    = tt2000[ifiles]
		endif
	endif

	;Number of files kept
	;   - If TRANGE[0] < TSTART of first file, COUNT will be zero
	;   - However, there still may be files with TRANGE[0] < TSTART < TRANGE[1]
	if nstart eq 0 && nend eq 0 then begin
		return, ''
	endif
	count = n_elements(files_out)

	;The last caveat:
	;   - Our filter may be too lenient. The first file may or may not contain
	;     data within our interval.
	;   - Check if it starts on the same day. If not, toss it
	;   - There may be many files with the same start time, but different
	;     version numbers. Make sure we get all copies of the first start
	;     time.
	cdf_tt2000, tt2000[0], year, month, day, hour, /BREAKDOWN, /TOINTEGER
	if year ne syr || month ne smo || day ne sday then begin
		;Filter out all files matching the first starting date
		ibad = where( tt2000 eq tt2000[0], nbad, COMPLEMENT=igood, NCOMPLEMENT=count)
	
		;Extract files
		if count gt 0 then begin
			files_out = files_out[igood]
		endif else begin
			message, 'No files in time interval.', /INFORMATIONAL
			files_out = ''
		endelse
	endif
	
	return, files_out
end


;+
;   Filter files by version number.
;
; :Private:
;
; :Params:
;       FILENAMES:      in, required, type=string/strarr
;                       File names to be filtered.
;
; :Keywords:
;       COUNT:          out, optional, type=integer
;                       Number of files that pass the filter.
;       LATEST_VERSION: in, optional, type=boolean, default=0
;                       If set, only the newest version of the files is returned. This
;                           is the default if `MIN_VERSION` and `VERSION` are not set.
;       MIN_VERSION:    in, optional, type=string, default=''
;                       A version number formatted as 'X.Y.Z'. Files with
;                           versions smaller this version will be filtered out.
;       VERSION:        in, optional, type=string, default=''
;                       A version number formatted as 'X.Y.Z'. Only files of
;                           this version will be returned.
;
; :Returns:
;       FILES_OUT:      Files that pass the filter.
;-
function MrMMS_SDC_Query::FilterVersion, filenames, $
COUNT=count, $
LATEST_VERSION=latest_version, $
MIN_VERSION=min_version, $
VERSION=version
	compile_opt idl2
	on_error, 2
	
	;Defaults
	tf_latest = keyword_set(latest_version)
	tf_checkv = n_elements(version)     gt 0
	tf_minv   = n_elements(min_version) gt 0
	if tf_checkv + tf_minv + tf_latest  eq 0 then tf_latest = 1B
	if tf_checkv + tf_minv + tf_latest  gt 1 $
		then message, 'VERSION, MIN_VERSION and LATEST_VERSION are mutually exclusive.'

	;Results
	files_out = filenames
	count     = n_elements(filenames)

	;Extract X, Y, Z version numbers from file
	fversion = stregex(files_out, 'v([0-9]+)\.([0-9]+)\.([0-9]+)\.cdf$', /SUBEXP, /EXTRACT)
	fv = fix(fversion[1:3,*])

;------------------------------------;
; Minimum Version                    ;
;------------------------------------;
	if tf_minv then begin
		;Version numbers to match
		min_vXYZ = fix(strsplit(min_version, '.', /EXTRACT))
	
		;Select file versions
		iv = where(   (fv[0,*] gt min_vXYZ[0]) or $
		            ( (fv[0,*] eq min_vXYZ[0]) and (fv[1,*] gt min_vXYZ[1]) ) or $
		            ( (fv[0,*] eq min_vXYZ[0]) and (fv[1,*] eq min_vXYZ[1]) and (fv[2,*] ge min_vXYZ[2]) ), count )
		if count gt 0 then files_out = files_out[iv]

;------------------------------------;
; Exact Version                      ;
;------------------------------------;
	endif else if tf_checkv then begin
		;Version numbers to match
		vXYZ  = fix(strsplit(version, '.', /EXTRACT))
		
		;Select file versions
		iv = where( (fv[0,*] eq vXYZ[0]) and (fv[1,*] eq vXYZ[1]) and (fv[2,*] eq vXYZ[2]), count )
		if count gt 0 then files_out = files_out[iv]

;------------------------------------;
; Latest Version                     ;
;------------------------------------;
	endif else begin
		;Step through X, Y, Z version numbers
		for i = 0, 2 do begin
			;Find latest X, Y, Z version
			iv = where( fv[i,*] eq max(fv[i,*]), count)
			
			;Select version
			fv        = fv[*,iv]
			files_out = files_out[iv]
		endfor
	endelse
	
	;Return
	if count eq 0 then files_out = ''
	if count eq 1 then files_out = files_out[0]
	return, files_out
end


;+
;   Download files one-by-one.
;
; :Keywords:
;       COUNT:          out, optional, type=integer
;                       Number of file names returned.
;
; :Returns:
;       FILENAMES:      Names of files that match search criteria
;-
function MrMMS_SDC_Query::Get, $
COUNT=count
	compile_opt idl2

	;Catch errors
	catch, the_error
	if the_error ne 0 then begin
		catch, /CANCEL
		
		;Delete progress bar
		if n_elements(progress_bar) gt 0 && obj_valid(progress_bar) $
			then obj_destroy, progress_bar
		
		;Reset the callback function
		self -> SetProperty, CALLBACK_FUNCTION='MrWebURI_Callback'
		
		;Report error
		MrPrintF, 'LogErr'
		
		;CD to original directory
		;   - ::CD calls ::GetResponseHeader, which purposely generates an
		;     error. Report this error before calling ::CD
		if n_elements(uri) gt 0 then self -> CD, oldURI
		
		count = 0
		return, ''
	endif
	
	;Get the current URI
	oldURI = self -> GetURI()
	
;---------------------------------------------------------------------
; Search for Files ///////////////////////////////////////////////////
;---------------------------------------------------------------------

	;Local or remote search
	if self.offline $
		then files = self -> GetLocalNames(COUNT=count) $
		else files = self -> GetFileNames(COUNT=count)
	if count eq 0 then message, 'No files found.'

;---------------------------------------------------------------------
; Filter Files ///////////////////////////////////////////////////////
;---------------------------------------------------------------------
	files = self -> FilterFiles(files, COUNT=count)
	if count eq 0 then message, 'No files remain after time and version filters.'
	
	;
	; The API does not like long URIs so requesting information for
	; each file with the "file" field does not work. We will have to
	; submit a more general request, then trim the results again. To
	; make this easier, sort the initial results.
	;
	; The file filter will mix up the order, so sort files here
	;
	files = files[ sort(self -> Path_BaseName(files)) ]

;---------------------------------------------------------------------
; Local Files ////////////////////////////////////////////////////////
;---------------------------------------------------------------------

	;Simulate what the local version would look like
	files_out   = strarr(count)
	local_files = self -> uri2dir(files)

	;Check if files exist locally
	tf_local = file_test(local_files)

	;Select local files
	iLocal = where(tf_local, nLocal, COMPLEMENT=iDownload, NCOMPLEMENT=nDownload)
	if nLocal gt 0 then begin
		;Do not download anything
		if self.no_download then begin
			files_out[iLocal] = local_files[iLocal]
		
		;Download files that are different
		endif else begin
			;Get remote and local file sizes
			;   - The API does not like long lists of file names, so we have to
			;     have to re-do the entire search.
			self -> GetFileInfo, remote_name, remote_size, /SORT
			idx  = value_locate(remote_name, file_basename(files[iLocal]))
			remote_name = remote_name[idx]
			remote_size = remote_size[idx]
			
			;Get the file size of local files.
			self -> CD, FILES=file_basename(files[iLocal])
			self -> GetLocalInfo, local_name, local_size, /SORT

			;Check if they are the same
			iSame = where(local_size eq remote_size, nSame, COMPLEMENT=iDiff, NCOMPLEMENT=nDiff)
			
			;Re-download files if they are different.
			if nDiff gt 0 then begin
				iDownload = nDownload eq 0 ? iLocal[iDiff] : [iDownload, iLocal[iDiff]]
				nDownload = nDownload + nDiff
				if self.verbose then MrPrintF, 'LogText', nDiff, FORMAT='(%"Updating %i local files.")'
			endif
			
			;Use local file if they are the same.
			if nSame gt 0 then begin
				nLocal            = nSame
				iLocal            = iLocal[iSame]
				files_out[iLocal] = local_files[iLocal]
			endif
		endelse
		
		;Local files have been transferred to FILES_OUT. Now, download files.
		;   - Remaining LOCAL_FILES are the names given to files after download.
		local_files = local_files[iDownload]
		files       = files[iDownload]
	endif

;-----------------------------------------------------
; Remote: No Download \\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\
;-----------------------------------------------------
	if self.no_download then begin
		;Toss all remote files
		files_out = files_out[iLocal]
		count     = nLocal
		
		;Information
		if self.verbose then MrPrintF, 'LogText', count-nLocal, FORMAT='(%"%i remote files not downloaded.")'
	
;-----------------------------------------------------
; Remote: Download \\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\
;-----------------------------------------------------
	endif else if nDownload gt 0 then begin
		if self.verbose then MrPrintF, 'LogText', nDownload, FORMAT='(%"Downloading %i files.")'
	
	;-----------------------------------------------------
	; Progress Bar \\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\
	;-----------------------------------------------------

		;theText = string(FORMAT='(%"File %i of %i.")', i+1, nFiles)    ;Another choice for the text string.
		progressBar = obj_new('cgProgressBar', TEXT=fileOut, TITLE='Downloading...', $
		                      /CANCEL, /START)
	
		;Create and set the callback structure
		callbackData = { bar:          progressBar, $
		                 last_update:  0.0, $
		                 last_size:    0L, $
		                 oNet:         self, $
		                 percent:      0B, $
		                 total_size:   0LL $
		               }
		
		self -> SetProperty, CALLBACK_FUNCTION='MrWebURI_Callback_Download'
	
	;-----------------------------------------------------
	; Download \\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\
	;-----------------------------------------------------
		;Strip the path
		files = self -> Path_BaseName(files)
	
		;Download remote files
		;   - Only the latest version are downloaded. If an older version
		;     is specified, no file will be returned.
		for i = 0, nDownload-1 do begin
			;Reset the progress bar and callback data
			progressBar -> Update, 0.0
			self -> SetProperty, CALLBACK_DATA=callbackData
		
			;CD to the file that we want to download
			self -> CD, FILES=files[i]
			
			;Make sure its place exists
			if ~file_test(file_dirname(local_files[i]), /DIRECTORY) $
				then file_mkdir, file_dirname(local_files[i])
			
			;Download the file.
			files_out[iDownload[i]] = self -> Download(local_files[i])
			if self.verbose then MrPrintF, 'LogText', files_out[iDownload[i]], FORMAT='(%"File downloaded to \"%s\".")'
		endfor
	
	;-----------------------------------------------------
	; Clean Up \\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\
	;-----------------------------------------------------
		;Destroy progress bar
		if nDownload gt 0 then begin
			;Destroy the progress bar
			obj_destroy, progressBar
			
			;Reset the callback function
			self -> SetProperty, CALLBACK_FUNCTION='MrWebURI_Callback'
		endif
		
		;Return to old URI
		self -> CD, oldURI
	endif
	
;-----------------------------------------------------
; Return \\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\
;-----------------------------------------------------
	
	;Return files
	return, files_out
end


;+
;   Download files in bulk. A zip file is retrieved then unzipped.
;
; :Keywords:
;       COUNT:          out, optional, type=integer
;                       Number of file names returned.
;
; :Returns:
;       FILENAMES:      Names of files that match search criteria
;-
function MrMMS_SDC_Query::GetBulk, $
UNPACK=unpack, $
COUNT=count
	compile_opt idl2

	;Catch errors
	catch, the_error
	if the_error ne 0 then begin
		catch, /CANCEL
		
		;Report error
		MrPrintF, 'LogErr'
		
		;CD to original directory
		;   - ::CD calls ::GetResponseHeader, which purposely generates an
		;     error. Report this error before calling ::CD
		if n_elements(uri) gt 0 then self -> CD, uri
		
		count = 0
		return, ''
	endif
	
	tf_unpack = keyword_set(unpack)
	
	;Set URI to the Download service
	uri = self -> GetURI()
	self -> CD, /DOWNLOAD
	
	;Get the zip file
	file = self -> Download()
	
	;Unpack
	if tf_unpack then file = self -> GetBulk_Unpack(file, COUNT=count)
	
	;Return files
	return, file
end


;+
;   Unpackage a zip file of data files that have been downloaded in bulk.
;
; :Private:
;
; :Params:
;       ZIPFILE:        in, required, type=string
;                       Name of the zip file to be unpacked.
;
; :Keywords:
;       COUNT:          out, optional, type=integer
;                       Number of file names returned.
;
; :Returns:
;       FILENAMES:      Names of files that match search criteria
;-
function MrMMS_SDC_Query::GetBulk_Unpack, zipfile
	compile_opt idl2
	on_error, 2

	;Note to self.
	if MrCmpVersion('8.2.3') le 0 $
		then message, 'FILE_ZIP routines were introduced in IDL 8.2.3. Update this program.', /INFORMATIONAL

	;
	; The zip file directory structure is:
	;
	;     mms/public/[sc]/[instr]/[mode]/[level]/[year]/[month]
	;
	; Wheras the SDC directory structure is
	;
	;     mms/public/data/[sc]/[instr]/[mode]/[level]/[year]/[month]
	;

	;Check the LOCAL_DATA_ROOT system variable
	;   - Default to creating an SDC-like directory structure
	mms_data_root = getenv('MMS_DATA_ROOT')
	if mms_data_root eq '' then begin
		;Set it equal to $HOME/MrWebData
		mms_data_root = filepath('', ROOT_DIR=getenv('HOME'), SUBDIRECTORY='MrWebData')
		if ~file_test(mms_data_root, /DIRECTORY) then file_mkdir, mms_data_root
	
		;Set environment variable
		MrPrintF, 'LogWarn', 'LOCAL_DATA_ROOT environment variable not defined.'
		MrPrintF, 'LogWarn', 'Setting it equal to "' + mms_data_root + '".'
		setenv, 'MMS_DATA_ROOT=' + mms_data_root
	endif

	;
	;Download structure
	;   self.directory/file.zip                          [zipPath/zipfile]
	;       ->  unzip
	;                 /CAA_Download_[date]_[time]               [/zipRoot]
	;                 /CAA_Download_[date]_[time]/Dataset
	;                 /CAA_Download_[date]_[time]/Dataset/datafile.cdf
	;
	;Variable names
	;   zipPath     -   self.directory
	;   zipfile     -   zipPath/filename.zip            -   CAA_Download_[date]_[time].zip
	;   zipRoot     -   CAA_Download_[date]_[time]      -   [directory]
	;   unzipDir    -   zipPath/zipRoot
	;


	;Change to the directory into which the zip file was downloaded
	;   - pwd -> self.directory [= zipPath]
	cd, CURRENT=pwd
	download_dir = file_dirname(zipfile)
	cd, download_dir

	;Unzip & Delete file
	;   - mms/public/[sc]/[instr]/[mode]/[level]/[year]/[month]/
	spawn, 'unzip -qo ' + zipfile
	file_delete, zipfile
	
	;Get all files just downloaded
	;   - Use only the relative directory
	unzip_dir = filepath('', ROOT_DIR=download_dir, SUBDIRECTORY='mms')
	filenames = file_search(unzip_dir, 'mms*.cdf', COUNT=nFiles)
	files_out = strarr(nFiles)
	
	;Move to destination
	for i = 0, nFiles - 1 do begin
		;Build the output directory
		dir_parts = strsplit(file_dirname(filenames[i]), path_sep(), /EXTRACT)
		dir_out   = filepath('', ROOT_DIR=mms_data_root, SUBDIRECTORY=dir_parts[2:*])
		if ~file_test(dir_out, /DIRECTORY) then file_mkdir, dir_out
		
		;Filename
		files_out[i] = filepath(file_basename(filenames[i]), ROOT_DIR=dir_out)
		
		;Move the file
		file_move, filenames[i], files_out[i], /OVERWRITE
	endfor
	
	;Delete the unzipped directory tree
	file_delete, unzip_dir, /RECURSIVE
	
	;Change back to the current directory
	cd, pwd
	return, files_out
end


;+
;   Get information about files.
;
; :Params:
;       FILENAMES:      out, optional, type=strarr
;                       Names of the files that match search criteria.
;       FILESIZE:       out, optional, type=ulonarr
;                       Names of the files that match search criteria.
;       MODIFIED:       out, optional, type=strarr
;                       Dates files were modified, formatted as 'YYYY-MM-DDThh:mm:ss'.
;       TIMETAG:        out, optional, type=strarr
;                       Time tag of files, formatted as 'YYYY-MM-DDThh:mm:ss'.
;
; :Keywords:
;       COUNT:          out, optional, type=integer
;                       Number of file names returned.
;       SORT:           in, optional, type=boolean, default=0
;                       If set, results will be sorted alphabetically by `FILENAMES`.
;       TT2000:         in, optional, type=boolean, default=0
;                       If set, `TIMETAG` and `MODIFIED` will be returned as tt2000 values.
;-
pro MrMMS_SDC_Query::GetFileInfo, filenames, filesize, modified, timetag, $
COUNT=nFiles, $
SORT=tf_sort, $
TT2000=tt2000
	compile_opt idl2

	;Catch errors
	catch, the_error
	if the_error ne 0 then begin
		catch, /CANCEL
		
		;Delete the tar file
		if n_elements(file) gt 0 && file_test(file, /REGULAR) $
			then file_delete, file
		
		;Delete the JSON file
		if n_elements(jsonfile) gt 0 && file_test(jsonfile, /REGULAR) $
			then file_delete, jsonfile
			
		MrPrintF, 'LogErr'
		
		;CD to original directory
		;   - ::CD calls ::GetResponseHeader, which purposely generates an
		;     error. Report this error before calling ::CD
		if n_elements(oldURI) gt 0 then self -> CD, oldURI
		
		return
	endif
	
;-----------------------------------------------------
; Download File Info \\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\
;-----------------------------------------------------
	
	;CD to the Info service
	oldURI = self -> GetURI()
	self -> CD, /FILE_INFO
	
	;Download the JSON file
	file = self -> Download()

	;Replace the ".tar.gz" extension with ".json"
	jsonfile  = stregex(file, '(.*)\.tar\.gz', /SUBEXP, /EXTRACT)
	jsonfile  = jsonfile[1]
	jsonfile += '.json'
	
	;Rename the file
	file_move, file, jsonfile
	
;-----------------------------------------------------
; Read Info from File \\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\
;-----------------------------------------------------
	
	;Read the file
	jsonstr = ''
	openr, lun, jsonfile, /GET_LUN
	readf, lun, jsonstr, FORMAT='(a)'
	free_lun, lun
	
	;Delete the file
	file_delete, jsonfile
	
;-----------------------------------------------------
; Parse Info \\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\
;-----------------------------------------------------
	
	;Parse string
	json = json_parse(jsonstr)

	;Parse further
	nFiles    = n_elements(json['files'])
	filenames = strarr(nFiles)
	timetag   = strarr(nFiles)
	modified  = strarr(nFiles)
	filesize  = ulonarr(nFiles)

	foreach value, json['files'], idx do begin
		;Get the file info
		file_hash = json['files', idx]
		
		;Extract the file info
		filenames[idx] = file_hash['file_name']
		modified[idx]  = file_hash['modified_date']
		timetag[idx]   = file_hash['timetag']
		filesize[idx]  = file_hash['file_size']
	endforeach
	
;-----------------------------------------------------
; Organize Results \\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\
;-----------------------------------------------------
	
	;Sort
	if keyword_set(tf_sort) then begin
		isort = sort(filenames)
		filenames = filenames[isort]
		modified  = modified[isort]
		timetag   = timetag[isort]
		filesize  = filesize[temporary(isort)]
	endif
	
	;Scalar
	if nFiles eq 1 then begin
		filenames = filenames[0]
		modified  = modified[0]
		timetag   = timetag[0]
		filesize  = filesize[0]
	endif
	
	;TT2000 times
	if keyword_set(tt2000) then begin
		;Convert TIMETAG to TT2000
		cdf_tt2000, timetag, fix(strmid(timetag,  0, 4)), fix(strmid(timetag,  5, 2)), fix(strmid(timetag,  8, 2)), $
		                     fix(strmid(timetag, 11, 2)), fix(strmid(timetag, 14, 2)), fix(strmid(timetag, 17, 2)), $
		                     /COMPUTE_EPOCH
		                     
		;Convert MODIFIED to TT2000
		cdf_tt2000, modified, fix(strmid(modified,  0, 4)), fix(strmid(modified,  5, 2)), fix(strmid(modified,  8, 2)), $
		                      fix(strmid(modified, 11, 2)), fix(strmid(modified, 14, 2)), fix(strmid(modified, 17, 2)), $
		                      /COMPUTE_EPOCH
	endif
	
	;Return to the original URL
	self -> CD, oldURI
end


;+
;   Get names of files
;
; :Keywords:
;       COUNT:          out, optional, type=integer
;                       Number of file names returned.
;       SORT:           in, optional, type=boolean, default=0
;                       If set, results will be sorted alphabetically.
;
; :Returns:
;       FILENAMES:      Names of files that match search criteria
;-
function MrMMS_SDC_Query::GetFileNames, $
COUNT=count, $
sort=tf_sort
	compile_opt idl2

	;Catch errors
	catch, the_error
	if the_error ne 0 then begin
		catch, /CANCEL
		
		;Delete the tar file
		if n_elements(file) gt 0 && file_test(file, /REGULAR) $
			then file_delete, file
		
		;Delete the TXT file
		if n_elements(namefile) gt 0 && file_test(namefile, /REGULAR) $
			then file_delete, namefile
		
		;Report error
		MrPrintF, 'LogErr'
		
		;CD to original directory
		;   - ::CD calls ::GetResponseHeader, which purposely generates an
		;     error. Report this error before calling ::CD
		if n_elements(uri) gt 0 then self -> CD, uri
		
		count = 0
		return, ''
	endif
	
;-----------------------------------------------------
; Download Filenames \\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\
;-----------------------------------------------------

	;CD to the File Names service
	count = 0
	uri   = self -> GetURI()
	self -> CD, /FILE_NAMES

	;Download the JSON file
	file = self -> Download()
	if file eq '' then return, file
	
	;Return to the original URI
	self -> CD, uri
	
;-----------------------------------------------------
; Parse Results from File \\\\\\\\\\\\\\\\\\\\\\\\\\\\
;-----------------------------------------------------

	;Replace the ".tar.gz" extension with ".txt"
	namefile  = stregex(file, '(.*)\.tar\.gz', /SUBEXP, /EXTRACT)
	namefile  = namefile[1]
	namefile += '.txt'
	
	;Rename the file
	file_move, file, namefile
	
	;Read the file
	names = ''
	openr, lun, namefile, /GET_LUN
	readf, lun, names, FORMAT='(a)'
	free_lun, lun
	
	;Delte the file
	file_delete, namefile
	
;-----------------------------------------------------
; Finish \\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\
;-----------------------------------------------------
	
	;Create string array
	filenames = strsplit(names, ',', /EXTRACT)
	count     = n_elements(filenames)
	
	;Sort the results alphabetically
	if keyword_set(tf_sort) then begin
		temp_base = self -> Path_BaseName(filenames)
		filenames = filenames[sort(temporary(temp_base))]
	endif
	
	;Return
	return, filenames
end


;+
;   Get names of local files.
;
; :Keywords:
;       COUNT:          out, optional, type=integer
;                       Number of file names returned.
;       FILTER:         in, optional, type=boolean, default=0
;                       If set, files will be filtered by time and version.
;       SORT:           in, optional, type=boolean, default=0
;                       If set, results will be sorted alphabetically.
;
; :Returns:
;       FILENAMES:      Names of files that match search criteria
;-
function MrMMS_SDC_Query::GetLocalNames, $
COUNT=count, $
FILTER=filter, $
SORT=tf_sort
	compile_opt idl2
	
	catch, the_error
	if the_error ne 0 then begin
		catch, /CANCEL
		if obj_valid(oFile) then obj_destroy, oFile
		MrPrintF, 'LogErr'
		count = 0
		return, ''
	endif

;-----------------------------------------------------
; Create Local File Names \\\\\\\\\\\\\\\\\\\\\\\\\\\\
;-----------------------------------------------------
	
	;Directories from field-value pairs
	;   - TSTART will be made from MrTokens
	;   - VERSION will be the asterisk
	if (*self.field_values).file[0] eq '' then begin
		uri = MrMMS_Build_Filename( (*self.field_values).sc_id, $
		                            (*self.field_values).instrument_id, $
		                            (*self.field_values).data_rate_mode, $
		                            (*self.field_values).data_level, $
		                            OPTDESC  = (*self.field_values).descriptor, $
		                            SDC_ROOT = self.local_root )
		
	;Directories from file names
	endif else begin
		ftemp    = strsplit((*self.field_values).file, ',', /EXTRACT)
		uri_root = MrMMS_Build_Path( ftemp, SDC_ROOT=self.local_root )
		uri      = uri_root + temporary(ftemp)
	endelse

;-----------------------------------------------------
; Dropbox \\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\
;-----------------------------------------------------
	if self.dropbox_root ne '' then begin
		;Directories from field-value pairs
		if (*self.field_values).file[0] eq '' then begin
			uri_dropbox = MrMMS_Build_Filename( (*self.field_values).sc_id, $
			                                    (*self.field_values).instrument_id, $
			                                    (*self.field_values).data_rate_mode, $
			                                    (*self.field_values).data_level, $
			                                    OPTDESC   = (*self.field_values).descriptor, $
			                                    DIRECTORY = self.dropbox_root )
		
		;Directories from file names
		endif else begin
			ftemp       = strsplit((*self.field_values).file, ',', /EXTRACT)
			uri_dropbox = self.dropbox_root + path_sep() + temporary(ftemp)
		endelse
		
		;Combine with local results
		uri = [uri, temporary(uri_dropbox)]
	endif

;-----------------------------------------------------
; Search for Files \\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\
;-----------------------------------------------------

	;Create a File URI object for local searches
	oFile = MrFileURI('file://' + self.local_root)

	;Step through each URI
	count = 0L
	for i = 0, n_elements(uri) - 1 do begin
		;Search for files
		temp = oFile -> Search(uri[i], COUNT=nFiles)

		;Combine them
		if nFiles gt 0 then begin
			if count eq 0 $
				then files = temporary(temp) $
				else files = [files, temporary(temp)]
		endif
		
		;Increase count
		count += nFiles
	endfor

	;Destroy the file object
	obj_destroy, oFile

;-----------------------------------------------------
; Filter Files \\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\
;-----------------------------------------------------

	;Filter by time and version
	if keyword_set(filter) && count gt 0 $
		then files = self -> FilterFiles(files, COUNT=count)
	
	;Sort alphabetically
	if keyword_set(tf_sort) && count gt 0 $
		then files = files[sort(file_basename(files))]

;-----------------------------------------------------
; Return \\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\
;-----------------------------------------------------
	if count eq 0 then files = ''
	return, files
end


;+
;   Get information about files.
;
; :Params:
;       FILENAMES:      out, optional, type=strarr
;                       Names of the files that match search criteria.
;       FILESIZE:       out, optional, type=ulonarr
;                       Names of the files that match search criteria.
;       MODIFIED:       out, optional, type=strarr
;                       Dates files were modified, formatted as 'YYYY-MM-DDThh:mm:ss'.
;       TIMETAG:        out, optional, type=strarr
;                       Time tag of files, formatted as 'YYYY-MM-DDThh:mm:ss'.
;
; :Keywords:
;       COUNT:          out, optional, type=integer
;                       Number of file names returned.
;       SORT:           in, optional, type=boolean, default=0
;                       If set, results will be sorted alphabetically by `FILENAME`.
;       TT2000:         in, optional, type=boolean, default=0
;                       If set, `TIMETAG` and `MODIFIED` will be returned as tt2000 values.
;-
pro MrMMS_SDC_Query::GetLocalInfo, filenames, filesize, modified, timetag, $
COUNT=count, $
SORT=tf_sort, $
TT2000=tt2000
	compile_opt idl2
	on_error, 2

	;Defaults
	tf_tt2000 = keyword_set(tt2000)

	;Get files
	file_uris = self -> GetLocalNames(COUNT=count, /FILTER)
	if count eq 0 then help, /tr

	;Parse the files for their paths
	self -> ParseURI, file_uris, PATH=file_path 
	
	;FILENAMES
	filenames = self -> Path_Basename(file_path)
	
	;TIMETAG
	MrMMS_Parse_Filename, filenames, TSTART=timetag
	MrMMS_Parse_Time, timetag, year, month, day, hour, minute, second, INTEGER=tf_tt2000
	if tf_tt2000 $
		then cdf_tt2000, timetag, year, month, day, hour, minute, second, /COMPUTE_TT2000 $
		else timetag = year + '-' + month + '-' + day + 'T' + hour + ':' + minute + ':' + second
	
	;FILESIZE
	finfo    = file_info(file_path)
	filesize = finfo.size
	
	;MODIFIED
	;   - SysTime() does not accept arrays
	modified = strarr(count)
	for i = 0, count-1 do begin
		;Breakdown modified time
		caldat, systime(finfo[i].mtime, /JULIAN), month, day, year, hour, minute, second
		
		;Create TT2000 time
		if tf_tt2000 then begin
			cdf_tt2000, tt2000, year, month, day, hour, minute, second, /COMPUTE_TT2000
			modified[i] = tt2000
		
		;Create ISO time
		endif else begin
			modified[i] = string(year,   FORMAT='(i04)') + '-' + $
			              string(month,  FORMAT='(i02)') + '-' + $
			              string(day,    FORMAT='(i02)') + 'T' + $
			              string(hour,   FORMAT='(i02)') + ':' + $
			              string(minute, FORMAT='(i02)') + ':' + $
			              string(second, FORMAT='(i02)')
		endelse
	endfor
	
	;SORT
	if keyword_set(tf_sort) then begin
		isort     = sort(filenames)
		filenames = filenames[isort]
		filesize  = filesize[isort]
		modified  = modified[isort]
		tiemtag   = timetag[temporary(isort)]
	endif
end


;+
;   Get local file versions.
;
; :Params:
;       MAX_VERSION:    out, optional, type=strarr
;                       Maximum version number for each MMS data file type.
;       FILE_TYPE:      out, optional, type=strarr
;                       Types of files associated with `MAX_VERSION`
;
; :Keywords:
;       COUNT:          out, optional, type=integer
;                       Number of elements returned.
;-
pro MrMMS_SDC_Query::GetLocalVersions, max_version, file_type, $
COUNT=count
	compile_opt idl2
	on_error, 2

	;Get files
	file_uris = self -> GetLocalNames(COUNT=count, /FILTER)
	
	;Parse the uris for their paths
	self -> ParseURI, file_uris, PATH=file_path 
	
	;Separate file name from its path
	filenames = self -> Path_Basename(file_path)
	
	;Parse file names
	MrMMS_Parse_Filename, filenames, $
	                      SC      = sc, $
	                      INSTR   = instr, $
	                      MODE    = mode, $
	                      LEVEL   = level, $
	                      OPTDESC = optdesc, $
	                      VERSION = max_version
	
	;Create file types
	file_type = sc + '_' + instr + '_' + mode + '_' + level
	iOptDesc = where(optdesc ne '', nOptDesc)
	if nOptDesc ne 0 then file_type[iOptDesc] += '_' + optdesc[iOptDesc]
	
	;Search for unique file types and versions
	;   - THIS SHOULD NOT BE NECESSARY AFTER THE FILTER!
;	iuniq       = uniq(file_type, sort(file_type))
;	count       = n_elements(iuniq)
;	file_type   = file_type[iuniq]
;	max_version = max_version[iuniq]
end


;+
;   Get object property.
;
; :Keywords:
;       TEND:           out, optional, type=string
;                       End time of data interval.
;       TSTART:         out, optional, type=string
;                       Start time of data interval.
;       _REF_EXTRA:     out, optional, type=any
;                       Any keyword accepted by MrWebURI::GetProperty.
;-
pro MrMMS_SDC_Query::GetProperty, $
TSTART=tstart, $
TEND=tend, $
_REF_EXTRA=extra
	compile_opt idl2
	on_error, 2
	
	;Get properties
	if arg_present(tstart) gt 0 then tstart = self.tstart
	if arg_present(tend)   gt 0 then tend   = self.tend

	;MrWebURI properties
	if n_elements(extra) gt 0 then self -> MrWebURI::GetProperty, _STRICT_EXTRA=extra
end


;+
;   Get file versions.
;
; :Params:
;       MAX_VERSION:    out, optional, type=strarr
;                       Maximum version number for each MMS data file type.
;       FILE_TYPE:      out, optional, type=strarr
;                       Types of files associated with `MAX_VERSION`
;
; :Keywords:
;       COUNT:          out, optional, type=integer
;                       Number of elements returned.
;-
pro MrMMS_SDC_Query::GetVersions, max_version, file_type, $
COUNT=nVersions
	compile_opt idl2

	;Catch errors
	catch, the_error
	if the_error ne 0 then begin
		catch, /CANCEL
		
		;Delete the tar file
		if n_elements(file) gt 0 && file_test(file, /REGULAR) $
			then file_delete, file
		
		;Delete the JSON file
		if n_elements(jsonfile) gt 0 && file_test(jsonfile, /REGULAR) $
			then file_delete, jsonfile
		
		;Print the error
		;   - ::CD will call ::GetResponseHeader which will intentionally
		;     throw an error. Must report this error first.
		MrPrintF, 'LogErr'
		
		;CD to the original URI
		if n_elements(uri) gt 0 then self -> CD, uri
		return
	endif
	
	;CD to the Versions service
	uri = self -> GetURI()
	self -> CD, /V_INFO
	
	;Download the JSON file
	file = self -> Download()

	;Replace the ".tar.gz" extension with ".json"
	jsonfile  = stregex(file, '(.*)\.tar\.gz', /SUBEXP, /EXTRACT)
	jsonfile  = jsonfile[1]
	jsonfile += '.json'
	
	;Rename the file
	file_move, file, jsonfile
	
	;Read the file
	jsonstr = ''
	openr, lun, jsonfile, /GET_LUN
	readf, lun, jsonstr, FORMAT='(a)'
	free_lun, lun
	
	;Delte the file
	file_delete, jsonfile
	
	;Parse string
	json = json_parse(jsonstr)
	
	;Parse further
	nVersions   = n_elements(json['versions'])
	file_type   = strarr(nVersions)
	max_version = strarr(nVersions)
	foreach value, json['versions'], idx do begin
		;Get the file info
		file_hash = json['versions', idx]
		
		;Extract the file info
		file_type[idx]   = file_hash['file_type']
		max_version[idx] = file_hash['max_version']
	endforeach
	
	;Return to the original URI
	self -> CD, uri
end


;+
;   Get the download link.
;
; :Private:
;
; :Returns:
;       URI:            The current URI.
;-
function MrMMS_SDC_Query::GetURI
	compile_opt idl2, hidden
	on_error, 2
	
	;Build the URI
	uri = self -> MrURI::BuildURI( SCHEME = self.scheme, $
	                               HOST   = self.host, $
	                               PATH   = self.path, $
	                               QUERY  = self.query )
	
	return, uri
end


;+
;    Print the list of files in the current directory.
;
; :Params:
;       SEARCHSTR:      in, optional, type=string, default=''
;                       A pattern to match against files in the current directory.
;                           Files and directoires matching this search string will
;                           be returned. If the search string is preceeded by a
;                           directory path, the search will take place in that
;                           directory instead of the current directory.
;
; :Keywords:
;       COUNT:          in, optional, type=integer
;                       Number of files found.
;       INFO:           in, optional, type=boolean, default=0
;                       In addition to displaying file names, show file size, date
;                           modified, and file time tag.
;       OUTPUT:         out, optional, type=strarr/struct
;                       A named variable into which the directory contents are returned.
;                           If present, the contents are not printed to the display. If
;                           `INFO` is set, a structure is returned. Otherwise a string
;                           array of of file names is returned.
;-
pro MrMMS_SDC_Query::LS, srchstr, $
COUNT=count, $
INFO=info, $
OUTPUT=output
	compile_opt idl2
	on_error, 2
	
	tf_info = keyword_set(info)

;---------------------------------------------------------------------
; File Info //////////////////////////////////////////////////////////
;---------------------------------------------------------------------
	if tf_info then begin
		;Get Info
		self -> GetFileInfo, filenames, filesize, modified, timetag, COUNT=count

		;Return the information
		if arg_present(output) then begin
			output = { filenames:     temporary(filename), $
			           file_size:     temporary(filesize), $
			           date_modified: temporary(modified), $
			           timetag:       temporary(timetag) }
		
		;Display the information
		endif else begin
			;Format the output
			size_fmt = strtrim(max(strlen(string(filesize, FORMAT='(i0)'))), 2)
			mod_fmt  = 'a-' + strtrim(max(strlen(modified)), 2)
			tag_fmt  = 'a-' + strtrim(max(strlen(timetag)), 2)
			name_fmt = 'a-' + strtrim(max(strlen(filenames)), 2)
			
			;Header
			print, 'SIZE', 'MODIFIED', 'TIMETAG', 'FILENAME', $
			       FORMAT='(a-' + size_fmt + ', 2x, ' + mod_fmt + ', 2x, ' + tag_fmt + ', 2x, ' + name_fmt + ')'
			
			;Info
			fmt = '(i' + size_fmt + ', 2x, ' + mod_fmt + ', 2x, ' + tag_fmt + ', 2x, ' + name_fmt + ')'
			for i = 0, count-1 do print, filesize[i], modified[i], timetag[i], filenames[i], FORMAT=fmt
		endelse

;---------------------------------------------------------------------
; File Names /////////////////////////////////////////////////////////
;---------------------------------------------------------------------
	endif else begin
		files = self -> GetFileNames(COUNT=count)

		;Print or return?
		if arg_present(output) $
			then output = temporary(files) $
			else print, transpose(files)
	endelse
	
end


;+
;   Change connection settings to point to the given URI.
;
; :Private:
;
; :Params:
;       URI:        in, required, type=string
;                   Change connection settings to this fully resolved URI.
;       SUCCESS:    out, optional, type=boolean
;                   A flag indicating whether the operaion was successful.
;-
pro MrMMS_SDC_Query::SetURI, uri, success
	compile_opt idl2
	on_error, 2

;---------------------------------------------------------------------
; Parse the URI //////////////////////////////////////////////////////
;---------------------------------------------------------------------
	self -> ParseURI, uri, $
	                  SCHEME       = scheme, $
	                  HOST         = host, $
	                  PATH         = path, $
	                  QUERY        = query, $
	                  FIELD_VALUES = field_values
	if ~stregex(scheme, 'https?', /BOOLEAN) then message, 'SCHEME must be http or https.'
	if host ne 'lasp.colorado.edu' then message, 'HOST must be lasp.colorado.edu.'
	
	;::ParseURI returns the path with a leading "/". We must remove it
	;to prevent it from compounding when IDLnetURL::SetProperty is called.
	pos = stregex(path, '^/+', LEN=len)
	if pos ne -1 then path = strmid(path, len)

;---------------------------------------------------------------------
; On-Line Mode ///////////////////////////////////////////////////////
;---------------------------------------------------------------------
	success = 1B
	if ~self.offline then begin
	;---------------------------------------------------------------------
	; Set the Requested Data String //////////////////////////////////////
	;---------------------------------------------------------------------
		self -> IDLnetURL::SetProperty, URL_SCHEME = scheme, $
		                                URL_HOST   = host, $
		                                URL_PATH   = path, $
		                                URL_QUERY  = query
	
	;---------------------------------------------------------------------
	; Check Response /////////////////////////////////////////////////////
	;---------------------------------------------------------------------
		;Get the header
		rh     = self -> GetResponseHeader()
		header = self -> ParseResponseHeader(rh)
	
		;Handle errors
		case header.code of
			;OK
			200: ;Everything is ok
			
			;Timeout
			28: begin
				success = 0B
				MrPrintF, 'LogWarn', 'Operation timed out.'
			endcase
			
			;No content
			204: begin
				success = 0B
				MrPrintF, 'LogWarn', 'No file availble for: ' + strjoin(strsplit(query, '&', /EXTRACT), ' ') + '.'
			endcase
			
			;Unauthorized
			401: begin
				MrPrintF, 'LogText', 'Log-In required.'
				self -> LogIn
			endcase
			
			;Error
			else: begin
				success = 0B
				MrPrintF, 'LogText', uri
				MrPrintF, 'LogErr', header.code, header.status, FORMAT='(%"CODE: %i; STATUS: %s")'
			endelse
		endcase
	endif
	
;-----------------------------------------------------
; Set Properties \\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\
;-----------------------------------------------------
	
	;Set properties
	if success then begin
		self.host   = host
		self.path   = path
		self.query  = query
		self.scheme = scheme
		if query ne '' then struct_assign, field_values, *self.field_values
;		if n_elements(fragment) gt 0 then self.fragment  = fragment
;		if n_elements(username) gt 0 then self.username  = username
;		if n_elements(password) gt 0 then self.password  = password
;		if n_elements(port)     gt 0 then self.port      = port

	;Reset properties
	endif else begin
		self -> IDLnetURL::SetProperty, URL_HOSTNAME = self.host, $
		                                URL_PATH     = self.path, $
		                                URL_QUERY    = self.query, $
;		                                URL_USERNAME = self.username, $
;		                                URL_PORT     = self.port, $
		                                URL_SCHEME   = self.scheme
;		                                URL_PASSWORD = self.password
	endelse
end


;+
;   Convert a web URI to a directory structure.
;
; :Private:
;
; :Params:
;       URI:        in, required, type=string/strarr
;                   A URI or a file name returned by ::GetFileNames() to be converted
;                       to a directory.
;-
function MrMMS_SDC_Query::uri2dir, uri
	compile_opt strictarr
	on_error, 2

	nURI = n_elements(uri)

	;Breakdown the URLs
	self -> ParseURI, uri, $
	                  SCHEME       = scheme, $
	                  HOST         = host, $
	                  PATH         = path, $
	                  QUERY        = query, $
	                  FIELD_VALUES = field_values

	;Extract the directory and file names
	;   - If a filename is given, it will not have host and scheme
	if scheme[0] eq '' then begin
		scheme   = nURI eq 1 ? self.scheme : replicate(self.scheme, nURI)
		host     = nURI eq 1 ? self.host   : replicate(self.host,   nURI)
		basename = self -> Path_BaseName(uri)
		dirname  = self -> Path_DirName(uri)
	
	;Offline mode
	;   - Local files are already given. Return just the path.
	endif else if scheme[0] eq 'file' then begin
		return, path
	
	;Build paths from URI path
	endif else begin
		basename = self -> Path_BaseName(path)
		dirname  = self -> Path_DirName(path)
	endelse
	
	;Remove mms/(data|public) so that all data is stored in the same location
	parts = stregex(dirname, 'mms/(data|public)/(.*)', /SUBEXP, /EXTRACT)

	;Create the directory chain
	dir = strarr(nURI)
	for i = 0, nURI-1 do begin
		dir[i] = filepath(basename[i], $
		                  ROOT_DIR     = self.local_root, $
		                  SUBDIRECTORY = parts[3,i])
	endfor
	if nURI eq 1 then dir = dir[0]
	
	return, dir
end


;+
;   Replace HTML reserved characters with their encoded strings.
;
; :Private:
;
; :Params:
;       PASSWORD:           in, required, type=string
;                           Password for the CSA website.
;
; :Returns:
;       PWD:                Password with reserved characters replaced by encoded ones.
;-
function MrMMS_SDC_Query::Check_Password, password
	compile_opt idl2

	;Catch errors
	catch, the_error
	if the_error ne 0 then begin
		catch, /cancel
		MrPrintF, 'LogErr'
		return, ''
	endif

	;Editable password
	pass = password

	;Check for reserved characters in password and replace with URL-Encoded equivalents.
	reserved = ["%","!","#","$","&","'","(",")","*","+",",","/",":",";","=","?","@","[","]"]
	encoded = ["%25","%21","%23","%24","%26","%27","%28","%29","%2A","%2B","%2C","%2F","%3A","%3B","%3D","%3F","%40","%5B","%5D"]

	;Check each reserved character individually
	for i = 0, n_elements(reserved) -1 do begin
		;Start at beginning of string
		startpos = 0
		foundpos = 0
		reserved_positions = -1
		
		;Step through each character in the password.
		;    - Stop if no more reserved characters are found
		;    - Or if we get to the end of the string.
		while foundpos ne -1 and startpos le strlen(pass)-1 do begin
			;Look for the reserved character
			foundpos = strpos(pass,reserved[i],startpos)
			
			;Record where the reserved character was found, then advance.
			if foundpos ne -1 then begin
				if startpos eq 0 then reserved_positions = foundpos else reserved_positions = [reserved_positions,foundpos]
				startpos = foundpos+1
			endif
		endwhile
		
		;If reserved characters were found
		if reserved_positions[0] ne -1 then begin
			
			;Start at the end of the password, so that as reserved characters are
			;replaced by encoded strings, the position of other reserved characters within
			;the password does not change.
			reserved_positions = reverse(reserved_positions)
			
			;Step through each reserved character
			for j = 0, n_elements(reserved_positions) -1 do begin
			
				;Found at end of the password?
				if reserved_positions[j] eq strlen(pass)-1 then begin
					pass_before = strmid(pass,0,reserved_positions[j])
					new_pass = pass_before+encoded[i]
					pass = new_pass
					
				;Found at beginning of the password?
				endif else if reserved_positions[j] eq 0 then begin
					pass_after = strmid(pass,reserved_positions[j]+1,strlen(pass)-(reserved_positions[j]+1))
					new_pass = encoded[i]+pass_after
					pass = new_pass
				
				;Found in the middle of the password.
				endif else begin
					pass_before = strmid(pass,0,reserved_positions[j])
					pass_after = strmid(pass,reserved_positions[j]+1,strlen(pass)-(reserved_positions[j]+1))
					new_pass = pass_before+encoded[i]+pass_after
					pass = new_pass
				endelse
			endfor
		endif
	endfor

	return, pass
end


;+
;   Download the data from the CSA.
;
; :Keywords:
;       SHOW:               in, optional, type=boolean, default=0
;                           If set, the URL of the query will be printed to the command
;                               window.
;
; :Returns:
;       OUT_FNAMES:         Names of the downloaded files.
;-
function MrMMS_SDC_Query::WGet, $
SHOW=show, $
ANCILLARY=ancillary, $
CANCEL=cancel, $
HK=hk, $
INFO=info, $
NAMES=names, $
NO_GUI=no_gui, $
VERSION=version
	compile_opt idl2

	;Catch errors
	catch, the_error
	if the_error ne 0 then begin
		catch, /cancel
		if file_test(zipfile) then file_delete, zipfile
		if n_elements(pwd) gt 0 then cd, pwd
		MrPrintF, 'LogErr'
		return, ''
	endif

	;Show the command?
	cancel = 0B
	show   = keyword_set(show)
	tf_gui = ~keyword_set(no_gui)
	
;---------------------------------------------------------------------
;Was the GUI Used? ///////////////////////////////////////////////////
;---------------------------------------------------------------------
	cd, CURRENT=pwd
	
	;Make sure a request has been set
	self -> SetRequest, ANCILLARY = ancillary, $
	                    HK        = hk, $
	                    INFO      = info, $
	                    NAMES     = names, $
	                    VERSION   = version

	;Make sure a request has been set.
	if show then begin
		url = self -> GetRequest()
		print, url
	endif

	;Temporary output directory
	if self.directory eq '' $
		then cd, CURRENT=directory $
		else directory = self.directory
	
;---------------------------------------------------------------------
; Output File ////////////////////////////////////////////////////////
;---------------------------------------------------------------------
	caldat, systime(1, /JULIAN), month, day, year, hour, minute, second
	timestamp = string(year, month, day, hour, minute, second, $
	                   FORMAT='(%"%04i%02i%02i_%02i%02i%02i")')

	;Default filename for the zip file.
	filename = filepath('MrMMS_SDC_Query_' + timestamp + '.tar.gz', ROOT_DIR=directory)
	
;---------------------------------------------------------------------
; Download ///////////////////////////////////////////////////////////
;---------------------------------------------------------------------

	;Catch errors
	ntries = 0
	catch, the_error
	if the_error ne 0 then begin
		;Get the response code
		self -> GetProperty, RESPONSE_HEADER=header, RESPONSE_CODE=code
	
		;LogIn
		if code eq 401 && ntries lt 2 then begin
			self -> LogIn
			ntries += 1
		
		;Error - Cleanup
		endif else begin
			catch, /CANCEL
			
			;More informative errors
			case code of
				  2: MrPrintF, 'LogErr',  'Error code 2. May require IDL restart.'
				 42: begin
					cancel = 1B
					MrPrintF, 'LogText', 'Download cancelled.'
				endcase
				 56: MrPrintF, 'LogErr', 'Failure receiving network data. (Did you suspend IDL?)'
				201: MrPrintF, 'LogErr',  'LogIn Failed.'
				else: begin
					;Print the error
					MrPrintF, 'LogText', 'HTTP error code: ' + string(code, FORMAT='(i0)')
					MrPrintF, 'LogErr'
				endcase
			endcase
		
			;Destroy progress bar and delete files
			if n_elements(zipfile) gt 0 && file_test(zipfile, /REGULAR) then file_delete, zipfile
			
			;Return
			return, ''
		endelse
	endif

	;Get data.
	;   - Cookies (credentials) are stored in HEADER, which is reset by GET.
	zipfile = self -> IDLnetURL::Get(FILENAME=filename)
	
;---------------------------------------------------------------------
; Clean Up ///////////////////////////////////////////////////////////
;---------------------------------------------------------------------
	
	;Return the file
	return, filename
end


;+
;   Cleanup after the object is destroyed. This will destroy the widget, if it exists.
;-
pro MrMMS_SDC_Query::Cleanup
	compile_opt idl2

	;Catch errors
	catch, the_error
	if the_error ne 0 then begin
		catch, /CANCEL
		MrPrintF, 'LogErr'
		return
	endif

	;Cleanup the super class
	self -> MrWebURI::Cleanup
end


;+
;   Initialization method.
;
; :Keywords:
;       RESET_PATH:     in, optional, type=boolean, default=0
;                       If set, the path will be returned to the default path. This is
;                           equivalent to setting `PUBLIC`, `DOWNLOAD`, and `SCIENCE`.
;       RESET_QUERY:    in, optional, type=boolean, default=0
;                       If set, no information from previous queries will be retained.
;                           The default is to use values from the previous query unless
;                           otherwise provided via the `SC`, `INSTR`, `MODE`, `LEVEL`,
;                           `OPTDESC`, `TSTART`, `TEND`, or `ANC_PRODUCT` keywords.
;                           This keyword takes precedence over the aforementioned
;                           keywords.
;
;       ANCILLARY:      in, optional, type=boolean, default=0
;                       If set, the request will be for anciallary data products.
;                           Cannot be used with the `HK`, `SCIENCE`, or `VERSION`.
;       HK:             in, optional, type=boolean, default=0
;                       If set, the request will be for house keeping data products.
;                           Cannot be used with the `ANCILLARY`, `SCIENCE`, or `VERSION`.
;       SCIENCE:        in, optional, type=boolean, default=0
;                       If set, the request will be for science data products.
;                           Cannot be used with the `ANCILLARY`, or `HK`. If
;                           None of the three keywords are set, the current path will
;                           be checked for one of those options. If still nothing is found
;                           then SCIENCE will be the default.
;
;       DOWNLOAD:       in, optional, type=boolean, default=0
;                       If set, the URL will request that files be downloaded. Cannot
;                           be used with `INFO`, `NAMES`, or `V_INFO`. If none of the
;                           four keywords are set, the current URI will be examined for
;                           a choice. If still nothing has been selected, DOWNLOAD will
;                           be the default.
;       FILE_INFO:      in, optional, type=boolean, default=0
;                       If set, the URL will request file information. Cannot be used
;                           with `DOWNLOAD`, `NAMES`, or `V_INFO`
;       FILE_NAMES:     in, optional, type=boolean, default=0
;                       If set, the URL will requets file names. Cannot be used with
;                           `DOWNLOAD`, `INFO`, or `V_INFO`
;       V_INFO:         in, optional, type=boolean, default=0
;                       If set, the request will be for file version information.
;                           Cannot be used with the `ANCILLARY` or `HK`, or with
;                           `DOWNLOAD`, `INFO`, or `NAMES`.
;       FILES:          in, optional, type=string/strarr, default=''
;                       Names of the files to be downloaded. If set, then `SCIENCE`, 
;                           `DOWNLOAD`, and `RESET_QUERY` will be set.
;
;       PUBLIC:         in, optional, type=boolean, default=0
;                       If set, the public site will be queried (no password required,
;                           contains data of quality L2 and above). Cannot be used
;                           with `TEAM`. If neither keywords are set, the current URL
;                           is examined. If still neither are chosen, PUBLIC is the default.
;       TEAM:           in, optional, type=string/strarr
;                       If set, the public site will be queried (password required,
;                           contains data of quality L1A and above). Cannot be used
;                           with `TEAM`.
;
;       ANC_PRODUCT:    in, optional, type=string/strarr
;                       Ancillary data product names. Should only be used with `ANCILLARY`.
;       INSTR:          in, optional, type=string/strarr
;                       Instrument ID.
;       LEVEL:          in, optional, type=string/strarr
;                       Data quality level.
;       MODE:           in, optional, type=string/strarr
;                       Data rate mode.
;       OPTDESC:        in, optional, type=string/strarr
;                       Optional file name descriptor.
;       SC_ID:          in, optional, type=string/strarr
;                       Spacecraft ID. Options are: {'mms1' | 'mms2' | 'mms3' | 'mms4'}
;       TEND:           in, optional, type=string
;                       End time of data interval. Must be formatted as YYYY-MM-DD or
;                           YYYY-MM-DDThh:mm:ss
;       TSTART:         in, optional, type=string/strarr
;                       Start time of data interval. Must be formatted as YYYY-MM-DD or
;                           YYYY-MM-DDThh:mm:ss
;       VERSION:        in, optional, type=string/strarr
;                       File name version number, formatted as X.Y.Z, where X, Y, and Z
;                           are integers.
;
;       _REF_EXTRA:     in, optional, type=any
;                       Any keyword accepted by MrWebURI::Init.
;-
function MrMMS_SDC_Query::init, uri, $
ANC_PRODUCT=ancprod, $
ANCILLARY=ancillary, $
DOWNLOAD=download, $
HK=hk, $
FILES=files, $
FILE_INFO=info, $
FILE_NAMES=names, $
INSTR=instr, $
LEVEL=level, $
LOCAL_ROOT=local_root, $
MODE=mode, $
OPTDESC=optdesc, $
PUBLIC_SITE=public, $
RESET_PATH=reset_path, $
RESET_QUERY=reset_query, $
SCIENCE=science, $
SC_ID=sc_id, $
TEAM_SITE=team, $
TEND=tend, $
TSTART=tstart, $
V_INFO=v_info, $
VERSION=version, $
_REF_EXTRA = extra
	compile_opt idl2

	;Catch errors
	catch, the_error
	if the_error ne 0 then begin
		catch, /CANCEL
		MrPrintF, 'LogErr'
		return, 0
	endif
	
	;Set the local data root
	if n_elements(local_root) eq 0 then begin
		local_root = filepath('', ROOT_DIR=file_search('~', /TEST_DIRECTORY, /EXPAND_TILDE), $
		                          SUBDIRECTORY=['MrWebData', 'https', 'lasp.colorado.edu', 'mms'] )
		MrPrintF, 'LogWarn', 'Setting local data root to: "' + local_root + '".'
	endif

	;Superclass INIT.
	success = self -> MrWebURI::Init(LOCAL_ROOT=local_root, _STRICT_EXTRA=extra)
	if success eq 0 then return, 0
	
	;Allowed query values
	*self.field_values = { sc_id:          '', $
	                       instrument_id:  '', $
	                       data_rate_mode: '', $
	                       data_level:     '', $
	                       descriptor:     '', $
	                       version:        '', $
	                       product:        '', $
	                       start_date:     '', $
	                       end_date:       '', $
	                       file:           '' }
	
	;Create the SDC GUI
	self -> CD, uri, $
	            ANC_PRODUCT = ancprod, $
	            ANCILLARY   = ancillary, $
	            DOWNLOAD    = download, $
	            HK          = hk, $
	            FILES       = files, $
	            FILE_INFO   = info, $
	            FILE_NAMES  = names, $
	            INSTR       = instr, $
	            LEVEL       = level, $
	            MODE        = mode, $
	            OPTDESC     = optdesc, $
	            PUBLIC_SITE = public, $
	            RESET_PATH  = reset_path, $
	            RESET_QUERY = reset_query, $
	            SCIENCE     = science, $
	            SC_ID       = sc_id, $
	            TEAM_SITE   = team, $
	            TEND        = tend, $
	            TSTART      = tstart, $
	            V_INFO      = v_info, $
	            VERSION     = version

	return, 1
end


;+
;   Object definition.
;
; :Params:
;       CLASS:          out, optional, type=structure
;                       The class definition structure.
;
; :Fields:
;       TEND:               End time of data interval to be queried.
;       TSTART:             Start time of data interval to be queried.
;-
pro MrMMS_SDC_Query__define, class

	class = { MrMMS_SDC_Query, $
	          Inherits MrWebURI, $
	          tstart:  '', $
	          tend:    '' $
	        }
end