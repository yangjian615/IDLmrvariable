; docformat = 'rst'
;
; NAME:
;       MrMMS_SDC_Build_URI
;
;*****************************************************************************************
;   Copyright (c) 2017, University of New Hampshire                                      ;
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
;   Build an MMS URI that follows the Science Data Center API conventions.
;
;   Links:
;       https://lasp.colorado.edu/mms/sdc/public/about/how-to/
;
; :Categories:
;       MMS
;
; :Keywords:
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
;                       Ancillary data product names. Setting this keyword will
;                           automatically set `ANCILLARY`.
;       FILES:          in, optional, type=string/strarr, default=''
;                       Names of the files to be downloaded. If set, then `RESET_QUERY`, 
;                           will be set.
;       INSTR:          in, optional, type=string/strarr
;                       Instrument ID. If INSTR='hk', the `HK` keyword will be auto-set.
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
;       2017/01/31  -   Written by Matthew Argall
;-
FUNCTION MrMMS_SDC_Build_URI, $
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

	;Type OF information to get
	tf_reset_path  = Keyword_Set(reset_path)
	tf_reset_query = Keyword_Set(reset_query)
	
	tf_science     = Keyword_Set(science)
	tf_anc         = Keyword_Set(ancillary)
	tf_hk          = Keyword_Set(hk)
	
	tf_info        = Keyword_Set(info)
	tf_names       = Keyword_Set(names)
	tf_v_info      = Keyword_Set(v_info)
	tf_download    = Keyword_Set(download)
	
	tf_team        = Keyword_Set(team)
	tf_public      = Keyword_Set(public)
	
	;Default values
	IF N_Elements(sc_id)   EQ 0 THEN sc_id   = ''
	IF N_Elements(instr)   EQ 0 THEN instr   = ''
	IF N_Elements(mode)    EQ 0 THEN mode    = ''
	IF N_Elements(level)   EQ 0 THEN level   = ''
	IF N_Elements(optdesc) EQ 0 THEN optdesc = ''
	IF N_Elements(version) EQ 0 THEN version = ''
	IF N_Elements(ancprod) EQ 0 THEN ancprod = ''
	IF N_Elements(files)   EQ 0 THEN files   = ''
	IF N_Elements(tstart)  EQ 0 THEN tstart  = ''
	IF N_Elements(tend)    EQ 0 THEN tend    = ''
	
	;If FILES is provided, other query parameters are ignored.
	IF N_Elements(files) GT 0 THEN BEGIN
		IF ~Array_Equal( [sc_id, instr, mode, level, optdesc, version, ancprod, tstart, tend], '' ) $
			THEN Message, 'The FILES keyword cannot be used with any other keyword.'
	ENDIF
	
;---------------------------------------------------------------------
; Defaults & Restrictions ////////////////////////////////////////////
;---------------------------------------------------------------------
	
	;Set ANCILLARY and HK keywords automatically
	IF ancprod[0] NE ''   THEN tf_anc = 1B
	IF instr[0]   EQ 'hk' THEN tf_hk  = 1B
	
	;Restrictions
	IF tf_v_info && (tf_anc || tf_hk) THEN Message, 'V_INFO cannot be used with ANCILLARY or HK.'
	IF tf_anc && tf_hk                THEN Message, 'ANCILLARY and HK are mutually exclusive.'
	IF tf_team && tf_public           THEN Message, 'TEAM_SITE and PUBLIC_SITE are mutually exclusive.'
	IF tf_names + tf_info + tf_v_info + tf_download GT 1 $
		THEN Message, 'Only one of FILE_NAMES, FILE_INFO, V_INFO, and DOWNLOAD may be selected.'

;---------------------------------------------------------------------
; Team or Public /////////////////////////////////////////////////////
;---------------------------------------------------------------------
	;Team site
	CASE 1 OF
		tf_team:   site = 'sitl'
		tf_public: site = 'public'
		ELSE:      site = 'public'
	ENDCASE

;---------------------------------------------------------------------
; Names | Info | Version | Download //////////////////////////////////
;---------------------------------------------------------------------
	CASE 1 OF
		tf_names:    info_type = 'file_names'
		tf_info:     info_type = 'file_info'
		tf_v_info:   info_type = 'version_info'
		tf_download: info_type = 'download'
		ELSE:        info_type = 'download'
	ENDCASE

;---------------------------------------------------------------------
; Ancillary | HK | Science ///////////////////////////////////////////
;---------------------------------------------------------------------
	
	;Type OF data
	CASE 1 OF
		tf_anc:     data_type = 'ancillary'
		tf_hk:      data_type = 'hk'
		tf_science: data_type = 'science'
		ELSE:       data_type = 'science'
	ENDCASE

;---------------------------------------------------------------------
; Query String ///////////////////////////////////////////////////////
;---------------------------------------------------------------------
	
	;Query string
	;   - Does not like empty fields
	uri_query = ''
	IF sc_id[0]   NE '' THEN uri_query += ( N_Elements(sc_id)   EQ 1 ? 'sc_id='          + sc_id   : 'sc_id='          + strjoin(sc_id,   ',') ) + '&'
	IF instr[0]   NE '' THEN uri_query += ( N_Elements(instr)   EQ 1 ? 'instrument_id='  + instr   : 'instrument_id='  + strjoin(instr,   ',') ) + '&'
	IF mode[0]    NE '' THEN uri_query += ( N_Elements(mode)    EQ 1 ? 'data_rate_mode=' + mode    : 'data_rate_mode=' + strjoin(mode,    ',') ) + '&'
	IF level[0]   NE '' THEN uri_query += ( N_Elements(level)   EQ 1 ? 'data_level='     + level   : 'data_level='     + strjoin(level,   ',') ) + '&'
	IF optdesc[0] NE '' THEN uri_query += ( N_Elements(optdesc) EQ 1 ? 'descriptor='     + optdesc : 'descriptor='     + strjoin(optdesc, ',') ) + '&'
	IF version[0] NE '' THEN uri_query += ( N_Elements(version) EQ 1 ? 'version='        + version : 'version='        + strjoin(version, ',') ) + '&'
	IF ancprod[0] NE '' THEN uri_query += ( N_Elements(ancprod) EQ 1 ? 'product='        + ancprod : 'ancprod='        + strjoin(ancprod, ',') ) + '&'
	IF files[0]   NE '' THEN uri_query += ( N_Elements(files)   EQ 1 ? 'file='           + files   : 'file='           + strjoin(files,   ',') ) + '&'
	
	;The query requires two dates from two different days
	;   - If we want data from a single day, then some fudging needs to be done
	;   - Also, IF TSTART=2015-288 and TEND=2015-289, then the ancillary file
	;     returned will be 2015-288 to 2015-289. This file, however, contains
	;     only 2 minutes of data from 288. There is another file from 2015-287
	;     to 2015-288. In order to find it, TSTART must be 2015-287. Thus, here,
	;     we bump TSTART and TEND by one day for ancillary files
	date_start = tstart
	date_end   = tend
	IF date_start NE '' && date_end NE '' THEN BEGIN
		;Extract the date part
		date_start = StrMid(tstart, 0, 10)
		date_end   = StrMid(tend,   0, 10)
		time_end   = StrMid(tend,  11,  8)
		IF time_end EQ '' THEN time_end = '00:00:00'

		;Bump down DATE_START IF:
		;   - ANCILLARY data is being downloaded
		IF tf_anc THEN BEGIN
			;Parse the date
			month = Fix(StrMid(date_start, 5, 2))
			day   = Fix(StrMid(date_start, 8, 2))
			year  = Fix(StrMid(date_start, 0, 4))
			
			;Bump the END date up by one day
			CDF_TT2000, tt_end, year, month, day-1, /COMPUTE_EPOCH
			date_start = StrMid(CDF_Encode_TT2000(tt_end), 0, 10)
		ENDIF

		;Bump up DATE_END IF:
		;   - DATE_START = DATE_END
		;   - ANCILLARY data is being downloaded
		IF date_start EQ date_end || time_end NE '00:00:00' || tf_anc THEN BEGIN
			;Parse the date
			month = Fix(StrMid(date_end, 5, 2))
			day   = Fix(StrMid(date_end, 8, 2))
			year  = Fix(StrMid(date_end, 0, 4))
			
			;Bump the end date up by one day
			CDF_TT2000, tt_end, year, month, day+1, /COMPUTE_EPOCH
			date_end = StrMid(CDF_Encode_TT2000(tt_end), 0, 10)
		ENDIF
	ENDIF

	;TSTART
	IF tstart NE '' THEN BEGIN
		;2015-05-14T12:00:00
		IF MrTokens_IsMatch(tstart, '%Y-%M-%dT%H:%m:%S') THEN BEGIN
			uri_query  += 'start_date=' + date_start + '&'
			self.tstart = tstart
		;2015-05-14
		ENDIF ELSE IF MrTokens_IsMatch(tstart, '%Y-%M-%d') THEN BEGIN
			uri_query  += 'start_date=' + StrMid(date_start, 0, 10) + '&'
			self.tstart = tstart + 'T00:00:00'
		;Unrecognized format
		ENDIF ELSE BEGIN
			Message, 'Invalid format for TSTART: "' + tstart + '".'
		ENDELSE
	ENDIF

	;TEND
	IF tend NE '' THEN BEGIN
		;2015-05-14T12:00:00
		IF MrTokens_IsMatch(tend, '%Y-%M-%dT%H:%m:%S') THEN BEGIN
			uri_query += 'end_date=' + StrMid(date_end, 0, 10) + '&'
		;2015-05-14
		ENDIF ELSE IF MrTokens_IsMatch(tend, '%Y-%M-%d') THEN BEGIN
			uri_query += 'end_date=' + date_end + '&'
		;Unrecognized format
		ENDIF ELSE BEGIN
			Message, 'Invalid format for TEND: "' + tend + '".'
		ENDELSE
	ENDIF

	;Trim the trailing "&"
	uri_query = StrMid(uri_query, 0, strlen(uri_query)-1)

;---------------------------------------------------------------------
; Create the URI /////////////////////////////////////////////////////
;---------------------------------------------------------------------
	uri_scheme = 'https'
	uri_host   = 'lasp.colorado.edu'
	uri_path   = 'mms/sdc/' + site + '/files/api/v1/' + info_type + '/' + data_type
	uri        = uri_scheme + '://' + uri_host + '/' + uri_path + '?' + uri_query
	
	return, uri
END
