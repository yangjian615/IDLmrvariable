; docformat = 'rst'
;
; NAME:
;       MrBar_Get_Data__Define
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
;+
;   Provide a GUI, similar to Dialog_Pickfile, for downloading files from the internet.
;
; :See Also:
;   MrParseURL.pro
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
;       2016-06-25  -   Written by Matthew Argall
;-
;*****************************************************************************************
;+
;   Provide information when IDL's Print procedure is used.
;-
function MrBar_Get_Data::_OverloadPrint
	compile_opt idl2
	on_error, 2
	
	;Get info from the superclass
	uri_print = self -> MrWebURI::_OverloadPrint()
	
	;Create strings
	campaign = string('  Campaign', '=', self.campaign, FORMAT='(a-26, a-2, a0)')
	order    = string('  Order',    '=', self.order,    FORMAT='(a-26, a-2, a0)')
	type     = string('  Type',     '=', self.type,     FORMAT='(a-26, a-2, a0)')
	level    = string('  Level',    '=', self.level,    FORMAT='(a-26, a-2, a0)')
	date     = string('  Date',     '=', self.date,     FORMAT='(a-26, a-2, a0)')
	version  = string('  Version',  '=', self.version,  FORMAT='(a-26, a-2, a0)')
	tstart   = string('  TStart',   '=', self.tstart,   FORMAT='(a-26, a-2, a0)')
	tend     = string('  TEnd',     '=', self.tend,     FORMAT='(a-26, a-2, a0)')

	;Output array
	outStr = [ [ uri_print ], $
	           [ campaign  ], $
	           [ order     ], $
	           [ type      ], $
	           [ level     ], $
	           [ date      ], $
	           [ version   ], $
	           [ tstart    ], $
	           [ tend      ] $
	         ]

	;Print the array
	return, outStr
end


;+
;   Build a URI
;
; :Keywords:
;       CAMPAIGN:       in, optional, type=string
;                       Campaign number.
;       ORDER:          in, optional, type=string
;                       Launch order.
;       LEVEL:          in, optional, type=string
;                       Data quality level.
;       TEND:           in, optional, type=string
;                       End time of data interval. Must be formatted as YYYY-MM-DD or
;                           YYYY-MM-DDThh:mm:ss
;       TSTART:         in, optional, type=string/strarr
;                       Start time of data interval. Must be formatted as YYYY-MM-DD or
;                           YYYY-MM-DDThh:mm:ss
;       TYPE:           in, optional, type=string
;                       Data product type.
;       DATE:           in, optional, type=string
;                       Date of launch.
;       VERSION:        in, optional, type=string
;                       File version number.
;       _REF_EXTRA:     in, optional, type=any
;                       Any keyword accepted by MrWebURI::Init.
;
; :Returns:
;       URL:            in, optional, type=string
;                       URL build from the keyword parameters.
;-
function MrBar_Get_Data::BuildURI, $
CAMPAIGN=campaign, $
DATE=date, $
LEVEL=level, $
ORDER=order, $
TYPE=type, $
VERSION=version, $
_REF_EXTRA=extra
	compile_opt idl2
	on_error, 2
	
	;Outlet for superclass functions
	if n_elements(extra) gt 0 then begin
		uri = self -> MrURI::BuildURI(_STRICT_EXTRA=extra)
		return, uri
	endif

;---------------------------------------------------------------------
; URL Scheme & Host //////////////////////////////////////////////////
;---------------------------------------------------------------------
	;SCHEME and HOST are always the same
	uri_scheme = 'http'
	uri_host   = 'barreldata.ucsc.edu'

;---------------------------------------------------------------------
; Form the Path //////////////////////////////////////////////////////
;---------------------------------------------------------------------
	uri_dirname  = '/data_products/'
	uri_basename = ''

	;Default values
	if n_elements(campaign) eq 0 then campaign = self.campaign
	if n_elements(order)    eq 0 then order    = self.order
	if n_elements(type)     eq 0 then type     = self.type
	if n_elements(level)    eq 0 then level    = self.level
	if n_elements(date)     eq 0 then date     = self.date
	if n_elements(dirdate)  eq 0 then dirdate  = self.dirdate
	if n_elements(version)  eq 0 then version  = self.version
	if n_elements(tstart)   eq 0 then tstart   = self.tstart
	if n_elements(tend)     eq 0 then tend     = self.tend
	
	;Directory and file dates
	if MrTokens_IsMatch(date, '%Y%M%d') then begin
		ddate = strmid(date, 2)
		fdate = date
	endif else begin
		ddate = MrTokens_IsMatch(dirdate, '%y%M%d') ? dirdate : '%y%M%d'
		fdate = '%Y%M%d'
	endelse
	
	uri_dirname += 'v' + version + '/' + level + '/' + campaign + order + '/' + ddate
	uri_basename = 'bar_' + campaign + order + '_' + level + '_' + type + '_' + fdate + '_v' + version + '.cdf'
	
	uri_path = uri_dirname + '/' + uri_basename

;---------------------------------------------------------------------
; TStart & TEnd //////////////////////////////////////////////////////
;---------------------------------------------------------------------

	;The search for files can be quicker if we provide a DATE_START and DATE_END
	;   - DATE_START is inclusive, but DATE_END is not
	;   - We must make sure they are on adjacent days
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
		
		;Set the start and end dates
		self.date_start = date_start
		self.date_end   = date_end
	endif


	;TSTART
	if tstart ne '' then begin
		;2015-05-14T12:00:00
		if MrTokens_IsMatch(tstart, '%Y-%M-%dT%H:%m:%S') $
			then self.tstart = tstart $
			else message, 'TSTART must be formatted as  "%Y-%M-%dT%H:%m:%S".'
	endif

	;TEND
	if tend ne '' then begin
		;2015-05-14T12:00:00
		if MrTokens_IsMatch(tend, '%Y-%M-%dT%H:%m:%S') $
			then self.tend  = tend $
			else message, 'TEND must be formatted as  "%Y-%M-%dT%H:%m:%S".'
	endif

;---------------------------------------------------------------------
; Create the URI /////////////////////////////////////////////////////
;---------------------------------------------------------------------
	uri = self -> MrURI::BuildURI( HOST     = uri_host, $
	                               PATH     = uri_path, $
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
pro MrBar_Get_Data::CD, uri, $
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
	self -> MrURI::CD, uri, SUCCESS=success
	
	;Success status
	if ~success && ~arg_present(success) then message, 'Cannot change directories.'
end


;+
;   Cleanup after the object is destroyed.
;-
pro MrBar_Get_Data::Cleanup
	compile_opt idl2
	on_error, 2

	;Superclasses
	self -> MrWebURI::Cleanup
end


;+
;   Fill the drop list with the contents of the selected directory.
;
; :Keywords:
;       COUNT:          out, optional, type=integer
;                       Number of files that match the search criteria.
;
; :Returns:
;       FILES:          out, optional, type=integer
;                       Names of the files found.
;-
function MrBar_Get_Data::Get, $
COUNT=count
	compile_opt strictarr
	on_error, 2
	
	;Provide BARREL-specific input
	files = self -> MrWebURI::Get( COUNT     = count, $
	                               /CLOSEST, $
	                               /NEWEST, $
	                               TSTART    = self.tstart, $
	                               TEND      = self.tend, $
	                               TIMEORDER = '%Y%M%d%H%m%S', $
	                               TPATTERN  = '%Y-%M-%dT%H:%m:%S' )

	;Return results
	return, files
end


;+
;   Set object properties.
;
; :Keywords:
;       CAMPAIGN:       out, optional, type=string
;                       Campaign number.
;       ORDER:          out, optional, type=string
;                       Launch order.
;       LEVEL:          out, optional, type=string
;                       Data quality level.
;       TEND:           out, optional, type=string
;                       End time of data interval. Must be formatted as YYYY-MM-DD or
;                           YYYY-MM-DDThh:mm:ss
;       TSTART:         out, optional, type=string/strarr
;                       Start time of data interval. Must be formatted as YYYY-MM-DD or
;                           YYYY-MM-DDThh:mm:ss
;       TYPE:           out, optional, type=string
;                       Data product type.
;       DATE:           out, optional, type=string
;                       Date of launch.
;       VERSION:        out, optional, type=string
;                       File version number.
;       _REF_EXTRA:     out, optional, type=any
;                       Any keyword accepted by MrWebURI::GetProperty.
;-
pro MrBar_Get_Data::GetProperty, $
CAMPAIGN=campaign, $
DATE=date, $
LEVEL=level, $
ORDER=order, $
TEND=tend, $
TSTART=tstart, $
TYPE=type, $
VERSION=version, $
_REF_EXTRA=extra
	compile_opt idl2
	on_error, 2
	
	;Set properties
	if arg_present(campaign) then campaign = self.campaign
	if arg_present(date)     then date     = self.date
	if arg_present(level)    then level    = self.level
	if arg_present(order)    then order    = self.order
	if arg_present(type)     then type     = self.type
	if arg_present(version)  then version  = self.version
	if arg_present(tstart)   then tstart   = self.tstart
	if arg_present(tend)     then tend     = self.tend

	;Superclass properties
	if n_elements(extra) gt 0 then self -> MrWebURI::GetProperty, _STRICT_EXTRA=extra
end


;+
;   Get the download link.
;
; :Private:
;
; :Returns:
;       URI:            The current URI.
;-
function MrBar_Get_Data::GetURI
	compile_opt idl2, hidden
	on_error, 2
	
	;Build the URI
	uri = self -> BuildURI( CAMPAIGN = self.campaign, $
	                        DATE     = self.date, $
	                        LEVEL    = self.level, $
	                        ORDER    = self.order, $
	                        TYPE     = self.type, $
	                        VERSION  = self.version )
	
	return, uri
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
function MrBar_Get_Data::uri2dir, uri
	compile_opt strictarr
	on_error, 2

	nURI = n_elements(uri)

	;Breakdown the URLs
	self -> ParseURI, uri, $
	                  SCHEME       = scheme, $
	                  HOST         = host, $
	                  PATH         = path

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
	
	;Remove data_products/ so that all data is stored in the same location
	parts = stregex(dirname, 'data_products/(.*)', /SUBEXP, /EXTRACT)

	;Create the directory chain
	dir = strarr(nURI)
	for i = 0, nURI-1 do begin
		dir[i] = filepath(basename[i], $
		                  ROOT_DIR     = self.local_root, $
		                  SUBDIRECTORY = parts[1,i])
	endfor
	if nURI eq 1 then dir = dir[0]
	
	return, dir
end


;+
;   Set object properties.
;
; :Keywords:
;       CAMPAIGN:       in, optional, type=string
;                       Campaign number.
;       ORDER:          in, optional, type=string
;                       Launch order.
;       LEVEL:          in, optional, type=string
;                       Data quality level.
;       TEND:           in, optional, type=string
;                       End time of data interval. Must be formatted as YYYY-MM-DD or
;                           YYYY-MM-DDThh:mm:ss
;       TSTART:         in, optional, type=string/strarr
;                       Start time of data interval. Must be formatted as YYYY-MM-DD or
;                           YYYY-MM-DDThh:mm:ss
;       TYPE:           in, optional, type=string
;                       Data product type.
;       DATE:           in, optional, type=string
;                       Date of launch.
;       VERSION:        in, optional, type=string
;                       File version number.
;       _REF_EXTRA:     in, optional, type=any
;                       Any keyword accepted by MrWebURI::SetProperty.
;-
pro MrBar_Get_Data::SetProperty, $
CAMPAIGN=campaign, $
DATE=date, $
LEVEL=level, $
ORDER=order, $
TEND=tend, $
TSTART=tstart, $
TYPE=type, $
VERSION=version, $
_REF_EXTRA=extra
	compile_opt idl2
	on_error, 2
	
	;Set properties
	if n_elements(campaign) gt 0 then self.campaign = strupcase(campaign)
	if n_elements(date)     gt 0 then self.date     = date
	if n_elements(level)    gt 0 then self.level    = level
	if n_elements(order)    gt 0 then self.order    = order
	if n_elements(type)     gt 0 then self.type     = strlowcase(type)
	if n_elements(version)  gt 0 then self.version  = version
	if n_elements(tstart)   gt 0 then self.tstart   = tstart
	if n_elements(tend)     gt 0 then self.tend     = tend

	;Superclass properties
	if n_elements(extra) gt 0 then self -> MrWebURI::SetProperty, _STRICT_EXTRA=extra
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
pro MrBar_Get_Data::SetURI, uri, success
	compile_opt idl2
	on_error, 2
	
	;
	; ::SetURI does not like MrTokens, but ::Search loves them.
	;   - ::GetURI will call ::BuildURI, which builds a uri with MrTokens in it.
	;   - ::Search will call ::GetURI for the current uri
	;   - If an error occurs, ::Search will return to said uri, which ::SetURI does not like
	;   - Here, look for the only two possible token strings
	;
	if stregex(uri, '%(y|Y)%M%d', /BOOLEAN) then begin
		pos    = stregex(uri, '%(y|Y)%M%d')
		uriOut = strmid(uri, 0, pos[0])
	endif else begin
		uriOut = uri
	endelse

	;Superclass
	self -> MrWebURI::SetURI, uriOut, success
	if success eq 0 then return
	
	;Set properties
	if success then begin
		;Obtain properties from file name
		if stregex(self.path, '\.cdf$', /BOOLEAN) then begin
			;Extract the file name
			basename = self -> Path_Basename( self.path )
		
			;Parse the file name
			MrBar_Parse_Filename, basename, $
			                      CAMPAIGN = campaign, $
			                      DATE     = date, $
			                      LEVEL    = level, $
			                      ORDER    = order, $
			                      TYPE     = type
		
		;Obtain properties from directory
		endif else begin
			parts = stregex(self.path, 'data_products/((v[0-9]+)?/?)((l[0-3])?/?)((([0-9]+)([A-Z]+))?/?)([0-9]{6})?', /SUBEXP, /EXTRACT)
			if parts[2] ne '' then version  = parts[2]
			if parts[4] ne '' then level    = parts[4]
			if parts[7] ne '' then campaign = parts[7]
			if parts[8] ne '' then order    = parts[8]
			if parts[9] ne '' then dirdate  = parts[9]
		endelse
		
		;Set properties
		;   - The directory date and filename date must be quasi-independent
		;   - We search for DIRDATE before searching for files from DATE
		;   - If filename DATE is given, so must be DIRDATE
		;   - If DIRDATE is given, a file with DATE may not exist
		self.dirdate = n_elements(dirdate) eq 0 ? '' : dirdate
		self -> SetProperty, CAMPAIGN = campaign, $
		                     DATE     = date, $
		                     LEVEL    = level, $
		                     ORDER    = order, $
		                     TYPE     = type
	endif
end


;+
;   Initialization method.
;
; :Keywords:
;       CAMPAIGN:       in, optional, type=string, default='4'
;                       Campaign number.
;       ORDER:          in, optional, type=string, default='A'
;                       Launch order.
;       LEVEL:          in, optional, type=string, default='l2'
;                       Data quality level.
;       TEND:           in, optional, type=string
;                       End time of data interval. Must be formatted as YYYY-MM-DD or
;                           YYYY-MM-DDThh:mm:ss
;       TSTART:         in, optional, type=string/strarr
;                       Start time of data interval. Must be formatted as YYYY-MM-DD or
;                           YYYY-MM-DDThh:mm:ss
;       TYPE:           in, optional, type=string
;                       Data product type.
;       DATE:           in, optional, type=string, default='%y%M%d'
;                       Date of launch.
;       VERSION:        in, optional, type=string
;                       File version number.
;       _REF_EXTRA:     in, optional, type=any
;                       Any keyword accepted by MrWebURI::Init.
;-
function MrBar_Get_Data::init, uri, $
CAMPAIGN=campaign, $
DATE=date, $
LEVEL=level, $
LOCAL_ROOT=local_root, $
ORDER=order, $
TEND=tend, $
TSTART=tstart, $
TYPE=type, $
VERSION=version, $
_REF_EXTRA=extra
	compile_opt idl2

	;Catch errors
	catch, the_error
	if the_error ne 0 then begin
		catch, /CANCEL
		MrPrintF, 'LogErr'
		return, 0
	endif
	
	;Defaults
	if n_elements(campaign) eq 0 then campaign = '4'
	if n_elements(order)    eq 0 then order    = 'A'
	if n_elements(level)    eq 0 then level    = 'l2'
	if n_elements(version)  eq 0 then version  = '06'
	if n_elements(date)     eq 0 then date     = '%Y%M%d'
	if n_elements(uri)      eq 0 then uri      = 'http://barreldata.ucsc.edu/data_products/'
	
	;Set the local data root
	if n_elements(local_root) eq 0 then begin
		local_root = filepath('', ROOT_DIR=file_search('~', /TEST_DIRECTORY, /EXPAND_TILDE), $
		                          SUBDIRECTORY=['MrWebData', 'barrel'] )
		MrPrintF, 'LogWarn', 'Setting local data root to: "' + local_root + '".'
	endif

	;Superclass INIT.
	success = self -> MrWebURI::Init(LOCAL_ROOT=local_root, _STRICT_EXTRA=extra)
	if success eq 0 then return, 0
	
	;Set object properties
	;   - Do not call ::CD because ::CD and ::SetURI do not like MrTokens
	self -> SetProperty, CAMPAIGN = campaign, $
	                     DATE     = date, $
	                     LEVEL    = level, $
	                     ORDER    = order, $
	                     TEND     = tend, $
	                     TSTART   = tstart, $
	                     TYPE     = type, $
	                     VERSION  = version

	;Change directories to wherever indicated
	self -> CD, uri

	return, 1
end


;+
;   The class definition statement.
;
; :Params:
;       CLASS:              out, optional, type=structure
;                           Class definition structure.
;
; :Fields:
;       CAMPAIGN:       Campaign number.
;       ORDER:          Launch order.
;       LEVEL:          Data quality level.
;       TEND:           End time of data interval.
;       TSTART:         Start time of data interval.
;       TYPE:           Data product type.
;       DATE:           Date of launch.
;       VERSION:        File version number.
;-
pro MrBar_Get_Data__Define, class
	compile_opt idl2

	class = { MrBar_Get_Data, $
	          inherits MrWebURI, $
	          campaign: '', $
	          order:    '', $
	          type:     '', $
	          level:    '', $
	          date:     '', $
	          dirdate:  '', $
	          version:  '', $
	          tstart:   '', $
	          tend:     '' $
	        }
end