; docformat = 'rst'
;
; NAME:
;       MrBar_Get_Data__Define
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
;+
;   Find and download BARREL data.
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
;       2017-01-21  -   Written by Matthew Argall
;-
;*****************************************************************************************
;+
;   Provide information when IDL's Print procedure is used.
;-
FUNCTION MrBar_Get_Data::_OverloadPrint
	Compile_Opt idl2
	On_Error, 2
	
	;Get properties of oWebURI
	uri  = self.oWebURI -> GetURI()
	self -> GetProperty, DATE_START  = date_start, $
	                     DATE_END    = date_end, $
	                     LOCAL_ROOT  = local_root, $
	                     MIRROR_ROOT = mirror_root, $
	                     NO_DOWNLOAD = no_download, $
	                     OFFLINE     = offline

	;Convert pointers to strings
	campaign = N_Elements(*self.campaign) EQ 1 ? *self.campaign : '[' + StrJoin(*self.campaign, ',') + ']'
	level    = N_Elements(*self.level)    EQ 1 ? *self.level    : '[' + StrJoin(*self.level,    ',') + ']'
	order    = N_Elements(*self.order)    EQ 1 ? *self.order    : '[' + StrJoin(*self.order,    ',') + ']'
	type     = N_Elements(*self.type)     EQ 1 ? *self.type     : '[' + StrJoin(*self.type,     ',') + ']'
	
	;Create strings
	uri      = String('  URI',         '=', uri,          FORMAT='(a-26, a-2, a0)')
	local    = String('  Local_Root',  '=', local_root,   FORMAT='(a-26, a-2, a0)')
	mirror   = String('  Mirror_Root', '=', mirror_root,  FORMAT='(a-26, a-2, a0)')
	offline  = String('  Offline',     '=', offline,      FORMAT='(a-26, a-2, a0)')
	no_dl    = String('  No_Download', '=', no_download,  FORMAT='(a-26, a-2, a0)')
	campaign = String('  Campaign',    '=', campaign,     FORMAT='(a-26, a-2, a0)')
	order    = String('  Order',       '=', order,        FORMAT='(a-26, a-2, a0)')
	type     = String('  Type',        '=', type,         FORMAT='(a-26, a-2, a0)')
	level    = String('  Level',       '=', level,        FORMAT='(a-26, a-2, a0)')
	version  = String('  Version',     '=', self.version, FORMAT='(a-26, a-2, a0)')
	tstart   = String('  Date_Start',  '=', date_start,   FORMAT='(a-26, a-2, a0)')
	tend     = String('  Date_End',    '=', date_end,     FORMAT='(a-26, a-2, a0)')

	;Output array
	outStr = [ [ uri        ], $
	           [ local      ], $
	           [ mirror     ], $
	           [ offline    ], $
	           [ no_dl      ], $
	           [ campaign   ], $
	           [ order      ], $
	           [ type       ], $
	           [ level      ], $
	           [ version    ], $
	           [ tstart     ], $
	           [ tend       ] $
	         ]

	;Print the array
	return, outStr[0, Sort(outStr)]
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
;
; :Returns:
;       URL:            in, optional, type=string
;                       URL build from the keyword parameters.
;-
function MrBar_Get_Data::BuildURI, $
CAMPAIGN=campaign, $
LEVEL=level, $
ORDER=order, $
TYPE=type, $
VERSION=version
	compile_opt idl2
	on_error, 2

	;Default values
	IF N_Elements(campaign) EQ 0 THEN campaign = *self.campaign
	IF N_Elements(order)    EQ 0 THEN order    = *self.order
	IF N_Elements(type)     EQ 0 THEN type     = *self.type
	IF N_Elements(level)    EQ 0 THEN level    = *self.level
	IF N_Elements(version)  EQ 0 THEN version  =  self.version
	date = '%Y%M%d'
	
	;Directory and file dates
	uri = MrBar_Build_URI( campaign, order, level, type, date, version )
	
	RETURN, uri
END


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
FUNCTION MrBar_Get_Data::Get, $
COUNT=count
	Compile_Opt idl2
	On_Error, 2
	
	;Build the URI path
	uri = self -> BuildURI()

	;Allocate memory
	nAlloc = 100
	count  = 0
	files  = StrArr(nAlloc)

	;Get the files
	FOR i = 0, N_Elements(uri) - 1 DO BEGIN
		;Find files
		temp = self.oWebURI -> Get( uri[i], $
		                            COUNT = nFiles, $
		                            /CLOSEST, $
		                            /NEWEST )
		
		;Make room
		IF count + nFiles GE nAlloc THEN BEGIN
			files   = [files, StrArr(nAlloc)]
			nAlloc *= 2
		ENDIF
		
		;Store results
		files[count:count+nFiles-1] = Temporary(temp)
		count += nFiles
	ENDFOR
	
	;Trim results
	files = count EQ 1 ? files[0] : files[0:count-1]
	RETURN, files
END


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
;       TYPE:           out, optional, type=string
;                       Data product type.
;       DATE:           out, optional, type=string
;                       Date of launch.
;       VERSION:        out, optional, type=string
;                       File version number.
;-
PRO MrBar_Get_Data::GetProperty, $
CAMPAIGN=campaign, $
LEVEL=level, $
ORDER=order, $
TYPE=type, $
VERSION=version, $
_REF_EXTRA=extra
	Compile_Opt idl2
	On_Error, 2
	
	;Get properties
	IF Arg_Present(campaign) THEN campaign = *self.campaign
	IF Arg_Present(level)    THEN level    = *self.level
	IF Arg_Present(order)    THEN order    = *self.order
	IF Arg_Present(type)     THEN type     = *self.type
	IF Arg_Present(version)  THEN version  = self.version
	
	;Get a limited number of superclass properties
	IF N_Elements(extra) GT 0 THEN BEGIN
		kwrds = ['DATE_END', 'DATE_START', 'LOCAL_ROOT', 'MIRROR_ROOT', 'NO_DOWNLOAD', 'OFFLINE', 'REMOTE_ROOT']
		tf_member = MrIsMember(kwrds, extra, iMember, COMPLEMENT=iBad)
		IF ~Array_Equal(tf_member, 1) THEN BEGIN
			MrPrintF, 'LogWarn', 'Keywords not allowed: ' + $
			          (N_Elements(iBad) EQ 1 ? '"' + extra[iBad] + '".' $
			                                 : '["' + StrJoin(extra[iBad], '", "') + '"]')
		ENDIF
		
		;Get properties
		self.oWebURI -> GetProperty, _STRICT_EXTRA=extra[iMember]
	ENDIF
END


;+
;   Set object properties.
;
; :Keywords:
;       CAMPAIGN:       in, optional, type=string
;                       Campaign number.
;       DATE_START:     in, optional, type=string
;                       End time of data interval. Must be formatted as YYYY-MM-DD or
;                           YYYY-MM-DDThh:mm:ss
;       DATE_END:       in, optional, type=string/strarr
;                       Start time of data interval. Must be formatted as YYYY-MM-DD or
;                           YYYY-MM-DDThh:mm:ss
;       ORDER:          in, optional, type=string
;                       Launch order.
;       LEVEL:          in, optional, type=string
;                       Data quality level.
;       TYPE:           in, optional, type=string
;                       Data product type.
;       DATE:           in, optional, type=string
;                       Date of launch.
;       VERSION:        in, optional, type=string
;                       File version number.
;-
PRO MrBar_Get_Data::SetProperty, $
CAMPAIGN=campaign, $
DATE_END=date_end, $
DATE_START=date_start, $
LEVEL=level, $
LOCAL_ROOT=local_root, $
MIRROR_ROOT=mirror_root, $
NO_DOWNLOAD=no_download, $
OFFLINE=offline, $
ORDER=order, $
REMOTE_ROOT=remote_root, $
TYPE=type, $
VERSION=version
	Compile_Opt idl2
	On_Error, 2
	
	;Set a limited number of superclass properties
	self.oWebURI -> SetProperty, DATE_END    = date_end, $
	                             DATE_START  = date_start, $
	                             LOCAL_ROOT  = local_root, $
	                             MIRROR_ROOT = mirror_root, $
	                             NO_DOWNLOAD = no_download, $
	                             OFFLINE     = offline, $
	                             REMOTE_ROOT = remote_root
	
	;CAMPAIGN
	IF N_Elements(campaign) GT 0 THEN BEGIN
		theCampaign = campaign[0] EQ '' ? ['1', '2', '3', '4'] : campaign
		IF ~Array_Equal( StRegEx(theCampaign, '[1-4]', /BOOLEAN), 1 ) THEN Message, 'CAMPAIGN must be 1-4.'
		*self.campaign = Temporary(theCampaign)
	ENDIF
	
	;LEVEL
	IF N_Elements(level) GT 0 THEN BEGIN
		theLevel = level[0] eq '' ? ['l0', 'l2'] : level
		IF ~Array_Equal( StRegEx(theLevel, '(l0|l2)', /BOOLEAN), 1 ) THEN Message, 'LEVEL must be {"l0" | "l2"}.'
		*self.level = Temporary(theLevel)
	ENDIF
	
	;ORDER
	IF N_Elements(order) GT 0 THEN BEGIN
		theOrder = order[0] EQ '' ? ['A', 'B', 'C', 'D', 'E', 'F', 'G', 'H', 'I', 'J', $
		                             'K', 'L', 'M', 'N', 'O', 'P', 'Q', 'R', 'S', 'T', $
		                             'U', 'V', 'W', 'X', 'Y', 'Z'] $
		                          : order
		IF ~Array_Equal(StRegEx(theOrder, '[A-Z]', /BOOLEAN), 1) THEN Message, 'ORDER must be an uppercase character.'
		*self.order = Temporary(theOrder)
	ENDIF
	
	;TYPE
	IF N_Elements(type) GT 0 THEN BEGIN
		theType = type[0] EQ '' ? ['ephm', 'fspc', 'hkpg', 'magn', 'misc', 'mspc', 'rcnt', 'sspc'] : type
		IF ~Array_Equal(StRegEx(theType, '(ephm|fspc|hkpg|magn|misc|mspc|rcnt|sspc)', /BOOLEAN, /FOLD_CASE), 1) $
			THEN Message, 'Invalid value for TYPE.'
		*self.type = StrLowCase(theType)
	ENDIF
	
	;VERSION
	IF N_Elements(version) GT 0 THEN BEGIN
		IF version NE '' && ~StRegEx(version, '[0-9]{2}', /BOOLEAN) $
			THEN Message, 'VERSION must be two digits.'
		self.version = version
	ENDIF
END



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
FUNCTION MrBar_Get_Data::Search, $
COUNT=count
	Compile_Opt idl2
	On_Error, 2
	
	;Build the URI path
	uri  = self -> BuildURI()
	nURI = N_Elements(nURI)

	;Allocate memory
	nAlloc = 100
	count  = 0
	files  = StrArr(nAlloc)

	;Get the files
	FOR i = 0, nURI - 1 DO BEGIN
		IF nURI GT 5 THEN MrPrintF, 'LogText', i+1, N_Elements(uri), FORMAT='(%"Search for URI %i of %i")'
	
		;Find files
		temp = self.oWebURI -> Search( uri[i], $
		                               COUNT = nFiles )
		
		;Make room
		IF count + nFiles GE nAlloc THEN BEGIN
			files   = [files, StrArr(nAlloc)]
			nAlloc *= 2
		ENDIF
		
		;Store results
		files[count:count+nFiles-1] = Temporary(temp)
		count += nFiles
	ENDFOR
	
	;Trim results
	files = count EQ 1 ? files[0] : files[0:count-1]
	RETURN, files
END


;+
;   Cleanup after the object is destroyed.
;-
PRO MrBar_Get_Data::Cleanup
	Compile_Opt idl2
	On_Error, 2

	;Objects
	Obj_Destroy, self.oWebURI
	
	;Pointers
	Ptr_Free, self.campaign
	Ptr_Free, self.level
	Ptr_Free, self.order
	Ptr_Free, self.type
END


;+
;   Initialization method.
;
; :Keywords:
;       CAMPAIGN:       in, optional, type=string, default='4'
;                       Campaign number.
;       DATE_START:     in, optional, type=string
;                       End time of data interval. Must be formatted as YYYY-MM-DD or
;                           YYYY-MM-DDThh:mm:ss
;       DATE_END:       in, optional, type=string/strarr
;                       Start time of data interval. Must be formatted as YYYY-MM-DD or
;                           YYYY-MM-DDThh:mm:ss
;       ORDER:          in, optional, type=string, default='A'
;                       Launch order.
;       LEVEL:          in, optional, type=string, default='l2'
;                       Data quality level.
;       TYPE:           in, optional, type=string
;                       Data product type.
;       DATE:           in, optional, type=string, default='%y%M%d'
;                       Date of launch.
;       VERSION:        in, optional, type=string
;                       File version number.
;-
FUNCTION MrBar_Get_Data::init, uri, $
CAMPAIGN=campaign, $
DATE_END=date_end, $
DATE_START=date_start, $
LEVEL=level, $
LOCAL_ROOT=local_root, $
MIRROR_ROOT=mirror_root, $
NO_DOWNLOAD=no_download, $
OFFLINE=offline, $
ORDER=order, $
REMOTE_ROOT=remote_root, $
TYPE=type, $
VERSION=version
	Compile_Opt idl2

	;Catch errors
	Catch, the_error
	IF the_error NE 0 THEN BEGIN
		Catch, /CANCEL
		MrPrintF, 'LogErr'
		RETURN, 0
	ENDIF
	
	;Defaults
	IF N_Elements(campaign) EQ 0 THEN campaign = ''
	IF N_Elements(order)    EQ 0 THEN order    = ''
	IF N_Elements(level)    EQ 0 THEN level    = ''
	IF N_Elements(type)     EQ 0 THEN type     = ''
	IF N_Elements(version)  EQ 0 THEN version  = ''
	IF N_Elements(uri)      EQ 0 THEN uri      = 'http://barreldata.ucsc.edu/data_products/'
	
	;Allocate heap
	self.campaign = Ptr_New(/ALLOCATE_HEAP)
	self.order    = Ptr_New(/ALLOCATE_HEAP)
	self.level    = Ptr_New(/ALLOCATE_HEAP)
	self.type     = Ptr_New(/ALLOCATE_HEAP)
	
	;Set the local data root
	IF N_Elements(local_root) EQ 0 THEN BEGIN
		local_root = FilePath('', ROOT_DIR=File_Search('~', /TEST_DIRECTORY, /EXPAND_TILDE), $
		                          SUBDIRECTORY=['MrWebData', 'barrel'] )
		MrPrintF, 'LogWarn', 'Setting local data root to: "' + local_root + '".'
	ENDIF

	;Superclass INIT.
	self.oWebURI = MrWebURI( uri, $
	                         DATE_START  = date_start, $
	                         DATE_END    = date_end, $
	                         FPATTERN    = '%Y%M%d', $
	                         LOCAL_ROOT  = local_root, $
	                         MIRROR_ROOT = mirror_root, $
	                         NO_DOWNLOAD = no_download, $
	                         NSEGMENT    = 4, $
	                         OFFLINE     = offline, $
	                         REMOTE_ROOT = remote_root, $
	                         TPATTERN    = '%Y-%M-%dT%H:%m:%S', $
	                         VREGEX      = 'v([0-9]{2})' )
	IF ~Obj_Valid(self.oWebURI) THEN RETURN, 0
	
	;Set object properties
	;   - Do not call ::CD because ::CD and ::SetURI DO not like MrTokens
	self -> SetProperty, CAMPAIGN = campaign, $
	                     LEVEL    = level, $
	                     ORDER    = order, $
	                     TYPE     = type, $
	                     VERSION  = version

	RETURN, 1
END


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
;       TYPE:           Data product type.
;       VERSION:        File version number.
;-
PRO MrBar_Get_Data__Define, class
	Compile_Opt idl2

	class = { MrBar_Get_Data, $
	          Inherits IDL_Object, $
	          oWebURI:  Obj_New(), $
	          campaign: Ptr_New(), $
	          order:    Ptr_New(), $
	          type:     Ptr_New(), $
	          level:    Ptr_New(), $
	          version:  '' $
	        }
END