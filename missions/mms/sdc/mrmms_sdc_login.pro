; docformat = 'rst'
;
; NAME:
;       MrMMS_SDC_LogIn
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
;   Log-In to the Science Data Center's MMS team site. The home page for the public
;   site is:
;
;       https://lasp.colorado.edu/mms/sdc/public/
;
; :Categories:
;       MMS
;
; :Params:
;       USERNAME:       in, optional, type=string
;                       Log-in username. If first log-in try is unsuccessful, a dialog
;                           box will appear asking for the username.
;       PASSWORD:       in, optional, type=string
;                       Log-in password. If first log-in try is unsuccessful, a dialog
;                           box will appear asking for the password.
;
; :Keywords:
;       CANCEL:         out, optional, type=boolean
;                       Returns true (1) if the user cancelled the log-in attempt, and
;                           false (0) otherwise.
;       GROUP_LEADER:   out, optional, type=long
;                       Widget ID of the parent widget.
;       HEADER:         out, optional, type=string
;                       The response header of the final log-in attempt.
;       MAXTRIES:       in, optional, type=integer, default=3
;                       Maximum number of log-in attempts before quitting.
;       NETURL:         in, optional, type=IDLnetURL objref
;                       An IDLnetURL object reference to be used for the log-in attempt.
;                           If provided, it is returned as the output.
;
; :Returns:
;       ONETURL:        out, required, type=IDLnetURL objref
;                       An IDLnetURL object with a valid connection to the team side
;                           of the MMS Science Data Center.
;
; :See Also:
;   MrWeb_Get_Response_Header.pro
;   MrWeb_Callback_Response_Header.pro
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
;       2017/02/02  -   Written by Matthew Argall
;-
FUNCTION MrMMS_SDC_LogIn, username, password, $
CANCEL=cancel, $
GROUP_LEADER=group_leader, $
HEADER=header, $
MAXTRIES=maxTries, $
NETURL=neturl
	Compile_Opt idl2

	;Catch errors
	Catch, the_error
	IF the_error NE 0 THEN BEGIN
		Catch, /CANCEL
		IF ~Arg_Present(neturl) && Obj_Valid(neturl) THEN Obj_Destoy, neturl
		MrPrintF, 'LogErr'
		RETURN, Obj_New()
	ENDIF
	
	;Defaults
	scheme  = 'https'
	host    = 'lasp.colorado.edu'
	path    = 'mms/sdc/sitl/latis/dap/properties.txt'     ;Some sort of database. 
	query   = 'version'                                   ;SITL uses these two to validate connections
	IF N_Elements(maxTries) EQ 0 THEN maxTries = 3
	IF Obj_Valid(neturl) THEN BEGIN
		IF ~Obj_IsA(neturl, 'IDLnetURL') THEN Message, 'NETURL must be an IDLnetURL object.'
		oNetURl = neturl
	ENDIF ELSE BEGIN
		oNetURL = Obj_New('IDLnetURL')
	ENDELSE
	
	;Create a network connection
	oNetURL -> SetProperty, URL_SCHEME   = scheme, $
	                        URL_HOST     = host, $
	                        URL_PATH     = path, $
	                        URL_USERNAME = username, $
	                        URL_PASSWORD = password, $
	                        URL_QUERY    = query
	
	;
	; The callback routine aborts, so the response code property
	; will not be the response code of the header. Parse the code
	; from the header
	;
	
	;Keep trying until successful or until the user gives up.
	;   - '401 Authorization Requied'
	;   - '401 Unauthorized'
	cancel   = 0
	nTries   = 0
	tf_retry = 1

	;Log-In
	WHILE ~cancel && tf_retry && nTries LE maxTries DO BEGIN
		;Login
		oLogin = Obj_New('MrLogin', GROUP_LEADER=group_leader)
		oLogin -> GetProperty, USERNAME=username, PASSWORD=password, CANCEL=cancel
		Obj_Destroy, oLogin

		;Check what happened
		IF ~cancel THEN BEGIN
			;Input log-in credentials and check IF they were accepted
			oNetURL -> SetProperty, URL_USERNAME=username, URL_PASSWORD=password
			header   = MrWeb_Get_Response_Header(NETURL=oNetURL)
			
			;
			; For some reason, when a new IDL session starts, the first log-in
			; attempt always fails because the header has not been updated from
			; "unauthorized" to "OK". One must get the response header a second
			; time.
			;
			IF nTries EQ 0 THEN header = MrWeb_Get_Response_Header(NETURL=oNetURL)
			
			;Parse the response header
			tf_retry = StRegEx(header, '401 (Un)?Author', /FOLD_CASE, /BOOLEAN)

			;Indicate error
			IF tf_retry THEN BEGIN
				IF nTries LT maxTries-1 $
					THEN MrPrintF, 'LogWarn', 'Incorrect username or password. Try again.' $
					ELSE Message, 'Incorrect username or password.'
			ENDIF
		ENDIF
		
		nTries += 1
	ENDWHILE
	
	RETURN, oNetURL
END

