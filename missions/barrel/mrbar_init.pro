; docformat = 'rst'
;
; NAME:
;       MrBar_Init
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
;   Establish universal settings for configuring BARREL data with the MrVariable interface.
;
; :Categories:
;       MrVariable, BARREL
;
; :Keywords:
;       DROPBOX_ROOT:   in, optional, type=string, default=MRWEBDATA_DROPBOX_ROOT environment variable
;                       Local folder in which files are temporarily placed before
;                           being moved to into `LOCAL_ROOT`.
;       LOCAL_ROOT:     in, optional, type=string, default=MRWEBDATA_LOCAL_ROOT environment variable
;                       Local directory root where downloaded files are to be saved.
;                           The underlying directory structure should mimic the remote
;                           site from which data was obtained.
;       MIRROR_ROOT:    out, optional, type=string, default=''
;                       Root data directory of a remote mirror site.
;       NO_DOWNLOAD:    in, optional, type=boolean, default=0
;                       If set, use only those files that are saved locally. Files are
;                           still searched for on the remote server. Remote files are
;                           normally downloaded if they are more recent and/or have a
;                           higher version number than local files.
;       OFFLINE:        in, optional, type=boolean, default=0
;                       If set, the object will function in offline mode (search for
;                           files locally). Automatically sets `NO_DOWNLOAD` to the same value.
;       REMOTE_ROOT:    in, optional, type=string, default=<public sdc>
;                       Root data directory of a remote server.
;       VERBOSE:        in, optional, type=boolean, default=0
;                       If set, status messages will be printed to standard output.
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
;       2017/01/21  -   Written by Matthew Argall
;-
pro MrBar_Init, $
DROPBOX_ROOT=dropbox_root, $
LOCAL_ROOT=local_root, $
MIRROR_ROOT=mirror_root, $
NO_DOWNLOAD=no_download, $
OFFLINE=offline, $
VERBOSE=verbose
	compile_opt idl2
	on_error, 2
	
	;Useful links
	sdc_public = 'https://lasp.colorado.edu/mms/sdc/public/data/'
	sdc_team   = 'https://lasp.colorado.edu/mms/sdc/team/about/browse/'
	unh_mirror = 'http://mmsdata.sr.unh.edu/'
	
	;Check if the system variable exists
	defsysv, '!MrBar', EXISTS=tf_exist
	
	;Create the system variable
	if ~tf_exist then begin
		;Create the web object
		oBar = MrBar_Get_Data( DROPBOX_ROOT = dropbox_root, $
		                       LOCAL_ROOT   = local_root, $
		                       MIRROR_ROOT  = mirror_root, $
		                       NO_DOWNLOAD  = no_download, $
		                       OFFLINE      = offline, $
		                       VERBOSE      = verbose )
		
		;Create the system variable
		;   - It is read-only, so it must be a valid object.
		;   - To change settings, the ::SetProperty method can be used.
		if obj_valid(oBar) then defsysv, '!MrBar', oBar, 1
	
	;Set object properties
	endif else begin
		!MrBar -> SetProperty, LOCAL_ROOT  = local_root, $
		                       MIRROR_ROOT = mirror_root, $
		                       NO_DOWNLOAD = no_download, $
		                       OFFLINE     = offline, $
		                       VERBOSE     = verbose
	endelse
end
