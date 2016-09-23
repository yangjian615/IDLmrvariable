; docformat = 'rst'
;
; NAME:
;   MrVar_Config__Define
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
; PURPOSE
;+
;   Configuration object for MrVariable settings.
;
; :Categories:
;   MrVariable, Graphics
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
;       2016/05/29  -   Written by Matthew Argall
;-
;*****************************************************************************************
;+
;   The initialization method.
;
; :Params:
;       DOWNLOAD_FLAG:      in, optional, type=byte, default=1B
;                           Indicates how data should be downloaded. Optinos are::
;                               0 - Do not download. Check local directory.
;                               1 - Search for files on web. If they exist and are
;                                   not found locally, then download them.
;       REMOTE_DATA_ROOT:   in, optional, type=string, default='https://lasp.colorado.edu/mms/sdc/public/data/'
;                           Directory on remote data server in which to search for data.
;       LOCAL_DATA_ROOT:    in, optional, type=string, default='~/MrWebData'
;                           Directory on remote data server in which to search for data.
;       LOCAL_MIRROR_ROOT:  in, optional, type=string, default='/nfs'
;                           Root directory at which `REMOTE_DATA_DIR` is being mirrored.
;                               If no files are found on the mirror, the remote server
;                               will be checked and files downloaded to `LOCAL_DATA_DIR`.
;-
function MrVar_Config::INIT, $
DOWNLOAD_FLAG     = download_flag, $
REMOTE_DATA_ROOT  = remote_data_root, $
LOCAL_DATA_ROOT   = local_data_root, $
LOCAL_MIRROR_ROOT = local_mirror_root
	compile_opt idl2

	;Error handling
	catch, the_error
	if the_error ne 0 then begin
		catch, /CANCEL
		MrPrintF, 'LogErr'
		return, 0
	endif
	
	;Create the web object
	oWeb = MrWebData(DIRECTORY=local_data_root)

	;Defaults
	if n_elements(download_flag)     eq 0 then download_flag     = 1B
	if n_elements(local_data_root)   eq 0 then local_data_root   = oWeb.local_root
	if n_elements(remote_data_root)  eq 0 then remote_data_root  = 'https://lasp.colorado.edu/mms/sdc/public/data/'
	if n_elements(local_mirror_root) eq 0 then local_mirror_root = '/nfs'
	
	;Set properties
	self.download_flag     = download_flag
	self.local_data_root   = local_data_root
	self.local_mirror_root = local_mirror_root
	self.remote_data_root  = remote_data_root
	self.oWeb              = oWeb

	return, 1
end


;+
;   Clean up after the object is destroyed
;-
pro MrVar_Config::CLEANUP
	compile_opt idl2
	on_error, 2
	
	self -> IDL_Object::Cleanup
end


;+
;   Get object properties.
;
; :Keywords:
;       DOWNLOAD_FLAG:      out, optional, type=byte, default=1B
;                           Indicates how data should be downloaded. Optinos are::
;                               0 - Do not download. Check local directory.
;                               1 - Search for files on web. If they exist and are
;                                   not found locally, then download them.
;       REMOTE_DATA_ROOT:   out, optional, type=string, default='https://lasp.colorado.edu/mms/sdc/public/data/'
;                           Directory on remote data server in which to search for data.
;       LOCAL_DATA_ROOT:    out, optional, type=string, default='~/MrWebData'
;                           Directory on remote data server in which to search for data.
;       LOCAL_MIRROR_ROOT:  out, optional, type=string, default='/nfs'
;                           Root directory at which `REMOTE_DATA_DIR` is being mirrored.
;                               If no files are found on the mirror, the remote server
;                               will be checked and files downloaded to `LOCAL_DATA_DIR`.
;-
pro MrVar_Config::GetProperty, $
DOWNLOAD_FLAG     = download_flag, $
REMOTE_DATA_ROOT  = remote_data_root, $
LOCAL_DATA_ROOT   = local_data_root, $
LOCAL_MIRROR_ROOT = local_mirror_root, $
OWEB              = oWeb
	compile_opt idl2
	on_error, 2
	
	;Defaults
	if arg_present(download_flag)     eq 0 then download_flag     = self.download_flag
	if arg_present(local_data_root)   eq 0 then local_data_root   = self.local_data_root
	if arg_present(remote_data_root)  eq 0 then remote_data_root  = self.remote_data_root
	if arg_present(local_mirror_root) eq 0 then local_mirror_root = self.local_mirror_root
	if arg_present(oWeb)              eq 0 then oWeb              = self.oWeb
end


;+
;   Set object properties.
;
; :Keywords:
;       DOWNLOAD_FLAG:      in, optional, type=byte, default=1B
;                           Indicates how data should be downloaded. Optinos are::
;                               0 - Do not download. Check local directory.
;                               1 - Search for files on web. If they exist and are
;                                   not found locally, then download them.
;       REMOTE_DATA_ROOT:   in, optional, type=string, default='https://lasp.colorado.edu/mms/sdc/public/data/'
;                           Directory on remote data server in which to search for data.
;       LOCAL_DATA_ROOT:    in, optional, type=string, default='~/MrWebData'
;                           Directory on remote data server in which to search for data.
;       LOCAL_MIRROR_ROOT:  in, optional, type=string, default='/nfs'
;                           Root directory at which `REMOTE_DATA_DIR` is being mirrored.
;                               If no files are found on the mirror, the remote server
;                               will be checked and files downloaded to `LOCAL_DATA_DIR`.
;-
pro MrVar_Config::SetProperty, $
DOWNLOAD_FLAG     = download_flag, $
REMOTE_DATA_ROOT  = remote_data_root, $
LOCAL_DATA_ROOT   = local_data_root, $
LOCAL_MIRROR_ROOT = local_mirror_root
	compile_opt idl2
	on_error, 2
	
	;Defaults
	if n_elements(download_flag)     eq 0 then self.download_flag     = download_flag
	if n_elements(local_data_root)   eq 0 then self.local_data_root   = local_data_root
	if n_elements(remote_data_root)  eq 0 then self.remote_data_root  = remote_data_root
	if n_elements(local_mirror_root) eq 0 then self.local_mirror_root = local_mirror_root
end


;+
;   The class definition statement.
;
; :Params:
;       CLASS:          out, optional, type=structure
;
; :Fields:
;       DATA:       Data to be accessed via bracket overloading.
;-
pro MrVar_Config__DEFINE, class
	compile_opt idl2
	
	class = { MrVar_Config, $
	          inherits IDL_Object, $
	          oWeb:             obj_new(), $
	          download_flag:    0B, $
	          remote_data_dir:  '', $
	          local_data_dir:   '', $
	          local_mirror_dir: '' $
	        }
end