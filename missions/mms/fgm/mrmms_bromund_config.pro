; docformat = 'rst'
;
; NAME:
;       MrMMS_Bromund_Config
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
;   Configure Ken Brommund's despin and coordinate transformation routines.
;
; :Keywords:
;       DATA_PATH_ROOT:     in, optional, type=string, default='/nfs/'
;                           Root of the SDC-like directory structure.
;       DROPBOX_ROOT:       in, optional, type=string, default='/nfs/fsm/temp/'
;                           Directory in which new files are placed before being moved
;                               to `DATA_PATH_ROOT`.
;       LOG_PATH_ROOT:      in, optional, type=string, default='/nfs/fsm/logs/'
;                           Root directory in which log files are saved.
;       CAL_PATH_ROOT:      in, optional, type=string, default='/nfs/mag_cal/'
;                           Root directory in which calibration files are stored.
;       CDF_BASE:           in, optional, type=string, default='~/idl_patch/'
;                           Where CDF skeleton file tools are kept.
;       HK_ROOT:            in, optional, type=string, default='/nfs/hk/'
;                           Root directory in which house keeping files are stored.
;       LOGU:               in, optional, type=string, default=-2
;                           Logical unit number of a log file.
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
;       2017-04-18  -   Written by Matthew Argall
;-
PRO MrMMS_Bromund_Config, $
DATA_PATH_ROOT=data_path_root, $
DROPBOX_ROOT=dropbox_root, $
LOG_PATH_ROOT=log_path_root, $
CAL_PATH_ROOT=cal_path_root, $
CDF_BASE=cdf_base, $
HK_ROOT=hk_root, $
LOGU=logu
	Compile_Opt idl2
	On_Error, 2
	
	;Add ken's routines to the IDL path
	mms_fg_config
	
	IF GetEnv('DATA_PATH_ROOT') EQ '' && N_Elements(data_path_root) EQ 0 THEN !mms_fg.data_path_root = '/nfs/'
	IF GetEnv('DROPBOX_ROOT')   EQ '' && N_Elements(dropbox_root)   EQ 0 THEN !mms_fg.dropbox_root   = '/nfs/fsm/temp/'
	IF GetEnv('LOG_PATH_ROOT')  EQ '' && N_Elements(log_path_root)  EQ 0 THEN !mms_fg.log_path_root  = '/nfs/fsm/logs/'
	IF GetEnv('CAL_PATH_ROOT')  EQ '' && N_Elements(cal_path_root)  EQ 0 THEN !mms_fg.cal_path_root  = '/nfs/mag_cal/'
	IF GetEnv('CDF_BASE')       EQ '' && N_Elements(cdf_base)       EQ 0 THEN !mms_fg.cdf_base       = '~/idl_patch/'
	IF GetEnv('HK_ROOT')        EQ '' && N_Elements(hk_root)        EQ 0 THEN !mms_fg.hk_root        = '/nfs/hk/'
	IF N_Elements(logu) EQ 0 THEN !mms_fg.logu = -2
END