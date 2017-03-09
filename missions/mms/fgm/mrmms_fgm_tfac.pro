; docformat = 'rst'
;
; NAME:
;       MrMMS_FGM_TFAC
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
; PURPOSE:
;+
;   Create a transformation matrix to field-aligned coordinates (FAC).
;
; :Categories:
;       MMS, FGM, MrVariable
;
; :Params:
;       SC:                 in, required, type=string/strarr
;                           The MMS spacecraft identifier. Options are:
;                               {'mms1' | 'mms2' | 'mms3' | 'mms4'}
;       MODE:               in, required, type=string/strarr
;                           Telemetry mode of the data. Options include:
;                               {'slow' | 'fast' | 'srvy' | 'brst'}
;       FAC:                in, optional, type=string, default=''
;                           The field-aligned coordinate system to create. Options are:
;                               {'' | 'VXB' | 'EXB' }.
;       VAR:                in, optional, type=string/objref
;                           A MrVariable name or object for which the time tags define
;                               the grid onto which the transformation matrix is
;                               interpolated. If not present, then the magnetic field
;                               is interpolated to the time of whichever variable defines
;                               the perpendicular direction, as per `FAC`.
;
; :Keywords:
;       COORDS:             in, optional, type=string, default='GSE'
;                           Original coordinate system.
;       INSTR:              in, optional, type=string, default='dfg'
;                           The fluxgate instrument to use. The default is 'DFG' for
;                               `MODE`='srvy' and 'FGM' for `MODE`='brst'.
;       LEVEL:              in, optional, type=float, default='L2'
;                           Data quality level to use.
;       NO_LOAD:            in, optional, type=boolean, default=0
;                           Set if the data that define the field-aligned coordinate
;                               system has already been loaded into the variable cache.
;       TRANGE:             in, optional, type=strarr(2), default=MrVar_GetTRange()
;                           Time interval over which to create the transformation.
;
; :See Also:
;    MrVar_FAC.pro
;    MrMMS_FGM_Load_FAC_Data.pro
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
;       2016/09/24  -   Written by Matthew Argall
;-
function MrMMS_FGM_TFAC, sc, mode, fac, var, $
COORD_SYS=coord_sys, $
INSTR=instr, $
LEVEL=level, $
NO_LOAD=no_load, $
SUFFIX=suffix, $
TRANGE=trange, $
VARNAMES=varnames
	compile_opt idl2

	catch, the_error
	if the_error ne 0 then begin
		catch, /CANCEL
		MrPrintF, 'LogErr'
		return, obj_new()
	endif

;-------------------------------------------
; Load the Data ////////////////////////////
;-------------------------------------------
	MrMMS_FGM_Load_FAC_Data, sc, mode, fac, var, $
	                         COORD_SYS = coord_sys, $
	                         INSTR     = instr, $
	                         LEVEL     = level, $
	                         NO_LOAD   = no_load, $
	                         SUFFIX    = suffix, $
	                         TRANGE    = trange, $
	                         VARNAMES  = varnames

;-------------------------------------------
; Sampling Rates ///////////////////////////
;-------------------------------------------
	;Transformation
	if n_elements(varnames) eq 2 $
		then oT = MrVar_FAC(varnames[0], varnames[1], TYPE=fac) $
		else oT = MrVar_FAC(varnames[0], TYPE=fac)

;-------------------------------------------
; Set Properties ///////////////////////////
;-------------------------------------------
	oB_fac = MrVar_Get(varnames[0])
	oT    -> AddAttr, 'CATDESC',  'Transformation matrix to field algined coordinates.'
	oT    -> AddAttr, 'DEPEND_0', oB_fac['DEPEND_0']

	return, oT
end