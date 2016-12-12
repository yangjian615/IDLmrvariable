; docformat = 'rst'
;
; NAME:
;       MrMMS_Anc_Load
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
;   Restrict time range for certain variables.
;
;   Calling Sequence::
;      MrVar_TLimit, varnames
;      MrVar_TLimit, varnames, trange
;
; :Categories:
;   MrVariable
;
; :Params:
;       VARNAMES:       in, required, type=strarr(2)
;                       Names of the variables for which to limit the time range. If
;                           undefined, the names of all variables present in the cache
;                           are used.
;       TRANGE:         in, required, type=strarr(2), default=MrVar_GetTRange()
;                       Time interval by which to restrict data, formatted as
;                           ISO-8601 date-time strings.
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
;       2016-11-22  -   Written by Matthew Argall
;-
pro MrVar_TLimit, varnames, trange
	compile_opt idl2
	on_error, 2

	;Defaults
	if n_elements(trange)   gt 0 && trange[0] ne '' then MrVar_SetTRange, trange
	if n_elements(varnames) eq 0 then begin
		MrVar_Names, varnames
	endif else if ~array_equal( MrVar_IsCached(varnames), 1 ) then begin
		message, 'All variables identified by VARNAMES must be in the cache.'
	endif

	;Useful data
	nVars      = n_elements(varnames)
	trange     = MrVar_GetTRange()
	trange_ssm = MrVar_GetTRange('SSM')

;-----------------------------------------------------
; Pick out the Epoch Variables \\\\\\\\\\\\\\\\\\\\\\\
;-----------------------------------------------------
	;Find epoch variables
	nTime = 0
	oTime = objarr(nVars)
	tName = strarr(nVars)
	for i = 0, nVars - 1 do begin
		;Get the variable object
		;   - Skip variables that are not cached.
		theVar = MrVar_Get(varnames[i])
		if ~obj_valid(theVar) then continue
		
		;TIME
		if obj_isa(theVar, 'MrTimeVar') then begin
			tempTime = theVar
		
		;DATA - Get DEPEND_0 object
		endif else if theVar -> HasAttr('DEPEND_0') then begin
			tempTime = MrVar_Get(theVar['DEPEND_0'])
		
		;NEITHER
		endif else begin
			tempTime = obj_new()
		endelse
		
		;Store the epoch variable
		if obj_valid(tempTime) then begin
			if ~MrIsMember( tName, tempTime.name ) then begin
				oTime[nTime]  = tempTime
				tName[nTime]  = tempTime.name
				nTime        += 1
			endif
		endif
	endfor
	
	;Return if no time variables
	if nTime eq 0 then return
	
	;Trim down results
	tName = tName[0:nTime-1]
	oTime = oTime[0:nTime-1]

;-----------------------------------------------------
; Loop Over Epoch Variables \\\\\\\\\\\\\\\\\\\\\\\\\\
;-----------------------------------------------------
	for i = 0, nTime - 1 do begin

	;-----------------------------------------------------
	; Set Epoch Range \\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\
	;-----------------------------------------------------
		;Get the variable
		theTime = oTime[i]

		;Convert to SSM and find relevant data
		t_ssm = theTime -> GetData('SSM', trange[0])
		iKeep = where( (t_ssm ge trange_ssm[0]) and (t_ssm le trange_ssm[1]), nKeep)
		if nKeep eq 0 then begin
			iKeep = value_locate(t_ssm, trange_ssm[0]) > 0
			iKeep = [iKeep, iKeep+1]
			MrPrintF, 'LogText', 'No data found in time interval for variable "' + theTime.name + '".'
			MrPrintF, 'LogText', '  Interval: [' + strjoin(trange, ', ') + ']'
			MrPrintF, 'LogText', '  Closest:  [' + strjoin(theTime[iKeep], ', ') + ']'
			MrPrintF, 'LogWarn', 'Choosing closest two points.'
		
		endif else if nKeep eq 1 then begin
			if t_ssm[iKeep]-trange_ssm[0] lt trange_ssm[0]-t_ssm[iKeep] $
				then iKeep = [iKeep, iKeep+1] $
				else iKeep = [iKeep-1, iKeep]
			MrPrintF, 'LogText', 'Only one point found in time interval for variable "' + theTime.name + '".'
			MrPrintF, 'LogText', '  Interval: [' + strjoin(trange, ', ') + ']'
			MrPrintF, 'LogText', '  Closest:  [' + strjoin(theTime[iKeep], ', ') + ']'
			MrPrintF, 'LogWarn', 'Including next closest point.'
		endif

		;Trim the time data
		newEpoch  = MrTimeVar( theTime[iKeep], NAME='TLimit(' + theTime.name + ')' )
		theTime  -> CopyAttrTo, newEpoch 

	;-----------------------------------------------------
	; Set Data Range \\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\
	;-----------------------------------------------------
		for j = 0, nVars - 1 do begin
			;Skip if the variable does not have an DEPEND_0 attribute
			theVar = MrVar_Get(varnames[j])
			if ~theVar -> HasAttr('DEPEND_0') then continue
			
			;Skip if the DEPEND_0 attribute is not the one we are looking at
			;   - DEPEND_0 may be an object or a variable name
			oDep0 = MrVar_Get(theVar['DEPEND_0'])
			if ~oDep0 -> IsIdentical( theTime ) then continue
			
			;Trim the variable data
			case obj_class(theVar) of
				'MRSCALARTS': theVar -> SetData, newEpoch, theVar[iKeep]
				'MRVECTORTS': theVar -> SetData, newEpoch, theVar[iKeep,*]
				'MRMATRIXTS': theVar -> SetData, newEpoch, theVar[iKeep,*,*]
				'MRTIMESERIES': begin
					case theVar.n_dimensions of
						1: theVar -> SetData, newEpoch, theVar[iKeep]
						2: theVar -> SetData, newEpoch, theVar[iKeep,*]
						3: theVar -> SetData, newEpoch, theVar[iKeep,*,*]
						4: theVar -> SetData, newEpoch, theVar[iKeep,*,*,*]
						5: theVar -> SetData, newEpoch, theVar[iKeep,*,*,*,*]
						6: theVar -> SetData, newEpoch, theVar[iKeep,*,*,*,*,*]
						7: theVar -> SetData, newEpoch, theVar[iKeep,*,*,*,*,*,*]
						8: theVar -> SetData, newEpoch, theVar[iKeep,*,*,*,*,*,*,*]
						else: message, 'Variable has unexpected number of dimensions: "' + theVar.name + '".'
					endcase
				endcase
				
				;TODO: Determine which is the time-varying dimension
				'MrVariable': MrPrintF, 'LogWarn', 'TLimit not possible for object class MrVariable.'
				else: MrPrintF, 'LogWarn', 'Unknown object of class "' + obj_class(theVar) + '".'
			endcase
		endfor
	endfor
end