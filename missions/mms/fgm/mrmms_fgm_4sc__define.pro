; docformat = 'rst'
;
; NAME:
;   MrMMS_FGM_4sc__Define
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
;   A class for applying multi-spacecraft analysis methods to MMS FGM magnetic field data.
;
; :Categories:
;   MMS, MrVariable
;
; :See Also:
;   MrVar_RecipVec__Define.pro
;   MrVar_BField_4sc__Define.pro
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
;       2016/11/24  -   Written by Matthew Argall
;-
;*****************************************************************************************
;+
;   The initialization method.
;
;   Calling Sequence:
;       o4sc = MrMMS_FGM_4sc()
;       o4sc = MrMMS_FGM_4sc(mode)
;       o4sc = MrMMS_FGM_4sc(mode, ephdesc)
;       o4sc = MrMMS_FGM_4sc(B1, B2, B3, B4)
;       o4sc = MrMMS_FGM_4sc(B1, B2, B3, B4, R1, R2, R3, R4)
;
; :Params:
;       B1:         in, required, type=integer/string/objref
;                   The data mode or a data variable. If a data mode is provided, data is
;                       loaded from source files. If a data variable is provided, it
;                       may be a name, number, or objref of the vector magnetic field from
;                       spacecraft 1.
;       B2:         in, optional, type=integer/string/objref
;                   Ephemeris product or variable. If an ephemeris product, position data
;                       is loaded from source files. If a variable, B2 can be the name,
;                       number, or objref of the vector magnetic field from spacecraft 2.
;       B3:         in, optional, type=integer/string/objref
;                   Name, number, or objref of the vector magnetic field from spacecraft 3.
;       B4:         in, optional, type=integer/string/objref
;                   Name, number, or objref of the vector magnetic field from spacecraft 4.
;       R1:         in, optional, type=integer/string/objref
;                   Name, number, or objref of the position vector from spacecraft 1.
;       R2:         in, optional, type=integer/string/objref
;                   Name, number, or objref of the position vector from spacecraft 2.
;       R3:         in, optional, type=integer/string/objref
;                   Name, number, or objref of the position vector from spacecraft 3.
;       R4:         in, optional, type=integer/string/objref
;                   Name, number, or objref of the position vector from spacecraft 4.
;
; :Keywords:
;       _REF_EXTRA:     in, optional, type=boolean, default=0
;                       Any keyword accepted by MrMMS_FGM_4sc::Load is also accepted here.
;-
function MrMMS_FGM_4sc::INIT, b1, b2, b3, b4, r1, r2, r3, r4, $
_REF_EXTRA=extra
	compile_opt idl2

	;Error handling
	catch, the_error
	if the_error ne 0 then begin
		catch, /CANCEL
		MrPrintF, 'LogErr'
		return, 0
	endif
	
	;Initialize superclass
	if ~self -> MrVar_BField_4sc::Init() $
		then message, 'Unable to initialize superclass.'

;-----------------------------------------------------
; Load Data \\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\
;-----------------------------------------------------
	case n_params() of
		;USAGE:
		;   - oFGM = MrMMS_FGM_4sc()
		0: if n_elements(extra) gt 0 then message, 'Keywords not allowed when no parameters are given.'
		
		;USAGE:
		;   - oFGM = MrMMS_FGM_4sc(mode)
		1: begin
			mode = b1
			self -> Load, mode, _STRICT_EXTRA=extra
			self -> LoadPosition
		endcase
		
		;USAGE:
		;   - oFGM = MrMMS_FGM_4sc(mode, ephdesc)
		2: begin
			mode    = b1
			ephdesc = b2
			self -> Load, mode, _STRICT_EXTRA=extra
			self -> LoadPosition, ephdesc
		endcase
		
		;USAGE:
		;   - oFGM = MrMMS_FGM_4sc(b1, b2, b3, b4)
		4: begin
			if n_elements(extra) gt 0 then message, 'Keywords not allowed when B1-4 are given.'
			self -> SetData, b1, b2, b3, b4
		endcase
		
		;USAGE:
		;   - oFGM = MrMMS_FGM_4sc(b1, b2, b3, b4)
		8: begin
			if n_elements(extra) gt 0 then message, 'Keywords not allowed when B1-4 and R1-4 are given.'
			self -> SetData, b1, b2, b3, b4
			self -> SetPosition, r1, r2, r3, r4
		endcase
		
		else: message, 'Incorrect number of parameters.'
	endcase
	
	return, 1
end


;+
;   Clean up after the object is destroyed
;-
pro MrMMS_FGM_4sc::CLEANUP
	compile_opt idl2
	on_error, 2

	self -> MrVar_BField_4sc::Cleanup
end


;+
;   Transform a spherical coordinate grid into a cartesian coordinate grid.
;
; :Keywords:
;       INSTR:          out, optional, type=string
;                       Instrument identifier.
;       LEVEL:          out, optional, type=string
;                       Data quality level.
;       MODE:           out, optional, type=string
;                       Telemetry mode.
;       COORD_SYS:      out, optional, type=string
;                       Coordinate system in which the data resides.
;-
pro MrMMS_FGM_4sc::GetProperty, $
INSTR=instr, $
LEVEL=level, $
MODE=mode, $
COORD_SYS=coord_sys
	compile_opt idl2
	on_error, 2
	
	if arg_present(instr)     then instr     = self.instr
	if arg_present(level)     then level     = self.level
	if arg_present(mode)      then mode      = self.mode
	if arg_present(coord_sys) then coord_sys = self.coord_sys
end


;+
;   Load data from source files.
;-
pro MrMMS_FGM_4sc::Load, mode, $
COORD_SYS=coord_sys, $
INSTR=instr, $
LEVEL=level, $
TRANGE=trange, $
VARNAMES=varnames
	compile_opt idl2
	
	catch, the_error
	if the_error ne 0 then begin
		catch, /CANCEL
		MrPrintF, 'LogErr'
		return
	endif
	
	;Defaults
	if n_elements(instr)     eq 0 then instr     = 'fgm'
	if n_elements(coord_sys) eq 0 then coord_sys = 'gse'
	if n_elements(level) eq 0 then begin
		case instr of
			'fgm': level = 'l2'
			'afg': level = 'l2pre'
			'dfg': level = 'l2pre'
			else: message, 'Invalid FGM instrument: "' + instr + '".'
		endcase
	endif

;-------------------------------------------
; Load Data ////////////////////////////////
;-------------------------------------------
	
	;Allocate memory
	nSC      = 4
	varnames = strarr(nSC)
	oMag     = objarr(nSC)
	
	;Loop through each spacecraft
	for i = 0, nSC-1 do begin
		;Spacecraft identifier
		sc = 'mms' + string(i+1, FORMAT='(i1)')
		
		;Load the data
		MrMMS_FGM_Load_Data, sc, mode, $
		                     INSTR     = instr, $
		                     LEVEL     = level, $
		                     VARFORMAT = '*_b_' + coord_sys + '_*', $
		                     VARNAMES  = temp_names
		
		;Pull the vector magnetic field
		iVar    = where(stregex(temp_names, '_bvec_' + coord_sys, /BOOLEAN))
		oMag[i] = MrVar_Get( temp_names[iVar] )
		
		;Store names
		varnames[i] = oMag[i].name
	endfor

;-------------------------------------------
; Set Data /////////////////////////////////
;-------------------------------------------
	
	;Set data
	self -> SetData, oMag[0], oMag[1], oMag[2], oMag[3]

	;Set Properties
	self -> SetProperty, INSTR     = instr, $
	                     MODE      = mode, $
	                     LEVEL     = level, $
	                     COORD_SYS = coord_sys
end


;+
;   Load position data from source files. Position data will be interpolated to
;   the time stamps of property OB1, which means ::Load or ::SetData must be
;   called first.
;
; :Params:
;       EPHDESC:        in, optional, type=string, default='ephts04d'
;                       Name of the ephemeris data product from which position vectors
;                           are loaded. Valid choices are derived from the optional
;                           descriptor of MEC file names, or 'defeph'.
;-
pro MrMMS_FGM_4sc::LoadPosition, ephdesc
	compile_opt idl2
	on_error, 2
	
	;Defaults
	if self.coord_sys      eq '' then message, 'COORD_SYS property must be defined.'
	if n_elements(ephdesc) eq 0  then ephdesc   = 'ephts04d'
	
	nSC      = 4
	varnames = strarr(nSC)
	oPos     = objarr(nSC)

	for i = 0, nSC - 1 do begin
		;Spacecraft identifier
		sc = 'mms' + string(i+1, FORMAT='(i1)')
		
		;Definitive ephemeris
		if ephdesc eq 'defeph' then begin
			MrMMS_Load_data, sc, $
			                 ANC_PRODUCT = ephdesc, $
			                 /ANCILLARY, $
			                 VARFORMAT   = '*R*', $
			                 VARNAMES    = temp_names
		
		;MEC files
		endif else begin
			MrMMS_Load_Data, sc, 'mec', 'srvy', 'l2', $
			                 OPTDESC   = ephdesc, $
			                 VARFORMAT = '*_r_' + self.coord_sys, $
			                 VARNAMES  = temp_names
		endelse

		;Retrieve the variable
		iPos        = where( stregex(temp_names, '(_r_|_R$)', /BOOLEAN) )
		oPos[i]     = MrVar_Get( (temporary(temp_names))[iPos] )
		varnames[i] = oPos[i].name
	endfor
	
	;Set object properties
	self -> SetPosition, oPos[0], oPos[1], oPos[2], oPos[3]
end


;+
;   Quantities for which the spatial gradients are computed. All variables will be
;   interpolated to P1.
;
; :Params:
;       B1:         in, required, type=integer/string/objref
;                   Name, number, or objref of the vector magnetic field from spacecraft 1.
;       B2:         in, required, type=integer/string/objref
;                   Name, number, or objref of the vector magnetic field from spacecraft 2.
;       B3:         in, required, type=integer/string/objref
;                   Name, number, or objref of the vector magnetic field from spacecraft 3.
;       B4:         in, required, type=integer/string/objref
;                   Name, number, or objref of the vector magnetic field from spacecraft 4.
;
; :Keywords:
;       COORD_SYS:  in, optional, type=string
;                   Coordinate system of the magnetic field. If not provided, `B1`s name
;                       property will be dissected according to the MMS variable naming
;                       convention. If this fails, the empty string is used.
;       INSTR:      in, optional, type=string
;                   FGM instrument that recorded the magnetic field. If not provided, `B1`s name
;                       property will be dissected according to the MMS variable naming
;                       convention. If this fails, the empty string is used.
;       LEVEL:      in, optional, type=string
;                   Quality level of the data. If not provided, `B1`s name
;                       property will be dissected according to the MMS variable naming
;                       convention. If this fails, the empty string is used.
;       MODE:       in, optional, type=string
;                   Data rate mode of the data. If not provided, `B1`s name
;                       property will be dissected according to the MMS variable naming
;                       convention. If this fails, the empty string is used.
;-
pro MrMMS_FGM_4sc::SetData, b1, b2, b3, b4, $
INSTR=instr, $
LEVEL=level, $
MODE=mode, $
COORD_SYS=coord_sys
	compile_opt idl2
	on_error, 2
	
	;Set data first
	self -> MrVar_BField_4sc::SetData, b1, b2, b3, b4
	
	;Dissect variable names
	parts = stregex( self.oB1.name, '(mms[1-4])_'            + $    ;SC
	                                '(afg|dfg|fgm)_.*'       + $    ;instr
	                                '(dmpa|bcs|gse|gsm)_.*'  + $    ;coord sys
	                                '(slow|fast|srvy|brst)_' + $    ;mode
	                                '(l1a|l1b|l2pre|l2)_', $        ;level
	                                /SUBEXP, /EXTRACT )
	
	;Default properties
	;   - If the stregex does not match, empty strings are returned.
	if n_elements(instr)     eq 0 then instr     = parts[1]
	if n_elements(coord_sys) eq 0 then coord_sys = parts[2]
	if n_elements(mode)      eq 0 then mode      = parts[3]
	if n_elements(level)     eq 0 then level     = parts[4]
	
	;Set properties
	self.coord_sys = coord_sys
	self.instr     = instr
	self.level     = level
	self.mode      = mode
end


;+
;   Transform a spherical coordinate grid into a cartesian coordinate grid.
;
; :Keywords:
;       INSTR:          in, optional, type=string
;                       Instrument identifier.
;       LEVEL:          in, optional, type=string
;                       Data quality level.
;       MODE:           in, optional, type=string
;                       Telemetry mode.
;       COORD_SYS:      in, optional, type=string
;                       Coordinate system in which the data resides.
;-
pro MrMMS_FGM_4sc::SetProperty, $
INSTR=instr, $
LEVEL=level, $
MODE=mode, $
COORD_SYS=coord_sys
	compile_opt idl2
	on_error, 2
	
	if n_elements(instr)     gt 0 then self.instr     = instr
	if n_elements(level)     gt 0 then self.level     = level
	if n_elements(mode)      gt 0 then self.mode      = mode
	if n_elements(coord_sys) gt 0 then self.coord_sys = coord_sys
end


;+
;   The class definition statement.
;
; :Params:
;       CLASS:          out, optional, type=structure
;-
pro MrMMS_FGM_4sc__DEFINE
	compile_opt idl2
	
	class = { MrMMS_FGM_4sc, $
	          inherits MrVar_BField_4sc, $
	          coord_sys: '', $
	          instr:     '', $
	          level:     '', $
	          mode:      '' $
	        }
end