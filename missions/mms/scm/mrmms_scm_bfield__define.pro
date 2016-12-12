; docformat = 'rst'
;
; NAME:
;   MrMMS_SCM_BField__Define
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
;   A class for MMS FGM vector magnetic field.
;
; :Categories:
;   MrVariable, MrTimeSeries, FGM
;
; :See Also:
;   MrVar_BField__Define.pro
;   MrTimeSeries__Define.pro
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
;       2016/10/24  -   Written by Matthew Argall
;-
;*****************************************************************************************
;+
;   The initialization method.
;
;   Calling Sequence:
;       oB = MrMMS_SCM_BField()
;       oB = MrMMS_SCM_BField(sc, mode)
;
; :Params:
;       SC:             in, required, type=NxM array
;                       Spacecraft identifier. Options are: {'mms1' | 'mms2' | 'mms3' | 'mms4'}.
;       MODE:           in, required, type=NxM array
;                       Data telemetry mode. Options are: {'srvy' | 'brst'}
;
; :Keywords:
;       _REF_EXTRA:     in, optional, type=boolean, default=0
;                       Any keyword accepted by ::SetData or ::Load methods is
;                           accepted via keyword inheritance. How they are used depends
;                           on the calling sequence.
;-
function MrMMS_SCM_BField::INIT, sc, mode, $
_REF_EXTRA=extra
	compile_opt idl2

	;Error handling
	catch, the_error
	if the_error ne 0 then begin
		catch, /CANCEL
		MrPrintF, 'LogErr'
		return, 0
	endif
	
	;Initialize the superclass
	if ~self -> MrVar_BField::Init() $
		then message, 'Unable to initialize superclass MrVar_BField.'
	
	;USAGE:
	;   - oB = MrMMS_SCM_BField()
	if n_elements(sc) eq 0 then begin
		if n_elements(extra) gt 0 then self -> SetData, _STRICT_EXTRA=extra
	
	;USAGE:
	;   - oB = MrMMS_SCM_BField(sc, mode)
	endif else begin
		self -> Load, sc, mode, _STRICT_EXTRA=extra
	
	endelse
	
	return, 1
end


;+
;   Clean up after the object is destroyed
;-
pro MrMMS_SCM_BField::CLEANUP
	compile_opt idl2
	on_error, 2
	
	self -> MrVar_BField::Cleanup
end


;+
;   Compute the variations of the magnetic field about the background values. If a FAC
;   has been defined, DB is rotated into field-aligned coordinates.
;
;       dB = B - B0
;
;   NOTE:
;       SCM, by definition, is the variation of the magnetic field. The main benefit
;       if this method is the rotation into FAC.
;
; :Returns:
;       DB:         out, required, type=MrVectorTS object
;                   Perturbations of the magnetic field.
;-
function MrMMS_SCM_BField::dB, $
CACHE=cache, $
NAME=name
	compile_opt idl2
	on_error, 2
	
	if n_elements(name) eq 0 then name = 'dB(' + self.oB.name + ')'
	
	;Get relevant data
	if ~obj_valid(self.oB)  then message, 'No magnetic field data has been loaded.'
	
	;Subtrack the background field
	;   - NO NEED TO SUBTRACT THE BACKGROUND FIELD!!
	;   - SCM does not contain a DC component
	
	;Rotate to FAC
	if self.fac ne 'NONE' then begin
		oT     = self -> T_FAC()
		dB     = oT ## self.oB
		labels = ['||', 'perp1', 'perp2']
	endif else begin
		dB     = self.oB -> Copy(name)
		labels = ['x', 'y', 'z']
	endelse
	
	;Set attributes
	dB -> AddAttr, 'CATDESC',    'Perturbations from the background magnetic field.'
	dB -> AddAttr, 'LABEL',      labels
	dB -> AddAttr, 'PLOT_TITLE', 'AC magnetic field'
	dB -> AddAttr, 'UNITS',      self.oB['UNITS']
	dB -> AddAttr, 'TITLE',      'dB'
	
	return, dB
end


;+
;   Transform a spherical coordinate grid into a cartesian coordinate grid.
;
; :Keywords:
;       SC:             out, optional, type=string
;                       MMS spacecraft identifier.
;       INSTR:          out, optional, type=string
;                       Instrument identifier.
;       LEVEL:          out, optional, type=string
;                       Data quality level.
;       MODE:           out, optional, type=string
;                       Telemetry mode.
;       OPTDESC:        out, optional, type=string
;                       EDI-specific optional descriptor.
;       COORD_SYS:      out, optional, type=string
;                       Coordinate system in which the data resides.
;       TYPE_FAC:       out, optional, type=string
;                       Name of the field-aligned coordinate system.
;       _REF_EXTRA:     out, optional, type=string
;                       Any keyword accepted by MrVectorTS::GetProperty is accepted here.
;-
pro MrMMS_SCM_BField::GetProperty, $
SC=sc, $
INSTR=instr, $
MODE=mode, $
LEVEL=level, $
OPTDESC=optdesc, $
COORD_SYS=coord_sys, $
_REF_EXTRA=extra
	compile_opt idl2
	on_error, 2
	
	if arg_present(sc)        then sc        = self.sc
	if arg_present(instr)     then instr     = self.instr
	if arg_present(mode)      then mode      = self.mode
	if arg_present(level)     then level     = self.level
	if arg_present(optdesc)   then optdesc   = self.optdesc
	if arg_present(coord_sys) then coord_sys = self.coord_sys
	
	if n_elements(extra) gt 0 then self -> MrVectorTS::GetProperty, _STRICT_EXTRA=extra
end


;+
;   Load the distribution from source. Loaded data should subsequently be saved as
;   properties unique to the subclass (and optionally removed from the variable cache).
;
; :Params:
;       SC:                 in, required, type=string/strarr
;                           The MMS spacecraft identifier. Options are:
;                               {'mms1' | 'mms2' | 'mms3' | 'mms4'}
;       MODE:               in, required, type=string/strarr
;                           Telemetry mode of the data. Options include: {'srvy' | 'brst'}
;
; :Keywords:
;       COORD_SYS:          in, optional, type=string, default='gse'
;                           Coordinate system of the original distribution function.
;                               Options are: {'dbcs' | 'gse'}.
;       LEVEL:              in, optional, type=string, default='L2'
;                           Data quality level. Options are: {'l1a' | 'l1b' | 'ql' | 'l2'}
;       OPTDESC:            in, optional, type=string
;                           Optional descriptor of data product. This is determined
;                               automatically except in the case of 'schb'. Options are:
;                               {'scs' | 'scf' | 'scsrvy' | 'scb' | 'schb'}.
;       TRANGE:             out, optional, type=string, default=MrVar_GetTRange()
;                           Start and end times over which to read data.
;       VARNAMES:           out, optional, type=strarr
;                           Names of the variables loaded into the cache: [bvec, bmag].
;-
pro MrMMS_SCM_BField::Load, sc, mode, $
COORD_SYS=coord_sys, $
OPTDESC=optdesc, $
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

;-------------------------------------------
; Defaults /////////////////////////////////
;-------------------------------------------
	
	instr = 'scm'
	if n_elements(coord_sys) eq 0 then coord_sys = 'gse'
	if n_elements(suffix)    eq 0 then suffix    = ''
	if n_elements(level)     eq 0 then level     = 'l2'
	if n_elements(optdesc)   eq 0 then begin
		case mode of
			'slow': optdesc = 'scs'
			'fast': optdesc = 'scf'
			'srvy': optdesc = 'scsrvy'
			'brst': optdesc = 'scb' ;There is also schb
			else: message, 'Invalid MODE: "' + mode + '".'
		endcase
	endif

;-------------------------------------------
; Load Data ////////////////////////////////
;-------------------------------------------
	
	;Variable names
	b_vname = strjoin([sc, instr, 'acb', coord_sys, optdesc, mode, level], '_')
	
	;Get the FPI distribution function
	MrMMS_Load_Data, sc, instr, mode, level, $
	                 SUFFIX    = suffix, $
	                 VARFORMAT = b_vname

;-------------------------------------------
; Finish ///////////////////////////////////
;-------------------------------------------
	;Output variable names
	varames = b_vname
	
	;Destroy the old variable
	self -> SetData, BFIELD=b_vname
	
	;Set object properties
	self.sc        = sc
	self.instr     = instr
	self.mode      = mode
	self.level     = level
	self.optdesc   = optdesc
	self.coord_sys = coord_sys
end


;+
;   Load the DC magnetic field from source.
;-
pro MrMMS_SCM_BField::Load_B0
	compile_opt idl2
	on_error, 2
	
	if self.sc   eq '' then message, 'Must set SC property.'
	if self.mode eq '' then message, 'Must set MODE property.'

	;Load the data
	oB0 = MrMMS_FGM_BField( self.sc, self.mode, $
	                        COORD_SYS = self.coord_sys, $
	                        LEVEL     = self.level )
	
	;Remove names from cache
	MrVar_Remove, oB0
	
	;Set the field.
	self -> SetB0, oB0
end


;+
;   Load the electric field from source.
;-
pro MrMMS_SCM_BField::Load_E
	compile_opt idl2
	on_error, 2
	
	if self.sc        eq '' then message, 'The SC property must be set.'
	if self.mode      eq '' then message, 'The MODE property must be set.'
	if self.coord_sys eq '' then message, 'The COORD_SYS property must be set.'
	level = self.level eq '' ? 'l2' : self.level

	;EDP does not make a srvy product; slow and fast are kept separate.
	if self.mode eq 'srvy' then begin
		MrPrintF, 'LogWarn', 'TODO: Combine slow and fast survey data.'
		edp_mode = 'fast'
	endif else begin
		edp_mode = self.mode
	endelse
	
	if self.coord_sys eq 'dmpa' then begin
		MrPrintF, 'LogWarn', 'EDP despun coordinates are DSL.'
		edp_cs = 'dsl'
	endif else begin
		edp_cs = self.coord_sys
	endelse
	
	;Get the magnetic field
	e_vname = strjoin([self.sc, 'edp', 'dce', edp_cs, edp_mode, level], '_')

	;Load the data
	MrMMS_Load_Data, self.sc, 'edp', edp_mode, self.level, $
	                 OPTDESC   = 'dce', $
	                 SUFFIX    = suffix, $
	                 VARFORMAT = e_vname, $
	                 VARNAME   = varnames

	;Store data
	self -> SetData, EFIELD=e_vname
end


;+
;   Set object properties.
;
; :Keywords:
;       SC:             out, optional, type=string
;                       MMS spacecraft identifier.
;       INSTR:          out, optional, type=string
;                       Instrument identifier.
;       LEVEL:          out, optional, type=string
;                       Data quality level.
;       MODE:           out, optional, type=string
;                       Telemetry mode.
;       OPTDESC:        out, optional, type=string
;                       EDI-specific optional descriptor.
;       COORD_SYS:      out, optional, type=string
;                       Coordinate system in which the data resides.
;       TYPE_FAC:       out, optional, type=string
;                       Name of the field-aligned coordinate system.
;       _REF_EXTRA:     out, optional, type=string
;                       Any keyword accepted by MrVectorTS::SetProperty is accepted here.
;-
pro MrMMS_SCM_BField::SetProperty, $
SC=sc, $
INSTR=instr, $
MODE=mode, $
LEVEL=level, $
OPTDESC=optdesc, $
COORD_SYS=coord_sys, $
_REF_EXTRA=extra
	compile_opt idl2
	on_error, 2
	
	if n_elements(sc)        gt 0 then self.sc        = sc
	if n_elements(instr)     gt 0 then self.instr     = instr
	if n_elements(mode)      gt 0 then self.mode      = mode
	if n_elements(level)     gt 0 then self.level     = level
	if n_elements(optdesc)   gt 0 then self.optdesc   = optdesc
	if n_elements(coord_sys) gt 0 then self.coord_sys = coord_sys
	
	if n_elements(extra) gt 0 then self -> MrVectorTS::SetProperty, _STRICT_EXTRA=extra
end


;+
;   The class definition statement.
;
; :Params:
;       CLASS:          out, optional, type=structure
;-
pro MrMMS_SCM_BField__DEFINE
	compile_opt idl2
	
	class = { MrMMS_SCM_BField, $
	          inherits MrVar_BField, $
	          sc:        '', $
	          instr:     '', $
	          mode:      '', $
	          level:     '', $
	          optdesc:   '', $
	          coord_sys: '' $
	        }
end