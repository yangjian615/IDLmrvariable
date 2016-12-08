; docformat = 'rst'
;
; NAME:
;   MrMMS_EDP_EField__Define
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
;   A class for MMS EDP vector electric field.
;
; :Categories:
;   MrVariable, MMS, EDP
;
; :See Also:
;   MrVar_EField__Define.pro
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
;       2016/12/04  -   Written by Matthew Argall
;-
;*****************************************************************************************
;+
;   The initialization method.
;
;   Calling Sequence:
;       oB = MrMMS_EDP_EField()
;       oB = MrMMS_EDP_EField(sc, mode)
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
function MrMMS_EDP_EField::INIT, sc, mode, $
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
	if ~self -> MrVar_EField::Init() $
		then message, 'Unable to initialize superclass MrVar_BField.'
	
	;USAGE:
	;   - oB = MrMMS_EDP_EField()
	if n_elements(sc) eq 0 then begin
		if n_elements(extra) gt 0 then self -> SetData, _STRICT_EXTRA=extra
	
	;USAGE:
	;   - oB = MrMMS_EDP_EField(sc, mode)
	endif else begin
		self -> Load, sc, mode, _STRICT_EXTRA=extra
	endelse
	
	return, 1
end


;+
;   Clean up after the object is destroyed
;-
pro MrMMS_EDP_EField::CLEANUP
	compile_opt idl2
	on_error, 2
	
	self -> MrVar_EField::Cleanup
end


;+
;   Get object properties.
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
pro MrMMS_EDP_EField::GetProperty, $
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
	
	if n_elements(extra) gt 0 then self -> MrVar_EField::GetProperty, _STRICT_EXTRA=extra
end


;+
;   Load the distribution from source. Loaded data should subsequently be saved as
;   properties unique to the subclass (and optionally removed from the variable cache).
;
; :Private:
;-
function MrMMS_EDP_EField::Load
	compile_opt idl2
	on_error, 2

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
	suffix  = '_fac_perp'
	e_vname = strjoin([self.sc, 'edp', 'dce', edp_cs, edp_mode, self.level], '_')

	;Load the data
	MrMMS_Load_Data, self.sc, 'edp', edp_mode, self.level, $
	                 OPTDESC   = 'dce', $
	                 SUFFIX    = suffix, $
	                 VARFORMAT = e_vname, $
	                 VARNAME   = varnames

	;Store data
	oE = MrVar_Get(e_vname + suffix)
	
	;Remove names from caceh
	MrVar_Remove, varnames
	
	;Set property
	self -> SetData, EFIELD=oE
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
;                           Telemetry mode of the data. Options include: {'fast' | 'brst'}
;
; :Keywords:
;       COORD_SYS:          in, optional, type=string, default='gse'
;                           Coordinate system of the original distribution function.
;                               Options are: {'dbcs' | 'gse'}.
;       INSTR:              in, optional, type=string/strarr, default='l2'
;                           FGM instrument to use. Options include:
;                               {'afg' | 'dfg' | 'fgm'}
;       LEVEL:              in, optional, type=string/strarr, default='l2'
;                           Data quality level. Options include:
;                               {'l1a' | 'ql' | 'l2pre' | 'l2'}
;       TRANGE:             out, optional, type=string, default=MrVar_GetTRange()
;                           Start and end times over which to read data.
;       VARNAMES:           out, optional, type=strarr
;                           Names of the variables loaded into the cache: [bvec, bmag].
;-
pro MrMMS_EDP_EField::Load_B, sc, mode, $
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

;-------------------------------------------
; Defaults /////////////////////////////////
;-------------------------------------------
	
	if n_elements(instr)     eq 0 then instr     = 'fgm'
	if n_elements(coord_sys) eq 0 then coord_sys = 'gse'
	if n_elements(suffix)    eq 0 then suffix    = ''
	if n_elements(level) eq 0 then begin
		case instr of
			'dfg': level = mode eq 'brst' ? 'l2'    : 'l2pre'
			'afg': level = mode eq 'brst' ? 'l2pre' : 'l2'
			'fgm': level = 'l2'
			else: message, 'INSTR invalid: "' + instr + ".'
		endcase
	endif
	
	if coord_sys eq 'dsl' || coord_sys eq 'dbcs' then begin
		MrPrintF, 'LogWarn', 'FGM despun coordinates are DMPA, not ' + strupcase(coord_sys) + '.'
		fgm_cs = 'dmpa'
	endif else begin
		fgm_cs = coord_sys
	endelse

;-------------------------------------------
; Load Data ////////////////////////////////
;-------------------------------------------
	
	;Variable names
	b_vname    = strjoin([sc, instr, 'b',    fgm_cs, mode, level], '_')
	bvec_vname = strjoin([sc, instr, 'bvec', fgm_cs, mode, level], '_')
	
	;Get the FPI distribution function
	MrMMS_FGM_Load_Data, sc, mode, level, $
	                     INSTR     = instr, $
	                     LEVEL     = level, $
	                     SUFFIX    = suffix, $
	                     VARFORMAT = b_vname

;-------------------------------------------
; Finish ///////////////////////////////////
;-------------------------------------------
	;Output variable names
	self -> SetData, BFIELD=bvec_vname
	
	;Set object properties
	self.sc        = sc
	self.instr     = instr
	self.mode      = mode
	self.level     = level
	self.optdesc   = n_elements(optdesc) eq 0 ? '' : optdesc
	self.coord_sys = fgm_cs
end


;+
;   Load the distribution from source. Loaded data should subsequently be saved as
;   properties unique to the subclass (and optionally removed from the variable cache).
;
; :Private:
;
; :Params:
;       SPECIES:        in, optional, type=string, default='e'
;                       Species of particles for which the velocity is loaded. Choices
;                           are "e" for electrons and "i" for ions.
;-
function MrMMS_EDP_EField::Load_V, species
	compile_opt idl2
	on_error, 2

	;Reminders
	;   - FPI despun coordinates are DBCS, not DSL or DMPA
	;   - FPI does not operate in slow survey, so there is no "srvy" product.
	if self.coord_sys eq 'dsl' then begin
		MrPrintF, 'LogWarn', 'FPI despun coordinates are DBCS, not DSL.'
		fpi_cs = 'dbcs'
	endif
	if n_elements(species) eq 0 then species = 'e'
	fpi_mode = self.mode eq 'srvy' ? 'fast' : self.mode
	fpi_instr = 'd' + species + 's'
	
	;Get the magnetic field
	suffix  = '_fac_perp'
	v_vname = strjoin( [self.sc, fpi_instr, 'bulkv', fpi_cs, fpi_mode], '_' )
	
	;Load the data
	MrMMS_FPI_Load_Data, self.sc, fpi_mode, $
	                     LEVEL     = self.level, $
	                     OPTDESC   = fpi_instr + '-moms', $
	                     VARFORMAT = v_vname, $
	                     VARNAMES  = varnames
	
	;Store data
	oV = MrVar_Get(v_vname)
	
	;Remove names from caceh
	MrVar_Remove, varnames
	
	;Return the field
	self -> SetData, VELOCITY=oV
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
pro MrMMS_EDP_EField::SetProperty, $
SC=sc, $
INSTR=instr, $
MODE=mode, $
LEVEL=level, $
OPTDESC=optdesc, $
COORD_SYS=coord_sys
	compile_opt idl2
	on_error, 2
	
	if n_elements(sc)        gt 0 then self.sc        = sc
	if n_elements(instr)     gt 0 then self.instr     = instr
	if n_elements(mode)      gt 0 then self.mode      = mode
	if n_elements(level)     gt 0 then self.level     = level
	if n_elements(optdesc)   gt 0 then self.optdesc   = optdesc
	if n_elements(coord_sys) gt 0 then self.coord_sys = coord_sys
end


;+
;   The class definition statement.
;
; :Params:
;       CLASS:          out, optional, type=structure
;-
pro MrMMS_EDP_EField__DEFINE
	compile_opt idl2
	
	class = { MrMMS_EDP_EField, $
	          inherits MrVar_EField, $
	          sc:        '', $
	          instr:     '', $
	          mode:      '', $
	          level:     '', $
	          optdesc:   '', $
	          coord_sys: '' $
	        }
end