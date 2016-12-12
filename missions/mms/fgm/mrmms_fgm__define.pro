; docformat = 'rst'
;
; NAME:
;   MrMMS_FPI_Dist4D__Define
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
;   Calculate moments and reduced distributions from a 3D distribution function.
;
; :Categories:
;   MrVariable, MrTimeSeries, MrMMS_FPI_Dist4D
;
; :See Also:
;   MrVariable__Define.pro
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
; :Params:
;       TIME:           in, required, type=NxM array
;                       Name or reference of a MrTimeVar object, or an array
;                           of time stamps. If a name is provided, the assiciated
;                           variable must exist in the variable cache.
;       DATA:           in, required, type=NxM array
;                       Name or reference of a MrVariable object, or the dependent
;                           variable data. If a name is given, the associated variable
;                           must exist in the variable cache. 
;
; :Keywords:
;       CACHE:          in, optional, type=boolean, default=0
;                       If set, both `TIME` and `DATA` are added to the variable cache.
;       NAME:           in, optional, type=integer
;                       Name to be given to the variable object.
;       NO_CLOBBER:     in, optional, type=boolean, default=0
;                       If set, do not clobber variables of the same name. Instead,
;                           rename this variable by appending "_#" to `NAME`, where
;                           "#" represents a unique number. Ignored unless `CACHE` is set.
;       NO_COPY:        in, optional, type=boolean, default=0
;                       If set `DATA` will be copied directly into the object
;                           and will be left undefined (a MrTimeSeries object will not
;                           be destroyed, but its array will be empty).
;-
function MrMMS_FGM::INIT, sc, mode, $
_REF_EXTRA=extra
	compile_opt idl2

	;Error handling
	catch, the_error
	if the_error ne 0 then begin
		catch, /CANCEL
		MrPrintF, 'LogErr'
		return, 0
	endif
	
	;Allocate heap memory
	self.type_fac = 'NONE'

;-----------------------------------------------------
; Load Data \\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\
;-----------------------------------------------------
	case n_params() of
		;USAGE:
		;   - oFGM = MrMMS_FGM()
		0: if n_elements(extra) gt 0 then message, 'Keywords not allowed when no parameters are given.'
		
		;USAGE:
		;   - oFGM = MrMMS_FGM(variable)
		1: begin
			if n_elements(extra) gt 0 then message, 'Keywords not allowed when variable is given.'
		
			;Set the data
			variable = sc
			self -> SetData, variable
		end
		
		;USAGE:
		;   - oFGM = MrMMS_FGM(sc, mode)
		2: self -> Load, sc, mode, _STRICT_EXTRA=extra
		
		else: message, 'Incorrect number of parameters.'
	endcase
	
	return, 1
end


;+
;   Clean up after the object is destroyed
;-
pro MrMMS_FGM::CLEANUP
	compile_opt idl2
	on_error, 2
	
	;
	; Do not destroy OBVEC or OBMAG. They have been loaded into
	; the variable cache and may be used elsewhere.
	;
	
	obj_destroy, self.oBvec
	obj_destroy, self.oBmag
	obj_destroy, self.oPar_FAC
	obj_destroy, self.oPerp_FAC
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
;-
pro MrMMS_FGM::GetProperty, $
SC=sc, $
INSTR=instr, $
MODE=mode, $
LEVEL=level, $
OPTDESC=optdesc, $
COORD_SYS=coord_sys, $
TYPE_FAC=type_fac
	compile_opt idl2
	on_error, 2
	
	if arg_present(sc)        then sc        = self.sc
	if arg_present(instr)     then instr     = self.instr
	if arg_present(mode)      then mode      = self.mode
	if arg_present(level)     then level     = self.level
	if arg_present(optdesc)   then optdesc   = self.optdesc
	if arg_present(coord_sys) then coord_sys = self.coord_sys
	if arg_present(type_fac)  then type_fac  = self.type_fac
end


;+
;   Get the magnetic field data.
;
; :Params:
;       OBMAG:          out, optional, type=MrScalarTS object
;                       Magnitude of the magnetic field.
;
; :Returns:
;       OBVEC:          out, required, type=MrVectorTS object
;                       Vector magnetic field.
;-
function MrMMS_FGM::GetData, oBmag
	compile_opt idl2
	on_error, 2
	
	oBmag = self.oBmag
	return, self.oBvec
end


;+
;   Transform a spherical coordinate grid into a cartesian coordinate grid.
;
; :Returns:
;       T:              The transformation matrix to the field-aligned coordinate system.
;-
function MrMMS_FGM::GetTFAC
	compile_opt idl2
	on_error, 2

;-----------------------------------------------------
; Electric Field \\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\
;-----------------------------------------------------
	if self.type_fac eq 'EXB' then begin
		;Parallel: B
		;   - z_hat = self.oPar_FAC
		
		;Perp-1: ExB
		oX_hat = self.oPerp_FAC -> Cross(self.oPar_FAC)
		oX_hat = oX_hat -> Normalize()
		
		;Perp-2: Bx(ExB)
		oY_hat = self.oPar_FAC -> Cross(oX_hat)

;-----------------------------------------------------
; Velocity \\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\
;-----------------------------------------------------
	endif else if self.type_fac eq 'VXB' then begin
		;Parallel: B
		;   - oZ_hat = self.oPar_FAC
		
		;Perp-2: bxv = -vxb = e
		oY_hat = self.oPar_FAC -> Cross(self.oPerp_FAC)
		oY_hat = oY_hat -> Normalize()
		
		;Perp-1: (bxv)xb = exb
		oX_hat = oY_hat -> Cross(self.oPar_FAC)

;-----------------------------------------------------
; Radial/Azimuthal \\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\
;-----------------------------------------------------
	endif else if self.type_fac eq 'RADAZ' then begin
		;Parallel: B
		;   - oZ_hat = self.oPar_FAC
		
		;Perp2: bxr (azimuth)
		oY_hat = self.oPar_FAC -> Cross(self.oPerp_FAC)
		oY_hat = oY_hat -> Normalize()
		
		;Perp1: bx(bxr) (radial)
		oX_hat = oY_hat -> Cross(self.oPar_FAC)

;-----------------------------------------------------
; X-Axis \\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\
;-----------------------------------------------------
	endif else if self.type_fac eq '' gt 0 then  begin
		;Parallel to B
		;   - oZ_hat = oB_hat
		
		;Perp2: XxB
		oY_hat = self.oPar_FAC -> Cross([1,0,0])
		oY_hat = oY_hat -> Normalize()
		
		;Perp1: Yx(XxB)
		oX_hat = oY_hat -> Cross(self.oPar_FAC)

;-----------------------------------------------------
; Unknown \\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\
;-----------------------------------------------------
	endif else begin
		message, 'FAC TYPE not recognized: "' + self.type_fac + '".'
	endelse

;-----------------------------------------------------
; Form Rotation Matrix \\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\
;-----------------------------------------------------

	;Time dimension first
	;   - [time, component, axis]
	T = MrMatrixTS( self.oPar_FAC['TIMEVAR'], [ [[ oX_hat['DATA'] ]], $
	                                            [[ oY_hat['DATA'] ]], $
	                                            [[ self.oPar_FAC['DATA'] ]] ])

	;Return the matrix
	return, T
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
;       SPECIES:            in, optional, type=string, default='e'
;                           Particle species for which the distribution function is
;                               to be loaded. Options are: {'e' | 'i'} for electrons
;                               and ions, respectively.
;       FAC:                in, optional, type=string
;                           The field-aligned coordinate system into which the instrument
;                               look directions should be rotated.
;
; :Keywords:
;       COORD_SYS:          in, optional, type=string, default='gse'
;                           Coordinate system of the original distribution function.
;                               Options are: {'dbcs' | 'gse'}.
;       LEVEL:              in, optional, type=string/strarr, default='l2'
;                           Data quality level. Options include:
;                               {'sitl' | 'l1b' | 'l2'}
;       TEAM_SITE:          in, optional, type=boolean, default=0
;                           If set, requests will be sent to the team site (password
;                               required, L1A and above). Automatically set if `LEVEL`
;                               is below level 2. This option is sticky.
;       TRANGE:             out, optional, type=string, default=MrVar_GetTRange()
;                           Start and end times over which to read data.
;       VARNAMES:           out, optional, type=strarr
;                           Names of the variables loaded into the cache: [bvec, bmag].
;-
pro MrMMS_FGM::Load, sc, mode, $
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
	
	if n_elements(instr)     eq 0 then instr     = 'fgm'
	if n_elements(coord_sys) eq 0 then coord_sys = 'gse'
	if n_elements(suffix)    eq 0 then suffix    = ''
	
	if n_elements(level) then begin
		case instr of
			'dfg': level = mode eq 'brst' ? 'l2'    : 'l2pre'
			'afg': level = mode eq 'brst' ? 'l2pre' : 'l2'
			'fgm': level = 'l2'
			else: message, 'INSTR invalid: "' + instr + ".'
		endcase
	endif
	
	if coord_sys eq 'dsl' || coord_sys eq 'dbcs' then begin
		MrPrintF, 'LogWarn', 'FGM despun coordinates are DMPA.'
		fgm_cs = 'dmpa'
	endif else begin
		fgm_cs = 'dmpa'
	endelse
	
	;Variable names
	b_vname = strjoin([sc, instr, 'b', fgm_cs, mode, level], '_')
	
	;Get the FPI distribution function
	MrMMS_Load_Data, sc, instr, mode, level, $
	                 SUFFIX    = suffix, $
	                 VARFORMAT = b_vname

	;Split the magnitude from the vector
	self -> Load_SplitB, b_vname+suffix, bvec_vname, bmag_vname
	
	;Destroy the old variable
	MrVar_Delete, b_vname
	
	;Store data
	self.oBvec = MrVar_Get(bvec_vname)
	self.oBmag = MrVar_Get(bmag_vname)
	varnames   = [bvec_vname, bmag_vname]
	
	;Set object properties
	self.sc        = sc
	self.instr     = instr
	self.mode      = mode
	self.level     = level
	self.optdesc   = n_elements(optdesc) eq 0 ? '' : optdesc
	self.coord_sys = fgm_cs
end


;+
;   FGM CDF files combine B and |B| into a 4D array. Split up the vector and magnitude.
;
; :Params:
;       B_NAME:         in, required, type=string
;                       Name of the magnetic field variable.
;-
pro MrMMS_FGM::Load_SplitB, b_name, bvec_name, bmag_name
	compile_opt idl2
	on_error, 2

	;Find a place to split the variable name
	;   - Add "vec" and "mag"
	pos = strpos(b_name, '_b_')
	if pos eq -1 then begin
		pos       = stregex(b_name, '_(afg|dfg|fgm)_', LEN=len)
		bvec_name = strmid(b_name, 0, pos+len) + 'vec_' + strmid(b_name, pos+len)
		bmag_name = strmid(b_name, 0, pos+len) + 'mag_' + strmid(b_name, pos+len)
	endif else begin
		bvec_name = strmid(b_name, 0, pos+2) + 'vec' + strmid(b_name, pos+2)
		bmag_name = strmid(b_name, 0, pos+2) + 'mag' + strmid(b_name, pos+2)
	endelse

	;Grab the variable
	oB = MrVar_Get(b_name)
	oT = oB['TIMEVAR']

	;Separate magnitude from vector
	Bxyz  = MrVectorTS( oT, oB[*,0:2], NAME=bvec_name, /CACHE )
	Bmag  = MrScalarTS( oT, oB[*,3],   NAME=bmag_name, /CACHE )
	
	;Copy over all attributes
	oB -> CopyAttrTo, Bxyz
	oB -> CopyAttrTo, Bmag

	;BVEC - Set new attributes
	Bxyz['COLOR']     = ['blue', 'forest green', 'red']
	Bxyz['DIMENSION'] = 1
	Bxyz['LABEL']     = ['Bx', 'By', 'Bz']
	Bxyz['MIN_VALUE'] = min(Bxyz['MIN_VALUE'])
	Bxyz['MAX_VALUE'] = max(Bxyz['MAX_VALUE'])
	Bxyz['TITLE']     = 'B!C(nT)'
	
	;BMAG - Set new attributes
	Bmag['COLOR']     = 'black'
	Bmag['MIN_VALUE'] = 0
	Bmag['MAX_VALUE'] = max(Bmag['MAX_VALUE'])
	Bmag['TITLE']     = '|B|!C(nT)'
end


;+
;   Load the distribution from source. Loaded data should subsequently be saved as
;   properties unique to the subclass (and optionally removed from the variable cache).
;
; :Private:
;
; :Params:
;       SC:                 in, required, type=string/strarr
;                           The MMS spacecraft identifier. Options are:
;                               {'mms1' | 'mms2' | 'mms3' | 'mms4'}
;       MODE:               in, required, type=string/strarr
;                           Telemetry mode of the data. Options include: {'fast' | 'brst'}
;       LEVEL:              in, optional, type=string/strarr, default='l2'
;                           Data quality level. Options include:
;                               {'sitl' | 'l1b' | 'l2'}
;       COORD_SYS:          in, optional, type=string, default='gse'
;                           Coordinate system of the original distribution function.
;                               Options are: {'dbcs' | 'gse'}.
;-
function MrMMS_FGM::Load_FAC_E
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
	
	;Return the field
	return, oE
end


;+
;   Load the distribution from source. Loaded data should subsequently be saved as
;   properties unique to the subclass (and optionally removed from the variable cache).
;
; :Private:
;
; :Params:
;       SC:                 in, required, type=string/strarr
;                           The MMS spacecraft identifier. Options are:
;                               {'mms1' | 'mms2' | 'mms3' | 'mms4'}
;       MODE:               in, required, type=string/strarr
;                           Telemetry mode of the data. Options include: {'fast' | 'brst'}
;       LEVEL:              in, optional, type=string/strarr, default='l2'
;                           Data quality level. Options include:
;                               {'sitl' | 'l1b' | 'l2'}
;       COORD_SYS:          in, optional, type=string, default='gse'
;                           Coordinate system of the original distribution function.
;                               Options are: {'dbcs' | 'gse'}.
;-
function MrMMS_FGM::Load_FAC_V
	compile_opt idl2
	on_error, 2

	;Reminder warning
	;   - GSE is not available yet
	;   - FPI does not operate in slow survey, so there is no "srvy" product.
	if self.coord_sys eq 'gse' then begin
		MrPrintF, 'LogWarn', 'FPI data is not available in GSE. Using DBCS.'
		fpi_cs = 'dbcs'
	endif else if self.coord_sys eq 'dmpa' then begin
		MrPrintF, 'LogWarn', 'FPI despun coordinates are DBCS.'
		fpi_cs = 'dbcs'
	endif
	fpi_mode = self.mode eq 'srvy' ? 'fast' : self.mode
	
	;Get the magnetic field
	suffix  = '_fac_perp'
	v_vname = strjoin( [self.sc, 'des_bulkv', fpi_cs, fpi_mode], '_' )
	
	;Load the data
	MrMMS_FPI_Load_Data, self.sc, fpi_mode, $
	                     LEVEL     = self.level, $
	                     OPTDESC   = 'des-moms', $
	                     SUFFIX    = suffix, $
	                     VARFORMAT = v_vname, $
	                     VARNAMES  = varnames
	
	;Store data
	oV = MrVar_Get(v_vname + suffix)
	
	;Remove names from caceh
	MrVar_Remove, varnames
	
	;Return the field
	return, oV
end


;+
;   Load data required to create a transformation to a field-aligned coordinate system.
;   Data rates and modes for each instrument are taken into account.
;
; :Params:
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
;       PERP_MODE:          in, optional, type=string, default=same as MODE property
;                           Data rate mode in which the perpiduclar data is loaded.
;-
pro MrMMS_FGM::Load_FAC, fac, var, $
PERP_MODE=perp_mode
	compile_opt idl2

	catch, the_error
	if the_error ne 0 then begin
		catch, /CANCEL
		MrPrintF, 'LogErr'
		return
	endif
	
	;Defaults
	if n_elements(fac)       eq 0 then fac       = ''
	if n_elements(perp_mode) eq 0 then perp_mode = self.mode

;-------------------------------------------
; Sampling Rates ///////////////////////////
;-------------------------------------------

	;--------------------------------|
	; instr    | slow | fast |  brst |
	;---------------------------------
	; afg      |    4 |     8 |    64 |
	; dfg      |    8 |    16 |   128 |
	; fgm      |    4 |     8 |   128 |
	; scm      |   32 |    32 |  4096 |
	; edp      |   10 |    32 |  4096 |
	; edi-amb  |   32 |    32 |  1024 |
	; edi-q0   |    ? |     ? |     ? |
	; edi-elf  |  0.2 |  0.2  |  0.2  |
	; fpi      |  --  |  0.22 | 31.25 |
	;---------------------------------|
	
;-------------------------------------------
; Load Data ////////////////////////////////
;-------------------------------------------
	case strupcase(fac) of
		'VXB':  oPerpVec = self -> Load_FAC_V()
		'EXB':  oPerpVec = self -> Load_FAC_E()
		'':     ;Do nothing
		'NONE': ;Do nothing
		else:  message, 'FAC not recognized: "' + fac + '".'
	endcase

;-------------------------------------------
; Inteprolate Data /////////////////////////
;-------------------------------------------
	;Specific time cadence desired
	if n_elements(var) gt 0 then begin
		;Get the variable
		oVar = MrVar_Get(var)

		;Interpolate magnetic field
		oPar = self.oBvec -> Interpol(oVar)
		oPar -> SetName, self.oBvec.name + '_fac_par'
		
		;Interpolate perpendicular vector
		if n_elements(oPerpVec) gt 0 then begin
			oPerp  = oPerpVec -> Interpol(oVar)
			oPerp -> SetName, oPerpVec.name
		endif
	
	;Common time base for FAC
	endif else begin
		;Perpendicular vector is defined.
		if n_elements(oPerpVec) gt 0 then begin
			;Interpolate the magnetic field
			oPar  = self.oBvec -> Interpol(oPerpVec)
			oPar -> SetName, self.oBvec.name + '_fac_par'
			
			;Use the same perpendicular vector
			oPerp = oPerpVec
		
		;Only parallel direction is explicitly defined.
		endif else begin
			oPar = self.oBvec -> Copy()
			oPar -> SetName, self.oBvec.name + '_fac_par'
		endelse
	endelse

;-------------------------------------------
; Set FAC //////////////////////////////////
;-------------------------------------------
	self -> SetFAC, oPar, oPerp, fac
end


;+
;   Set the vector magnetic field and its magnitude (also, see the ::Load method).
;
; :Params:
;       VARIABLE:       in, required, type=string/integer/objref
;                       Name, number, of MrTimeSeries objref of the distribution function
;                           variable.
;-
pro MrMMS_FGM::SetData, b_var, bmag_var
	compile_opt idl2
	on_error, 2
	
	;Vector field
	oBvec = MrVar_Get(b_var)
	if ~obj_isa(oBvec, 'MrVectorTS') then message, 'B_VAR must be a MrVectorTS object.'
	self.oBvec = oBvec
	
	;Magnitude
	if n_elements(bmag_var) gt 0 then begin
		oBmag = MrVar_Get(b_var)
		if ~obj_isa(oBmag, 'MrScalarTS') then message, 'BMAG_VAR must be a MrScalarTS object.'
		self.oBmag = oBmag
	endif
end


;+
;   Set the field-aligned coordinate system.
;
;   CALLING SEQUENCE:
;       oDist3D -> SetFAC, par
;       oDist3D -> SetFAC, par, perp, fac_type
;
; :Keywords:
;       PAR:            in, required, type=Nx3 array
;                       Variable name or MrVectorTS object associated with a vector that
;                           defines the parallel direction. If a name is given, the
;                           variable must be in the variable cache.
;       PERP:           in, optional, type=Nx3 array
;                       Variable name or MrVectorTS object associated with a vector that
;                           defines one perpendicular direction. If a name is given, the
;                           variable must be in the variable cache.
;       TYPE_FAC:       in, optional, type=string
;                       Type of field-aligned coordinates to make. Options include
;                           {'ExB' | 'VxB' | 'RadAz' | ''}. Ignored if `PERP` is not given.
;-
pro MrMMS_FGM::SetFAC, par, perp, type_fac
	compile_opt idl2
	
	catch, the_error
	if the_error ne 0 then begin
		catch, /CANCEL
		MrPrintF, 'LogErr'
		
		;Reset data
		self.oPar_FAC  = oPar_FAC
		self.oPerp_FAC = oPerp_FAC
		self.type_fac  = old_fac
		
		return
	endif
	
	;Set FAC type
	self.type_fac = n_elements(type_fac) eq 0 ? '' : strupcase(type_fac)
	if self.type_fac eq 'NONE' then return
	
	;Keep the old data
	oPar_FAC  = self.oPar_FAC
	oPerp_FAC = self.oPerp_FAC
	old_fac   = self.type_fac
	
	;
	; Steps:
	;   1. Obtain par object
	;   2. If par time tags differ from implicit time, interpolate
	;   3. Obtain perp object, if given
	;   4. If perp time tags differ from implicit time, interpolate
	;   5. Create FAC Grid
	;

;-----------------------------------------------------
; PAR \\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\
;-----------------------------------------------------

	;OBJECT
	if size(par, /TNAME) eq 'OBJREF' then begin
		if obj_isa(par, 'MrVectorTS') $
			then oPar = par $
			else message, 'PAR must be a MrVectorTS object.'
	
	;NAME
	endif else if size(par, /TNAME) eq 'STRING' then begin
		;Cached?
		if MrVar_IsCached(par) $
			then oPar = MrVar_Get(par) $
			else message, 'PAR must be the name of a cached variable.'
	
	;OTHER
	endif else begin
		message, 'PAR must be a variable name or a MrVectorTS object.'
	endelse
	
	;Unit vector (direction, not magnitude)
	obj_destroy, self.oPar_FAC
	self.oPar_FAC = oPar -> Normalize()

;-----------------------------------------------------
; PERP \\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\
;-----------------------------------------------------
	if n_elements(perp) gt 0 then begin

		;OBJECT
		if size(perp, /TNAME) eq 'OBJREF' then begin
			if obj_isa(perp, 'MrVectorTS') $
				then oPerp = perp $
				else message, 'PERP must be a MrVectorTS object.'
	
		;NAME
		endif else if size(perp, /TNAME) eq 'STRING' then begin
			;Cached?
			if MrVar_IsCached(perp) $
				then oPerp = MrVar_Get(perp) $
				else message, 'PERP must be the name of a cached variable.'
	
		;OTHER
		endif else begin
			message, 'PERP must be a variable name or a MrVectorTS object.'
		endelse
		
		;Unit vector
		obj_destroy, self.oPerp_FAC
		self.oPerp_FAC = oPerp -> Normalize()
	endif
end


;+
;   The class definition statement.
;
; :Params:
;       CLASS:          out, optional, type=structure
;-
pro MrMMS_FGM__DEFINE
	compile_opt idl2
	
	class = { MrMMS_FGM, $
	          sc:        '', $
	          instr:     '', $
	          mode:      '', $
	          level:     '', $
	          optdesc:   '', $
	          coord_sys: '', $
	          type_fac:  '', $
	          oBvec:     obj_new(), $
	          oBmag:     obj_new(), $
	          oPar_FAC:  obj_new(), $
	          oPerp_FAC: obj_new() $
	        }
end