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
function MrMMS_Dist::INIT, sc, instr, mode, level, $
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
	
	;Load data
	vnames = self -> Load( sc, instr, mode, level, _STRICT_EXTRA=extra )
	
	return, 1
end


;+
;   Clean up after the object is destroyed
;-
pro MrMMS_Dist::CLEANUP
	compile_opt idl2
	on_error, 2

	obj_destroy, self.oPar_FAC
	obj_destroy, self.oPerp_FAC
end


;+
;   Create a distribution function from the data obtained via ::Load.
;-
function MrMMS_Dist::GetDist
	compile_opt idl2
	on_error, 2
	
	message, 'MrMMS_Dist::GetDist must be over-ridden by a subclass.'
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
pro MrMMS_Dist::GetProperty, $
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
;                           Names of all variables that have been loaded.
;-
pro MrMMS_Dist::Load, sc, instr, mode, level, $
OPTDESC=optdesc, $
SUFFIX=suffix, $
TEAM_SITE=team_site, $
VARFORMAT=varformat, $
VARNAMES=varname, $
TRANGE=trange
	compile_opt idl2

	catch, the_error
	if the_error ne 0 then begin
		catch, /CANCEL
		MrPrintF, 'LogErr'
		return
	endif
	
	;Get the FPI distribution function
	MrMMS_Load_Data, sc, instr, mode, level, $
	                 OPTDESC   = optdesc, $
	                 SUFFIX    = suffix, $
	                 TEAM_SITE = team_site, $
	                 VARFORMAT = varformat, $
	                 VARNAMES  = varnames
	
	;Set object properties
	self.sc    = sc
	self.instr = instr
	self.mode  = mode
	self.level = level
	self.optdesc = n_elements(optdesc) eq 0 ? '' : optdesc
end


;+
;   Find and read MMS FPI data.
;
; :Params:
;       FAC:                in, optional, type=string
;                           The field-aligned coordinate system into which the instrument
;                               look directions should be rotated.
;
; :Keywords:
;       COORD_SYS:          in, optional, type=string, default='gse'
;                           Coordinate system of the original distribution function.
;                               Options are: {'dbcs' | 'gse'}.
;       VARNAMES:           out, optional, type=strarr
;                           Names of all variables that have been loaded.
;-
pro MrMMS_Dist::Load_FAC, type_fac, time, $
COORD_SYS=coord_sys, $
INSTR = instr
	compile_opt idl2

	catch, the_error
	if the_error ne 0 then begin
		catch, /CANCEL
		MrPrintF, 'LogErr'
		
		;Delete any variables that were loaded
		MrVar_Delete, varnames
		
		return
	endif
	
	;Allow TYPE_FAC to be "NONE"
	if n_elements(type_fac) gt 0 && strlowcase(type_fac) eq 'none' then begin
		self.type_fac = type_fac
		return
	endif
	
	;Load FAC data
	suffix = '_MMS_Dist_FAC'
	MrMMS_FGM_Load_FAC_Data, self.sc, self.mode, type_fac, time, $
	                         COORD_SYS = coord_sys, $
	                         INSTR     = instr, $
	                         LEVEL     = self.level, $
	                         SUFFIX    = suffix, $
	                         VARNAMES  = varnames

	;Get variable objects
	oPar = MrVar_Get(varnames[0])
	if n_elements(varnames) gt 1 then oPerp = MrVar_Get(varnames[1])
	
	;Remove variables from caceh
	MrVar_Remove, varnames
	
	;Set FAC grid
	self -> SetFAC, oPar, oPerp, type_fac
	self.coord_sys=coord_sys
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
pro MrMMS_Dist::SetFAC, par, perp, type_fac
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
;   Transform a spherical coordinate grid into a cartesian coordinate grid.
;
; :Returns:
;       T:              The transformation matrix to the field-aligned coordinate system.
;-
function MrMMS_Dist::T_FAC
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
;   The class definition statement.
;
; :Params:
;       CLASS:          out, optional, type=structure
;-
pro MrMMS_Dist__DEFINE
	compile_opt idl2
	
	class = { MrMMS_Dist, $
	          sc:        '', $
	          instr:     '', $
	          mode:      '', $
	          level:     '', $
	          optdesc:   '', $
	          coord_sys: '', $
	          type_fac:  '', $
	          oPar_FAC:  obj_new(), $
	          oPerp_FAC: obj_new() $
	        }
end