; docformat = 'rst'
;
; NAME:
;       MrMMS_EDI_GPA_Compute
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
;   Compute the pitch and gyrophase angle of EDI particle trajectory information.
;   Resulting data is added to the MrVariable cache.
;
; :Categories:
;   MMS, EDI, MrVariable
;
; :Params:
;       TRAJ:       in, required, type=string/objref
;                   The MrVariable object reference or name corresponding to EDI
;                       particle trajectory data.
;       B:          in, required, type=string/objref
;                   The MrVectorTS object reference or name corresponding to fluxgate.
;                       magnetic field data.
;       VEC:        in, optional, type=string/objref, default=''
;                   The MrVectorTS object reference or name corresponding to a vector
;                       that is used to determine the direction perpendicular to the
;                       magnetic field.
;       TYPE:       in, optional, type=string, default=''
;                   If `VEC` is given, then TYPE describes the type of coordinate
;                       system. Options are: {'' | 'VxB', 'ExB', 'RadAz'}.
;
; :Keywords:
;       NAME_GA:    out, optional, type=string
;                   Name to be given to the MrScalarTS variable containing gyrophase data.
;       NAME_PA:    out, optional, type=string
;                   Name to be given to the MrScalarTS variable containing pitch angle data.
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
;       2014/06/28  -   Written by Matthew Argall
;-
pro MrMMS_EDI_PGA_Compute, traj, b, vec, type, $
NAME_PA=pa_name, $
NAME_GA=ga_name
	compile_opt idl2
	on_error, 2
	
	;Make sure we have object references
	oTraj = size(traj, /TNAME) eq 'STRING' ? MrVar_Get(traj) : traj
	oB    = size(b,    /TNAME) eq 'STRING' ? MrVar_Get(b)    : b
	oVec  = size(vec,  /TNAME) eq 'STRING' ? MrVar_Get(vec)  : vec
	
;-------------------------------------------
; Interpolate Data /////////////////////////
;-------------------------------------------

	;Interpolate data to EDI times
	oB_gdu = b   -> Interpol(oTraj, NAME=oB.name   + '_gdu')
	oV_gdu = vec -> Interpol(oTraj, NAME=oVec.name + '_gdu')

;-------------------------------------------
; Rotate to Field-Aligned CS ///////////////
;-------------------------------------------
	
	;Convert to cartesian vectors
	x = sin(oTraj[1,*] * !dpi/180D) * cos(oTraj[0,*] * !dpi/180D)
	y = sin(oTraj[1,*] * !dpi/180D) * sin(oTraj[0,*] * !dpi/180D)
	z = cos(oTraj[1,*] * !dpi/180D)
	oTraj_xyz = MrVectorTS( [ temporary(x), temporary(y), temporary(z) ] )

	;Field-Aligned Coordinates
	oT        = MrVar_FAC(oB_gdu, oV_gdu, TYPE=type)
	oTraj_fac = oT ## oTraj_xyz

;-------------------------------------------
; Pitch-Angle and Gyrophase ////////////////
;-------------------------------------------
	
	;Pitch Angle
	;   - b.v = |b| |v| cos(theta)
	;   - theta = acos(b.v)
	pa = acos(oTraj_fac[*,2]) * 180.0/!pi
	
	;Gyrophase Angle
	ga = atan(oTraj_fac[*,1], oTraj_fac[*,0]) * 180.0/!pi
	
	;Create output variable
	oPA = MrScalarTS(pa, /CACHE, /NO_COPY, NAME=pa_name)
	oGA = MrScalarTS(ga, /CACHE, /NO_COPY, NAME=ga_name)

;-------------------------------------------
; Set Attributes //////////.////////////////
;-------------------------------------------
	;Pitch Angle
	oTraj -> CopyAttrTo, oPA, 'DEPEND_0'
	oPA   -> SetAttrValue, 'LABEL',      'PA'
	oPA   -> SetAttrValue, 'PLOT_TITLE', 'EDI Pitch Angle'
	oPA   -> SetAttrValue, 'TITLE',      'Pitch Angle!C(Degrees)'
	oPA   -> SetAttrValue, 'UNITS',      'Degrees'
	
	;Gyrophase
	oTraj -> CopyAttrTo, oGA, 'DEPEND_0'
	oGA   -> SetAttrValue, 'LABEL',      'GA'
	oGA   -> SetAttrValue, 'PLOT_TITLE', 'EDI Gyrophase Angle'
	oPA   -> SetAttrValue, 'TITLE',      'Gyrophase!C(Degrees)'
	oGA   -> SetAttrValue, 'UNITS',      'Degrees'
end