; docformat = 'rst'
;
; NAME:
;    MrVar_Dist_Rotate
;
; PURPOSE:
;+
;   Rotate a distribution function into field-aligned coordinates. The particle
;   Distribution, `F` must have 'DEPEND_1' and 'DEPEND_2' attributes, which contain
;   the azimuth and polar look angles, respectively, of the particle instrument.
;
;   Calling Sequence
;       f_fac = MrVar_Dist_Rotate( f, par )
;       f_fac = MrVar_Dist_Rotate( f, par, perp, fac )
;
; :Categories:
;    MMS
;
; :Params:
;       F:              in, required, type=string/integer/objref
;                       The name, index, or MrTimeSeries objref of the distribution function
;                           to be rotated.
;       PAR:            in, required, type=string/integer/objref
;                       The name, index, or MrVectorTS object of the vector that defines
;                           new parallel direction of the field-aligned coordinate system.
;       PERP:           in, required, type=string/integer/objref
;                       The name, index, or MrVectorTS object of the vector that defines
;                           principle perpendicular direction of the field-aligned
;                           coordinate system.
;       FAC:            in, required, type=string
;                       Name of the field-aligned coordinate system.
;
; :Keywords:
;       CACHE:          in, required, type=boolean, default=0
;                       If set, the output distribution will be added to the variable cache.
;       NAME:           in, required, type=string
;                       Name to be given to the output variable.
;       ORIENTATION:    in, required, type=string, default=3
;                       Orientation of the azimuth and polar angles in the FAC system.
;       REBIN:          in, required, type=boolean, default=0
;                       If set, the distribution will be rebinned into a uniform grid.
;
; :Returns:
;       OUTDIST:        out, required, type=objref
;                       A copy of `F` with the 'DEPEND_1' and 'DEPEND_2' attributes
;                           rotated into FAC.
;
; :See Also:
;       MrVar_Grid_Cart2Sphere.pro
;       MrVar_Grid_sphere2fac.pro
;       MrVar_Grid_sphere2fac.pro
;
; :Author:
;    Matthew Argall::
;    University of New Hampshire
;    Morse Hall Room 348
;    8 College Road
;    Durham, NH 03824
;    matthew.argall@unh.edu
;
; :History:
;    Modification History::
;       2017/02/28  -   Written by Matthew Argall
;-
FUNCTION MrVar_Dist_Rotate, f, par, perp, fac, $
CACHE=cache, $
NAME=name, $
ORIENTATION=orientation, $
REBIN=rebin
	Compile_Opt idl2
	On_Error, 2
	
	;Get the distribution
	oDist = MrVar_Get(f)
	tf_rebin = Keyword_Set(rebin)
	IF N_Elements(name) EQ 0 THEN name = oDist.name + '_fac'
	phi_fac_vname   = name + '_phi'
	theta_fac_vname = name + '_theta'

	;Convert from instrument coordinates to field-aligned coordinates
	MrVar_Grid_sphere2fac, par, oDist['DEPEND_1'], oDist['DEPEND_2'], oPhi_FAC, oTheta_FAC, $
	                       FAC         = fac, $
	                       ORIENTATION = orientation, $
	                       PERP        = perp, $
	                       /SPHERE, $
	                       TIME        = oDist['DEPEND_0']

;-----------------------------------------------------
; Store Variables \\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\
;-----------------------------------------------------

	;Set names
	oPhi_FAC   -> SetName, phi_fac_vname
	oTheta_FAC -> SetName, theta_fac_vname

	;Distribution
	outDist = oDist -> Copy( name, CACHE=cache )
	outDist['DEPEND_1'] = oPhi_FAC
	outDist['DEPEND_2'] = oTheta_FAC

;-----------------------------------------------------
; Rebin the Data \\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\
;-----------------------------------------------------

	IF tf_rebin THEN BEGIN
		;Calculate the volume of each velocity space element
		of_old = MrDist4D(f)
		odV    = of_old -> VolumeElement()
		Obj_Destroy, of_old
		
		;Rebin the data
		;   - The 4D distribution will have bogus angular information
		;     until we re-bin it.
		of_new  = MrDist4D(outDist)
		outDist = of_new -> RebinAngles(odV)
		Obj_Destroy, of_temp
	ENDIF

;-----------------------------------------------------
; Finished! \\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\
;-----------------------------------------------------
	RETURN, outDist
END