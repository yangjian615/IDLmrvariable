; docformat = 'rst'
;
; NAME:
;       MrDist_PAEn
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
;   Reduce a single 3D distribution function to 2D by averaging over azimuth angle.
;
; :Categories:
;   Distribution Function
;
;
; :Params:
;       DISTFN:         in, required, type=NxMxL float
;                       The 3D distribution function to be reduced. Its dimensions must
;                           be [nPhi, nTheta, nEnergy].
;       PHI:            in, required, type=fltarr(N)
;                       The azimuthal coordinates on a spherical grid where each point
;                           in `DISTFN` is defined.
;       THETA:          in, required, type=fltarr(M)
;                       The polar coordinates on a spherical grid where each point
;                           in `DISTFN` is defined.
;
; :Keywords:
;       NTHETA_BINS:    in, optional, type=integer, default=nTheta
;                       The number of evently spaced polar angle bins for the reduced
;                           distribution.
;       PHI_RANGE:      in, optional, type=fltarr(2), default=[0.0\, 360.0]
;                       The range in azimuthal angle over which to average.
;
; :Returns:
;       DIST1D:         out, required, type=fltarr(L)
;                       The 1D distribution with size nEnergy.
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
;       2016/08/26  -   Written by Matthew Argall
;-
FUNCTION MrDist_Density_MMS_Example, $
NO_LOAD=no_load
	Compile_Opt idl2
	
	Catch, the_error
	IF the_error NE 0 THEN BEGIN
		Catch, /CANCEL
		MrPrintF, 'LogErr'
		RETURN, !Null
	ENDIF

	;Set the time range
	trange = ['2016-01-07T09:34:30','2016-01-07T09:38:00']
	MrVar_SetTRange, trange
	
	;Constants
	sc        = 'mms2'
	mode      = 'brst'
	level     = 'l2'
	species   = 'e'
	coord_sys = 'dbcs'
	tf_load   = ~Keyword_Set(no_load)

;-----------------------------------------------------
; Load MMS Data \\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\
;-----------------------------------------------------
	instr    = 'd' + species + 's'
	edp_mode = 'fast'
	
	IF mode EQ 'brst' $
		THEN fgm_mode = mode $
		ELSE fgm_mode = 'srvy'
	
	IF coord_sys EQ 'dbcs' $
		THEN fgm_coords = 'dmpa' $
		ELSE fgm_coords = coord_sys
	
	IF level EQ 'l2' $
		THEN fgm_instr = 'fgm' $
		ELSE fgm_instr = 'dfg'
	
	IF tf_load THEN BEGIN
		;DES Distribution
		MrMMS_FPI_Load_Dist3D, sc, mode, species, $
		                       COORD_SYS = coord_sys, $
		                       LEVEL     = level
		
		;FPI Moments
		MrMMS_FPI_Load_Data, sc, mode, $
		                     OPTDESC   = instr + '-moms', $
		                     VARFORMAT = ['*numberdensity*', '*bulkv_' + coord_sys + '*']
	
		;FGM
		MrMMS_FGM_Load_Data, sc, fgm_mode, $
		                     INSTR     = fgm_instr, $
		                     VARFORMAT = '*_b_' + fgm_coords + '*'
	
		;SCPot
		MrMMS_Load_Data, sc, 'edp', edp_mode, level, $
		                 OPTDESC   = 'scpot', $
		                 VARFORMAT = '*scpot*'
	ENDIF
	
;-----------------------------------------------------
; Photoelectron Model \\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\
;-----------------------------------------------------
	;Get the equivalent Moments file name
;	fmoms = MrMMS_Get_Filenames(sc, 'fpi', mode, level, $
;	                            COUNT   = nMoms, $
;	                            OPTDESC = 'd' + species + 's-moms')
;	fmoms = !MrMMS -> uri2dir(fmoms)
	
	;Read the photoelectron model global attributes
;	IF N_Elements(nMoms) GT 1 THEN MrPrintF, 'LogWarn', 'More than one moments file. Taking model from first.'
;	oCDF    = MrCDF_File(fmoms[0])
;	scl     = oCDF -> GetGlobalAttrValue('Photoelectron_model_scaling_factor')
;	fphe    = oCDF -> GetGlobalAttrValue('Photoelectron_model_filenames')
;	pot_scl = oCDF -> GetGlobalAttrValue('Spacecraft_potential_scaling_factor')
;	pot_off = oCDF -> GetGlobalAttrValue('Spacecraft_potential_offset')
;	Obj_Destroy, oCDF
	
	;Get the photo electron model
;	stepper_id = StRegEx( fphe, 'p([0-9]-[0-9])', /SUBEXP, /EXTRACT )
;	stepper_id = stepper_id[1]
	MrMMS_FPI_Load_Models, sc, mode, species, SUFFIX='_photoelectron_model'

;-----------------------------------------------------
; Extract Data \\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\
;-----------------------------------------------------
	
	;Variable names
	b_vname      = StrJoin( [sc, fgm_instr, 'b',     fgm_coords, fgm_mode, level], '_' )
	bvec_vname   = StrJoin( [sc, fgm_instr, 'bvec',  fgm_coords, fgm_mode, level], '_' )
	bmag_vname   = StrJoin( [sc, fgm_instr, 'bmag',  fgm_coords, fgm_mode, level], '_' )
	vsc_vname    = StrJoin( [sc, 'edp',     'scpot', edp_mode, level], '_' )
	n_vname      = StrJoin( [sc, instr,     'numberdensity', mode], '_' )
	v_vname      = StrJoin( [sc, instr,     'bulkv', coord_sys, mode], '_' )
	dist_vname   = StrJoin( [sc, instr,     'dist', mode], '_' )
	parity_vname = StrJoin( [sc, instr,     'steptable', 'parity', mode], '_' )
	delphi_vname    = StrJoin( [sc, instr,  'startdelphi', 'count', mode], '_' )
	ph_delphi_vname = StrJoin( ['mms', instr,  'startdelphi', 'counts', mode], '_' ) + '_photoelectron_model'
	ph_bgdist_p0_vname = StrJoin( ['mms', instr,  'bgdist', 'p0', mode], '_' ) + '_photoelectron_model'
	ph_bgdist_p1_vname = StrJoin( ['mms', instr,  'bgdist', 'p1', mode], '_' ) + '_photoelectron_model'
	ph_scl_vname       = StrJoin( [sc, instr,  'scl', 'model', mode], '_' ) + '_photoelectron_model'
	
	;Get the variables
	oBvec   = MrVar_Get( bvec_vname )
	oN      = MrVar_Get( n_vname )
	oV      = MrVar_Get( v_vname )
	oVsc    = MrVar_Get( vsc_vname )
	oDist   = MrVar_Get( dist_vname )
	oPhi    = MrVar_Get( oDist['DEPEND_1'] )
	oTheta  = MrVar_Get( oDist['DEPEND_2'] )
	oE      = MrVar_Get( oDist['DEPEND_3'] )
	oParity = MrVar_Get( parity_vname )
	oDelPhi = MrVar_Get( delphi_vname )
	oPhDelPhi  = MrVar_Get( ph_delphi_vname )
	oBgDist_P0 = MrVar_Get( ph_bgdist_p0_vname )
	oBgDist_P1 = MrVar_Get( ph_bgdist_p1_vname )
	oScl       = MrVar_Get( ph_scl_vname )
	
	;Convert degrees to radians
	rad2deg = !dpi / 180D

	;Extract the data
	B      = oBvec['DATA']
	t_fgm  = oBvec['TIME', 'SSM']
	t_fpi  = oN['TIME', 'SSM']
	n      = oN['DATA']
	v      = oV['DATA']
	t_edp  = oVsc['TIME', 'SSM']
	Vsc    = oVsc['DATA']
	fdist  = oDist['DATA']
	phi    = oPhi['DATA'] * rad2deg
	theta  = oTheta['DATA'] * rad2deg
	energy = oE['DATA']
	parity = oParity['DATA']
	delphi = oDelPhi['DATA']
	photo_delphi    = oPhDelPhi['DATA']
	photo_bgdist_p0 = oBgDist_P0['DATA']
	photo_bgdist_p1 = oBgDist_P1['DATA']
	nphoto          = oScl[[0]]
;	pot_scl         = Fix((StrSplit(pot_scl, ' ', /EXTRACT))[0], TYPE=4)
;	pot_off         = Fix((StrSplit(pot_off, ' ', /EXTRACT))[0], TYPE=4)

;-----------------------------------------------------
; Dimension Sizes \\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\
;-----------------------------------------------------
	;
	; Integration:
	;   Sum( Sum( Sum( f * V^2 * dv * sin(Theta) * dTheta * dPhi ) ) )
	;
	; Both dPhi and dTheta are constant 
	;

	nPhi_dims    = Size(phi,    /N_DIMENSIONS)
	nTheta_dims  = Size(theta,  /N_DIMENSIONS)
	nEnergy_dims = Size(energy, /N_DIMENSIONS)

	;Number of points
	nTime   = N_Elements(t_fpi)
	nPhi    = nPhi_dims    EQ 2 ? N_Elements(phi[0,*])    : N_Elements(phi)
	nTheta  = nTheta_dims  EQ 2 ? N_Elements(theta[0,*])  : N_Elements(theta)
	nEnergy = nEnergy_dims EQ 2 ? N_Elements(energy[0,*]) : N_Elements(energy)
	
;-----------------------------------------------------
; Subtract Photo Electron Model \\\\\\\\\\\\\\\\\\\\\\
;-----------------------------------------------------
	;Locate the photoelectron model spin phase angles within the distribution spin phase angles
	idx       = Floor(delphi/16.0)
	photo_idx = Floor(photo_delphi/16.0)
	iPhoto    = Value_Locate(photo_idx, idx)
	
	;Separate parity 0 from parity1
	i0 = Where(parity EQ 0, n0, COMPLEMENT=i1, NCOMPLEMENT=n1)
	
	;Create the photoelectron distribution
	fphoto = FltArr(n0+n1, nPhi, nTheta, nEnergy)
	IF n0 GT 0 THEN fphoto[i0,*,*,*] = photo_bgdist_p0[iPhoto[i0],*,*,*]
	IF n1 GT 0 THEN fphoto[i1,*,*,*] = photo_bgdist_p1[iPhoto[i1],*,*,*]
stop
	;Scale the photo electron distribution and subtract it from
	;the skymap
	fphoto     *= nphoto
	fdist_photo = (fdist - Temporary(fphoto)) > 0
	
	;Clear up some data
	delphi          = !Null
	i0              = !Null
	i1              = !Null
	parity          = !Null
	photo_idx       = !Null
	photo_delphi    = !Null
	iPhoto          = !Null
	photo_bgdist_p0 = !Null
	photo_bgdist_p1 = !Null
	
;-----------------------------------------------------
; Integrate Over Phi \\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\
;-----------------------------------------------------

	;PHI Step
	IF nPhi_dims EQ 2 $
		THEN dPhi = Median( phi[*,1:*] - phi[*,0:nPhi-2] ) $
		ELSE dPhi = Median( phi[1:*] - phi )
	
	;PHI Integration
	fdist       = Total( fdist,       2 ) * dPhi
	fdist_photo = Total( fdist_photo, 2 ) * dPhi
stop
	;Clear variables
	phi       = !Null
	dPhi      = !Null
	nPhi_dims = !Null
	
;-----------------------------------------------------
; Integrate Over Theta \\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\
;-----------------------------------------------------
	
	;THETA Step
	IF nTheta_dims EQ 2 THEN BEGIN
		dTheta     = Median( theta[*,1:*] - theta[*,0:nPhi-2] ) 
		theta_temp = Rebin( theta, nTime, nTheta, nEnergy )
	ENDIF ELSE BEGIN
		dTheta     = Median( theta[1:*] - theta )
		theta_temp = Rebin( Reform( theta, 1, nTheta ), nTime, nTheta, nEnergy )
	ENDELSE
	
	;THETA Integration
	fdist       = Total( fdist       * Sin( theta_temp ),            2 ) * dTheta
	fdist_photo = Total( fdist_photo * Sin( Temporary(theta_temp) ), 2 ) * dTheta
	
	;Clear variables
	nTheta_dims = !Null
	dTheta      = !Null
	theta       = !Null
	
;-----------------------------------------------------
; Integrate Over Energy \\\\\\\\\\\\\\\\\\\\\\\\\\\\\\
;-----------------------------------------------------
	;Interpolate spacecraft potential
	Vsc_fpi = Interpol( Temporary(Vsc), Temporary(t_edp), t_fpi )

	;Energy steps
	;   - dE/E is just the derivative of a natural log
	;   - Change bases: log_a(B) = log_x(B) / log_x(A), where x=10, A=exp, B=E
	;   - dE/E = const = energy separation in log space
	dLogE = alog10(energy[*,1]) - alog10(energy[*,0])
	dE    = energy * rebin(dLogE, nTime, nEnergy) / alog10( exp(1) )

	;Convert energy to velocity
	me   = MrConstants('m_e')
	q    = MrConstants('q')
	eV2J = MrConstants('eV2J')
	v    = Sqrt( 2 * eV2J * energy / me )
	vpot = Sqrt( 2 * q * Vsc_fpi / me )
	dV   = eV2J * dE / (v * me)

	;Resize arrays
	vpot    = Rebin( vpot,    nTime, nEnergy )
	Vsc_fpi = Rebin( Vsc_fpi, nTime, nEnergy )

	;Integrate with and without the spacecraft potential correction
	;   - Convert from m^-3 to cm^-3
	n_dist           = Total( fdist * v^2 * dv, 2 ) * 1e6
	n_dist_pot       = Total( fdist * v * Sqrt(v^2 - vpot^2) * dv, 2 ) * 1e6
	n_dist_photo_pot = Total( fdist_photo * v * Sqrt(v^2 - vpot^2) * dv, 2 ) * 1e6
	
	;Clear variables
	Vsc_fpi = !Null
	dLogE   = !Null
	dE      = !Null
	v       = !Null
	vpot    = !Null
	dV      = !Null

;-----------------------------------------------------
; Plot Results \\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\
;-----------------------------------------------------
	tr_ssm = MrVar_GetTRange('SSM')
	
	;Plot the results
	p1 = MrPlot( t_fpi, n, $
	             COLOR       = 'Black', $
	             NAME        = 'des-moms', $
	             TITLE       = 'DES Number Density', $
	             YRANGE      = [0, max(n)*1.1], $
	             YTITLE      = 'N!C(cm^-3)', $
	             XRANGE      = tr_ssm, $
	             XTICKFORMAT = 'time_labels', $
	             XTITLE      = 'Time ' + StrMid(trange[0], 0, 10) )
	
	p2 = MrPlot( t_fpi, n_dist, $
	             COLOR    = 'Blue', $
	             NAME     = 'des-dist', $
	             OVERPLOT = p1, $
	             YTITLE   = 'N!C(cm^-3)', $
	             XTITLE   = 'Seconds Since ' + StrMid(trange[0], 0, 10) )
	
	p3 = MrPlot( t_fpi, n_dist_pot, $
	             COLOR    = 'Forest Green', $
	             NAME     = 'scpot', $
	             OVERPLOT = p1, $
	             YTITLE   = 'N!C(cm^-3)', $
	             XTITLE   = 'Seconds Since ' + StrMid(trange[0], 0, 10) )
	
	p4 = MrPlot( t_fpi, n_dist_photo_pot, $
	             COLOR    = 'Red', $
	             NAME     = 'scpot&photo', $
	             OVERPLOT = p1, $
	             YTITLE   = 'N!C(cm^-3)', $
	             XTITLE   = 'Seconds Since ' + StrMid(trange[0], 0, 10) )
	
	;Legend
	l1 = MrLegend( /AUTO_TEXT_COLOR, $
	               HORIZONTAL_ALIGNMENT = 'RIGHT', $
	               POSITION             = [1.0, 1.0], $
	               /RELATIVE, $
	               SAMPLE_WIDTH         = 0.0, $
	               TARGET               = [p1, p2, p3, p4], $
	               VERTICAL_ALIGNMENT   = 'TOP' )

	p1 -> Order, /BRING_FORWARD
	p1 -> Refresh
	return, p1.window
end