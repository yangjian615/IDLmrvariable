; docformat = 'rst'
;
; NAME:
;    MrMMS_FPI_Test_Moments
;
; PURPOSE:
;+
;   Compare UNH-made gyrophase and pitch angle distributions and energy spectrograms
;   with those of FPI.
;
; :Categories:
;    MMS, SPEDAS
;
; :Examples:
;   To use::
;       IDL> .r test_MrDist_1D
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
;       2016/02/10  -   Written by Matthew Argall
;-
FUNCTION MrMMS_FPI_Test_Moments, sc, mode, species, $
NO_LOAD=no_load
	Compile_Opt idl2
	
	Catch, the_error
	IF the_error NE 0 THEN BEGIN
		Catch, /CANCEL
		MrPrintF, 'LogErr'
		RETURN, Obj_New()
	ENDIF
	
;	MrVar_SetTRange, ['2015-10-16T08:00:00', '2015-10-16T14:00:00']
	MrVar_SetTRange, ['2016-01-07T09:34:30', '2016-01-07T09:38:00']

	sc      = 'mms2'
	mode    = 'brst'
	species = 'e'
	instr   = 'd' + species + 's'
	level   = 'l2'
	tf_load = ~Keyword_Set(no_load)

;-------------------------------------------
; Variable Names ///////////////////////////
;-------------------------------------------
	;Source names
	scpot_vname = StrJoin( [sc, 'edp', 'scpot', 'fast', level], '_' )
	n_vname     = StrJoin([sc, instr, 'numberdensity', mode], '_')
	v_vname     = StrJoin([sc, instr, 'bulkv',      'dbcs', mode], '_')
	p_vname     = StrJoin([sc, instr, 'prestensor', 'dbcs', mode], '_')
	t_vname     = StrJoin([sc, instr, 'temptensor', 'dbcs', mode], '_')
	q_vname     = StrJoin([sc, instr, 'heatq',      'dbcs', mode], '_')
	
	f_vname      = StrJoin( [sc, instr, 'dist',                  mode], '_')
	dphi_vname   = StrJoin( [sc, instr, 'startdelphi', 'count',  mode], '_' )
	parity_vname = StrJoin( [sc, instr, 'steptable',   'parity', mode], '_' )
	
	ph_dphi_vname = StrJoin( ['mms', instr, 'startdelphi', 'counts', mode], '_' ) + '_model'
	ph_f0_vname   = StrJoin( ['mms', instr, 'bgdist',      'p0',     mode], '_' ) + '_model'
	ph_f1_vname   = StrJoin( ['mms', instr, 'bgdist',      'p1',     mode], '_' ) + '_model'
	ph_e0_vname   = StrJoin( ['mms', instr, 'energy0',               mode], '_' ) + '_model'
	ph_e1_vname   = StrJoin( ['mms', instr, 'energy1',               mode], '_' ) + '_model'
	ph_scl_vname  = StrJoin( [sc,    instr, 'scl',         'model',  mode], '_' ) + '_model'
	
	;Derived names
	fph_vname   = StrJoin([sc, instr, 'dist', 'photo', mode], '_')
	ncorr_vname = StrJoin([sc, instr, 'numberdensity', 'corr', mode], '_')
	vcorr_vname = StrJoin([sc, instr, 'bulkv',         'corr', mode], '_')
	pcorr_vname = StrJoin([sc, instr, 'pres',          'corr', mode], '_')
	qcorr_vname = StrJoin([sc, instr, 'heatflux',      'corr', mode], '_')
	dist_vname  = StrJoin([sc, instr, 'dist4d',        mode], '_')
	
	vx_vname    = v_vname     + '_x'
	vy_vname    = v_vname     + '_y'
	vz_vname    = v_vname     + '_z'
	pxx_vname   = p_vname     + '_xx'
	pxy_vname   = p_vname     + '_xy'
	pxz_vname   = p_vname     + '_xz'
	pyy_vname   = p_vname     + '_yy'
	pyz_vname   = p_vname     + '_yz'
	pzz_vname   = p_vname     + '_zz'
	vc_x_vname  = vcorr_vname + '_x'
	vc_y_vname  = vcorr_vname + '_y'
	vc_z_vname  = vcorr_vname + '_z'
	pc_xx_vname = pcorr_vname + '_xx'
	pc_xy_vname = pcorr_vname + '_xy'
	pc_xz_vname = pcorr_vname + '_xz'
	pc_yy_vname = pcorr_vname + '_yy'
	pc_yz_vname = pcorr_vname + '_yz'
	pc_zz_vname = pcorr_vname + '_zz'


;-------------------------------------------
; Load Data ////////////////////////////////
;-------------------------------------------
	IF tf_load THEN BEGIN
		;Load FPI Distribution
		MrMMS_FPI_Load_Dist3D, sc, mode, species, fac, $
		                       COORD_SYS   = coord_sys, $
		                       LEVEL       = level, $
		                       ORIENTATION = orientation, $
		                       SUFFIX      = suffix, $
		                       TEAM_SITE   = team_site, $
		                       TRANGE      = trange, $
		                       VARNAMES    = varnames
		
		;Photoelectron model
		MrMMS_FPI_Load_Models, sc, mode, species, $
		                       SUFFIX = '_model'
		
		;Create f_photo
		ofPhoto = MrMMS_FPI_Dist_Photo( f_vname, dphi_vname, ph_scl_vname, ph_dphi_vname, $
		                                ph_f0_vname, ph_e0_vname, ph_f1_vname, ph_e1_vname, $
		                                parity_vname, $
		                                /CACHE, $
		                                NAME = fph_vname )
		
		;Load FPI Moments
		MrMMS_FPI_Load_Data, sc, mode, $
		                     OPTDESC   = instr + '-moms', $
		                     TEAM_SITE = team_site, $
		                     VARFORMAT = ['*numberdensity*', '*bulkv_dbcs*', $
		                                  '*pres*dbcs*', '*temp*dbcs*', '*heat*']
		
		;Spacecraft potential
		MrMMS_Load_Data, sc, 'edp', 'fast', 'l2', $
		                 OPTDESC  = 'scpot', $
		                 VARNAMES = '*scpot*'
	ENDIF

;-------------------------------------------
; Photo Electron Distribution //////////////
;-------------------------------------------
	
	;Subtract from the distribution function
	ofPhoto = MrVar_Get(fph_vname)
	ofDist  = MrVar_Get(f_vname)
	ofcorr  = ofDist - ofPhoto > 0
	ofDist -> CopyAttrTo, ofcorr

;-------------------------------------------
; Compute Moments //////////////////////////
;-------------------------------------------
	;Distribution function
	oDist  = MrDist4D(ofcorr, VSC=scpot_vname, SPECIES=species)
	oDist -> Moments, /CACHE, $
	                  DENSITY  = oNcorr, $
	                  VELOCITY = oVcorr, $
	                  PRESSURE = oPcorr
	
	;Rename
	oNcorr -> SetName, ncorr_vname
	oVcorr -> SetName, vcorr_vname
	oPcorr -> SetName, pcorr_vname
	
	
;	oNcorr = oDist -> Density(NAME=ncorr_vname, /CACHE)
;	oVcorr = oDist -> Velocity(NAME=vcorr_vname, /CACHE)
;	oPcorr = oDist -> Pressure(NAME=pcorr_vname, /CACHE)
;	oQcorr = oDist -> HeatFlux(NAME=qcorr_vname, /CACHE)

;-------------------------------------------
; Split Into Components ////////////////////
;-------------------------------------------
	;FPI Velocity
	oV_fpi = MrVar_Get(v_vname)
	oV_fpi -> Split, oVx, oVy, oVz, /CACHE
	
	;Velocity
	oVcorr = MrVar_Get(vcorr_vname)
	oVcorr -> Split, oVc_x, oVc_y, oVc_z, /CACHE
	
	;FPI Pressure
	oP   = MrVar_Get(p_vname)
	oPxx = MrScalarTS(oP['TIMEVAR'], oP[*,0,0], NAME=oP.name+'_xx', /CACHE)
	oPxy = MrScalarTS(oP['TIMEVAR'], oP[*,0,1], NAME=oP.name+'_xy', /CACHE)
	oPxz = MrScalarTS(oP['TIMEVAR'], oP[*,0,2], NAME=oP.name+'_xz', /CACHE)
	oPyy = MrScalarTS(oP['TIMEVAR'], oP[*,1,1], NAME=oP.name+'_yy', /CACHE)
	oPyz = MrScalarTS(oP['TIMEVAR'], oP[*,1,2], NAME=oP.name+'_yz', /CACHE)
	oPzz = MrScalarTS(oP['TIMEVAR'], oP[*,2,2], NAME=oP.name+'_zz', /CACHE)
	
	;Pressure
	oPcorr = MrVar_Get(pcorr_vname)
	oPc_xx = MrScalarTS(oPcorr['TIMEVAR'], oPcorr[*,0,0], NAME=oPcorr.name+'_xx', /CACHE)
	oPc_xy = MrScalarTS(oPcorr['TIMEVAR'], oPcorr[*,0,1], NAME=oPcorr.name+'_xy', /CACHE)
	oPc_xz = MrScalarTS(oPcorr['TIMEVAR'], oPcorr[*,0,2], NAME=oPcorr.name+'_xz', /CACHE)
	oPc_yy = MrScalarTS(oPcorr['TIMEVAR'], oPcorr[*,1,1], NAME=oPcorr.name+'_yy', /CACHE)
	oPc_yz = MrScalarTS(oPcorr['TIMEVAR'], oPcorr[*,1,2], NAME=oPcorr.name+'_yz', /CACHE)
	oPc_zz = MrScalarTS(oPcorr['TIMEVAR'], oPcorr[*,2,2], NAME=oPcorr.name+'_zz', /CACHE)

;-------------------------------------------
; Attributes ///////////////////////////////
;-------------------------------------------
	;Density FPI
	oN_fpi = MrVar_Get(n_vname)
	oN_fpi['COLOR']  = 'Black'
	oN_fpi['LABEL']  = 'moms'
	
	;Density Corrected
	oNcorr = MrVar_Get(ncorr_vname)
	oNcorr['COLOR']     = 'Blue'
	oNcorr['LABEL']     = 'dist'
	
	;FPI Velocity
	oVx['LABEL'] = 'moms'
	oVx['TITLE'] = 'Vx!C(km/s)'
	
	oVy['LABEL'] = 'moms'
	oVy['TITLE'] = 'Vy!C(km/s)'
	
	oVz['LABEL'] = 'moms'
	oVz['TITLE'] = 'Vz!C(km/s)'
	
	;Velocity
	oVc_x['COLOR'] = 'Blue'
	oVc_x['LABEL'] = 'dist'
	oVc_x['TITLE'] = 'Vx!C(km/s)'
	
	oVc_y['COLOR'] = 'Blue'
	oVc_y['LABEL'] = 'dist'
	oVc_z['TITLE'] = 'Vx!C(km/s)'
	
	oVc_z['COLOR'] = 'Blue'
	oVc_z['LABEL'] = 'dist'
	oVc_z['TITLE'] = 'Vx!C(km/s)'
	
	;FPI Pressure
	oPxx['LABEL'] = 'moms'
	oPxx['TITLE'] = 'Pxx!C(nPa)'
	
	oPxy['LABEL'] = 'moms'
	oPxy['TITLE'] = 'Pxy!C(nPa)'
	
	oPxz['LABEL'] = 'moms'
	oPxz['TITLE'] = 'Pxz!C(nPa)'
	
	oPyy['LABEL'] = 'moms'
	oPyy['TITLE'] = 'Pyy!C(nPa)'
	
	oPyz['LABEL'] = 'moms'
	oPyz['TITLE'] = 'Pyz!C(nPa)'
	
	oPzz['LABEL'] = 'moms'
	oPzz['TITLE'] = 'Pzz!C(nPa)'
	
	
	;Pressure
	oPc_xx['COLOR'] = 'Blue'
	oPc_xx['LABEL'] = 'dist'
	oPc_xx['TITLE'] = 'Pxx!C(nPa)'
	
	oPc_xy['COLOR'] = 'Blue'
	oPc_xy['LABEL'] = 'dist'
	oPc_xy['TITLE'] = 'Pxy!C(nPa)'
	
	oPc_xz['COLOR'] = 'Blue'
	oPc_xz['LABEL'] = 'dist'
	oPc_xz['TITLE'] = 'Pxz!C(nPa)'
	
	oPc_yy['COLOR'] = 'Blue'
	oPc_yy['LABEL'] = 'dist'
	oPc_yy['TITLE'] = 'Pyy!C(nPa)'
	
	oPc_yz['COLOR'] = 'Blue'
	oPc_yz['LABEL'] = 'dist'
	oPc_yz['TITLE'] = 'Pyz!C(nPa)'
	
	oPc_zz['COLOR'] = 'Blue'
	oPc_zz['LABEL'] = 'dist'
	oPc_zz['TITLE'] = 'Pzz!C(nPa)'

;-------------------------------------------
; Plot /////////////////////////////////////
;-------------------------------------------
	win = MrVar_PlotTS(  [n_vname, vx_vname, vy_vname, vz_vname, $
	                      pxx_vname, pxy_vname, pxz_vname, pyy_vname, pyz_vname, pzz_vname], $
	                    /NO_REFRESH, $
	                    YSIZE = 650 )
	win = MrVar_OPlotTS( [n_vname, vx_vname, vy_vname, vz_vname, $
	                      pxx_vname, pxy_vname, pxz_vname, pyy_vname, pyz_vname, pzz_vname], $
	                     [ncorr_vname, vc_x_vname, vc_y_vname, vc_z_vname, $
	                      pc_xx_vname, pc_xy_vname, pc_xz_vname, pc_yy_vname, pc_yz_vname, pc_zz_vname] )
	win[0] -> SetLayout, [1,1]
	win -> TrimLayout
	win -> SetProperty, OXMARGIN=[12,8]

	win -> Refresh
	RETURN, win
END