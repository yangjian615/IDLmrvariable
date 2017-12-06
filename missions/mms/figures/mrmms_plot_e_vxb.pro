; docformat = 'rst'
;
; NAME:
;       MrMMS_Plot_E_VxB
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
;   Plot the parallel and perpendicular components of the electric field.
;       1. Bx, By, Bz, |B|
;       2. Ex: VexB, VixB, Eperp
;       3. Ey: VexB, VixB, Eperp
;       4. Ez: VexB, VixB, Eperp
;       5. Epar
;       6. E'x, E'y, E'z
;       7. Jx, Jy, Jz
;       8. J.E'
;
; :Categories:
;   MMS, EDI, MrVariable
;
; :Params:
;       SC:         in, required, type=string
;                   MMS spacecraft ID. Options are {'mms1' | 'mms2' | 'mms3' | 'mms4'}
;       MODE:       in, required, type=string
;                   Data rate mode. Options are {'srvy' | 'brst'}
;
; :Keywords:
;       COORDS:     in, optional, type=string, default='gse'
;                   Coordinate system of the data.
;       NO_LOAD:    in, optional, type=boolean, default=0
;                   If set, data will not be loaded from source CDF files.
;       OUTPUT_DIR: in, optional, type=string, default=pwd
;                   A directory in which to save the figure. If neither `OUTPUT_DIR`
;                       nor `OUTPUT_EXT` are defined, no file is generated.
;       OUTPUT_EXT: in, optional, type=string, default=pwd
;                   File extensions for the output figure. Options include: 'eps', 'gif',
;                       'jpg', 'ps', 'pdf', 'png', 'tiff'. If neither `OUTPUT_DIR` nor
;                       `OUTPUT_EXT` are defined, no file is generated.
;       TRANGE:     in, optional, type=string/strarr(2), default=MrVar_GetTRange()
;                   The start and end times of the data interval to be plotted, formatted
;                       as 'YYYY-MM-DDThh:mm:ss'
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
;       2016/06/28  -   Written by Matthew Argall
;       2017/01/18  -   Added E', J, and J.E' to the plot. - MRA
;-
function MrMMS_Plot_E_VxB, sc, mode, $
COORDS=coords, $
NO_LOAD=no_load, $
OUTPUT_DIR=output_dir, $
OUTPUT_EXT=output_ext, $
TRANGE=trange
	compile_opt idl2
	
	catch, the_error
	if the_error ne 0 then begin
		catch, /CANCEL
		if n_elements(win) gt 0 then obj_destroy, win
		MrPrintF, 'LogErr'
		return, !Null
	endif
	
	IF N_Elements(coords) EQ 0 THEN coords = 'gse'
	IF N_Elements(trange) GT 0 THEN MrVar_SetTRange, trange
	tf_load = ~Keyword_Set(no_load)
	
;-------------------------------------------
; Get Data /////////////////////////////////
;-------------------------------------------
	level = 'l2'
	
	fgm_coords = coords EQ 'dsl' || coords EQ 'dbcs' ? 'dmpa' : coords
	fgm_mode   = mode
	
	edp_coords = coords EQ 'dbcs' || coords EQ 'dmpa' ? 'dsl' : coords
	edp_mode   = 'fast'
	
	fpi_coords = coords EQ 'dsl' || coords EQ 'dmpa' ? 'dbcs' : coords
	fpi_mode   = mode EQ 'srvy' ? 'fast' : mode
	
	if tf_load then begin
		;FGM
		MrMMS_FGM_Load_Data, sc, fgm_mode, $
		                     VARFORMAT = '*b_'+fgm_coords+'*'
		
		;EDP E-Field
		MrMMS_Load_Data, sc, 'edp', edp_mode, level, $
		                 OPTDESC   = 'dce', $
		                 VARFORMAT = '*_dce_'+edp_coords+'_*'
	
		;DES
		MrMMS_FPI_Load_Data, sc, fpi_mode, $
;		                     LEVEL     = 'l1b', $
		                     OPTDESC   = 'dis-moms', $
		                     VARFORMAT = ['*numberdensity_', '*bulk?_'+fpi_coords+'_']+fpi_mode
	
		;DIS
		MrMMS_FPI_Load_Data, sc, fpi_mode, $
;		                     LEVEL     = 'l1b', $
		                     OPTDESC   = 'des-moms', $
		                     VARFORMAT = ['*numberdensity_', '*bulk?_'+fpi_coords+'_']+fpi_mode
	endif

;-------------------------------------------
; Variable Names ///////////////////////////
;-------------------------------------------
	;Source names
	ne_vname   = StrJoin( [sc, 'des', 'numberdensity',             fpi_mode], '_' )
	ve_vname   = StrJoin( [sc, 'des', 'bulkv',         fpi_coords, fpi_mode], '_' )
	ni_vname   = StrJoin( [sc, 'dis', 'numberdensity',             fpi_mode], '_' )
	vi_vname   = StrJoin( [sc, 'dis', 'bulkv',         fpi_coords, fpi_mode], '_' )
	b_vname    = StrJoin( [sc, 'fgm', 'b',    fgm_coords, fgm_mode, level], '_' )
	bvec_vname = StrJoin( [sc, 'fgm', 'bvec', fgm_coords, fgm_mode, level], '_' )
	e_vname    = StrJoin( [sc, 'edp', 'dce',  edp_coords, edp_mode, level], '_' )
	
	Ex_des_vname  = StrJoin( [sc, 'des', 'ex',             fpi_coords, fpi_mode, level], '_' )
	Ey_des_vname  = StrJoin( [sc, 'des', 'ey',             fpi_coords, fpi_mode, level], '_' )
	Ez_des_vname  = StrJoin( [sc, 'des', 'ez',             fpi_coords, fpi_mode, level], '_' )
	Ex_dis_vname  = StrJoin( [sc, 'dis', 'ex',             fpi_coords, fpi_mode, level], '_' )
	Ey_dis_vname  = StrJoin( [sc, 'dis', 'ey',             fpi_coords, fpi_mode, level], '_' )
	Ez_dis_vname  = StrJoin( [sc, 'dis', 'ez',             fpi_coords, fpi_mode, level], '_' )
	j_vname       = StrJoin( [sc, 'dis', 'currentdensity', fpi_coords, fpi_mode, level], '_' )
	Ex_perp_vname = StrJoin( [sc, 'edp', 'ex', 'perp',  edp_mode, level], '_' )
	Ey_perp_vname = StrJoin( [sc, 'edp', 'ey', 'perp',  edp_mode, level], '_' )
	Ez_perp_vname = StrJoin( [sc, 'edp', 'ez', 'perp',  edp_mode, level], '_' )
	E_par_vname   = StrJoin( [sc, 'edp', 'e',  'par',   edp_mode, level], '_' )
	E_prime_vname = StrJoin( [sc, 'edp', 'e',  'prime', edp_mode, level], '_' )
	jdote_vname   = StrJoin( [sc, 'edp', 'jdoteprime',  edp_mode, level], '_' )

;-------------------------------------------
; Ve,i x B /////////////////////////////////
;-------------------------------------------

	;Get the data
	oVe = MrVar_Get(ve_vname)
	oVi = MrVar_Get(vi_vname)
	oB  = MrVar_Get(bvec_vname)
	oE  = MrVar_Get(e_vname)
	
	;Interpolate to DES
	oVi_des = oVi -> Interpol(oVe)
	oB_des  = oB  -> Interpol(oVe)
	
	;Compute the convective electric field
	;   - Convert km/s * nT to mV/m
	oVexB = -1e-3 * oVe     -> Cross(oB_des)
	oVixB = -1e-3 * oVi_des -> Cross(oB_des)
	
	;Set properties
	oVexB['DEPEND_0'] = oVe['DEPEND_0']
	oVexB['UNITS']    = 'mV/m'
	oVexB -> Cache
	
	oVixB['DEPEND_0'] = oVe['DEPEND_0']
	oVixB['UNITS']    = 'mV/m'
	oVixB -> Cache
	
	;Split into components
	oVexB -> Split, oEx_des, oEy_des, oEz_des, NAMES=[Ex_des_vname, Ey_des_vname, Ez_des_vname], /CACHE
	oVixB -> Split, oEx_dis, oEy_dis, oEz_dis, NAMES=[Ex_dis_vname, Ey_dis_vname, Ez_dis_vname], /CACHE

	;Clear data
	obj_destroy, oVixB

;-------------------------------------------
; Current Density //////////////////////////
;-------------------------------------------
	oNe = MrVar_Get(ne_vname)

	;Compute current density
	;   - 1e15 converts to micro-A/m^3
	oJ = 1e15 * MrConstants('q') * oNe * (oVi_des - oVe)
	oJ -> SetName, j_vname
	oJ -> Cache
	
	;Clear data
	obj_destroy, oVi_des
	
;-------------------------------------------
; E Perp & Par /////////////////////////////
;-------------------------------------------

	;Interpolate B to E
	oE_des     = oE     -> Interpol(oVe)
	ob_hat_des = oB_des -> Normalize()

	;Parallel
	oEpar = oE_des -> Dot(ob_hat_des)

	;Perpendicular
	oEx_perp = oE_des[*,0] - oE_des[*,0] * ob_hat_des[*,0]
	oEy_perp = oE_des[*,1] - oE_des[*,1] * ob_hat_des[*,1]
	oEz_perp = oE_des[*,2] - oE_des[*,2] * ob_hat_des[*,2]
	oEperp = MrVectorTS( oE_des['TIMEVAR'], [ [ oEx_perp['DATA'] ], $
	                                          [ oEy_perp['DATA'] ], $
	                                          [ oEz_perp['DATA'] ] ] )

	;Set properties
	oEpar -> SetName, E_par_vname
	oEpar['DEPEND_0'] = oVe['DEPEND_0']
	oEpar['TITLE']    = 'Epar!C(mV/m)'
	oEpar['UNITS']    = 'mV/m'
	oEpar -> Cache

	oEperp['DEPEND_0'] = oVe['DEPEND_0']
	oEperp['UNITS']    = 'mV/m'
	
	;Split perp into components
	oEperp -> Split, oEx, oEy, oEz, NAMES=[Ex_perp_vname, Ey_perp_vname, Ez_perp_vname], /CACHE
	
	;Clear data
	obj_destroy, [oB_des, ob_hat_des, oEperp, oEx_perp, oEy_perp, oEz_perp]
	
;-------------------------------------------
; J.Eprime /////////////////////////////////
;-------------------------------------------
	;Electron rest frame
	oEprime = oE_des + oVexB
	oEprime -> SetName, E_prime_vname
	oEprime -> Cache
	
	;Dissipation
	oJdotEprime = oJ -> Dot(oEprime)
	oJdotEprime -> SetName, jdote_vname
	oJdotEprime -> Cache
	
	;Clear data
	obj_destroy, [oE_des, oVexB]

;-------------------------------------------
; Attributes ///////////////////////////////
;-------------------------------------------
	;B
	oB = MrVar_Get(b_vname)
	oB['PLOT_TITLE'] = strupcase(sc)
	oB['TITLE']      = 'B!C(nT)'
	
	;Ex DES
	oEx_des['LABEL'] = 'DES'
	oEx_des['COLOR'] = 'Red'
	oEx_des['TITLE'] = 'Ex!C(mV/m)'
	
	;Ez DES
	oEy_des['LABEL'] = 'DES'
	oEy_des['COLOR'] = 'Red'
	oEy_des['TITLE'] = 'Ey!C(mV/m)'
	
	;Ez DES
	oEz_des['LABEL'] = 'DES'
	oEz_des['COLOR'] = 'Red'
	oEz_des['TITLE'] = 'Ez!C(mV/m)'
	
	;Ex DIS
	oEx_dis['LABEL'] = 'DIS'
	oEx_dis['COLOR'] = 'Blue'
	oEx_dis['TITLE'] = 'Ex!C(mV/m)'
	
	;Ex DIS
	oEy_dis['LABEL'] = 'DIS'
	oEy_dis['COLOR'] = 'Blue'
	oEy_dis['TITLE'] = 'Ey!C(mV/m)'
		
	;Ez DIS
	oEz_dis['LABEL'] = 'DIS'
	oEz_dis['COLOR'] = 'Blue'
	oEz_dis['TITLE'] = 'Ez!C(mV/m)'
	
	;CURRENT DENSITY
	oJ['LABEL'] = ['Jx', 'Jy', 'Jz']
	oJ['TITLE'] = 'J!C($\mu$A/m^2)'
	oJ['UNITS'] = '\muA/m^2'
	
	;EperpX EDP
	oEx['LABEL'] = 'EDP'
	oEx['TITLE'] = 'Ex_perp!C(mV/m)'
	
	;EperpY EDP
	oEy['LABEL'] = 'EDP'
	oEy['TITLE'] = 'Ey_perp!C(mV/m)'
	
	;EperpZ EDP
	oEz['LABEL'] = 'EDP'
	oEz['TITLE'] = 'Ez_perp!C(mV/m)'
	
	;EPRIME
	oEprime['TITLE'] = "E'!C(mV/m)"
	oEprime['LABEL'] = ["E'x", "E'y", "E'z"]
	
	;J.EPRIME
	oJdotEprime['TITLE'] = "J.E'!C(nW/m^3)"

;-------------------------------------------
; Plot Data ////////////////////////////////
;-------------------------------------------
	;Plot variables
	win = MrVar_PlotTS( [B_vname, Ex_perp_vname, Ey_perp_vname, Ez_perp_vname, E_par_vname, $
	                     E_prime_vname, j_vname, jdote_vname], $
	                    /NO_REFRESH, $
	                    YSIZE=750 )
	win -> Refresh, /DISABLE
	
	;Overplot
	win = MrVar_OPlotTS( [Ex_perp_vname, Ey_perp_vname, Ez_perp_vname], $
	                     [Ex_des_vname,  Ey_des_vname,  Ez_des_vname] )
	win = MrVar_OPlotTS( [Ex_perp_vname, Ey_perp_vname, Ez_perp_vname], $
	                     [Ex_dis_vname,  Ey_dis_vname,  Ez_dis_vname] )
	
	win[0] -> SetLayout, [1,1]
	win    -> TrimLayout
	win.OXMargin = [10,10]
	win -> refresh

;-------------------------------------------
; Save Figure //////////////////////////////
;-------------------------------------------
	IF N_Elements(output_dir) GT 0 || N_Elements(output_ext) GT 0 THEN BEGIN
		;Defaults
		IF N_Elements(output_dir) EQ 0 THEN BEGIN
			CD, CURRENT=output_dir
		ENDIF ELSE IF ~File_Test(output_dir, /DIRECTORY) THEN BEGIN
			MrPrintF, 'LogText', 'Creating directory: "' + output_dir + '".'
			File_MKDir, output_dir
		ENDIF
		
		;File name
		fname   = StrJoin( [sc, 'efield', mode, level, 'frozen-in-Vix5'], '_' )
		fname   = FilePath( fname, ROOT_DIR=output_dir )
		
		;Save the figure
		fout = MrVar_PlotTS_Save( win, fname, output_ext )
	ENDIF

;-------------------------------------------
; Done /////////////////////////////////////
;-------------------------------------------
	RETURN, win
END