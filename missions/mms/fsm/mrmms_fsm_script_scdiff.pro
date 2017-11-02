; docformat = 'rst'
;
; NAME:
;       mms_fsm_script_scdiff
;
;*****************************************************************************************
;   Copyright (c) 2017, Matthew Argall                                                   ;
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
;   Plot the difference of FSM components between the various spacecraft.
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
;       2017/10/02  -   Written by Matthew Argall
;-
;*****************************************************************************************

;Set time range and LMN system
tf_load = 0B
;MrVar_SetTRange, '2017-07-26T' + ['07:03:30', '07:07:00']
MrVar_SetTRange, '2017-07-11T' + ['22:33:23', '22:34:30']
sc      = 'mms'+['1', '2', '3', '4']
mode    = 'brst'
level   = 'l2'
coords  = 'omb'
nsum    = 1024
;output_dir = '/home/argall/figures/20151016/'

scm_coords = 'gse'
fgm_coords = coords EQ 'omb' ? 'bcs' : coords

;Load data
IF tf_load THEN BEGIN
	!MrMMS.offline = 1B
	!MrMMS.dropbox_root = '/nfs/fsm/temp/'
	MrMMS_Load_Data, sc, 'fsm', mode, 'l3', OPTDESC='8khz-cs-test', VARFORMAT='*b_'+coords+'_'+mode+'_*'
	!MrMMS.offline = 0B
	
	MrMMS_FGM_Load_Data, sc, mode, VARFORMAT='*_b_'+fgm_coords+'_*'
	MrMMS_Load_Data, sc, 'scm', mode, 'l2', OPTDESC='scb', VARFORMAT='*_acb_'+scm_coords+'_*_'+level
ENDIF

;-------------------------------------------
; Variable Names ///////////////////////////
;-------------------------------------------

;Data
b_fsm_vnames = sc + '_' + StrJoin( ['fsm', 'b',    coords,            mode, 'l3'], '_' )
b_fgm_vnames = sc + '_' + StrJoin( ['fgm', 'bvec', fgm_coords,        mode, 'l2'], '_' )
b_scm_vnames = sc + '_' + StrJoin( ['scm', 'acb',  scm_coords, 'scb', mode, 'l2'], '_' )

;Derived
ndiff = ['12', '13', '14', '23', '24', '34']
db_fsm_vnames = 'mms' + ndiff + '_' + StrJoin( ['fsm', 'b',    coords,            mode, 'l3'], '_' )
db_fgm_vnames = 'mms' + ndiff + '_' + StrJoin( ['fgm', 'bvec', fgm_coords,        mode, 'l2'], '_' )
db_scm_vnames = 'mms' + ndiff + '_' + StrJoin( ['scm', 'acb',  scm_coords, 'scb', mode, 'l2'], '_' )

;-------------------------------------------
; FGM Differences //////////////////////////
;-------------------------------------------
oB1_fgm = MrVar_Get(b_fgm_vnames[0])
oB2_fgm = MrVar_Get(b_fgm_vnames[1])
oB3_fgm = MrVar_Get(b_fgm_vnames[2])
oB4_fgm = MrVar_Get(b_fgm_vnames[3])

oB2_fgm = oB2_fgm -> Interpol(oB1_fgm)
oB3_fgm = oB3_fgm -> Interpol(oB1_fgm)
oB4_fgm = oB4_fgm -> Interpol(oB1_fgm)

;Rotate to OMB
IF coords EQ 'omb' THEN BEGIN
	omb2smpa = mms_fg_xomb2smpa()
	bcs2smpa = mms_fg_xbcs2smpa( [0,0,1] )
	bcs2omb  = Transpose(omb2smpa) ## bcs2smpa
	bcs2omb  = Rebin( Reform(bcs2omb, 1, 3, 3), 2, 3, 3 )
	oBCS2OMB = MrMatrixTS( MrVar_GetTRange(), bcs2omb )
	oBCS2OMB = oBCS2OMB -> Interpol(oB1_fgm)
	
	oB1_fgm = oBCS2OMB # oB1_fgm
	oB2_fgm = oBCS2OMB # oB2_fgm
	oB3_fgm = oBCS2OMB # oB3_fgm
	oB4_fgm = oBCS2OMB # oB4_fgm
ENDIF

;Signal differences
oB12_fgm = oB2_fgm - oB1_fgm
oB13_fgm = oB3_fgm - oB1_fgm
oB14_fgm = oB4_fgm - oB1_fgm
oB23_fgm = oB3_fgm - oB2_fgm
oB24_fgm = oB4_fgm - oB2_fgm
oB34_fgm = oB4_fgm - oB3_fgm

oB12_fgm -> SetName, db_fgm_vnames[0]
oB13_fgm -> SetName, db_fgm_vnames[1]
oB14_fgm -> SetName, db_fgm_vnames[2]
oB23_fgm -> SetName, db_fgm_vnames[3]
oB24_fgm -> SetName, db_fgm_vnames[4]
oB34_fgm -> SetName, db_fgm_vnames[5]

oB12_fgm -> Cache
oB13_fgm -> Cache
oB14_fgm -> Cache
oB23_fgm -> Cache
oB24_fgm -> Cache
oB34_fgm -> Cache

oB12_fgm['PLOT_TITLE'] = 'FGM'
oB12_fgm['TITLE']      = 'dB12!C(nT)'
oB13_fgm['TITLE']      = 'dB13!C(nT)'
oB14_fgm['TITLE']      = 'dB14!C(nT)'
oB23_fgm['TITLE']      = 'dB23!C(nT)'
oB24_fgm['TITLE']      = 'dB24!C(nT)'
oB34_fgm['TITLE']      = 'dB34!C(nT)'

;-------------------------------------------
; FSM Differences //////////////////////////
;-------------------------------------------

oB1_fsm = MrVar_Get(b_fsm_vnames[0])
oB2_fsm = MrVar_Get(b_fsm_vnames[1])
oB3_fsm = MrVar_Get(b_fsm_vnames[2])
oB4_fsm = MrVar_Get(b_fsm_vnames[3])

oB2_fsm = oB2_fsm -> Interpol(oB1_fsm)
oB3_fsm = oB3_fsm -> Interpol(oB1_fsm)
oB4_fsm = oB4_fsm -> Interpol(oB1_fsm)

;Signal differences
oB12_fsm = oB2_fsm - oB1_fsm
oB13_fsm = oB3_fsm - oB1_fsm
oB14_fsm = oB4_fsm - oB1_fsm
oB23_fsm = oB3_fsm - oB2_fsm
oB24_fsm = oB4_fsm - oB2_fsm
oB34_fsm = oB4_fsm - oB3_fsm

oB12_fsm -> SetName, db_fsm_vnames[0]
oB13_fsm -> SetName, db_fsm_vnames[1]
oB14_fsm -> SetName, db_fsm_vnames[2]
oB23_fsm -> SetName, db_fsm_vnames[3]
oB24_fsm -> SetName, db_fsm_vnames[4]
oB34_fsm -> SetName, db_fsm_vnames[5]

oB12_fsm -> Cache
oB13_fsm -> Cache
oB14_fsm -> Cache
oB23_fsm -> Cache
oB24_fsm -> Cache
oB34_fsm -> Cache

oB12_fsm['PLOT_TITLE'] = 'FSM'
oB12_fsm['TICKFORMAT'] = '(a1)'
oB13_fsm['TICKFORMAT'] = '(a1)'
oB14_fsm['TICKFORMAT'] = '(a1)'
oB23_fsm['TICKFORMAT'] = '(a1)'
oB24_fsm['TICKFORMAT'] = '(a1)'
oB34_fsm['TICKFORMAT'] = '(a1)'
oB12_fsm['NSUM']       = nsum
oB13_fsm['NSUM']       = nsum
oB14_fsm['NSUM']       = nsum
oB23_fsm['NSUM']       = nsum
oB24_fsm['NSUM']       = nsum
oB34_fsm['NSUM']       = nsum

;-------------------------------------------
; SCM Differences //////////////////////////
;-------------------------------------------
oB1_scm = MrVar_Get(b_scm_vnames[0])
oB2_scm = MrVar_Get(b_scm_vnames[1])
oB3_scm = MrVar_Get(b_scm_vnames[2])
oB4_scm = MrVar_Get(b_scm_vnames[3])

oB2_scm = oB2_scm -> Interpol(oB1_scm)
oB3_scm = oB3_scm -> Interpol(oB1_scm)
oB4_scm = oB4_scm -> Interpol(oB1_scm)

;Signal differences
oB12_scm = oB2_scm - oB1_scm
oB13_scm = oB3_scm - oB1_scm
oB14_scm = oB4_scm - oB1_scm
oB23_scm = oB3_scm - oB2_scm
oB24_scm = oB4_scm - oB2_scm
oB34_scm = oB4_scm - oB3_scm

oB12_scm -> SetName, db_scm_vnames[0]
oB13_scm -> SetName, db_scm_vnames[1]
oB14_scm -> SetName, db_scm_vnames[2]
oB23_scm -> SetName, db_scm_vnames[3]
oB24_scm -> SetName, db_scm_vnames[4]
oB34_scm -> SetName, db_scm_vnames[5]

oB12_scm -> Cache
oB13_scm -> Cache
oB14_scm -> Cache
oB23_scm -> Cache
oB24_scm -> Cache
oB34_scm -> Cache

oB12_scm['PLOT_TITLE'] = 'SCM'
oB12_scm['TICKFORMAT'] = '(a1)'
oB13_scm['TICKFORMAT'] = '(a1)'
oB14_scm['TICKFORMAT'] = '(a1)'
oB23_scm['TICKFORMAT'] = '(a1)'
oB24_scm['TICKFORMAT'] = '(a1)'
oB34_scm['TICKFORMAT'] = '(a1)'
oB12_scm['NSUM']       = nsum
oB13_scm['NSUM']       = nsum
oB14_scm['NSUM']       = nsum
oB23_scm['NSUM']       = nsum
oB24_scm['NSUM']       = nsum
oB34_scm['NSUM']       = nsum

;-------------------------------------------
; Plot /////////////////////////////////////
;-------------------------------------------
trange      = MrVar_GetTRange('SSM')
xtickformat = trange[1] - trange[0] LE 60 ? '' : 'time_labels'

vnames = [ [db_fgm_vnames], [db_fsm_vnames], [db_scm_vnames] ]
vnames = Reform( Transpose(vnames), N_Elements(vnames) )
win = MrVar_PlotTS( vnames, $
                    LAYOUT = [3, N_Elements(db_fsm_vnames)], $
                    /NO_REFRESH, $
                    XSIZE  = 800, $
                    YSIZE  = 650 )
win[0] -> SetLayout, [1,1]
win.xgap = 0.5
win[15].xtickformat = xtickformat
win[16].xtickformat = xtickformat
win[17].xtickformat = xtickformat
;win -> SetGlobal, XRANGE=hms_to_ssm(['07:03:30', '07:07:00']), XTICKINTERVAL=90, XMINOR=6, YRANGE=[-1.0, 1.0]
win -> TrimLayout
win -> Refresh

;-------------------------------------------
; Save Figure //////////////////////////////
;-------------------------------------------
IF N_Elements(output_dir) GT 0 || N_Elements(output_ext) GT 0 THEN BEGIN
	;Defaults
	IF N_Elements(output_dir) EQ 0 THEN BEGIN
		CD, CURRENT=output_dir
		MrPrintF, 'LogText', 'Saving file to: "' + output_dir + '".'
	ENDIF
	
	;File name
	fname = StrJoin( [sc, 'fsm', mode, 'l3', '8kHz-scdiff-'+coords], '_' )
	fname = FilePath( fname, ROOT_DIR=output_dir )
	
	;Save the figure
	fout = MrVar_PlotTS_Save( win, fname, output_ext )
ENDIF
END