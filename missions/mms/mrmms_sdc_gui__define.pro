; docformat = 'rst'
;
; NAME:
;       MrMMS_SDC_GUI__DEFINE
;
;*****************************************************************************************
;   Copyright (c) 2016, University of New Hampshire                                      ;
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
;       * Neither the name of the University of New Hampshire nor the names of its       ;
;         contributors may be used to endorse or promote products derived from this      ;
;         software without specific prior written permission.                            ;
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
;   Create a GUI for collecting information related to data requests to the MMS
;   Science Data Center.
;
; :Categories:
;       MMS
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
;-
;*****************************************************************************************
;+
;   The purpose of this method is to change directories when the CD button is pressed.
;
; :Private:
;
; :Params:
;       EVENT:          in, required, type=structure
;                       Event structure returned by XManager.
;-
pro MrMMS_SDC_GUI::CD, event
	compile_opt idl2

	;Catch errors
	catch, the_error
	if the_error ne 0 then begin
		catch, /cancel
		MrPrintF, 'LogErr'
		return
	endif

	;Open the directory selection GUI
	directory = dialog_pickfile(/DIRECTORY, DIALOG_PARENT=self.tlb, $
	                            PATH=self.directory, TITLE='Change Directories')

	;Set the new directory
	self.directory = directory

	;Update the directory text widget
	dirID = widget_info(self.tlb, FIND_BY_UNAME='DIRECTORY')
	widget_control, dirID, SET_VALUE=directory
end


;+
;   The purpose of this method is to create and realize a widget form. Inputs will be
;   used to construct the URL needed to download data.
;-
pro MrMMS_SDC_GUI::Create, $
GROUP_LEADER=group_leader
	compile_opt idl2

	;Catch errors
	catch, the_error
	if the_error ne 0 then begin
		catch, /cancel
		MrPrintF, 'LogErr'
		return
	endif

;---------------------------------------------------------------------
;Make the Top Level Base /////////////////////////////////////////////
;---------------------------------------------------------------------

	;Make a top-level base with or without a groupleader. Create_GUI can be called by
	;other widgets, so if a GROUP_LEADER is given, then make Create_GUI modal.
	if n_elements(group_leader) ne 0 then begin
		no_block = 0
		tlb = widget_base(GROUP_LEADER=group_leader, TITLE='Cluster File Downloader', /COLUMN, $
		                  XOFFSET=100, YOFFSET=0, UNAME='tlb', /BASE_ALIGN_CENTER, $
		                  /MODAL)
	endif else begin
		no_block = 1
		tlb = widget_base(TITLE='Cluster File Downloader', /COLUMN, XOFFSET=100, YOFFSET=0, $
		                  UNAME='tlb', /BASE_ALIGN_CENTER)
	endelse

	self.tlb = tlb

;---------------------------------------------------------------------
;Change Directories //////////////////////////////////////////////////
;---------------------------------------------------------------------

	base   = widget_base(tlb, ROW=1, /BASE_ALIGN_LEFT)
	button = widget_button(base, VALUE='CD', UNAME='CD', XSIZE=10*!d.x_ch_size, UVALUE={object: self, method: 'CD'})
	text   = widget_text(base, VALUE=self.directory, UNAME='DIRECTORY', XSIZE=80)

;---------------------------------------------------------------------
;Create Form /////////////////////////////////////////////////////////
;---------------------------------------------------------------------
	sc           = 'mms' + ['1', '2', '3', '4']
	label_width  = 20 * !d.x_ch_size
	form_width   = 30

	;Create the form
	base0 = widget_base(tlb, COLUMN=2)

	;Create a required fields column (left) and optional/status (right) column
	req_col  = widget_base(base0, COLUMN=1, /BASE_ALIGN_LEFT, FRAME=5)
	right_col = widget_base(base0, COLUMN=1)

;---------------------------------------------------------------------
;Required Fields Column //////////////////////////////////////////////
;---------------------------------------------------------------------
	label = widget_label(req_col, VALUE='Required Fields:', /ALIGN_CENTER, FRAME=3, XSIZE=50*!D.X_CH_Size)

	;START TIME
	wbase = widget_base(req_col, COLUMN=2)
	label = widget_label(wbase, VALUE='Start Date:', /ALIGN_RIGHT, XSIZE=label_width)
	text  = widget_text(wbase, UNAME='TSTART', VALUE=self.tstart, /EDITABLE, XSIZE=form_width)

	;END TIME
	wbase = widget_base(req_col, COLUMN=2)
	label = widget_label(wbase, VALUE='End Date:', /ALIGN_RIGHT, XSIZE=label_width)
	text  = widget_text(wbase, UNAME='TEND', VALUE=self.tend, /EDITABLE, XSIZE=form_width)

	;SPACECRAFT
	isSet = MrIsMember(strsplit(self.sc, ',', /EXTRACT), sc)
	wbase  = widget_base(req_col, COLUMN=2)
	label  = widget_label(wbase, VALUE='Spacecraft:', /ALIGN_RIGHT, XSIZE=label_width)
	bgroup = cw_bgroup(wbase, sc, COLUMN=2, /NONEXCLUSIVE, UNAME='SC', $
	                   XSIZE=form_width*!D.X_CH_Size, XOFFSET=10*!D.X_CH_Size, FRAME=1, $
	                   UVALUE={method: ''}, SET_VALUE=isSet)

	;INSTRUMENT
	wbase = widget_base(req_col, COLUMN=2)
	label = widget_label(wbase, VALUE='Instrument:', /ALIGN_RIGHT, XSIZE=label_width)
	text  = widget_text(wbase, UNAME='INSTR', VALUE=self.instr, /EDITABLE, XSIZE=form_width)

	;MODE
	wbase = widget_base(req_col, COLUMN=2)
	label = widget_label(wbase, VALUE='Mode:', /ALIGN_RIGHT, XSIZE=label_width)
	text  = widget_text(wbase, UNAME='MODE', VALUE=self.mode, /EDITABLE, XSIZE=form_width)

	;LEVEL
	wbase = widget_base(req_col, COLUMN=2)
	label = widget_label(wbase, VALUE='Level:', /ALIGN_RIGHT, XSIZE=label_width)
	text  = widget_text(wbase, UNAME='LEVEL', VALUE=self.level, /EDITABLE, XSIZE=form_width)

	;OPTDESC
	wbase = widget_base(req_col, COLUMN=2)
	label = widget_label(wbase, VALUE='Optdesc:', /ALIGN_RIGHT, XSIZE=label_width)
	text  = widget_text(wbase, UNAME='OPTDESC', VALUE=self.optdesc, /EDITABLE, XSIZE=form_width)

;---------------------------------------------------------------------
; Drop Lists /////////////////////////////////////////////////////////
;---------------------------------------------------------------------
	;The right column will have an droplist base (top) and a status base (bottom)
	opt_col = widget_base(right_col, COLUMN=1, FRAME=5)
	label   = widget_label(opt_col, VALUE='Helpers:', /ALIGN_CENTER, FRAME=3, XSIZE=40*!D.X_CH_Size)
	
	;Theh droplist base will contain a column for INSTR, MODE, LEVEL, OPTDESC
	dl_base = widget_base(opt_col, COLUMN=2)
	
	;Put headers at the top
	text  = widget_label(dl_base, VALUE='Instr:', /ALIGN_LEFT, XSIZE=label_width)
	wlist = widget_list(dl_base, UNAME='DL_INSTR', VALUE=*self.instruments, YSIZE=8, /MULTIPLE, $
	                         UVALUE={object: self, method: 'Select_DLItems'})
	
	mode = ['slow', 'fast', 'srvy', 'brst']
	text = widget_label(dl_base, VALUE='Mode:', /ALIGN_LEFT, XSIZE=label_width)
	wlist = widget_list(dl_base, UNAME='DL_MODE', VALUE=*self.modes, YSIZE=8, /MULTIPLE, $
	                         UVALUE={object: self, method: 'Select_DLItems'})
	
	level = ['l1a', 'l1b', 'ql', 'l2pre', 'l2', 'l2plus']
	text = widget_label(dl_base, VALUE='Level:', /ALIGN_LEFT, XSIZE=label_width)
	wlist = widget_list(dl_base, UNAME='DL_LEVEL', VALUE=*self.levels, YSIZE=8, /MULTIPLE, $
	                         UVALUE={object: self, method: 'Select_DLItems'})
	
	optdesc = ['']
	text = widget_label(dl_base, VALUE='Optdesc:', /ALIGN_LEFT, XSIZE=label_width)
	wlist = widget_list(dl_base, UNAME='DL_OPTDESC', VALUE=*self.optdescs, YSIZE=8, /MULTIPLE, $
	                         UVALUE={object: self, method: 'Select_DLItems'})

;---------------------------------------------------------------------
;Create OK and Cancel and "Show All" Buttons /////////////////////////
;---------------------------------------------------------------------
	okBase = widget_base(tlb, ROW=1)
	button = widget_button(okBase, /ALIGN_CENTER, UNAME='OK', VALUE='OK', $
	                       UVALUE={object: self, method:'Ok'}, XSIZE=10*!D.X_CH_Size)
	button = widget_button(okBase, /ALIGN_CENTER, UNAME='DONE', VALUE='Done', $
	                       UVALUE={object: self, method:'Done'}, XSIZE=10*!D.X_CH_Size)

;---------------------------------------------------------------------
;Create the State Variable, Realize, and Start Event Handling ////////
;---------------------------------------------------------------------

	;Realize the top-level base
	widget_control, tlb, /REALIZE
	
	;Call XMANAGER
	xmanager, 'MrMMS_SDC_GUI', tlb, cleanup='MrMMS_SDC_GUI_Cleanup', $
	          event_handler='MrMMS_SDC_GUI_Events', NO_BLOCK=no_block
end


;+
;   Handle events from the 'Done' button.
;
; :Private:
;
; :Params:
;       EVENT:          in, required, type=structure
;                       Event structure returned by XManager.
;-
pro MrMMS_SDC_GUI::Done, event
	compile_opt idl2

	;Catch errors
	catch, the_error
	if the_error ne 0 then begin
		catch, /cancel
		MrPrintF, 'LogErr'
		return
	endif

	;Destroy the widget and the object
	obj_destroy, self
end


;+
;   Get object properties.
;-
pro MrMMS_SDC_GUI::GetProperty, $
DIRECTORY = directory, $
INSTR = instr, $
MODE = mode, $
LEVEL = level, $
OPTDESC = optdesc, $
PRODUCTS = products, $
SC = sc, $
TSTART = tstart, $
TEND = tend, $
VERSION = version
	compile_opt idl2

	;Catch errors
	catch, the_error
	if the_error ne 0 then begin
		catch, /cancel
		MrPrintF, 'LogErr'
		return
	endif

	;Set properties
	if arg_present(directory) gt 0 then directory = self.directory
	if arg_present(tstart)    gt 0 then tstart    = self.tstart
	if arg_present(tend)      gt 0 then tend      = self.tend
	
	;Convert to string array
	if arg_present(sc)       gt 0 then sc       = strsplit(self.sc,       ',', /EXTRACT)
	if arg_present(instr)    gt 0 then instr    = strsplit(self.instr,    ',', /EXTRACT)
	if arg_present(mode)     gt 0 then mode     = strsplit(self.mode,     ',', /EXTRACT)
	if arg_present(level)    gt 0 then level    = strsplit(self.level,    ',', /EXTRACT)
	if arg_present(optdesc)  gt 0 then optdesc  = strsplit(self.optdesc,  ',', /EXTRACT)
	if arg_present(products) gt 0 then products = strsplit(self.products, ',', /EXTRACT)
	if arg_present(version)  gt 0 then version  = strsplit(self.version,  ',', /EXTRACT)
end


;+
;   Handle events from the 'OK' button.
;
; :Private:
;
; :Params:
;       EVENT:          in, required, type=structure
;                       Event structure returned by XManager.
;-
pro MrMMS_SDC_GUI::Ok, event
	compile_opt idl2

	;Catch errors
	catch, the_error
	if the_error ne 0 then begin
		catch, /cancel
		MrPrintF, 'LogErr'
		return
	endif

;---------------------------------------------------------------------
; Widget IDs and Values //////////////////////////////////////////////
;---------------------------------------------------------------------
	;Get the widget IDs of all of the widgets in the form
	scID     = widget_info(self.tlb, FIND_BY_UNAME='SC')
	instrID  = widget_info(self.tlb, FIND_BY_UNAME='INSTR')
	modeID   = widget_info(self.tlb, FIND_BY_UNAME='MODE')
	levelID  = widget_info(self.tlb, FIND_BY_UNAME='LEVEL')
	optID    = widget_info(self.tlb, FIND_BY_UNAME='OPTDESC')
	tstartID = widget_info(self.tlb, FIND_BY_UNAME='TSTART')
	tendID   = widget_info(self.tlb, FIND_BY_UNAME='TEND')
	dirID    = widget_info(self.tlb, FIND_BY_UNAME='DIRECTORY')

	;Get their values
	widget_control, scID,     GET_VALUE=sc
	widget_control, instrID,  GET_VALUE=instr
	widget_control, modeID,   GET_VALUE=mode
	widget_control, levelID,  GET_VALUE=level
	widget_control, optID,    GET_VALUE=optdesc
	widget_control, tstartID, GET_VALUE=tstart
	widget_control, tendID,   GET_VALUE=tend
	widget_control, dirID,    GET_VALUE=directory

;---------------------------------------------------------------------
; Translate Values to MMS Keys ///////////////////////////////////////
;---------------------------------------------------------------------

	;Which spacecraft were chosen?
	;   - {C1 | C2 | C3 | C4}
	spacecraft = ['mms1', 'mms2', 'mms3', 'mms4']
	iSC = where(sc eq 1, count)
	if count gt 0 $
		then sc = spacecraft[iSC] $
		else sc = ''

;---------------------------------------------------------------------
; Set Properties & Kill GUI //////////////////////////////////////////
;---------------------------------------------------------------------

	;Set the object properties
	self -> SetProperty, SC         = SC, $
	                     INSTR      = instr, $
	                     MODE       = mode, $
	                     LEVEL      = level, $
	                     OPTDESC    = optdesc, $
	                     TSTART     = tstart, $
	                     TEND       = tend, $
	                     VERSION    = version, $
	                     PRODUCTS   = products, $
	                     DIRECTORY  = directory
	
	;Destroy GUI
	widget_control, self.tlb, /DESTROY
end


;+
;   The purpose of this method is to transfer the selected dataset ID into the DATASET_ID
;   text field.
;
; :Private:
;
; :Params:
;       EVENT:          in, required, type=structure
;                       Event structure returned by XManager.
;-
pro MrMMS_SDC_GUI::Select_DLItems, event
	compile_opt idl2

	;Catch errors
	catch, the_error
	if the_error ne 0 then begin
		catch, /CANCEL
		MrPrintF, 'LogErr'
		return
	endif


	;UNAME and selected items of droplist
	dl_uname = widget_info(event.id, /UNAME)
	iSelect  = Widget_Info(Event.ID, /LIST_SELECT)
	
	;Get the UNAME of the corresponding text widget by removing 'DL_'
	txt_uname = strmid(dl_uname, 3)
	dataID    = widget_info(self.tlb, FIND_BY_UNAME=txt_uname)
	
	;Pick the list items
	case txt_uname of
		'MODE':    items = strjoin( (*self.modes)[iSelect],       ',')
		'LEVEL':   items = strjoin( (*self.levels)[iSelect],      ',')
		'INSTR':   items = strjoin( (*self.instruments)[iSelect], ',')
		'OPTDESC': items = strjoin( (*self.optdescs)[iSelect],    ',')
		else: message, 'UNAME not recognized: "' + txt_uname + '".'
	endcase
	
	;Put it in the text widget
	Widget_Control, dataID, SET_VALUE=items
end


;+
;   Set object properties
;-
pro MrMMS_SDC_GUI::SetProperty, $
DIRECTORY = directory, $
INSTR = instr, $
MODE = mode, $
LEVEL = level, $
OPTDESC = optdesc, $
PRODUCTS = products, $
SC = sc, $
TSTART = tstart, $
TEND = tend, $
TEAM = team, $
VERSION = version
	compile_opt idl2

	;Catch errors
	catch, the_error
	if the_error ne 0 then begin
		catch, /cancel
		MrPrintF, 'LogErr'
		return
	endif

	;Set properties
	if n_elements(directory) gt 0 then self.directory = directory
	if n_elements(tstart)    gt 0 then self.tstart    = tstart
	if n_elements(tend)      gt 0 then self.tend      = tend
	if n_elements(team)      gt 0 then self.team      = keyword_set(team)
	
	;Store as comma-delimited string
	if n_elements(sc)        gt 0 then self.sc        = strjoin(sc,       ',')
	if n_elements(instr)     gt 0 then self.instr     = strjoin(instr,    ',')
	if n_elements(mode)      gt 0 then self.mode      = strjoin(mode,     ',')
	if n_elements(level)     gt 0 then self.level     = strjoin(level,    ',')
	if n_elements(optdesc)   gt 0 then self.optdesc   = strjoin(optdesc,  ',')
	if n_elements(products)  gt 0 then self.products  = strjoin(products, ',')
	if n_elements(version)   gt 0 then self.version   = strjoin(version,  ',')
end


;+
;   Forward events to the proper event handling method.
;
; :Private:
;
; :Params:
;       EVENT:          in, required, type=structure
;                       Event structure returned by XManager.
;-
pro MrMMS_SDC_GUI_Events, event
	compile_opt idl2

	;Catch errors
	catch, the_error
	if the_error ne 0 then begin
		catch, /cancel
		MrPrintF, 'LogErr'
		return
	endif

	;Destroy the widget
	widget_control, event.id, GET_UVALUE=event_handler
	if event_handler.method ne '' then call_method, event_handler.method, event_handler.object, event
end


;+
;   Cleanup after the widget is destroyed.
;
; :Private:
;-
pro MrMMS_SDC_GUI_Cleanup, tlb
	;Nothing to do.
end


;+
;   Cleanup after the object is destroyed. This will destroy the widget, if it exists.
;-
pro MrMMS_SDC_GUI::Cleanup
	compile_opt idl2

	;Catch errors
	catch, the_error
	if the_error ne 0 then begin
		catch, /cancel
		MrPrintF, 'LogErr'
		return
	endif

	;Destroy the widget
	isValid = widget_info(self.tlb, /VALID_ID)
	if isValid then widget_control, self.tlb, /DESTROY

	;Free Pointers
	ptr_free, self.instruments
	ptr_free, self.modes
	ptr_free, self.levels
	ptr_free, self.optdescs

	;Cleanup the super class
	self -> IDL_Object::Cleanup
end


;+
;   The Initialization method.
;-
function MrMMS_SDC_GUI::Init, $
DIRECTORY=directory, $
GROUP_LEADER=group_leader, $
INSTR = instr, $
MODE = mode, $
LEVEL = level, $
NO_GUI = no_gui, $
OPTDESC = optdesc, $
PRODUCTS = products, $
SC = sc, $
TSTART = tstart, $
TEND = tend, $
TEAM = team, $
VERSION = version
	compile_opt idl2

	;Catch errors
	catch, the_error
	if the_error ne 0 then begin
		catch, /CANCEL
		MrPrintF, 'LogErr'
		return, 0
	endif
	
	;Allocate heap
	self.instruments = ptr_new(/ALLOCATE_HEAP)
	self.modes       = ptr_new(/ALLOCATE_HEAP)
	self.levels      = ptr_new(/ALLOCATE_HEAP)
	self.optdescs    = ptr_new(/ALLOCATE_HEAP)
	
	;Defaults
	if n_elements(sc)        eq 0 then sc        = ''
	if n_elements(instr)     eq 0 then instr     = ''
	if n_elements(mode)      eq 0 then mode      = ''
	if n_elements(level)     eq 0 then level     = ''
	if n_elements(optdesc)   eq 0 then optdesc   = ''
	if n_elements(tstart)    eq 0 then tstart    = ''
	if n_elements(tend)      eq 0 then tend      = ''
	if n_elements(version)   eq 0 then version   = ''
	if n_elements(products)  eq 0 then products  = ''
	if n_elements(directory) eq 0 then directory = filepath('MrWebData', ROOT_DIR=file_search('~', /EXPAND_TILDE))

	;Allocate memory to the spacecraft.
	self.instruments = ptr_new(/ALLOCATE_HEAP)

	;Set the object properties
	self -> SetProperty, SC        = SC, $
	                     INSTR     = instr, $
	                     MODE      = mode, $
	                     LEVEL     = level, $
	                     OPTDESC   = optdesc, $
	                     TSTART    = tstart, $
	                     TEND      = tend, $
	                     TEAM      = team, $
	                     VERSION   = version, $
	                     PRODUCTS  = products, $
	                     DIRECTORY = directory

	;A list of datasets
	*self.instruments = ['aft', $
	                     'aspoc', $
	                     'dfg', $
	                     'dsp', $
	                     'edi', $
	                     'edp', $
	                     'epd-eis', $
	                     'feeps', $
	                     'fgm', $
	                     'fpi', $
	                     'hpca', $
	                     'mec', $
	                     'scm' $
	                    ]
	
	;Modes and Levels
	*self.levels   = ['l1a', 'l1b', 'ql', 'sitl', 'l2pre', 'l2', 'l2plus']
	*self.modes    = ['slow', 'fast', 'srvy', 'brst']
	*self.optdescs = ''
	
	;Create the GUI
	if ~keyword_set(no_gui) then self -> Create, GROUP_LEADER=group_leader
	
	return, 1
end


;+
;   Object definition.
;
; :Params:
;       CLASS:          out, optional, type=structure
;                       The class definition structure.
;-
pro MrMMS_SDC_GUI__Define, class

	class = { MrMMS_SDC_GUI, $
	          Inherits IDL_Object, $
	          directory:       '', $
	          tlb:             0L, $
	          team:            0B, $
	          instr:           '', $
	          instruments:     Ptr_New(), $
	          mode:            '', $
	          modes:           ptr_new(), $
	          level:           '', $
	          levels:          ptr_new(), $
	          optdesc:         '', $
	          optdescs:        ptr_new(), $
	          products:        '', $
	          password:        '', $
	          tstart:          '', $
	          tend:            '', $
	          sc:              '', $
	          version:         '' $
	        }
end