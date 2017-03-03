; docformat = 'rst'
;
; NAME:
;   MrMMS_EDI_Q0_Dist__Define
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
;   Return distribution function of EDI counts.
;
; :See Also:
;   MrMMS_Dist__Define.pro
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
;       2016/11/05  -   Written by Matthew Argall
;-
;*****************************************************************************************
;+
;   The initialization method.
;
; :Params:
;       SC:             in, required, type=string
;                       MMS spacecraft identifier. Options are: {'mms1' | 'mms2' | 'mms3' | 'mms4' }
;       MODE:           in, required, type=string
;                       Telemetry mode. Options are: {'slow' | 'fast' | 'srvy' | 'brst'}
;       OPTDESC:        in, optional, type=string, defualt='q0'
;                       EDI-specific optional descriptor. Options are: {'amb' | 'amb-pm2' |
;                           'alt-cc' | 'alt-oc' | 'alt-oob' | 'alt-ooc'}
;
; :Keywords:
;       _REF_EXTRA:     in, optional, type=boolean, default=0
;                       Any keyword accepted by the ::Load method is also accepted here.
;-
function MrMMS_EDI_Q0_Dist::INIT, sc, mode, optdesc, $
_REF_EXTRA=extra
	compile_opt idl2

	;Error handling
	catch, the_error
	if the_error ne 0 then begin
		catch, /CANCEL
		MrPrintF, 'LogErr'
		return, 0
	endif
	
	self.type_fac = 'NONE'
	
	;Load data
	self -> Load, sc, mode, optdesc, $
	              _STRICT_EXTRA = extra

	return, 1
end


;+
;   Clean up after the object is destroyed
;-
pro MrMMS_EDI_Q0_Dist::CLEANUP
	compile_opt idl2
	on_error, 2
	
	obj_destroy, self.oFGM
	obj_destroy, self.oCounts_GDU1
	obj_destroy, self.oCounts_GDU2
	obj_destroy, self.oTraj_GDU1
	obj_destroy, self.oTraj_GDU2
end


;+
;   Bin EDI data by pitch or gyrophase angle. Resulting data is added to the
;   MrVariable cache.
;
; :Params:
;       OCOUNTS:        in, required, type=MrScalarTS object
;                       Counts data to be binned.
;       OPHI:           in, required, type=MrScalarTS object
;                       Azimuthal angle at which `OCOUNTS` was measured.
;       OTHETA:         in, required, type=MrScalarTS object
;                       Polar angle at which `OCOUNTS` was measured.
;
; :Keywords:
;       DPHI:           in, optional, type=float, default=1.5
;                       Width of each azimuthal bin (degrees). Cannot be used with `NPHI_BINS`.
;       DTHETA:         in, optional, type=float, default=0.5
;                       Width of each polar angle bin (degrees). Cannot be used with `NTHETA_BINS`.
;       DTIME:          in, optional, type=float, default=5.0
;                       Duration, in seconds, of each time bin.
;       NPHI_BINS:      in, optional, type=integer
;                       Number of bins spanning `PHI_RANGE`. Cannot be used with `DPHI`.
;       NTHETA_BINS:    in, optional, type=integer
;                       Number of bins spanning `THETA_RANGE`. Cannot be used with `DTHETA`.
;       PHI_RANGE:      in, optional, type=fltarr(2)
;                       Minimum and maximum azimuthal angles over which to bin (degrees).
;       THETA_RANGE:    in, optional, type=fltarr(2)
;                       Minimum and maximum polar angles over which to bin (degrees).
;-
function MrMMS_EDI_Q0_Dist::Bin, oCounts, oPhi, oTheta, $
DPHI=dphi, $
DTIME=dt, $
DTHETA=dtheta, $
NPHI_BINS=nPhi_bins, $
NTHETA_BINS=nTheta_bins, $
PHI_RANGE=phi_range, $
THETA_RANGE=theta_range
	compile_opt idl2
	on_error, 2

;-------------------------------------------
; Defaults /////////////////////////////////
;-------------------------------------------
	;Defaults
	if n_elements(dt)          eq 0 then dt          = 1.0
	if n_elements(phi_range)   eq 0 then phi_range   = [-180,180]
	if n_elements(theta_range) eq 0 then theta_range = [80,100]
	if n_elements(dPhi)   eq 0 && n_elements(nPhi_bins)   eq 0 then dPhi   = 5.0
	if n_elements(dTheta) eq 0 && n_elements(nTheta_bins) eq 0 then dTheta = 0.5

	;Restrictions
	if n_elements(nPhi_bins) gt 0 && n_elements(dPhi) gt 0 $
		then message, 'NPHI_BINS and DPHI are mutually exclusive.'
	if n_elements(nTheta_bins) gt 0 && n_elements(dTheta) gt 0 $
		then message, 'NTHETA_BINS and DTHETA are mutually exclusive.'

;-------------------------------------------
; Bin Information //////////////////////////
;-------------------------------------------
	
	;Time range
	oTime         = oCounts['TIMEVAR']
	trange        = [oTime[0, 'TT2000'], oTime[-1, 'TT2000']]
	trange_ssm    = MrCDF_epoch2ssm(trange)
	trange_ssm[0] = trange_ssm[0] - (trange_ssm[0] mod dt)
	trange_ssm[1] = trange_ssm[1] - (trange_ssm[1] mod dt) + dt

	;Time bins
	;   - BINSIZE = (MAX – MIN) / (NBINS – 1)
	t_ssm   = oTime -> GetData('SSM')
	nt_bins = long( (trange_ssm[1] - trange_ssm[0]) / dt + 1 )
	t_bins  = linspace(trange_ssm[0], trange_ssm[1], nt_bins)
	
	;Phi bins
	if n_elements(dPhi) gt 0 $
		then nPhi_bins = long( (phi_range[1] - phi_range[0]) / dPhi + 1 ) $
		else dPhi      = (phi_range[1] - phi_range[0]) / (nPhi_bins - 1)
	phi_bins = linspace(phi_range[0], phi_range[1], nPhi_bins)
	
	;Theta bins
	if n_elements(dTheta) gt 0 $
		then nTheta_bins = long( (theta_range[1] - theta_range[0]) / dTheta + 1 ) $
		else dTheat      = (theta_range[1] - theta_range[0]) / (nTheta_bins - 1)
	theta_bins = linspace(theta_range[0], theta_range[1], nTheta_bins)

;-------------------------------------------
; Bin by Time //////////////////////////////
;-------------------------------------------
	;Allocate memory
	theDist = ulonarr(nt_bins, nPhi_bins, nTheta_bins)
	
	;Histogram in time
	tHist  = histogram( temporary(t_ssm), $
	                    MIN             = trange_ssm[0], $
	                    MAX             = trange_ssm[1], $
	                    NBINS           = nt_bins, $
	                    REVERSE_INDICES = rit )
	
	;Loop over each tiem bin
	for i = 0, n_elements(tHist) - 1 do begin
		nt = rit[i+1] - rit[i]
		if nt eq 0 then continue
		
		;Indices within the source
		it = rit[rit[i]:rit[i+1]-1]

	;-------------------------------------------
	; Bin by Phi ///////////////////////////////
	;-------------------------------------------
		;Histogram in Phi
		phiHist = histogram( oPhi[it], $
		                     MIN             = phi_range[0], $
		                     MAX             = phi_range[1], $
		                     NBINS           = nPhi_bins, $
		                     REVERSE_INDICES = riPhi )
		
		;Loop over each Phi bin
		for j = 0, n_elements(phiHist) - 1 do begin
			nPhi = riPhi[j+1] - riPhi[j]
			if nPhi eq 0 then continue

			;Indices within the source
			iPhi  = riPhi[riPhi[j]:riPhi[j+1]-1]
			itPhi = it[iPhi]

		;-------------------------------------------
		; Bin by Theta /////////////////////////////
		;-------------------------------------------
			;Histogram in theta
			tempTheta = nPhi eq 1 ? [oTheta[itPhi]] : oTheta[itPhi]
			thetaHist = histogram( temporary(tempTheta), $
			                       MIN             = theta_range[0], $
			                       MAX             = theta_range[1], $
			                       NBINS           = nTheta_bins, $
			                       REVERSE_INDICES = riTheta )

			;Loop over each Theta bin
			for k = 0, n_elements(thetaHist) - 1 do begin
				nTheta = riTheta[k+1] - riTheta[k]
				if nTheta eq 0 then continue
				
				;Inidices within the source
				iTheta     = riTheta[riTheta[k]:riTheta[k+1]-1]
				itPhiTheta = itPhi[iTheta]

				;Save the data
				;   - Sum all counts
				;   - Normalize by time spent in each bin
				theDist[i,j,k] = total(oCounts[itPhiTheta]) / n_elements(itPhiTheta)
			endfor
		endfor
	endfor
	theDist = round(theDist)

;-------------------------------------------
; Output Data //////////////////////////////
;-------------------------------------------

	;Time Bins
	oBinnedT  = MrTimeVar(t_bins+dt/2.0, 'SSM', /NO_COPY, T_REF=oTime[[0]])
	oBinnedT -> AddAttr, 'DELTA_PLUS',  dt/2.0
	oBinnedT -> AddAttr, 'DELTA_MINUS', dt/2.0
	oBinnedT -> AddAttr, 'TIME_BASE',   'J2000'
	oBinnedT -> AddAttr, 'TITLE',       'Center time tags for EDI Q0 Phi Spectrogram'
	oBinnedT -> AddAttr, 'UNITS',       'ns'

	;Phi Bins
	if nPhi_bins gt 1 then begin
		oPhiBins  = MrVariable(phi_bins + dPhi/2.0, /NO_COPY)
		oPhiBins -> AddAttr, 'AXIS_RANGE',  phi_range
		oPhiBins -> AddAttr, 'DELTA_PLUS',  dPhi/2.0
		oPhiBins -> AddAttr, 'DELTA_MINUS', dPhi/2.0
		oPhiBins -> AddAttr, 'PLOT_TITLE',  'Azimuthal Angle Bins'
		oPhiBins -> AddAttr, 'TITLE',       'Phi'
		oPhiBins -> AddAttr, 'UNITS',       'degrees'
	endif

	;Theta Bins
	if nTheta_bins gt 1 then begin
		oThetaBins  = MrVariable(theta_bins + dTheta/2.0, /NO_COPY)
		oThetaBins -> AddAttr, 'AXIS_RANGE',  theta_range
		oThetaBins -> AddAttr, 'DELTA_PLUS',  dTheta/2.0
		oThetaBins -> AddAttr, 'DELTA_MINUS', dTheta/2.0
		oThetaBins -> AddAttr, 'PLOT_TITLE',  'Polar Angle Bins'
		oThetaBins -> AddAttr, 'TITLE',       'Theta'
		oThetaBins -> AddAttr, 'UNITS',       'degrees'
	endif

	;Binned Counts
	oBinnedCts  = MrTimeSeries(oBinnedT, reform(theDist), /NO_COPY, DIMENSION=1)
	oBinnedCts -> AddAttr, 'PLOT_TITLE',    'EDI Quality 0 Binned Electron Counts'
	oBinnedCts -> AddAttr, 'MISSING_VALUE', 0
	oBinnedCts -> AddAttr, 'SCALE',         1
	oBinnedCts -> AddAttr, 'TITLE',         'Normalized Counts'
	oBinnedCts -> AddAttr, 'UNITS',         'Counts'
	
	if nPhi_bins gt 1 && nTheta_bins gt 1 then begin
		oBinnedCts -> AddAttr, 'DEPEND_1', oPhiBins
		oBinnedCts -> AddAttr, 'DEPEND_2', oThetaBins
	endif else if nPhi_bins gt 1 then begin
		oBinnedCts -> AddAttr, 'DEPEND_1', oPhiBins
	endif else if nTheta_bins gt 1 then begin
		oBinnedCts -> AddAttr, 'DEPEND_1', oThetaBins
	endif
	
	;Return
	return, oBinnedCts
end


;+
;   Retrieve data.
;
; :Params:
;       OCOUNTS:        out, required, type=MrScalarTS object
;                       Electron counts.
;       OPHI:           out, required, type=MrScalarTS object
;                       Azimuthal angle of electron incident trajectories.
;       OTHETA:         out, required, type=MrScalarTS object
;                       Polar angle of electron incident trajectories.
;
; :Keywords:
;       GDU1:           in, optional, type=boolean
;                       If set, only data from GDU1 is returned. The default is to
;                           combine data from both GDU1 and GDU2 into a single array.
;                           Cannot be used with `GDU2`.
;       GDU2:           in, optional, type=boolean
;                       If set, only data from GDU2 is returned. The default is to
;                           combine data from both GDU1 and GDU2 into a single array.
;                           Cannot be used with `GDU1`.
;       TYPE_FAC:       in, optional, type=string
;                       Name of the field-aligned coordinate system into which data
;                           is rotated. Options are: {'None' | '' | 'ExB' | 'VxB'}
;-
pro MrMMS_EDI_Q0_Dist::GetData, oCounts, oPhi, oTheta, $
TYPE_FAC=type_fac, $
GDU1=gdu1, $
GDU2=gdu2
	compile_opt idl2
	on_error, 2
	
	;Pick a single gdu?
	tf_gdu1 = keyword_set(gdu1)
	tf_gdu2 = keyword_set(gdu2)
	if tf_gdu1 && tf_gdu2 then message, 'Keywords GDU1 and GDU2 are mutually exclusive.'
	if n_params() ne 3 then message, 'Three output parameters are required.'

;-------------------------------------------
; Both GDUs ////////////////////////////////
;-------------------------------------------
	if ~tf_gdu1 && ~tf_gdu2 then begin
		;Time
		t_gdu1 = self.oCounts_GDU1['TIMEVAR'] -> GetData('TT2000')
		t_gdu2 = self.oCounts_GDU2['TIMEVAR'] -> GetData('TT2000')
		
		;Combine data
		;   - Time is always along the first dimension
		time   = [ temporary(t_gdu1),         temporary(t_gdu2)         ]
		counts = [ self.oCounts_GDU1['DATA'], self.oCounts_GDU2['DATA'] ]
		traj   = [ self.oTraj_GDU1['DATA'],   self.oTraj_GDU2['DATA']   ]

		;Sort the data
		it     = sort(time)
		time   = time[it]
		traj   = traj[it,*]
		counts = counts[it]

		;Create new objects
		oTime   = MrTimeVar( time, 'TT2000', /NO_COPY )
		oCounts = MrScalarTS( oTime, counts, /NO_COPY )
		oTraj   = MrTimeSeries( oTime, traj, /NO_COPY )
		gdu     = 'BOTH'
	
;-------------------------------------------
; GDU1 Alone ///////////////////////////////
;-------------------------------------------
	endif else if tf_gdu1 then begin
		oTime   = self.oCounts_GDU1['TIMEVAR']
		oCounts = self.oCounts_GDU1
		oTraj   = self.oTraj_GDU1
		gdu     = 'GDU1'
	
;-------------------------------------------
; GDU2 Alone ///////////////////////////////
;-------------------------------------------
	endif else begin
		oTime   = self.oCounts_GDU2['TIMEVAR']
		oCounts = self.oCounts_GDU2
		oTraj   = self.oTraj_GDU2
		gdu     = 'GDU2'
	endelse

;-----------------------------------------------------
; Incidence Angles \\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\
;-----------------------------------------------------
	if n_elements(type_fac) gt 0 then begin
		;Reload FAC if:
		;   - FAC_TYPE changes
		;   - OPAR_FAC time tags are different from OTRAJ
		;   - Timetags will change if /GDU1 or /GDU2 are set in, e.g., ::GetDist
		tf_reload = 0B
		if (strupcase(type_fac) ne self.type_fac) || gdu ne self.gdu then begin
			self -> Load_FAC, type_fac, oTime, $
			                  COORD_SYS = self.coord_sys
		endif
	endif
	
	;Get Azimuth and Polar angles
	self -> Grid, oTraj, oPhi, oTheta

	;Set the GDU
	self.gdu = gdu
end


;+
;   Create a distribution as a function of time, azimuth angle, and polar angle.
;
; :Keywords:
;       CACHE:          in, optional, type=boolean, default=0
;                       If set, the distribution is added to the variable cache.
;       DPHI:           in, optional, type=float, default=1.5
;                       Width of each azimuthal bin (degrees). Cannot be used with `NPHI_BINS`.
;       DTHETA:         in, optional, type=float, default=0.1
;                       Width of each polar angle bin (degrees). Cannot be used with `NTHETA_BINS`.
;       DTIME:          in, optional, type=float, default=1.0
;                       Duration, in seconds, of each time bin.
;       GDU1:           in, optional, type=boolean
;                       If set, only data from GDU1 is returned. The default is to
;                           combine data from both GDU1 and GDU2 into a single array.
;                           Cannot be used with `GDU2`.
;       GDU2:           in, optional, type=boolean
;                       If set, only data from GDU2 is returned. The default is to
;                           combine data from both GDU1 and GDU2 into a single array.
;                           Cannot be used with `GDU1`.
;       NAME:           in, optional, type=string, default=sc_instr_dist_optdesc_mode_level
;                       Name to be given to the distribution.
;       NPHI_BINS:      in, optional, type=integer
;                       Number of bins spanning `PHI_RANGE`. Cannot be used with `DPHI`.
;       NTHETA_BINS:    in, optional, type=integer
;                       Number of bins spanning `THETA_RANGE`. Cannot be used with `DTHETA`.
;       PHI_RANGE:      in, optional, type=fltarr(2)
;                       Minimum and maximum azimuthal angles over which to bin (degrees).
;       THETA_RANGE:    in, optional, type=fltarr(2)
;                       Minimum and maximum polar angles over which to bin (degrees).
;       TYPE_FAC:       in, optional, type=string
;                       Name of the field-aligned coordinate system into which data
;                           is rotated. Options are: {'None' | '' | 'ExB' | 'VxB'}
;
; :Returns:
;       ODIST:          out, required, type=MrTimeSeries
;                       The distribution of counts as a function of time, azimuth angle,
;                           and polar angle.
;-
function MrMMS_EDI_Q0_Dist::GetDist, $
CACHE=cache, $
DPHI=dphi, $
DTIME=dt, $
DTHETA=dtheta, $
GDU1=gdu1, $
GDU2=gdu2, $
NAME=name, $
NPHI_BINS=nPhi_bins, $
NTHETA_BINS=nTheta_bins, $
PHI_RANGE=phi_range, $
THETA_RANGE=theta_range, $
TYPE_FAC=type_fac
	compile_opt idl2
	
	catch, the_error
	if the_error ne 0 then begin
		catch, /CANCEL
		MrPrintF, 'LogErr'
		return, !Null
	endif
	
	;Default name
	if n_elements(name) eq 0 then name = strjoin([self.sc, 'edi', 'dist', self.optdesc, self.mode, self.level], '_')
	
	;Get the data
	self -> GetData, oCounts, oPhi, oTheta, $
	                 GDU1     = gdu1, $
	                 GDU2     = gdu2, $
	                 TYPE_FAC = type_fac
	
	;Bin the data
	oDist = self -> Bin( oCounts, oPhi, oTheta, $
	                     DPHI        = dphi, $
	                     DTIME       = dt, $
	                     DTHETA      = dtheta, $
	                     NPHI_BINS   = nPhi_bins, $
	                     NTHETA_BINS = nTheta_bins, $
	                     NTIME_BINS  = nt_bins, $
	                     PHI_RANGE   = phi_range, $
	                     THETA_RANGE = theta_range )
	
	;Set the name and cache variable
	oDist -> SetName, name
	if keyword_set(cache) then oDist -> Cache
	
	;Return the distribution
	return, oDist
end


;+
;   Create a distribution as a function of time and azimuth angle. Data is averaged
;   over all polar angles.
;
; :Keywords:
;       CACHE:          in, optional, type=boolean, default=0
;                       If set, the distribution is added to the variable cache.
;       DPHI:           in, optional, type=float, default=1.5
;                       Width of each azimuthal bin (degrees). Cannot be used with `NPHI_BINS`.
;       DTIME:          in, optional, type=float, default=1.0
;                       Duration, in seconds, of each time bin.
;       GDU1:           in, optional, type=boolean
;                       If set, only data from GDU1 is returned. The default is to
;                           combine data from both GDU1 and GDU2 into a single array.
;                           Cannot be used with `GDU2`.
;       GDU2:           in, optional, type=boolean
;                       If set, only data from GDU2 is returned. The default is to
;                           combine data from both GDU1 and GDU2 into a single array.
;                           Cannot be used with `GDU1`.
;       NAME:           in, optional, type=string, default=sc_instr_phidist_optdesc_mode_level
;                       Name to be given to the distribution.
;       NPHI_BINS:      in, optional, type=integer
;                       Number of bins spanning `PHI_RANGE`. Cannot be used with `DPHI`.
;       PHI_RANGE:      in, optional, type=fltarr(2)
;                       Minimum and maximum azimuthal angles over which to bin (degrees).
;       THETA_RANGE:    in, optional, type=fltarr(2)
;                       Minimum and maximum polar angles over which to bin (degrees).
;       TYPE_FAC:       in, optional, type=string
;                       Name of the field-aligned coordinate system into which data
;                           is rotated. Options are: {'None' | '' | 'ExB' | 'VxB'}
;
; :Returns:
;       ODIST:          out, required, type=MrTimeSeries
;                       The distribution of counts as a function of time and azimuth angle,
;                           averaged over polar angle.
;-
function MrMMS_EDI_Q0_Dist::GetPhiSpec, $
CACHE=cache, $
DPHI=dPhi, $
DTIME=dt, $
GDU1=gdu1, $
GDU2=gdu2, $
NAME=name, $
NPHI_BINS=nPhi_bins, $
PHI_RANGE=phi_range, $
THETA_RANGE=theta_range, $
TYPE_FAC=type_fac
	compile_opt idl2
	on_error, 2
	
	;Defaults
	nTheta_bins = 1
	if n_elements(name) eq 0 then name = strjoin([self.sc, 'edi', 'phispec', self.optdesc, self.mode, self.level], '_')
	
	;Get the data
	self -> GetData, oCounts, oPhi, oTheta, $
	                 GDU1     = gdu1, $
	                 GDU2     = gdu2, $
	                 TYPE_FAC = type_fac
	
	;Create the distribution
	oPhiDist = self -> Bin( oCounts, oPhi, oTheta, $
	                        DPHI        = dphi, $
	                        DTIME       = dt, $
	                        NPHI_BINS   = nPhi_bins, $
	                        NTHETA_BINS = nTheta_bins, $
	                        PHI_RANGE   = phi_range, $
	                        THETA_RANGE = theta_range )
	
	;Set the name and cache variable
	oPhiDist -> SetName, name
	if keyword_set(cache) then oPhiDist -> Cache
	
	return, oPhiDist
end


;+
;   Create a distribution as a function of time and polar angle. Data is averaged
;   over all azimuthal angles.
;
; :Keywords:
;       CACHE:          in, optional, type=boolean, default=0
;                       If set, the distribution is added to the variable cache.
;       DTHETA:         in, optional, type=float, default=0.1
;                       Width of each polar angle bin (degrees). Cannot be used with `NTHETA_BINS`.
;       DTIME:          in, optional, type=float, default=1.0
;                       Duration, in seconds, of each time bin.
;       GDU1:           in, optional, type=boolean
;                       If set, only data from GDU1 is returned. The default is to
;                           combine data from both GDU1 and GDU2 into a single array.
;                           Cannot be used with `GDU2`.
;       GDU2:           in, optional, type=boolean
;                       If set, only data from GDU2 is returned. The default is to
;                           combine data from both GDU1 and GDU2 into a single array.
;                           Cannot be used with `GDU1`.
;       NAME:           in, optional, type=string, default=sc_instr_thetadist_optdesc_mode_level
;                       Name to be given to the distribution.
;       NTHETA_BINS:    in, optional, type=integer
;                       Number of bins spanning `THETA_RANGE`. Cannot be used with `DTHETA`.
;       PHI_RANGE:      in, optional, type=fltarr(2)
;                       Minimum and maximum azimuthal angles over which to bin (degrees).
;       THETA_RANGE:    in, optional, type=fltarr(2)
;                       Minimum and maximum polar angles over which to bin (degrees).
;       TYPE_FAC:       in, optional, type=string
;                       Name of the field-aligned coordinate system into which data
;                           is rotated. Options are: {'None' | '' | 'ExB' | 'VxB'}
;
; :Returns:
;       ODIST:          out, required, type=MrTimeSeries
;                       The distribution of counts as a function of time, polar angle,
;                           averaged over azimuthal angle.
;-
function MrMMS_EDI_Q0_Dist::GetThetaSpec, $
CACHE=cache, $
DTIME=dt, $
DTHETA=dTheta, $
GDU1=gdu1, $
GDU2=gdu2, $
NAME=name, $
NTHETA_BINS=nTheta_bins, $
PHI_RANGE=phi_range, $
THETA_RANGE=theta_range, $
TYPE_FAC=type_fac
	compile_opt idl2
	on_error, 2
	
	;Defaults
	nPhi_bins = 1
	if n_elements(name) eq 0 then name = strjoin([self.sc, 'edi', 'thetaspec', self.optdesc, self.mode, self.level], '_')
	
	;Get the data
	self -> GetData, oCounts, oPhi, oTheta, $
	                 GDU1     = gdu1, $
	                 GDU2     = gdu2, $
	                 TYPE_FAC = type_fac
	
	;Create the distribution
	oThetaDist = self -> Bin( oCounts, oPhi, oTheta, $
	                          DTHETA      = dTheta, $
	                          DTIME       = dt, $
	                          NPHI_BINS   = nPhi_bins, $
	                          NTHETA_BINS = nTheta_bins, $
	                          PHI_RANGE   = phi_range, $
	                          THETA_RANGE = theta_range )
	
	;Set the name and cache variable
	oThetaDist -> SetName, name
	if keyword_set(cache) then oThetaDist -> Cache
	
	return, oThetaDist
end


;+
;   Obtain the azimuthal and polar angle of electron incident trajectories. If a
;   field-aligned coordinate system has been loaded, trajectories are rotated
;   accordingly.
;
;   NOTES:
;      OTRAJ may consist of data from either one or two GDUs, and each GDU has
;      different time tags. As such, the field-aligned coordinate system needs
;      to be updated accordingly
;
; :Params:
;       OTRAJ:          in, required, type=MrTimeSeries objref
;                       Electron incident trajectories.
;       OPHI:           out, required, type=MrScalarTS object
;                       Azimuthal angle of electron incident trajectories.
;       OTHETA:         out, required, type=MrScalarTS object
;                       Polar angle of electron incident trajectories.
;
; :Keywords:
;       TYPE_FAC:       out, required, type=MrScalarTS object
;                       Name of a field-aligned coordinate system. If given, angles
;                           are rotated accordingly. Options are: {'none', '', 'ExB', 'VxB'}
;-
pro MrMMS_EDI_Q0_Dist::Grid, oTraj, oPhi, oTheta
	compile_opt idl2
	on_error, 2
	
	;Size of input
	nDims = size(oTraj, /N_DIMENSIONS)
	dims  = size(oTraj, /DIMENSIONS)
	nTime = dims[0]
	nChan = nDims eq 2 ? 1 : dims[2]

;-----------------------------------------------------
; Rotate to FAC \\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\
;-----------------------------------------------------
	
	if self.oFGM.type_fac ne 'NONE' then begin
		;Transformation to FAC
		oT = self.oFGM -> T_FAC()
	
		;Compute the vector components
		deg2rad         = !dpi / 180D
		traj_vec        = fltarr(nTime, 3, nChan)
		traj_vec[*,0,*] = sin(oTraj[*,1,*] * deg2rad) * cos(oTraj[*,0,*] * deg2rad)
		traj_vec[*,1,*] = sin(oTraj[*,1,*] * deg2rad) * sin(oTraj[*,0,*] * deg2rad)
		traj_vec[*,2,*] = cos(oTraj[*,1,*] * deg2rad)
		
		;Rotate vector components
		traj_fac = fltarr(nTime, 3, nChan)
		for i = 0, nChan - 1 do begin
			traj_fac[*,0,i] = oT[*,0,0] * traj_vec[*,0,i] + oT[*,1,0] * traj_vec[*,1,i] + oT[*,2,0] * traj_vec[*,2,i]
			traj_fac[*,1,i] = oT[*,0,1] * traj_vec[*,0,i] + oT[*,1,1] * traj_vec[*,1,i] + oT[*,2,1] * traj_vec[*,2,i]
			traj_fac[*,2,i] = oT[*,0,2] * traj_vec[*,0,i] + oT[*,1,2] * traj_vec[*,1,i] + oT[*,2,2] * traj_vec[*,2,i]
		endfor
		
		;Compute polar coordinates
		rad2deg    = 180D / !dpi
		traj_phi   = atan( reform(traj_fac[*,1,*]), reform(traj_fac[*,0,*]) ) * rad2deg
		traj_theta = acos( reform(traj_fac[*,2,*]) ) * rad2deg

;-----------------------------------------------------
; Keep Original Coordinates \\\\\\\\\\\\\\\\\\\\\\\\\\
;-----------------------------------------------------
	endif else begin
		traj_phi   = reform(oTraj[*,0,*])
		traj_theta = reform(oTraj[*,1,*])
	endelse
	
;-----------------------------------------------------
; Output \\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\
;-----------------------------------------------------
	;PHI
	oPhi  = MrScalarTS( oTraj['TIMEVAR'], traj_phi )
	oPhi -> AddAttr, 'AXIS_RANGE', [0, 360]
	oPhi -> AddAttr, 'PLOT_TITLE', 'Azimuthal Angle of Particle Trajectories'
	oPhi -> AddAttr, 'TITLE',      'Phi'
	oPhi -> AddAttr, 'UNITS',      'degrees'

	;THETA
	oTheta  = MrScalarTS( oTraj['TIMEVAR'], traj_theta )
	oTheta -> AddAttr, 'AXIS_RANGE', [0, 180]
	oTheta -> AddAttr, 'PLOT_TITLE', 'Polar Angle of Particle Trajectories'
	oTheta -> AddAttr, 'TITLE',      'Theta'
	oTheta -> AddAttr, 'UNITS',      'degrees'
end


;+
;   Load EDI data from source.
;
; :Params:
;       SC:             in, required, type=string
;                       MMS spacecraft identifier. Options are: {'mms1' | 'mms2' | 'mms3' | 'mms4' }
;       MODE:           in, required, type=string
;                       Telemetry mode. Options are: {'slow' | 'fast' | 'srvy' | 'brst'}
;       OPTDESC:        in, optional, type=string, defualt='q0'
;                       EDI-specific optional descriptor. Options are: {'amb' | 'amb-pm2' |
;                           'alt-cc' | 'alt-oc' | 'alt-oob' | 'alt-ooc'}
;
; :Keywords:
;       COORD_SYS:      in, required, type=string
;                       Coordinate system in which data is loaded. Options are: { 'dbcs' | 'gse' }.
;       LEVEL:          in, required, type=string, default='l2'
;                       Data quality level. Options are: { 'l1a' | 'ql' | 'l2' }
;-
pro MrMMS_EDI_Q0_Dist::Load, sc, mode, optdesc, $
COORD_SYS=coord_sys, $
LEVEL=level
	compile_opt idl2
	on_error, 2
	
	instr = 'edi'
	if n_elements(level)     eq 0 then level     = 'l2'
	if n_elements(optdesc)   eq 0 then optdesc   = 'q0'
	if n_elements(coord_sys) eq 0 then coord_sys = 'gse'
	
	;Load the data
	MrMMS_Load_Data, sc, instr, mode, level, $
	                 OPTDESC   = optdesc, $
	                 VARFORMAT = ['*flux*', '*counts*', '*traj_' + coord_sys + '_gdu?_' + mode + '*'], $
	                 VARNAMES  = varnames

	;Extract variable names
	counts_gdu1_vname = varnames[ where(stregex(varnames, '(counts|flux)[1-4]?_gdu1_', /BOOLEAN)) ]
	counts_gdu2_vname = varnames[ where(stregex(varnames, '(counts|flux)[1-4]?_gdu2_', /BOOLEAN)) ]
	traj_gdu1_vname   = varnames[ where(stregex(varnames, '(traj)[1-4]?_' + coord_sys + '_gdu1_' + mode, /BOOLEAN)) ]
	traj_gdu2_vname   = varnames[ where(stregex(varnames, '(traj)[1-4]?_' + coord_sys + '_gdu2_' + mode, /BOOLEAN)) ]

	;Get the data
	self.oCounts_GDU1 = MrVar_Get(counts_gdu1_vname)
	self.oCounts_GDU2 = MrVar_Get(counts_gdu2_vname)
	self.oTraj_GDU1   = MrVar_Get(traj_gdu1_vname)
	self.oTraj_GDU2   = MrVar_Get(traj_gdu2_vname)
	
	;Delete variable names from cache
	MrVar_Remove, varnames
	
	;Set properties
	self.sc        = sc
	self.instr     = instr
	self.mode      = mode
	self.level     = level
	self.optdesc   = n_elements(optdesc) eq 0 ? '' : optdesc
	self.coord_sys = coord_sys
end


;+
;   Load the magnetic field from source.
;-
pro MrMMS_EDI_Q0_Dist::Load_B
	compile_opt idl2
	on_error, 2
	
	if self.sc   eq '' then message, 'The SC property must be set.'
	if self.mode eq '' then message, 'The MODE property must be set.'
	
	if self.coord_sys eq 'dbcs' $
		then cs = 'dmpa' $
		else cs = self.coord_sys
	
	;Load B-field
	oFGM = MrMMS_FGM_BField( self.sc, self.mode, $
	                         INSTR     = 'dfg', $
	                         LEVEL     = 'l2pre', $
	                         COORD_SYS = cs )
	
	;Set property
	if obj_valid(oFGM) then self -> SetB, oFGM
end


;+
;   Set the magnetic field data.
;
; :Params:
;       BFIELD:         in, required, type=string/integer/objref
;                       Name, number, or objref of at MrVar_BField variable.
;-
pro MrMMS_EDI_Q0_Dist::SetB, bfield
	compile_opt idl2
	on_error, 2

	;Get the variable
	oBtemp = MrVar_Get(bfield)
	if ~obj_isa(oB, 'MrVar_BField') then message, 'VARIABLE must be a MrVar_BField object.'
	
	;Interpolate
	if self.oTime -> IsIdentical(oB['TIMEVAR']) $
		then oB = oBtemp -> Copy(oBtemp) $
		else oB = oBtemp -> Interpol(self.oTime)
	
	;Set
	self.oFGM = oB
end


;+
;   Set the magnetic field data.
;
; :Params:
;       TYPE:           in, required, type=string
;                       Type of field-aligned coordinate system to use.
;-
pro MrMMS_EDI_Q0_Dist::SetFAC, type
	compile_opt idl2
	on_error, 2
	
	self.oFGM -> Load_FAC, type
end


;+
;   Set the counts and trajectories data (also, see the ::Load method).
;
; :Params:
;       COUNTS_GDU1:    in, required, type=string/integer/objref
;                       Name, number, of MrTimeSeries objref of GDU1 counts.
;       COUNTS_GDU2:    in, required, type=string/integer/objref
;                       Name, number, of MrTimeSeries objref of GDU2 counts.
;       TRAJ_GDU1:      in, required, type=string/integer/objref
;                       Name, number, of MrTimeSeries objref of GDU1 trajectories.
;       TRAJ_GDU2:      in, required, type=string/integer/objref
;                       Name, number, of MrTimeSeries objref of GDU2 trajectories.
;-
pro MrMMS_EDI_Q0_Dist::SetData, counts_gdu1, counts_gdu2, traj_gdu1, traj_gdu2
	compile_opt idl2
	on_error, 2

;-------------------------------------------
; Get the Data Objects /////////////////////
;-------------------------------------------
	
	;Get the variable object
	oCts_GDU1  = MrVar_Get(counts_gdu1)
	oCts_GDU2  = MrVar_Get(counts_gdu2)
	oTraj_GDU1 = MrVar_Get(traj_gdu1)
	oTraj_GDU2 = MrVar_Get(traj_gdu2)
	
	;Make sure it has all of the dependent variable attributes
	if ~obj_isa(oDist, 'MrTimeSeries') then message, 'VARIABLE must be a MrTimeSeries object.'
	oTime   = oDist['TIMEVAR']
	oPhi    = MrVar_Get( oDist['DEPEND_1'] )
	oTheta  = MrVar_Get( oDist['DEPEND_2'] )
	oEnergy = MrVar_Get( oDist['DEPEND_3'] )
	
	;Check sizes
	nDims   = size(oDist, /N_DIMENSIONS)
	dims    = size(oDist, /DIMENSIONS)
	nTime   = dims[0]
	nPhi    = dims[1]
	nTheta  = dims[2]
	nEnergy = dims[3]

;-------------------------------------------
; Check Name ///////////////////////////////
;-------------------------------------------
	parts = stregex(oTraj_GDU1.name, '(mms[1-4])_(edi)_.+_(dbcs|gse|gsm)_(fast|brst)_(l1a|ql|l2)', /SUBEXP, /EXTRACT)
	if parts[0] ne '' then begin
		sc        = parts[1]
		instr     = parts[2]
		coord_sys = parts[3]
		mode      = parts[4]
		level     = parts[5]
	endif

;-------------------------------------------
; Phi //////////////////////////////////////
;-------------------------------------------
	;Dimensions:
	;   - [nPhi]
	;   - [nTime, nPhi]
	;   - [nTime, nPhi, nTheta]
	nDims = size(oPhi, /N_DIMENSIONS)
	dims  = size(oPhi, /DIMENSIONS)
	case nDims of
		3: if dims[0] ne nTime && dims[1] ne nPhi && dims[2] ne nTheta then message, 'DEPEND_1 is incorrect size.'
		2: if dims[0] ne nTime && dims[1] ne nPhi then message, 'DEPEND_1 is incorrect size.'
		1: if dims[0] ne nPhi then message, 'DEPEND_1 is incorrect size.'
		else: message, 'DEPEND_1 is incorrect size.'
	endcase

;-------------------------------------------
; Theta ////////////////////////////////////
;-------------------------------------------
	;Dimensions:
	;   - [nTheta]
	;   - [nTime, nTheta]
	;   - [nTime, nPhi, nTheta]
	nDims = size(oTheta, /N_DIMENSIONS)
	dims  = size(oTheta, /DIMENSIONS)
	case nDims of
		3: if dims[0] ne nTime && dims[1] ne nPhi && dims[2] ne nTheta then message, 'DEPEND_2 is incorrect size.'
		2: if dims[0] ne nTime && dims[1] ne nTheta then message, 'DEPEND_2 is incorrect size.'
		1: if dims[0] ne nTheta then message, 'DEPEND_2 is incorrect size.'
		else: message, 'DEPEND_2 is incorrect size.'
	endcase

;-------------------------------------------
; Energy ///////////////////////////////////
;-------------------------------------------
	;Dimensions:
	;   - [nEnergy]
	;   - [nTime, nEnergy]
	nDims = size(oEnergy, /N_DIMENSIONS)
	dims  = size(oEnergy, /DIMENSIONS)
	case nDims of
		2: if dims[0] ne nTime && dims[1] ne nEnergy then message, 'DEPEND_3 is incorrect size.'
		1: if dims[0] ne nEnergy then message, 'DEPEND_3 is incorrect size.'
		else: message, 'DEPEND_3 is incorrect size.'
	endcase
	
	;Set Data
	self.oDist = oDist
	if n_elements(sc) gt 0 then begin
		self.sc        = sc
		self.instr     = instr
		self.mode      = mode
		self.level     = level
		self.optdesc   = 'q0'
		self.coord_sys = coord_sys
	endif
end


;+
;   The class definition statement.
;
; :Params:
;       CLASS:          out, optional, type=structure
;-
pro MrMMS_EDI_Q0_Dist__DEFINE
	compile_opt idl2
	
	class = { MrMMS_EDI_Q0_Dist, $
	          oFGM:         obj_new(), $
	          gdu:          '', $
	          oCounts_GDU1: obj_new(), $
	          oCounts_GDU2: obj_new(), $
	          oTraj_GDU1:   obj_new(), $
	          oTraj_GDU2:   obj_new() $
	        }
end