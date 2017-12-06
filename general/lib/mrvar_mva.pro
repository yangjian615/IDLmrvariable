; docformat = 'rst'
;
; NAME:
;       MrVar_MVA
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
;   Perform a minimum (maximum) variance analysis on the magnetic (electric) field.
;
;   The convention used in this program is suitable for the study of the day-side
;   magnetosphere: the components are ordered such that N x M = L and such that the
;   GSE-x component of N points toward the sun, the GSE-z component of L points North
;   and the sign of M is chosen to create a right-handed coordinate system.
;
;   Eigenvectors are ordered in the following manner::
;       X[*,0] = N = Normal component (N) [i.e. the minimum (maximum) variance component for MVAB (MVAE)]
;       X[*,1] = M = Intermediately varying component (M)
;       X[*,2] = L = Maximally (minimally) varying component for MVAB (MVAE) 
;
;   Eigenvalues correspond to::
;           min(EIGENVALUES) -> Normal component
;           mid(EIGENVALUES) -> Intermediate component
;           max(EIGENVALUES) -> Maximum (minimum) component for MVAB (MVAE)
;
; REFERENCES::
;   Bengt, Sonnerup, Scheible, chp. 8, Minimum and Maximum Variance Analysis from the
;       ISEE "Analysis Methods for Multi-Spacecraft Data" book, 2000
;   Sonnerup, "Magnetopause Properties from AMPTE/IRM Observations of the
;       Convection Electric Field: Method Development", JGR 1987
;
; :Examples:
;   Run the test program::
;       IDL> void = MrVar_MVA(/TEST)
;
; :History:
;   Modification History::
;       2017-05-04  -   Written by Matthew Argall
;-
;*****************************************************************************************
;+
;   This example program uses data from the Appendix at the end of Chapter 8 in ISSI 
;   Analysis Methods for Multi-Spacecraft Data. Data points are from AMPTE/IRM, 
;   from 05:18:20–05:19:26 UT on October 19, 1984 for use in benchmark tests
;   of MVA and other programs
;
;   The procedure that follows will duplicates the results from the text.
;-
PRO MrVar_MVA_Test
	Compile_Opt idl2
	On_Error, 2
	
	;---------------------------------------------------------------------
	; Sample Data ////////////////////////////////////////////////////////
	;---------------------------------------------------------------------
	;Time (s) seconds from 15:18:20.49
	t = [0.00,4.38,8.75,13.12,17.49,21.87,26.24,30.61,34.98,39.36,43.73,48.10,52.47,56.85,61.22,65.59]

	;Density (#/cm^3)
	N = [12.18,9.22,9.92,18.08,20.39,15.00,20.19,23.53,24.31,25.91,26.17,24.49,22.20,22.86,17.56,17.86]

	;B(x,y,z) (nT)
	Bx = transpose([-13.6,-14.8,-13.4,-14,-7.1,-0.9,-10,-6.1,1.2,-3.4,-0.9,-1,11,19.1,24.9,29.2])
	By = transpose([-24.7,-24.9,-17.2,-25,-4.5,-5,-0.4,-4.8,1.6,-3.9,1.2,-1.5,13.2,34.4,50.1,47.1])
	Bz = transpose([54.6,58.7,62.4,43.8,33.5,44.4,44.6,21.1,21,4.1,5,12.3,29.7,20.1,1.9,-10.6])

	;Velocity (km/s)
	Vx = transpose([-111,-102,-111,-135,-128,-82,-139,-143,-132,-117,-112,-98,-90,-83,-82,-93])
	Vy = transpose([-211,-213,-196,-229,-252,-237,-228,-241,-226,-217,-210,-212,-186,-168,-129,-123])
	Vz = transpose([57,41,34,54,54,51,77,57,80,79,93,92,104,121,88,53])

	;Combine the components of the magnetic field and of the velocity
	B = double([Bx, By, Bz])
	v = double([Vx, Vy, Vz])

	;---------------------------------------------------------------------
	; Minimum Variance Analysis //////////////////////////////////////////
	;---------------------------------------------------------------------
	;Find the minimum variance frame
	mvab_eigvecs = MrMVA(B, EIGENVALUES=mvab_eigvals)

	;Rotate the data into the MVAB frame
	Bavg   = mean(B, DIMENSION=2)
	mvab_Bavg = [mvab_eigvecs[0,0]*Bavg[0] + mvab_eigvecs[1,0]*Bavg[1] + mvab_eigvecs[2,0]*Bavg[2], $
	             mvab_eigvecs[0,1]*Bavg[0] + mvab_eigvecs[1,1]*Bavg[1] + mvab_eigvecs[2,1]*Bavg[2], $
	             mvab_eigvecs[0,2]*Bavg[0] + mvab_eigvecs[1,2]*Bavg[1] + mvab_eigvecs[2,2]*Bavg[2]]

	;print the results of MVAB.
	print, '-----------------------------------------------------'
	print, 'MVAB eigenvalues and eigenvectors'
	print, FORMAT='(%"       %s        %s        %s        %s    %s")', '!', 'x', 'y', 'z', '<B> (nT)'
	print, FORMAT='(%"N  %7.2f  %7.4f  %7.4f  %7.4f  %6.2f")', mvab_eigvals[0], mvab_eigvecs[*,0], mvab_Bavg[0]
	print, FORMAT='(%"M  %7.2f  %7.4f  %7.4f  %7.4f  %6.2f")', mvab_eigvals[1], mvab_eigvecs[*,1], mvab_Bavg[1]
	print, FORMAT='(%"L  %7.2f  %7.4f  %7.4f  %7.4f  %6.2f")', mvab_eigvals[2], mvab_eigvecs[*,2], mvab_Bavg[2]


	print, ''
	print, 'Compare with the table in Figure 8.2'
	print, FORMAT='(%"       %s        %s        %s        %s    %s")', '!', 'x', 'y', 'z', '<B> (nT)'
	print, FORMAT='(%"N  %7.2f  %7.4f  %7.4f  %7.4f  %6.2f")',    7.08,  0.8671, -0.4978,  0.0187,  -0.58
	print, FORMAT='(%"M  %7.2f  %7.4f  %7.4f  %7.4f  %6.2f")',  138.01, -0.2886, -0.5326, -0.7954, -23.40
	print, FORMAT='(%"L  %7.2f  %7.4f  %7.4f  %7.4f  %6.2f")', 1012.86, -0.4061, -0.6845,  0.6055,  15.36


	;---------------------------------------------------------------------
	; Maximum Variance Analysis //////////////////////////////////////////
	;---------------------------------------------------------------------
	;Compute the convective electric field: E = -v x B
	E = -[v[1,*]*B[2,*] - v[2,*]*B[1,*], $      ;Ex
	      v[2,*]*B[0,*] - v[0,*]*B[2,*], $      ;Ey
	      v[0,*]*B[1,*] - v[1,*]*B[0,*]]        ;Ez
	E *= 1.0e-3                                 ;convert to mV/m

	;Ma
	mvae_eigvecs = MrMVA(E, EIGENVALUES=mvae_eigvals, /MAXIMUM)

	;
	;Use the normal component of MVAE to adjust the particle velocity into 
	;a frame moving with the discontinuity and do another iteration of MVAE (section 8.4.1-2)
	;E' = E + (un x B) where un is the velocity of the discontinuity in the normal direction
	;and E is the electric field in the spacecraft frame.
	;In section 8.4.2, a value of (u dot n) = -5 is chosen
	;
	un = -5 * mvae_eigvecs[*,0]
	unxB = [un[1]*B[2,*] - un[2]*B[1,*], $      ;unxB_x
	        un[2]*B[0,*] - un[0]*B[2,*], $      ;unxB_y
	        un[0]*B[1,*] - un[1]*B[0,*]]        ;unxB_z

	;calculate the electric field in the frame of the discontinuity. Convert to mV/m
	E_prime = E + unxB * 1.0e-3

	;perform the next iteration of MVAE
	mvae2_eigvecs = MrMVA(E_prime, EIGENVALUES=mvae2_eigvals, /MAXIMUM)

	;Calculate the average field
	Eavg = mean(E, DIMENSION=2)
	Ep_avg = mean(E_prime, DIMENSION=2)

	;and rotate the average values into the new MVA frame
	;Note that the results published in Multi-Spacecraft methods are actually a left-handed
	;coordinate system. To get the same signs, we would have to negate the M component
	mvae_Eavg = [Eavg[0]*mvae_eigvecs[0,0] + Eavg[1]*mvae_eigvecs[1,0] + Eavg[2]*mvae_eigvecs[2,0], $
	             Eavg[0]*mvae_eigvecs[0,1] + Eavg[1]*mvae_eigvecs[1,1] + Eavg[2]*mvae_eigvecs[2,1], $
	             Eavg[0]*mvae_eigvecs[0,2] + Eavg[1]*mvae_eigvecs[1,2] + Eavg[2]*mvae_eigvecs[2,2]]

	mvae2_Eavg = [Ep_avg[0]*mvae2_eigvecs[0,0] + Ep_avg[1]*mvae2_eigvecs[1,0] + Ep_avg[2]*mvae2_eigvecs[2,0], $
	              Ep_avg[0]*mvae2_eigvecs[0,1] + Ep_avg[1]*mvae2_eigvecs[1,1] + Ep_avg[2]*mvae2_eigvecs[2,1], $
	              Ep_avg[0]*mvae2_eigvecs[0,2] + Ep_avg[1]*mvae2_eigvecs[1,2] + Ep_avg[2]*mvae2_eigvecs[2,2]]

	;---------------------------------------------------------------------
	;Print the Results ///////////////////////////////////////////////////
	;---------------------------------------------------------------------
	;create labels for the eigenvectors
	lambda = '!4k!X'
	nlm = ['N', 'M', 'L']

	;print the results of the second iteration of MVAE.
	print, ''
	print, '-----------------------------------------------------'
	print, 'MVAE: Iteration II'
	print, FORMAT='(%"       %s        %s        %s        %s    %s")', '!', 'x', 'y', 'z', '<E> (mV/m)'
	print, FORMAT='(%"N  %7.2f  %7.4f  %7.4f  %7.4f  %6.2f")', mvae2_eigvals[0], mvae2_eigvecs[*,0], mvae2_Eavg[0]
	print, FORMAT='(%"M  %7.2f  %7.4f  %7.4f  %7.4f  %6.2f")', mvae2_eigvals[1], mvae2_eigvecs[*,1], mvae2_Eavg[1]
	print, FORMAT='(%"L  %7.2f  %7.4f  %7.4f  %7.4f  %6.2f")', mvae2_eigvals[2], mvae2_eigvecs[*,2], mvae2_Eavg[2]


	print, ''
	print, 'Compare with the table in Figure 8.10'
	print, FORMAT='(%"       %s        %s        %s        %s    %s")', '!', 'x', 'y', 'z', '<E> (mV/m)'
	print, FORMAT='(%"N  %7.2f  %7.4f  %7.4f  %7.4f  %6.2f")', 17.05,  0.9017, -0.4309,  0.0361,  7.35
	print, FORMAT='(%"M  %7.2f  %7.4f  %7.4f  %7.4f  %6.2f")',  0.50, -0.0627, -0.0476, -0.9969, -0.13
	print, FORMAT='(%"L  %7.2f  %7.4f  %7.4f  %7.4f  %6.2f")',  0.21, -0.4278, -0.9011,  0.0699, -0.03

END


;+
;   Perform a minimum or variance analysis.
;
; :Params:
;       VAR:            in, required, type=int/string/objref
;                       The name, number, or objref of a MrVectorTS variable that contains
;                           the 3-component field to be analyzed.
;
; :Keywords:
;       MAXIMUM:        in. optional. type=boolean. default=0
;                       If set, a maximum variance analysis will be performed. It will
;                           be assumed that `FIELD` is the 3-components of the vector
;                           electric field. An side-effect is that eigenvalues and their
;                           corresponding eigenvectors will be ordered from largest to
;                           smallest. The default is to perform a minimum vairance
;                           analysis.
;       COVARIANCE:     out. optional. type:3x3 MrVariable
;                       Covariance matrix of `VAR` (from which `EIGVECS` are determined).
;       DOUBLE:         in, optional, type=boolean, default=0
;                       If set, computation will be performed in double precision.
;       EIGENVALUES:    out. optional, type=objref
;                       A MrVariable object containing the minimum, intermediate, and
;                           maximum eigenvalues.
;       TEST:           in. optional, type=boolean, default=0
;                       If set, a bench-marking program based on Appendix at the end of
;                           chapter 8 of Sonnerup reference above will be run. All other
;                           keywords are ignored and !Null is returned.
;
; :Returns:
;       OEIGVEC:        out, required, type=3x3 objref
;                       The minimum, intermediate, and maximum normalized eigenvectors
;                           of the covariant matrix, as described above. Eigvec[*,0]
;                           corresponds to eigval[0], etc.
;-
FUNCTION MrVar_MVA, var, $
COVARIANCE=oM, $
DOUBLE=double, $
EIGENVALUES=oEigVals, $
MAXIMUM=maximum, $
TEST=test
	Compile_Opt idl2
	On_Error, 2
	
	;Test program
	IF Keyword_Set(test) THEN BEGIN
		MrVar_MVA_Test
		RETURN, !Null
	ENDIF
	
	;Get the variable
	oVar = MrVar_Get(var)
	IF ~IsA(oVar, 'MrVectorTS') THEN Message, 'VAR must be a MrVectorTS object.'

	;Check whether to perform minimum or maximum variance. default is minimum variance
	;Arrange the eigenvalues in ascending order for MVAB and descending order for MVAE
	ascending = ~Keyword_Set(maximum)
	
	;Return the covariance matrix?
	tf_m = Arg_Present(oM)
	
;-------------------------------------------
; Min/Max Variance Analsysis ///////////////
;-------------------------------------------
	
	;compute the covariant matrix for the magnetic field
	;then find the eigenvalues and eigenvectors of the covariance matrix
	M           = Correlate( Transpose(oVar['DATA']), /COVARIANCE, DOUBLE=double)
	eigenvalues = EigenQL(M, EIGENVECTORS=eigenvectors, DOUBLE=double, ASCENDING=ascending)

	;The eigenvectors are stored in rows. 
	;If the x component of N is negative, make it positive to correspond with +X-GSE.
	;If the z component of L is negative, make it positive to correspond with +Z-GSE.
	IF eigenvectors[0,0] LT 0 THEN eigenvectors[*,0] = -eigenvectors[*,0]
	IF eigenvectors[2,2] LT 0 THEN eigenvectors[*,2] = -eigenvectors[*,2]
	
	;If the determinant of EIGVECS is negative, this indicates a left-handed coordinate
	;system. To remedy this, take the negative of the M-component
	IF Determ(eigenvectors) LT 0 THEN eigenvectors[*,1] = -eigenvectors[*,1]
	
;-------------------------------------------
; Create Variables /////////////////////////
;-------------------------------------------
	
	;Create variables
	oEigVec = MrVariable( eigenvectors, $
	                      NAME = 'MVA(' + oVar.name + ')', $
	                      /NO_COPY )
	
	oEigVals = MrVariable( eigenvalues, $
	                       NAME = 'MVA_EigVals(' + oVar.name + ')', $
	                       /NO_COPY )
	
	IF tf_m THEN BEGIN
		oM = MrVariable( M, $
		                 NAME = 'MVA_CovarMat(' + oVar.name + ')', $
		                 /NO_COPY )
	ENDIF
	
;-------------------------------------------
; Add Attributes ///////////////////////////
;-------------------------------------------
	IF Keyword_Set(maximum) THEN BEGIN
		vecdesc = 'Eigenvectors of a maximum variance analysis. Rows 0, 1, and 2 ' + $
		          'represent the maximum (N), intermediate (M), and minimum (L) ' + $
		          'variance directions. The eigenvectors are oriented in the NML sense ' + $
		          '(not the traditional LMN sense) so that the N, M, and L directions ' + $
		          'align with GSE X, Y, and Z directions at the subsolar magnetopause.'
		valdesc = 'Eigenvalues of a maximum variance analysis. Elements 0, 1, and 2 ' + $
		          'correspond to the maximum (N), intermediate (M), and minimum (L) ' + $
		          'variance directions.' 
	ENDIF ELSE BEGIN
		vecdesc = 'Eigenvectors of a minimum variance analysis. Rows 0, 1, and 2 ' + $
		          'represent the minimum (N), intermediate (M), and maximum (L)' + $
		          'variance directions. The eigenvectors are oriented in the NML sense ' + $
		          '(not the traditional LMN sense) so that the N, M, and L directions ' + $
		          'align with GSE X, Y, and Z directions at the subsolar magnetopause.'
		valdesc = 'Eigenvectors of a minimum variance analysis. Rows 0, 1, and 2 ' + $
		          'correspond to the minimum (N), intermediate (M), and maximum (L)' + $
		          'variance directions.'
	ENDELSE
	
	;Eigen vectors
	oEigVec['CATDESC']    = vecdesc
	oEigVec['LABEL']      = ['N', 'M', 'L']
	oEigVec['LABL_PTR_1'] = ['N', 'M', 'L']
	oEigVec['LABL_PTR_2'] = ['X', 'Y', 'Z']
	
	;Eigenvalues
	oEigVals['CATDESC'] = valdesc
	oEigVals['LABEL']   = ['N', 'M', 'L']
	
	;Covariance matrix
	IF tf_m THEN BEGIN
		oM['CATDESC']    = 'Covariance matrix of the variance analysis.'
		oM['LABL_PTR_1'] = ['X', 'Y', 'Z']
		oM['LABL_PTR_2'] = ['X', 'Y', 'Z']
	ENDIF
	
	RETURN, oEigVec
END