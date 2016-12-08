; docformat = 'rst'
;
; NAME:
;   MrTimeVar__Define
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
;   The purpose of this class is subclass the IDL_Object such that all _Overload*
;   operations behave as they would for a normal array.
;
;   _OVERLOAD:
;       An operator (such as AND, EQ, ^, *, etc.) calls the overload
;       method for the object on the left side first. So, for two MrVariable objects,
;       A and B::
;
;               print, A [operator] B
;
;       will call A's _Overload method first.
;
;   ARRAY TRUNCATION
;       If two arrays are compared, results will be truncated to have the same number
;       of elements as the shorter array. If an array and a scalar are compared, each
;       elements of the array is compared against the scalar value. This is the same
;       as for normal IDL variables.
;
;   CACHING
;       The ::Cache method will cache the MrVariable object. When the object is destroyed,
;       it will automatically be removed from the cache.
;
; :Categories:
;   MrVariable, Graphics
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
;       2016/05/27  -   Written by Matthew Argall
;       2016/10/22  -   Convert to and from Julday and SSM. - MRA
;-
;*****************************************************************************************
;+
;   The initialization method.
;
; :Params:
;       TIME:               in, required, type=depends
;                           Array of time values.
;       TYPE:               in, optional, type=array, default='ISO-8601'
;                           Units of the time array.
;
; :Keywords:
;       NO_COPY:            in, optional, type=boolean, default=0
;                           If set, `DATA` will be copied directly into the object and
;                               will be left undefined.
;       T_REF:              in, optional, type=string, default=''
;                           A reference time from which `TIME` is measured. Applicable
;                               only to certain values of `TYPE`. Must be formatted as
;                               an ISO-8601 string.
;-
function MrTimeVar::INIT, time, type, $
CACHE=cache, $
NAME=name, $
NO_CLOBBER=no_clobber, $
NO_COPY=no_copy, $
T_REF=t_ref
	compile_opt idl2

	;Error handling
	catch, the_error
	if the_error ne 0 then begin
		catch, /CANCEL
		MrPrintF, 'LogErr'
		return, 0
	endif
	
	;Defaults
	if n_elements(name) eq 0 then name = 'MrTimeVar'
	
	;Initialize superclass
	success = self -> MrVariable::Init( CACHE      = cache, $
	                                    NAME       = name, $
	                                    NO_CLOBBER = no_clobber )
	if success eq 0 then return, 0

	;Set time
	if n_elements(time) gt 0 then self -> SetData, time, type, NO_COPY=no_copy, T_REF=t_ref

	return, 1
end


;+
;   Allow square-bracket array indexing from the right side of an operator.
;
; :Params:
;       ISRANGE:            in, required, type=intarr
;                           A vector that has one element for each Subscript argument
;                               supplied by the user; each element contains a zero if the
;                               corresponding input argument was a scalar index value or
;                               array of indices, or a one if the corresponding input
;                               argument was a subscript range.
;       I1:                 in, required, type=integer/intarr(3)
;                           Index subscripts. Either a scalar, an index array, or a 
;                               subscript range in the form [start, stop, step_size]
;       I2:                 in, optional, type=string/intarr(3)
;                           An index range used in conjunction with IDL's StrMid() function
;                               to return a substring of the time array, or a time type
;                               accepted by the ::fromISO method, which will return time
;                               as the indicated type.
;
; :Returns:
;       RESULT:             out, required, type=numeric array
;                           The subarray accessed by the input parameters.
;-
function MrTimeVar::_OverloadBracketsRightSide, isRange, i1, i2
	compile_opt idl2
	on_error, 2

	;Number of subscripts given
	nSubscripts = n_elements(isRange)

	;String operations.
	if IsA(i1, /SCALAR, 'STRING') then begin
		case strupcase(i1) of
			'DATA':    return, *self.data
			'POINTER': return,  self.data
			'PTR':     return,  self.data
			else:      return,  self -> GetAttrValue(i1)
		endcase

	;Scalar operations
	;   - 0   returns the self object
	;   - [0] returns the first data element
	;   - All other cases return data
	endif else if nSubscripts eq 1 && isRange[0] eq 0 && IsA(i1, /SCALAR) && i1 eq 0 then begin
		return, self
	
	;Two indices
	;   - Must be defined and a scalar string
	endif

;---------------------------------------------------------------------
; 1D Subscripts ///////////////////////////////////////////////////////
;---------------------------------------------------------------------
	;Range or index?
	if isRange[0] $
		then time = (*self.data)[i1[0]:i1[1]:i1[2]] $
		else time = (*self.data)[i1] 

;---------------------------------------------------------------------
; Convert Type ///////////////////////////////////////////////////////
;---------------------------------------------------------------------
	if nSubscripts eq 2 then begin
		if isRange[1] then begin
			time = strmid(time, i2[0], i2[1]-i2[0]+1)
		endif else begin
			if IsA(i2, 'STRING') $
				then time = self -> fromISO(time, i2) $
				else message, 'The second subscript must be a time type or subscript range.'
		endelse
	endif

	return, time
end


;+
;   Clean up after the object is destroyed
;-
pro MrTimeVar::CLEANUP
	compile_opt idl2
	on_error, 2
	
	self -> MrVariable::Cleanup
end


;+
;   Convert ISO-8601 time to Julian day (begins at noon) via IDL's
;   JULDAY function.
;
; :Params:
;       ISO:            in, required, type=strarr
;                       ISO times to be converted to TT2000 format.
;
; :Returns:
;       JUL:            out, required, type=int64
;                       The Julian Date for the specified date.
;-
function MrTimeVar::iso2julday, iso
	compile_opt idl2
	on_error, 2
	
	;Make sure time is properly formatted
	tpattern = '%Y-%M-%dT%H:%m:%S%f'
	if ~MrTokens_IsMatch(iso[0], tpattern) $
		then message, 'Input time is not a recognized ISO format.'
	
	;Compute TT2000 times
	;   - StrMid is ~100x faster than MrTimeParser
	;   - Requires implicit array to be ISO time
	jul = julday( fix(strmid(iso,  5, 2)), $      ;month
	              fix(strmid(iso,  8, 2)), $      ;day
	              fix(strmid(iso,  0, 4)), $      ;year
	              fix(strmid(iso, 11, 2)), $      ;hour
	              fix(strmid(iso, 14, 2)), $      ;minute
	              double(strmid(iso, 17))  $      ;second
	            )
	
	;Return the array
	return, jul
end


;+
;   Convert ISO-8601 time to seconds since midnight (SSM).
;
; :Params:
;       ISO:                in, required, type=strarr
;                           ISO times to be converted to seconds since midnight.
;       REF_DATE:           out, optional, type=string
;                           Reference date of midnight, taken as the date of the
;                               first point in `ISO`.
;
; :Returns:
;       T_SSM:              out, required, type=int64
;                           Time, in seconds since midnight
;-
function MrTimeVar::iso2ssm, iso, ref_date
	compile_opt idl2
	on_error, 2
	
	;Make sure time is properly formatted
	tpattern = '%Y-%M-%dT%H:%m:%S%f'
	if ~MrTokens_IsMatch(iso[0], tpattern) $
		then message, 'Input time is not a recognized ISO format.'
	
	;Reference date
	;   - Make sure it is scalar
	t_ref = n_elements(ref_date) eq 0 ? strmid(iso[0], 0, 10) : ref_date[0]

	;This is ~100x faster than MrTimeParser
	;   - Requires implicit array to be ISO time
	yr   = fix(strmid(iso,  0, 4))
	mo   = fix(strmid(iso,  5, 2))
	day  = fix(strmid(iso,  8, 2))
	hour = double(strmid(iso, 11, 2))
	mnt  = double(strmid(iso, 14, 2))
	sec  = double(strmid(iso, 17, 2))
;	dec  = double(strmid(iso, 19, strlen(iso[0])-20)) ;Capture the decimal, but not the time zone
	dec  = double(strmid(iso, 19)) ;Capture the decimal, but not the time zone
	
	;Calculate SSM
	t_ssm = hour*3600D + mnt*60D + sec + dec
	
	;Add number of days since the reference date
	yr0    = fix(strmid(t_ref, 0, 4))
	mo0    = fix(strmid(t_ref, 5, 2))
	day0   = fix(strmid(t_ref, 8, 2))
	nDays  = julday(mo, day, yr) - julday(mo0, day0, yr0)
	t_ssm += nDays*86400D
	
	;Return the array
	return, t_ssm
end


;+
;   Convert ISO-8601 time to CDF_TIME_TT2000 format.
;
; :Params:
;       ISO:                in, required, type=strarr
;                           ISO times to be converted to TT2000 format.
;
; :Returns:
;       TT2000:             out, required, type=int64
;                           TT2000 values corresponding to input times.
;-
function MrTimeVar::iso2tt2000, iso
	compile_opt idl2
	on_error, 2
	
	;Make sure time is properly formatted
	tpattern = '%Y-%M-%dT%H:%m:%S%f'
	if ~MrTokens_IsMatch(iso[0], tpattern) $
		then message, 'Input time is not a recognized ISO format.'
	
	;Provide enough decimal places
	;   - 00.1 would turn into 00.001 without two trailing zeros.
	seconds = strmid(iso, 17)
	if strlen(seconds[0]) le 2 then seconds += '.0'
	seconds += '000000000'
	
	;Compute TT2000 times
	;   - StrMid is ~100x faster than MrTimeParser
	;   - Requires implicit array to be ISO time
	cdf_tt2000, tt2000, $
	            fix(strmid(iso,  0, 4)), $      ;year
	            fix(strmid(iso,  5, 2)), $      ;month
	            fix(strmid(iso,  8, 2)), $      ;day
	            fix(strmid(iso, 11, 2)), $      ;hour
	            fix(strmid(iso, 14, 2)), $      ;minute
	            fix(strmid(seconds, 0, 2)), $   ;second
	            fix(strmid(seconds, 3, 3)), $   ;milli
	            fix(strmid(seconds, 6, 3)), $   ;micro
	            fix(strmid(seconds, 9, 3)), $   ;nano
	            /COMPUTE_EPOCH

	;Return the array
	return, tt2000
end


;+
;   Convert ISO-8601 time to CDF_EPOCH16 format.
;
; :Params:
;       ISO:                in, required, type=strarr
;                           ISO times to be converted to TT2000 format.
;
; :Returns:
;       CDF_EPOCH16:        out, required, type=dcomplex
;                           CDF_EPOCH16 values corresponding to input times.
;-
function MrTimeVar::iso2epoch16, iso
	compile_opt idl2
	on_error, 2
	
	;Make sure time is properly formatted
	tpattern = '%Y-%M-%dT%H:%m:%S%f'
	if ~MrTokens_IsMatch(iso[0], tpattern) $
		then message, 'Input time is not a recognized ISO format.'
	
	;Compute EPOCH16 times
	;   - StrMid is ~100x faster than MrTimeParser
	;   - Requires implicit array to be ISO time
	cdf_epoch16, epoch16, $
	             fix(strmid(iso,  0, 4)), $     ;year
	             fix(strmid(iso,  5, 2)), $     ;month
	             fix(strmid(iso,  8, 2)), $     ;day
	             fix(strmid(iso, 11, 2)), $     ;hour
	             fix(strmid(iso, 14, 2)), $     ;minute
	             fix(strmid(iso, 17, 2)), $     ;second
	             fix(strmid(iso, 20, 3)), $     ;milli
	             fix(strmid(iso, 23, 3)), $     ;micro
	             fix(strmid(iso, 26, 3)), $     ;nano
	             fix(strmid(iso, 29, 3)), $     ;pico
	             /COMPUTE_EPOCH

	;Return the array
	return, epoch16
end


;+
;   Convert ISO-8601 time to CDF_EPOCH format.
;
; :Params:
;       ISO:                in, required, type=strarr
;                           ISO times to be converted to TT2000 format.
;
; :Returns:
;       CDF_EPOCH:          out, required, type=dcomplex
;                           CDF_EPOCH values corresponding to input times.
;-
function MrTimeVar::iso2epoch, iso
	compile_opt idl2
	on_error, 2
	
	;Make sure time is properly formatted
	tpattern = '%Y-%M-%dT%H:%m:%S%f'
	if ~MrTokens_IsMatch(iso[0], tpattern) $
		then message, 'Input time is not a recognized ISO format.'
	
	;Compute EPOCH16 times
	;   - StrMid is ~100x faster than MrTimeParser
	;   - Requires implicit array to be ISO time
	cdf_epoch, epoch, $
	           fix(strmid(iso,  0, 4)), $      ;year
	           fix(strmid(iso,  5, 2)), $      ;month
	           fix(strmid(iso,  8, 2)), $      ;day
	           fix(strmid(iso, 11, 2)), $      ;hour
	           fix(strmid(iso, 14, 2)), $      ;minute
	           fix(strmid(iso, 17, 2)), $      ;second
	           fix(strmid(iso, 20, 3)), $      ;milli
	           /COMPUTE_EPOCH
	
	;Return the array
	return, epoch
end


;+
;   Convert CDF_EPOCH time to ISO-8601 format.
;
; :Params:
;       CDF_EPOCH:          in, required, type=dcomplex
;                           CDF_EPOCH values to be converted to ISO format.
;
; :Returns:
;       ISO:                out, required, type=strarr
;                           ISO times corresponding to input times.
;-
function MrTimeVar::epoch2iso, epoch
	compile_opt idl2
	on_error, 2
	
	;Breakdown the epoch time
	cdf_epoch, epoch, yr, mo, day, hr, mnt, sec, milli, /BREAKDOWN_EPOCH

	;Format ISO string
	iso = string(yr,    FORMAT='(i04)') + '-' + $
	      string(mo,    FORMAT='(i02)') + '-' + $
	      string(day,   FORMAT='(i02)') + 'T' + $
	      string(hr,    FORMAT='(i02)') + ':' + $
	      string(mnt,   FORMAT='(i02)') + ':' + $
	      string(sec,   FORMAT='(i02)') + '.' + $
	      string(milli, FORMAT='(i03)') + 'Z'
	
	;Return the array
	return, iso
end


;+
;   Convert CDF_EPOCH16 time to ISO-8601 format.
;
; :Params:
;       CDF_EPOCH16:        in, required, type=dcomplex
;                           CDF_EPOCH16 values to be converted to ISO format.
;
; :Returns:
;       ISO:                out, required, type=strarr
;                           ISO times corresponding to input times.
;-
function MrTimeVar::Epoch16toISO, epoch16
	compile_opt idl2
	on_error, 2
	
	;Breakdown the epoch time
	cdf_epoch16, epoch16, yr, mo, day, hr, mnt, sec, milli, micro, nano, pico, /BREAKDOWN_EPOCH

	;Format ISO string
	iso = string(yr,    FORMAT='(i04)') + '-' + $
	      string(mo,    FORMAT='(i02)') + '-' + $
	      string(day,   FORMAT='(i02)') + 'T' + $
	      string(hr,    FORMAT='(i02)') + ':' + $
	      string(mnt,   FORMAT='(i02)') + ':' + $
	      string(sec,   FORMAT='(i02)') + '.' + $
	      string(milli, FORMAT='(i03)') + $
	      string(micro, FORMAT='(i03)') + $
	      string(nano,  FORMAT='(i03)') + $
	      string(pico,  FORMAT='(i03)') + 'Z'
	
	;Return the array
	return, iso
end


;+
;   Convert Julian day to ISO-8601 time via IDL's CALDAT procedure.
;
; :Params:
;       ISO:            in, required, type=strarr
;                       ISO times to be converted to TT2000 format.
;
; :Returns:
;       JUL:            out, required, type=int64
;                       The Julian Date for the specified date.
;-
function MrTimeVar::julday2iso, jul
	compile_opt idl2
	on_error, 2
	
	;Parse the julian date
	caldat, jul, month, day, year, hour, minute, second
	
	;Create ISO time
	;   - Keep milliseconds
	iso = string(year,   FORMAT='(i04)') + '-' + $
	      string(month,  FORMAT='(i02)') + '-' + $
	      string(day,    FORMAT='(i02)') + 'T' + $
	      string(hour,   FORMAT='(i02)') + ':' + $
	      string(minute, FORMAT='(i02)') + ':' + $
	      string(second, FORMAT='(f06.3)') + 'Z'
	
	;Return the array
	return, iso
end


;+
;   Convert Julian day to ISO-8601 time via IDL's CALDAT procedure.
;
; :Params:
;       SSM:            in, required, type=double
;                       Times in seconds since midnight.
;       T_REF:          in, required, type=string
;                       ISO date from which `SSM` is referenced.
;
; :Returns:
;       ISO:            out, required, type=strarr
;                       ISO times.
;-
function MrTimeVar::ssm2iso, ssm, t_ref
	compile_opt idl2
	on_error, 2
	
	;Make sure time is properly formatted
	tpattern = '%Y-%M-%d'
	if ~MrTokens_IsMatch(t_ref, tpattern) $
		then message, 'Input time is not a recognized ISO format.'
	
	;Get the reference time
	iso_ref = strmid(t_ref[0], 0, 10)
	
	;Compute time from seconds
	hour   = fix(ssm / 3600.0)
	minute = fix((ssm mod 3600.0) / 60.0)
	second = ssm mod 60.0
	
	;Create iso time
	iso = iso_ref + 'T' + $
	      string(hour,   FORMAT='(i02)')     + ':' + $
	      string(minute, FORMAT='(i02)')     + ':' + $
	      string(second, FORMAT='(f013.10)') + 'Z'
	
	;Return the array
	return, iso
end


;+
;   Convert CDF_TIME_TT2000 time to ISO-8601 format.
;
; :Params:
;       TT2000:             out, required, type=int64
;                           TT2000 values to be converted to ISO format.
;
; :Returns:
;       ISO:                in, required, type=strarr
;                           ISO values corresponding to input times.
;-
function MrTimeVar::TT2000toISO, tt2000
	compile_opt idl2
	on_error, 2
	
	;Breakdown the epoch time
	cdf_tt2000, tt2000, yr, mo, day, hr, mnt, sec, milli, micro, nano, /BREAKDOWN_EPOCH, /TOINTEGER

	;Format ISO string
	iso = string(yr,    FORMAT='(i04)') + '-' + $
	      string(mo,    FORMAT='(i02)') + '-' + $
	      string(day,   FORMAT='(i02)') + 'T' + $
	      string(hr,    FORMAT='(i02)') + ':' + $
	      string(mnt,   FORMAT='(i02)') + ':' + $
	      string(sec,   FORMAT='(i02)') + '.' + $
	      string(milli, FORMAT='(i03)') + $
	      string(micro, FORMAT='(i03)') + $
	      string(nano,  FORMAT='(i03)') + 'Z'

	;Return the array
	return, iso
end


;+
;   Concatenate an array, or a series of arrays, to the implicit array.
;
; :Params:
;       DATA:           in, required, type=Nx1 or 1xN array
;                       Array to be concatenated to the implicit array. If undefined or
;                           !Null, then nothing is appended.
;       TYPE:           in, optional, type=array, default='ISO-8601'
;                       Units of the input time array. See ::toISO for options.
;
; :Keywords:
;       BEFORE:         in, optional, type=boolean, default=0
;                       If set, `DATA` will be appended to the beginning of the implicit
;                           array. The default is to append to the end.
;       NO_COPY:        in, optional, type=boolean, default=0
;                       If set, `DATA` will be copied directly into the object and
;                           will be left undefined.
;-
pro MrTimeVar::Append, data, type, $
BEFORE=before, $
NO_COPY=no_copy
	compile_opt idl2
	on_error, 2
	
	;Allow input data to be empty. In this case, do nothing.
	if n_elements(data) eq 0 then return

	;Defaults
	before  = keyword_set(before)
	no_copy = keyword_set(no_copy)

	;Is the object empty?
	if n_elements(*self.data) eq 0 then begin
		self -> SetData, data, type, NO_COPY=no_copy
	
	;Append
	endif else begin
		;Must convert new data to ISO format
		if no_copy $
			then iso_time = self -> toISO(temporary(data), type, TOKEN_FMT=token_fmt) $
			else iso_time = self -> toISO(data, type, token_fmt)
		
		;Concatenate the data
		if before eq 1 $
			then self -> SetData, [temporary(iso_time), *self.data], 'ISO-8601' $
			else self -> SetData, [*self.data, temporary(iso_time)], 'ISO-8601'
	endelse
end


;+
;   Identify the type of time-string given.
;
; :Params:
;       TIME:           in, required, type=string/strarr
;                       The time string to be identified. All elements are assumed to
;                           be formatted identically.
;       CAN_CONVERT:    out, optional, type=boolean
;                       Returns 1 if `TIME` can be converted to the desired ISO-8601
;                           time format: '%Y-%M-%dT%H:%m:%S%f'. Returns 0 otherwise.
;
; :Returns:
;       TOKEN_FMT:      in, optional, type=boolean, default=0
;                       A MrTokens string identifying how `TIME` can be parsed.
;-
function MrTimeVar::IdentifyType, time
	compile_opt idl2
	on_error, 2

	token_fmt   = ''

	;Choices
	test_fmt = [ '%Y-%M-%d', $                  ;Date
	             '%Y-%M-%d/%H:%m:%S', $         ;SPEDAS format, UTC
	             '%Y-%M-%dT%H:%m:%S', $         ;ISO, UTC assumed
	             '%Y-%M-%dT%H:%m:%S%z', $       ;ISO, time zone given (typically "Z" for UTC)
	             '%Y-%M-%dT%H:%m:%S%f', $       ;ISO w/ fraction of second
	             '%Y-%M-%dT%H:%m:%S%f%z', $     ;ISO w/ fraction of a second & time zone
	             '%w %c %d %H:%m:%S %Y', $      ;Format returned by IDL's SysTime() -- Does not work for single digit days!
	             '%H:%m:%S' $                   ;Time
	           ]
	
	;Step through each type
	tf_pass = 0B
	i       = 0
	while ~tf_pass && i lt n_elements(test_fmt) do begin
		tf_pass = MrTokens_IsMatch(time[0], test_fmt[i], /EXACT)
		i      += 1
	endwhile
	
	;Get the token format
	if tf_pass then token_fmt = test_fmt[i-1]

	return, token_fmt
end


;+
;   Get time array data.
;
;   CALLING SEQUENCE
;       time = oTime -> GetData(type)
;       time = oTime -> GetData('CUSTOM', time_fmt)
;
; :Params:
;       TYPE:           in, optional, type=array, default='ISO-8601'
;                       Specify to output `TIME` in one of the following formats::
;                           'CDF_EPOCH'       - CDF Epoch values (milliseconds)
;                           'CDF_EPOCH16'     - CDF Epoch16 values (picoseconds)
;                           'CDF_EPOCH_LONG'  - CDF Epoch16 values (picoseconds)
;                           'CDF_TIME_TT2000' - CDF TT2000 values (nanoseconds)
;                           'TT2000'          - CDF TT2000 values (nanoseconds)
;                           'JULDAY'          - Julian date
;                           'ISO-8601'        - ISO-8601 formatted string
;                           'SSM'             - Seconds since midnight
;                           'CUSTOM'          - Custom time string format
;       T_REF:          in, optional, type=array, default=first element of time array
;                       Reference time. Ignored unless `TYPE` is 'SSM'
;
; :Keywords:
;       DESTROY:        in, optional, type=boolean, default=0
;                       If set, the object will be destroyed after returning the array.
;       NO_COPY:        in, optional, type=boolean, default=0
;                       If set, the array will be removed frome the object and return.
;                           This prevents two copies of the array from being in
;                           memory.
;       TOKEN_FMT:      in, optional, type=string
;                       The MrTokens pattern that specifies how `TIME` should be
;                           output. If provided and `TYPE` is undefined, automatically
;                           sets `TYPE` = 'CUSTOM'.
;
; :Returns:
;       TIME:           out, required, type=array
;                       Array of time values to be stored.
;-
function MrTimeVar::GetData, type, t_ref, $
DESTROY=destroy, $
NO_COPY=no_copy, $
TOKEN_FMT=token_fmt
	compile_opt idl2
	on_error, 2
	
	;Output type
	if n_elements(type) eq 0 then begin
		if n_elements(token_fmt) eq 0 $
			then type = 'ISO-8601' $
			else type = 'CUSTOM'
	endif

	;Convert to ISO-8601
	case strupcase(type) of
		'CDF_TIME_TT2000': time = self -> iso2tt2000(*self.data)
		'CDF_EPOCH':       time = self -> iso2epoch(*self.data)
		'CDF_EPOCH16':     time = self -> iso2epoch16(*self.data)
		'CDF_EPOCH_LONG':  time = self -> iso2epoch16(*self.data)
		'JULDAY':          time = self -> iso2julday(*self.data)
		'SSM':             time = self -> iso2ssm(*self.data, t_ref)
		'TT2000':          time = self -> iso2tt2000(*self.data)
		'ISO-8601':        time = *self.data
		'CUSTOM':          MrTimeParser, *self.data, self.token_format, token_fmt, time
		else:              message, 'Unknown time type: "' + type + '".' ;MrTimeParser, time, type, '%Y-%M-%dT%H:%m:%S%z', iso_time
	endcase

	;Destroy the data and the object?
	if keyword_set(destroy) then begin
		obj_destroy, self
	
	;Free memory
	endif else if keyword_set(no_copy) then begin
		ptr_free, self.data
		self.data = ptr_new(/ALLOCATE_HEAP)
	endif
	
	;Return the array
	return, time
end


;+
;   Get the median sampling interval of the implicit array.
;
; :Params:
;       TYPE:           in, optional, type=string, default='SSM'
;                       Specify the units in which the sampling interval is computed::
;                           'CDF_EPOCH'       - CDF Epoch values (milliseconds)
;                           'CDF_EPOCH16'     - CDF Epoch16 values (picoseconds)
;                           'CDF_EPOCH_LONG'  - CDF Epoch16 values (picoseconds)
;                           'CDF_TIME_TT2000' - CDF TT2000 values (nanoseconds)
;                           'TT2000'          - CDF TT2000 values (nanoseconds)
;                           'JULDAY'          - Julian date
;                           'ISO-8601'        - ISO-8601 formatted string
;                           'SSM'             - Seconds since midnight
;                           'CUSTOM'          - Custom time string format
;
; :Returns:
;       SI:             out, required, type=number
;                       Median sampling interval. Units are those implied by `TYPE`.
;-
function MrTimeVar::GetSI, type
	compile_opt idl2
	on_error, 2
	
	;Default units
	if n_elements(type) eq 0 then type = 'SSM'
	
	;Compute sampling interval
	t  = self -> GetData(type)
	si = median(t[1:*] - t)
	
	;Return the array
	return, si
end


;+
;   Set the time array.
;
; :Params:
;       TIME:               in, required, type=array
;                           Array of values to be stored, or a MrVariable object whose
;                               array is to be copied.
;       TYPE:               in, optional, type=array, default='ISO-8601'
;                           Units of the time array. Accepted values are::
;                               'CDF_EPOCH'       - CDF Epoch values (milliseconds)
;                               'CDF_EPOCH16'     - CDF Epoch16 values (picoseconds)
;                               'CDF_EPOCH_LONG'  - CDF Epoch16 values (picoseconds)
;                               'CDF_TIME_TT2000' - CDF TT2000 values (nanoseconds)
;                               'TT2000'          - CDF TT2000 values (nanoseconds)
;                               'ISO-8601'        - ISO-8601 formatted string
;                               'CUSTOM'          - Custom time string format
;
; :Keywords:
;       NO_COPY:            in, optional, type=boolean, default=0
;                           If set `DATA` will be copied directly into the object
;                               and will be left undefined (a MrVariable object will not
;                               be destroyed, but its array will be empty).
;       T_REF:              in, optional, type=string, default=''
;                           A reference time from which `TIME` is measured. Applicable
;                               only to certain values of `TYPE`. Must be formatted as
;                               an ISO-8601 string.
;       TOKEN_FMT:          in, optional, type=string, default='%Y-%M-%dT%H:%m:%S%z'
;                           The MrTokens pattern that allows `TIME` to be broken down.
;                               If provided and `TYPE` is undefined, automatically sets
;                               `TYPE` = 'CUSTOM'. Ignored if `TIME` is not a string.
;-
pro MrTimeVar::SetData, time, type, $
NO_COPY=no_copy, $
T_REF=t_ref
	compile_opt idl2
	on_error, 2

;-----------------------------------------------------
; Check Inputs \\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\
;-----------------------------------------------------
	no_copy = keyword_set(no_copy)
	if ~MrIsA(time, /ROW) && ~MrIsA(time, /COLUMN) then message, 'TIME must be a Nx1 or 1xN.'

;-----------------------------------------------------
; Take Time from MrVariable Object \\\\\\\\\\\\\\\\\\\
;-----------------------------------------------------
	if IsA(time, 'OBJREF') then begin
		if obj_isa(time, 'MrTimeVar') $
			then iso_time = time -> GetData(NO_COPY=no_copy) $
			else message, 'Only "MrTimeVar" objects can be given.'

;-----------------------------------------------------
; Convert to Time String \\\\\\\\\\\\\\\\\\\\\\\\\\\\\
;-----------------------------------------------------
	endif else begin
		;Times-strings
		if n_elements(type) eq 0 then begin
			if size(time, /TNAME) eq 'STRING' then begin
				;Try to convert to the desired format
				token_fmt = '%Y-%M-%dT%H:%m:%S%f'
				tf_iso    = MrTokens_IsMatch(time[0], token_fmt)

				;Is it ISO?
				if tf_iso then begin
					type = 'ISO-8601'
			
				;If not, identify types
				endif else begin
					;Identify type
					type = self -> IdentifyType(time[0])
					if type eq '' then message, 'Unrecognized time format.'
				endelse
			endif else begin
				message, 'TIME units unknown. Please provide TYPE.'
			endelse
		endif

		;Convert to ISO
		if type eq 'ISO-8601' then begin
			iso_time = keyword_set(no_copy) ? temporary(time) : time
		endif else begin
			if keyword_set(no_copy) $
				then iso_time = self -> toISO(temporary(time), type, T_REF=t_ref) $
				else iso_time = self -> toISO(time, type, T_REF=t_ref)
		endelse
	endelse
;-----------------------------------------------------
; Finish Up \\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\
;-----------------------------------------------------

	;Ensure dimensionality
	sz = size(iso_time)
	if (sz[0] eq 2 && sz[1] gt 1) || sz[0] ge 3 then message, 'TIME must be a column or row vector.'

	;Ensure row vector
	if sz[0] eq 2 && sz[1] eq 1 $
		then *self.data = reform(temporary(iso_time)) $
		else *self.data = temporary(iso_time)
	
	;Set format
	self.token_format = type
end


;+
;   Convert ISO-8601 time strings to another time convetion.
;
; :Params:
;       TYPE:               in, required, type=string
;                           Units of the time array. Accepted values are::
;                               'CDF_EPOCH'       - CDF Epoch values (milliseconds)
;                               'CDF_EPOCH16'     - CDF Epoch16 values (picoseconds)
;                               'CDF_EPOCH_LONG'  - CDF Epoch16 values (picoseconds)
;                               'CDF_TIME_TT2000' - CDF TT2000 values (nanoseconds)
;                               'TT2000'          - CDF TT2000 values (nanoseconds)
;                               'ISO-8601'        - ISO-8601 formatted string
;
; :Returns:
;       ISO_TIME:           out, required, type=strarr
;                           Time array of the requested datatype.
;-
function MrTimeVar::fromISO, iso_time, type
	compile_opt idl2
	on_error, 2

	;Convert to ISO
	case strupcase(type) of
		'CDF_EPOCH':       time  = self -> iso2epoch(iso_time)
		'CDF_EPOCH16':     time  = self -> iso2epoch16(iso_time)
		'CDF_EPOCH_LONG':  time  = self -> iso2epoch16(iso_time)
		'CDF_TIME_TT2000': time  = self -> iso2tt2000(iso_time)
		'JULDAY':          time  = self -> iso2julday(iso_time)
		'SSM':             time  = self -> iso2ssm(iso_time, t_ref)
		'TT2000':          time  = self -> iso2tt2000(iso_time)
		'ISO-8601':        time  = iso_time
		else: message, 'Unknown time type: "' + type + '".'
	endcase
	
	;Return the array
	return, time
end


;+
;   Convert a time variable to ISO-8601 format.
;
; :Params:
;       TIME:               in, required, type=array
;                           Times to be converted to ISO-8601 format.
;       TYPE:               in, required, type=string
;                           Units of the time array. Accepted values are::
;                               'CDF_EPOCH'       - CDF Epoch values (milliseconds)
;                               'CDF_EPOCH16'     - CDF Epoch16 values (picoseconds)
;                               'CDF_EPOCH_LONG'  - CDF Epoch16 values (picoseconds)
;                               'CDF_TIME_TT2000' - CDF TT2000 values (nanoseconds)
;                               'TT2000'          - CDF TT2000 values (nanoseconds)
;                               'SSM'             - Seconds since midnight
;                               'ISO-8601'        - ISO-8601 formatted string
;                               'CUSTOM'          - Custom time string format
;
; :Keywords:
;       T_REF:              in, optional, type=string, default=''
;                           A reference time from which `TIME` is measured. Applicable
;                               only to certain values of `TYPE`. Must be formatted as
;                               an ISO-8601 string.
;       TOKEN_FMT:          out, optional, type=string
;                           MrTokens pattern identifying how to parse the results.
;
; :Returns:
;       ISO_TIME:           out, required, type=strarr
;                           ISO time strings corresponding to the input times.
;-
function MrTimeVar::toISO, time, type, $
T_REF=t_ref, $
TOKEN_FMT=token_fmt
	compile_opt idl2
	on_error, 2

	;Convert to ISO
	case strupcase(type) of
		'CDF_EPOCH': begin
			iso_time  = self -> CDFEpoch2ISO(time)
			token_fmt = '%Y-%M-%dT%H:%m:%S.%1%z'
		endcase
		
		'CDF_EPOCH16': begin
			iso_time  = self -> CDFEpoch16toISO(time)
			token_fmt = '%Y-%M-%dT%H:%m:%S.%1%2%3%4%z'
		endcase
		
		'CDF_EPOCH_LONG': begin
			iso_time  = self -> CDFEpoch16toISO(time)
			token_fmt = '%Y-%M-%dT%H:%m:%S.%1%2%3%4%z'
		endcase
		
		'CDF_TIME_TT2000': begin
			iso_time  = self -> tt2000toISO(time)
			token_fmt = '%Y-%M-%dT%H:%m:%S.%1%2%3%z'
		endcase
		
		'JULDAY': begin
			iso_time  = self -> julday2iso(time)
			token_fmt = '%Y-%M-%dT%H:%m:%S.%1%z'
		endcase
		
		'SSM': begin
			iso_time  = self -> ssm2iso(time, t_ref)
			token_fmt = '%Y-%M-%dT%H:%m:%S%f%z'
		endcase
		
		'TT2000': begin
			iso_time  = self -> tt2000toISO(time)
			token_fmt = '%Y-%M-%dT%H:%m:%S.%1%2%3%z'
		endcase
		
		'ISO-8601': begin
			token_fmt = '%Y-%M-%dT%H:%m:%S%f%z'
			if ~MrTokens_IsMatch(time[0], token_fmt) then message, 'TIME is not a recognized ISO format.'
			iso_time  = time
		endcase
		
		else: begin
			if size(time, /TNAME) eq 'STRING' $
				then MrTimeParser, time, type, '%Y-%M-%dT%H:%m:%S%f%z', iso_time $
				else message, 'Values for TIME and TYPE are not compatible.'
		endcase
	endcase
	
	;Return the array
	return, iso_time
end


;+
;   Finds the intervals within a given monotonic vector that brackets a given set of
;   one or more search values. See IDL's `Value_Locate <http://exelisvis.com/docs/VALUE_LOCATE.html>`
;
; :Params:
;       VALUE:              in, required, type=any
;                           Values to be located in the implicit array.
;       TYPE:               in, optional, type=any, default='ISO-8601'
;                           The time basis of `VALUE`. Any basis recognized by the
;                               ::FromISO method is accepted.
;
; :Keywords:
;       L64:                in, optional, type=boolean, default=0
;                           If set, indices will be returned as type Long64.
;
; :Returns:
;       RESULT:             Indices into the implicit array.
;-
function MrTimeVar::Value_Locate, value, type, $
L64=l64
	compile_opt idl2
	on_error, 2
	
	;Defaults
	if n_elements(type) eq 0 then type = 'ISO-8601'
	
	;Locate values
	if strupcase(type) ne 'ISO-8601' then begin
		time = self -> fromISO(*self.data, type)
		result = value_locate( temporary(time), value, L64=l64 )
	endif else begin
		result = value_locate(*self.data, value, L64=l64)
	endelse

	return, result
end


;+
;   The class definition statement.
;
; :Params:
;       CLASS:          out, optional, type=structure
;
; :Fields:
;       DATA:       Data to be accessed via bracket overloading.
;-
pro MrTimeVar__DEFINE, class
	compile_opt idl2
	
	class = { MrTimeVar, $
	          inherits MrVariable, $
	          token_format: '' $
	        }
end