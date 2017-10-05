; docformat = 'rst'
;
; NAME:
;       MrVariable_Cache__Define
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
;   The purpose of this method is to add functionality to the IDL_Container class.
;   Enhancements include::
;       - Retrieve index of object within container
;       - Print a list of object in the container
;       - Container can be cleared before adding anything to it.
;       - Objects can be replaced.
;       - Bracket overloading allows access a la ISA and POSITION.
;       - Objects can be removed without destroying them.
;
; :Author:
;   Matthew Argall::
;       University of New Hampshire
;       Morse Hall, Room 113
;       8 College Rd.
;       Durham, NH, 03824
;       matthew.argall@unh.edu
;
; :Copyright:
;       Matthew Argall 2016
;
; :History:
;   Modification History::
;       2016-02-13  -   Written by Matthew Argall
;       2016-09-27  -   Added SEARCHSTR and REGEX to ::GetNames. - MRA
;       2017-08-29  -   ::FindByNames returns objects in the order of the input names. - MRA
;-
;*****************************************************************************************
;+
;   The purpose of this method is to provide an array-like means of accessing graphics
;   objects within the container. Like calling the Get method with either the POSITION or
;   ISA keywords.
;
; :Params:
;       ISRANGE:            in, required, type=intarr
;                           A vector of 1's and 0's indicating if the corresponding
;                               subscript parameters `SUBSCRIPT1` are index ranges or
;                               index values.
;       SUBSCRIPT1:         in, required, type=intarr/strarr
;                           Index subscript of the graphics object to be returned, or the
;                               class names of the objects to return.
;-
function MrVariable_Cache::_OverloadBracketsRightSide, isRange, subscript1
	compile_opt idl2
	on_error, 2

;-----------------------------------------------------
;Integer Subscripts \\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\
;-----------------------------------------------------
	if MrIsA(subscript1, /INTEGER) then begin

		;If a scalar or array was given, get what was asked for.
		if isRange[0] eq 0 then begin
			;Negative index?
			iNeg = where(subscript1 lt 0, nNeg)
			if nNeg gt 0 then begin
				nObj = self -> Count()
				subscript1[iNeg] += nObj
			endif
	
			result = self -> Get(subscript1)
	
		;If a range was given, create an index array.
		endif else begin
			position = linspace(subscript1[0], subscript1[1], subscript1[2], /INTERVAL, TYPE=3)
			result = self -> Get(position)
		endelse

;-----------------------------------------------------
;String Subscripts \\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\
;-----------------------------------------------------
	endif else if size(subscript1, /TNAME) eq 'STRING' then begin
		result = self -> FindByName(subscript1)
	
	endif else message, 'Invalid subscript.'

	return, result
end


;+
;   The purpose of this method is to allow for iteration through the objects within the
;   container via the FOREACH operator.
;
; :Params:
;       VALUE:              in, out, required, type=object
;                           The current object.
;       KEY:                in, out, required, type=int/string
;                           The index of the current object.
;
; :Returns:
;       RESULT:             1 if there is a current element to retrieve, 0 if there are
;                               no more elements.
;-
function MrVariable_Cache::_OverloadForeach, value, key
	compile_opt idl2
	on_error, 2

	;Stop if there are no more objects.
	count = self -> Count()
	if key ge count then return, 0
	
	;Get all of the variable names
	varnames = self -> GetNames()

	;First object
	if n_elements(key) eq 0 then begin
		key      = varnames[0]
		value    = self -> Get(0)
		return, 1
	endif
	
	;Index of the previous iteration
	;   - Return if the variable was removed or if it was the last.
	idx = where(varnames eq key, n)
	if n   eq 0       then return, 0
	if idx eq count-1 then return, 0
	
	;Obtain the next key and value
	key = varnames[idx+1]
	value = self -> Get(idx+1)
	return, 1
end


;+
;   The purpose of this method provide output when the PRINT procedure is called.
;
; :Returns:
;       RESULTS:            A string to be printed by the PRINT procedure.
;-
function MrVariable_Cache::_OverloadPrint
	compile_opt idl2
	on_error, 2

	;Get all of the objects
	allObjs = self -> Get(/ALL, COUNT=nObjs)
	if nObjs eq 0 then return, 'No variables cached.'

	;Header
	printStr      = strarr(1,nObjs+1)
	printStr[0,0] = string('Index', 'Name', FORMAT='(1x, a5, 3x, a4)')

	;Print object information
	for i = 0, nObjs-1 do begin
		name = allObjs[i] -> GetName()
		printStr[0,i+1] = string(i, name, FORMAT='(1x, i5, 3x, a0)')
	endfor

	return, printStr
end


;+
;   The purpose of this method provide output when the PRINT procedure is called.
;
; :Returns:
;       RESULTS:            A string to be printed by the PRINT procedure.
;-
function MrVariable_Cache::_OverloadImpliedPrint
	compile_opt idl2
	on_error, 2

	;Return the same information as Print.
	return, self -> _OverloadPrint()
end


;+
;   The purpose of this method is to add functionality to the IDL_Container::Add method.
;   Additions include::
;       - Container can be cleared before new objects are added
;       - Objects cleared in this way can be destroyed
;       - Variables can be renamed
;
;   NOTES:
;       Only one variable is accepted because this method will be called
;       by the variable attempting to cache itself.
;
; :Params:
;       VARIABLE:       in, required, type=object
;                       An object instance or array of object instances to be added to
;                           the container object.
; :Keywords:
;       CLEAR:          in, optional, type=boolean, default=0
;                       Clear all contained items before adding the new ones.
;       DESTROY:        in, optional, type=boolean, default=0
;                       Destroy all objects being cleared. This is only valid when
;                           `CLEAR` is set.
;       POSITION:       in, optional, type=boolean, default=0
;                       Each index value specifies the position within the container at
;                           which a new object should be placed. If not specified, objects
;                           are added to the end of the list of contained items.
;       NO_CLOBBER:     in, optional, type=boolean, default=0
;                       If set, and if the variable's name clashes with that of another
;                           variable, the input variable will be renamed by appending
;                           "_#" to its name, where # represents an integer. The default
;                           is to replace the object already in the container.
;       NAME_OUT:       out, optional, type=string
;                       If `NO_CLOBBER` is set, then this is the new name of `VARIABLE`
;                           after it is renamed. If `VARIABLE` is not renamed, the
;                           original name is returned.
;-
pro MrVariable_Cache::Add, variable, $
CLEAR      = clear, $
DESTROY    = destroy, $
POSITION   = index, $
NO_CLOBBER = no_clobber, $
NAME_OUT   = name_out
	compile_opt idl2
	on_error, 2
	
;-----------------------------------------------------
; Check Inputs \\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\
;-----------------------------------------------------
	;Defaults
	tf_clear   = keyword_set(clear)
	tf_clobber = ~keyword_set(no_clobber)
	name_out   = variable -> GetName()

	;Clear all contained items?
	if tf_clear then self -> Remove, /ALL, DESTROY=destroy
	
	;Check if the variable is already in the container
	if self -> IsContained(variable) then return
	
	;Variable must be a MrVariable
	if ~obj_valid(variable) || ~obj_isa(variable, 'MRVARIABLE') $
		then message, 'VARIABLE must be a valid MrVariable object reference.'
	
;-----------------------------------------------------
; Enforce Unique Names \\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\
;-----------------------------------------------------
	;Get names of all variables
	varname = variable -> GetName()
	names   = self     -> GetNames(COUNT=count)
	
	;Check if the name exists
	if count eq 0 $
		then tf_exists = 0 $
		else tf_exists = max(names eq varname, iMatch)
	
	;If it exists
	if tf_exists then begin
		;Rename the variable
		if tf_clobber then begin
			self -> Replace, iMatch, variable, /DESTROY
			
		;Replace the variable
		endif else begin
			;Find matching names
			;   - More than one match may exist. Other matches have "_#" appended.
			imatch = where(strmid(names, 0, strlen(varname)) eq varname, nmatch)
			
			;Avoid clash
			if nmatch gt 0 then begin
				;Look for trailing _#
				parts  = stregex(strmid(names[imatch], strlen(varname)), '^_([0-9]+)$', /SUBEXP, /EXTRACT)
				varnum = fix(reform(parts[1,*]))
			
				;Append the max varnum plus one to the variable name
				;   - Important that VARIABLE is not in the cache, so ::SetName
				;     does not duplicate work.
				name_out  = varname + '_' + string(max(varnum)+1, FORMAT='(i0)')
				variable -> SetName, name_out
			endif
			
			;Add to the container
			self -> IDL_Container::Add, variable, POSITION=index
		endelse
	
;-----------------------------------------------------
; Name Already Is Unique \\\\\\\\\\\\\\\\\\\\\\\\\\\\\
;-----------------------------------------------------
	endif else begin
		self -> IDL_Container::Add, variable, POSITION=index
	endelse
end


;+
;   Retrieve objects from the cache by name, index, or object reference.
;
; :Params:
;       VAR:            in, required, type=name/number/objref
;                       Name, index, or object reference of the variable(s) to be retrieved.
;
; :Keywords:
;       ALL:            in, optional, type=boolean, default=0
;                       If set, all objects in the container matching `ISA` are returned.
;                           In this case `VARS` is ignored.
;       ISA:            in, optional, type=string/strarr
;                       Object class names used to filter the results of `ALL`.
;       ISCONTAINED:    out, optional, type=boolean
;                       A named variable to return true if `VARS` is contained in the
;                           container and false otherwise. This keyword is most useful
;                           when `VAR` is an object reference.
;       COUNT:          out, optional, type=integer
;                       A named variable to return the number of objects returned.
;
; :Returns:
;       VARIABLES:      out, required, type=objref
;                       Objects references to objects in the container.
;-
function MrVariable_Cache::Get, var, $
ALL=all, $
ISA=isa, $
ISCONTAINED=isContained, $
COUNT=count
	compile_opt idl2
	on_error, 2
	
	;Get all variables
	if keyword_set(all) then begin
		variables = self -> IDL_Container::Get(/ALL, ISA=isa, COUNT=count, /NULL)
		return, variables
	endif

	;Get by name or container index
	case size(var, /TNAME) of
		;Get by name
		'STRING': variables = self -> FindByName(var, COUNT=count)
		
		;Check if the objects are in the cache
		'OBJREF': begin
			;All must be MrVariable objects
			if ~array_equal(obj_isa(var, 'MrVariable'), 1) $
				then message, 'VAR must be an (array of) MrVariable objects.'
			
			;Outputs
			count = n_elements(var)
			variables = var
		endcase
		
		;Get by container index
		else: begin
			if MrIsA(var, /NUMBER) $
				then variables = self -> IDL_Container::Get(POSITION=var, COUNT=count) $
				else message, 'VAR must be a name, index, or object reference.'
		endcase
	endcase
	
	;Are the variables cached?
	;   - This should be always true for names and indices
	if arg_present(isContained) then isContained = self -> IsContained(variables)
	
	return, variables
end


;+
;   Get the index within the container at which the given object(s) is stored.
;
; :Params:
;       OBJECTS:        in, required, type=object/objarr
;                       An object instance or array of object instances to be added to
;                           the container object.
;
; :Returns:
;       INDEX:          The index within the container at which `OBJECTS` are stored.
;-
function MrVariable_Cache::GetIndex, Objects
	compile_opt idl2
	on_error, 2

	;Get the index at which the object is stored.
	tf_contained = self -> IDL_Container::IsContained(Objects, POSITION=Index)
	return, index
end


;+
;   Return the names of variables stored in the container.
;
; :Params:
;       SEARCHSTR:      in, optional, type=string
;                       A search string to match a subset of the variables names.
;
; :Keywords:
;       COUNT:          out, optional, type=integer
;                       Number of variables in the container
;       REGEX:          in, optional, type=boolean, default=0
;                       If set, `SEARCHSTR` will be applied via IDL's StRegEx()
;                           function. The default is to use StrMatch().
;
; :Returns:
;       VARNAMES:       The names of all variables in the container.
;-
function MrVariable_Cache::GetNames, searchstr, $
COUNT=count, $
REGEX=regex
	compile_opt idl2
	on_error, 2
	
	;Number of objects
	count = self -> Count()
	if count eq 0 then return, ''
	
	;Get names
	;   - Do not use FOREACH because it depends on this method.
	varnames = strarr(count)
	for i = 0, count-1 do begin
		theVar      = self -> Get(i)
		varnames[i] = theVar -> GetName()
	endfor

	;Match a search string?
	if n_elements(searchstr) gt 0 then begin
		;Search for matches
		if keyword_set(regex) $
			then iKeep = where(stregex(varnames, searchstr, /BOOLEAN), count) $
			else iKeep = where(strmatch(varnames, searchstr), count)
		
		;Keep matches
		if count gt 0 $
			then varnames = varnames[iKeep] $
			else varnames = ''
	endif

	;Return the names
	return, varnames
end


;+
;   Determine if any cached variable has a given name.
;
; :Params:
;       NAME:           in, required, type=string/strarr
;                       Determine if any cached variables have this name.
;
; :Keywords:
;       COUNT:          out, required, type=string
;                       Number of `NAME`s that match names of cached variables. 
;
; :Returns:
;       TF_HAS:         A Vector the same length as `NAME` containing 1 for each
;                           name that matches that of a cached variable and 0 for
;                           those that do not.
;-
function MrVariable_Cache::HasVar, name, $
COUNT=count
	compile_opt idl2
	on_error, 2
	
	;Get the names of the cached variabled
	varnames = self -> GetNames(COUNT=nNames)
	
	;Check if NAME is cached
	if nNames eq 0 $
		then tf_has = replicate(0, n_elements(name)) $
		else tf_has = MrIsMember(varnames, name, COUNT=count)

	;Return matches
	return, tf_has
end


;+
;   Find a variable by its name.
;
; :Params:
;       NAMES:          in, required, type=string/strarr
;                       Name of the variable to be retrieved. May contain any wildcard
;                           recognized by IDL's StrMatch function
;
; :Keywords:
;       COUNT:          out, required, type=integer
;                       Number of variables found with name `NAME`.
;       POSITION:       out, required, type=int/intarr
;                       The cache index of each variable found.
;       REGEX:          in, required, type=boolean, default=0
;                       If set, IDL's StRegEx function will be used to find variables
;                           instead of StrMatch. In this case, `NAME` may have any
;                           wildcard accepted by StRegEx.
;
; :Returns:
;       VARIABLE:       Variable with name `NAME`.
;-
function MrVariable_Cache::FindByName_v1, names, $
COUNT=count, $
POSITION=varIndex, $
REGEX=regex
	compile_opt idl2
	on_error, 2
	
	;Regex or strmatch?
	count    = 0
	tf_regex = Keyword_Set(regex)

	;Get all of the variables
	allVars   = self -> Get(/ALL, COUNT=nVars)
	IF nVars EQ 0 THEN RETURN, Obj_New()

	;Allocate memory
	varsFound = objarr(nVars)
	varIndex  = lonarr(nVars)
	
	;Loop through variables
	for i = 0, nVars-1 do begin
		varname = allVars[i] -> GetName()
		
		;Check for match
		if tf_regex $
			then tf_match = max( stregex(varname, name, /BOOLEAN) ) $
			else tf_match = max( strmatch(name, varname) )
		
		;Match?
		if tf_match then begin
			varsFound[count] = allVars[i]
			varIndex[count]  = i
			count           += 1
		endif
	endfor
	
	;Trim results
	;   - If no variables were found, return a single null object
	if count le 1 $
		then varsFound = varsFound[0] $
		else varsFound = varsFound[0:count-1]

	;Return matches
	return, varsFound
END


;+
;   Find a variable by its name.
;
; :Params:
;       NAMES:          in, required, type=string/strarr
;                       Name of the variable to be retrieved. May contain any wildcard
;                           recognized by IDL's StrMatch function
;
; :Keywords:
;       COUNT:          out, required, type=integer
;                       Number of variables found with name `NAMES`.
;       POSITION:       out, required, type=int/intarr
;                       The cache index of each variable found.
;       REGEX:          in, required, type=boolean, default=0
;                       If set, IDL's StRegEx function will be used to find variables
;                           instead of StrMatch. In this case, `NAMES` may have any
;                           wildcard accepted by StRegEx.
;
; :Returns:
;       VARIABLE:       Variable with name `NAMES`.
;-
FUNCTION MrVariable_Cache::FindByName, names, $
COUNT=count, $
POSITION=varIndex, $
REGEX=regex
	Compile_Opt idl2
	On_Error, 2
	
	;Regex or strmatch?
	count    = 0
	tf_regex = Keyword_Set(regex)

	;Get all of the variables
	allVars   = self -> Get(/ALL, COUNT=nVars)
	IF nVars EQ 0 THEN RETURN, Obj_New()

	;Allocate memory
	nNames    = N_Elements(names)
	varsFound = ObjArr(nNames)
	varIndex  = LonArr(nNames)
	
	;Get all of the variable names
	varnames = StrArr(nVars)
	FOR i = 0, nVars - 1 DO varnames[i] = allVars[i] -> GetName()
	
	;Step through each name
	FOR i = 0, nNames - 1 DO BEGIN
		;Check for match
		;   - This works because variable names within the cache are forced to be unique
		IF tf_regex $
			THEN tf_match = Max( StRegEx(varnames, names[i], /BOOLEAN), iMax ) $
			ELSE tf_match = Max( StrMatch(varnames, names[i]), iMax )
		
		;Keep matches
		IF tf_match THEN BEGIN
			varsFound[count] = allVars[iMax]
			varIndex[count]  = iMax
			count           += 1
		ENDIF
	ENDFOR
	
	;Trim results
	;   - If no variables were found, return a single null object
	IF count LE 1 $
		THEN varsFound = varsFound[0] $
		ELSE varsFound = varsFound[0:count-1]
	
	RETURN, varsFound
END


;+
;   The purpose of this method is to provide a means of relacing objects in an
;   IDL Container. 
;
; :Params:
;       OLD:            in, required, type=int/string/object
;                       The cache index, variable name, or variable object to be
;                           replaced.
;       NEW:            in, required, type=object
;                       The object that will replace `OLD`
;
; :Keywords:
;       DESTROY:        in, optional, type=boolean, default=0
;                       Destroy the `OLD` object being replaced
;       TAKE_NAME:      in, optional, type=boolean, default=0
;                       If set, `NEW` will be given the name of `OLD`.
;-
pro MrVariable_Cache::Replace, old, new, $
DESTROY = destroy, $
TAKE_NAME = take_name
	compile_opt idl2
	on_error, 2

	;The NEW variable must not already be in the cache
	if self -> IsContained(new) then message, 'Variable NEW must not already be in the cache.'
	
	;Get/verify OLD object reference
	oOld = self -> Get(old)
	tf_contained = self -> IsContained(oOld, POSITION=index)
	if ~tf_contained then begin
		message, 'OLD variable is not cached. Cannot replace.', /INFORMATIONAL
		return
	endif
	
	;Give OLD's name to NEW
	if keyword_set(take_name) then new -> SetName, oOld.name
	
	;Remove the old object and add the new one.
	self -> Remove, oOld, DESTROY=destroy
	self -> Add, new, POSITION=index
end


;+
;   The purpose of this method is to add functionality to the IDL_Container class.
;   Additions include::
;       - Specific classes of objects can be removed.
;       - Child_Object, Position, and Type can be supplied together
;       - Objects can be destroyed while being removed.
;
; :Params:
;       CHILDREN:           in, optional, type=object/string/integer
;                           A scalar or array of MrVariable objects, names or cache
;                               positions
;
; :Keywords:
;       ALL:                in, optional, type=boolean, default=0
;                           Remove all objects from the container.
;       DESTROY:            in, optional, type=boolean, default=0
;                           Destroy all objects being removed.
;-
pro MrVariable_Cache::Remove, children, $
ALL = all, $
DESTROY = destroy
	compile_opt idl2

	;Error handling
	catch, the_error
	if the_error ne 0 then begin
		catch, /cancel
		MrPrintF, 'LogErr'
		return
	endif

	;Defaults
	tf_all     = keyword_set(all)
	tf_destroy = keyword_set(destroy)
	
;---------------------------------------------------------------------
; Remove All /////////////////////////////////////////////////////////
;---------------------------------------------------------------------
	;Remove all objects?
	if tf_all then begin
		if tf_destroy then allObj = self -> Get(/ALL, COUNT=nObj)
		self -> IDL_Container::Remove, /ALL
		if tf_destroy && nObj gt 0 then obj_destroy, allObj
	
;---------------------------------------------------------------------
; Remove by ObjRef ///////////////////////////////////////////////////
;---------------------------------------------------------------------
	endif else if size(children, /TNAME) eq 'OBJREF' then begin
		self -> IDL_Container::Remove, children
		if tf_destroy then obj_destroy, children

;---------------------------------------------------------------------
; Remove by Position /////////////////////////////////////////////////
;---------------------------------------------------------------------
	endif else if MrIsA(children, /INTEGER) then begin
		childObj = self -> Get(children)
		self -> IDL_Container::Remove, childObj
		if tf_destroy then obj_destroy, childObj
	
;---------------------------------------------------------------------
; Remove by Name /////////////////////////////////////////////////////
;---------------------------------------------------------------------
	endif else if size(children, /TNAME) eq 'STRING' then begin
		for i = 0, n_elements(children) - 1 do begin
			childObj = self -> FindByName(children[i])
			self -> IDL_Container::Remove, childObj
			if tf_destroy then obj_destroy, childObj
		endfor
	
	endif else begin
		message, 'CHILDREN must be an object, string, or integer.'
	endelse
end


;+
;   Object class definition
;
; :Params:
;       CLASS:          out, optional, type=structure
;                       The class definition structure.
;-
pro MrVariable_Cache::Cleanup
	compile_opt idl2
	on_error, 2

	;Cleanup superclasses
	self -> IDL_Object::Cleanup
	self -> IDL_Container::Cleanup
end

;+
;   Object class definition
;
; :Params:
;       CLASS:          out, optional, type=structure
;                       The class definition structure.
;-
pro MrVariable_Cache__define, class
	compile_opt idl2

	class = { MrVariable_Cache, $
	          inherits IDL_Object, $
	          inherits IDL_Container $
	        }
end