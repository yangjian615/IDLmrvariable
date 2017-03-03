; docformat = 'rst'
;
; NAME:
;   MrVar_Attrs__Define
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
;   A means of giving attributes to an object. It is an interface to the Hash()
;   function.
;
; :Categories:
;   MrVariable, Attributes
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
;       2016/10/26  -   Written by Matthew Argall
;-
;*****************************************************************************************
;+
;   The initialization method.
;
; :Params:
;       ATTRNAME:       in, optional, type=string/hash/struct
;                       The name of the attribute for which the value is to be changed,
;                           or a hash or structure whos keys/tags are the attribute names.
;                           If a hash, keys must be strings. Values cannot be complex
;                           datatypes.
;       ATTRVALUE:      in, optional, type=any
;                       The value of the attribute(s) to be added. ATTRVALUE must be
;-
function MrVar_Attrs::INIT, attrName, attrValue
	compile_opt idl2

	;Error handling
	catch, the_error
	if the_error ne 0 then begin
		catch, /CANCEL
		MrPrintF, 'LogErr'
		return, 0
	endif

	;Allocate heap to pointers.
	self.attributes = hash()
	
	;Attributes given?
	if n_elements(attrName) gt 0 $
		then self -> AddAttr, attrName, attrValue

	return, 1
end


;+
;   Clean up after the object is destroyed
;-
pro MrVar_Attrs::CLEANUP
	compile_opt idl2
	on_error, 2
	
	;Destroy objects
	obj_destroy, self.attributes
end


;+
;   Set attribute values.
;
; :Params:
;       ATTRNAME:       in, required, type=string/hash/struct
;                       The name of the attribute for which the value is to be changed,
;                           or a hash or structure whos keys/tags are the attribute names.
;                           If a hash, keys must be strings. Values cannot be complex
;                           datatypes.
;       ATTRVALUE:      in, optional, type=any
;                       The value of the attribute(s) to be added. ATTRVALUE must be
;
; :Keyword:
;       OVERWRITE:      in, optional, type=boolean, default=0
;                       If set, attributes that already exist will be over-written. The
;                           default is to issue a warning.
;-
pro MrVar_Attrs::AddAttr, attrName, attrValue, $
OVERWRITE=overwrite
	compile_opt idl2
	on_error, 2
	
	;Defaults
	tf_overwrite = keyword_set(overwrite)

;-------------------------------------------------------
; Hash /////////////////////////////////////////////////
;-------------------------------------------------------
	if isa(attrName, 'HASH') then begin
		;Step through each key
		foreach val, attrName, key do begin
			;KEY must be a string
			if size(key, /TNAME) ne 'STRING' then begin
				MrPrintF, 'LogWarn', 'Hash key must be a string: ' + strtrim(key, 2) + '.'
				continue
			endif
			
			;KEY must be new
			if self -> HasAttr(key) && ~tf_overwrite then begin
				MrPrintF, 'LogWarn', 'Attribute already exists: "' + key + '".', LEVEL=5
				continue
			endif
			
			;Validate datatype
			if self -> AttrValue_IsValid(val) then begin
				self.attributes[key] = val
			endif else begin
				MrPrintF, 'LogWarn', size(val, /TNAME), key, $
				          FORMAT = '(%"Invalid attribute datatype (%s) for attribute %s")', $
				          LEVEL  = 5
				continue
			endelse
		endforeach

;-------------------------------------------------------
; Structure ////////////////////////////////////////////
;-------------------------------------------------------
	endif else if isa(attrName, 'STRUCT') then begin
		;Attributes must already exist
		nTags  = n_tags(attrName)
		tags   = tag_names(attrName)
		tf_has = self -> HasAttr(tags)
		
		;Loop through each value
		for i = 0, nTags - 1 do begin
			tag = tags[i]
		
			;TAG must not exist
			if tf_has[i] && ~tf_overwrite then begin
				MrPrintF, 'LogWarn', 'Attribute already exists: "' + tag + '".', LEVEL=5
				continue
			endif
			
			;Validate datatype
			if self -> AttrValue_IsValid(attrName.(i)) then begin
				self.attributes[tag] = attrName.(i)
			endif else begin
				MrPrintF, 'LogWarn', size(attrName.(i), /TNAME), tag, $
				          FORMAT = '(%"Invalid attribute datatype (%s) for attribute %s")', $
				          LEVEL  = 5
				continue
			endelse
		endfor

;-------------------------------------------------------
; String ///////////////////////////////////////////////
;-------------------------------------------------------
	endif else if isa(attrName, 'STRING') then begin
		;Check if attributes exist
		tf_has = self -> HasAttr(attrName)

	;-------------------------------------------------------
	; Scalar Name //////////////////////////////////////////
	;-------------------------------------------------------
		if isa(attrName, /SCALAR) then begin
			if tf_has && ~tf_overwrite then begin
				;Issue a warning from the calling program
				MrPrintF, 'LogWarn', 'Attribute already exists: "' + attrName + '".', LEVEL=5
			endif else begin
				;Validate datatype
				if self -> AttrValue_IsValid(attrValue) then begin
					self.attributes[attrName] = attrValue
				endif else begin
					MrPrintF, 'LogWarn', size(attrValue, /TNAME), attrName, $
					          FORMAT = '(%"Invalid attribute datatype (%s) for attribute %s")', $
					          LEVEL  = 5
				endelse
			endelse

	;-------------------------------------------------------
	; Scalar Value /////////////////////////////////////////
	;-------------------------------------------------------
		endif else if isa(attrValue, /SCALAR) then begin
			;An array of names can be given.
			names = attrName
			
			;
			; TODO: Handle case below when NGOOD=0
			;
		
			;Attribute names must not exist
			if max(tf_has) eq 1 && ~tf_overwrite then begin
				ibad = where(~tf_has, nbad, COMPLEMENT=igood, NCOMPLEMENT=ngood)
				for i = 0, nbad do MrPrintF, 'LogWarn', 'Attribute does not exist: "' + attrName[ibad[i]] + '".', LEVEL=5
				names = attrNames[igood]
			endif
			
			;Validate datatype
			if self -> AttrValue_IsValid(attrValue) then begin
				self.attributes[names] = attrValue
			endif else begin
				MrPrintF, 'LogWarn', size(attrValue, /TNAME), attrName[0], $
				          FORMAT = '(%"Invalid attribute datatype (%s) for attribute %s")', $
				          LEVEL  = 5
			endelse

	;-------------------------------------------------------
	; Array or List of Values //////////////////////////////
	;-------------------------------------------------------
		endif else begin
			;Restriction of Hash
			if n_elements(attrName) ne n_elements(attrValue) $
				then message, 'ATTRNAME and ATTRVALUE must have the same number of elements.'
		
			;Loop over each value
			foreach val, attrValue, idx do begin
				;Attribute must exist
				if ~tf_has[idx] && ~tf_create then begin
					MrPrintF, 'LogWarn', 'Attribute does not exist: "' + attrName[idx] + '".', LEVEL=5
					continue
				endif
				
				;Validate datatype
				if self -> AttrValue_IsValid(attrValue[idx]) then begin
					self.attributes[attrName[idx]] = attrValue[idx]
				endif else begin
					MrPrintF, 'LogWarn', size(attrValue[idx], /TNAME), attrName[idx], $
					          FORMAT = '(%"Invalid attribute datatype (%s) for attribute %s")', $
					          LEVEL  = 5
				endelse
			endforeach
		endelse
		
;-------------------------------------------------------
; Other ////////////////////////////////////////////////
;-------------------------------------------------------
	endif else begin
		;
		; Do not accept lists for ATTRNAME because numeric
		; attribute names are not allowed.
		;
	
		message, 'Invalid datatype (' + size(attrName, /TNAME) + ')for ATTRNAME.'
	endelse
end


;+
;   Determine if an attribute value is a valid datatype.
;
; :Private:
;
; :Params:
;       VALUE:          in, required, type=any
;                       Value for which the datatype is to be validated.
;
; :Keywords:
;       TYPE_NAME:      in, optional, type=boolean, default=0
;                       If set, `VALUE` is taken to be a datatype name.
;       TYPE_CODE:      in, optional, type=boolean, default=0
;                       If set, `VALUE` is taken to be a datatype code.
;-
function MrVar_Attrs::AttrValue_IsValid, value, $
TYPE_NAME=type_name, $
TYPE_CODE=type_code
	compile_opt idl2
	on_error, 2
	
	;Check keywords
	tf_type_name = keyword_set(type_name)
	tf_type_code = keyword_set(type_code)
	if tf_type_name + tf_type_code gt 1 then message, 'TYPE_NAME and TYPE_CODE are mutually exclusive.'
	
	;Get the type code
	if tf_type_name then begin
		tcode = self -> TypeName2Code(value)
	endif else if tf_type_code then begin
		tcode = value
	endif else begin
		tcode = size(value, /TYPE)
	endelse
	
	;Check valid values
	case tcode of
		 0: tf_valid = 0B ;UNDEFINED
		 1: tf_valid = 1B ;BYTE
		 2: tf_valid = 1B ;INT
		 3: tf_valid = 1B ;LONG
		 4: tf_valid = 1B ;FLOAT
		 5: tf_valid = 1B ;DOUBLE
		 6: tf_valid = 0B ;COMPLEX
		 7: tf_valid = 1B ;STRING
		 8: tf_valid = 0B ;STRUCT
		 9: tf_valid = 0B ;DCOMPLEX
		10: tf_valid = 0B ;POINTER
		11: tf_valid = 0B ;OBJREF
		12: tf_valid = 1B ;UINT
		13: tf_valid = 1B ;ULONG
		14: tf_valid = 1B ;LONG64
		15: tf_valid = 1B ;ULONG64
		else: message, 'Invalid datatype.'
	endcase
	
	return, tf_valid
end


;+
;   Get the names of all attributes
;
; :Keywords:
;       COUNT:          out, optional, type=integer
;                       Named variable to recieve the number of attribute names.
;-
function MrVar_Attrs::AttrNames, $
COUNT=count
	compile_opt idl2
	on_error, 2
	
	;Hash returns a list that we want to convert to a string array
	names = self.attributes -> Keys()
	names = names -> ToArray()
	
	;Return the count as well?
	if arg_present(count) then count = self.attributes -> Count()
	
	return, names
end


;+
;   Copy variable attributes to another MrVar_Attrs object.
;
; :Params:
;       VAR:            in, required, type=objref
;                       The destination MrVar_Attrs object.
;       ATTRNAMES:      in, optional, type=string/strarr, default=all attrs
;                       Names of the attributes to be copied.
;
; :Keywords:
;       OVERWRITE:      in, optional, type=boolean, default=0
;                       If set, then existing attributes with names appearing in
;                           `ATTRNAMES` will have their values overwritten. The
;                           default is to issue a warning and skip the attribute.
;-
pro MrVar_Attrs::CopyAttrTo, var, attrNames, $
OVERWRITE=overwrite
	compile_opt idl2
	on_error, 2
	
	;VAR must be a MrVar_Attrs
	if ~obj_isa(var, 'MrVar_Attrs') then message, 'VAR must inherit the MrVar_Attrs class.'
	
	;Default to copying all attributes
	if n_elements(attrName) eq 0 $
		then attrNames = self -> GetAttrNames(COUNT=nAttrs) $
		else nAttrs    = n_elements(attrNames)

	;Copy the variable attributes
	for i = 0, nAttrs-1 do var -> AddAttr, attrNames[i], self[attrNames[i]], OVERWRITE=overwrite
end


;+
;   Get attribute names.
;
; :Keywords:
;       COUNT:          in, optional, type=integer
;                       Number of attribute names returned.
;
; :Returns:
;       ATTRNAMES:      Names of all variable attributes.
;-
function MrVar_Attrs::GetAttrNames, $
COUNT=count
	compile_opt idl2
	on_error, 2

	;Return null for non-existant attributes
	attrList  = self.attributes -> Keys()
	attrNames = attrList -> ToArray(/NO_COPY)
	count     = n_elements(attrNames)

	return, attrNames
end


;+
;   Get an attribute value.
;
; :Params:
;       ATTRNAME:       in, required, type=string
;                       Name of the attribute for which the value is retreived.
;
; :Keywords:
;       NULL:           in, optional, type=boolean
;                       If set, and `ATTRNAME` is not a valid attribute name,
;                           then silently return !Null. The default is to throw
;                           an error.
;
; :Returns:
;       ATTRVALUE:      The attribute value.
;-
function MrVar_Attrs::GetAttrValue, attrName, $
NULL=null
	compile_opt idl2
	on_error, 2

	;Check if the attribute exists
	tf_exists = self.attributes -> HasKey(attrName)
	
	;If it exists
	if tf_exists then begin
		;Return the value
		value = self.attributes[attrName]
		
	;Otherwise
	endif else begin
		if keyword_set(null) $
			then value = !Null $
			else message, 'Attribute does not exist: "' + attrName + '".'
	endelse

	return, value
end


;+
;   Determine if the variable has an attribute.
;
; :Params:
;       ATTRNAME:       in, required, type=string,hash,struct
;                       The name of the attribute to be check
;-
function MrVar_Attrs::HasAttr, attrName
	compile_opt idl2
	on_error, 2

	;Number of names given
	nNames = n_elements(attrName)
	
	;Scalar name
	if nNames eq 1 then begin
		tf_has = self.attributes -> HasKey(attrName)
	
	;Loop over each name
	;   - Hash::HasKey accepts only scalars
	endif else begin
		tf_has = bytarr(nNames)
		foreach key, attrName, idx do tf_has[idx] = self.attributes -> HasKey(key)
	endelse
	
	return, tf_has
end


;+
;   Remove attributes.
;
; :Params:
;       ATTRNAMES:      in, required, type=string/hash/struct
;                       Attribute names to be removed. A warning is issued for names
;                           that do not already exist.
;-
pro MrVar_Attrs::RemoveAttr, attrNames
	compile_opt idl2
	on_error, 2
	
	;Which attributes exist
	tf_has = self -> HasAttr(attrNames)
	
	;Separate existent from non-existent
	iBad = where(tf_has eq 0, nBad, COMPLEMENT=iGood, NCOMPLEMENT=nGood)
	
	;Issue warning for non-existent
	if nBad gt 0 then begin
		for i = 0, nBad - 1 do MrPrintF, 'LogWarn', 'No such attribute: "' + attrNames[iBad[i]] + '".'
	endif
	
	;Remove the attributes
	if nGood gt 0 then self.attributes -> Remove, attrNames[iGood]
end


;+
;   Set attribute values.
;
;   Calling Sequence:
;       var -> SetAttrValue, struct
;       var -> SetAttrValue, hash
;       var -> SetAttrValue, string, value
;       var -> SetAttrValue, strarr, array
;       var -> SetAttrValue, strarr, list
;
; :Params:
;       ATTRNAME:       in, required, type=string/strarr/hash/struct
;                       The name of the attribute for which the value is to be changed.
;                           Or a hash or structure whos keys/tags are the attribute names.
;                           If an array of strings, then each element is an attribute name
;                           and `ATTRVALUE` may be an array or list of values with the same
;                           number of elements as ATTRNAME. If ATTRNAME is a hash, keys
;                           must be strings. Values cannot be complex datatypes.
;       ATTRVALUE:      in, optional, type=any accepted by ::AttrValue_IsValid
;                       The value of the attribute(s) to be added. ATTRVALUE must be
;
; :Keyword:
;       CREATE:         in, optional, type=boolean, default=0
;                       If set, then attributes will be created if they do not exist.
;-
pro MrVar_Attrs::SetAttrValue, attrName, attrValue, $
CREATE=create
	compile_opt idl2
	on_error, 2
	
;-------------------------------------------------------
; Hash /////////////////////////////////////////////////
;-------------------------------------------------------
	if isa(attrName, 'HASH') then begin
		
		;Step through each key
		foreach val, attrName, key do begin
			;Skip invalid attributes, but issue warning
			catch, the_error
			if the_error eq 0 $
				then self -> SetAttributeValue, key, val, CREATE=create $
				else MrPrintF, 'LogWarn', !error_state.msg
		endforeach

;-------------------------------------------------------
; Structure ////////////////////////////////////////////
;-------------------------------------------------------
	endif else if isa(attrName, 'STRUCT') then begin
		
		;Number of attributes and their names
		nTags = n_tags(attrName)
		tags  = tag_names(attrName)
		
		;Loop through each value
		for i = 0, nTags - 1 do begin
			
			;Skip invalid attributes, but issue warning
			catch, the_error
			if the_error eq 0 $
				then self -> SetAttributeValue, tags[i], attrName.(i), CREATE=create $
				else MrPrintF, 'LogWarn', !error_state.msg
		endfor

;-------------------------------------------------------
; String ///////////////////////////////////////////////
;-------------------------------------------------------
	endif else if isa(attrName, 'STRING', /SCALAR) then begin
		
		;Set the attribute
		self -> SetAttributeValue, attrName, attrValue, CREATE=create

;-------------------------------------------------------
; String Array with Array or List of Values ////////////
;-------------------------------------------------------
	endif else if isa(attrName, 'STRING', /ARRAY) then begin
			
		;Restriction of Hash
		if n_elements(attrName) ne n_elements(attrValue) $
			then message, 'ATTRNAME and ATTRVALUE must have the same number of elements.'
		
		;Loop over each value
		foreach val, attrValue, idx do begin
		
			;Skip invalid attributes, but issue warning
			catch, the_error
			if the_error eq 0 $
				then self -> SetAttributeValue, attrName[i], val, CREATE=create $
				else MrPrintF, 'LogWarn', !error_state.msg
		endforeach
		
;-------------------------------------------------------
; Other ////////////////////////////////////////////////
;-------------------------------------------------------
	endif else begin
		;
		; Do not accept lists for ATTRNAME because numeric
		; attribute names are not allowed.
		;
	
		message, 'Invalid value for ATTRNAME.'
	endelse
end


;+
;   Set the value of a single attribute.
;
; :Params:
;       ATTRNAME:       in, required, type=string
;                       The name of the attribute for which the value is to be changed.
;       ATTRVALUE:      in, optional, type=any accepted by ::AttrValue_IsValid
;                       The value of the attribute.
;
; :Keyword:
;       CREATE:         in, optional, type=boolean, default=0
;                       If set, then the attribute will be created if it does not
;                           already exist.
;-
pro MrVar_Attrs::SetAttributeValue, attrName, attrValue, $
CREATE=create
	compile_opt idl2
	on_error, 2
	
	;Check inputs
	if ~IsA(attrName, 'STRING', /SCALAR) $
		then message, 'ATTRNAME must be a scalar string.'
	
	if self -> HasAttr(attrName) eq 0 && ~keyword_set(create) $
		then message, 'Attribute name does not exist "' + attrName

	if self -> AttrValue_IsValid(attrValue) eq 0 then begin
		message, string(size(attrValue, /TNAME), attrName, $
		                FORMAT='(%"Invalid attribute datatype (%s) for attribute %s")')
	endif
	
	;Set attribute value
	;   - Will simultaneously create the attribute
	self.attributes[attrName] = attrValue
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
pro MrVar_Attrs__DEFINE
	compile_opt idl2
	
	class = { MrVar_Attrs, $
	          attributes: obj_new(), $
	        }
end