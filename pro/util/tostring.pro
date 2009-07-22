;+
; NAME:
;	tostring
;
; PURPOSE:
;	Convert an idl variable or value to a string.  The resulting string can 
;	be used for printing or evaluation using the command() function.
;
; CALLING SEQUENCE:
;	s=tostring(var)
;
; INPUTS:
;	var: anIDL variable;
;
; OUTPUTS:
;	a string representation of the input variable or value.
;
; LIMITATIONS:
;	Does not support complex, pointer, objref and undefined.
;	Only 1-d arrays currently supported.
;	Structures also cannot contain any of the unsupported types.
;
; EXAMPLE:
;	IDL> struct=[{a:35.2, b:[-25.1d, !dpi]}, {a:121.2, b:[55.2d, 61.3d]}]
;	IDL> s=tostring(struct)	
;   IDL> print,s
;   [{a: 3.5200001e+01, b: [-2.5100000000000001d+01,3.1415926535897931d+00]},
;    {a: 1.2120000e+02, b: [5.5200000000000003d+01,6.1299999999999997d+01]}]
;	IDL> if not execute('newstruct='+s) then message,'error'
;
; MODIFICATION HISTORY:
;	Created: 2009-07-22 Erin Sheldon, BNL
;
;-
function _array2string, arr
	; only 1-d for now
	nd=size(arr,/n_dim)
	if nd gt 1 then message,'Only 1-d arrays are supported'

	narr=n_elements(arr)
	sarr=strarr(narr)
	for i=0L,narr-1 do begin
		sarr[i]=tostring(arr[i])
	endfor

	return, '['+strjoin(sarr, ',')+']'
end

function _scalar_format, val, isdouble=isdouble
	tn=size(val,/tname)

	case tn of
		'STRING': f="(%'\'%s\'')"
		'DOUBLE': begin 
			isdouble=1
			f='(e)'
		end
		'FLOAT': f='(e)'
		'BYTE': f="(%'%dB')"
		'INT': f="(%'%d')"
		'UINT': f="(%'%dU')"
		'LONG': f="(%'%dL')"
		'ULONG': f="(%'%dUL')"
		'LONG64': f="(%'%dLL')"
		'ULONG64': f="(%'%dULL')"
		else: message,'Unsupported type: '+tn
	endcase

	return, f
end
function _scalar2string, val
	f=_scalar_format(val, isdouble=isdouble)

	valstr = string(val, format=f)
	valstr = strtrim(valstr, 2)

	if keyword_set(isdouble) then begin
		strput, valstr, 'd', strpos(valstr, 'e')
	endif

	return, valstr
end

function _struct2string, struct
	tags=strlowcase(tag_names(struct))
	nt=n_elements(tags)
	sarr=strarr(nt)
	for i=0L, nt-1 do begin
		tagname=tags[i]
		val = struct[0].(i)

		valstr=tostring(val)

		s=tagname+': '+valstr
		sarr[i] = s
	endfor

	s='{'+strjoin(sarr, ', ')+'}'
	return, s
end


function tostring, var
	if n_elements(var) eq 0 then begin
		on_error, 2
		message,'usage: s=tostring(var)',/inf
		message,'  Convert an IDL variable to a string',/inf
		message,'  Supports all types but complex,pointer,objref,undefined'
	endif


	sz=size(var)
	tname=size(var,/tname)

	; structs always show up as sz[0] >=1 but if there is only one element
	; we don't put [] around the definition

	if tname eq 'STRUCT' then begin
		if n_elements(var) eq 1 then begin
			return, _struct2string(var)
		endif else begin
			return, _array2string(var)
		endelse
	endif else begin
		if sz[0] gt 0 then begin
			return, _array2string(var)
		endif else begin
			return, _scalar2string(var)
		endelse
	endelse

end
