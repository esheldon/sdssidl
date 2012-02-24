function _csv2arr, s
    ; the input is assumed to be either
    ;   
    ; or
    ;   val1,val2,val3,...
    ; if a third val looks like (something,..) then it is
    ; assumed to be a tuple, and it is converted
    ; into a form like [something,..], leaving off
    ; empty parts like for (5,)
    

    p1 = strpos(s, ',')
    if p1 eq -1 then message,'Expected at least two elements in dtype tuple'
    name = strmid(s, 0, p1)

    s2 = strmid(s, p1+1)
    p2 = strpos(s2, ',')
    if p2 eq -1 then begin
        dtype = s2
    endif else begin
        dtype = strmid(s2, 0, p2)

        dims = strmid(s2, p2+1)

        ; this is either a number or a tuple
        if strmid(dims, 0, 1) eq '(' then begin
            dims = strmid(dims, 1, strlen(dims)-2)

            ; this is to remove the (3,) type things
            dims = '[' + strjoin( strsplit(dims, ',',/extract), ',' ) +']'
        endif

    endelse

    sarr=[name,dtype]
    if n_elements(dims) ne 0 then begin
        sarr=[sarr,dims]
    endif

    return, sarr
end

function _dtype2stringarr, dtype
	; convert a single dtype to a string array
	; e.g. ('fieldname','type',len)

	totlen=strlen(dtype)

	p1=strpos(dtype,'(')
	if p1 ne 0 then message,'expected brace "(" at front of single dtype'
	p2 = strpos(dtype,')',/reverse_search)
	if p2 ne (totlen-1) then $
		message,'expected brace ")" at end of single dtype'

	tmp = strmid(dtype, 1, totlen-2)

    sarr = _csv2arr(tmp)

	return, sarr

end

function _descr2value, descr_input, endian=endian

	front=strmid(descr_input,0,1)

	endian='not-given'
	case front of
		'<': endian = 'little'
		'>': endian = 'big'
		'|': endian = 'none'
		else: endian = 'notgiven'
	endcase

	if endian eq 'notgiven' then begin
		descr = descr_input
	endif else begin
		descr = strmid(descr_input, 1)
	endelse

	first = strmid(descr,0,1)
	if first eq 'S' then begin
		; this is a string
		l=strlen(descr)
		if l eq 1 then begin
			message,'string declared without a length: '+descr
		endif

		lenstr=strmid(descr,1)
		command = 'len = long('+lenstr+')'
		if not execute(command) then begin
			message,'Could not interpret string length in dtype: '+descr
		endif
		if len lt 1 then message,'string declared with bad length: "'+descr+'"'

		return, mkstr(len)
	endif

	; numbers
	case strlowcase(descr) of
		'u1': return,0b
		'i1': begin
            message,'WARNING: IDL does not support signed byte types',/inf
            return, 0b
        end

		'u2': return,0u
		'i2': return,0

		'u4': return, 0ul
		'i4': return, 0l

		'u8': return, 0ull
		'i8': return, 0ll

		'f4': return, 0.0
		'f8': return, 0d
		else: begin
			message,'could not interpret descr: '+descr	
		end
	endcase
end

pro _dtype2fieldname_and_value, dtype, fieldname, value, endian
	; first convert to an array of strings
	sarr = _dtype2stringarr(dtype)

	; now we must interpet. Still assuming 1-d subarrays
	; we do not correctly interpret these types
	;  ('fieldname','2f4')
	; it must be
	;  ('fieldname','f4', 2)

	narr = n_elements(sarr)
	if narr eq 2 then begin
		fieldname = eval( sarr[0] )
		descr = eval( sarr[1] )
		value = _descr2value(descr, endian=endian)
	endif else if narr eq 3 then begin
		fieldname = eval( sarr[0] )
		descr = eval(sarr[1])
		value = _descr2value(descr, endian=endian)

		repeatnum = eval(sarr[2])

		value = replicate(value, repeatnum)
	endif else begin
		message,"Exptected form ('fieldname','descr') or "+$
			"('fieldname','descr',len)"
	endelse

end
pro _pop_next_dtype, str, first, rest

	;if str = (f1),(f2),(f3)
	;then first=(f1)
	;and rest= (f2),(f3)
	;
	; so it pops from the front
	;
	;problem is f1 could be 'name1','type1',shape
	;  and shape could itself be (2,3,5) or whatever
	;  so watch for ())

	; But for now, we only support 1-d sub arrays for now
	; so we won't see a shape with braces


	totlen = strlen(str)

	; assuming only 1-d subarrays, this will find the end
	; of the declaration

	p=strpos(str, ')')
	if p eq -1 then message,'Unexpectedly ran to end of dtype'

    if p lt (totlen-1) then begin
        ; case of tuple (name,type,(a,b,..))
        if strmid(str,p+1,p+2) eq ')' then begin
            p = p+1
        endif
    endif

	first = strmid(str,0,p+1)
	rest = strmid(str,p+1)
	if strpos(rest,',') eq '0' then begin
		rest = strmid(rest,1)
	endif
end
function dtype2struct, dtype_in, endian=endian

	; convert a numpy dtype string to a structure definition

	; it must have the form
	; "[field_definition1, fdef2, fdef3, ...]"
	; field defs can be ('name','type') or ('name','type',shape)

	; plan: convert to something we can send either to mrdstruct
	; or something we can send to create_struct

	
	; find and remove the first [

	; first remove all white spaces
	dtype = repstr(dtype_in, ' ', '')
	; remove beginning [
	dtype = repstr(dtype, '[', '')
	; and ending ]
	dtype = repstr(dtype, ']', '')

	;print,dtype

	; now go through and get the pieces between ( and )

	delvarx, endian

	rest = dtype
	while rest ne '' do begin
		rest_old = rest
		_pop_next_dtype, rest_old, first, rest

		_dtype2fieldname_and_value, first, fieldname, value, tendian
		if n_elements(struct) eq 0 then begin
			struct=create_struct(fieldname, value)
		endif else begin
			struct=create_struct(struct, fieldname, value)
		endelse

		add_arrval, tendian, endian
	endwhile

	return,struct
end
