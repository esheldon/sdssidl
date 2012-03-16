pro _struct2dtype_print, dtypes
    n=n_elements(dtypes)
    print,'[', format='(a,$)'
    for i=0L, n-1 do begin
        if i ne 0 then print,' ',format='(a,$)'
        print, dtypes[i], format='(a,$)'
        if i lt (n-1) then print
    endfor
    print,']'
end
function _struct2dtype_get_typefield, data, index
    tname=size(data[0].(index),/tname)

    case tname of
        'BYTE': td='u1'
        'INT': td='i2'
        'UINT': td='u2'
        'LONG': td='i4'
        'ULONG': td='u4'
        'LONG64': td='i8'
        'ULONG64': td='u8'
        'FLOAT': td='f4'
        'DOUBLE': td='f8'
        'STRING': begin
            ssize = max(strlen(data.(index)))
            td = 'S'+ntostr(ssize)
        end
        else: begin
            message,'Unsupported field type: '+tname
        end
    endcase

    td = "'"+td+"'"
    return, td
end
function _struct2dtype_get_dtype, data, tags, index
    dims=size(data[0].(index),/dim)

    td = _struct2dtype_get_typefield(data, index)

    dt = ["'"+tags[index]+"'",td]

    if dims[0] ne 0 then begin
        if n_elements(dims) eq 1 then begin
            dimstr = ntostr(dims[0])
        endif else begin
            dimstr = '('+strjoin(ntostr(dims),',')+')'
        endelse
        dt = [dt, dimstr]
    endif

    dt = strjoin(dt,',')
    dt='('+dt+')'
    return, dt
end

; produce a numpy dtype, in string form, from the input struct
function struct2dtype, str, doprint=doprint
    ntags = n_tags(str)
    if ntags eq 0 then begin
        message,'input must be a structure'
    endif

    tags=strlowcase(tag_names(str))

    for i=0L, ntags-1 do begin
        dt=_struct2dtype_get_dtype(str, tags, i)
        add_arrval, dt, dtypes
    endfor

    if keyword_set(doprint) then begin
        _struct2dtype_print, dtypes
    endif

    dtype = '[' + strjoin(dtypes, ',') + ']'

    return, dtype
end
