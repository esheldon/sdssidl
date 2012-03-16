pro _wr_write_data, lun, struct
      writeu, lun, struct
end

pro _wr_write_rec_header, lun, struct

    nrows = n_elements(struct)
    dtypes=struct2dtype(struct)

    row_string = string(nrows, format='("SIZE = ",a20)')
    printf,lun,row_string
    printf,lun,'{"_VERSION": "1.0",'
    printf,lun,' "_DTYPE":[',format='(a,$)'

    n=n_elements(dtypes)
    for i=0L, n-1 do begin
        if (i ne 0) then printf,lun,'            '
        printf,lun,dtypes[i],format='(a,$)'
        if i lt (n-1) then printf,lun,','
    endfor

    ; finish last line of list and dict
    printf,lun,']}'

    ; this ends the header
    printf,lun,'END'

    ; add blank line
    printf,lun

end


pro _wr_fix_strings, struct
    ; make sure strings are all fixed length
    ntags = n_tags(struct)
    if ntags eq 0 then begin
        message,'input must be a structure'
    endif

    tags=tag_names(struct)
    s1=struct[0]
    for i=0L, ntags-1 do begin

        tn=size(s1.(i),/tname)
        if tn eq 'STRING' then begin

            slens = strlen(struct.(i))
            maxlen=max(slens)
            w=where(slens ne maxlen, nw)

            if nw gt 0 then begin
                sformat = '(A'+ntostr(maxlen)+')'
                newstrings = string(struct.(i), format=sformat)

                tdim = size(struct.(i), /dim)
                if tdim[0] eq 0 then begin
                    struct.(i) = newstrings
                endif else begin
                    struct.(i) = reform( newstrings, tdim )
                endelse
            endif
        endif
    endfor
end

function _wr_expand_tilde_gdl_kludge, fname
	; expand_tilde fails in gdl, do a kludge
	dir=file_dirname(fname)
	newdir = expand_tilde(dir)
	return, path_join(newdir, file_basename(fname))
end

function _wr_open_file, filename, clobber=clobber
    fn=_wr_expand_tilde_gdl_kludge(filename)

    if fexist(fn) then begin
        if not keyword_set(clobber) then begin
            on_error, 2
            message,'File '+filename+' exists, send /clobber to overwrite'
        endif
    endif

    openw, lun, filename, /get_lun, error=error
    if error ne 0 then begin 
        on_error, 2
        message,'Error opening file '+filename+': '+!error_state.sys_msg
    endif

    return, lun
end

pro write_rec, struct, filename, clobber=clobber
    if n_params() lt 2 then begin
        on_error,2
        message,'usage: write_rec, struct, filename, /clobber',/inf
        message,'halting'
    endif

    _wr_fix_strings, struct
    lun=_wr_open_file(filename, clobber=clobber)
    _wr_write_rec_header, lun, struct
    _wr_write_data, lun, struct

    free_lun, lun
end
