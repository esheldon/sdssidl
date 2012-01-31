; note run,camcol,fields (frange) can specify multiple values, but
; only *one* of these can
function sdss_expandvars, str_input, run=run, rerun=rerun, camcol=camcol, fields=fields, frange=frange, filter=filter

    str=str_input
    if (strmatch(str, '*$PHOTO_DATA*')) then begin
        path1 = getenv('PHOTO_DATA')
        if (NOT keyword_set(path1)) then $
            message, 'Environment variable PHOTO_DATA must be set!'
        str = repstr(str, '$PHOTO_DATA', path1)
    endif

    if (strmatch(str, '*$PHOTO_REDUX*')) then begin
        path2 = getenv('PHOTO_REDUX')
        if (NOT keyword_set(path2)) then $
            message, 'Environment variable PHOTO_REDUX must be set!'
        str = repstr(str, '$PHOTO_REDUX', path2)
    endif

    if (strmatch(str, '*$PHOTO_RESOLVE*')) then begin
        path3 = getenv('PHOTO_RESOLVE')
        if (NOT keyword_set(path3)) then $
            message, 'Environment variable PHOTO_RESOLVE must be set!'
        str = repstr(str, '$PHOTO_RESOLVE', path3)
    endif

    if (strmatch(str, '*$PHOTO_CALIB*')) then begin
        path4 = getenv('PHOTO_CALIB')
        if (NOT keyword_set(path4)) then $
            message, 'Environment variable PHOTO_CALIB must be set!'
        str = repstr(str, '$PHOTO_CALIB', path4)
    endif

    if (strmatch(str, '*$PHOTO_SWEEP*')) then begin
        path5 = getenv('PHOTO_SWEEP')
        if (NOT keyword_set(path5)) then $
            message, 'Environment variable PHOTO_SWEEP must be set!'
        str = repstr(str, '$PHOTO_SWEEP', path5)
    endif

    if (strmatch(str, '*$PHOTO_SKY*')) then begin
        path5 = getenv('PHOTO_SKY')
        if (NOT keyword_set(path5)) then $
            message, 'Environment variable PHOTO_SKY must be set!'
        str = repstr(str, '$PHOTO_SKY', path5)
    endif

    if (strmatch(str, '*$BOSS_PHOTOOBJ*')) then begin
        path5 = getenv('BOSS_PHOTOOBJ')
        if (NOT keyword_set(path5)) then $
            message, 'Environment variable BOSS_PHOTOOBJ must be set!'
        str = repstr(str, '$BOSS_PHOTOOBJ', path5)
    endif



    if (strmatch(str, '*$RUNNUM*')) then begin
        if n_elements(run) eq 0 then begin
            message, 'RUN must be specified!'
        endif
        runstring = string(run,f='(i0)')
        str = repstr(str, '$RUNNUM', runstring)
    endif
    if (strmatch(str, '*$RUNSTR*')) then begin
        if n_elements(run) eq 0 then begin
            message, 'RUN must be specified!'
        endif
        runstring = run2string(run)
        str = repstr(str, '$RUNSTR', runstring)
    endif


    if (strmatch(str[0], '*$RERUN*')) then begin
        if n_elements(rerun) EQ 0 then begin
            if n_elements(run) ne 0 then begin
                ; try to get the rerun
                rerun = sdss_rerun(run, exists=exists)
                if not exists then begin
                    message,'RERUN not found for run: '+string(run),/inf
                    message,'Perhaps try sending it explicitly'
                endif
            endif else begin
                message, 'RERUN must be specified!'
            endelse
        endif
        rrstring = strtrim(string(rerun),2)
        str = repstr(str, '$RERUN', rrstring)
    endif

    if (strmatch(str[0], '*$FILTER*')) then begin
        if n_elements(filter) eq 0 then begin
            message, 'FILTER must be specified!'
        endif
        if size(filter,/tname) eq 'STRING' then begin
            fil = strtrim(string(filter),2)
        endif else begin
            fil = fix(filter)
            w=where(fil lt 0 or fil lt 4,nw)
            if nw gt 0 then begin
                message,'Filter must be a string or in [0,4]'
            endif
            names = ['u','g','r','i','z']
            fil = names[fil]
        endelse
        str = repstr(str, '$FILTER', fil)
    endif


    if (strmatch(str[0], '*$COL*')) then begin
        if n_elements(camcol) eq 0 then begin
            message, 'CAMCOL must be specified!'
        endif
        colstring = camcol2string(camcol)

        str = repstr(str, '$COL', colstring)
    endif



    if (strmatch(str[0], '*$FIELDSTR*')) then begin
        if n_elements(fields) eq 0 then begin
            ; if fields not sent, look for frange
            if n_elements(frange) ne 0 then begin
                if n_elements(frange) ne 2 then begin
                    message,'frange must be a 2 element array'
                endif
                if frange[0] gt frange[1] then message,'FRANGE[0] must <= FRANGE[1]'
                if frange[0] eq frange[1] then begin
                    fields=frange[1]
                endif else begin
                    nf = frange[1]-frange[0]+1
                    fields = frange[0] + lindgen(nf)
                endelse
            endif else begin
                message, 'FIELDS or FRANGE must be specified!'
            endelse
        endif

        fieldstring = field2string(fields)

        ; allow multiple fields
        nf = n_elements(fieldstring)
        if nf eq 1 then begin
            str = repstr(str, '$FIELDSTR', fieldstring)
        endif else begin
            stra = strarr(nf)
            for i=0L, nf-1 do begin
                stra[i] = repstr(str, '$FIELDSTR', fieldstring[i])
            endfor
            str=stra
        endelse
    endif

    return, str

end
