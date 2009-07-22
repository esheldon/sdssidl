pro testfileio, ftype, type,t

    if n_elements(ftype) eq 0 or n_elements(type) eq 0 then begin
        print,'usage: testfileio, ftype, type, t'
        print,'  ftype="binary" "ascii"'
        print,'  type="all" "row" "col" "rowcol"'
        return
    endif

    ftype=strlowcase(ftype)
    type=strlowcase(type)

    stdef = {a:0.0, b:0, c:0L, s:' hello0'}
    n=10
    st = replicate(stdef, 10)

    st.a = findgen(n)
    st.b = indgen(n)
    st.c = lindgen(n)
    st.s = ' hello'+ntostr(indgen(n))

    file = expand_path('~/tmp/test.bin')
    if ftype eq 'ascii' then begin
        ascii_write, st, file
    endif else begin
        openw, lun, file, /get_lun
        writeu, lun, st
        free_lun, lun
    endelse

    comm='t='+ftype+'_read(file, stdef, n'
    case type of
        'all': begin
            comm=comm+')'

            print,'test1'
            if not execute(comm) then message,'Failed to execute'
            colprint,t.a,t.b,t.c,t.s
            print,'test2'
            if not execute(comm) then message,'Failed to execute'
            colprint,t.a,t.b,t.c, t.s
        end
        'row': begin
            comm=comm+', row=[2,5])'

            print,'test1'
            if not execute(comm) then message,'Failed to execute'
            colprint,t.a,t.b,t.c,t.s
            print,'test2'
            if not execute(comm) then message,'Failed to execute'
            colprint,t.a,t.b,t.c,t.s
        end
        'col': begin
            ;comm=comm+', col=[1,3])'
            comm=comm+', col=["b","s"])'
            
            print,'test1'
            if not execute(comm) then message,'Failed to execute'
            colprint,t.b,t.s
            print,'test2'
            if not execute(comm) then message,'Failed to execute'
            colprint,t.b,t.s
        end
        'rowcol': begin
            comm=comm+', row=[2,4], col=[1,3])'

            print,'test1'
            if not execute(comm) then message,'Failed to execute'
            colprint,t.b,t.s
            print,'test2'
            if not execute(comm) then message,'Failed to execute'
            colprint,t.b,t.s
        end
        else: message,'unsupported type: '+ntostr(type)
    endcase

    help,t,/str
end
