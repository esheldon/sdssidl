;+
; NAME:
;   sdssidl_test_suite
;
; PURPOSE:
;   Run a suite of tests on the sdssidl distribution.  This is primarily for
;   testing the C/C++ extensions, but a suite of tests for the structure
;   manipulation routines is also tested.
;
; CALLING SEQUENCE:
;   sdssidl_test_suite, atlas_info=
;
; OPTIONAL INPUTS:
;   atlas_info:  A structure with the file name of an atlas file to read
;       and the id of the object in the file to read.  For example
;           {file:'fpAtlas-000756-3-0125.fit', id:55}
;       NOTE: The atlas code will not be tested unless this input is sent. 
;
; COMMON BLOCKS:
;   common sdssidl_test_suite_common, pronames, funcnames, f1, f2
;
; RESTRICTIONS:
;   If the C/C++ extensions have not been compiled and linked in then these
;   tests will fail.  Even if ./configure & make have been run to compile
;   code, sdsspix_mask() and pgsql_query() may not have been compiled if
;   the GSL or POSTGESQL libraries were not found on the system.  In this
;   case the failures will simply state that the extension modules were not
;   found.
;
; MODIFICATION HISTORY:
;   Created:  Assembled over many years, finally added to repo 2008-03-27. 
;       Erin Sheldon, NYU
;-


pro _print_sep
    print
    print,'-------------------------------------------------------------'
end
pro _pmessage, mess, format=format, ntab=ntab
    common sdssidl_test_suite_common, pronames, funcnames, f1, f2

    tab=mkstr(4)
    if n_elements(ntab) eq 0 then begin
        ntab=0
    endif

    if ntab eq 0 then begin
        indent=''
    endif else begin
        indent=mkstr(ntab, val=tab)
    endelse
    print,indent+mess,format=format
end


function sdssidl_test_suite_struct_test

    common sdssidl_test_suite_common, pronames, funcnames, f1, f2

    _print_sep
    _pmessage,'Testing structure routines'
   

    errors=0L

    _pmessage,'add_tag: ', ntab=1
    _pmessage,f=f1,'Adding a tag', ntab=2
    t1 = {a:35}
    add_tag, t1, 'b', 6.2, new
    if not tag_exist(new,'b') then begin
        errors=errors+1
        res='Failed' 
    endif else begin
        res='OK'
    endelse
    _pmessage,f=f2,res


    _pmessage,f=f1,'Testing initialization',ntab=2
    if new.b ne 6.2 then begin
        errors=errors+1
        res='Failed'
    endif else begin
        res='OK'
    endelse
    _pmessage,f=f2,res



    _pmessage,'add_tags: ', ntab=1

    _pmessage,f=f1, 'Adding tags', ntab=2
    tnames = ['b','c']
    tform =['3','82.1']
    add_tags, t1, tnames, tform, new
    if not tag_exist(new,'b') or not tag_exist(new,'c') then begin
        errors=errors+1
        res='Failed'
    endif else begin
        res='OK'
    endelse

    _pmessage, res, f=f2

    _pmessage,'Testing initialization', f=f1,ntab=2
    if new.b ne 3 or new.c ne 82.1 then begin
        errors=errors+1
        res='Failed' 
    endif else begin
        res='OK'
    endelse
    _pmessage, res, f=f2




    _pmessage, 'alter_tags()', ntab=1
    _pmessage, 'Changing a:35 to a string', f=f1,ntab=2
    t1 = {a:35}
    t2 = alter_tags(t1,{a:'hello world'}) 
    if t2.a ne '      35' then begin
        errors=errors+1
        res='Failed' 
    endif else begin
        res='OK'
    endelse
    _pmessage,res,f=f2



    _pmessage,'combine_structs', ntab=1
    t1 = replicate({a:35}, 20)
    t2 = replicate({b:2.2},20)

    combine_structs, t1, t2, t3
    _pmessage,'Checking names (a,b)', f=f1, ntab=2
    if not tag_exist(t3,'a') or not tag_exist(t3,'b') then begin
        errors=errors+1
        res='Failed' 
    endif else begin
        res='OK' 
    endelse
    _pmessage,res,f=f2

    _pmessage,'Checking values: ', f=f1,ntab=2
    if t3[0].a ne 35 or t3[18].b ne 2.2 then begin
        errors=errors+1
        res='Failed' 
    endif else begin
        res='OK'
    endelse
    _pmessage, res, f=f2




    _pmessage,'concat_structs', ntab=1
    n1=10
    n2=5
    t1 = replicate({a:20,b:2.2}, n1) 
    t2 = replicate({a:13,b:1.9}, n2)

    _pmessage,'Concating '+ntostr(n1)+'+'+ntostr(n2), f=f1,ntab=2
    concat_structs, t1, t2, t3
    if n_elements(t3) ne (n1+n2) then begin
        errors=errors+1
        res='Failed' 
    endif else begin
        res='OK'
    endelse
    _pmessage,res,f=f2


    _pmessage,'remove_tags() ', ntab=1
    _pmessage,'Removing "a"', f=f1,ntab=2
    t1 = {a:20, b:2.2}
    t2 = remove_tags(t1, 'a')
    if tag_exist(t2,'a') then begin
        errors=errors+1
        res='Failed' 
    endif else begin
        res='OK'
    endelse
    _pmessage,res,f=f2


    _pmessage,'rename_tags() ', ntab=1
    _pmessage,'Renaming "a" to "stuff"', f=f1, ntab=2
    t1 = {a:13,b:1.9}
    t2 = rename_tags(t1, 'a', 'stuff')
    if tag_exist(t2,'a') or not tag_exist(t2,'stuff') then begin
        errors=errors+1
        res='Failed' 
    endif else begin
        res='OK'
    endelse
    _pmessage,res,f=f2



    _pmessage,'reorder_tags() ', ntab=1
    _pmessage,'Reordering (a,b) to (b,a)', f=f1, ntab=2
    t1 = {a:1, b:2.2}
    t2 = reorder_tags(t1, ['b','a'])
    tn = tag_names(t2)
    if tn[0] ne 'B' or tn[1] ne 'A' then begin
        errors=errors+1
        res='Failed'
    endif else begin
        res='OK'
    endelse
    _pmessage,res,f=f2


    _pmessage,'zero_struct', ntab=1
    _pmessage,'Zeroing {a:35, b:2.2, c:"stuff", d:[55.2,101.1]}',f=f1,ntab=2
    print
    _pmessage,'         d:[55.2,101.1]}',f=f1,ntab=2
    t1 = {a:35, b:2.2, c:'stuff',d:[55.2, 101.1]}
    zero_struct, t1
    if (t1.a ne 0 $
            or t1.b ne 0 $
            or t1.c ne '' $
            or t1.d[0] ne 0 $
            or t1.d[1] ne 0) then begin
        
        errors=errors+1
        res='Failed'
    endif else begin
        res='OK'
    endelse
    _pmessage,res,f=f2

    return,errors


end

function sdssidl_test_suite_htm_test

    common sdssidl_test_suite_common, pronames, funcnames, f1, f2

    _print_sep

    _pmessage,'Testing htm procedures'

    errors=0L
    _pmessage,'htm_index()', ntab=1
    w=where(funcnames eq 'HTM_INDEX',nw)
    if nw eq 0 then begin
        _pmessage,'Module HTM_INDEX not found',ntab=2
        errors=errors+1
    endif else begin


        _pmessage,'Testing Execution ra=200,dec=0,depth=8',f=f1,ntab=2
        ra = 200d
        dec = 0d
        depth = 8
        correctid = 657424
        comm = 'htmid = htm_index(ra, dec, depth, status=status)'
        if not execute(comm) then begin
            print,'Failed to execute command'
            errors=errors+1
        endif else begin
            _pmessage,'OK',f=f2
            _pmessage,'Testing return value and status',f=f1,ntab=2
            if status ne 0 then begin
                print,'htm_index() returned failure status'
                errors=errors+1
            endif else begin
                if htmid ne correctid then begin
                    errors=errors+1
                    print,'Failed'
                    print,'returned htm index is wrong: '+ntostr(htmid)+$
                        ' instead of '+ntostr(correctid)
                endif else begin
                    _pmessage,'OK',f=f2
                endelse
            endelse

        endelse
    endelse


    _pmessage,'htm_match', ntab=1
    w=where(pronames eq 'HTMMATCHC',nw)
    if nw eq 0 then begin
        _pmessage,'Module HTMMATCHC, needed by htm_match, not found',ntab=2
        errors=errors+1
    endif else begin


        ra1 = [200d, 50d]
        dec1 = [0d, 25d]
        ra2 = [200d, 50d]
        dec2 = [0d, 25d]
        depth = 8
        angle = 1d/3600d*!pi/180d
        htm_match, ra1, dec1, ra2, dec2, angle, m1, m2, d12
        _pmessage,'Testing Matches',f=f1, ntab=2
        if n_elements(m1) ne 2 then begin
            print,'Failed to match'
            errors=errors+1
        endif else begin
            _pmessage,'OK',f=f2
        endelse
    endelse

    _pmessage,'htm_intersect()',ntab=1
    w=where(funcnames eq 'HTM_INTERSECT',nw)
    if nw eq 0 then begin
        _pmessage,'Module HTM_INTERSECT not found',ntab=2
        errors=errors+1
    endif else begin

        ra = 200d
        dec = 0d
        depth = 8
        angle = 30d/3600d*!pi/180d
        correctids = [657424, 885792]

        _pmessage,'Testing Execution 200,0,8,'+ntostr(angle),f=f1,ntab=2
        comm = 'htmids = htm_intersect(ra, dec, depth, angle, status=status)'
        if not execute(comm) then begin
            print,'Failed to execute command'
            errors=errors+1
        endif else begin
            _pmessage,'OK',f=f2
            _pmessage,'Testing return value and status',f=f1,ntab=2
            if status ne 0 then begin
                print,'htm_intersect() returned failure status'
                errors=errors+1
            endif else begin
                match, htmids, correctids, mhtm, mcorr, /sort
                if n_elements(mhtm) ne 2 then begin
                    print,'returned htm indices are wrong'
                    errors=errors+1
                endif else begin
                    _pmessage,'OK',f=f2
                endelse
            endelse

        endelse
    endelse

    return,errors


end

function sdssidl_test_suite_fileio_test, ftype_in

    common sdssidl_test_suite_common, pronames, funcnames, f1, f2
    ftype_in=strlowcase(ftype_in)

    if strmatch(ftype_in,'*ascii*') then begin
        ftype='ascii'
        case ftype_in of
            
            'ascii_csv': csv=1
            'ascii_tab': tab=1
            'ascii_white': begin
                delim=' '
            end
            else: message,'Unknown type: '+ntostr(ftype_in)
        endcase
    endif else begin
        ftype='binary'
    endelse

    _print_sep
    _pmessage,'Testing fileio procedures: '+ftype_in

    errors=0L

    case ftype of
        'ascii': begin
            w=where(pronames eq 'ASCII_WRITE',nw)
            if nw eq 0 then begin
                print, 'Module ASCII_WRITE not found'
                errors=errors+1
                return,errors
            endif

            w=where(funcnames eq 'ASCII_READ',nw)
            if nw eq 0 then begin
                print, 'Module ASCII_READ not found'
                errors=errors+1
                return,errors
            endif

            ext = '.ascii'
        end
        'binary': begin
            w=where(funcnames eq 'BINARY_READ',nw)
            if nw eq 0 then begin
                print, 'Module BINARY_READ not found'
                errors=errors+1
                return,errors
            endif
            ext='.bin'
        end
        else: message,'Unsupported type: '+ntostr(ftype)
    endcase


    ; Çreate the output structure and structure definition
    stdef = {      $
            a:0.0, $
            b:0, $
            c:0L, $
            arr:[0LL,0LL], $
            s:'hello0', $
            sarr:['hello0','hello0'] $
        }
    n=10
    st = replicate(stdef, 10)

    st.a = findgen(n)
    st.b = indgen(n)
    st.c = lindgen(n)
    strtmp = 'hello'+ntostr(indgen(n))
    st.s = strtmp
    st.sarr[0] = strtmp
    st.sarr[1] = strtmp


    ; output file
    file = tmpfile(tmpdir='/tmp', suffix=ext)

    ; Write as ascii or binary
    _pmessage,'Writing temp file: '+file+' as '+ftype, ntab=1
    if ftype eq 'ascii' then begin
        comm='ascii_write,st,file,delim=delim,tab=tab,csv=csv,status=status'
        if not execute(comm) then begin
            _pmessage, 'Failed to execute ascii_write'
            errors=errors+1
            return, errors
        endif
        if status ne 0 then begin
            _pmessage, 'ascii_write returned failure status'
            errors=errors+1
            return, errors
        endif
    endif else begin
        openw, lun, file, /get_lun, error=oerror
        if oerror ne 0 then begin
            _pmessage,'openw returned failure status'
            errors=error+1
            return,errors
        endif
        writeu, lun, st
        free_lun, lun
    endelse


    _pmessage,'Reading file: '+file+' as '+ftype, ntab=1
    ; Now read using the appropriate function
    tcomm='t='+ftype+'_read(file, stdef, n'
    if ftype eq 'ascii' then tcomm=tcomm+',tab=tab,csv=csv,delim=delim'

    types = ['all','row','col','rowcol']
    for ti=0L, n_elements(types)-1 do begin
        type = types[ti]

        ; Note each test is run twice to watch for memory errors
        case type of
            'all': begin
                _pmessage,'Reading all ...', f=f1, ntab=2
                comm=tcomm+')'

                comm = comm+' & '+comm
                if not execute(comm) then begin
                    errors=errors+1
                    print,'Read failed'
                endif else begin
                    _pmessage, 'OK', f=f2
                    _pmessage,'Comparing result to input ...',$
                        f=f1,ntab=3
                    td=compare_struct(st, t)
                    if td.ndiff ne 0 then begin
                        errors=errors+1
                        print,ntostr(td.ndiff)+' differences found'
                    endif else begin
                        _pmessage,'OK',f=f2
                    endelse
                endelse

            end
            'row': begin
                row='[2,5]'
                rownums=[2,5]
                _pmessage,'Reading rows='+row+' ...', f=f1,ntab=2
                comm = tcomm+', row='+row+')'
                comm = comm+' & '+comm

                if not execute(comm) then begin
                    errors=errors+1
                    print,'Read failed'
                endif else begin
                    _pmessage,'OK',f=f2
                    _pmessage,'Comparing result to input ...',$
                        f=f1,ntab=3
                    tst = st[rownums]
                    td=compare_struct(tst, t)
                    if n_elements(td) ne 1 or td[0].ndiff ne 0 then begin
                        errors=errors+1
                        print,ntostr(long(total(td.ndiff)))+' differences found'
                    endif else begin
                        _pmessage,'OK',f=f2
                    endelse

                endelse

            end
            'col': begin
                colnums = [1,3]
                colnames = (tag_names(st))[colnums]
                col = '[1,3]'
                _pmessage,'Reading col='+col+' ...', f=f1,ntab=2
                comm = tcomm+', col='+col+')'
                comm = comm+' & '+comm

                if not execute(comm) then begin
                    errors=errors+1
                    print,'Read failed'
                endif else begin
                    _pmessage,'OK',f=f2

                    _pmessage,'Comparing result to input ...',f=f1,ntab=3
                    tst = extract_tags(st, colnames)
                    td=compare_struct(tst, t)
                    if td.ndiff ne 0 then begin
                        errors=errors+1
                        print,ntostr(td.ndiff)+' differences found'
                    endif else begin
                        _pmessage,'OK',f=f2
                    endelse

                endelse

            end
            'rowcol': begin
                row='[2,5]'
                rownums=[2,5]
                colnums = [1,3]
                colnames = (tag_names(st))[colnums]
                col = '[1,3]'
                _pmessage,'Reading row='+row+' col='+row+' ...', f=f1,ntab=2
                comm = tcomm+', row='+row+', col='+col+')'
                comm = comm+' & '+comm

                if not execute(comm) then begin
                    errors=errors+1
                    print,'Read failed'
                endif else begin
                    _pmessage,'OK',f=f2

                    _pmessage,'Comparing result to input ...',f=f1,ntab=3
                    tst = extract_tags(st, colnames)
                    tst = tst[rownums]
                    td=compare_struct(tst, t)
                    if td.ndiff ne 0 then begin
                        errors=errors+1
                        print,ntostr(td.ndiff)+' differences found'
                    endif else begin
                        _pmessage,'OK',f=f2
                    endelse

                endelse

            end
            else: message,'unsupported type: '+ntostr(type)
        endcase
    endfor

    if fexist(file) then begin
        _pmessage,'Removing file: '+file,ntab=1
        file_delete, file
    endif
    return,errors

end



function sdssidl_test_suite_pgsql_test

    common sdssidl_test_suite_common, pronames, funcnames, f1, f2

    errors=0

    _print_sep
    name='pgsql_query()'
    _pmessage, 'Testing: '+name, ntab=0

    w=where(funcnames eq 'PGSQL_QUERY',nw)
    if nw eq 0 then begin
        print, 'Module '+name+' not found'
        errors=errors+1
        return,errors
    endif

    pg=obj_new('postgres')

    temp_table = 'sdssidl_test_suite_table'+ntostr(long(10+randomu(seed)*1000))
    _pmessage, 'Creating table: '+temp_table

    query = "CREATE TABLE "+temp_table+" (id int, sometext text)"

    _pmessage, query, ntab=2
    pg->query, query, status=status
    if status ne pg->status_val('no_result') then begin
        errors=errors+1
        _pmessage,'Failed to create table.  status='+ntostr(status),ntab=0
        _pmessage,'This could be an error in the coe, or perhaps the database is not set up yet, or permissions are not set',ntab=1
        return,errors
    endif else begin
        _pmessage,'OK',f=f2,ntab=3
    endelse

    _pmessage,'Populating table: '+temp_table, ntab=1
    query="insert into "+temp_table+" values(3,'test')"
    _pmessage,query, ntab=2
    pg->query, query, status=status
    if status ne pg->status_val('no_result') then begin
        _pmessage,'Failed to populate the table',ntab=0
        errors=errors+1
    endif else begin
        _pmessage,'OK',ntab=1
    endelse


    _pmessage,'Reading from table: '+temp_table, ntab=1
    query='select id from '+temp_table+' limit 10'
    _pmessage,query,ntab=2
    t=pg->query(query, status=status)
    if status ne pg->status_val('success') then begin
        _pmessage,'Failed to read from the table',ntab=0
        errors=errors+1
    endif else begin
        if t[0].id ne 3 then begin
            _pmessage,'Read gave wrong data: id='+ntostr(t[0].id),ntab=0
            errors=errors+1
        endif else begin
            _pmessage,'OK',ntab=1
        endelse
    endelse


    _pmessage,'Dropping the table: '+temp_table,ntab=1
    query='drop table '+temp_table
    _pmessage,query,ntab=2
    pg->query, query, status=status
    if status ne pg->status_val('no_result') then begin
        _pmessage,'Failed to drop the table',ntab=0
        errors=errors+1
    endif else begin
        _pmessage,'OK',ntab=1
    endelse

    return,errors


end

function sdssidl_test_suite_total_int_test


    common sdssidl_test_suite_common, pronames, funcnames, f1, f2

    _print_sep
    errors=0
    _pmessage,'total_int()',ntab=1
    w=where(funcnames eq 'TOTAL_INT',nw)
    if nw eq 0 then begin
        errors=errors+1
        _pmessage,'Module TOTAL_INT not found',ntab=2
    endif else begin
        _pmessage,'Testing execution',f=f1,ntab=2
        comm = 'res = total_int([1,1,1,1])'
        if not execute(comm) then begin
            errors=errors+1
            _pmessage,'Failed',f=f2
        endif else begin
            _pmessage,'OK',f=f2
            _pmessage,'Testing return value',f=f1,ntab=2
            if res ne 4 then begin
                res='Failed1' 
            endif else begin
                res='OK'
            endelse
            _pmessage,res,f=f2
        endelse
    endelse

    return,errors
end
    
function sdssidl_test_suite_gauleg_test

    common sdssidl_test_suite_common, pronames, funcnames, f1, f2

    _print_sep
    errors=0
    _pmessage,'gauleg',ntab=1
    w=where(pronames eq 'GAULEG',nw)
    if nw eq 0 then begin
        errors=errors+1
        _pmessage,'Module GAULEG not found',ntab=2
    endif else begin
        _pmessage,'Testing execution',f=f1,ntab=2
        comm = 'gauleg, 3.5, 10.0, 20, x, w'
        if not execute(comm) then begin
            errors=errors+1
            _pmessage,'Failed',f=f2
        endif else begin
            _pmessage,'OK',f=f2
            _pmessage,'Testing return values',f=f1,ntab=2

            if n_elements(x) ne 20 or n_elements(w) ne 20 then begin
                errors=errors+1
                print,'X,W wrong size'
            endif else begin
                if (abs(x[5]-4.6828255)) gt 0.01 then begin
                    print,'x[5] grossly wrong'
                    errors=errors+1
                endif else begin
                    _pmessage,'OK',f=f2
                endelse
            endelse

        endelse
    endelse

    return,errors
end 

function sdssidl_test_suite_sdsspix_mask_test

    common sdssidl_test_suite_common, pronames, funcnames, f1, f2

    _print_sep
    errors=0
    _pmessage,'sdsspix_mask',ntab=1
    w=where(funcnames eq 'SDSSPIX_MASK',nw)
    if nw eq 0 then begin
        errors=errors+1
        _pmessage,'Module SDSSPIX_MASK not found',ntab=2
    endif else begin

        dir=getenv('SDSSIDL_DIR')
        dir=path_join(dir, 'data')
        maskfile=path_join(dir, 'sample-pixel.mask')
        if not file_test(maskfile) then begin
            errors=errors+1
            print,'sample maskfile '+maskfile+' not found'
            print,'This should have been part of SDSSIDL'
            return,errors
        endif

        _pmessage,'Testing execution',f=f1,ntab=2
        clam = [15d, 25d]
        ceta = [-32.5d, -32d]

        comm = 'mflags=sdsspix_mask(clam, ceta, maskfile, status=status)'
        if not execute(comm) then begin
            errors=errors+1
            _pmessage,'Failed',f=f2
        endif else begin
            _pmessage,'OK',f=f2
            _pmessage,'Testing return and status values:  ',f=f1,ntab=2

            if status ne 0 then begin
                errors=errors+1
                print,'sdsspix_mask() returned error status'
            endif else begin
                bad=(n_elements(mflags) ne 2  or size(mflags,/tname) ne 'LONG')
                if (bad) then begin
                    errors=errors+1
                    print,'Returned flags are wrong format or missing'
                    help,mflags
                endif else begin
                    _pmessage,'OK',f=f2
                endelse
            endelse

        endelse
    endelse

    return, errors

end


function sdssidl_test_suite_sphpoly_comp_test

    common sdssidl_test_suite_common, pronames, funcnames, f1, f2

    _print_sep
    errors=0
    mname='sphpoly_comp'
    _pmessage,mname,ntab=1
    mname=strupcase(mname)
    w=where(funcnames eq mname,nw)
    if nw eq 0 then begin
        errors=errors+1
        _pmessage,'Module '+mname+' not found',ntab=2
    endif else begin

        dir=getenv('SDSSIDL_DIR')
        dir=path_join(dir, 'data')
        maskfile=path_join(dir, 'sample-sphpoly-mask.dat')
        if not file_test(maskfile) then begin
            errors=errors+1
            print,'sample maskfile '+maskfile+' not found'
            print,'This should have been part of SDSSIDL'
            return,errors
        endif

        _pmessage,'Testing execution',f=f1,ntab=2
        ra=[200d,205d]
        dec=[0d,0.1d]

        comm = 'comp=sphpoly_comp(ra, dec, maskfile, status=status)'
        if not execute(comm) then begin
            errors=errors+1
            _pmessage,'Failed',f=f2
        endif else begin
            _pmessage,'OK',f=f2
            _pmessage,'Testing return and status values:  ',f=f1,ntab=2

            if status ne 0 then begin
                errors=errors+1
                print,'sphpoly_comp() returned error status'
            endif else begin
                bad=(n_elements(comp) ne 2  or size(comp,/tname) ne 'DOUBLE')
                if (bad) then begin
                    errors=errors+1
                    print,'Returned completeness are wrong format or missing'
                    help,mflags
                endif else begin
                    _pmessage,'OK',f=f2
                endelse
            endelse

        endelse
    endelse

    return, errors

end


function sdssidl_test_suite_atlas_test, info

    common sdssidl_test_suite_common, pronames, funcnames, f1, f2

    errors=0L
    _pmessage,'read_atlas (rdatlas)', ntab=1
    w=where(pronames eq 'RDATLAS',nw)
    if nw eq 0 then begin
        errors=errors+1
        _pmessage,'Module RDATLAS not found',ntab=2
    endif else begin

        file = info.file
        id = info.id

        _pmessage,'Testing execution', f=f1, ntab=2
        comm = 'rdatlas, file, id, imr=imr, status=status'
        if not execute(comm) then begin
            errors=errors+1
            _pmessage,'Failed',f=f2
        endif else begin
            _pmessage,'OK',f=f2
            _pmessage,'Testing output and status',f=f1, ntab=2
            if status ne 0 then begin
                errors=errors+1
                print,'rdatlas returned error status'
            endif else begin
                bad=(n_elements(imr) eq 0 or size(imr,/n_dim) ne 2 or $
                        size(imr,/tname) ne 'LONG')
                if (bad) then begin
                    errors=errors+1
                    print,'Returned image is wrong format or missing'
                    help,imr
                endif else begin
                    _pmessage,'OK',f=f2
                endelse
            endelse
        endelse
    endelse 

    return,errors
end



PRO sdssidl_test_suite, atlas_info=atlas_info


    common sdssidl_test_suite_common, pronames, funcnames, f1, f2

    f1='(%"%-50s",$)'
    f2='(%"%10s")'

    pronames  = routine_info(/system)
    funcnames  = routine_info(/system, /functions)

    ntests=0L
    nfailed=0L

    ntests=ntests+1
    nerror = sdssidl_test_suite_struct_test() 
    if nerror ne 0 then nfailed=nfailed+1
    add_arrval, 'struct', testnames
    add_arrval, (nerror eq 0), teststatus

    ntests=ntests+1
    nerror=sdssidl_test_suite_pgsql_test()
    if nerror ne 0 then nfailed=nfailed+1
    add_arrval, 'Postgres', testnames
    add_arrval, (nerror eq 0), teststatus

    ntests=ntests+1
    nerror= sdssidl_test_suite_fileio_test('ascii_csv')
    if nerror ne 0 then nfailed=nfailed+1
    add_arrval, 'file i/o ascii csv', testnames
    add_arrval, (nerror eq 0), teststatus
    
    ntests=ntests+1
    nerror= sdssidl_test_suite_fileio_test('ascii_tab')
    if nerror ne 0 then nfailed=nfailed+1
    add_arrval, 'file i/o ascii tab', testnames
    add_arrval, (nerror eq 0), teststatus

    ntests=ntests+1
    nerror= sdssidl_test_suite_fileio_test('ascii_white')
    if nerror ne 0 then nfailed=nfailed+1
    add_arrval, 'file i/o ascii tab', testnames
    add_arrval, (nerror eq 0), teststatus

    ntests=ntests+1
    nerror=sdssidl_test_suite_fileio_test('binary') ne 0
    if nerror ne 0 then nfailed=nfailed+1
    add_arrval, 'file i/o binary', testnames
    add_arrval, (nerror eq 0), teststatus

    ntests=ntests+1
    nerror= sdssidl_test_suite_htm_test()
    if nerror ne 0 then nfailed=nfailed+1
    add_arrval, 'HTM', testnames
    add_arrval, (nerror eq 0), teststatus

    ntests=ntests+1
    nerror=sdssidl_test_suite_total_int_test()
    if nerror ne 0 then nfailed=nfailed+1
    add_arrval, 'total_int', testnames
    add_arrval, (nerror eq 0), teststatus

    ntests=ntests+1
    nerror=sdssidl_test_suite_gauleg_test()
    if nerror ne 0 then nfailed=nfailed+1
    add_arrval, 'gauleg', testnames
    add_arrval, (nerror eq 0), teststatus

    ntests=ntests+1
    nerror=sdssidl_test_suite_sdsspix_mask_test()
    if nerror ne 0 then nfailed=nfailed+1
    add_arrval, 'sdsspix', testnames
    add_arrval, (nerror eq 0), teststatus

    ntests=ntests+1
    nerror=sdssidl_test_suite_sphpoly_comp_test()
    if nerror ne 0 then nfailed=nfailed+1
    add_arrval, 'spherical polygons', testnames
    add_arrval, (nerror eq 0), teststatus

    if n_elements(atlas_info) ne 0 then begin
        ntests=ntests+1
        nerror=sdssidl_test_suite_atlas_test(atlas_info)
        if nerror ne 0 then nfailed=nfailed+1
        add_arrval, 'atlas', testnames
        add_arrval, (nerror eq 0), teststatus
    endif


    _print_sep
    fperc = float(nfailed)/ntests
    print,'Nfailed/Ntests = '+ntostr(nfailed)+'/'+ntostr(ntests)+$
        '('+ntostr(fperc)+'%)'
    if nfailed gt 0 then begin
        w=where(teststatus eq 0, nw)
        print
        print,'The following tests failed: '
        for i=0L,nw-1 do print,'    '+testnames[w[i]]
        
    endif
return


       
END 
