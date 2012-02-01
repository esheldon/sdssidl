;+
; Class
;   This is an IDL class file for working with a columns database.
;
;   The columns database is defined by its implementation in
;   python
;       http://code.google.com/p/pycolumns/
;
;   The columns database is just a directory with some files
;   if type 'rec' inside.  It is designed for write once (or append) 
;   and  read many, optimized for speed of reading.  It is not
;   inherently relational, although relations may exist.
;
; Examples:
;
;   Say you have a columns database in 'maxbcg-input-dr8.cols'
;       IDL> coldir='maxbcg-input-dr8.cols'
;       IDL> c=obj_new('columns',coldir)
;
;   To see what data you have
;       IDL> c->show
;       IDL> info=c->colinfo()
;
;   To read some data
;       IDL> data=c->read(colnames, rows=)
;
;   so to read *all rows* of ra,dec
;
;       IDL> data=c->read(['ra','dec'])
;
;   to read rows [25,33,1999]
;
;       IDL> data=c->read(['ra','dec'], rows=[25,33,1999])                                                                                                                    
;
;   Usually you will want to make some selections on the data based on
;   certain columns and then read more columns
;
;       IDL> radec=c->read(['ra','dec'])
;       IDL> w=where(ra > 80 and ra < 100 and dec > 0 and dec < 5)
;       IDL> data=c->read(some_columns, rows=w)
;-
function columns::init, coldir
    if n_elements(coldir) eq 0 then begin
        on_error, 2
        message,'send a directory during initialization'
    endif

    self.dir = coldir
    if file_test(coldir) then begin
        self->load_columns
    endif

    return, 1
end

pro columns::show
    print,'Columns Directory:'
    print,'  dir: ',self.dir
    if self.ncol eq 0 then return

    print,'Columns:'
    print,'  name             type  dtype      size'
    print,'  --------------------------------------------------'
    ci = self->colinfo()
    for i=0L,self.ncol-1 do begin
        if strlen(ci[i].name) gt 15 then begin
            print,format='("  ",a)', ci[i].name
            print,format='(a23, $)',ci[i].type
        endif else begin
            print,format='("  ",a-15, " ",a5, $)', ci[i].name, ci[i].type
        endelse
        print,format='(" ",a-10," ",i0)', ci[i].datatype+ci[i].shape, ci[i].size
        ;print,format='("  ",a-15,"  ",a5, " ",a-10," ",i0)', ci[i].name, ci[i].type, ci[i].datatype+ci[i].shape, ci[i].size
    endfor
end

function columns::colinfo
    if self.ncol gt 0 then begin
        if not ptr_valid(self.colinfo) then begin
            on_error, 2
            message,'colinfo struct pointer is not valid. expected struct with length '+string(self.ncol)
        endif
        return, *self.colinfo
    endif else begin
        return, -1
    endelse
end

function columns::read, colnames, rows=rows, all=all, verbose=verbose
    ncol = n_elements(colnames)
    if ncol eq 0 and not keyword_set(all) then begin
        on_error, 2
        print,'usage: c->read(colnames, rows=, /all, /verbose)'
        print,'  use /all to read all columns'
    endif
    if ncol eq 0 and keyword_set(all) then begin
        colnames = (*self.colinfo).name
    endif

    verbose = keyword_set(verbose)
    if verbose then silent=0 else silent=1

    stdef = self->_structdef(colnames, colid=colid)

    for i=0L, n_elements(colid)-1 do begin
        id = colid[i]

        if verbose then begin
            print,f='("  column: ",a,"  ", $)', (*self.colinfo)[id].name
        endif
        fname = (*self.colinfo)[id].filename
        data = read_rec(fname, rows=rows, silent=silent)

        if i eq 0 then begin
            st = replicate(stdef, n_elements(data))
        endif

        if (*self.colinfo)[id].type eq 'rec' then begin
            for j=0L, n_tags(data)-1 do begin
                st.(i).(j) = data.(j)
            endfor
        endif else begin
            st.(i) = data.(0)
        endelse
    endfor

    return, st

end

function columns::_structdef, colnames, colid=colid
    ci = self->colinfo() 
    match, colnames, ci.name, minput, colid
    if minput[0] eq -1 then begin
        on_error, 2
        message,'no column names matched: ['+strjoin(colnames, ', ')+']'
    endif
    
    if n_elements(minput) ne n_elements(colnames) then begin
        cbad = colnames
        remove, minput, cbad
        on_error, 2
        message,'column name(s) did not match: ['+strjoin(cbad, ', ')+']'
    endif

    for i=0L, n_elements(colnames)-1 do begin
        ii = colid[i]

        if i eq 0 then begin
            st = create_struct(ci[ii].name, *ci[ii].datatype_actual)
        endif else begin
            st = create_struct(st, ci[ii].name, *ci[ii].datatype_actual)
        endelse
    endfor

    return, st
end


pro columns::load_columns

    self->_free_colinfo

    recfiles = file_search(self.dir, '*.rec')    
    colfiles = file_search(self.dir, '*.col')    

    if recfiles[0] ne '' then begin
        add_arrval, recfiles, files
    endif
    if colfiles[0] ne '' then begin
        add_arrval, colfiles, files
    endif

    self.ncol = n_elements(files)
    if self.ncol eq 0 then return

    files = files[sort(files)]

    st = self->_colinfo(files)
    self.colinfo = ptr_new(st, /no_copy)

end

pro columns::create
    if not file_test(self.dir) then begin
        print,'Making columns directory: ',self.dir
        file_mkdir, self.dir
    endif
end



function columns::_colinfo, files

    nf = n_elements(files)
    st = self->_colinfo_struct(nf)
    for i=0L, nf-1 do begin
        st[i].filename = files[i]
        st[i].name = self->_extract_name(files[i])

        h = read_recheader(files[i])

        if strpos(files[i], '.rec') ne -1 then begin
            st[i].type = 'rec'
            st[i].datatype = 'STRUCT'
            st[i].size = h._size

            st[i].datatype_actual = ptr_new(h._structdef)

        endif else if strpos(files[i], '.col') ne -1 then begin
            st[i].type = 'col'

            if n_tags(h._structdef) ne 1 then begin
                on_error, 2
                message,'col types should not be compound'
            endif

            st[i].datatype = size(h._structdef.(0), /tname)
            st[i].size = h._size

            if n_elements(h._structdef.(0)) gt 1 then begin
                shape=size(h._structdef.(0),/dim)
                st[i].shape = '['+strjoin( string(shape,f='(i0)'), ', ' )+']'
            endif

            st[i].datatype_actual = ptr_new(h._structdef.(0))

        endif else begin
            on_error, 2
            message,'only support rec and col types'
        endelse


    endfor
          
    return, st
end

function columns::_colinfo_struct, n
    st = {filename: '', $
          name: '', $
          type: '', $
          datatype: '', $
          shape: '', $
          size: 0LL, $
          datatype_actual: ptr_new()}

    if n_elements(n) ne 0 then begin
        st=replicate(st, n)
    endif

    return, st
end

function columns::_extract_name, file
    ; get base names without extension
    nf = n_elements(file)
    if nf eq 0 then begin
        on_error, 2
        message,'usage: c->_extract_name(file)'
    endif

    f = file
    f = file_basename(f)

    ; always remove .gz endings
    f=repstr(f, '.gz', '')

    p=strpos(f, '.', /reverse_search)
    if p eq -1 then begin
        on_error, 2
        message,'expected a (not .gz) extension in file: '+file[i]
    endif
    name = strmid(f, 0, p)

    return, name 
end

pro columns::_free_colinfo
    if ptr_valid(self.colinfo) then begin
        for i=0L, n_elements(*self.colinfo)-1 do begin
            ptr_free, (*self.colinfo)[i].datatype_actual
        endfor
        ptr_free, self.colinfo
    endif
end


function columns::cleanup
    self->_free_colinfo
    return, 1
end


pro columns__define

    struct = {              $ 
        columns,            $
        dir: '',            $
        ncol: 0L,           $
        colinfo: ptr_new()  $
    }

end 
