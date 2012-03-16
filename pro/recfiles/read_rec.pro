
;+
; NAME:
;  READ_REC
;
;
; PURPOSE:
;  Read from the standardized file format ".rec" into an array of structures.
;  These file formats are written by the WRITE_REC procedure or using
;  the sfile.py module in esutil python (http://code.google.com/p/esutil/)
;
; CALLING SEQUENCE:
;    struct = read_rec(filename, columns=, rows=, numrows=, 
;                      columns=,
;                      hdrstruct=, status=, /silent, error=)
;
;
; INPUTS:
;  filename: Name of the file to be read.
;
; OPTIONAL INPUTS:
;
;  columns: A string or integer (zero offset) array or scalar containing the
;           names of the columns to extract from the structure.  By default all
;           columns are returned.  If the binary_read or ascii_read C++
;           programs is compiled and available, this is used to save memory and
;           speed, otherwise the whole thing is read and the desired columns
;           are extracted, with the expected limitations.
;
;  rows: A numerical array, zero offset, with the requested rows to read.  By
;        Default all rows are read.  Again, if binary_read or ascii_read is
;        available it is used (see columns).  Takes precedence over numrows=
;  numrows=: Read the first numrows of the file.  Useful if you have a very
;            large file and you just want to check out what the first few rows
;            look like without using lots of memory or time.
;  /silent: don't print informational messages
;
;
; OUTPUTS:
;  struct: The data read from the file.
;
; OPTIONAL OUTPUTS:
;  hdrstruct: The header for the idlst file.
;  error=error
;  status=status
;
; EXAMPLE:
;  file = '~/blah.rec'
;  struct = read_rec(file)
;
;-
;
;
;
;  Copyright (C) 2012  Erin Sheldon, NYU.  erin dot sheldon at gmail dot com
;
;    This program is free software; you can redistribute it and/or modify
;    it under the terms of the GNU General Public License as published by
;    the Free Software Foundation; either version 2 of the License, or
;    (at your option) any later version.
;
;    This program is distributed in the hope that it will be useful,
;    but WITHOUT ANY WARRANTY; without even the implied warranty of
;    MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
;    GNU General Public License for more details.
;
;    You should have received a copy of the GNU General Public License
;    along with this program; if not, write to the Free Software
;    Foundation, Inc., 51 Franklin St, Fifth Floor, Boston, MA  02110-1301  USA
;
;

pro rrec_swap_endian, struct, endian

	common read_rec_block, $
		c_bread_found, c_aread_found, isbig_endian, subcol, subrow

	for i=0, n_tags(struct)-1 do begin  
		;; idl built-in procedure swap_endian
		if (isbig_endian and endian[i] eq 'little') $
			  or $
		   ( (not isbig_endian) and endian[i] eq 'big') then begin
			temp = swap_endian(struct.(i))
			struct.(i) = temp
		endif
	endfor 

end 


pro rrec_extract_rows, rows, struct, nkeep

	nstruct = n_elements(struct)
	w=where(rows lt nstruct and rows ge 0, nkeep)

	if nkeep eq 0 then begin 
		print,'None of requested rows are in struct. No data returned.'
	endif else begin 
		tst = struct[rows[w]]
		struct = 0
		struct = temporary(tst)
	endelse 

end 


pro rrec_var_info, var, type_name, format

    tstr = size(var, /struct)
    nel = tstr.n_elements
    if nel eq 1 then repcnt = '' else repcnt = strtrim(nel,2)

    ;; for mrd_struct we need more description
	type_name = tstr.type_name

    case tstr.type_name of
        'INT': format = repcnt+'I0'
        'LONG': format = repcnt+'I0'
        'UINT': format = repcnt+'I0'
        'ULONG': format = repcnt+'I0'
        'LONG64': format = repcnt+'I0'
        'ULONG64': format = repcnt+'I0'

        'BYTE': format = repcnt+'I0'

        'FLOAT': format = repcnt+'g0'
        'DOUBLE': format = repcnt+'f0'
        'STRING': BEGIN 
            ;; need to read the extra character in front, either tab or ,
            ;; That is what the X does
            lenstr = ntostr(strlen(var[0]))
            maxlenarr = 'X,A'+replicate(lenstr, nel)
            format = strjoin(maxlenarr, ',')
        end 
        else: begin 
            print,'Type not supported: '+tstr.type_name
            error = -4000
        end 
    endcase 

end 

pro rrec_get_ascii_format_string, struct, format
	; need a format string only if there are strings in the structure
	; format will be undefined otherwise

	delvarx, format
	for i=0L, n_tags(struct)-1 do begin
		rrec_var_info, struct[0].(i), type_name, fmt
		add_arrval, fmt, formats
		add_arrval, type_name, type_names
	endfor


	w=where(type_names EQ 'STRING', nw)
	if nw ne 0 then begin
		format = '('+strjoin(formats,', ')+')'
	endif

end

function rrec_ascii, hdrstruct, lun, nkeep, $
        rows=rows, numrows=numrows, columns=columns, $
        silent=silent, verbose=verbose, $
        status=status


    status = 1

	common read_rec_block, $
		c_bread_found, c_aread_found, isbig_endian, subcol, subrow

	structdef = hdrStruct._structdef

    if (n_elements(rows) ne 0 or n_elements(columns) ne 0) and $
            c_aread_found then begin  
      
        ; User wants subset of data and we have ascii_read compiled
        ; This can save lots of memory and time reading subsets

        ; need CALL_FUNCTION for the systems that don't have ascii_read
        ; compiled.  For some reason, IDL gives a compile if it doesn't find it
        ; even if its never called.

		; will this work?
		tabstring = string(9b)

        if hdrStruct._delim eq ',' then begin 
            csv=1
        endif else if hdrstruct._delim eq tabstring then begin
            tab=1
        endif else begin
            ; This is probably an old space separated file from the early
            ; days
            whitespace=1
        endelse
      
        struct = CALL_FUNCTION($
            'ascii_read',$
            lun, structdef, hdrStruct._size, $
            rows=rows, columns=columns, csv=csv, tab=tab, $
            whitespace=whitespace, $
            verbose=verbose, $
            status = status)

        if size(struct, /tname) eq 'STRUCT' then begin 
            nkeep = n_elements(struct)
        endif else begin 
            nkeep = 0
        endelse 

    endif else begin 

        ; Reading ascii without ascii_read(). If there are string
		; fields, format will return defined.
		rrec_get_ascii_format_string, structdef, format 

        ; rows= takes precedence
        if n_elements(rows) eq 0 and n_elements(numrows) ne 0 then begin 
          
            nrows = numrows < hdrStruct._size
            if not keyword_set(silent) then begin 
                print,'Reading first '+strtrim(nrows,2)+' rows'
            endif 
          
        endif else begin 
            nrows = hdrStruct._size
            if not keyword_set(silent) then begin 
                print,'Reading '+strtrim(nrows,2)+' rows'
            endif 
        endelse 

        struct = replicate(structdef, hdrStruct._size)
        readf, lun, struct, format=format

        ; Extract rows
        if n_elements(rows) ne 0 then begin 
            if not keyword_set(silent) then begin 
                nextract = n_elements(rows)
                print,'Extracting '+strtrim(nextract,2)+' rows'
            endif 
          
            rrec_extract_rows, rows, struct, nkeep
            if nkeep eq 0 then return,struct
        endif else nkeep = hdrstruct._size
      
        ; extract collumns
        if n_elements(columns) ne 0 then begin 
			newstruct = extract_tags(struct, columns)
			struct=0
			struct = temporary(newstruct)
        endif 
    endelse 

    return,struct
end 

function rrec_binary, hdrstruct, lun, nkeep, $
                    rows=rows, numrows=numrows, columns=columns, $
                    verbose=verbose, silent=silent, $
                    status=status


	common read_rec_block, $
		c_bread_found, c_aread_found, isbig_endian, subcol, subrow


	structdef = hdrStruct._structdef
	if (n_elements(rows) ne 0 or n_elements(columns) ne 0) and $
		c_bread_found then begin  

		; This can save lots of memory and time reading small subsets

		; need CALL_FUNCTION for the systems that don't have binary_read
		; compiled.  For some reason, IDL gives a compile error if it doesn't
		; find it even if its never called.

		struct = CALL_FUNCTION('binary_read', $
			lun, structdef, hdrStruct._size, $
			rows=rows, columns=columns, $
			verbose=verbose, $
			status = status)

		if size(struct, /tname) EQ 'STRUCT' then begin 
			nkeep = n_elements(struct)
		endif else begin 
			nkeep = 0
		endelse 

	endif else begin 

		;; rows= takes precedence
		if n_elements(rows) eq 0 and n_elements(numrows) ne 0 then begin 

			nrows = numrows < hdrStruct._size
			if not keyword_set(silent) then begin 
				print,'Reading first '+strtrim(nrows,2)+' rows'
			endif 

		endif else begin 
			nrows = hdrStruct._size
			if not keyword_set(silent) then begin 
				print,'Reading '+strtrim(nrows,2)+' rows'
			endif 
		endelse 

		struct = replicate(structdef, nrows)
		readu, lun, struct

		;; extract requested rows
		if n_elements(rows) ne 0 then begin 
			;; tell the user how much we are reading
			if not keyword_set(silent) then begin 
				nextract = n_elements(rows)
				print,'Extracting '+strtrim(nextract,2)+' rows'
			endif 

			rrec_extract_rows, rows, struct, nkeep
			if nkeep eq 0 then return,struct
		endif else nkeep = hdrstruct._size

		;; extract requested columns
		if n_elements(columns) ne 0 then begin 
			newstruct = extract_tags(struct, columns)
			struct=0
			struct = temporary(newstruct)
		endif 

	endelse 

	; swap if this is BIG_ENDIAN machine and the byte order
	; is not IEEE standard OR if little endian and its IEEE

	rrec_swap_endian, struct, hdrStruct._endian


	return,struct
end 


function rrec_getdata, hdrStruct, lun, $
		rows=rows, numrows=numrows, columns=columns, $
		verbose=verbose, silent=silent, $
		error=error

	; Different file formats

	do_ascii=0
	if tag_exist(hdrStruct,'_delim') then begin 
		if hdrStruct._delim ne '' then do_ascii = 1
	endif

	if do_ascii then begin
		struct = rrec_ascii(hdrstruct, lun, nkeep, $
			rows=rows, numrows=numrows, $
			columns=columns, $
			verbose=verbose, $
			silent=silent)
	endif else begin
		struct = rrec_binary(hdrstruct, lun, nkeep, $
			rows=rows, numrows=numrows, $
			columns=columns,$
			verbose=verbose, $
			silent=silent)
	endelse

	if nkeep eq 0 then error = -3000 else error = 0
	return,struct

end 

function rrec_expand_tilde_gdl_kludge, fname
	; expand_tilde fails in gdl, do a kludge
	dir=file_dirname(fname)
	newdir = expand_tilde(dir)
	return, path_join(newdir, file_basename(fname))
end


function rrec_skip_header, filename

	common read_rec_block, $
		c_bread_found, c_aread_found, isbig_endian, subcol, subrow

	if (c_bread_found or c_aread_found) and ( subcol or subrow ) then begin
		openr, lun, filename, /get_lun, error=error, /stdio, bufsize=0
	endif else begin 
		openr, lun, filename, /get_lun, error=error
	endelse 

	line = ''
	readf, lun, line
	line = strtrim(line,2)
	while line ne 'END' do begin
		readf, lun, line
		line = strtrim(line,2)
	endwhile

	;; read the next line which should be empty
	readf, lun, line
	line=strtrim(line,2)
	IF line NE '' THEN message,'Expected blank line after END'

	return, lun

end


FUNCTION read_rec, filename, $
		error=error, status=status, $
		rows=rows, columns=columns, $
		numrows=numrows, $
		hdrStruct=hdrStruct, $
		verbose=verbose, $
		silent=silent

	status = 1
	IF n_elements(filename) eq 0 THEN BEGIN 
		print,'-Syntax: struct = read_rec(filename, /silent, $'
		print,'                rows=, numrows=, columns=, hdrStruct=, error=, status=)'
		return,-1
	ENDIF 

	;; can use binary_read when we want just certain columns or rows
	common read_rec_block, $
		c_bread_found, c_aread_found, isbig_endian, subcol, subrow


	if n_elements(columns) ne 0 then subcol=1 else subcol=0
	if n_elements(rows) ne 0 then subrow=1 else subrow=0


	; can we use binary_read/ascii_read?
	IF n_elements(c_bread_found) EQ 0 THEN BEGIN 
		funcNames  = routine_info(/system,/functions)

		w = where(funcNames EQ 'BINARY_READ',nw)
		IF nw EQ 0 THEN c_bread_found = 0 ELSE c_bread_found = 1

		w = where(funcNames EQ 'ASCII_READ',nw)
		IF nw EQ 0 THEN c_aread_found = 0 ELSE c_aread_found = 1

		isbig_endian = is_ieee_big()
	ENDIF 



	; expand tilde and such
	filename = rrec_expand_tilde_gdl_kludge(filename)


	; read the header and return as a structure.   
	hdrStruct = read_recheader(filename)


	; This will read through the END and the empty line afterward
	lun = rrec_skip_header(filename)


	if hdrstruct._size le 0 then begin 
		message,'nrows <= 0, something is wrong'
	endif 


	; read ascii/binary data
	struct = rrec_getdata(hdrStruct, lun, $
		rows=rows, numrows=numrows, $
		columns=columns, $
		verbose=verbose, $
		silent=silent, error=error)

	free_lun,lun

	IF error EQ 0 THEN status = 0
	return,struct

END 
