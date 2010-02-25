
;+
; NAME:
;  READ_IDLSTRUCT
;
;
; PURPOSE:
;  Read from the standardized file format ".st" into an array of structures.
;  These file formats are written by the WRITE_IDLSTRUCT procedure.  See the
;  documentation for that procedure for more info.
;
;
; CATEGORY:
;  File I/O
;
;
; CALLING SEQUENCE:
;    struct = read_idlstruct(filename, columns=, rows=, numrows=, 
;                           hdrstruct=, status=, /silent, error=)
;
;
; INPUTS:
;  filename: Name of the file to be read.
;
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
; PROCEDURES CALLED:
;  READ_IDLHEADER
;  (SWAP_ENDIAN)
;  (IEEE_TO_HOST)
;  (ASCII_READ)
;  (BINARY_READ)
;
; EXAMPLE:
;  file = '~/blah.st'
;  struct = read_idlstruct(file)
;
; MODIFICATION HISTORY:
;  Created  02-Jul-2004 Erin Sheldon, UofChicago
;  ES: Added columns keyword.  07-Mar-2004  
;  ES: Added use of binary_read C function when available if columns 
;      or rows are requested.  7-Nov-2005
;  ES: Added use of ascii_read C function when available and if
;      columns, rows requested.  28-April-2006
;
;-
;
;
;
;  Copyright (C) 2005  Erin Sheldon, NYU.  erin dot sheldon at gmail dot com
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

	common read_rec_block, c_bread_found, c_aread_found, isbig_endian

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

FUNCTION rrec_match_columns, hdrStruct, columns

  ;; Always returned in numerical order, the order they are in
  ;; the structure

  ;; remove duplicates
  col = columns[rem_dup(columns)]

  IF size(col, /tname) EQ 'STRING' THEN BEGIN 
      ;; column names sent
      match, hdrStruct.field_names, strlowcase(col), mstr, mcol

      IF mstr[0] NE -1 THEN BEGIN 
          nrequest = n_elements(columns)
          ngood = n_elements(mstr)
          IF ngood NE nrequest THEN BEGIN 
              ind = lindgen(nrequest)
              remove, mcol, ind
              IF n_elements(ind) EQ 1 THEN BEGIN 
                  message,$
                    "Column '"+col[ind]+"' not found. Skipping",/inf
              ENDIF ELSE BEGIN 
                  message,$
                    "Columns ['"+strjoin(col[ind], "','")+$
                    "'] not found. Skipping",/inf
              ENDELSE 
          ENDIF 
      ENDIF 
  ENDIF ELSE BEGIN 
      ;; columns numbers sent
      ntags = n_elements(hdrStruct.field_names)
      w = where(col LT ntags, nw)
      IF nw EQ 0 THEN mstr = -1 ELSE mstr = col[w]
  ENDELSE 

  mstr = mstr[sort(mstr)]
  return,mstr

END 

PRO rrec_extract_columns, hdrStruct, columns, struct

  mstr = rrec_match_columns(hdrStruct, columns)

  IF mstr[0] EQ -1 THEN BEGIN 
      print,'None of the requested columns match the existing columns.'
      print,'Returning all data'
  ENDIF ELSE BEGIN 
      ;; Only need to extract if they are a subset of the original
      ;; columns
      IF n_elements(mstr) LT n_elements(hdrStruct.field_names) THEN BEGIN 
          
          tst = mrd_struct(hdrStruct.field_names[mstr], $
                           hdrStruct.field_descriptions[mstr], $
                           n_elements(struct))
          struct_assign, struct, tst, /nozero
          struct = 0
          struct = temporary(tst)
      ENDIF
  ENDELSE 
      
END 

PRO rrec_extract_rows, rows, struct, nkeep

  nstruct = n_elements(struct)
  w=where(rows LT nstruct AND rows GE 0, nkeep)

  IF nkeep EQ 0 THEN BEGIN 
      print,'None of requested rows are in struct. No data returned.'
  ENDIF ELSE BEGIN 
      tst = struct[rows[w]]
      struct = 0
      struct = temporary(tst)
  ENDELSE 

END 


FUNCTION rrec_ascii, hdrStruct, lun, nkeep, $
        rows=rows, numrows=numrows, columns=columns, $
        silent=silent, verbose=verbose, $
        status=status


    status = 1
    COMMON read_rec_block, c_bread_found, c_aread_found, isbig_endian


	structdef = hdrStruct._structdef

    if (n_elements(rows) ne 0 or n_elements(columns) ne 0) and $
            c_aread_found then begin  
      
        ; User wants subset of data and we have ascii_read compiled
        ; This can save lots of memory and time reading subsets

        ; need CALL_FUNCTION for the systems that don't have ascii_read
        ; compiled.  For some reason, IDL gives a compile if it doesn't find it
        ; even if its never called.

		; will this work?
		tab = string(9b)

        if hdrStruct._delim eq ',' then begin 
            csv=1
        endif else if hdrstruct._delim eq tab then begin
            tab=1
        endif else begin
            ; This is probably an old space separated file from the early
            ; days
            whitespace=1
        endelse
      
        struct = CALL_FUNCTION($
            'ascii_read',$
            lun, structdef, hdrStruct._nrows, $
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

        ; Reading ascii without ascii_read() and there are string fields
        ; In this case we need a format string
        w=where(hdrStruct.field_types EQ 'STRING', nw)
        if nw ne 0 then begin
            format = '('+strjoin(hdrstruct.field_input_formats,', ')+')'
        endif
      
        ; rows= takes precedence
        if n_elements(rows) eq 0 and n_elements(numrows) ne 0 then begin 
          
            nrows = numrows < hdrStruct._nrows
            if not keyword_set(silent) then begin 
                print,'Reading first '+strtrim(nrows,2)+' rows'
            endif 
          
        endif else begin 
            nrows = hdrStruct._nrows
            if not keyword_set(silent) then begin 
                print,'Reading '+strtrim(nrows,2)+' rows'
            endif 
        endelse 

        struct = replicate(structdef, hdrStruct._nrows)
        readf, lun, struct, format=format

        ; Extract rows
        if n_elements(rows) ne 0 then begin 
            if not keyword_set(silent) then begin 
                nextract = n_elements(rows)
                print,'Extracting '+strtrim(nextract,2)+' rows'
            endif 
          
            rrec_extract_rows, rows, struct, nkeep
            if nkeep eq 0 then return,struct
        endif else nkeep = hdrstruct._nrows
      
        ; extract collumns
        if n_elements(columns) ne 0 then begin 
            rrec_extract_columns, hdrStruct, columns, struct  
        endif 
    endelse 

    return,struct
end 

FUNCTION rrec_binary, hdrStruct, lun, nkeep, $
                    rows=rows, numrows=numrows, columns=columns, $
                    verbose=verbose, silent=silent, $
                    status=status

  COMMON read_rec_block, c_bread_found, c_aread_found, isbig_endian



  structdef = hdrStruct._structdef
  IF (n_elements(rows) NE 0 OR n_elements(columns) NE 0) AND $
    c_bread_found THEN BEGIN  

      ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
      ;; User wants subset of data and we have binary_read compiled
      ;; This can save lots of memory and time reading small subsets
      ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

      ;; need CALL_FUNCTION for the systems that don't have binary_read
      ;; compiled.  For some reason, IDL gives a compile error if it doesn't
      ;; find it even if its never called.

      struct = CALL_FUNCTION('binary_read', $
                             lun, structdef, hdrStruct._nrows, $
                             rows=rows, columns=columns, $
                             verbose=verbose, $
                             status = status)

      IF size(struct, /tname) EQ 'STRUCT' THEN BEGIN 
          nkeep = n_elements(struct)
      ENDIF ELSE BEGIN 
          nkeep = 0
      ENDELSE 

  ENDIF ELSE BEGIN 

      ;; rows= takes precedence
      IF n_elements(rows) EQ 0 AND n_elements(numrows) NE 0 THEN BEGIN 

          nrows = numrows < hdrStruct._nrows
          IF NOT keyword_set(silent) THEN BEGIN 
              print,'Reading first '+strtrim(nrows,2)+' rows'
          ENDIF 

      ENDIF ELSE BEGIN 
          nrows = hdrStruct._nrows
          IF NOT keyword_set(silent) THEN BEGIN 
              print,'Reading '+strtrim(nrows,2)+' rows'
          ENDIF 
      ENDELSE 

      struct = replicate(structdef, nrows)
      readu, lun, struct

      ;; extract requested rows
      IF n_elements(rows) NE 0 THEN BEGIN 
          ;; Tell the user how much we are reading
          IF NOT keyword_set(silent) THEN BEGIN 
              nextract = n_elements(rows)
              print,'Extracting '+strtrim(nextract,2)+' rows'
          ENDIF 

          rrec_extract_rows, rows, struct, nkeep
          IF nkeep EQ 0 THEN return,struct
      ENDIF ELSE nkeep = hdrstruct._nrows
      
      ;; extract requested columns
      IF n_elements(columns) NE 0 THEN BEGIN 
          rrec_extract_columns, hdrStruct, columns, struct  
      ENDIF 

  ENDELSE 

  ; swap if this is BIG_ENDIAN machine and the byte order
  ; is not IEEE standard OR if little endian and its IEEE
      
  rrec_swap_endian, struct, hdrStruct._endian


  return,struct
END 


function rrec_getdata, hdrStruct, lun, $
		rows=rows, numrows=numrows, columns=columns, $
		verbose=verbose, silent=silent, $
		error=error

	; Different file formats

	if hdrstruct._delim ne 'none' then begin 
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



FUNCTION read_rec, filename_in, $
                         error=error, status=status, $
                         rows=rows, columns=columns, $
                         numrows=numrows, $
                         hdrStruct=hdrStruct, $
                         verbose=verbose, $
                         silent=silent

  status = 1
  IF n_elements(filename_in) eq 0 THEN BEGIN 
      print,'-Syntax: struct = read_rec(filename, /silent, $'
      print,'                rows=, numrows=, columns=, hdrStruct=, error=, status=)'
      return,-1
  ENDIF 

  ;; can use binary_read when we want just certain columns or rows
  COMMON read_rec_block, c_bread_found, c_aread_found, isbig_endian

  filename = rrec_expand_tilde_gdl_kludge(filename_in)

  IF n_elements(c_bread_found) EQ 0 THEN BEGIN 
      funcNames  = routine_info(/system,/functions)

      w = where(funcNames EQ 'BINARY_READ',nw)
      IF nw EQ 0 THEN c_bread_found = 0 ELSE c_bread_found = 1

      w = where(funcNames EQ 'ASCII_READ',nw)
      IF nw EQ 0 THEN c_aread_found = 0 ELSE c_aread_found = 1

      isbig_endian = is_ieee_big()
  ENDIF 


  ;; Open the file, watching for errors. Need /stdio and bufsize=0
  ;; for binary_read and ascii_read.  Note reading with /stdio is
  ;; much slower if all fields are being read and we are just using
  ;; readu or readf in IDL, so we check first

  IF ( (c_bread_found OR c_aread_found) AND $
       ( (n_elements(columns) NE 0) OR (n_elements(rows) NE 0) ) ) THEN BEGIN
      openr, lun, filename, /get_lun, error=error, /stdio, bufsize=0
  ENDIF ELSE BEGIN 
      openr, lun, filename, /get_lun, error=error
  ENDELSE 
  IF error NE 0 THEN BEGIN 
      print,'Error opening file '+filename+': '+!error_state.sys_msg
      return,-1
  ENDIF  

  ;; read the header. This will read through the END and the empty
  ;; line afterward
  hdrStruct = read_recheader(lun)


  IF hdrStruct._nrows LE 0 THEN BEGIN 
      free_lun, lun
      print,'NROWS = 0.  No data read'
      error = -1000
      return, -1
  ENDIF 

  ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
  ;; Create the structure and replicate it nrows
  ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

  struct = rrec_getdata(hdrStruct, lun, $
                      rows=rows, numrows=numrows, $
                      columns=columns, $
                      verbose=verbose, $
                      silent=silent, error=error)

  ;; free the unit if just filename was entered
  free_lun,lun

  IF error EQ 0 THEN status = 0
  return,struct

END 
