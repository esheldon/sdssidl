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
;  Eli Rykoff (16-March-2012): Added _ri_mrd_struct which works without execute()
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

function _ri_mrd_struct, names, values, nrow, no_execute = no_execute,  $
    structyp=structyp,  tempdir=tempdir, silent=silent, old_struct=old_struct

compile_opt idl2

; Keywords TEMPDIR, SILENT and OLD_STRUCT no longer do anything but are kept
; for backward compatibility.


;; noexecute = keyword_set(no_execute) or lmgr(/vm)
 noexecute = 1

 if noexecute then begin

    ntags = n_elements(names)
    for i=0,ntags-1 do begin
;
; create variable with the specified data type
;
	case strlowcase(values[i]) of 
;
; scalar values
;
	    '0b': v = 0B
	    '0' : v = 0S
            '0u': v = 0US
	    '0l': v = 0L
	    '0ll' : v = 0LL
            '0ul' : v = 0UL
            '0ull' : v = 0ULL
	    '0.': v = 0.0
            '0.0': v = 0.0
            '0.0d': v = 0.0d0
	    '0.0d0': v = 0.0d0
 	    '0.d0': v = 0.0d0
             '" "': v = " "          ;Added July 2004
	    'complex(0.,0.)': v = complex(0.,0.)
	    'dcomplex(0.d0,0.d0)': v = dcomplex(0.d0,0.d0)
;
; strings and arrays
;`
	    else: begin	     
	        value = strlowcase(values[i])
		remchar,value,"'"
		remchar,value,'"'   
		if strlen(value) EQ 1 then v= value else begin 
	        type = gettok(value,'(')
		if type eq 'string' then $
			junk = gettok(value,',')      ;remove "replicate(32b"
		dimen_string = gettok(value,')')	
		dimen = long(strsplit(dimen_string,',',/extract))
		case type of
                    'bytarr': v = make_array(dimen=dimen,/byte)
		    'intarr': v = make_array(dimen=dimen,/int)
                    'uintarr': v = make_array(dimen=dimen,/uint)
		    'fltarr': v = make_array(dimen=dimen,/float)
		    'lonarr': v = make_array(dimen=dimen,/long)
                    'ulonarr': v = make_array(dimen=dimen,/ulong)
		    'lon64arr': v = make_array(dimen=dimen,/l64)
                    'ulon64arr' : v = make_array(dimen=dimen,/ul64)
		    'dblarr': v = make_array(dimen=dimen,/double)
		    'complexarr': v = make_array(dimen=dimen,/complex)
		    'dcomplexarr': v = make_array(dimen=dimen,/dcomplex)
                    'ptr_new': v = ptr_new()
                    'string': begin
                        ndimen = n_elements(dimen)-1
                        if ndimen gt 0 then begin
                            v = make_array(dimen=dimen[1:*],/string)
                            v[*] = string(replicate(32B,dimen[0]))
                        end else v = string(replicate(32B,dimen[0]))
                    end
                    'mkstr': v = string(replicate(32B,dimen))
                    'strarr': begin
                        ndimen = n_elements(dimen)-1
                        if ndimen gt 0 then begin
                            v = make_array(dimen=dimen[1:*],/string)
                            v[*] = string(replicate(32B,dimen[0]))
                        endif
                    end
                    else: message,'ERROR - Invalid field value: ' + values[i]		      
                endcase
            endelse 

	    end
	endcase     	
	if i eq 0 then struct = create_struct(names[i],v) $
		  else struct = create_struct(temporary(struct),names[i],v)
    end; for i    

endif else begin

; Build up the structure use a combination of execute and
; create_struct calls.  Basically we build as many rows as
; will fit in an execute call and create that structure.  Then
; we append that structure to whatever we've done before using
; create_struct

nel = N_elements(names)
strng = "a={"

comma = ' '
for i=0,nel-1 do  begin
    fval = values[i]
    if (fval eq '0') then fval = '0s'
  
   ; Now for each element put in a name/value pair.
    tstrng = strng + comma+names[i] + ':' + fval
    
    ; The nominal max length of the execute is 131
    ; We need one chacacter for the "}"
    if strlen(tstrng) gt 130 then begin
        strng = strng + "}"
        res = execute(strng)
	if  res eq 0 then return, 0
        struct = n_elements(struct) eq 0 ? a: $
	         create_struct(temporary(struct), a)
	strng = "a={" + names[i] + ":" + fval
	
    endif else strng = tstrng
    comma = ","

endfor
	

if strlen(strng) gt 3 then begin
    strng = strng + "}"
    res = execute(strng)
     if  res eq 0 then return, 0
     struct = n_elements(struct) eq 0 ? a : create_struct(temporary(struct), a)
 endif
 
endelse
if keyword_set(structyp) then $
     struct = create_struct(temporary(struct), name=structyp)


if nrow le 1 then return, struct $
             else return, replicate(struct, nrow)

end




PRO ri_swap_endian, struct

  FOR i=0, n_tags(struct)-1 DO BEGIN  
      ;; Call RSI procedure swap_endian
      temp = swap_endian(struct.(i))
      struct.(i) = temp
  ENDFOR 

END 

FUNCTION ri_match_columns, hdrStruct, columns

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

PRO ri_extract_columns, hdrStruct, columns, struct

  mstr = ri_match_columns(hdrStruct, columns)

  IF mstr[0] EQ -1 THEN BEGIN 
      print,'None of the requested columns match the existing columns.'
      print,'Returning all data'
  ENDIF ELSE BEGIN 
      ;; Only need to extract if they are a subset of the original
      ;; columns
      IF n_elements(mstr) LT n_elements(hdrStruct.field_names) THEN BEGIN 

          tst = _ri_mrd_struct(hdrStruct.field_names[mstr], $
                           hdrStruct.field_descriptions[mstr], $
                           n_elements(struct))
          struct_assign, struct, tst, /nozero
          struct = 0
          struct = temporary(tst)
      ENDIF
  ENDELSE 
      
END 

PRO ri_extract_rows, rows, struct, nkeep

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


FUNCTION ri_ascii, hdrStruct, lun, nkeep, $
        rows=rows, numrows=numrows, columns=columns, $
        silent=silent, verbose=verbose, $
        status=status


    status = 1
    COMMON read_idlstruct_block, c_bread_found, c_aread_found, isbig_endian

    structdef = _ri_mrd_struct($
        hdrStruct.field_names, hdrStruct.field_descriptions, 1)


    if (n_elements(rows) ne 0 or n_elements(columns) ne 0) and $
            c_aread_found then begin  
      
        ; User wants subset of data and we have ascii_read compiled
        ; This can save lots of memory and time reading subsets

        ; need CALL_FUNCTION for the systems that don't have ascii_read
        ; compiled.  For some reason, IDL gives a compile if it doesn't find it
        ; even if its never called.

        if hdrStruct.data_format eq 'ASCII_CSV' then begin 
            csv=1
        endif else if hdrstruct.data_format eq 'ASCII_TAB' then begin
            tab=1
        endif else begin
            ; This is probably an old space separated file from the early
            ; days
            whitespace=1
        endelse
      
        struct = CALL_FUNCTION($
            'ascii_read',$
            lun, structdef, hdrStruct.nrows, $
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
          
            nrows = numrows < hdrStruct.nrows
            if not keyword_set(silent) then begin 
                print,'Reading first '+strtrim(nrows,2)+' rows'
            endif 
          
        endif else begin 
            nrows = hdrStruct.nrows
            if not keyword_set(silent) then begin 
                print,'Reading '+strtrim(nrows,2)+' rows'
            endif 
        endelse 

        struct = replicate(structdef, hdrStruct.nrows)
        readf, lun, struct, format=format

        ; Extract rows
        if n_elements(rows) ne 0 then begin 
            if not keyword_set(silent) then begin 
                nextract = n_elements(rows)
                print,'Extracting '+strtrim(nextract,2)+' rows'
            endif 
          
            ri_extract_rows, rows, struct, nkeep
            if nkeep eq 0 then return,struct
        endif else nkeep = hdrstruct.nrows
      
        ; extract collumns
        if n_elements(columns) ne 0 then begin 
            ri_extract_columns, hdrStruct, columns, struct  
        endif 
    endelse 

    return,struct
end 

FUNCTION ri_binary, hdrStruct, lun, nkeep, $
                    rows=rows, numrows=numrows, columns=columns, $
                    verbose=verbose, silent=silent, $
                    status=status

  COMMON read_idlstruct_block, c_bread_found, c_aread_found, isbig_endian


  structdef = _ri_mrd_struct(hdrStruct.field_names, $
                             hdrStruct.field_descriptions, $
                             1)

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
                             lun, structdef, hdrStruct.nrows, $
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

          nrows = numrows < hdrStruct.nrows
          IF NOT keyword_set(silent) THEN BEGIN 
              print,'Reading first '+strtrim(nrows,2)+' rows'
          ENDIF 

      ENDIF ELSE BEGIN 
          nrows = hdrStruct.nrows
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

          ri_extract_rows, rows, struct, nkeep
          IF nkeep EQ 0 THEN return,struct
      ENDIF ELSE nkeep = hdrstruct.nrows
      
      ;; extract requested columns
      IF n_elements(columns) NE 0 THEN BEGIN 
          ri_extract_columns, hdrStruct, columns, struct  
      ENDIF 

  ENDELSE 

  ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
  ;; swap if this is BIG_ENDIAN machine and the byte order
  ;; is not IEEE standard OR if little endian and its IEEE
  ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
      
  IF (isbig_endian AND $
      ( (hdrStruct.byte_order EQ 'NATIVE_LITTLE_ENDIAN') OR $
        (hdrStruct.byte_order EQ 'IEEE_LITTLE')) ) THEN BEGIN 
      
      IF NOT keyword_set(silent) THEN BEGIN 
          print,'Byte swapping to IEEE_BIG format'
      ENDIF 
      ri_swap_endian, struct 
      
  ENDIF ELSE IF ( (NOT isbig_endian) AND $
                  ( (hdrStruct.byte_order EQ 'IEEE') OR $
                    (hdrStruct.byte_order EQ 'IEEE_BIG')) ) THEN BEGIN 
      IF NOT keyword_set(silent) THEN BEGIN 
          print,'Byte swapping to IEEE_LITTLE format'
      ENDIF 
      ieee_to_host, struct
      
  ENDIF


  return,struct
END 


FUNCTION ri_getdata, hdrStruct, lun, $
                     rows=rows, numrows=numrows, columns=columns, $
                     verbose=verbose, silent=silent, $
                     error=error

  ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
  ;; Different file formats
  ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

  df=strlowcase(hdrstruct.data_format)
  if strmatch(df, '*ascii*') then begin 
      struct = ri_ascii(hdrstruct, lun, nkeep, $
                        rows=rows, numrows=numrows, $
                        columns=columns, $
                        verbose=verbose, $
                        silent=silent)
  endif else if strmatch(df,'*binary*') THEN BEGIN 
      struct = ri_binary(hdrstruct, lun, nkeep, $
                         rows=rows, numrows=numrows, $
                         columns=columns,$
                         verbose=verbose, $
                         silent=silent)
  ENDIF ELSE BEGIN 
      free_lun, lun
      print,'File format '+hdrStruct.data_format+' not supported'
      error = -2000
      return,-1
  ENDELSE 

  IF nkeep EQ 0 THEN error = -3000 ELSE error = 0
  return,struct

END 

function _expand_tilde_gdl_kludge, fname
	; expand_tilde fails in gdl, do a kludge
	dir=file_dirname(fname)
	newdir = expand_tilde(dir)
	return, path_join(newdir, file_basename(fname))
end



FUNCTION read_idlstruct, filename_in, $
                         error=error, status=status, $
                         rows=rows, columns=columns, $
                         numrows=numrows, $
                         hdrStruct=hdrStruct, $
                         verbose=verbose, $
                         silent=silent

  status = 1
  IF n_elements(filename_in) eq 0 THEN BEGIN 
      print,'-Syntax: struct = read_idlstruct(filename, /silent, $'
      print,'                rows=, numrows=, columns=, hdrStruct=, error=, status=)'
      return,-1
  ENDIF 

  ;; can use binary_read when we want just certain columns or rows
  COMMON read_idlstruct_block, c_bread_found, c_aread_found, isbig_endian

  filename = _expand_tilde_gdl_kludge(filename_in)

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
  hdrStruct = read_idlheader(lun)

  IF hdrStruct.nrows LE 0 THEN BEGIN 
      free_lun, lun
      print,'NROWS = 0.  No data read'
      error = -1000
      return, -1
  ENDIF 

  ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
  ;; Create the structure and replicate it nrows
  ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

  struct = ri_getdata(hdrStruct, lun, $
                      rows=rows, numrows=numrows, $
                      columns=columns, $
                      verbose=verbose, $
                      silent=silent, error=error)

  ;; free the unit if just filename was entered
  free_lun,lun

  IF error EQ 0 THEN status = 0
  return,struct

END 

