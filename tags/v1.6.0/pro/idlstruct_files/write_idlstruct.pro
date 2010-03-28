;+
; NAME:
;  WRITE_IDLSTRUCT
;
;
; PURPOSE: 
;   Write a structure array to a standardized file type ".st".  The file is
;   self-describing such that it can be read using READ_IDLSTRUCT() without
;   prior knowledge of the file structure.  The data is either written as
;   unformatted binary in the native or IEEE formats or as ascii when the
;   /ascii keyword is sent.  These procedures provide a natural way to write
;   and read IDL data in ascii or binary form.  Unlike fits files, all IDL data
;   types other than complex and pointers are supported for both formats,
;   including array fields for strings.  Data can be appended naturally using
;   the /append keyword.  Because the user can use the native byte order, read
;   of files can be much faster, up to a factor of 3 over fits files.
;
;
; CATEGORY:
;  File I/O
;
; CALLING SEQUENCE:
;  write_idlstruct, struct, filename, 
;                   /ascii, /csv, /no_c_write, 
;                   /ieee_big, /ieee_little, 
;                   /append, /noheader, 
;                   hdrStruct=hdrStruct, 
;                   info_struct=info_struct, 
;                   error=error, 
;                   status=status
;
;
; INPUTS:
;
;  struct: An IDL structure array. All basic IDL data types other than
;          complex and pointers are supported for both binary
;          and ascii formats.
;
;  filename: Name of the file to be written or appended.
;
; OPTIONAL INPUTS:
;
;  hdrStruct=: The user can input keyword-value to be written into the header
;              through this structure.  For example: 
;                 IDL> hdr={date:"12-May-1974", val1:[2.5, 66.7], val2:33} 
;                 IDL> write_idlstruct, struct, file, hdrStruct=hdr
;
;  info_struct=: This structure contains info on the makeup of the structure,
;                and output formatting for ascii.  It can be returned on the
;                first call and sent again when appending to save time. 
;
; KEYWORD PARAMETERS:
;
;  /ascii: write an ascii data file rather than unformatted binary.  If the
;          system routine ASCII_WRITE is present it will be used unless
;          /no_c_write is sent
;
;  /csv: For ascii, use commas to separate values rather than the default
;        tabs. 
;
;  /no_c_write: Don't attempt to use the ascii_write C DLM.
;
;  /ieee_big: Use IEEE XDR byte ordering rather than the native for
;             LITTLE_ENDIAN machines (they are the same for BIG_ENDIAN) Using
;             native format can increase the speed of reads and writes by a
;             large amount, but then BIG_ENDIAN users must swap the byteorder
;             to use the data. All this swapping is done automatically by
;             read_idlstruct so, unlike fits, the user can use whatever is
;             desired.  
;
;  /ieee_little: Use LITTLE_ENDIAN byte ordering.  
;
;  /append: append to the file and update the header.  Strings will be written
;           with the same length as previous writes; otherwise the file could
;           not be read (except for ascii when ascii_read is available).  
;
;  /noheader: don't write a header.
;
; OUTPUTS:
;  The structure is written to a file
;
;
; OPTIONAL OUTPUTS:
;  info_struct: see above
;  status=: 0 for success, otherwise failure.
;  error=:  Description of error that occured.
;     -1000: Input data is not a structure.
;     -3000: Unsupported complex type sent
;     -4000: Other unsupported type
;     Plus the errors returned by openw
; 
;
; SDSSIDL ROUTINES CALLED:
;  STRINGREP
;
; SIDE EFFECTS:
;
;  For string fields in the structure, the strings are padded to the largest
;  string in the array for that field.  This can take up a lot of extra space
;  if you have one string in a million that is really long, so you might
;  consider trimming them yourself.  Also, beware of appending with strings
;  since write_idlstruct will use the previous string length.
;
; COMMON BLOCKS:
;  write_idlstruct_block, ROW_STRING_FORMAT, isbig_endian, version, 
;                         use_c_write, old_info_struct
;
; EXAMPLE:
;  struct = {a:findgen(10), b:dindgen(10,10), c:23353233}
;  struct = replicate(struct, 1000)
;  write_idlstruct, struct, 'test.st'
;  write_idlstruct, struct, 'test2.st', /ascii
;
;
; MODIFICATION HISTORY:
;   Created  02-Jul-2004 Erin Sheldon, UofChicago
;   Improved /append now ensures proper string lengths.  7-Nov-2005 E.S.
;   Added support for CSV.  Made default delimiter tab rather than space.
;       Supported styles are tab and csv now, but old space delimited
;       files can still be read by read_idlstruct.pro.  27-April-2006 E.S.
;   Reading strings in csv now supported even without the ascii_read() C++
;       code linked to IDL.    2008-03-28 Erin Sheldon, NYU
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


FUNCTION wi_getinfo, struct, $
  ascii=ascii, csv=csv, $
  ieee_big=ieee_big, ieee_little=ieee_little, $
  error=error

  COMMON write_idlstruct_block, $
    ROW_STRING_FORMAT, isbig_endian, version, use_c_write, old_info_struct

  IF keyword_set(csv) THEN BEGIN 
      data_format = 'ASCII_CSV'
  ENDIF ELSE IF keyword_set(ascii) THEN BEGIN 
      data_format = 'ASCII_TAB'
  ENDIF ELSE BEGIN       
      data_format = 'BINARY'
  ENDELSE 

  IF keyword_set(ascii) THEN BEGIN 
      byte_order = 'NA'
  ENDIF ELSE BEGIN 
      IF keyword_set(ieee_big) THEN BEGIN 
          byte_order = 'IEEE_BIG'
      ENDIF ELSE IF keyword_set(ieee_little) THEN BEGIN 
          byte_order = 'IEEE_LITTLE'
      ENDIF ELSE BEGIN 
          ;; Written in native
          IF isbig_endian THEN byte_order = 'IEEE_BIG' $
          ELSE byte_order = 'IEEE_LITTLE'
      ENDELSE 

  ENDELSE 

  ;; For ascii output
  tab = '"'+string(9b)+'"'
  IF keyword_set(csv) THEN BEGIN 
      separator = "','"
  ENDIF ELSE BEGIN 
      separator = tab
  ENDELSE 

  nrows = n_elements(struct)
  tags = strlowcase( tag_names(struct) )
  ntags = n_elements(tags)

  tmp = struct[0]
  ;; This is needed to get the tag descriptions right below
  zero_struct, tmp

  descriptions = strarr(ntags)
  types = strarr(ntags)
  maxlen = replicate(-1, ntags)        ; For strings
  formats = strarr(ntags)

  FOR tag=0L, ntags-1 DO BEGIN 
      tstr = stringrep(tmp.(tag), /struct)
      type = tstr.type_name
      desc = tstr.stringrep

      nel = tstr.n_elements

      IF nel EQ 1 THEN repcnt='' ELSE repcnt=strtrim(nel,2)

      ;; separators and formats for ASCII output
      IF tag NE ntags-1 THEN BEGIN 
          sep = separator
      ENDIF ELSE BEGIN 
          sep = ':,'+separator
      ENDELSE 

      ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
      ;; Get ASCII output formats, and force strings in a given
      ;; field to be fixed-length
      ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

      types[tag] = type

      CASE type OF
          'BYTE': format = repcnt+'(I0,'+sep+')'
          'INT': format = repcnt+'(I0,'+sep+')'
          'LONG': format = repcnt+'(I0,'+sep+')'
          'UINT': format = repcnt+'(I0,'+sep+')'
          'ULONG': format = repcnt+'(I0,'+sep+')'
          'LONG64': format = repcnt+'(I0,'+sep+')'
          'ULONG64': format = repcnt+'(I0,'+sep+')'
          'FLOAT': BEGIN 
              IF desc EQ '0.00000' THEN desc = '0.0'
              format = repcnt+'(g0,'+sep+')'
          END 
          'DOUBLE': BEGIN 
              IF desc EQ '0.0000000D' THEN desc = '0.0D'
              ;; This format is designed to work well with ra/dec
              format = repcnt+'(e15.8,'+sep+')'
          END 
          'STRING': begin 
              ;; strings cannot be variable length, even for binary output
              maxlen[tag] = max( strlen(struct.(tag) ) )

              ;; need to output at least one character
              IF maxlen[tag] EQ 0 THEN maxlen[tag] = 1

              maxlen_str = strtrim(maxlen[tag],2)
              ;;sformat = repcnt+'(A'+maxlen_str+','+sep+')'

              ;; Make all these strings the same length.  Variable length
              ;; not supported in binary, so make default.

              sformat = '(A'+maxlen_str+')'
              newstrings = string(struct.(tag), format=sformat)
              if tstr.n_dimensions eq 0 then begin 
                  struct.(tag) = newstrings
              endif else begin 
                  tdim = size( struct.(tag), /dim)
                  struct.(tag) = reform( newstrings, tdim )
              endelse 
              
              if tstr.n_dimensions eq 0 then begin 
                  desc = 'mkstr('+maxlen_str+')'
              endif else begin 
                  desc = repstr(desc, 'STRARR(', 'mkstr('+maxlen_str+',')
              endelse 

              format=repcnt+'(A,'+sep+')'
          end 
          'COMPLEX': BEGIN 
              print,'Complex type not supported'
              error=-3000
              return,-1
          END 
          ELSE: BEGIN 
              print,'Type not supported: '+type
              error = -4000
          END 
      ENDCASE 

      descriptions[tag] = desc

      ;; only put newline on last one
      IF tag NE ntags-1 THEN BEGIN 
          formats[tag] = '($,'+format+')'
      ENDIF ELSE BEGIN 
          formats[tag] = '('+format+')'
      ENDELSE 


  ENDFOR 

  info_struct = {nrows:nrows, $
                 nrows_total:nrows, $
                 data_format:data_format,$
                 byte_order:byte_order, $
                 idlstruct_version: version, $
                 field_names:tags, $
                 field_types: types, $
                 field_descriptions:descriptions, $
                 field_maxlen: maxlen, $
                 field_output_formats: formats}

  return,info_struct

END 





PRO wi_update_nrows, lun, add_nrows

  COMMON write_idlstruct_block, $
    ROW_STRING_FORMAT, isbig_endian, version, use_c_write, old_info_struct

  point_lun, -lun, original_pos

  point_lun, lun, 0
  line = ''
  readf, lun, line
  tmp = strsplit(line, /extract)
  nrows_old = ulong64(tmp[2])
  nrows = nrows_old + add_nrows

  row_string = strtrim(nrows,2)
  row_string = string(row_string, format=ROW_STRING_FORMAT)

  point_lun, lun, 0
  printf,lun,'NROWS  = '+row_string

  point_lun, lun, original_pos

END





FUNCTION wi_parstring, var

  tstr = size(var, /struct)

  IF tstr.n_dimensions EQ 1 THEN BEGIN 
      
      IF tstr.type_name EQ 'STRING' THEN BEGIN 
          pval = '['+strjoin( "'"+var+"'", ',')+']'
      ENDIF ELSE BEGIN 
          pval = '['+strjoin( strtrim(var,2), ',')+']'
      ENDELSE 
      
  ENDIF ELSE IF tstr.n_dimensions EQ 0 THEN BEGIN 
      
      ;; Scalar
      IF size(var, /tname) EQ 'STRING' THEN BEGIN 
          pval = "'"+var+"'"
      ENDIF ELSE BEGIN 
          pval = strtrim(var, 2)
      ENDELSE 

  ENDIF ELSE BEGIN 
      message,'Multidimensional arrays not supported in header parameters'
  ENDELSE 

  return,pval

END 




PRO wi_header, lun, hstruct, hdrStruct=hdrStruct

  ;; lun must be positioned at the beginning of the file

  COMMON write_idlstruct_block, $
    ROW_STRING_FORMAT, isbig_endian, version, use_c_write, old_info_struct

  row_string = strtrim(hstruct.nrows,2)
  row_string = string(row_string, format=ROW_STRING_FORMAT)
  printf,lun,'NROWS  = '+row_string
  printf,lun,'FORMAT = '+hstruct.data_format
  printf,lun,'BYTE_ORDER = '+hstruct.byte_order
  printf,lun,'IDLSTRUCT_VERSION = '+strtrim(hstruct.idlstruct_version, 2)

  ;; process any input keyword-value pairs
  IF n_elements(hdrStruct) NE 0 THEN BEGIN 

      tags=tag_names(hdrStruct)
      ntags = n_elements(tags)

      required_tags = ['NROWS', 'FORMAT', 'BYTE_ORDER', 'IDLSTRUCT_VERSION','DATA_FORMAT']
      match, tags, required_tags, mt, mr

      IF mt[0] NE -1 THEN BEGIN 
          nmatch=n_elements(mt)
          FOR i=0L, nmatch-1 DO BEGIN 
              print,$
                'Input header keyword "'+tags[mt[i]]+'"'+$
                ' duplicates required field: ignoring'      
          ENDFOR 

      ENDIF 

      w=where(tags NE 'NROWS' AND $
              tags NE 'FORMAT' AND $
              tags NE 'BYTE_ORDER' AND $
              tags NE 'IDLSTRUCT_VERSION', nw)

      IF nw NE 0 THEN BEGIN 
          FOR i=0L, nw-1 DO BEGIN 
              ii = w[i]

              pval = wi_parstring(hdrstruct.(ii))
              printf, lun, tags[ii]+' = '+pval

          ENDFOR 
      ENDIF 

  ENDIF 


  nFields = n_elements(hstruct.field_names)

  FOR i=0L, nFields-1 DO BEGIN 
      printf, lun, hstruct.field_names[i]+'  '+hstruct.field_descriptions[i]
  ENDFOR 

  printf,lun,'END'
  printf,lun

END 





PRO wi_check_string_formats, struct, info_struct, filename, append
  
  COMMON write_idlstruct_block, $
    ROW_STRING_FORMAT, isbig_endian, version, use_c_write, old_info_struct


  append = 1
  IF NOT fexist(filename) THEN BEGIN 
      ;; IF file doesn't exist, don't worry about the request to append
      append = 0 
      return
  ENDIF 

  IF n_elements(info_struct) EQ 0 THEN BEGIN 
      IF n_elements(old_info_struct) NE 0 THEN BEGIN 
          info_struct = old_info_struct
      ENDIF  
  ENDIF 

  ;; Must make sure any string entries are proper length.

  ;; If info_struct not here, and the file exists, just read the
  ;; header and check the lengthts.
  IF n_elements(info_struct) EQ 0 THEN BEGIN 

      hdr = read_idlheader(filename,status=status)
      IF status NE 0 THEN return

      ;; Can only do this with mkstr defined strings
      stringtags = where( strmatch(hdr.field_descriptions, '*mkstr*'), nstring)
      IF nstring NE 0 THEN BEGIN 
          maxlen = lonarr(nstring)

          FOR i=0L, nstring-1 DO BEGIN 
              tag = stringtags[i]
              ts = (strsplit(strtrim(hdr.field_descriptions[tag],2), $
                             'mkstr(', /extract))[0]
              ts = ( strsplit(ts, ')', /extract) )[0]
              lenstr = ( strsplit(ts, ',',/extract) )[0]
              
              maxlen[i] = long(lenstr)
          ENDFOR 
      ENDIF 
  ENDIF ELSE BEGIN 
      ;; input info struct should have maxlen for strings
      stringtags = where(info_struct.field_types EQ 'STRING', nstring)
      IF nstring NE 0 THEN maxlen = info_struct.field_maxlen[stringtags]
  ENDELSE 

  IF nstring NE 0 THEN BEGIN 
      FOR i=0L, nstring-1 DO BEGIN 
          tag = stringtags[i]
          maxlen_str = strtrim(maxlen[i],2)
          sformat = '(A'+maxlen_str+')'
          struct.(tag) = string( struct.(tag), format=sformat)
      ENDFOR 
  ENDIF 

END 


function _expand_tilde_gdl_kludge, fname
	; expand_tilde fails in gdl, do a kludge
	dir=file_dirname(fname)
	newdir = expand_tilde(dir)
	return, path_join(newdir, file_basename(fname))
end



PRO write_idlstruct, struct, filename_in, $
                     ascii=ascii, csv=csv, $
                     ieee_big=ieee_big, ieee_little=ieee_little, $
                     append_in=append_in, $
                     noheader=noheader, no_c_write=no_c_write, $
                     hdrStruct=hdrStruct, $
                     info_struct=info_struct, $
                     status=status, error=error


  status = 1
  IF n_params() LT 2 THEN BEGIN 
    print,'-Syntax: write_idlstruct, struct, filename, $'
    print,'                /ascii, /csv, $'
    print,'                /ieee_big, /ieee_little, $'
    print,'                /append, /noheader, /no_c_write, '
    print,'                hdrStruct=, $'
    print,'                info_struct=, $'
    print,'                status=, error='
    return 
  ENDIF 

  filename = _expand_tilde_gdl_kludge(filename_in)

  COMMON write_idlstruct_block, $
    ROW_STRING_FORMAT, isbig_endian, version, use_c_write, old_info_struct

  version = 1.0

  ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
  ;; If ASCII_WRITE is available, then use it
  ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

  IF n_elements(use_c_write) EQ 0 THEN BEGIN 
      proNames  = routine_info(/system)
      w = where(proNames EQ 'ASCII_WRITE',nw)
      IF nw EQ 0 THEN use_c_write = 0 ELSE use_c_write = 1
  ENDIF 

  ;; Use fixed length for updating later
  IF n_elements(ROW_STRING_FORMAT) EQ 0 THEN BEGIN 
      ROW_STRING_FORMAT = '(a11)'
      isbig_endian = is_ieee_big()
  ENDIF 

  ;; Check the structure
  IF size(struct, /tname) NE 'STRUCT' THEN BEGIN 
      print,'Data must be in structure form'
      error = -10000
      return
  ENDIF 

  ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
  ;; If appending, we need to be careful
  ;; note, we may decide not to append within this procedure if file
  ;; does not already exist. We may have to open the file to check
  ;; the strings, so this should come before hte openw statement
  ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

  IF keyword_set(append_in) THEN BEGIN 
      wi_check_string_formats, $
        struct, info_struct, filename, append
  ENDIF 

  ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
  ;; Open the file
  ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

  IF NOT keyword_set(ascii) AND $
    (keyword_set(ieee_big) OR keyword_set(ieee_little)) THEN BEGIN

      IF keyword_set(ieee_big) AND NOT isbig_endian THEN BEGIN 
          swap_endian=1 
      ENDIF ELSE IF keyword_set(ieee_little) AND isbig_endian THEN BEGIN 
          swap_endian=1
      ENDIF 
  ENDIF 

  openw, lun, filename, /get_lun, error=error, append=append, $
         swap_endian=swap_endian
  IF error NE 0 THEN BEGIN 
      print,'Error opening file '+filename+': '+!error_state.sys_msg
      return
  ENDIF

  ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
  ;; If the info struct not sent, get the info we need
  ;; Otherwise, just update the number of rows.
  ;; Note: if /append is sent, we must use either the input
  ;; info_struct or the old one for updating
  ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
                    

  nrows = n_elements(struct)
  IF n_elements(info_struct) EQ 0 THEN BEGIN 
      info_struct = $
        wi_getinfo(struct, ascii=ascii, csv=csv, $
                   error=error,$
                   ieee_big=ieee_big, ieee_little=ieee_little)
      IF error NE 0 THEN return
      old_info_struct = info_struct
  ENDIF ELSE BEGIN 
      info_struct.nrows = nrows
      info_struct.nrows_total = info_struct.nrows_total + nrows
  ENDELSE 
  

  ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
  ;; Create the header if not appending
  ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

  IF NOT keyword_set(append) AND NOT keyword_set(noheader) THEN BEGIN 
      wi_header, lun, info_struct, hdrStruct=hdrStruct
  ENDIF 


  ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
  ;; Update nrows if appending This is *NOT* a bottleneck
  ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

  IF keyword_set(append) AND NOT keyword_set(noheader) THEN BEGIN 
      wi_update_nrows, lun, info_struct.nrows
  ENDIF 


  ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
  ;; Write the data
  ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

  IF keyword_set(ascii) OR keyword_set(csv) THEN BEGIN 

      ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
      ;; Writing an ascii table
      ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

      ;; The C code is much faster
      IF use_c_write AND NOT keyword_set(no_c_write) THEN BEGIN 

          free_lun,lun

          IF keyword_set(csv) THEN delim=','
          exp_filename = expand_path(filename)

          ascii_write, struct, exp_filename, /append, delim=delim, status=status

      ENDIF ELSE BEGIN 

          ntags = n_elements(info_struct.field_names)
          FOR row=0L, nrows-1 DO BEGIN  
              FOR tag=0L, ntags-1 DO BEGIN 
                  
                  printf,lun,struct[row].(tag),$
                    format=info_struct.field_output_formats[tag]
                  
              ENDFOR 
          ENDFOR 

          free_lun,lun

      ENDELSE 

  ENDIF ELSE BEGIN  

      ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
      ;; Binary table
      ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

      writeu, lun, struct
      free_lun,lun

  ENDELSE 



  status = 0

END 
