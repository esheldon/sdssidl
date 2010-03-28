;+
; NAME:
;  READ_IDLHEADER
;
;
; PURPOSE:
;  Read the header from a ".st" (idl structure) file. The result is a
;  structure containing the NROWS, FORMAT keywords and the field names, types
;  and descriptions, as well as any other keyword-value pairs the user has
;  added.  Strings are returned as strings, all others are returned as double
;  precision floating point.  Arrays are allows for numbers.
;
;
; CATEGORY:
;  File I/O
;
;
; CALLING SEQUENCE:
;  hstruct = read_idlheader(filename/unit)'
;
;
; INPUTS:
;  filename or unit: Name of the file to be read, or the logical unit of an
;                    open file.
;
; OUTPUTS:
;  A structure containing the NROWS, FORMAT keywords and the field names, types
;  and descriptions.  Possible also user added keyword-value pairs (which may
;  be arrays for numbers).
;
; EXAMPLE:
;  file = 'test.st'
;  hstruct = read_idlheader(file)
;
;
; MODIFICATION HISTORY:
;  Created  02-Jul-2004 Erin Sheldon, UofChicago
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

function _expand_tilde_gdl_kludge, fname

	; expand_tilde fails in gdl, do a kludge
	dir=file_dirname(fname)
	newdir = expand_tilde(dir)
	return, path_join(newdir, file_basename(fname))
end

FUNCTION rih_openfile, filename, error=error, tname=tname

  tname = size(filename,/tname)

  IF (tname EQ 'INT' OR tname EQ 'UINT' OR $
      tname EQ 'LONG' OR tname EQ 'ULONG' OR $
      tname EQ 'LONG64' OR tname EQ 'ULONG64' OR $
      tname EQ 'BYTE') THEN BEGIN 

      lun = filename
      error = 0
      return, lun

  ENDIF ELSE IF tname EQ 'STRING' THEN BEGIN 

	  fname = _expand_tilde_gdl_kludge(filename)
      openr, lun, fname, /get_lun, error=error

      IF error NE 0 THEN BEGIN 
          print,'Error opening file '+filename+': '+!error_state.sys_msg
          return,-1
      ENDIF ELSE BEGIN 
          return,lun
      ENDELSE 

  ENDIF ELSE BEGIN
 
      print,'Datatype of filename/unit '+tname+' is incorrect'
      error = -2000
      return,-1

  ENDELSE 

END 

FUNCTION rih_input_format, var

    tstr = size(var, /struct)
    nel = tstr.n_elements
    if nel eq 1 then repcnt = '' else repcnt = strtrim(nel,2)

    ;; for mrd_struct we need more description
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

    return,format

END 

FUNCTION rih_getvalue, description, format=format, error=error

  error = 1

  ;; Turn the value into an exectute string
  ;;print,description
  command = 'tmp = '+description
  
  ;; just execute the string
  IF execute(command) THEN BEGIN 
      error = 0
      IF arg_present(format) THEN BEGIN 
          format = rih_input_format(tmp)
      ENDIF 
      return,tmp
  ENDIF ELSE BEGIN 
      return,-1
  ENDELSE 

END 

FUNCTION rih_getvalue_old, line, tsplit, error=error

  error = 1

  ;; look for a string
  IF strmid(tsplit[2], 0, 1) EQ '"' THEN BEGIN 
      first = strpos(line, '"')
      IF first EQ -1 THEN BEGIN 
          print,'Bad string format for header keyword ',tsplit[0]
          return, -1
      ENDIF 
      second = strpos(line, '"', first+1)
      IF second EQ -1 THEN BEGIN 
          print,'Bad string format for header keyword ',tsplit[0]
          return, -1
      ENDIF 
      
      value = strmid(line, first+1, second-(first+1))
      
  ENDIF ELSE BEGIN 
      
      ;; How many numbers are there?
      nval = n_elements(tsplit)-2
      IF nval EQ 1 THEN BEGIN 
          IF valid_num(tsplit[2], val) THEN BEGIN 
              value = val
          ENDIF ELSE BEGIN 
              print,'Invalid number in keyword "'+tsplit[0]+'": ignoring'
              delvarx, value
              return,-1
          ENDELSE 
      ENDIF ELSE BEGIN 
          value = dblarr(nval)
          
          FOR i=0L, nval-1 DO BEGIN 
              IF valid_num(tsplit[i+2], val) THEN BEGIN 
                  value[i] = val
              ENDIF ELSE BEGIN 
                  print,'Invalid number in keyword "'+tsplit[0]+'": ignoring'
                  delvarx, value
                  return,-1
              ENDELSE 
          ENDFOR 
      ENDELSE 
      
  ENDELSE 
  error = 0
  return,value


END 

FUNCTION read_idlheader, filename, status=status
  
  status = 1
  IF n_params() LT 1 THEN BEGIN 
      print,'-Syntax: hdrStruct = read_idlheader(filename/unit, status=)'
      return,-1
  ENDIF 

  ;; Open the file or just set lun if filename is file unit
  lun = rih_openfile(filename, error=error, tname=tname)

  IF error NE 0 THEN return, -1

  hdrStruct = {nrows:0ULL, $
               data_format:'', $
               byte_order:'', $
               idlstruct_version:0.0}

  line = ''
  readf, lun, line
  line=strtrim(line,2)
  WHILE line NE 'END' DO BEGIN 

      ;; remove comments
      ;; Only look for stuff before any comment mark #
      cp = strpos(line,'#')
      IF cp NE -1 THEN BEGIN 
          line = strmid(line,0,cp)
      ENDIF 

      tmp = strsplit(line, /extract)
      nel = n_elements(tmp)

      IF nel EQ 2 THEN BEGIN 
          ;; This is a field description in version >= 0.9
          IF hdrstruct.idlstruct_version GE 0.9 THEN BEGIN 

              add_arrval, tmp[0], tag_names
              add_arrval, tmp[1], descriptions
              
              ;; Get the format
              var = rih_getvalue(tmp[1], format=format, error=error)
              type = size(var, /tname)
              add_arrval, format, formats
              add_arrval, type, types
              add_arrval, n_elements(var), lengths

          ENDIF ELSE BEGIN 
              print,'Bad header format ',line
          ENDELSE 

      ENDIF ELSE IF nel GE 3 THEN BEGIN 

          ;; Is this a keyword/value pair?
          IF tmp[1] EQ "=" THEN BEGIN 

              hname = tmp[0]
              CASE hname OF
                  'NROWS': BEGIN 
                      hdrStruct.nrows = ulong64(tmp[2])
                  END 
                  'FORMAT': BEGIN 
                      hdrStruct.data_format = tmp[2]
                  END
                  'BYTE_ORDER': BEGIN 
                      hdrStruct.byte_order = tmp[2]
                  END 
                  'IDLSTRUCT_VERSION': BEGIN 
                      hdrStruct.idlstruct_version = float( tmp[2] )
                  END 
                  ELSE: BEGIN 

                      IF hdrstruct.idlstruct_version GE 0.9 THEN BEGIN 
                          num = n_elements(tmp)
                          sendtmp = strjoin( tmp[2:num-1], " ")
                          hvalue = $
                            rih_getvalue(sendtmp, error=rerr)
                          
                      ENDIF ELSE BEGIN 
                          hvalue = $
                            rih_getvalue_old(line, tmp, error=rerr)
                      ENDELSE 

                      IF rerr EQ 0 THEN BEGIN 
                          hdrStruct = create_struct(hdrStruct, hname, hvalue)
                      ENDIF 
                  END 
              ENDCASE 
              
          ENDIF ELSE BEGIN 

              ;; Field description in old format

              IF tmp[1] EQ 'STRING' THEN tmp[2]="'"+tmp[2]+"'"
              add_arrval, tmp[0], tag_names
              add_arrval, tmp[1], types
              add_arrval, tmp[2], descriptions

              IF nel GT 3 THEN BEGIN 
                  add_arrval, tmp[3], formats
                  add_arrval, tmp[4], lengths
              ENDIF ELSE BEGIN 
                  add_arrval, '', formats
                  add_arrval, '', lengths
              ENDELSE 

          ENDELSE 
      ENDIF ELSE IF nel EQ 2 THEN BEGIN 
          

          
      ENDIF 
      readf, lun, line
      line=strtrim(line,2)
  ENDWHILE 

  ;; read the next line which should be empty
  readf, lun, line
  line=strtrim(line)
  IF line NE '' THEN message,'Bad header'

  IF tname EQ 'STRING' THEN free_lun, lun

  thdrStruct = {$
                 field_names:tag_names, $
                 field_types:types,$
                 field_descriptions:descriptions, $
                 field_input_formats:formats, $
                 field_lengths:lengths $
               }

  hdrStruct = create_struct(hdrStruct, thdrStruct)

  status = 0
  return,hdrStruct

END 
