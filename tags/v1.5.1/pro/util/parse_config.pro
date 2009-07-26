;+
; NAME:
;  PARSE_CONFIG
;
;
; PURPOSE:
;  Parse simple 2 column "option value" config file. Each is read in as a 
;    string. Everything to the right of a comment character # is ignored,
;    as are empty lines. The outputs are string arrays for the keywords
;    and values.  A optional output 'struct' is a structure with each keyword
;    value pair as tag and value in the structure.
;
; CALLING SEQUENCE:
;  parse_config, config_file, keywords, values
;
; INPUTS:
;  config_file: full path to file
;
; OUTPUTS:
;  keywords: the names of the config variables
;  values: values of config variables
;
; OPTIONAL OUTPUTS:
;  status=: 0 if success
;
; MODIFICATION HISTORY:
;  ??-??-2002  Erin Sheldon UofMichigan
;  02-Apr-2004: Allow comments, empty lines
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

pro parse_config, config_file, keywords, values, struct=struct, status=status

    status = 1
    if n_params() lt 1 then begin 
        print,'-Syntax: parse_config, config_file, keywords, values, struct=, status='
        return
    endif 

    on_error, 2

    delvarx, keywords, values

    openr, lun, config_file, /get_lun, error=error

    if error ne 0 then begin 
        print,'Error opening file: ',config_file
        return
    endif 

    nlines=numlines(config_file)

    line=' '
    delvarx, keywords
    delvarx, values
    for i=0l, nlines-1 do begin 
        readf, lun, line

        ;; First, only look for stuff before any comment mark #
        cp = strpos(line,'#')
        if cp ne -1 then begin 
            line = strmid(line,0,cp)
        endif 

        ;; Remove leading and trailing white space
        line=strtrim(line)

        if (line ne '') then begin 

            ;; strsplit defaults to splitting on white space
            tmp = strsplit(line, /extract)

            ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
            ;; After leading/trailing whitespace and comments
            ;; are removed, should only result in a key-value pair
            ;; else ignore
            ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

            if n_elements(tmp) eq 2 then begin 
                add_arrval, tmp[0], keywords
                add_arrval, tmp[1], values
            endif 

        endif 
    endfor 

    free_lun, lun

    if arg_present(struct) then begin
        uk = keywords[ rem_dup(keywords) ]
        if n_elements(uk) ne n_elements(keywords) then begin
            message,'There are duplicate keywords.  Cannot create config struct',/inf
        endif else begin
    
            ; Create the config structure

            nkey = n_elements(keywords)
            if nkey eq 0 then return
  
            for i=0l, nkey-1 do begin 
       
                if i eq 0 then begin 
                    struct = create_struct(keywords[i], values[i])
                endif else begin 
                    struct = create_struct(struct, keywords[i], values[i])
                endelse 
      
            endfor 
        endelse

    endif


    status = 0
    return

end 
