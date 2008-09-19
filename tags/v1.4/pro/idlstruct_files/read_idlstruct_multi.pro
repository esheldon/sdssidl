;+
; NAME:
;  READ_IDLSTRUCT_MULTI
;
; PURPOSE:
;  Read binary tables from multiple FITS files using mrdfits.  Return
;  a single structure containing all data.
;
; CALLING SEQUENCE:
;  struct = read_idlstruct_multi(files, $
;                                /diff, $
;                                columns=, $
;                                count=, $
;                                /silent, $
;                                status=)
;
;
; INPUTS:
;  files: A set of fits file names.
;
; OPTIONAL INPUTS:
;  columns: The columns to read.
;
; KEYWORD PARAMETERS:
;  /diff:  This informs that the structures from each file may be different. A
;          slower but safer method is used that defines the output structure
;          based upon the first file and just copies the matching tags
;          afterward.
;  /silent: Be quiet except for errors.
;
; OUTPUTS:
;  Combined structure.
;
; OPTIONAL OUTPUTS:
;  count: the total count of rows read.
;
; MODIFICATION HISTORY:
;  Author: Erin Sheldon, NYU
;  Documented: 2006-July-21.  E.S.
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


FUNCTION read_idlstruct_multi, infiles, $
                               hdr=hdr, $
                               count=count, $
                               diff=diff, $
                               columns=columns, $
                               silent=silent, $
                               status=status

  status = 1
  IF n_params() LT 1 THEN BEGIN 
      print,'-Syntax: st = read_idlstruct_multi(files, hdr=, count=count, columns=, /silent, /diff)'
      print
      print,'Must all be the same structure unless /diff is set. If /diff then'
      print,' the structure is taken from the *first* file'
      return, -1
  ENDIF 

  delvarx,struct

  ;; See which files actually exist
  nfiles = n_elements(infiles)
  FOR i=0L, nfiles-1 DO BEGIN
      IF fexist(infiles[i]) THEN BEGIN 
          add_arrval, infiles[i], files
      ENDIF ELSE BEGIN 
          message,'File '+infiles[i]+' does not exist. Skipping',/inf
          return,-1
      ENDELSE 
  ENDFOR 

  nfiles = n_elements(files)
  IF nfiles EQ 0 THEN BEGIN  
      print,'None of the files were found'
      return,-1
  ENDIF 

  ;; just one file?
  IF nfiles EQ 1 THEN BEGIN 
      struct = read_idlstruct(infiles, columns=columns, error=error, $
                              silent=silent, status=status, hdr=hdr)
      IF error NE 0 THEN return,-1
      count = n_elements(struct)
      return,struct
  ENDIF 
  

  ;; first go through, read the headers and find how many objects
  ;; are in each.  Then allocate a big struct and copy it in.

  IF NOT keyword_set(silent) THEN BEGIN
      print
      print,'Reading headers'
  ENDIF 
  ntotal = 0LL
  numlist = lonarr(nfiles)
  FOR i=0L, nfiles-1 DO BEGIN 

      hdr = read_idlheader(files[i])
      numlist[i] = hdr.nrows
      ntotal = ntotal + numlist[i]

  ENDFOR 

  IF NOT keyword_set(silent) AND nfiles GT 1 THEN BEGIN
      print
      print,'Total number of rows: '+ntostr(ntotal)
  ENDIF 

  IF NOT keyword_set(diff) THEN BEGIN 

      ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
      ;; No indication structs are different
      ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

      beg =0L
      FOR i=0L, nfiles-1 DO BEGIN 
          
          IF numlist[i] NE 0 THEN BEGIN 
              IF NOT keyword_set(silent) THEN $
                print,'Reading File: '+ntostr(files[i])
              t=read_idlstruct(files[i], silent=silent, columns=columns, status=status)
              
              IF status NE 0 THEN BEGIN 
                  message,'Error reading file: ',files[i],/inf
                  message,'No data returned',/inf
                  return,-1
              ENDIF 
              IF i EQ 0 THEN BEGIN 
                  struct=replicate(t[0], ntotal)
                  struct[beg:beg+numlist[i]-1] = t
                  beg = beg+numlist[i]
              ENDIF ELSE BEGIN 
                  struct[beg:beg+numlist[i]-1] = t
                  beg = beg+numlist[i]
              ENDELSE 
              
              t = 0
              
          ENDIF ELSE BEGIN  
              print,'File is empty: '+ntostr(files[i])
          ENDELSE 
      ENDFOR 
  ENDIF ELSE BEGIN 

      ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
      ;; User has indicated that the structures may be 
      ;; different. Use slower but safer method.
      ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

      beg =0L
      FOR i=0L, nfiles-1 DO BEGIN 
          
          IF numlist[i] NE 0 THEN BEGIN 
              IF NOT keyword_set(silent) THEN $
                print,'Reading File: '+ntostr(files[i])
              t=read_idlstruct(files[i], silent=silent, columns=columns, status=status)
              
              IF status NE 0 THEN BEGIN 
                  message,'Error reading file: ',files[i],/inf
                  message,'No data returned',/inf
                  return,-1
              ENDIF 

              IF i EQ 0 THEN BEGIN 
                  struct_tags = tag_names(t[0])
                  struct_ind  = lindgen(n_elements(struct_tags))

                  ;; "zero" the struct for those tags that
                  ;; will not be copied.
                  struct = t[0]
                  zero_struct,struct

                  ;; simple copy on first one
                  struct=replicate(struct, ntotal)
                  struct[beg:beg+numlist[i]-1] = t
                  beg = beg+numlist[i]
              ENDIF ELSE BEGIN 

                  ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
                  ;; must match up the tags for possibly different struct
                  ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

                  newtags = tag_names(t[0])
                  match, struct_tags, newtags, mstr, mnew

                  IF mstr[0] NE -1 THEN BEGIN 
                      nmatch = n_elements(mnew)
                      ;; loop over tags
                      FOR j=0L, nmatch-1 DO BEGIN 
                          si = mstr[j]
                          sn = mnew[j]
                          struct[beg:beg+numlist[i]-1].(si) = t.(sn)
                      ENDFOR 
                  ENDIF ELSE BEGIN 
                      message,'No compatible tags from file: '+files[i],/inf
                      message,'Structure will be zero for rows ['+ntostr(beg)+', '+ntostr(numlist[i]-1)+']'
                  ENDELSE 

                  beg = beg+numlist[i]
              ENDELSE 
              
              t = 0
              
          ENDIF ELSE BEGIN  
              print,'File is empty: '+ntostr(files[i])
          ENDELSE 
      ENDFOR 
  ENDELSE 

  count = ntotal
  status = 0
  return,struct
END 
