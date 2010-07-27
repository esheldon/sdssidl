PRO read_tsobj_make_struct, indices, bigstruct, str

;+
;
; NAME: 
;    READ_TSOBJ_MAKE_STRUCT
;
; PURPOSE:
;   Creates a structure with the user's input parameters.
;
; CALLING SEQUENCE:
;    read_tsobj_make_struct, indices, bigstruct, substruct
;
; INPUTS: 
;   indices: the index for each tag in bigstruct 
;   bigstruct: the structure from which the new struct
;      is to be created.
;
; Outputs: substruct: A structure with requested tags in it
;
;
; Author:  Erin Scott Sheldon
; Date: 10/7/98
; Modified:  01/12/99  Comment: Made run,rerun,camcol,field defaults
; 26-Aug-2002: Minimal tags added in read_tsobj_make_tags now.  This
;              just copies them.  Tags now gotten from bigstruct
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


  IF n_params() LT 2 THEN BEGIN 
      print, '-syntax: make_struct, indices, bigstruct, substruct'
      return
  ENDIF 

  delvarx, str
  tags = (tag_names(bigstruct))[indices]

  FOR i=0L, n_elements(tags)-1 DO BEGIN 
      IF n_elements(str) EQ 0 THEN BEGIN 
          str = create_struct( tags[i], bigstruct[0].(indices[i]) )
      ENDIF ELSE BEGIN 
          str = create_struct( str, tags[i], bigstruct[0].(indices[i]) )
      ENDELSE 
  ENDFOR 

  return 
END 


