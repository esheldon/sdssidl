;+
; NAME:
;  htm_readbin
;
; PURPOSE:
;  Read a binary output file created by htm_match into a structure.
;
; CALLING SEQUENCE:
;  struct = htm_readbin(file, status=)
;
; INPUTS:
;  file: The path to a binary file.
;
; OPTIONAL OUTPUTS:
;  status: The status: 0 is success.
;
; MODIFICATION HISTORY:
;  Created: 2006-08-15 Erin Sheldon, NYU
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

FUNCTION htm_readbin, file, status=status

  status = 1
  IF n_elements(file) EQ 0 THEN BEGIN 
      print,'-Syntax: struct = htm_readbin(file, status=)'
      print
      message,'Halting'
  ENDIF 

  print,'Reading file: ',file
  openr, lun, file[0], /get_lun, error=error
  IF ERROR NE 0 THEN BEGIN 
      print,!ERR_STRING
      free_lun, lun
      return, -1
  ENDIF 

  ;; Number of matches
  nrows = 0LL
  readu, lun, nrows

  ;; We have status ne 0 for nrows=0
  IF nrows EQ 0 THEN BEGIN 
      print,'File has no rows'
      return,-1
  ENDIF 

  ;; variable type of indices
  ind_type = 0L
  readu, lun, ind_type

  ;; variable type of distance
  dis_type = 0L
  readu, lun, dis_type

  ;; Create variables
  CASE ind_type OF
      2: ind_var = 0
      3: ind_var = 0L
      12: ind_var = 0U
      13: ind_var = 0UL
      14: ind_var = 0LL
      15: ind_var = 0ULL
      ELSE: message,'Unsupported index type: ',ind_type
  ENDCASE 

  CASE dis_type OF
      4: dis_var = 0.0
      5: dis_var = 0d
      ELSE: message,'Unsupported distance type: ',ind_type
  ENDCASE 
  

  struct = { $
             ind1: ind_var, $
             ind2: ind_var, $
             d12:  dis_var  $
           }
  struct = replicate(struct, nrows)

  readu, lun, struct

  free_lun, lun

  status = 0
  return, struct

END 
