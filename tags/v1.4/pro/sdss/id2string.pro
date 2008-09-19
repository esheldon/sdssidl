
;+
;
; NAME: 
;    ID2STRING
;       
; PURPOSE: 
;    Function outputs a string containing id of a photo object
;    in the correct format: length 5 with zero padding.
;	
;
; CALLING SEQUENCE: 
;    result = id2string(id)
;      
; INPUTS: 
;    id: may be an array.
;	
; REVISION HISTORY:
;     Author: Erin Scott Sheldon  UChicago 25-Feb-2004
;     Now just use a format string.  2006-10-07, Erin Sheldon, NYU
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



function id2string, id

  if N_params() eq 0 then begin
      on_error,2
      print,'-Syntax: result = id2string(id)'
      print,' Use doc_library,"id2string"  for more help.'  
      message,'Halting'
  endif

  return, strtrim(string(id,format='(I20.5)'),2)

end 
