FUNCTION field2string, field


;+
;
; NAME: 
;    FIELD2STRING
;       
; PURPOSE: 
;    Function outputs a string containing field of a photo tsObj 
;    file in the correct format: width of 4 with zero padding.
;	
;
; CALLING SEQUENCE: 
;    result = field2string(field)
;      
; INPUTS: 
;    field number (May be an array)
;	
; REVISION HISTORY:
;     Author: Erin Scott Sheldon  Umich 5/25/99
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



  if N_params() eq 0 then begin
      on_error, 2
      print,'-Syntax: result = field2string(field)'
      print,' Use doc_library,"field2string"  for more help.'  
      message,'Halting'
  endif

  return, strtrim(string(field,format='(I20.4)'),2)

end






