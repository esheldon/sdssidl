
;+
;
; NAME: 
;    ntostr
;       
; PURPOSE: 
;    Convert a number to a string.  Cuts off white spaces.
;	
; CALLING SEQUENCE: 
;    result = ntostr(num, format=)
;
; INPUTS: 
;    num:  the object to be converted. May be an array.
;
; OPTIONAL INPUTS:
;   format= The format to send to the string() function.  This is the
;      most natural way to format the output.
;
; EXAMPLES:
;  IDL> print,ntostr(3.528162895)
;  3.52816
;  IDL> print,ntostr(3.528162895,format='(F10.2)')
;  3.53
;  IDL> print,ntostr([3.528162895,421.3812166],format='(F10.2)')
;  3.53 421.38
;  IDL> print,ntostr([32,157231],format='(I20.5)')
;  00032 157231
;
; REVISION HISTORY:
;	Author: Erin Scott Sheldon  UofM 6/1/99
;       Added format keyword.  Some time in 2002, ESS, UChicago                                      
;-                                       
;
;
;
;  Copyright (C) 2006  Erin Sheldon, NYU.  erin dot sheldon at gmail dot com
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

function ntostr, num, p1, p2, format=format, round=round


  if n_elements(num) eq 0 then begin 
      on_error, 2
      print,'-Syntax: string = ntostr(num, format=)'
      message,'Halting'
  endif 

  ;; type checking: 8=struct 10=pointer 11=object
  type = size(num, /type)
  if (type eq 8) or (type eq 10) or (type eq 11) then begin
      message,'Input must be a number or string (scalar or array)',/inf
      return,''
  endif 

  ;; remove leading and trailing blanks
  tmp = strtrim(string(num, format=format), 2)

  return, tmp

END
