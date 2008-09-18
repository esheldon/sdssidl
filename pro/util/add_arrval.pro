
;+
;
; NAME:
;    ADD_ARRVAL
;       
; PURPOSE:
;    Add an element to an array. If input array is undefined, 
;    set it equal to the new value.
;
; CALLING SEQUENCE:
;    add_arrval, newval, array, front=front
;
; INPUTS: 
;    newval: the new value to be added
;
; KEYWORD PARAMETERS:
;    /front: If set then put the new value at the front of the array, else
;       put at end.
;
; SIDE EFFECTS: 
;    array is created or augmented.       
;
; REVISION HISTORY:
;    14-NOV-2000 Erin Scott Sheldon UofMich
;       
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

pro add_arrval, newval, array, front=front

  on_error,2

  if n_params() eq 0 then begin 
      print,'Syntax: add_arrval, newval, array [, front=front]'
      print,''
      print,'/front to add new element at front of array'
      print,'Use doc_library,"add_arrval"  for more help.'  
      return
  endif 

  if n_elements(newval) eq 0 then begin
      print,'Must enter a value to add to array'
      print,'Syntax: add_arrval, newval, array [, front=front]'
      return
  endif 

  if n_elements(array) eq 0 then array = newval else begin
      if not keyword_set(front) then array=[temporary(array), newval] $
      else array=[newval, temporary(array)]
  endelse 

  return

end
