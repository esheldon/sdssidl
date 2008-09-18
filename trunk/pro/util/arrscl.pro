function arrscl, arr, min, max, arrmin=arrmin, arrmax=arrmax

;+
;
; NAME:
;    ARRSCL
;       
; PURPOSE:
;    Rescale the range of an array.
;
; CALLING SEQUENCE:
;    result = arrscl(arr, min, max, arrmin=, arrmax=)
;                 
; INPUTS: 
;    arr:  The array.
;    min:  new minimum.
;    max:  new maximum.
;
; OPTIONAL INPUTS:
;    arrmin:  A number to be used as minimum of array range.
;    arrmax:  A number to be used as maximum of array range.
;
;    NOTE:  These are useful if the original array is known to only be a
;           sample of possible range.  e.g. if 
;                arr=randomu(seed,20) 
;           then one might wish to give arrmin=0, arrmax=1
;       
; OUTPUTS:
;    The rescaled array. 
;	
; REVISION HISTORY:
;    Author: Erin Scott Sheldon UofMich 10/18/99                                        
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


  if n_params() ne 3 then begin
      print,'-Syntax: result = arrscl(arr, min, max, arrmin=, arrmax=)'
      print
      print,'Use doc_library,"arrscl"  for more help.' 
      return,0
  endif 

  if n_elements(arrmax) eq 0 then arrmax = max(arr)
  if n_elements(arrmin) eq 0 then arrmin = min(arr)
  a = (max - min)/(arrmax - arrmin)
  b = (arrmax*min - arrmin*max)/(arrmax - arrmin)

  return, a*arr + b

end 
