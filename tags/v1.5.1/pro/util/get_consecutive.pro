;+
; NAME:
;  get_consecutive
;
; PURPOSE:
;  Return indices for first/last pairs of consecutive integers.
;  The same index is returned for first/last if integer is isolated.
;
; CALLING SEQUENCE:
;  get_consecutive, numbers, first, last
;
; INPUTS:
;  numbers: A sorted array of numbers. 
;
; OUTPUTS:
;  first,last: Arrays containing indices to first/last of integers.
;
; MODIFICATION HISTORY:
;  Created 2005, Erin Sheldon, UChicago
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

pro get_consecutive, numbers, first, last

  if n_params() lt 3 then begin 
      print,'-Syntax: get_consecutive, numbers, first, last'
      return
  endif 

  n=n_elements(numbers)
  first = replicate(-1l, n)
  last = first

  j=0ull
  first[0] = 0
  last[0] = 0
  for i=1l, n-1 do begin 

      if numbers[i] eq (numbers[i-1]+1) then begin 
          last[j] = i
      endif else begin 
          j = j+1
          first[j] = i
          last[j] = i
      endelse 

  endfor 

  w=where(first ne -1)
  first = first[w]
  last = last[w]

end 
