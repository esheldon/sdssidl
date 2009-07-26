;+
; NAME:
;  ARRAY_UNION()
;
; PURPOSE:
;  Return the union of elements in two arrays.  Duplicates are removed.
;  The type of the returned array is that of array1.
;
; CALLING SEQUENCE:
;  union = array_union(array1, array2 [, nkeep])
;
; INPUTS:
;  array1, array2
;
; OUTPUTS:
;  union: All unique values from both arrays.
;  nkeep: number kept.
;
; MODIFICATION HISTORY:
;  30-Sept-2005, Created, Erin Sheldon, NYU
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

function array_union, array1, array2, nkeep

  nkeep = 0
  if n_params() lt 2 then begin 
      print,'-Syntax: union = array_union(array1, array2 [, nkeep])'
      return,-1
  endif 

 
  if array1[0] eq -1 and array2[0] eq -1 then return,-1

  n1 = n_elements(array1) & n2 = n_elements(array2)
  indnew = replicate(array1[0], n1+n2)
  indnew[0:n1-1] = array1
  indnew[n1:n1+n2-1] = array2

  rmd = rem_dup(indnew)
  indnew = indnew[rmd]

  return,indnew

end 
