;+
; NAME:
;  ARRAY_INTERSECT()
;
; PURPOSE:
;  Pick the common elements from two arrays. Duplicates are removed.  The
;  type of the returned array is that of array1.
;
; CALLING SEQUENCE:
;  intersection = array_intersect(array1, array2 [, nkeep])
;
; INPUTS:
;  array1, array2
;
; OUTPUTS:
;  intersection: The elements common to both arrays, with the type of array1.
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


function array_intersect, array1, array2, nkeep

  nkeep = 0

  if n_params() lt 2 then begin 
      print,'-Syntax: intersect = array_intersect(array1, array2 [, nkeep])'
      return,-1
  endif 

  ;; must remove duplicates for the match program

  n1 = n_elements(array1)
  n2 = n_elements(array2)
  rmd1 = rem_dup(array1) & nrmd1 = n_elements(rmd1)
  rmd2 = rem_dup(array2) & nrmd2 = n_elements(rmd2)


  ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
  ;; do some work to avoid excessive memory use
  ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

  ;; worst memory case
  if (nrmd1 ne n1) and (nrmd2 ne n2) then begin 
      match, array1[rmd1], array2[rmd2], m1, m2, /sort
      if m1[0] eq -1 then return,-1

      nkeep = n_elements(m1)
      return,array1[rmd1[m1]]
  endif 

  ;; these both pretty bad
  if (nrmd1 ne n1) and (nrmd2 eq n2) then begin 
      rmd2 = 0
      match, array1[rmd1], array2, m1, m2, /sort
      if m1[0] eq -1 then return,-1

      nkeep = n_elements(m1)
      return,array1[rmd1[m1]]
  endif 
  if (nrmd1 eq n1) and (nrmd2 ne n2) then begin 
      rmd1 = 0
      match, array1, array2[rmd2], m1, m2, /sort
      if m1[0] eq -1 then return,-1

      nkeep = n_elements(m1)
      return,array1[m1]
  endif 
  
  ;; both are unique: minimal memory usage
  rmd1 = 0 & rmd2 = 0

  match, array1, array2, m1, m2, /sort

  if m1[0] eq -1 then return,-1

  nkeep = n_elements(m1)
  return,array1[m1]

end 
