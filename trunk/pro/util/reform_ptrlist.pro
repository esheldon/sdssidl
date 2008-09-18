;+
; NAME:
;  REFORM_PTRLIST()
;
;
; PURPOSE:
;  Take an array of pointers, each of which may point to arrays themselves, 
;  and return an array of pointers in which the pointers each point to 
;  individual objects only.  The original pointers are freed unless the
;  /nofree keyword is set.
;
; CATEGORY:
;  Utility
;
;
; CALLING SEQUENCE:
;   output = reform_ptrlist(ptrlist, /nofree)
;
;
; INPUTS:
;  ptrlist: Array of pointers.
;
; KEYWORD PARAMETERS:
;  /nofree: The original pointers are freed unless this keyword is sent.
;
; OUTPUTS:
;  A new pointer array as described in the purpose.
;
;
; OPTIONAL OUTPUTS:
;  status=: 0 for success
;
; MODIFICATION HISTORY:
;   1-August-2005  Erin Sheldon, UChicago
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


function reform_ptrlist, ptrlist, status=status, nofree=nofree

  if n_params() lt 1 then begin 
      print,'-Syntax:  output = reform_ptrlist(ptrlist, /nofree)'
      return,-1
  endif 

  ;; Number in list
  nlist = n_elements(ptrlist)

  ;; Count the individual elements
  numlist = ulon64arr(nlist)
  ntot = 0ULL
  for i=0ULL, nlist-1 do begin 
      if ptr_valid(ptrlist[i]) then begin 

          if n_elements(one) eq 0 then one=(*ptrlist[i])[0]

          numlist[i] = n_elements( *ptrlist[i] )
          ntot = ntot + numlist[i]

      endif 
  endfor 

  output = ptrarr(ntot)
  beg=0ULL
  for i=0ULL,nlist-1 do begin 
      if numlist[i] ne 0 then begin 
          tmpstruct = temporary(*ptrlist[i])
          for j=0l, numlist[i]-1 do begin 
              output[beg+j] = ptr_new(tmpstruct[j])
          endfor 
          tmpstruct = 0
          if not keyword_set(nofree) then ptr_free, ptrlist[i]
          beg = beg+numlist[i]
      endif
  endfor 

  return,output
end 
