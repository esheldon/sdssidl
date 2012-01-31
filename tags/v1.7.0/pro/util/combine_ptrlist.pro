;+
; NAME:
;  COMBINE_PTRLIST
;
;
; PURPOSE:
;  Combine a ptrarr, each containg pointers to list of the same types of 
;  objects, into a normal idl variable array. The objects must be able
;  to be replicated with the REPLICATE() function.  The pointers are freed
;  unless the /nofree keyword is sent.
;
;
; CATEGORY:
;  Utility.
;
;
; CALLING SEQUENCE:
;  output = combine_ptrlist(ptrlist, /nofree, status=)
;
;
; INPUTS:
;  ptrlist: An array or scalar of pointers.
;
; KEYWORD PARAMETERS
;  /nofree: If set, the data is copied rather than freed.
;
; OUTPUTS:
;  The data from the ptrlist combined into a single IDL variable.
;
;
; OPTIONAL OUTPUTS:
;  status=: 0 success, otherwise failure.
;
; SIDE EFFECTS:
;  The pointers are freed.
;
; EXAMPLE:
;  ptr[0] = ptr_new(struct1, /no_copy)
;  ...
;  ptr[n] = ptr_new(structn, /no_copy)
;  data = combine_ptrlist(ptr)
;
;
; MODIFICATION HISTORY:
;  Doc created 1-August-2005: Erin Sheldon, UChicago
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

function combine_ptrlist, ptrlist, nofree=nofree, status=status

  status = 1
  if n_params() lt 1 then begin 
      print,'-Syntax:  output = combine_ptrlist(ptrlist, /nofree, status=)'
      print,'All elements in the lists must be of the same type'
      return,-1
  endif 

  ;; number in list
  nlist = n_elements(ptrlist)

  ;; count the individual elements
  numlist = ulon64arr(nlist)
  ntot = 0ull
  for i=0ull, nlist-1 do begin 
      if ptr_valid(ptrlist[i]) then begin 

          if n_elements(one) eq 0 then one=(*ptrlist[i])[0]

          numlist[i] = n_elements( *ptrlist[i] )
          ntot = ntot + numlist[i]

      endif 
  endfor 

  if n_elements(one) eq 0 then return,-1

  ;; prepare the output
  output = replicate(one, ntot)

  ;; Copy from the pointers into the output
  beg=0ULL
  for i=0ull, nlist-1 do begin 
      if numlist[i] ne 0 then begin 
          output[beg:beg+numlist[i]-1] = *ptrlist[i]
		  if not keyword_set(nofree) then begin
			  ptr_free, ptrlist[i]
		  endif
          beg=beg+numlist[i]
      endif 
  endfor 

  status = 0
  return,output

end 
