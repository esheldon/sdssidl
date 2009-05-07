;+
;
; NAME:
;    match_dup
;       
; PURPOSE:
;    Find the matching elements of two arrays of integers (arr1 and arr2), 
;    each of which may have duplicates.  Unlike the match.pro program, 
;    match arrays (match1 and match2) may not correspond directly 
;    due to the possibility of duplicates.  However, one can use the 
;    reverse_indices arrays to find that correspondence (if /SORT is not set)
;
; CALLING SEQUENCE:
;    match_dup, arr1, arr2, match1, match2, $
;               numlist1=, h1=, reverse_indices1=, $
;               numlist2=, h2=, reverse_indices2=, $
;               /SORT
;
; INPUTS: 
;    arr1: integer array which may contain duplicates.
;    arr2: integer array which may contain duplicates.
;
;
; KEYWORD PARAMETERS:
;    /sort:  use a sorting algorithm to find matches instead of histogram.
;      This is generally slower (~factor of 2), but may be better for sparse 
;      arrays containing large integers, which may cause memory
;      problems for histogram.  numlist, h, and rev_arr are not available
;      when this keywod is set.
;       
; OUTPUTS: 
;    match1: match indices for first array.
;    match2: match indices for second array. 
;
; OPTIONAL OUTPUTS: 
;    ** these only returned if n_elements(arr1) gt 1 and
;    ** n_elements(arr2) gt 1, and the /SORT keyword is not set
;    numlist1: Total num. of matches for each unique element that 
;               matched
;    h1: the histogram of arr1 in intersection(arr1,arr2)
;    reverse_indices1=reverse_indices1: the reverse indices from the 
;         histogram function on arr1
;    ** same for arr2
;
; CALLED ROUTINES:
;    histogram
;    match 
;    uniq
;
;	
; REVISION HISTORY:
;    29-May-2002: Erin Scott Sheldon UofMich in IDL v5.2
;    07-May-2009: Amy E. Kimball UWash.
;                 Fixed a bug where code crashed if no overlap in the range of
;                 arr1 and arr2 (and sort=0).
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


pro match_dup, arr1, arr2, match1, match2, numlist1=numlist1, h1=h1, reverse_indices1=reverse_indices1, numlist2=numlist2, h2=h2, reverse_indices2=reverse_indices2, sort=sort

  if n_params() lt 2 then begin 
      print,'-Syntax: match_dup, arr1, arr2, match1, match2, numlist1=numlist1, h1=h1, reverse_indices1=reverse_indices1, numlist2=numlist2, h2=h2, reverse_indices2=reverse_indices2, /SORT'
      return
  endif 

  narr1 = n_elements(arr1)
  narr2 = n_elements(arr2)

  ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
  ;; special case of only one element sent for arr1
  ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

  if narr1 eq 1 then begin 
      match2=where(arr2 eq arr1[0], nw)
      if nw eq 0 then match1 = -1l else match1 = [0l]
      return
  endif 

  ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
  ;; special case of only one element sent for arr2
  ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

  if narr2 eq 1 then begin 
      match1=where(arr1 eq arr2[0], nw)
      if nw eq 0 then match2 = -1 else match2 = [0l]
      return
  endif 

  ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
  ;; general case
  ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

  if not keyword_set(sort) then begin 

      ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
      ;; Histogram method
      ;; which integers in the intersection are in 
      ;; both arrays? Non-zero elements of histogram
      ;; indicate matches.
      ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

      max = min( [max(arr1, min=min1), max(arr2, min=min2)] )
      min = max( [min1, min2] )
      if min gt max then begin
        ;; no possible overlap between the two arrays
        match1 = -1
        match2 = -1
        numlist1=0L
        numlist2=0L
        return
      endif
      h1=histogram(arr1, reverse_indices=reverse_indices1, $
                   min=min, max=max $
                  )
      h2=histogram(arr2, reverse_indices=reverse_indices2, $
                   min=min, max=max $
                  )
      
      
      wm = where( (h1 ne 0) and (h2 ne 0), nm)
      
      if nm ne 0 then begin 
          
          n1 = long( total(h1[wm]) )
          n2 = long( total(h2[wm]) )
          
          ptrlist1 = ptrarr(n1)
          numlist1 = lonarr(n1)
          nmatch1 = 0l
          
          ptrlist2 = ptrarr(n2)
          numlist2 = lonarr(n2)
          nmatch2 = 0l
          
          for i=0l, nm-1 do begin 
              
              binnum = wm[i]
              
              ;; match indices
              s1 = reverse_indices1[ $
                     reverse_indices1[binnum]:reverse_indices1[binnum+1] -1 $
                     ]
              s2 = reverse_indices2[ $
                     reverse_indices2[binnum]:reverse_indices2[binnum+1] -1 $
                     ]
              
              ns1 = n_elements(s1)
              ns2 = n_elements(s2)
              
              ;; pointers
              ptrlist1[i] = ptr_new(s1, /no_copy)
              numlist1[i] = ns1
              nmatch1 = nmatch1 + ns1
              
              ptrlist2[i] = ptr_new(s2, /no_copy)
              numlist2[i] = ns2
              nmatch2 = nmatch2 + ns2
              
          endfor 
          
      endif else begin 
          match1 = -1
          match2 = -1
          numlist1 = 0l
          numlist2 = 0l
          return
      endelse 
            
  endif else begin 
            
      ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
      ;; sort/uniq method
      ;; match the unique ones, then collect
      ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

      s1 = sort(arr1)
      s2 = sort(arr2)

      ;; uniq will return sorted
      uniq1 = uniq(arr1, s1)
      uniq2 = uniq(arr2, s2)

      ;; are there matches?
      match, arr1[uniq1], arr2[uniq2], m1, m2, /sort

      if m1[0] eq -1 then begin 
          match1 = -1
          match2 = -1
          return
      endif 

      ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
      ;; now collect the matches, duplicates and all
      ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

      n1 = n_elements(m1)
      n2 = n_elements(m2)

      ptrlist1 = ptrarr(n1)
      numlist1 = lonarr(n1)
      nmatch1 = 0l
          
      ptrlist2 = ptrarr(n2)
      numlist2 = lonarr(n2)
      nmatch2 = 0l

      tind1 = -1
      tind2 = -1

      ;; matches for arr1
      flag = 0
      nn = 0l
      for i=0l, n1-1 do begin 
          
          if nn le narr1 then begin 
              val = arr1[ uniq1[ m1[i] ] ]
              while arr1[s1[nn]] le val do begin 
                  if arr1[s1[nn]] eq val then begin 
                      if (flag eq 0)  then begin 
                          tind1 = nn
                          flag = 1
                      endif 
                      tind2 = nn
                  endif 
                  nn = nn + 1l
                  if nn eq narr1 then goto,jump1
              endwhile 

          endif 
          
jump1:
          if tind1 ne -1 then begin 
              ptrlist1[i] = ptr_new(s1[tind1:tind2], /no_copy)
              numlist1[i] = tind2-tind1+1
              nmatch1 = nmatch1 + numlist1[i]
          endif 

          flag = 0
          tind1 = -1
          tind2 = -1
      endfor
 
      ;; matches for arr2
      flag = 0
      nn = 0l
      for i=0l, n2-1 do begin 
          
          if nn le narr2 then begin 
              while arr2[s2[nn]] le arr2[uniq2[ m2[i] ]] do begin 
                  if (arr2[s2[nn]] eq arr2[uniq2[ m2[i] ]]) then begin 
                      if (flag eq 0)  then begin 
                          tind1 = nn
                          flag = 1
                      endif 
                      tind2 = nn
                  endif 
                  nn = nn + 1l
                  if nn eq narr2 then goto,jump2
              endwhile 

          endif 
jump2:          
          if tind1 ne -1 then begin 
              ptrlist2[i] = ptr_new(s2[tind1:tind2], /no_copy)
              numlist2[i] = tind2-tind1+1
              nmatch2 = nmatch2 + numlist2[i]
          endif 

          flag = 0
          tind1 = -1
          tind2 = -1
      endfor

  endelse 

  ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
  ;; now extract match arrays from pointers
  ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

  match1 = lonarr(nmatch1)
  match2 = lonarr(nmatch2)
  
  beg=0l
  for i=0l, n1-1 do begin 
      if numlist1[i] ne 0 then begin 
          match1[beg:beg+numlist1[i]-1] = *ptrlist1[i]
      endif 
      ptr_free, ptrlist1[i]
      beg = beg + numlist1[i]
  endfor 
  
  beg=0l
  for i=0l, n2-1 do begin 
      if numlist2[i] ne 0 then begin 
          match2[beg:beg+numlist2[i]-1] = *ptrlist2[i]
      endif 
      ptr_free, ptrlist2[i]
      beg = beg + numlist2[i]
  endfor 
  
  
  return
end 
