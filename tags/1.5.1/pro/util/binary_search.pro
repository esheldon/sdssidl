pro binary_search,arr,x,index, round=round, edgedefault=edgedefault
;+
;
; NAME:
;    BINARY_SEARCH
;
; CALLING SEQUENCE:
;    binary_search,arr,x,index [, /round ]
;
; PURPOSE:
;    Perform a binary search on arr, an input array, for the closest match
;    to x.  
;
;    Will give closest element of arr that is _smaller_ than x.  Set /round to 
;    gaurantee you get the closest overall element. -E.S.S.
;    Set /edgedefault to use endpoint if input x is gr arr[n-1] or x is lt arr[0]
;                                                   -E.S.S.
;
; Modification History
;   Author: Dave Johnston.
;   Added round keyword  04/29/00  Erin Scott Sheldon  UofMich
;   Added edgedefault keyword 07-Mar-2001
; 
;-
;
;
;
;  Copyright (C) 2006  Dave Johnston
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

  if n_params() eq 0 then begin
      print,'syntax- binary_search,arr,x,index'
      return
  endif

  n=n_elements(arr)

  if (x lt arr[0]) or (x gt arr[n-1]) then BEGIN

      IF keyword_set(edgedefault) THEN BEGIN 
          CASE 1 OF
              (x LT arr[0]): index=0L
              (x GT arr[n-1]): index=n-1
          ENDCASE
      ENDIF ELSE index=-1

      return
  ENDIF

  down=-1
  up=n

  while up-down gt 1 do begin
      mid=down+(up-down)/2
      if x ge arr(mid) then begin
          down=mid
      endif else begin
          up=mid
      endelse
  ENDWHILE

  index=down

  IF keyword_set(round) AND (index NE n-1) THEN BEGIN
      IF abs(x-arr[index]) GT abs(x-arr[index+1]) THEN index = index+1
  ENDIF 

  return
end	
