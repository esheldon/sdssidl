PRO photomap, pstruct, order, ra, dec, row, col, kx, ky, clr=clr


;+
;
; NAME: 
;   PHOTOMAP
;       
; PURPOSE: 
;     map ra and dec to row and col.  The map centers on the first
;     ra-dec position in the lists.
;     NOTE: If clr is not given, then the program uses
;           RED (clr=2) to find the map.
;	
;
; CALLING SEQUENCE:
;    photomap, pstruct, order, ra, dec, row, col
;                 
;
; INPUTS: 
;     pstruct: a photo structure.  Should cover ra and dec.
;         order: order of polynomial to fit
;         ra: the ra positions to trasform in degrees.
;         dec: the dec positions to transform in degrees.
;
; OPTIONAL INPUTS: clr:  The index to use for mapping.
;
; OUTPUTS: row, col:  mapped ra and dec positions.
;
; CALLED ROUTINES:
;                CONVERT2XY
;                POLYWARP
;                KMAP
; 
;
; REVISION HISTORY:
;	Erin Scott Sheldon  U of Michigan  7/8/99
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



  IF N_params() LT 4 THEN BEGIN 
     print,'-Syntax: photomap, pstruct, order, ra, dec '+ $
             '[outrow, outcol, kx, ky, clr=clr]'
     print,''
     print,'Use doc_library,"photomap"  for more help.'  
     return
  ENDIF 

  IF n_elements(clr) EQ 0 THEN clr = 2

  n=n_elements(pstruct)
  nra = n_elements(ra)
  ndec = n_elements(dec)
  ntot = n+nra
  IF nra NE ndec THEN BEGIN
    print,'ra and dec must be same size'
    return
  ENDIF 

  convert2xy, [ra, pstruct.ra],[dec, pstruct.dec], $
              x1, x2, $
              rac=ra[0], decc=dec[0]

                                ; only use pstruct to do mapping
  polywarp, pstruct.rowc[clr], pstruct.colc[clr], $
            x1[nra:ntot-1], x2[nra:ntot-1], $
            order, kx, ky

                                ; now map the input ra's and dec's
  kmap, x1[0:nra-1], x2[0:nra-1], row, col, kx, ky

  return
END 
