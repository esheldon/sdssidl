;+
;  NAME:
;    create_photomap
;
;  PURPOSE:
;    Map ra/dec to pixel coordinates.
;
;  Created:
;    2002, Erin Sheldon, UofMichigan
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

PRO create_photomap, ra, dec, row, col, mapStruct, $
                     ra_center=ra_center, dec_center=dec_center, $
                     map_order=map_order

  IF n_params() LT 5 THEN BEGIN 
      print,'-Syntax: create_photomap, ra, dec, row, col, mapStruct, $'
      print,'          ra_center=ra_center, dec_center=dec_center, $'
      print,'          map_order=map_order'
      print,'Defaults: map_order=1, ra/dec_center is mean'
      return
  ENDIF 

  IF n_elements(map_order) EQ 0 THEN map_order=1

  nra  = n_elements(ra)
  ndec = n_elements(dec)
  nrow = n_elements(row)
  ncol = n_elements(col)

  IF (nra NE ndec) OR (nra NE nrow) OR (nra NE ncol) THEN BEGIN
    print,'ra, dec, row, col must be same size'
    return
  ENDIF 

  IF n_elements(ra_center) EQ 0 OR n_elements(dec_center) EQ 0 THEN BEGIN 
      
      minra  = min(ra,  max=maxra)
      mindec = min(dec, max=maxdec)

      ra_center  = ( maxra  + minra  )/2.0
      dec_center = ( maxdec + mindec )/2.0

  ENDIF 

  astr_struct, astr_struct
  astr_struct.crval = [ra_center, dec_center]

  ;; Convert ra/dec to x/y
  rd2xy, ra, dec, astr_struct, x1, x2

  ;; get transformation
  polywarp, row, col, x1, x2, map_order, kx, ky

  mapStruct = create_struct('map_order', map_order, $
                            'astr_struct', astr_struct, $
                            'kx', kx, $
                            'ky', ky)

END 
