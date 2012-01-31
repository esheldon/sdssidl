pro make_exp, disk, siz, rzero, counts=counts, aratio=aratio,$
                  theta=theta, cen=cen

;+
; NAME: 
;    make_exp
;
; PURPOSE: 
;    make an array with values set to an ellitical exponential.
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


  IF n_params() LT 3 THEN BEGIN 
    print,'-Syntax: make_exp, disk, siz, rzero, counts=counts, aratio=aratio, theta=theta, cen=cen'
    print,'Theta in degrees'
    return
  ENDIF

  ;;; Check the parameters
  rzero=float(rzero > .1)
  IF n_elements(counts) EQ 0 THEN counts = 1.0 ELSE counts = float(counts)
  IF n_elements(aratio) EQ 0 THEN BEGIN 
    aratio=1.0
    theta=0.0
  ENDIF ELSE BEGIN 
    aratio=float(aratio)
    IF n_elements(theta) EQ 0 THEN theta=0.0 ELSE float=float(theta)
  ENDELSE 
  IF aratio LT .1 THEN aratio=.1

  sx=long(siz(0))
  sy=long(siz(1))
  IF n_elements(cen) EQ 0 THEN cen=[(sx-1.)/2.,(sy-1.)/2.]
  cx=cen(0)
  cy=cen(1)

  ;;;;;;;;;;;;;;;;;;;;;;;;;;
  ;;; Create the disk
  ;;;;;;;;;;;;;;;;;;;;;;;;;;
  disk=fltarr(sx,sy)
  index=lindgen(sx*sy)
  x=float(index MOD sx)
  y=float(index/sx)
  ct=cos(double(theta)*!dpi/180d)
  st=sin(double(theta)*!dpi/180d)
  xp=(x-cx)*ct + (y-cy)*st
  yp=((cx-x)*st + (y-cy)*ct)/aratio
  rr=sqrt(xp^2+yp^2)/rzero
  w=where(rr lt 10.8,cat)       ;watch floating point underflow
				;this will allow ~.00002 as smallest number
				;in disk
  IF cat GT 0 THEN BEGIN 
    disk(index(w))=exp(-rr(w))
    disk=(counts/total(disk))*disk
  ENDIF 
  
  return
END 




