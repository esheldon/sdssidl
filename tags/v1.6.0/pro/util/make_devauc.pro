;+
; NAME:
;   MAKE_DEVAUC
;
; PURPOSE:
; Dave Johnston's code.  My Changes. E.S.S.
; This code makes de Vaucouleurs intensity profile, used for elliptal galaxies
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

pro make_devauc, disk, siz, rzero, counts=counts, aratio=aratio,$
                 theta=theta, cen=cen

  if n_params() LT 3 then begin
    print, '-syntax: disk, siz, rzero, counts=counts, aratio=aratio,theta=theta, cen=cen'
    print,'  Theta in degrees'
    return
  endif

  sx=long64(siz[0])
  sy=long64(siz[1])

;; check some parameters

  IF n_elements(counts) EQ 0 THEN counts = 1.0 ELSE counts=float(counts)
  IF n_elements(aratio) EQ 0 THEN aratio = 1.0 ELSE aratio = float(aratio)
  IF n_elements(theta) EQ 0 THEN theta = 0.0 ELSE theta = float(theta)
  IF n_elements(cen) EQ 0 THEN BEGIN
    cen = [(sx-1.0)/2., (sy-1.)/2.]
  ENDIF
  cx = float(cen(0))
  cy = float(cen(1))

;; create our galaxy
  disk=fltarr(sx,sy)

  index= l64indgen(sx*sy)
  x=index mod sx
  y=index/sx

  ct=cos(theta*!Pi/180.)
  st=sin(theta*!Pi/180.)

;; Rotate the coordinate system
  xp=(x-cx)*ct + (y-cy)*st
  yp=(-1*(x-cx)*st + (y-cy)*ct)/aratio

;; define r in this coordinate system
  r=sqrt(xp^2+yp^2)

;; scale r
  rr=r/rzero
  arg = 7.67*( (rr)^(.25) -1 )

;; use the appropriate function
;Dave used Hubble law instead for some reason
;disk(index)=(1+rr)^(-2) 

  w=where(arg LT 10.8, good)    ;watch floating point underflow
                                ;this will allow ~.00002 as smallest number
                                ;in disk
  IF good GT 0 THEN BEGIN
    disk(index[w]) = exp(-arg(w))
    disk = counts/total(disk)*disk
  ENDIF

return
end




