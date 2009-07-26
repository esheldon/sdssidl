pro make_gaussian,gauss,size=siz,cntrd=cen,counts=counts,fwhm=fwhm, $ 
aratio=aratio,theta=theta

;+
; NAME:
;   MAKE_GAUSSIAN
;
; PURPOSE:
;   make an array with a gaussian in it
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
	print,'-syntax ','make_gaussian,gauss,size=siz,cntrd=cen,counts=counts,fwhm=fwhm,aratio=aratio,theta=theta'
	return
endif

rzero=.600561*fwhm
if n_elements(counts) eq 0 then counts=1.
if n_params() eq 0 then begin
	print,'-syntax','make_gaussian,gauss,size=siz,cntrd=cen,counts=counts,rzero=rzero,aratio=aratio,theta=theta'
	return
endif

sx=long(siz(0))
sy=long(siz(1))
if n_elements(cen) eq 0 then cen=[(sx-1.)/2.,(sy-1.)/2.]
cx=cen(0)
cy=cen(1)
gauss=fltarr(sx,sy)
index=lindgen(sx*sy)

x=index mod sx
y=index/sx
 
if n_elements(aratio) eq 0 then begin
	xp=x-cx   &  yp=y-cy
endif else begin
	aratio=aratio > .1
	ct=cos(theta*!Pi/180.)
	st=sin(theta*!Pi/180.)
	xp=(x-cx)*ct + (y-cy)*st
	yp=((cx-x)*st + (y-cy)*ct)/aratio
endelse

rr=(xp^2+yp^2)/(rzero^2)
w=where(rr lt 11.51,cat)		;watch floating point underflow
				;this will allow ~.00002 as smallest number
				;in disk
if cat gt 0 then begin
	gauss(index(w))=exp(-rr(w)) 
	gauss=gauss*(counts/total(gauss))
endif
return
end





