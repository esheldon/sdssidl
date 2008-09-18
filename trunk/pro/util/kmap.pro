pro kmap,xin,yin,xout,yout,kx,ky
;
;+
; NAME:
;    KMAP
;
; CALLING SEQUENCE:
;   kmap,xin,yin,xout,yout,kx,ky
;
; PURPOSE:
;    this maps (xin,yin) to (xout,yout)
;    with the transformation kx,ky
;    that were presumably found with polywarp
;    the formula is (summation over i,j)
;    xout=(kx)ij * xin^j * yin^i
;    yout=(ky)ij * xin^j * yin^i
;    hence the two matrices kx and ky hold all the 
;    transformation coefficients
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
	print,'-syntax kmap,xin,yin,xout,yout,kx,ky'
	return
endif

siz=size(kx)
dim=siz(1)	;kx and ky are dim by dim matrices
num=n_elements(xin)
xout=dblarr(num)
yout=xout
xout1=xout
yout1=xout

for i=dim-1,0,-1 do begin
	for j=dim-1,0,-1 do begin
		xout1=kx(i,j)+xin*xout1
		yout1=ky(i,j)+xin*yout1
	endfor
	xout=xout1+xout*yin
	yout=yout1+yout*yin
	xout1=dblarr(num)
	yout1=xout1
endfor

return
end

		

