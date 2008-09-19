pro bolo_mag,mag,sn,zeropoint,bflux,bsn,bmag,see,seew


;+
; NAME:
;  BOLO_MAG
;
;
; PURPOSE:
;  compute bolometric magnitude of g,r,i mag and zeropoint is (5,n)
;
;
; CATEGORY:
;  sdss routine
;
;
; CALLING SEQUENCE:
;  bolo_mag,mag,sn,zeropoint,bflux,bsn,bmag,see,seew
;
; MODIFICATION HISTORY:
;   Creation: 19-Nov-2002, Dave Johnston, UofChicago
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
   print,'-syntax bolo_mag,mag,sn,zeropoint,bflux,bsn,bmag,see,seew'
   return
endif

;constants mostly from the EDR paper
lupb=[ 1.4 , 0.9 , 1.2 , 1.8 , 7.4 ] *1e-10
const= -0.921034        ;-ln(10)/2.5
Xref=1.3
UGref=1.68      ;my guess from info in EDR paper 

ss=size(mag)
n=ss(2)
ones=replicate(1,n)
lup=ones##lupb

zpt=zeropoint > 0.0 < 40.0

F0=10.0^(zpt/2.5)

flux=2.0*54.0*lup*F0*sinh( (mag*const-alog(lup))   > (-50.0) < (50.0) )
flux=flux < 1.0e7 > (-100.0)


if 1 then begin
   w1=1.0
   w2=1.0
   w3=1.0
endif else begin
      w1=sn(1,*)^2
      w2=sn(2,*)^2
      w3=sn(3,*)^2
endelse

bflux=w1*flux(1,*)+w2*flux(2,*)+w3*flux(3,*)

if n_elements(see) gt 0 then begin
   seew=(see(1,*)^2)*w1*flux(1,*)+(see(2,*)^2)*w2*flux(2,*)+(see(3,*)^2)*w3*flux(3,*)
   seew=sqrt((seew >0.0)/(bflux > .00001))
endif

s1=(w1*flux(1,*)/(sn(1,*) >.01 ))^2
s2=(w2*flux(2,*)/(sn(2,*) >.01 ))^2
s3=(w3*flux(3,*)/(sn(3,*) >.01 ))^2

bsn=bflux/sqrt((s1+s2+s3) >.000001)

bsn=reform(bsn)
bflux=reform(bflux)

zpt=zpt(2,*)
F0=10.0^(zpt/2.5)
xx=bflux/(2.0*54*lupb(2)*F0)
xx=xx+sqrt(xx^2+1)

bmag=(1.0/const)*( alog(xx) +alog(lupb(2)))

return
end





