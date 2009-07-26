pro compute_zeropoint,a,b,c,k,X,UG,IZ,zpt

;+
; NAME:
;    COMPUTE_ZEROPOINT
;
;
; PURPOSE:
;    compute zeropoint,zpt, from from calib info a,b,c,k are from fcPCalib 
;    file single 5-float X is airmass in fpField file single 5-float
;    UG,IZ model_colors 1d-arrays of anylength
;
;
; CATEGORY:
;    SDSS specific routine
;
;
; CALLING SEQUENCE:
;    compute_zeropoint,a,b,c,k,X,UG,IZ,zpt
;
; MODIFICATION HISTORY:
;    Creation: 19-Nov-2002, Dave Johnston, UofChicago
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


;constants mostly from the EDR paper
lupb=[ 1.4 , 0.9 , 1.2 , 1.8 , 7.4 ] *1e-10
const= -0.921034        ;-ln(10)/2.5
Xref=1.3
UGref=1.68      ;my guesses from info in EDR paper 
IZref=.351

n=n_elements(UG)

ones=replicate(1,n)
aa=ones##a
bb=ones##b
cc=ones##c
kk=ones##k
XX=ones##X

UGC=fltarr(5,n)
UGC(0,*)=UG
UGC(1,*)=UG
UGC(2,*)=UG
UGC(3,*)=IZ
UGC(4,*)=IZ

UGC2=fltarr(5,n)
UGC2(0,*)=UG-UGref
UGC2(1,*)=UG-UGref
UGC2(2,*)=UG-UGref
UGC2(3,*)=IZ-IZref
UGC2(4,*)=IZ-IZref

zpt=-1*(aa-bb*UGC+kk*XX+cc*(UGC2)*(XX-Xref))

return
end
