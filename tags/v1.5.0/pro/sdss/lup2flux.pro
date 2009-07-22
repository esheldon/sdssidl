;+
; NAME:
;  lup2flux
;
; PURPOSE:
;  convert SDSS luptitudes to flux; this has different conventions than lups2nmgy which gives the expected result for the princeton redux. 
;
; CATEGORY:
;  SDSS specific routine
;
; CALLING SEQUENCE:
;   lup2flux, mag, flux, magerr, fluxerr, b=b
;
; INPUTS:
;   mag: magnitude array.  Can be either full 5-bandpass array or single.
;
; OPTIONAL INPUTS:
;   magerr: err in mags.  If there then fluxerr will be returned.
;   b=b: If single bandpass, must be specified with b=0, 1, 2, 3, or 4
;
; OUTPUTS:
;   flux: Linear flux
;
; OPTIONAL OUTPUTS:
;   fluxerr: only returned if err is sent.
;
;
; MODIFICATION HISTORY:
;   ??-??-?? Dave Johnston. Translation of Zeljko's SM code
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


PRO lup2flux,mag,flux,magerr,fluxerr,b=b

  IF n_params() EQ 0 THEN BEGIN
      print,'-syntax lup2flux,mag,flux,magerr,fluxerr,b=b'
      return
  endif

  if n_elements(mag) eq 1 then sz=size(mag) else sz=size(reform(mag)) 

  lupb=[ 1.4d , 0.9d , 1.2d , 1.8d , 7.4d ] *1e-10
  F0=3630.78
  POGSON = 1.08574

  IF sz(0) EQ 1 or sz(0) eq 0 THEN BEGIN
      IF n_elements(b) EQ 0 THEN BEGIN
          print,'Error: MUST specify bandpass (keyword b) for 1-D magnitude arrays'
          return
      ENDIF
      n=sz(1)
      lup=lupb(b)
  ENDIF

  IF sz(0) EQ 2 THEN BEGIN
      IF sz(1) EQ 5 THEN BEGIN
          n=sz(2)
          ones=replicate(1,n)
          lup=ones##lupb
      ENDIF ELSE BEGIN
          print,'ERROR: 2D arrays must be of size (5,N)'
          IF sz(2) EQ 5 THEN print,'hint: might need to use transpose if array is (N,5)'
          return
      ENDELSE
  ENDIF

  tmpmag=(-mag/POGSON - alog(lup)) > (-50.0) < 50.0
  tmpcnts= 2.0*lup*(exp(tmpmag)-exp(-tmpmag))/2.0
  flux=F0*tmpcnts

  IF n_elements(magerr) NE 0 THEN BEGIN
      fluxerr=F0*magerr/POGSON*sqrt(tmpcnts^2+4.0*lup^2)
  ENDIF

  return
END










