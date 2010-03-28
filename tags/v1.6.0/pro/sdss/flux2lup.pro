;+
; NAME:
;  flux2lup
;
; PURPOSE:
;  Convert flux to SDSS luptitudes; this has different conventions than nmgy2lups which gives the expected result for the princeton redux. 
;
; CATEGORY:
;  SDSS specific routine
;
; CALLING SEQUENCE:
;   flux2lup, flux, mag, fluxerr, magerr,b=b
;
; INPUTS:
;  flux: flux array.  Can be either full 5-bandpass array or single.
;
; OPTIONAL INPUTS:
;   fluxerr: err in flux.  If there then magerr will be returned.
;   b=b: If single bandpass, must be specified with b=0, 1, 2, 3, or 4
;
; OUTPUTS:
;   mag: mags in luptitudes
;
; OPTIONAL OUTPUTS:
;   magerr: only returned if fluxerr is sent.
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

PRO flux2lup,flux,mag,fluxerr,magerr,b=b

  IF n_params() EQ 0 THEN BEGIN
      print,'-syntax flux2lup,flux,mag,fluxerr,magerr,b=b'
      RETURN
  ENDIF

  if n_elements(flux) eq 1 then sz=size(flux) else sz=size(reform(flux)) 

  lupb=[ 1.4 , 0.9 , 1.2 , 1.8 , 7.4 ] *1e-10
  F0=3630.78
  POGSON = 1.08574

  IF sz(0) EQ 1 or sz(0) eq 0 THEN BEGIN
      IF n_elements(b) EQ 0 THEN BEGIN
          print,'Error: MUST specify bandpass (keyword b) for 1-D magnitude arrays'
          RETURN
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
          RETURN
      ENDELSE
  ENDIF

  x =(flux < 1e10)/(2.0*lup*F0)
  mu=alog(x+sqrt(x^2+1.0))
  mag=float(-POGSON*(mu+alog(lup)))

  IF n_elements(fluxerr) GT 0 THEN begin
      aux=sqrt(flux^2+(2.0*lup*F0)^2)
      magerr= float(POGSON*fluxerr/aux)
  ENDIF

  RETURN
END

