pro sigma_clip,array,amean,asigma,nsig=nsig,nIter=nIter,$
               plot=plot,_extra=ex,pause=pause,index=index,silent=silent

;+
; NAME:
;    SIGMA_CLIP
;
; PURPOSE:
;    a simple sigma clipping algoritm
;
; CALLING SEQUENCE:
;  sigma_clip, array, amean, asigma, nsig=, niter=, 
;              /silent, pause=, index=, /plot, _extra=
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

  if n_params() lt 3 then begin
      print, '-syntax sigma_clip, arr, mean, sigma, nsig=, nIter=, index=, pause=, /plot, _extra='
      return
  endif

  IF NOT keyword_set(silent) THEN silent = 0 ELSE silent=1

  if n_elements(nsig) eq 0 then nsig=3.5
  if n_elements(niter) eq 0 then niter=4

  wif=n_elements(array)
  index=lindgen(wif)
  
  for i=0,nIter-1 do begin
      if keyword_set(plot) then BEGIN
          IF n_elements(parts) EQ 0 THEN parts=(max(array)-min(array))/50
          if i eq 0 then plothist,array[index],_extra=ex,bin=parts
          if i gt 0 then plothist2,array[index],/overplot,bin=parts,_extra=ex
      endif

      mom=moment(array[index], Maxmoment=2)
      m=mom(0)
      s=sqrt(mom(1))
      clip=nsig*s
      if NOT silent then print,' mean',m,'   sigma',s,'  number',wif 
      w=where(abs(array[index]-m) lt clip,wif)
      if wif eq 0 then begin
          IF NOT silent THEN print,'nsig is too small. Everything clipped on iteration',i
          ;mom=moment(array[index], Maxmoment=2)
          amean=m
          asigma=s
          return
      endif 
      if n_elements(pause) gt 0 then wait,pause
      index=index(w)
  endfor

  mom=moment(array[index], Maxmoment=2)
  amean=mom(0)
  asigma=sqrt(mom(1))
  if NOT silent then print,' final mean',amean,'   final sigma',asigma
  return
end


