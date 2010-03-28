;+
; NAME:
;    BAYES_PROB_NC
;
;
; PURPOSE:
;    Util for bayes_prob.pro Create p from prob using number counts,m
;
; MODIFICATION HISTORY:
;    Creation:  ??-??-?? Dave Johnston, UofChicago
;
;     Current version: 1.5
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


PRO bayes_prob_nc,p,mag,prob,niter=niter,bin=bin,_extra=ex,plot=plot
;create p from prob using number counts,m
  
  IF n_params() EQ 0 THEN BEGIN
      print,'-syntax bayes_prob_nc,p,m,prob'
      RETURN
  ENDIF

  COMMON bayes_prob_block, PROB_PSF_DEFAULT, MAXMAG, MINMAG, MINSEE, MAXSEE, $
    PROBFLAG_NOITER_REQUESTED, PROBFLAG_NOITER, PROBFLAG_NOPROB, $
    PROBFLAG_NOMEAS, $
    PROBFLAG_MAGBOUNDS, PROBFLAG_SEEINGBOUNDS, $
    PROBFLAG_NEGCON,$
    SEE_A, SEE_OFF, MAG_A, MAG_OFF, N_SEEBINS, N_MAGBINS

  IF n_elements(PROB_PSF_DEFAULT) EQ 0 THEN bayes_set_common

  IF n_elements(niter) EQ 0 then niter=6
  IF n_elements(bin) EQ 0 then bin=.5

  m=mag > MINMAG < MAXMAG
  
  lr=(1-p)/(p > 1e-11)
  
  h=histogram(m,rev=rev,binsize=bin)

  nh=n_elements(h)
  ntot=fltarr(nh)
  nstar=ntot
  ngal=ntot
  mh=ntot
  prob=p
  
  FOR j=0, niter DO begin
      FOR i=0, nh-1 DO BEGIN
          IF rev(i) eq rev(i+1) then BEGIN 
                                ;none here
          ENDIF  ELSE BEGIN 
              w=rev(rev(i):rev(i+1)-1)
              n=float(n_elements(w))
              IF j EQ 0 THEN mh(i)=mean(m(w))
              ntot(i)=n
              ngal(i)=total(prob(w))
              nstar(i)=n-ngal(i)
              rat=nstar(i)/(ngal(i) > 1e-11)
              prob(w)=1.0/(1.0+rat*lr(w))
              
          ENDELSE
          
      ENDFOR         
      IF keyword_set(plot) THEN BEGIN
          
          IF j EQ 0 THEN BEGIN
              plot,mh,ngal,color=130,psym=10,_extra=ex 
              oplot,mh,nstar,color=130,psym=10 
          ENDIF  ELSE BEGIN
              oplot,mh,ngal,psym=10
              oplot,mh,nstar,psym=10
          ENDELSE
      ENDIF
      
  ENDFOR  
  IF keyword_set(plot) THEN BEGIN
      oplot,mh,ngal,color=100,psym=10
      oplot,mh,nstar,color=100,psym=10
  ENDIF

  RETURN 

END 
