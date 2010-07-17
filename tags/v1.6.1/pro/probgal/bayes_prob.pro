;+
; NAME:
;   BAYES_PROB
;
;
; PURPOSE:
;   Calculate bayesian probabilities for an object being a galaxy
;
; CALLING SEQUENCE:
;  bayes_prob, cat, probflags, probgal, rat, ls, lg, c, see, mag
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

PRO bayes_prob,cat,probflags,probgal,rat,ls,lg,c,see,mag,combined=combined

  IF n_params() EQ 0 THEN BEGIN 
      print,'-syntax bayes_prob,cat,prob,rat,ls,lg,c,see,mag'
      print,'version 1.5'
      return
  ENDIF

  COMMON bayes_prob_block, PROB_PSF_DEFAULT, MAXMAG, MINMAG, MINSEE, MAXSEE, $
    PROBFLAG_NOITER_REQUESTED, PROBFLAG_NOITER, PROBFLAG_NOPROB, $
    PROBFLAG_NOMEAS, $
    PROBFLAG_MAGBOUNDS, PROBFLAG_SEEINGBOUNDS, $
    PROBFLAG_NEGCON,$
    SEE_A, SEE_OFF, MAG_A, MAG_OFF, N_SEEBINS, N_MAGBINS

  IF n_elements(PROB_PSF_DEFAULT) EQ 0 THEN bayes_set_common

  ncat = n_elements(cat)
  probgal = replicate((1. - PROB_PSF_DEFAULT), ncat)

  IF keyword_set(combined) THEN BEGIN
      mag = cat.mag
      see = cat.see
      c = cat.c
  ENDIF ELSE BEGIN
      bayes_combine_gri,cat,probflags,mag,see,c,detnum
  ENDELSE

  bayes_pars,mag,see,probflags,musl,sigsl,musg,sigsg,lgr,mug,sigg

  ;; some hard sanity bounds
  musl = musl > (-.01) < 0.1
  sigsl = sigsl > 0.005 < 0.09
  musg = musg > 0.01 < 0.1
  sigsg = sigsg > 0.005 < 0.1
  mug = mug > 0.1 < 0.5
  sigg = sigg > 0.05 < 0.11
  lgr = lgr > 0.1 < 0.7

  s2p=1.0/sqrt(2.0*!pi)

  usl=((alog(c+1.0)-musl)/sigsl)^2 < 90.0
  usg=((c-musg)/sigsg)^2 < 90.0

  ug=((alog(c+1.0)-mug)/sigg)^2 < 90.0
  
  lsl=s2p*exp(-.5*usl)/(sigsl*(c+1.0))
  lsg=s2p*exp(-.5*usg)/sigsg
  ls = lsl + lgr*lsg

  lg=s2p*exp(-.5*ug)/(sigg*(c+1.0))
  
  rat=ls/lg

  w = where(c GT -10.0 AND c LT musl-2.5*sigsl AND rat < 1.0, n_neg)
  IF n_neg GT 0 THEN BEGIN
      rat(w) = 99.0
      probflags(w) = probflags(w) + PROBFLAG_NEGCON
  ENDIF

  wc=where(c NE -10.0, nwc, comp=comp, ncomp=ncomp)
  IF nwc NE 0 THEN probgal[wc]=1.0/(1.0+rat[wc])
  IF ncomp NE 0 THEN probflags[comp] = probflags[comp] + PROBFLAG_NOPROB

  RETURN

END 












