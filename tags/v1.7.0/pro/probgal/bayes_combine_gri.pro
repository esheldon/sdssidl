;+
; NAME:
;    COMBINE_GRI
;
;
; PURPOSE:
;    Combine g,r, and i fluxes and output new magnitudes for psfcounts
;    and counts_exp.  Also, make a combined seeing from the three bandpasses.
;    Thanks Zelko for corrections on flux conversion  
;
; CATEGORY:
;    SDSS specific routine for galaxy probability calculations
;
;
; CALLING SEQUENCE:
;    combine_gri,cat,mag,see,c,detnum,det=det,av=av
;
;
; INPUTS:
;    cat: PHOTO structure containing adaptive moments, psfcounts, counts_exp
;         flags, flags2
;
;
; MODIFICATION HISTORY:
;     Created: 29-APR-2003 Dave Johnston
;     Modified: 13-OCT-2003 Ryan Scranton; added model and cmodel switches
;     Modified: 5-APR-2004 Ryan Scranton; switched to flux-weighted combination
;
;     Current version: 1.5
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


PRO bayes_combine_gri,cat,probflags,mag,see,c,ivarsum,mag_psf,gweight,iweight,$
                      flat=flat,av=av,verbose=verbose

  IF n_params() EQ 0 THEN BEGIN
      print,'-syntax bayes_combine_gri,cat,mag,see,c,ivarsum,det=det,av=av'
      RETURN
  ENDIF

  COMMON bayes_prob_block, PROB_PSF_DEFAULT, MAXMAG, MINMAG, MINSEE, MAXSEE, $
    PROBFLAG_NOITER_REQUESTED, PROBFLAG_NOITER, PROBFLAG_NOPROB, $
    PROBFLAG_NOMEAS, $
    PROBFLAG_MAGBOUNDS, PROBFLAG_SEEINGBOUNDS, $
    PROBFLAG_NEGCON,$
    SEE_A, SEE_OFF, MAG_A, MAG_OFF, N_SEEBINS, N_MAGBINS

  IF n_elements(PROB_PSF_DEFAULT) EQ 0 THEN bayes_set_common

                                ;constants mostly from the EDR paper
  boloff= 0.0

;  off=.15
;  power=.2


  n_cat = n_elements(cat)

  ivarg=fltarr(n_cat)
  ivarr=fltarr(n_cat)
  ivari=fltarr(n_cat)
  ivarsum=fltarr(n_cat)
  keep=intarr(n_cat)

  fs = {binned1:'Y'}
  fs.binned1='Y'
  indexg = sdss_flag_select(cat.flags[1], 'object1', fs)
  indexr = sdss_flag_select(cat.flags[2], 'object1', fs)
  indexi = sdss_flag_select(cat.flags[3], 'object1', fs)

  IF indexg[0] NE -1 THEN keep(indexg) = keep(indexg) + 1
  IF indexr[0] NE -1 THEN keep(indexr) = keep(indexr) + 1
  IF indexi[0] NE -1 THEN keep(indexi) = keep(indexi) + 1

  w_blank = where(keep EQ 0, n_blank)
  w_keep = where(keep NE 0, n_keep)

  IF keyword_set(verbose) THEN BEGIN
      print,ntostr(n_keep),' of ',ntostr(n_cat),' objects passed flag cuts...'
  ENDIF

  IF keyword_set(av) THEN BEGIN
      gpsf=cat.psfcounts_mean(1)
      rpsf=cat.psfcounts_mean(2)
      ipsf=cat.psfcounts_mean(3)
      
      gpsferr=cat.psfcounts_err(1)
      rpsferr=cat.psfcounts_err(2)
      ipsferr=cat.psfcounts_err(3)
      
      gexp=cat.cmodel_counts_mean(1)
      rexp=cat.cmodel_counts_mean(2)
      iexp=cat.cmodel_counts_mean(3)
      
      gexperr=cat.cmodel_counts_err(1)
      rexperr=cat.cmodel_counts_err(2)
      iexperr=cat.cmodel_counts_err(3)
      
  ENDIF ELSE BEGIN
      gpsf=cat.psfcounts(1)
      rpsf=cat.psfcounts(2)
      ipsf=cat.psfcounts(3)
      
      gpsferr=cat.psfcountserr(1)
      rpsferr=cat.psfcountserr(2)
      ipsferr=cat.psfcountserr(3)
      
      IF tag_exist(cat,'cmodel_counts') THEN BEGIN
          gexp=cat.cmodel_counts(1)
          rexp=cat.cmodel_counts(2)
          iexp=cat.cmodel_counts(3)
          
          gexperr=cat.cmodel_countserr(1)
          rexperr=cat.cmodel_countserr(2)
          iexperr=cat.cmodel_countserr(3)
      ENDIF ELSE BEGIN
          make_cmodel_counts,cat,cmodel,cmodelerr
          gexp=cmodel(1,*)
          rexp=cmodel(2,*)
          iexp=cmodel(3,*)
          
          gexperr=cmodelerr(1,*)
          rexperr=cmodelerr(2,*)
          iexperr=cmodelerr(3,*)
      ENDELSE
  ENDELSE

  lowlim = 14.0
  highlim = 24.0

  gmom=cat.m_rr_cc_psf(1)
  rmom=cat.m_rr_cc_psf(2)
  imom=cat.m_rr_cc_psf(3)

  ggpsf=gpsf > lowlim < highlim
  rrpsf=rpsf > lowlim < highlim
  iipsf=ipsf > lowlim < highlim

  ggpsferr=gpsferr > 0.0001 < 5.0
  rrpsferr=rpsferr > 0.0001 < 5.0
  iipsferr=ipsferr > 0.0001 < 5.0

  ggexp=gexp > lowlim < highlim
  rrexp=rexp > lowlim < highlim
  iiexp=iexp > lowlim < highlim

  ggexperr=gexperr > 0.0001 < 5.0
  rrexperr=rexperr > 0.0001 < 5.0
  iiexperr=iexperr > 0.0001 < 5.0

  lup2flux,ggpsf,fgpsf,ggpsferr,fgpsferr,b=1
  lup2flux,rrpsf,frpsf,rrpsferr,frpsferr,b=2
  lup2flux,iipsf,fipsf,iipsferr,fipsferr,b=3

  lup2flux,ggexp,fgexp,ggexperr,fgexperr,b=1
  lup2flux,rrexp,frexp,rrexperr,frexperr,b=2
  lup2flux,iiexp,fiexp,iiexperr,fiexperr,b=3

  f_psf = fltarr(n_cat)
  f_exp = fltarr(n_cat)

  IF n_keep GT 0 THEN BEGIN

      IF keyword_set(flat) THEN BEGIN
          IF indexg[0] NE -1 THEN ivarg(indexg) = 1.0
          IF indexr[0] NE -1 THEN ivarr(indexr) = 1.0
          IF indexi[0] NE -1 THEN ivari(indexi) = 1.0
      ENDIF ELSE BEGIN
          IF indexg[0] NE -1 THEN $
            ivarg(indexg) = 1.0/(fgexperr(indexg)*fgexperr(indexg))
          IF indexr[0] NE -1 THEN $
            ivarr(indexr) = 1.0/(frexperr(indexr)*frexperr(indexr))
          IF indexi[0] NE -1 THEN $
            ivari(indexi) = 1.0/(fiexperr(indexi)*fiexperr(indexi))
      ENDELSE
      
      ivarsum(w_keep) = ivarg(w_keep) + ivarr(w_keep) + ivari(w_keep)
      
      f_psf(w_keep) = (ivarg(w_keep)*fgpsf(w_keep) + $
                       ivarr(w_keep)*frpsf(w_keep) + $
                       ivari(w_keep)*fipsf(w_keep))/ivarsum(w_keep)
      
      f_exp(w_keep) = (ivarg(w_keep)*fgexp(w_keep) + $
                       ivarr(w_keep)*frexp(w_keep) + $
                       ivari(w_keep)*fiexp(w_keep))/ivarsum(w_keep)
      
      gweight = ivarg(w_keep)/(ivarr(w_keep) + 1.0)
      iweight = ivari(w_keep)/(ivarr(w_keep) + 1.0)
  ENDIF

  flux2lup,f_psf,mag_psf,b=2
  flux2lup,f_exp,mag_exp,b=2

  mag_psf=mag_psf+boloff
  mag_exp=mag_exp+boloff

  con=mag_psf-mag_exp

  off=0.2
  power=0.4

  c=(((con+off) > 0)^power)-off^power
  c=c<1.5

;  c = con
  
  mom=(gmom*fgpsf+rmom*frpsf+imom*fipsf)/(fgpsf+frpsf+fipsf)
  mom2seeing,mom,see
  mag=mag_exp

  IF n_blank GT 0 THEN BEGIN
      c(w_blank)=-10.0
      mag(w_blank)=25.0
      ivarsum(w_blank)=0
      probflags[w_blank] = probflags[w_blank] + PROBFLAG_NOMEAS
  ENDIF
  
  RETURN

END




