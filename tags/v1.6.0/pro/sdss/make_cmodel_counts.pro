;+
; NAME:
;  MAKE_CMODEL_COUNTS
;
;
; PURPOSE:
;  Create model magnitudes by combining the exponential fit with the
;  devaucoleur fit using cflux = fracpsf*devflux + (1.0-fracpdf)*expflux
;
;
; CATEGORY:
;  SDSS specific routine
;
;
; CALLING SEQUENCE:
;  make_cmodel_counts, cat, cmodel, cmodelerr
;
;
; INPUTS:
;  cat: must contain these tags:
;    COUNTS_DEV, COUNTS_DEVERR
;    COUNTS_EXP, COUNTS_EXPERR
;    FRACPSF
;    FLAGS
;
; OUTPUTS:
;  cmodel, cmodelerr: combined model mags and error
;
; PROCEDURE:
;  cflux = fracpsf*devflux + (1.0-fracpdf)*expflux
;
; MODIFICATION HISTORY:
;  ??-??-??: Ryan Scranton Pitt
;  02-Apr-2004: Added /defaults keyword.  For bad measurements, just copy in
;               the default -9999. for measurements and errors.
;-


PRO make_cmodel_counts,cat,cmodel,cmodelerr, defaults=defaults

  is_structure = size(cat,/type)
  
  IF is_structure NE 8 THEN BEGIN  
      print,'-Syntax: make_cmodel_counts,cat,cmodel,cmodelerr, /defaults'

      cmodel = fltarr(5,1)
      cmodelerr = fltarr(5,1)
      RETURN
  ENDIF

  have_dev = tag_exist(cat,'counts_dev')
  have_exp = tag_exist(cat,'counts_exp')
  have_deverr = tag_exist(cat,'counts_deverr')
  have_experr = tag_exist(cat,'counts_experr')
  have_fracpsf = tag_exist(cat,'fracpsf')
  have_flags = tag_exist(cat,'flags')

  IF have_dev NE 1 OR have_exp NE 1 OR $
    have_fracpsf NE 1 OR have_flags NE 1 THEN BEGIN 
      print,'Need dev_counts, exp_counts, fracpsf, and flags.  Returning...'
      RETURN
  ENDIF

  IF have_experr AND have_deverr THEN BEGIN
      do_errors = 1
  ENDIF ELSE BEGIN
      do_errors = 0
  ENDELSE

  ncat = n_elements(cat)

  IF keyword_set(defaults) THEN BEGIN 
      ;; We wil only copy in those which pass these cuts
      wu = where(cat.counts_exp[0] GT 1 AND cat.counts_dev[0] GT 1, nwu)
      wg = where(cat.counts_exp[1] GT 1 AND cat.counts_dev[1] GT 1, nwg)
      wr = where(cat.counts_exp[2] GT 1 AND cat.counts_dev[2] GT 1, nwr)
      wi = where(cat.counts_exp[3] GT 1 AND cat.counts_dev[3] GT 1, nwi)
      wz = where(cat.counts_exp[4] GT 1 AND cat.counts_dev[4] GT 1, nwz)

      cmodel = replicate(-9999., 5, ncat)
      IF do_errors THEN cmodelerr = cmodel
  ENDIF ELSE BEGIN 

      wu = lindgen(ncat)
      wg = wu
      wr = wu
      wi = wu
      wz = wu

      cmodel = fltarr(5,ncat)
      IF do_errors THEN cmodelerr = cmodel

  ENDELSE 

  uexp = cat.counts_exp(0) > 1.0 < 28.0
  gexp = cat.counts_exp(1) > 1.0 < 28.0
  rexp = cat.counts_exp(2) > 1.0 < 28.0
  iexp = cat.counts_exp(3) > 1.0 < 28.0
  zexp = cat.counts_exp(4) > 1.0 < 28.0

  udev = cat.counts_dev(0) > 1.0 < 28.0
  gdev = cat.counts_dev(1) > 1.0 < 28.0
  rdev = cat.counts_dev(2) > 1.0 < 28.0
  idev = cat.counts_dev(3) > 1.0 < 28.0
  zdev = cat.counts_dev(4) > 1.0 < 28.0

  IF do_errors THEN BEGIN
      uexperr = cat.counts_experr(0) > 0.0 < 5.0
      gexperr = cat.counts_experr(1) > 0.0 < 5.0
      rexperr = cat.counts_experr(2) > 0.0 < 5.0
      iexperr = cat.counts_experr(3) > 0.0 < 5.0
      zexperr = cat.counts_experr(4) > 0.0 < 5.0
      
      udeverr = cat.counts_deverr(0) > 0.0 < 5.0
      gdeverr = cat.counts_deverr(1) > 0.0 < 5.0
      rdeverr = cat.counts_deverr(2) > 0.0 < 5.0
      ideverr = cat.counts_deverr(3) > 0.0 < 5.0
      zdeverr = cat.counts_deverr(4) > 0.0 < 5.0
  ENDIF

  ufrac = cat.fracpsf(0) > 0.0 < 1.0 
  gfrac = cat.fracpsf(1) > 0.0 < 1.0 
  rfrac = cat.fracpsf(2) > 0.0 < 1.0 
  ifrac = cat.fracpsf(3) > 0.0 < 1.0 
  zfrac = cat.fracpsf(4) > 0.0 < 1.0 

  IF do_errors THEN BEGIN 
      lup2flux,uexp,fuexp,uexperr,fuexperr,b=0
      lup2flux,gexp,fgexp,gexperr,fgexperr,b=1
      lup2flux,rexp,frexp,rexperr,frexperr,b=2
      lup2flux,iexp,fiexp,iexperr,fiexperr,b=3
      lup2flux,zexp,fzexp,zexperr,fzexperr,b=4
      
      lup2flux,udev,fudev,udeverr,fudeverr,b=0
      lup2flux,gdev,fgdev,gdeverr,fgdeverr,b=1
      lup2flux,rdev,frdev,rdeverr,frdeverr,b=2
      lup2flux,idev,fidev,ideverr,fideverr,b=3
      lup2flux,zdev,fzdev,zdeverr,fzdeverr,b=4
  ENDIF ELSE BEGIN
      lup2flux,uexp,fuexp,b=0
      lup2flux,gexp,fgexp,b=1
      lup2flux,rexp,frexp,b=2
      lup2flux,iexp,fiexp,b=3
      lup2flux,zexp,fzexp,b=4
      
      lup2flux,udev,fudev,b=0
      lup2flux,gdev,fgdev,b=1
      lup2flux,rdev,frdev,b=2
      lup2flux,idev,fidev,b=3
      lup2flux,zdev,fzdev,b=4
  ENDELSE

  fuexp = ufrac*fudev + (1.0-ufrac)*fuexp
  fgexp = gfrac*fgdev + (1.0-gfrac)*fgexp
  frexp = rfrac*frdev + (1.0-rfrac)*frexp
  fiexp = ifrac*fidev + (1.0-ifrac)*fiexp
  fzexp = zfrac*fzdev + (1.0-zfrac)*fzexp

  IF do_errors THEN BEGIN
      fuexperr = ufrac*fudeverr + (1.0-ufrac)*fuexperr
      fgexperr = gfrac*fgdeverr + (1.0-gfrac)*fgexperr
      frexperr = rfrac*frdeverr + (1.0-rfrac)*frexperr
      fiexperr = ifrac*fideverr + (1.0-ifrac)*fiexperr
      fzexperr = zfrac*fzdeverr + (1.0-zfrac)*fzexperr

      flux2lup,fuexp,ucmod,fuexperr,ucmoderr,b=0
      flux2lup,fgexp,gcmod,fgexperr,gcmoderr,b=1
      flux2lup,frexp,rcmod,frexperr,rcmoderr,b=2
      flux2lup,fiexp,icmod,fiexperr,icmoderr,b=3
      flux2lup,fzexp,zcmod,fzexperr,zcmoderr,b=4
  ENDIF ELSE BEGIN
      flux2lup,fuexp,ucmod,b=0
      flux2lup,fgexp,gcmod,b=1
      flux2lup,frexp,rcmod,b=2
      flux2lup,fiexp,icmod,b=3
      flux2lup,fzexp,zcmod,b=4
  ENDELSE

  cmodel[0,wu] = ucmod[wu]
  cmodel[1,wg] = gcmod[wg]
  cmodel[2,wr] = rcmod[wr]
  cmodel[3,wi] = icmod[wi]
  cmodel[4,wz] = zcmod[wz]
  
  IF do_errors THEN BEGIN
      cmodelerr[0,wu] = ucmoderr[wu]
      cmodelerr[1,wg] = gcmoderr[wg]
      cmodelerr[2,wr] = rcmoderr[wr]
      cmodelerr[3,wi] = icmoderr[wi]
      cmodelerr[4,wz] = zcmoderr[wz]
  ENDIF
 
  RETURN

END
