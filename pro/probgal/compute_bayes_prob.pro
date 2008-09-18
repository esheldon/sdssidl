;+
; NAME:
;    COMPUTE_BAYES_PROB
;
;
; PURPOSE:
;    Compute Bayesian probability for SG separation cat. Must have psfcounts,
;    counts_exp, seeing (from adaptive moments).  Outputs the galaxy
;    probability and fills in objc_prob_psf with 1-probgal if the tag exists.
;    Fills in objc_prob_flags if it exists.
;
; CATEGORY:
;    SDSS specific routine
;
;
; CALLING SEQUENCE:
;    compute_bayes_prob, cat, probgal, probflags
;
;
; INPUTS:
;    cat: PHOTO structure containing tags psfcounts,
;         counts_exp, seeing (from adaptive moments)
;
;
; OPTIONAL INPUTS:
;    NONE
;
;
; KEYWORD PARAMETERS:
;    /byfield: split catalog by field
;    /noiter: don't iterate on the number counts
;
;
; OUTPUTS:
;   probgal: Probability of being a galaxy: default is 10000. (so objc_prob_psf
;            will be -9999. by default)
;   probflags: diagnostic flags
;
;
; OPTIONAL OUTPUTS:
;   NONE
;
;
; COMMON BLOCKS:
;   NONE
;
;
; SIDE EFFECTS:
;   cat.objc_prob_psf and cat.objc_prob_flags are filled in if they exist
;
;
; RESTRICTIONS:
;   If the structure already contains cmodel_counts, then it needs thest tags:
;
;     CMODEL_COUNTS, CMODEL_COUNTSERR, PSFCOUNTS, PSFCOUNTSERR, M_RR_CC_PSF,
;       FLAGS, FLAGS2
;   
;   Otherwise, cmodel_counts will be computed, in which case these tags are
;   required:
;
;     COUNTS_DEV, COUNTS_DEVERR, COUNTS_EXP, COUNTS_EXPERR, FRACPSF
;
;   If /byfield, then need RUN, RERUN, CAMCOL, FIELD, ID
;
; PROCEDURE:
;
;
;
; EXAMPLE:
;
;
;
; MODIFICATION HISTORY:
;    Creation:  Version 1.0 ??-??-?? Dave Johnston, UofChicago
;               Version 1.2 29-April-2003 DJ
;               Version 1.5 ??-??-2004 Ryan Scranton, Pitt
;
;-

PRO compute_bayes_prob,cat,probgal,probflags,byfield=byfield,noiter=noiter,$
                       combined=combined

  IF n_params() EQ 0 THEN BEGIN
      print,'-syntax compute_bayes_prob, cat, probgal, probflags, /byfield, /noiter'
      print,' version 1.5'
      RETURN
  ENDIF

  COMMON bayes_prob_block, PROB_PSF_DEFAULT, MAXMAG, MINMAG, MINSEE, MAXSEE, $
    PROBFLAG_NOITER_REQUESTED, PROBFLAG_NOITER, PROBFLAG_NOPROB, $
    PROBFLAG_NOMEAS, $
    PROBFLAG_MAGBOUNDS, PROBFLAG_SEEINGBOUNDS, $
    PROBFLAG_NEGCON,$
    SEE_A, SEE_OFF, MAG_A, MAG_OFF, N_SEEBINS, N_MAGBINS

  IF n_elements(PROB_PSF_DEFAULT) EQ 0 THEN bayes_set_common

  ;; set up output arrays
  ncat = n_elements(cat)
  probgal = replicate(1. - PROB_PSF_DEFAULT, ncat)
  IF n_elements(probflags) NE ncat THEN BEGIN
      probflags = replicate(0L, ncat)
      probflags[*] = 0
  ENDIF

  IF keyword_set(combined) AND keyword_set(byfield) THEN BEGIN
      print,"Cannot use 'combined' and 'byfield' together.  Returning..."
      RETURN
  ENDIF

  ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
  ;; check that the catalog has the correct tags
  ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

  IF keyword_set(combined) EQ 0 THEN BEGIN
      tags = tag_names(cat[0])
      
      IF tag_exist(cat,'CMODEL_COUNTS') THEN BEGIN
          required_tags =  ['PSFCOUNTS','PSFCOUNTSERR','CMODEL_COUNTS',$
                            'CMODEL_COUNTSERR','M_RR_CC_PSF','FLAGS', 'FLAGS2']

      ENDIF ELSE BEGIN
          required_tags =  ['PSFCOUNTS','PSFCOUNTSERR','COUNTS_DEV',$
                            'COUNTS_DEVERR','COUNTS_EXP','COUNTS_EXPERR',$
                            'FRACPSF','M_RR_CC_PSF','FLAGS', 'FLAGS2']
      ENDELSE
      

      IF keyword_set(byfield) THEN BEGIN
          required_tags = [required_tags,'RUN','RERUN','CAMCOL','FIELD','ID']
      ENDIF

      nreq = n_elements(required_tags)
      match, tags, required_tags, mt, mrt
      IF n_elements(mrt) NE nreq THEN BEGIN 
          nt = lindgen(nreq)
          IF mrt[0] NE -1 THEN BEGIN 
              remove, mrt, nt
          ENDIF 
          print,'Missing tags: '
          colprint,required_tags[nt]
          RETURN
      ENDIF 
  ENDIF

  ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
  ;; Calculate initial gaussian positions based
  ;; on magnitude/seeing
  ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

  bayes_prob,cat,probflags,probgal,rat,ls,lg,c,see,mag,combined=combined

  ;;;;;;;;;;;;;;;;;;;;;;;;;;
  ;; The good ones
  ;;;;;;;;;;;;;;;;;;;;;;;;;;

  w=where((probflags AND PROBFLAG_NOPROB) EQ 0, nw,comp=comp,ncomp=ncomp)
  IF (nw EQ 0) OR keyword_set(noiter) THEN RETURN

  ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
  ;; Iterate on number counts if there are enough objects
  ;; This sets relative height of gaussians
  ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

  IF nw GE 10 THEN BEGIN 
      
      IF keyword_set(byfield) THEN BEGIN
          ;; field-by-field basis
          field_indices,cat[w].run,cat[w].rerun,cat[w].camcol,cat[w].field,$
            nf,rev,index,str,h
          
          FOR i=0L, nf-1 DO BEGIN
              IF rev[i] NE rev[i+1] THEN BEGIN
                  wf=rev[rev[i]:rev[i+1]-1]
                                ;recursive call
                  compute_bayes_prob,cat[w[wf]],prob_byfield,probflags_byfield
                  probgal[w[wf]]   = prob_byfield
                  probflags[w[wf]] = probflags_byfield
              ENDIF
          ENDFOR
      ENDIF ELSE BEGIN
          ;; Do all objects at once
          bayes_prob_nc, probgal[w], mag[w], prob2
          probgal[w] = prob2
      ENDELSE 
      
  ENDIF ELSE BEGIN 
      
      print,'warning --too few to iterate number counts'
      probflags = probflags + PROBFLAG_NOITER
      
  ENDELSE 

  RETURN

END
