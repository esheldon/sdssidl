;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;+
; NAME: 
;    makegauss
;
; PURPOSE: 
;    make an array with values set to a gaussian. Assumes center of
;    pixel is (0,0), same as the adaptive moment code. Note, SDSS
;    convention is that bottom-left corner is (0,0)
;    Dave J.  My changes E.S.S.
;-
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

pro makegauss, gauss, image_size, sigma_in, $
               fwhm=fwhm, counts=counts, aratio=aratio, $
               theta=theta, cen=cen, gridfac=gridfac_in

  IF n_params() LT 2 THEN BEGIN 
    print,'-Syntax makegauss, gauss, image_size, sigma, fwhm=fwhm, counts=counts, aratio=aratio, theta=theta, cen=cen, gridfac=gridfac'
    print,'sigma is in wide direction if aratio < 1.0'
    print,'Theta in degrees.'
    return
  ENDIF

  ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
  ;; check some parameters
  ;; Will make copies of sigma and gridfac so they an be
  ;; modified.
  ;; FWHM takes precedence over sigma
  ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

  IF n_elements(fwhm) NE 0 THEN BEGIN
      sigma = 0.42466091*fwhm
  ENDIF ELSE IF n_elements(sigma_in) NE 0 THEN BEGIN 
      sigma = sigma_in
  ENDIF ELSE BEGIN
      print,'You must specify fwhm or sigma'
      return
  ENDELSE 

  IF n_elements(counts) EQ 0 THEN counts=1.   ELSE counts=counts
  IF n_elements(theta) EQ 0  THEN theta = 0.0 ELSE theta=theta
  
  sx=long(image_size[0])
  sy=long(image_size[1])
  IF n_elements(cen) EQ 0 THEN cen=[(sx-1.)/2.,(sy-1.)/2.]
  cx=cen[0]
  cy=cen[1]

  ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
  ;; Should we begin with a finer sampled image, then rebin
  ;; down to input size?  This is preferable when the sampling
  ;; is poor
  ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

  IF n_elements(gridfac_in) NE 0 THEN BEGIN 
      
      ;; Ensure the factor is a scalar and integer
      gridfac = long(gridfac_in[0])
      IF gridfac GT 1 THEN BEGIN 

          ;; Keep the old size around for rebinning
          sxIn = sx
          syIn = sy
          
          ;; New size
          sx = sx*gridfac
          sy = sy*gridfac
          
          ;; new center is same fraction of total size. The unit is (size-1)
          ;; because we are zero-offset
          
          cx_frac = cx/(sxIn-1.0)
          cy_frac = cy/(syIn-1.0)
          
          cx = cx_frac*(sx-1)
          cy = cy_frac*(sy-1)
          
          ;; new size
          sigma = sigma*gridfac
      ENDIF 

  ENDIF 

  ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
  ;; Compute the exponent of the gaussian at each point in
  ;; the image.  Use clever indexing technique
  ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

  index=lindgen(sx*sy)

  x=index MOD sx
  y=index/sx

  IF n_elements(aratio) EQ 0 THEN BEGIN 
    xp=x-cx   &  yp=y-cy
  ENDIF ELSE BEGIN 

    aratio=aratio > .1
    ct=cos(theta*!Pi/180.)
    st=sin(theta*!Pi/180.)
    xp=(x-cx)*ct + (y-cy)*st
    yp=((cx-x)*st + (y-cy)*ct)/aratio

  ENDELSE 

  ;; The exponent
  rr=(1.0/2.0/sigma^2)*(xp^2+yp^2)

  ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
  ;; Compute the gaussian
  ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

  gauss=fltarr(sx,sy)
  w=where(rr lt 10.8,cat)      ;watch floating point underflow
                                ;this will allow ~.00002 as smallest number
                                ;in disk

  IF cat GT 0 THEN gauss(index[w])=exp(-rr[w])
 
  ;; rebin if needed
  IF n_elements(gridfac) NE 0 THEN gauss = rebin(gauss, sxIn, syIn)

  ;; set the counts
  gauss = counts/total(gauss)*gauss

  return 
END






