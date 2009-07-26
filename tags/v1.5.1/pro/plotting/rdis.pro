
pro rdis, image, pls, $
          xmn=xmn, xmx=xmx, ymn=ymn, ymx=ymx, $
          low=low, high=high, $
          log=log, zoom=zoom, full=full, $
          flipx=flipx, flipy=flipy, rot=rot, $
          silent=silent, $
          sigmahigh=sigmahigh, sigmalow=sigmalow,$
          _extra=extra

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;+
; NAME:
;    RDIS
;
; PURPOSE:
;    Wrapper for tvim2.  One can zoom, rescale, flip across x or y, or 
;    rotate by 180 degrees.
;
; CALLING SEQUENCE:
;       imdis, image [, pls, 
;          sigmahigh=sigmahigh, sigmalow=sigmalow,$
;          xmn=xmn, xmx=xmx, ymn=ymn, ymx=ymx, $
;          low=low, high=high, $
;          log=log, zoom=zoom, full=full, $
;          flipx=flipx, flipy=flipy, rot=rot, $
;          silent=silent, _extra=extra]
;
; INPUTS:
;       image: image to be displayed
;	
; OPTIONAL INPUTS:
;       pls:  the plot structure.  If not input, rdis_setup is called.
;             You can call rdis_setup yourself and set the equilavent of
;             the keywords in the structure:
;                         xmn, xmx, ymn, ymx, low, high
;
; INPUT KEYWORD PARAMETERS:
;    THESE OVERIDE PLS
;    xmn, xmx, ymn, ymx:  range of x and y values of image to display.
;                         these can be set in pls.
;    low, high: low and high of image.  
;    full: show the full image.
;    rot: rotate image
;    silent: be quiet.
;
; PROCEDURE:
;	
;
; REVISION HISTORY:
;	Tim McKay	UM	1/29/97
;       Erin Scott Sheldon made pls optional input 
;                          Cleaned up, added comments.  11/24/99  
;-
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

  On_error,2                    ;Return to caller

  IF N_params() EQ 0 THEN BEGIN 
      print,'-Syntax: rdis, image, [pls, '
      print,'     sigmalow=sigmalow, sigmahigh=sigmahigh,$'
      print,'     xmn=xmn, xmx=xmx, ymn=ymn, ymx=ymx, $'
      print,'     low=low, high=high, $'
      print,'     log=log, zoom=zoom, full=full, $'
      print,'     flipx=flipx, flipy=flipy, rot=rot, $'
      print,'     silent=silent, _extra=extra]'
      print
      print,'pls is now optional If not given, it will'
      print,'be found by rdis.pro'
      print,'For more help use doc_library,"rdis"'
      return
  ENDIF 

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
; Check parameters
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

  IF NOT keyword_set(silent) THEN silent = 0
  IF NOT keyword_set(full) THEN full = 0
  IF NOT keyword_set(flipx) THEN flipx = 0
  IF NOT keyword_set(flipy) THEN flipy = 0
  IF NOT keyword_set(rot) THEN rot = 0
  IF n_elements(zoom) NE 0 THEN BEGIN
      IF zoom EQ 0 THEN dozoom = 0 ELSE dozoom = 1
  ENDIF ELSE dozoom = 0
  
  IF n_elements(sigmalow) EQ 0 THEN sigmalow = 1. ; Default level below mean 
  IF n_elements(sigmahigh) EQ 0 THEN sigmahigh = 5. ; Default level above mean

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
; Set up the Plot Structure.
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
  
  IF n_elements(pls) EQ 0 THEN rdis_setup, image, pls

  ;; See if Plot Structure is set up properly.  If not then fix it.

  IF (pls.xmn EQ 0 AND pls.xmx EQ 0) THEN BEGIN 
	r=size(image)
	xmin=0
	xmax=r(1)-1
  ENDIF ELSE BEGIN 
	xmin=pls.xmn
	xmax=pls.xmx
  ENDELSE 
  
  IF (pls.ymn EQ 0 AND pls.ymx EQ 0) THEN BEGIN 
	r=size(image)
	ymin=0
	ymax=r(2)-1
  ENDIF ELSE BEGIN 
	ymin=pls.ymn
	ymax=pls.ymx
  ENDELSE 

  ;; case of log axis
  IF keyword_set(log) THEN BEGIN  
	imin=alog10(abs(image)) 
  ENDIF ELSE BEGIN 
	imin=image
  ENDELSE 
  
  ;; keywords supercede the plot structure
  IF n_elements(xmn) NE 0 THEN xmin=xmn
  IF n_elements(xmx) NE 0 THEN xmax=xmx
  IF n_elements(ymn) NE 0 THEN ymin=ymn
  IF n_elements(ymx) NE 0 THEN ymax=ymx

  ;; Check if high,low are set.  If not, calculate mean and sigma.
  IF (pls.low eq pls.high) THEN BEGIN 
      IF (n_elements(low) EQ 0) OR (n_elements(high) EQ 0) THEN BEGIN 
          sigma_clip, imin(xmin:xmax,ymin:ymax), mean, sigma, $
            niter=2.0, nsig=3.5,/silent
          
      ENDIF 
      IF n_elements(low) NE 0 THEN rlow=low ELSE BEGIN
          rlow = mean - sigmalow*sigma
          low = rlow
          pls.low = low
      ENDELSE 
      IF n_elements(high) NE 0 THEN rhigh=high ELSE BEGIN
          rhigh = mean + sigmahigh*sigma
          high = rhigh
          pls.high = high
      ENDELSE 
  ENDIF ELSE BEGIN 
      rlow=pls.low
      rhigh=pls.high
  ENDELSE 


  ;; Zooming.  
  IF dozoom AND (NOT full) THEN BEGIN 
      IF zoom EQ 1 THEN BEGIN 
          cursor, x, y, /data, /up 
          cursor, x1, y1, /data, /up
          xmax = max( [x, x1], min=xmin)
          ymax = max( [y, y1], min=ymin)
          pls.xmx = xmax & pls.xmn = xmin
          pls.ymx = ymax & pls.ymn = ymin

      ENDIF ELSE BEGIN    
          info=size(image)
          c1 = (pls.xmx + pls.xmn)/2
          c2 = (pls.ymx + pls.ymn)/2
          xmax = c1 + ( zoom*(pls.xmx-c1) )	
          xmin = c1 - ( zoom*(c1-pls.xmn) )	
          ymax = c2 + ( zoom*(pls.ymx-c2) )	
          ymin = c2 - ( zoom*(c2-pls.ymn) )	
	
          IF (xmin LE 0) THEN xmin=0
          IF (ymin LE 0) THEN ymin=0
          IF (xmax GE info(1)) THEN xmax=info(1)-1
          IF (ymax GE info(2)) THEN ymax=info(2)-1

          pls.xmn = xmin
          pls.xmx = xmax
          pls.ymn = ymin
          pls.ymx = ymax  
      ENDELSE 
  ENDIF 

  ;; The full image
  IF keyword_set(full) THEN BEGIN 
      rdis_setup, image, pls
      r=size(image)
      xmin=0
      xmax=r(1)-1
      ymin=0
      ymax=r(2)-1
  ENDIF 

  ;; Flip across the x-direction
  IF keyword_set(flipx) THEN BEGIN 
      IF pls.xmn EQ 0 THEN BEGIN 
          imin=reverse(image)		
      ENDIF ELSE BEGIN 
          r=size(image)
          imin=reverse(image)
          xmax=r(1)-pls.xmn
          xmin=r(1)-pls.xmx 
      ENDELSE 
  ENDIF 

  ;; Flip across the y-direction
  IF keyword_set(flipy) THEN BEGIN 
      IF pls.ymn EQ 0 THEN BEGIN 
          imin=reverse(image, 2)		
      ENDIF ELSE BEGIN 
          r=size(image)
          imin=reverse(image,2)
          ymax=r(2)-pls.ymn
          ymin=r(2)-pls.ymx
      ENDELSE 
  ENDIF 

  ;; Rotate 180 degrees
  IF keyword_set(rot) THEN BEGIN 
      IF pls.ymn EQ 0 THEN BEGIN 
          imin=rotate(image, 2)
      ENDIF ELSE BEGIN 
          r=size(image)
          imin=rotate(image,2)
          xmax=r(1)-pls.xmn
          xmin=r(1)-pls.xmx 
          ymax=r(2)-pls.ymn
          ymin=r(2)-pls.ymx
      ENDELSE 
  ENDIF 

  IF NOT silent THEN BEGIN 
      rstr = ntostr( long(xmin) )+':'+ntostr( long(xmax) )
      rstr = rstr + ', '+ntostr( long(ymin) )+':'+ntostr(long( ymax) )
      mes = ' Displaying im('+rstr+')'
      mes = mes + '   range = ['+ntostr(rlow)+', '+ntostr(rhigh)+']'
      print,mes
  ENDIF 

  ;; Plot the image.
  tvim2_scl, imin, xmin, xmax, ymin, ymax, range=[rlow, rhigh], _extra=extra

  return
  end



