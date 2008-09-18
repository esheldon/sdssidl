;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;+
;
; NAME:  PLOT_CLFLAG
;    
;       
; PURPOSE:  
;    Plots classification flags for a set of objects.
;    
;
; CALLING SEQUENCE:
;    plot_clflags,pstruct,index,log=log
;    
;
; INPUTS: 
;    pstruct: a photo output structure (must have .classification tag)
;
;
; REVISION HISTORY:  Judith Racusin 6/15/00, modified from plot_flags by Tim Mckay
;    
;       
;                                      
;-                                       
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

PRO plot_clflags,pstruct,index,log=log

  on_error, 2

  IF n_params() LT 1 THEN BEGIN
      print,'Syntax: plot_clflags, pstruct, index, log=log'
      return
  ENDIF

  numflags = 12

  tags=tag_names(pstruct)
  w1=where(tags EQ 'CLASSIFICATION', nw1)

  IF (n_params() EQ 1) THEN index=indgen(n_elements(pstruct))
  IF (n_elements(pstruct[0].classification) EQ 5) THEN n=2 ELSE n=0

  f=long(pstruct(index).classification[n])

  hist=intarr(numflags)
  
  FOR j=0,numflags-1 DO BEGIN
      h=2L^j
      k=where((f AND h) NE 0)
      s=size(k)
      IF ( s(0) EQ 1) THEN hist(j)=s(1)
  ENDFOR 

  xt1='Classification'

  if keyword_set(log) then begin
      plot,hist,psym=10,/ylog,yrange=[0.1,10000],xtitle=xt1,xrange=[0,numflags-1],xstyle=1
  ENDIF else begin
      plot,hist,psym=10,xtitle=xt1,xrange=[0,numflags-1],xstyle=1
  ENDELSE 

return

END
