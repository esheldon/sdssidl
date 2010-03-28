;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;+
;
; NAME:
;    PLOT_TSFLAGS
; PURPOSE:
;    Plots target selection flags for a set of objects 
; 
; Inputs:  pstruct: a photo output structure (must have .flags tag...)
;	   index: indices of the object of interest
;
; Outputs: Plots flags for these objects
;
; Author:  Sarah Monk, modified from plot_flags by Tim McKay
; Date: 5/8/00
;
;-
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

pro plot_tsflags, pstruct, index, log=log

  on_error, 2

  if n_params() LT 2 then begin
      print,'-syntax plot_flags, pstruct, index, log=log'
      return
  endif

  pold=!p.multi
  !p.multi=[0,1,2]

  numflags = 27
  numflags2 = 10

  tags=tag_names(pstruct)
  w1=where(tags EQ 'PRIMTARGET', nw1)
  w2=where(tags EQ 'SECTARGET', nw2)

  IF NOT keyword_set(objc) THEN BEGIN
      f=long(pstruct(index).primtarget)
      IF nw2 EQ 0 THEN BEGIN
          doflags2 = 0
          !p.multi=pold
      ENDIF ELSE BEGIN 
          doflags2 = 1
          f2=long(pstruct(index).sectarget)
      ENDELSE 
  ENDIF 

  help,f
  hist=lonarr(numflags)
  
  for j=0,numflags-1 do begin
      h=long(2L^j)
      k=where((f and h) ne 0)
      s=size(k)
      if (s(0) eq 1) then hist(j)=s(1)
  endfor

  xt1='primtarget'

  if keyword_set(log) then begin
      plot,hist,psym=10,/ylog,yrange=[0.1,10000],xtitle=xt1
  endif else begin
      plot,hist,psym=10,xtitle=xt1
  endelse	

  IF doflags2 THEN BEGIN 

      help,f2
      hist2=lonarr(numflags2)

      FOR j=0, numflags2-1 DO BEGIN
          h=long(2L^j)
          k=where((f2 and h) ne 0)
          s=size(k)
          if (s(0) eq 1) then hist2(j)=s(1)
      ENDFOR 

      xt2='sectarget'

      if keyword_set(log) then begin
          plot,hist2,psym=10,/ylog,yrange=[0.1,10000],xtitle=xt2
      endif else begin
          plot,hist2,psym=10,xtitle=xt2
      endelse	
      
  ENDIF 

  !p.multi=pold

return

end
