pro plot_flags, pstruct, index, color_index, log=log, objc=objc

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;+
;
; NAME:
;    PLOT_FLAGS
; PURPOSE:
;    Plots flags for a set of objects 
; 
; Inputs:  pstruct: a photo output structure (must have .flags tag...)
;	   index: indices of the objects of interest
;	   colorindex: which color do you want to look at???
;
; Outputs: Plots flags for these objects....
;
; Author:  Tim McKay
; Date: 1/7/99
; Erin Scott Sheldon UM 2/5/00 Added flags2
;
;-
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;Help message
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

  if n_params() LT 3 then begin
      print,'-syntax plot_flags, pstruct, index, color_index, log=log, objc=objc'
      return
  endif

  pold=!p.multi
  !p.multi=[0,1,2]

  numflags = 32
  numflags2 = 21

  tags=tag_names(pstruct)
  w1=where(tags EQ 'FLAGS2', nw1)
  w2=where(tags EQ 'OBJC_FLAGS2', nw2)

  IF NOT keyword_set(objc) THEN BEGIN
      f=long(pstruct(index).flags(color_index))
      IF nw2 EQ 0 THEN BEGIN
          doflags2 = 0
          !p.multi=pold
      ENDIF ELSE BEGIN 
          doflags2 = 1
          f2=long(pstruct(index).flags2(color_index))
      ENDELSE 
  ENDIF ELSE BEGIN 
      f=long(pstruct(index).objc_flags)
      IF nw1 EQ 0 THEN BEGIN
          doflags2 = 0
          !p.multi=pold
      ENDIF ELSE BEGIN 
          doflags2 = 1
          f2=long(pstruct(index).objc_flags2)
      ENDELSE 
  ENDELSE 

  help,f
  hist=lonarr(numflags)
  
  for j=0,numflags-1 do begin
      h=long(2L^j)
      k=where((f and h) ne 0)
      s=size(k)
      if (s(0) eq 1) then hist(j)=s(1)
  endfor

  xt1='flags'

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

      xt2='flags2'

      if keyword_set(log) then begin
          plot,hist2,psym=10,/ylog,yrange=[0.1,10000],xtitle=xt2
      endif else begin
          plot,hist2,psym=10,xtitle=xt2
      endelse	
      
  ENDIF 

  !p.multi=pold

return

end
