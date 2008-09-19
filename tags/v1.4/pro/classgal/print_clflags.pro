;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;+
;
; NAME:  PRINT_CLFLAG
;    
;       
; PURPOSE:  
;    Prints classification flags for a single object
;    
;
; CALLING SEQUENCE:
;    print_clflags,pstruct,index
;    
;
; INPUTS: 
;    pstruct: a photo output structure (must have .classification tag)
;    index: index of object of interest
;
;
; REVISION HISTORY:  Judith Racusin 6/15/00, modified from print_flags by Tim Mckay
;    
;       
;                                      
;-                                       
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

PRO print_clflags,pstruct,index

  on_error, 2

  IF n_params() LT 2 THEN BEGIN
      print,'Syntax: print_clflags,pstruct,index'
      return
  END

  IF n_elements(index) GT 1 THEN BEGIN 
      print,'Can only do one object at a time'
      return
  ENDIF 

  numflags=12

  tags=tag_names(pstruct)
  w1=where(tags EQ 'CLASSIFICATION', nw1)

  fstr = ['ELLIP', $
          'ELLIP_LIKELY', $
          'SPIRAL_LIKELY', $
          'SPIRAL', $ 
          'FAILED', $
          'LENS_GALAXY_G', $
          'LENS_GALAXY_R', $
          'LENS_GALAXY_I', $
          'BRG', $
          'NOT_A_GALAXY', $
          'BAD_ASYMMETRY',$
          'BAD_CONCENTRATION']

  IF (n_elements(pstruct[0].classification) EQ 5) THEN n=2 ELSE n=0
  f=pstruct(index).classification[n]

  print
  print,'Galaxy Classification'+' = ',ntostr(f)
  print,'---------------------------'
  for j=0,numflags-1 do begin
      h=2^j
      if ((f and h) ne 0) then begin
          print,'  ',fstr[j]+'  ',ntostr(j)
      endif
  endfor

return
END 
