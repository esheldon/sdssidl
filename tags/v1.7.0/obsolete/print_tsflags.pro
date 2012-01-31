;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;+
; NAME:
;    PRINT_TSFLAGS
;
; PURPOSE:
;    Prints target selection flags for a single object
;
; Inputs:  pstruct: a photo output structure (must have .flags tag...)
;	   index: index of the object of interest
;
; Outputs: Prints target selection flags status
;
; Author:  Sarah Monk, modified from print_flags by T. McKay
; Date: 5/8/00
; 
;
;-
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

pro print_tsflags, pstruct, index 

  on_error, 2

  if n_params() LT 2 then begin
      print,'-syntax print_tsflags, pstruct, index'
      print,' Only for use with a single object'
      return
  endif

  IF n_elements(index) GT 1 THEN BEGIN 
      print,'Can only do one object at a time'
      return
  ENDIF 

  numflags=27
  numflags2=10

  tags=tag_names(pstruct)
  w1=where(tags EQ 'PRIMTARGET', nw1)
  w2=where(tags EQ 'SECTARGET', nw2)


  fstr = ['QSO_HIZ', $
          'QSO_CAP', $
          'QSO_SKIRT', $
          'QSO_FIRST_CAP', $
          'QSO_FIRST_SKIRT', $
          'GALAXY_RED', $
          'GALAXY', $
          'GALAXY_BIG', $
          'GALAXY_BRIGHT_CORE', $
          'ROSAT_A', $
          'ROSAT_B', $
          'ROSAT_C', $
          'ROSAT_D', $
          'STAR_BHB', $
          'STAR_CARBON', $
          'STAR_BROWN_DWARF', $
          'STAR_SUB_DWARF', $
          'STAR_CATY_VAR', $
          'STAR_RED_DWARF', $
          'STAR_WHITE_DWARF', $
          'SERENDIP_BLUE', $
	  'SERENDIP_FIRST', $
          'SERENDIP_RED', $
          'SERENDIP_DISTANT', $
          'SERENDIP_MANUAL', $
	  'QSO_FAINT', $
	  'GALAXY_RED_II']




  f2str=  ['LIGHT_TRAP', $ 
           'REDDEN_STD', $ 
           'TEST_TARGET', $ 
           'QA', $
           'SKY', $
           'SPECTROPHOTO_STD', $
           'GUIDE_STAR', $
           'BUNDLE_HOLE', $ 
           'QUALITY_HOLE', $ 
           'HOT_STD']






    

      f=long(pstruct(index).primtarget)
      fstr = 'AR_TARGET_'+fstr
      IF nw1 NE 0 THEN BEGIN 
          f2str = 'TAR_TARGET_'+f2str
          f2=long(pstruct(index).sectarget)
          doflags2 = 1
      ENDIF ELSE doflags2=0
 

  ;; primtarget

  print
  print,'primtarget'+' = ',ntostr(f)
  print,'------------------'
  for j=0,numflags-1 do begin
      h=long(2L^j)
      if ((f and h) ne 0) then begin
          print,fstr[j]+'  ',ntostr(j)
      endif
  endfor

  ;; sectarget

  IF doflags2 THEN BEGIN 
      print
      print,'sectarget'+' = ',ntostr(f2)
      print,'------------------'
      FOR j=0, numflags2-1 DO BEGIN 
          h=long(2L^j)
          if ((f2 and h) ne 0) then begin
              print,f2str[j]+'  ',ntostr(j)
          endif
      ENDFOR
  ENDIF 


return

end
