;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;+
; NAME:
;    PRINT_FLAGS
;
; PURPOSE:
;    Prints the bits which are set in the entered SDSS bitmask. 
;    The user can enter the flags or a structure with containing the
;    flags.
;
; CALLING SEQUENCE:
;  print_flags, flags'
;          OR   
;  print_flags, struct [, colorindex], /objc_flags'
;
; Inputs: 
;    flags:  The bitmask for a single object.
;      OR
;    struct:  A structure that contains sdss flags.  If /objc_flags is set,
;             the struct must contain OBJC_FLAGS, OBJC_FLAGS2 to get useful
;             output.  Otherwise, the user must also enter the index of the 
;             SDSS bandpass and the struct must contain FLAGS or FLAGS2
;
; Author:  Tim McKay
; Date: 1/7/99
; Erin Scott Sheldon  UM  2/5/00 added flags2, made modular.
; Erin Sheldon - Allow user to enter either the flags or a structure.
;
;-
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

PRO print_flags_flag_info, fnames, fnames2, fvals, fvals2

  fnames = ['CANONICAL_CENTER', $
          'BRIGHT', $
          'EDGE', $
          'BLENDED ', $
          'CHILD', $
          'PEAKCENTER', $
          'NODEBLEND', $
          'NOPROFILE', $
          'NOPETRO', $
          'MANYPETRO', $
          'NOPETRO_BIG', $
          'DEBLEND_TOO_MANY_PEAKS', $
          'CR', $
          'MANYR50', $
          'MANYR90', $
          'BAD_RADIAL', $
          'INCOMPLETE_PROFILE', $
          'INTERP', $
          'SATUR', $
          'NOTCHECKED', $
          'SUBTRACTED', $
          'NOSTOKES', $
          'BADSKY', $
          'PETROFAINT ', $
          'TOO_LARGE', $
          'DEBLENDED_AS_PSF', $
          'DEBLEND_PRUNED', $
          'ELLIPFAINT', $
          'BINNED1', $
          'BINNED2', $
          'BINNED4 ', $
          'MOVED']

  fnames2 =  ['DEBLENDED_AS_MOVING', $ 
           'NODEBLEND_MOVING', $ 
           'TOO_FEW_DETECTIONS', $ 
           'BAD_MOVING_FIT', $
           'STATIONARY', $
           'PEAKS_TOO_CLOSE', $
           'MEDIAN_CENTRE', $
           'LOCAL_EDGE', $ 
           'BAD_COUNTS_ERROR', $ 
           'BAD_MOVING_FIT_CHILD', $ 
           'DEBLEND_UNASSIGNED_FLUX', $ 
           'SATUR_CENTER', $ 
           'INTERP_CENTER', $
           'DEBLENDED_AT_EDGE', $
           'DEBLEND_NOPEAK', $
           'PSF_FLUX_INTERP', $
           'TOO_FEW_GOOD_DETECTIONS', $
           'MEASURED', $ 
           'GROWN_MERGED', $ 
           'HAS_CENTER', $
           'MEASURE_BRIGHT']

  fvals = 2L^lindgen(n_elements(fnames))
  fvals2 = 2L^lindgen(n_elements(fnames2))  

END 

PRO print_flags_checkflags, flags, fnames, fvals, type=type

  nf = n_elements(fnames)
  IF n_elements(type) EQ 0 THEN type = 'flags' 
  print
  print,type+' = ',ntostr(flags)
  print,'------------------'
  FOR j=0, nf-1 DO BEGIN 
      if ((flags and fvals[j]) ne 0) then begin
          print,fnames[j]+'  ',ntostr(j)
      endif
  ENDFOR  

END 

PRO print_flags_struct, pstruct, cindex, objc_flags=objc_flags

  tags = tag_names(pstruct)
  print_flags_flag_info, fnames, fnames2, fvals, fvals2

  if keyword_set(objc_flags) then BEGIN

      wf=where(tags EQ 'OBJC_FLAGS', nwf)
      wf2=where(tags EQ 'OBJC_FLAGS2', nwf2)

      IF nwf EQ 0 THEN BEGIN 
          print,'OBJC_FLAGS tag is undefined for input structure'
      ENDIF ELSE BEGIN 
          f = long(pstruct[0].objc_flags)
          print_flags_checkflags, f, fnames, fvals, type='OBJC_FLAGS'
      ENDELSE 


      IF nwf2 EQ 0 THEN BEGIN 
          print,'OBJC_FLAGS2 tag is undefined for input structure'
          doflags2 = 0
      ENDIF ELSE BEGIN 
          f = long(pstruct[0].objc_flags2)
          print_flags_checkflags, f, fnames2, fvals2, type='OBJC_FLAGS2'
      ENDELSE 

  ENDIF ELSE BEGIN 

      wf=where(tags EQ 'FLAGS', nwf)
      wf2=where(tags EQ 'FLAGS2', nwf2)

      IF nwf EQ 0 THEN BEGIN  
          print,'FLAGS tag is undefined for input structure'
      ENDIF ELSE BEGIN 
          f = long(pstruct[0].objc_flags[cindex])
          print_flags_checkflags, f, fnames, fvals, type='FLAGS'
      ENDELSE 

      IF nwf2 EQ 0 THEN BEGIN 
          print,'FLAGS2 tag is undefined for input structure'
          doflags2 = 0 
      ENDIF ELSE BEGIN 
          f = long(pstruct[0].objc_flags2[cindex])
          print_flags_checkflags, f, fnames2, fvals2, type='FLAGS2'
      ENDELSE 

  ENDELSE 

END 

PRO print_flags_syntax
  print,'-Syntax: print_flags, flags'
  print,'   OR   '
  print,'-Syntax print_flags, pstruct [, colorindex], /objc_flags'
  return
END 


pro print_flags, flags, cindex, objc_flags=objc_flags

  on_error, 2
  np = n_params()

  IF np EQ 0 THEN BEGIN 
      print_flags_syntax & return
  ENDIF 


  tn = size(flags, /tname)
  IF np EQ 1 THEN BEGIN 
      ;; user entered the flags
      IF tn NE 'STRUCT' THEN BEGIN 
          print_flags_flag_info, fnames, fnames2, fvals, fvals2
          print_flags_checkflags, flags, fnames, fvals, type=type
          return
      ENDIF 

      ;; user entered a struct but not cindex: look for /objc_flags
      IF tn EQ 'STRUCT' THEN BEGIN 
          IF NOT keyword_set(objc_flags) THEN BEGIN 
              print,'you must enter a color index unless /objc_flags is set'
              print_flags_syntax & return
          ENDIF 
          print_flags_struct, flags, objc_flags=objc_flags
          return
      ENDIF 
  ENDIF 

  IF np EQ 2 THEN BEGIN 
      ;; the second parameter is useless
      IF tn NE 'STRUCT' THEN BEGIN 
          print_flags_flag_info, fnames, fnames2, fvals, fvals2
          print_flags_checkflags, flags, fnames, fvals, type=type
          return
      ENDIF 
      ;; it will be used
      print_flags_struct, flags, objc_flags=objc_flags
      return
  ENDIF 

return

end
