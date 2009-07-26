;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;+
;
; NAME:
;    TSFLAG_SELECT
; PURPOSE:
;    Makes cuts based on target selection flags using input target flag
;    struct. These cuts are strictly "anded" 
;    together, so they must all be true for the object to survive.
; 
; CALLING SEQUENCE:
;    tsflag_select, pstruct, flag_struct, select_index
;
; Inputs:  pstruct: a photo output structure (must have primtarget, sectarget 
;          tags...)
;	   flag_struct: Premade flag structure. This will require any
;		flags set to 'Y' and insist that any flag set to 'N' be
;		off
;          input_index: you can input an index, from an earlier selection
;		for instance. If this has size(input_index)(0)=0 then
;		the returned selection index will be -1
;
; Outputs: select_index: indices of selected objects....
;          nkeep: number of objects that passed cuts
;
; Author:  Sarah Monk, modified from flag_select by Tim McKay
; Date: 5/8/00
; Added input_index, nkeep. cleaned up: 03-06-2003 E.S.S.
; Fixed poor memory usage. 11-Feb-2004 E.S.S.
;
;-
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;


pro tsflag_select, pstruct, flag_struct, select_index, nkeep, input_index=input_index

  on_error, 2

  if n_params() LT 3 then begin
      print,'-syntax tsflag_select, pstruct, tsflag_struct, select_index, nkeep, input_index=input_index'
      return
  endif

  IF n_elements(input_index) NE 0 THEN BEGIN 
      IF input_index[0] EQ -1 THEN BEGIN 
          select_index = -1
          nkeep = 0L
          return
      ENDIF 
      k = input_index
  ENDIF ELSE BEGIN 
      k=lindgen(n_elements(pstruct))
  ENDELSE 

  tags=tag_names(pstruct)
  w1=where(tags EQ 'PRIMTARGET', nw1)
  w2=where(tags EQ 'SECTARGET', nw2)

  IF nw1 EQ 0 THEN message,'PRIMTARGET undefined for input structure'

  f = long( pstruct.primtarget )

  IF nw2 EQ 0 THEN BEGIN
      doflags2 = 0
  ENDIF ELSE BEGIN 
      doflags2 = 1
      f2 = pstruct.sectarget
  ENDELSE 
 
  fs=flag_struct

  ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
  ;; Begin flags
  ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

  if (fs.QSO_HIZ eq 'Y') then begin
	h=where((f[k] and 2L^0) ne 0,nkeep)
	if (nkeep eq 0) then begin
	     select_index = -1
	     return
	endif
	k=k[h]
  endif 
  if (fs.QSO_HIZ eq 'N') then begin
	h=where((f[k] and 2L^0) eq 0,nkeep)
	if (nkeep eq 0) then begin
	     select_index = -1
	     return
	endif
	k=k[h]
  endif

  if (fs.QSO_CAP eq 'Y') then begin
	h=where((f[k] and 2L^1) ne 0,nkeep)
	if (nkeep eq 0) then begin
	     select_index = -1
	     return
	endif
	k=k[h]
  endif 
  if (fs.QSO_CAP eq 'N') then begin
	h=where((f[k] and 2L^1) eq 0,nkeep)
	if (nkeep eq 0) then begin
	     select_index = -1
	     return
	endif
	k=k[h]
  endif

  if (fs.QSO_SKIRT eq 'Y') then begin
	h=where((f[k] and 2L^2) ne 0,nkeep)
	if (nkeep eq 0) then begin
	     select_index = -1
	     return
	endif
	k=k[h]
  endif 
  if (fs.QSO_SKIRT eq 'N') then begin
	h=where((f[k] and 2L^2) eq 0,nkeep)
	if (nkeep eq 0) then begin
	     select_index = -1
	     return
	endif
	k=k[h]
  endif

  if (fs.QSO_FIRST_CAP eq 'Y') then begin
	h=where((f[k] and 2L^3) ne 0,nkeep)
	if (nkeep eq 0) then begin
	     select_index = -1
	     return
	endif
	k=k[h]
  endif 
  if (fs.QSO_FIRST_CAP eq 'N') then begin
	h=where((f[k] and 2L^3) eq 0,nkeep)
	if (nkeep eq 0) then begin
	     select_index = -1
	     return
	endif
	k=k[h]
  endif

  if (fs.QSO_FIRST_SKIRT eq 'Y') then begin
	h=where((f[k] and 2L^4) ne 0,nkeep)
	if (nkeep eq 0) then begin
	     select_index = -1
	     return
	endif
	k=k[h]
  endif 
  if (fs.QSO_FIRST_SKIRT eq 'N') then begin
	h=where((f[k] and 2L^4) eq 0,nkeep)
	if (nkeep eq 0) then begin
	     select_index = -1
	     return
	endif
	k=k[h]
  endif

  if (fs.GALAXY_RED eq 'Y') then begin
	h=where((f[k] and 2L^5) ne 0,nkeep)
	if (nkeep eq 0) then begin
	     select_index = -1
	     return
	endif
	k=k[h]
  endif 
  if (fs.GALAXY_RED eq 'N') then begin
	h=where((f[k] and 2L^5) eq 0,nkeep)
	if (nkeep eq 0) then begin
	     select_index = -1
	     return
	endif
	k=k[h]
  endif

  if (fs.GALAXY eq 'Y') then begin
	h=where((f[k] and 2L^6) ne 0,nkeep)
	if (nkeep eq 0) then begin
	     select_index = -1
	     return
	endif
	k=k[h]
  endif 
  if (fs.GALAXY eq 'N') then begin
	h=where((f[k] and 2L^6) eq 0,nkeep)
	if (nkeep eq 0) then begin
	     select_index = -1
	     return
	endif
	k=k[h]
  endif

  if (fs.GALAXY_BIG eq 'Y') then begin
	h=where((f[k] and 2L^7) ne 0,nkeep)
	if (nkeep eq 0) then begin
	     select_index = -1
	     return
	endif
	k=k[h]
  endif 
  if (fs.GALAXY_BIG eq 'N') then begin
	h=where((f[k] and 2L^7) eq 0,nkeep)
	if (nkeep eq 0) then begin
	     select_index = -1
	     return
	endif
	k=k[h]
  endif

  if (fs.GALAXY_BRIGHT_CORE eq 'Y') then begin
	h=where((f[k] and 2L^8) ne 0,nkeep)
	if (nkeep eq 0) then begin
	     select_index = -1
	     return
	endif
	k=k[h]
  endif 
  if (fs.GALAXY_BRIGHT_CORE eq 'N') then begin
	h=where((f[k] and 2L^8) eq 0,nkeep)
	if (nkeep eq 0) then begin
	     select_index = -1
	     return
	endif
	k=k[h]
  endif

  if (fs.ROSAT_A eq 'Y') then begin
	h=where((f[k] and 2L^9) ne 0,nkeep)
	if (nkeep eq 0) then begin
	     select_index = -1
	     return
	endif
	k=k[h]
  endif 
  if (fs.ROSAT_A eq 'N') then begin
	h=where((f[k] and 2L^9) eq 0,nkeep)
	if (nkeep eq 0) then begin
	     select_index = -1
	     return
	endif
	k=k[h]
  endif

  if (fs.ROSAT_B eq 'Y') then begin
	h=where((f[k] and 2L^10) ne 0,nkeep)
	if (nkeep eq 0) then begin
	     select_index = -1
	     return
	endif
	k=k[h]
  endif 
  if (fs.ROSAT_B eq 'N') then begin
	h=where((f[k] and 2L^10) eq 0,nkeep)
	if (nkeep eq 0) then begin
	     select_index = -1
	     return
	endif
	k=k[h]
  endif

  if (fs.ROSAT_C eq 'Y') then begin
	h=where((f[k] and 2L^11) ne 0,nkeep)
	if (nkeep eq 0) then begin
	     select_index = -1
	     return
	endif
	k=k[h]
  endif 
  if (fs.ROSAT_C eq 'N') then begin
	h=where((f[k] and 2L^11) eq 0,nkeep)
	if (nkeep eq 0) then begin
	     select_index = -1
	     return
	endif
	k=k[h]
  endif

  if (fs.ROSAT_D eq 'Y') then begin
	h=where((f[k] and 2L^12) ne 0,nkeep)
	if (nkeep eq 0) then begin
	     select_index = -1
	     return
	endif
	k=k[h]
  endif 
  if (fs.ROSAT_D eq 'N') then begin
	h=where((f[k] and 2L^12) eq 0,nkeep)
	if (nkeep eq 0) then begin
	     select_index = -1
	     return
	endif
	k=k[h]
  endif

  if (fs.STAR_BHB eq 'Y') then begin
	h=where((f[k] and 2L^13) ne 0,nkeep)
	if (nkeep eq 0) then begin
	     select_index = -1
	     return
	endif
	k=k[h]
  endif 
  if (fs.STAR_BHB eq 'N') then begin
	h=where((f[k] and 2L^13) eq 0,nkeep)
	if (nkeep eq 0) then begin
	     select_index = -1
	     return
	endif
	k=k[h]
  endif

  if (fs.STAR_CARBON eq 'Y') then begin
	h=where((f[k] and 2L^14) ne 0,nkeep)
	if (nkeep eq 0) then begin
	     select_index = -1
	     return
	endif
	k=k[h]
  endif 
  if (fs.STAR_CARBON eq 'N') then begin
	h=where((f[k] and 2L^14) eq 0,nkeep)
	if (nkeep eq 0) then begin
	     select_index = -1
	     return
	endif
	k=k[h]
  endif

  if (fs.STAR_BROWN_DWARF eq 'Y') then begin
	h=where((f[k] and 2L^15) ne 0,nkeep)
	if (nkeep eq 0) then begin
	     select_index = -1
	     return
	endif
	k=k[h]
  endif 
  if (fs.STAR_BROWN_DWARF eq 'N') then begin
	h=where((f[k] and 2L^15) eq 0,nkeep)
	if (nkeep eq 0) then begin
	     select_index = -1
	     return
	endif
	k=k[h]
  endif

  if (fs.STAR_SUB_DWARF eq 'Y') then begin
	h=where((f[k] and 2L^16) ne 0,nkeep)
	if (nkeep eq 0) then begin
	     select_index = -1
	     return
	endif
	k=k[h]
  endif 
  if (fs.STAR_SUB_DWARF eq 'N') then begin
	h=where((f[k] and 2L^16) eq 0,nkeep)
	k=k[h]
	if (nkeep eq 0) then begin
	     select_index = -1
	     return
	endif
  endif

  if (fs.STAR_CATY_VAR eq 'Y') then begin
	h=where((f[k] and 2L^17) ne 0,nkeep)
	if (nkeep eq 0) then begin
	     select_index = -1
	     return
	endif
	k=k[h]
  endif 
  if (fs.STAR_CATY_VAR eq 'N') then begin
	h=where((f[k] and 2L^17) eq 0,nkeep)
	if (nkeep eq 0) then begin
	     select_index = -1
	     return
	endif
	k=k[h]
  endif

  if (fs.STAR_RED_DWARF eq 'Y') then begin
	h=where((f[k] and 2L^18) ne 0,nkeep)
	if (nkeep eq 0) then begin
	     select_index = -1
	     return
	endif
	k=k[h]
  endif 
  if (fs.STAR_RED_DWARF eq 'N') then begin
	h=where((f[k] and 2L^18) eq 0,nkeep)
	if (nkeep eq 0) then begin
	     select_index = -1
	     return
	endif
	k=k[h]
  endif

  if (fs.STAR_WHITE_DWARF eq 'Y') then begin
	h=where((f[k] and 2L^19) ne 0,nkeep)
	if (nkeep eq 0) then begin
	     select_index = -1
	     return
	endif
	k=k[h]
  endif 
  if (fs.STAR_WHITE_DWARF eq 'N') then begin
	h=where((f[k] and 2L^19) eq 0,nkeep)
	if (nkeep eq 0) then begin
	     select_index = -1
	     return
	endif
	k=k[h]
  endif

  if (fs.SERENDIP_BLUE eq 'Y') then begin
	h=where((f[k] and 2L^20) ne 0,nkeep)
	if (nkeep eq 0) then begin
	     select_index = -1
	     return
	endif
	k=k[h]
  endif 
  if (fs.SERENDIP_BLUE eq 'N') then begin
	h=where((f[k] and 2L^20) eq 0,nkeep)
	if (nkeep eq 0) then begin
	     select_index = -1
	     return
	endif
	k=k[h]
  endif

  if (fs.SERENDIP_FIRST eq 'Y') then begin
	h=where((f[k] and 2L^21) ne 0,nkeep)
	if (nkeep eq 0) then begin
	     select_index = -1
	     return
	endif
	k=k[h]
  endif 
  if (fs.SERENDIP_FIRST eq 'N') then begin
	h=where((f[k] and 2L^21) eq 0,nkeep)
	if (nkeep eq 0) then begin
	     select_index = -1
	     return
	endif
	k=k[h]
  endif

  if (fs.SERENDIP_RED eq 'Y') then begin
	h=where((f[k] and 2L^22) ne 0,nkeep)
	if (nkeep eq 0) then begin
	     select_index = -1
	     return
	endif
	k=k[h]
  endif 
  if (fs.SERENDIP_RED eq 'N') then begin
	h=where((f[k] and 2L^22) eq 0,nkeep)
	if (nkeep eq 0) then begin
	     select_index = -1
	     return
	endif
	k=k[h]
  endif

  if (fs.SERENDIP_DISTANT eq 'Y') then begin
	h=where((f[k] and 2L^23) ne 0,nkeep)
	if (nkeep eq 0) then begin
	     select_index = -1
	     return
	endif
	k=k[h]
  endif 
  if (fs.SERENDIP_DISTANT eq 'N') then begin
	h=where((f[k] and 2L^23) eq 0,nkeep)
	if (nkeep eq 0) then begin
	     select_index = -1
	     return
	endif
	k=k[h]
  endif

  if (fs.SERENDIP_MANUAL eq 'Y') then begin
	h=where((f[k] and 2L^24) ne 0,nkeep)
	if (nkeep eq 0) then begin
	     select_index = -1
	     return
	endif
	k=k[h]
  endif 
  if (fs.SERENDIP_MANUAL eq 'N') then begin
	h=where((f[k] and 2L^24) eq 0,nkeep)
	if (nkeep eq 0) then begin
	     select_index = -1
	     return
	endif
	k=k[h]
  endif

  ;; These two are the same
  if (fs.QSO_MAG_OUTLIER eq 'Y') then begin
	h=where((f[k] and 2L^25) ne 0,nkeep)
	if (nkeep eq 0) then begin
	     select_index = -1
	     return
	endif
	k=k[h]
  endif 
  if (fs.QSO_MAG_OUTLIER eq 'N') then begin
	h=where((f[k] and 2L^25) eq 0,nkeep)
	if (nkeep eq 0) then begin
	     select_index = -1
	     return
	endif
	k=k[h]
  endif

  if (fs.QSO_FAINT eq 'Y') then begin
	h=where((f[k] and 2L^25) ne 0,nkeep)
	if (nkeep eq 0) then begin
	     select_index = -1
	     return
	endif
	k=k[h]
  endif 
  if (fs.QSO_FAINT eq 'N') then begin
	h=where((f[k] and 2L^25) eq 0,nkeep)
	if (nkeep eq 0) then begin
	     select_index = -1
	     return
	endif
	k=k[h]
  endif

  if (fs.GALAXY_RED_II eq 'Y') then begin
	h=where((f[k] and 2L^26) ne 0,nkeep)
	if (nkeep eq 0) then begin
	     select_index = -1
	     return
	endif
	k=k[h]
  endif 
  if (fs.GALAXY_RED_II eq 'N') then begin
	h=where((f[k] and 2L^26) eq 0,nkeep)
	if (nkeep eq 0) then begin
	     select_index = -1
	     return
	endif
	k=k[h]
  endif

  if (fs.ROSAT_E eq 'Y') then begin
	h=where((f[k] and 2L^27) ne 0,nkeep)
	if (nkeep eq 0) then begin
	     select_index = -1
	     return
	endif
	k=k[h]
  endif 
  if (fs.ROSAT_E eq 'N') then begin
	h=where((f[k] and 2L^27) eq 0,nkeep)
	if (nkeep eq 0) then begin
	     select_index = -1
	     return
	endif
	k=k[h]
  endif

  if (fs.STAR_PN eq 'Y') then begin
	h=where((f[k] and 2L^28) ne 0,nkeep)
	if (nkeep eq 0) then begin
	     select_index = -1
	     return
	endif
	k=k[h]
  endif 
  if (fs.STAR_PN eq 'N') then begin
	h=where((f[k] and 2L^28) eq 0,nkeep)
	if (nkeep eq 0) then begin
	     select_index = -1
	     return
	endif
	k=k[h]
  endif

  if (fs.QSO_REJECT eq 'Y') then begin
	h=where((f[k] and 2L^29) ne 0,nkeep)
	if (nkeep eq 0) then begin
	     select_index = -1
	     return
	endif
	k=k[h]
  endif 
  if (fs.QSO_REJECT eq 'N') then begin
	h=where((f[k] and 2L^29) eq 0,nkeep)
	if (nkeep eq 0) then begin
	     select_index = -1
	     return
	endif
	k=k[h]
  endif

  if (fs.SOUTHERN_SURVEY eq 'Y') then begin
	h=where((f[k] and 2L^31) ne 0,nkeep)
	if (nkeep eq 0) then begin
	     select_index = -1
	     return
	endif
	k=k[h]
  endif 
  if (fs.SOUTHERN_SURVEY eq 'N') then begin
	h=where((f[k] and 2L^31) eq 0,nkeep)
	if (nkeep eq 0) then begin
	     select_index = -1
	     return
	endif
	k=k[h]
  endif

  ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
  ;; begin flags2
  ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

  IF NOT doflags2 THEN BEGIN
      select_index=k
      return
  ENDIF 

  if (fs.LIGHT_TRAP eq 'Y') then begin
      h=where((f2[k] and 2L^0) ne 0,nkeep)
      if (nkeep eq 0) then begin
          select_index = -1
          return
      endif
      k=k[h]
  endif 
  if (fs.LIGHT_TRAP eq 'N') then begin
      h=where((f2[k] and 2L^0) eq 0,nkeep)
      if (nkeep eq 0) then begin
          select_index = -1
          return
      endif
      k=k[h]
  endif

  if (fs.REDDEN_STD eq 'Y') then begin
      h=where((f2[k] and 2L^1) ne 0,nkeep)
      if (nkeep eq 0) then begin
          select_index = -1
          return
      endif
      k=k[h]
  endif 
  if (fs.REDDEN_STD eq 'N') then begin
      h=where((f2[k] and 2L^1) eq 0,nkeep)
      if (nkeep eq 0) then begin
          select_index = -1
          return
      endif
      k=k[h]
  ENDIF

  if (fs.TEST_TARGET eq 'Y') then begin
      h=where((f2[k] and 2L^2) ne 0,nkeep)
      if (nkeep eq 0) then begin
          select_index = -1
          return
      endif
      k=k[h]
  endif 
  if (fs.TEST_TARGET eq 'N') then begin
      h=where((f2[k] and 2L^2) eq 0,nkeep)
      if (nkeep eq 0) then begin
          select_index = -1
          return
      endif
      k=k[h]
  endif

  if (fs.QA eq 'Y') then begin
      h=where((f2[k] and 2L^3) ne 0,nkeep)
      if (nkeep eq 0) then begin
          select_index = -1
          return
      endif
      k=k[h]
  endif 
  if (fs.QA eq 'N') then begin
      h=where((f2[k] and 2L^3) eq 0,nkeep)
      if (nkeep eq 0) then begin
          select_index = -1
          return
      endif
      k=k[h]
  endif

  if (fs.SKY eq 'Y') then begin
      h=where((f2[k] and 2L^4) ne 0,nkeep)
      if (nkeep eq 0) then begin
          select_index = -1
          return
      endif
      k=k[h]
  endif 
  
  if (fs.SKY eq 'N') then begin
      h=where((f2[k] and 2L^4) eq 0,nkeep)
      if (nkeep eq 0) then begin
          select_index = -1
          return
      endif
      k=k[h]
  ENDIF

 if (fs.SPECTROPHOTO_STD eq 'Y') then begin
      h=where((f2[k] and 2L^5) ne 0,nkeep)
      if (nkeep eq 0) then begin
          select_index = -1
          return
      endif
      k=k[h]
  endif 
  
  if (fs.SPECTROPHOTO_STD eq 'N') then begin
      h=where((f2[k] and 2L^5) eq 0,nkeep)
      if (nkeep eq 0) then begin
          select_index = -1
          return
      endif
      k=k[h]
  ENDIF

 if (fs.GUIDE_STAR eq 'Y') then begin
      h=where((f2[k] and 2L^6) ne 0,nkeep)
      if (nkeep eq 0) then begin
          select_index = -1
          return
      endif
      k=k[h]
  endif 
  
  if (fs.GUIDE_STAR eq 'N') then begin
      h=where((f2[k] and 2L^6) eq 0,nkeep)
      if (nkeep eq 0) then begin
          select_index = -1
          return
      endif
      k=k[h]
  ENDIF

if (fs.BUNDLE_HOLE eq 'Y') then begin
      h=where((f2[k] and 2L^7) ne 0,nkeep)
      if (nkeep eq 0) then begin
          select_index = -1
          return
      endif
      k=k[h]
  endif 
  
  if (fs.BUNDLE_HOLE eq 'N') then begin
      h=where((f2[k] and 2L^7) eq 0,nkeep)
      if (nkeep eq 0) then begin
          select_index = -1
          return
      endif
      k=k[h]
  ENDIF

if (fs.QUALITY_HOLE eq 'Y') then begin
      h=where((f2[k] and 2L^8) ne 0,nkeep)
      if (nkeep eq 0) then begin
          select_index = -1
          return
      endif
      k=k[h]
  endif 
  
  if (fs.QUALITY_HOLE eq 'N') then begin
      h=where((f2[k] and 2L^8) eq 0,nkeep)
      if (nkeep eq 0) then begin
          select_index = -1
          return
      endif
      k=k[h]
  ENDIF

if (fs.HOT_STD eq 'Y') then begin
      h=where((f2[k] and 2L^9) ne 0,nkeep)
      if (nkeep eq 0) then begin
          select_index = -1
          return
      endif
      k=k[h]
  endif 
  
  if (fs.HOT_STD eq 'N') then begin
      h=where((f2[k] and 2L^9) eq 0,nkeep)
      if (nkeep eq 0) then begin
          select_index = -1
          return
      endif
      k=k[h]
  ENDIF


  select_index=k 


return

end
