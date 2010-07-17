PRO read_tsobj_make_tags, taglist, phototags, goodtags, indices, nmatch, $
                          verbose=verbose

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;+
;
; NAME:
;    READ_TSOBJ_MAKE_TAGS
;
; PURPOSE:
;   Finds all input tags that match with photo tags and puts them into goodtags
;
; CALLING SEQUENCE:
;   read_tsobj_make_tags, taglist, phototags, goodtags, indices
;
; INPUTS:  
;   taglist:  The user input tags.
;   phototags: The allowed photo tags
;
; OUTPUTS: 
;   goodtags: Those of the input tags which match to photo tags
;   indices: The indices corresponding to the proper photo tags
;   nmatch: number of matches
;
; Author:  Erin Scott Sheldon
; Date: 10/7/98
; Modified: 11/20/98  Comment:  Corrected erroneous error message. E.S.S.
;           05/03/00  Comment:  Rewrote entirely.
;
;-
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

  IF n_params() LT 2 then begin
      print,'-syntax:  make_tags, taglist, phototags, goodtags, indices, verbose=verbose'
      return
  ENDIF 

  IF n_elements(verbose) EQ 0 THEN verbose=2

  ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
  ;; Match this way rather than use match.pro 
  ;; -match.pro re-orders alphabetically)
  ;; -match cannot take vectors with duplicates
  ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

  delvarx, goodtags, indices

  nmatch = 0L
  FOR i=0, n_elements(taglist)-1 DO BEGIN 

      ;; any matches in phototags?
      wtag = where(phototags EQ taglist[i], nwtag)

      IF nwtag NE 0 THEN BEGIN 
          ;; make sure not a duplicate
          IF nmatch NE 0 THEN BEGIN 
              wdup = where( goodtags EQ taglist[i], ndup)
          ENDIF ELSE ndup = 0
          
          ;; copy in if not duplicate
          IF ndup EQ 0 THEN BEGIN 
              add_arrval, phototags[wtag[0]], goodtags
              add_arrval, wtag[0], indices
              nmatch = nmatch + 1
          ENDIF 
      ENDIF ELSE IF verbose GT 0 THEN BEGIN 
          ;; no match found
          message,'tag "'+taglist[i]+'" was not found',/inf
      ENDIF 

  ENDFOR 
  
  return 
END 



