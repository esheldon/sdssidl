pro make_tags, taglist, phototags, goodtags, indices

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;+
;
; NAME:
;    MAKE_TAGS
;
; PURPOSE:
;   Finds all input tags that match with photo tags and puts them into goodtags
;
; INPUTS:  
;   taglist:  The user input tags.
;   phototags: The allowed photo tags
;
; OUTPUTS: 
;   goodtags: Those of the input tags which match to photo tags
;   indices: The indices corresponding to the proper photo tags
;
; Author:  Erin Scott Sheldon
; Date: 10/7/98
; Modified: 11/20/98  Comment:  Corrected erroneous error message. E.S.S.
;           05/03/00  Comment:  Rewrote entirely.
;
;-
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

  IF n_params() LT 2 then begin
      print,'-syntax:  make_tags, taglist, phototags, goodtags, indices'
      return
  ENDIF 

  goodtags = strarr(1)
  indices = intarr(1)

  ww = where(taglist EQ 'RUN' OR taglist EQ 'CAMCOL' OR taglist EQ 'RERUN' OR $
             taglist EQ 'FIELD', nww)
  IF nww NE 0 THEN BEGIN 
      remove, ww, taglist
  ENDIF 

  num=0
  FOR i=0, n_elements(taglist)-1 DO BEGIN 

      wtag = where(phototags EQ taglist[i], nwtag)
      IF nwtag NE 0 THEN BEGIN 
          IF (num EQ 0) THEN BEGIN 
              goodtags[0] = phototags[wtag[0]]
              indices[0] = wtag[0]
              num=1
          ENDIF ELSE BEGIN 
              goodtags = [ goodtags, phototags[wtag[0]] ]
              indices = [ indices, wtag[0] ]
          ENDELSE
      ENDIF ELSE BEGIN 
          print,'-----------------------------------------'
          print,' WARNING: ', taglist[i],' tag is not found'
          print,'-----------------------------------------'
      ENDELSE 

  ENDFOR 
  
  print,'-----------------------------------------'	
  IF goodtags[0] EQ '' THEN BEGIN 
      print,' No correct tags: will use minimal tags only'
  ENDIF ELSE BEGIN 
      print,' Good tags found: ',strtrim(string(n_elements(goodtags)),2)
  ENDELSE 
  print,'-----------------------------------------'
  
  return 
end



