PRO make_struct, tags, indices, bigstruct, str
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;
; NAME: 
;    MAKE_STRUCT
; PURPOSE:
;   Creates a structure with the user's input parameters.
;
; INPUTS: 
;   tags: The tags to be put into the structure str
;   check: The type of each tag
;
; Outputs: str: A structure with the users tags in it
;
; Author:  Erin Scott Sheldon
; Date: 10/7/98
; Modified:  01/12/99  Comment: Made field, camcol, run required 
;                               parameters.
;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

  IF n_params() LT 2 THEN BEGIN 
      print, '-syntax: make_struct, tags, indices, bigstruct, outstruct'
      return
  ENDIF 

  str = create_struct('run',0,'camcol',0,'rerun',0,'field',0)

  IF tags[0] NE '' THEN BEGIN 
      FOR i = 0, n_elements(tags)-1 DO BEGIN 
          str = create_struct( str, tags[i], bigstruct[0].(indices[i]) )
      ENDFOR 
  ENDIF 

  return 
END 


