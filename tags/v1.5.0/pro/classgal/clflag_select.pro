;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;+
;
; NAME:  CLFLAG_SELECT
;    
;       
; PURPOSE:  
;    Makes cuts based on classification flags using input classification structure.
;      These cuts are "anded" together, they must all be true for object to
;      survive.
;    
;
; CALLING SEQUENCE:
;     clflag_select,pstruct,clflag_struct,select_index
;    
;
; INPUTS: 
;     pstruct:  A photo output structure (must have classification tag).
;     clflag_struct:  premade classification flag structure.  This will require
;          any flags set to 'Y' and insist that any flag set to 'N' be off
;
; OUTPUTS: 
;    select_index:  indices of selected objects
;
; REVISION HISTORY:  Judith Racusin 6/15/00, modified from flag_select by Tim Mckay
;                    8/6/01 added bad_asymmetry and bad_concentration
;       
;                                      
;-                                       
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;


PRO clflag_select,pstruct,clflag_struct,select_index


  on_error, 2

  IF n_params() LT 3 THEN BEGIN
      print,'Syntax:  clflag_select,pstruct,clflag_struct,select_index'
      return
  END

  k=lindgen(n_elements(pstruct))

  tags=tag_names(pstruct)
  w1=where(tags EQ 'CLASSIFICATION', nw1)
  IF nw1 EQ 0 THEN BEGIN
      print,'Not classified'
      return
  ENDIF

  fs=clflag_struct
  IF (n_elements(pstruct[0].classification) EQ 5) THEN n=2 ELSE n=0
  f=pstruct(k).classification[n]

  IF (fs.ellip EQ 'Y') THEN BEGIN
        h=where((f(k) AND 2L^0) NE 0)
        IF ((size(h))(0) EQ 0) THEN BEGIN
             select_index = -1
             return
        ENDIF
        k=k(h)
  ENDIF 
  IF (fs.ellip EQ 'N') THEN BEGIN
        h=where((f(k) AND 2L^0) EQ 0)
        IF ((size(h))(0) eq 0) THEN BEGIN
             select_index = -1
             return
        ENDIF
        k=k(h)
  ENDIF
  IF (fs.ellip_likely EQ 'Y') THEN BEGIN
        h=where((f(k) AND 2L^1) NE 0)
        IF ((size(h))(0) EQ 0) THEN BEGIN
             select_index = -1
             return
        ENDIF
        k=k(h)
  ENDIF 
  IF (fs.ellip_likely EQ 'N') THEN BEGIN
        h=where((f(k) AND 2L^1) EQ 0)
        IF ((size(h))(0) eq 0) THEN BEGIN
             select_index = -1
             return
        ENDIF
        k=k(h)
  ENDIF
   IF (fs.spiral_likely EQ 'Y') THEN BEGIN
        h=where((f(k) AND 2L^2) NE 0)
        IF ((size(h))(0) EQ 0) THEN BEGIN
             select_index = -1
             return
        ENDIF
        k=k(h)
  ENDIF 
  IF (fs.spiral_likely EQ 'N') THEN BEGIN
        h=where((f(k) AND 2L^2) EQ 0)
        IF ((size(h))(0) eq 0) THEN BEGIN
             select_index = -1
             return
        ENDIF
        k=k(h)
  ENDIF
  IF (fs.spiral EQ 'Y') THEN BEGIN
        h=where((f(k) AND 2L^3) NE 0)
        IF ((size(h))(0) EQ 0) THEN BEGIN
             select_index = -1
             return
        ENDIF
        k=k(h)
  ENDIF 
  IF (fs.spiral EQ 'N') THEN BEGIN
        h=where((f(k) AND 2L^3) EQ 0)
        IF ((size(h))(0) eq 0) THEN BEGIN
             select_index = -1
             return
        ENDIF
        k=k(h)
  ENDIF
   IF (fs.failed EQ 'Y') THEN BEGIN
        h=where((f(k) AND 2L^4) NE 0)
        IF ((size(h))(0) EQ 0) THEN BEGIN
             select_index = -1
             return
        ENDIF
        k=k(h)
  ENDIF 
  IF (fs.failed EQ 'N') THEN BEGIN
        h=where((f(k) AND 2L^4) EQ 0)
        IF ((size(h))(0) eq 0) THEN BEGIN
             select_index = -1
             return
        ENDIF
        k=k(h)
  ENDIF
  IF (fs.lens_galaxy_g EQ 'Y') THEN BEGIN
        h=where((f(k) AND 2L^5) NE 0)
        IF ((size(h))(0) EQ 0) THEN BEGIN
             select_index = -1
             return
        ENDIF
        k=k(h)
  ENDIF 
  IF (fs.lens_galaxy_g EQ 'N') THEN BEGIN
        h=where((f(k) AND 2L^5) EQ 0)
        IF ((size(h))(0) eq 0) THEN BEGIN
             select_index = -1
             return
        ENDIF
        k=k(h)
  ENDIF
  IF (fs.lens_galaxy_r EQ 'Y') THEN BEGIN
        h=where((f(k) AND 2L^6) NE 0)
        IF ((size(h))(0) EQ 0) THEN BEGIN
             select_index = -1
             return
        ENDIF
        k=k(h)
  ENDIF 
  IF (fs.lens_galaxy_r EQ 'N') THEN BEGIN
        h=where((f(k) AND 2L^6) EQ 0)
        IF ((size(h))(0) eq 0) THEN BEGIN
             select_index = -1
             return
        ENDIF
        k=k(h)
  ENDIF
  IF (fs.lens_galaxy_i EQ 'Y') THEN BEGIN
        h=where((f(k) AND 2L^7) NE 0)
        IF ((size(h))(0) EQ 0) THEN BEGIN
             select_index = -1
             return
        ENDIF
        k=k(h)
  ENDIF 
  IF (fs.lens_galaxy_i EQ 'N') THEN BEGIN
        h=where((f(k) AND 2L^7) EQ 0)
        IF ((size(h))(0) eq 0) THEN BEGIN
             select_index = -1
             return
        ENDIF
        k=k(h)
  ENDIF
  IF (fs.brg EQ 'Y') THEN BEGIN
        h=where((f(k) AND 2L^8) NE 0)
        IF ((size(h))(0) EQ 0) THEN BEGIN
             select_index = -1
             return
        ENDIF
        k=k(h)
  ENDIF 
  IF (fs.brg EQ 'N') THEN BEGIN
        h=where((f(k) AND 2L^8) EQ 0)
        IF ((size(h))(0) eq 0) THEN BEGIN
             select_index = -1
             return
        ENDIF
        k=k(h)
  ENDIF
  IF (fs.NOT_a_galaxy EQ 'Y') THEN BEGIN
        h=where((f(k) AND 2L^9) NE 0)
        IF ((size(h))(0) EQ 0) THEN BEGIN
             select_index = -1
             return
        ENDIF
        k=k(h)
  ENDIF 
  IF (fs.NOT_a_galaxy EQ 'N') THEN BEGIN
        h=where((f(k) AND 2L^9) EQ 0)
        IF ((size(h))(0) eq 0) THEN BEGIN
             select_index = -1
             return
        ENDIF
        k=k(h)
  ENDIF
  IF (fs.bad_asymmetry EQ 'Y') THEN BEGIN
        h=where((f(k) AND 2L^10) NE 0)
        IF ((size(h))(0) EQ 0) THEN BEGIN
             select_index = -1
             return
        ENDIF
        k=k(h)
  ENDIF 
  IF (fs.bad_asymmetry EQ 'N') THEN BEGIN
        h=where((f(k) AND 2L^10) EQ 0)
        IF ((size(h))(0) eq 0) THEN BEGIN
             select_index = -1
             return
        ENDIF
        k=k(h)
  ENDIF
  IF (fs.bad_concentration EQ 'Y') THEN BEGIN
        h=where((f(k) AND 2L^11) NE 0)
        IF ((size(h))(0) EQ 0) THEN BEGIN
             select_index = -1
             return
        ENDIF
        k=k(h)
  ENDIF 
  IF (fs.bad_concentration EQ 'N') THEN BEGIN
        h=where((f(k) AND 2L^11) EQ 0)
        IF ((size(h))(0) eq 0) THEN BEGIN
             select_index = -1
             return
        ENDIF
        k=k(h)
  ENDIF
  select_index=k

return
END
