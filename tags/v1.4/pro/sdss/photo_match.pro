PRO photo_match,run1,rerun1,camcol1,field1,id1,$
                run2,rerun2,camcol2,field2,id2,m1,m2,count=count, silent=silent

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;+
;
; NAME:
;    PHOTO_MATCH
;       
; PURPOSE:
;    Match to lists of photo objects by their run, rerun, camcol, field, 
;    and rerun.  This uniquely defines each object.  
;
; CALLING SEQUENCE:
;    photo_match, run1, rerun1, camcol1, field1, id1, $
;                 run2, rerun2, camcol2, field2, id2, $
;                 matches1, matches2,
;                 count=count, /silent
;
; INPUTS: 
;    run1, rerun1, camcol1, field1, id1:   unique info for object list #1
;    run2, rerun2, camcol2, field2, id2:   unique info for object list #2
;
; OPTIONAL INPUTS:
;    None.
;
; KEYWORD PARAMETERS:
;    /silent: don't print out removal of duplicates
;       
; OUTPUTS: 
;    matches1, matches2:  match indices for each list.
;
; OPTIONAL OUTPUTS:
;    count: number of matches
;
; CALLED ROUTINES:
;    MATCH
; 
; PROCEDURE: 
;    Match two photo lists putting their run,rerun,camcol,field,id
;    into one super index which is a 64 bit number encoding these
;    to a unique number . Then a regular "sort match" can be run
;    this is therefore a n_log(n) algorithm. Duplicates are
;    removed before matching.
;	
; REVISION HISTORY:
;    Dave Johnston  11/12/99  UChicago
;    Remove Duplicates: Erin S. Sheldon ??  UofMich
;    David Johnston: fixed bug that limited runs to 1844 by taking away
;      an uneeded digit from camcol ,added check for values that are too big
;      the bug would probably never cause a mismatch even for run > 1844.
;      added a check for things out of range.  04/07/2003 
;
;-                                       
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;


  IF n_params() EQ 0 THEN BEGIN 
      print,'-syntax photo_match, run1, rerun1, camcol1, field1, id1, '
      print,'                     run2, rerun2, camcol2, field2, id2, '
      print,'                     m1, m2, count=count, /silent'
      print
      print,'Use doc_library, "photo_match"  for more help'
      return
  ENDIF 

  ;; will return with -9999 if no matching is done
  m1 = -9999L
  m2 = -9999L

  IF NOT keyword_set(silent) THEN silent=0

  ;; Check for out-of-range values
  somebad = 0L
      
  w=where(run1 LT 0 OR run1 GT 18445,nbad) & somebad=somebad+nbad
  IF nbad NE 0 THEN print,'Out of range values in first run array'
  w=where(run2 LT 0 OR run2 GT 18445,nbad) & somebad=somebad+nbad
  IF nbad NE 0 THEN print,'Out of range values in second run array'
  w=where(rerun1 LT 0 OR rerun1 GT 999,nbad) & somebad=somebad+nbad
  IF nbad NE 0 THEN print,'Out of range values in first rerun array'
  w=where(rerun2 LT 0 OR rerun2 GT 999,nbad) & somebad=somebad+nbad
  IF nbad NE 0 THEN print,'Out of range values in second rerun array'
  w=where(camcol1 LT 1 OR camcol1 GT 6,nbad) & somebad=somebad+nbad
  IF nbad NE 0 THEN print,'Out of range values in first camcol array'
  w=where(camcol2 LT 1 OR camcol2 GT 6,nbad) & somebad=somebad+nbad
  IF nbad NE 0 THEN print,'Out of range values in second camcol array'
  w=where(field1 LT 0 OR field1 GT 9999,nbad) & somebad=somebad+nbad
  IF nbad NE 0 THEN print,'Out of range values in first field array'
  w=where(field2 LT 0 OR field2 GT 9999,nbad) & somebad=somebad+nbad
  IF nbad NE 0 THEN print,'Out of range values in second field array'
  w=where(id1 LT 0 OR id1 GT 99999,nbad) & somebad=somebad+nbad
  IF nbad NE 0 THEN print,'Out of range values in first id array'
  w=where(id2 LT 0 OR id2 GT 99999,nbad) & somebad=somebad+nbad
  IF nbad NE 0 THEN print,'Out of range values in second id array'
      
  IF somebad NE 0 THEN BEGIN
      print,'ERROR: some values out of range. Returning without matching'
      return
  ENDIF

  ;; create super index for each set
  super1 = photoid(run1,rerun1,camcol1,field1,id1)
  super2 = photoid(run2,rerun2,camcol2,field2,id2)
  
  ;; remove duplicates
  m1tmp = rem_dup(super1)
  m2tmp = rem_dup(super2)
  
  nsup1 = n_elements(super1)
  nm1=n_elements(m1tmp)
  IF nsup1 NE nm1 THEN BEGIN
      IF NOT silent THEN print,'Removed '+ntostr(nsup1-nm1)+' duplicates from list 1'
  ENDIF 
  
  nsup2 = n_elements(super2)
  nm2=n_elements(m2tmp)
  IF nsup2 NE nm2 THEN BEGIN
      IF NOT silent THEN print,'Removed '+ntostr(nsup2-nm2)+' duplicates from list 2'
  ENDIF 
  
  ;; match
  match, super1[m1tmp], super2[m2tmp], m1, m2, count=count, /sort

  IF count NE 0 THEN BEGIN 
      m1 = m1tmp[m1]
      m2 = m2tmp[m2]
  ENDIF 

  return
END 

