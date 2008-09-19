PRO matchphoto, str1, str2, i1, i2, silent=silent

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;+
;
; NAME: 
;     matchphoto
;       
; PURPOSE: 
;     match two photo structures by id and field.
;     !!!! Use PHOTO_MATCH  its much faster.
;
; CALLING SEQUENCE: matchphoto, str1, str2, i1, i2, silent=silent
;      
; INPUTS: str1, str2: the two structures.  Must include id and field
;
; INPUT KEYWORD PARAMETERS:
;         silent: shut off messages except errors.
;
; OUTPUTS: i1, i2: The indices of matches.  i1[index] corresponds directly
;                  to i2[index]
;
; REVISION HISTORY:
;	Erin Scott Sheldon  UofM  7/4/99
;       
;                                      
;-                                       
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;


  IF N_params() LT 2 THEN BEGIN 
     print,'-Syntax: matchphoto, str1, str2, i1, i2'
     print,''
     print,'Use doc_library,"matchphoto"  for more help.'  
     return
  ENDIF 

IF NOT keyword_set(silent) THEN silent = 0

IF NOT silent THEN s=systime(1)

r1=str1[0].run
r2=str2[0].run

IF r1 NE r2 THEN BEGIN
  print,'Must be same run'
  return
ENDIF

field1 = str1[ rem_dup(str1.field) ].field
field2 = str2[ rem_dup(str2.field) ].field

n1=n_elements(field1)
n2=n_elements(field2)

i1=[-1]
i2=[-1]

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
; can save ourselves time by looping over fewer objects.
; That is why the code is duplicated below so much.
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

IF n1 LE n2 THEN BEGIN

  field = field1
  FOR i=0, n1-1 DO BEGIN

    w2=where(str2.field EQ field[i], nw2)
    IF nw2 NE 0 THEN BEGIN
      w1=where( str1.field EQ field[i] , nw1)
      ;;;; can save ourselves time by looping over fewer id
      IF nw1 LT nw2 THEN BEGIN
        ind1=[-1]
        ind2=[-1]
        FOR jj=0, nw1-1 DO BEGIN 
          w=where( str2[w2].id EQ str1[w1[jj]].id, nw)
      
          ind1 = [ind1, jj]
          ind2 = [ind2, w]
        ENDFOR
        w=where(ind2 NE -1, nw)
        IF nw NE 0 THEN BEGIN
          i1 = [i1, w1[ind1[w]] ]
          i2 = [i2, w2[ind2[w]] ]
        ENDIF
      ENDIF ELSE BEGIN
        ind1=[-1]
        ind2=[-1]
        FOR jj=0, nw2-1 DO BEGIN
          w=where( str1[w1].id EQ str1[w2[jj]].id, nw)
      
          ind1 = [ind1, jj]
          ind2 = [ind2, w]
        ENDFOR
        w=where(ind2 NE -1, nw)
        IF nw NE 0 THEN BEGIN
          i1 = [i1, w1[ind1[w]] ]
          i2 = [i2, w2[ind2[w]] ]
        ENDIF
      ENDELSE
    ENDIF 
  ENDFOR
ENDIF ELSE BEGIN
  field = field2
  FOR i=0, n2-1 DO BEGIN
  
    w1=where(str1.field EQ field[i], nw1)
    IF nw1 NE 0 THEN BEGIN
      w2=where( str2.field EQ field[i] , nw2)

      ;;;; can save ourselves time by looping over fewer id
      IF nw1 LT nw2 THEN BEGIN
        ind1=[-1]
        ind2=[-1]
        FOR jj=0, nw1-1 DO BEGIN 
          w=where( str2[w2].id EQ str1[w1[jj]].id, nw)
      
          ind1 = [ind1, jj]
          ind2 = [ind2, w]
        ENDFOR
        w=where(ind2 NE -1, nw)
        IF nw NE 0 THEN BEGIN
          i1 = [i1, w1[ind1[w]] ]
          i2 = [i2, w2[ind2[w]] ]
        ENDIF
      ENDIF ELSE BEGIN
        ind1=[-1]
        ind2=[-1]
        FOR jj=0, nw2-1 DO BEGIN
          w=where( str1[w1].id EQ str1[w2[jj]].id, nw)
      
          ind1 = [ind1, jj]
          ind2 = [ind2, w]
        ENDFOR
        w=where(ind2 NE -1, nw)
        IF nw NE 0 THEN BEGIN
          i1 = [i1, w1[ind1[w]] ]
          i2 = [i2, w2[ind2[w]] ]
        ENDIF
      ENDELSE
    ENDIF 
  ENDFOR
ENDELSE 

n=n_elements(i1)
i1 = i1[1:n-1]
i2 = i2[1:n-1]

IF NOT silent THEN BEGIN
  print,ntostr(n-1),' Matches found'
  t=systime(1)-s
  ptime,t
ENDIF 


return
END













