function ntostr_old, num, pos2, pos1, format=format, round=round

  IF N_params() EQ 0 THEN BEGIN 
     print,'-Syntax: string = ntostr_old( num [, pos2, pos1, round=round] )'
     print,''
     print,'Use doc_library,"ntostr"  for more help.'  
     return,''
  ENDIF 

  ;; type checking: 8=struct 10=pointer 11=object
  typestruct = size(num, /struct)
  type = typestruct.type

  IF (type EQ 8) OR (type EQ 10) OR (type EQ 11) THEN BEGIN
      message,'Input must be a number or string (scalar or array)',/inf
      return,''
  ENDIF 

  ;; remove leading and trailing blanks
  tmp = strtrim(string(num, format=format), 2)

  np1 = n_elements(pos1)
  np2 = n_elements(pos2)

  IF np1 EQ 0 THEN pos1 = 0
  IF np2 EQ 0 THEN pos2 = strlen(tmp)

  np1 = n_elements(pos1)
  np2 = n_elements(pos2)
  
  IF keyword_set(round) THEN BEGIN 

      ;; check if scalar
      IF typestruct.n_dimensions EQ 0 THEN BEGIN 

          ;; number of positions left of decimal
          nleft = long( alog10( abs(num) ) ) + 1 > 1
          ;; add extra for minus sign
          nleft = nleft + (num LT 0.0)
          
          ;; so how many are available to the right of the decimal,
          ;; given pos2 (length of string)?
          nav = pos2 - nleft - 1 ;1 for decimal place
          
          IF nav GE 0 THEN BEGIN 
              tnum = rnd(num, nav)
          ENDIF ELSE BEGIN 
              ;; deal with decimal place: 0 means just including decimal as
              ;; above. -1 means not including decimal, but rounding should
              ;; be same
              nav = nav+1
              tnum = rnd(num, nav)
          ENDELSE 
          tmp = strtrim(string(tnum, format=format), 2)
          
          return, strmid( tmp, pos1, pos2)
      ENDIF ELSE message,'Cannot round non-scalar input',/inf
  ENDIF 

  if np1 ne 0 or np2 ne 0 then begin 

      nnum = typestruct.n_elements
      IF np1 LT nnum THEN usepos1 = replicate(pos1[0],nnum) ELSE usepos1=pos1
      IF np2 LT nnum THEN usepos2 = replicate(pos2[0],nnum) ELSE usepos2=pos2
      FOR i=0L, nnum-1 DO tmp[i] = strmid(tmp[i], usepos1[i],usepos2[i])

  endif 

  return,tmp

END
