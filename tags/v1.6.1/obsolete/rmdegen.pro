PRO rmdegen, array, outarr,indices=indices, nonew=nonew

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;+
;
; NAME: 
;    RMDEGEN
;       
; PURPOSE: 
;    remove the denerate values from an array.  Return an array
;    or indices containing only 1 of each value in array.
;    THIS PROGRAM IS SLOW.  Use rem_dup of idlastron library.
;
; CALLING SEQUENCE: 
;    rmdegen, array, outarr,indices=indices             
;
; INPUTS: 
;    array of any type
;
; OPTIONAL OUTPUTS: 
;     outarr: an array containing 
;     indices: indices of that would be in outarr elements.  Can save
;              memory using this option
; 
; KEYWORD PARAMETERS: 
;    /nonew:  don't make a new array, just find the indices.
;
;
; REVISION HISTORY:
;	Erin Scott Sheldon  UofM  6/15/99
;       
;                                      
;-                                       
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;


  IF N_params() EQ 0 THEN BEGIN 
     print,'-Syntax: rmdegen, array [,outarr, indices=indices, nonew=nonew]'
     print,''
     print,'Use doc_library,"rmdegen"  for more help.'  
     return
  ENDIF 

  IF NOT keyword_set(nonew) THEN nonew=0

  nn = n_elements(array)
  w=lindgen(nn)
  ii=lonarr(nn)

  i=0
  p=where(array NE array[0], np)
  WHILE np NE 0 DO BEGIN
      i=i+1
      w = w[p]
      ii[i] = w[0]
      p = where(array[w] NE array[w[0]], np)
  ENDWHILE
 
  indices = ii[0:i]
  IF NOT nonew THEN outarr = array[indices]
return
END
