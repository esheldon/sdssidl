;+
;
; NAME: 
;    STRIPE2STRING
;       
; PURPOSE: 
;    Function outputs a string containing stripe of a photo tsObj 
;    file in the correct format: length 2 with zero padding.
;	
;
; CALLING SEQUENCE: 
;    result = stripe2string(stripe)
;      
; INPUTS: 
;    stripe: may be an array.
;	
; REVISION HISTORY:
;     Author: Erin Scott Sheldon  Umich 23-OCT-2001
;     Now just use a format string.  2006-10-07, Erin Sheldon, NYU
;                                      
;-                                       

function stripe2string, stripe

  if n_params() eq 0 then begin
      on_error, 2
      print,'-Syntax: result = stripe2string(stripe)'
      message,'Halting'
  endif

  if size(stripe,/tname) eq 'STRING' then return,stripe
  return, string(stripe,format='(i02)')
end 


