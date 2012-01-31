
;+
;
; NAME: 
;    ID2STRING
;       
; PURPOSE: 
;    Function outputs a string containing id of a photo object
;    in the correct format: length 5 with zero padding.
;	
;
; CALLING SEQUENCE: 
;    result = id2string(id)
;      
; INPUTS: 
;    id: may be an array.
;	
; REVISION HISTORY:
;     Author: Erin Scott Sheldon  UChicago 25-Feb-2004
;     Now just use a format string.  2006-10-07, Erin Sheldon, NYU
;                                      
;-                                       

function id2string, id

  if N_params() eq 0 then begin
      on_error, 2
      print,'-Syntax: result = id2string(id)'
      message,'Halting'
  endif

  if size(id,/tname) eq 'STRING' then return,id
  return, string(id,format='(i05)')
end 


