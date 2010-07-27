;+
;
; NAME: 
;    FIELD2STRING
;       
; PURPOSE: 
;    Function outputs a string containing field of a photo tsObj 
;    file in the correct format: width of 4 with zero padding.
;	
;
; CALLING SEQUENCE: 
;    result = field2string(field)
;      
; INPUTS: 
;    field number (May be an array)
;	
; REVISION HISTORY:
;     Author: Erin Scott Sheldon  Umich 5/25/99
;     Now just use a format string.  2006-10-07, Erin Sheldon, NYU
;                                      
;-                                       

function field2string, field, isglob=isglob

    if N_params() eq 0 then begin
        on_error, 2
        print,'-Syntax: result = ield2string(field, isglob=)'
        message,'Halting'
    endif

    isglob=0
    if size(field,/tname) eq 'STRING' then begin
        if field[0] eq '*' then isglob=1
        return,field
    endif
    return, string(field,format='(i04)')

end 






