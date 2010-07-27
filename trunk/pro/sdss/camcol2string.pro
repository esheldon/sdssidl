;+
;
; NAME: 
;    camcol2string
;       
; PURPOSE: 
;    Function outputs a string containing sdss camcol
;    This just formats to '(i0)'
;
; CALLING SEQUENCE: 
;    result = camcol2string(camcol)
;      
; INPUTS: 
;    camcol: may be an array.
;
; Optional Outputs:
;    isglob: true of the entered camcol is '*'
;	
; REVISION HISTORY:
;     Author: Erin Scott Sheldon  Umich 23-OCT-2001
;     Now just use a format string.  2006-10-07, Erin Sheldon, NYU
;     Added isglob check: 2010-07-26, Erin Sheldon, BNL
;                                      
;-                                       


function camcol2string, camcol, isglob=isglob

    if N_params() eq 0 then begin
        on_error, 2
        print,'-Syntax: result = camcol2string(camcol, isglob=)'
        message,'Halting'
    endif

    isglob=0
    if size(camcol,/tname) eq 'STRING' then begin
        if camcol[0] eq '*' then isglob=1
        return,camcol
    endif
    return, strtrim(string(camcol,f='(i0)'),2)
end 


