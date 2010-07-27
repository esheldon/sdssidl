;+
;
; NAME: 
;    run2string
;       
; PURPOSE: 
;    Function outputs a string containing run of a photo tsObj 
;    file in the correct format: length 6 with zero padding.
;	
;
; CALLING SEQUENCE: 
;    result = run2string(run)
;      
; INPUTS: 
;    run: may be an array.
;
; Optional Outputs:
;    isglob: true of the entered camcol is '*'
;
; REVISION HISTORY:
;     Author: Erin Scott Sheldon  Umich 5/25/99
;     Now just use a format string.  2006-10-07, Erin Sheldon, NYU
;                                      
;-                                       


function run2string, run, isglob=isglob
    if N_params() eq 0 then begin
        on_error, 2
        print,'-Syntax: result = run2string(run, isglob=)'
        message,'Halting'
    endif

    isglob=0
    if size(run,/tname) eq 'STRING' then begin
        if run[0] eq '*' then begin
            isglob=1
        endif
        return,run
    endif
    return, string(run,format='(i06)')
end 






