;+
; NAME:
;  SDSS_FLAG
;
; PURPOSE:
;  Return the SDSS flag value given the flag name.
;
; CATEGORY:
;  SDSS specific
;
; CALLING SEQUENCE:
;  flagval = sdss_flag(flag_type, flag_name, status=)
;
; INPUTS:
;  flag_type: The flag type. A list of supported types can be gotten from
;           IDL> sf=obj_new('sdss_flags')
;           IDL> print, sf->flagtypes 
;    E.g. OBJECT1, PRIMTARGET, etc.
;  flag_name: The flag name, e.g. GALAXY, or PRIMARY
;
; OUTPUTS:
;  The flag value
;
; OPTIONAL OUTPUTS:
;  status: 1 for success, 0 for failure
;
; EXAMPLES:
;   IDL> print,sdss_flag('target', 'galaxy_red')
;       32
;   if (primtarget and sdss_flag('target','galaxy_red')) ne 0 then .....
;
; MODIFICATION HISTORY:
;  Added to archive mid 2005 from ancient existing code. Erin Sheldon, UChicago
;
;-


function sdss_flag, flag_type, flag_name

    if n_params() lt 2 then begin
        on_error, 2
        print,'-Syntax: flagval = sdss_flag(flag_type, flag_name, status=)'
        print
        message,'Halting'
    endif 

    sf=obj_new('sdss_flags')
    flags = sf->flag(flag_type, flag_name, status=status)
    obj_destroy, sf
    return, flags

end 
