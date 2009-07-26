;+
;
; NAME:
;   sdss_flag_print
;
; PURPOSE:
;   Print all flags that are set.
;
; CALLING SEQUENCE:
;   sdss_flag_print, flags, flagtype, _extra=_extra
;
; INPUTS:
;   flags: a flag array.
;   flagtype: The flag type. Currently supported flags may be listed 
;      using the
;         IDL> sf->print_flagtypes 
;      method, but is most likely at least
;         OBJECT1, OBJECT2, PRIMTARGET, SECTARGET, STATUS
;
; EXAMPLE:
;   sdss_flag_print, struct.flags2, 'object2'
;
; MODIFICATION HISTORY:
;   Generalized from old program.  2006-12-3, Erin Sheldon, NYU.
;
;-

pro sdss_flag_print, flags, flagtype
    
    if n_elements(flags) eq 0 or n_elements(flagtype) eq 0 then begin
        on_error, 2
        print,'-Syntax: sdss_flag_print, flags, flagtype'
        print
        message,'Halting'
    endif

    sdssidl_setup
    !sdss->printflags, flags, flagtype

end


