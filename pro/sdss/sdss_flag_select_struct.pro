;+
; NAME:
;   sdss_flag_select_struct
;
; PURPOSE:
;   Return a flag selection structure for a given flag type.  This can be
;   sent to sdss_flag_select.  This will also show you all the possible
;   flags for a given type, although not all elements must be in the
;   structure sent to sdss_flag_select.
;
; CATEGORY:
;   SDSS specific routine
;
; CALLING SEQUENCE:
;   fs = sdss_flag_select_struct(flagtype, default=, status=)
;
; INPUTS:
;  flagtype: The flag type. Currently supported flags may be listed 
;      using the
;         IDL> ft = sf->flagtypes()
;      or
;         IDL> sf->print_flagdefs
;      methods, but are most likely at least
;         OBJECT1, OBJECT2, PRIMTARGET, SECTARGET, STATUS, RUNSTATUS
;
; OPTIONAL INPUTS:
;  default: The default selection value. 
;        'y': yes, the flag is set
;        'n': no, the flag is not set
;        'd': Don't care
;     The default value is 'd'
;
; OUTPUTS:
;   A flag select structure that can be sent to sdss_flag_select
;
; OPTIONAL OUTPUTS:
;   status: 1 for success, 0 for failure.
;
; EXAMPLES:
;   IDL> fs=sdss_flag_select_struct('object2')
;   IDL> fs.bright = 'N'
;   IDL> fs.satur = 'N'
;   IDL> keep = sdss_flag_select(struct.flags2, 'object2', fs)
;
;   Alternatively, if you already know the flags to check:
;   IDL> fs = {bright:'n', satur:'n'}
;   IDL> keep = sdss_flag_select(struct.flags2, 'object2', fs)
;
; MODIFICATION HISTORY:
;
;-

function sdss_flag_select_struct, flagtype, default=default, status=status

    if n_elements(flagtype) eq 0 then begin
        on_error, 2
        print,'-Syntax: ss = sdss_flag_select_struct(flagtype, default=, status=)'
        print
        message,'Halting'
    endif


    sf=obj_new('sdss_flags')
    st=sf->select_struct(flagtype, default=default, status=status)
    obj_destroy, sf

    return, st

end
