;+
;
; NAME:
;   sdss_flag_plot
;
; PURPOSE:
;   Plot a histogram of the flags set for a set of objects.
;
; CALLING SEQUENCE:
;   sdss_flag_plot, flags, flagtype, _extra=_extra
;
; INPUTS:
;   flags: a flag array.
;   flagtype: The flag type. Currently supported flags may be listed 
;      using the
;         IDL> sf->print_flagtypes 
;      method, but is most likely at least
;         OBJECT1, OBJECT2, PRIMTARGET, SECTARGET, STATUS
;
; OPTIONAL INPUTS:
;   _extra: Extra plotting keywords.
;
; EXAMPLE:
;   sdss_flag_plot, struct.primtarget, 'primtarget'
;
; MODIFICATION HISTORY:
;   Generalized from old program.  2006-12-3, Erin Sheldon, NYU.
;
;-

pro sdss_flag_plot, flags, flagtype, xtitle=xtitle, ytitle=ytitle, _extra=_extra

    if n_elements(flags) eq 0 or n_elements(flagtype) eq 0 then begin
        on_error, 2
        print,'-Syntax: sdss_flag_plot, flags, flagtype, _extra=_extra'
        print
        message,'Halting'
    endif

    sdssidl_setup
    !sdss->plotflags, flags, flagtype, xtitle=xtitle, ytitle=ytitle, _extra=_extra

end


