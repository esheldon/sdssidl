;+
;
; NAME:
;  sdss_flag_truct([flagtype], status=)
;
; PURPOSE:
;  Return the flag structure for all types or a given type if
;  the flagtype parameter is sent.  This structure contains the
;  flag values by name; e.g. if no flagtype is sent, the object1
;  flag "bright" is given by:
;    flagstruct.object1.bright
;  if flagtype "object1" is sent then this is simplified:
;    flagstruct.bright
;  Note, individual flag values can be extracted with the flag
;  method, or sdss_flag() procedure:
;    print,sdss_flag(flagtype,flagname)
;
; CALLING SEQUENCE:
;  si = sdss_flag_struct(flagstype)
;  
; INPUTS:
;  flagtype: The flag type. Currently supported flags may be listed 
;      using the
;         IDL> ft = sf->flagtypes()
;         IDL> sf->print_flagdefs
;      methods, but is most likely at least
;         OBJECT1, OBJECT2, PRIMTARGET, SECTARGET, STATUS
; OUTPUTS:
;   The flag structure.
;
; OPTIONAL OUTPUTS:
;  status: 1 if success, 0 if failure
;
; EXAMPLES:
;  IDL> fs=sdss_flagstruct('object1')
;
; MODIFICATION HISTORY:
;  Created: Mid-2004  Erin Sheldon Uchicago
;
;-

function sdss_flag_struct, flagtype, status=status    

    sdssidl_setup
    return, !sdss->flagstruct(flagtype, status=status)

end
