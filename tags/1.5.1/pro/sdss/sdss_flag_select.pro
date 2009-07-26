;+
;
; NAME:
;  flag_select()
;
; PURPOSE:
;  Check the bits in a set of input flags.  The flags, flag type, and
;  a structure describing the flags to check are input.  The flags can
;  be ANDED or OR-ED together.  The indices of the input that match
;  the selection criteria is returned, or -1 if none.
;
; CALLING SEQUENCE:
;
;  si = sdss_flag_select(flags, flagtype, select_struct, [nkeep, 
;                       /orflags, input_index=]
;  
; INPUTS:
;  flags: A set of flags.  May be an array.
;  flagtype: The flag type. Currently supported flags may be listed 
;      using the
;         IDL> sf->print_flagtypes 
;      method, but is most likely at least
;         OBJECT1, OBJECT2, PRIMTARGET, SECTARGET, STATUS
;
;  select_struct:  A structure with flag names and select criteria. The 
;      available flags can be listed with the method:
;         IDL> sf->print_allflags
;      The select criteria are
;         'y', 'n', or 'd' (yes it is set, no it isn't set, don't care)
;      e.g For OBJECT1 flags:
;         IDL> fs = {child: 'y', satur: 'n'}
;      The flag names for a given flag type may be gotten:
;         IDL> fnames=sf->flagnames(flagtype)
;      or
;         IDL> sf->print_allflags
;
;
; OPTIONAL INPUTS:
;  /orflags: OR the flags. Default is to AND them.
;  input_index=: A beginning index to restrict the search.
;
; OPTIONAL OUTPUTS:
;   nkeep: The number that matched the selection criteria.
;
; EXAMPLES:
;   IDL> fst = {galaxy_red:  'Y'}
;   IDL> si = sdss_flag_select(st.primtarget, 'primtarget', fst}
;
;   IDL> fst = {child: 'y', satur: 'n'}
;   IDL> si = sdss_flag_select(st.flags[2], 'object1', fst)
;
;   ; Choose either lrg or main sample galaxies usign the /orflags option.
;   IDL> fst = {galaxy_red: 'Y', galaxy: 'Y'}
;   IDL> si = sdss_flag_select(st.primtarget, 'primtarget', fst, /orflags}
;
;
; MODIFICATION HISTORY:
;  Created: Mid-2005  Erin Sheldon Uchicago
;
;-

function sdss_flag_select, flags, flagtype, select_struct, nkeep, orflags=orflags, input_index=input_index

  if n_params() lt 3 then begin 
      on_error, 2
      print,'-Syntax: keep = sdss_flag_select(flags, flagtype, select_struct, [nkeep, /orflags, input_index=])'
      print
      message,'Halting'
  endif 

    sdssidl_setup
    return,!sdss->flag_select(flags, flagtype, select_struct, nkeep, $
                              orflags=orflags, input_index=input_index)

end 

