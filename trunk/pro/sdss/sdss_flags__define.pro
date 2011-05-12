;+
; NAME:
;  SDSS_FLAGS__DEFINE  (An IDL Class File)
;
; PURPOSE:
;  Defines a set of methods for doing SDSS flag selection.
;
; CATEGORY:
;  SDSS 
;
; CALLING SEQUENCE:
;  sf=obj_new('sdss_files')
;
; SIDE EFFECTS:
;  The sdss flagvals file is read and internal pointers are created to
;  the data.
;
; RESTRICTIONS:
;  The flagvals.idl data file must exist in the expected place.
;     getenv("SDSSIDL_DIR")+'/data/flagvals.idl'
;
; METHODS:
;  Use the methods procedure to list the methods of this file.
;     IDL> methods, 'sdss_files'
;  And the doc_method procedure to get documentation on individual
;  methods.
;     IDL> doc_method, 'sdss_files::flag_select'
;
;  A quick list of useful methods:
;  
;    ::flag_select(flags,type,select_struct)  - Do flag selection using a flag struct
;    ::flag(type,name)  - Return the flag value for the input type and name
;    ::flagtypes()    - Return available flag types.
;    ::flagnames(type)  - Return flag names for a given flag type.
;    ::print_flagdefs - Print all available flag info.
;
; EXAMPLES:
;   IDL> sf=obj_new('sdss_flags')
;   IDL> fst = {galaxy_red:  'Y'}
;   IDL> si = sf->flag_select(st.primtarget, 'primtarget', fst}
;
;   IDL> fst = {child: 'y', satur: 'n'}
;   IDL> si = sf->flag_select(st.flags[2], 'object1', fst)
;
;   ; Choose either lrg or main sample galaxies usign the /orflags option.
;   IDL> fst = {galaxy_red: 'Y', galaxy: 'Y'}
;   IDL> si = sf->flag_select(st.primtarget, 'primtarget', fst, /orflags}
;
;
; MODIFICATION HISTORY:
;  Created: Mid-2005  Erin Sheldon Uchicago
;
;-
;
;  Copyright (C) 2005  Erin Sheldon, NYU.  erin dot sheldon at gmail dot com
;
;    This program is free software; you can redistribute it and/or modify
;    it under the terms of the GNU General Public License as published by
;    the Free Software Foundation; either version 2 of the License, or
;    (at your option) any later version.
;
;    This program is distributed in the hope that it will be useful,
;    but WITHOUT ANY WARRANTY; without even the implied warranty of
;    MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
;    GNU General Public License for more details.
;
;    You should have received a copy of the GNU General Public License
;    along with this program; if not, write to the Free Software
;    Foundation, Inc., 51 Franklin St, Fifth Floor, Boston, MA  02110-1301  USA
;
;
;

function sdss_flags::init, status=status
  self->read_flagstruct, status=status
  if status ne 0 then return, 0 else return,1
end 

;docstart::sdss_flags::read_flagstruct
;
; NAME:
;  sdss_flags::read_flagstruct
;
; PURPOSE:
;  Read the flag info into memory, store in a structure, and set
;  the internal pointers.  
;
; CALLING SEQUENCE:
;
;  si = sf->read_flagstruct
;
; RESTRICTIONS:
;  The flagvals.idl data file must exist in the expected place.
;     getenv("SDSSIDL_DIR")+'/data/flagvals.idl'
;
; MODIFICATION HISTORY:
;  Created: Mid-2005  Erin Sheldon Uchicago
;
;docend::sdss_flags::read_flagstruct

pro sdss_flags::read_flagstruct, status=status

  on_error, 2
  common sdss_flags_block, flagstruct, flagtypes

  status = 1
  flagdir = getenv("SDSSIDL_DIR")
  flagfile = concat_dir(flagdir,'data/flagvals.idl')

  nlines = numlines(flagfile)
  openr, lun, flagfile, /get_lun, error=ferror

  if ferror ne 0 then begin 
      print,!error_state.msg
      message,'Flags not loaded',/inf
      return
  endif 

  all_lines = ''
  line = ''
  while not eof(lun) do begin 
      readf, lun, line

      ;; remove comments
      line = ( strsplit(line, ';', /extract) )[0]
      ;; remove line continuations
      line = ( strsplit(line, '$', /extract) )[0]

      all_lines = all_lines + line
  endwhile 

  free_lun, lun
  if not execute(all_lines) then begin 
      message,'could not execute file: '+all_lines,/inf
      message,'Flags not loaded',/inf
      return
  endif 

  status = 0
  flagtypes = tag_names(flagstruct)

end 


;docstart::sdss_flags::flagtypes
;
; NAME:
;  sdss_flags::flagtypes()
;
; PURPOSE:
;  Return all available flag types. Should be at least:
;         OBJECT1, OBJECT2, PRIMTARGET, SECTARGET, STATUS
;
; CALLING SEQUENCE:
;
;  si = sf->flagtypes()
;
; MODIFICATION HISTORY:
;  Created: Mid-2005  Erin Sheldon Uchicago
;
;docend::sdss_flags::flagtypes

function sdss_flags::flagtypes
    common sdss_flags_block, flagstruct, flagtypes

    if n_elements(flagtypes) eq 0 then begin
        message,'flag info is not defined',/inf
        return, ''
    endif else begin
        return, flagtypes
    endelse
end 

;docstart::sdss_flags::flagnames
;
; NAME:
;  sdss_flags::flagnames()
;
; PURPOSE:
;  Return the flag names for a given flag type.
;
; CALLING SEQUENCE:
;
;  si = sf->flagnames(flagtype)
;  
; INPUTS:
;  flagtype: The flag type. Currently supported flags may be listed 
;      using the
;         IDL> ft = sf->flagtypes()
;         IDL> sf->print_flagdefs
;      methods, but is most likely at least
;         OBJECT1, OBJECT2, PRIMTARGET, SECTARGET, STATUS
; OUTPUTS:
;  The flag names for the input type. Returns '' for failure, and
;  status is set to 1.
;
; OPTIONAL OUTPUTS:
;  status: 1 if success, 0 if failure
;
; MODIFICATION HISTORY:
;  Created: Mid-2005  Erin Sheldon Uchicago
;
;docend::sdss_flags::flagnames

function sdss_flags::flagnames, flagtype, status=status

    if n_elements(flagtype) eq 0 then begin
        on_error, 2
        print,'-Syntax:  fn = sf->flagnames(flagtype, status=)'
        print
        message,'Halting'
    endif
    
    common sdss_flags_block, flagstruct, flagtypes

    status=1

    if not self->flagtype_exists(flagtype, tindex=tindex) then begin 
        return, '' 
    endif 
    
    status=0
    return,tag_names( flagstruct.(tindex) )

end 

function sdss_flags::flagname, flagtype, bit, status=status

	status=1
	if n_elements(flagtype) eq 0 or n_elements(bit) eq 0 then begin
		on_error, 2
        print,'-Syntax:  fn = sf->flagname(flagtype, bit, status=)'
        print
        message,'Halting'
    endif

	names = self->flagnames(flagtype, status=status)
	if status ne 0 then return,''

	maxbit = n_elements(names)-1
	if bit lt 0 or bit gt maxbit then begin
		message,'bit '+ntostr(bit)+' is out of range: 0 '+ntostr(maxbit)
	endif

	return, names[bit]
   
end

;docstart::sdss_flags::flagstruct
;
; NAME:
;  sdss_flags::flagstruct([flagtype], status=)
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
;  si = sf->flagstruct(flagstype)
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
;docend::sdss_flags::flagstruct

function sdss_flags::flagstruct, flagtype, status=status    

    common sdss_flags_block, flagstruct, flagtypes

    status=1

    if n_elements(flagstruct) eq 0 then begin
        message,'Flag info is not defined',/inf
        return, -1
    endif

    if n_elements(flagtype) eq 0 then begin
        status=0
        return, flagstruct
    endif else begin
        if not self->flagtype_exists(flagtype, tindex=tindex) then begin
            return, -1
        endif else begin
            status=0
            return, flagstruct.(tindex)
        endelse
    endelse

end

;docstart::sdss_flags::select_struct
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
;docend::sdss_flags::select_struct


function sdss_flags::select_struct, flagtype, default=default, status=status

    if n_elements(flagtype) eq 0 then begin
        on_error, 2
        print,'-Syntax: ss = sf->select_struct(flagtype, default=, status=)'
        print
        message,'Halting'
    endif

    common sdss_flags_block, flagstruct, flagtypes

    status=1

    ; Don't care by default
    if n_elements(default) eq 0 then default = 'D'

    if not self->flagtype_exists(flagtype, tindex=tindex) then begin
        message,'Unknown flagtype "'+ntostr(flagtype)+'"',/inf
        return, -1
    endif

    ; loop over all tags and create a select struct
    fs = flagstruct.(tindex)
    tags = tag_names(fs)
    ntags = n_elements(tags)
    select_struct = create_struct(tags[0], default) 
    for i=1,ntags-1 do begin
        select_struct = create_struct(select_struct, tags[i], default)
    endfor

    status=0
    return, select_struct

end


;docstart::sdss_flags::print_flagdefs
;
; NAME:
;  sdss_flags::print_flagdefs
;
; PURPOSE:
;  Print all flag info.
;
; CALLING SEQUENCE:
;  sf->print_flagdefs
;
; MODIFICATION HISTORY:
;  Created: Mid-2005  Erin Sheldon Uchicago
;
;docend::sdss_flags::print_flagdefs

pro sdss_flags::print_flagdefs

    common sdss_flags_block, flagstruct, flagtypes

    if n_elements(flagstruct) eq 0 then begin
        message,'Flag info is not defined',/inf
        return
    endif

    n=n_elements(flagtypes)

    print,'FlagType','FlagName','FlagVal',format = '(A20, A30, A15)'
    for i=0l, n-1 do begin 

        names = tag_names( flagstruct.(i) )
        nn = n_elements(names)

        for j=0l, nn-1 do begin 

            print,$
                flagtypes[i], names[j], flagstruct.(i).(j), $
                format = '(A20, A30, I15)'

        endfor 
        print
    endfor 

end 

;docstart::sdss_flags::flagtype_exists
;
; NAME:
;  sdss_flags::flagtype_exists()
;
; PURPOSE:
;  Check if a flagtype exists.  Returns 1 if the flagtype exists, else 0.  
;
; CALLING SEQUENCE:
;
;  si = sf->flagtype_exists(flagtype, tindex=)
;  
; INPUTS:
;  flagtype: The flag type. Currently supported flags may be listed 
;      using the
;         IDL> ft = sf->flagtypes()
;         IDL> sf->print_flagdefs
;      methods, but is most likely at least
;         OBJECT1, OBJECT2, PRIMTARGET, SECTARGET, STATUS, RUNSTATUS
;
; OPTIONAL OUTPUTS:
;   tindex: The flagtype index.
;
; MODIFICATION HISTORY:
;  Created: Mid-2005  Erin Sheldon Uchicago
;
;docend::sdss_flags::flagtype_exists

function sdss_flags::flagtype_exists, flagtype, tindex=tindex

    common sdss_flags_block, flagstruct, flagtypes

    if n_elements(flagtype) eq 0 then begin 
        on_error, 2
        print,'-Syntax: IF NOT sf->flagtype_exists(flagtype, tindex=) THEN ....'
        print,' tindex contains the index for flagtype'
        print
        message,'Halting'
    endif 

    if n_elements(flagtypes) eq 0 then begin
        message,'Flag info is not defined',/inf
        return, 0
    endif

    type = strupcase(flagtype[0])

    tindex = where(flagtypes eq type, nw)
    if nw eq 0 then return,0

    tindex = tindex[0]
    return,1

end 

;docstart::sdss_flags::flag_exists
;
; NAME:
;  sdss_flags::flag_exists()
;
; PURPOSE:
;  Check if the flag for a given flag type exists.  Returns 1 if the 
;  flag exists, else 0.  Both the flagtype and flag name must exist.  
;
; CALLING SEQUENCE:
;
;  si = sf->flag_exists(flagtype, flagname, tindex=)
;  
; INPUTS:
;  flagtype: The flag type. Currently supported flags may be listed 
;      using the
;         IDL> ft = sf->flagtypes()
;         IDL> sf->print_flagdefs
;      methods, but is most likely at least
;         OBJECT1, OBJECT2, PRIMTARGET, SECTARGET, STATUS, RUNSTATUS
;
;  flagname: A flag name for the given flag type. E.g for
;      PRIMTARGET: 'galaxy_red'.  The list of flag names for a 
;      given flag type may be gotten with:
;         IDL> fnames=sf->flagnames(flagtype)
;      or
;         IDL> sf->print_flagdefs
;
; OPTIONAL OUTPUTS:
;   tindex: The flagtype index.
;   nindex: The flagname index.
;
; MODIFICATION HISTORY:
;  Created: Mid-2005  Erin Sheldon Uchicago
;
;docend::sdss_flags::flag_exists

function sdss_flags::flag_exists, flagtype, flagname, tindex=tindex, nindex=nindex

    common sdss_flags_block, flagstruct, flagtypes

    if n_params() lt 2 then begin 
        on_error, 2
        print,'-Syntax: IF NOT sf->flag_exists(flagtype, flagname, tindex=, nindex=) THEN ....'
        print
        message,'Halting'
    endif 

    nindex=-1
    tindex=-1

    ;; string
    if size(flagtype, /type) eq 7 then begin 
        if not self->flagtype_exists(flagtype, tindex=tindex) then begin 
            return,0
        endif 
    endif else begin 
        tindex = flagtype
    endelse 

    name = strupcase(flagname[0])
    nindex = where( name eq tag_names(flagstruct.(tindex)), nn)

    if nn eq 0 then return,0 

    nindex = nindex[0]
    return, 1

end 

;docstart::sdss_flags::flag
;
; NAME:
;  sdss_flags::flag()
;
; PURPOSE:
;  Return the flag value given the input flag name.
;
; CALLING SEQUENCE:
;
;  si = sf->flag(flagtype, flagname, status=]
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
;  flagname: A flag name for the given flag type. E.g for
;      PRIMTARGET: 'galaxy_red'.  The list of flag names for a 
;      given flag type may be gotten with:
;         IDL> fnames=sf->flagnames(flagtype)
;      or
;         IDL> sf->print_flagdefs
;
; OPTIONAL OUTPUTS:
;   status=: 0 for success.
;
; EXAMPLES:
;   IDL> print,sdss_flag('target', 'galaxy_red')
;       32
;   if (primtarget and sdss_flag('target','galaxy_red')) ne 0 then .....
;
; MODIFICATION HISTORY:
;  Created: Mid-2005  Erin Sheldon Uchicago
;
;docend::sdss_flags::flag

function sdss_flags::flag, flagtype, flagname, status=status

    common sdss_flags_block, flagstruct, flagtypes

    status = 1
    if n_params() lt 2 then begin 
        on_error, 2
        print,'-Syntax: flag = sf->flag(flagtype, flagname, status=)'
        print
        message,'Halting'
    endif 

    if not self->flag_exists(flagtype, flagname, ti=ti, ni=ni) then begin 
        if ti eq -1 then begin 
            message,'Unknown flagtype: '+ntostr(flagtype),/inf
            return,-1
        endif 
        if ni eq -1 then begin 
            message,$
                'Unknown flagname "'+ntostr(flagname)+'" '+$
                'for flag type: '+ntostr(flagtype),/inf
            return,-1
        endif 
    endif 

    flagval = flagstruct.(ti).(ni)
    status = 0
    return, flagval

end 

;docstart::sdss_flags::andflags
;
; NAME:
;  sdss_flags::andflags()
;
; PURPOSE:
;  Check the bits in a set of input flags.  The flags, flag type, and
;  a structure describing the flags to check are input.  The flags are
;  ANDED together.  The indices of the input that match the selection 
;  criteria is returned, or -1 if none.
;
; CALLING SEQUENCE:
;
;  si = sf->andflags(flags, flagtype, select_struct, [nkeep, input_index=]
;  
; INPUTS:
;  flags: A set of flags.
;  flagtype: The flag type. Currently supported flags may be listed 
;      using the
;         IDL> print,sf->flagtypes() 
;      method, but is most likely at least
;         OBJECT1, OBJECT2, PRIMTARGET, SECTARGET, STATUS, RUNSTATUS
;  select_struct:  A structure with flag names and select criteria. The 
;      available flags can be listed with the method:
;         IDL> sf->print_flagdefs
;      The select criteria are
;         'y', 'n', or 'd' (yes it is set, no it isn't set, don't care)
;      e.g For OBJECT1 flags:
;         IDL> fs = {child: 'y', satur: 'n'}
;      The flag names for a given flag type may be gotten:
;         IDL> fnames=sf->flagnames(flagtype)
;      or
;         IDL> sf->print_flagdefs
;
;
;
; OPTIONAL INPUTS:
;   input_index=: A beginning index to restrict the search.
;
; OPTIONAL OUTPUTS:
;   nkeep: The number that matched the selection criteria.
;
; MODIFICATION HISTORY:
;  Created: Mid-2005  Erin Sheldon Uchicago
;
;docend::sdss_flags::andflags

function sdss_flags::andflags, flags, flagtype, select_struct, nkeep, input_index=input_index

    common sdss_flags_block, flagstruct, flagtypes

    status = 1
    nkeep = 0
    if n_params() lt 3 then begin 
        on_error, 2
        print,'-Syntax: keep = sf->andflags(flags, flagtype, select_struct, [nkeep, input_index=])'
        print,'AND the result of flag selection'
        print
        message,'Halting'
    endif 

    if not self->flagtype_exists(flagtype, tindex=ti) then begin 
        message,'Unknown flagtype: '+ntostr(flagtype)
    endif 

    ;; only check flags that have an entry in our flag list
    intags = tag_names(select_struct)
    ftags = tag_names( flagstruct.(ti))
    match, ftags, intags, mf, minput
  
    if mf[0] eq -1 then begin 
        message,$
            'None of the input flag names from input select_struct are known',/inf
        return,-1
    endif 

    ;; indices of kept objects
    nflags = n_elements(flags)
    if n_elements(input_index) eq 0 then keep = lindgen(nflags) $
    else keep = input_index

    ;; loop over the matches

    nmatch = n_elements(mf)
    for i=0l, nmatch-1 do begin 

        fi = mf[i]
        mi = minput[i]

        flagval = flagstruct.(ti).(fi)
        if strupcase(select_struct.(mi)) eq 'Y' then begin 
            w = where( (flags[keep] and flagval) ne 0, nw)
        endif else begin 
            w = where( (flags[keep] and flagval) eq 0, nw) 
        endelse 
        if nw eq 0 then return,-1
        keep = keep[w]

    endfor 

    nkeep = n_elements(keep)
    status = 0
    return,keep

end 

;docstart::sdss_flags::orflags
;
; NAME:
;  sdss_flags::orflags()
;
; PURPOSE:
;  Check the bits in a set of input flags.  The flags, flag type, and
;  a structure describing the flags to check are input.  The flags are
;  OR-ED together.  The indices of the input that match the selection 
;  criteria is returned, or -1 if none.
;
; CALLING SEQUENCE:
;
;  si = sf->orflags(flags, flagtype, select_struct, [nkeep, input_index=]
;  
; INPUTS:
;  flags: A set of flags.
;  flagtype: The flag type. Currently supported flags may be listed 
;      using the
;         IDL> print,sf->flagtypes() 
;      method, but is most likely at least
;         OBJECT1, OBJECT2, PRIMTARGET, SECTARGET, STATUS, RUNSTATUS
;  select_struct:  A structure with flag names and select criteria. The 
;      available flags can be listed with the method:
;         IDL> sf->print_flagdefs
;      The select criteria are
;         'y', 'n', or 'd' (yes it is set, no it isn't set, don't care)
;      e.g For OBJECT1 flags:
;         IDL> fs = {child: 'y', satur: 'n'}
;      The flag names for a given flag type may be gotten:
;         IDL> fnames=sf->flagnames(flagtype)
;      or
;         IDL> sf->print_flagdefs
;
;
; OPTIONAL INPUTS:
;   input_index=: A beginning index to restrict the search.
;
; OPTIONAL OUTPUTS:
;   nkeep: The number that matched the selection criteria.
;
; MODIFICATION HISTORY:
;  Created: Mid-2005  Erin Sheldon Uchicago
;
;docend::sdss_flags::orflags

function sdss_flags::orflags, flags, flagtype, select_struct, nkeep, input_index=input_index

  if n_params() lt 3 then begin 
      print,'-Syntax: keep = sf->orflags(flags, flagtype, select_struct, [nkeep, input_index=])'
      print,'OR the results of the flag selection'
      print
      message,'Halting'
  endif 

  tn = tag_names(select_struct)
  nt = n_elements(tn)

  nkeep = 0ll
  for i=0l, nt-1 do begin 
      send_struct = create_struct(tn[i], select_struct.(i))
      si = self->andflags(flags, flagtype, send_struct, tnkeep, $
                             input_index=input_index)

      if tnkeep ne 0 then begin 
          add_arrval, si, select_index
          nkeep = nkeep + tnkeep
      endif 
  endfor 

  if nkeep ne 0 then begin 
      rmd = rem_dup(select_index)
      if n_elements(rmd) lt nkeep then begin 
          select_index = select_index[rmd]
          nkeep = n_elements(select_index)
      endif 
  endif else begin 
      select_index = -1
      nkeep = 0ll
  endelse 

  return, select_index
end 

;docstart::sdss_flags::flag_select
;
; NAME:
;  sdss_flags::flag_select()
;
; PURPOSE:
;  Check the bits in a set of input flags.  The flags, flag type, and
;  a structure describing the flags to check are input.  The flags can
;  be ANDED or OR-ED together.  The indices of the input that match
;  the selection criteria is returned, or -1 if none.
;
; CALLING SEQUENCE:
;
;  si = sf->flag_select(flags, flagtype, select_struct, [nkeep, 
;                       /orflags, input_index=]
;  
; INPUTS:
;  flags: A set of flags.  May be an array.
;  flagtype: The flag type. Currently supported flags may be listed 
;      using the
;         IDL> print,sf->flagtypes() 
;      method, but is most likely at least
;         OBJECT1, OBJECT2, PRIMTARGET, SECTARGET, STATUS, RUNSTATUS
;
;  select_struct:  A structure with flag names and select criteria. The 
;      available flags can be listed with the method:
;         IDL> sf->print_flagdefs
;      The select criteria are
;         'y', 'n', or 'd' (yes it is set, no it isn't set, don't care)
;      e.g For OBJECT1 flags:
;         IDL> fs = {child: 'y', satur: 'n'}
;      The flag names for a given flag type may be gotten:
;         IDL> fnames=sf->flagnames(flagtype)
;      or
;         IDL> sf->print_flagdefs
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
;   IDL> sf=obj_new('sdss_flags')
;   IDL> fst = {galaxy_red:  'Y'}
;   IDL> si = sf->flag_select(st.primtarget, 'primtarget', fst}
;
;   IDL> fst = {child: 'y', satur: 'n'}
;   IDL> si = sf->flag_select(st.flags[2], 'object1', fst)
;
;   ; Choose either lrg or main sample galaxies usign the /orflags option.
;   IDL> fst = {galaxy_red: 'Y', galaxy: 'Y'}
;   IDL> si = sf->flag_select(st.primtarget, 'primtarget', fst, /orflags}
;
;
; MODIFICATION HISTORY:
;  Created: Mid-2005  Erin Sheldon Uchicago
;
;docend::sdss_flags::flag_select

function sdss_flags::flag_select, flags, flagtype, select_struct, nkeep, orflags=orflags, input_index=input_index

  if n_params() lt 3 then begin 
      on_error, 2
      print,'-Syntax: keep = sf->flag_select(flags, flagtype, select_struct, [nkeep, /orflags, input_index=])'
      print
      message,'Halting'
  endif 

  if keyword_set(orflags) then begin 
      si=self->orflags(flags, flagtype, select_struct, nkeep, $
                       input_index=input_index)
  endif else begin 
      si=self->andflags(flags, flagtype, select_struct, nkeep, $
                        input_index=input_index)
  endelse 
  return, si

end 



;docstart::sdss_flags::plotflags
;
; NAME:
;   plotflags
;
; PURPOSE:
;   Plot a histogram of the flags set for a set of objects.
;
; CALLING SEQUENCE:
;   sf->plotflags, flags, flagtype, _extra=_extra
;
; INPUTS:
;   flags: a flag array.
;   flagtype: The flag type. Currently supported flags may be listed 
;      using the
;         IDL> print,sf->flagtypes() 
;      method, but is most likely at least
;         OBJECT1, OBJECT2, PRIMTARGET, SECTARGET, STATUS, RUNSTATUS
;
; OPTIONAL INPUTS:
;   _extra: Extra plotting keywords.
;
; EXAMPLE:
;   sf=obj_new('sdss_flags')
;   sf->plotflags, struct.primtarget, 'primtarget'
;
; MODIFICATION HISTORY:
;   Generalized from old program.  2006-12-3, Erin Sheldon, NYU.
;
;docend::sdss_flags::plotflags

pro sdss_flags::plotflags, flags, flagtype, frac=frac, title=title, xtitle=xtitle, ytitle=ytitle, label_charsize=label_charsize, _extra=_extra

    if n_elements(flags) eq 0 or n_elements(flagtype) eq 0 then begin
        on_error, 2
        print,'-Syntax: sf->plotflags, flags, flagtype, /frac, _extra=_extra'
        print
        message,'Halting'
    endif

    log2 = alog(2)

    flagnames = self->flagnames(flagtype, status=status)    
    if status ne 0 then message,'Could not get flagnames for type: ',flagtype

    if n_elements(title) eq 0 then title=flagtype+' flag bits'
    if n_elements(ytitle) eq 0 then begin
		if keyword_set(frac) then begin
			ytitle='Fraction'
		endif else begin
			ytitle='Number'
		endelse
	endif

    nf = n_elements(flagnames)
	flagbits = intarr(nf)
	flaghist=lon64arr(nf)
    for i=0L, nf-1 do begin
        flagval = self->flag(flagtype, flagnames[i])

		flagbit = fix( rnd(alog(flagval)/log2) )
		flagbits[i] = flagbit

        w=where( (flags and flagval) ne 0, nw)
		flaghist[i] = nw
    endfor


    if n_elements(flaghist) ne 0 then begin
		if keyword_set(frac) then flaghist=flaghist/float(n_elements(flags))

        pplot, flagbits, flaghist, psym=10, position=[0.1,0.3,0.85,0.95],$
            xtitle=xtitle, ytitle=ytitle, title=title, _extra=_extra, $
			xminor=1, $
			xrange=[0,31], $
			xticks=32-1, xtickf='(a1)'


		ypos = replicate(!y.window[0] - 0.02, 32)
		xpos = !x.window[0] + (!x.window[1] - !x.window[0]) * findgen(32)/31

		names=flagnames
		names = ntostr(flagbits,f='(i02)')+' '+names
		for i=0L, n_elements(names)-1 do begin
			xyouts, xpos[i], ypos[i], names[i], alignment=0.0, $
				orientation=-45, charsize=label_charsize, $
				/normal
		endfor

    endif else begin
        print,'No flags set'
    endelse
end

pro sdss_flags::_plot_label_setup, flagtype
	self.sdss_flags_tempstring = flagtype
end
function sdss_flags::_plot_label, axis, index, value
	return, self->flagname(self.sdss_flags_tempstring, value)
end

;docstart::sdss_flags::printflags
;
; NAME:
;   printflags
;
; PURPOSE:
;   Print all flags that are set.
;
; CALLING SEQUENCE:
;   sf->printflags, flags, flagtype, _extra=_extra
;
; INPUTS:
;   flags: a flag array.
;   flagtype: The flag type. Currently supported flags may be listed 
;      using the
;         IDL> print,sf->flagtypes() 
;      method, but is most likely at least
;         OBJECT1, OBJECT2, PRIMTARGET, SECTARGET, STATUS, RUNSTATUS
;
; EXAMPLE:
;   sf=obj_new('sdss_flags')
;   sf->printflags, struct.flags2, 'object2'
;
; MODIFICATION HISTORY:
;   Generalized from old program.  2006-12-3, Erin Sheldon, NYU.
;
;docend::sdss_flags::printflags

pro sdss_flags::printflags, flags, flagtype

    nflag = n_elements(flags)
    ntype = n_elements(flagtype)
    
    if nflag eq 0 or ntype eq 0 then begin
        on_error, 2
        print,'-Syntax: sf->printflags, flags, flagtype'
        print
        message,'Halting'
    endif

    flagnames = self->flagnames(flagtype, status=status)    
    if status ne 0 then message,'Could not get flagnames for type: ',flagtype

    nf=n_elements(flagnames)
    for i=0L, nf-1 do begin
        flag = self->flag(flagtype, flagnames[i])
        if ( (flags[0] and flag) ne 0 ) then begin
           print,flagtype+'::'+strlowcase(flagnames[i]) 
        endif
    endfor

end

function sdss_flags::cleanup
  return,1
end 

pro sdss_flags__define

  struct = {$
             sdss_flags, $
             sdss_flags_tempstring: '' $
           }

end 
