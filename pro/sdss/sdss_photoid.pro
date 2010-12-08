;+
; NAME:
;   sdss_photoid
;
; PURPOSE:
;   Create unique super id from run,rerun,camcol,field,id
;
; CATEGORY:
;   SDSS Specific routine.
;
;
; CALLING SEQUENCE:
;   superid = sdss_photoid(run,[rerun,camcol,field,id])
;    OR
;   superid = sdss_photoid(struct)
;
;
; INPUTS:
;   run,rerun,camcol,field,id (may be arrays)
;       You can also just subsets if in order, e.g.
;           run
;           run,rerun,camcol
;       zeros will be used for the others.
;    OR
;   struct: must contain the above (may be array of structs)
;
; Keywords:
;   /old: If set, the old exponents are used.  These were chosen
;       poorly and should not be used for new ids.
;
; OUTPUTS:
;   A superid is returned
;
; MODIFICATION HISTORY:
;   Created ??-??-2002 Erin Sheldon UofMichigan
;
;
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
;-

pro sdss_photoid_usage
    print,'usage:'
    print,'    super=sdss_photoid(run,[rerun,camcol,field,id])'
    print,'all arrays must be the same size'
    print
    print,'alternative syntax:'
    print,'    super=sdss_photoid(struct)'
    print
    message,'halting'
end


function sdss_photoid, run, rerun, camcol, field, id, old=old

    ten=ulong64(10)
    if keyword_set(old) then begin
        p1 = 0LL
        p2 = 6LL
        p3 = 11LL
        p4 = 12LL
        p5 = 15LL
    endif else begin
        p1 = 0LL
        p2 = 4LL
        p3 = 8LL
        p4 = 9LL
        p5 = 13LL
    endelse

    if n_tags(run) ne 0 then begin
        ; a struct was entered
        super = $
            ulong64(run.id)*ten^p1     + $
            ulong64(run.field)*ten^p2   + $
            ulong64(run.camcol)*ten^p3 + $
            ulong64(run.rerun)*ten^p4  + $
            ulong64(run.run)*ten^p5

    endif else begin
        npar = n_params()
        nr   = n_elements(run)

        if nr eq 0 then begin
            sdss_photoid_usage
        endif
      
        super = ulong64(run)*ten^p5

        if npar gt 1  then begin
            if n_elements(rerun) ne nr then sdss_photoid_usage
            super = super + ulong64(rerun)*ten^p4

            if npar gt 2 then begin
                if n_elements(camcol) ne nr then sdss_photoid_usage
                super = super + ulong64(camcol)*ten^p3  

                if npar gt 3 then begin
                    if n_elements(field) ne nr then sdss_photoid_usage
                    super = super + ulong64(field)*ten^p2

                    if npar gt 4 then begin
                        if n_elements(id) ne nr then sdss_photoid_usage
                        super = super + ulong64(id)*ten^p1
                    endif
                endif
            endif
        endif

    endelse

    return, super

end 
