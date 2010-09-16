;+
; NAME:
;   PHOTOID
;
;
; PURPOSE:
;   Create unique super id from run,rerun,camcol,field,id
;
;
; CATEGORY:
;   SDSS Specific routine.
;
;
; CALLING SEQUENCE:
;   superid = photoid(run,rerun,camcol,field,id)
;    OR
;   superid = photoid(struct)
;
;
; INPUTS:
;   run,rerun,camcol,field,id (may be arrays)
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



function photoid, run, rerun, camcol, field, id, old=old

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


    np = n_params()
    if n_params() eq 1 then begin 

        super = ulong64(run.id)*ten^p1
        super = temporary(super) + ulong64(run.field)*ten^p2
        super = temporary(super) + ulong64(run.camcol)*ten^p3
        super = temporary(super) + ulong64(run.rerun)*ten^p4 
        super = temporary(super) + ulong64(run.run)*ten^p5

    endif else if np eq 5 then begin 

        nr   = n_elements(run)
        nrer = n_elements(rerun)
        nc   = n_elements(camcol)
        nf   = n_elements(field)
        nid  = n_elements(id)

        if total([nr,nrer,nc,nf,nid]) ne 5*nr then begin 
            print,'All arrays must be same size'
            return, -1
        endif 

        super = ulong64(id)*ten^p1
        super = temporary(super) + ulong64(field)*ten^p2
        super = temporary(super) + ulong64(camcol)*ten^p3
        super = temporary(super) + ulong64(rerun)*ten^p4
        super = temporary(super) + ulong64(run)*ten^p5

    endif else begin 
        print,'Syntax: superid=photoid(run,rerun,camcol,field,id)'
        print,'  OR '
        print,'Syntax: superid=photoid(struct)'
        return,-1
    endelse 

    return,super

end 
