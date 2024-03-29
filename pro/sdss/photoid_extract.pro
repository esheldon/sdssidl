;+
; NAME:
;   PHOTOID_EXTRACT
;
; PURPOSE:
;   Extract run,rerun,camcol,field,id from a superid (Ulong64) created with
;   the photoid.pro function.
;
; CATEGORY:
;   SDSS specific routine
;
; CALLING SEQUENCE:
;   photoid_extract, super, run, rerun, camcol, field, id
;
; INPUTS:
;   super: A super id created using the photoid.pro function
;
; Keywords:
;   /old: If set, the old exponents are used.  These were chosen
;       poorly and should not be used for new ids.
;
; OUTPUTS:
;   run,rerun,camcol,field,id
;
; EXAMPLE:
;   superid = photoid(run,rerun,camcol,field,id)
;   photoid_extract, superid, run, rerun, camcol, field, id
;
; MODIFICATION HISTORY:
;   Created: 5/30/2003 Erin Sheldon UofChicago
;
;-


PRO photoid_extract, super, run, rerun, camcol, field, id, old=old

    if n_params() lt 1 then begin 
        print,'-Syntax: photoid_extract, super, run, rerun, camcol, field, id, /old'
        print,' super index cannot be floating point'
        return
    endif 

    ten = 10ULL

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

    run = $
        super/ten^p5
    rerun = $
        super/ten^p4 - run*ten^(p5-p4)
    camcol = $
        super/ten^p3 - run*ten^(p5-p3) - rerun*ten^(p4-p3)
    field = $
        super/ten^p2 - run*ten^(p5-p2) - rerun*ten^(p4-p2) - camcol*ten^(p3-p2)
    id =  $
        super/ten^p1 - run*ten^(p5-p1) - rerun*ten^(p4-p1) - camcol*ten^(p3-p1) - field*ten^(p2-p1)

    run = long(run)
    rerun = fix(rerun)
    camcol = fix(camcol)
    field = fix(field)
    id = long(id) 

  return
END 
