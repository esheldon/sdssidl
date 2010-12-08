;+
;
; This function just calls sdss_photoid.  See the documentation for that
; function for more details.
;
;-


function photoid, run, rerun, camcol, field, id, old=old

    return,sdss_photoid(run,rerun,camcol,field,id,old=old)

end 
