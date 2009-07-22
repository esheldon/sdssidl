
;+
; NAME:
;    CETA2STRIPENUM
; PURPOSE:
;    Find which stripe goes with this eta.
; CALLING SEQUENCE:
;    stripe = ceta2stripenum(ceta, /allowfan)
; INPUTS: 
;    ceta: Corrected SDSS survey longitude.
; KEYWORD PARAMETERS:
;   /closest:  In the southern stripes take the closest of stripes 76, 82, 86.
;       This allows for the fanning out of runs near the ends of stripes.
; OUTPUTS: 
;    stripe
; REVISION HISTORY:
;    14-NOV-2000 Taken from astrotools.  Works with modified corrected coords.
;-                                       

function ceta2stripenum, ceta, closest=closest

    if n_params() lt 1 then begin 
        print,'-Syntax: stripe = ceta2stripenum(ceta, /closest)'
        on_error, 2
        message,'Halting'
    endif 

    stripesep = 2.5
    stripe = fix( (ceta+58.75)/stripesep )

    if keyword_set(closest) then begin
        w=where(stripe gt 76, nw)
        if nw ne 0 then begin
            mst = [76, 82, 86]
            for i=0L, nw-1 do begin
                diff = abs( (stripe[w[i]]-mst) )
                mind = min(diff, mini)
                stripe[w[i]] = mst[mini]
            endfor
        endif
    endif

    return, stripe

end 
