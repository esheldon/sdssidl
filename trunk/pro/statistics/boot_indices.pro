;+
; NAME:
;   boot_indices
; PURPOSE:
;   Returns random indices between 0 and imax-1 drawn with replacement for use in bootstrapping.
;   If only imax is sent, then imax randoms are returned in this range.  
;   If the nsample argument is sent and is not 1 then a [nsample,imax]
;   array is returned.
; CATEGORY:
;   statistics.
; CALLING SEQUENCE:
;   ind = boot_indices(imax, nsample=)
; INPUTS:
;   imax: Randoms are returned with values from 0 to imax-1. By default
;       the returned array is also this length.
; OPTIONAL INPUTS:
;   nsample: If not 1 an [nsample,imax] array is returned.
; OUTPUTS:
;   Random indices from 0 to imax-1 drawn with replacement.
; MODIFICATION HISTORY:
;   2007-04-24 Documented, Erin Sheldon, NYU
;-
function boot_indices, imax, nsample=nsample
    on_error, 2
    if n_elements(imax) eq 0 then begin
        print,'-Syntax: bind = boot_indices(imax, nsample=)'
        message,'Halting'
    endif

    if n_elements(nsample) eq 0 then nsample=1

    if imax gt (2L^31-1) then l64=1

    ntot = long64(imax)*nsample

    indarr = round( (imax-1)*randomu(seed, ntot), l64=l64)

    if nsample ne 1 then indarr = reform(indarr, nsample, imax, /overwrite)
    return, indarr
end

