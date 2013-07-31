;+
; NAME:
;   boot_samples
;
; PURPOSE:
;   Create bootstrap data for input variables. The input data are up to 
;   8 equal length arrays where each element represents information about 
;   the same event or object. For example they could be the flux in different 
;   wavelenghts of some astronomical object, or parameters of an event in a 
;   detector. The output is the mean of each bootstrap sample, for each input
;   variable, in an [nsample, nvar] array, or 1-d for nvar=1.  
;
; CATEGORY:
;   Statistics.
;
; CALLING SEQUENCE:
;   samples = boot_samples(nsample, v1, ..., v8, /verbose)
;
; INPUTS:
;   nsample: Number of bootstrap samples to perform.
;   v1..v8: Up to 8 data arrays of equal length.  The number of 8 comes
;       from the IDL limit of 8 dimensions in an array.
;
; KEYWORD PARAMETERS:
;   /verbose: Report progress.
;
; OUTPUTS:
;   samples: The mean of each bootstrap sample in a [nsample, nvar] array.
;       The reform() program is run to reduce [nsample,1] arrays.
;
; EXAMPLE:
;   bs = bootstrap(1000, x, y)
;
; MODIFICATION HISTORY:
;   Documented: 2007-04-24, Erin Sheldon, NYU
;
;-

pro _boot_samples_format, nsamp, format, len
    ilen = ntostr( strlen(ntostr(nsamp-1)) )
    format='(a, i0'+ilen+', $)'
    tstr = string('Sample: ', 0, f=form)
    len = strlen(tstr)
end
function _boot_samples_inputparse, d1, d2, d3, d4, d5, d6, d7 ,d8
    dinput = intarr(8)
    for i=1,8 do begin
        istr=string(i,f='(i0)')
        comm = 'n=n_elements(d'+istr+')'
        if not execute(comm) then message,'Failed to count d'+istr
        dinput[i-1] = n
    endfor
    return, dinput
end

function boot_samples, nsamp, d1, d2, d3, d4, d5, d6, d7, d8, verbose=verbose
    
    on_error, 2
    dinput = _boot_samples_inputparse(d1,d2,d3,d4,d5,d6,d7,d8)
    wthere=where(dinput ne 0, nvar)
    if nvar eq 0 or n_elements(nsamp) eq 0 then begin
        print,'-Syntax: bsamp = boot_samples(nsamp, v1,...v8, /verbose)'
        message,'Halting'
    endif

    wbad=where(dinput[wthere] ne dinput[wthere[0]], nbad)
    if nbad ne 0 then message,'All input variables must be same length'

    ndata = dinput[0]

    samples = dblarr(nsamp, nvar)
    _boot_samples_format, nsamp, form, len
    for si=0L, nsamp-1 do begin
        if keyword_set(verbose) then begin
            print, backspace(len)+'Sample: ',i, f=form
        endif
        bind = boot_indices(ndata)

        for vi=0L, nvar-1 do begin
            i = wthere[vi]
            istr = string(i,f='(i0)')
            comm = 'bdata = d'+istr+'[bind]'
            if not execute(comm) then message,'Could not extract from d'+istr
            samples[si, i] = mean(bdata)
        endfor
    endfor

    ; no copy made
    samples = reform(samples, /overwrite)
    return, samples

end
