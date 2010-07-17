;+
; NAME:
;   boot_hist
;
; PURPOSE:
;   Create a histogram with bootstrapped errors.
;
; CATEGORY:
;   statistics
;
; CALLING SEQUENCE:
;   binstruct = boot_hist(nsample, data, weights=, indarray=, /silent, 
;                           bhist=, wbhist=wbhist, 
;                           _extra=histogram keywords)
;
; INPUTS:
;   nsample: Number of bootstrap samples to create.
;   data: An array of data.
;
; OPTIONAL INPUTS:
;   weights: Weights for each point.
;   _extra: Keywords sent to histogram such as binsize, min, max, etc. 
;       Keywords special to the binner() program are also accepted.
;
; KEYWORD PARAMETERS:
;   /silent: Do not print progress reports.
;
; OUTPUTS:
;   binstruct: A structure with bin info, incliding bin centers, histogram
;       optional weighted histogram, and errors from bootstrapping.
;
; OPTIONAL OUTPUTS:
;   bhist: The histogram bootstrap samples used.
;   wbhist: The weighted histogram bootstrap samples used.
;
; MODIFICATION HISTORY:
;   2007-04-24, Documented, Erin Sheldon, NYU
;
;-
pro _boot_hist_format, nsamp, format, len
    ilen = ntostr( strlen(ntostr(nsamp-1)) )
    format='(a, i0'+ilen+', $)'
    tstr = string('Sample: ', 0, f=form)
    len = strlen(tstr)
end

function boot_hist, nsamp, data, weights=weights, indarray=indarray, $
        bhist=bhist, wbhist=wbhist, $
        silent=silent, _extra=_extra

    ndata = n_elements(data)
    nweights = n_elements(weights)

    nind = n_elements(indarray)
    if nind ne 0 then begin
        sz=size(indarray)
        if sz[0] ne 2 then message,'indarray must be a 2-d array'
        if sz[1] ne nsamp or sz[2] ne ndata then begin
            message,'indarray must be an [nsamp,ndata] array'
        endif
    endif

    if nweights ne 0 then begin
        if nweights ne ndata then message,'data and weights must be same length'
    endif
    
    ; first the standard binning
    binstruct = binner(data, weights=weights, _extra=_extra)


    if not keyword_set(silent) then print,'Bootstrapping histogram'
    _boot_hist_format, nsamp, form, len
    for i=0L, nsamp-1 do begin
        if not keyword_set(silent) then print, backspace(len)+'Sample: ',i, f=form
   
        if nind ne 0 then begin
            bind = indarray[i, *]
        endif else begin
            bind = boot_indices(ndata) 
        endelse

        bdata = data[bind]
        if nweights ne 0 then bweights = weights[bind]
        bs = binner(bdata, weights=bweights, _extra=_extra)

        if i eq 0 then begin
           nbin = n_elements(bs.hist) 
           bhist = dblarr(nsamp, nbin)
           if nweights ne 0 then wbhist=dblarr(nsamp,nbin)
        endif

        bhist[i,*] = bs.hist
        if nweights ne 0 then begin
            wbhist[i,*] = bs.whist
        endif 

    endfor
    if not keyword_set(silent) then print

    ts = boot_stats(bhist)
    binstruct = create_struct(binstruct, 'hist_err', ts.sdev)
    if nweights ne 0 then begin
        wts = boot_stats(wbhist)
        binstruct = create_struct(binstruct, 'whist_err', wts.sdev)
    endif
    return, binstruct
end


