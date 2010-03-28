;+
; NAME:
;   rev2ind
;
; PURPOSE:
;   Extract the list of indices from the reverse_indices structure created
;   by histogram().  Either return an array of *all* the indices or a 
;   pointer list of indices in each bin, which is an alternative representation
;   from that is more useful in certain situations.
;
; CALLING SEQUENCE:
;   ind=rev2ind(rev)
;
; INPUTS:
;   rev: A reverse indices array returned by histogram.
;
; KEYWORD PARAMETERS:
;   /bybin: Rather than returning an array of all indices return an array
;       of pointers, pointing to the indices for each bin of the histogram.
;       Empty bins contain pointers to the integer -1
;
; OUTPUTS:
;   Either the array of all indices or the pointer array for each bin.  Note
;   the indices are not sorted.
;
; PROCEDURE:
;   By default this simply returns all the indices:
;       nbin=rev[0]-1
;       return, rev[nbin+1:n_elements(rev)-1]
;   If /bybin is sent a ptrarr(nbin) is returned
;
; EXAMPLE:
;   h=histogram(x, min=0.1, max=2.3, binsize=0.01, rev=rev)
;   w=rev2ind(rev)
;   ; plot the ones that match the min,max criteria
;   plot, x[w], y[w], psym=3
;
; MODIFICATION HISTORY:
;   Created: 2008-17-03, Erin Sheldon, NYU
;
;-

function rev2ind, rev, bybin=bybin

    if n_elements(rev) eq 0 then begin
        message,'usage: ind=rev2ind(rev)'
    endif

    ; Determine the number of bins
    nbin=rev[0]-1

    ; By default just return the list of all indices
    if not keyword_set(bybin) then begin
        ind = rev[nbin+1:n_elements(rev)-1]
        return, ind
    endif

    ; Pointer list
    ptrlist = ptrarr(nbin)
    for i=0L, nbin-1 do begin
        if rev[i] ne rev[i+1] then begin
            w=rev[ rev[i]:rev[i+1]-1 ]
            ptrlist[i] = ptr_new(w, /no_copy)
        endif else begin
            ptrlist[i] = ptr_new(-1)
        endelse
    endfor
    
    return, ptrlist

end
