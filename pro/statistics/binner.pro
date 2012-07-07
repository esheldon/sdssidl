
;+
; NAME:
;   binner
;
; PURPOSE:
;   Generic binning routine.  Bins 1- or 2-d data sets and supports weighted
;   histograms and binning into groups with N elements, as well as the usual
;   fixed binsize.  For 1-d it generates histograms, returning info about bin 
;   edges and centers and allowing weighted histograms.  For 2-d the data are 
;   binned by x and the y values are averaged in each bin using a specified 
;   method.  NOTE for 2-d, bins containing no objects are not returned in the 
;   result.  The result is a structure containing the relevant information.
;
; CATEGORY:
;   statistics
;
; CALLING SEQUENCE:
;   binstruct = binner(x [, y, (all histogram keywords), weights=, method=, nperbin=, reverse_indices=, indices=, index_array=)
;
; INPUTS:
;   x: Array of data.
;   y (optional): Optional y data.  If y is sent, the data are binned in x 
;     and statistics are also returned for the y data in each bin.  Note in
;     this case bins with zero members are not included in the result.
;
; OPTIONAL KEYWORD INPUTS:
;   _extra: All the inputs which can be sent to histogram, such as binsize, 
;       min, max, etc get passed through the _extra argument.  These will be
;       used when appropriate for the requested method.
;   method: Allowed values depend on 1-d or 2-d.  
;       For 1-d:
;           'full': Calculate more statistics for each bin. This requires using
;               reverse indices from histogram, which is slower and uses more 
;               memory. These default to the center of the bin when no data 
;               is present.
;                   xmean: the mean position in the bin 
;                   xmedian: the median position in the bin.
;           'weighted': if sent, a weighted histogram "whist" is also returned. 
;               Weights must also be sent, and in fact sending weights without
;               sending method implies method='weighted'.  When weighted is sent, this also
;               implies 'full' as described above.
;           'standard': just run histogram and return bin stats. DEFAULT
;       For 2-d:
;           'mean': Calculate all stats using standard mean, variance, etc. DEFAULT
;           'median': Use the median instead of the mean.
;           'sigmaclip': Use sigma clipping to find the mean.  nsig=3.5, niter=4
;           'weighted': Calculate weighted statistics. Weights must be sent, and 
;               sending weights without sending method implies method='weighted'.
;
;   weights: Weight for each point. Will be used for weighted histogram and 
;       weighted statistics in the 2-d case.  Inclusion of this keyword implies
;       method='weighted' if that keyword is not sent.
;   nperbin: Bin such that there this many points per bin.  If this keyword is not
;       sent then equal sized bins are used according to the histogram keywords.
;
; OUTPUTS:
;  binstruct: A structure containing statistics for the requested binning.
;       1-d: 
;           method: the method used, 'full', 'weighted', or 'standard'
;           xlow: lower side of bin
;           xcenter: center of bin
;           xhigh: right size of bin
;           hist: the histogram
;           if method is 'full' or 'weighted'
;               xmean: mean of data in bin
;               xmedian: median of data in bin
;           If weighted:
;               whist: the weighted histogram, sum of weights in each bin
;
;       2-d:
;           'method': method for calculating statistics
;           xmean: mean if x data in bin
;           ymean: mean if y data in bin, calculated using 'method'
;           yerr:  error on mean in bin
;           ysdev: standard deviation in bin
;           hist: histogram of data in the bins
;           if method is 'weighted':
;               whist: the weighted histogram, sum of weights in each bin
;        
;
; OPTIONAL OUTPUTS:
;   reverse_indices: from histogram.  
;   indices: An array of indices for all elements that passed the criteria
;       passed to histogram, e.g. min=, max=. Extracted from reverse_indices
;   index_array: An array of pointers each of which points to
;       an array of indices for objects in the bin. 
;
; RESTRICTIONS:
;   Requires IDL 5.6 or later for the locations keyword to histogram. Only
;   a restriction for the 1-d case when not binning by number.
;
; EXAMPLE:
;   1-d:
;       bs = binner(x, min=0.1, max=20)
;       pplot, bs.xcenter, bs.hist, psym=10
;       
;       bs = binner(x, method='weighted', weights=wts)
;       pplot, bs.xcenter, bs.whist, psym=10
;   2-d:
;       bs = binner(x, y, method='median')
;       pplot, bs.xmean, bs.ymean, yerr=bs.yerr, psym=8
;
;       bs = binner(x, y, method='weighted', weights=wts)
;       pplot, bs.xmean, bs.ymean, yerr=bs.yerr, psym=8
;   
; MODIFICATION HISTORY:
;   Creation: Conglomerated from many programs. 2007-04-04 Erin Sheldon, NYU
;
;-
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
;
;


; all arg error checking done in main program
function binner_collect_statistics, x, y, hist, revind, method, $
        weights=weights

    wb = where(hist gt 0, nbin)
    if nbin eq 0 then return,-1

    method = strlowcase(method)
    allowed_methods = ['mean','median','sigmaclip','weighted']
    w=where(allowed_methods eq method, nw)
    if nw eq 0 then message,'Unknown method: '+ntostr(method)

    for ibin=0l, nbin-1 do begin 

        binnum = wb[ibin]

        if revind[binnum] ne revind[binnum+1] then begin 
            w=revind[ revind[binnum]:revind[binnum+1]-1 ]

            nw=n_elements(w)
            if nw ge 2 then begin 
                if method eq 'weighted' then begin 
                    txmean = total(x[w]*weights[w])/total(weights[w])
                    wmom, y[w], dummy, tymean, tysdev, tyerr, $
                        weights=weights[w]
                    add_arrval, total(weights[w]), whist 
                endif else begin 

                    ; Unweighted means
                    if method eq 'median' then begin 
                        txmean = median( x[w] )
                        tymean = median( y[w] )
                        tysdev = stdev( y[w] )
                    endif else begin 

                        txmean = mean(x[w])
                        if method eq 'sigmaclip' then begin 
                            sigma_clip, y[w], tymean, tysdev, nsig=3.5, niter=4,$
                                /silent

                            if tymean eq 0 then begin 
                                print,'ymean = 0'
                                result=moment( y[w] , maxmoment=2, sdev=tysdev)
                                tymean = result[0]
                                print,'New ymean = ',ymean
                                print,y[w]
                            endif 

                        endif else begin 
                            result=moment( y[w] , maxmoment=2, sdev=tysdev)
                            tymean = result[0]
                        endelse 
                    endelse 

                    tyerr = tysdev/sqrt(nw)

                endelse 

                add_arrval, tymean, ymean
                add_arrval, tysdev, ysdev
                add_arrval, tyerr, yerr

                add_arrval, txmean, xmean

            endif else begin ; nw ge 2?
                add_arrval, y[w[0]], ymean
                add_arrval, abs(y[w[0]]), ysdev
                add_arrval, abs(y[w[0]]), yerr
                add_arrval, x[w[0]], xmean
            endelse
        endif  
           
    endfor 

    outstruct = {                   $
        method: method,             $
        xmean: xmean,               $
        ymean: ymean,               $
        yerr: yerr,                 $
        ysdev: ysdev,               $
        hist: hist[wb]}

    if n_elements(whist) ne 0 then begin
        outstruct = create_struct(outstruct, 'whist', whist)
    endif
    return, outstruct

end 

pro binner_binbynum_fixrev, w, nbin, rev
    ; convert the indices in rev to the unlimited, unsorted frame
    for i=0L, nbin-1 do begin
        if rev[i] ne rev[i+1] then begin

            ind = rev[ rev[i]:rev[i+1]-1 ]
            rev[ rev[i]:rev[i+1]-1 ] = w[ind]

        endif
    endfor
end

; get reverse indices for binning by number
function binner_revbynum, x, nperbin, min=xmin, max=xmax, hist=hist, _extra=_extra
    if n_elements(xmin) eq 0 then xmin = min(x)
    if n_elements(xmax) eq 0 then xmax = max(x)

    ww=where( x ge xmin and x le xmax, nx)

    s = sort(x[ww])
    ww = ww[s]

    ind = lindgen(nx)

    hist = histogram(ind, bin=nperbin, reverse_indices=rev)
    nbin = n_elements(hist)
    binner_binbynum_fixrev, ww, nbin, rev
    return, rev 
end


; wrapper for histogram.  Can calculate weighted hist.
function binner_histogram, data, method, weights=weights, nperbin=nperbin, reverse_indices=rev, _extra=_extra

    method = strlowcase(method)
    nweights=n_elements(weights)

    ; lots O' special cases
    if n_elements(nperbin) ne 0 then begin
        ; binning with equal number per bin
        rev = binner_revbynum(data, nperbin, hist=hist, _extra=_extra)
        nhist = n_elements(hist)
    endif else begin
        hist = histogram(data, locations=xlow, rev=rev, _extra=_extra)

        ; Get the binsize
        nhist = n_elements(hist)
        if nhist gt 1 then begin
            binsize = xlow[1]-xlow[0] 
        endif else begin
            ; Get binsize from the data, not great but all we can do unless
            ; we keep binsize through the keywords.  Should we?
            ind = rev[ rev[0]:rev[1]-1 ]
            dmax=max(data[ind], min=dmin)
            binsize = dmax-dmin
        endelse

        xcenter = xlow + 0.5*binsize
        xhigh = xlow + binsize

    endelse


    ; Can add weights or gether more statistics
    ; There is a check in rev just for future versions where we might add
    ; logic to not calculate rev, but this would be rare
    if n_elements(rev) ne 0 then begin
        ; We will return sum of weights rather than counting '1' for each point
        if method eq 'weighted' or nweights ne 0 then begin
            whist = dblarr(nhist)
        endif

        ; get center as default
        xmean = dblarr(nhist)
        xmedian = dblarr(nhist)

        if n_elements(xlow) eq 0  then begin
            calcrange = 1
            xlow=dblarr(nhist)
            xhigh=dblarr(nhist)
        endif else calcrange=0

        for i=0L, nhist-1 do begin
            if rev[i] ne rev[i+1] then begin

                ind=rev[ rev[i]:rev[i+1]-1 ]

                if method eq 'weighted' or nweights ne 0 then whist[i] = total( weights[ind] )
                tdata = data[ind]
                xmean[i]   = mean(tdata)
                xmedian[i] = median([tdata])

                if calcrange then begin
                    xlow[i] = min(tdata, max=tmax)
                    xhigh[i] = tmax
                endif
 
            endif 
        endfor
    endif

    ; Create output structure
    binstruct = {hist: hist}

    if n_elements(xlow) ne 0 then begin
        binstruct = create_struct(binstruct, 'xlow', xlow)
    endif
    if n_elements(xhigh) ne 0 then begin
        binstruct = create_struct(binstruct, 'xhigh', xhigh)
    endif
    if n_elements(xcenter) ne 0 then begin
        binstruct = create_struct(binstruct, 'xcenter', xcenter)
    endif
    if n_elements(xmean) ne 0 then begin
        binstruct = create_struct(binstruct, 'xmean', xmean, 'xmedian', xmedian)
    endif
    if n_elements(whist) ne 0 then begin
        binstruct = create_struct(binstruct, 'whist', whist)
    endif

 
    return, binstruct
end


function binner_binbynum, x, y, nperbin, method, $
    weights=weights, $
    min=xmin, max=xmax, $
    reverse_indices=rev, $
    _extra=_extra

    rev = binner_revbynum(x, nperbin, min=xmin, max=xmax, hist=hist)
    binstruct = $
        binner_collect_statistics(x, y, hist, rev, method, weights=weights)
    return, binstruct
end 

function binner_bin, x, y, method,  $
    nperbin=nperbin, $
    min=xmin, max=xmax, $
    weights=weights, $
    reverse_indices=rev, $
    _extra=_extra

    if n_elements(nperbin) ne 0 then begin
        rev = binner_revbynum(x, nperbin, min=xmin, max=xmax, hist=hist)
    endif else begin
        hist = histogram(x, rev=rev, min=xmin, max=xmax, _extra=_extra)
    endelse

    binstruct = $
        binner_collect_statistics(x, y, hist, rev, method, $
                                    weights=weights)
    return,binstruct
end 





pro binner_syntax
    print,'-Syntax: binstruct = binner(x [, y, (all histogram keywords), weights=, nperbin=, /median, /sigma_clip, reverse_indices=, index_array=, _extra=histotram_keywords])'
    on_error, 2
    message,'Halting'
end

function binner_checkargs, x, y, weights, method
    nx=n_elements(x)
    ny=n_elements(y)
    nweights=n_elements(weights)
    nmethod=n_elements(method)

    if nx eq 0 then binner_syntax

    if ny ne 0 then begin
        if ny ne nx then begin
            message,'y must be same length as x',/inf
            binner_syntax
        endif
    endif
    
    if nmethod ne 0 then begin
        if strlowcase(method) eq 'weighted' and nweights eq 0 then begin
            message,'You must send weights for the "weighted" method'
        endif
    endif

    if nweights ne 0 then begin
        if nweights ne nx then begin
            message,'weights must be same length as data', /inf
            binner_syntax
        endif
    endif
    return, nx
end

function binner, x, y, $
        method=method, weights=weights, nperbin=nperbin, $
        reverse_indices=rev, $
        indices=indices, $
        index_array=index_array, $
        _extra=_extra

    ndata = binner_checkargs(x, y, weights)

    if n_elements(weights) ne 0 and n_elements(method) eq 0 then begin
        method='weighted'
    endif

    if n_elements(y) ne 0 then begin

        if n_elements(method) eq 0 then method='mean'
        bs = binner_bin(x, y, method, $
                        nperbin=nperbin, $
                        weights=weights, $
                        reverse_indices=rev, $
                        _extra=_extra)
    endif else begin

        if n_elements(method) eq 0 then method='standard'
        bs = binner_histogram(x, method,  $
            nperbin=nperbin,              $  
            weights=weights,              $
            reverse_indices=rev,          $
            _extra=_extra)
    endelse

    if n_elements(rev) ne 0 then begin
        if arg_present(indices) then begin
            indices = rev2ind(rev)
        endif
        if arg_present(index_array) then begin
            index_array = rev2ind(rev, /bybin)
        endif
    endif

    return, bs

end 
