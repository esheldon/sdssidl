;+
; NAME:
;   PLOTHIST
; PURPOSE:
;   Plot the histogram of an array with the corresponding abcissa. This is an
;   enhanced version of the original Landsman program from the Goddard 
;   library. Enhancements include weighting of points, aspect ratio control,
;   bootstrapping for errors, and allowing all histogram keywords.
;
; CALLING SEQUENCE:
;   plothist, array, weights=, 
;       peak=, norm=, aspect=, /error, nboot=, 
;       /noplot, /overplot, 
;       /fill, fcolor=, /fline, forientation=, fpattern=, fspacing=
;       xhist=, yhist=, reverse_indices=
;       ...plotting keywords...histogram keywords]
;
; INPUTS:
;   array: data to histogram and plot.
;
; OPTIONAL INPUTS:
;   histogram keywords: See docs on histogram on it bins data. 
;       E.g. binsize, nbin, min, max, etc.
;   weights: Weights for each point.  Plotted histogram will be total of weights
;       rather than simple counts in bins.
;   peak: Peak value for plotted histogram.
;   norm: normalization of histogram (intergration is simple rectangle 
;       integration)
;   aspect: Aspect ratio of the plot.
;   /error: Plot poisson error bars for unweighted histograms.
;   nboot: Bootstrap the histogram to get errors using this number of samples.
;       Is most useful for weighted histograms.
;   /noplot: Don't plot, just get the bin statistics.
;   /overplot: overplot on current plot.
;   /fill: Fill the histogram.  See below for fill keywords.
;   extra plotting keywords: See docs on plot 
;    
;
; OPTIONAL OUTPUTS:
;   xhist - X vector used in making the plot  
;   yhist - Y vector used in making the plot
;   reverse_indices: See docs for the histogram program.
;   bstruct: Lots of statistics for the bins.
;
; The following keywords take effect only if the FILL keyword is set:
;      fcolor - color to use for filling the histogram
;      /fline - if set, will use lines rather than solid color for fill (see
;              the LINE_FILL keyword in the POLYFILL routine)
;      forientation - angle of lines for fill (see the ORIENTATION keyword
;              in the POLYFILL routine)
;      fpattern - the pattern to use for the fill (see the PATTERN keyword
;              in the POLYFILL routine)
;      fspacing - the spacing of the lines to use in the fill (see the SPACING
;              keyword in the POLYFILL routine)
;
; EXAMPLES:
;       IDL> a = randomn(seed,1000)
;       IDL> plothist,a, binsize = 0.1
;
;       IDL> plothist, a, nbin=25, norm=1.0
;
; MODIFICATION HISTORY:
;   Written     W. Landsman            January, 1991
;   Add inherited keywords W. Landsman        March, 1994
;   Use ROUND instead of NINT  W. Landsman   August, 1995
;   Add NoPlot and Overplot keywords.   J.Wm.Parker  July, 1997
;   Add Peak keyword.   J.Wm.Parker  Jan, 1998
;   Add FILL,FCOLOR,FLINE,FPATTERN,FSPACING keywords. J.Wm.Parker Jan, 1998
;	Converted to IDL V5.0   W. Landsman 21-Jan-1998
;   Allowed min/max to be sent. Erin Scott Sheldon UofMich Jun-07-2001
;   Added norm= keyword Erin Scott Sheldon UChicago 17-Oct-2002
;   Now uses bin centers rather than the leading edge of the bin.
;       Erin Scott Sheldon UChicago 25-Nov-2003
;   Big rewrite using the binner.pro and pplot.pro functions.  Now 
;       allows weighted histograms and all histogram keywords.  Also 
;       allows pplot keywords such as aspect for aspect ratio.
;                    Erin Sheldon, NYU, 2007-04-07
;   Allow bootstrapping using the nboot keyword. Calls boot_hist.pro in
;       this case.  2007-04-24 Erin Sheldon, NYU
;-

pro plothist, arr, xhistout, yhistout, weights=weights, $
        binsize=binsize, nbins=nbins, min=min, max=max, $ 
        error=error, nboot=nboot, silent=silent, $
        noplot=noplot, overplot=overplot, $
        psym = psym, peak=peak, fill=fill, fcolor=fcolor, fline=fline, $
        fspacing=fspacing, fpattern=fpattern, $
        forientation=forientation, $
        anonymous_ = dummy_, norm = norm, $
        aspect=aspect, $
        bstruct=bstruct, $
        xhist=xhist, yhist=yhist, histerr=histerr, $
        reverse_indices=rev, $
        _extra = _extra

    if n_params() lt 1 then begin   
        on_error,2
        print,'Syntax - plothist, arr, weights=, '
        print,'       peak=, norm=, aspect=, '
        print,'       /noplot, /overplot, '
        print,'       /fill, fcolor=, /fline, forientation=, fpattern=, fspacing=, '
        print,'       xhist=, yhist=, reverse_indices=, bstruct=, '
        print,'       ...histogram and plotting keywords...'
        message,'Halting'
    endif

    if n_elements(nboot) ne 0 then begin
        ; bootstrapping
        bstruct = boot_hist(nboot, arr, weights=weights,    $
            binsize=binsize, nbins=nbins, min=min, max=max, $
            reverse_indices=rev, silent=silent)
        if n_elements(weights) ne 0 then begin
            histerr = bstruct.whist_err
        endif else begin
            histerr = bstruct.hist_err
        endelse
    endif else begin
        ; standard histogram
        bstruct = binner(arr, weights=weights, $
            binsize=binsize, nbins=nbins, min=min, max=max, $
            reverse_indices=rev)
        if keyword_set(error) and n_elements(weights) eq 0 then begin
            histerr = sqrt(bstruct.hist)
        endif
    endelse

    if n_elements(weights) ne 0 then begin
        yhist = bstruct.whist
    endif else begin
        yhist = bstruct.hist
    endelse
    xhist = bstruct.xcenter

    if arg_present(xhistout) then xhistout=xhist
    if arg_present(yhistout) then yhistout=yhist

    bsize = bstruct.xhigh[0]-bstruct.xlow[0]

    N_hist = N_elements( yhist )

    ; normalize histogram?
    if n_elements(peak) ne 0 then begin
        mval = (Peak / float(max(yhist)))
    endif else if n_elements(norm) ne 0 then begin
        ; simple rectangle integration
        toty = total(yhist*bsize)
        mval = norm[0]/toty
    endif 
    if n_elements(mval) ne 0 then begin
        yhist=yhist*mval
        if n_elements(histerr) ne 0 then histerr=histerr*mval
    endif

    ; If not doing a plot, exit here.
    if keyword_set(NoPlot) then return


    if n_elements(psym) eq 0 then psym = 10 ;Default histogram plotting

    pplot, $ 
        [xhist[0] - bsize, xhist, xhist[n_hist-1]+ bsize] , [0,yhist,0],  $ 
        aspect=aspect, overplot=overplot, psym=psym, _extra=_extra
    if n_elements(histerr) ne 0 then begin
        pplot, xhist, yhist, yerr=histerr, psym=3, /overplot, hat=0, $
            _extra=_extra
    endif

    ; If doing a fill of the histogram, then go for it.

    if keyword_set(Fill) then begin
        Xfill = transpose([[Xhist-bsize/2.0],[Xhist+bsize/2.0]])
        Xfill = reform(Xfill, n_elements(Xfill))
        Xfill = [Xfill[0], Xfill, Xfill[n_elements(Xfill)-1]]
        Yfill = transpose([[Yhist],[Yhist]])
        Yfill = reform(Yfill, n_elements(Yfill))
        Yfill = [0, Yfill, 0]

        if keyword_set(Fcolor) then Fc = Fcolor else Fc = !P.Color
        if keyword_set(Fline) then begin
            if keyword_set(Fspacing) then Fs = Fspacing else Fs = 0
            if keyword_set(Forientation) then Fo = Forientation else Fo = 0
            polyfill, Xfill,Yfill, color=Fc, /line_fill, spacing=Fs, orient=Fo
        endif else begin
            if keyword_set(Fpattern) then begin
                polyfill, Xfill,Yfill, color=Fc, pattern=Fpattern
            endif else begin
                polyfill, Xfill,Yfill, color=Fc
            endelse
        endelse

        ; Because the POLYFILL can erase/overwrite parts of the originally $
        ; plotted histogram, we need to replot it here.

        oplot, $
            [xhist[0] - bsize, xhist, xhist[n_hist-1]+ bsize] , $
            [0,yhist,0],  $ 
            PSYM = psym, _EXTRA = _extra
    endif

    return
end
