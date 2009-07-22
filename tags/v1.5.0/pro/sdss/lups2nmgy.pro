;+
; NAME:
;   lups2nmgy
;
; PURPOSE:
;   Convert asinh mags, or luptitudes, to nanomaggies.
;
; CATEGORY:
;   SDSS
;
; CALLING SEQUENCE:
;   nmgy = lups2nmgy(lups, lups_err=, band=, ivar=)
;
; INPUTS:
;   lups: Asinh mags, or luptitudes. May be a scalar or array.  If 
;       a [nband,n] array and nband is not 5 then the band= argument 
;       must be sent to specify the bands present.
;
; OPTIONAL INPUTS:
;   lups_err: The mag errors.
;   band: The bandpasses present. Default [0,1,2,3,4]
;
; OUTPUTS:
;   Nanomaggies.
;
; OPTIONAL OUTPUTS:
;   ivar: Inverse variance of nanomaggies. If lups_err is sent 
;       then ivar may be returned through a named variable.
;
; MODIFICATION HISTORY:
;   2007-04-?? Erin Sheldon, NYU.  
;-

function lups2nmgy, lups, lups_err=lups_err, band=band, ivar=ivar

    on_error, 2
    if n_elements(lups) eq 0 then begin
        print,'-Syntax: nmgy = lups2nmgy(lups, lups_err=, band= , ivar=)'
        on_error, 2
        message,'Halting'
    endif

    nb = n_elements(band)
    sz = size(lups)
    if sz[0] eq 0 then begin
        ; scalar
        if nb ne 1 then message,'For scalar input you must specify a single band'
        nobj = 1
        nband=nb
    endif else if sz[0] eq 1 then begin
        ; 1-d array input
        ; ambiguous if array of 5 is entered!  We default to assuming it is really
        ; an array of [5,1]
        nobj = n_elements(lups)
        if nobj eq 5 then begin
            if nb eq 0 then begin
                print,'Ambiguous input: assuming 5-band input, one obj'
                band=[0,1,2,3,4]
                nb=5
            endif else if nb ne 1 and nb ne 5 then begin
                ; if band is present, must be 1 or all bands
                message,'length(5) 1-d array: band must be length 1 or 5'
            endif
            if nb eq 5 then nobj=1
        endif else begin
            if nb ne 1 then message,'For 1-d array input you must specify a single band'
        endelse
        nband=nb
    endif else if sz[0] eq 2 then begin
        ; 2-d array input
        nband = sz[1]
        ;if nband ne 5 then begin
        ;    message,'2-d array input must be of the form [nband, nobject]'
        ;endif

        if nb eq 0 then begin
            band=[0,1,2,3,4]
            nb = 5
        endif

        if nb ne nband then begin
            message,'n_elements(band) must be equal to number of bands in input array'
        endif

        nobj = sz[2]
    endif else begin
        message,'Input must be 0, 1, or 2 dimensions'
    endelse

    bvalues=[1.4D-10, 0.9D-10, 1.2D-10, 1.8D-10, 7.4D-10]
    bnmgy=bvalues*1.e9


    nmgy = lups
    nmgy[*] = -9999.
    for i=0L, nband-1 do begin
        b = band[i]
        nmgy[i, *] = 2.0*bnmgy[b]*sinh( -alog(bvalues[b])-0.4*alog(10.0)*lups[i,*] )
    endfor

    if n_elements(lups_err) ne 0 and arg_present(ivar) then begin
        if n_elements(lups_err) ne n_elements(lups) then begin
            message,'lups_err must be same size as lups input'
        endif

        ivar = lups
        ivar[*] = -9999.
        for i=0L, nband-1 do begin
            b=band[i]
            w=where(lups_err[i,*] gt 0.0, ngood)
            if ngood ne 0 then begin

                terr=2.0*bnmgy[b]*cosh(-alog(bvalues[b])- 0.4*alog(10.0)*lups[i,w])*0.4*alog(10.)*lups_err[i,w]
                ww = where(terr gt 0.0, nww)
                if nww ne 0 then begin
                    ivar[i,w[ww]] = 1.0/terr[ww]^2 
                endif
            endif
        endfor
    endif

    return,nmgy

end

