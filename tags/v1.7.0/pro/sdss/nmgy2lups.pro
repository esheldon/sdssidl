;+
; NAME:
;   nmgy2lups
;
; PURPOSE:
;   Convert flux in nanomaggies to asinh mags, or luptitudes. 
;
; CATEGORY:
;   SDSS
;
; CALLING SEQUENCE:
;   lups = nmgy2lups(nmgy, ivar=, band=, lups_err=)
;
; INPUTS:
;   nmgy: Flux in nanomaggies.  May be a scalar or array.  If 
;       a [nband,n] array and nband is not 5 then the band= argument 
;       must be sent to specify the bands present. There some ambiguity
;       when the input is an array of 5 and band is not sent. The code
;       assumes this is 5-band input for a single object.
;
; OPTIONAL INPUTS:
;   ivar: The inverse variance of the flux, same shape as nmgy.
;   band: The bandpasses present. Default [0,1,2,3,4]
;
; OUTPUTS:
;   Asinh mags, or luptitudes.
;
; OPTIONAL OUTPUTS:
;   lups_err: If ivar is sent then lups_err may be returned through
;       a named variable.
;
; EXAMPLE:
;   st = pgsql_query('select * from datasweep limit 1000')
;   lups = nmgy2lups(st.modelflux)
;
; MODIFICATION HISTORY:
;   2007-04-?? Erin Sheldon, NYU.  Got much from Blanton's maggies2lups
;       but added some extra functionality, error handling, and units of
;       nanomaggies.
;
;-

function nmgy2lups, nmgy, ivar=ivar, band=band, lups_err=lups_err

    on_error, 2
    if n_elements(nmgy) eq 0 then begin
        print,'-Syntax: lups = nmgy2lups(nmgy, ivar=, lups_err=, band=)'
        on_error, 2
        message,'Halting'
    endif

    nb = n_elements(band)
    sz = size(nmgy)
    if sz[0] eq 0 then begin
        ; scalar
        if nb ne 1 then message,'For scalar input you must specify a single band'
        nobj = 1
        nband=nb
    endif else if sz[0] eq 1 then begin
        ; 1-d array input
        ; ambiguous if array of 5 is entered!  We default to assuming it is really
        ; an array of [5,1]
        nobj = n_elements(nmgy)
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


    lups = nmgy
    lups[*] = -9999.
    for i=0L, nband-1 do begin
        b = band[i]
        lups[i, *] = 2.5*alog10(1.0/bvalues[b]) - asinh2(0.5*nmgy[i,*]/bnmgy[b])/(0.4*alog(10.0));
    endfor

    if n_elements(ivar) ne 0 and arg_present(lups_err) then begin
        if n_elements(ivar) ne n_elements(nmgy) then begin
            message,'ivar must be same size as flux input'
        endif

        lups_err = ivar
        lups_err[*] = -9999.
        for i=0L, nband-1 do begin
            b=band[i]
            w=where(ivar[i,*] gt 0.0, ngood)
            if ngood ne 0 then begin
                terr = 1.0/sqrt(ivar[i,w])
                lups_err[i,w] = 2.5*terr/( 2.*bnmgy[b]*alog(10.0)*sqrt(1.0 + (0.5*nmgy[i,w]/bnmgy[b])^2 ))
            endif
        endfor
    endif

    return,lups

end

