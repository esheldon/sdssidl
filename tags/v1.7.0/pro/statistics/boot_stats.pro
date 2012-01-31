;+
; NAME:
;   boot_stats
;
; PURPOSE:
;   Calculate the statistics (mean, sdev, covariance) for a set of bootstrap samples. 
;   This program does not create bootstrap samples, see boot_indices() and 
;   boot_samples() for that.  The input data are a 1-d array of bootstrap samples
;   or a 2-d [nsample, nvar] array with the second dimension representing the value
;   of different variables for each bootstrap sample.
;
; CATEGORY:
;   statistics
;
; CALLING SEQUENCE:
;   struct = boot_stats(data)
;
; INPUTS:
;   data: A 1-d or 2-d array of bootstrap data.  For 1-d this is just all the
;       bootstrap data for a single variable.  For 2-d input it must be of the
;       shape [nsample, nvar]
;
; OUTPUTS:
;   struct: A structure containing the mean and standard deviation of the
;       bootstrap data.  For 2-d input these are arrays with lenght the number
;       of variables.  For 2-d the covariance between variables is also returned.
;
; EXAMPLE:
;   ; We have variables x and y measured for a given event.
;   ; First create the averages of these for various bootstrap samples.
;   nsample = 100
;   bsamples = boot_samples(nsample, x, y)
;   bs = boot_stats(bsamples)
;       
;
; MODIFICATION HISTORY:
;   Documented: 2006-04-24, Erin Sheldon, NYU
;
;-
function boot_stats, data
  
    on_error, 2
    IF n_params() LT 1 THEN BEGIN 
        print,'-Syntax: bs = boot_stats(bootdata)'
        print,'    bootdata must be 1 or 2-d array'
        print,'    For 2-d must be [nsamples, nvar]'
        message,'Halting'
    ENDIF 

    sz = size(data)
    if sz[0] ne 1 and sz[0] ne 2 then message,'bootsamples must be a 1-d or 2-d array'
    if sz[0] eq 1 then begin
        ; These are simply the bootstrap data for a single variable
        st = {mean: 0d, sdev: 0d}
        mom = moment(data, maxmoment=2)
        st.mean = mom[0]
        st.sdev = sqrt(mom[1])
        return, st
    endif

    ; 2-d data: multiple variables.
    nsamp = sz[1]
    nvar = sz[2]

    st = {mean:dblarr(nvar), sdev:dblarr(nvar), cov:dblarr(nvar,nvar)} 
    for i=0L, nvar-1 do begin
        mom = moment(data[*,i], maxmoment=2)
        st.mean[i] = mom[0]
        st.sdev[i] = sqrt(mom[1])
    endfor
            
    for i=0L, nvar-1 do begin
        idiff = data[*,i]-st.mean[i]
        for j=i, nvar-1 do begin
            jdiff = data[*,j]-st.mean[j]
            st.cov[j,i] = total(idiff*jdiff, /double)
            if i ne j then st.cov[i,j] = st.cov[j,i]
        endfor
    endfor

    return, st
END 
