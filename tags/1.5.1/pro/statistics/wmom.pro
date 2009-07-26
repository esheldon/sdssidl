;+
; NAME:
;    WMOM
;
; PURPOSE:
;    Find weighted mean,stdev,err of an input array.
;
; CALLING SEQUENCE: 
;    wmom, array, sigma, wmean, wsig, werr, inputweights=, inputmean=, 
;          weights=weights, /noweights, /calcerr
;
; INPUTS:
;    array: an array of numbers
;    sigma: the errors on each array element. See below.
;
; OPTIONAL INPUTS:
;    inputmean: use this number as the mean instead of
;       re-calculating the mean
;    weights: Use these as the weights for each point rather than 1/sigma^2.  
;       If input, the sigma input parameter is ignored.  If not input but a 
;       named variable is present, the used weights will be returned. (note 
;       the inputweights keyword is still supported but deprecated).
;
; OUTPUTS: 
;    wmean: weighted mean
;    wsig: weighted standard deviation
;    werr: weighted error in mean
;    /noweights: no weight: straight mean/variance is done.
;    /calcerr: Default is to set the error on the mean to
;              sqrt(1../total(weights)) where weights = 1/sigma^2 which
;              is correct for a gaussian distribution.  If /calcerr, then the
;              formula below is used
;
; PROCEDURE: 
;    wi      = 1/sig[i]^2 (or use inputweights[i])
;    wmean   = sum( xi*wi)/sum(wi) (or =inputmean)
;    wsig^2  = sum( wi*(xi-wmean)^2 )/sum(wi)
;            
;    the error on the mean is simple if the user inputs the
;    sigmas:  
;         werr = 1./sqrt(sum(wi))
;
;    If the user inputs weights, which may not be 1/err^2 then
;    this formula is used:
;
;         werr^2 = sum( wi^2*(xi - wmean)^2 ) / ( sum(wi) )^2
;
;    This formula is also used if /calcerr. 
;
; REVISION HISTORY:
;    Author: Erin Scott Sheldon UofMich  8/99
;    Revision History:
;       Added optional inputmean, inputweights inputs
;       Added weights keyword to replace inputweights.
;       Added noweights keyword. E.S.S. 13-Nov-2001
;-
;  Copyright (C) 2005  Erin Sheldon, NYU.  erin dot sheldon at gmail dot com
;
;    This program is free software; you can redistribute it and/or modify
;    it under the terms of the GNU General Public License, version 2, as 
;    published by the Free Software Foundation.
;

pro wmom, array, sigma, wmean, wsig, werr, inputmean=inputmean, inputweights=inputweights, weights=weights, noweights=noweights, calcerr=calcerr

  
    ndata = n_elements(array)
    if ndata eq 0 then begin 
        print,'-Syntax: wmom, array, sigma, wmean, wsig, werr, inputmean=, weights=, /noweights, /calcerr'
        on_error, 2
        message,'Halting'
    endif
  
    ; single data point entered
    if ndata eq 1 then begin
        if n_elements(inputmean) ne 0 then wmean=inputmean else wmean=array[0]
        wsig=0.0
        werr=0.0
        weights = 1.0
        return
    endif

    ; backward compatibility
    if n_elements(inputweights) ne 0 then weights=inputweights

    if keyword_set(noweights) then begin 
        ; straight mean
        if n_elements(inputmean) ne 0 then begin
            wmean = inputmean
            wvar = total( (array-wmean)^2 )/(ndata-1)
            wsig = sqrt(wvar)
            werr = wsig/sqrt(ndata)
        endif else begin
            mom=moment(array, maxmoment=2, sdev=wsig)
            wmean = mom[0]
            werr = wsig/sqrt(ndata)
        endelse
        return
    endif else begin

        if n_elements(weights) eq 0 then begin 
            ;; user must input sigma: error estimate on each point
            good=where(sigma gt 0.0, ngood)
            if ngood eq 0 then message,'All sigma are zero'
 
            weights = sigma
            weights[*] = 0

            weights[good] = ( 1./sigma[good]^2 )
            wtot = total(weights)

            ;; Uncertainty in the mean.
            werr = 1./sqrt(wtot)

        endif else begin 
          
            ;; user has input a set of weights
            wtot = total(weights)
            calcerr = 1

        endelse 

        if n_elements(inputmean) eq 0 then begin
            wmean = total(weights*array)/wtot 
        endif else begin
            wmean=inputmean
        endelse

        ;; variance about the mean.
        wvar = total( weights*(array - wmean)^2)/wtot
        wsig=sqrt( temporary(wvar) )

        ;; calculate error on mean from distribution
        if keyword_set(calcerr) then begin 
            werr = sqrt(  total(weights^2*(array-wmean)^2 ) )/wtot
        endif 

        return
    endelse 
    
end
