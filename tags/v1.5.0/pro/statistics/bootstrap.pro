;+
; NAME:
;   bootstrap
;
; PURPOSE:
;   Create bootstrap samples and calculate statistics for input variables.
;   The input data are up to 8 equal length arrays where each element 
;   represents information about the same event or object. For example they
;   could be the flux in different wavelenghts of some astronomical object,
;   or parameters of an event in a detector.
;
; CATEGORY:
;   Statistics.
;
; CALLING SEQUENCE:
;   struct = bootstrap(nsample, v1, ..., v8, /verbose)
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
;   struct: A structure containing the mean and standard deviation of the
;       bootstrap data.  For 2-d input these are arrays with lenght the number
;       of variables.  For 2-d the covariance between variables is also returned.
;
; EXAMPLE:
;   bs = bootstrap(1000, x, y)
;
; MODIFICATION HISTORY:
;   Documented: 2007-04-24, Erin Sheldon, NYU
;
;-
function bootstrap, nsamp, d1, d2, d3, d4, d5, d6, d7, d8, verbose=verbose
    if n_params() lt 2 then begin
        print,'-Syntax: bstruct = bootstrap(nsamp, v1,...v8, /verbose)'
        message,'Halting'
    endif
    samples = boot_samples(nsamp, d1, d2, d3, d4, d5, d6, d7, d8, verbose=verbose)
    statstruct = boot_stats(samples)
    return, statstruct
end
