;+
; NAME:
;  sdss_rerun
;
; PURPOSE:
;  Return the associated SDSS rerun for the input run(s)
;
; CATEGORY:
;  SDSS specific
;
; CALLING SEQUENCE:
;   rerun=sdss_rerun(runs, exists=)
;
; INPUTS:
;   runs: A run or runs.
;
; OUTPUTS:
;   The associated rerun.
;
; OPTIONAL OUTPUTS:
;   exists: 1 if the specified run exists, 0 if not
;
; MODIFICATION HISTORY:
;   Documented 2011-05-12, Erin Sheldon, BNL
;
;-


function sdss_rerun, runs, exists=exists
    sf=obj_new('sdss_files')
    return, sf->rerun(runs, exists=exists)
    obj_destroy, sf
end
