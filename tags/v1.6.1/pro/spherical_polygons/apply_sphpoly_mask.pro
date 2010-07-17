;+
; NAME:
;  APPLY_SPHPOLY_MASK
;
;
; PURPOSE:
;  Call sphpoly_completeness to get the completenss for a set of ra/dec points,
;  then return indices of those which pass the completeness cut.
;
;
; CATEGORY:
;  SDSS routine.
;
;
; CALLING SEQUENCE:
;   apply_sphpoly_mask, ra, dec, masked, unmasked, 
;            /lameta, 
;            maskfile=, 
;            completeness=, 
;            compcut=, 
;            poly_id=, 
;            poly_area=, 
;            /silent=
;
;
; INPUTS:
;  ra/dec: Scalar or arrays of points in double precision.
;
;
; OPTIONAL INPUTS:
;  maskFile=: The mask file to use. Default is the sphpoly_mask_file entry in
;             config file.
;  compcut=: The completeness cut to use, default is > 0.0
;
;
; KEYWORD PARAMETERS:
;  /lameta: The inputs are clambda/ceta instead of ra/dec
;  /silent: Don't print stuff
;
; OUTPUTS:
;  masked, unmasked: indices of masked and unmasked objects.
;
; OPTIONAL OUTPUTS:
;  poly_id: The polygon id for each object
;  poly_area: The polygon area for each object.
;
; RESTRICTIONS:
;  You must have compiled the code.  See the README file in the base directory
;  of the SDSSIDL distribution.
;
;
; MODIFICATION HISTORY:
;   Created Erin Sheldon, UChicago, some time in 2003
;
;-


PRO apply_sphpoly_mask, ra, dec, masked, unmasked, lameta=lameta, maskfile=maskfile, completeness=completeness, compcut=compcut, poly_id=poly_id, poly_area=poly_area, silent=silent

  IF n_params() LT 2 THEN BEGIN 
      print,'-Syntax: apply_sphpoly_mask, ra, dec, masked, unmasked, /lameta, maskfile=maskfile, completeness=completeness, compcut=compcut, poly_id=poly_id, poly_area=poly_area, /silent'
      return
  ENDIF 

  IF n_elements(compcut) EQ 0 THEN compcut = 0

  IF keyword_set(lameta) THEN BEGIN 
      csurvey2eq, ra, dec, rain, decin

      sphpoly_completeness, rain, decin, completeness, $
        poly_id=poly_id, poly_area=poly_area, $
        maskfile=maskfile, silent=silent

  ENDIF ELSE BEGIN 
      sphpoly_completeness, ra, dec, completeness, $
        poly_id=poly_id, poly_area=poly_area, $
        maskfile=maskfile, silent=silent
  ENDELSE 

  unmasked = where(completeness GT compcut, comp=masked)

END 
