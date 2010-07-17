;+
; NAME:
;  SPHPOLY_COMPLETENESS
;
;
; PURPOSE:
;  Call Andreas' spherical polygon code, which checks ra/dec points against a
;  spherical polygon mask, and return the completeness.
;
;
; CATEGORY:
;  SDSS routine
;
;
; CALLING SEQUENCE:
;  sphpoly_completeness, ra, dec, completeness, $
;                        poly_id=poly_id, poly_area=poly_area, $
;                        maskfile=maskfile, verbose=verbose
;
;
; INPUTS:
;  ra/dec: scalar or arrays of ra/dec in double precision.
;
;
; OPTIONAL INPUTS:
;  maskFile=: The mask file to use.  Default is from the config file
;
;
; KEYWORD PARAMETERS:
;  /verbose: Print messages.
;
;
; OUTPUTS:
;  completeness: The completeness for each ra/dec point.
;
;
; OPTIONAL OUTPUTS:
;  poly_id: The polygon id for each object.
;  poly_area: The area of the polygon each object is in.
;
;
; RESTRICTIONS:
;  You must have compiled the code.  See the README file in the base directory
;  of the SDSSIDL distribution.
;
; MODIFICATION HISTORY:
;   Created Erin Sheldon, UChicago, some time in 2003
;
;-



PRO sphpoly_completeness, ra, dec, completeness, $
                          poly_id=poly_id, poly_area=poly_area, $
                          maskfile=maskfile, verbose=verbose

  IF n_params() LT 2 THEN BEGIN 
      print,'-Syntax: sphpoly_completeness, ra, dec, completeness, poly_id=, poly_area=, maskfile=, /verbose'
      return
  ENDIF 

  IF n_elements(maskfile) EQ 0 THEN BEGIN 
      maskFile = sdssidl_config("sphpoly_mask_file")
  ENDIF

  IF float(!version.release) LT 5.5 THEN BEGIN
      message,'This function uses an IDL DLM written in C',/inf
      message,'Only IDL >= 5.5 supported'
  ENDIF ELSE BEGIN 
      
      completeness = sphpoly_comp(ra, dec, maskFile, $
                                  poly_id=poly_id, $
                                  poly_area=poly_area, $
                                  verbose=verbose)
  ENDELSE 

END 
