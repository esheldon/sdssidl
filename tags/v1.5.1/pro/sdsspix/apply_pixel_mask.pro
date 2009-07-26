;+
; NAME:
;  APPLY_PIXEL_MASK
;
;
; PURPOSE:
;  Test input clambda,ceta points against Ryan Scranton's SDSSPix masks.  Calls
;  a version of Ryan's mask code, linked into IDL via a Dynamically Loadable
;  Module (DLM).  Additional C modules were added to Ryan's code to check for
;  edges and holes.
;
;
; CATEGORY:
;  SDSS specific routine.
;
;
; CALLING SEQUENCE:
;    apply_pixel_mask, clambda, ceta, masked, unmasked, maskflags, 
;                      maskfile=, $
;                      /basic, /simple, $
;                      /bound, /combined, $
;                      $
;                      maxangle=, $
;                      /twoquad, status=
;
;
; INPUTS:
;  clambda, ceta: SDSS survey coordinates (corrected) to be tested against the
;                 mask.  These can be arrays. Use eq2csurvey to convert ra/dec
;                 to clambda, ceta
;
;
; OPTIONAL INPUTS:
;  maskFile=: Which mask file to use. The default is to use the "basic" masked
;             defined in the config file (see also keyword parameters).
;  maxAngle=: The maximum angle around each object.  When this is given, then
;             edges will be checked for within this radius.
;
;
; KEYWORD PARAMETERS:
;   FOR MASK EXPLANATION, see below
;
;  /basic: Use the "basic" mask defined in the config file.
;  /simple: Use the "simple" mask defined in the config file.
;  /bound: Use the "bound" mask.
;  /combined: Use the "combined" mask.
;
;  /twoquad: When the maxAngle are sent and and /twoquad is set, objects pass
;            the mask if two adjacent quadrants are unmasked.
;
;
;
; OUTPUTS:
;  masked: Indices of points in the input list were masked out.
;  unmasked: Indices of points in the input list were not masked.
;  maskflags: The mask flags for each object.  
;
;      FLAGS_MASKED = '1'X     ; Central point masked, or quadrants masked
;      FLAGS_QUAD1_MASKED = '2'X  ; Quadrant masked in simple test
;      FLAGS_QUAD2_MASKED = '4'X  
;      FLAGS_QUAD3_MASKED = '8'X
;      FLAGS_QUAD4_MASKED = '10'X
;
;      FLAGS_QUAD1_MASKED_MONTE = '20'X  ; Quadrant masked in monte-carlo test
;      FLAGS_QUAD2_MASKED_MONTE = '40'X
;      FLAGS_QUAD3_MASKED_MONTE = '80'X
;      FLAGS_QUAD4_MASKED_MONTE = '100'X
;
;   See PIXEL_MASKFLAGS_SELECT.PRO for examples of how to use these flags.
;
;
; MASK EXPLANATION:
;
;  "_basic": This mask contains the rough geometry for the current data set.
;            It delineates the ends of the data in each stripe as well as any
;            gaps in the middle of stripes where data is missing from one or
;            both strips.
;
;  "_simple": The first level of additional complexity.  This mask includes all
;             of the pixels in the "_basic" mask as well as smaller masks to
;             block out small areas lost to missing fields in otherwise
;             continuous data.  As with the masks in "_basic", these masks are
;             all made by hand.  In the dr3 implementation, this mask blocked
;             out an additional 17 square degrees compared to the area masked
;             in the "_basic" mask.
;
;  "_bound": The next level in complexity.  This contains all of the pixels in
;            "_simple" as well as masks to remove areas around bright stars.
;            These bright star masks are the first ones included which are not
;            made by hand.  In the dr3 implementation, this mask blocked out an
;            additional 100 square degrees compared to the area masked in the
;            "_simple" mask.
;
;  "_combined": The final level of complexity.  This contains all of the pixels
;               in "_bound" as well as pixels removing areas where systematic
;               errors are likely to contaminate data.  In the dr3
;               implementation,, 0 < reddening < 0.2, 0.85 < seeing < 1.4) ,
;               this mask blocked out an additional 1210 square degrees
;               compared to the area masked in the "_bound" mask.
;
; RESTRICTIONS:
;  You must have compiled the code.  See the README file in the base directory
;  of the SDSSIDL distribution.
;
;
; EXAMPLE:
;  apply_pixel_mask, clambda, ceta, masked, unmasked, maskflags, /combined
;
;
; MODIFICATION HISTORY:
;    Created: Erin Sheldon, UChicago, some time in 2003
;    Converted from call_external to DLM: E.S. Nov-2004
;
;-


PRO apply_pixel_mask, clambda, ceta, masked, unmasked, maskflags, $
                      maskfile=maskfile, $
                      basic=basic, simple=simple, $
                      bound=bound, combined=combined, $
                      $
                      maxangle=maxangle, $
                      twoquad=twoquad, $
                      $
                      $         ; obsolete
                      silent=silent, verbose=verbose, status=status

  status = 1
  IF n_params() LT 2 THEN BEGIN 
      print,'-Syntax: apply_pixel_mask, clambda, ceta, masked, unmasked, maskflags, $'
      print,'              maskfile=, '
      print,'              /basic, /simple, /bound, /combined, '
      print,'              maxangle=, '
      print,'              /twoquad, status='
      print
      print
      print,'* Default is basic mask'
      print,'* Send maxangle=maxangle to check edges'
      print,'  By default, cannot hit any edge.  Send /twoquad to allow two adjacent '
      print,'  unmasked quadrants. For more complicated checks use the maskflags output'
      print
      print,'* clambda, ceta, maxangle in degrees'
      print
      return
  ENDIF 

  idlvers = float(!version.release)
  IF float(idlvers) LT 5.5 THEN BEGIN 
      print,'The C DLM for sdsspix is only supported in IDL 5.5 or greater'
      return
  ENDIF 

  ;; Mask file

  IF n_elements(maskFile) EQ 0 THEN BEGIN 

      IF keyword_set(combined) THEN BEGIN 
          maskFile = sdssidl_config("pixel_mask_combined")
      ENDIF ELSE IF keyword_set(bound) THEN BEGIN 
          maskFile = sdssidl_config("pixel_mask_bound")
      ENDIF ELSE IF keyword_set(simple) THEN BEGIN 
          maskFile = sdssidl_config("pixel_mask_simple")
      ENDIF ELSE BEGIN 
          maskFile = sdssidl_config("pixel_mask_basic")
      ENDELSE 

  ENDIF 

  IF n_elements(maxangle) NE 0 THEN BEGIN 
      check_edge = 1
  ENDIF ELSE BEGIN 
      check_edge = 0
  ENDELSE 

  ;; IDL has a bug that it gives a compile error if the sdsspix_mask()
  ;; DLM was not loaded.  We wrap this in a command string to avoid
  ;; this

  message,'Using maskfile: '+maskFile,/inf

  command = $
    'maskFlags = sdsspix_mask(clambda, ceta, maskFile, '+$
    '                         maxangle=maxangle, '+$
    '                         verbose=verbose, status=status)'
      
  IF NOT execute(command) THEN BEGIN 
      message,'Could not execute sdsspix_mask() call.',/inf
      return
  ENDIF 
  IF status NE 0 THEN return

  IF NOT check_edge THEN BEGIN 
      
      ;; Just make sure cenral object not masked
      pixel_maskflags_select, maskflags, unmasked, nunmasked, $
                              out=masked, nout=nmasked

  ENDIF ELSE BEGIN 

      ;; Check if any edges were hit.
      IF keyword_set(twoquad) THEN BEGIN

          pixel_maskflags_select, maskflags, unmasked, nunmasked, $
                                  out=masked, nout=nmasked, /twoquad
          
      ENDIF ELSE BEGIN 

          ;; check any edges hit
          pixel_maskflags_select, maskflags, unmasked, nunmasked, $
                                  out=masked, nout=nmasked, /noedges

      ENDELSE 
  ENDELSE 


  return

END 
