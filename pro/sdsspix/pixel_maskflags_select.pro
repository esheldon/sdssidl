;+
; NAME:
;  PIXEL_MASKFLAGS_SELECT
;
;
; PURPOSE:
;  Select objects based on their SDSSPIX flags.
;
;
; CATEGORY:
;  SDSS routine.
;
;
; CALLING SEQUENCE:
;  pixel_maskflags_select, maskflags, keep, nkeep, /noedges, /twoquad, 
;                          out=out, nout=nout
;
;
; INPUTS:
;  maskflags: The maskflags for a set of objects, as output by the 
;             sdsspix_mask DLM
;
; KEYWORD PARAMETERS:
;  /noedges: No edges can be hit.
;  /twoquad: Its ok if two adjacent quadrants don't hit an edge.
;
;
; OUTPUTS:
;  keep=: The indices which pass the mask.
;  nkeep=: The number that passed.
;
;
; OPTIONAL OUTPUTS:
;  out=: The indices of objects that were masked.
;  nout=: The number that were masked.
;
; MODIFICATION HISTORY:
;  Created: Erin Sheldon, UofChicago, sometime in 2003.
;
;-


PRO pixel_maskflags_select, maskflags, keep, nkeep, noedges=noedges, twoquad=twoquad, out=out, nout=nout

  IF n_params() LT 1 THEN BEGIN 
      print,'-Syntax: pixel_maskflags_select, maskflags, keep, nkeep, noedges=noedges, twoquad=twoquad, out=out, nout=nout'
      return
  ENDIF 

  FLAGS_MASKED = '1'X           ; Central point masked, or quadrants masked
  FLAGS_QUAD1_MASKED = '2'X     ; Quadrant masked in simple test
  FLAGS_QUAD2_MASKED = '4'X
  FLAGS_QUAD3_MASKED = '8'X
  FLAGS_QUAD4_MASKED = '10'X

  FLAGS_QUAD1_MASKED_MONTE = '20'X ; Quadrant masked in monte-carlo test
  FLAGS_QUAD2_MASKED_MONTE = '40'X
  FLAGS_QUAD3_MASKED_MONTE = '80'X
  FLAGS_QUAD4_MASKED_MONTE = '100'X

  IF keyword_set(noedges) THEN BEGIN 

      ;;;;;;;;;;;;;;;;;;;;;;;;
      ;; no edges can be hit
      ;;;;;;;;;;;;;;;;;;;;;;;;

      keep = where( (maskflags EQ 0), nkeep, comp=out, ncomp=nout)

  ENDIF ELSE IF keyword_set(twoquad) THEN BEGIN 

      ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
      ;; Allow unmasked if two adjacent quadrants are unmasked
      ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

      keep = where( ((maskflags AND FLAGS_MASKED) EQ 0)  AND $
                    ( $
                      ((maskflags AND (FLAGS_QUAD1_MASKED+FLAGS_QUAD2_MASKED)) EQ 0) OR $
                      ((maskflags AND (FLAGS_QUAD2_MASKED+FLAGS_QUAD3_MASKED)) EQ 0) OR $
                      ((maskflags AND (FLAGS_QUAD3_MASKED+FLAGS_QUAD4_MASKED)) EQ 0) OR $
                      ((maskflags AND (FLAGS_QUAD4_MASKED+FLAGS_QUAD1_MASKED)) EQ 0) $
                    ), $
                    nkeep, $
                    comp = out, ncomp = nout)

  ENDIF ELSE BEGIN 
      
      ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
      ;; just make sure central point is unmasked
      ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

      keep = where ( ((maskflags AND FLAGS_MASKED) EQ 0), nkeep, $
                     comp=out, ncomp=nout)

  ENDELSE 

END 
