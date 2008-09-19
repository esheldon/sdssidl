;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;+
;
; NAME:  
;    MAKE_CLFLAG_STRUCT
;    
; PURPOSE:  
;    Sets up parameter structure for galaxy classification flag selection.
;    
;
; CALLING SEQUENCE:
;     make_clflag_struct,clflag_struct
;    
;
; INPUTS: 
;     clflag_struct:  classification flag structure
;
;
; REVISION HISTORY:  Judith Racusin 6/15/00, modified from make_flag_struct by Tim Mckay
;    
;       
;                                      
;-                                       
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;


PRO make_clflag_struct,clflag_struct

  on_error, 2

  IF n_params() NE 1 THEN BEGIN
    print,'Syntax: make_clflag_struct,clflag_struct'
    print,''
    return
  END

  clflag_struct={ELLIP: 'D', $
                 ELLIP_LIKELY: 'D', $
                 SPIRAL_LIKELY: 'D', $
                 SPIRAL: 'D', $ 
                 FAILED: 'D',$
                 LENS_GALAXY_G: 'D', $ 
                 LENS_GALAXY_R: 'D', $
                 LENS_GALAXY_I: 'D', $
                 BRG: 'D', $
                 NOT_A_GALAXY: 'D', $
                 BAD_ASYMMETRY: 'D', $
                 BAD_CONCENTRATION: 'D'}


return
END

