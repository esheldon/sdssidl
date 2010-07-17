;+
; NAME:
;       MAKE_FLAG_STRUCT
; PURPOSE:
;	Set up a structure for sdss flag selection.
;       Can select on either flags or flags2. Kept for back-compatibilit,
;       use sdss_objflag_struct()
;
; CALLING SEQUENCE:
;      make_flag_struct, flag_struct 
;
; INPUTS:
;       
; OUTPUTS:
;	flag_struct: the structure used for sdss object selection....
;
; OPTIONAL OUTPUT ARRAYS:
;
; INPUT KEYWORD PARAMETERS:
; 
; PROCEDURE: This sets up the structure for flag selection of sdss objects
;	
;
; REVISION HISTORY:
;	Tim McKay	UM	1/8/99
;       Philf Fischer           1/15/99
;       Erin Scott Sheldon UM 2/5/00 Added flags2
;-


pro make_flag_struct, flag_struct

  on_error, 2
  
  if N_params() ne 1 then begin
      print,'Syntax - make_flag_struct, flag_struct'
      return
  endif

  flag_struct = sdss_objflag_struct()
  
end



