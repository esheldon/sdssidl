;+
; NAME:
;       MAKE_RUNSTATUS_STRUCT
; PURPOSE:
;	Set up a parameter structure for sdss RUN STATUS selection. Kept
;  only for back-compatibility, use sdss_runstatus_struct()
;
; CALLING SEQUENCE:
;      make_runstatus_struct, runstatus_struct
;
; INPUTS:
;       
; OUTPUTS:
;	runstatus_struct: the structure used for sdss object selection....
;
; OPTIONAL OUTPUT ARRAYS:
;
; INPUT KEYWORD PARAMETERS:
; 
; PROCEDURE: This sets up the structure for run status selection of 
;   sdss runs/reruns
;	
;
; REVISION HISTORY:
;  ??-??-2002: Erin Scott Sheldon UofChicago
;  18-Nov-2002: Added bad2 flag checking: tsField files
;	
;-


pro make_runstatus_struct, runstatus_struct

 on_error, 2

 if N_params() ne 1 then begin
        print,'Syntax - make_runstatus_struct, runstatus_struct'
        return
 endif

 runstatus_struct = sdss_runstatus_struct()
 return 
end



