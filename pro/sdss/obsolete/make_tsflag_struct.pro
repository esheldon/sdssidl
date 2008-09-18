;+
; NAME:
;       MAKE_TSFLAG_STRUCT
; PURPOSE:
;	Set up a parameter structure for sdss target flag selection.
;
; INPUTS: none
;       
; OUTPUTS:
;	flag_struct: the structure used for sdss object selection....
;; 
; PROCEDURE: This sets up the structure for target flag selection of sdss objects
;	
;
; REVISION HISTORY: Sarah Monk 5/8/00 (modified from make_flag_struct)
;-


pro make_tsflag_struct, tsflag_struct

 on_error, 2

 if N_params() ne 1 then begin
        print,'Syntax - make_tsflag_struct, tsflag_struct'
        return
 endif

 tsflag_struct = { $              ;Begin flags
                   QSO_HIZ: 'D', $
                   QSO_CAP: 'D', $
                   QSO_SKIRT: 'D', $
                   QSO_FIRST_CAP: 'D', $
                   QSO_FIRST_SKIRT: 'D', $
                   QSO_MAG_OUTLIER: 'D', $
                   QSO_FAINT: 'D', $
                   QSO_REJECT: 'D', $
                   $
                   GALAXY_RED: 'D', $
                   GALAXY_RED_II: 'D', $
                   GALAXY: 'D', $
                   GALAXY_BIG: 'D', $
                   GALAXY_BRIGHT_CORE: 'D', $
                   $
                   ROSAT_A: 'D', $
                   ROSAT_B: 'D', $
                   ROSAT_C: 'D', $
                   ROSAT_D: 'D', $
                   ROSAT_E: 'D', $
                   $
                   STAR_BHB: 'D', $
                   STAR_CARBON: 'D', $
                   STAR_BROWN_DWARF: 'D', $
                   STAR_SUB_DWARF: 'D', $
                   STAR_CATY_VAR: 'D', $
                   STAR_RED_DWARF: 'D', $
                   STAR_WHITE_DWARF: 'D', $
                   STAR_PN: 'D', $
                   $
                   SERENDIP_BLUE: 'D', $
                   SERENDIP_FIRST: 'D', $
                   SERENDIP_RED: 'D', $
                   SERENDIP_DISTANT: 'D', $
                   SERENDIP_MANUAL: 'D', $
                   $
                   SOUTHERN_SURVEY: 'D', $
                   $
                   $            ;begin sectarget
                   LIGHT_TRAP: 'D', $ 
                   REDDEN_STD: 'D', $ 
                   TEST_TARGET: 'D', $ 
                   QA: 'D', $
                   SKY: 'D', $
                   SPECTROPHOTO_STD: 'D', $
                   GUIDE_STAR: 'D', $
                   BUNDLE_HOLE: 'D', $ 
                   QUALITY_HOLE: 'D', $ 
                   HOT_STD: 'D'}


  return 
  end

