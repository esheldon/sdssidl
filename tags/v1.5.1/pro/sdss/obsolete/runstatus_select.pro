;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;+
;
; NAME:
;    RUNSTATUS_SELECT
;
; PURPOSE:
;    Makes cuts on runs/reruns based on the runstatus flag structure. 
;    For tsobj_exist='N', adatc_exist='N', etc. this program will "or"
;    together the results from each camcol; i.e. if any camcol is missing
;    data then this check fails.  For 'Y' they are anded together so all
;    camcols must pass.
;
; CALLING SEQUENCE:
;    runstatus_select, flag_struct, select_index, input_index=input_index
; 
; Inputs:  
;    flag_struct: Premade flag structure. Set a flag to 'Y' to require
;       that attribute, to 'N' to require a lack of that attribute.
;    input_index: you can input an index, from an earlier selection
;		for instance. If this has size(input_index)(0)=0 then
;		the returned selection index will be -1
;
; Outputs: 
;   select_index: indices of selected runs/reruns
;   nkeep: number that passed cuts
;
; Examples:
;   1)
;   run_status = sdss_runstatus()
;   make_runstatus_struct, st
;   st.adatc_exist = 'Y'
;   runstatus_select, rs, si
;   print_struct,run_status[si]
;   
;   2)
;   w=where(run_status.tsobj_photo_v ge 5.3)
;   runstatus_select, rs, si, input_index=w
;   print_struct,run_status[si]
;
; Creation:
;  ??-??-2002 Erin Scott Sheldon UofMich
;  Added nkeep: 03-June-2003
;-
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;


pro runstatus_select, flag_struct, select_index, nkeep, input_index=input_index

  on_error, 2

  if n_params() LT 2 then begin
      print,'-syntax runstatus_select, flag_struct, select_index, nkeep, '+$
        'input_index=input_index'
      return
  endif

  rs = sdss_runstatus()
  select_index = sdss_flag_select(rs.flags, 'runstatus', flag_struct, $
      input_index=input_index) 
  return
 



  sdssidl_setup,/silent

  run_status = sdss_runstatus(exists=rs_exists)
  IF NOT rs_exists THEN BEGIN 
      message,'RUN_STATUS information not defined'
  ENDIF 

  IF n_elements(input_index) NE 0 THEN BEGIN 
      dt = size(input_index,/tname)
      IF (dt EQ 'INT') OR (dt EQ 'LONG') THEN BEGIN 
          IF input_index[0] EQ -1 THEN BEGIN 
              select_index = -1
              nkeep = 0
              return
          ENDIF ELSE BEGIN 
              k = input_index
          ENDELSE 
      ENDIF ELSE message,'input_index must be integer form'
  ENDIF ELSE BEGIN 
 	k=lindgen(n_elements(RUN_STATUS))
  ENDELSE 
  
  fs=flag_struct

  ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
  ;; Select on run status
  ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;


  ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
  ;; rerun is known
  ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

  IF (fs.RERUN_EXIST EQ 'Y') THEN BEGIN 
      h=where( run_status[k].rerun GE 0, nkeep)
      IF nkeep EQ 0 THEN BEGIN
          select_index = -1
          return
      ENDIF 
      k = k[h]
  ENDIF 
  IF (fs.RERUN_EXIST EQ 'N') THEN BEGIN 
      h=where( run_status[k].rerun LT 0, nkeep)
      IF nkeep EQ 0 THEN BEGIN
          select_index = -1
          return
      ENDIF 
      k = k[h]
  ENDIF 
  
  ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
  ;; stripe is known
  ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

  IF (fs.STRIPE_EXIST EQ 'Y') THEN BEGIN 
      h=where( run_status[k].stripe GT 0, nkeep)
      IF nkeep EQ 0 THEN BEGIN
          select_index = -1
          return
      ENDIF 
      k = k[h]
  ENDIF 
  IF (fs.STRIPE_EXIST EQ 'N') THEN BEGIN 
      h=where( run_status[k].stripe LE 0, nkeep)
      IF nkeep EQ 0 THEN BEGIN
          select_index = -1
          return
      ENDIF 
      k = k[h]
  ENDIF 

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
; Begin bad flags 
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

  bad = run_status.bad         ;backward because 'Y' means the bit is not set

  ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
  ;; astrans exists
  ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

  IF (fs.ASTRANS_EXIST EQ 'Y') THEN BEGIN 
      h=where( (bad[k] AND 2L^0) EQ 0, nkeep)
      IF nkeep EQ 0 THEN BEGIN
          select_index = -1
          return
      ENDIF 
      k = k[h]
  ENDIF 
  IF (fs.ASTRANS_EXIST EQ 'N') THEN BEGIN 
      h=where( (bad[k] AND 2L^0) NE 0, nkeep)
      IF nkeep EQ 0 THEN BEGIN
          select_index = -1
          return
      ENDIF 
      k = k[h]
  ENDIF 


  ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
  ;; ALL tsObj files exist
  ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

  offset = 0
  IF (fs.TSOBJ_EXIST EQ 'Y') THEN BEGIN 
      h=where( ( (bad[k] AND 2L^(1+offset) ) EQ 0 ) AND $
               ( (bad[k] AND 2L^(2+offset) ) EQ 0 ) AND $
               ( (bad[k] AND 2L^(3+offset) ) EQ 0 ) AND $
               ( (bad[k] AND 2L^(4+offset) ) EQ 0 ) AND $
               ( (bad[k] AND 2L^(5+offset) ) EQ 0 ) AND $
               ( (bad[k] AND 2L^(6+offset) ) EQ 0 ), nkeep)
      IF nkeep EQ 0 THEN BEGIN
          select_index = -1
          return
      ENDIF 
      k = k[h]
  ENDIF
  IF (fs.TSOBJ_EXIST EQ 'N') THEN BEGIN 
      h=where( ( (bad[k] AND 2L^(1+offset) ) NE 0 ) OR $
               ( (bad[k] AND 2L^(2+offset) ) NE 0 ) OR $
               ( (bad[k] AND 2L^(3+offset) ) NE 0 ) OR $
               ( (bad[k] AND 2L^(4+offset) ) NE 0 ) OR $
               ( (bad[k] AND 2L^(5+offset) ) NE 0 ) OR $
               ( (bad[k] AND 2L^(6+offset) ) NE 0 ), nkeep)
      IF nkeep EQ 0 THEN BEGIN
          select_index = -1
          return
      ENDIF 
      k = k[h]
  ENDIF 

  ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
  ;; tsObj files exist for a given camcol
  ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

  IF (fs.TSOBJ1_EXIST EQ 'Y') THEN BEGIN 
      h=where( (bad[k] AND 2L^(1+offset) ) EQ 0, nkeep)
      IF nkeep EQ 0 THEN BEGIN
          select_index = -1
          return
      ENDIF 
      k = k[h]
  ENDIF
  IF (fs.TSOBJ1_EXIST EQ 'N') THEN BEGIN 
      h=where( (bad[k] AND 2L^(1+offset) ) NE 0, nkeep)
      IF nkeep EQ 0 THEN BEGIN
          select_index = -1
          return
      ENDIF 
      k = k[h]
  ENDIF
  IF (fs.TSOBJ2_EXIST EQ 'Y') THEN BEGIN 
      h=where( (bad[k] AND 2L^(2+offset) ) EQ 0, nkeep)
      IF nkeep EQ 0 THEN BEGIN
          select_index = -1
          return
      ENDIF 
      k = k[h]
  ENDIF
  IF (fs.TSOBJ2_EXIST EQ 'N') THEN BEGIN 
      h=where( (bad[k] AND 2L^(2+offset) ) NE 0, nkeep)
      IF nkeep EQ 0 THEN BEGIN
          select_index = -1
          return
      ENDIF 
      k = k[h]
  ENDIF
  IF (fs.TSOBJ3_EXIST EQ 'Y') THEN BEGIN 
      h=where( (bad[k] AND 2L^(3+offset) ) EQ 0, nkeep)
      IF nkeep EQ 0 THEN BEGIN
          select_index = -1
          return
      ENDIF 
      k = k[h]
  ENDIF
  IF (fs.TSOBJ3_EXIST EQ 'N') THEN BEGIN 
      h=where( (bad[k] AND 2L^(3+offset) ) NE 0, nkeep)
      IF nkeep EQ 0 THEN BEGIN
          select_index = -1
          return
      ENDIF 
      k = k[h]
  ENDIF
  IF (fs.TSOBJ4_EXIST EQ 'Y') THEN BEGIN 
      h=where( (bad[k] AND 2L^(4+offset) ) EQ 0, nkeep)
      IF nkeep EQ 0 THEN BEGIN
          select_index = -1
          return
      ENDIF 
      k = k[h]
  ENDIF
  IF (fs.TSOBJ4_EXIST EQ 'N') THEN BEGIN 
      h=where( (bad[k] AND 2L^(4+offset) ) NE 0, nkeep)
      IF nkeep EQ 0 THEN BEGIN
          select_index = -1
          return
      ENDIF 
      k = k[h]
  ENDIF
  IF (fs.TSOBJ5_EXIST EQ 'Y') THEN BEGIN 
      h=where( (bad[k] AND 2L^(5+offset) ) EQ 0, nkeep)
      IF nkeep EQ 0 THEN BEGIN
          select_index = -1
          return
      ENDIF 
      k = k[h]
  ENDIF
  IF (fs.TSOBJ5_EXIST EQ 'N') THEN BEGIN 
      h=where( (bad[k] AND 2L^(5+offset) ) NE 0, nkeep)
      IF nkeep EQ 0 THEN BEGIN
          select_index = -1
          return
      ENDIF 
      k = k[h]
  ENDIF
  IF (fs.TSOBJ6_EXIST EQ 'Y') THEN BEGIN 
      h=where( (bad[k] AND 2L^(6+offset) ) EQ 0, nkeep)
      IF nkeep EQ 0 THEN BEGIN
          select_index = -1
          return
      ENDIF 
      k = k[h]
  ENDIF
  IF (fs.TSOBJ6_EXIST EQ 'N') THEN BEGIN 
      h=where( (bad[k] AND 2L^(6+offset) ) NE 0, nkeep)
      IF nkeep EQ 0 THEN BEGIN
          select_index = -1
          return
      ENDIF 
      k = k[h]
  ENDIF


  ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
  ;; All fpAtlas files exist
  ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

  offset = 6
  IF (fs.FPATLAS_EXIST EQ 'Y') THEN BEGIN 
      h=where( ( (bad[k] AND 2L^(1+offset) ) EQ 0 ) AND $
               ( (bad[k] AND 2L^(2+offset) ) EQ 0 ) AND $
               ( (bad[k] AND 2L^(3+offset) ) EQ 0 ) AND $
               ( (bad[k] AND 2L^(4+offset) ) EQ 0 ) AND $
               ( (bad[k] AND 2L^(5+offset) ) EQ 0 ) AND $
               ( (bad[k] AND 2L^(6+offset) ) EQ 0 ), nkeep)
      IF nkeep EQ 0 THEN BEGIN
          select_index = -1
          return
      ENDIF 
      k = k[h]
  ENDIF
  IF (fs.FPATLAS_EXIST EQ 'N') THEN BEGIN 
      h=where( ( (bad[k] AND 2L^(1+offset) ) NE 0 ) OR $
               ( (bad[k] AND 2L^(2+offset) ) NE 0 ) OR $
               ( (bad[k] AND 2L^(3+offset) ) NE 0 ) OR $
               ( (bad[k] AND 2L^(4+offset) ) NE 0 ) OR $
               ( (bad[k] AND 2L^(5+offset) ) NE 0 ) OR $
               ( (bad[k] AND 2L^(6+offset) ) NE 0 ), nkeep)
      IF nkeep EQ 0 THEN BEGIN
          select_index = -1
          return
      ENDIF 
      k = k[h]
  ENDIF 
  
  ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
  ;; fpAtlas files exist for a given camcol
  ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

  IF (fs.FPATLAS1_EXIST EQ 'Y') THEN BEGIN 
      h=where( (bad[k] AND 2L^(1+offset) ) EQ 0, nkeep)
      IF nkeep EQ 0 THEN BEGIN
          select_index = -1
          return
      ENDIF 
      k = k[h]
  ENDIF
  IF (fs.FPATLAS1_EXIST EQ 'N') THEN BEGIN 
      h=where( (bad[k] AND 2L^(1+offset) ) NE 0, nkeep)
      IF nkeep EQ 0 THEN BEGIN
          select_index = -1
          return
      ENDIF 
      k = k[h]
  ENDIF
  IF (fs.FPATLAS2_EXIST EQ 'Y') THEN BEGIN 
      h=where( (bad[k] AND 2L^(2+offset) ) EQ 0, nkeep)
      IF nkeep EQ 0 THEN BEGIN
          select_index = -1
          return
      ENDIF 
      k = k[h]
  ENDIF
  IF (fs.FPATLAS2_EXIST EQ 'N') THEN BEGIN 
      h=where( (bad[k] AND 2L^(2+offset) ) NE 0, nkeep)
      IF nkeep EQ 0 THEN BEGIN
          select_index = -1
          return
      ENDIF 
      k = k[h]
  ENDIF
  IF (fs.FPATLAS3_EXIST EQ 'Y') THEN BEGIN 
      h=where( (bad[k] AND 2L^(3+offset) ) EQ 0, nkeep)
      IF nkeep EQ 0 THEN BEGIN
          select_index = -1
          return
      ENDIF 
      k = k[h]
  ENDIF
  IF (fs.FPATLAS3_EXIST EQ 'N') THEN BEGIN 
      h=where( (bad[k] AND 2L^(3+offset) ) NE 0, nkeep)
      IF nkeep EQ 0 THEN BEGIN
          select_index = -1
          return
      ENDIF 
      k = k[h]
  ENDIF
  IF (fs.FPATLAS4_EXIST EQ 'Y') THEN BEGIN 
      h=where( (bad[k] AND 2L^(4+offset) ) EQ 0, nkeep)
      IF nkeep EQ 0 THEN BEGIN
          select_index = -1
          return
      ENDIF 
      k = k[h]
  ENDIF
  IF (fs.FPATLAS4_EXIST EQ 'N') THEN BEGIN 
      h=where( (bad[k] AND 2L^(4+offset) ) NE 0, nkeep)
      IF nkeep EQ 0 THEN BEGIN
          select_index = -1
          return
      ENDIF 
      k = k[h]
  ENDIF
  IF (fs.FPATLAS5_EXIST EQ 'Y') THEN BEGIN 
      h=where( (bad[k] AND 2L^(5+offset) ) EQ 0, nkeep)
      IF nkeep EQ 0 THEN BEGIN
          select_index = -1
          return
      ENDIF 
      k = k[h]
  ENDIF
  IF (fs.FPATLAS5_EXIST EQ 'N') THEN BEGIN 
      h=where( (bad[k] AND 2L^(5+offset) ) NE 0, nkeep)
      IF nkeep EQ 0 THEN BEGIN
          select_index = -1
          return
      ENDIF 
      k = k[h]
  ENDIF
  IF (fs.FPATLAS6_EXIST EQ 'Y') THEN BEGIN 
      h=where( (bad[k] AND 2L^(6+offset) ) EQ 0, nkeep)
      IF nkeep EQ 0 THEN BEGIN
          select_index = -1
          return
      ENDIF 
      k = k[h]
  ENDIF
  IF (fs.FPATLAS6_EXIST EQ 'N') THEN BEGIN 
      h=where( (bad[k] AND 2L^(6+offset) ) NE 0, nkeep)
      IF nkeep EQ 0 THEN BEGIN
          select_index = -1
          return
      ENDIF 
      k = k[h]
  ENDIF


  ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
  ;; All psField files exist
  ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

  offset = 12
  IF (fs.PSFIELD_EXIST EQ 'Y') THEN BEGIN 
      h=where( ( (bad[k] AND 2L^(1+offset) ) EQ 0 ) AND $
               ( (bad[k] AND 2L^(2+offset) ) EQ 0 ) AND $
               ( (bad[k] AND 2L^(3+offset) ) EQ 0 ) AND $
               ( (bad[k] AND 2L^(4+offset) ) EQ 0 ) AND $
               ( (bad[k] AND 2L^(5+offset) ) EQ 0 ) AND $
               ( (bad[k] AND 2L^(6+offset) ) EQ 0 ), nkeep)
      IF nkeep EQ 0 THEN BEGIN
          select_index = -1
          return
      ENDIF 
      k = k[h]
  ENDIF
  IF (fs.PSFIELD_EXIST EQ 'N') THEN BEGIN 
      h=where( ( (bad[k] AND 2L^(1+offset) ) NE 0 ) OR $
               ( (bad[k] AND 2L^(2+offset) ) NE 0 ) OR $
               ( (bad[k] AND 2L^(3+offset) ) NE 0 ) OR $
               ( (bad[k] AND 2L^(4+offset) ) NE 0 ) OR $
               ( (bad[k] AND 2L^(5+offset) ) NE 0 ) OR $
               ( (bad[k] AND 2L^(6+offset) ) NE 0 ), nkeep)
      IF nkeep EQ 0 THEN BEGIN
          select_index = -1
          return
      ENDIF 
      k = k[h]
  ENDIF 

  ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
  ;; psField files exist for a given camcol
  ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

  IF (fs.PSFIELD1_EXIST EQ 'Y') THEN BEGIN 
      h=where( (bad[k] AND 2L^(1+offset) ) EQ 0, nkeep)
      IF nkeep EQ 0 THEN BEGIN
          select_index = -1
          return
      ENDIF 
      k = k[h]
  ENDIF
  IF (fs.PSFIELD1_EXIST EQ 'N') THEN BEGIN 
      h=where( (bad[k] AND 2L^(1+offset) ) NE 0, nkeep)
      IF nkeep EQ 0 THEN BEGIN
          select_index = -1
          return
      ENDIF 
      k = k[h]
  ENDIF
  IF (fs.PSFIELD2_EXIST EQ 'Y') THEN BEGIN 
      h=where( (bad[k] AND 2L^(2+offset) ) EQ 0, nkeep)
      IF nkeep EQ 0 THEN BEGIN
          select_index = -1
          return
      ENDIF 
      k = k[h]
  ENDIF
  IF (fs.PSFIELD2_EXIST EQ 'N') THEN BEGIN 
      h=where( (bad[k] AND 2L^(2+offset) ) NE 0, nkeep)
      IF nkeep EQ 0 THEN BEGIN
          select_index = -1
          return
      ENDIF 
      k = k[h]
  ENDIF
  IF (fs.PSFIELD3_EXIST EQ 'Y') THEN BEGIN 
      h=where( (bad[k] AND 2L^(3+offset) ) EQ 0, nkeep)
      IF nkeep EQ 0 THEN BEGIN
          select_index = -1
          return
      ENDIF 
      k = k[h]
  ENDIF
  IF (fs.PSFIELD3_EXIST EQ 'N') THEN BEGIN 
      h=where( (bad[k] AND 2L^(3+offset) ) NE 0, nkeep)
      IF nkeep EQ 0 THEN BEGIN
          select_index = -1
          return
      ENDIF 
      k = k[h]
  ENDIF
  IF (fs.PSFIELD4_EXIST EQ 'Y') THEN BEGIN 
      h=where( (bad[k] AND 2L^(4+offset) ) EQ 0, nkeep)
      IF nkeep EQ 0 THEN BEGIN
          select_index = -1
          return
      ENDIF 
      k = k[h]
  ENDIF
  IF (fs.PSFIELD4_EXIST EQ 'N') THEN BEGIN 
      h=where( (bad[k] AND 2L^(4+offset) ) NE 0, nkeep)
      IF nkeep EQ 0 THEN BEGIN
          select_index = -1
          return
      ENDIF 
      k = k[h]
  ENDIF
  IF (fs.PSFIELD5_EXIST EQ 'Y') THEN BEGIN 
      h=where( (bad[k] AND 2L^(5+offset) ) EQ 0, nkeep)
      IF nkeep EQ 0 THEN BEGIN
          select_index = -1
          return
      ENDIF 
      k = k[h]
  ENDIF
  IF (fs.PSFIELD5_EXIST EQ 'N') THEN BEGIN 
      h=where( (bad[k] AND 2L^(5+offset) ) NE 0, nkeep)
      IF nkeep EQ 0 THEN BEGIN
          select_index = -1
          return
      ENDIF 
      k = k[h]
  ENDIF
  IF (fs.PSFIELD6_EXIST EQ 'Y') THEN BEGIN 
      h=where( (bad[k] AND 2L^(6+offset) ) EQ 0, nkeep)
      IF nkeep EQ 0 THEN BEGIN
          select_index = -1
          return
      ENDIF 
      k = k[h]
  ENDIF
  IF (fs.PSFIELD6_EXIST EQ 'N') THEN BEGIN 
      h=where( (bad[k] AND 2L^(6+offset) ) NE 0, nkeep)
      IF nkeep EQ 0 THEN BEGIN
          select_index = -1
          return
      ENDIF 
      k = k[h]
  ENDIF

  ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
  ;; All fpm files exist
  ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

  offset = 18
  IF (fs.FPM_EXIST EQ 'Y') THEN BEGIN 
      h=where( ( (bad[k] AND 2L^(1+offset) ) EQ 0 ) AND $
               ( (bad[k] AND 2L^(2+offset) ) EQ 0 ) AND $
               ( (bad[k] AND 2L^(3+offset) ) EQ 0 ) AND $
               ( (bad[k] AND 2L^(4+offset) ) EQ 0 ) AND $
               ( (bad[k] AND 2L^(5+offset) ) EQ 0 ) AND $
               ( (bad[k] AND 2L^(6+offset) ) EQ 0 ), nkeep)
      IF nkeep EQ 0 THEN BEGIN
          select_index = -1
          return
      ENDIF 
      k = k[h]
  ENDIF
  IF (fs.FPM_EXIST EQ 'N') THEN BEGIN 
      h=where( ( (bad[k] AND 2L^(1+offset) ) NE 0 ) OR $
               ( (bad[k] AND 2L^(2+offset) ) NE 0 ) OR $
               ( (bad[k] AND 2L^(3+offset) ) NE 0 ) OR $
               ( (bad[k] AND 2L^(4+offset) ) NE 0 ) OR $
               ( (bad[k] AND 2L^(5+offset) ) NE 0 ) OR $
               ( (bad[k] AND 2L^(6+offset) ) NE 0 ), nkeep)
      IF nkeep EQ 0 THEN BEGIN
          select_index = -1
          return
      ENDIF 
      k = k[h]
  ENDIF 

  ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
  ;; fpm files exist for a given camcol
  ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

  IF (fs.FPM1_EXIST EQ 'Y') THEN BEGIN 
      h=where( (bad[k] AND 2L^(1+offset) ) EQ 0, nkeep)
      IF nkeep EQ 0 THEN BEGIN
          select_index = -1
          return
      ENDIF 
      k = k[h]
  ENDIF
  IF (fs.FPM1_EXIST EQ 'N') THEN BEGIN 
      h=where( (bad[k] AND 2L^(1+offset) ) NE 0, nkeep)
      IF nkeep EQ 0 THEN BEGIN
          select_index = -1
          return
      ENDIF 
      k = k[h]
  ENDIF
  IF (fs.FPM2_EXIST EQ 'Y') THEN BEGIN 
      h=where( (bad[k] AND 2L^(2+offset) ) EQ 0, nkeep)
      IF nkeep EQ 0 THEN BEGIN
          select_index = -1
          return
      ENDIF 
      k = k[h]
  ENDIF
  IF (fs.FPM2_EXIST EQ 'N') THEN BEGIN 
      h=where( (bad[k] AND 2L^(2+offset) ) NE 0, nkeep)
      IF nkeep EQ 0 THEN BEGIN
          select_index = -1
          return
      ENDIF 
      k = k[h]
  ENDIF
  IF (fs.FPM3_EXIST EQ 'Y') THEN BEGIN 
      h=where( (bad[k] AND 2L^(3+offset) ) EQ 0, nkeep)
      IF nkeep EQ 0 THEN BEGIN
          select_index = -1
          return
      ENDIF 
      k = k[h]
  ENDIF
  IF (fs.FPM3_EXIST EQ 'N') THEN BEGIN 
      h=where( (bad[k] AND 2L^(3+offset) ) NE 0, nkeep)
      IF nkeep EQ 0 THEN BEGIN
          select_index = -1
          return
      ENDIF 
      k = k[h]
  ENDIF
  IF (fs.FPM4_EXIST EQ 'Y') THEN BEGIN 
      h=where( (bad[k] AND 2L^(4+offset) ) EQ 0, nkeep)
      IF nkeep EQ 0 THEN BEGIN
          select_index = -1
          return
      ENDIF 
      k = k[h]
  ENDIF
  IF (fs.FPM4_EXIST EQ 'N') THEN BEGIN 
      h=where( (bad[k] AND 2L^(4+offset) ) NE 0, nkeep)
      IF nkeep EQ 0 THEN BEGIN
          select_index = -1
          return
      ENDIF 
      k = k[h]
  ENDIF
  IF (fs.FPM5_EXIST EQ 'Y') THEN BEGIN 
      h=where( (bad[k] AND 2L^(5+offset) ) EQ 0, nkeep)
      IF nkeep EQ 0 THEN BEGIN
          select_index = -1
          return
      ENDIF 
      k = k[h]
  ENDIF
  IF (fs.FPM5_EXIST EQ 'N') THEN BEGIN 
      h=where( (bad[k] AND 2L^(5+offset) ) NE 0, nkeep)
      IF nkeep EQ 0 THEN BEGIN
          select_index = -1
          return
      ENDIF 
      k = k[h]
  ENDIF
  IF (fs.FPM6_EXIST EQ 'Y') THEN BEGIN 
      h=where( (bad[k] AND 2L^(6+offset) ) EQ 0, nkeep)
      IF nkeep EQ 0 THEN BEGIN
          select_index = -1
          return
      ENDIF 
      k = k[h]
  ENDIF
  IF (fs.FPM6_EXIST EQ 'N') THEN BEGIN 
      h=where( (bad[k] AND 2L^(6+offset) ) NE 0, nkeep)
      IF nkeep EQ 0 THEN BEGIN
          select_index = -1
          return
      ENDIF 
      k = k[h]
  ENDIF


  ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
  ;; All adatc files exist
  ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

  offset = 24
  IF (fs.ADATC_EXIST EQ 'Y') THEN BEGIN 
      h=where( ( (bad[k] AND 2L^(1+offset) ) EQ 0 ) AND $
               ( (bad[k] AND 2L^(2+offset) ) EQ 0 ) AND $
               ( (bad[k] AND 2L^(3+offset) ) EQ 0 ) AND $
               ( (bad[k] AND 2L^(4+offset) ) EQ 0 ) AND $
               ( (bad[k] AND 2L^(5+offset) ) EQ 0 ) AND $
               ( (bad[k] AND 2L^(6+offset) ) EQ 0 ), nkeep)
      IF nkeep EQ 0 THEN BEGIN
          select_index = -1
          return
      ENDIF 
      k = k[h]
  ENDIF
  IF (fs.ADATC_EXIST EQ 'N') THEN BEGIN 
      h=where( ( (bad[k] AND 2L^(1+offset) ) NE 0 ) OR $
               ( (bad[k] AND 2L^(2+offset) ) NE 0 ) OR $
               ( (bad[k] AND 2L^(3+offset) ) NE 0 ) OR $
               ( (bad[k] AND 2L^(4+offset) ) NE 0 ) OR $
               ( (bad[k] AND 2L^(5+offset) ) NE 0 ) OR $
               ( (bad[k] AND 2L^(6+offset) ) NE 0 ), nkeep)
      IF nkeep EQ 0 THEN BEGIN
          select_index = -1
          return
      ENDIF 
      k = k[h]
  ENDIF 

  ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
  ;; adatc files exist for a given camcol
  ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

  IF (fs.ADATC1_EXIST EQ 'Y') THEN BEGIN 
      h=where( (bad[k] AND 2L^(1+offset) ) EQ 0, nkeep)
      IF nkeep EQ 0 THEN BEGIN
          select_index = -1
          return
      ENDIF 
      k = k[h]
  ENDIF
  IF (fs.ADATC1_EXIST EQ 'N') THEN BEGIN 
      h=where( (bad[k] AND 2L^(1+offset) ) NE 0, nkeep)
      IF nkeep EQ 0 THEN BEGIN
          select_index = -1
          return
      ENDIF 
      k = k[h]
  ENDIF
  IF (fs.ADATC2_EXIST EQ 'Y') THEN BEGIN 
      h=where( (bad[k] AND 2L^(2+offset) ) EQ 0, nkeep)
      IF nkeep EQ 0 THEN BEGIN
          select_index = -1
          return
      ENDIF 
      k = k[h]
  ENDIF
  IF (fs.ADATC2_EXIST EQ 'N') THEN BEGIN 
      h=where( (bad[k] AND 2L^(2+offset) ) NE 0, nkeep)
      IF nkeep EQ 0 THEN BEGIN
          select_index = -1
          return
      ENDIF 
      k = k[h]
  ENDIF
  IF (fs.ADATC3_EXIST EQ 'Y') THEN BEGIN 
      h=where( (bad[k] AND 2L^(3+offset) ) EQ 0, nkeep)
      IF nkeep EQ 0 THEN BEGIN
          select_index = -1
          return
      ENDIF 
      k = k[h]
  ENDIF
  IF (fs.ADATC3_EXIST EQ 'N') THEN BEGIN 
      h=where( (bad[k] AND 2L^(3+offset) ) NE 0, nkeep)
      IF nkeep EQ 0 THEN BEGIN
          select_index = -1
          return
      ENDIF 
      k = k[h]
  ENDIF
  IF (fs.ADATC4_EXIST EQ 'Y') THEN BEGIN 
      h=where( (bad[k] AND 2L^(4+offset) ) EQ 0, nkeep)
      IF nkeep EQ 0 THEN BEGIN
          select_index = -1
          return
      ENDIF 
      k = k[h]
  ENDIF
  IF (fs.ADATC4_EXIST EQ 'N') THEN BEGIN 
      h=where( (bad[k] AND 2L^(4+offset) ) NE 0, nkeep)
      IF nkeep EQ 0 THEN BEGIN
          select_index = -1
          return
      ENDIF 
      k = k[h]
  ENDIF
  IF (fs.ADATC5_EXIST EQ 'Y') THEN BEGIN 
      h=where( (bad[k] AND 2L^(5+offset) ) EQ 0, nkeep)
      IF nkeep EQ 0 THEN BEGIN
          select_index = -1
          return
      ENDIF 
      k = k[h]
  ENDIF
  IF (fs.ADATC5_EXIST EQ 'N') THEN BEGIN 
      h=where( (bad[k] AND 2L^(5+offset) ) NE 0, nkeep)
      IF nkeep EQ 0 THEN BEGIN
          select_index = -1
          return
      ENDIF 
      k = k[h]
  ENDIF
  IF (fs.ADATC6_EXIST EQ 'Y') THEN BEGIN 
      h=where( (bad[k] AND 2L^(6+offset) ) EQ 0, nkeep)
      IF nkeep EQ 0 THEN BEGIN
          select_index = -1
          return
      ENDIF 
      k = k[h]
  ENDIF
  IF (fs.ADATC6_EXIST EQ 'N') THEN BEGIN 
      h=where( (bad[k] AND 2L^(6+offset) ) NE 0, nkeep)
      IF nkeep EQ 0 THEN BEGIN
          select_index = -1
          return
      ENDIF 
      k = k[h]
  ENDIF


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
; Begin bad2 flags
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

  IF NOT tag_exist(run_status, 'bad2') THEN return
  bad2 = run_status.bad2

  ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
  ;; All tsfield files exist
  ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

  offset = 0
  IF (fs.TSFIELD_EXIST EQ 'Y') THEN BEGIN 
      h=where( ( (bad2[k] AND 2L^(1+offset) ) EQ 0 ) AND $
               ( (bad2[k] AND 2L^(2+offset) ) EQ 0 ) AND $
               ( (bad2[k] AND 2L^(3+offset) ) EQ 0 ) AND $
               ( (bad2[k] AND 2L^(4+offset) ) EQ 0 ) AND $
               ( (bad2[k] AND 2L^(5+offset) ) EQ 0 ) AND $
               ( (bad2[k] AND 2L^(6+offset) ) EQ 0 ), nkeep)
      IF nkeep EQ 0 THEN BEGIN
          select_index = -1
          return
      ENDIF 
      k = k[h]
  ENDIF
  IF (fs.TSFIELD_EXIST EQ 'N') THEN BEGIN 
      h=where( ( (bad2[k] AND 2L^(1+offset) ) NE 0 ) OR $
               ( (bad2[k] AND 2L^(2+offset) ) NE 0 ) OR $
               ( (bad2[k] AND 2L^(3+offset) ) NE 0 ) OR $
               ( (bad2[k] AND 2L^(4+offset) ) NE 0 ) OR $
               ( (bad2[k] AND 2L^(5+offset) ) NE 0 ) OR $
               ( (bad2[k] AND 2L^(6+offset) ) NE 0 ), nkeep)
      IF nkeep EQ 0 THEN BEGIN
          select_index = -1
          return
      ENDIF 
      k = k[h]
  ENDIF 

  ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
  ;; tsfield files exist for a given camcol
  ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

  IF (fs.TSFIELD1_EXIST EQ 'Y') THEN BEGIN 
      h=where( (bad2[k] AND 2L^(1+offset) ) EQ 0, nkeep)
      IF nkeep EQ 0 THEN BEGIN
          select_index = -1
          return
      ENDIF 
      k = k[h]
  ENDIF
  IF (fs.TSFIELD1_EXIST EQ 'N') THEN BEGIN 
      h=where( (bad2[k] AND 2L^(1+offset) ) NE 0, nkeep)
      IF nkeep EQ 0 THEN BEGIN
          select_index = -1
          return
      ENDIF 
      k = k[h]
  ENDIF
  IF (fs.TSFIELD2_EXIST EQ 'Y') THEN BEGIN 
      h=where( (bad2[k] AND 2L^(2+offset) ) EQ 0, nkeep)
      IF nkeep EQ 0 THEN BEGIN
          select_index = -1
          return
      ENDIF 
      k = k[h]
  ENDIF
  IF (fs.TSFIELD2_EXIST EQ 'N') THEN BEGIN 
      h=where( (bad2[k] AND 2L^(2+offset) ) NE 0, nkeep)
      IF nkeep EQ 0 THEN BEGIN
          select_index = -1
          return
      ENDIF 
      k = k[h]
  ENDIF
  IF (fs.TSFIELD3_EXIST EQ 'Y') THEN BEGIN 
      h=where( (bad2[k] AND 2L^(3+offset) ) EQ 0, nkeep)
      IF nkeep EQ 0 THEN BEGIN
          select_index = -1
          return
      ENDIF 
      k = k[h]
  ENDIF
  IF (fs.TSFIELD3_EXIST EQ 'N') THEN BEGIN 
      h=where( (bad2[k] AND 2L^(3+offset) ) NE 0, nkeep)
      IF nkeep EQ 0 THEN BEGIN
          select_index = -1
          return
      ENDIF 
      k = k[h]
  ENDIF
  IF (fs.TSFIELD4_EXIST EQ 'Y') THEN BEGIN 
      h=where( (bad2[k] AND 2L^(4+offset) ) EQ 0, nkeep)
      IF nkeep EQ 0 THEN BEGIN
          select_index = -1
          return
      ENDIF 
      k = k[h]
  ENDIF
  IF (fs.TSFIELD4_EXIST EQ 'N') THEN BEGIN 
      h=where( (bad2[k] AND 2L^(4+offset) ) NE 0, nkeep)
      IF nkeep EQ 0 THEN BEGIN
          select_index = -1
          return
      ENDIF 
      k = k[h]
  ENDIF
  IF (fs.TSFIELD5_EXIST EQ 'Y') THEN BEGIN 
      h=where( (bad2[k] AND 2L^(5+offset) ) EQ 0, nkeep)
      IF nkeep EQ 0 THEN BEGIN
          select_index = -1
          return
      ENDIF 
      k = k[h]
  ENDIF
  IF (fs.TSFIELD5_EXIST EQ 'N') THEN BEGIN 
      h=where( (bad2[k] AND 2L^(5+offset) ) NE 0, nkeep)
      IF nkeep EQ 0 THEN BEGIN
          select_index = -1
          return
      ENDIF 
      k = k[h]
  ENDIF
  IF (fs.TSFIELD6_EXIST EQ 'Y') THEN BEGIN 
      h=where( (bad2[k] AND 2L^(6+offset) ) EQ 0, nkeep)
      IF nkeep EQ 0 THEN BEGIN
          select_index = -1
          return
      ENDIF 
      k = k[h]
  ENDIF
  IF (fs.TSFIELD6_EXIST EQ 'N') THEN BEGIN 
      h=where( (bad2[k] AND 2L^(6+offset) ) NE 0, nkeep)
      IF nkeep EQ 0 THEN BEGIN
          select_index = -1
          return
      ENDIF 
      k = k[h]
  ENDIF

  ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
  ;; All photoz files exist
  ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

  offset = 6
  IF (fs.PHOTOZ_EXIST EQ 'Y') THEN BEGIN 
      h=where( ( (bad2[k] AND 2L^(1+offset) ) EQ 0 ) AND $
               ( (bad2[k] AND 2L^(2+offset) ) EQ 0 ) AND $
               ( (bad2[k] AND 2L^(3+offset) ) EQ 0 ) AND $
               ( (bad2[k] AND 2L^(4+offset) ) EQ 0 ) AND $
               ( (bad2[k] AND 2L^(5+offset) ) EQ 0 ) AND $
               ( (bad2[k] AND 2L^(6+offset) ) EQ 0 ), nkeep)
      IF nkeep EQ 0 THEN BEGIN
          select_index = -1
          return
      ENDIF 
      k = k[h]
  ENDIF
  IF (fs.PHOTOZ_EXIST EQ 'N') THEN BEGIN 
      h=where( ( (bad2[k] AND 2L^(1+offset) ) NE 0 ) OR $
               ( (bad2[k] AND 2L^(2+offset) ) NE 0 ) OR $
               ( (bad2[k] AND 2L^(3+offset) ) NE 0 ) OR $
               ( (bad2[k] AND 2L^(4+offset) ) NE 0 ) OR $
               ( (bad2[k] AND 2L^(5+offset) ) NE 0 ) OR $
               ( (bad2[k] AND 2L^(6+offset) ) NE 0 ), nkeep)
      IF nkeep EQ 0 THEN BEGIN
          select_index = -1
          return
      ENDIF 
      k = k[h]
  ENDIF 

  ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
  ;; photoz files exist for a given camcol
  ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

  IF (fs.PHOTOZ1_EXIST EQ 'Y') THEN BEGIN 
      h=where( (bad2[k] AND 2L^(1+offset) ) EQ 0, nkeep)
      IF nkeep EQ 0 THEN BEGIN
          select_index = -1
          return
      ENDIF 
      k = k[h]
  ENDIF
  IF (fs.PHOTOZ1_EXIST EQ 'N') THEN BEGIN 
      h=where( (bad2[k] AND 2L^(1+offset) ) NE 0, nkeep)
      IF nkeep EQ 0 THEN BEGIN
          select_index = -1
          return
      ENDIF 
      k = k[h]
  ENDIF
  IF (fs.PHOTOZ2_EXIST EQ 'Y') THEN BEGIN 
      h=where( (bad2[k] AND 2L^(2+offset) ) EQ 0, nkeep)
      IF nkeep EQ 0 THEN BEGIN
          select_index = -1
          return
      ENDIF 
      k = k[h]
  ENDIF
  IF (fs.PHOTOZ2_EXIST EQ 'N') THEN BEGIN 
      h=where( (bad2[k] AND 2L^(2+offset) ) NE 0, nkeep)
      IF nkeep EQ 0 THEN BEGIN
          select_index = -1
          return
      ENDIF 
      k = k[h]
  ENDIF
  IF (fs.PHOTOZ3_EXIST EQ 'Y') THEN BEGIN 
      h=where( (bad2[k] AND 2L^(3+offset) ) EQ 0, nkeep)
      IF nkeep EQ 0 THEN BEGIN
          select_index = -1
          return
      ENDIF 
      k = k[h]
  ENDIF
  IF (fs.PHOTOZ3_EXIST EQ 'N') THEN BEGIN 
      h=where( (bad2[k] AND 2L^(3+offset) ) NE 0, nkeep)
      IF nkeep EQ 0 THEN BEGIN
          select_index = -1
          return
      ENDIF 
      k = k[h]
  ENDIF
  IF (fs.PHOTOZ4_EXIST EQ 'Y') THEN BEGIN 
      h=where( (bad2[k] AND 2L^(4+offset) ) EQ 0, nkeep)
      IF nkeep EQ 0 THEN BEGIN
          select_index = -1
          return
      ENDIF 
      k = k[h]
  ENDIF
  IF (fs.PHOTOZ4_EXIST EQ 'N') THEN BEGIN 
      h=where( (bad2[k] AND 2L^(4+offset) ) NE 0, nkeep)
      IF nkeep EQ 0 THEN BEGIN
          select_index = -1
          return
      ENDIF 
      k = k[h]
  ENDIF
  IF (fs.PHOTOZ5_EXIST EQ 'Y') THEN BEGIN 
      h=where( (bad2[k] AND 2L^(5+offset) ) EQ 0, nkeep)
      IF nkeep EQ 0 THEN BEGIN
          select_index = -1
          return
      ENDIF 
      k = k[h]
  ENDIF
  IF (fs.PHOTOZ5_EXIST EQ 'N') THEN BEGIN 
      h=where( (bad2[k] AND 2L^(5+offset) ) NE 0, nkeep)
      IF nkeep EQ 0 THEN BEGIN
          select_index = -1
          return
      ENDIF 
      k = k[h]
  ENDIF
  IF (fs.PHOTOZ6_EXIST EQ 'Y') THEN BEGIN 
      h=where( (bad2[k] AND 2L^(6+offset) ) EQ 0, nkeep)
      IF nkeep EQ 0 THEN BEGIN
          select_index = -1
          return
      ENDIF 
      k = k[h]
  ENDIF
  IF (fs.PHOTOZ6_EXIST EQ 'N') THEN BEGIN 
      h=where( (bad2[k] AND 2L^(6+offset) ) NE 0, nkeep)
      IF nkeep EQ 0 THEN BEGIN
          select_index = -1
          return
      ENDIF 
      k = k[h]
  ENDIF




  select_index=k 

return

end

