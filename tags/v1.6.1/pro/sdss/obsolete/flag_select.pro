;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;+
;
; NAME:
;    FLAG_SELECT
; PURPOSE:
;    Makes cuts based on SDSS flags FLAGS[clr], FLAGS2[clr], or OBJC_FLAGS using the input flag structure. 
;    These cuts are strictly "anded" together, so they must all be true for 
;    the object to survive.
; 
; Inputs:  pstruct: a photo output structure (must have .flags tag...)
;	   flag_struct: Premade flag structure. This will require any
;		flags set to 'Y' and insist that any flag set to 'N' be
;		off
;	   colorindex: which color do you want to cut on...
;	   objc: set if you want to select on objc flags
;	   input_index: you can input an index, from an earlier selection
;		for instance. If this has input_index is -1 then
;		the returned selection index will be -1
;
; Outputs: select_index: indices of selected objects....
;          nkeep: number of objects that passed cuts.
;
; Author:  Tim McKay
; Date: 1/8/99
; Phil Fischer: 1/15/99
; Erin Scott Sheldon UM 2/5/00 Added flags2
; Erin Scott Sheldon UM 20-Mar-2001 Corrected Bug. Made more easily 
;       updatable. Added new flags.
; E.S.S. Added AMOMENT_UNWEIGHTED flag (same bit as faint)
; E.S.S. Added nkeep. 03-June-2003
; Fixed poor memory usage. 11-Feb-2004 E.S.S.
;
;-
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;


pro flag_select, pstruct, flag_struct, colorindex, select_index, nkeep, objc=objc, input_index=input_index


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;Help message
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

  on_error,2

  if n_params() LT 3 then begin
      print,'-syntax flag_select, pstruct, flag_struct, colorindex, select_index, nkeep, objc=objc, input_index=input_index'
      return
  endif

  if keyword_set(input_index) then begin
	if input_index[0] EQ -1 then begin
	   select_index = -1
           nkeep = 0L
	   return
	endif else begin
	   k = input_index
	endelse
  endif else begin
	k=lindgen(n_elements(pstruct))
  endelse

  tags=tag_names(pstruct)
  w1=where(tags EQ 'FLAGS2', nw1)
  w2=where(tags EQ 'OBJC_FLAGS2', nw2)

  IF NOT keyword_set(objc) THEN BEGIN
      f=long(pstruct.flags[colorindex])
      IF nw1 EQ 0 THEN BEGIN
          doflags2 = 0
      ENDIF ELSE BEGIN 
          doflags2 = 1
          f2=long(pstruct.flags2[colorindex])
      ENDELSE 
  ENDIF ELSE BEGIN 
      f=long(pstruct.objc_flags)
      IF nw2 EQ 0 THEN BEGIN
          doflags2 = 0
      ENDIF ELSE BEGIN 
          doflags2 = 1
          f2=long(pstruct.objc_flags2)
      ENDELSE 
  ENDELSE 
  
  fs=flag_struct

  ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
  ;; Begin flags1
  ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

  OBJECT1_CANONICAL_CENTER =    '1'X ; used canonical, not local, centre   
  OBJECT1_BRIGHT       =        '2'X ; detected by Bright Objects          
  OBJECT1_EDGE         =        '4'X ; object is too close to edge of frame
  OBJECT1_BLENDED      =        '8'X ; object is/was blended               
  OBJECT1_CHILD        =       '10'X ; object is a child                   
  OBJECT1_PEAKCENTER   =       '20'X ; given centre position of peak pixel 
  OBJECT1_NODEBLEND    =       '40'X ; no deblending attempted             
  OBJECT1_NOPROFILE    =       '80'X ; too small to estimate a profile     
  OBJECT1_NOPETRO      =      '100'X ; no Petrosian radius                 
  OBJECT1_MANYPETRO    =      '200'X ; more than one Petrosian radius      
  OBJECT1_NOPETRO_BIG  =      '400'X ; no Petrosian radius as object is too big
  OBJECT1_DEBLEND_TOO_MANY_PEAKS = '800'X ; too many peaks to deblend      
  OBJECT1_CR            =     '1000'X ; contains a CR pixel                
  OBJECT1_MANYR50      =     '2000'X ; more than one 50% radius            
  OBJECT1_MANYR90      =     '4000'X ; more than one 90% radius            
  OBJECT1_BAD_RADIAL   =     '8000'X ; some low S/N radial points          
  OBJECT1_INCOMPLETE_PROFILE = '10000'X ; r_P includes off-frame pixels    
  OBJECT1_INTERP       =    '20000'X ; object contains interpolated pixels 
  OBJECT1_SATUR        =    '40000'X ; object contains saturated pixels    
  OBJECT1_NOTCHECKED   =    '80000'X ; object contains NOTCHECKED pixels   
  OBJECT1_SUBTRACTED   =   '100000'X ; object had wings subtracted         
  OBJECT1_NOSTOKES     =   '200000'X ; object has no measured stokes params
  OBJECT1_BADSKY       =   '400000'X ; sky level so bad that object is -ve 
  OBJECT1_PETROFAINT   =   '800000'X ; >= 1 Petrosian radius too faint     
  OBJECT1_TOO_LARGE    =  '1000000'X ; object is too large                 
  OBJECT1_DEBLENDED_AS_PSF = '2000000'X ; deblender treated obj as PSF     
  OBJECT1_DEBLEND_PRUNED = '4000000'X ; deblender pruned peak list         
  OBJECT1_ELLIPFAINT   = '8000000'X ; Centre's fainter than desired elliptical isophote 
  OBJECT1_BINNED1      = '10000000'X ; object was found in 1x1 binned image
  OBJECT1_BINNED2      = '20000000'X ; object was found in 2x2 binned image
  OBJECT1_BINNED4      = '40000000'X ; object was found in 4x4 binned image
  OBJECT1_MOVED        = '80000000'X  ; Object appears to have moved during 
                                       ; the exposure.  May have been        
                                       ; deblended as a moving object.       

  if (fs.CANONICAL_CENTER eq 'Y') then begin
	h=where((f[k] and OBJECT1_CANONICAL_CENTER) ne 0,nkeep)
	if (nkeep eq 0) then begin
	     select_index = -1
	     return
	endif
	k=k[h]
  endif 
  if (fs.CANONICAL_CENTER eq 'N') then begin
	h=where((f[k] and OBJECT1_CANONICAL_CENTER) eq 0,nkeep)
	if (nkeep eq 0) then begin
	     select_index = -1
	     return
	endif
	k=k[h]
  endif

  if (fs.BRIGHT eq 'Y') then begin
	h=where((f[k] and OBJECT1_BRIGHT) ne 0,nkeep)
	if (nkeep eq 0) then begin
	     select_index = -1
	     return
	endif
	k=k[h]
  endif 
  if (fs.BRIGHT eq 'N') then begin
	h=where((f[k] and OBJECT1_BRIGHT) eq 0,nkeep)
	if (nkeep eq 0) then begin
	     select_index = -1
	     return
	endif
	k=k[h]
  endif

  if (fs.EDGE eq 'Y') then begin
	h=where((f[k] and OBJECT1_EDGE) ne 0,nkeep)
	if (nkeep eq 0) then begin
	     select_index = -1
	     return
	endif
	k=k[h]
  endif 
  if (fs.EDGE eq 'N') then begin
	h=where((f[k] and OBJECT1_EDGE) eq 0,nkeep)
	if (nkeep eq 0) then begin
	     select_index = -1
	     return
	endif
	k=k[h]
  endif

  if (fs.BLENDED eq 'Y') then begin
	h=where((f[k] and OBJECT1_BLENDED) ne 0,nkeep)
	if (nkeep eq 0) then begin
	     select_index = -1
	     return
	endif
	k=k[h]
  endif 
  if (fs.BLENDED eq 'N') then begin
	h=where((f[k] and OBJECT1_BLENDED) eq 0,nkeep)
	if (nkeep eq 0) then begin
	     select_index = -1
	     return
	endif
	k=k[h]
  endif

  if (fs.CHILD eq 'Y') then begin
	h=where((f[k] and OBJECT1_CHILD) ne 0,nkeep)
	if (nkeep eq 0) then begin
	     select_index = -1
	     return
	endif
	k=k[h]
  endif 
  if (fs.CHILD eq 'N') then begin
	h=where((f[k] and OBJECT1_CHILD) eq 0,nkeep)
	if (nkeep eq 0) then begin
	     select_index = -1
	     return
	endif
	k=k[h]
  endif

  if (fs.PEAKCENTER eq 'Y') then begin
	h=where((f[k] and OBJECT1_PEAKCENTER) ne 0,nkeep)
	if (nkeep eq 0) then begin
	     select_index = -1
	     return
	endif
	k=k[h]
  endif 
  if (fs.PEAKCENTER eq 'N') then begin
	h=where((f[k] and OBJECT1_PEAKCENTER) eq 0,nkeep)
	if (nkeep eq 0) then begin
	     select_index = -1
	     return
	endif
	k=k[h]
  endif

  if (fs.NODEBLEND eq 'Y') then begin
	h=where((f[k] and OBJECT1_NODEBLEND) ne 0,nkeep)
	if (nkeep eq 0) then begin
	     select_index = -1
	     return
	endif
	k=k[h]
  endif 
  if (fs.NODEBLEND eq 'N') then begin
	h=where((f[k] and OBJECT1_NODEBLEND) eq 0,nkeep)
	if (nkeep eq 0) then begin
	     select_index = -1
	     return
	endif
	k=k[h]
  endif

  if (fs.NOPROFILE eq 'Y') then begin
	h=where((f[k] and OBJECT1_NOPROFILE) ne 0,nkeep)
	if (nkeep eq 0) then begin
	     select_index = -1
	     return
	endif
	k=k[h]
  endif 
  if (fs.NOPROFILE eq 'N') then begin
	h=where((f[k] and OBJECT1_NOPROFILE) eq 0,nkeep)
	if (nkeep eq 0) then begin
	     select_index = -1
	     return
	endif
	k=k[h]
  endif

  if (fs.NOPETRO eq 'Y') then begin
	h=where((f[k] and OBJECT1_NOPETRO) ne 0,nkeep)
	if (nkeep eq 0) then begin
	     select_index = -1
	     return
	endif
	k=k[h]
  endif 
  if (fs.NOPETRO eq 'N') then begin
	h=where((f[k] and OBJECT1_NOPETRO) eq 0,nkeep)
	if (nkeep eq 0) then begin
	     select_index = -1
	     return
	endif
	k=k[h]
  endif

  if (fs.MANYPETRO eq 'Y') then begin
	h=where((f[k] and OBJECT1_MANYPETRO) ne 0,nkeep)
	if (nkeep eq 0) then begin
	     select_index = -1
	     return
	endif
	k=k[h]
  endif 
  if (fs.MANYPETRO eq 'N') then begin
	h=where((f[k] and OBJECT1_MANYPETRO) eq 0,nkeep)
	if (nkeep eq 0) then begin
	     select_index = -1
	     return
	endif
	k=k[h]
  endif

  if (fs.NOPETRO_BIG eq 'Y') then begin
	h=where((f[k] and OBJECT1_NOPETRO_BIG) ne 0,nkeep)
	if (nkeep eq 0) then begin
	     select_index = -1
	     return
	endif
	k=k[h]
  endif 
  if (fs.NOPETRO_BIG eq 'N') then begin
	h=where((f[k] and OBJECT1_NOPETRO_BIG) eq 0,nkeep)
	if (nkeep eq 0) then begin
	     select_index = -1
	     return
	endif
	k=k[h]
  endif

  if (fs.DEBLEND_TOO_MANY_PEAKS eq 'Y') then begin
	h=where((f[k] and OBJECT1_DEBLEND_TOO_MANY_PEAKS) ne 0,nkeep)
	if (nkeep eq 0) then begin
	     select_index = -1
	     return
	endif
	k=k[h]
  endif 
  if (fs.DEBLEND_TOO_MANY_PEAKS eq 'N') then begin
	h=where((f[k] and OBJECT1_DEBLEND_TOO_MANY_PEAKS) eq 0,nkeep)
	if (nkeep eq 0) then begin
	     select_index = -1
	     return
	endif
	k=k[h]
  endif

  if (fs.CR eq 'Y') then begin
	h=where((f[k] and OBJECT1_CR) ne 0,nkeep)
	if (nkeep eq 0) then begin
	     select_index = -1
	     return
	endif
	k=k[h]
  endif 
  if (fs.CR eq 'N') then begin
	h=where((f[k] and OBJECT1_CR) eq 0,nkeep)
	if (nkeep eq 0) then begin
	     select_index = -1
	     return
	endif
	k=k[h]
  endif

  if (fs.MANYR50 eq 'Y') then begin
	h=where((f[k] and OBJECT1_MANYR50) ne 0,nkeep)
	if (nkeep eq 0) then begin
	     select_index = -1
	     return
	endif
	k=k[h]
  endif 
  if (fs.MANYR50 eq 'N') then begin
	h=where((f[k] and OBJECT1_MANYR50) eq 0,nkeep)
	if (nkeep eq 0) then begin
	     select_index = -1
	     return
	endif
	k=k[h]
  endif

  if (fs.MANYR90 eq 'Y') then begin
	h=where((f[k] and OBJECT1_MANYR90) ne 0,nkeep)
	if (nkeep eq 0) then begin
	     select_index = -1
	     return
	endif
	k=k[h]
  endif 
  if (fs.MANYR90 eq 'N') then begin
	h=where((f[k] and OBJECT1_MANYR90) eq 0,nkeep)
	if (nkeep eq 0) then begin
	     select_index = -1
	     return
	endif
	k=k[h]
  endif

  if (fs.BAD_RADIAL eq 'Y') then begin
	h=where((f[k] and OBJECT1_BAD_RADIAL) ne 0,nkeep)
	if (nkeep eq 0) then begin
	     select_index = -1
	     return
	endif
	k=k[h]
  endif 
  if (fs.BAD_RADIAL eq 'N') then begin
	h=where((f[k] and OBJECT1_BAD_RADIAL) eq 0,nkeep)
	if (nkeep eq 0) then begin
	     select_index = -1
	     return
	endif
	k=k[h]
  endif

  if (fs.INCOMPLETE_PROFILE eq 'Y') then begin
	h=where((f[k] and OBJECT1_INCOMPLETE_PROFILE) ne 0,nkeep)
	if (nkeep eq 0) then begin
	     select_index = -1
	     return
	endif
	k=k[h]
  endif 
  if (fs.INCOMPLETE_PROFILE eq 'N') then begin
	h=where((f[k] and OBJECT1_INCOMPLETE_PROFILE) eq 0,nkeep)
	k=k[h]
	if (nkeep eq 0) then begin
	     select_index = -1
	     return
	endif
  endif

  if (fs.INTERP eq 'Y') then begin
	h=where((f[k] and OBJECT1_INTERP) ne 0,nkeep)
	if (nkeep eq 0) then begin
	     select_index = -1
	     return
	endif
	k=k[h]
  endif 
  if (fs.INTERP eq 'N') then begin
	h=where((f[k] and OBJECT1_INTERP) eq 0,nkeep)
	if (nkeep eq 0) then begin
	     select_index = -1
	     return
	endif
	k=k[h]
  endif

  if (fs.SATUR eq 'Y') then begin
	h=where((f[k] and OBJECT1_SATUR) ne 0,nkeep)
	if (nkeep eq 0) then begin
	     select_index = -1
	     return
	endif
	k=k[h]
  endif 
  if (fs.SATUR eq 'N') then begin
	h=where((f[k] and OBJECT1_SATUR) eq 0,nkeep)
	if (nkeep eq 0) then begin
	     select_index = -1
	     return
	endif
	k=k[h]
  endif

  if (fs.NOTCHECKED eq 'Y') then begin
	h=where((f[k] AND OBJECT1_NOTCHECKED) ne 0,nkeep)
	if (nkeep eq 0) then begin
	     select_index = -1
	     return
	endif
	k=k[h]
  endif 
  if (fs.NOTCHECKED eq 'N') then begin
	h=where((f[k] and OBJECT1_NOTCHECKED) eq 0,nkeep)
	if (nkeep eq 0) then begin
	     select_index = -1
	     return
	endif
	k=k[h]
  endif

  if (fs.SUBTRACTED eq 'Y') then begin
	h=where((f[k] and OBJECT1_SUBTRACTED) ne 0,nkeep)
	if (nkeep eq 0) then begin
	     select_index = -1
	     return
	endif
	k=k[h]
  endif 
  if (fs.SUBTRACTED eq 'N') then begin
	h=where((f[k] and OBJECT1_SUBTRACTED) eq 0,nkeep)
	if (nkeep eq 0) then begin
	     select_index = -1
	     return
	endif
	k=k[h]
  endif

  if (fs.NOSTOKES eq 'Y') then begin
	h=where((f[k] and OBJECT1_NOSTOKES) ne 0,nkeep)
	if (nkeep eq 0) then begin
	     select_index = -1
	     return
	endif
	k=k[h]
  endif 
  if (fs.NOSTOKES eq 'N') then begin
	h=where((f[k] and OBJECT1_NOSTOKES) eq 0,nkeep)
	if (nkeep eq 0) then begin
	     select_index = -1
	     return
	endif
	k=k[h]
  endif

  if (fs.BADSKY eq 'Y') then begin
	h=where((f[k] and OBJECT1_BADSKY) ne 0,nkeep)
	if (nkeep eq 0) then begin
	     select_index = -1
	     return
	endif
	k=k[h]
  endif 
  if (fs.BADSKY eq 'N') then begin
	h=where((f[k] and OBJECT1_BADSKY) eq 0,nkeep)
	if (nkeep eq 0) then begin
	     select_index = -1
	     return
	endif
	k=k[h]
  endif

  if (fs.PETROFAINT eq 'Y') then begin
	h=where((f[k] and OBJECT1_PETROFAINT) ne 0,nkeep)
	if (nkeep eq 0) then begin
	     select_index = -1
	     return
	endif
	k=k[h]
  endif 
  if (fs.PETROFAINT eq 'N') then begin
	h=where((f[k] and OBJECT1_PETROFAINT) eq 0,nkeep)
	if (nkeep eq 0) then begin
	     select_index = -1
	     return
	endif
	k=k[h]
  endif

  if (fs.TOO_LARGE eq 'Y') then begin
	h=where((f[k] and OBJECT1_TOO_LARGE) ne 0,nkeep)
	if (nkeep eq 0) then begin
	     select_index = -1
	     return
	endif
	k=k[h]
  endif 
  if (fs.TOO_LARGE eq 'N') then begin
	h=where((f[k] and OBJECT1_TOO_LARGE) eq 0,nkeep)
	if (nkeep eq 0) then begin
	     select_index = -1
	     return
	endif
	k=k[h]
  endif

  if (fs.DEBLENDED_AS_PSF eq 'Y') then begin
	h=where((f[k] and OBJECT1_DEBLENDED_AS_PSF) ne 0,nkeep)
	if (nkeep eq 0) then begin
	     select_index = -1
	     return
	endif
	k=k[h]
  endif 
  if (fs.DEBLENDED_AS_PSF eq 'N') then begin
	h=where((f[k] and OBJECT1_DEBLENDED_AS_PSF) eq 0,nkeep)
	if (nkeep eq 0) then begin
	     select_index = -1
	     return
	endif
	k=k[h]
  endif

  if (fs.DEBLEND_PRUNED eq 'Y') then begin
	h=where((f[k] and OBJECT1_DEBLEND_PRUNED) ne 0,nkeep)
	if (nkeep eq 0) then begin
	     select_index = -1
	     return
	endif
	k=k[h]
  endif 
  if (fs.DEBLEND_PRUNED eq 'N') then begin
	h=where((f[k] and OBJECT1_DEBLEND_PRUNED) eq 0,nkeep)
	if (nkeep eq 0) then begin
	     select_index = -1
	     return
	endif
	k=k[h]
  endif

  if (fs.ELLIPFAINT eq 'Y') then begin
	h=where((f[k] and OBJECT1_ELLIPFAINT) ne 0,nkeep)
	if (nkeep eq 0) then begin
	     select_index = -1
	     return
	endif
	k=k[h]
  endif 
  if (fs.ELLIPFAINT eq 'N') then begin
	h=where((f[k] and OBJECT1_ELLIPFAINT) eq 0,nkeep)
	if (nkeep eq 0) then begin
	     select_index = -1
	     return
	endif
	k=k[h]
  endif

  if (fs.BINNED1 eq 'Y') then begin
	h=where((f[k] and OBJECT1_BINNED1) ne 0,nkeep)
	if (nkeep eq 0) then begin
	     select_index = -1
	     return
	endif
	k=k[h]
  endif 
  if (fs.BINNED1 eq 'N') then begin
	h=where((f[k] and OBJECT1_BINNED1) eq 0,nkeep)
	if (nkeep eq 0) then begin
	     select_index = -1
	     return
	endif
	k=k[h]
  endif

  if (fs.BINNED2 eq 'Y') then begin
	h=where((f[k] and OBJECT1_BINNED2) ne 0,nkeep)
	if (nkeep eq 0) then begin
	     select_index = -1
	     return
	endif
	k=k[h]
  endif 
  if (fs.BINNED2 eq 'N') then begin
	h=where((f[k] and OBJECT1_BINNED2) eq 0,nkeep)
	if (nkeep eq 0) then begin
	     select_index = -1
	     return
	endif
	k=k[h]
  endif

  if (fs.BINNED4 eq 'Y') then begin
	h=where((f[k] and OBJECT1_BINNED4) ne 0,nkeep)
	if (nkeep eq 0) then begin
	     select_index = -1
	     return
	endif
	k=k[h]
  endif 
  if (fs.BINNED4 eq 'N') then begin
	h=where((f[k] and OBJECT1_BINNED4) eq 0,nkeep)
	if (nkeep eq 0) then begin
	     select_index = -1
	     return
	endif
	k=k[h]
  endif

  if (fs.MOVED eq 'Y') then begin
	h=where((f[k] and OBJECT1_MOVED) ne 0,nkeep)
	if (nkeep eq 0) then begin
	     select_index = -1
	     return
	endif
	k=k[h]
  endif 
  if (fs.MOVED eq 'N') then begin
	h=where((f[k] and OBJECT1_MOVED) eq 0,nkeep)
	if (nkeep eq 0) then begin
	     select_index = -1
	     return
	endif
	k=k[h]
  endif

  ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
  ;; begin flags2
  ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

  IF NOT doflags2 THEN BEGIN
      select_index=k
      return
  ENDIF

  OBJECT2_DEBLENDED_AS_MOVING= '1'X ; deblended as a moving object        
  OBJECT2_NODEBLEND_MOVING   = '2'X ; no deblend of moving object         
  OBJECT2_TOO_FEW_DETECTIONS = '4'X ; too few detections to deblend       
  OBJECT2_BAD_MOVING_FIT     = '8'X ; Fit to moving object was too poor   
  OBJECT2_STATIONARY        = '10'X ; velocity is consistent with zero    
  OBJECT2_PEAKS_TOO_CLOSE   = '20'X ; at least some peaks were too close, 
                                    ; and thus merged                     
  OBJECT2_MEDIAN_CENTRE     = '40'X ; centre is of median-smoothed image  
  OBJECT2_LOCAL_EDGE        = '80'X ; per-band centre's too near edge     
  OBJECT2_BAD_COUNTS_ERROR = '100'X ; psf|fiberCountsErr is bad/unknown   
  OBJECT2_BAD_MOVING_FIT_CHILD = '200'X ; moving child's fit was too poor 
  OBJECT2_DEBLEND_UNASSIGNED_FLUX = '400'X ; deblender failed to assign   
                                           ; enough of flux to children   
  OBJECT2_SATUR_CENTER     = '800'X ; object's centre's saturated         
  OBJECT2_INTERP_CENTER   = '1000'X ; object's centre is very close to at 
                                    ; least one interpolated pixel        
  OBJECT2_DEBLENDED_AT_EDGE='2000'X ; object is deblended though EDGE     
  OBJECT2_DEBLEND_NOPEAK   ='4000'X ; object had no detected peak         
  OBJECT2_PSF_FLUX_INTERP  ='8000'X ; a signifcant amount of PSF's flux   
                                    ; is interpolated                     
  OBJECT2_TOO_FEW_GOOD_DETECTIONS='10000'X ; too few good detections to   
                                           ; deblend as moving            
  OBJECT2_CENTER_OFF_AIMAGE   = '20000'X ; at least one peak's centre lay   
                                         ; off the atlas image in some band 

  OBJECT2_DEBLEND_DEGENERATE = '40000'X ; at least one potential child has
                                        ;been pruned as being too similar
                                        ;to some other template 
  OBJECT2_BRIGHTEST_GALAXY_CHILD = '80000'X ; this is the brightest child
                                          ;galaxy in a blend 
  OBJECT2_CANONICAL_BAND = '100000'X ; This band was primary (usually r')
  OBJECT2_AMOMENT_FAINT =  '200000'X ; too faint for adaptive moments 
  OBJECT2_AMOMENT_UNWEIGHTED = '200000'X ; failed so tried unweighted mom
  OBJECT2_AMOMENT_SHIFT =  '400000'X ; centre moved too far while
                                ;determining adaptive moments 
  OBJECT2_AMOMENT_MAXITER = '800000'X ; Too many iterations while
                                ;determining adaptive moments 
  OBJECT2_MAYBE_CR =       '1000000'X ; object may be a cosmic ray 
  OBJECT2_MAYBE_EGHOST =   '2000000'X ; object may be an electronics ghost
  OBJECT2_NOTCHECKED_CENTER = '4000000'X ; object's centre is NOTCHECKED 
  

  if (fs.DEBLENDED_AS_MOVING eq 'Y') then begin
      h=where((f2[k] and OBJECT2_DEBLENDED_AS_MOVING) ne 0,nkeep)
      if (nkeep eq 0) then begin
          select_index = -1
          return
      endif
      k=k[h]
  endif 
  if (fs.DEBLENDED_AS_MOVING eq 'N') then begin
      h=where((f2[k] and OBJECT2_DEBLENDED_AS_MOVING) EQ 0,nkeep)
      if (nkeep eq 0) then begin
          select_index = -1
          return
      endif
      k=k[h]
  endif 

  if (fs.NODEBLEND_MOVING eq 'Y') then begin
      h=where((f2[k] and OBJECT2_NODEBLEND_MOVING) NE 0,NKEEP)
      if (nkeep eq 0) then begin
          select_index = -1
          return
      endif
      k=k[h]
  endif
  if (fs.NODEBLEND_MOVING eq 'N') then begin
      h=where((f2[k] and OBJECT2_NODEBLEND_MOVING) eq 0,nkeep)
      if (nkeep eq 0) then begin
          select_index = -1
          return
      endif
      k=k[h]
  endif

  if (fs.TOO_FEW_DETECTIONS eq 'Y') then begin
      h=where((f2[k] and OBJECT2_TOO_FEW_DETECTIONS) ne 0,nkeep)
      if (nkeep eq 0) then begin
          select_index = -1
          return
      endif
      k=k[h]
  endif 
  if (fs.TOO_FEW_DETECTIONS eq 'N') then begin
      h=where((f2[k] and OBJECT2_TOO_FEW_DETECTIONS) EQ 0,NKEEP)
      if (nkeep eq 0) then begin
          select_index = -1
          return
      endif
      k=k[h]
  endif 

  if (fs.BAD_MOVING_FIT eq 'Y') then begin
      h=where((f2[k] and OBJECT2_BAD_MOVING_FIT) NE 0,NKEEP)
      if (nkeep eq 0) then begin
          select_index = -1
          return
      endif
      k=k[h]
  ENDIF
  if (fs.BAD_MOVING_FIT eq 'N') then begin
      h=where((f2[k] and OBJECT2_BAD_MOVING_FIT) eq 0,nkeep)
      if (nkeep eq 0) then begin
          select_index = -1
          return
      endif
      k=k[h]
  ENDIF

  if (fs.STATIONARY eq 'Y') then begin
      h=where((f2[k] and OBJECT2_STATIONARY) ne 0,nkeep)
      if (nkeep eq 0) then begin
          select_index = -1
          return
      endif
      k=k[h]
  endif 
  if (fs.STATIONARY eq 'N') then begin
      h=where((f2[k] and OBJECT2_STATIONARY) EQ 0,NKEEP)
      if (nkeep eq 0) then begin
          select_index = -1
          return
      endif
      k=k[h]
  endif 

  if (fs.PEAKS_TOO_CLOSE eq 'Y') then begin
      h=where((f2[k] and OBJECT2_PEAKS_TOO_CLOSE) NE 0,NKEEP)
      if (nkeep eq 0) then begin
          select_index = -1
          return
      endif
      k=k[h]
  endif
  if (fs.PEAKS_TOO_CLOSE eq 'N') then begin
      h=where((f2[k] and OBJECT2_PEAKS_TOO_CLOSE) eq 0,nkeep)
      if (nkeep eq 0) then begin
          select_index = -1
          return
      endif
      k=k[h]
  endif

  if (fs.MEDIAN_CENTRE eq 'Y') then begin
      h=where((f2[k] and OBJECT2_MEDIAN_CENTRE) ne 0,nkeep)
      if (nkeep eq 0) then begin
          select_index = -1
          return
      endif
      k=k[h]
  endif 
  if (fs.MEDIAN_CENTRE eq 'N') then begin
      h=where((f2[k] and OBJECT2_MEDIAN_CENTRE) EQ 0,NKEEP)
      if (nkeep eq 0) then begin
          select_index = -1
          return
      endif
      k=k[h]
  endif 

  if (fs.LOCAL_EDGE eq 'Y') then begin
      h=where((f2[k] and OBJECT2_LOCAL_EDGE) NE 0,NKEEP)
      if (nkeep eq 0) then begin
          select_index = -1
          return
      endif
      k=k[h]
  endif
  if (fs.LOCAL_EDGE eq 'N') then begin
      h=where((f2[k] and OBJECT2_LOCAL_EDGE) eq 0,nkeep)
      if (nkeep eq 0) then begin
          select_index = -1
          return
      endif
      k=k[h]
  endif

  if (fs.BAD_COUNTS_ERROR eq 'Y') then begin
      h=where((f2[k] and OBJECT2_BAD_COUNTS_ERROR) ne 0,nkeep)
      if (nkeep eq 0) then begin
          select_index = -1
          return
      endif
      k=k[h]
  endif 
  if (fs.BAD_COUNTS_ERROR eq 'N') then begin
      h=where((f2[k] and OBJECT2_BAD_COUNTS_ERROR) EQ 0,NKEEP)
      if (nkeep eq 0) then begin
          select_index = -1
          return
      endif
      k=k[h]
  endif 

  if (fs.BAD_MOVING_FIT_CHILD eq 'Y') then begin
      h=where((f2[k] and OBJECT2_BAD_MOVING_FIT_CHILD) NE 0,NKEEP)
      if (nkeep eq 0) then begin
          select_index = -1
          return
      endif
      k=k[h]
  ENDIF
  if (fs.BAD_MOVING_FIT_CHILD eq 'N') then begin
      h=where((f2[k] and OBJECT2_BAD_MOVING_FIT_CHILD) eq 0,nkeep)
      if (nkeep eq 0) then begin
          select_index = -1
          return
      endif
      k=k[h]
  ENDIF

  if (fs.DEBLEND_UNASSIGNED_FLUX eq 'Y') then begin
      h=where((f2[k] and OBJECT2_DEBLEND_UNASSIGNED_FLUX) ne 0,nkeep)
      if (nkeep eq 0) then begin
          select_index = -1
          return
      endif
      k=k[h]
  endif 
  if (fs.DEBLEND_UNASSIGNED_FLUX eq 'N') then begin
      h=where((f2[k] and OBJECT2_DEBLEND_UNASSIGNED_FLUX) EQ 0,NKEEP)
      if (nkeep eq 0) then begin
          select_index = -1
          return
      endif
      k=k[h]
  endif 

  if (fs.SATUR_CENTER eq 'Y') then begin
      h=where((f2[k] and OBJECT2_SATUR_CENTER) NE 0,NKEEP)
      if (nkeep eq 0) then begin
          select_index = -1
          return
      endif
      k=k[h]
  endif
  if (fs.SATUR_CENTER eq 'N') then begin
      h=where((f2[k] and OBJECT2_SATUR_CENTER) eq 0,nkeep)
      if (nkeep eq 0) then begin
          select_index = -1
          return
      endif
      k=k[h]
  endif

  if (fs.INTERP_CENTER eq 'Y') then begin
      h=where((f2[k] and OBJECT2_INTERP_CENTER) ne 0,nkeep)
      if (nkeep eq 0) then begin
          select_index = -1
          return
      endif
      k=k[h]
  endif 
  if (fs.INTERP_CENTER eq 'N') then begin
      h=where((f2[k] and OBJECT2_INTERP_CENTER) EQ 0,NKEEP)
      if (nkeep eq 0) then begin
          select_index = -1
          return
      endif
      k=k[h]
  endif 


  if (fs.DEBLENDED_AT_EDGE eq 'Y') then begin
      h=where((f2[k] and OBJECT2_DEBLENDED_AT_EDGE) NE 0,NKEEP)
      if (nkeep eq 0) then begin
          select_index = -1
          return
      endif
      k=k[h]
  endif
  if (fs.DEBLENDED_AT_EDGE eq 'N') then begin
      h=where((f2[k] and OBJECT2_DEBLENDED_AT_EDGE) eq 0,nkeep)
      if (nkeep eq 0) then begin
          select_index = -1
          return
      endif
      k=k[h]
  endif

  if (fs.DEBLEND_NOPEAK eq 'Y') then begin
      h=where((f2[k] and OBJECT2_DEBLEND_NOPEAK) ne 0,nkeep)
      if (nkeep eq 0) then begin
          select_index = -1
          return
      endif
      k=k[h]
  endif 
  if (fs.DEBLEND_NOPEAK eq 'N') then begin
      h=where((f2[k] and OBJECT2_DEBLEND_NOPEAK) EQ 0,NKEEP)
      if (nkeep eq 0) then begin
          select_index = -1
          return
      endif
      k=k[h]
  endif 


  if (fs.PSF_FLUX_INTERP eq 'Y') then begin
      h=where((f2[k] and OBJECT2_PSF_FLUX_INTERP) NE 0,NKEEP)
      if (nkeep eq 0) then begin
          select_index = -1
          return
      endif
      k=k[h]
  ENDIF
  if (fs.PSF_FLUX_INTERP eq 'N') then begin
      h=where((f2[k] and OBJECT2_PSF_FLUX_INTERP) eq 0,nkeep)
      if (nkeep eq 0) then begin
          select_index = -1
          return
      endif
      k=k[h]
  ENDIF

  if (fs.TOO_FEW_GOOD_DETECTIONS eq 'Y') then begin
      h=where((f2[k] and OBJECT2_TOO_FEW_GOOD_DETECTIONS) ne 0,nkeep)
      if (nkeep eq 0) then begin
          select_index = -1
          return
      endif
      k=k[h]
  endif 
  if (fs.TOO_FEW_GOOD_DETECTIONS eq 'N') then begin
      h=where((f2[k] and OBJECT2_TOO_FEW_GOOD_DETECTIONS) EQ 0,NKEEP)
      if (nkeep eq 0) then begin
          select_index = -1
          return
      endif
      k=k[h]
  endif 


  if (fs.CENTER_OFF_AIMAGE eq 'Y') then begin
      h=where((f2[k] and OBJECT2_CENTER_OFF_AIMAGE) ne 0,nkeep)
      if (nkeep eq 0) then begin
          select_index = -1
          return
      endif
      k=k[h]
  endif 
  if (fs.CENTER_OFF_AIMAGE eq 'N') then begin
      h=where((f2[k] and OBJECT2_CENTER_OFF_AIMAGE) EQ 0,NKEEP)
      if (nkeep eq 0) then begin
          select_index = -1
          return
      endif
      k=k[h]
  endif 

  if (fs.DEBLEND_DEGENERATE eq 'Y') then begin
      h=where((f2[k] and OBJECT2_DEBLEND_DEGENERATE) ne 0,nkeep)
      if (nkeep eq 0) then begin
          select_index = -1
          return
      endif
      k=k[h]
  endif 
  if (fs.DEBLEND_DEGENERATE eq 'N') then begin
      h=where((f2[k] and OBJECT2_DEBLEND_DEGENERATE) EQ 0,NKEEP)
      if (nkeep eq 0) then begin
          select_index = -1
          return
      endif
      k=k[h]
  endif 

  if (fs.BRIGHTEST_GALAXY_CHILD eq 'Y') then begin
      h=where((f2[k] and OBJECT2_BRIGHTEST_GALAXY_CHILD) ne 0,nkeep)
      if (nkeep eq 0) then begin
          select_index = -1
          return
      endif
      k=k[h]
  endif 
  if (fs.BRIGHTEST_GALAXY_CHILD eq 'N') then begin
      h=where((f2[k] and OBJECT2_BRIGHTEST_GALAXY_CHILD) EQ 0,NKEEP)
      if (nkeep eq 0) then begin
          select_index = -1
          return
      endif
      k=k[h]
  endif 

  if (fs.CANONICAL_BAND eq 'Y') then begin
      h=where((f2[k] and OBJECT2_CANONICAL_BAND) ne 0,nkeep)
      if (nkeep eq 0) then begin
          select_index = -1
          return
      endif
      k=k[h]
  endif 
  if (fs.CANONICAL_BAND eq 'N') then begin
      h=where((f2[k] and OBJECT2_CANONICAL_BAND) EQ 0,NKEEP)
      if (nkeep eq 0) then begin
          select_index = -1
          return
      endif
      k=k[h]
  endif 

  if (fs.AMOMENT_FAINT eq 'Y') then begin
      h=where((f2[k] and OBJECT2_AMOMENT_FAINT) ne 0,nkeep)
      if (nkeep eq 0) then begin
          select_index = -1
          return
      endif
      k=k[h]
  endif 
  if (fs.AMOMENT_FAINT eq 'N') then begin
      h=where((f2[k] and OBJECT2_AMOMENT_FAINT) EQ 0,NKEEP)
      if (nkeep eq 0) then begin
          select_index = -1
          return
      endif
      k=k[h]
  endif 

  if (fs.AMOMENT_UNWEIGHTED eq 'Y') then begin
      h=where((f2[k] and OBJECT2_AMOMENT_UNWEIGHTED) ne 0,nkeep)
      if (nkeep eq 0) then begin
          select_index = -1
          return
      endif
      k=k[h]
  endif 
  if (fs.AMOMENT_UNWEIGHTED eq 'N') then begin
      h=where((f2[k] and OBJECT2_AMOMENT_UNWEIGHTED) EQ 0,NKEEP)
      if (nkeep eq 0) then begin
          select_index = -1
          return
      endif
      k=k[h]
  endif 

  if (fs.AMOMENT_SHIFT eq 'Y') then begin
      h=where((f2[k] and OBJECT2_AMOMENT_SHIFT) ne 0,nkeep)
      if (nkeep eq 0) then begin
          select_index = -1
          return
      endif
      k=k[h]
  endif 
  if (fs.AMOMENT_SHIFT eq 'N') then begin
      h=where((f2[k] and OBJECT2_AMOMENT_SHIFT) EQ 0,NKEEP)
      if (nkeep eq 0) then begin
          select_index = -1
          return
      endif
      k=k[h]
  endif 

  if (fs.AMOMENT_MAXITER eq 'Y') then begin
      h=where((f2[k] and OBJECT2_AMOMENT_MAXITER) ne 0,nkeep)
      if (nkeep eq 0) then begin
          select_index = -1
          return
      endif
      k=k[h]
  endif 
  if (fs.AMOMENT_MAXITER eq 'N') then begin
      h=where((f2[k] and OBJECT2_AMOMENT_MAXITER) EQ 0,NKEEP)
      if (nkeep eq 0) then begin
          select_index = -1
          return
      endif
      k=k[h]
  endif 

  if (fs.MAYBE_CR eq 'Y') then begin
      h=where((f2[k] and OBJECT2_MAYBE_CR) ne 0,nkeep)
      if (nkeep eq 0) then begin
          select_index = -1
          return
      endif
      k=k[h]
  endif 
  if (fs.MAYBE_CR eq 'N') then begin
      h=where((f2[k] and OBJECT2_MAYBE_CR) EQ 0,NKEEP)
      if (nkeep eq 0) then begin
          select_index = -1
          return
      endif
      k=k[h]
  endif 

  if (fs.MAYBE_EGHOST eq 'Y') then begin
      h=where((f2[k] and OBJECT2_MAYBE_EGHOST) ne 0,nkeep)
      if (nkeep eq 0) then begin
          select_index = -1
          return
      endif
      k=k[h]
  endif 
  if (fs.MAYBE_EGHOST eq 'N') then begin
      h=where((f2[k] and OBJECT2_MAYBE_EGHOST) EQ 0,NKEEP)
      if (nkeep eq 0) then begin
          select_index = -1
          return
      endif
      k=k[h]
  endif 

  if (fs.NOTCHECKED_CENTER eq 'Y') then begin
      h=where((f2[k] and OBJECT2_NOTCHECKED_CENTER) ne 0,nkeep)
      if (nkeep eq 0) then begin
          select_index = -1
          return
      endif
      k=k[h]
  endif 
  if (fs.NOTCHECKED_CENTER eq 'N') then begin
      h=where((f2[k] and OBJECT2_NOTCHECKED_CENTER) EQ 0,NKEEP)
      if (nkeep eq 0) then begin
          select_index = -1
          return
      endif
      k=k[h]
  endif 

  select_index=k 


return

end

