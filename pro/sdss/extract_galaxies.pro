pro extract_galaxies, pstruct, color_index, ostruct, max_mag=max_mag, min_mag=min_mag, indices=indices, silent=silent

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;
; NAME: 
;    EXTRACT_GALAXIES
;
; PURPOSE:
;     Extracts a clean set of galaxies
; 
; Inputs:  pstruct: input photo structure 
;	   color_index: bandpass to select on:
;          ostruct: output photo structure containing galaxies
;          max_mag: maximum magnitude to use (default=24.0)
;          min_mag: minimum mag cut (default=12.0)
;	   silent: don't print out status or make plots
;
; Outputs: Plots flags for these objects....
;	   indices: returns the indices of the galaxies in the original
;			struct
;
; Author:  Phil Fischer
;    Date: 1/14/99
; Altered to get galaxies: Erin Scott Sheldon
;    Date: 2/19/99
; Added silent and indices options: Tim McKay
;    Date: 6/10/99
; Added min_mag keyword. Used type concordance in different
;    bandpasses istead of objc_type: Erin S. Sheldon
;    Date: 3-Jul-2002
;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;Help message
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

  if n_params() LT 2 then begin
      print,'-syntax extract_galaxies, pstruct, color_index, ostruct,max_mag=max_mag, min_mag=min_mag, indices=indices, silent=silent'
      return
  endif

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;; if requested, use photo type classifier to make the first cut
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
  
  IF NOT keyword_set(silent) THEN silent = 0

  IF NOT silent THEN print,'Number of entries: ',n_elements(pstruct)

  galtype = 3
  
  _ss = where( ( (pstruct.type[1] EQ galtype) AND (pstruct.type[2] EQ galtype) OR $
                 (pstruct.type[2] EQ galtype) AND (pstruct.type[3] EQ galtype) )$
               ,nss)

  IF nss EQ 0 THEN BEGIN
      print,'No objects passed PHOTO type cut'
      delvarx,ostruct
      indices=-1
      return
  ENDIF 
  index=_ss
  ostruct=pstruct(_ss)
  IF NOT silent THEN print,'After type cut: ',n_elements(ostruct)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;; extract based on object1 flags in selected bandpass
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

  make_flag_struct, fs
  fs.satur = 'N'
  fs.canonical_center='N' 
  fs.edge='N' 
  fs.blended='N' 
  ;;;;;;;;;;;;;;;;;;;;;; fs.child='N' ;;; don't care if galaxies are children
  ;;;;;;;;;;;;;;;;;;;;;;fs.peakcenter='N' 
  fs.nodeblend='N' 
  fs.nopetro='N'   
  ;;;;;;;;;;;;;;;;;;;;;;fs.manypetro='N'   
  fs.manyr50='N'   
  fs.manyr90='N'   
  fs.incomplete_profile='N'   
  fs.interp='N'   
  fs.notchecked='N'   
  fs.subtracted='N'   
  fs.nostokes='N'   
  fs.badsky='N'   
  ;;;;;;;;;;;;;;;;;;;;;fs.petrofaint='N'   
  fs.too_large='N'   
  fs.deblended_as_psf='N'
  fs.deblend_pruned='N'   
  fs.ellipfaint='N'   
  fs.moved='N'   
  flag_select,ostruct,fs,color_index,_ss

  IF _ss[0] EQ -1 THEN BEGIN
      print,'No objects passed FLAGS cuts'
      delvarx,ostruct
      indices=-1
      return
  ENDIF 
  index=index(_ss)
  ostruct=ostruct(_ss)
  IF NOT silent THEN print,'After FLAGS cuts: ',n_elements(ostruct)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;; extract multiple entries using the objc flags
;;;; check for things in all colors
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

  make_flag_struct, fs
  fs.satur='N'
  fs.bright='N'
  fs.blended='N'  ;;;; gets rid of parent
  flag_select,ostruct,fs,color_index,_ss,objc=1
  IF _ss[0] EQ -1 THEN BEGIN
      print,'No objects passed OBJC_FLAGS cuts'
      delvarx,ostruct
      indices=-1
      return
  ENDIF 

  index=index(_ss)
  ostruct=ostruct(_ss)
  IF NOT silent THEN print,'After OBJC_FLAGS cuts: ', n_elements(ostruct)  

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;; cut on maximum magnitude
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
  
  minm=12.0
  maxm=24.0
  if n_elements(max_mag) NE 0 THEN maxm=max_mag
  IF n_elements(min_mag) NE 0 THEN minm=min_mag

  _ss=where( (ostruct.fibercounts[color_index] lt maxm) AND $
             (ostruct.fibercounts[color_index] GT minm), nss)
  IF nss EQ 0 THEN BEGIN
      print,'No objects passed magnitude cuts'
      delvarx,ostruct
      indices=-1
      return
  ENDIF 
  index=index(_ss)
  ostruct=ostruct(_ss)
  IF NOT silent THEN print,'After magnitude cuts: ', n_elements(ostruct)  

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

  indices=index

  IF NOT silent THEN BEGIN 
      title='Extracted Galaxies'
      xtitle='fibercounts'
      ytitle='petrorad'
      
      plot,ostruct.fibercounts(color_index),ostruct.petrorad(color_index), $
           yrange=[0,10],psym=3,title=title,xtitle=xtitle,ytitle=ytitle
  ENDIF 

END 










