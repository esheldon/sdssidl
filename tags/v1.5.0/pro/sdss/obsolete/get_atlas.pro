;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;+
;
; NAME:
;       GET_ATLAS
;       This routine is OBSOLETE.  It now just calls vew_atlas.
;
;-
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;



pro get_atlas, struct, index, clr=clr, dir=dir, $
               maxsize=maxsize, noprompt_in=noprompt_in, nodisplay=nodisplay,$
               imtot=imtot, imu=imu, img=img, imr=imr, imi=imi, imz=imz,$
               row0=row0, col0=col0, $
               drow=drow, dcol=dcol, $
               maguse=maguse,$
               silent=silent, hideradec=hideradec,$
               _extra=extra


  if N_params() LT 2 then begin
      print,'This routine is OBSOLETE. It now just calls view_atlas'
      print
      print,'Syntax -  get_atlas, struct, index, clr=clr, dir=dir, '
      print,'       maxsize=maxsize, noprompt=noprompt, nodisplay=nodisplay,'
      print,'       imtot=imtot, imu=imu, img=img, imr=imr, imi=imi, imz=imz,'
      print,'       row0=row0, col0=col0, silent=silent, hideradec=hideradec,'
      print,'       _extra=extra'
      print
      print
      print,'-If you want to zoom, enter r when prompted.'
      print,'Use doc_library,"get_atlas"  for more help'
      return
  endif

  message,'get_atlas is obsolete.  Use read_atlas and view_atlas',/inf
  view_atlas, struct, index=index, clr=clr, $
    imu=imu, img=img, imr=imr, imi=imi, imz=imz, $
    imtot=imtot, $
    row0=row0, col0=col0, $
    drow=drow, dcol=dcol, $
    ncol=ncol, nrow=nrow, $
    maguse=maguse, $
    silent=silent, hideradec=hideradec, _extra=_extra


end









