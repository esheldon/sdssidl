PRO fchart_circ_radec, photostr, ra, dec, fchart, useind=useind, $
                       shift2r=shift2r,$
                       clr=clr, $
                       radius=radius, $
                       radarcsec=radarcsec, radarcmin=radarcmin,$
                       tol=tol, $
                       linestyle=linestyle,$
                       nonoise=nonoise, $
                       circ_rad = circ_rad, $
                       circ_color=circ_color,$
                       circ_obj=circ_obj,                $
                       nodisplay=nodisplay, psfile=psfile, $
                       silent=silent, $
                       objx=objx, objy=objy, photoid=photoid, $
                       nocirc = nocirc, $
                       pos_ang=pos_ang, rmax=rmax, rmin=rmin,$
                       _extra=extra


;+
;
; NAME: 
;    FCHART_CIRC_RADEC
;       
; PURPOSE: 
;    Build a finding chart and (unless requested otherwise ) circle positions 
;    in ra and dec arrays.
;	
;
; CALLING SEQUENCE: 
;    fchart_circ_radec, photostr, ra, dec, fchart, $
;                      clr=clr, $
;                      radius=radius, radarcsec=radarcsec, $
;                      radarcmin=radarcmin,$
;                      tol=tol, $
;                      linestyle=linestyle,$
;                      nonoise=nonoise, $
;                      circ_rad = circ_rad, $
;                      circ_color=circ_color,$
;                      circ_obj=circ_obj,                $
;                      nodisplay=nodisplay, psfile=psfile, $
;                      silent=silent, $
;                      objx=objx, objy=objy, photoid=photoid, $
;                      nocirc = nocirc, $
;                      pos_ang=pos_ang, rmax=rmax, rmin=rmin,$
;                      _extra=extra
;
; INPUTS: photostr:  A photo structure.  Must contain RUN and CAMCOL tags to
;             find atlas directory (calls fetch_dir) Also must contain RA and
;             DEC so that CIRC_RADEC can make a good mapping from row,col to
;             ra and dec
;         ra, dec:   The arrays of ra and dec positions to be circled.  Finding
;             chart is made for object in photostr that most closely matches
;             the _first_ ra and dec in list.  Note, if you set /nocirc, none
;             of the objects in the list will be circled.
;
; OPTIONAL INPUTS:
;         clr:       Color to make finding chart.  [0,1,2,3,4]->[u,g,r,i,z]
;                 default is clr=2
;         radius:    Optional input.  Radius to make finding chart in pixels.
;         radarcsec: radius in arcseconds (takes precedence over radius)
;         radarcmin: radius in arcminutes (takes precedence over radarcsec)
;         tol:       Optional input tolerance (in arcsec) used to find nearest 
;                    object to ra dec. This should be large to gaurantee it 
;                    will find and object.  Default is 500 arcsec. Will search 
;                    on first ra dec in list. 
;         cird_rad:  array of radii used to make circle around objects in 
;                    the list.  Default is size of finding chart/10.0 for the
;                    first and 1/20. for the rest
;         circ_color: colors to use for circles. Default is all white or
;                     black depending on device
;         circ_obj: If set, circle the object used to make finding chart.
;         psfile:    Name of psfile in which to plot.
;         photoid:   Can input photoid to make fchart from, else it will find
;                    nearest match in ra,dec. If not input, can
;                    be returned with this keyword. See below.
;        pos_ang, rmax, rmin: instead of just circle, overplot an ellipse
;         _extra:    Extra plotting options.
;
; KEYWORD PARAMETERS:
;         /nonoise: keyword sent to fchart.  Set for no noise added to fchart.
;         /nodisplay:  If set, won't display the image.
;         /silent:   Shut off messages except errors.
;         /nocirc: If set, no circling is done.
;
; OPTIONAL OUTPUTS:
;         fchart:  Return the image.
;         objx, objy:  The position of object used to make finding chart.
;         photoid:   If not input, returns id used to make finding chart
;
; CALLED ROUTINES:
;                  (CLOSE_MATCH_RADEC)
;                  FCHART
;                  (DISPLAY_FCHART)
;                  (CIRC_RADEC)
;                  FETCH_DIR
;                  (FETCH_RERUN)
; 
; PROCEDURE: 
;   if not input in photoid, match first ra and dec in structure to 
;   the photostr.  Then make a finding chart and circle all the ra and dec.
;	
;
; REVISION HISTORY:
;	Author: Erin Scott Sheldon Umich 5/25/99
;       Added radarcsec and radarcmin keyword parameters. 02-Jul-2002
;       
;                                      
;-                                       
;
;
;
;  Copyright (C) 2005  Erin Sheldon, NYU.  erin dot sheldon at gmail dot com
;
;    This program is free software; you can redistribute it and/or modify
;    it under the terms of the GNU General Public License as published by
;    the Free Software Foundation; either version 2 of the License, or
;    (at your option) any later version.
;
;    This program is distributed in the hope that it will be useful,
;    but WITHOUT ANY WARRANTY; without even the implied warranty of
;    MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
;    GNU General Public License for more details.
;
;    You should have received a copy of the GNU General Public License
;    along with this program; if not, write to the Free Software
;    Foundation, Inc., 51 Franklin St, Fifth Floor, Boston, MA  02110-1301  USA
;
;                                       



  if N_params() LT 3 then begin
     print,'-Syntax: fchart_circ_radec, photostr, ra, dec [, '
     print,'           fchart, '
     print,'           clr=clr, radius=radius, tol=tol, '
     print,'           nonoise=nonoise, '
     print,'           nocirc=nocirc, '
     print,'           circ_rad = circ_rad, '
     print,'           circ_obj=circ_obj,'
     print,'           nodisplay=nodisplay, psfile=psfile, '
     print,'           silent=silent, '
     print,'           objx=objx, objy=objy, photoid=photoid, '
     print,'           _extra=extra ]'
     print,''
     print,'Use doc_library,"fchart_circ_radec"  for more help.'  
     return
  endif

  IF n_elements(clr) EQ 0 THEN clr=2

  tags = tag_names(photostr)
  wrun = where(tags EQ 'RUN',nrun)
  wcol = where(tags EQ 'CAMCOL',ncol)
  wrer = where(tags EQ 'RERUN',nrer)
  IF (nrun EQ 0) OR (ncol EQ 0) THEN BEGIN 
      print,'Structure must contain "RUN" and "CAMCOL"'
      return
  ENDIF 
  run = photostr[0].run
  IF nrer EQ 0 THEN BEGIN
      fetch_rerun, run, rerun
  ENDIF ELSE BEGIN
      rerun = photostr[0].rerun
  ENDELSE 
  camcol = photostr[0].camcol
  fetch_dir, run, camcol, rerun, dir, atldir

  IF NOT keyword_set(silent) THEN silent = 0

  IF n_elements(radius) EQ 0 THEN radius = 300.
  IF n_elements(radarcsec) NE 0 THEN radius = radarcsec/0.4
  IF n_elements(radarcmin) NE 0 THEN radius = radarcmin*60.0/0.4

  nn = n_elements(ra)
  nn2 = n_elements(dec)
  IF (nn NE nn2) THEN BEGIN
    print,'ra and dec should be same size'
    return
  ENDIF 

;;; use input id for nearest object if it is given, else find the nearest
;;; object to first ra-dec pair within tolerance

  IF NOT silent THEN print
  IF (n_elements(photoid) EQ 0) THEN BEGIN 
    IF NOT silent THEN print,'Getting photoid of closest object'
    IF NOT keyword_set(tol) THEN tol = 500/3600. ;100 arcseconds
    allow = 1
    close_match_radec, ra[0], dec[0], photostr.ra, photostr.dec, $
             match1, photoid, tol, allow, /silent
  ENDIF

  IF photoid[0] EQ -1 THEN BEGIN
    print,'No matches found'
    return
  ENDIF 

;;; make fchart for nearest object (photoid) and display it

  IF n_elements(psfile) NE 0 THEN BEGIN
      pcharold = !p.charsize
      !p.charsize = 1.
      begplot,name=psfile
  ENDIF 
  IF NOT silent THEN print,'Making the fchart'
  fchart, photostr, photoid, radius, clr, fchart, $
    shift2r=shift2r, $
    dir=atldir, objx=objx, objy=objy, silent=silent, nonoise=nonoise, $
    useind=useind

  IF keyword_set(nodisplay) THEN return 

  IF !d.name EQ 'PS' THEN BEGIN 
      pcharold = !p.charsize
      !p.charsize=1.0
  ENDIF 
  display_fchart, fchart, photostr[photoid],objx,objy,clr,/nocirc,$
                  silent=silent, nodisplay=nodisplay, _extra=extra
  IF !d.name EQ 'PS' THEN !p.charsize=pcharold

  IF keyword_set(nocirc) THEN return

  s=size(fchart)
  ss = min([s[1],s[2]])

  ;; circle object centered in fchart?
  IF (n_elements(circ_obj) NE 0)  THEN BEGIN
    ra = [photostr[photoid].ra, ra]
    dec = [photostr[photoid].dec, dec]
    nn = nn+1
    IF (n_elements(circ_rad) NE 0) THEN circ_radius = [ss/10.0,circ_rad]
  ENDIF ELSE BEGIN 
      IF n_elements(circ_rad) NE 0 THEN BEGIN 
          ncircrad = n_elements(circ_rad)
          IF ncircrad NE nn THEN message,'circ_rad must be same size as ra/dec'$
          ELSE circ_radius = circ_rad
      ENDIF ELSE circ_radius = [ss/10.0, replicate(ss/20.0,nn)]
  ENDELSE 
  IF NOT silent THEN BEGIN 
    print,'' 
    print,strtrim(string(nn),2)+' positions to be circled'
  ENDIF 

;;; circle the ra's and dec's with different colors and sizes

  IF n_elements(pos_angle) NE 0 THEN BEGIN 

      IF n_elements(rmax) NE nn THEN rmax_send=replicate(rmax,nn) $
      ELSE rmax_send=rmax
      IF n_elements(rmin) NE nn THEN rmin_send=replicate(rmin,nn) $
      ELSE rmin_send=rmin
      IF n_elements(pos_ang) NE nn THEN pos_ang_send=replicate(pos_ang,nn) $
      ELSE pos_ang_send=pos_ang

  ENDIF 

  nc=n_elements(circ_color)
  IF nc NE 0 THEN BEGIN 
      IF nc NE nn THEN  message,'circ_color array must be same size as ra/dec arrays' $
      ELSE clruse = circ_color
  ENDIF ELSE  BEGIN
      simpctable
      IF !d.name EQ 'X' THEN clruse=replicate(!white ,nn) $
      ELSE clruse=replicate(!p.color,nn)
  ENDELSE 

  nl=n_elements(linestyle)
  IF nl NE 0 THEN BEGIN 
      IF nl NE nn THEN message,'linestyle must be same size as ra/dec arrays' $
      ELSE linest=linestyle
  ENDIF ELSE linest=replicate(0, nn)

  FOR i=0, nn-1 DO BEGIN

      IF n_elements(pos_ang_send) NE 0 THEN possend=pos_ang_send[i]
      IF n_elements(rmax_send) NE 0 THEN rmxsnd=rmax_send[i]
      IF n_elements(rmin_send) NE 0 THEN rmnsnd=rmin_send[i]

      box = 0
      IF (n_elements(circ_obj) NE 0 AND i EQ 0) THEN box=1

      circ_radec, $
        photostr, photoid, objx, objy, ra[i],dec[i], circ_radius[i],$
        color=clruse[i], $
        box=box,clr=clr,silent=silent, linestyle=linest[i], $
        pos_ang=possend, rmax=rmxsnd, rmin=rmnsnd
      
  ENDFOR
  IF n_elements(psfile) NE 0 THEN BEGIN
      endplot,/noprint
      !p.charsize = pcharold
  ENDIF 
  IF NOT silent THEN print

return
end















