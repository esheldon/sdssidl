
;+
;
; NAME:
;       FIND_RADEC_FCHART
;
; PURPOSE:
;	Create a finding chart for the input coordinates.
;
; CALLING SEQUENCE:
;       find_radec_fchart, ra, dec, fchart, pstruct, $
;                      clr=clr, $
;                      radius=radius, radarcsec=radarcsec, radarcmin=radarcmin, $
;                      tol=tol, $
;                      nonoise=nonoise, $
;                      nocirc=nocirc, $
;                      circ_rad=circ_rad,$
;                      circ_color=circ_color,$
;                      nodisplay=nodisplay, psfile=psfile, $
;                      silent=silent, $
;                      photoid=photoid, $
;                      objx=objx, objy=objy, $
;                      linestyle=linestyle,$
;                      _extra=extra, runuse=runuse, astrans=astrans
;
; INPUTS: 
;       ra, dec:   position of interest in degrees. If is an array, then
;                  search is based on first element only.
;
; OPTIONAL INPUTS:
;       clr:       bandpass of images from which to create finding chart.
;                  Must be an integer [r,g,u,i,z] -> [0,1,2,3,4]
;                  Default is red (2)
;       radius:    Half length of the square finding chart in pixels.
;       radarcsec: radius in arcseconds (takes precedence over radius)
;       radarcmin: radius in arcminutes (takes precedence over radarcsec)
;
;       tol:       Tolerance for finding nearby object in arcseconds.  
;                  Default is 100.
;       cird_rad:  array of radii used to make circle around objects in 
;                    the list.  Default is size of finding chart/10.0 for the
;                    first and 1/20. for the rest
;         circ_color: colors to use for circles. Default is all white or
;                     black depending on device
;       psfile: make ps file with this name
;       linestyle: linestyles for circles. Default is all 0 (solid line)
;       runuse: find_radec may return more than one run. Set this value
;            to an integer to choose which run.
;       _extra:    Extra plotting keywords.
;
; KEYWORD PARAMETERS:
;       /nodisplay: don't display, just make fchart and return
;       /nonoise:   Set for no noise in fchart.
;       /nocirc: don't draw circles
;       /astrans: use direct astrans method
;
; OPTIONAL OUTPUTS: 
;       fchart:    The finding chart
;       pstruct:   A photo structure containing all objects in the frame 
;                  of ra,dec as well as the two frames before and after (if 
;                  its the first or last frame then it uses next two or 
;                  previous two.
;       altdir: atlas directory used to make fchart
;       photoid:     Returns Id of nearest object.
;       objx,objy: x,y positon of object used to make fchart
;
; CALLED ROUTINES:
;
;       FIND_RADEC 
;       TSOBJ_NAME
;            RUN_DIR
;       READ_PHOTO_COL
;       FCHART_CIRC_RADEC
;            CIRC_RADEC
;       (SAO)
;
; PROCEDURE: 
;	
;	Use find_radec to get the run, camcol, and field of an object that
;       is nearby to ra,dec and then read in the nearby objects.  Then create 
;       a finding chart and circle the ra,dec postion.
;
; REVISION HISTORY:
;	Author: Erin Scott Sheldon  UofMich 10/15/99  
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


PRO find_radec_fchart_mark_image, image, markStruct_in, $
                                       mapStruct=mapStruct, $
                                       xoffset=xoffset, yoffset=yoffset, $
                                       order=order, cfac=cfac

  markStruct = markStruct_in


  IF n_elements(cfac) EQ 0 THEN cfac=1

  ;; Some statistics
  imsize = size(image)
  ny = imsize[2]
  ss = imsize[1]

  ;; In pixels
  defrad = ss/10.0

  ;; radius
  ;; Checking this only works if the defaults have been defined!
  IF tag_exist(markStruct,'radius') THEN BEGIN 

      markStruct.radius = markStruct.radius*cfac

  ENDIF ELSE BEGIN 

      IF tag_exist(markStruct,'x') THEN nmark=n_elements(markStruct.x) $
      ELSE nmark = n_elements(markStruct.ra)

      IF nmark EQ 1 THEN BEGIN 
          markRadius = defrad
      ENDIF ELSE BEGIN 
          markRadius = [defrad, replicate(defrad/2.0,nMark-1)]
      ENDELSE 

      markStruct=create_struct(markStruct, 'radius', markRadius)
  ENDELSE 

  xshift = -0.5
  yshift = -0.5

  IF tag_exist(markStruct,'x') AND tag_exist(markStruct,'y') THEN BEGIN 
      ;; Add shift because plotting programs use center of pixel as
      ;; (0,0)

      markStruct.x = markStruct.x + xshift
      markStruct.y = markStruct.y + yshift

      markxy, markStruct, order=order
  ENDIF ELSE IF (tag_exist(markStruct,'ra') AND $
                 tag_exist(markStruct,'dec')) THEN BEGIN 

      IF n_elements(mapStruct) EQ 0 THEN BEGIN 
          print,'You must enter mapStruct to circle ra/dec'
          return
      ENDIF 

      mark_radec, mapStruct, markStruct, $
        xoffset=xoffset, yoffset=yoffset, $
        order=order, ny=ny

  ENDIF ELSE BEGIN 
      print,'You must enter x/y or ra/dec'
      return
  ENDELSE 


END 

PRO find_radec_fchart, ra, dec, $
  useind=useind, $
  clr=clr, $
  $                             ; Units
  pixels=pixels, $
  arcminutes=arcminutes, $
  arcseconds=arcseconds, $
  $
  radius=radius, $
  $
  runuse=runuse, $
  maxsize=maxsize, $
  $
  markPosition=markPosition, $
  markStruct=markStruct, $      ; mark style for ra/dec (object)
  extra_markStruct=extra_markStruct, $ ; extra markings
  $
  shift2r=shift2r, $
  $
  tol=tol, $
  astrans=astrans, $
  $
  fchart=fchart, $
  pstruct=pstruct, $
  photoid=photoid, $
  $
  $
  nonoise=nonoise, $
  $
  directions=directions, $
  circra=circra, $
  circdec=circdec,$
  circ_rad=circ_rad,$
  circ_color=circ_color,$
  linestyle=linestyle,$
  circobj=circobj, $
  nocirc=nocirc, $
  $
  nodisplay=nodisplay, $
  order=order, $
  psfile=psfile, $
  silent=silent, $
  objx=objx, objy=objy, $
  _extra=extra
  

  IF N_params() EQ 0 THEN BEGIN 
      print,'-Syntax: find_radec_fchart, ra, dec, fchart, pstruct, $'
      print,'          useind=useind, $'
      print,'          clr=clr, $'
      print,'          shift2r=shift2r, $'
      print,'          $'
      print,'          radius=radius, $'
      print,'          radarcsec=radarcsec, $'
      print,'          radarcmin=radarcmin, $'
      print,'          $'
      print,'          tol=tol, $'
      print,'          runuse=runuse, astrans=astrans, $'
      print,'          photoid=photoid, $'
      print,'          $'
      print,'          nonoise=nonoise, $'
      print,'          $'
      print,'          directions=directions, $'
      print,'          circra=circra, $'
      print,'          circdec=circdec,$'
      print,'          circ_rad=circ_rad,$'
      print,'          circ_color=circ_color,$'
      print,'          linestyle=linestyle,$'
      print,'          circobj=circobj, $'
      print,'          nocirc=nocirc, $'
      print,'          $'
      print,'          nodisplay=nodisplay, $'
      print,'          order=order, $'
      print,'          psfile=psfile, $'
      print,'          silent=silent, $'
      print,'          objx=objx, objy=objy, $'
      print,'          _extra=extra'
      print,''
      print,'Use doc_library,"find_radec_fchart"  for more help.'  
      return
  ENDIF 

  ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
  ;; Check input parameters
  ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

  IF n_elements(clr) EQ 0 THEN clr=2

  ;; What units? By default, arcminutes.
  ;; We always work in pixel coordinates. Multiply all lenghts by cfac

  IF keyword_set(pixels) THEN BEGIN 
      cfac = 1.0
  ENDIF ELSE IF keyword_set(arcseconds) THEN BEGIN 
      cfac = 1.0/0.4
  ENDIF ELSE BEGIN ;; Default is arcminutes
      cfac = 60.0/0.4
  ENDELSE 

  rad_default = 300             ; pixels
  IF n_elements(radius) EQ 0 THEN BEGIN 
      radius = rad_default/cfac
  ENDIF 

  IF n_elements(markStruct) EQ 0 AND keyword_set(markPosition) THEN BEGIN 
      markStruct = {hole_fraction:1./3.}
  ENDIF 

  IF n_elements(tol) EQ 0 THEN tolerance = 100/3600. ELSE tolerance=tol*3600.

  ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
  ;; Look for the ra/dec
  ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

  delvarx, fchart
  find_radec, ra[0], dec[0], run, camcol, field, silent=silent, astrans=astrans
  IF run[0] EQ -1 THEN return


  ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
  ;; Now see if any of the runs containing ra/dec have both
  ;; tsObj and fpAtlas
  ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

  IF n_elements(runuse) NE 0 THEN BEGIN 
      run = run[runuse[0]]
      camcol = camcol[runuse[0]]
  ENDIF 

  ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
  ;; get a run that we can use: loop over the run/camcols and
  ;; make sure we have the atlas images
  ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

  run_status = sdss_runstatus(exists=rs_exists)
  if not rs_exists then begin
      message,'RUN_STATUS information not found'
  endif

  select_struct = {tsobj_exist:'y',fpatlas_exist:'y'}
  rsi = sdss_flag_select(run_status.flags, 'runstatus', select_struct, nrsi) 
  if nrsi eq 0 then begin
      message,'  *No runs with tsobj/fpatlas found',/inf
      return
  endif

  ; Get first good one
  n_found = n_elements(run)
  FOR i=0L, n_found-1 DO BEGIN 
      IF nrsi NE 0 THEN BEGIN 
          wr = where(run_status[rsi].run EQ run[i], nwr)
          IF nwr NE 0 THEN BEGIN  
              run = run[i]
              camcol = camcol[i]
              BREAK
          ENDIF 
      ENDIF 
  ENDFOR 

  IF n_elements(wr) EQ 0 OR wr[0] EQ -1 THEN BEGIN
      message,'  * No matching runs found with tsObj/fpAtlas',/inf
      return
  ENDIF
  wr = rsi[wr]
  IF n_elements(runuse) EQ 0 THEN runuse = i

  rerun = max(run_status[wr].rerun, maxrerun_index)
  wr = wr[maxrerun_index]

  ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
  ;; Read in the fields surrounding matching field
  ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

  fields = field[runuse[0]] + [-1,0,1]
  pstruct = sdss_read('tsobj',run,camcol,fields=fields)
  ;colid = [ run, rerun, camcol ]
  ;read_tsobj, colid, pstruct, start=field[runuse]-1, nframes=3

  ;; anything found?
  IF n_elements(pstruct) EQ 0 THEN return

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
; Make the finding chart
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

  ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
  ;; Find nearest object which we will use to make finding chart
  ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

  IF NOT silent THEN print
  IF (n_elements(photoid) EQ 0) THEN BEGIN 
      IF NOT silent THEN print,'Getting photoid of closest object'
      IF NOT keyword_set(tol) THEN tol = 500/3600. ;100 arcseconds
      allow = 1
      close_match_radec, ra[0], dec[0], pstruct.ra, pstruct.dec, $
        match1, photoid, tol, allow, /silent
  ENDIF

  IF photoid[0] EQ -1 THEN BEGIN
      print,'No matches found'
      return
  ENDIF 

  ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
  ;; make fchart for photoid and display it
  ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
  
  IF n_elements(psfile) NE 0 THEN BEGIN
      pcharold = !p.charsize
      begplot,name=psfile
      !p.charsize=1.0
  ENDIF 

  IF NOT silent THEN print,'Making the fchart'
  fchart, pstruct, photoid, radius*cfac, clr, fchart, $
    shift2r=shift2r, $
    objx=objx, objy=objy, silent=silent, nonoise=nonoise, $
    useind=useind

  ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
  ;; No need to continue of no display
  ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

  IF keyword_set(nodisplay) THEN return 

  ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
  ;; Display the finding chart
  ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

  display_fchart, fchart, pstruct[photoid],objx,objy,clr,/nocirc,$
    silent=silent, nodisplay=nodisplay, order=order, directions=directions,$
    _extra=extra

  ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
  ;; Should we mark positions?
  ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

  ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
  ;; Get the mapping from ra/dec to row,col in r-band
  ;; for marking positions
  ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

  IF (n_elements(markStruct) NE 0 OR $
      n_elements(extra_markstruct) NE 0) THEN BEGIN 

      xshift = 0.5
      yshift = 0.5

      w=where(pstruct.field EQ pstruct[photoid].field)
      map_order = 1
      create_photomap, $
        pstruct[w].ra, pstruct[w].dec, pstruct[w].rowc[2], pstruct[w].colc[2],$
        mapStruct, map_order=map_order, $
        ra_center = ramatch, dec_center=decmatch
      
      xoffset = objx - pstruct[photoid].colc[2] - xshift
      yoffset = objy - pstruct[photoid].rowc[2] - xshift
  ENDIF 

  IF n_elements(markStruct) NE 0 THEN BEGIN 

      IF NOT tag_exist(markStruct,'ra') THEN $
        markStruct = create_struct(markStruct,'ra',0d)
      IF NOT tag_exist(markStruct,'dec') THEN $
        markStruct = create_struct(markStruct,'dec',0d)

      markStruct.ra = ra
      markStruct.dec = dec

      find_radec_fchart_mark_image, $
        fchart, markStruct, $
        mapStruct=mapStruct, $
        xoffset=xoffset, yoffset=yoffset, $
        order=order, cfac=cfac

  ENDIF 

  IF n_elements(extra_markStruct) NE 0 THEN BEGIN 
      find_radec_fchart_mark_image, $
        fchart, extra_markStruct, $
        mapStruct=mapStruct, $
        xoffset=xoffset, yoffset=yoffset, $
        order=order, cfac=cfac
  ENDIF 

  IF n_elements(psfile) NE 0 THEN BEGIN
      endplot
      !p.charsize = pcharold
  ENDIF 
  IF NOT silent THEN print

  return 
END 
