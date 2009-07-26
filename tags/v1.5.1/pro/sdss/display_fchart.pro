
;+
;
; NAME: 
;    DISPLAY_FCHART
;       
; PURPOSE: display a photo finding chart and circle the object that was used to
;          make the chart.  Label axis with arcminutes from the center.  Output
;          inmages, .jpg, .png, or .ps if requested.  Gif no longer supported
;          by idl.
;	
;
; CALLING SEQUENCE:  
;      
;                 
;
; INPUTS: fchart:  The finding chart
;         object:  The photo object struct used to make the finding chart.
;         objx:    x-position of object in the finding chart.
;         objy:    y-position
;         clr:     color index the finding chart was made from 
;                                          used in the title.
;
; INPUT KEYWORD PARAMETERS:
;         order=: Order to draw images. default is order=0 is 0,0 at bottom
;                 left. for order=1, 0,0 is top left.
;         /box:  Draw a box instead of a circle.
;         /jpeg: Write a jpeg from display.
;         /png: Write a png from display.
;         /ps:   If set a postscript files are created for the requested items.
;         fnamepng: name of png file. Default fchart.png
;         fnameps: name of ps file.  Default is fchart.ps
;         fnamegif: name if gif file.  Default is fchart.gif
;         silent:  If set, program will be silent except for error messages.
;         nodisplay: If set, there will be No x-window display of the chart.  
;               -May still make ps files.-
;         hideradec: Don't show the ra and dec on the plots.
;         circ_rad: radius of circle.
;         nocirc:  if set, no circle is drawn.
;         _extra: any extra plotting command keywords.
;       
; OUTPUTS: 
;
; OPTIONAL OUTPUTS: May output .ps or .gif files if requested.
;
; CALLED ROUTINES:
;
;                  RADECSTR
;                     RADEC
;                  SIGMA_CLIP
;                  BEGPLOT
;                  ENDPLOT
;                  RDIS_SETUP
;                  RDIS:
;                      TVIM2_SCL:
;                               TVIM2
;                  TVCIRCLE
;                  TVBOX
;
;
; REVISION HISTORY: 
;      Author: Erin Scott Sheldon  Umich 5/25/99
;      Changed order in title: run-rerun-camcol-field-id 
;      Fixed circle color problem.
;                                               02-Jul-2002
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



PRO display_fchart_getcolor, circ_color
  IF !d.name EQ 'X' THEN BEGIN 
      defsysv, '!white', exist=wexist
      IF NOT wexist THEN BEGIN 
          max = float(!d.n_colors)-1.
          circ_color = 1.*max
      ENDIF ELSE circ_color=!white
  ENDIF ELSE BEGIN 
      circ_color=!p.color
  ENDELSE 
END 

PRO display_fchart, fchart, object, objx, objy, clr, $
                    order=order, $
                    box=box, png=png, jpeg=jpeg, gif=gif, ps=ps, $
                    fnameps=fnameps, $
                    fnamegif=fnamegif, silent=silent, nodisplay=nodisplay, $
                    hideradec=hideradec, circ_rad=circ_rad, nocirc=nocirc, $
                    directions=directions,$
                    maguse=maguse,$
                    _extra=extra



  IF N_params() LT 5 THEN BEGIN 
     print,'-Syntax: '
     print,''
     print,'Use doc_library,"display_fchart"  for more help.'  
     return
  ENDIF 

  on_error, 2

  IF n_elements(fchart) EQ 0 THEN BEGIN 
      print,'No fchart!'
      return
  ENDIF 

  ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
  ; Some initializations
  ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

  IF NOT keyword_set(gif)       THEN gif = 0
  IF NOT keyword_set(ps)        THEN ps=0
  IF NOT keyword_set(fnameps)   THEN fnameps = 'fchart.ps'
  IF NOT keyword_set(fnamegif)  THEN fnamegif = 'fchart.gif'
  IF NOT keyword_set(silent)    THEN silent = 0
  IF NOT keyword_set(nodisplay) THEN nodisplay=0

  IF NOT keyword_set(nocirc) THEN nocirc = 0

  ;; This is needed to fit all the info on.

  colors=['u','g','r','i','z'] 
 
  tags = tag_names(object)
  run = run2string(object.run)
  
  field = field2string(object.field)
  camcol = strtrim(string(object.camcol),2)
  id = strn(object.id,length=5,padchar='0')
;  id = strtrim(string(object.id), 2)

  wrer = where(tags EQ 'RERUN', nwrer)
  IF nwrer NE 0 THEN BEGIN
      rerun='-'+strtrim(string(object[0].(wrer[0])), 2)
  ENDIF ELSE BEGIN
      rerun = ''
  ENDELSE 

  object_name = run+rerun+'-'+camcol+'-'+field+'-'+id

  ;; Set up titles
  title = object_name + '   Filter: '+colors[clr]
  IF (NOT keyword_set(hideradec) ) THEN BEGIN 
      radecstr, object.ra, object.dec, rastr, decstr
      title = title + '  ' +rastr+' : ' + decstr
  ENDIF

  ytitle='Offset (arcminutes)'

  wmag = sdss_maguse(object, maguse=maguse, silent=silent)

  IF wmag NE -1 THEN BEGIN 

      c=object.(wmag)
      xtitle=''
      FOR kk=0, 4 DO BEGIN
          IF (xtitle NE '') THEN xtitle = xtitle+'  '
          mag = strmid( strtrim(string(c[kk]),2), 0, 5)
          xtitle = xtitle + colors[kk]+'='+mag
      ENDFOR

      addtitle = ''
      FOR kk=0,3 DO BEGIN
          diff = strmid(strtrim(string(c[kk] - c[kk+1]),2), 0, 5)
          addtitle = addtitle + '  '+colors[kk]+'-'+colors[kk+1]+'='+diff
      ENDFOR
      xtitle = xtitle + addtitle  
  ENDIF ELSE BEGIN 
      xtitle = ''
  ENDELSE 

  ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
  ; Now, draw the circle around our object and then display and save.
  ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
    
  ;;;; set appropriate scaling of the circle ;;;;

  ss = size(fchart)
  sx = ss[1]*.4/60.0
  sy = ss[2]*.4/60.0
  IF (NOT keyword_set(circ_rad)) THEN circ_rad = min([ss[1],ss[2]])/20.0

  ;;;; make x and y tick labels
  xlabels = [ntostr(-2*sx/4.0, 5), $
             ntostr(-sx/4.0, 5)  , $
             '0.0', $
             ntostr(sx/4.0,4), $
             ntostr(2*sx/4.0,4)]
  ylabels = xlabels

  pold=!p.multi
  !p.multi=[0,0,1]

  ;; Postscript output
  IF (ps) THEN BEGIN 

      begplot, name=fnameps

      display_fchart_getcolor, circ_color

      tvasinh, fchart, /noframe, title=title, _extra=extra, order=order

      IF (NOT nocirc) THEN BEGIN 
          IF (NOT keyword_set(box) ) THEN $
            tvcircle, circ_rad, objx, objy, circ_color, /data $
          ELSE tvbox, 2*circ_rad, objx, objy, circ_color, /data

      ENDIF
      axis,xaxis=0,xticks=4,xtickn=xlabels, xtitle=xtitle
      axis,xaxis=1,xticks=4,xtickn=[' ',' ',' ',' ',' ']
      axis,yaxis=0,yticks=4,ytickn=ylabels, ytitle=ytitle
      axis,yaxis=1,yticks=4,ytickn=[' ',' ',' ',' ',' ']

      IF keyword_set(directions) THEN BEGIN 
          angle = $
            angle_rowcol2radec(object.run, $
                               object.camcol, $
                               object.field, clr)
          plot_ne_arrows, angle, fracsize=0.05, offset_frac=offset_frac,$
            order=order
      ENDIF 

      endplot

  ENDIF 

  ;; X window and gif output
  IF (NOT nodisplay) THEN BEGIN 

;      angle = $
;        angle_rowcol2radec(object.run, $
;                           object.camcol, $
;                           object.field, clr)

;      tvasinh, rotate(fchart, angle*180./!pi), $
;        /noframe, title=title, _extra=extra, order=order
;      plot_ne_arrows, 0.0, fracsize=0.05, offset_frac=offset_frac,$
;        order=order

      tvasinh, fchart, /noframe, title=title, _extra=extra, order=order
      display_fchart_getcolor, circ_color

      IF (NOT nocirc) THEN BEGIN
          IF (NOT keyword_set(box) ) THEN $
            tvcircle, circ_rad, objx, objy, circ_color, /data $
          ELSE tvbox, 2*circ_rad, objx, objy, circ_color, /data
      ENDIF
      axis,xaxis=0,xticks=4,xtickn=xlabels, xtitle=xtitle
      axis,xaxis=1,xticks=4,xtickn=[' ',' ',' ',' ',' ']
      axis,yaxis=0,yticks=4,ytickn=ylabels, ytitle=ytitle
      axis,yaxis=1,yticks=4,ytickn=[' ',' ',' ',' ',' ']

      IF keyword_set(directions) THEN BEGIN 
          angle = $
            angle_rowcol2radec(object.run, $
                               object.camcol, $
                               object.field, clr)
          plot_ne_arrows, angle, fracsize=0.05, offset_frac=offset_frac,$
            order=order
      ENDIF 

      IF (gif) THEN BEGIN
          wshow, 0, iconic=0
          write_gif, fnamegif, tvrd()
      ENDIF
  ENDIF 
  !p.multi=pold


return
end











