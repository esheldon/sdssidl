;+
; NAME:
;  MARKXY
;
;
; PURPOSE:
;  Mark the specified x,y point on the current device using one of the 
;  pre-defined marks.  These include a cross with a user-defined hole at
;  the center, a circle, an ellipse, or a box.  The details can be all
;  specified with a mark structure, or by specifying each separately.
;
;
; CATEGORY:
;  Plotting
;
; CALLING SEQUENCE:
;  markxy, markstruct -OR- radius, x, y, 
;       mark_type=, 
;       /cross, hole_fraction=, 
;       /box, 
;       /circle, 
;       /ellipse, axis_ratio=, pos_angle=,
;       color=, linestyle=, _extra=
;
;
; INPUTS:
;  Either of the following formats:
;    markstruct: a structure defining the mark.  Can contain the following:
;       x, y, radius, mark_type, hole_fraction, axis_ratio, pos_angle,
;       color, linestyle
;
;    radius, x, y:  The basics for the mark, with other info being specified 
;       through keywords.  These can all be set with a markstruct as well.
;
;
; OPTIONAL INPUTS:
;    /cross: Use a cross for the mark (default).
;    hole_fraction: The size of the hole in the cross as a percentage. Default
;       is 1/3
;
;    /box: Mark the position with a box
;    /circle: Mark the position with a circle
;
;    /ellipse: Mark the position with a ellipse
;    axis_ratio: axis ratio of ellipse. Default 1.
;    pos_angle: position angle of ellipse.  default is 0.
;
;    color=, linestyle=: Style for drawing marks.
;    _extra: extra stuff sent to plot routine.
;
; EXAMPLES:
;   IDL> markxy, radius, x, y, /box
;
;   IDL> markstruct = {x: 2.5, y: 2.6, type:'circle', color:!red}
;   IDL> markxy, markstruct
;   
; MODIFICATION HISTORY:
;  Created: sometime in 2005.  Documented, andded to archive, 23-Sep-2005.
;     Erin Sheldon, Uchicago/NYU
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


PRO markxy_syntax

  print,'-Syntax: markxy, radius, x, y, $'
  print,'                     -- OR --'
  print,'         markxy, markStruct, $'
  print,'                 $                   ; a code for mark style'
  print,'                 mark_type=mark_type, $'
  print,'                 $                   ; cross (default)'
  print,'                 /cross, $'
  print,'                 hole_fraction=hole_fraction, $'
  print,'                 $                   ; box'
  print,'                 /box, $'
  print,'                 $                   ; circle'
  print,'                 /circle, $'
  print,'                 $                   ; ellipse'
  print,'                 /ellipse, $'
  print,'                 axis_ratio=axis_ratio, $'
  print,'                 pos_angle=pos_angle, _extra=_extra'
  print
  print,'mark_type = "cross" "box" "circle" "ellipse"'
  return

END 

FUNCTION markxy_getval, input_array, nmark, imark

  IF n_elements(input_array) EQ nmark THEN return,input_array[imark] $
  ELSE return,input_array[0]

END 

PRO markxy_checkinputs, radius, x, y, $
                        mark_type=mark_type, $
                        hole_fraction=hole_fraction, $
                        axis_ratio=axis_ratio, $
                        pos_angle=pos_angle, $
                        color=color, $
                        linestyle=linestyle

  IF n_params() LT 3 THEN BEGIN 
      message,'You must input x, y, and radius'
  ENDIF 

  nradius = n_elements(radius)
  nx = n_elements(x)
  ny = n_elements(y)

  IF (nx NE ny) THEN BEGIN 
      message,'x, y must be same size'
  ENDIF 

  ;; These can be all length one and applied to all positions  
  ntype = n_elements(mark_type)
  nhole = n_elements(hole_fraction)
  naxis = n_elements(axis_ratio)
  npos = n_elements(pos_angle)
  ncolor = n_elements(color)
  nline = n_elements(linestyle)

  IF (nradius NE 0) AND (nradius NE nx) AND (nradius NE 1) THEN BEGIN 
      message,'radius must be size 1 or size of x,y'
  ENDIF

  IF (ntype NE 0) AND (ntype NE nx) AND (ntype NE 1) THEN BEGIN 
      message,'type must be size 1 or size of x,y'
  ENDIF
  IF (nhole NE 0) AND (nhole NE nx) AND (nhole NE 1) THEN BEGIN 
      message,'hole must be size 1 or size of x,y'
  ENDIF 
  IF (naxis NE 0) AND (naxis NE nx) AND (naxis NE 1) THEN BEGIN 
      message,'axis must be size 1 or size of x,y'
  ENDIF
  IF (npos NE 0) AND (npos NE nx) AND (npos NE 1) THEN BEGIN 
      message,'pos must be size 1 or size of x,y'
  ENDIF 
  IF (ncolor NE 0) AND (ncolor NE nx) AND (ncolor NE 1) THEN BEGIN 
      message,'color must be size 1 or size of x,y'
  ENDIF 
  IF (nline NE 0) AND (nline NE nx) AND (nline NE 1) THEN BEGIN 
      message,'line must be size 1 or size of x,y'
  ENDIF 
 
END 



PRO markxy, radius_OR_markStruct, x, y, $
            $                   ; a code for mark style
            mark_type=mark_type, $
            $                   ; cross (default)
            cross=cross, $
            hole_fraction=hole_fraction, $
            box=box, $
            $                   ; circle
            circle=circle, $
            $                   ; ellipse
            ellipse=ellipse, $
            axis_ratio=axis_ratio, $
            pos_angle=pos_angle, $
            color=color, $
            linestyle=linestyle, $
            _extra=_extra

  ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
  ;; Process the inputs
  ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

  np = n_params()
  IF (np LT 1) THEN BEGIN 
      markxy_syntax
      return
  ENDIF ELSE BEGIN 
      rd = size(radius_OR_markStruct, /tname)
      IF rd EQ 'STRUCT' THEN BEGIN 
          markStruct = radius_OR_markStruct

          markstruct_extract, $
            markStruct, $
            mark_radius=radius, $
            x=x, y=y, $
            mark_type=mark_type, $
            hole_fraction=hole_fraction, $
            axis_ratio=axis_ratio, $
            pos_angle=pos_angle, $
            color=color, $
            linestyle=linestyle

      ENDIF ELSE BEGIN 
          IF np NE 3 THEN BEGIN 
              markxy_syntax
              return
          ENDIF 
          radius = radius_OR_markStruct
      ENDELSE 
  ENDELSE 

  markxy_checkinputs, radius, x, y, $
    mark_type=mark_type, $
    hole_fraction=hole_fraction, $
    axis_ratio=axis_ratio, $
    pos_angle=pos_angle, $
    color=color, $
    linestyle=linestyle

  ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
  ;; Loop over all input positions
  ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
  
  nmark = n_elements(x)

  FOR imark=0L, nmark-1 DO BEGIN 

      ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
      ;; Get the input value for this mark.  Getval will properly deal
      ;; with single inputs versus array inputs
      ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

      uradius = markxy_getval(radius, nmark, imark)
      
      IF n_elements(mark_type) NE 0 THEN BEGIN 
          umark_type=markxy_getval(mark_type,nmark,imark)
      ENDIF 
      IF n_elements(hole_fraction) NE 0 THEN BEGIN 
          uhole_fraction=markxy_getval(hole_fraction,nmark,imark)
      ENDIF 

      IF n_elements(pos_angle) NE 0 THEN BEGIN 
          upos_angle=markxy_getval(pos_angle,nmark,imark)
      ENDIF 
      IF n_elements(axis_ratio) NE 0 THEN BEGIN 
          uaxis_ratio=markxy_getval(axis_ratio,nmark,imark)
      ENDIF 

      IF n_elements(color) NE 0 THEN BEGIN 
          ucolor=markxy_getval(color,nmark,imark)
      ENDIF 
      IF n_elements(linestyle) NE 0 THEN BEGIN 
          ulinestyle=markxy_getval(linestyle,nmark,imark)
      ENDIF 

      ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
      ;; Can use a code to determine mark type
      ;; Always takes precedence over the keywords
      ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

      IF n_elements(umark_type) NE 0 THEN BEGIN 
          cross=0
          box=0
          circle=0
          ellipse=0
          CASE umark_type OF
              'cross': cross=1
              'box': box=1
              'circle': circle=1
              'ellipse': ellipse=1
              ELSE: BEGIN 
                  print,'mark_type must be: '
                  print,'     "cross"'
                  print,'     "box"'
                  print,'     "circle"'
                  print,'     "ellipse"'
                  message,'Error'
                  return
              END 
          ENDCASE 
      ENDIF 

      ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
      ;; Make the mark
      ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

      IF keyword_set(box) THEN BEGIN 

          tvbox, uradius, x[imark], y[imark], /data, noclip=0, $
            color=ucolor, linestyle=ulinestyle, _extra=_extra

      ENDIF ELSE IF keyword_set(circle) THEN BEGIN 

          tvcircle, uradius, x[imark], y[imark], /data, noclip=0, $
            color=ucolor, linestyle=ulinestyle, _extra=_extra

      ENDIF ELSE IF keyword_set(ellipse) THEN BEGIN 
          
          plot_ellipse, uradius, x[imark], y[imark], $
            axis_ratio=uaxis_ratio, $
            pos_angle=upos_angle, $
            color=ucolor, linestyle=ulinestyle, _extra=_extra
          
      ENDIF ELSE BEGIN 
          
          ;; The default is a hole at the center 1/3. of size
          IF n_elements(uhole_fraction) EQ 0 THEN $
            uhole_fraction = 1./3.

          plot_cross, uradius, x[imark], y[imark], $
            hole_fraction=uhole_fraction, $
            color=ucolor, linestyle=ulinestyle, _extra=_extra
          
      ENDELSE 


  ENDFOR 


return

  ;; Can use a code to determine mark type
  ;; Always takes precedence over the keywords
  IF n_elements(umark_type) NE 0 THEN BEGIN 
      cross=0
      box=0
      circle=0
      ellipse=0
      CASE mark_type OF
          'cross': cross=1
          'box': box=1
          'circle': circle=1
          'ellipse': ellipse=1
          ELSE: BEGIN 
              print,'mark_type must be: '
              print,'     "cross"'
              print,'     "box"'
              print,'     "circle"'
              print,'     "ellipse"'
              message,'Error'
              return
          END 
      ENDCASE 
  ENDIF 


  IF keyword_set(box) THEN BEGIN 
      tvbox,    radius, x, y, /data, noclip=0, $
        color=color, linestyle=linestyle, _extra=_extra
  ENDIF ELSE IF keyword_set(circle) THEN BEGIN 
      tvcircle, radius, x, y, /data, noclip=0, $
        color=color, linestyle=linestyle, _extra=_extra
  ENDIF ELSE IF keyword_set(ellipse) THEN BEGIN 

      plot_ellipse, radius, x, y, $
        axis_ratio=axis_ratio, $
        pos_angle=pos_angle, $
        color=color, linestyle=linestyle, _extra=_extra

  ENDIF ELSE BEGIN 

      plot_cross, radius, x, y, hole_fraction=hole_fraction, $
        color=color, linestyle=linestyle, _extra=_extra

  ENDELSE 

END 
